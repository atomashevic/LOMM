#!/usr/bin/env Rscript

audit_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot locate audit script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

audit_parse_args <- function(args) {
  parsed <- list(reps = 25L, output = NULL)
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token %in% c("--reps", "--output")) {
      if (index == length(args)) stop("Missing value after ", token, ".", call. = FALSE)
      key <- sub("^--", "", token)
      parsed[[key]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      stop("Unknown argument: ", token, call. = FALSE)
    }
  }
  parsed$reps <- suppressWarnings(as.integer(parsed$reps))
  if (!is.finite(parsed$reps) || parsed$reps < 1L || parsed$reps > 400L) {
    stop("--reps must be an integer from 1 to 400.", call. = FALSE)
  }
  parsed
}

audit_atomic_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid()))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) stop("Failed to publish: ", path, call. = FALSE)
}

audit_panel_metrics <- function(simulation, scenario, time_points, replication) {
  latent <- do.call(rbind, simulation$latents)
  observed <- as.matrix(simulation$panel[, simulation$item_names, drop = FALSE])
  tail_start <- max(1L, time_points - ceiling(0.10 * time_points) + 1L)
  latent_by_clip <- simulation$latents
  energy_fraction <- vapply(latent_by_clip, function(clip) {
    numerator <- sum(clip[tail_start:time_points, , drop = FALSE]^2)
    denominator <- sum(clip^2)
    if (denominator > 0) numerator / denominator else NA_real_
  }, numeric(1))
  data.frame(
    scenario = scenario$scenario_family,
    dimensions = scenario$scenario_nfac,
    time_points = time_points,
    horizon = (time_points - 1) * simulation$dt,
    replication = replication,
    max_abs_activity_scaled_latent = max(abs(latent)),
    activity_scaled_latent_iqr = stats::IQR(as.numeric(latent), type = 8),
    median_channel_iqr = stats::median(apply(observed, 2L, stats::IQR, type = 8)),
    final_tenth_energy_fraction = mean(energy_fraction, na.rm = TRUE),
    nonfinite_latent_values = sum(!is.finite(latent)),
    nonfinite_observed_values = sum(!is.finite(observed)),
    stringsAsFactors = FALSE
  )
}

audit_summarize <- function(rows, replications) {
  groups <- split(
    seq_len(nrow(rows)),
    interaction(rows$scenario, rows$dimensions, rows$time_points, drop = TRUE)
  )
  summaries <- lapply(groups, function(indices) {
    data <- rows[indices, , drop = FALSE]
    data.frame(
      Scenario = if (data$scenario[[1]] == "oai_ranked_persistence") {
        "Amplifying dynamics, one dominant dimension"
      } else {
        "Amplifying dynamics, switching dominant dimension"
      },
      Dimensions = data$dimensions[[1]],
      `Time points` = data$time_points[[1]],
      Horizon = data$horizon[[1]],
      Replications = replications,
      `Median maximum absolute activity-scaled latent` = stats::median(
        data$max_abs_activity_scaled_latent
      ),
      `95th percentile maximum absolute activity-scaled latent` = unname(stats::quantile(
        data$max_abs_activity_scaled_latent, 0.95, type = 8
      )),
      `Median activity-scaled latent IQR` = stats::median(
        data$activity_scaled_latent_iqr
      ),
      `Median observed-channel IQR` = stats::median(data$median_channel_iqr),
      `Median final-tenth energy fraction` = stats::median(
        data$final_tenth_energy_fraction
      ),
      `Nonfinite latent values` = sum(data$nonfinite_latent_values),
      `Nonfinite observed values` = sum(data$nonfinite_observed_values),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, summaries)
  output[order(output$Scenario, output$Dimensions, output$`Time points`), , drop = FALSE]
}

run_amplification_audit <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- audit_parse_args(args)
  script_path <- audit_script_path()
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
  if (is.null(parsed$output)) {
    parsed$output <- file.path(
      repo_root, "paper", "tables", "finite_horizon_amplification_diagnostics.csv"
    )
  }
  old_directory <- getwd()
  old_grid_flag <- Sys.getenv("EMOTION_BENCHMARK_GRID_ONLY", unset = NA_character_)
  on.exit({
    setwd(old_directory)
    if (is.na(old_grid_flag)) Sys.unsetenv("EMOTION_BENCHMARK_GRID_ONLY") else
      Sys.setenv(EMOTION_BENCHMARK_GRID_ONLY = old_grid_flag)
  }, add = TRUE)
  setwd(repo_root)
  Sys.setenv(EMOTION_BENCHMARK_GRID_ONLY = "1")
  suppressMessages(source(
    file.path(repo_root, "scripts", "simulations", "run_emotion_dynega_simulation.R"),
    local = .GlobalEnv
  ))

  scenarios <- build_emotion_scenarios()
  scenario_table <- data.frame(
    scenario_index = seq_along(scenarios),
    scenario = vapply(scenarios, `[[`, character(1), "scenario"),
    stringsAsFactors = FALSE
  )
  conditions <- expand.grid(
    scenario_idx = seq_along(scenarios),
    N = c(50L, 100L),
    TimeP = c(100L, 300L),
    NVAR = c(4L, 6L),
    Error_sd = c(0.125, 0.25, 0.50),
    Loadings = c(0.4, 0.6, 0.8),
    KEEP.OUT.ATTRS = FALSE
  )
  condition_names <- scenario_table$scenario[conditions$scenario_idx]
  keep <- grepl("^oai_", condition_names) &
    conditions$N == 50L & conditions$NVAR == 4L &
    abs(conditions$Error_sd - 0.25) < 1e-12 &
    abs(conditions$Loadings - 0.6) < 1e-12
  selected_indices <- which(keep)
  cfg <- build_benchmark_config(smoke = FALSE)
  cfg$integrator <- "linear_exact"

  rows <- vector("list", length(selected_indices) * parsed$reps)
  row_index <- 1L
  for (condition_index in selected_indices) {
    condition <- conditions[condition_index, , drop = FALSE]
    scenario <- scenarios[[condition$scenario_idx[[1]]]]
    for (replication in seq_len(parsed$reps)) {
      grid_row_id <- (condition_index - 1L) * 400L + replication
      scenario_row <- make_grid_row(
        scenario = scenario,
        grid_scope = "amplification_diagnostic",
        N = condition$N,
        TimeP = condition$TimeP,
        NVAR = condition$NVAR,
        Error_sd = condition$Error_sd,
        Loadings = condition$Loadings,
        rep_idx = replication,
        row_id = grid_row_id
      )
      simulation <- simulate_benchmark_panel(
        scenario_row, cfg, scenario_row$seed_base[[1]]
      )
      rows[[row_index]] <- audit_panel_metrics(
        simulation, scenario, condition$TimeP[[1]], replication
      )
      row_index <- row_index + 1L
    }
  }
  output <- audit_summarize(do.call(rbind, rows), parsed$reps)
  if (nrow(output) != 12L || any(!is.finite(as.matrix(output[, 2:10]))) ||
      any(output$`Nonfinite latent values` != 0L) ||
      any(output$`Nonfinite observed values` != 0L)) {
    stop("Finite-horizon amplification audit failed its output contract.", call. = FALSE)
  }
  audit_atomic_csv(output, normalizePath(dirname(parsed$output), mustWork = FALSE) |>
    file.path(basename(parsed$output)))
  cat("Published finite-horizon amplification diagnostics: ", parsed$output, "\n", sep = "")
  invisible(output)
}

if (sys.nframe() == 0L) run_amplification_audit()
