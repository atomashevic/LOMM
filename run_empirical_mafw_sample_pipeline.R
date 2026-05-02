#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("run_empirical_mafw_sample_pipeline.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "extract.r"))

# Compare empirical fit for two dynamic regimes:
# 1) homogeneous: all latent factors share the same eta / zeta
# 2) heterogeneous: latent factors follow a ranked eta / zeta profile around
#    a shared base pair, which creates factor-specific temporal regimes

read_flag <- function(name, default = FALSE) {
  value <- trimws(Sys.getenv(name, unset = if (default) "1" else "0"))
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

read_int <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(as.integer(default))
  }
  as.integer(value)
}

read_num_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(as.numeric(default))
  }
  as.numeric(strsplit(value, ",", fixed = TRUE)[[1]])
}

read_chr_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

read_char <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  value
}

build_frame_score_paths <- function() {
  list(
    "oai-base" = read_char(
      "EMPIRICAL_DLO_OAI_BASE_PATH",
      "/home/atomasevic/FER-evaluation/results/clip/mafw-sample/frame_scores.csv"
    ),
    "oai-large" = read_char(
      "EMPIRICAL_DLO_OAI_LARGE_PATH",
      "/home/atomasevic/FER-evaluation/results/clip-large/mafw-sample/frame_scores.csv"
    ),
    "oai-large-100" = read_char(
      "EMPIRICAL_DLO_OAI_LARGE_100_PATH",
      "/home/atomasevic/FER-evaluation/results/clip-large/mafw-sample-100/frame_scores.csv"
    )
  )
}

log_line <- function(...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] %s", timestamp, paste(..., collapse = "")))
}

format_vector_spec <- function(x, digits = 4L) {
  paste(formatC(as.numeric(x), format = "f", digits = digits), collapse = ";")
}

build_ranked_profile <- function(base_value, n_fac, spread, name) {
  spread <- as.numeric(spread)
  if (!is.finite(spread) || spread < 0) {
    stop(sprintf("%s spread must be non-negative.", name), call. = FALSE)
  }
  if (n_fac <= 1L || spread == 0) {
    return(rep(as.numeric(base_value), n_fac))
  }

  anchors <- seq(-1, 1, length.out = n_fac)
  values <- as.numeric(base_value) * (1 + spread * anchors)

  if (base_value < 0) {
    values <- pmin(values, -1e-4)
  } else if (base_value > 0) {
    values <- pmax(values, 1e-4)
  }

  values
}

expand_dynamic_parameters <- function(mode,
                                      n_fac,
                                      eta_base,
                                      zeta_base,
                                      eta_spread = 0,
                                      zeta_spread = 0) {
  if (mode == "homogeneous" || n_fac <= 1L) {
    eta_vec <- rep(as.numeric(eta_base), n_fac)
    zeta_vec <- rep(as.numeric(zeta_base), n_fac)
    profile <- "shared"
  } else if (mode == "heterogeneous") {
    eta_vec <- build_ranked_profile(eta_base, n_fac, eta_spread, "eta")
    zeta_vec <- build_ranked_profile(zeta_base, n_fac, zeta_spread, "zeta")
    profile <- "linear_ranked"
  } else {
    stop(sprintf("Unknown candidate mode: %s", mode), call. = FALSE)
  }

  list(
    mode = mode,
    profile = profile,
    eta = eta_vec,
    zeta = zeta_vec,
    n_fac = n_fac
  )
}

build_candidate_grid <- function(modes,
                                 n_fac,
                                 eta_vals,
                                 zeta_vals,
                                 sigma_q_vals,
                                 dt_vals,
                                 eta_spread_vals,
                                 zeta_spread_vals) {
  rows <- list()

  if ("homogeneous" %in% modes) {
    rows[[length(rows) + 1L]] <- expand.grid(
      mode = "homogeneous",
      eta_base = eta_vals,
      zeta_base = zeta_vals,
      sigma_q = sigma_q_vals,
      dt = dt_vals,
      eta_spread = 0,
      zeta_spread = 0,
      stringsAsFactors = FALSE
    )
  }

  if ("heterogeneous" %in% modes && n_fac > 1L) {
    rows[[length(rows) + 1L]] <- expand.grid(
      mode = "heterogeneous",
      eta_base = eta_vals,
      zeta_base = zeta_vals,
      sigma_q = sigma_q_vals,
      dt = dt_vals,
      eta_spread = eta_spread_vals,
      zeta_spread = zeta_spread_vals,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) {
    stop("No candidate modes requested.", call. = FALSE)
  }

  grid <- dplyr::bind_rows(rows)
  grid$candidate_id <- seq_len(nrow(grid))
  grid
}

evaluate_candidate_spec <- function(empirical_target,
                                    candidate_row,
                                    n_replicates,
                                    integrator,
                                    ord0_embed,
                                    seed) {
  dynamics <- expand_dynamic_parameters(
    mode = candidate_row$mode[[1]],
    n_fac = length(unique(empirical_target$community_map)),
    eta_base = candidate_row$eta_base[[1]],
    zeta_base = candidate_row$zeta_base[[1]],
    eta_spread = candidate_row$eta_spread[[1]],
    zeta_spread = candidate_row$zeta_spread[[1]]
  )

  synthetic_panel <- simulate_empirical_dlo_panel(
    clip_lengths = empirical_target$clip_lengths,
    community_map = empirical_target$community_map,
    loadings = empirical_target$loadings,
    measurement_var = empirical_target$measurement_var,
    eta = dynamics$eta,
    zeta = dynamics$zeta,
    sigma_q = candidate_row$sigma_q[[1]],
    dt = candidate_row$dt[[1]],
    n_replicates = n_replicates,
    model_name = paste0(empirical_target$model, "_", dynamics$mode, "_synthetic"),
    integrator = integrator,
    seed = seed
  )

  synthetic_target <- build_property_target_from_panel(
    panel = synthetic_panel,
    model_name = paste0(empirical_target$model, "_", dynamics$mode, "_synthetic"),
    score_cols = empirical_target$score_cols,
    ord0_embed = ord0_embed
  )

  loss <- score_candidate_loss(empirical_target, synthetic_target)

  summary_row <- data.frame(
    candidate_id = candidate_row$candidate_id[[1]],
    mode = dynamics$mode,
    profile = dynamics$profile,
    n_fac = dynamics$n_fac,
    eta_base = candidate_row$eta_base[[1]],
    zeta_base = candidate_row$zeta_base[[1]],
    eta_spread = candidate_row$eta_spread[[1]],
    zeta_spread = candidate_row$zeta_spread[[1]],
    eta_spec = format_vector_spec(dynamics$eta),
    zeta_spec = format_vector_spec(dynamics$zeta),
    sigma_q = candidate_row$sigma_q[[1]],
    dt = candidate_row$dt[[1]],
    n_replicates = n_replicates,
    raw_loss = loss$raw_loss,
    latent_loss = loss$latent_loss,
    total_loss = loss$total_loss,
    synthetic_n_dim = synthetic_target$n_dim,
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_row,
    comparison = loss$comparison,
    synthetic_panel = synthetic_panel,
    synthetic_target = synthetic_target
  )
}

write_mode_outputs <- function(mode_result, output_dir, prefix) {
  utils::write.csv(mode_result$synthetic_panel,
                   file.path(output_dir, paste0(prefix, "_synthetic_panel.csv")),
                   row.names = FALSE)
  utils::write.csv(mode_result$synthetic_target$raw_metrics,
                   file.path(output_dir, paste0(prefix, "_synthetic_raw_metrics.csv")),
                   row.names = FALSE)
  utils::write.csv(mode_result$synthetic_target$latent_metrics,
                   file.path(output_dir, paste0(prefix, "_synthetic_latent_metrics.csv")),
                   row.names = FALSE)
  utils::write.csv(mode_result$comparison,
                   file.path(output_dir, paste0(prefix, "_metric_comparison.csv")),
                   row.names = FALSE)
}

write_verdict_file <- function(mode_summary, output_path) {
  mode_summary <- mode_summary[order(mode_summary$total_loss), , drop = FALSE]
  best_mode <- mode_summary$mode[[1]]
  best_loss <- mode_summary$total_loss[[1]]

  lines <- c(
    sprintf("Best overall mode: %s", best_mode),
    sprintf("Best overall total_loss: %.4f", best_loss),
    ""
  )

  for (idx in seq_len(nrow(mode_summary))) {
    row <- mode_summary[idx, , drop = FALSE]
    lines <- c(
      lines,
      sprintf(
        "%s | total_loss=%.4f | raw_loss=%.4f | latent_loss=%.4f | eta=%s | zeta=%s | sigma_q=%.4f | dt=%.4f",
        row$mode[[1]],
        row$total_loss[[1]],
        row$raw_loss[[1]],
        row$latent_loss[[1]],
        row$eta_spec[[1]],
        row$zeta_spec[[1]],
        row$sigma_q[[1]],
        row$dt[[1]]
      )
    )
  }

  if (all(c("homogeneous", "heterogeneous") %in% mode_summary$mode)) {
    homo_loss <- mode_summary$total_loss[mode_summary$mode == "homogeneous"][[1]]
    hetero_loss <- mode_summary$total_loss[mode_summary$mode == "heterogeneous"][[1]]
    margin <- abs(homo_loss - hetero_loss)
    winner <- if (hetero_loss < homo_loss) "heterogeneous" else "homogeneous"
    lines <- c(
      lines,
      "",
      sprintf("Winner between homogeneous and heterogeneous: %s", winner),
      sprintf("Loss margin: %.4f", margin)
    )
  }

  writeLines(lines, con = output_path, useBytes = TRUE)
}

run_single_model <- function(model_name,
                             frame_scores_path,
                             output_dir,
                             candidate_modes,
                             eta_vals,
                             zeta_vals,
                             sigma_q_vals,
                             dt_vals,
                             eta_spread_vals,
                             zeta_spread_vals,
                             n_replicates,
                             ord0_embed,
                             integrator,
                             seed) {
  if (!file.exists(frame_scores_path)) {
    stop(sprintf("Missing frame score file for %s: %s", model_name, frame_scores_path), call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  empirical_target <- build_empirical_target(
    frame_scores_path = frame_scores_path,
    model_name = model_name,
    ord0_embed = ord0_embed
  )

  candidate_grid <- build_candidate_grid(
    modes = candidate_modes,
    n_fac = length(unique(empirical_target$community_map)),
    eta_vals = eta_vals,
    zeta_vals = zeta_vals,
    sigma_q_vals = sigma_q_vals,
    dt_vals = dt_vals,
    eta_spread_vals = eta_spread_vals,
    zeta_spread_vals = zeta_spread_vals
  )

  utils::write.csv(candidate_grid, file.path(output_dir, "candidate_grid.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$panel, file.path(output_dir, "empirical_panel_clean.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$qc, file.path(output_dir, "empirical_qc.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$raw_metrics, file.path(output_dir, "empirical_raw_metrics.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$latent_metrics, file.path(output_dir, "empirical_latent_metrics.csv"), row.names = FALSE)

  evaluated <- vector("list", nrow(candidate_grid))
  summary_rows <- vector("list", nrow(candidate_grid))

  for (idx in seq_len(nrow(candidate_grid))) {
    candidate_row <- candidate_grid[idx, , drop = FALSE]
    log_line(
      "Evaluating ", model_name,
      " candidate ", idx, "/", nrow(candidate_grid),
      " [", candidate_row$mode[[1]], "]"
    )

    evaluated[[idx]] <- evaluate_candidate_spec(
      empirical_target = empirical_target,
      candidate_row = candidate_row,
      n_replicates = n_replicates,
      integrator = integrator,
      ord0_embed = ord0_embed,
      seed = seed + idx
    )
    summary_rows[[idx]] <- evaluated[[idx]]$summary
  }

  candidate_summary <- dplyr::bind_rows(summary_rows) %>%
    dplyr::arrange(total_loss, latent_loss, raw_loss)
  candidate_summary$candidate_rank <- seq_len(nrow(candidate_summary))

  mode_summary <- candidate_summary %>%
    dplyr::group_by(mode) %>%
    dplyr::slice_min(order_by = total_loss, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(total_loss)
  mode_summary$delta_from_best <- mode_summary$total_loss - min(mode_summary$total_loss)

  utils::write.csv(candidate_summary, file.path(output_dir, "candidate_summary.csv"), row.names = FALSE)
  utils::write.csv(mode_summary, file.path(output_dir, "mode_summary.csv"), row.names = FALSE)
  utils::write.csv(mode_summary[1, , drop = FALSE], file.path(output_dir, "winner_summary.csv"), row.names = FALSE)
  write_verdict_file(mode_summary, file.path(output_dir, "verdict.txt"))

  for (mode_name in mode_summary$mode) {
    best_id <- mode_summary$candidate_id[mode_summary$mode == mode_name][[1]]
    write_mode_outputs(
      mode_result = evaluated[[best_id]],
      output_dir = output_dir,
      prefix = mode_name
    )
  }

  saveRDS(
    list(
      empirical = empirical_target,
      candidate_grid = candidate_grid,
      candidate_summary = candidate_summary,
      mode_summary = mode_summary,
      evaluated = evaluated
    ),
    file.path(output_dir, "comparison_bundle.rds")
  )

  list(
    empirical = empirical_target,
    candidate_summary = candidate_summary,
    mode_summary = mode_summary,
    evaluated = evaluated
  )
}

main <- function() {
  smoke_mode <- read_flag("RUN_EMPIRICAL_DLO_SMOKE", default = FALSE)
  seed <- read_int("EMPIRICAL_DLO_SEED", 20260421L)
  models <- read_chr_vec("EMPIRICAL_DLO_MODELS", c("oai-large"))
  candidate_modes <- read_chr_vec("EMPIRICAL_DLO_COMPARE_MODES", c("homogeneous", "heterogeneous"))
  integrator <- read_char("EMPIRICAL_DLO_INTEGRATOR", "linear_exact")
  ord0_embed <- read_int("EMPIRICAL_DLO_ORD0_EMBED", 10L)

  frame_score_paths <- build_frame_score_paths()

  output_root_default <- if (smoke_mode) {
    file.path(repo_root, "empirical_outputs_smoke", "mafw-sample")
  } else {
    file.path(repo_root, "empirical_outputs", "mafw-sample")
  }
  output_root <- read_char("EMPIRICAL_DLO_OUTPUT_ROOT", output_root_default)

  if (smoke_mode) {
    eta_vals <- read_num_vec("EMPIRICAL_DLO_ETA", c(-0.10, -0.30, -0.50))
    zeta_vals <- read_num_vec("EMPIRICAL_DLO_ZETA", c(0.10, 0.25, 0.40))
    sigma_q_vals <- read_num_vec("EMPIRICAL_DLO_SIGMA_Q", c(0.10, 0.25))
    dt_vals <- read_num_vec("EMPIRICAL_DLO_DT", c(0.50, 1.00))
    eta_spread_vals <- read_num_vec("EMPIRICAL_DLO_ETA_SPREAD", c(0.20, 0.50))
    zeta_spread_vals <- read_num_vec("EMPIRICAL_DLO_ZETA_SPREAD", c(0.20, 0.50))
    n_replicates <- read_int("EMPIRICAL_DLO_REPS", 4L)
  } else {
    eta_vals <- read_num_vec("EMPIRICAL_DLO_ETA", c(-0.10, -0.25, -0.40, -0.60))
    zeta_vals <- read_num_vec("EMPIRICAL_DLO_ZETA", c(0.10, 0.25, 0.40))
    sigma_q_vals <- read_num_vec("EMPIRICAL_DLO_SIGMA_Q", c(0.10, 0.25, 0.40))
    dt_vals <- read_num_vec("EMPIRICAL_DLO_DT", c(0.50, 1.00))
    eta_spread_vals <- read_num_vec("EMPIRICAL_DLO_ETA_SPREAD", c(0.20, 0.50))
    zeta_spread_vals <- read_num_vec("EMPIRICAL_DLO_ZETA_SPREAD", c(0.20, 0.50))
    n_replicates <- read_int("EMPIRICAL_DLO_REPS", 8L)
  }

  run_config <- list(
    smoke_mode = smoke_mode,
    seed = seed,
    models = models,
    candidate_modes = candidate_modes,
    integrator = integrator,
    ord0_embed = ord0_embed,
    output_root = output_root,
    eta_vals = eta_vals,
    zeta_vals = zeta_vals,
    sigma_q_vals = sigma_q_vals,
    dt_vals = dt_vals,
    eta_spread_vals = eta_spread_vals,
    zeta_spread_vals = zeta_spread_vals,
    n_replicates = n_replicates
  )

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  saveRDS(run_config, file.path(output_root, "run_config.rds"))

  results <- vector("list", length(models))
  names(results) <- models

  for (model_name in models) {
    frame_scores_path <- frame_score_paths[[model_name]]
    if (is.null(frame_scores_path)) {
      stop(sprintf("Unknown model '%s'. Supported models: %s", model_name, paste(names(frame_score_paths), collapse = ", ")))
    }

    model_output_dir <- file.path(output_root, model_name)
    log_line("Running empirical DLO mode comparison for ", model_name, " -> ", model_output_dir)

    started <- Sys.time()
    results[[model_name]] <- run_single_model(
      model_name = model_name,
      frame_scores_path = frame_scores_path,
      output_dir = model_output_dir,
      candidate_modes = candidate_modes,
      eta_vals = eta_vals,
      zeta_vals = zeta_vals,
      sigma_q_vals = sigma_q_vals,
      dt_vals = dt_vals,
      eta_spread_vals = eta_spread_vals,
      zeta_spread_vals = zeta_spread_vals,
      n_replicates = n_replicates,
      ord0_embed = ord0_embed,
      integrator = integrator,
      seed = seed
    )
    elapsed <- difftime(Sys.time(), started, units = "secs")

    winner <- results[[model_name]]$mode_summary[1, , drop = FALSE]
    summary_row <- data.frame(
      model = model_name,
      best_mode = winner$mode[[1]],
      total_loss = winner$total_loss[[1]],
      raw_loss = winner$raw_loss[[1]],
      latent_loss = winner$latent_loss[[1]],
      empirical_n_dim = results[[model_name]]$empirical$n_dim,
      synthetic_n_dim = winner$synthetic_n_dim[[1]],
      elapsed_seconds = as.numeric(elapsed),
      stringsAsFactors = FALSE
    )
    utils::write.csv(summary_row, file.path(model_output_dir, "run_summary.csv"), row.names = FALSE)
    log_line(
      "Finished ", model_name,
      " in ", sprintf("%.1f", as.numeric(elapsed)),
      "s; best_mode=", summary_row$best_mode[[1]],
      "; total_loss=", sprintf("%.4f", summary_row$total_loss[[1]])
    )
  }

  all_summary <- dplyr::bind_rows(lapply(models, function(model_name) {
    winner <- results[[model_name]]$mode_summary[1, , drop = FALSE]
    data.frame(
      model = model_name,
      best_mode = winner$mode[[1]],
      total_loss = winner$total_loss[[1]],
      raw_loss = winner$raw_loss[[1]],
      latent_loss = winner$latent_loss[[1]],
      empirical_n_dim = results[[model_name]]$empirical$n_dim,
      synthetic_n_dim = winner$synthetic_n_dim[[1]],
      stringsAsFactors = FALSE
    )
  }))

  utils::write.csv(all_summary, file.path(output_root, "run_summary.csv"), row.names = FALSE)
  saveRDS(results, file.path(output_root, "run_results.rds"))

  log_line("All requested models finished. Summary written to ", file.path(output_root, "run_summary.csv"))
}

if (sys.nframe() == 0L) {
  main()
}
