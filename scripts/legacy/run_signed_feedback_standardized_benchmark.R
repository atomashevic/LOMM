#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

if (!nzchar(Sys.getenv("BENCHMARK_METHODS"))) {
  Sys.setenv(BENCHMARK_METHODS = "dynEGA,graphicalVAR,gimme")
}
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = "benchmark_outputs_signed_feedback_standardized")
}
if (!nzchar(Sys.getenv("SIGNED_FEEDBACK_DRY_RUN"))) {
  Sys.setenv(SIGNED_FEEDBACK_DRY_RUN = "1")
}
if (!nzchar(Sys.getenv("SIGNED_FEEDBACK_RUN_GRID"))) {
  Sys.setenv(SIGNED_FEEDBACK_RUN_GRID = "feasibility")
}
if (!nzchar(Sys.getenv("BENCHMARK_CHECKPOINT_EACH"))) {
  Sys.setenv(BENCHMARK_CHECKPOINT_EACH = "1")
}
if (!nzchar(Sys.getenv("BENCHMARK_WORKERS"))) {
  Sys.setenv(BENCHMARK_WORKERS = "4")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_WORKERS"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_WORKERS = "2")
}
if (!nzchar(Sys.getenv("BENCHMARK_GIMME_WORKERS"))) {
  Sys.setenv(BENCHMARK_GIMME_WORKERS = "1")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC = "900")
}
if (!nzchar(Sys.getenv("BENCHMARK_GIMME_TIMEOUT_SEC"))) {
  Sys.setenv(BENCHMARK_GIMME_TIMEOUT_SEC = "900")
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_candidate <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  ""
}
script_path <- if (nzchar(script_candidate) && !identical(script_candidate, "-")) {
  normalizePath(script_candidate, mustWork = TRUE)
} else {
  normalizePath("run_signed_feedback_standardized_benchmark.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "run_method_benchmark.R"))

parse_signed_feedback_bool_env <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  tolower(value) %in% c("1", "true", "yes", "y")
}

signed_feedback_real_max <- function(eta, zeta) {
  A <- matrix(c(0, 1, eta, zeta), nrow = 2, byrow = TRUE)
  max(Re(eigen(A)$values))
}

signed_feedback_q2 <- function(n_factors) {
  q2 <- c(`2` = 1.5, `3` = 2.5, `4` = 4.0)
  unname(q2[as.character(n_factors)])
}

signed_feedback_label <- function(value, prefix) {
  if (value < 0) {
    paste0(prefix, "neg", formatC(abs(value) * 1000, format = "f", digits = 0))
  } else if (value == 0) {
    paste0(prefix, "zero")
  } else {
    paste0(prefix, "pos", formatC(value * 1000, format = "f", digits = 0))
  }
}

signed_feedback_error_label <- function(error_variance) {
  paste0("errsd", formatC(sqrt(error_variance) * 1000, format = "f", digits = 0))
}

signed_feedback_eta_strength <- function(eta) {
  ifelse(
    eta <= -0.30,
    "strong_restoring",
    ifelse(eta <= -0.15, "moderate_restoring", "weak_restoring")
  )
}

build_signed_feedback_condition_grid <- function(grid_scope = c("main", "feasibility"),
                                                 growth_cap = 5) {
  grid_scope <- match.arg(grid_scope)
  if (identical(grid_scope, "main")) {
    grid <- expand.grid(
      eta_scalar = c(-0.05, -0.15, -0.30),
      zeta_scalar = c(-0.30, 0.00, 0.08),
      scenario_nfac = c(2L, 3L, 4L),
      N = c(50L, 100L),
      TimeP = c(50L, 100L),
      NVAR = c(3L, 4L),
      Error = c(0.125^2, 0.25^2, 0.50^2),
      Loadings = c(0.4, 0.6, 0.8),
      Q = c(0.50, 0.75, 1.00),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    grid <- expand.grid(
      eta_scalar = c(-0.15, -0.30),
      zeta_scalar = c(-0.30, 0.00, 0.08),
      scenario_nfac = c(2L, 4L),
      N = c(50L, 100L),
      TimeP = 100L,
      NVAR = 3L,
      Error = 0.025,
      Loadings = 0.6,
      Q = 0.75,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  }

  grid$benchmark_grid <- grid_scope
  grid$dt <- ifelse(grid$TimeP == 50L, 0.5, 0.4)
  grid$growth_horizon <- grid$dt * (grid$TimeP - 1L)
  grid$growth_cap <- growth_cap
  grid$real_max <- mapply(
    signed_feedback_real_max,
    grid$eta_scalar,
    grid$zeta_scalar
  )
  grid$finite_growth <- exp(grid$real_max * grid$growth_horizon)
  grid$feedback_regime <- ifelse(
    grid$zeta_scalar < 0,
    "damped",
    ifelse(grid$zeta_scalar == 0, "critical", "persistent")
  )
  grid$eta_strength <- signed_feedback_eta_strength(grid$eta_scalar)
  grid$dynamic_scenario <- paste(grid$eta_strength, grid$feedback_regime, sep = "_")
  grid$kept <- grid$finite_growth <= growth_cap
  grid$exclusion_reason <- ifelse(
    grid$kept,
    "",
    sprintf("finite_growth %.3f exceeds cap %.3f", grid$finite_growth, growth_cap)
  )
  grid[order(
    grid$benchmark_grid,
    grid$TimeP,
    grid$N,
    grid$scenario_nfac,
    grid$zeta_scalar,
    grid$eta_scalar,
    grid$NVAR,
    grid$Error,
    grid$Loadings,
    grid$Q
  ), ]
}

build_signed_feedback_scenario_grid <- function(condition_grid, reps = 1L,
                                                scenario_family = "signed_feedback_standardized") {
  reps <- as.integer(reps)
  if (!is.finite(reps) || is.na(reps) || reps < 1L) {
    stop("Signed-feedback reps must be a positive integer.", call. = FALSE)
  }

  kept <- condition_grid[condition_grid$kept, , drop = FALSE]
  if (nrow(kept) == 0L) {
    stop("Signed-feedback admissibility filter removed all scenarios.", call. = FALSE)
  }

  rows <- vector("list", nrow(kept) * reps)
  out_idx <- 1L
  for (row_idx in seq_len(nrow(kept))) {
    row <- kept[row_idx, , drop = FALSE]
    n_factors <- as.integer(row$scenario_nfac[[1]])
    eta_label <- signed_feedback_label(row$eta_scalar[[1]], "eta")
    zeta_label <- signed_feedback_label(row$zeta_scalar[[1]], "zeta")
    err_label <- signed_feedback_error_label(row$Error[[1]])
    load_label <- paste0("load", formatC(row$Loadings[[1]] * 100, format = "f", digits = 0))
    q_label <- paste0("q", formatC(row$Q[[1]] * 100, format = "f", digits = 0))

    for (rep_idx in seq_len(reps)) {
      rows[[out_idx]] <- data.frame(
        scenario = paste(
          "signed_feedback",
          row$benchmark_grid[[1]],
          row$feedback_regime[[1]],
          eta_label,
          zeta_label,
          paste0(n_factors, "d"),
          paste0("n", row$N[[1]]),
          paste0("t", row$TimeP[[1]]),
          paste0("p", row$NVAR[[1]]),
          err_label,
          load_label,
          q_label,
          sep = "_"
        ),
        scenario_family = scenario_family,
        scenario_mode = row$dynamic_scenario[[1]],
        scenario_nfac = n_factors,
        Q2 = signed_feedback_q2(n_factors),
        N = as.integer(row$N[[1]]),
        TimeP = as.integer(row$TimeP[[1]]),
        NVAR = as.integer(row$NVAR[[1]]),
        Error = as.numeric(row$Error[[1]]),
        Q = as.numeric(row$Q[[1]]),
        Loadings = as.numeric(row$Loadings[[1]]),
        base_err = as.numeric(row$Error[[1]]),
        rep = rep_idx,
        dt = as.numeric(row$dt[[1]]),
        active_gain = 1.0,
        background_gain = 0.08,
        dwell_min_frac = 1.0,
        dwell_max_frac = 1.0,
        switching = FALSE,
        benchmark_grid = row$benchmark_grid[[1]],
        feedback_regime = row$feedback_regime[[1]],
        eta_strength = row$eta_strength[[1]],
        dynamic_scenario = row$dynamic_scenario[[1]],
        eta_scalar = row$eta_scalar[[1]],
        zeta_scalar = row$zeta_scalar[[1]],
        finite_growth = row$finite_growth[[1]],
        growth_cap = row$growth_cap[[1]],
        growth_horizon = row$growth_horizon[[1]],
        stringsAsFactors = FALSE
      )
      rows[[out_idx]]$eta <- list(rep(row$eta_scalar[[1]], n_factors))
      rows[[out_idx]]$zeta <- list(rep(row$zeta_scalar[[1]], n_factors))
      rows[[out_idx]]$sigma_q <- list(rep(0.25 * (row$Q[[1]] / 0.75), n_factors))
      rows[[out_idx]]$factor_archetypes <- list(paste0("signed_feedback_", seq_len(n_factors)))
      out_idx <- out_idx + 1L
    }
  }

  scenario_grid <- dplyr::bind_rows(rows)
  scenario_grid$seed_base <- ifelse(
    scenario_grid$benchmark_grid == "main",
    610000L,
    710000L
  ) + seq_len(nrow(scenario_grid))
  scenario_grid
}

summarize_signed_feedback_results <- function(results_df) {
  results_df %>%
    dplyr::mutate(success = status == "ok") %>%
    dplyr::group_by(
      benchmark_grid,
      method,
      method_family,
      method_label,
      feedback_regime,
      eta_strength,
      dynamic_scenario,
      eta_scalar,
      zeta_scalar,
      scenario_nfac,
      N,
      TimeP,
      NVAR,
      Error,
      Loadings,
      Q
    ) %>%
    dplyr::summarize(
      rows = dplyr::n(),
      n_ok = sum(success, na.rm = TRUE),
      n_timeout = sum(status == "timeout", na.rm = TRUE),
      n_error = sum(!success & status != "timeout", na.rm = TRUE),
      finite_growth = mean(finite_growth, na.rm = TRUE),
      exact_rate = mean(n_dim_exact[success], na.rm = TRUE),
      assignment_accuracy = mean(assignment_accuracy[success], na.rm = TRUE),
      score_cor_common = mean(score_cor_common[success], na.rm = TRUE),
      score_nrmse_common = mean(score_nrmse_common[success], na.rm = TRUE),
      runtime_sec = mean(runtime_sec, na.rm = TRUE),
      .groups = "drop"
    )
}

select_signed_feedback_pilot_rows <- function(scenario_grid, target_rows) {
  target_rows <- as.integer(target_rows)
  if (!is.finite(target_rows) || is.na(target_rows) || target_rows <= 0L ||
      target_rows >= nrow(scenario_grid)) {
    return(scenario_grid)
  }

  strata_col <- if ("dynamic_scenario" %in% names(scenario_grid)) {
    "dynamic_scenario"
  } else if ("feedback_regime" %in% names(scenario_grid)) {
    "feedback_regime"
  } else {
    "benchmark_grid"
  }
  strata <- split(seq_len(nrow(scenario_grid)), scenario_grid[[strata_col]])
  per_stratum <- ceiling(target_rows / length(strata))
  selected <- unlist(lapply(strata, function(idx) {
    if (length(idx) <= per_stratum) {
      return(idx)
    }
    idx[unique(round(seq(1, length(idx), length.out = per_stratum)))]
  }), use.names = FALSE)
  selected <- sort(unique(selected))
  if (length(selected) > target_rows) {
    selected <- selected[seq_len(target_rows)]
  }
  scenario_grid[selected, , drop = FALSE]
}

write_signed_feedback_metadata <- function(cfg, main_conditions, feasibility_conditions,
                                           run_grid, scenario_grid, out_dir) {
  conn <- file(file.path(out_dir, "signed_feedback_metadata.txt"), open = "wt")
  on.exit(close(conn), add = TRUE)

  cat("run_date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "", file = conn)
  cat("methods: ", paste(cfg$methods, collapse = ","), "\n", sep = "", file = conn)
  cat("run_grid: ", run_grid, "\n", sep = "", file = conn)
  cat("integrator: ", cfg$integrator, "\n", sep = "", file = conn)
  cat("growth_cap: ", unique(c(main_conditions$growth_cap, feasibility_conditions$growth_cap)), "\n", sep = "", file = conn)
  cat("main_candidate_cells: ", nrow(main_conditions), "\n", sep = "", file = conn)
  cat("main_retained_cells: ", sum(main_conditions$kept), "\n", sep = "", file = conn)
  cat("feasibility_candidate_cells: ", nrow(feasibility_conditions), "\n", sep = "", file = conn)
  cat("feasibility_retained_cells: ", sum(feasibility_conditions$kept), "\n", sep = "", file = conn)
  cat("selected_runnable_rows: ", nrow(scenario_grid), "\n", sep = "", file = conn)
  cat("selected_reps: ", max(scenario_grid$rep), "\n\n", sep = "", file = conn)
  cat("pilot_rows_env: ", Sys.getenv("SIGNED_FEEDBACK_PILOT_ROWS", unset = ""), "\n", sep = "", file = conn)

  cat("main_design:\n", file = conn)
  cat("eta: -0.05,-0.15,-0.30\n", file = conn)
  cat("zeta: -0.30,0.00,0.08\n", file = conn)
  cat("nfac: 2,3,4\n", file = conn)
  cat("N: 50,100\n", file = conn)
  cat("TimeP: 50,100\n", file = conn)
  cat("NVAR: 3,4\n", file = conn)
  cat("Error_variance: 0.125^2,0.25^2,0.50^2\n", file = conn)
  cat("Error_sd: 0.125,0.25,0.50\n", file = conn)
  cat("Loadings: 0.4,0.6,0.8\n", file = conn)
  cat("Q: 0.50,0.75,1.00\n", file = conn)
  cat("sigma_q_rule: 0.25 * (Q / 0.75)\n\n", file = conn)

  cat("feasibility_design:\n", file = conn)
  cat("eta: -0.15,-0.30\n", file = conn)
  cat("zeta: -0.30,0.00,0.08\n", file = conn)
  cat("nfac: 2,4\n", file = conn)
  cat("N: 50,100\nTimeP: 100\nNVAR: 3\nError: 0.025\n", file = conn)
  cat("Loadings: 0.6\nQ: 0.75\n", file = conn)
  cat("sigma_q_rule: 0.25 * (Q / 0.75)\n\n", file = conn)

  cat("dt_rule: TimeP=50 -> 0.5; TimeP=100 -> 0.4\n", file = conn)
  cat("growth_rule: exp(max(Re(eigen(A))) * dt * (TimeP - 1)), A=[[0,1],[eta,zeta]]\n\n", file = conn)
  cat("graphical_var_timeout_sec: ", cfg$graphical_var_timeout_sec, "\n", sep = "", file = conn)
  cat("graphical_var_lambda_beta: ", cfg$graphical_var_lambda_beta, "\n", sep = "", file = conn)
  cat("graphical_var_lambda_kappa: ", cfg$graphical_var_lambda_kappa, "\n", sep = "", file = conn)
  cat("graphical_var_nlambda: ", cfg$graphical_var_nlambda, "\n", sep = "", file = conn)
  cat("graphical_var_maxit_in: ", cfg$graphical_var_maxit_in, "\n", sep = "", file = conn)
  cat("graphical_var_maxit_out: ", cfg$graphical_var_maxit_out, "\n", sep = "", file = conn)
  cat("gimme_timeout_sec: ", cfg$gimme_timeout_sec, "\n\n", sep = "", file = conn)

  cat("selected_counts_by_grid_regime:\n", file = conn)
  counts <- as.data.frame(table(scenario_grid$benchmark_grid, scenario_grid$feedback_regime))
  names(counts) <- c("benchmark_grid", "feedback_regime", "rows")
  cat("benchmark_grid,feedback_regime,rows\n", file = conn)
  utils::write.table(counts, file = conn, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
  cat("\npackages:\n", file = conn)
  for (pkg in required_packages_for_methods(cfg$methods)) {
    cat(pkg, ": ", as.character(utils::packageVersion(pkg)), "\n", sep = "", file = conn)
  }
}

main <- function() {
  out_dir <- Sys.getenv("BENCHMARK_OUT_DIR", "benchmark_outputs_signed_feedback_standardized")
  dry_run <- parse_signed_feedback_bool_env("SIGNED_FEEDBACK_DRY_RUN", default = TRUE)
  run_grid <- Sys.getenv("SIGNED_FEEDBACK_RUN_GRID", "feasibility")
  if (!run_grid %in% c("main", "feasibility")) {
    stop("SIGNED_FEEDBACK_RUN_GRID must be 'main' or 'feasibility'.", call. = FALSE)
  }
  growth_cap <- parse_num_env("SIGNED_FEEDBACK_GROWTH_CAP", 5)
  main_reps <- parse_int_env("SIGNED_FEEDBACK_MAIN_REPS", 1L)
  feasibility_reps <- parse_int_env("SIGNED_FEEDBACK_FEASIBILITY_REPS", 1L)
  scenario_family <- Sys.getenv(
    "SIGNED_FEEDBACK_SCENARIO_FAMILY",
    "signed_feedback_standardized"
  )

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  main_conditions <- build_signed_feedback_condition_grid("main", growth_cap = growth_cap)
  feasibility_conditions <- build_signed_feedback_condition_grid("feasibility", growth_cap = growth_cap)
  main_grid <- build_signed_feedback_scenario_grid(
    main_conditions,
    reps = main_reps,
    scenario_family = scenario_family
  )
  feasibility_grid <- build_signed_feedback_scenario_grid(
    feasibility_conditions,
    reps = feasibility_reps,
    scenario_family = scenario_family
  )
  scenario_grid <- if (identical(run_grid, "main")) main_grid else feasibility_grid
  pilot_rows <- parse_int_env("SIGNED_FEEDBACK_PILOT_ROWS", NA_integer_)
  if (is.finite(pilot_rows) && !is.na(pilot_rows) && pilot_rows > 0L) {
    scenario_grid <- select_signed_feedback_pilot_rows(scenario_grid, pilot_rows)
  }

  utils::write.csv(main_conditions, file.path(out_dir, "signed_feedback_main_conditions.csv"), row.names = FALSE)
  utils::write.csv(feasibility_conditions, file.path(out_dir, "signed_feedback_feasibility_conditions.csv"), row.names = FALSE)
  utils::write.csv(flatten_scenario_grid_for_csv(main_grid), file.path(out_dir, "signed_feedback_main_grid.csv"), row.names = FALSE)
  utils::write.csv(flatten_scenario_grid_for_csv(feasibility_grid), file.path(out_dir, "signed_feedback_feasibility_grid.csv"), row.names = FALSE)

  grid_counts <- data.frame(
    grid = c("main_conditions", "main_rows", "feasibility_conditions", "feasibility_rows"),
    rows = c(nrow(main_conditions), nrow(main_grid), nrow(feasibility_conditions), nrow(feasibility_grid)),
    retained_cells = c(sum(main_conditions$kept), NA_integer_, sum(feasibility_conditions$kept), NA_integer_),
    reps = c(NA_integer_, main_reps, NA_integer_, feasibility_reps),
    stringsAsFactors = FALSE
  )
  utils::write.csv(grid_counts, file.path(out_dir, "signed_feedback_grid_counts.csv"), row.names = FALSE)

  if (any(!main_grid$finite_growth <= main_grid$growth_cap) ||
      any(!feasibility_grid$finite_growth <= feasibility_grid$growth_cap)) {
    stop("Signed-feedback grid contains rows above the growth cap.", call. = FALSE)
  }

  cfg <- build_benchmark_config(smoke = TRUE)
  unsupported_methods <- setdiff(cfg$methods, c("dynEGA", "graphicalVAR", "gimme"))
  if (length(unsupported_methods) > 0L) {
    stop(
      "Signed-feedback runner supports DynEGA, GraphicalVAR, and GIMME only. Unsupported methods: ",
      paste(unsupported_methods, collapse = ", "),
      call. = FALSE
    )
  }
  cfg$scenario_grid <- scenario_grid
  cfg$out_dir <- out_dir
  cfg$checkpoint_each <- TRUE
  cfg$progress <- TRUE
  cfg$parallel_workers <- parse_int_env("BENCHMARK_WORKERS", cfg$parallel_workers)
  cfg$graphical_var_workers <- parse_int_env("BENCHMARK_GRAPHICALVAR_WORKERS", cfg$graphical_var_workers)
  cfg$gimme_workers <- parse_int_env("BENCHMARK_GIMME_WORKERS", cfg$gimme_workers)

  write_signed_feedback_metadata(cfg, main_conditions, feasibility_conditions, run_grid, scenario_grid, out_dir)

  if (dry_run) {
    cat("Saved signed-feedback standardized dry-run grids to ", out_dir, "\n", sep = "")
    return(invisible(NULL))
  }

  if (!isTRUE(cfg$resume)) {
    unlink(file.path(out_dir, c(
      "benchmark_results.csv",
      "checkpoint_results.csv",
      "benchmark_results.rds",
      "benchmark_summary.csv",
      "signed_feedback_summary_by_regime.csv"
    )))
  }

  assert_required_packages(cfg$methods)

  results_df <- run_benchmark_suite(cfg)
  summary_df <- summarize_benchmark_results(results_df)
  signed_summary_df <- summarize_signed_feedback_results(results_df)

  utils::write.csv(results_df, file.path(out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(out_dir, "benchmark_summary.csv"), row.names = FALSE)
  utils::write.csv(signed_summary_df, file.path(out_dir, "signed_feedback_summary_by_regime.csv"), row.names = FALSE)

  cat("Saved signed-feedback standardized benchmark outputs to ", out_dir, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
