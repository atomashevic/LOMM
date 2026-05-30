#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

parse_signed_zeta_bool_env <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  tolower(value) %in% c("1", "true", "yes", "y")
}

if (!nzchar(Sys.getenv("BENCHMARK_METHODS"))) {
  Sys.setenv(BENCHMARK_METHODS = "dynEGA,graphicalVAR")
}
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = "benchmark_outputs_signed_zeta_smoke")
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
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC = "90")
}
use_fixed_graphicalvar <- parse_signed_zeta_bool_env("SIGNED_ZETA_USE_FIXED_GRAPHICALVAR", default = TRUE)
if (isTRUE(use_fixed_graphicalvar)) {
  if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_LAMBDA_BETA"))) {
    Sys.setenv(BENCHMARK_GRAPHICALVAR_LAMBDA_BETA = "0.1")
  }
  if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_LAMBDA_KAPPA"))) {
    Sys.setenv(BENCHMARK_GRAPHICALVAR_LAMBDA_KAPPA = "0.1")
  }
  if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_MAXIT_IN"))) {
    Sys.setenv(BENCHMARK_GRAPHICALVAR_MAXIT_IN = "25")
  }
  if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_MAXIT_OUT"))) {
    Sys.setenv(BENCHMARK_GRAPHICALVAR_MAXIT_OUT = "25")
  }
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
  normalizePath("run_signed_zeta_smoke_benchmark.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "run_method_benchmark.R"))

signed_feedback_real_max <- function(eta, zeta) {
  A <- matrix(c(0, 1, eta, zeta), nrow = 2, byrow = TRUE)
  max(Re(eigen(A)$values))
}

build_signed_zeta_admissibility_grid <- function(growth_cap = 5) {
  eta_vals <- c(-0.05, -0.15, -0.30, -0.60)
  zeta_vals <- c(-0.60, -0.30, -0.10, 0.00, 0.08, 0.12, 0.20)
  nfac_vals <- c(2L, 4L)
  timep_vals <- c(50L, 100L)

  grid <- expand.grid(
    eta_scalar = eta_vals,
    zeta_scalar = zeta_vals,
    scenario_nfac = nfac_vals,
    TimeP = timep_vals,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid$dt <- ifelse(grid$TimeP == 50L, 0.5, 0.4)
  grid$growth_horizon <- grid$dt * (grid$TimeP - 1L)
  grid$growth_cap <- growth_cap
  grid$real_max <- mapply(
    signed_feedback_real_max,
    grid$eta_scalar,
    grid$zeta_scalar
  )
  grid$finite_growth <- exp(grid$real_max * grid$dt * (grid$TimeP - 1L))
  grid$feedback_regime <- ifelse(
    grid$zeta_scalar < 0,
    "damped",
    ifelse(grid$zeta_scalar == 0, "critical", "persistent")
  )
  grid$kept <- grid$finite_growth <= growth_cap
  grid$exclusion_reason <- ifelse(
    grid$kept,
    "",
    sprintf("finite_growth %.3f exceeds cap %.3f", grid$finite_growth, growth_cap)
  )
  grid[order(grid$TimeP, grid$scenario_nfac, grid$zeta_scalar, grid$eta_scalar), ]
}

build_signed_zeta_scenario_grid <- function(admissibility_grid, reps = 1L,
                                            scenario_family = "signed_zeta_smoke") {
  reps <- as.integer(reps)
  if (!is.finite(reps) || is.na(reps) || reps < 1L) {
    stop("Signed-zeta reps must be a positive integer.", call. = FALSE)
  }

  kept <- admissibility_grid[admissibility_grid$kept, , drop = FALSE]
  if (nrow(kept) == 0L) {
    stop("Signed-zeta admissibility filter removed all scenarios.", call. = FALSE)
  }

  rows <- vector("list", nrow(kept) * reps)
  out_idx <- 1L
  for (row_idx in seq_len(nrow(kept))) {
    row <- kept[row_idx, , drop = FALSE]
    n_factors <- as.integer(row$scenario_nfac[[1]])
    zeta_label <- if (row$zeta_scalar[[1]] < 0) {
      paste0("neg", formatC(abs(row$zeta_scalar[[1]]) * 100, format = "f", digits = 0))
    } else if (row$zeta_scalar[[1]] == 0) {
      "zero"
    } else {
      paste0("pos", formatC(row$zeta_scalar[[1]] * 100, format = "f", digits = 0))
    }
    eta_label <- paste0("eta", formatC(abs(row$eta_scalar[[1]]) * 100, format = "f", digits = 0))

    for (rep_idx in seq_len(reps)) {
      rows[[out_idx]] <- data.frame(
        scenario = paste(
          "signed_zeta",
          row$feedback_regime[[1]],
          eta_label,
          zeta_label,
          paste0(n_factors, "d"),
          paste0("t", row$TimeP[[1]]),
          sep = "_"
        ),
        scenario_family = scenario_family,
        scenario_mode = row$feedback_regime[[1]],
        scenario_nfac = n_factors,
        Q2 = ifelse(n_factors == 2L, 1.5, 4.0),
        N = 16L,
        TimeP = as.integer(row$TimeP[[1]]),
        NVAR = 3L,
        Error = 0.025,
        Q = 0.75,
        Loadings = 0.6,
        base_err = 0.025,
        rep = rep_idx,
        dt = as.numeric(row$dt[[1]]),
        active_gain = 1.0,
        background_gain = 0.08,
        dwell_min_frac = 1.0,
        dwell_max_frac = 1.0,
        switching = FALSE,
        feedback_regime = row$feedback_regime[[1]],
        eta_scalar = row$eta_scalar[[1]],
        zeta_scalar = row$zeta_scalar[[1]],
        finite_growth = row$finite_growth[[1]],
        growth_cap = row$growth_cap[[1]],
        growth_horizon = row$growth_horizon[[1]],
        stringsAsFactors = FALSE
      )
      rows[[out_idx]]$eta <- list(rep(row$eta_scalar[[1]], n_factors))
      rows[[out_idx]]$zeta <- list(rep(row$zeta_scalar[[1]], n_factors))
      rows[[out_idx]]$sigma_q <- list(rep(0.25, n_factors))
      rows[[out_idx]]$factor_archetypes <- list(paste0("signed_feedback_", seq_len(n_factors)))
      out_idx <- out_idx + 1L
    }
  }

  scenario_grid <- dplyr::bind_rows(rows)
  scenario_grid$seed_base <- 510000L + seq_len(nrow(scenario_grid))
  scenario_grid
}

summarize_signed_zeta_results <- function(results_df) {
  results_df %>%
    dplyr::mutate(success = status == "ok") %>%
    dplyr::group_by(
      method,
      method_family,
      method_label,
      feedback_regime,
      eta_scalar,
      zeta_scalar,
      scenario_nfac,
      TimeP
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

write_signed_zeta_metadata <- function(cfg, admissibility_grid, scenario_grid, out_dir) {
  conn <- file(file.path(out_dir, "signed_zeta_metadata.txt"), open = "wt")
  on.exit(close(conn), add = TRUE)

  cat("run_date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "", file = conn)
  cat("methods: ", paste(cfg$methods, collapse = ","), "\n", sep = "", file = conn)
  cat("integrator: ", cfg$integrator, "\n", sep = "", file = conn)
  cat("growth_cap: ", unique(scenario_grid$growth_cap), "\n", sep = "", file = conn)
  cat("candidate_cells: ", nrow(admissibility_grid), "\n", sep = "", file = conn)
  cat("unique_runnable_cells: ", sum(admissibility_grid$kept), "\n", sep = "", file = conn)
  cat("runnable_rows: ", nrow(scenario_grid), "\n", sep = "", file = conn)
  cat("N: 16\nNVAR: 3\nError: 0.025\nLoadings: 0.6\nQ: 0.75\nreps: ", max(scenario_grid$rep), "\n", sep = "", file = conn)
  cat("TimeP_vals: ", paste(sort(unique(scenario_grid$TimeP)), collapse = ","), "\n", sep = "", file = conn)
  cat("nfac_vals: ", paste(sort(unique(scenario_grid$scenario_nfac)), collapse = ","), "\n", sep = "", file = conn)
  cat("eta_vals: ", paste(sort(unique(admissibility_grid$eta_scalar)), collapse = ","), "\n", sep = "", file = conn)
  cat("zeta_vals: ", paste(sort(unique(admissibility_grid$zeta_scalar)), collapse = ","), "\n", sep = "", file = conn)
  cat("dt_rule: TimeP=50 -> 0.5; TimeP=100 -> 0.4\n", file = conn)
  cat("growth_rule: exp(max(Re(eigen(A))) * dt * (TimeP - 1)), A=[[0,1],[eta,zeta]]\n\n", file = conn)
  cat("graphical_var_timeout_sec: ", cfg$graphical_var_timeout_sec, "\n", sep = "", file = conn)
  cat("graphical_var_lambda_beta: ", cfg$graphical_var_lambda_beta, "\n", sep = "", file = conn)
  cat("graphical_var_lambda_kappa: ", cfg$graphical_var_lambda_kappa, "\n", sep = "", file = conn)
  cat("graphical_var_maxit_in: ", cfg$graphical_var_maxit_in, "\n", sep = "", file = conn)
  cat("graphical_var_maxit_out: ", cfg$graphical_var_maxit_out, "\n\n", sep = "", file = conn)

  cat("runnable_counts_by_regime:\n", file = conn)
  counts <- as.data.frame(table(scenario_grid$feedback_regime, scenario_grid$TimeP))
  names(counts) <- c("feedback_regime", "TimeP", "rows")
  cat("feedback_regime,TimeP,rows\n", file = conn)
  utils::write.table(
    counts,
    file = conn,
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    append = TRUE
  )
  cat("\npackages:\n", file = conn)
  for (pkg in required_packages_for_methods(cfg$methods)) {
    cat(pkg, ": ", as.character(utils::packageVersion(pkg)), "\n", sep = "", file = conn)
  }
}

main <- function() {
  growth_cap <- parse_num_env("SIGNED_ZETA_GROWTH_CAP", 5)
  reps <- parse_int_env("SIGNED_ZETA_REPS", 1L)
  scenario_family <- Sys.getenv("SIGNED_ZETA_SCENARIO_FAMILY", "signed_zeta_smoke")
  out_dir <- Sys.getenv("BENCHMARK_OUT_DIR", "benchmark_outputs_signed_zeta_smoke")
  dry_run <- parse_bool_env("SIGNED_ZETA_DRY_RUN", default = FALSE)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  admissibility_grid <- build_signed_zeta_admissibility_grid(growth_cap = growth_cap)
  scenario_grid <- build_signed_zeta_scenario_grid(
    admissibility_grid,
    reps = reps,
    scenario_family = scenario_family
  )

  utils::write.csv(
    admissibility_grid,
    file.path(out_dir, "signed_zeta_admissibility_grid.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    flatten_scenario_grid_for_csv(scenario_grid),
    file.path(out_dir, "signed_zeta_benchmark_grid.csv"),
    row.names = FALSE
  )

  grid_counts <- data.frame(
    scope = c("candidate_cells", "unique_runnable_cells", "runnable_rows"),
    rows = c(nrow(admissibility_grid), sum(admissibility_grid$kept), nrow(scenario_grid)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(grid_counts, file.path(out_dir, "signed_zeta_grid_counts.csv"), row.names = FALSE)

  if (any(!scenario_grid$finite_growth <= scenario_grid$growth_cap)) {
    stop("Runnable signed-zeta grid contains rows above the growth cap.", call. = FALSE)
  }

  cfg <- build_benchmark_config(smoke = TRUE)
  unsupported_methods <- setdiff(cfg$methods, c("dynEGA", "graphicalVAR"))
  if (length(unsupported_methods) > 0L) {
    stop(
      "Signed-zeta runner supports only DynEGA and GraphicalVAR. Unsupported methods: ",
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

  write_signed_zeta_metadata(cfg, admissibility_grid, scenario_grid, out_dir)

  if (dry_run) {
    cat("Saved signed-zeta dry-run grids to ", out_dir, "\n", sep = "")
    return(invisible(NULL))
  }

  if (!isTRUE(cfg$resume)) {
    unlink(file.path(out_dir, c(
      "benchmark_results.csv",
      "checkpoint_results.csv",
      "benchmark_results.rds",
      "benchmark_summary.csv",
      "signed_zeta_summary_by_regime.csv"
    )))
  }

  assert_required_packages(cfg$methods)

  results_df <- run_benchmark_suite(cfg)
  summary_df <- summarize_benchmark_results(results_df)
  signed_summary_df <- summarize_signed_zeta_results(results_df)

  utils::write.csv(results_df, file.path(out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(out_dir, "benchmark_summary.csv"), row.names = FALSE)
  utils::write.csv(signed_summary_df, file.path(out_dir, "signed_zeta_summary_by_regime.csv"), row.names = FALSE)

  cat("Saved signed-zeta smoke benchmark outputs to ", out_dir, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
