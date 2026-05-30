#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

if (!nzchar(Sys.getenv("BENCHMARK_METHODS"))) {
  Sys.setenv(BENCHMARK_METHODS = "graphicalVAR")
}
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = "benchmark_outputs_signed_feedback_graphicalvar_pilot")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_SCENARIO_FAMILY"))) {
  Sys.setenv(SIGNED_ZETA_SCENARIO_FAMILY = "signed_feedback_graphicalvar_pilot")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_USE_FIXED_GRAPHICALVAR"))) {
  Sys.setenv(SIGNED_ZETA_USE_FIXED_GRAPHICALVAR = "0")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC = "900")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_WORKERS"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_WORKERS = "2")
}
if (!nzchar(Sys.getenv("BENCHMARK_WORKERS"))) {
  Sys.setenv(BENCHMARK_WORKERS = "1")
}
if (!nzchar(Sys.getenv("BENCHMARK_CHECKPOINT_EACH"))) {
  Sys.setenv(BENCHMARK_CHECKPOINT_EACH = "1")
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
  normalizePath("run_signed_feedback_graphicalvar_pilot.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "run_signed_zeta_smoke_benchmark.R"))

build_graphicalvar_pilot_targets <- function() {
  base <- expand.grid(
    TimeP = c(50L, 100L),
    scenario_nfac = c(2L, 4L),
    feedback_regime = c("damped", "critical", "persistent"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  base$eta_scalar <- -0.15
  base$zeta_scalar <- ifelse(
    base$feedback_regime == "damped",
    -0.30,
    ifelse(
      base$feedback_regime == "critical",
      0.00,
      ifelse(base$TimeP == 50L, 0.12, 0.08)
    )
  )
  base$pilot_cell <- paste(
    paste0("t", base$TimeP),
    paste0(base$scenario_nfac, "d"),
    base$feedback_regime,
    sep = "_"
  )
  base[order(base$TimeP, base$scenario_nfac, base$feedback_regime), ]
}

select_graphicalvar_pilot_grid <- function(scenario_grid, targets) {
  keep <- rep(FALSE, nrow(scenario_grid))
  for (idx in seq_len(nrow(targets))) {
    target <- targets[idx, , drop = FALSE]
    keep <- keep | (
      scenario_grid$TimeP == target$TimeP[[1]] &
        scenario_grid$scenario_nfac == target$scenario_nfac[[1]] &
        scenario_grid$feedback_regime == target$feedback_regime[[1]] &
        abs(scenario_grid$eta_scalar - target$eta_scalar[[1]]) < 1e-12 &
        abs(scenario_grid$zeta_scalar - target$zeta_scalar[[1]]) < 1e-12 &
        scenario_grid$rep == 1L
    )
  }

  out <- scenario_grid[keep, , drop = FALSE]
  if (nrow(out) != nrow(targets)) {
    stop(
      "Pilot grid selection expected ", nrow(targets),
      " rows but found ", nrow(out), ".",
      call. = FALSE
    )
  }
  out$pilot_cell <- targets$pilot_cell[match(
    paste(out$TimeP, out$scenario_nfac, out$feedback_regime),
    paste(targets$TimeP, targets$scenario_nfac, targets$feedback_regime)
  )]
  out
}

main <- function() {
  growth_cap <- parse_num_env("SIGNED_ZETA_GROWTH_CAP", 5)
  scenario_family <- Sys.getenv(
    "SIGNED_ZETA_SCENARIO_FAMILY",
    "signed_feedback_graphicalvar_pilot"
  )
  out_dir <- Sys.getenv(
    "BENCHMARK_OUT_DIR",
    "benchmark_outputs_signed_feedback_graphicalvar_pilot"
  )
  dry_run <- parse_bool_env("SIGNED_ZETA_DRY_RUN", default = FALSE)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  admissibility_grid <- build_signed_zeta_admissibility_grid(growth_cap = growth_cap)
  full_grid <- build_signed_zeta_scenario_grid(
    admissibility_grid,
    reps = 1L,
    scenario_family = scenario_family
  )
  targets <- build_graphicalvar_pilot_targets()
  scenario_grid <- select_graphicalvar_pilot_grid(full_grid, targets)

  utils::write.csv(
    admissibility_grid,
    file.path(out_dir, "signed_zeta_admissibility_grid.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    targets,
    file.path(out_dir, "signed_zeta_graphicalvar_pilot_targets.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    flatten_scenario_grid_for_csv(scenario_grid),
    file.path(out_dir, "signed_zeta_benchmark_grid.csv"),
    row.names = FALSE
  )
  grid_counts <- data.frame(
    scope = c("candidate_cells", "unique_runnable_cells", "pilot_rows"),
    rows = c(nrow(admissibility_grid), sum(admissibility_grid$kept), nrow(scenario_grid)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(grid_counts, file.path(out_dir, "signed_zeta_grid_counts.csv"), row.names = FALSE)

  if (any(!scenario_grid$finite_growth <= scenario_grid$growth_cap)) {
    stop("Pilot grid contains rows above the growth cap.", call. = FALSE)
  }

  cfg <- build_benchmark_config(smoke = TRUE)
  if (!setequal(cfg$methods, "graphicalVAR")) {
    stop("GraphicalVAR pilot requires BENCHMARK_METHODS=graphicalVAR.", call. = FALSE)
  }
  cfg$scenario_grid <- scenario_grid
  cfg$out_dir <- out_dir
  cfg$checkpoint_each <- TRUE
  cfg$progress <- TRUE
  cfg$parallel_workers <- parse_int_env("BENCHMARK_WORKERS", cfg$parallel_workers)
  cfg$graphical_var_workers <- parse_int_env(
    "BENCHMARK_GRAPHICALVAR_WORKERS",
    cfg$graphical_var_workers
  )

  write_signed_zeta_metadata(cfg, admissibility_grid, scenario_grid, out_dir)

  if (dry_run) {
    cat("Saved signed-feedback GraphicalVAR pilot dry-run grids to ", out_dir, "\n", sep = "")
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

  cat("Saved signed-feedback GraphicalVAR pilot outputs to ", out_dir, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
