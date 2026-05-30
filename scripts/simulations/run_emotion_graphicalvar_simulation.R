#!/usr/bin/env Rscript

## Reset emotion-dynamics simulation, GraphicalVAR phase.
## Reuses the DynEGA reset grid and fits one GraphicalVAR structure per panel,
## then applies louvain, leiden, and walktrap community detection post hoc.

repo_root <- normalizePath(".", mustWork = TRUE)

old_grid_only <- Sys.getenv("EMOTION_BENCHMARK_GRID_ONLY", "")
old_method <- Sys.getenv("BENCHMARK_METHODS", "")
old_graphical_algorithms <- Sys.getenv("BENCHMARK_GRAPHICALVAR_COMMUNITY_ALGORITHMS", "")
old_checkpoint_each <- Sys.getenv("BENCHMARK_CHECKPOINT_EACH", "")
old_resume <- Sys.getenv("BENCHMARK_RESUME", "")
old_workers <- Sys.getenv("BENCHMARK_WORKERS", "")
old_graphical_workers <- Sys.getenv("BENCHMARK_GRAPHICALVAR_WORKERS", "")
old_timeout <- Sys.getenv("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC", "")

on.exit({
  restore_env <- function(name, value) {
    if (nzchar(value)) {
      do.call(Sys.setenv, stats::setNames(as.list(value), name))
    } else {
      Sys.unsetenv(name)
    }
  }
  restore_env("EMOTION_BENCHMARK_GRID_ONLY", old_grid_only)
  restore_env("BENCHMARK_METHODS", old_method)
  restore_env("BENCHMARK_GRAPHICALVAR_COMMUNITY_ALGORITHMS", old_graphical_algorithms)
  restore_env("BENCHMARK_CHECKPOINT_EACH", old_checkpoint_each)
  restore_env("BENCHMARK_RESUME", old_resume)
  restore_env("BENCHMARK_WORKERS", old_workers)
  restore_env("BENCHMARK_GRAPHICALVAR_WORKERS", old_graphical_workers)
  restore_env("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC", old_timeout)
}, add = TRUE)

Sys.setenv(EMOTION_BENCHMARK_GRID_ONLY = "1")
source(file.path(repo_root, "scripts", "simulations", "run_emotion_dynega_simulation.R"))

parse_graphical_bool <- parse_emotion_bool
parse_graphical_int <- parse_emotion_int

parse_graphical_num <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.numeric(default))
  }
  value <- suppressWarnings(as.numeric(raw))
  if (!is.finite(value) || is.na(value)) {
    return(as.numeric(default))
  }
  as.numeric(value)
}

write_graphicalvar_grid_counts <- function(grid, out_dir) {
  grid_counts <- grid %>%
    dplyr::count(grid_scope, name = "structure_fits") %>%
    dplyr::mutate(graphicalvar_rows_with_three_cd = structure_fits * 3L)
  grid_counts <- dplyr::bind_rows(
    grid_counts,
    data.frame(
      grid_scope = "total",
      structure_fits = sum(grid_counts$structure_fits),
      graphicalvar_rows_with_three_cd = sum(grid_counts$graphicalvar_rows_with_three_cd)
    )
  )
  utils::write.csv(grid_counts, file.path(out_dir, "benchmark_grid_counts.csv"), row.names = FALSE)
}

write_graphicalvar_old_vs_new_grid_comparison <- function(out_dir, reps) {
  comparison <- data.frame(
    aspect = c(
      "Sample size",
      "Time points",
      "Variables per measured factor",
      "Measured factors",
      "Measurement error",
      "Measurement loadings",
      "Methods",
      "Community detection",
      "Neutrality",
      "Replications",
      "Scenario layer"
    ),
    old_rmd_grid = c(
      "N = 50, 100",
      "Not factorial in the Rmd prose; examples use 300 frames",
      "NVAR = 4, 6",
      "NFAC = 2, 3",
      "0.125, 0.25, 0.50; squared in measurement-error covariance",
      "0.4, 0.6, 0.8",
      "DynEGA/EII focus: weighted, unweighted, edge-list EII",
      "Not a three-algorithm method-comparison factor",
      "Neutrality used as baseline/scaling, not an observed block",
      "500 generated matrices per condition in prose",
      "Four DLO emotion scenarios"
    ),
    new_graphicalvar_phase = c(
      "N = 50, 100",
      "Core TimeP = 100; sensitivity TimeP = 300",
      "Core NVAR = 4; sensitivity NVAR = 6",
      "NFAC = 2, 3, 4",
      "Error_sd = 0.125, 0.25, 0.50; generator uses Error_variance = Error_sd^2",
      "0.4, 0.6, 0.8",
      "GraphicalVAR after DynEGA; bounded GIMME held for the next phase",
      "louvain, leiden, walktrap applied post hoc to the same fixedPCC fit",
      "Removed from the generator: only measured latent factors are simulated",
      paste0(reps, " per panel for the GraphicalVAR launch"),
      "Four old translated scenarios plus two OpenAI-results-inspired scenarios"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    comparison,
    file.path(out_dir, "old_vs_new_grid_comparison.csv"),
    row.names = FALSE
  )
}

run_emotion_graphicalvar <- function() {
  dry_run <- parse_graphical_bool("EMOTION_GRAPHICALVAR_DRY_RUN", default = FALSE)
  smoke <- parse_graphical_bool("EMOTION_GRAPHICALVAR_SMOKE", default = FALSE)
  reps <- parse_graphical_int("EMOTION_GRAPHICALVAR_REPS", 3L)
  workers <- parse_graphical_int("EMOTION_GRAPHICALVAR_WORKERS", 4L)
  timeout_sec <- parse_graphical_num("EMOTION_GRAPHICALVAR_TIMEOUT_SEC", 900)
  out_dir <- Sys.getenv(
    "BENCHMARK_OUT_DIR",
    if (isTRUE(smoke)) {
      file.path("simulation_outputs", "emotion_graphicalvar_smoke")
    } else {
      file.path("simulation_outputs", "emotion_graphicalvar")
    }
  )

  Sys.setenv(
    BENCHMARK_METHODS = "graphicalVAR",
    BENCHMARK_GRAPHICALVAR_COMMUNITY_ALGORITHMS = "louvain,leiden,walktrap",
    BENCHMARK_CHECKPOINT_EACH = "1",
    BENCHMARK_RESUME = "1",
    BENCHMARK_WORKERS = as.character(workers),
    BENCHMARK_GRAPHICALVAR_WORKERS = as.character(workers),
    BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC = as.character(timeout_sec)
  )

  cfg <- build_benchmark_config(smoke = FALSE)
  cfg$methods <- "graphicalVAR"
  cfg$graphical_var_community_algorithms <- c("louvain", "leiden", "walktrap")
  cfg$parallel_workers <- min(max(1L, cfg$available_cores - 1L), workers)
  cfg$graphical_var_workers <- cfg$parallel_workers
  cfg$graphical_var_timeout_sec <- timeout_sec
  cfg$checkpoint_each <- TRUE
  cfg$resume <- TRUE
  cfg$out_dir <- out_dir
  cfg$smoke <- smoke
  cfg$smoke_profile <- if (smoke) "emotion_graphicalvar_smoke" else "emotion_graphicalvar_reset"
  cfg$dry_run <- dry_run
  cfg$scenarios <- build_emotion_scenarios()
  cfg$scenario_grid <- build_emotion_dynega_grid(reps = reps, smoke = smoke)

  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    flatten_scenario_grid_for_csv(cfg$scenario_grid),
    file.path(cfg$out_dir, "benchmark_grid_main_full.csv"),
    row.names = FALSE
  )
  write_graphicalvar_grid_counts(cfg$scenario_grid, cfg$out_dir)
  write_graphicalvar_old_vs_new_grid_comparison(cfg$out_dir, reps = if (smoke) 1L else reps)
  write_metadata(cfg, cfg$out_dir)

  if (isTRUE(dry_run)) {
    cat("Saved reset GraphicalVAR dry-run grid to ", cfg$out_dir, "\n", sep = "")
    return(invisible(NULL))
  }

  assert_required_packages(cfg$methods)
  method_specs <- build_method_specs(cfg)
  results_df <- run_benchmark_pass(cfg, method_specs, cfg$graphical_var_workers)
  summary_df <- summarize_benchmark_results(results_df)

  utils::write.csv(results_df, file.path(cfg$out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(cfg$out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(cfg$out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(cfg$out_dir, "benchmark_summary.csv"), row.names = FALSE)
  make_overview_plot(summary_df, cfg$out_dir)
  write_metadata(cfg, cfg$out_dir)

  cat("Saved reset GraphicalVAR simulation outputs to ", cfg$out_dir, "\n", sep = "")
}

run_emotion_graphicalvar()
