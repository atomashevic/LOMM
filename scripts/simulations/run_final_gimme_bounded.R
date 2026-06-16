#!/usr/bin/env Rscript

## Final GIMME bounded comparator.
## Matches the final GraphicalVAR bounded NVAR = 4 envelope.
## One gimmeSEM structure fit is shared across louvain, leiden, and walktrap.

repo_root <- normalizePath(".", mustWork = TRUE)

old_flags <- Sys.getenv(c(
  "RUN_METHOD_BENCHMARK",
  "RUN_METHOD_BENCHMARK_SMOKE",
  "EMOTION_BENCHMARK_GRID_ONLY"
))
on.exit({
  for (flag_name in names(old_flags)) {
    if (nzchar(old_flags[[flag_name]])) {
      do.call(Sys.setenv, stats::setNames(as.list(old_flags[[flag_name]]), flag_name))
    } else {
      Sys.unsetenv(flag_name)
    }
  }
}, add = TRUE)

Sys.setenv(
  RUN_METHOD_BENCHMARK = "0",
  RUN_METHOD_BENCHMARK_SMOKE = "0",
  EMOTION_BENCHMARK_GRID_ONLY = "1"
)

source(file.path(repo_root, "scripts", "simulations", "run_emotion_dynega_simulation.R"))

parse_final_bool <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, if (default) "1" else "0")
  tolower(trimws(raw)) %in% c("1", "true", "yes", "y")
}

parse_final_int <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.integer(default))
  }
  value <- suppressWarnings(as.integer(raw))
  if (!is.finite(value) || is.na(value)) {
    return(as.integer(default))
  }
  as.integer(value)
}

parse_final_num <- function(name, default) {
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

assign_final_gimme_stage <- function(grid) {
  grid$gimme_stage <- NA_character_
  grid$gimme_stage_order <- NA_integer_
  grid$gimme_feasibility_tier <- NA_character_

  old <- grepl("^old_", grid$scenario_family)
  oai <- grepl("^oai_", grid$scenario_family)

  set_stage <- function(idx, order, stage, tier) {
    idx <- idx & is.na(grid$gimme_stage)
    grid$gimme_stage[idx] <<- stage
    grid$gimme_stage_order[idx] <<- as.integer(order)
    grid$gimme_feasibility_tier[idx] <<- tier
  }

  set_stage(
    old & grid$N == 50L,
    1L,
    "01_old_N50_2d3d_nvar4",
    "old_N50"
  )
  set_stage(
    oai & grid$N == 50L,
    2L,
    "02_oai_N50_2d3d_nvar4",
    "oai_N50"
  )
  set_stage(
    old & grid$N == 100L,
    3L,
    "03_old_N100_2d3d_nvar4",
    "old_N100"
  )
  set_stage(
    oai & grid$N == 100L,
    4L,
    "04_oai_N100_2d3d_nvar4",
    "oai_N100"
  )

  if (any(is.na(grid$gimme_stage))) {
    stop("Some final GIMME grid rows were not assigned to a stage.", call. = FALSE)
  }

  scenario_priority <- ifelse(old, 0L, 1L)
  grid[order(
    grid$gimme_stage_order,
    scenario_priority,
    grid$scenario,
    grid$N,
    grid$TimeP,
    grid$NVAR,
    grid$Error_sd,
    grid$Loadings,
    grid$rep
  ), , drop = FALSE]
}

build_final_gimme_bounded_grid <- function(reps = 100L) {
  scenarios <- build_emotion_scenarios()
  scenarios <- scenarios[vapply(
    scenarios,
    function(x) as.integer(x$scenario_nfac) %in% c(2L, 3L),
    logical(1)
  )]

  rows <- list()
  row_id <- 1L
  for (scenario in scenarios) {
    for (N in c(50L, 100L)) {
      for (Error_sd in c(0.125, 0.25, 0.50)) {
        for (Loadings in c(0.4, 0.6, 0.8)) {
          for (rep_idx in seq_len(reps)) {
            rows[[row_id]] <- make_grid_row(
              scenario = scenario,
              grid_scope = "final_gimme_bounded_nvar4",
              N = N,
              TimeP = 100L,
              NVAR = 4L,
              Error_sd = Error_sd,
              Loadings = Loadings,
              rep_idx = rep_idx,
              row_id = row_id
            )
            row_id <- row_id + 1L
          }
        }
      }
    }
  }

  assign_final_gimme_stage(dplyr::bind_rows(rows))
}

write_gimme_bounded_grid_counts <- function(grid, out_dir, n_algorithms) {
  grid_counts <- grid |>
    dplyr::count(
      gimme_stage_order,
      gimme_stage,
      gimme_feasibility_tier,
      name = "structure_fits"
    ) |>
    dplyr::arrange(gimme_stage_order) |>
    dplyr::mutate(method_rows_with_cd = structure_fits * n_algorithms)
  grid_counts <- dplyr::bind_rows(
    grid_counts,
    data.frame(
      gimme_stage_order = NA_integer_,
      gimme_stage = "total",
      gimme_feasibility_tier = "total",
      structure_fits = sum(grid_counts$structure_fits),
      method_rows_with_cd = sum(grid_counts$method_rows_with_cd)
    )
  )
  utils::write.csv(grid_counts, file.path(out_dir, "benchmark_grid_counts.csv"), row.names = FALSE)
}

write_gimme_bounded_manifest <- function(cfg, out_dir) {
  structure_fits <- nrow(cfg$scenario_grid)
  n_conditions <- structure_fits / cfg$reps
  method_rows <- structure_fits * length(cfg$gimme_community_algorithms)
  manifest <- data.frame(
    item = c(
      "output_dir",
      "scenario_families",
      "source_dimensions",
      "sample_sizes",
      "time_points",
      "indicators_per_dimension",
      "error_levels",
      "loading_levels",
      "replications",
      "conditions",
      "primary_structure_fits",
      "method_rows_with_cd",
      "community_detection",
      "primary_readout",
      "workers",
      "timeout_sec",
      "stop_crit",
      "checkpoint_file"
    ),
    value = c(
      out_dir,
      paste(unique(cfg$scenario_grid$scenario_family), collapse = ","),
      "2,3",
      "50,100",
      "100",
      "4",
      "0.125,0.25,0.50",
      "0.4,0.6,0.8",
      as.character(cfg$reps),
      as.character(n_conditions),
      as.character(structure_fits),
      as.character(method_rows),
      paste(cfg$gimme_community_algorithms, collapse = ","),
      "louvain",
      as.character(cfg$gimme_workers),
      as.character(cfg$gimme_timeout_sec),
      as.character(cfg$gimme_stop_crit),
      file.path(out_dir, "checkpoint_results.csv")
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest,
    file.path(out_dir, "final_gimme_bounded_manifest.csv"),
    row.names = FALSE
  )
}

run_final_gimme_bounded <- function() {
  dry_run <- parse_final_bool("FINAL_GIMME_BOUNDED_DRY_RUN", default = FALSE)
  reps <- parse_final_int("FINAL_GIMME_BOUNDED_REPS", 100L)
  workers <- parse_final_int("FINAL_GIMME_BOUNDED_WORKERS", 24L)
  timeout_sec <- parse_final_num("FINAL_GIMME_BOUNDED_TIMEOUT_SEC", 900)
  stop_crit <- Sys.getenv("FINAL_GIMME_BOUNDED_STOP_CRIT", "standard")
  out_dir <- Sys.getenv(
    "FINAL_GIMME_BOUNDED_OUT_DIR",
    file.path("simulation_outputs", "final_simulation_results", "gimme_bounded_nvar4")
  )

  Sys.setenv(
    BENCHMARK_METHODS = "gimme",
    BENCHMARK_GIMME_COMMUNITY_ALGORITHMS = "louvain,leiden,walktrap",
    BENCHMARK_CHECKPOINT_EACH = "1",
    BENCHMARK_RESUME = "1",
    BENCHMARK_WORKERS = as.character(workers),
    BENCHMARK_GIMME_WORKERS = as.character(workers),
    BENCHMARK_GIMME_TIMEOUT_SEC = as.character(timeout_sec),
    BENCHMARK_GIMME_STOP_CRIT = stop_crit,
    BENCHMARK_OUT_DIR = out_dir
  )

  cfg <- build_benchmark_config(smoke = FALSE)
  cfg$methods <- "gimme"
  cfg$gimme_community_algorithms <- c("louvain", "leiden", "walktrap")
  cfg$parallel_workers <- min(max(1L, cfg$available_cores - 1L), workers)
  cfg$gimme_workers <- cfg$parallel_workers
  cfg$gimme_timeout_sec <- timeout_sec
  cfg$gimme_stop_crit <- stop_crit
  cfg$gimme_scope <- "final bounded NVAR=4 comparator grid matching GraphicalVAR envelope"
  cfg$checkpoint_each <- TRUE
  cfg$resume <- TRUE
  cfg$out_dir <- out_dir
  cfg$smoke <- FALSE
  cfg$smoke_profile <- "final_gimme_bounded_nvar4"
  cfg$dry_run <- dry_run
  cfg$scenarios <- build_emotion_scenarios()
  cfg$TimeP_vals <- 100L
  cfg$N_vals <- c(50L, 100L)
  cfg$NVAR_vals <- 4L
  cfg$Error_vals <- c(0.125, 0.25, 0.50)
  cfg$base_err_vals <- cfg$Error_vals
  cfg$Q_vals <- 0.75
  cfg$Loadings_vals <- c(0.4, 0.6, 0.8)
  cfg$reps <- reps
  cfg$scenario_grid <- build_final_gimme_bounded_grid(reps = reps)

  expected_rows <- 6L * 2L * length(cfg$N_vals) * length(cfg$Error_vals) *
    length(cfg$Loadings_vals) * cfg$reps
  if (nrow(cfg$scenario_grid) != expected_rows) {
    stop(
      sprintf(
        "Final GIMME bounded grid has %d rows; expected %d.",
        nrow(cfg$scenario_grid),
        expected_rows
      ),
      call. = FALSE
    )
  }

  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    flatten_scenario_grid_for_csv(cfg$scenario_grid),
    file.path(cfg$out_dir, "benchmark_grid_main_full.csv"),
    row.names = FALSE
  )
  write_gimme_bounded_grid_counts(
    cfg$scenario_grid,
    cfg$out_dir,
    length(cfg$gimme_community_algorithms)
  )
  write_gimme_bounded_manifest(cfg, cfg$out_dir)
  write_metadata(cfg, cfg$out_dir)

  cat(
    sprintf(
      "Prepared final GIMME bounded grid: %d structure fits x %d community algorithms = %d method rows.\n",
      nrow(cfg$scenario_grid),
      length(cfg$gimme_community_algorithms),
      nrow(cfg$scenario_grid) * length(cfg$gimme_community_algorithms)
    )
  )
  cat(
    sprintf(
      "Output directory: %s; workers: %d; timeout: %.0f s; stop_crit: %s.\n",
      cfg$out_dir,
      cfg$gimme_workers,
      cfg$gimme_timeout_sec,
      cfg$gimme_stop_crit
    )
  )

  if (isTRUE(dry_run)) {
    cat("Dry run complete; no GIMME fits were launched.\n")
    return(invisible(NULL))
  }

  assert_required_packages(cfg$methods)
  method_specs <- build_method_specs(cfg)
  results_df <- run_benchmark_pass(cfg, method_specs, cfg$gimme_workers)
  summary_df <- summarize_benchmark_results(results_df)

  utils::write.csv(results_df, file.path(cfg$out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(cfg$out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(cfg$out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(cfg$out_dir, "benchmark_summary.csv"), row.names = FALSE)
  make_overview_plot(summary_df, cfg$out_dir)
  write_gimme_bounded_manifest(cfg, cfg$out_dir)
  write_metadata(cfg, cfg$out_dir)

  cat("Saved final GIMME bounded outputs to ", cfg$out_dir, "\n", sep = "")
}

run_final_gimme_bounded()
