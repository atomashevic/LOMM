#!/usr/bin/env Rscript

## Final bounded DynEGA community-detection variants.
## Complements the full DynEGA Louvain/expand run with Leiden/expand and
## Walktrap/expand on the final bounded NVAR = 4 all-method comparator grid.

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

parse_final_chr <- function(name, default) {
  raw <- trimws(Sys.getenv(name, paste(default, collapse = ",")))
  values <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(default)
  }
  values
}

build_final_dynega_bounded_cd_grid <- function(reps = 100L) {
  scenarios <- build_emotion_scenarios()
  scenario_keep <- vapply(scenarios, `[[`, integer(1), "scenario_nfac") %in% c(2L, 3L)
  scenarios <- scenarios[scenario_keep]

  scenario_lookup <- data.frame(
    scenario_idx = seq_along(scenarios),
    scenario = vapply(scenarios, `[[`, character(1), "scenario"),
    scenario_family = vapply(scenarios, `[[`, character(1), "scenario_family"),
    scenario_mode = vapply(scenarios, `[[`, character(1), "scenario_mode"),
    scenario_nfac = vapply(scenarios, `[[`, integer(1), "scenario_nfac"),
    Q2 = vapply(scenarios, `[[`, numeric(1), "Q2"),
    active_gain = vapply(scenarios, `[[`, numeric(1), "active_gain"),
    background_gain = vapply(scenarios, `[[`, numeric(1), "background_gain"),
    dwell_min_frac = vapply(scenarios, `[[`, numeric(1), "dwell_min_frac"),
    dwell_max_frac = vapply(scenarios, `[[`, numeric(1), "dwell_max_frac"),
    switching = vapply(scenarios, function(x) isTRUE(x$switching), logical(1)),
    stringsAsFactors = FALSE
  )
  scenario_eta <- lapply(scenarios, function(x) as.numeric(x$eta))
  scenario_zeta <- lapply(scenarios, function(x) as.numeric(x$zeta))
  scenario_sigma_q <- lapply(scenarios, function(x) as.numeric(x$sigma_q))
  scenario_archetypes <- lapply(scenarios, function(x) as.character(x$factor_archetypes))

  conditions <- expand.grid(
    scenario_idx = seq_along(scenarios),
    N = c(50L, 100L),
    TimeP = 100L,
    NVAR = 4L,
    Error_sd = c(0.125, 0.25, 0.50),
    Loadings = c(0.4, 0.6, 0.8),
    KEEP.OUT.ATTRS = FALSE
  )
  condition_idx <- rep(seq_len(nrow(conditions)), each = reps)
  condition_rows <- conditions[condition_idx, , drop = FALSE]
  rep_idx <- rep(seq_len(reps), times = nrow(conditions))
  scenario_idx <- condition_rows$scenario_idx
  row_id <- seq_len(nrow(condition_rows))

  row <- data.frame(
    scenario = scenario_lookup$scenario[scenario_idx],
    scenario_family = scenario_lookup$scenario_family[scenario_idx],
    scenario_mode = scenario_lookup$scenario_mode[scenario_idx],
    scenario_nfac = as.integer(scenario_lookup$scenario_nfac[scenario_idx]),
    Q2 = scenario_lookup$Q2[scenario_idx],
    N = as.integer(condition_rows$N),
    TimeP = as.integer(condition_rows$TimeP),
    NVAR = as.integer(condition_rows$NVAR),
    Error = as.numeric(condition_rows$Error_sd),
    Error_sd = as.numeric(condition_rows$Error_sd),
    Error_variance = as.numeric(condition_rows$Error_sd)^2,
    Q = 0.75,
    Loadings = as.numeric(condition_rows$Loadings),
    base_err = as.numeric(condition_rows$Error_sd),
    rep = as.integer(rep_idx),
    benchmark_grid = "final_dynega_bounded_nvar4_cd",
    grid_scope = "final_dynega_bounded_nvar4_cd",
    stringsAsFactors = FALSE
  )
  row$eta <- scenario_eta[scenario_idx]
  row$zeta <- scenario_zeta[scenario_idx]
  row$sigma_q <- scenario_sigma_q[scenario_idx]
  row$factor_archetypes <- scenario_archetypes[scenario_idx]
  row$active_gain <- as.numeric(scenario_lookup$active_gain[scenario_idx])
  row$background_gain <- as.numeric(scenario_lookup$background_gain[scenario_idx])
  row$dwell_min_frac <- as.numeric(scenario_lookup$dwell_min_frac[scenario_idx])
  row$dwell_max_frac <- as.numeric(scenario_lookup$dwell_max_frac[scenario_idx])
  row$switching <- scenario_lookup$switching[scenario_idx]
  row$dt <- 0.4
  row$seed_base <- 900000L + as.integer(row_id)
  row
}

write_final_cd_manifest <- function(cfg, out_dir, algorithms) {
  structure_fits <- nrow(cfg$scenario_grid)
  manifest <- data.frame(
    item = c(
      "output_dir",
      "condition_count",
      "replications",
      "structure_fits",
      "community_algorithms",
      "method_rows_with_cd",
      "method",
      "uni_method",
      "workers",
      "dynEGA_internal_ncores",
      "checkpoint_file"
    ),
    value = c(
      out_dir,
      as.character(structure_fits / cfg$reps),
      as.character(cfg$reps),
      as.character(structure_fits),
      paste(algorithms, collapse = ","),
      as.character(structure_fits * length(algorithms)),
      "dynEGA",
      "expand",
      as.character(cfg$parallel_workers),
      as.character(cfg$dynEGA_ncores),
      file.path(out_dir, "checkpoint_results.csv")
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest,
    file.path(out_dir, "final_dynega_bounded_cd_manifest.csv"),
    row.names = FALSE
  )
}

write_final_cd_grid_counts <- function(grid, out_dir, n_algorithms) {
  counts <- grid |>
    dplyr::count(scenario_family, scenario_nfac, name = "structure_fits") |>
    dplyr::mutate(method_rows_with_cd = structure_fits * n_algorithms)
  utils::write.csv(
    counts,
    file.path(out_dir, "benchmark_grid_counts.csv"),
    row.names = FALSE
  )
}

run_final_dynega_bounded_cd <- function() {
  dry_run <- parse_final_bool("FINAL_DYNEGA_CD_DRY_RUN", default = FALSE)
  reps <- parse_final_int("FINAL_DYNEGA_CD_REPS", 100L)
  workers <- parse_final_int("FINAL_DYNEGA_CD_WORKERS", 16L)
  algorithms <- parse_final_chr("FINAL_DYNEGA_CD_ALGORITHMS", c("leiden", "walktrap"))
  invalid <- setdiff(algorithms, c("leiden", "walktrap"))
  if (length(invalid) > 0L) {
    stop(
      "FINAL_DYNEGA_CD_ALGORITHMS may only contain leiden and walktrap for this runner: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }
  out_dir <- Sys.getenv(
    "FINAL_DYNEGA_CD_OUT_DIR",
    file.path("simulation_outputs", "final_simulation_results", "dynega_bounded_nvar4_cd")
  )

  Sys.setenv(
    BENCHMARK_METHODS = "dynEGA",
    BENCHMARK_VARIANT_SMOKE = "1",
    BENCHMARK_DYNEGA_VARIANT_ALGORITHMS = paste(algorithms, collapse = ","),
    BENCHMARK_DYNEGA_VARIANT_UNI_METHODS = "expand",
    BENCHMARK_CHECKPOINT_EACH = "1",
    BENCHMARK_RESUME = "1",
    BENCHMARK_WORKERS = as.character(workers),
    BENCHMARK_OUT_DIR = out_dir
  )

  cfg <- build_benchmark_config(smoke = FALSE)
  cfg$methods <- "dynEGA"
  cfg$variant_smoke <- TRUE
  cfg$dynEGA_variant_algorithms <- algorithms
  cfg$dynEGA_variant_uni_methods <- "expand"
  cfg$dynEGA_ncores <- 1L
  cfg$parallel_workers <- min(max(1L, cfg$available_cores - 1L), workers)
  cfg$checkpoint_each <- TRUE
  cfg$resume <- TRUE
  cfg$out_dir <- out_dir
  cfg$smoke <- FALSE
  cfg$smoke_profile <- "final_dynega_bounded_nvar4_cd"
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
  cfg$scenario_grid <- build_final_dynega_bounded_cd_grid(reps = reps)

  expected_fits <- 6L * 2L * length(cfg$N_vals) * length(cfg$Error_vals) *
    length(cfg$Loadings_vals) * cfg$reps
  if (nrow(cfg$scenario_grid) != expected_fits) {
    stop(
      sprintf(
        "Final DynEGA bounded CD grid has %d rows; expected %d.",
        nrow(cfg$scenario_grid),
        expected_fits
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
  write_final_cd_grid_counts(cfg$scenario_grid, cfg$out_dir, length(algorithms))
  write_final_cd_manifest(cfg, cfg$out_dir, algorithms)
  write_metadata(cfg, cfg$out_dir)

  cat(
    sprintf(
      "Prepared final DynEGA bounded CD grid: %d structure fits x %d community algorithms = %d method rows.\n",
      nrow(cfg$scenario_grid),
      length(algorithms),
      nrow(cfg$scenario_grid) * length(algorithms)
    )
  )
  cat(
    sprintf(
      "Output directory: %s; workers: %d; algorithms: %s.\n",
      cfg$out_dir,
      cfg$parallel_workers,
      paste(algorithms, collapse = ",")
    )
  )

  if (isTRUE(dry_run)) {
    cat("Dry run complete; no DynEGA CD fits were launched.\n")
    return(invisible(NULL))
  }

  assert_required_packages(cfg$methods)
  method_specs <- build_method_specs(cfg)
  method_specs <- Filter(
    function(spec) {
      identical(spec$method_family, "dynEGA") &&
        spec$community_algorithm %in% algorithms &&
        identical(spec$uni_method, "expand")
    },
    method_specs
  )
  if (length(method_specs) != length(algorithms)) {
    stop(
      "Expected one DynEGA method spec per requested algorithm; got ",
      length(method_specs),
      " for algorithms ",
      paste(algorithms, collapse = ", "),
      call. = FALSE
    )
  }

  results_df <- run_benchmark_pass(cfg, method_specs, cfg$parallel_workers)
  summary_df <- summarize_benchmark_results(results_df)

  utils::write.csv(results_df, file.path(cfg$out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(cfg$out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(cfg$out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(cfg$out_dir, "benchmark_summary.csv"), row.names = FALSE)
  make_overview_plot(summary_df, cfg$out_dir)
  write_final_cd_manifest(cfg, cfg$out_dir, algorithms)
  write_metadata(cfg, cfg$out_dir)

  cat("Saved final DynEGA bounded CD outputs to ", cfg$out_dir, "\n", sep = "")
}

run_final_dynega_bounded_cd()
