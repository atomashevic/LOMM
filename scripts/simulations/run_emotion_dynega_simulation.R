#!/usr/bin/env Rscript

## Reset emotion-dynamics simulation, DynEGA phase only.
## This wrapper reuses run_method_simulation.R estimators and scoring while
## locking the old-Rmd-compatible measured-factor grid.

repo_root <- normalizePath(".", mustWork = TRUE)

old_run_flags <- Sys.getenv(c("RUN_METHOD_BENCHMARK", "RUN_METHOD_BENCHMARK_SMOKE"))
on.exit({
  for (flag_name in names(old_run_flags)) {
    if (nzchar(old_run_flags[[flag_name]])) {
      do.call(Sys.setenv, stats::setNames(as.list(old_run_flags[[flag_name]]), flag_name))
    } else {
      Sys.unsetenv(flag_name)
    }
  }
}, add = TRUE)
Sys.setenv(RUN_METHOD_BENCHMARK = "0", RUN_METHOD_BENCHMARK_SMOKE = "0")

source(file.path(repo_root, "scripts", "simulations", "run_method_simulation.R"))

parse_emotion_bool <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, if (default) "1" else "0")
  tolower(trimws(raw)) %in% c("1", "true", "yes", "y")
}

parse_emotion_int <- function(name, default) {
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

q2_for_nfac <- function(nfac) {
  c(`2` = 1.5, `3` = 2.5, `4` = 4.0)[[as.character(nfac)]]
}

scenario_spec <- function(base_name, nfac) {
  factor_rows <- expression_factor_archetypes[seq_len(nfac), , drop = FALSE]

  old_modes <- list(
    old_low_single_stable = list(
      scenario_family = "old_low_single_stable",
      scenario_mode = "old_low_single_stable",
      sigma_scale = 0.50,
      active_gain = 0.35,
      background_gain = 0.05,
      dwell_min_frac = 1.00,
      dwell_max_frac = 1.00,
      switching = FALSE
    ),
    old_high_single_stable = list(
      scenario_family = "old_high_single_stable",
      scenario_mode = "old_high_single_stable",
      sigma_scale = 1.00,
      active_gain = 1.00,
      background_gain = 0.08,
      dwell_min_frac = 1.00,
      dwell_max_frac = 1.00,
      switching = FALSE
    ),
    old_high_switch_stable = list(
      scenario_family = "old_high_switch_stable",
      scenario_mode = "old_high_switch_stable",
      sigma_scale = 1.00,
      active_gain = 1.00,
      background_gain = 0.08,
      dwell_min_frac = 0.20,
      dwell_max_frac = 0.45,
      switching = TRUE
    ),
    old_high_switch_volatile = list(
      scenario_family = "old_high_switch_volatile",
      scenario_mode = "old_high_switch_volatile",
      sigma_scale = 1.25,
      active_gain = 1.25,
      background_gain = 0.08,
      dwell_min_frac = 0.03,
      dwell_max_frac = 0.10,
      switching = TRUE
    )
  )

  if (base_name %in% names(old_modes)) {
    mode <- old_modes[[base_name]]
    return(list(
      scenario = paste0(base_name, "_", nfac, "d"),
      scenario_family = mode$scenario_family,
      scenario_mode = mode$scenario_mode,
      scenario_nfac = as.integer(nfac),
      Q2 = q2_for_nfac(nfac),
      eta = factor_rows$eta,
      zeta = factor_rows$zeta,
      sigma_q = factor_rows$sigma_q * mode$sigma_scale,
      factor_archetypes = factor_rows$factor_archetype,
      active_gain = mode$active_gain,
      background_gain = mode$background_gain,
      dwell_min_frac = mode$dwell_min_frac,
      dwell_max_frac = mode$dwell_max_frac,
      switching = mode$switching
    ))
  }

  ranked_eta <- -0.05 * seq_len(nfac)
  ranked_zeta <- 0.06 + 0.02 * seq_len(nfac)

  if (identical(base_name, "oai_ranked_persistence")) {
    return(list(
      scenario = paste0(base_name, "_", nfac, "d"),
      scenario_family = "oai_ranked_persistence",
      scenario_mode = "oai_ranked_persistence",
      scenario_nfac = as.integer(nfac),
      Q2 = q2_for_nfac(nfac),
      eta = ranked_eta,
      zeta = ranked_zeta,
      sigma_q = rep(0.25, nfac),
      factor_archetypes = paste0("oai_ranked_", seq_len(nfac)),
      active_gain = 1.00,
      background_gain = 0.08,
      dwell_min_frac = 1.00,
      dwell_max_frac = 1.00,
      switching = FALSE
    ))
  }

  if (identical(base_name, "oai_broad_geometry")) {
    return(list(
      scenario = paste0(base_name, "_", nfac, "d"),
      scenario_family = "oai_broad_geometry",
      scenario_mode = "oai_broad_geometry",
      scenario_nfac = as.integer(nfac),
      Q2 = q2_for_nfac(nfac),
      eta = ranked_eta,
      zeta = ranked_zeta,
      sigma_q = rep(0.35, nfac),
      factor_archetypes = paste0("oai_broad_", seq_len(nfac)),
      active_gain = 1.10,
      background_gain = 0.10,
      dwell_min_frac = 0.12,
      dwell_max_frac = 0.30,
      switching = TRUE
    ))
  }

  stop("Unknown reset scenario base: ", base_name, call. = FALSE)
}

build_emotion_scenarios <- function() {
  base_names <- c(
    "old_low_single_stable",
    "old_high_single_stable",
    "old_high_switch_stable",
    "old_high_switch_volatile",
    "oai_ranked_persistence",
    "oai_broad_geometry"
  )
  scenarios <- list()
  idx <- 1L
  for (base_name in base_names) {
    for (nfac in c(2L, 3L, 4L)) {
      scenarios[[idx]] <- scenario_spec(base_name, nfac)
      idx <- idx + 1L
    }
  }
  scenarios
}

make_grid_row <- function(scenario, grid_scope, N, TimeP, NVAR, Error_sd,
                          Loadings, rep_idx, row_id) {
  row <- data.frame(
    scenario = scenario$scenario,
    scenario_family = scenario$scenario_family,
    scenario_mode = scenario$scenario_mode,
    scenario_nfac = as.integer(scenario$scenario_nfac),
    Q2 = scenario$Q2,
    N = as.integer(N),
    TimeP = as.integer(TimeP),
    NVAR = as.integer(NVAR),
    Error = as.numeric(Error_sd),
    Error_sd = as.numeric(Error_sd),
    Error_variance = as.numeric(Error_sd)^2,
    Q = 0.75,
    Loadings = as.numeric(Loadings),
    base_err = as.numeric(Error_sd),
    rep = as.integer(rep_idx),
    benchmark_grid = grid_scope,
    grid_scope = grid_scope,
    stringsAsFactors = FALSE
  )
  row$eta <- list(as.numeric(scenario$eta))
  row$zeta <- list(as.numeric(scenario$zeta))
  row$sigma_q <- list(as.numeric(scenario$sigma_q))
  row$factor_archetypes <- list(as.character(scenario$factor_archetypes))
  row$active_gain <- as.numeric(scenario$active_gain)
  row$background_gain <- as.numeric(scenario$background_gain)
  row$dwell_min_frac <- as.numeric(scenario$dwell_min_frac)
  row$dwell_max_frac <- as.numeric(scenario$dwell_max_frac)
  row$switching <- isTRUE(scenario$switching)
  row$dt <- ifelse(as.integer(TimeP) == 100L, 0.4, 0.25)
  row$seed_base <- 700000L + as.integer(row_id)
  row
}

build_emotion_dynega_grid <- function(reps = 3L, smoke = FALSE) {
  scenarios <- build_emotion_scenarios()
  if (isTRUE(smoke)) {
    scenarios <- scenarios[vapply(
      scenarios,
      function(x) x$scenario %in% c(
        "old_low_single_stable_2d",
        "old_high_switch_volatile_3d",
        "oai_ranked_persistence_4d"
      ),
      logical(1)
    )]
    reps <- 1L
  }

  rows <- list()
  row_id <- 1L

  add_rows <- function(grid_scope, N_vals, TimeP_vals, NVAR_vals,
                       Error_vals, Loadings_vals, reps_local) {
    for (scenario in scenarios) {
      for (N in N_vals) {
        for (TimeP in TimeP_vals) {
          for (NVAR in NVAR_vals) {
            for (Error_sd in Error_vals) {
              for (Loadings in Loadings_vals) {
                for (rep_idx in seq_len(reps_local)) {
                  rows[[row_id]] <<- make_grid_row(
                    scenario = scenario,
                    grid_scope = grid_scope,
                    N = N,
                    TimeP = TimeP,
                    NVAR = NVAR,
                    Error_sd = Error_sd,
                    Loadings = Loadings,
                    rep_idx = rep_idx,
                    row_id = row_id
                  )
                  row_id <<- row_id + 1L
                }
              }
            }
          }
        }
      }
    }
  }

  if (isTRUE(smoke)) {
    add_rows(
      grid_scope = "smoke",
      N_vals = 50L,
      TimeP_vals = 100L,
      NVAR_vals = 4L,
      Error_vals = c(0.125, 0.50),
      Loadings_vals = c(0.4, 0.8),
      reps_local = 1L
    )
  } else {
    add_rows(
      grid_scope = "core_NVAR4_TimeP100",
      N_vals = c(50L, 100L),
      TimeP_vals = 100L,
      NVAR_vals = 4L,
      Error_vals = c(0.125, 0.25, 0.50),
      Loadings_vals = c(0.4, 0.6, 0.8),
      reps_local = reps
    )
    add_rows(
      grid_scope = "sensitivity_NVAR6",
      N_vals = c(50L, 100L),
      TimeP_vals = 100L,
      NVAR_vals = 6L,
      Error_vals = 0.25,
      Loadings_vals = 0.6,
      reps_local = reps
    )
    add_rows(
      grid_scope = "sensitivity_TimeP300",
      N_vals = 50L,
      TimeP_vals = 300L,
      NVAR_vals = 4L,
      Error_vals = 0.25,
      Loadings_vals = 0.6,
      reps_local = reps
    )
  }

  dplyr::bind_rows(rows)
}

write_old_vs_new_grid_comparison <- function(out_dir, reps) {
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
    new_dynega_phase = c(
      "N = 50, 100",
      "Core TimeP = 100; sensitivity TimeP = 300",
      "Core NVAR = 4; sensitivity NVAR = 6",
      "NFAC = 2, 3, 4",
      "Error_sd = 0.125, 0.25, 0.50; generator uses Error_variance = Error_sd^2",
      "0.4, 0.6, 0.8",
      "DynEGA first; GraphicalVAR and bounded GIMME held for next phases",
      "louvain, leiden, walktrap for DynEGA",
      "Removed from the generator: only measured latent factors are simulated",
      paste0(reps, " per panel for the DynEGA launch"),
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

write_grid_counts <- function(grid, out_dir) {
  grid_counts <- grid %>%
    dplyr::count(grid_scope, name = "panels") %>%
    dplyr::mutate(dynega_rows_with_three_cd = panels * 3L)
  grid_counts <- dplyr::bind_rows(
    grid_counts,
    data.frame(
      grid_scope = "total",
      panels = sum(grid_counts$panels),
      dynega_rows_with_three_cd = sum(grid_counts$dynega_rows_with_three_cd)
    )
  )
  utils::write.csv(grid_counts, file.path(out_dir, "benchmark_grid_counts.csv"), row.names = FALSE)
}

run_emotion_dynega <- function() {
  dry_run <- parse_emotion_bool("EMOTION_DYNEGA_DRY_RUN", default = FALSE)
  smoke <- parse_emotion_bool("EMOTION_DYNEGA_SMOKE", default = FALSE)
  reps <- parse_emotion_int("EMOTION_DYNEGA_REPS", 3L)
  workers <- parse_emotion_int("EMOTION_DYNEGA_WORKERS", 12L)
  out_dir <- Sys.getenv(
    "BENCHMARK_OUT_DIR",
    if (isTRUE(smoke)) {
      file.path("simulation_outputs", "emotion_dynega_smoke")
    } else {
      file.path("simulation_outputs", "emotion_dynega")
    }
  )

  Sys.setenv(
    BENCHMARK_METHODS = "dynEGA",
    BENCHMARK_VARIANT_SMOKE = "1",
    BENCHMARK_DYN_ALGORITHMS = "louvain,leiden,walktrap",
    BENCHMARK_DYN_UNI_METHODS = "expand",
    BENCHMARK_CHECKPOINT_EACH = "1",
    BENCHMARK_RESUME = "1",
    BENCHMARK_WORKERS = as.character(workers)
  )

  cfg <- build_benchmark_config(smoke = FALSE)
  cfg$methods <- "dynEGA"
  cfg$variant_smoke <- TRUE
  cfg$dynEGA_variant_algorithms <- c("louvain", "leiden", "walktrap")
  cfg$dynEGA_variant_uni_methods <- "expand"
  cfg$parallel_workers <- min(max(1L, cfg$available_cores - 1L), workers)
  cfg$checkpoint_each <- TRUE
  cfg$resume <- TRUE
  cfg$out_dir <- out_dir
  cfg$smoke <- smoke
  cfg$smoke_profile <- if (smoke) "emotion_dynega_smoke" else "emotion_dynega_reset"
  cfg$dry_run <- dry_run
  cfg$scenarios <- build_emotion_scenarios()
  cfg$scenario_grid <- build_emotion_dynega_grid(reps = reps, smoke = smoke)

  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    flatten_scenario_grid_for_csv(cfg$scenario_grid),
    file.path(cfg$out_dir, "benchmark_grid_main_full.csv"),
    row.names = FALSE
  )
  write_grid_counts(cfg$scenario_grid, cfg$out_dir)
  write_old_vs_new_grid_comparison(cfg$out_dir, reps = if (smoke) 1L else reps)
  write_metadata(cfg, cfg$out_dir)

  if (isTRUE(dry_run)) {
    cat("Saved reset DynEGA dry-run grid to ", cfg$out_dir, "\n", sep = "")
    return(invisible(NULL))
  }

  assert_required_packages(cfg$methods)

  method_specs <- build_method_specs(cfg)
  results_df <- run_benchmark_pass(cfg, method_specs, cfg$parallel_workers)
  summary_df <- summarize_benchmark_results(results_df)

  utils::write.csv(results_df, file.path(cfg$out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(cfg$out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(cfg$out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(cfg$out_dir, "benchmark_summary.csv"), row.names = FALSE)
  make_overview_plot(summary_df, cfg$out_dir)
  write_metadata(cfg, cfg$out_dir)

  cat("Saved reset DynEGA simulation outputs to ", cfg$out_dir, "\n", sep = "")
}

if (!parse_emotion_bool("EMOTION_BENCHMARK_GRID_ONLY", default = FALSE)) {
  run_emotion_dynega()
}
