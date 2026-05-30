#!/usr/bin/env Rscript

## Reset emotion-dynamics simulation, bounded GIMME phase.
## Reuses the DynEGA reset grid, but runs it as an ordered feasibility ladder.
## One gimmeSEM structure fit is shared across louvain, leiden, and walktrap.

repo_root <- normalizePath(".", mustWork = TRUE)

old_grid_only <- Sys.getenv("EMOTION_BENCHMARK_GRID_ONLY", "")
old_method <- Sys.getenv("BENCHMARK_METHODS", "")
old_gimme_algorithms <- Sys.getenv("BENCHMARK_GIMME_COMMUNITY_ALGORITHMS", "")
old_checkpoint_each <- Sys.getenv("BENCHMARK_CHECKPOINT_EACH", "")
old_resume <- Sys.getenv("BENCHMARK_RESUME", "")
old_workers <- Sys.getenv("BENCHMARK_WORKERS", "")
old_gimme_workers <- Sys.getenv("BENCHMARK_GIMME_WORKERS", "")
old_timeout <- Sys.getenv("BENCHMARK_GIMME_TIMEOUT_SEC", "")
old_stop_crit <- Sys.getenv("BENCHMARK_GIMME_STOP_CRIT", "")

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
  restore_env("BENCHMARK_GIMME_COMMUNITY_ALGORITHMS", old_gimme_algorithms)
  restore_env("BENCHMARK_CHECKPOINT_EACH", old_checkpoint_each)
  restore_env("BENCHMARK_RESUME", old_resume)
  restore_env("BENCHMARK_WORKERS", old_workers)
  restore_env("BENCHMARK_GIMME_WORKERS", old_gimme_workers)
  restore_env("BENCHMARK_GIMME_TIMEOUT_SEC", old_timeout)
  restore_env("BENCHMARK_GIMME_STOP_CRIT", old_stop_crit)
}, add = TRUE)

Sys.setenv(EMOTION_BENCHMARK_GRID_ONLY = "1")
source(file.path(repo_root, "scripts", "simulations", "run_emotion_dynega_simulation.R"))

parse_gimme_bool <- parse_emotion_bool
parse_gimme_int <- parse_emotion_int

parse_gimme_num <- function(name, default) {
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

assign_gimme_stage <- function(grid) {
  grid$gimme_stage <- NA_character_
  grid$gimme_stage_order <- NA_integer_
  grid$gimme_feasibility_tier <- NA_character_

  set_stage <- function(idx, order, stage, tier) {
    idx <- idx & is.na(grid$gimme_stage)
    grid$gimme_stage[idx] <<- stage
    grid$gimme_stage_order[idx] <<- as.integer(order)
    grid$gimme_feasibility_tier[idx] <<- tier
  }

  core <- grid$grid_scope == "core_NVAR4_TimeP100"
  old <- grepl("^old_", grid$scenario_family)
  oai <- grepl("^oai_", grid$scenario_family)
  mid_signal <- abs(grid$Error_sd - 0.25) < 1e-12 & abs(grid$Loadings - 0.6) < 1e-12

  set_stage(
    core & grid$N == 50L & grid$scenario_nfac %in% c(2L, 3L) & mid_signal,
    1L,
    "01_gate_core_N50_2d3d_mid_signal",
    "gate"
  )
  set_stage(
    core & old & grid$N == 50L & grid$scenario_nfac %in% c(2L, 3L),
    2L,
    "02_old_core_N50_2d3d",
    "primary_feasible"
  )
  set_stage(
    core & oai & grid$N == 50L & grid$scenario_nfac %in% c(2L, 3L),
    3L,
    "03_oai_core_N50_2d3d",
    "primary_feasible"
  )
  set_stage(
    core & old & grid$N == 50L & grid$scenario_nfac == 4L,
    4L,
    "04_old_core_N50_4d",
    "stress_4d"
  )
  set_stage(
    core & old & grid$N == 100L & grid$scenario_nfac %in% c(2L, 3L),
    5L,
    "05_old_core_N100_2d3d",
    "primary_expansion"
  )
  set_stage(
    core & oai & grid$N == 100L & grid$scenario_nfac %in% c(2L, 3L),
    6L,
    "06_oai_core_N100_2d3d",
    "primary_expansion"
  )
  set_stage(
    core & old & grid$N == 100L & grid$scenario_nfac == 4L,
    7L,
    "07_old_core_N100_4d",
    "stress_4d"
  )
  set_stage(
    core & oai & grid$N == 50L & grid$scenario_nfac == 4L & mid_signal,
    8L,
    "08_oai_core_N50_4d_probe",
    "oai_4d_probe"
  )
  set_stage(
    core & oai & grid$N == 50L & grid$scenario_nfac == 4L,
    9L,
    "09_oai_core_N50_4d_remaining",
    "oai_4d_stress"
  )
  set_stage(
    core & oai & grid$N == 100L & grid$scenario_nfac == 4L,
    10L,
    "10_oai_core_N100_4d",
    "oai_4d_stress"
  )
  set_stage(
    grid$grid_scope == "sensitivity_NVAR6" & grid$scenario_nfac %in% c(2L, 3L),
    11L,
    "11_sensitivity_NVAR6_2d3d",
    "sensitivity"
  )
  set_stage(
    grid$grid_scope == "sensitivity_NVAR6" & grid$scenario_nfac == 4L,
    12L,
    "12_sensitivity_NVAR6_4d",
    "sensitivity_4d"
  )
  set_stage(
    grid$grid_scope == "sensitivity_TimeP300" & grid$scenario_nfac %in% c(2L, 3L),
    13L,
    "13_sensitivity_TimeP300_2d3d",
    "sensitivity"
  )
  set_stage(
    grid$grid_scope == "sensitivity_TimeP300" & grid$scenario_nfac == 4L,
    14L,
    "14_sensitivity_TimeP300_4d",
    "sensitivity_4d"
  )

  if (any(is.na(grid$gimme_stage))) {
    stop("Some reset GIMME grid rows were not assigned to a feasibility stage.", call. = FALSE)
  }

  scenario_priority <- ifelse(grepl("^old_", grid$scenario_family), 0L, 1L)
  grid[order(grid$gimme_stage_order, scenario_priority, grid$scenario, grid$N, grid$TimeP,
             grid$NVAR, grid$Error_sd, grid$Loadings, grid$rep), , drop = FALSE]
}

build_emotion_gimme_grid <- function(reps = 3L, smoke = FALSE) {
  if (isTRUE(smoke)) {
    grid <- build_emotion_dynega_grid(reps = 1L, smoke = FALSE)
    grid <- grid[
      grid$scenario %in% c(
        "old_low_single_stable_2d",
        "old_high_switch_volatile_3d",
        "oai_ranked_persistence_2d"
      ) &
        grid$N == 50L &
        grid$TimeP == 100L &
        grid$NVAR == 4L &
        abs(grid$Error_sd - 0.25) < 1e-12 &
        abs(grid$Loadings - 0.6) < 1e-12,
      ,
      drop = FALSE
    ]
  } else {
    grid <- build_emotion_dynega_grid(reps = reps, smoke = FALSE)
  }
  assign_gimme_stage(grid)
}

write_gimme_grid_counts <- function(grid, out_dir) {
  grid_counts <- grid %>%
    dplyr::count(gimme_stage_order, gimme_stage, gimme_feasibility_tier, name = "structure_fits") %>%
    dplyr::arrange(gimme_stage_order) %>%
    dplyr::mutate(gimme_rows_with_three_cd = structure_fits * 3L)
  grid_counts <- dplyr::bind_rows(
    grid_counts,
    data.frame(
      gimme_stage_order = NA_integer_,
      gimme_stage = "total",
      gimme_feasibility_tier = "total",
      structure_fits = sum(grid_counts$structure_fits),
      gimme_rows_with_three_cd = sum(grid_counts$gimme_rows_with_three_cd)
    )
  )
  utils::write.csv(grid_counts, file.path(out_dir, "benchmark_grid_counts.csv"), row.names = FALSE)
}

write_gimme_old_vs_new_grid_comparison <- function(out_dir, reps, stop_crit) {
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
      "Scenario layer",
      "GIMME feasibility rule"
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
      "Four DLO emotion scenarios",
      "No bounded GIMME comparator in the old Rmd"
    ),
    new_gimme_phase = c(
      "N = 50, 100, reached stage-wise only if feasible",
      "Core TimeP = 100 first; TimeP = 300 sensitivity only after core stages",
      "Core NVAR = 4 first; NVAR = 6 sensitivity only after core stages",
      "NFAC = 2, 3 before 4D stress stages",
      "Error_sd = 0.125, 0.25, 0.50; generator uses Error_variance = Error_sd^2",
      "0.4, 0.6, 0.8",
      "Bounded GIMME after DynEGA and GraphicalVAR",
      "louvain, leiden, walktrap applied post hoc to one gimmeSEM structure",
      "Removed from the generator: only measured latent factors are simulated",
      paste0(reps, " per panel for the GIMME ladder"),
      "Four old translated scenarios plus two OpenAI-results-inspired scenarios",
      paste0("Stage-wise ladder; stop_crit=", stop_crit, "; stop after timeout-heavy stage")
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    comparison,
    file.path(out_dir, "old_vs_new_grid_comparison.csv"),
    row.names = FALSE
  )
}

read_checkpoint_if_exists <- function(out_dir) {
  path <- file.path(out_dir, "checkpoint_results.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

summarize_stage_status <- function(checkpoint_df, stage_name) {
  if (is.null(checkpoint_df) || nrow(checkpoint_df) == 0L || !"gimme_stage" %in% names(checkpoint_df)) {
    return(data.frame())
  }
  baseline <- checkpoint_df[
    checkpoint_df$gimme_stage == stage_name & checkpoint_df$method == "gimme",
    ,
    drop = FALSE
  ]
  if (nrow(baseline) == 0L) {
    return(data.frame())
  }
  data.frame(
    gimme_stage = stage_name,
    structure_fits = nrow(baseline),
    ok = sum(baseline$status == "ok"),
    timeout = sum(baseline$status == "timeout"),
    error = sum(!(baseline$status %in% c("ok", "timeout"))),
    non_ok_rate = mean(baseline$status != "ok"),
    max_runtime_sec = max(baseline$runtime_sec, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

write_partial_gimme_outputs <- function(cfg) {
  checkpoint <- read_checkpoint_if_exists(cfg$out_dir)
  if (is.null(checkpoint) || nrow(checkpoint) == 0L) {
    return(invisible(NULL))
  }

  utils::write.csv(
    checkpoint,
    file.path(cfg$out_dir, "benchmark_results_partial.csv"),
    row.names = FALSE
  )

  status_by_stage <- checkpoint %>%
    dplyr::count(gimme_stage_order, gimme_stage, gimme_feasibility_tier, status, name = "rows") %>%
    dplyr::arrange(gimme_stage_order, status)
  utils::write.csv(
    status_by_stage,
    file.path(cfg$out_dir, "benchmark_status_by_stage_partial.csv"),
    row.names = FALSE
  )

  ok <- checkpoint[checkpoint$status == "ok", , drop = FALSE]
  if (nrow(ok) > 0L) {
    summary_df <- summarize_benchmark_results(ok)
    utils::write.csv(
      summary_df,
      file.path(cfg$out_dir, "benchmark_summary_partial_ok_only.csv"),
      row.names = FALSE
    )
  }

  lines <- c(
    "# GIMME partial feasibility summary",
    "",
    sprintf("Updated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Rows recorded: %d", nrow(checkpoint)),
    paste(capture.output(print(table(checkpoint$status, useNA = "ifany"))), collapse = "\n"),
    "",
    "GIMME rows are three community-detection variants per structure fit; use method == 'gimme' rows for fit-level timeout accounting."
  )
  writeLines(lines, file.path(cfg$out_dir, "partial_feasibility_summary.md"))
  invisible(NULL)
}

run_emotion_gimme <- function() {
  dry_run <- parse_gimme_bool("EMOTION_GIMME_DRY_RUN", default = FALSE)
  smoke <- parse_gimme_bool("EMOTION_GIMME_SMOKE", default = FALSE)
  reps <- parse_gimme_int("EMOTION_GIMME_REPS", 3L)
  workers <- parse_gimme_int("EMOTION_GIMME_WORKERS", 1L)
  timeout_sec <- parse_gimme_num("EMOTION_GIMME_TIMEOUT_SEC", 900)
  max_non_ok_rate <- parse_gimme_num("EMOTION_GIMME_STAGE_MAX_NON_OK_RATE", 0.50)
  min_stage_fits <- parse_gimme_int("EMOTION_GIMME_STAGE_MIN_FITS_FOR_STOP", 2L)
  stage_chunk_size <- parse_gimme_int("EMOTION_GIMME_STAGE_CHUNK_SIZE", if (smoke) 1L else 2L)
  stage_chunk_size <- max(1L, stage_chunk_size)
  stop_crit <- Sys.getenv("EMOTION_GIMME_STOP_CRIT", "standard")
  out_dir <- Sys.getenv(
    "BENCHMARK_OUT_DIR",
    if (isTRUE(smoke)) {
      file.path("simulation_outputs", "emotion_gimme_smoke")
    } else {
      file.path("simulation_outputs", "emotion_gimme")
    }
  )

  Sys.setenv(
    BENCHMARK_METHODS = "gimme",
    BENCHMARK_GIMME_COMMUNITY_ALGORITHMS = "louvain,leiden,walktrap",
    BENCHMARK_CHECKPOINT_EACH = "1",
    BENCHMARK_RESUME = "1",
    BENCHMARK_WORKERS = as.character(workers),
    BENCHMARK_GIMME_WORKERS = as.character(workers),
    BENCHMARK_GIMME_TIMEOUT_SEC = as.character(timeout_sec),
    BENCHMARK_GIMME_STOP_CRIT = stop_crit
  )

  cfg <- build_benchmark_config(smoke = FALSE)
  cfg$methods <- "gimme"
  cfg$gimme_community_algorithms <- c("louvain", "leiden", "walktrap")
  cfg$parallel_workers <- workers
  cfg$gimme_workers <- workers
  cfg$gimme_timeout_sec <- timeout_sec
  cfg$gimme_stop_crit <- stop_crit
  cfg$gimme_scope <- "reset emotion-dynamics staged feasibility ladder over the DynEGA grid"
  cfg$checkpoint_each <- TRUE
  cfg$resume <- TRUE
  cfg$out_dir <- out_dir
  cfg$smoke <- smoke
  cfg$smoke_profile <- if (smoke) "emotion_gimme_smoke" else "emotion_gimme_feasibility_ladder"
  cfg$dry_run <- dry_run
  cfg$scenarios <- build_emotion_scenarios()
  cfg$scenario_grid <- build_emotion_gimme_grid(reps = reps, smoke = smoke)
  cfg$N_vals <- c(50L, 100L)
  cfg$TimeP_vals <- c(100L, 300L)
  cfg$NVAR_vals <- c(4L, 6L)
  cfg$Error_vals <- c(0.125, 0.25, 0.50)
  cfg$Loadings_vals <- c(0.4, 0.6, 0.8)

  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    flatten_scenario_grid_for_csv(cfg$scenario_grid),
    file.path(cfg$out_dir, "benchmark_grid_main_full.csv"),
    row.names = FALSE
  )
  write_gimme_grid_counts(cfg$scenario_grid, cfg$out_dir)
  write_gimme_old_vs_new_grid_comparison(
    cfg$out_dir,
    reps = if (smoke) 1L else reps,
    stop_crit = stop_crit
  )
  write_metadata(cfg, cfg$out_dir)

  if (isTRUE(dry_run)) {
    cat("Saved reset GIMME dry-run grid to ", cfg$out_dir, "\n", sep = "")
    return(invisible(NULL))
  }

  assert_required_packages(cfg$methods)
  method_specs <- build_method_specs(cfg)
  stage_names <- unique(cfg$scenario_grid$gimme_stage[order(cfg$scenario_grid$gimme_stage_order)])
  all_rows <- list()
  stop_ladder <- FALSE

  for (stage_name in stage_names) {
    if (isTRUE(stop_ladder)) {
      break
    }
    stage_grid <- cfg$scenario_grid[cfg$scenario_grid$gimme_stage == stage_name, , drop = FALSE]
    message(
      sprintf(
        "[%s] starting GIMME stage %s with %d structure fits",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stage_name,
        nrow(stage_grid)
      )
    )

    chunks <- split(seq_len(nrow(stage_grid)), ceiling(seq_len(nrow(stage_grid)) / stage_chunk_size))
    for (chunk_idx in seq_along(chunks)) {
      chunk_grid <- stage_grid[chunks[[chunk_idx]], , drop = FALSE]
      stage_cfg <- cfg
      stage_cfg$scenario_grid <- chunk_grid
      message(
        sprintf(
          "[%s] stage %s chunk %d/%d with %d structure fits",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          stage_name,
          chunk_idx,
          length(chunks),
          nrow(chunk_grid)
        )
      )

      chunk_rows <- run_benchmark_pass(stage_cfg, method_specs, cfg$gimme_workers)
      all_rows[[paste(stage_name, chunk_idx, sep = "__chunk_")]] <- chunk_rows
      write_partial_gimme_outputs(cfg)

      checkpoint <- read_checkpoint_if_exists(cfg$out_dir)
      stage_status <- summarize_stage_status(checkpoint, stage_name)
      if (nrow(stage_status) > 0L) {
        utils::write.table(
          stage_status,
          file = file.path(cfg$out_dir, "stage_stop_checks.csv"),
          sep = ",",
          row.names = FALSE,
          col.names = !file.exists(file.path(cfg$out_dir, "stage_stop_checks.csv")),
          append = file.exists(file.path(cfg$out_dir, "stage_stop_checks.csv")),
          quote = TRUE
        )
        message(
          sprintf(
            "[%s] stage %s fit-level status: ok=%d timeout=%d error=%d non_ok_rate=%.3f",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            stage_name,
            stage_status$ok,
            stage_status$timeout,
            stage_status$error,
            stage_status$non_ok_rate
          )
        )
        if (stage_status$structure_fits >= min_stage_fits &&
            stage_status$non_ok_rate >= max_non_ok_rate) {
          message(
            sprintf(
              "[%s] stopping GIMME ladder after %s: non_ok_rate %.3f >= %.3f",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              stage_name,
              stage_status$non_ok_rate,
              max_non_ok_rate
            )
          )
          stop_ladder <- TRUE
          break
        }
      }
    }
  }

  checkpoint <- read_checkpoint_if_exists(cfg$out_dir)
  if (is.null(checkpoint) || nrow(checkpoint) == 0L) {
    results_df <- dplyr::bind_rows(all_rows)
  } else {
    results_df <- checkpoint
  }
  summary_df <- summarize_benchmark_results(results_df)

  utils::write.csv(results_df, file.path(cfg$out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(cfg$out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(cfg$out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(cfg$out_dir, "benchmark_summary.csv"), row.names = FALSE)
  make_overview_plot(summary_df, cfg$out_dir)
  write_partial_gimme_outputs(cfg)
  write_metadata(cfg, cfg$out_dir)

  cat("Saved reset GIMME simulation outputs to ", cfg$out_dir, "\n", sep = "")
}

run_emotion_gimme()
