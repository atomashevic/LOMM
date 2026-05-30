#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

repo_root <- normalizePath(".", mustWork = TRUE)

source(file.path(repo_root, "scripts", "empirical", "run_lomm_openai_fer_observation_ablation.R"))

read_char_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(default)
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

paper_status <- function(x) {
  ifelse(x <= 0.33, "Pass", ifelse(x <= 1.00, "Partial", "Fail"))
}

add_paper_calls <- function(metric_comparison_long) {
  metric_comparison_long |>
    dplyr::mutate(
      max_discrepancy = pmax(median_diff_scaled, wasserstein_scaled, na.rm = TRUE),
      paper_call = paper_status(max_discrepancy)
    )
}

make_range_spec <- function(stage,
                            label,
                            eta,
                            zeta,
                            sigma_q,
                            dt,
                            init_mode = "fixed_current",
                            init_x_sd = NA_real_,
                            init_dx_sd = NA_real_,
                            burn_in = 0L,
                            radius_multiplier = NA_real_,
                            slope = NA_real_,
                            source_candidate_id = NA_integer_,
                            diagnostic = FALSE) {
  spec <- make_spec(
    stage = stage,
    label = label,
    eta = eta,
    zeta = zeta,
    sigma_q = sigma_q,
    dt = dt,
    obs_scale = 1.0,
    resid_rho = 0.10,
    rho_q = 0,
    common_sd = 0,
    common_phi = 0.6,
    cross_loading_sd = 0,
    temperature = 1.0,
    init_x_sd = init_x_sd,
    init_dx_sd = init_dx_sd,
    link = "softmax",
    diagnostic = diagnostic
  )
  spec$init_mode <- init_mode
  spec$burn_in <- as.integer(burn_in)
  spec$radius_multiplier <- radius_multiplier
  spec$slope <- slope
  spec$source_candidate_id <- source_candidate_id
  spec
}

evaluate_range_generated_spec <- function(empirical_target,
                                          spec,
                                          eta,
                                          zeta,
                                          n_replicates,
                                          ord0_embed,
                                          seed) {
  init_x_sd <- if (is.na(spec$init_x_sd[[1]])) NULL else spec$init_x_sd[[1]]
  init_dx_sd <- if (is.na(spec$init_dx_sd[[1]])) NULL else spec$init_dx_sd[[1]]

  synthetic_panel <- simulate_empirical_dlo_panel(
    clip_lengths = empirical_target$clip_lengths,
    community_map = empirical_target$community_map,
    loadings = empirical_target$loadings,
    measurement_var = empirical_target$measurement_var,
    eta = eta,
    zeta = zeta,
    sigma_q = spec$sigma_q[[1]],
    dt = spec$dt[[1]],
    n_replicates = n_replicates,
    model_name = spec$candidate_label[[1]],
    integrator = "linear_exact",
    seed = seed,
    rho_q = 0,
    obs_scale = 1.0,
    resid_rho = 0.10,
    common_sd = 0,
    common_phi = 0.6,
    cross_loading_sd = 0,
    link = "softmax",
    temperature = 1.0,
    init_x_sd = init_x_sd,
    init_dx_sd = init_dx_sd,
    burn_in = spec$burn_in[[1]]
  )

  target_loss <- tryCatch({
    synthetic_target <- build_property_target_from_panel(
      panel = synthetic_panel,
      model_name = spec$candidate_label[[1]],
      score_cols = empirical_target$score_cols,
      ord0_embed = ord0_embed,
      algorithm = "louvain"
    )
    list(target = synthetic_target, loss = weighted_loss(empirical_target, synthetic_target))
  }, error = function(e) {
    log_line("Candidate failed during metric extraction: ", spec$candidate_label[[1]], " :: ", conditionMessage(e))
    list(
      target = list(n_dim = NA_integer_, community_sizes = NA_integer_),
      loss = list(
        raw_loss = Inf,
        latent_loss = Inf,
        total_loss = Inf,
        comparison = data.frame()
      )
    )
  })

  list(
    panel = synthetic_panel,
    target = target_loss$target,
    loss = target_loss$loss
  )
}

evaluate_range_existing_panel <- function(empirical_target,
                                          spec,
                                          synthetic_panel,
                                          ord0_embed) {
  target_loss <- tryCatch({
    synthetic_target <- build_property_target_from_panel(
      panel = synthetic_panel,
      model_name = spec$candidate_label[[1]],
      score_cols = empirical_target$score_cols,
      ord0_embed = ord0_embed,
      algorithm = "louvain"
    )
    list(target = synthetic_target, loss = weighted_loss(empirical_target, synthetic_target))
  }, error = function(e) {
    log_line("Candidate failed during metric extraction: ", spec$candidate_label[[1]], " :: ", conditionMessage(e))
    list(
      target = list(n_dim = NA_integer_, community_sizes = NA_integer_),
      loss = list(
        raw_loss = Inf,
        latent_loss = Inf,
        total_loss = Inf,
        comparison = data.frame()
      )
    )
  })

  list(
    panel = synthetic_panel,
    target = target_loss$target,
    loss = target_loss$loss
  )
}

append_range_result <- function(results, summary_rows, comparison_rows,
                                empirical_target, spec, result, idx) {
  summary_rows[[idx]] <- summarize_result(spec, result, empirical_target, idx)

  comparison <- result$loss$comparison
  if (nrow(comparison) > 0L) {
    comparison <- add_paper_calls(comparison)
    comparison$candidate_id <- idx
    comparison$stage <- spec$stage[[1]]
    comparison$candidate_label <- spec$candidate_label[[1]]
    comparison_rows[[idx]] <- comparison
  }

  results[[idx]] <- result

  list(
    results = results,
    summary_rows = summary_rows,
    comparison_rows = comparison_rows
  )
}

range_spec_key <- function(candidate_summary) {
  key_cols <- c(
    "stage", "init_mode", "init_x_sd", "init_dx_sd", "burn_in",
    "radius_multiplier", "slope", "source_candidate_id"
  )
  key_cols <- intersect(key_cols, names(candidate_summary))
  key_df <- candidate_summary[, key_cols, drop = FALSE]
  key_df[] <- lapply(key_df, function(x) {
    if (is.numeric(x)) {
      ifelse(is.na(x), "NA", formatC(x, digits = 8, format = "fg"))
    } else {
      ifelse(is.na(x), "NA", as.character(x))
    }
  })
  do.call(paste, c(key_df, sep = "|"))
}

select_top_unique_range_ids <- function(candidate_summary, stage, n) {
  stage_df <- candidate_summary[
    candidate_summary$stage == stage &
      !candidate_summary$diagnostic &
      is.finite(candidate_summary$total_loss),
    ,
    drop = FALSE
  ]
  stage_df <- stage_df[order(stage_df$total_loss), , drop = FALSE]
  if (nrow(stage_df) == 0L) return(integer(0))
  key <- range_spec_key(stage_df)
  stage_df$candidate_id[!duplicated(key)][seq_len(min(n, length(unique(key))))]
}

write_range_report <- function(output_dir, candidate_summary, metric_comparison_long) {
  conclusions <- candidate_summary[
    candidate_summary$stage %in% c("F_init_final", "T_tanh") &
      is.finite(candidate_summary$total_loss),
    ,
    drop = FALSE
  ]
  conclusions <- conclusions[order(conclusions$total_loss), , drop = FALSE]
  if (nrow(conclusions) == 0L) {
    conclusions <- candidate_summary[order(candidate_summary$total_loss), , drop = FALSE]
  }
  best <- conclusions[1, , drop = FALSE]

  best_metrics <- metric_comparison_long[
    metric_comparison_long$candidate_id == best$candidate_id[[1]] &
      metric_comparison_long$metric %in% c(
        "channel_iqr", "diff_iqr", "lag1_acf", "run_length_entropy",
        "spectral_entropy", "effective_rank", "mean_abs_pairwise_cor"
      ),
    ,
    drop = FALSE
  ]

  lines <- c(
    "# LOMM/OpenAI-FER range-geometry diagnostics",
    "",
    sprintf("- Best diagnostic candidate: `%s`", best$candidate_label[[1]]),
    sprintf("- Stage: `%s`", best$stage[[1]]),
    sprintf("- Total weighted loss: %.4f", best$total_loss[[1]]),
    sprintf("- Raw weighted loss: %.4f", best$raw_loss[[1]]),
    sprintf("- Latent weighted loss: %.4f", best$latent_loss[[1]]),
    sprintf("- Empirical n_dim: %s; synthetic n_dim: %s", best$empirical_n_dim[[1]], best$synthetic_n_dim[[1]]),
    "",
    "Best-candidate paper-facing metric calls:",
    ""
  )

  for (idx in seq_len(nrow(best_metrics))) {
    lines <- c(lines, sprintf(
      "- `%s` `%s`: empirical=%.4f synthetic=%.4f max_discrepancy=%.4f call=%s",
      best_metrics$family[[idx]],
      best_metrics$metric[[idx]],
      best_metrics$empirical_median[[idx]],
      best_metrics$synthetic_median[[idx]],
      best_metrics$max_discrepancy[[idx]],
      best_metrics$paper_call[[idx]]
    ))
  }

  writeLines(lines, file.path(output_dir, "range_geometry_report.md"))
}

main <- function() {
  smoke_mode <- read_flag("RUN_LOMM_RANGE_GEOMETRY_SMOKE", FALSE)
  seed <- read_int("LOMM_RANGE_GEOMETRY_SEED", 20260520L)
  ord0_embed <- read_int("LOMM_RANGE_GEOMETRY_ORD0_EMBED", 10L)
  n_replicates <- read_int("LOMM_RANGE_GEOMETRY_REPS", if (smoke_mode) 1L else 3L)
  final_replicates <- read_int("LOMM_RANGE_GEOMETRY_FINAL_REPS", if (smoke_mode) 2L else 20L)
  max_clips <- read_num("LOMM_RANGE_GEOMETRY_MAX_CLIPS", if (smoke_mode) 10 else Inf)
  top_init_final <- read_int("LOMM_RANGE_GEOMETRY_TOP_INIT_FINAL", if (smoke_mode) 2L else 3L)
  top_tanh_sources <- read_int("LOMM_RANGE_GEOMETRY_TOP_TANH_SOURCES", if (smoke_mode) 1L else 3L)

  frame_scores_path <- read_char(
    "LOMM_RANGE_GEOMETRY_FRAME_SCORES",
    "/home/atomasevic/FER-evaluation/results/clip-large/mafw-sample-130-retained100/frame_scores.csv"
  )
  output_dir <- read_char(
    "LOMM_RANGE_GEOMETRY_OUTPUT_DIR",
    file.path(repo_root, "empirical_outputs_exploratory", "lomm_openai_fer_range_geometry_diagnostics")
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  eta <- read_num_vec("LOMM_RANGE_GEOMETRY_ETA", c(-0.05, -0.10, -0.15))
  zeta <- read_num_vec("LOMM_RANGE_GEOMETRY_ZETA", c(0.08, 0.10, 0.12))
  sigma_q <- read_num_vec("LOMM_RANGE_GEOMETRY_SIGMA_Q", 0.25)[[1]]
  dt <- read_num_vec("LOMM_RANGE_GEOMETRY_DT", 0.50)[[1]]

  init_modes <- read_char_vec(
    "LOMM_RANGE_GEOMETRY_INIT_MODES",
    c("fixed_current", "zero", "stochastic", "stationary_burnin")
  )
  init_x_sd_vals <- read_num_vec("LOMM_RANGE_GEOMETRY_INIT_X_SD", c(0.00, 0.05, 0.10))
  init_dx_sd_vals <- read_num_vec("LOMM_RANGE_GEOMETRY_INIT_DX_SD", c(0.00, 0.10, 0.25, 0.50))
  burn_in_vals <- read_num_vec("LOMM_RANGE_GEOMETRY_BURN_IN", c(50, 100, 200))
  radius_multiplier_vals <- read_num_vec("LOMM_RANGE_GEOMETRY_RADIUS_MULTIPLIER", c(1.0, 1.5, 2.0, 3.0))
  slope_vals <- read_num_vec("LOMM_RANGE_GEOMETRY_SLOPE", c(0.75, 1.00, 1.25, 1.50))

  saveRDS(
    list(
      smoke_mode = smoke_mode,
      seed = seed,
      ord0_embed = ord0_embed,
      n_replicates = n_replicates,
      final_replicates = final_replicates,
      max_clips = max_clips,
      frame_scores_path = frame_scores_path,
      eta = eta,
      zeta = zeta,
      sigma_q = sigma_q,
      dt = dt,
      init_modes = init_modes,
      init_x_sd_vals = init_x_sd_vals,
      init_dx_sd_vals = init_dx_sd_vals,
      burn_in_vals = burn_in_vals,
      radius_multiplier_vals = radius_multiplier_vals,
      slope_vals = slope_vals
    ),
    file.path(output_dir, "run_config.rds")
  )

  log_line("Building retained target from ", frame_scores_path)
  empirical_target <- build_retained_target(
    frame_scores_path = frame_scores_path,
    model_name = "oai-large-retained100",
    max_clips = max_clips,
    ord0_embed = ord0_embed
  )
  n_fac <- length(unique(empirical_target$community_map))
  eta <- resize_factor_profile(eta, n_fac, "eta")
  zeta <- resize_factor_profile(zeta, n_fac, "zeta")

  utils::write.csv(empirical_target$panel, file.path(output_dir, "empirical_panel_clean.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$qc, file.path(output_dir, "empirical_qc.csv"), row.names = FALSE)

  results <- list()
  summary_rows <- list()
  comparison_rows <- list()
  idx <- 1L

  log_line("Generating confirmed retained-100 structural baseline")
  confirmed_spec <- make_range_spec(
    stage = "confirmed_structural",
    label = "confirmed_resid_0.10_fixed_current",
    eta = eta,
    zeta = zeta,
    sigma_q = sigma_q,
    dt = dt,
    init_mode = "fixed_current"
  )
  confirmed_result <- evaluate_range_generated_spec(
    empirical_target, confirmed_spec, eta, zeta, n_replicates, ord0_embed, seed + idx
  )
  appended <- append_range_result(results, summary_rows, comparison_rows, empirical_target, confirmed_spec, confirmed_result, idx)
  results <- appended$results
  summary_rows <- appended$summary_rows
  comparison_rows <- appended$comparison_rows
  confirmed_panel <- confirmed_result$panel
  idx <- idx + 1L

  amp_compare <- compare_amplitude_decomp(
    empirical_panel = empirical_target$panel,
    synthetic_panel = confirmed_panel,
    score_cols = empirical_target$score_cols
  )
  utils::write.csv(amp_compare, file.path(output_dir, "amplitude_decomposition_emp_vs_syn.csv"), row.names = FALSE)

  init_specs <- list()
  spec_idx <- 1L
  if ("fixed_current" %in% init_modes) {
    init_specs[[spec_idx]] <- make_range_spec(
      "I_init",
      "init_fixed_current",
      eta, zeta, sigma_q, dt,
      init_mode = "fixed_current"
    )
    spec_idx <- spec_idx + 1L
  }
  if ("zero" %in% init_modes) {
    init_specs[[spec_idx]] <- make_range_spec(
      "I_init",
      "init_zero",
      eta, zeta, sigma_q, dt,
      init_mode = "zero",
      init_x_sd = 0,
      init_dx_sd = 0
    )
    spec_idx <- spec_idx + 1L
  }
  if ("stochastic" %in% init_modes) {
    for (init_x_sd in init_x_sd_vals) {
      for (init_dx_sd in init_dx_sd_vals) {
        init_specs[[spec_idx]] <- make_range_spec(
          "I_init",
          sprintf("init_stochastic_x_%0.2f_v_%0.2f", init_x_sd, init_dx_sd),
          eta, zeta, sigma_q, dt,
          init_mode = "stochastic",
          init_x_sd = init_x_sd,
          init_dx_sd = init_dx_sd
        )
        spec_idx <- spec_idx + 1L
      }
    }
  }
  if ("stationary_burnin" %in% init_modes) {
    for (burn_in in burn_in_vals) {
      init_specs[[spec_idx]] <- make_range_spec(
        "I_init",
        sprintf("init_stationary_burnin_%03d", as.integer(burn_in)),
        eta, zeta, sigma_q, dt,
        init_mode = "stationary_burnin",
        burn_in = burn_in
      )
      spec_idx <- spec_idx + 1L
    }
  }

  for (spec in init_specs) {
    log_line("Init/burn-in candidate ", spec$candidate_label[[1]])
    result <- evaluate_range_generated_spec(
      empirical_target, spec, eta, zeta, n_replicates, ord0_embed, seed + idx
    )
    appended <- append_range_result(results, summary_rows, comparison_rows, empirical_target, spec, result, idx)
    results <- appended$results
    summary_rows <- appended$summary_rows
    comparison_rows <- appended$comparison_rows
    idx <- idx + 1L
  }

  candidate_summary <- dplyr::bind_rows(summary_rows)
  init_parent_ids <- select_top_unique_range_ids(candidate_summary, "I_init", top_init_final)

  for (parent_id in init_parent_ids) {
    parent <- candidate_summary[candidate_summary$candidate_id == parent_id, , drop = FALSE]
    log_line("Final init confirmation parent=", parent_id)
    spec <- make_range_spec(
      "F_init_final",
      sprintf("final_%s_reps_%02d", parent$candidate_label[[1]], final_replicates),
      eta, zeta, sigma_q, dt,
      init_mode = parent$init_mode[[1]],
      init_x_sd = parent$init_x_sd[[1]],
      init_dx_sd = parent$init_dx_sd[[1]],
      burn_in = parent$burn_in[[1]]
    )
    result <- evaluate_range_generated_spec(
      empirical_target, spec, eta, zeta, final_replicates, ord0_embed, seed + idx
    )
    appended <- append_range_result(results, summary_rows, comparison_rows, empirical_target, spec, result, idx)
    results <- appended$results
    summary_rows <- appended$summary_rows
    comparison_rows <- appended$comparison_rows
    idx <- idx + 1L
  }

  candidate_summary <- dplyr::bind_rows(summary_rows)
  tanh_source_ids <- select_top_unique_range_ids(candidate_summary, "I_init", top_tanh_sources)
  tanh_source_ids <- unique(c(1L, tanh_source_ids))

  for (source_id in tanh_source_ids) {
    source_panel <- results[[source_id]]$panel
    for (radius_multiplier in radius_multiplier_vals) {
      for (slope in slope_vals) {
        log_line("Tanh source=", source_id, " radius=", radius_multiplier, " slope=", slope)
        label <- sprintf("tanh_src_%03d_radius_%0.2f_slope_%0.2f", source_id, radius_multiplier, slope)
        spec <- make_range_spec(
          "T_tanh",
          label,
          eta, zeta, sigma_q, dt,
          init_mode = "posthoc_tanh",
          radius_multiplier = radius_multiplier,
          slope = slope,
          source_candidate_id = source_id
        )
        tanh_panel <- apply_empirical_tanh_link(
          synthetic_panel = source_panel,
          empirical_panel = empirical_target$panel,
          score_cols = empirical_target$score_cols,
          radius_multiplier = radius_multiplier,
          slope = slope
        )
        result <- evaluate_range_existing_panel(empirical_target, spec, tanh_panel, ord0_embed)
        appended <- append_range_result(results, summary_rows, comparison_rows, empirical_target, spec, result, idx)
        results <- appended$results
        summary_rows <- appended$summary_rows
        comparison_rows <- appended$comparison_rows
        idx <- idx + 1L
      }
    }
  }

  candidate_summary <- dplyr::bind_rows(summary_rows) |>
    dplyr::arrange(total_loss)
  candidate_summary$candidate_rank <- seq_len(nrow(candidate_summary))
  metric_comparison_long <- dplyr::bind_rows(comparison_rows)
  channel_frontier <- candidate_channel_iqr_frontier(candidate_summary, metric_comparison_long)

  conclusion_candidates <- candidate_summary[
    candidate_summary$stage %in% c("F_init_final", "T_tanh") &
      is.finite(candidate_summary$total_loss),
    ,
    drop = FALSE
  ]
  conclusion_candidates <- conclusion_candidates[order(conclusion_candidates$total_loss), , drop = FALSE]
  best_id <- conclusion_candidates$candidate_id[[1]]

  utils::write.csv(candidate_summary, file.path(output_dir, "candidate_summary.csv"), row.names = FALSE)
  utils::write.csv(metric_comparison_long, file.path(output_dir, "metric_comparison_long.csv"), row.names = FALSE)
  utils::write.csv(channel_frontier, file.path(output_dir, "channel_iqr_frontier.csv"), row.names = FALSE)
  utils::write.csv(candidate_summary[candidate_summary$stage %in% c("I_init", "F_init_final"), ],
                   file.path(output_dir, "init_burnin_candidate_summary.csv"), row.names = FALSE)
  utils::write.csv(metric_comparison_long[metric_comparison_long$stage %in% c("I_init", "F_init_final"), ],
                   file.path(output_dir, "init_burnin_metric_comparison_long.csv"), row.names = FALSE)
  utils::write.csv(candidate_summary[candidate_summary$stage == "T_tanh", ],
                   file.path(output_dir, "tanh_link_candidate_summary.csv"), row.names = FALSE)
  utils::write.csv(metric_comparison_long[metric_comparison_long$stage == "T_tanh", ],
                   file.path(output_dir, "tanh_link_metric_comparison_long.csv"), row.names = FALSE)
  utils::write.csv(candidate_summary[candidate_summary$candidate_id == best_id, ],
                   file.path(output_dir, "run_summary.csv"), row.names = FALSE)
  utils::write.csv(results[[best_id]]$panel, file.path(output_dir, "best_synthetic_panel.csv"), row.names = FALSE)

  tanh_rows <- candidate_summary[candidate_summary$stage == "T_tanh", , drop = FALSE]
  if (nrow(tanh_rows) > 0L) {
    tanh_best_id <- tanh_rows$candidate_id[[which.min(tanh_rows$total_loss)]]
    utils::write.csv(results[[tanh_best_id]]$panel, file.path(output_dir, "tanh_link_best_panel.csv"), row.names = FALSE)
  }

  saveRDS(
    list(
      empirical = empirical_target,
      candidate_summary = candidate_summary,
      metric_comparison_long = metric_comparison_long,
      channel_iqr_frontier = channel_frontier,
      results = results
    ),
    file.path(output_dir, "comparison_bundle.rds")
  )

  write_range_report(output_dir, candidate_summary, metric_comparison_long)
  log_line("Done. Best diagnostic candidate: ", candidate_summary$candidate_label[[match(best_id, candidate_summary$candidate_id)]],
           " total_loss=", round(candidate_summary$total_loss[[match(best_id, candidate_summary$candidate_id)]], 4))
}

if (sys.nframe() == 0L) {
  main()
}
