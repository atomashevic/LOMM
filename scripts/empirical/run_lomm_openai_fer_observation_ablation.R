#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

repo_root <- normalizePath(".", mustWork = TRUE)

source(file.path(repo_root, "extract.r"))

read_flag <- function(name, default = FALSE) {
  value <- trimws(Sys.getenv(name, unset = if (default) "1" else "0"))
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

read_int <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(as.integer(default))
  as.integer(value)
}

read_num <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(as.numeric(default))
  as.numeric(value)
}

read_num_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(as.numeric(default))
  as.numeric(strsplit(value, ",", fixed = TRUE)[[1]])
}

read_optional_num_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(as.numeric(default))
  raw <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  out <- ifelse(raw %in% c("NA", "na", "NULL", "null", ""), NA_real_, as.numeric(raw))
  as.numeric(out)
}

read_char <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(default)
  value
}

log_line <- function(...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] %s", timestamp, paste(..., collapse = "")))
}

format_vector_spec <- function(x, digits = 4L) {
  paste(formatC(as.numeric(x), format = "f", digits = digits), collapse = ";")
}

resize_factor_profile <- function(x, n_fac, name) {
  x <- as.numeric(x)
  if (length(x) == 1L || length(x) == n_fac) {
    return(x)
  }
  if (length(x) < 1L || any(!is.finite(x))) {
    stop(sprintf("%s must contain finite values.", name), call. = FALSE)
  }

  stats::approx(
    x = seq(0, 1, length.out = length(x)),
    y = x,
    xout = seq(0, 1, length.out = n_fac),
    rule = 2
  )$y
}

build_retained_target <- function(frame_scores_path,
                                  model_name,
                                  max_clips,
                                  ord0_embed) {
  cleaned <- clean_empirical_frame_scores(
    frame_scores_path = frame_scores_path,
    model_name = model_name
  )

  qc <- cleaned$qc
  usable <- qc$clip[!qc$excluded]
  if (is.finite(max_clips)) {
    usable <- utils::head(usable, max_clips)
    qc <- qc[qc$clip %in% usable, , drop = FALSE]
  }

  panel <- cleaned$panel[cleaned$panel$clip %in% usable, , drop = FALSE]
  target <- build_property_target_from_panel(
    panel = panel,
    model_name = model_name,
    score_cols = EMPIRICAL_EMOTION_COLUMNS,
    ord0_embed = ord0_embed,
    algorithm = "louvain"
  )
  target$qc <- qc
  target$clip_lengths <- qc$usable_length
  names(target$clip_lengths) <- qc$clip
  target
}

metric_weights <- c(
  channel_iqr = 3.0,
  mean_abs_pairwise_cor = 2.5,
  effective_rank = 2.0,
  lag1_acf = 1.5,
  diff_iqr = 1.0,
  run_length_entropy = 1.0,
  spectral_entropy = 0.75,
  sample_entropy = 0.50,
  permutation_entropy = 0.50
)

weighted_loss <- function(empirical_target, synthetic_target) {
  base_loss <- score_candidate_loss(empirical_target, synthetic_target)
  comparison <- base_loss$comparison
  comparison$weight <- unname(metric_weights[comparison$metric])
  comparison$weight[!is.finite(comparison$weight)] <- 0

  used <- comparison[comparison$weight > 0 & is.finite(comparison$metric_loss), , drop = FALSE]
  raw_used <- used[used$family == "raw", , drop = FALSE]
  latent_used <- used[used$family == "latent", , drop = FALSE]

  raw_loss <- stats::weighted.mean(raw_used$metric_loss, raw_used$weight, na.rm = TRUE)
  latent_loss <- stats::weighted.mean(latent_used$metric_loss, latent_used$weight, na.rm = TRUE)
  if (!is.finite(raw_loss)) raw_loss <- Inf
  if (!is.finite(latent_loss)) latent_loss <- Inf

  n_dim_penalty <- abs(empirical_target$n_dim - synthetic_target$n_dim)
  community_penalty <- optimal_community_size_loss(
    empirical_target$community_sizes,
    synthetic_target$community_sizes
  ) / length(empirical_target$score_cols)

  list(
    raw_loss = raw_loss,
    latent_loss = latent_loss + n_dim_penalty + community_penalty,
    total_loss = 0.5 * raw_loss + 0.5 * (latent_loss + n_dim_penalty + community_penalty),
    comparison = comparison
  )
}

make_spec <- function(stage,
                      label,
                      eta,
                      zeta,
                      sigma_q,
                      dt,
                      obs_scale = 1.0,
                      resid_rho = 0,
                      rho_q = 0,
                      common_sd = 0,
                      common_phi = 0.6,
                      cross_loading_sd = 0,
                      temperature = 1.0,
                      init_x_sd = NA_real_,
                      init_dx_sd = NA_real_,
                      link = "softmax",
                      diagnostic = FALSE) {
  data.frame(
    stage = stage,
    candidate_label = label,
    eta_spec = format_vector_spec(eta),
    zeta_spec = format_vector_spec(zeta),
    sigma_q = sigma_q,
    dt = dt,
    obs_scale = obs_scale,
    resid_rho = resid_rho,
    rho_q = rho_q,
    common_sd = common_sd,
    common_phi = common_phi,
    cross_loading_sd = cross_loading_sd,
    link = link,
    temperature = temperature,
    init_x_sd = init_x_sd,
    init_dx_sd = init_dx_sd,
    diagnostic = diagnostic,
    stringsAsFactors = FALSE
  )
}

evaluate_generated_spec <- function(empirical_target,
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
    rho_q = spec$rho_q[[1]],
    obs_scale = spec$obs_scale[[1]],
    resid_rho = spec$resid_rho[[1]],
    common_sd = spec$common_sd[[1]],
    common_phi = spec$common_phi[[1]],
    cross_loading_sd = spec$cross_loading_sd[[1]],
    link = spec$link[[1]],
    temperature = spec$temperature[[1]],
    init_x_sd = init_x_sd,
    init_dx_sd = init_dx_sd
  )

  synthetic_target <- build_property_target_from_panel(
    panel = synthetic_panel,
    model_name = spec$candidate_label[[1]],
    score_cols = empirical_target$score_cols,
    ord0_embed = ord0_embed,
    algorithm = "louvain"
  )
  loss <- weighted_loss(empirical_target, synthetic_target)

  list(
    panel = synthetic_panel,
    target = synthetic_target,
    loss = loss
  )
}

evaluate_existing_panel <- function(empirical_target,
                                    spec,
                                    synthetic_panel,
                                    ord0_embed) {
  synthetic_target <- build_property_target_from_panel(
    panel = synthetic_panel,
    model_name = spec$candidate_label[[1]],
    score_cols = empirical_target$score_cols,
    ord0_embed = ord0_embed,
    algorithm = "louvain"
  )
  loss <- weighted_loss(empirical_target, synthetic_target)

  list(
    panel = synthetic_panel,
    target = synthetic_target,
    loss = loss
  )
}

summarize_result <- function(spec, result, empirical_target, idx) {
  row <- spec
  row$candidate_id <- idx
  row$raw_loss <- result$loss$raw_loss
  row$latent_loss <- result$loss$latent_loss
  row$total_loss <- result$loss$total_loss
  row$empirical_n_dim <- empirical_target$n_dim
  row$synthetic_n_dim <- result$target$n_dim
  row$empirical_community_sizes <- paste(empirical_target$community_sizes, collapse = "-")
  row$synthetic_community_sizes <- paste(result$target$community_sizes, collapse = "-")
  row
}

append_result <- function(results, summary_rows, comparison_rows, cor_rows,
                          empirical_target, spec, result, idx) {
  summary_rows[[idx]] <- summarize_result(spec, result, empirical_target, idx)

  comparison <- result$loss$comparison
  comparison$candidate_id <- idx
  comparison$stage <- spec$stage[[1]]
  comparison$candidate_label <- spec$candidate_label[[1]]
  comparison_rows[[idx]] <- comparison

  cor_rows[[idx]] <- cor_sign_summary(
    result$panel,
    score_cols = empirical_target$score_cols,
    label = spec$candidate_label[[1]]
  )
  cor_rows[[idx]]$candidate_id <- idx
  cor_rows[[idx]]$stage <- spec$stage[[1]]

  results[[idx]] <- result

  list(
    results = results,
    summary_rows = summary_rows,
    comparison_rows = comparison_rows,
    cor_rows = cor_rows
  )
}

top_candidate_ids <- function(candidate_summary, stage, n) {
  stage_df <- candidate_summary[candidate_summary$stage == stage &
                                  !candidate_summary$diagnostic &
                                  is.finite(candidate_summary$total_loss), , drop = FALSE]
  stage_df <- stage_df[order(stage_df$total_loss), , drop = FALSE]
  utils::head(stage_df$candidate_id, n)
}

structural_spec_key <- function(candidate_summary) {
  key_cols <- c(
    "eta_spec", "zeta_spec", "sigma_q", "dt", "obs_scale", "resid_rho",
    "rho_q", "common_sd", "common_phi", "cross_loading_sd", "link",
    "temperature", "init_x_sd", "init_dx_sd"
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

select_final_parent_ids <- function(candidate_summary, n) {
  valid_for_final <- candidate_summary[
    !candidate_summary$diagnostic & is.finite(candidate_summary$total_loss),
    ,
    drop = FALSE
  ]
  valid_for_final <- valid_for_final[order(valid_for_final$total_loss), , drop = FALSE]
  if (nrow(valid_for_final) == 0L) {
    return(integer(0))
  }

  spec_key <- structural_spec_key(valid_for_final)
  valid_for_final$candidate_id[!duplicated(spec_key)][seq_len(min(n, length(unique(spec_key))))]
}

select_conclusion_candidates <- function(candidate_summary) {
  valid <- candidate_summary[!candidate_summary$diagnostic & is.finite(candidate_summary$total_loss), , drop = FALSE]
  if (nrow(valid) == 0L) {
    valid <- candidate_summary[is.finite(candidate_summary$total_loss), , drop = FALSE]
  }

  final <- valid[valid$stage == "F_final", , drop = FALSE]
  if (nrow(final) > 0L) {
    return(final[order(final$total_loss), , drop = FALSE])
  }

  valid[order(valid$total_loss), , drop = FALSE]
}

make_report <- function(candidate_summary, output_dir, smoke_mode) {
  conclusion_candidates <- select_conclusion_candidates(candidate_summary)
  best <- conclusion_candidates[1, , drop = FALSE]
  conclusion_basis <- if (any(conclusion_candidates$stage == "F_final")) {
    "confirmed F_final rows"
  } else {
    "best available non-diagnostic ladder rows"
  }

  lines <- c(
    "# Coupled-observation LOMM vs OpenAI-FER ablation",
    "",
    sprintf("- Smoke mode: `%s`", smoke_mode),
    sprintf("- Conclusion basis: `%s`", conclusion_basis),
    sprintf("- Best candidate: `%s`", best$candidate_label[[1]]),
    sprintf("- Stage: `%s`", best$stage[[1]]),
    sprintf("- Total weighted loss: %.4f", best$total_loss[[1]]),
    sprintf("- Raw weighted loss: %.4f", best$raw_loss[[1]]),
    sprintf("- Latent weighted loss: %.4f", best$latent_loss[[1]]),
    sprintf("- Empirical n_dim: %s; synthetic n_dim: %s", best$empirical_n_dim[[1]], best$synthetic_n_dim[[1]]),
    "",
    "Top candidates:",
    ""
  )

  top <- utils::head(conclusion_candidates, 10)
  for (idx in seq_len(nrow(top))) {
    lines <- c(lines, sprintf(
      "- `%s` [%s]: total=%.4f raw=%.4f latent=%.4f obs_scale=%.2f resid_rho=%.2f rho_q=%.2f common_sd=%.2f cross_loading_sd=%.3f temp=%.2f",
      top$candidate_label[[idx]],
      top$stage[[idx]],
      top$total_loss[[idx]],
      top$raw_loss[[idx]],
      top$latent_loss[[idx]],
      top$obs_scale[[idx]],
      top$resid_rho[[idx]],
      top$rho_q[[idx]],
      top$common_sd[[idx]],
      top$cross_loading_sd[[idx]],
      top$temperature[[idx]]
    ))
  }

  writeLines(lines, file.path(output_dir, "ablation_report.md"))
}

main <- function() {
  smoke_mode <- read_flag("RUN_LOMM_OPENAI_FER_SMOKE", FALSE)
  seed <- read_int("LOMM_OPENAI_FER_SEED", 20260519L)
  ord0_embed <- read_int("LOMM_OPENAI_FER_ORD0_EMBED", 10L)
  n_replicates <- read_int("LOMM_OPENAI_FER_REPS", if (smoke_mode) 1L else 3L)
  final_replicates <- read_int("LOMM_OPENAI_FER_FINAL_REPS", if (smoke_mode) 2L else 10L)
  max_clips <- read_num("LOMM_OPENAI_FER_MAX_CLIPS", if (smoke_mode) 15 else Inf)

  frame_scores_path <- read_char(
    "LOMM_OPENAI_FER_FRAME_SCORES",
    "/home/atomasevic/FER-evaluation/results/clip-large/mafw-sample-130-retained100/frame_scores.csv"
  )
  output_dir <- read_char(
    "LOMM_OPENAI_FER_OUTPUT_DIR",
    file.path(repo_root, "empirical_outputs_exploratory", "lomm_openai_fer_observation_ablation")
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  eta <- read_num_vec("LOMM_OPENAI_FER_ETA", c(-0.05, -0.10, -0.15))
  zeta <- read_num_vec("LOMM_OPENAI_FER_ZETA", c(0.08, 0.10, 0.12))
  sigma_q <- read_num_vec("LOMM_OPENAI_FER_SIGMA_Q", 0.25)[[1]]
  dt <- read_num_vec("LOMM_OPENAI_FER_DT", 0.50)[[1]]

  grid <- if (smoke_mode) {
    list(
      shrink = read_num_vec("LOMM_OPENAI_FER_SHRINK", c(0.65, 1.00)),
      obs_scale = read_num_vec("LOMM_OPENAI_FER_OBS_SCALE", c(0.50, 1.00)),
      resid_rho = read_num_vec("LOMM_OPENAI_FER_RESID_RHO", c(0.00, 0.25)),
      rho_q = read_num_vec("LOMM_OPENAI_FER_RHO_Q", c(0.00, 0.30)),
      common_sd = read_num_vec("LOMM_OPENAI_FER_COMMON_SD", c(0.00, 0.10)),
      common_phi = read_num_vec("LOMM_OPENAI_FER_COMMON_PHI", c(0.50)),
      cross_loading_sd = read_num_vec("LOMM_OPENAI_FER_CROSS_LOADING_SD", c(0.00, 0.05)),
      temperature = read_num_vec("LOMM_OPENAI_FER_TEMPERATURE", c(1.00)),
      init_dx_sd = read_optional_num_vec("LOMM_OPENAI_FER_INIT_DX_SD", c(NA_real_, 0.50)),
      stage_b_top = read_int("LOMM_OPENAI_FER_STAGE_B_TOP", 2L),
      stage_c_top = read_int("LOMM_OPENAI_FER_STAGE_C_TOP", 2L),
      stage_d_top = read_int("LOMM_OPENAI_FER_STAGE_D_TOP", 2L),
      stage_e_top = read_int("LOMM_OPENAI_FER_STAGE_E_TOP", 2L)
    )
  } else {
    list(
      shrink = read_num_vec("LOMM_OPENAI_FER_SHRINK", c(0.35, 0.50, 0.65, 0.80, 1.00)),
      obs_scale = read_num_vec("LOMM_OPENAI_FER_OBS_SCALE", c(0.35, 0.50, 0.65, 0.80, 1.00)),
      resid_rho = read_num_vec("LOMM_OPENAI_FER_RESID_RHO", c(0.00, 0.10, 0.25, 0.40)),
      rho_q = read_num_vec("LOMM_OPENAI_FER_RHO_Q", c(0.00, 0.15, 0.30, 0.50)),
      common_sd = read_num_vec("LOMM_OPENAI_FER_COMMON_SD", c(0.00, 0.05, 0.10, 0.20)),
      common_phi = read_num_vec("LOMM_OPENAI_FER_COMMON_PHI", c(0.20, 0.50, 0.75)),
      cross_loading_sd = read_num_vec("LOMM_OPENAI_FER_CROSS_LOADING_SD", c(0.00, 0.025, 0.05)),
      temperature = read_num_vec("LOMM_OPENAI_FER_TEMPERATURE", c(0.75, 1.00, 1.50)),
      init_dx_sd = read_optional_num_vec("LOMM_OPENAI_FER_INIT_DX_SD", c(NA_real_, 0.25, 0.50)),
      stage_b_top = read_int("LOMM_OPENAI_FER_STAGE_B_TOP", 6L),
      stage_c_top = read_int("LOMM_OPENAI_FER_STAGE_C_TOP", 8L),
      stage_d_top = read_int("LOMM_OPENAI_FER_STAGE_D_TOP", 5L),
      stage_e_top = read_int("LOMM_OPENAI_FER_STAGE_E_TOP", 3L)
    )
  }

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
      grid = grid
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
  utils::write.csv(empirical_target$raw_metrics, file.path(output_dir, "empirical_raw_metrics.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$latent_metrics, file.path(output_dir, "empirical_latent_metrics.csv"), row.names = FALSE)

  results <- list()
  summary_rows <- list()
  comparison_rows <- list()
  cor_rows <- list()
  idx <- 1L

  log_line("Evaluating baseline")
  baseline_spec <- make_spec("baseline", "baseline_current_winner", eta, zeta, sigma_q, dt)
  baseline_result <- evaluate_generated_spec(
    empirical_target, baseline_spec, eta, zeta, n_replicates, ord0_embed, seed + idx
  )
  appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, baseline_spec, baseline_result, idx)
  results <- appended$results
  summary_rows <- appended$summary_rows
  comparison_rows <- appended$comparison_rows
  cor_rows <- appended$cor_rows
  baseline_panel <- baseline_result$panel
  idx <- idx + 1L

  for (shrink in grid$shrink) {
    log_line("Stage A shrink=", shrink)
    spec <- make_spec(
      "A_rescale",
      sprintf("stage_A_shrink_%0.2f", shrink),
      eta,
      zeta,
      sigma_q,
      dt,
      diagnostic = TRUE
    )
    rescaled <- robust_rescale_panel(
      synthetic_panel = baseline_panel,
      empirical_panel = empirical_target$panel,
      score_cols = empirical_target$score_cols,
      shrink = shrink
    )
    result <- evaluate_existing_panel(empirical_target, spec, rescaled, ord0_embed)
    appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, spec, result, idx)
    results <- appended$results
    summary_rows <- appended$summary_rows
    comparison_rows <- appended$comparison_rows
    cor_rows <- appended$cor_rows
    idx <- idx + 1L
  }

  for (obs_scale in grid$obs_scale) {
    for (resid_rho in grid$resid_rho) {
      log_line("Stage B obs_scale=", obs_scale, " resid_rho=", resid_rho)
      spec <- make_spec(
        "B_obs_resid",
        sprintf("stage_B_obs_%0.2f_resid_%0.2f", obs_scale, resid_rho),
        eta,
        zeta,
        sigma_q,
        dt,
        obs_scale = obs_scale,
        resid_rho = resid_rho
      )
      result <- evaluate_generated_spec(empirical_target, spec, eta, zeta, n_replicates, ord0_embed, seed + idx)
      appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, spec, result, idx)
      results <- appended$results
      summary_rows <- appended$summary_rows
      comparison_rows <- appended$comparison_rows
      cor_rows <- appended$cor_rows
      idx <- idx + 1L
    }
  }

  candidate_summary <- dplyr::bind_rows(summary_rows)
  for (parent_id in top_candidate_ids(candidate_summary, "B_obs_resid", grid$stage_b_top)) {
    parent <- candidate_summary[candidate_summary$candidate_id == parent_id, , drop = FALSE]
    for (rho_q in grid$rho_q) {
      for (cross_loading_sd in grid$cross_loading_sd) {
        log_line("Stage C parent=", parent_id, " rho_q=", rho_q, " cross_loading_sd=", cross_loading_sd)
        spec <- make_spec(
          "C_latent_cross",
          sprintf("stage_C_parent_%03d_rho_%0.2f_cross_%0.3f", parent_id, rho_q, cross_loading_sd),
          eta,
          zeta,
          sigma_q,
          dt,
          obs_scale = parent$obs_scale[[1]],
          resid_rho = parent$resid_rho[[1]],
          rho_q = rho_q,
          cross_loading_sd = cross_loading_sd
        )
        result <- evaluate_generated_spec(empirical_target, spec, eta, zeta, n_replicates, ord0_embed, seed + idx)
        appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, spec, result, idx)
        results <- appended$results
        summary_rows <- appended$summary_rows
        comparison_rows <- appended$comparison_rows
        cor_rows <- appended$cor_rows
        idx <- idx + 1L
      }
    }
  }

  candidate_summary <- dplyr::bind_rows(summary_rows)
  for (parent_id in top_candidate_ids(candidate_summary, "C_latent_cross", grid$stage_c_top)) {
    parent <- candidate_summary[candidate_summary$candidate_id == parent_id, , drop = FALSE]
    for (common_sd in grid$common_sd) {
      for (common_phi in grid$common_phi) {
        log_line("Stage D parent=", parent_id, " common_sd=", common_sd, " common_phi=", common_phi)
        spec <- make_spec(
          "D_common_ar1",
          sprintf("stage_D_parent_%03d_common_%0.2f_phi_%0.2f", parent_id, common_sd, common_phi),
          eta,
          zeta,
          sigma_q,
          dt,
          obs_scale = parent$obs_scale[[1]],
          resid_rho = parent$resid_rho[[1]],
          rho_q = parent$rho_q[[1]],
          common_sd = common_sd,
          common_phi = common_phi,
          cross_loading_sd = parent$cross_loading_sd[[1]]
        )
        result <- evaluate_generated_spec(empirical_target, spec, eta, zeta, n_replicates, ord0_embed, seed + idx)
        appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, spec, result, idx)
        results <- appended$results
        summary_rows <- appended$summary_rows
        comparison_rows <- appended$comparison_rows
        cor_rows <- appended$cor_rows
        idx <- idx + 1L
      }
    }
  }

  candidate_summary <- dplyr::bind_rows(summary_rows)
  for (parent_id in top_candidate_ids(candidate_summary, "D_common_ar1", grid$stage_d_top)) {
    parent <- candidate_summary[candidate_summary$candidate_id == parent_id, , drop = FALSE]
    for (init_dx_sd in grid$init_dx_sd) {
      for (temperature in grid$temperature) {
        init_label <- if (is.na(init_dx_sd)) "fixed" else sprintf("stoch_%0.2f", init_dx_sd)
        log_line("Stage E parent=", parent_id, " init=", init_label, " temperature=", temperature)
        spec <- make_spec(
          "E_init_link",
          sprintf("stage_E_parent_%03d_init_%s_temp_%0.2f", parent_id, init_label, temperature),
          eta,
          zeta,
          sigma_q,
          dt,
          obs_scale = parent$obs_scale[[1]],
          resid_rho = parent$resid_rho[[1]],
          rho_q = parent$rho_q[[1]],
          common_sd = parent$common_sd[[1]],
          common_phi = parent$common_phi[[1]],
          cross_loading_sd = parent$cross_loading_sd[[1]],
          temperature = temperature,
          init_x_sd = if (is.na(init_dx_sd)) NA_real_ else 0.10,
          init_dx_sd = init_dx_sd
        )
        result <- evaluate_generated_spec(empirical_target, spec, eta, zeta, n_replicates, ord0_embed, seed + idx)
        appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, spec, result, idx)
        results <- appended$results
        summary_rows <- appended$summary_rows
        comparison_rows <- appended$comparison_rows
        cor_rows <- appended$cor_rows
        idx <- idx + 1L
      }
    }
  }

  candidate_summary <- dplyr::bind_rows(summary_rows)
  final_parent_ids <- select_final_parent_ids(candidate_summary, grid$stage_e_top)
  for (parent_id in final_parent_ids) {
    parent <- candidate_summary[candidate_summary$candidate_id == parent_id, , drop = FALSE]
    log_line("Final confirmation parent=", parent_id)
    spec <- make_spec(
      "F_final",
      sprintf("stage_F_parent_%03d_reps_%02d", parent_id, final_replicates),
      eta,
      zeta,
      sigma_q,
      dt,
      obs_scale = parent$obs_scale[[1]],
      resid_rho = parent$resid_rho[[1]],
      rho_q = parent$rho_q[[1]],
      common_sd = parent$common_sd[[1]],
      common_phi = parent$common_phi[[1]],
      cross_loading_sd = parent$cross_loading_sd[[1]],
      temperature = parent$temperature[[1]],
      init_x_sd = parent$init_x_sd[[1]],
      init_dx_sd = parent$init_dx_sd[[1]]
    )
    result <- evaluate_generated_spec(empirical_target, spec, eta, zeta, final_replicates, ord0_embed, seed + idx)
    appended <- append_result(results, summary_rows, comparison_rows, cor_rows, empirical_target, spec, result, idx)
    results <- appended$results
    summary_rows <- appended$summary_rows
    comparison_rows <- appended$comparison_rows
    cor_rows <- appended$cor_rows
    idx <- idx + 1L
  }

  candidate_summary <- dplyr::bind_rows(summary_rows) |>
    dplyr::arrange(total_loss)
  candidate_summary$candidate_rank <- seq_len(nrow(candidate_summary))
  comparison_long <- dplyr::bind_rows(comparison_rows)
  cor_summary <- dplyr::bind_rows(cor_rows)

  conclusion_candidates <- select_conclusion_candidates(candidate_summary)
  best_id <- conclusion_candidates$candidate_id[[1]]
  utils::write.csv(candidate_summary, file.path(output_dir, "candidate_summary.csv"), row.names = FALSE)
  utils::write.csv(comparison_long, file.path(output_dir, "metric_comparison_long.csv"), row.names = FALSE)
  utils::write.csv(cor_summary, file.path(output_dir, "cor_sign_summary.csv"), row.names = FALSE)
  utils::write.csv(results[[best_id]]$panel, file.path(output_dir, "best_synthetic_panel.csv"), row.names = FALSE)
  utils::write.csv(results[[1L]]$panel, file.path(output_dir, "baseline_synthetic_panel.csv"), row.names = FALSE)

  saveRDS(
    list(
      empirical = empirical_target,
      candidate_summary = candidate_summary,
      metric_comparison_long = comparison_long,
      cor_sign_summary = cor_summary,
      results = results
    ),
    file.path(output_dir, "comparison_bundle.rds")
  )
  make_report(candidate_summary, output_dir, smoke_mode)

  run_summary <- conclusion_candidates[1, , drop = FALSE]
  utils::write.csv(run_summary, file.path(output_dir, "run_summary.csv"), row.names = FALSE)
  log_line("Done. Best candidate: ", run_summary$candidate_label[[1]], " total_loss=", round(run_summary$total_loss[[1]], 4))
}

if (sys.nframe() == 0L) {
  main()
}
