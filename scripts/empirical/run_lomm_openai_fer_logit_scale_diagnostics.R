#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

repo_root <- normalizePath(".", mustWork = TRUE)

source(file.path(repo_root, "scripts", "empirical", "run_lomm_openai_fer_range_geometry_diagnostics.R"))

evaluate_scale_pair <- function(empirical_panel,
                                synthetic_panel,
                                score_cols,
                                scale_label,
                                ord0_embed) {
  empirical_target <- build_property_target_from_panel(
    panel = empirical_panel,
    model_name = paste0("oai-large-retained100-", scale_label),
    score_cols = score_cols,
    ord0_embed = ord0_embed,
    algorithm = "louvain"
  )

  synthetic_target <- build_property_target_from_panel(
    panel = synthetic_panel,
    model_name = paste0("synthetic-retained100-", scale_label),
    score_cols = score_cols,
    ord0_embed = ord0_embed,
    algorithm = "louvain"
  )

  loss <- weighted_loss(empirical_target, synthetic_target)

  list(
    scale_label = scale_label,
    empirical_target = empirical_target,
    synthetic_target = synthetic_target,
    loss = loss
  )
}

summarize_scale_result <- function(result) {
  data.frame(
    scale_label = result$scale_label,
    raw_loss = result$loss$raw_loss,
    latent_loss = result$loss$latent_loss,
    total_loss = result$loss$total_loss,
    empirical_n_dim = result$empirical_target$n_dim,
    synthetic_n_dim = result$synthetic_target$n_dim,
    empirical_community_sizes = paste(result$empirical_target$community_sizes, collapse = "-"),
    synthetic_community_sizes = paste(result$synthetic_target$community_sizes, collapse = "-"),
    stringsAsFactors = FALSE
  )
}

score_range_summary <- function(panel, score_cols, panel_label, scale_label) {
  data.frame(
    panel_label = panel_label,
    scale_label = scale_label,
    channel = score_cols,
    min = vapply(panel[score_cols], min, numeric(1), na.rm = TRUE),
    q25 = vapply(panel[score_cols], stats::quantile, numeric(1), probs = 0.25, na.rm = TRUE, names = FALSE),
    median = vapply(panel[score_cols], stats::median, numeric(1), na.rm = TRUE),
    mean = vapply(panel[score_cols], mean, numeric(1), na.rm = TRUE),
    q75 = vapply(panel[score_cols], stats::quantile, numeric(1), probs = 0.75, na.rm = TRUE, names = FALSE),
    max = vapply(panel[score_cols], max, numeric(1), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

write_logit_report <- function(output_dir,
                               run_summary,
                               metric_comparison_long,
                               eps) {
  main_metrics <- c(
    "std_value", "channel_iqr", "diff_iqr", "lag1_acf",
    "run_length_entropy", "spectral_entropy", "effective_rank",
    "mean_abs_pairwise_cor"
  )

  selected <- metric_comparison_long[
    metric_comparison_long$metric %in% main_metrics,
    ,
    drop = FALSE
  ]
  selected <- selected[order(selected$scale_label, selected$family, selected$metric), , drop = FALSE]

  lines <- c(
    "# LOMM/OpenAI-FER clamped-logit scale diagnostics",
    "",
    sprintf("- Logit clamp epsilon: `%g`", eps),
    "",
    "Scale-level weighted losses:",
    ""
  )

  for (idx in seq_len(nrow(run_summary))) {
    row <- run_summary[idx, , drop = FALSE]
    lines <- c(lines, sprintf(
      "- `%s`: total=%.4f raw=%.4f latent=%.4f n_dim=%s/%s",
      row$scale_label[[1]],
      row$total_loss[[1]],
      row$raw_loss[[1]],
      row$latent_loss[[1]],
      row$empirical_n_dim[[1]],
      row$synthetic_n_dim[[1]]
    ))
  }

  lines <- c(lines, "", "Main metric calls:", "")

  for (idx in seq_len(nrow(selected))) {
    row <- selected[idx, , drop = FALSE]
    lines <- c(lines, sprintf(
      "- `%s` `%s` `%s`: empirical=%.4f synthetic=%.4f max_discrepancy=%.4f call=%s",
      row$scale_label[[1]],
      row$family[[1]],
      row$metric[[1]],
      row$empirical_median[[1]],
      row$synthetic_median[[1]],
      row$max_discrepancy[[1]],
      row$paper_call[[1]]
    ))
  }

  writeLines(lines, file.path(output_dir, "logit_scale_report.md"))
}

main <- function() {
  source_dir <- read_char(
    "LOMM_LOGIT_SOURCE_DIR",
    file.path(repo_root, "empirical_outputs_exploratory", "lomm_openai_fer_observation_ablation_retained100_confirm_resid_unique")
  )
  output_dir <- read_char(
    "LOMM_LOGIT_OUTPUT_DIR",
    file.path(repo_root, "empirical_outputs_exploratory", "lomm_openai_fer_logit_scale_diagnostics")
  )
  ord0_embed <- read_int("LOMM_LOGIT_ORD0_EMBED", 10L)
  eps <- read_num("LOMM_LOGIT_EPS", 1e-4)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  empirical_panel <- utils::read.csv(file.path(source_dir, "empirical_panel_clean.csv"), stringsAsFactors = FALSE)
  synthetic_panel <- utils::read.csv(file.path(source_dir, "best_synthetic_panel.csv"), stringsAsFactors = FALSE)
  score_cols <- intersect(EMPIRICAL_EMOTION_COLUMNS, names(empirical_panel))

  empirical_logit <- logit_transform_score_panel(empirical_panel, score_cols = score_cols, eps = eps)
  synthetic_logit <- logit_transform_score_panel(synthetic_panel, score_cols = score_cols, eps = eps)

  log_line("Evaluating original score scale")
  score_result <- evaluate_scale_pair(
    empirical_panel = empirical_panel,
    synthetic_panel = synthetic_panel,
    score_cols = score_cols,
    scale_label = "score_scale",
    ord0_embed = ord0_embed
  )

  log_line("Evaluating clamped logit scale")
  logit_result <- evaluate_scale_pair(
    empirical_panel = empirical_logit,
    synthetic_panel = synthetic_logit,
    score_cols = score_cols,
    scale_label = "logit_scale",
    ord0_embed = ord0_embed
  )

  results <- list(
    score_scale = score_result,
    logit_scale = logit_result
  )

  run_summary <- dplyr::bind_rows(lapply(results, summarize_scale_result)) |>
    dplyr::arrange(total_loss)

  comparison_rows <- lapply(results, function(result) {
    comparison <- add_paper_calls(result$loss$comparison)
    comparison$scale_label <- result$scale_label
    comparison
  })
  metric_comparison_long <- dplyr::bind_rows(comparison_rows)
  metric_call_summary <- metric_comparison_long |>
    dplyr::count(scale_label, family, paper_call, name = "n_metrics") |>
    dplyr::arrange(scale_label, family, paper_call)

  metric_call_wide <- metric_comparison_long |>
    dplyr::select(
      scale_label, family, metric, empirical_median, synthetic_median,
      max_discrepancy, paper_call
    ) |>
    tidyr::pivot_wider(
      names_from = scale_label,
      values_from = c(empirical_median, synthetic_median, max_discrepancy, paper_call)
    ) |>
    dplyr::arrange(family, metric)

  range_summary <- dplyr::bind_rows(
    score_range_summary(empirical_panel, score_cols, "empirical", "score_scale"),
    score_range_summary(synthetic_panel, score_cols, "synthetic", "score_scale"),
    score_range_summary(empirical_logit, score_cols, "empirical", "logit_scale"),
    score_range_summary(synthetic_logit, score_cols, "synthetic", "logit_scale")
  )

  utils::write.csv(empirical_panel, file.path(output_dir, "empirical_panel_score_scale.csv"), row.names = FALSE)
  utils::write.csv(synthetic_panel, file.path(output_dir, "synthetic_panel_score_scale.csv"), row.names = FALSE)
  utils::write.csv(empirical_logit, file.path(output_dir, "empirical_panel_logit_scale.csv"), row.names = FALSE)
  utils::write.csv(synthetic_logit, file.path(output_dir, "synthetic_panel_logit_scale.csv"), row.names = FALSE)
  utils::write.csv(score_result$empirical_target$latent_panel, file.path(output_dir, "empirical_latent_scores_score_scale.csv"), row.names = FALSE)
  utils::write.csv(score_result$synthetic_target$latent_panel, file.path(output_dir, "synthetic_latent_scores_score_scale.csv"), row.names = FALSE)
  utils::write.csv(logit_result$empirical_target$latent_panel, file.path(output_dir, "empirical_latent_scores_logit_scale.csv"), row.names = FALSE)
  utils::write.csv(logit_result$synthetic_target$latent_panel, file.path(output_dir, "synthetic_latent_scores_logit_scale.csv"), row.names = FALSE)
  utils::write.csv(run_summary, file.path(output_dir, "run_summary.csv"), row.names = FALSE)
  utils::write.csv(metric_comparison_long, file.path(output_dir, "metric_comparison_long.csv"), row.names = FALSE)
  utils::write.csv(metric_call_summary, file.path(output_dir, "metric_call_summary.csv"), row.names = FALSE)
  utils::write.csv(metric_call_wide, file.path(output_dir, "metric_call_wide.csv"), row.names = FALSE)
  utils::write.csv(range_summary, file.path(output_dir, "score_range_summary.csv"), row.names = FALSE)

  saveRDS(
    list(
      run_summary = run_summary,
      metric_comparison_long = metric_comparison_long,
      metric_call_summary = metric_call_summary,
      metric_call_wide = metric_call_wide,
      range_summary = range_summary,
      results = results
    ),
    file.path(output_dir, "comparison_bundle.rds")
  )

  write_logit_report(output_dir, run_summary, metric_comparison_long, eps = eps)
  message(sprintf(
    "Done. Best scale: %s total_loss=%.4f",
    run_summary$scale_label[[1]],
    run_summary$total_loss[[1]]
  ))
}

if (sys.nframe() == 0L) {
  main()
}
