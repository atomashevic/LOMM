#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

repo_root <- normalizePath(".", mustWork = TRUE)

source(file.path(repo_root, "scripts", "empirical", "run_lomm_openai_fer_observation_ablation.R"))
source(file.path(repo_root, "scripts", "empirical", "run_lomm_openai_fer_range_geometry_diagnostics.R"))

evaluate_normalized_panel <- function(empirical_target,
                                      synthetic_panel,
                                      candidate_label,
                                      ord0_embed) {
  synthetic_target <- build_property_target_from_panel(
    panel = synthetic_panel,
    model_name = candidate_label,
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

summarize_normalized_result <- function(result,
                                        candidate_id,
                                        candidate_label,
                                        method,
                                        alpha,
                                        source_label,
                                        empirical_target) {
  data.frame(
    candidate_id = candidate_id,
    candidate_label = candidate_label,
    method = method,
    alpha = alpha,
    source_label = source_label,
    raw_loss = result$loss$raw_loss,
    latent_loss = result$loss$latent_loss,
    total_loss = result$loss$total_loss,
    empirical_n_dim = empirical_target$n_dim,
    synthetic_n_dim = result$target$n_dim,
    empirical_community_sizes = paste(empirical_target$community_sizes, collapse = "-"),
    synthetic_community_sizes = paste(result$target$community_sizes, collapse = "-"),
    stringsAsFactors = FALSE
  )
}

append_normalized_result <- function(results,
                                     summary_rows,
                                     comparison_rows,
                                     empirical_target,
                                     result,
                                     candidate_id,
                                     candidate_label,
                                     method,
                                     alpha,
                                     source_label) {
  summary_rows[[candidate_id]] <- summarize_normalized_result(
    result = result,
    candidate_id = candidate_id,
    candidate_label = candidate_label,
    method = method,
    alpha = alpha,
    source_label = source_label,
    empirical_target = empirical_target
  )

  comparison <- add_paper_calls(result$loss$comparison)
  comparison$candidate_id <- candidate_id
  comparison$candidate_label <- candidate_label
  comparison$method <- method
  comparison$alpha <- alpha
  comparison_rows[[candidate_id]] <- comparison

  results[[candidate_id]] <- result

  list(
    results = results,
    summary_rows = summary_rows,
    comparison_rows = comparison_rows
  )
}

write_normalization_report <- function(output_dir,
                                       candidate_summary,
                                       metric_comparison_long) {
  best <- candidate_summary[order(candidate_summary$total_loss), , drop = FALSE][1, , drop = FALSE]
  main_metrics <- c(
    "channel_iqr", "diff_iqr", "lag1_acf", "run_length_entropy",
    "spectral_entropy", "effective_rank", "mean_abs_pairwise_cor"
  )
  best_metrics <- metric_comparison_long[
    metric_comparison_long$candidate_id == best$candidate_id[[1]] &
      metric_comparison_long$metric %in% main_metrics,
    ,
    drop = FALSE
  ]

  lines <- c(
    "# LOMM/OpenAI-FER score-normalization diagnostics",
    "",
    sprintf("- Best candidate: `%s`", best$candidate_label[[1]]),
    sprintf("- Method: `%s`; alpha: `%.2f`", best$method[[1]], best$alpha[[1]]),
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

  writeLines(lines, file.path(output_dir, "score_normalization_report.md"))
}

main <- function() {
  source_dir <- read_char(
    "LOMM_SCORE_NORM_SOURCE_DIR",
    file.path(repo_root, "empirical_outputs_exploratory", "lomm_openai_fer_observation_ablation_retained100_confirm_resid_unique")
  )
  output_dir <- read_char(
    "LOMM_SCORE_NORM_OUTPUT_DIR",
    file.path(repo_root, "empirical_outputs_exploratory", "lomm_openai_fer_score_normalization_diagnostics")
  )
  ord0_embed <- read_int("LOMM_SCORE_NORM_ORD0_EMBED", 10L)
  alpha_vals <- read_num_vec("LOMM_SCORE_NORM_ALPHA", c(0.25, 0.50, 0.75, 1.00))
  methods <- trimws(strsplit(read_char("LOMM_SCORE_NORM_METHODS", "affine_iqr,empirical_quantile"), ",", fixed = TRUE)[[1]])
  source_label <- read_char("LOMM_SCORE_NORM_SOURCE_LABEL", "confirmed_resid_0.10")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  empirical_panel <- utils::read.csv(file.path(source_dir, "empirical_panel_clean.csv"), stringsAsFactors = FALSE)
  synthetic_panel <- utils::read.csv(file.path(source_dir, "best_synthetic_panel.csv"), stringsAsFactors = FALSE)

  score_cols <- intersect(EMPIRICAL_EMOTION_COLUMNS, names(empirical_panel))
  empirical_target <- build_property_target_from_panel(
    panel = empirical_panel,
    model_name = "oai-large-retained100",
    score_cols = score_cols,
    ord0_embed = ord0_embed,
    algorithm = "louvain"
  )

  utils::write.csv(empirical_panel, file.path(output_dir, "empirical_panel_clean.csv"), row.names = FALSE)
  utils::write.csv(synthetic_panel, file.path(output_dir, "source_synthetic_panel.csv"), row.names = FALSE)

  results <- list()
  summary_rows <- list()
  comparison_rows <- list()
  candidate_id <- 1L

  baseline <- evaluate_normalized_panel(
    empirical_target = empirical_target,
    synthetic_panel = synthetic_panel,
    candidate_label = "source_confirmed",
    ord0_embed = ord0_embed
  )
  appended <- append_normalized_result(
    results, summary_rows, comparison_rows,
    empirical_target = empirical_target,
    result = baseline,
    candidate_id = candidate_id,
    candidate_label = "source_confirmed",
    method = "source",
    alpha = 0,
    source_label = source_label
  )
  results <- appended$results
  summary_rows <- appended$summary_rows
  comparison_rows <- appended$comparison_rows
  candidate_id <- candidate_id + 1L

  for (method in methods) {
    for (alpha in alpha_vals) {
      candidate_label <- sprintf("%s_alpha_%0.2f", method, alpha)
      message(sprintf("Evaluating %s", candidate_label))

      normalized_panel <- switch(
        method,
        affine_iqr = affine_iqr_normalize_panel(
          synthetic_panel = synthetic_panel,
          empirical_panel = empirical_panel,
          score_cols = score_cols,
          alpha = alpha
        ),
        empirical_quantile = empirical_quantile_normalize_panel(
          synthetic_panel = synthetic_panel,
          empirical_panel = empirical_panel,
          score_cols = score_cols,
          alpha = alpha
        ),
        stop(sprintf("Unknown normalization method: %s", method), call. = FALSE)
      )

      result <- evaluate_normalized_panel(
        empirical_target = empirical_target,
        synthetic_panel = normalized_panel,
        candidate_label = candidate_label,
        ord0_embed = ord0_embed
      )
      appended <- append_normalized_result(
        results, summary_rows, comparison_rows,
        empirical_target = empirical_target,
        result = result,
        candidate_id = candidate_id,
        candidate_label = candidate_label,
        method = method,
        alpha = alpha,
        source_label = source_label
      )
      results <- appended$results
      summary_rows <- appended$summary_rows
      comparison_rows <- appended$comparison_rows
      candidate_id <- candidate_id + 1L
    }
  }

  candidate_summary <- dplyr::bind_rows(summary_rows) |>
    dplyr::arrange(total_loss)
  candidate_summary$candidate_rank <- seq_len(nrow(candidate_summary))
  metric_comparison_long <- dplyr::bind_rows(comparison_rows)
  channel_frontier <- candidate_channel_iqr_frontier(candidate_summary, metric_comparison_long)

  best_id <- candidate_summary$candidate_id[[1]]
  utils::write.csv(candidate_summary, file.path(output_dir, "candidate_summary.csv"), row.names = FALSE)
  utils::write.csv(metric_comparison_long, file.path(output_dir, "metric_comparison_long.csv"), row.names = FALSE)
  utils::write.csv(channel_frontier, file.path(output_dir, "channel_iqr_frontier.csv"), row.names = FALSE)
  utils::write.csv(candidate_summary[candidate_summary$candidate_id == best_id, ],
                   file.path(output_dir, "run_summary.csv"), row.names = FALSE)
  utils::write.csv(results[[best_id]]$panel, file.path(output_dir, "best_synthetic_panel.csv"), row.names = FALSE)

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

  write_normalization_report(output_dir, candidate_summary, metric_comparison_long)
  message(sprintf(
    "Done. Best score-normalization candidate: %s total_loss=%.4f",
    candidate_summary$candidate_label[[1]],
    candidate_summary$total_loss[[1]]
  ))
}

if (sys.nframe() == 0L) {
  main()
}
