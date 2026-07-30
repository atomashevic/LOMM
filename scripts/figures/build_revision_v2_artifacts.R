#!/usr/bin/env Rscript

revision_v2_parse_cli <- function(arguments) {
  value <- list(aggregate_dir = NULL, output_root = NULL)
  index <- 1L
  while (index <= length(arguments)) {
    flag <- arguments[[index]]
    if (!flag %in% c("--aggregate-dir", "--output-root") || index == length(arguments)) {
      stop("Usage: build_revision_v2_artifacts.R --aggregate-dir DIR --output-root DIR.", call. = FALSE)
    }
    name <- sub("^--", "", flag)
    name <- gsub("-", "_", name, fixed = TRUE)
    value[[name]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (is.null(value$aggregate_dir) || is.null(value$output_root)) {
    stop("Both --aggregate-dir and --output-root are required.", call. = FALSE)
  }
  value
}

revision_v2_read_csv <- function(directory, filename, columns) {
  path <- file.path(directory, filename)
  if (!file.exists(path) || file.info(path)$size <= 0L) {
    stop("Missing revision-v2 aggregate artifact: ", path, call. = FALSE)
  }
  data <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      filename, " omits required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  data
}

revision_v2_require_aggregate_contract <- function(directory) {
  required <- c(
    "revision_estimand_spec.csv",
    "upstream_artifact_manifest.csv",
    "revision_results_long.csv",
    "identity_validation.csv",
    "revision_estimand_results.csv",
    "revision_paired_differences.csv"
  )
  paths <- file.path(directory, required)
  missing <- required[!file.exists(paths) | file.info(paths)$size <= 0L]
  if (length(missing) > 0L) {
    stop(
      "The selected aggregate directory is incomplete: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(required)
}

revision_v2_select <- function(data, conditions) {
  keep <- rep(TRUE, nrow(data))
  for (name in names(conditions)) {
    keep <- keep & !is.na(data[[name]]) & data[[name]] %in% conditions[[name]]
  }
  data[keep, , drop = FALSE]
}

revision_v2_nonempty <- function(data, label) {
  if (nrow(data) == 0L) {
    stop("No revision-v2 aggregate rows support ", label, ".", call. = FALSE)
  }
  data
}

revision_v2_write_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  invisible(path)
}

revision_v2_interval_plot <- function(data, labels, title, xlab, zero = FALSE) {
  estimate <- suppressWarnings(as.numeric(data$estimate))
  low <- suppressWarnings(as.numeric(data$conf_low))
  high <- suppressWarnings(as.numeric(data$conf_high))
  finite <- is.finite(estimate)
  if (!any(finite)) {
    stop("Figure has no finite revision-v2 estimates: ", title, call. = FALSE)
  }
  estimate <- estimate[finite]
  low <- low[finite]
  high <- high[finite]
  labels <- labels[finite]
  low[!is.finite(low)] <- estimate[!is.finite(low)]
  high[!is.finite(high)] <- estimate[!is.finite(high)]
  order_index <- order(estimate)
  estimate <- estimate[order_index]
  low <- low[order_index]
  high <- high[order_index]
  labels <- labels[order_index]
  limits <- range(c(low, high, if (zero) 0 else numeric()), finite = TRUE)
  graphics::par(mar = c(4.2, 12.5, 3.2, 1.2))
  graphics::plot(
    estimate,
    seq_along(estimate),
    xlim = limits,
    yaxt = "n",
    ylab = "",
    xlab = xlab,
    pch = 19,
    col = "#0072B2",
    main = title
  )
  graphics::segments(low, seq_along(estimate), high, seq_along(estimate), lwd = 1.5)
  graphics::axis(2, at = seq_along(estimate), labels = labels, las = 1, cex.axis = 0.72)
  if (zero) {
    graphics::abline(v = 0, lty = 2, col = "grey45")
  }
  graphics::box()
}

revision_v2_write_figure <- function(stem, output_root, plotter) {
  figure_dir <- file.path(output_root, "figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(file.path(figure_dir, paste0(stem, ".pdf")), width = 8.4, height = 6.0)
  tryCatch(plotter(), finally = grDevices::dev.off())
  grDevices::png(
    file.path(figure_dir, paste0(stem, ".png")),
    width = 1680,
    height = 1200,
    res = 200
  )
  tryCatch(plotter(), finally = grDevices::dev.off())
}

revision_v2_build_tables <- function(estimands, paired, long, output_root) {
  table_dir <- file.path(output_root, "tables")
  empirical <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "empirical_cv",
    summary_level = "heldout_clip_cluster",
    contribution = "max_discrepancy"
  )), "the empirical realism table")
  revision_v2_write_csv(
    empirical,
    file.path(table_dir, "empirical_realism_table.csv")
  )

  overall <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "overall",
    community_algorithm = "louvain"
  )), "the overall method table")
  revision_v2_write_csv(overall, file.path(table_dir, "table1_method_overall.csv"))

  scenario <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "scenario_family",
    community_algorithm = "louvain"
  )), "the condition key")
  condition_key <- unique(scenario[, c("scenario_family", "stability_class"), drop = FALSE])
  condition_key$condition_code <- sprintf("C%02d", seq_len(nrow(condition_key)))
  revision_v2_write_csv(
    condition_key[, c("condition_code", "scenario_family", "stability_class")],
    file.path(table_dir, "figure5_condition_key.csv")
  )

  challenges <- revision_v2_nonempty(revision_v2_select(paired, list(
    comparison_type = "challenge_minus_ideal",
    community_algorithm = "louvain"
  )), "the challenge-effects table")
  revision_v2_write_csv(
    challenges,
    file.path(table_dir, "figure6_factor_effects_table.csv")
  )
  stability <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "stability",
    community_algorithm = "louvain"
  )), "the stability-screen table")
  revision_v2_write_csv(
    stability,
    file.path(table_dir, "figure6_factor_effects_eta_screen.csv")
  )

  score_quality <- overall[overall$estimand_id %in% c(
    "adjusted_rand_index",
    "variation_information",
    "assignment_accuracy_conditional",
    "score_correlation_marginal",
    "score_nrmse_marginal"
  ), , drop = FALSE]
  revision_v2_nonempty(score_quality, "the score-quality table")
  revision_v2_write_csv(
    score_quality,
    file.path(table_dir, "table2_score_quality_marginal_means.csv")
  )

  runtime <- overall[overall$estimand_id %in% c(
    "completion_within_900s",
    "restricted_compute_900s",
    "runtime_median_successful",
    "runtime_iqr_successful"
  ), , drop = FALSE]
  revision_v2_nonempty(runtime, "the runtime-feasibility table")
  revision_v2_write_csv(
    runtime,
    file.path(table_dir, "table3_runtime_feasibility.csv")
  )
  accounting <- stats::aggregate(
    rep(1L, nrow(long)),
    long[, c(".source_job", "status"), drop = FALSE],
    sum
  )
  names(accounting)[[3L]] <- "rows"
  revision_v2_write_csv(
    accounting,
    file.path(table_dir, "table3_runtime_feasibility_accounting.csv")
  )
}

revision_v2_build_figures <- function(estimands, paired, output_root) {
  empirical <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "empirical_cv",
    summary_level = "heldout_clip_cluster",
    contribution = "max_discrepancy"
  )), "Figure 4")
  revision_v2_write_figure("figure4_empirical_realism", output_root, function() {
    labels <- paste(empirical$empirical_family, empirical$empirical_metric, sep = ": ")
    revision_v2_interval_plot(
      empirical,
      labels,
      "Held-out empirical realism",
      "Scaled absolute discrepancy"
    )
  })

  recovery <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "overall",
    community_algorithm = "louvain",
    estimand_id = "p_usable_and_correct"
  )), "Figure 5")
  revision_v2_write_figure("figure5_dimension_recovery", output_root, function() {
    revision_v2_interval_plot(
      recovery,
      recovery$method_id,
      "Status-inclusive dimension recovery",
      "Probability usable and correct"
    )
  })

  challenges <- revision_v2_nonempty(revision_v2_select(paired, list(
    comparison_type = "challenge_minus_ideal",
    community_algorithm = "louvain",
    estimand_id = "p_usable_and_correct"
  )), "Figure 6")
  revision_v2_write_figure("figure6_factor_effects", output_root, function() {
    revision_v2_interval_plot(
      challenges,
      challenges$left_label,
      "Measurement-challenge effects",
      "Paired difference from ideal",
      zero = TRUE
    )
  })

  runtime <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "overall",
    community_algorithm = "louvain",
    estimand_id = "completion_within_900s"
  )), "Figure 7")
  revision_v2_write_figure("figure7_runtime_feasibility", output_root, function() {
    revision_v2_interval_plot(
      runtime,
      runtime$method_id,
      "Completion within the 900-second budget",
      "Completion probability"
    )
  })
}

revision_v2_main <- function() {
  cli <- revision_v2_parse_cli(commandArgs(trailingOnly = TRUE))
  aggregate_dir <- normalizePath(cli$aggregate_dir, mustWork = TRUE)
  output_root <- normalizePath(cli$output_root, mustWork = FALSE)
  revision_v2_require_aggregate_contract(aggregate_dir)
  estimands <- revision_v2_read_csv(
    aggregate_dir,
    "revision_estimand_results.csv",
    c(
      "summary_level", "analysis_scope", "method_id", "community_algorithm",
      "stability_class", "estimand_id", "estimate", "conf_low", "conf_high",
      "empirical_family", "empirical_metric", "contribution"
    )
  )
  paired <- revision_v2_read_csv(
    aggregate_dir,
    "revision_paired_differences.csv",
    c(
      "comparison_type", "left_label", "right_label", "community_algorithm",
      "estimand_id", "estimate", "conf_low", "conf_high"
    )
  )
  long <- revision_v2_read_csv(
    aggregate_dir,
    "revision_results_long.csv",
    c(".source_job", "status")
  )
  revision_v2_build_tables(estimands, paired, long, output_root)
  revision_v2_build_figures(estimands, paired, output_root)
}

if (sys.nframe() == 0L) {
  revision_v2_main()
}
