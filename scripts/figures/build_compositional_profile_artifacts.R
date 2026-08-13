#!/usr/bin/env Rscript

figure_script_path <- function() {
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[!is.na(frames) & nzchar(frames)]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot locate figure script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

artifact_stop <- function(...) stop(..., call. = FALSE)

script_path <- figure_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
aggregator <- file.path(repo_root, "scripts", "lomm_1_1", "aggregate_realism_ablation.R")
LOMM11_PAIRED_AGGREGATOR_PATH <- normalizePath(aggregator, mustWork = TRUE)
source(aggregator, local = .GlobalEnv)

profile_order <- c("000", "100", "010", "110", "001", "101", "011", "111")
short_labels <- c(
  `000` = "Identical clips",
  `100` = "Clip baseline",
  `010` = "Clip scale",
  `110` = "Clip baseline + scale",
  `001` = "Activity transition",
  `101` = "Clip baseline + transition",
  `011` = "Clip scale + transition",
  `111` = "Clip baseline + scale\n+ transition"
)
manuscript_profile_labels <- c(
  `000` = "Identical clips",
  `100` = "Clip baseline",
  `010` = "Clip scale",
  `001` = "Activity transition",
  `110` = "Clip baseline + scale",
  `101` = "Clip baseline + activity transition",
  `011` = "Clip scale + activity transition",
  `111` = "Clip baseline + scale + activity transition"
)
family_labels <- c(
  probability = "Scores",
  logratio = "Log-ratio coordinates"
)
view_labels <- c(
  complete_case = "Complete frames only",
  ilr_gap2 = "Interpolated across gaps of up to 2 frames"
)
metric_labels <- c(
  level_median = "Level median",
  level_iqr = "Level IQR",
  difference_iqr = "First-difference IQR",
  lag1_correlation = "Lag-1 autocorrelation",
  run_length_entropy = "Run-length entropy",
  spectral_entropy = "Spectral entropy",
  effective_rank = "Effective rank",
  total_variance = "Total variance"
)
figure_caption <- paste(
  "Discrepancy between simulated and real score series for the eight",
  "configurations, averaged over five cross-validation folds. Points are mean",
  "discrepancies across eight summary statistics within each representation;",
  "smaller values indicate closer agreement with the real scores from 100 video",
  "clips. The diamond marks the clip baseline + scale configuration, and the",
  "dotted line marks the identical-clips estimate in each panel. Bootstrap",
  "intervals are not shown for individual configurations because their",
  "distributions were strongly displaced from the estimates themselves.",
  "Appendix C reports the bootstrap diagnostics."
)
all_views_table_caption <- paste(
  "Discrepancy on held-out clips for all eight configurations, in both the",
  "complete-frames and gap-interpolated analyses. Each estimate averages eight",
  "summary statistics within the stated representation; smaller values indicate",
  "closer agreement with the real scores. Bootstrap means, bias estimates,",
  "basic reverse-percentile intervals, and percentile intervals are retained as",
  "diagnostics. Because they are strongly displaced from the estimates, they are",
  "not used as uncertainty intervals for individual configurations. All",
  "bootstrap summaries are conditional on the settings selected within each",
  "training fold."
)
selected_metric_table_caption <- paste(
  "Discrepancy for each summary statistic under the clip baseline + scale",
  "configuration. Results are shown for the scores themselves and for their",
  "log-ratio coordinates, in both analyses. Smaller values indicate closer",
  "agreement with the real scores. Unit-count columns report the range across",
  "folds and simulation repetitions. Bootstrap means, bias estimates, and both",
  "interval constructions are reported as diagnostics rather than as uncertainty",
  "intervals for the individual discrepancies."
)

parse_args <- function(args) {
  parsed <- list(aggregate_dir = NULL)
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token == "--aggregate-dir") {
      if (index == length(args)) artifact_stop("Missing directory after --aggregate-dir.")
      parsed$aggregate_dir <- args[[index + 1L]]
      index <- index + 2L
    } else {
      artifact_stop("Unknown argument: ", token)
    }
  }
  if (is.null(parsed$aggregate_dir)) {
    artifact_stop("Provide the validated aggregate directory with --aggregate-dir.")
  }
  parsed
}

atomic_write_csv <- function(frame, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid()))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) artifact_stop("Failed to publish table: ", path)
}

atomic_write_text <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid()))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) artifact_stop("Failed to publish caption: ", path)
}

scientific_table <- function(primary) {
  primary <- primary[order(
    match(primary$family, names(family_labels)),
    match(primary$profile_id, profile_order)
  ), , drop = FALSE]
  data.frame(
    Representation = unname(family_labels[primary$family]),
    Configuration = unname(manuscript_profile_labels[primary$profile_id]),
    `Mean discrepancy` = primary$estimate,
    `Monte Carlo SE` = primary$monte_carlo_se,
    `Bootstrap mean` = primary$bootstrap_mean,
    `Bootstrap bias` = primary$bootstrap_bias,
    `Basic 95% interval: lower` = primary$basic_conf_low,
    `Basic 95% interval: upper` = primary$basic_conf_high,
    `Percentile 95% interval: lower` = primary$percentile_conf_low,
    `Percentile 95% interval: upper` = primary$percentile_conf_high,
    `Metrics averaged` = primary$metric_count,
    `Test clips` = primary$n_clips,
    Folds = primary$n_folds,
    `Simulated datasets per fold and configuration` = primary$n_reps,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

scientific_all_views_table <- function(profiles) {
  profiles <- profiles[order(
    match(profiles$analysis_view, names(view_labels)),
    match(profiles$family, names(family_labels)),
    match(profiles$profile_id, profile_order)
  ), , drop = FALSE]
  data.frame(
    Analysis = unname(view_labels[profiles$analysis_view]),
    Representation = unname(family_labels[profiles$family]),
    Configuration = unname(manuscript_profile_labels[profiles$profile_id]),
    `Mean discrepancy` = profiles$estimate,
    `Monte Carlo SE` = profiles$monte_carlo_se,
    `Bootstrap mean` = profiles$bootstrap_mean,
    `Bootstrap bias` = profiles$bootstrap_bias,
    `Basic 95% interval: lower` = profiles$basic_conf_low,
    `Basic 95% interval: upper` = profiles$basic_conf_high,
    `Percentile 95% interval: lower` = profiles$percentile_conf_low,
    `Percentile 95% interval: upper` = profiles$percentile_conf_high,
    `Metrics averaged` = profiles$metric_count,
    `Test clips` = profiles$n_clips,
    Folds = profiles$n_folds,
    `Simulated datasets per fold and configuration` = profiles$n_reps,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

scientific_selected_metric_table <- function(profile_metrics) {
  selected <- profile_metrics[
    profile_metrics$profile_id == "110", , drop = FALSE
  ]
  selected <- selected[order(
    match(selected$analysis_view, names(view_labels)),
    match(selected$family, names(family_labels)),
    match(selected$metric, names(metric_labels))
  ), , drop = FALSE]
  data.frame(
    Analysis = unname(view_labels[selected$analysis_view]),
    Representation = unname(family_labels[selected$family]),
    Metric = unname(metric_labels[selected$metric]),
    `Mean discrepancy` = selected$estimate,
    `Monte Carlo SE` = selected$monte_carlo_se,
    `Bootstrap mean` = selected$bootstrap_mean,
    `Bootstrap bias` = selected$bootstrap_bias,
    `Basic 95% interval: lower` = selected$basic_conf_low,
    `Basic 95% interval: upper` = selected$basic_conf_high,
    `Percentile 95% interval: lower` = selected$percentile_conf_low,
    `Percentile 95% interval: upper` = selected$percentile_conf_high,
    `Empirical units per fold: minimum` = selected$empirical_units_min,
    `Empirical units per fold: maximum` = selected$empirical_units_max,
    `Simulated units per fold: minimum` = selected$synthetic_units_min,
    `Simulated units per fold: maximum` = selected$synthetic_units_max,
    `Test clips` = selected$n_clips,
    Folds = selected$n_folds,
    `Simulated datasets per fold and configuration` = selected$n_reps,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

draw_profile_panel <- function(data, panel_title, limits) {
  ordered <- data[match(profile_order, data$profile_id), , drop = FALSE]
  y <- rev(seq_along(profile_order))
  graphics::plot(
    NA_real_, NA_real_, xlim = limits, ylim = c(0.5, 8.5), yaxt = "n",
    xlab = "Mean discrepancy on held-out clips (smaller is better)", ylab = "",
    bty = "l", main = panel_title
  )
  grid_values <- pretty(limits, n = 5L)
  graphics::abline(v = grid_values, col = "grey92", lwd = 0.7)
  homogeneous <- ordered$estimate[ordered$profile_id == "000"]
  graphics::abline(v = homogeneous, col = "grey55", lty = 3L, lwd = 1L)
  selected <- ordered$profile_id == "110"
  point_colors <- ifelse(selected, "#D55E00", "#0072B2")
  point_shapes <- ifelse(selected, 18L, ifelse(ordered$profile_id == "000", 1L, 16L))
  graphics::points(ordered$estimate, y, pch = point_shapes, col = point_colors, cex = 1.05)
  graphics::axis(2L, at = y, labels = unname(short_labels[ordered$profile_id]), las = 1L)
  graphics::box(bty = "l")
}

draw_figure <- function(primary, path, device = c("pdf", "png")) {
  device <- match.arg(device)
  temporary <- file.path(dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid()))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  if (device == "pdf") {
    grDevices::pdf(
      temporary, width = 7.1, height = 6.4, useDingbats = FALSE,
      timestamp = FALSE
    )
  } else {
    grDevices::png(
      temporary, width = 7.1, height = 6.4, units = "in", res = 300,
      type = "cairo-png"
    )
  }
  old_parameters <- graphics::par(no.readonly = TRUE)
  graphics::par(
    mfrow = c(2L, 1L), mar = c(3.4, 12.2, 2.1, 0.8), oma = c(0.4, 0, 0, 0),
    family = "sans", cex = 0.82, mgp = c(2.0, 0.55, 0), tcl = -0.25
  )
  limits <- range(primary$estimate, finite = TRUE)
  padding <- max(diff(limits) * 0.08, 0.005)
  limits <- limits + c(-padding, padding)
  for (family_name in names(family_labels)) {
    draw_profile_panel(
      primary[primary$family == family_name, , drop = FALSE],
      family_labels[[family_name]], limits
    )
  }
  graphics::par(old_parameters)
  grDevices::dev.off()
  if (!file.rename(temporary, path)) artifact_stop("Failed to publish figure: ", path)
}

run_artifact_build <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- parse_args(args)
  validated <- pa_validate_published(parsed$aggregate_dir)
  primary <- validated$profiles[
    validated$profiles$analysis_view == "complete_case", , drop = FALSE
  ]
  if (nrow(primary) != 16L ||
      !setequal(primary$profile_id, profile_order) ||
      !setequal(primary$family, names(family_labels))) {
    artifact_stop("The complete-case profile result is incomplete.")
  }
  table <- scientific_table(primary)
  all_views_table <- scientific_all_views_table(validated$profiles)
  selected_metric_table <- scientific_selected_metric_table(
    validated$profile_metrics
  )
  forbidden <- paste(
    "lomm[_ -]?[0-9]|revision|config_hash|profile_id|analysis_view|",
    "complete_case|ilr_gap2",
    sep = ""
  )
  displayed_text <- c(
    names(table), unlist(table[vapply(table, is.character, logical(1))]),
    names(all_views_table),
    unlist(all_views_table[vapply(all_views_table, is.character, logical(1))]),
    names(selected_metric_table),
    unlist(selected_metric_table[
      vapply(selected_metric_table, is.character, logical(1))
    ]),
    figure_caption, family_labels, view_labels, metric_labels, short_labels,
    manuscript_profile_labels,
    all_views_table_caption, selected_metric_table_caption
  )
  if (any(grepl(forbidden, displayed_text, ignore.case = TRUE, perl = TRUE))) {
    artifact_stop("A manuscript-facing label exposes an internal identifier.")
  }
  if (any(table$Configuration %in% LOMM11_PAIRED_VARIANTS)) {
    artifact_stop("A manuscript-facing configuration uses an internal factorial code.")
  }
  table_path <- file.path(repo_root, "paper", "tables", "compositional_profile_discrepancy.csv")
  all_views_table_path <- file.path(
    repo_root, "paper", "tables", "compositional_profile_discrepancy_all_views.csv"
  )
  selected_metric_table_path <- file.path(
    repo_root, "paper", "tables", "compositional_profile_selected_metrics.csv"
  )
  caption_path <- file.path(
    repo_root, "paper", "tables", "compositional_profile_discrepancy_caption.txt"
  )
  all_views_caption_path <- file.path(
    repo_root, "paper", "tables",
    "compositional_profile_discrepancy_all_views_caption.txt"
  )
  selected_metric_caption_path <- file.path(
    repo_root, "paper", "tables",
    "compositional_profile_selected_metrics_caption.txt"
  )
  pdf_path <- file.path(repo_root, "paper", "figures", "compositional_profile_discrepancy.pdf")
  png_path <- file.path(repo_root, "paper", "figures", "compositional_profile_discrepancy.png")
  atomic_write_csv(table, table_path)
  atomic_write_csv(all_views_table, all_views_table_path)
  atomic_write_csv(selected_metric_table, selected_metric_table_path)
  atomic_write_text(figure_caption, caption_path)
  atomic_write_text(all_views_table_caption, all_views_caption_path)
  atomic_write_text(selected_metric_table_caption, selected_metric_caption_path)
  draw_figure(primary, pdf_path, "pdf")
  draw_figure(primary, png_path, "png")
  cat(
    "Published compositional-profile artifacts:\n",
    table_path, "\n", all_views_table_path, "\n", selected_metric_table_path,
    "\n", caption_path, "\n", all_views_caption_path, "\n",
    selected_metric_caption_path, "\n", pdf_path, "\n", png_path, "\n",
    sep = ""
  )
  invisible(list(
    table = table_path, all_views_table = all_views_table_path,
    selected_metric_table = selected_metric_table_path, caption = caption_path,
    all_views_caption = all_views_caption_path,
    selected_metric_caption = selected_metric_caption_path,
    pdf = pdf_path, png = png_path
  ))
}

if (sys.nframe() == 0L) {
  tryCatch(
    run_artifact_build(),
    error = function(condition) {
      message("ERROR: ", conditionMessage(condition))
      quit(save = "no", status = 1L, runLast = FALSE)
    }
  )
}
