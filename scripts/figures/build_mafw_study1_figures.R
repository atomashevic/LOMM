#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

script_path <- function() {
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

stop_figure <- function(...) stop(..., call. = FALSE)

repo_root <- normalizePath(
  file.path(dirname(script_path()), "..", ".."), mustWork = TRUE
)
input_dir <- file.path(
  repo_root, "simulation_outputs", "lomm_1_1", "mafw_clip_level_benchmark"
)
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")

input_paths <- list(
  clip = file.path(input_dir, "clip_level_benchmark.csv"),
  metric = file.path(input_dir, "per_metric_benchmark.csv"),
  success = file.path(input_dir, "_SUCCESS.json")
)
output_paths <- list(
  main_pdf = file.path(figure_dir, "figure4_mafw_clip_benchmark.pdf"),
  main_png = file.path(figure_dir, "figure4_mafw_clip_benchmark.png"),
  appendix_pdf = file.path(figure_dir, "figureC1_mafw_metric_benchmark.pdf"),
  appendix_png = file.path(figure_dir, "figureC1_mafw_metric_benchmark.png"),
  clip_table = file.path(table_dir, "mafw_clip_benchmark.csv"),
  metric_table = file.path(table_dir, "mafw_metric_benchmark.csv"),
  main_caption = file.path(table_dir, "figure4_mafw_clip_benchmark_caption.txt"),
  appendix_caption = file.path(
    table_dir, "figureC1_mafw_metric_benchmark_caption.txt"
  )
)

required_clip_columns <- c(
  "source", "replicates", "plausibility_q025", "plausibility_median",
  "plausibility_q975", "coverage_q025", "coverage_median", "coverage_q975"
)
required_metric_columns <- c(
  "representation", "metric", "metric_label", "lomm_discrepancy",
  "static_discrepancy", "reference_lower", "reference_median",
  "reference_upper", "within_reference_upper"
)

validate_inputs <- function(clip, metric) {
  missing_files <- unlist(input_paths, use.names = FALSE)[
    !file.exists(unlist(input_paths, use.names = FALSE))
  ]
  if (length(missing_files) > 0L) {
    stop_figure("Missing completed benchmark inputs: ", paste(missing_files, collapse = ", "))
  }
  if (!all(required_clip_columns %in% names(clip))) {
    stop_figure("Clip-level benchmark columns are incomplete.")
  }
  if (!all(required_metric_columns %in% names(metric))) {
    stop_figure("Metric-level benchmark columns are incomplete.")
  }
  expected_sources <- c("LOMM", "MAFW reference", "Static logistic-normal")
  if (!setequal(as.character(clip$source), expected_sources) || nrow(clip) != 3L) {
    stop_figure("Clip-level benchmark sources are incomplete.")
  }
  expected_representations <- c("Scores", "Log-ratio coordinates")
  if (!setequal(as.character(metric$representation), expected_representations) ||
      nrow(metric) != 22L ||
      anyDuplicated(paste(metric$representation, metric$metric, sep = "\r"))) {
    stop_figure("Metric-level benchmark must contain 22 unique feature cells.")
  }
  numeric_clip <- unlist(clip[setdiff(required_clip_columns, "source")], use.names = FALSE)
  numeric_metric <- unlist(metric[c(
    "lomm_discrepancy", "static_discrepancy", "reference_lower",
    "reference_median", "reference_upper"
  )], use.names = FALSE)
  if (any(!is.finite(numeric_clip)) || any(!is.finite(numeric_metric))) {
    stop_figure("Benchmark summaries contain non-finite values.")
  }
  if (any(metric$reference_lower > metric$reference_median) ||
      any(metric$reference_median > metric$reference_upper)) {
    stop_figure("Metric reference intervals are not ordered.")
  }
  invisible(TRUE)
}

atomic_write_csv <- function(frame, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(
    dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid())
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) stop_figure("Failed to publish ", path)
}

atomic_write_text <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(
    dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid())
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) stop_figure("Failed to publish ", path)
}

atomic_save_plot <- function(plot, path, width, height, dpi = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  extension <- tools::file_ext(path)
  temporary <- file.path(
    dirname(path),
    paste0(".", tools::file_path_sans_ext(basename(path)),
           ".tmp-", Sys.getpid(), ".", extension)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  if (identical(extension, "pdf")) {
    ggsave(
      temporary, plot, width = width, height = height,
      device = grDevices::cairo_pdf, bg = "white"
    )
  } else {
    ggsave(
      temporary, plot, width = width, height = height,
      dpi = dpi, bg = "white"
    )
  }
  if (!file.rename(temporary, path)) stop_figure("Failed to publish ", path)
}

house_colors <- c(
  `MAFW reference` = "#5B6573",
  LOMM = "#0072B2",
  `Static logistic-normal` = "#D55E00"
)

clip_plot <- function(clip) {
  rows <- rbind(
    data.frame(
      source = clip$source,
      measure = "Coverage",
      lower = clip$coverage_q025,
      estimate = clip$coverage_median,
      upper = clip$coverage_q975,
      stringsAsFactors = FALSE
    ),
    data.frame(
      source = clip$source,
      measure = "Plausibility",
      lower = clip$plausibility_q025,
      estimate = clip$plausibility_median,
      upper = clip$plausibility_q975,
      stringsAsFactors = FALSE
    )
  )
  rows$source <- factor(
    rows$source,
    levels = c("MAFW reference", "LOMM", "Static logistic-normal")
  )
  rows$measure <- factor(rows$measure, levels = c("Coverage", "Plausibility"))

  ggplot(rows, aes(x = estimate, y = source, color = source)) +
    geom_errorbar(
      aes(xmin = lower, xmax = upper), orientation = "y",
      width = 0.16, linewidth = 0.55
    ) +
    geom_point(size = 2.2) +
    facet_wrap(~measure, nrow = 1L) +
    scale_color_manual(values = house_colors, guide = "none") +
    scale_x_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.25),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(x = "Proportion within real-reference threshold", y = NULL) +
    theme_classic(base_size = 9, base_family = "sans") +
    theme(
      axis.text = element_text(color = "#24313D"),
      axis.title = element_text(color = "#24313D"),
      strip.background = element_rect(fill = "#F1F4F6", color = NA),
      strip.text = element_text(face = "bold", hjust = 0),
      panel.spacing.x = grid::unit(1.1, "lines"),
      plot.margin = margin(6, 8, 6, 6)
    )
}

metric_plot <- function(metric) {
  metric_order <- c(
    "level_median", "level_iqr", "difference_iqr", "lag1_correlation",
    "run_length_entropy", "sample_entropy", "permutation_entropy",
    "spectral_entropy", "effective_rank", "robust_level_variance",
    "robust_change_variance"
  )
  metric_labels <- c(
    level_median = "Median level",
    level_iqr = "Level IQR",
    difference_iqr = "First-difference IQR",
    lag1_correlation = "Lag-1 autocorrelation",
    run_length_entropy = "Run-length entropy",
    sample_entropy = "Sample entropy",
    permutation_entropy = "Permutation entropy",
    spectral_entropy = "Spectral entropy",
    effective_rank = "Effective rank",
    robust_level_variance = "Robust level variance",
    robust_change_variance = "Robust change variance"
  )
  if (!setequal(as.character(metric$metric), metric_order)) {
    stop_figure("Metrics differ from the expected appendix order.")
  }
  metric$metric_label <- factor(
    unname(metric_labels[metric$metric]),
    levels = rev(unname(metric_labels[metric_order]))
  )
  metric$representation <- factor(
    metric$representation, levels = c("Scores", "Log-ratio coordinates")
  )

  ggplot(metric, aes(y = metric_label)) +
    geom_errorbar(
      aes(xmin = reference_lower, xmax = reference_upper),
      orientation = "y", width = 0.34, linewidth = 0.85,
      color = "#CBD2D8"
    ) +
    geom_point(
      aes(x = reference_median, color = "MAFW reference"), size = 1.65
    ) +
    geom_point(
      aes(x = lomm_discrepancy, color = "LOMM"), size = 2.15
    ) +
    facet_wrap(~representation, nrow = 1L) +
    scale_color_manual(
      values = house_colors[c("MAFW reference", "LOMM")],
      breaks = c("MAFW reference", "LOMM"), name = NULL
    ) +
    scale_x_continuous(
      limits = c(0, 0.62), breaks = c(0, 0.2, 0.4, 0.6),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(x = "Standardized discrepancy (lower is closer)", y = NULL) +
    theme_classic(base_size = 8.5, base_family = "sans") +
    theme(
      axis.text = element_text(color = "#24313D"),
      axis.title = element_text(color = "#24313D"),
      strip.background = element_rect(fill = "#F1F4F6", color = NA),
      strip.text = element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.justification = "left",
      legend.margin = margin(0, 0, 1, 0),
      legend.key.width = grid::unit(1.1, "lines"),
      panel.spacing.x = grid::unit(1.0, "lines"),
      panel.grid.major.x = element_line(color = "#EEF1F3", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 8, 6, 6)
    )
}

main_caption <- paste(
  "Cross-fitted similarity of LOMM and static logistic-normal series to MAFW",
  "evaluation series. Points are median proportions and bars are 2.5th and",
  "97.5th percentiles across 100 generated datasets for each generator or",
  "1,000 MAFW-to-MAFW reference comparisons. Plausibility is the proportion",
  "of simulated series with a nearest MAFW evaluation series within the",
  "fold-specific threshold. Coverage is the proportion of MAFW evaluation",
  "series with a nearest simulation within the same threshold. The threshold",
  "is the 95th percentile of directional nearest-neighbor distances between",
  "disjoint MAFW calibration samples. Higher values indicate closer agreement."
)

appendix_caption <- paste(
  "Metric-level discrepancies between LOMM and MAFW classifier-score series.",
  "Blue points are medians across 100 LOMM simulations. Gray points and bars",
  "show the median and central 95% reference range across 1,000 disjoint",
  "MAFW-to-MAFW comparisons.",
  "Results are averaged across four folds and shown for scores and isometric",
  "log-ratio coordinates. Discrepancy is the larger of the standardized",
  "absolute median difference and standardized Wasserstein distance. Smaller",
  "values indicate closer agreement. Eight of 22 LOMM medians exceeded the",
  "corresponding reference interval. The largest departures concerned sample",
  "entropy and spectral entropy in both representations."
)

main <- function() {
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  clip <- utils::read.csv(input_paths$clip, stringsAsFactors = FALSE)
  metric <- utils::read.csv(input_paths$metric, stringsAsFactors = FALSE)
  validate_inputs(clip, metric)

  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

  atomic_save_plot(clip_plot(clip), output_paths$main_pdf, 7.1, 2.65)
  atomic_save_plot(clip_plot(clip), output_paths$main_png, 7.1, 2.65, 450)
  atomic_save_plot(metric_plot(metric), output_paths$appendix_pdf, 7.1, 5.0)
  atomic_save_plot(metric_plot(metric), output_paths$appendix_png, 7.1, 5.0, 450)
  atomic_write_csv(clip, output_paths$clip_table)
  atomic_write_csv(metric, output_paths$metric_table)
  atomic_write_text(main_caption, output_paths$main_caption)
  atomic_write_text(appendix_caption, output_paths$appendix_caption)

  invisible(output_paths)
}

if (sys.nframe() == 0L) main()
