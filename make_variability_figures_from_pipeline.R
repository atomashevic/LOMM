#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("make_variability_figures_from_pipeline.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

read_char_env <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  value
}

input_dir <- read_char_env(
  "PIPELINE_INPUT_DIR",
  file.path(repo_root, "empirical_outputs_smoke", "mafw-sample", "oai-large-100")
)
compare_mode <- read_char_env("PIPELINE_COMPARE_MODE", "heterogeneous")
output_prefix <- read_char_env("PIPELINE_OUTPUT_PREFIX", compare_mode)
output_dir <- read_char_env("PIPELINE_OUTPUT_DIR", input_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

empirical_raw_path <- file.path(input_dir, "empirical_raw_metrics.csv")
empirical_latent_path <- file.path(input_dir, "empirical_latent_metrics.csv")
synthetic_raw_path <- file.path(input_dir, sprintf("%s_synthetic_raw_metrics.csv", compare_mode))
synthetic_latent_path <- file.path(input_dir, sprintf("%s_synthetic_latent_metrics.csv", compare_mode))
mode_summary_path <- file.path(input_dir, "mode_summary.csv")

stopifnot(file.exists(empirical_raw_path))
stopifnot(file.exists(empirical_latent_path))
stopifnot(file.exists(synthetic_raw_path))
stopifnot(file.exists(synthetic_latent_path))

empirical_raw <- utils::read.csv(empirical_raw_path, stringsAsFactors = FALSE)
empirical_latent <- utils::read.csv(empirical_latent_path, stringsAsFactors = FALSE)
synthetic_raw <- utils::read.csv(synthetic_raw_path, stringsAsFactors = FALSE)
synthetic_latent <- utils::read.csv(synthetic_latent_path, stringsAsFactors = FALSE)
mode_summary <- utils::read.csv(mode_summary_path, stringsAsFactors = FALSE)

mode_row <- mode_summary[mode_summary$mode == compare_mode, , drop = FALSE]
if (nrow(mode_row) == 0L) {
  stop(sprintf("Mode '%s' not found in %s", compare_mode, mode_summary_path), call. = FALSE)
}

variability_metrics <- c(
  "sample_entropy",
  "zero_crossing_rate",
  "lag1_acf",
  "std_value",
  "total_variation"
)

metric_labels <- c(
  sample_entropy = "Sample Entropy",
  zero_crossing_rate = "Zero-Crossing Rate",
  lag1_acf = "Lag-1 ACF",
  std_value = "Standard Deviation",
  total_variation = "Total Variation"
)

dataset_palette <- c(
  empirical = "#3b5bdb",
  dlo = "#c2610c"
)

model_name <- basename(normalizePath(input_dir, mustWork = TRUE))
synthetic_label <- if (compare_mode == "heterogeneous") {
  "heterogeneous DLO"
} else {
  sprintf("%s DLO", compare_mode)
}
param_subtitle <- sprintf(
  "Best %s parameters: eta=%s, zeta=%s, sigma_q=%.2f, dt=%.1f",
  compare_mode,
  mode_row$eta_spec[[1]],
  mode_row$zeta_spec[[1]],
  mode_row$sigma_q[[1]],
  mode_row$dt[[1]]
)

bind_metric_family <- function(empirical_metrics, synthetic_metrics, family_name) {
  empirical_df <- empirical_metrics[
    empirical_metrics$component == "series" & empirical_metrics$metric %in% variability_metrics,
    ,
    drop = FALSE
  ]
  synthetic_df <- synthetic_metrics[
    synthetic_metrics$component == "series" & synthetic_metrics$metric %in% variability_metrics,
    ,
    drop = FALSE
  ]

  empirical_df$dataset <- "empirical"
  synthetic_df$dataset <- "dlo"

  combined <- dplyr::bind_rows(empirical_df, synthetic_df)
  combined$family <- family_name
  combined$dataset <- factor(combined$dataset, levels = c("dlo", "empirical"))
  combined$metric <- factor(combined$metric, levels = variability_metrics, labels = metric_labels[variability_metrics])
  combined
}

raw_plot_df <- bind_metric_family(
  empirical_metrics = empirical_raw,
  synthetic_metrics = synthetic_raw,
  family_name = "Raw"
)

latent_plot_df <- bind_metric_family(
  empirical_metrics = empirical_latent,
  synthetic_metrics = synthetic_latent,
  family_name = "Latent"
)

plot_df <- dplyr::bind_rows(raw_plot_df, latent_plot_df)

summary_df <- plot_df |>
  dplyr::group_by(family, dataset, metric) |>
  dplyr::summarise(
    median = stats::median(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    q25 = stats::quantile(value, probs = 0.25, na.rm = TRUE),
    q75 = stats::quantile(value, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )

summary_path <- file.path(output_dir, sprintf("%s_variability_metric_summary.csv", output_prefix))
utils::write.csv(summary_df, summary_path, row.names = FALSE)

make_distribution_plot <- function(df, family_name, output_path) {
  family_df <- df[df$family == family_name, , drop = FALSE]
  gg <- ggplot2::ggplot(family_df, ggplot2::aes(x = dataset, y = value, fill = dataset)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.35, color = NA) +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.85) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 3) +
    ggplot2::scale_fill_manual(values = dataset_palette) +
    ggplot2::labs(
      title = sprintf("%s variability metrics: empirical %s vs %s", family_name, model_name, synthetic_label),
      subtitle = param_subtitle,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "#f3f4f6", color = "#d1d5db"),
      axis.text.x = ggplot2::element_text(size = 10)
    )

  ggplot2::ggsave(output_path, gg, width = 11, height = 7, dpi = 170)
}

make_dumbbell_plot <- function(summary_tbl, output_path) {
  wide_df <- summary_tbl |>
    dplyr::select(family, dataset, metric, median) |>
    tidyr::pivot_wider(names_from = dataset, values_from = median)

  wide_df$metric <- factor(wide_df$metric, levels = rev(metric_labels[variability_metrics]))

  gg <- ggplot2::ggplot(wide_df, ggplot2::aes(y = metric)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = empirical, xend = dlo, yend = metric),
      color = "#9ca3af",
      linewidth = 1.2
    ) +
    ggplot2::geom_point(ggplot2::aes(x = empirical), color = dataset_palette[["empirical"]], size = 2.8) +
    ggplot2::geom_point(ggplot2::aes(x = dlo), color = dataset_palette[["dlo"]], size = 2.8) +
    ggplot2::facet_wrap(~family, scales = "free_x") +
    ggplot2::labs(
      title = sprintf("Median variability properties: empirical %s vs %s", model_name, synthetic_label),
      subtitle = param_subtitle,
      x = "Median metric value",
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 11)

  ggplot2::ggsave(output_path, gg, width = 10, height = 5.5, dpi = 170)
}

raw_path <- file.path(output_dir, sprintf("%s_raw_variability_distributions.png", output_prefix))
latent_path <- file.path(output_dir, sprintf("%s_latent_variability_distributions.png", output_prefix))
alignment_path <- file.path(output_dir, sprintf("%s_variability_median_alignment.png", output_prefix))

make_distribution_plot(plot_df, "Raw", raw_path)
make_distribution_plot(plot_df, "Latent", latent_path)
make_dumbbell_plot(summary_df, alignment_path)

message("Figures written to: ", output_dir)
message("Raw plot: ", raw_path)
message("Latent plot: ", latent_path)
message("Alignment plot: ", alignment_path)
