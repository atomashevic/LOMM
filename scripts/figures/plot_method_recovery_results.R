#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(scales)
  library(stringr)
})

repo_root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"))) {
  stop("Run this script from the repository root.")
}

source_dir <- file.path(repo_root, "simulation_outputs", "method_comparison")
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

overall_path <- file.path(source_dir, "figure4_method_performance_assessment.csv")
level_path <- file.path(source_dir, "figure4_method_accuracy_reliable_summary.csv")
diff_path <- file.path(source_dir, "figure4_method_accuracy_reliable_differences.csv")

missing_inputs <- c(overall_path, level_path, diff_path)[
  !file.exists(c(overall_path, level_path, diff_path))
]
if (length(missing_inputs) > 0L) {
  stop("Missing required method-comparison inputs: ", paste(missing_inputs, collapse = ", "))
}

method_levels <- c("DynEGA", "GraphicalVAR", "GIMME")
method_colors <- c(
  DynEGA = "#0072B2",
  GraphicalVAR = "#E69F00",
  GIMME = "#CC79A7"
)
comparison_colors <- c(
  "DynEGA - GraphicalVAR" = "#E69F00",
  "DynEGA - GIMME" = "#CC79A7"
)

fmt_pct <- function(x) sprintf("%.1f", 100 * as.numeric(x))
fmt_num <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), as.numeric(x))
fmt_runtime <- function(x) {
  x <- as.numeric(x)
  ifelse(x < 10, sprintf("%.1f", x), sprintf("%.0f", x))
}

overall <- read_csv(overall_path, show_col_types = FALSE) |>
  mutate(method_group = factor(method_group, levels = method_levels)) |>
  arrange(method_group)

table1 <- overall |>
  transmute(
    Method = as.character(method_group),
    Scope = "Matched bounded comparison",
    `Structural cells` = structure_cells,
    `Method rows` = method_rows,
    `Hit rate (%)` = fmt_pct(exact_hit_rate),
    `Assignment (%)` = fmt_pct(assignment_accuracy),
    `Score r` = fmt_num(factor_score_correlation, 3),
    `Score NRMSE` = fmt_num(factor_score_nrmse, 3),
    `Median runtime (s)` = fmt_runtime(median_runtime_sec)
  )
write_csv(table1, file.path(table_dir, "table1_method_overall.csv"))

levels_df <- read_csv(level_path, show_col_types = FALSE) |>
  mutate(
    method_group = factor(method_group, levels = method_levels),
    stratum = factor(stratum, levels = c("Overall", "Scenario A", "Scenario B", "Scenario C"))
  )

diff_df <- read_csv(diff_path, show_col_types = FALSE) |>
  mutate(
    comparison = factor(comparison, levels = c("DynEGA - GraphicalVAR", "DynEGA - GIMME")),
    stratum = factor(stratum, levels = rev(c("Overall", "Scenario A", "Scenario B", "Scenario C")))
  )

panel_a <- levels_df |>
  filter(stratum == "Overall") |>
  mutate(method_y = as.numeric(method_group)) |>
  ggplot(aes(x = estimate, y = method_y, color = method_group, shape = method_group)) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = method_y), linewidth = 0.55) +
  geom_point(size = 2.7, stroke = 0.8) +
  scale_color_manual(values = method_colors, guide = "none") +
  scale_shape_manual(values = c(DynEGA = 16, GraphicalVAR = 17, GIMME = 15), guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.02), breaks = seq(0, 1, .25)) +
  scale_y_continuous(
    breaks = seq_along(method_levels),
    labels = method_levels,
    limits = c(0.65, length(method_levels) + 0.35)
  ) +
  labs(
    x = "Dimensional hit rate",
    y = NULL,
    title = "Overall recovery"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 10)
  )

panel_b <- levels_df |>
  filter(stratum != "Overall") |>
  ggplot(aes(x = stratum, y = estimate, color = method_group, group = method_group, shape = method_group)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.08, linewidth = 0.45,
                position = position_dodge(width = 0.45)) +
  geom_point(size = 2.25, stroke = 0.75, position = position_dodge(width = 0.45)) +
  geom_line(linewidth = 0.45, alpha = 0.75, position = position_dodge(width = 0.45)) +
  scale_color_manual(values = method_colors, name = "Method") +
  scale_shape_manual(values = c(DynEGA = 16, GraphicalVAR = 17, GIMME = 15), name = "Method") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, .25)) +
  labs(
    x = NULL,
    y = "Dimensional hit rate",
    title = "Recovery by scenario"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 10)
  )

panel_c <- diff_df |>
  mutate(
    stratum_y = as.numeric(stratum),
    comparison_offset = if_else(comparison == "DynEGA - GraphicalVAR", -0.12, 0.12),
    y_pos = stratum_y + comparison_offset
  ) |>
  ggplot(aes(x = estimate, y = y_pos, color = comparison, shape = comparison)) +
  geom_vline(xintercept = 0, color = "grey45", linetype = "dashed", linewidth = 0.4) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = y_pos), linewidth = 0.55) +
  geom_point(size = 2.35, stroke = 0.75) +
  scale_color_manual(values = comparison_colors, name = "Contrast") +
  scale_shape_manual(values = c("DynEGA - GraphicalVAR" = 17, "DynEGA - GIMME" = 15), name = "Contrast") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(-0.05, 0.80), breaks = seq(0, .75, .25)) +
  scale_y_continuous(
    breaks = seq_along(levels(diff_df$stratum)),
    labels = levels(diff_df$stratum),
    limits = c(0.65, length(levels(diff_df$stratum)) + 0.35)
  ) +
  labs(
    x = "Hit-rate difference",
    y = NULL,
    title = "Paired difference from DynEGA"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 10)
  )

figure5 <- (panel_a | panel_b) / panel_c +
  plot_layout(heights = c(1, 1.05), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 11)
  )

pdf_path <- file.path(figure_dir, "figure5_dimension_recovery.pdf")
png_path <- file.path(figure_dir, "figure5_dimension_recovery.png")

ggsave(pdf_path, figure5, width = 7.2, height = 5.6, device = cairo_pdf)
ggsave(png_path, figure5, width = 7.2, height = 5.6, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "table1_method_overall.csv"))
