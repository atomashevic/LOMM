#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(scales)
  library(stringr)
  library(tidyr)
})

repo_root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"))) {
  stop("Run this script from the repository root.")
}

input_path <- file.path(
  repo_root,
  "simulation_outputs",
  "empirical_link_options",
  "oai-large-100",
  "metric_comparison_long.csv"
)
selected_candidate <- "link_softmax_temp_1.00_intercept_none"
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

metric_levels <- c(
  "channel_iqr",
  "diff_iqr",
  "lag1_acf",
  "run_length_entropy",
  "spectral_entropy",
  "effective_rank"
)

metric_labels <- c(
  channel_iqr = "Channel IQR",
  diff_iqr = "Derivative IQR",
  lag1_acf = "Lag-1 ACF",
  run_length_entropy = "Run-length entropy",
  spectral_entropy = "Spectral entropy",
  effective_rank = "Effective rank"
)

status_levels <- c("Pass", "Partial", "Fail")
status_colors <- c(
  Pass = "#009E73",
  Partial = "#E69F00",
  Fail = "#D55E00"
)
component_colors <- c(
  "Scaled median difference" = "#0072B2",
  "Scaled Wasserstein distance" = "#CC79A7"
)

realism <- read_csv(input_path, show_col_types = FALSE) |>
  filter(
    candidate_label == selected_candidate,
    metric %in% metric_levels,
    family %in% c("raw", "latent")
  ) |>
  mutate(
    readout = recode(
      family,
      raw = "Observed score channels",
      latent = "Recovered community-score summaries"
    ),
    metric = factor(metric, levels = metric_levels),
    metric_label = factor(metric_labels[as.character(metric)],
                          levels = unname(metric_labels[metric_levels])),
    max_discrepancy = if_else(
      is.na(max_discrepancy),
      pmax(abs(median_diff_scaled), wasserstein_scaled, na.rm = TRUE),
      max_discrepancy
    ),
    status = case_when(
      max_discrepancy <= 0.33 ~ "Pass",
      max_discrepancy <= 1.00 ~ "Partial",
      TRUE ~ "Fail"
    ),
    status = factor(status, levels = status_levels),
    readout = factor(readout, levels = c("Observed score channels", "Recovered community-score summaries"))
  ) |>
  arrange(readout, metric)

realism_table <- realism |>
  transmute(
    Readout = recode(as.character(readout),
                     "Observed score channels" = "Channels",
                     "Recovered community-score summaries" = "Recovered community-score summaries"),
    Metric = as.character(metric_label),
    `Empirical median` = sprintf("%.3f", empirical_median),
    `LOMM median` = sprintf("%.3f", synthetic_median),
    `Empirical IQR` = sprintf("%.3f", empirical_iqr),
    `Scaled median diff.` = sprintf("%.3f", abs(median_diff_scaled)),
    `Scaled Wasserstein` = sprintf("%.3f", wasserstein_scaled),
    `Max discrepancy` = sprintf("%.3f", max_discrepancy),
    Status = as.character(status)
  )

write_csv(realism_table, file.path(table_dir, "empirical_realism_table.csv"))

status_panel <- realism |>
  mutate(metric_label_wrapped = factor(
    str_wrap(as.character(metric_label), width = 13),
    levels = str_wrap(unname(metric_labels[metric_levels]), width = 13)
  )) |>
  ggplot(aes(x = metric_label_wrapped, y = readout, fill = status)) +
  geom_tile(color = "white", linewidth = 0.7, width = 0.96, height = 0.82) +
  geom_text(aes(label = sprintf("%.3f", max_discrepancy)),
            size = 3.0, color = "black") +
  scale_fill_manual(values = status_colors, drop = FALSE) +
  scale_y_discrete(limits = c("Recovered community-score summaries", "Observed score channels")) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Call"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

component_panel <- realism |>
  select(readout, metric_label, status, median_diff_scaled, wasserstein_scaled) |>
  pivot_longer(
    cols = c(median_diff_scaled, wasserstein_scaled),
    names_to = "component",
    values_to = "scaled_discrepancy"
  ) |>
  mutate(
    component = recode(
      component,
      median_diff_scaled = "Scaled median difference",
      wasserstein_scaled = "Scaled Wasserstein distance"
    ),
    component = factor(component, levels = names(component_colors)),
    metric_label = factor(metric_label, levels = rev(unname(metric_labels[metric_levels])))
  ) |>
  ggplot(aes(x = abs(scaled_discrepancy), y = metric_label)) +
  geom_vline(xintercept = 0.33, linetype = "dashed",
             linewidth = 0.35, color = "grey40") +
  geom_vline(xintercept = 1.00, linetype = "dashed",
             linewidth = 0.35, color = "grey40") +
  geom_line(aes(group = interaction(readout, metric_label)),
            color = "grey78", linewidth = 0.45) +
  geom_point(aes(color = component, shape = component),
             size = 2.3, stroke = 0.7) +
  facet_wrap(~ readout, ncol = 1) +
  scale_color_manual(values = component_colors) +
  scale_x_continuous(
    limits = c(0, 2.1),
    breaks = c(0, 0.33, 1.0, 1.5, 2.0),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Scaled discrepancy",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.title = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 7.5),
    axis.text.y = element_text(size = 8)
  )

figure <- status_panel / component_panel +
  plot_layout(heights = c(0.95, 1.45), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

pdf_path <- file.path(figure_dir, "figure4_empirical_realism.pdf")
png_path <- file.path(figure_dir, "figure4_empirical_realism.png")

ggsave(pdf_path, figure, width = 8.2, height = 6.0, device = cairo_pdf)
ggsave(png_path, figure, width = 8.2, height = 6.0, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "empirical_realism_table.csv"))
