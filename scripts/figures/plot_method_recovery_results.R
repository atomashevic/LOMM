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

comparison_dir <- file.path(
  repo_root,
  "simulation_outputs/final_simulation_results/comparison_to_current_manuscript"
)
rows_path <- file.path(comparison_dir, "final_cd_rows.rds")
summary_path <- file.path(comparison_dir, "final_method_summary.csv")
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(rows_path) || !file.exists(summary_path)) {
  stop(
    "Missing final CD comparison outputs. Run ",
    "`Rscript scripts/simulations/compare_final_results_to_manuscript.R` first.",
    call. = FALSE
  )
}

method_levels <- c("DynEGA", "GraphicalVAR", "GIMME")
method_colors <- c(
  DynEGA = "#0072B2",
  GraphicalVAR = "#E69F00",
  GIMME = "#CC79A7"
)
method_shapes <- c(DynEGA = 16, GraphicalVAR = 17, GIMME = 15)

wilson_ci <- function(successes, trials, z = 1.96) {
  if (!is.finite(successes) || !is.finite(trials) || trials <= 0) {
    return(c(low = NA_real_, high = NA_real_))
  }
  p <- successes / trials
  denom <- 1 + z^2 / trials
  center <- (p + z^2 / (2 * trials)) / denom
  half_width <- z * sqrt((p * (1 - p) / trials) + (z^2 / (4 * trials^2))) / denom
  c(low = max(0, center - half_width), high = min(1, center + half_width))
}

summarise_hit <- function(data, grouping) {
  data %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      rows = n(),
      successes = sum(n_dim_exact == 1, na.rm = TRUE),
      hit_rate = successes / rows,
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      ci = list(wilson_ci(successes, rows)),
      ci_low = ci[["low"]],
      ci_high = ci[["high"]]
    ) %>%
    ungroup() %>%
    select(-ci)
}

fmt_pct <- function(x) sprintf("%.1f", as.numeric(x))
fmt_num <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), as.numeric(x))
fmt_runtime <- function(x) {
  x <- as.numeric(x)
  ifelse(x < 10, sprintf("%.2f", x), sprintf("%.1f", x))
}

rows <- readRDS(rows_path) %>%
  mutate(
    Method = factor(as.character(Method), levels = method_levels),
    scenario_label = recode(
      scenario_family,
      oai_broad_geometry = "OAI broad",
      oai_ranked_persistence = "OAI ranked",
      old_high_single_stable = "Old high single",
      old_high_switch_stable = "Old high switch",
      old_high_switch_volatile = "Old volatile",
      old_low_single_stable = "Old low single"
    ),
    scenario_label = factor(
      scenario_label,
      levels = c(
        "OAI broad", "OAI ranked", "Old high single",
        "Old high switch", "Old volatile", "Old low single"
      )
    ),
    dim_label = factor(paste0(scenario_nfac, "D"), levels = c("2D", "3D"))
  )

summary_df <- read_csv(summary_path, show_col_types = FALSE) %>%
  mutate(Method = factor(as.character(Method), levels = method_levels))

table1 <- summary_df %>%
  transmute(
    Method = as.character(Method),
    Cells = structural_cells,
    Rows = rows,
    ok,
    timeout,
    error,
    `Hit (%)` = fmt_pct(hit_all_pct),
    `Assign. (%)` = fmt_pct(assignment_available_pct),
    `Score r` = fmt_num(score_r_available, 3),
    `Score NRMSE` = fmt_num(score_nrmse_available, 3),
    `Runtime (s)` = fmt_runtime(runtime_ok_median_s)
  )
write_csv(table1, file.path(table_dir, "table1_method_overall.csv"))

overall <- summarise_hit(rows, "Method")
scenario <- summarise_hit(rows, c("Method", "scenario_label", "dim_label"))

panel_a <- overall %>%
  ggplot(aes(x = Method, y = hit_rate, color = Method, shape = Method, group = Method)) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.10,
    linewidth = 0.45
  ) +
  geom_point(size = 2.55, stroke = 0.75) +
  scale_color_manual(values = method_colors, guide = "none") +
  scale_shape_manual(values = method_shapes, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, .25)) +
  labs(x = NULL, y = "Dimensional hit rate", title = "Overall") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 10)
  )

panel_b <- scenario %>%
  ggplot(aes(x = scenario_label, y = hit_rate, color = Method, shape = Method, group = Method)) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.08,
    linewidth = 0.35,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 1.65, stroke = 0.65, position = position_dodge(width = 0.55)) +
  facet_grid(dim_label ~ .) +
  scale_color_manual(values = method_colors, name = "Method") +
  scale_shape_manual(values = method_shapes, name = "Method") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, .25)) +
  labs(x = NULL, y = "Dimensional hit rate", title = "By scenario family") +
  theme_minimal(base_size = 8.5) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7.6),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 10)
  )

figure5 <- panel_a / panel_b +
  plot_layout(heights = c(0.75, 1.35), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 11)
  )

pdf_path <- file.path(figure_dir, "figure5_dimension_recovery.pdf")
png_path <- file.path(figure_dir, "figure5_dimension_recovery.png")

ggsave(pdf_path, figure5, width = 7.4, height = 6.4, device = cairo_pdf)
ggsave(png_path, figure5, width = 7.4, height = 6.4, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "table1_method_overall.csv"))
