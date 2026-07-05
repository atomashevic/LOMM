#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
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
method_offsets <- c(DynEGA = -0.12, GraphicalVAR = 0, GIMME = 0.12)
condition_levels <- c(
  old_low_single_stable = "A",
  old_high_single_stable = "B",
  old_high_switch_stable = "C",
  old_high_switch_volatile = "D",
  oai_ranked_persistence = "E",
  oai_broad_geometry = "F"
)
condition_descriptions <- c(
  A = "low-expression single-dominance",
  B = "high-expression single-dominance",
  C = "high-expression switching",
  D = "high-expression rapid switching",
  E = "ranked-persistence",
  F = "broad switching-geometry"
)

bootstrap_hit_ci <- function(data, n_boot = 1000L, seed = 20260616L) {
  if (nrow(data) == 0L) {
    return(tibble(ci_low = NA_real_, ci_high = NA_real_))
  }
  clusters <- data %>%
    mutate(success = if_else(!is.na(n_dim_exact) & n_dim_exact == 1, 1, 0)) %>%
    group_by(cluster_id) %>%
    summarise(cluster_hit = mean(success), .groups = "drop")
  if (nrow(clusters) < 2L) {
    return(tibble(ci_low = NA_real_, ci_high = NA_real_))
  }
  set.seed(seed)
  boot_stats <- replicate(
    n_boot,
    mean(sample(clusters$cluster_hit, size = nrow(clusters), replace = TRUE), na.rm = TRUE)
  )
  tibble(
    ci_low = unname(quantile(boot_stats, 0.025, na.rm = TRUE)),
    ci_high = unname(quantile(boot_stats, 0.975, na.rm = TRUE))
  )
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
    condition_code = recode(scenario_family, !!!condition_levels),
    condition_code = factor(condition_code, levels = unname(condition_levels)),
    condition_index = as.integer(condition_code),
    method_offset = unname(method_offsets[as.character(Method)]),
    condition_x = condition_index + method_offset,
    dim_label = factor(paste0(scenario_nfac, "D source"), levels = c("2D source", "3D source")),
    cluster_id = paste(
      scenario, N, TimeP, NVAR, Error, Loadings, Q, base_err, rep,
      sep = "|"
    )
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

scenario <- rows %>%
  group_by(Method, dim_label, condition_code, condition_index) %>%
  group_modify(~ {
    ci <- bootstrap_hit_ci(.x)
    tibble(
      rows = nrow(.x),
      clusters = n_distinct(.x$cluster_id),
      successes = sum(!is.na(.x$n_dim_exact) & .x$n_dim_exact == 1),
      hit_rate = successes / rows,
      ci_low = ci$ci_low,
      ci_high = ci$ci_high
    )
  }) %>%
  ungroup()

condition_lookup <- tibble(
  condition_code = names(condition_descriptions),
  description = unname(condition_descriptions)
)
write_csv(condition_lookup, file.path(table_dir, "figure5_condition_key.csv"))

figure5 <- scenario %>%
  left_join(
    rows %>%
      distinct(Method, method_offset),
    by = "Method"
  ) %>%
  mutate(condition_x = condition_index + method_offset) %>%
  ggplot(aes(x = condition_x, y = hit_rate, color = Method, group = Method)) +
  geom_line(linewidth = 0.62, alpha = 0.95) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.08,
    linewidth = 0.55,
    alpha = 0.95,
    na.rm = TRUE
  ) +
  geom_point(aes(shape = Method), size = 1.95, stroke = 0.65) +
  facet_grid(dim_label ~ .) +
  scale_color_manual(values = method_colors, name = "Method") +
  scale_shape_manual(values = method_shapes, name = "Method") +
  scale_x_continuous(
    breaks = seq_along(condition_levels),
    labels = unname(condition_levels),
    expand = expansion(mult = c(0.025, 0.035))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, .25),
    expand = expansion(mult = c(0.01, 0.025))
  ) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  labs(
    x = "Condition",
    y = "Dimensional hit rate"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.35),
    axis.text.x = element_text(face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 8.4),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(7, 9, 7, 7)
  )

pdf_path <- file.path(figure_dir, "figure5_dimension_recovery.pdf")
png_path <- file.path(figure_dir, "figure5_dimension_recovery.png")

ggsave(pdf_path, figure5, width = 7.4, height = 4.7, device = cairo_pdf)
ggsave(png_path, figure5, width = 7.4, height = 4.7, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "table1_method_overall.csv"))
message("Wrote ", file.path(table_dir, "figure5_condition_key.csv"))
