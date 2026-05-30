#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
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

summary_path <- file.path(source_dir, "figure4_method_bcd_overlap_summary.csv")
effects_path <- file.path(source_dir, "figure4_factor_effects.csv")
dynega_path <- file.path(repo_root, "simulation_outputs", "emotion_dynega", "benchmark_results.csv")
graphicalvar_path <- file.path(repo_root, "simulation_outputs", "emotion_graphicalvar", "checkpoint_results.csv")
gimme_path <- file.path(repo_root, "simulation_outputs", "emotion_gimme", "checkpoint_results.csv")
missing_inputs <- c(summary_path, effects_path, dynega_path, graphicalvar_path, gimme_path)[
  !file.exists(c(summary_path, effects_path, dynega_path, graphicalvar_path, gimme_path))
]
if (length(missing_inputs) > 0L) {
  stop("Missing required inputs: ", paste(missing_inputs, collapse = ", "))
}

method_levels <- c("DynEGA", "GraphicalVAR", "GIMME")
method_colors <- c(
  DynEGA = "#0072B2",
  GraphicalVAR = "#E69F00",
  GIMME = "#CC79A7"
)
method_shapes <- c(
  DynEGA = 16,
  GraphicalVAR = 17,
  GIMME = 15
)

read_method <- function(path, method_family_value, method_group_value) {
  data <- read_csv(path, show_col_types = FALSE) |>
    filter(method_family == method_family_value, status == "ok") |>
    mutate(method_group = method_group_value)
  if (!"Error_sd" %in% names(data)) data$Error_sd <- data$Error
  data$Error_sd <- as.numeric(data$Error_sd)
  data
}

raw <- bind_rows(
  read_method(dynega_path, "dynEGA", "DynEGA"),
  read_method(graphicalvar_path, "graphicalVAR", "GraphicalVAR"),
  read_method(gimme_path, "gimme", "GIMME")
) |>
  filter(
    scenario_family %in% c(
      "old_high_single_stable",
      "old_high_switch_stable",
      "old_high_switch_volatile"
    ),
    scenario_nfac %in% c(2, 3),
    N == 50,
    TimeP == 100,
    NVAR == 4,
    Error_sd %in% c(0.125, 0.250, 0.500),
    Loadings %in% c(0.4, 0.6, 0.8)
  ) |>
  mutate(
    method_group = factor(method_group, levels = method_levels),
    Error_sd = as.numeric(Error_sd),
    Loadings = as.numeric(Loadings),
    loading_label = factor(
      paste0("Loading = ", sprintf("%.1f", Loadings)),
      levels = paste0("Loading = ", sprintf("%.1f", sort(unique(Loadings))))
    )
  )

effects <- read_csv(effects_path, show_col_types = FALSE) |>
  filter(response %in% c("exact_recovery", "score_cor_common"),
         term %in% c("scenario_family", "Error_sd", "Loadings")) |>
  mutate(partial_eta2 = round(partial_eta2, 3))
write_csv(effects, file.path(table_dir, "figure6_factor_effects_eta_screen.csv"))

long <- bind_rows(
  raw |>
    transmute(
      method_group, Error_sd, Loadings,
      loading_label, outcome = "Hit rate",
      value = as.numeric(n_dim_exact),
      eligible = TRUE
    ),
  raw |>
    transmute(
      method_group, Error_sd, Loadings,
      loading_label, outcome = "Score correlation",
      value = as.numeric(score_cor_common),
      eligible = as.numeric(n_dim_exact) == 1
    )
) |>
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Hit rate",
        "Score correlation"
      )
    )
  )

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

mean_ci <- function(mean_value, sd_value, n, z = 1.96) {
  if (!is.finite(mean_value) || !is.finite(n) || n <= 0) {
    return(c(low = NA_real_, high = NA_real_))
  }
  if (n <= 1 || !is.finite(sd_value)) {
    return(c(low = mean_value, high = mean_value))
  }
  half_width <- stats::qt(0.975, df = n - 1) * sd_value / sqrt(n)
  c(low = max(0, mean_value - half_width), high = min(1, mean_value + half_width))
}

hit_rate_stats <- function(x) {
  x <- x[is.finite(x)]
  trials <- length(x)
  successes <- sum(x == 1)
  interval <- wilson_ci(successes, trials)
  c(
    estimate = if (trials > 0) successes / trials else NA_real_,
    low = interval[["low"]],
    high = interval[["high"]],
    successes = successes
  )
}

fisher_score_stats <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) {
    return(c(estimate = NA_real_, low = NA_real_, high = NA_real_, successes = NA_real_))
  }
  z_values <- atanh(pmin(pmax(x, -0.999999), 0.999999))
  z_mean <- mean(z_values)
  estimate <- tanh(z_mean)
  if (n <= 3L) {
    return(c(estimate = estimate, low = 0, high = 1, successes = NA_real_))
  }
  half_width <- 1.96 / sqrt(n - 3)
  c(
    estimate = estimate,
    low = max(0, tanh(z_mean - half_width)),
    high = min(1, tanh(z_mean + half_width)),
    successes = NA_real_
  )
}

summary_df <- long |>
  filter(eligible, is.finite(value)) |>
  group_by(method_group, outcome, Error_sd, Loadings, loading_label) |>
  summarise(
    values = list(value),
    cell_n = dplyr::n(),
    cell_sd = stats::sd(value, na.rm = TRUE),
    cell_se = cell_sd / sqrt(cell_n),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(
    interval = list(
      if (outcome == "Hit rate") {
        hit_rate_stats(values)
      } else {
        fisher_score_stats(values)
      }
    ),
    mean_value = interval[["estimate"]],
    ci_low = interval[["low"]],
    ci_high = interval[["high"]],
    successes = interval[["successes"]],
    total_rows = cell_n
  ) |>
  ungroup() |>
  select(-values, -interval)
all_combinations <- expand.grid(
  method_group = factor(method_levels, levels = method_levels),
  outcome = levels(long$outcome),
  Error_sd = sort(unique(raw$Error_sd)),
  Loadings = sort(unique(raw$Loadings)),
  stringsAsFactors = FALSE
) |>
  mutate(
    outcome = factor(outcome, levels = levels(long$outcome)),
    loading_label = factor(
      paste0("Loading = ", sprintf("%.1f", Loadings)),
      levels = levels(raw$loading_label)
    )
  )
summary_df <- all_combinations |>
  left_join(
    summary_df,
    by = c("method_group", "outcome", "Error_sd", "Loadings", "loading_label")
  )
write_csv(summary_df, file.path(table_dir, "figure6_factor_effects_table.csv"))

score_strata <- summary_df |>
  filter(outcome == "Score correlation")

score_summary <- score_strata |>
  group_by(method_group) |>
  summarise(
    correct_rows = sum(cell_n, na.rm = TRUE),
    total_rows = nrow(raw |> filter(method_group == dplyr::first(method_group))),
    hit_rate = correct_rows / total_rows,
    mean_conditional_r = tanh(weighted.mean(atanh(pmin(pmax(mean_value, -0.999999), 0.999999)),
                                           w = cell_n,
                                           na.rm = TRUE)),
    min_conditional_r = min(mean_value, na.rm = TRUE),
    max_conditional_r = max(mean_value, na.rm = TRUE),
    min_correct_rows_per_stratum = min(cell_n, na.rm = TRUE),
    max_correct_rows_per_stratum = max(cell_n, na.rm = TRUE),
    max_ci_width = max(ci_high - ci_low, na.rm = TRUE),
    missing_strata = sum(!is.finite(mean_value)),
    .groups = "drop"
  ) |>
  mutate(
    Method = as.character(method_group),
    `Correct rows` = paste0(correct_rows, "/", total_rows),
    `Hit rate` = scales::percent(hit_rate, accuracy = 0.1),
    `Mean conditional r` = sprintf("%.3f", mean_conditional_r),
    `Conditional r range` = sprintf("%.3f-%.3f", min_conditional_r, max_conditional_r),
    `Correct rows per stratum` = paste0(min_correct_rows_per_stratum, "-", max_correct_rows_per_stratum),
    `Widest CI` = sprintf("%.3f", max_ci_width),
    `Missing strata` = missing_strata
  ) |>
  select(
    Method,
    `Correct rows`,
    `Hit rate`,
    `Mean conditional r`,
    `Conditional r range`,
    `Correct rows per stratum`,
    `Widest CI`,
    `Missing strata`
  )
write_csv(score_summary, file.path(table_dir, "table2_conditional_score_correlations.csv"))

figure6 <- ggplot(
  summary_df |> filter(outcome == "Hit rate"),
  aes(
    x = Error_sd,
    y = mean_value,
    color = method_group,
    shape = method_group,
    group = method_group
  )
) +
  geom_line(linewidth = 0.55, alpha = 0.9, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.018,
    linewidth = 0.55,
    alpha = 0.78,
    na.rm = TRUE
  ) +
  geom_point(size = 2.25, stroke = 0.75, na.rm = TRUE) +
  facet_wrap(~loading_label, nrow = 1) +
  scale_color_manual(values = method_colors, name = "Method") +
  scale_shape_manual(values = method_shapes, name = "Method") +
  scale_x_continuous(
    breaks = c(0.125, 0.250, 0.500),
    labels = c("0.125", "0.250", "0.500")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Measurement-error SD",
    y = "Dimensional hit rate"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    strip.text.y.left = element_text(angle = 0, hjust = 0.5, size = 7.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing.x = grid::unit(0.75, "lines"),
    panel.spacing.y = grid::unit(0.55, "lines")
  )

pdf_path <- file.path(figure_dir, "figure6_factor_effects.pdf")
png_path <- file.path(figure_dir, "figure6_factor_effects.png")

ggsave(pdf_path, figure6, width = 7.2, height = 2.9, device = cairo_pdf)
ggsave(png_path, figure6, width = 7.2, height = 2.9, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "figure6_factor_effects_table.csv"))
message("Wrote ", file.path(table_dir, "figure6_factor_effects_eta_screen.csv"))
message("Wrote ", file.path(table_dir, "table2_conditional_score_correlations.csv"))
