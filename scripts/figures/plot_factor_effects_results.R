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

comparison_dir <- file.path(
  repo_root,
  "simulation_outputs/final_simulation_results/comparison_to_current_manuscript"
)
rows_path <- file.path(comparison_dir, "final_cd_rows.rds")
conditional_path <- file.path(comparison_dir, "final_cd_conditional_score_summary.csv")
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(rows_path) || !file.exists(conditional_path)) {
  stop(
    "Missing final CD comparison outputs. Run ",
    "`Rscript scripts/simulations/compare_final_results_to_manuscript.R` first.",
    call. = FALSE
  )
}

method_levels <- c("DynEGA", "GraphicalVAR", "GIMME")
cd_levels <- c("Louvain", "Leiden", "Walktrap")
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

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), as.numeric(x)))
}

rows <- readRDS(rows_path) %>%
  mutate(
    Method = factor(as.character(Method), levels = method_levels),
    CD = factor(as.character(CD), levels = cd_levels),
    Error = as.numeric(Error),
    Loadings = as.numeric(Loadings),
    loading_label = factor(
      paste0("Loading = ", sprintf("%.1f", Loadings)),
      levels = paste0("Loading = ", sprintf("%.1f", sort(unique(Loadings))))
    )
  )

summary_df <- rows %>%
  group_by(Method, CD, Error, Loadings, loading_label) %>%
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

write_csv(summary_df, file.path(table_dir, "figure6_factor_effects_table.csv"))

partial_eta2_table <- function(data, response, terms) {
  model_data <- data %>%
    mutate(
      response_value = .data[[response]],
      Method = factor(Method, levels = method_levels),
      CD = factor(CD, levels = cd_levels),
      scenario_family = factor(scenario_family),
      scenario_nfac = factor(scenario_nfac),
      Error = factor(Error),
      Loadings = factor(Loadings)
    ) %>%
    filter(is.finite(response_value))
  fit <- stats::lm(
    response_value ~ Method + CD + scenario_family + scenario_nfac + Error + Loadings,
    data = model_data
  )
  tab <- as.data.frame(stats::anova(fit))
  tab$term <- rownames(tab)
  residual_ss <- tab["Residuals", "Sum Sq"]
  tab %>%
    filter(term %in% terms) %>%
    transmute(
      response = response,
      term,
      df = Df,
      ss = `Sum Sq`,
      partial_eta2 = `Sum Sq` / (`Sum Sq` + residual_ss)
    )
}

effects <- bind_rows(
  partial_eta2_table(
    rows %>% mutate(n_dim_exact = as.numeric(n_dim_exact)),
    "n_dim_exact",
    c("Method", "CD", "scenario_family", "scenario_nfac", "Error", "Loadings")
  ),
  partial_eta2_table(
    rows %>% filter(status == "ok"),
    "score_cor_common",
    c("Method", "CD", "scenario_family", "scenario_nfac", "Error", "Loadings")
  )
) %>%
  mutate(partial_eta2 = round(partial_eta2, 3))
write_csv(effects, file.path(table_dir, "figure6_factor_effects_eta_screen.csv"))

conditional <- read_csv(conditional_path, show_col_types = FALSE) %>%
  mutate(
    Method = factor(as.character(Method), levels = method_levels),
    CD = factor(as.character(CD), levels = cd_levels)
  ) %>%
  transmute(
    Method = as.character(Method),
    CD = as.character(CD),
    Correct = paste0(correct_rows, "/", total_rows),
    `Hit (%)` = percent(hit_rate, accuracy = 0.1),
    `Mean r` = fmt_num(mean_conditional_r, 3),
    `r range` = ifelse(
      is.na(min_conditional_r) | is.na(max_conditional_r),
      NA_character_,
      sprintf("%.3f-%.3f", min_conditional_r, max_conditional_r)
    ),
    `Stratum n` = ifelse(
      is.na(min_correct_rows_per_stratum) | is.na(max_correct_rows_per_stratum),
      NA_character_,
      paste0(min_correct_rows_per_stratum, "-", max_correct_rows_per_stratum)
    ),
    `Max CI` = fmt_num(widest_ci, 3),
    Missing = missing_strata
  )
write_csv(conditional, file.path(table_dir, "table2_conditional_score_correlations.csv"))

figure6 <- ggplot(
  summary_df,
  aes(
    x = Error,
    y = hit_rate,
    color = Method,
    shape = Method,
    group = Method
  )
) +
  geom_line(linewidth = 0.45, alpha = 0.88, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.018,
    linewidth = 0.40,
    alpha = 0.78,
    na.rm = TRUE
  ) +
  geom_point(size = 1.85, stroke = 0.65, na.rm = TRUE) +
  facet_grid(CD ~ loading_label) +
  scale_color_manual(values = method_colors, name = "Method") +
  scale_shape_manual(values = method_shapes, name = "Method") +
  scale_x_continuous(
    breaks = c(0.125, 0.250, 0.500),
    labels = c("0.125", "0.250", "0.500")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    x = "Measurement-error SD",
    y = "Dimensional hit rate"
  ) +
  theme_minimal(base_size = 8.7) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7.6),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing.x = grid::unit(0.65, "lines"),
    panel.spacing.y = grid::unit(0.55, "lines")
  )

pdf_path <- file.path(figure_dir, "figure6_factor_effects.pdf")
png_path <- file.path(figure_dir, "figure6_factor_effects.png")

ggsave(pdf_path, figure6, width = 7.4, height = 5.4, device = cairo_pdf)
ggsave(png_path, figure6, width = 7.4, height = 5.4, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "figure6_factor_effects_table.csv"))
message("Wrote ", file.path(table_dir, "figure6_factor_effects_eta_screen.csv"))
message("Wrote ", file.path(table_dir, "table2_conditional_score_correlations.csv"))
