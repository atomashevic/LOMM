#!/usr/bin/env Rscript

## RR Figure 4 candidate from the latest full DynEGA signed-feedback grid.
## The original hit-rate display is uninformative here because exact n.dim
## recovery is perfect across the current full DynEGA grid.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_signed_feedback_dynega_rr_figure4.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

input_dir <- Sys.getenv(
  "SIGNED_FEEDBACK_DYNEGA_MAIN_DIR",
  file.path(repo_root, "benchmark_outputs_signed_feedback_main_dynega_reps10")
)
out_dir <- Sys.getenv(
  "SIGNED_FEEDBACK_DYNEGA_RR_FIGURE_DIR",
  file.path(repo_root, "comparison_signed_feedback_dynega_rr")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

input_csv <- file.path(input_dir, "benchmark_results.csv")
if (!file.exists(input_csv)) {
  stop("Missing DynEGA benchmark results: ", input_csv, call. = FALSE)
}

okabe <- c(
  blue = "#0072B2",
  orange = "#E69F00",
  vermillion = "#D55E00",
  green = "#009E73",
  purple = "#CC79A7",
  black = "#000000"
)

theme_original_style <- function(base_size = 8.5) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "grey88", linewidth = 0.22),
      panel.grid.minor = element_line(color = "grey94", linewidth = 0.16),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.78), margin = margin(b = 2)),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(7, 8, 6, 7)
    )
}

save_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 320)
}

ci95 <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  1.96 * stats::sd(x) / sqrt(length(x))
}

format_decimal <- function(x, digits = 3) {
  formatC(x, format = "f", digits = digits)
}

partial_eta2 <- function(data, response, predictors) {
  keep <- data %>%
    select(all_of(c(response, predictors))) %>%
    filter(if_all(everything(), ~ !is.na(.x)))
  usable_predictors <- predictors[vapply(
    predictors,
    function(pred) length(unique(keep[[pred]])) > 1L,
    logical(1)
  )]
  if (nrow(keep) < 10L || length(usable_predictors) == 0L) {
    return(data.frame())
  }
  if (stats::sd(keep[[response]], na.rm = TRUE) < .Machine$double.eps) {
    return(data.frame(
      response = response,
      term = usable_predictors,
      df = NA_real_,
      ss = 0,
      partial_eta2 = 0,
      stringsAsFactors = FALSE
    ))
  }
  form <- stats::as.formula(paste(response, "~", paste(usable_predictors, collapse = " + ")))
  fit <- stats::lm(form, data = keep)
  tab <- stats::anova(fit)
  residual_ss <- tab["Residuals", "Sum Sq"]
  out <- data.frame(
    term = rownames(tab),
    df = tab[, "Df"],
    ss = tab[, "Sum Sq"],
    stringsAsFactors = FALSE
  ) %>%
    filter(term != "Residuals") %>%
    mutate(
      response = response,
      partial_eta2 = ss / (ss + residual_ss)
    ) %>%
    select(response, term, df, ss, partial_eta2) %>%
    arrange(response, desc(partial_eta2))
  rownames(out) <- NULL
  out
}

raw <- readr::read_csv(input_csv, show_col_types = FALSE)

dynega <- raw %>%
  filter(method_family == "dynEGA", status == "ok", benchmark_grid == "main") %>%
  mutate(
    exact_recovery = as.numeric(n_dim_exact),
    Error_sd_value = sqrt(as.numeric(Error)),
    Error_sd = factor(
      format_decimal(Error_sd_value, 3),
      levels = format_decimal(sort(unique(Error_sd_value)), 3)
    ),
    Loadings = factor(format_decimal(as.numeric(Loadings), 1), levels = format_decimal(sort(unique(as.numeric(Loadings))), 1)),
    Q = factor(format_decimal(as.numeric(Q), 2), levels = format_decimal(sort(unique(as.numeric(Q))), 2)),
    eta_scalar = factor(format_decimal(as.numeric(eta_scalar), 2), levels = format_decimal(sort(unique(as.numeric(eta_scalar))), 2)),
    zeta_scalar = factor(format_decimal(as.numeric(zeta_scalar), 2), levels = format_decimal(sort(unique(as.numeric(zeta_scalar))), 2)),
    scenario_nfac = factor(as.integer(scenario_nfac), levels = c(2, 3, 4)),
    true_dim_label = factor(paste0(scenario_nfac, "D"), levels = c("2D", "3D", "4D")),
    feedback_regime = factor(feedback_regime, levels = c("damped", "critical", "persistent")),
    feedback_label = factor(
      recode(
        as.character(feedback_regime),
        damped = "Damped\nzeta = -0.30",
        critical = "Critical\nzeta = 0.00",
        persistent = "Persistent\nzeta = 0.08"
      ),
      levels = c("Damped\nzeta = -0.30", "Critical\nzeta = 0.00", "Persistent\nzeta = 0.08")
    ),
    N = factor(N),
    TimeP = factor(TimeP),
    NVAR = factor(NVAR)
  )

if (nrow(dynega) == 0L) {
  stop("No valid DynEGA main-grid rows found in ", input_csv, call. = FALSE)
}

predictors <- c(
  "feedback_regime", "eta_scalar", "zeta_scalar", "scenario_nfac",
  "N", "TimeP", "NVAR", "Error_sd", "Loadings", "Q"
)

factor_effects <- bind_rows(
  partial_eta2(dynega, "exact_recovery", predictors),
  partial_eta2(dynega, "score_cor_common", predictors),
  partial_eta2(dynega, "score_nrmse_common", predictors)
)
readr::write_csv(factor_effects, file.path(out_dir, "figure4_factor_effects.csv"))

candidate_specs <- list(
  exact_hit_rate_timep_nvar = list(
    metric = "exact_recovery",
    x = "TimeP",
    color = "NVAR",
    facet1 = "feedback_regime",
    facet2 = "scenario_nfac",
    reason = "Literal remake of the old hit-rate figure."
  ),
  score_correlation_q_error = list(
    metric = "score_cor_common",
    x = "Q",
    color = "Error_sd",
    facet1 = "feedback_regime",
    facet2 = "Loadings",
    reason = "Most legible correlation candidate; Q and measurement error separate visible but smaller differences."
  ),
  selected_nrmse_loading_error = list(
    metric = "score_nrmse_common",
    x = "Loadings",
    color = "Error_sd",
    facet1 = "feedback_regime",
    facet2 = "scenario_nfac",
    reason = "Largest full-grid spread; shows feedback regime, measurement error, loading strength, and dimensionality."
  )
)

score_candidate <- function(name, spec) {
  vars <- unique(unname(unlist(spec[c("x", "color", "facet1", "facet2")])))
  summary <- dynega %>%
    group_by(across(all_of(vars))) %>%
    summarise(value = mean(.data[[spec$metric]], na.rm = TRUE), .groups = "drop")
  panel_ranges <- summary %>%
    group_by(across(all_of(c(spec$facet1, spec$facet2)))) %>%
    summarise(
      panel_range = diff(range(value, na.rm = TRUE)),
      panel_min = min(value, na.rm = TRUE),
      panel_max = max(value, na.rm = TRUE),
      .groups = "drop"
    )
  data.frame(
    candidate = name,
    metric = spec$metric,
    x = spec$x,
    color = spec$color,
    facet_rows = spec$facet1,
    facet_cols = spec$facet2,
    panels = nrow(panel_ranges),
    median_panel_range = median(panel_ranges$panel_range, na.rm = TRUE),
    max_panel_range = max(panel_ranges$panel_range, na.rm = TRUE),
    total_range = diff(range(summary$value, na.rm = TRUE)),
    min_value = min(summary$value, na.rm = TRUE),
    max_value = max(summary$value, na.rm = TRUE),
    reason = spec$reason,
    stringsAsFactors = FALSE
  )
}

candidate_scores <- bind_rows(Map(score_candidate, names(candidate_specs), candidate_specs)) %>%
  arrange(desc(total_range), desc(median_panel_range))
readr::write_csv(candidate_scores, file.path(out_dir, "figure4_candidate_scores.csv"))

exact_summary <- dynega %>%
  summarise(
    source_rows = n(),
    exact_recovery_mean = mean(exact_recovery, na.rm = TRUE),
    exact_recovery_min = min(exact_recovery, na.rm = TRUE),
    exact_recovery_max = max(exact_recovery, na.rm = TRUE),
    score_nrmse_mean = mean(score_nrmse_common, na.rm = TRUE),
    score_nrmse_min = min(score_nrmse_common, na.rm = TRUE),
    score_nrmse_max = max(score_nrmse_common, na.rm = TRUE)
  )
readr::write_csv(exact_summary, file.path(out_dir, "figure4_grid_summary.csv"))

selected_summary <- dynega %>%
  group_by(feedback_label, true_dim_label, Loadings, Error_sd) %>%
  summarise(
    score_nrmse = mean(score_nrmse_common, na.rm = TRUE),
    score_nrmse_ci = ci95(score_nrmse_common),
    score_cor = mean(score_cor_common, na.rm = TRUE),
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  )
readr::write_csv(selected_summary, file.path(out_dir, "figure4_selected_summary.csv"))

error_cols <- c(
  "0.125" = okabe[["blue"]],
  "0.250" = okabe[["orange"]],
  "0.500" = okabe[["vermillion"]]
)

selected_plot <- ggplot(
  selected_summary,
  aes(
    x = as.numeric(as.character(Loadings)),
    y = score_nrmse,
    color = Error_sd,
    group = Error_sd
  )
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  geom_errorbar(
    aes(ymin = pmax(0, score_nrmse - score_nrmse_ci), ymax = score_nrmse + score_nrmse_ci),
    width = 0.025,
    linewidth = 0.32,
    alpha = 0.7
  ) +
  facet_grid(true_dim_label ~ feedback_label) +
  scale_color_manual(values = error_cols, name = "Error SD") +
  scale_x_continuous(breaks = c(0.4, 0.6, 0.8), limits = c(0.37, 0.83)) +
  scale_y_continuous(limits = c(0, 1.12), breaks = seq(0, 1.0, by = 0.25)) +
  labs(
    x = "Loading strength",
    y = "Latent-score NRMSE"
  ) +
  theme_original_style(8.5) +
  theme(
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.title.y = element_text(margin = margin(r = 4))
  )
save_plot(selected_plot, "figure4_dynega_rr_selected", 7.6, 5.1)

hit_rate_summary <- dynega %>%
  group_by(feedback_label, true_dim_label, TimeP, NVAR) %>%
  summarise(
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    exact_recovery_ci = ci95(exact_recovery),
    rows = n(),
    .groups = "drop"
  )

hit_rate_plot <- ggplot(
  hit_rate_summary,
  aes(x = as.numeric(as.character(TimeP)), y = exact_recovery, color = NVAR, group = NVAR)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_grid(true_dim_label ~ feedback_label) +
  scale_color_manual(values = c("3" = okabe[["orange"]], "4" = okabe[["blue"]]), name = "NVAR") +
  scale_x_continuous(breaks = c(50, 100)) +
  scale_y_continuous(labels = label_percent(), limits = c(0.95, 1.005), breaks = c(0.95, 0.975, 1.0)) +
  labs(
    title = "Rejected literal hit-rate remake",
    subtitle = "The full DynEGA grid has perfect exact n.dim recovery, so this display hides the meaningful differences",
    x = "Time points / frames",
    y = "Exact recovery rate"
  ) +
  theme_original_style(8.2)
save_plot(hit_rate_plot, "figure4_candidate_rejected_hit_rate", 7.6, 5.1)

cor_summary <- dynega %>%
  group_by(feedback_label, Loadings, Q, Error_sd) %>%
  summarise(
    score_cor = mean(score_cor_common, na.rm = TRUE),
    score_cor_ci = ci95(score_cor_common),
    rows = n(),
    .groups = "drop"
  )

cor_plot <- ggplot(
  cor_summary,
  aes(x = as.numeric(as.character(Q)), y = score_cor, color = Error_sd, group = Error_sd)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  geom_errorbar(
    aes(ymin = pmax(0, score_cor - score_cor_ci), ymax = pmin(1, score_cor + score_cor_ci)),
    width = 0.03,
    linewidth = 0.32,
    alpha = 0.7
  ) +
  facet_grid(Loadings ~ feedback_label) +
  scale_color_manual(values = error_cols, name = "Error SD") +
  scale_x_continuous(breaks = c(0.50, 0.75, 1.00), limits = c(0.47, 1.03)) +
  scale_y_continuous(limits = c(0.73, 1.01), breaks = seq(0.75, 1.0, by = 0.05)) +
  labs(
    title = "Secondary candidate: score correlation by process-noise scale",
    subtitle = "Informative, but the spread is smaller than the NRMSE loading/error display",
    x = "Q",
    y = "Latent-score correlation"
  ) +
  theme_original_style(8.2)
save_plot(cor_plot, "figure4_candidate_score_correlation", 7.6, 5.1)

top_effects <- factor_effects %>%
  filter(response == "score_nrmse_common") %>%
  arrange(desc(partial_eta2)) %>%
  slice_head(n = 6) %>%
  transmute(
    term,
    partial_eta2 = format_decimal(partial_eta2, 3)
  )

readme_lines <- c(
  "# Signed-Feedback DynEGA RR Figure 4",
  "",
  "## Source",
  "",
  paste0("- Input CSV: `", input_csv, "`"),
  paste0("- Valid DynEGA main-grid rows: ", nrow(dynega)),
  paste0("- Exact n.dim recovery: ", format_decimal(exact_summary$exact_recovery_mean, 3),
         " (min=", format_decimal(exact_summary$exact_recovery_min, 3),
         ", max=", format_decimal(exact_summary$exact_recovery_max, 3), ")"),
  "",
  "## Selection",
  "",
  "The literal hit-rate remake was rejected because exact recovery is flat at 100% across the latest full DynEGA grid. The selected panel uses `score_nrmse_common`, where the full-grid differences are largest and interpretable.",
  "",
  "Top main effects for score NRMSE:",
  "",
  paste0("- `", top_effects$term, "`: partial eta2 = ", top_effects$partial_eta2),
  "",
  "Candidate scores are saved in `figure4_candidate_scores.csv`; selected plotting summaries are saved in `figure4_selected_summary.csv`.",
  "",
  "## Files",
  "",
  "- `figure4_dynega_rr_selected.{png,pdf}`",
  "- `figure4_candidate_rejected_hit_rate.{png,pdf}`",
  "- `figure4_candidate_score_correlation.{png,pdf}`",
  "- `figure4_factor_effects.csv`",
  "- `figure4_candidate_scores.csv`",
  "- `figure4_grid_summary.csv`",
  "- `figure4_selected_summary.csv`"
)
writeLines(readme_lines, file.path(out_dir, "README.md"))

caption_lines <- c(
  "Figure 4. DynEGA latent-score recovery across the full signed-feedback parameter grid.",
  "Points show mean latent-score NRMSE; error bars show 95% confidence intervals over the crossed simulation cells and replications.",
  "Panels split feedback regime and true dimensionality; colored lines split measurement-error standard deviation.",
  "The literal exact-dimensional hit-rate remake was not retained because exact n.dim recovery was 100% in all 58,320 latest DynEGA main-grid rows."
)
writeLines(caption_lines, file.path(out_dir, "figure4_dynega_rr_caption.txt"))

cat("Saved RR DynEGA Figure 4 bundle to ", out_dir, "\n", sep = "")
