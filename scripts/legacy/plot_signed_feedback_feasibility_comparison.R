#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_candidate <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  ""
}
script_path <- if (nzchar(script_candidate) && !identical(script_candidate, "-")) {
  normalizePath(script_candidate, mustWork = TRUE)
} else {
  normalizePath("plot_signed_feedback_feasibility_comparison.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

dynega_dir <- Sys.getenv(
  "SIGNED_FEEDBACK_DYNEGA_DIR",
  file.path(repo_root, "benchmark_outputs_signed_feedback_standardized")
)
graphicalvar_dir <- Sys.getenv(
  "SIGNED_FEEDBACK_GRAPHICALVAR_DIR",
  file.path(repo_root, "benchmark_outputs_signed_feedback_standardized_graphicalvar_12h")
)
out_dir <- Sys.getenv(
  "SIGNED_FEEDBACK_COMPARISON_OUT_DIR",
  file.path(repo_root, "comparison_signed_feedback_feasibility")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

required <- c(
  file.path(dynega_dir, "benchmark_results.csv"),
  file.path(graphicalvar_dir, "benchmark_results.csv")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop("Missing comparison input files: ", paste(missing, collapse = ", "), call. = FALSE)
}

okabe <- c(
  DynEGA = "#0072B2",
  GraphicalVAR = "#D55E00"
)

theme_report <- function(base_size = 9) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.18, color = "grey88"),
      strip.background = element_rect(fill = "grey95", color = "grey75"),
      strip.text = element_text(face = "bold"),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

save_report_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height)
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 320)
}

mean_or_na <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

se_or_na <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) <= 1L) {
    return(NA_real_)
  }
  stats::sd(x) / sqrt(length(x))
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

fmt_sec <- function(x) {
  ifelse(is.na(x), "--", paste0(formatC(x, format = "f", digits = 1), " s"))
}

fmt_ratio <- function(x) {
  ifelse(is.na(x), "--", paste0(formatC(x, format = "f", digits = 1), "x"))
}

append_md_table <- function(df, file) {
  if (nrow(df) == 0L) {
    cat("_No rows._\n\n", file = file, append = TRUE)
    return(invisible(NULL))
  }
  cat(paste(names(df), collapse = " | "), "\n", sep = "", file = file, append = TRUE)
  cat(paste(rep("---", ncol(df)), collapse = " | "), "\n", sep = "", file = file, append = TRUE)
  for (row_idx in seq_len(nrow(df))) {
    cat(paste(as.character(df[row_idx, , drop = TRUE]), collapse = " | "), "\n", sep = "", file = file, append = TRUE)
  }
  cat("\n", file = file, append = TRUE)
}

dynega_raw <- readr::read_csv(file.path(dynega_dir, "benchmark_results.csv"), show_col_types = FALSE)
graphicalvar_raw <- readr::read_csv(file.path(graphicalvar_dir, "benchmark_results.csv"), show_col_types = FALSE)

dynega <- dynega_raw %>%
  filter(method_family == "dynEGA", status == "ok", benchmark_grid == "feasibility")
graphicalvar <- graphicalvar_raw %>%
  filter(method_family == "graphicalVAR", benchmark_grid == "feasibility")

if (nrow(dynega) != 24L) {
  stop("Expected 24 valid DynEGA feasibility rows, found ", nrow(dynega), call. = FALSE)
}
if (nrow(graphicalvar) != 24L) {
  stop("Expected 24 GraphicalVAR feasibility rows, found ", nrow(graphicalvar), call. = FALSE)
}
if (any(graphicalvar$status != "ok")) {
  stop("GraphicalVAR comparison input contains non-ok rows.", call. = FALSE)
}

cell_key <- c(
  "scenario",
  "scenario_nfac",
  "N",
  "TimeP",
  "NVAR",
  "Error",
  "eta_scalar",
  "zeta_scalar",
  "feedback_regime"
)
if (!setequal(dynega$scenario, graphicalvar$scenario)) {
  stop("DynEGA and GraphicalVAR rows do not cover identical scenarios.", call. = FALSE)
}

combined <- bind_rows(dynega, graphicalvar) %>%
  mutate(
    method_display = recode(
      method_family,
      dynEGA = "DynEGA",
      graphicalVAR = "GraphicalVAR",
      .default = method_label
    ),
    method_display = factor(method_display, levels = c("DynEGA", "GraphicalVAR")),
    feedback_regime = factor(feedback_regime, levels = c("damped", "critical", "persistent")),
    eta_label = factor(sprintf("%.2f", eta_scalar), levels = rev(sprintf("%.2f", sort(unique(eta_scalar))))),
    zeta_label = factor(sprintf("%.2f", zeta_scalar), levels = sprintf("%.2f", sort(unique(zeta_scalar)))),
    panel_label = factor(
      paste0("N=", N, " / ", scenario_nfac, "D"),
      levels = c("N=50 / 2D", "N=50 / 4D", "N=100 / 2D", "N=100 / 4D")
    ),
    exact_recovery = n_dim_exact,
    nrmse = score_nrmse_common
  )

method_overview <- combined %>%
  group_by(method_display, method_family) %>%
  summarize(
    rows = n(),
    n_ok = sum(status == "ok"),
    exact_rate = mean(exact_recovery, na.rm = TRUE),
    assignment_accuracy = mean_or_na(assignment_accuracy),
    score_cor_common = mean_or_na(score_cor_common),
    nrmse = mean_or_na(nrmse),
    runtime_sec_mean = mean_or_na(runtime_sec),
    runtime_sec_median = stats::median(runtime_sec, na.rm = TRUE),
    runtime_sec_max = max(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  )

regime_summary <- combined %>%
  group_by(method_display, method_family, feedback_regime) %>%
  summarize(
    rows = n(),
    n_ok = sum(status == "ok"),
    exact_rate = mean(exact_recovery, na.rm = TRUE),
    exact_rate_se = se_or_na(exact_recovery),
    assignment_accuracy = mean_or_na(assignment_accuracy),
    assignment_accuracy_se = se_or_na(assignment_accuracy),
    score_cor_common = mean_or_na(score_cor_common),
    score_cor_common_se = se_or_na(score_cor_common),
    nrmse = mean_or_na(nrmse),
    nrmse_se = se_or_na(nrmse),
    runtime_sec_mean = mean_or_na(runtime_sec),
    runtime_sec_se = se_or_na(runtime_sec),
    .groups = "drop"
  )

dimension_summary <- combined %>%
  group_by(method_display, method_family, N, scenario_nfac, feedback_regime) %>%
  summarize(
    rows = n(),
    exact_rate = mean(exact_recovery, na.rm = TRUE),
    assignment_accuracy = mean_or_na(assignment_accuracy),
    score_cor_common = mean_or_na(score_cor_common),
    nrmse = mean_or_na(nrmse),
    runtime_sec_mean = mean_or_na(runtime_sec),
    .groups = "drop"
  )

wide <- combined %>%
  select(
    all_of(cell_key),
    method_family,
    exact_recovery,
    assignment_accuracy,
    score_cor_common,
    nrmse,
    runtime_sec
  ) %>%
  pivot_wider(
    names_from = method_family,
    values_from = c(exact_recovery, assignment_accuracy, score_cor_common, nrmse, runtime_sec)
  ) %>%
  mutate(
    delta_exact = exact_recovery_graphicalVAR - exact_recovery_dynEGA,
    delta_assignment = assignment_accuracy_graphicalVAR - assignment_accuracy_dynEGA,
    delta_score_cor = score_cor_common_graphicalVAR - score_cor_common_dynEGA,
    delta_nrmse = nrmse_graphicalVAR - nrmse_dynEGA,
    runtime_ratio_graphicalvar_to_dynega = runtime_sec_graphicalVAR / runtime_sec_dynEGA,
    eta_label = factor(sprintf("%.2f", eta_scalar), levels = rev(sprintf("%.2f", sort(unique(eta_scalar))))),
    zeta_label = factor(sprintf("%.2f", zeta_scalar), levels = sprintf("%.2f", sort(unique(zeta_scalar)))),
    panel_label = factor(
      paste0("N=", N, " / ", scenario_nfac, "D"),
      levels = c("N=50 / 2D", "N=50 / 4D", "N=100 / 2D", "N=100 / 4D")
    )
  )

readr::write_csv(combined, file.path(out_dir, "signed_feedback_feasibility_combined_results.csv"))
readr::write_csv(method_overview, file.path(out_dir, "signed_feedback_feasibility_method_overview.csv"))
readr::write_csv(regime_summary, file.path(out_dir, "signed_feedback_feasibility_regime_summary.csv"))
readr::write_csv(dimension_summary, file.path(out_dir, "signed_feedback_feasibility_dimension_summary.csv"))
readr::write_csv(wide, file.path(out_dir, "signed_feedback_feasibility_cell_comparison.csv"))

metric_long <- regime_summary %>%
  select(
    method_display,
    feedback_regime,
    exact_rate,
    exact_rate_se,
    assignment_accuracy,
    assignment_accuracy_se,
    score_cor_common,
    score_cor_common_se,
    nrmse,
    nrmse_se
  ) %>%
  pivot_longer(
    cols = c(exact_rate, assignment_accuracy, score_cor_common, nrmse),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    se = case_when(
      metric == "exact_rate" ~ exact_rate_se,
      metric == "assignment_accuracy" ~ assignment_accuracy_se,
      metric == "score_cor_common" ~ score_cor_common_se,
      metric == "nrmse" ~ nrmse_se,
      TRUE ~ NA_real_
    ),
    metric_label = recode(
      metric,
      exact_rate = "Exact recovery",
      assignment_accuracy = "Assignment accuracy",
      score_cor_common = "Score correlation",
      nrmse = "NRMSE"
    ),
    metric_label = factor(
      metric_label,
      levels = c("Exact recovery", "Assignment accuracy", "Score correlation", "NRMSE")
    )
  )

summary_plot <- ggplot(
  metric_long,
  aes(x = feedback_regime, y = value, color = method_display, group = method_display)
) +
  geom_line(position = position_dodge(width = 0.18), linewidth = 0.55) +
  geom_point(position = position_dodge(width = 0.18), size = 2.1) +
  geom_errorbar(
    aes(ymin = value - se, ymax = value + se),
    position = position_dodge(width = 0.18),
    width = 0.08,
    linewidth = 0.35,
    na.rm = TRUE
  ) +
  facet_wrap(~metric_label, scales = "free_y", nrow = 1) +
  scale_color_manual(values = okabe, name = NULL) +
  labs(
    title = "Signed-feedback feasibility: DynEGA vs GraphicalVAR",
    x = NULL,
    y = NULL
  ) +
  theme_report(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_report_plot(summary_plot, "signed_feedback_feasibility_regime_summary", 11.5, 3.2)

runtime_plot <- ggplot(
  dimension_summary,
  aes(x = feedback_regime, y = runtime_sec_mean, color = method_display, group = method_display)
) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 0.55) +
  geom_point(position = position_dodge(width = 0.2), size = 2.1) +
  facet_grid(scenario_nfac ~ N, labeller = label_both) +
  scale_color_manual(values = okabe, name = NULL) +
  scale_y_log10(labels = label_number(accuracy = 1)) +
  labs(
    title = "Runtime by signed-feedback regime",
    x = NULL,
    y = "Runtime seconds (log scale)"
  ) +
  theme_report(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_report_plot(runtime_plot, "signed_feedback_feasibility_runtime", 7.6, 5.2)

runtime_ratio_plot <- ggplot(wide, aes(x = zeta_label, y = eta_label, fill = runtime_ratio_graphicalvar_to_dynega)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = fmt_ratio(runtime_ratio_graphicalvar_to_dynega)), size = 2.5) +
  facet_wrap(~panel_label, nrow = 1) +
  scale_fill_viridis_c(option = "magma", trans = "log10", name = "Runtime ratio") +
  labs(
    title = "GraphicalVAR runtime multiplier over DynEGA",
    x = "zeta",
    y = "eta"
  ) +
  theme_report(base_size = 9) +
  theme(legend.position = "right")
save_report_plot(runtime_ratio_plot, "signed_feedback_feasibility_runtime_ratio_heatmap", 10.5, 3.2)

cell_metric_long <- combined %>%
  select(method_display, panel_label, eta_label, zeta_label, score_cor_common, nrmse, runtime_sec) %>%
  pivot_longer(
    cols = c(score_cor_common, nrmse, runtime_sec),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric_label = recode(
      metric,
      score_cor_common = "Score correlation",
      nrmse = "NRMSE",
      runtime_sec = "Runtime sec"
    ),
    metric_label = factor(metric_label, levels = c("Score correlation", "NRMSE", "Runtime sec"))
  )

cell_metric_plot <- ggplot(cell_metric_long, aes(x = zeta_label, y = eta_label, fill = value)) +
  geom_tile(color = "white", linewidth = 0.25) +
  facet_grid(metric_label + method_display ~ panel_label, scales = "free") +
  scale_fill_viridis_c(option = "viridis", name = "Value") +
  labs(
    title = "Cell-level metrics by method",
    x = "zeta",
    y = "eta"
  ) +
  theme_report(base_size = 7) +
  theme(legend.position = "right")
save_report_plot(cell_metric_plot, "signed_feedback_feasibility_cell_metric_heatmap", 12.5, 8.2)

overview_table <- method_overview %>%
  transmute(
    Method = as.character(method_display),
    Rows = rows,
    `Ok rows` = n_ok,
    `Exact recovery` = fmt_num(exact_rate, 3),
    `Assignment accuracy` = fmt_num(assignment_accuracy, 3),
    `Score correlation` = fmt_num(score_cor_common, 3),
    NRMSE = fmt_num(nrmse, 3),
    `Mean runtime` = fmt_sec(runtime_sec_mean),
    `Max runtime` = fmt_sec(runtime_sec_max)
  )

regime_table <- regime_summary %>%
  transmute(
    Method = as.character(method_display),
    Regime = as.character(feedback_regime),
    Rows = rows,
    `Exact recovery` = fmt_num(exact_rate, 3),
    `Assignment accuracy` = fmt_num(assignment_accuracy, 3),
    `Score correlation` = fmt_num(score_cor_common, 3),
    NRMSE = fmt_num(nrmse, 3),
    `Mean runtime` = fmt_sec(runtime_sec_mean)
  )

ratio_table <- wide %>%
  group_by(N, scenario_nfac, feedback_regime) %>%
  summarize(
    rows = n(),
    mean_runtime_ratio = mean(runtime_ratio_graphicalvar_to_dynega, na.rm = TRUE),
    max_runtime_ratio = max(runtime_ratio_graphicalvar_to_dynega, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    N = N,
    `Latent dimensions` = scenario_nfac,
    Regime = as.character(feedback_regime),
    Rows = rows,
    `Mean runtime ratio` = fmt_ratio(mean_runtime_ratio),
    `Max runtime ratio` = fmt_ratio(max_runtime_ratio)
  )

report_path <- file.path(out_dir, "signed_feedback_feasibility_comparison_report.md")
cat("# Signed-Feedback Feasibility Comparison\n\n", file = report_path)
cat("Inputs:\n\n", file = report_path, append = TRUE)
cat("- DynEGA: `", dynega_dir, "/benchmark_results.csv`\n", sep = "", file = report_path, append = TRUE)
cat("- GraphicalVAR: `", graphicalvar_dir, "/benchmark_results.csv`\n\n", sep = "", file = report_path, append = TRUE)
cat("This report uses only valid `ok` rows from the corrected 24-cell feasibility grid. The invalidated GraphicalVAR timeout rows from the earlier forked-timeout run are not used.\n\n", file = report_path, append = TRUE)

cat("## Method Overview\n\n", file = report_path, append = TRUE)
append_md_table(overview_table, report_path)

cat("## Regime Summary\n\n", file = report_path, append = TRUE)
append_md_table(regime_table, report_path)

cat("## Runtime Ratio Summary\n\n", file = report_path, append = TRUE)
append_md_table(ratio_table, report_path)

cat("## Figures\n\n", file = report_path, append = TRUE)
cat("- `signed_feedback_feasibility_regime_summary.png`\n", file = report_path, append = TRUE)
cat("- `signed_feedback_feasibility_runtime.png`\n", file = report_path, append = TRUE)
cat("- `signed_feedback_feasibility_runtime_ratio_heatmap.png`\n", file = report_path, append = TRUE)
cat("- `signed_feedback_feasibility_cell_metric_heatmap.png`\n\n", file = report_path, append = TRUE)

cat("## Interpretation\n\n", file = report_path, append = TRUE)
cat("Both methods achieved exact dimensional recovery and perfect assignment accuracy in all corrected feasibility cells. Because both methods recovered the same item partitions, common-score correlation and NRMSE are identical by construction in this comparison. The differentiating result is computational cost: GraphicalVAR completed properly, but with much larger runtime, especially in 4D persistent cells.\n", file = report_path, append = TRUE)

cat("Saved signed-feedback feasibility comparison outputs to ", out_dir, "\n", sep = "")
