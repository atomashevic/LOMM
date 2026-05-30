#!/usr/bin/env Rscript

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
  normalizePath("plot_signed_zeta_smoke_report.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

input_dir <- Sys.getenv(
  "SIGNED_ZETA_BENCHMARK_DIR",
  file.path(repo_root, "benchmark_outputs_signed_zeta_smoke")
)
out_dir <- Sys.getenv(
  "SIGNED_ZETA_REPORT_OUT_DIR",
  file.path(repo_root, "comparison_signed_zeta_smoke")
)
report_title <- Sys.getenv("SIGNED_ZETA_REPORT_TITLE", "Signed-Zeta Smoke Benchmark Report")
plot_title_prefix <- Sys.getenv("SIGNED_ZETA_PLOT_TITLE_PREFIX", "Signed-zeta smoke")
report_file <- Sys.getenv("SIGNED_ZETA_REPORT_FILE", "signed_zeta_smoke_report.md")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  "benchmark_results.csv",
  "signed_zeta_admissibility_grid.csv",
  "signed_zeta_benchmark_grid.csv",
  "signed_zeta_summary_by_regime.csv",
  "signed_zeta_metadata.txt"
)
missing_files <- required_files[!file.exists(file.path(input_dir, required_files))]
if (length(missing_files) > 0L) {
  stop(
    "Missing signed-zeta smoke inputs: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

okabe <- c(
  blue = "#0072B2",
  vermillion = "#D55E00",
  green = "#009E73",
  orange = "#E69F00",
  purple = "#CC79A7",
  sky = "#56B4E9",
  black = "#000000"
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

max_or_na <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

se_or_na <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) <= 1L) {
    return(NA_real_)
  }
  stats::sd(x) / sqrt(length(x))
}

format_percent <- function(x, digits = 1) {
  ifelse(is.na(x), "--", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

format_sec <- function(x) {
  ifelse(is.na(x), "--", paste0(formatC(x, format = "f", digits = 2), " s"))
}

fmt_scalar <- function(x) {
  formatC(x, format = "f", digits = 2)
}

escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("_", "\\\\_", x)
  x
}

append_md_table <- function(df, file) {
  if (nrow(df) == 0L) {
    cat("_No rows._\n\n", file = file, append = TRUE)
    return(invisible(NULL))
  }
  cat(paste(names(df), collapse = " | "), "\n", sep = "", file = file, append = TRUE)
  cat(paste(rep("---", ncol(df)), collapse = " | "), "\n", sep = "", file = file, append = TRUE)
  for (row_idx in seq_len(nrow(df))) {
    cat(
      paste(as.character(df[row_idx, , drop = TRUE]), collapse = " | "),
      "\n",
      sep = "",
      file = file,
      append = TRUE
    )
  }
  cat("\n", file = file, append = TRUE)
}

write_latex_table <- function(df, path, caption, label) {
  conn <- file(path, open = "wt")
  on.exit(close(conn), add = TRUE)
  align <- paste(rep("l", ncol(df)), collapse = "")
  cat("\\begin{table}[ht]\n\\centering\n", file = conn)
  cat("\\caption{", escape_latex(caption), "}\n", sep = "", file = conn)
  cat("\\label{", escape_latex(label), "}\n", sep = "", file = conn)
  cat("\\begin{tabular}{", align, "}\n\\hline\n", sep = "", file = conn)
  cat(paste(escape_latex(names(df)), collapse = " & "), " \\\\\n\\hline\n", sep = "", file = conn)
  for (row_idx in seq_len(nrow(df))) {
    cat(
      paste(escape_latex(df[row_idx, , drop = TRUE]), collapse = " & "),
      " \\\\\n",
      sep = "",
      file = conn
    )
  }
  cat("\\hline\n\\end{tabular}\n\\end{table}\n", file = conn)
}

results <- readr::read_csv(file.path(input_dir, "benchmark_results.csv"), show_col_types = FALSE)
admissibility <- readr::read_csv(
  file.path(input_dir, "signed_zeta_admissibility_grid.csv"),
  show_col_types = FALSE
)
runnable_grid <- readr::read_csv(
  file.path(input_dir, "signed_zeta_benchmark_grid.csv"),
  show_col_types = FALSE
)

results <- results %>%
  mutate(
    success = status == "ok",
    exact_all = if_else(success, n_dim_exact, 0),
    method_display = recode(
      method_family,
      dynEGA = "DynEGA",
      graphicalVAR = "GraphicalVAR",
      .default = method_label
    ),
    method_display = factor(method_display, levels = c("DynEGA", "GraphicalVAR")),
    feedback_regime = factor(feedback_regime, levels = c("damped", "critical", "persistent")),
    eta_label = factor(fmt_scalar(eta_scalar), levels = rev(fmt_scalar(sort(unique(eta_scalar))))),
    zeta_label = factor(fmt_scalar(zeta_scalar), levels = fmt_scalar(sort(unique(zeta_scalar)))),
    panel_label = factor(
      paste0(TimeP, " frames / ", scenario_nfac, "D"),
      levels = c("50 frames / 2D", "50 frames / 4D", "100 frames / 2D", "100 frames / 4D")
    )
  )

admissibility <- admissibility %>%
  mutate(
    feedback_regime = factor(feedback_regime, levels = c("damped", "critical", "persistent")),
    eta_label = factor(fmt_scalar(eta_scalar), levels = rev(fmt_scalar(sort(unique(eta_scalar))))),
    zeta_label = factor(fmt_scalar(zeta_scalar), levels = fmt_scalar(sort(unique(zeta_scalar)))),
    growth_log10 = log10(pmax(finite_growth, 1e-8)),
    growth_label = if_else(kept, formatC(finite_growth, format = "fg", digits = 2), "excluded")
  )

method_overview <- results %>%
  group_by(method_display, method_family) %>%
  summarize(
    rows = n(),
    n_ok = sum(success, na.rm = TRUE),
    n_timeout = sum(status == "timeout", na.rm = TRUE),
    n_error = sum(!success & status != "timeout", na.rm = TRUE),
    exact_rate = mean(exact_all, na.rm = TRUE),
    exact_rate_se = se_or_na(exact_all),
    assignment_accuracy = mean_or_na(assignment_accuracy[success]),
    assignment_accuracy_se = se_or_na(assignment_accuracy[success]),
    score_cor_common = mean_or_na(score_cor_common[success]),
    score_cor_common_se = se_or_na(score_cor_common[success]),
    score_nrmse_common = mean_or_na(score_nrmse_common[success]),
    score_nrmse_common_se = se_or_na(score_nrmse_common[success]),
    runtime_sec = mean_or_na(runtime_sec),
    runtime_sec_se = se_or_na(runtime_sec),
    .groups = "drop"
  ) %>%
  arrange(method_display)

method_regime_summary <- results %>%
  group_by(method_display, method_family, feedback_regime) %>%
  summarize(
    rows = n(),
    n_ok = sum(success, na.rm = TRUE),
    n_timeout = sum(status == "timeout", na.rm = TRUE),
    n_error = sum(!success & status != "timeout", na.rm = TRUE),
    exact_rate = mean(exact_all, na.rm = TRUE),
    exact_rate_se = se_or_na(exact_all),
    assignment_accuracy = mean_or_na(assignment_accuracy[success]),
    assignment_accuracy_se = se_or_na(assignment_accuracy[success]),
    score_cor_common = mean_or_na(score_cor_common[success]),
    score_cor_common_se = se_or_na(score_cor_common[success]),
    score_nrmse_common = mean_or_na(score_nrmse_common[success]),
    score_nrmse_common_se = se_or_na(score_nrmse_common[success]),
    runtime_sec = mean_or_na(runtime_sec),
    runtime_sec_se = se_or_na(runtime_sec),
    .groups = "drop"
  ) %>%
  arrange(method_display, feedback_regime)

method_zeta_summary <- results %>%
  group_by(method_display, method_family, feedback_regime, zeta_scalar) %>%
  summarize(
    rows = n(),
    n_ok = sum(success, na.rm = TRUE),
    exact_rate = mean(exact_all, na.rm = TRUE),
    exact_rate_se = se_or_na(exact_all),
    assignment_accuracy = mean_or_na(assignment_accuracy[success]),
    assignment_accuracy_se = se_or_na(assignment_accuracy[success]),
    score_cor_common = mean_or_na(score_cor_common[success]),
    score_cor_common_se = se_or_na(score_cor_common[success]),
    score_nrmse_common = mean_or_na(score_nrmse_common[success]),
    score_nrmse_common_se = se_or_na(score_nrmse_common[success]),
    runtime_sec = mean_or_na(runtime_sec),
    runtime_sec_se = se_or_na(runtime_sec),
    .groups = "drop"
  ) %>%
  arrange(method_display, zeta_scalar)

horizon_dimension_summary <- results %>%
  group_by(method_display, method_family, TimeP, scenario_nfac) %>%
  summarize(
    rows = n(),
    n_ok = sum(success, na.rm = TRUE),
    exact_rate = mean(exact_all, na.rm = TRUE),
    exact_rate_se = se_or_na(exact_all),
    assignment_accuracy = mean_or_na(assignment_accuracy[success]),
    assignment_accuracy_se = se_or_na(assignment_accuracy[success]),
    score_cor_common = mean_or_na(score_cor_common[success]),
    score_cor_common_se = se_or_na(score_cor_common[success]),
    score_nrmse_common = mean_or_na(score_nrmse_common[success]),
    score_nrmse_common_se = se_or_na(score_nrmse_common[success]),
    runtime_sec = mean_or_na(runtime_sec),
    runtime_sec_se = se_or_na(runtime_sec),
    .groups = "drop"
  ) %>%
  arrange(method_display, TimeP, scenario_nfac)

growth_filter_summary <- admissibility %>%
  group_by(zeta_scalar, TimeP) %>%
  summarize(
    candidate_cells = n(),
    retained_cells = sum(kept, na.rm = TRUE),
    excluded_cells = sum(!kept, na.rm = TRUE),
    max_retained_growth = max_or_na(finite_growth[kept]),
    max_candidate_growth = max(finite_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(zeta_scalar, TimeP)

excluded_cells <- admissibility %>%
  filter(!kept) %>%
  arrange(TimeP, zeta_scalar, eta_scalar, scenario_nfac)

write_csv(method_overview, file.path(out_dir, "signed_zeta_method_overview.csv"))
write_csv(method_regime_summary, file.path(out_dir, "signed_zeta_regime_summary.csv"))
write_csv(method_zeta_summary, file.path(out_dir, "signed_zeta_zeta_summary.csv"))
write_csv(horizon_dimension_summary, file.path(out_dir, "signed_zeta_horizon_dimension_summary.csv"))
write_csv(growth_filter_summary, file.path(out_dir, "signed_zeta_growth_filter_summary.csv"))
write_csv(excluded_cells, file.path(out_dir, "signed_zeta_excluded_cells.csv"))

overview_display <- method_overview %>%
  transmute(
    Method = as.character(method_display),
    Rows = rows,
    OK = n_ok,
    Timeouts = n_timeout,
    Errors = n_error,
    Exact = format_percent(exact_rate),
    Assignment = format_percent(assignment_accuracy),
    `Score cor.` = format_num(score_cor_common),
    NRMSE = format_num(score_nrmse_common),
    `Runtime (fixed-lambda smoke)` = format_sec(runtime_sec)
  )

regime_display <- method_regime_summary %>%
  transmute(
    Method = as.character(method_display),
    Regime = as.character(feedback_regime),
    Rows = rows,
    OK = n_ok,
    Exact = format_percent(exact_rate),
    Assignment = format_percent(assignment_accuracy),
    `Score cor.` = format_num(score_cor_common),
    NRMSE = format_num(score_nrmse_common),
    `Runtime (fixed-lambda smoke)` = format_sec(runtime_sec)
  )

growth_display <- growth_filter_summary %>%
  transmute(
    Zeta = fmt_scalar(zeta_scalar),
    Frames = TimeP,
    Candidate = candidate_cells,
    Retained = retained_cells,
    Excluded = excluded_cells,
    `Max retained growth` = format_num(max_retained_growth),
    `Max candidate growth` = format_num(max_candidate_growth)
  )

excluded_display <- excluded_cells %>%
  distinct(eta_scalar, zeta_scalar, TimeP, dt, growth_horizon, finite_growth, growth_cap, exclusion_reason) %>%
  transmute(
    Eta = fmt_scalar(eta_scalar),
    Zeta = fmt_scalar(zeta_scalar),
    Frames = TimeP,
    dt = fmt_scalar(dt),
    Horizon = format_num(growth_horizon, digits = 1),
    Growth = format_num(finite_growth),
    Cap = format_num(growth_cap),
    Reason = exclusion_reason
  )

write_latex_table(
  overview_display,
  file.path(out_dir, "signed_zeta_method_overview.tex"),
  paste0(report_title, " method overview."),
  "tab:signed-zeta-method-overview"
)
write_latex_table(
  regime_display,
  file.path(out_dir, "signed_zeta_regime_summary.tex"),
  paste0(report_title, " summary by feedback regime."),
  "tab:signed-zeta-regime-summary"
)
write_latex_table(
  growth_display,
  file.path(out_dir, "signed_zeta_growth_filter_summary.tex"),
  "Signed-zeta finite-horizon growth filter summary.",
  "tab:signed-zeta-growth-filter"
)

heatmap_data <- results %>%
  group_by(method_display, panel_label, TimeP, scenario_nfac, eta_label, zeta_label) %>%
  summarize(
    exact_rate = mean(exact_all, na.rm = TRUE),
    assignment_accuracy = mean_or_na(assignment_accuracy[success]),
    score_cor_common = mean_or_na(score_cor_common[success]),
    score_nrmse_common = mean_or_na(score_nrmse_common[success]),
    runtime_sec = mean_or_na(runtime_sec),
    .groups = "drop"
  ) %>%
  arrange(method_display, TimeP, scenario_nfac)

make_heatmap <- function(metric, title, fill_label, higher_is_better = TRUE, digits = 2) {
  plot_df <- heatmap_data %>%
    mutate(
      metric_value = .data[[metric]],
      label = if (metric %in% c("exact_rate", "assignment_accuracy")) {
        format_percent(metric_value, digits = 0)
      } else if (metric == "runtime_sec") {
        formatC(metric_value, format = "f", digits = 2)
      } else {
        formatC(metric_value, format = "f", digits = digits)
      }
    )

  fill_scale <- if (higher_is_better) {
    scale_fill_gradient(low = okabe[["vermillion"]], high = okabe[["blue"]], name = fill_label)
  } else {
    scale_fill_gradient(low = okabe[["blue"]], high = okabe[["vermillion"]], name = fill_label)
  }

  ggplot(plot_df, aes(x = zeta_label, y = eta_label, fill = metric_value)) +
    geom_tile(color = "white", linewidth = 0.35) +
    geom_text(aes(label = label), size = 2.2, color = "grey10") +
    facet_grid(method_display ~ panel_label) +
    fill_scale +
    labs(title = title, x = "zeta", y = "eta") +
    theme_report(base_size = 8) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing = grid::unit(0.35, "lines")
    )
}

save_report_plot(
  make_heatmap("exact_rate", paste0(plot_title_prefix, ": exact dimensional recovery"), "Exact", TRUE, 2),
  "signed_zeta_exact_recovery_heatmap",
  width = 11.5,
  height = 5.6
)
save_report_plot(
  make_heatmap("assignment_accuracy", paste0(plot_title_prefix, ": assignment accuracy"), "Assignment", TRUE, 2),
  "signed_zeta_assignment_accuracy_heatmap",
  width = 11.5,
  height = 5.6
)
save_report_plot(
  make_heatmap("score_cor_common", paste0(plot_title_prefix, ": common-score correlation"), "Correlation", TRUE, 2),
  "signed_zeta_score_correlation_heatmap",
  width = 11.5,
  height = 5.6
)
save_report_plot(
  make_heatmap("score_nrmse_common", paste0(plot_title_prefix, ": common-score NRMSE"), "NRMSE", FALSE, 2),
  "signed_zeta_nrmse_heatmap",
  width = 11.5,
  height = 5.6
)
save_report_plot(
  make_heatmap("runtime_sec", paste0(plot_title_prefix, ": fixed-lambda smoke runtime"), "Seconds", FALSE, 2),
  "signed_zeta_runtime_heatmap",
  width = 11.5,
  height = 5.6
)

regime_long <- dplyr::bind_rows(
  method_regime_summary %>%
    transmute(method_display, feedback_regime, metric = "Exact recovery", value = exact_rate, se = exact_rate_se),
  method_regime_summary %>%
    transmute(method_display, feedback_regime, metric = "Assignment accuracy", value = assignment_accuracy, se = assignment_accuracy_se),
  method_regime_summary %>%
    transmute(method_display, feedback_regime, metric = "Score correlation", value = score_cor_common, se = score_cor_common_se),
  method_regime_summary %>%
    transmute(method_display, feedback_regime, metric = "NRMSE", value = score_nrmse_common, se = score_nrmse_common_se)
) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("Exact recovery", "Assignment accuracy", "Score correlation", "NRMSE")
    ),
    lower = value - se,
    upper = value + se,
    lower = if_else(metric %in% c("Exact recovery", "Assignment accuracy", "Score correlation"), pmax(0, lower), lower),
    upper = if_else(metric %in% c("Exact recovery", "Assignment accuracy", "Score correlation"), pmin(1, upper), upper)
  )

regime_plot <- ggplot(
  regime_long,
  aes(x = feedback_regime, y = value, color = method_display, group = method_display)
) +
  geom_line(linewidth = 0.45) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.08, alpha = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c(DynEGA = okabe[["blue"]], GraphicalVAR = okabe[["vermillion"]]), name = NULL) +
  labs(title = paste0(plot_title_prefix, ": method behavior by feedback regime"), x = NULL, y = NULL) +
  theme_report(base_size = 8) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_report_plot(regime_plot, "signed_zeta_regime_summary", width = 9.2, height = 3.2)
save_report_plot(regime_plot, "signed_zeta_regime_summary_no_runtime", width = 9.2, height = 3.2)

growth_plot_data <- admissibility %>%
  distinct(eta_scalar, zeta_scalar, TimeP, eta_label, zeta_label, finite_growth, growth_log10, kept)

growth_plot <- ggplot(growth_plot_data, aes(x = zeta_label, y = eta_label, fill = growth_log10)) +
  geom_tile(aes(color = kept), linewidth = 0.45) +
  geom_text(
    aes(label = if_else(kept, formatC(finite_growth, format = "fg", digits = 2), "excluded")),
    size = 2.35,
    color = "grey10"
  ) +
  facet_wrap(~TimeP, nrow = 1, labeller = labeller(TimeP = function(x) paste0(x, " frames"))) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") +
  scale_fill_gradient2(
    low = okabe[["blue"]],
    mid = "white",
    high = okabe[["vermillion"]],
    midpoint = 0,
    name = "log10 growth"
  ) +
  labs(
    title = "Signed-zeta finite-horizon growth filter",
    subtitle = "Cells above the 5x growth cap are excluded before fitting.",
    x = "zeta",
    y = "eta"
  ) +
  theme_report(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

save_report_plot(growth_plot, "signed_zeta_growth_filter_heatmap", width = 8.8, height = 4.6)

report_path <- file.path(out_dir, report_file)
cat(
  "# ", report_title, "\n\n",
  "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n",
  "Input directory: `", input_dir, "`\n\n",
  "Output directory: `", out_dir, "`\n\n",
  sep = "",
  file = report_path
)

cat("## Acceptance Snapshot\n\n", file = report_path, append = TRUE)
cat(
  "- Candidate cells: ", nrow(admissibility), "\n",
  "- Unique runnable cells: ", nrow(distinct(runnable_grid, scenario, scenario_nfac, TimeP, eta_scalar, zeta_scalar)), "\n",
  "- Runnable scenario rows: ", nrow(runnable_grid), "\n",
  "- Replications: ", max(runnable_grid$rep, na.rm = TRUE), "\n",
  "- Result rows: ", nrow(results), "\n",
  "- Methods: ", paste(sort(unique(results$method_family)), collapse = ", "), "\n",
  "- GIMME/PCA rows: ", sum(results$method_family %in% c("gimme", "pca")), "\n",
  "- Max retained finite growth: ", format_num(max(results$finite_growth, na.rm = TRUE)), "\n",
  "- Growth horizons: ", paste(sort(unique(results$growth_horizon)), collapse = ", "), "\n\n",
  sep = "",
  file = report_path,
  append = TRUE
)

cat("## Method Overview\n\n", file = report_path, append = TRUE)
append_md_table(overview_display, report_path)

cat("## Feedback-Regime Summary\n\n", file = report_path, append = TRUE)
append_md_table(regime_display, report_path)

cat("## Growth Filter Summary\n\n", file = report_path, append = TRUE)
append_md_table(growth_display, report_path)

cat("## Excluded Unique Cells\n\n", file = report_path, append = TRUE)
append_md_table(excluded_display, report_path)

cat("## Figures\n\n", file = report_path, append = TRUE)
figure_files <- c(
  "signed_zeta_exact_recovery_heatmap.png",
  "signed_zeta_assignment_accuracy_heatmap.png",
  "signed_zeta_score_correlation_heatmap.png",
  "signed_zeta_nrmse_heatmap.png",
  "signed_zeta_runtime_heatmap.png",
  "signed_zeta_regime_summary_no_runtime.png",
  "signed_zeta_growth_filter_heatmap.png"
)
for (figure_file in figure_files) {
  cat("- `", figure_file, "`\n", sep = "", file = report_path, append = TRUE)
}

cat("\n## Interpretation Guardrails\n\n", file = report_path, append = TRUE)
cat(
  "- This is a smoke benchmark, not a replacement for the completed revision benchmark.\n",
  "- Positive `zeta` retained here is finite-horizon persistence under the growth cap, not damping.\n",
  "- GraphicalVAR uses the bounded fixed-lambda smoke setting recorded in `signed_zeta_metadata.txt`.\n",
  "- Runtime values in this report are fixed-lambda smoke runtimes and are not comparable to the full revision benchmark runtime table.\n",
  "- Do not merge these rows into the main revision benchmark until the pattern is inspected.\n",
  sep = "",
  file = report_path,
  append = TRUE
)

cat("Saved signed-zeta smoke report pack to ", out_dir, "\n", sep = "")
