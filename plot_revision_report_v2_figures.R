#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(readr)
  library(scales)
  library(patchwork)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_revision_report_v2_figures.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

out_dir <- Sys.getenv(
  "REVISION_REPORT_V2_OUT_DIR",
  file.path(repo_root, "comparison_revision_report_v2")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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
      panel.grid.major = element_line(linewidth = 0.2, color = "grey88"),
      strip.background = element_rect(fill = "grey95", color = "grey75"),
      strip.text = element_text(face = "bold"),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

save_report_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 320)
}

format_percent <- function(x, digits = 1) {
  ifelse(is.na(x), "--", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

format_sec <- function(x) {
  ifelse(is.na(x), "--", paste0(formatC(x, format = "f", digits = 1), " s"))
}

escape_latex <- function(x) {
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("_", "\\\\_", x)
  x
}

# Benchmark difference figure ------------------------------------------------

combined_path <- file.path(repo_root, "benchmark_outputs_revision_summary", "full_suite_combined_results.csv")
combined <- readr::read_csv(combined_path, show_col_types = FALSE)

main <- combined %>%
  filter(comparison_scope == "main_full_grid", method_family %in% c("dynEGA", "graphicalVAR")) %>%
  mutate(
    method_group = if_else(method_family == "dynEGA", "DynEGA", "GraphicalVAR"),
    exact_all = status == "ok" & n_dim_exact == 1
  )

method_overview <- main %>%
  group_by(method_group) %>%
  summarise(
    runs = n(),
    success_rate = mean(status == "ok"),
    timeout_rate = mean(status == "timeout"),
    exact_rate = mean(exact_all),
    score_cor = mean(score_cor_common, na.rm = TRUE),
    nrmse = mean(score_nrmse_common, na.rm = TRUE),
    runtime = mean(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(method_group = factor(method_group, levels = c("DynEGA", "GraphicalVAR")))

p_exact <- ggplot(method_overview, aes(x = method_group, y = exact_rate, fill = method_group)) +
  geom_col(width = 0.62, color = "grey25", linewidth = 0.25) +
  geom_text(aes(label = percent(exact_rate, accuracy = 0.1)), vjust = -0.35, size = 3) +
  scale_fill_manual(values = c(DynEGA = okabe[["blue"]], GraphicalVAR = okabe[["vermillion"]])) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "A. Exact dimensional recovery", x = NULL, y = "Full-grid rows") +
  theme_report() +
  theme(legend.position = "none")

p_runtime <- ggplot(method_overview, aes(x = method_group, y = runtime, fill = method_group)) +
  geom_col(width = 0.62, color = "grey25", linewidth = 0.25) +
  geom_text(aes(label = format_sec(runtime)), vjust = -0.35, size = 3) +
  scale_fill_manual(values = c(DynEGA = okabe[["blue"]], GraphicalVAR = okabe[["vermillion"]])) +
  scale_y_continuous(trans = "log10", labels = label_number(), expand = expansion(mult = c(0.02, 0.18))) +
  labs(title = "B. Mean runtime", x = NULL, y = "Seconds, log scale") +
  theme_report() +
  theme(legend.position = "none")

mode_summary <- main %>%
  group_by(method_group, scenario_mode, scenario_nfac) %>%
  summarise(
    exact_rate = mean(exact_all),
    timeout_rate = mean(status == "timeout"),
    runtime_median = median(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mode_label = recode(
      scenario_mode,
      low_single_stable = "Low signal\nstable",
      high_single_stable = "High signal\nstable",
      high_switch_stable = "High signal\nswitch",
      high_switch_volatile = "High signal\nvolatile"
    ),
    mode_label = factor(
      mode_label,
      levels = c("Low signal\nstable", "High signal\nstable", "High signal\nswitch", "High signal\nvolatile")
    ),
    true_dim = paste0(scenario_nfac, "D")
  )

wide_mode <- mode_summary %>%
  select(method_group, scenario_mode, mode_label, scenario_nfac, true_dim, exact_rate, timeout_rate, runtime_median) %>%
  pivot_wider(
    names_from = method_group,
    values_from = c(exact_rate, timeout_rate, runtime_median)
  ) %>%
  mutate(
    exact_advantage = exact_rate_DynEGA - exact_rate_GraphicalVAR,
    runtime_ratio = runtime_median_GraphicalVAR / runtime_median_DynEGA,
    timeout_gap = timeout_rate_GraphicalVAR - timeout_rate_DynEGA
  )

p_advantage <- ggplot(wide_mode, aes(x = true_dim, y = mode_label, fill = exact_advantage)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = paste0(sprintf("%+.1f", 100 * exact_advantage), " pp")), size = 2.8) +
  scale_fill_gradient2(
    low = okabe[["vermillion"]],
    mid = "white",
    high = okabe[["blue"]],
    midpoint = 0,
    labels = label_percent(accuracy = 1),
    name = "DynEGA minus\nGraphicalVAR"
  ) +
  labs(title = "C. Exact-recovery difference by scenario", x = "True dimensions", y = NULL) +
  theme_report() +
  theme(legend.position = "right", legend.key.height = grid::unit(0.35, "in"))

p_ratio <- ggplot(wide_mode, aes(x = true_dim, y = mode_label, fill = runtime_ratio)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = paste0(formatC(runtime_ratio, format = "f", digits = 0), "x")), size = 2.8) +
  scale_fill_viridis_c(trans = "log10", option = "cividis", name = "Median runtime\nratio") +
  labs(title = "D. Runtime cost: GraphicalVAR / DynEGA", x = "True dimensions", y = NULL) +
  theme_report() +
  theme(legend.position = "right", legend.key.height = grid::unit(0.35, "in"))

gimme <- combined %>%
  filter(method_family == "gimme", comparison_scope == "gimme_bounded") %>%
  mutate(
    status_group = case_when(
      status == "ok" ~ "Completed",
      status == "timeout" ~ "Timed out",
      TRUE ~ "Error"
    ),
    mode_label = recode(
      scenario_mode,
      low_single_stable = "Low signal\nstable",
      high_single_stable = "High signal\nstable",
      high_switch_stable = "High signal\nswitch",
      high_switch_volatile = "High signal\nvolatile"
    ),
    mode_label = factor(
      mode_label,
      levels = c("Low signal\nstable", "High signal\nstable", "High signal\nswitch", "High signal\nvolatile")
    ),
    true_dim = paste0(scenario_nfac, "D")
  )

gimme_status <- gimme %>%
  count(mode_label, true_dim, status_group) %>%
  group_by(mode_label, true_dim) %>%
  mutate(rate = n / sum(n)) %>%
  ungroup() %>%
  mutate(status_group = factor(status_group, levels = c("Completed", "Timed out", "Error")))

p_gimme <- ggplot(gimme_status, aes(x = true_dim, y = rate, fill = status_group)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.25) +
  facet_wrap(~mode_label, nrow = 1) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c(Completed = okabe[["green"]], `Timed out` = okabe[["orange"]], Error = "grey35"), name = NULL) +
  labs(title = "E. GIMME bounded-feasibility status", x = "True dimensions", y = "Rows") +
  theme_report() +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), legend.title = element_blank())

benchmark_plot <- (p_exact + p_runtime) / (p_advantage + p_ratio) / p_gimme +
  plot_layout(heights = c(0.88, 1.1, 0.95))

save_report_plot(benchmark_plot, "benchmark_differences_v2", width = 9.2, height = 10.2)

benchmark_rows <- method_overview %>%
  arrange(method_group) %>%
  transmute(
    Method = as.character(method_group),
    Scope = "Full grid",
    Runs = as.character(runs),
    Success = format_percent(success_rate),
    Timeout = format_percent(timeout_rate),
    `Exact recovery` = format_percent(exact_rate),
    `Score cor.` = format_num(score_cor),
    NRMSE = format_num(nrmse),
    Runtime = format_sec(runtime)
  )

gimme_overview <- combined %>%
  filter(method_family == "gimme", comparison_scope == "gimme_bounded") %>%
  summarise(
    Method = "GIMME",
    Scope = "Bounded feasibility",
    Runs = as.character(n()),
    Success = format_percent(mean(status == "ok")),
    Timeout = format_percent(mean(status == "timeout")),
    `Exact recovery` = format_percent(mean(status == "ok" & n_dim_exact == 1)),
    `Score cor.` = format_num(mean(score_cor_common, na.rm = TRUE)),
    NRMSE = format_num(mean(score_nrmse_common, na.rm = TRUE)),
    Runtime = format_sec(mean(runtime_sec, na.rm = TRUE))
  )

benchmark_rows <- bind_rows(benchmark_rows, gimme_overview)
benchmark_table <- c(
  "\\begin{tabular}{llrrrrrrr}",
  "\\toprule",
  "Method & Scope & Runs & Success & Timeout & Exact & Cor. & NRMSE & Runtime \\\\",
  "\\midrule",
  apply(benchmark_rows, 1, function(row) {
    paste(escape_latex(unname(row)), collapse = " & ") |> paste0(" \\\\")
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(benchmark_table, file.path(out_dir, "benchmark_overview_table_v2.tex"))

# OAI-large metric table and violin figure ----------------------------------

oai_dir <- file.path(repo_root, "empirical_outputs_smoke", "mafw-sample", "oai-large-100")
emp_raw <- readr::read_csv(file.path(oai_dir, "empirical_raw_metrics.csv"), show_col_types = FALSE)
syn_raw <- readr::read_csv(file.path(oai_dir, "heterogeneous_synthetic_raw_metrics.csv"), show_col_types = FALSE)
emp_latent <- readr::read_csv(file.path(oai_dir, "empirical_latent_metrics.csv"), show_col_types = FALSE)
syn_latent <- readr::read_csv(file.path(oai_dir, "heterogeneous_synthetic_latent_metrics.csv"), show_col_types = FALSE)
metric_compare <- readr::read_csv(file.path(oai_dir, "heterogeneous_metric_comparison.csv"), show_col_types = FALSE)

violin_metrics <- c("sample_entropy", "zero_crossing_rate", "lag1_acf", "std_value", "total_variation")
metric_labels <- c(
  sample_entropy = "Sample entropy",
  zero_crossing_rate = "Zero-crossing rate",
  lag1_acf = "Lag-1 ACF",
  std_value = "Standard deviation",
  total_variation = "Total variation"
)

bind_violin <- function(emp, syn, family_label) {
  bind_rows(
    emp %>% filter(component == "series", metric %in% violin_metrics) %>% mutate(dataset = "OpenAI-large"),
    syn %>% filter(component == "series", metric %in% violin_metrics) %>% mutate(dataset = "DLO simulation")
  ) %>%
    mutate(
      family = family_label,
      metric_label = factor(metric_labels[metric], levels = metric_labels[violin_metrics]),
      dataset = factor(dataset, levels = c("DLO simulation", "OpenAI-large"))
    )
}

violin_df <- bind_rows(
  bind_violin(emp_raw, syn_raw, "Raw emotion scores"),
  bind_violin(emp_latent, syn_latent, "DynEGA score space")
)

oai_violin <- ggplot(violin_df, aes(x = dataset, y = value, fill = dataset)) +
  geom_violin(trim = FALSE, alpha = 0.72, color = NA, scale = "width") +
  geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.9, color = "grey20", linewidth = 0.25) +
  facet_wrap(~family + metric_label, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = c(`DLO simulation` = okabe[["vermillion"]], `OpenAI-large` = okabe[["blue"]]), name = NULL) +
  labs(
    title = "OpenAI-large versus heterogeneous DLO simulation",
    subtitle = "Violin plots summarize per-series metrics; boxes show median and interquartile range.",
    x = NULL,
    y = NULL
  ) +
  theme_report(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )

save_report_plot(oai_violin, "oai_large_metric_violins_v2", width = 10.5, height = 5.8)

table_metrics <- c("sample_entropy", "lag1_acf", "std_value", "total_variation", "zero_crossing_rate")
oai_table_df <- metric_compare %>%
  filter(family %in% c("raw", "latent"), metric %in% table_metrics) %>%
  mutate(
    Family = if_else(family == "raw", "Raw", "Latent"),
    Metric = metric_labels[metric],
    `OpenAI-large` = format_num(empirical_median, 3),
    `DLO simulation` = format_num(synthetic_median, 3),
    `Scaled median diff.` = format_num(median_diff_scaled, 2),
    `Scaled distance` = format_num(wasserstein_scaled, 2)
  ) %>%
  arrange(Family, factor(metric, levels = table_metrics)) %>%
  select(Family, Metric, `OpenAI-large`, `DLO simulation`, `Scaled median diff.`, `Scaled distance`)

oai_table <- c(
  "\\begin{tabular}{llrrrr}",
  "\\toprule",
  "Family & Metric & OpenAI-large & DLO sim. & Median diff. & Distance \\\\",
  "\\midrule",
  apply(oai_table_df, 1, function(row) {
    paste(escape_latex(unname(row)), collapse = " & ") |> paste0(" \\\\")
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(oai_table, file.path(out_dir, "oai_metric_table_v2.tex"))

# Benchmark scenario examples ------------------------------------------------

old_run_env <- Sys.getenv("RUN_METHOD_BENCHMARK", unset = NA_character_)
old_smoke_env <- Sys.getenv("RUN_METHOD_BENCHMARK_SMOKE", unset = NA_character_)
Sys.unsetenv("RUN_METHOD_BENCHMARK")
Sys.unsetenv("RUN_METHOD_BENCHMARK_SMOKE")
invisible(capture.output(source(file.path(repo_root, "run_method_benchmark.R"), local = FALSE)))
if (!is.na(old_run_env)) Sys.setenv(RUN_METHOD_BENCHMARK = old_run_env)
if (!is.na(old_smoke_env)) Sys.setenv(RUN_METHOD_BENCHMARK_SMOKE = old_smoke_env)

cfg <- build_benchmark_config(smoke = FALSE)
scenario_grid <- build_scenario_grid(cfg)
example_rows <- scenario_grid %>%
  filter(
    scenario_nfac == 2L,
    N == 50L,
    TimeP == 100L,
    NVAR == 3L,
    abs(Error - 0.01) < 1e-12,
    abs(Q - 0.75) < 1e-12,
    abs(Loadings - 0.6) < 1e-12,
    rep == 1L
  ) %>%
  arrange(match(scenario_mode, c("low_single_stable", "high_single_stable", "high_switch_stable", "high_switch_volatile")))

example_data <- lapply(seq_len(nrow(example_rows)), function(i) {
  scen <- example_rows[i, , drop = FALSE]
  sim_data <- simulate_benchmark_panel(scen, cfg, seed = 91000 + i)
  latent <- as.data.frame(sim_data$latents[[1]])
  names(latent) <- paste0("Dimension ", seq_len(ncol(latent)))
  latent$time <- seq_len(nrow(latent))
  latent$scenario_mode <- scen$scenario_mode[[1]]
  latent
}) %>%
  bind_rows() %>%
  pivot_longer(starts_with("Dimension"), names_to = "dimension", values_to = "value") %>%
  mutate(
    scenario_label = recode(
      scenario_mode,
      low_single_stable = "Low signal, stable",
      high_single_stable = "High signal, stable",
      high_switch_stable = "High signal, switching",
      high_switch_volatile = "High signal, volatile switching"
    ),
    scenario_label = factor(
      scenario_label,
      levels = c("Low signal, stable", "High signal, stable", "High signal, switching", "High signal, volatile switching")
    )
  )

scenario_plot <- ggplot(example_data, aes(x = time, y = value, color = dimension)) +
  geom_hline(yintercept = 0, color = "grey82", linewidth = 0.25) +
  geom_line(linewidth = 0.45, alpha = 0.95) +
  facet_wrap(~scenario_label, ncol = 2) +
  scale_color_manual(values = c(`Dimension 1` = okabe[["blue"]], `Dimension 2` = okabe[["vermillion"]]), name = NULL) +
  labs(
    title = "Example simulated latent time series from benchmark scenarios",
    subtitle = "Examples use 2D scenarios from the benchmark generator; each panel shows one simulated video.",
    x = "Frame",
    y = "Latent activation"
  ) +
  theme_report()

save_report_plot(scenario_plot, "benchmark_scenario_examples_v2", width = 8.6, height = 5.9)

readme <- c(
  "# Revision report v2 figures",
  "",
  "Generated by `Rscript plot_revision_report_v2_figures.R`.",
  "",
  "## Outputs",
  "",
  "- `benchmark_differences_v2.{pdf,png}`: compact benchmark-difference panel for DynEGA, GraphicalVAR, and bounded GIMME.",
  "- `benchmark_overview_table_v2.tex`: method-level benchmark table fragment.",
  "- `oai_large_metric_violins_v2.{pdf,png}`: OpenAI-large versus heterogeneous DLO metric distributions.",
  "- `oai_metric_table_v2.tex`: OpenAI-large versus DLO metric table fragment.",
  "- `benchmark_scenario_examples_v2.{pdf,png}`: simulated latent time-series examples for benchmark scenario modes."
)
writeLines(readme, file.path(out_dir, "README.md"))

cat("Saved revision report v2 assets to ", out_dir, "\n", sep = "")
