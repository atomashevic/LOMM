#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(readr)
  library(scales)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_revision_first_benchmark_figures.R", mustWork = TRUE)
}
root <- dirname(script_path)
out_dir <- Sys.getenv(
  "REVISION_FIGURE_OUT_DIR",
  file.path(root, "comparison_revision_first_benchmark")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

input_dirs <- strsplit(
  Sys.getenv(
    "REVISION_INPUT_DIRS",
    paste(
      file.path(root, "benchmark_outputs_revision_dynega"),
      file.path(root, "benchmark_outputs_revision_graphicalvar"),
      file.path(root, "benchmark_outputs_revision_gimme"),
      sep = ","
    )
  ),
  ",",
  fixed = TRUE
)[[1]]
input_dirs <- trimws(input_dirs)
input_dirs <- input_dirs[nzchar(input_dirs)]

read_results <- function(path) {
  files <- c(
    file.path(path, "benchmark_results.csv"),
    file.path(path, "checkpoint_results.csv")
  )
  existing <- files[file.exists(files)]
  if (length(existing) == 0L) {
    return(NULL)
  }
  file <- existing[[1]]
  readr::read_csv(file, show_col_types = FALSE)
}

results <- Filter(Negate(is.null), lapply(input_dirs, read_results))
if (length(results) == 0L) {
  stop("No benchmark result CSVs found in REVISION_INPUT_DIRS.", call. = FALSE)
}

df <- bind_rows(results)
if (!"comparison_scope" %in% names(df)) {
  df$comparison_scope <- if_else(df$method_family == "gimme", "gimme_bounded", "main_full_grid")
}
if (!"Error" %in% names(df)) df$Error <- df$base_err
if (!"N" %in% names(df)) df$N <- NA_integer_
if (!"NVAR" %in% names(df)) df$NVAR <- NA_integer_
if (!"Q" %in% names(df)) df$Q <- NA_real_
if (!"Loadings" %in% names(df)) df$Loadings <- NA_real_

scenario_levels <- c(
  "low_single_stable",
  "high_single_stable",
  "high_switch_stable",
  "high_switch_volatile"
)
scenario_labels <- c(
  "low_single_stable" = "Low signal\nsingle stable",
  "high_single_stable" = "High signal\nsingle stable",
  "high_switch_stable" = "High signal\nswitch stable",
  "high_switch_volatile" = "High signal\nswitch volatile"
)
method_cols <- c(
  "DynEGA" = "#0072B2",
  "GraphicalVAR" = "#D55E00",
  "GIMME" = "#009E73"
)

clean <- df %>%
  mutate(
    success = status == "ok",
    status_group = case_when(
      status == "ok" ~ "ok",
      status == "timeout" ~ "timeout",
      TRUE ~ "error"
    ),
    method_group = case_when(
      method_family == "dynEGA" ~ "DynEGA",
      method_family == "graphicalVAR" ~ "GraphicalVAR",
      method_family == "gimme" ~ "GIMME",
      TRUE ~ method_label
    ),
    scenario_mode_label = factor(
      scenario_mode,
      levels = scenario_levels,
      labels = unname(scenario_labels)
    ),
    true_ndim = factor(paste0(scenario_nfac, "D"), levels = paste0(c(2, 3, 4), "D")),
    N = factor(N),
    NVAR = factor(NVAR),
    Error = factor(Error),
    Q = factor(Q),
    Loadings = factor(Loadings),
    TimeP = as.integer(TimeP)
  )

main <- clean %>%
  filter(method_family %in% c("dynEGA", "graphicalVAR"), comparison_scope == "main_full_grid")
gimme <- clean %>%
  filter(method_family == "gimme", comparison_scope == "gimme_bounded")

ci_mean <- function(x) {
  n_ok <- sum(is.finite(x))
  if (n_ok <= 1L) return(NA_real_)
  1.96 * sd(x, na.rm = TRUE) / sqrt(n_ok)
}

save_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 300)
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height)
}

if (nrow(main) > 0L) {
  hit_timep <- main %>%
    group_by(method_group, TimeP, scenario_mode_label, true_ndim) %>%
    summarise(
      exact = mean(n_dim_exact, na.rm = TRUE),
      exact_ci = ci_mean(n_dim_exact),
      runs = n(),
      .groups = "drop"
    )

  p1 <- ggplot(hit_timep, aes(x = TimeP, y = exact, color = method_group, group = method_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.9) +
    geom_errorbar(aes(ymin = pmax(0, exact - exact_ci), ymax = pmin(1, exact + exact_ci)), width = 10, alpha = 0.55) +
    facet_grid(true_ndim ~ scenario_mode_label) +
    scale_color_manual(values = method_cols, name = NULL) +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    scale_x_continuous(breaks = sort(unique(hit_timep$TimeP))) +
    labs(
      title = "Exact n.dim recovery across the revised first-simulation benchmark",
      subtitle = "Main full grid: DynEGA and GraphicalVAR only. GIMME is reported separately as bounded feasibility.",
      x = "Video length / time points",
      y = "Exact recovery rate"
    ) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot")
  save_plot(p1, "01_hit_rate_by_timep", 12.5, 8)

  factor_sensitivity <- bind_rows(
    main %>% transmute(method_group, factor = "N", level = as.character(N), n_dim_exact),
    main %>% transmute(method_group, factor = "NVAR", level = as.character(NVAR), n_dim_exact),
    main %>% transmute(method_group, factor = "Error", level = as.character(Error), n_dim_exact),
    main %>% transmute(method_group, factor = "Q", level = as.character(Q), n_dim_exact),
    main %>% transmute(method_group, factor = "Loadings", level = as.character(Loadings), n_dim_exact)
  ) %>%
    group_by(method_group, factor, level) %>%
    summarise(exact = mean(n_dim_exact, na.rm = TRUE), exact_ci = ci_mean(n_dim_exact), .groups = "drop") %>%
    mutate(factor = factor(factor, levels = c("N", "NVAR", "Error", "Q", "Loadings")))

  p2 <- ggplot(factor_sensitivity, aes(x = level, y = exact, color = method_group, group = method_group)) +
    geom_line(linewidth = 0.55) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = pmax(0, exact - exact_ci), ymax = pmin(1, exact + exact_ci)), width = 0.12, alpha = 0.55) +
    facet_wrap(~ factor, scales = "free_x", nrow = 1) +
    scale_color_manual(values = method_cols, name = NULL) +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    labs(
      title = "Sensitivity to the original 2023 simulation factors",
      x = "Factor level",
      y = "Exact n.dim recovery rate"
    ) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot")
  save_plot(p2, "02_old_factor_sensitivity", 12, 4.8)

  score_recovery <- main %>%
    group_by(method_group, TimeP, scenario_mode_label) %>%
    summarise(
      score_cor = mean(score_cor_common, na.rm = TRUE),
      nrmse = mean(score_nrmse_common, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(score_cor, nrmse), names_to = "metric", values_to = "value") %>%
    mutate(
      metric = factor(
        metric,
        levels = c("score_cor", "nrmse"),
        labels = c("Latent score correlation", "Latent score NRMSE")
      )
    )

  p3 <- ggplot(score_recovery, aes(x = TimeP, y = value, color = method_group, group = method_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.9) +
    facet_grid(metric ~ scenario_mode_label, scales = "free_y") +
    scale_color_manual(values = method_cols, name = NULL) +
    scale_x_continuous(breaks = sort(unique(score_recovery$TimeP))) +
    labs(
      title = "Latent score recovery in the revised benchmark",
      x = "Video length / time points",
      y = NULL
    ) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot")
  save_plot(p3, "03_score_recovery", 12.5, 6)

  runtime_status <- main %>%
    group_by(method_group) %>%
    summarise(
      `Median runtime, sec` = median(runtime_sec, na.rm = TRUE),
      `Failure or timeout rate` = mean(status_group != "ok", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-method_group, names_to = "metric", values_to = "value")

  p4 <- ggplot(runtime_status, aes(x = method_group, y = value, fill = method_group)) +
    geom_col(width = 0.58, show.legend = FALSE) +
    facet_wrap(~ metric, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = method_cols) +
    labs(
      title = "Runtime and failure profile for the main full-grid methods",
      x = NULL,
      y = NULL
    ) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
  save_plot(p4, "04_runtime_and_failures", 8.5, 4.5)
}

if (nrow(gimme) > 0L) {
  gimme_summary <- gimme %>%
    group_by(scenario_mode_label, true_ndim) %>%
    summarise(
      success_rate = mean(status_group == "ok", na.rm = TRUE),
      timeout_rate = mean(status_group == "timeout", na.rm = TRUE),
      exact_if_ok = mean(n_dim_exact[status_group == "ok"], na.rm = TRUE),
      runs = n(),
      .groups = "drop"
    ) %>%
    pivot_longer(c(success_rate, timeout_rate, exact_if_ok), names_to = "metric", values_to = "value") %>%
    mutate(
      metric = factor(
        metric,
        levels = c("success_rate", "timeout_rate", "exact_if_ok"),
        labels = c("Success rate", "Timeout rate", "Exact n.dim if ok")
      )
    )

  p5 <- ggplot(gimme_summary, aes(x = true_ndim, y = value, fill = metric)) +
    geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
    facet_wrap(~ scenario_mode_label, nrow = 1) +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    scale_fill_brewer(palette = "Dark2", name = NULL) +
    labs(
      title = "GIMME bounded feasibility subset",
      subtitle = "Subset: N=50, TimeP=100, NVAR=3, Error=0.025, Q=0.75, Loadings=0.6.",
      x = "True number of simulated dimensions",
      y = NULL
    ) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot")
  save_plot(p5, "05_gimme_bounded_feasibility", 12, 4.8)
}

note_lines <- c(
  "# Revised first-simulation benchmark figures",
  "",
  "## Sources",
  "",
  paste0("- `", input_dirs, "`"),
  "",
  "## Design",
  "",
  "This bundle represents the revision of the 2023 emotion-dynamics simulation benchmark.",
  "It preserves the original factor families (`N`, `TimeP`, `NVAR`, `Error`, `Q`, `Loadings`) and crosses them with the current 12 revised scenarios.",
  "",
  "DynEGA and GraphicalVAR are the main full-grid comparison.",
  "GIMME is reported as a bounded feasibility comparator: `N=50`, `TimeP=100`, `NVAR=3`, `Error=0.025`, `Q=0.75`, `Loadings=0.6`.",
  "",
  "## Outputs",
  "",
  "- `01_hit_rate_by_timep.{png,pdf}`",
  "- `02_old_factor_sensitivity.{png,pdf}`",
  "- `03_score_recovery.{png,pdf}`",
  "- `04_runtime_and_failures.{png,pdf}`",
  "- `05_gimme_bounded_feasibility.{png,pdf}`"
)
writeLines(note_lines, file.path(out_dir, "README.md"))

cat("Saved revised first-benchmark figures to ", out_dir, "\n", sep = "")
