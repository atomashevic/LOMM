#!/usr/bin/env Rscript

## Main Figure 4 candidates and method-comparison suite.
## The old Scenario A is treated as a supplementary hard edge case; the main
## text scenarios are renumbered from the old B-D sequence to A-C.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(cowplot)
})

repo_root <- normalizePath(".", mustWork = TRUE)
out_dir <- file.path(repo_root, "simulation_outputs", "method_comparison")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dynega_csv <- file.path(repo_root, "simulation_outputs", "emotion_dynega", "benchmark_results.csv")
graphicalvar_csv <- file.path(repo_root, "simulation_outputs", "emotion_graphicalvar", "benchmark_results_partial.csv")
gimme_csv <- file.path(repo_root, "simulation_outputs", "emotion_gimme", "benchmark_results.csv")

stopifnot(file.exists(dynega_csv), file.exists(graphicalvar_csv), file.exists(gimme_csv))

okabe <- c(
  blue = "#0072B2",
  orange = "#E69F00",
  vermillion = "#D55E00",
  green = "#009E73",
  purple = "#CC79A7",
  black = "#000000"
)

scenario_labels <- c(
  old_low_single_stable = "Supplementary hard scenario",
  old_high_single_stable = "Scenario A",
  old_high_switch_stable = "Scenario B",
  old_high_switch_volatile = "Scenario C",
  oai_ranked_persistence = "Scenario D",
  oai_broad_geometry = "Scenario E"
)

fmt <- function(x, digits = 3) {
  formatC(x, format = "f", digits = digits)
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("_", "\\\\_", x)
  x
}

partial_eta2 <- function(data, response, predictors) {
  keep <- data %>%
    select(all_of(c(response, predictors))) %>%
    filter(if_all(everything(), ~ !is.na(.x)))
  usable <- predictors[vapply(keep[predictors], function(x) length(unique(x)) > 1L, logical(1))]
  if (nrow(keep) < 10L || length(usable) == 0L) return(data.frame())
  if (stats::sd(keep[[response]], na.rm = TRUE) < .Machine$double.eps) {
    return(data.frame(response = response, term = usable, df = NA_real_, ss = 0, partial_eta2 = 0))
  }
  fit <- stats::lm(stats::as.formula(paste(response, "~", paste(usable, collapse = " + "))), data = keep)
  tab <- stats::anova(fit)
  residual_ss <- tab["Residuals", "Sum Sq"]
  data.frame(term = rownames(tab), df = tab[, "Df"], ss = tab[, "Sum Sq"], row.names = NULL) %>%
    filter(term != "Residuals") %>%
    mutate(response = response, partial_eta2 = ss / (ss + residual_ss)) %>%
    select(response, term, df, ss, partial_eta2) %>%
    arrange(response, desc(partial_eta2))
}

theme_rr <- function(base_size = 8.5) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "grey88", linewidth = 0.22),
      panel.grid.minor = element_line(color = "grey94", linewidth = 0.16),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.82), margin = margin(b = 2)),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      plot.title.position = "plot",
      panel.spacing.x = grid::unit(0.55, "lines"),
      panel.spacing.y = grid::unit(0.45, "lines"),
      plot.margin = margin(7, 8, 6, 7)
    )
}

save_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 320, bg = "white")
}

prepare_common <- function(df) {
  if (!"Error_sd" %in% names(df)) df$Error_sd <- df$Error
  df %>%
    mutate(
      Error_sd = factor(fmt(as.numeric(Error_sd), 3), levels = fmt(c(0.125, 0.250, 0.500), 3)),
      Loadings = factor(fmt(as.numeric(Loadings), 1), levels = fmt(c(0.4, 0.6, 0.8), 1)),
      scenario_family = factor(as.character(scenario_family), levels = names(scenario_labels)),
      scenario_label = factor(scenario_labels[as.character(scenario_family)], levels = unname(scenario_labels)),
      scenario_nfac = factor(as.integer(scenario_nfac), levels = c(2, 3, 4)),
      true_dim_label = factor(paste0(scenario_nfac, "D"), levels = c("2D", "3D", "4D")),
      community_algorithm = factor(community_algorithm, levels = c("louvain", "leiden", "walktrap")),
      N = factor(N),
      TimeP = factor(TimeP),
      NVAR = factor(NVAR),
      exact_recovery = as.numeric(n_dim_exact),
      factor_score_correlation = as.numeric(score_cor_common),
      factor_score_nrmse = as.numeric(score_nrmse_common),
      assignment_accuracy = as.numeric(assignment_accuracy),
      runtime_sec = as.numeric(runtime_sec)
    )
}

dynega <- readr::read_csv(dynega_csv, show_col_types = FALSE) %>%
  filter(method_family == "dynEGA", status == "ok") %>%
  prepare_common()
graphicalvar <- readr::read_csv(graphicalvar_csv, show_col_types = FALSE) %>%
  filter(method_family == "graphicalVAR", status == "ok") %>%
  prepare_common()
gimme <- readr::read_csv(gimme_csv, show_col_types = FALSE) %>%
  filter(method_family == "gimme", status == "ok") %>%
  prepare_common()

main_scenarios <- c("Scenario A", "Scenario B", "Scenario C")
loading_cols <- c("0.4" = okabe[["blue"]], "0.6" = okabe[["orange"]], "0.8" = okabe[["vermillion"]])
method_cols <- c("DynEGA" = okabe[["blue"]], "GIMME" = okabe[["purple"]], "GraphicalVAR" = okabe[["vermillion"]])

non_a <- dynega %>% filter(scenario_family != "old_low_single_stable")
predictors <- c(
  "scenario_family", "scenario_nfac", "community_algorithm",
  "N", "TimeP", "NVAR", "Error_sd", "Loadings", "benchmark_grid"
)
effect_screen <- bind_rows(
  partial_eta2(non_a, "exact_recovery", predictors),
  partial_eta2(non_a, "factor_score_correlation", predictors)
)
readr::write_csv(effect_screen, file.path(out_dir, "figure4_non_a_effect_screen.csv"))

scenario_spread <- non_a %>%
  group_by(scenario_label, true_dim_label, Error_sd, Loadings) %>%
  summarise(
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    factor_score_correlation = mean(factor_score_correlation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(scenario_label) %>%
  summarise(
    exact_range = diff(range(exact_recovery, na.rm = TRUE)),
    exact_min = min(exact_recovery, na.rm = TRUE),
    exact_max = max(exact_recovery, na.rm = TRUE),
    correlation_range = diff(range(factor_score_correlation, na.rm = TRUE)),
    correlation_min = min(factor_score_correlation, na.rm = TRUE),
    correlation_max = max(factor_score_correlation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(exact_range), desc(correlation_range))
readr::write_csv(scenario_spread, file.path(out_dir, "figure4_non_a_scenario_spread.csv"))

main_dynega_summary <- dynega %>%
  filter(scenario_label %in% main_scenarios) %>%
  group_by(scenario_label, true_dim_label, Error_sd, Loadings) %>%
  summarise(
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    factor_score_correlation = mean(factor_score_correlation, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  )
readr::write_csv(main_dynega_summary, file.path(out_dir, "figure4_main_non_a_dynega_summary.csv"))

main_hit_plot <- ggplot(
  main_dynega_summary,
  aes(x = as.numeric(as.character(Error_sd)), y = exact_recovery, color = Loadings, group = Loadings)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.9) +
  facet_grid(scenario_label ~ true_dim_label) +
  scale_color_manual(values = loading_cols, name = "Loading") +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Measurement error SD", y = "Exact n.dim hit rate") +
  theme_rr(8.5) +
  theme(axis.text.x = element_text(angle = 0), strip.text.y = element_text(angle = 0))
save_plot(main_hit_plot, "figure4_main_non_a_dynega_hit_rate", 8.4, 5.2)

main_cor_plot <- ggplot(
  main_dynega_summary,
  aes(x = as.numeric(as.character(Error_sd)), y = factor_score_correlation, color = Loadings, group = Loadings)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.9) +
  facet_grid(scenario_label ~ true_dim_label) +
  scale_color_manual(values = loading_cols, name = "Loading") +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  scale_y_continuous(labels = label_number(accuracy = 0.01), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Measurement error SD", y = "Factor-score correlation") +
  theme_rr(8.5) +
  theme(axis.text.x = element_text(angle = 0), strip.text.y = element_text(angle = 0))
save_plot(main_cor_plot, "figure4_main_non_a_dynega_factor_cor", 8.4, 5.2)

method_key_cols <- c("scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "rep", "community_algorithm")
gimme_keys <- gimme %>%
  filter(scenario_label %in% main_scenarios) %>%
  distinct(across(all_of(method_key_cols)))

method_overlap <- bind_rows(
  dynega %>%
    filter(scenario_label %in% main_scenarios) %>%
    semi_join(gimme_keys, by = method_key_cols) %>%
    mutate(method_group = "DynEGA"),
  gimme %>%
    filter(scenario_label %in% main_scenarios) %>%
    semi_join(gimme_keys, by = method_key_cols) %>%
    mutate(method_group = "GIMME"),
  graphicalvar %>%
    filter(scenario_label %in% main_scenarios) %>%
    semi_join(gimme_keys, by = method_key_cols) %>%
    mutate(method_group = "GraphicalVAR")
) %>%
  mutate(
    method_group = factor(method_group, levels = c("DynEGA", "GIMME", "GraphicalVAR")),
    scenario_dim = factor(
      paste0(scenario_label, " / ", true_dim_label),
      levels = as.vector(outer(main_scenarios, c("2D", "3D"), paste, sep = " / "))
    )
  ) %>%
  filter(true_dim_label %in% c("2D", "3D"))

method_summary <- method_overlap %>%
  group_by(method_group, scenario_label, true_dim_label, scenario_dim, Error_sd, Loadings) %>%
  summarise(
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    factor_score_correlation = mean(factor_score_correlation, na.rm = TRUE),
    assignment_accuracy = mean(assignment_accuracy, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  )
readr::write_csv(method_summary, file.path(out_dir, "figure4_method_bcd_overlap_summary.csv"))

method_hit_plot <- ggplot(
  method_summary,
  aes(x = as.numeric(as.character(Error_sd)), y = exact_recovery, color = method_group, group = method_group)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_grid(scenario_dim ~ Loadings) +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Measurement error SD", y = "Exact n.dim hit rate") +
  theme_rr(8.0) +
  theme(
    axis.text.x = element_text(angle = 0),
    strip.text.y = element_text(angle = 0, size = rel(0.78))
  )
save_plot(method_hit_plot, "figure4_method_bcd_overlap_hit_rate", 8.4, 8.6)

method_cor_plot <- ggplot(
  method_summary,
  aes(x = as.numeric(as.character(Error_sd)), y = factor_score_correlation, color = method_group, group = method_group)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_grid(scenario_dim ~ Loadings) +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  scale_y_continuous(labels = label_number(accuracy = 0.01), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Measurement error SD", y = "Factor-score correlation") +
  theme_rr(8.0) +
  theme(
    axis.text.x = element_text(angle = 0),
    strip.text.y = element_text(angle = 0, size = rel(0.78))
  )
save_plot(method_cor_plot, "figure4_method_bcd_overlap_factor_cor", 8.4, 8.6)

structure_key_cols <- setdiff(method_key_cols, "community_algorithm")
method_overlap$structure_cell_id <- apply(method_overlap[, structure_key_cols, drop = FALSE], 1, paste, collapse = "|")

assessment_table <- method_overlap %>%
  group_by(method_group) %>%
  summarise(
    scope = "Scenarios A-C: bounded GIMME overlap",
    dimensions = paste(sort(unique(as.character(true_dim_label))), collapse = ", "),
    scenarios = paste(sort(unique(as.character(scenario_label))), collapse = ", "),
    structure_cells = n_distinct(structure_cell_id),
    method_rows = n(),
    exact_hit_rate = mean(exact_recovery, na.rm = TRUE),
    assignment_accuracy = mean(assignment_accuracy, na.rm = TRUE),
    factor_score_correlation = mean(factor_score_correlation, na.rm = TRUE),
    factor_score_nrmse = mean(factor_score_nrmse, na.rm = TRUE),
    median_runtime_sec = median(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    assessment = case_when(
      method_group == "DynEGA" ~ "Best structural recovery and fastest runtime; correlations comparable rather than uniquely higher.",
      method_group == "GraphicalVAR" ~ "Moderate structural recovery and similar correlations, but much slower than DynEGA.",
      method_group == "GIMME" ~ "Weak structural recovery and slowest runtime in the bounded feasible subset.",
      TRUE ~ ""
    )
  ) %>%
  arrange(method_group)
readr::write_csv(assessment_table, file.path(out_dir, "figure4_method_performance_assessment.csv"))

assessment_tex <- assessment_table %>%
  mutate(
    exact_hit_rate = fmt(exact_hit_rate, 3),
    assignment_accuracy = fmt(assignment_accuracy, 3),
    factor_score_correlation = fmt(factor_score_correlation, 3),
    factor_score_nrmse = fmt(factor_score_nrmse, 3),
    median_runtime_sec = fmt(median_runtime_sec, 1)
  )
assessment_lines <- c(
  "\\begin{tabular}{p{0.14\\linewidth}rrrrrp{0.33\\linewidth}}",
  "\\toprule",
  "Method & Exact & Assign. & Corr. & NRMSE & Median s & Assessment \\\\",
  "\\midrule",
  apply(
    assessment_tex[
      ,
      c(
        "method_group", "exact_hit_rate", "assignment_accuracy",
        "factor_score_correlation", "factor_score_nrmse", "median_runtime_sec", "assessment"
      )
    ],
    1,
    function(row) paste(latex_escape(as.character(unname(row))), collapse = " & ") |> paste0(" \\\\")
  ),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(assessment_lines, file.path(out_dir, "figure4_method_performance_assessment.tex"))

method_gap_table <- method_summary %>%
  select(scenario_label, true_dim_label, Error_sd, Loadings, method_group, exact_recovery, assignment_accuracy, factor_score_correlation) %>%
  pivot_wider(names_from = method_group, values_from = c(exact_recovery, assignment_accuracy, factor_score_correlation)) %>%
  mutate(
    dyn_minus_graphicalvar_exact = exact_recovery_DynEGA - exact_recovery_GraphicalVAR,
    dyn_minus_gimme_exact = exact_recovery_DynEGA - exact_recovery_GIMME,
    dyn_minus_graphicalvar_assignment = assignment_accuracy_DynEGA - assignment_accuracy_GraphicalVAR,
    dyn_minus_gimme_assignment = assignment_accuracy_DynEGA - assignment_accuracy_GIMME
  ) %>%
  arrange(desc(pmin(dyn_minus_graphicalvar_exact, dyn_minus_gimme_exact)), desc(dyn_minus_graphicalvar_exact))
readr::write_csv(method_gap_table, file.path(out_dir, "figure4_method_bcd_overlap_condition_gaps.csv"))

cluster_boot_mean <- function(data, value_col, cluster_col, reps = 2000L) {
  value <- data[[value_col]]
  estimate <- mean(value, na.rm = TRUE)
  clusters <- unique(data[[cluster_col]])
  if (length(clusters) < 2L) {
    return(tibble(estimate = estimate, conf.low = NA_real_, conf.high = NA_real_))
  }
  split_indices <- split(seq_len(nrow(data)), data[[cluster_col]])
  boot_values <- replicate(reps, {
    sampled_clusters <- sample(clusters, length(clusters), replace = TRUE)
    sampled_indices <- unlist(split_indices[sampled_clusters], use.names = FALSE)
    mean(value[sampled_indices], na.rm = TRUE)
  })
  tibble(
    estimate = estimate,
    conf.low = unname(stats::quantile(boot_values, 0.025, na.rm = TRUE)),
    conf.high = unname(stats::quantile(boot_values, 0.975, na.rm = TRUE))
  )
}

summarise_cluster_ci <- function(data, group_cols, value_col, cluster_col) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(~ {
      cluster_boot_mean(.x, value_col = value_col, cluster_col = cluster_col) %>%
        mutate(n_rows = nrow(.x), n_clusters = n_distinct(.x[[cluster_col]]))
    }) %>%
    ungroup()
}

set.seed(20260518)
method_accuracy_units <- bind_rows(
  method_overlap %>% mutate(stratum = "Overall"),
  method_overlap %>% mutate(stratum = as.character(scenario_label))
) %>%
  mutate(
    stratum = factor(stratum, levels = c("Overall", main_scenarios)),
    method_group = factor(method_group, levels = c("DynEGA", "GraphicalVAR", "GIMME"))
  )

accuracy_level_summary <- summarise_cluster_ci(
  method_accuracy_units,
  group_cols = c("stratum", "method_group"),
  value_col = "exact_recovery",
  cluster_col = "structure_cell_id"
) %>%
  mutate(
    outcome = factor("Hit rate", levels = "Hit rate"),
    stratum = factor(stratum, levels = rev(c("Overall", main_scenarios)))
  )
readr::write_csv(accuracy_level_summary, file.path(out_dir, "figure4_method_accuracy_reliable_summary.csv"))

paired_key_cols <- method_key_cols
paired_accuracy <- method_overlap %>%
  mutate(pair_id = apply(select(., all_of(paired_key_cols)), 1, paste, collapse = "|")) %>%
  select(
    scenario_label, true_dim_label, structure_cell_id, pair_id,
    method_group, exact_recovery
  ) %>%
  pivot_wider(
    names_from = method_group,
    values_from = exact_recovery
  ) %>%
  filter(
    !is.na(DynEGA),
    !is.na(GIMME),
    !is.na(GraphicalVAR)
  )

paired_difference_units <- bind_rows(
  paired_accuracy %>% mutate(stratum = "Overall"),
  paired_accuracy %>% mutate(stratum = as.character(scenario_label))
) %>%
  transmute(
    stratum = factor(stratum, levels = c("Overall", main_scenarios)),
    structure_cell_id,
    exact_vs_graphicalvar = DynEGA - GraphicalVAR,
    exact_vs_gimme = DynEGA - GIMME
  ) %>%
  pivot_longer(
    cols = c(exact_vs_graphicalvar, exact_vs_gimme),
    names_to = "contrast",
    values_to = "difference"
  ) %>%
  mutate(
    outcome = "Hit rate",
    comparison = if_else(grepl("graphicalvar$", contrast), "DynEGA - GraphicalVAR", "DynEGA - GIMME")
  )

accuracy_difference_summary <- summarise_cluster_ci(
  paired_difference_units,
  group_cols = c("stratum", "outcome", "comparison"),
  value_col = "difference",
  cluster_col = "structure_cell_id"
) %>%
  mutate(
    outcome = factor(outcome, levels = "Hit rate"),
    stratum = factor(stratum, levels = rev(c("Overall", main_scenarios))),
    comparison = factor(comparison, levels = c("DynEGA - GraphicalVAR", "DynEGA - GIMME"))
  )
readr::write_csv(accuracy_difference_summary, file.path(out_dir, "figure4_method_accuracy_reliable_differences.csv"))

accuracy_series_cols <- c(
  "DynEGA" = okabe[["blue"]],
  "GraphicalVAR" = okabe[["vermillion"]],
  "GIMME" = okabe[["purple"]],
  "DynEGA - GraphicalVAR" = okabe[["vermillion"]],
  "DynEGA - GIMME" = okabe[["purple"]]
)
accuracy_series_shapes <- c(
  "DynEGA" = 16,
  "GraphicalVAR" = 17,
  "GIMME" = 15,
  "DynEGA - GraphicalVAR" = 17,
  "DynEGA - GIMME" = 15
)

accuracy_plot_df <- bind_rows(
  accuracy_level_summary %>%
    transmute(
      panel = "Method accuracy",
      outcome, stratum,
      series = as.character(method_group),
      estimate, conf.low, conf.high
    ),
  accuracy_difference_summary %>%
    transmute(
      panel = "DynEGA advantage",
      outcome, stratum,
      series = as.character(comparison),
      estimate, conf.low, conf.high
    )
) %>%
  mutate(
    panel = factor(panel, levels = c("Method accuracy", "DynEGA advantage")),
    series = factor(series, levels = names(accuracy_series_cols))
  )

accuracy_reliable_plot <- ggplot(
  accuracy_plot_df,
  aes(x = estimate, y = stratum, color = series, shape = series)
) +
  geom_vline(
    data = data.frame(
      panel = factor("DynEGA advantage", levels = levels(accuracy_plot_df$panel))
    ),
    aes(xintercept = 0),
    inherit.aes = FALSE,
    color = "grey55",
    linewidth = 0.28
  ) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.62),
    linewidth = 0.38
  ) +
  facet_grid(. ~ panel, scales = "free_x", space = "free_x") +
  scale_color_manual(values = accuracy_series_cols, name = NULL, drop = TRUE) +
  scale_shape_manual(values = accuracy_series_shapes, name = NULL, drop = TRUE) +
  scale_x_continuous(labels = label_percent(accuracy = 1), breaks = seq(-0.25, 1, by = 0.25)) +
  labs(x = "Hit rate / paired hit-rate difference", y = NULL) +
  theme_rr(8.3) +
  theme(
    panel.grid.minor = element_blank(),
    legend.key.width = grid::unit(0.65, "lines"),
    legend.position = "bottom"
  )
save_plot(accuracy_reliable_plot, "figure4_method_accuracy_reliable", 7.2, 3.0)

accuracy_levels_plot <- ggplot(
  accuracy_level_summary,
  aes(x = estimate, y = stratum, color = method_group, shape = method_group)
) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.55),
    linewidth = 0.38
  ) +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_shape_manual(values = c("DynEGA" = 16, "GraphicalVAR" = 17, "GIMME" = 15), name = NULL) +
  scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Hit rate", y = NULL) +
  theme_rr(8.3) +
  theme(panel.grid.minor = element_blank())
save_plot(accuracy_levels_plot, "figure4_method_accuracy_levels", 5.8, 2.8)

accuracy_difference_plot <- ggplot(
  accuracy_difference_summary,
  aes(x = estimate, y = stratum, color = comparison, shape = comparison)
) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.28) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.55),
    linewidth = 0.38
  ) +
  scale_color_manual(
    values = c("DynEGA - GraphicalVAR" = okabe[["vermillion"]], "DynEGA - GIMME" = okabe[["purple"]]),
    name = NULL
  ) +
  scale_shape_manual(values = c("DynEGA - GraphicalVAR" = 17, "DynEGA - GIMME" = 15), name = NULL) +
  scale_x_continuous(labels = label_percent(accuracy = 1), breaks = seq(-0.1, 0.8, by = 0.2)) +
  labs(x = "Paired hit-rate difference", y = NULL) +
  theme_rr(8.3) +
  theme(panel.grid.minor = element_blank())
save_plot(accuracy_difference_plot, "figure4_method_accuracy_differences", 5.8, 2.8)

conditional_correlation_units <- method_accuracy_units %>%
  filter(exact_recovery == 1, !is.na(factor_score_correlation))

conditional_correlation_summary <- summarise_cluster_ci(
  conditional_correlation_units,
  group_cols = c("stratum", "method_group"),
  value_col = "factor_score_correlation",
  cluster_col = "structure_cell_id"
) %>%
  mutate(
    outcome = factor("Latent correlation", levels = "Latent correlation"),
    stratum = factor(stratum, levels = rev(c("Overall", main_scenarios)))
  )

accuracy_conditional_correlation_summary <- bind_rows(
  accuracy_level_summary %>%
    mutate(
      panel = "Model accuracy",
      statistic = "Hit rate",
      conditional_on_correct_ndim = FALSE
    ),
  conditional_correlation_summary %>%
    mutate(
      panel = "Latent correlation",
      statistic = "Latent correlation",
      conditional_on_correct_ndim = TRUE
    )
) %>%
  select(
    panel, statistic, stratum, method_group,
    estimate, conf.low, conf.high, n_rows, n_clusters,
    conditional_on_correct_ndim
  )
readr::write_csv(
  accuracy_conditional_correlation_summary,
  file.path(out_dir, "figure4_method_accuracy_conditional_correlation_summary.csv")
)

method_shape_values <- c("DynEGA" = 16, "GraphicalVAR" = 17, "GIMME" = 15)
method_breaks <- c("DynEGA", "GraphicalVAR", "GIMME")

model_accuracy_panel <- ggplot(
  accuracy_level_summary,
  aes(x = estimate, y = stratum, color = method_group, shape = method_group)
) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.55),
    linewidth = 0.38
  ) +
  scale_color_manual(values = method_cols, breaks = method_breaks, name = NULL) +
  scale_shape_manual(values = method_shape_values, breaks = method_breaks, name = NULL) +
  scale_x_continuous(labels = label_percent(accuracy = 1), breaks = seq(0, 1, by = 0.25)) +
  coord_cartesian(xlim = c(0, 1.02)) +
  labs(title = "Model accuracy", x = "Hit rate", y = NULL) +
  theme_rr(8.3) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = rel(1.05)),
    legend.key.width = grid::unit(0.65, "lines")
  )

conditional_correlation_panel <- ggplot(
  conditional_correlation_summary,
  aes(x = estimate, y = stratum, color = method_group, shape = method_group)
) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.55),
    linewidth = 0.38
  ) +
  scale_color_manual(values = method_cols, breaks = method_breaks, name = NULL) +
  scale_shape_manual(values = method_shape_values, breaks = method_breaks, name = NULL) +
  scale_x_continuous(labels = label_number(accuracy = 0.01), breaks = seq(0, 1, by = 0.25)) +
  coord_cartesian(xlim = c(0, 1.02)) +
  labs(title = "Latent correlation", x = "Latent correlation (correct n.dim only)", y = NULL) +
  theme_rr(8.3) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = rel(1.05)),
    legend.key.width = grid::unit(0.65, "lines")
  )

accuracy_conditional_legend <- cowplot::get_legend(
  model_accuracy_panel + theme(legend.position = "bottom")
)

accuracy_conditional_correlation_plot <- cowplot::plot_grid(
  cowplot::plot_grid(
    model_accuracy_panel + theme(legend.position = "none"),
    conditional_correlation_panel + theme(legend.position = "none"),
    ncol = 2,
    align = "hv",
    axis = "tblr",
    rel_widths = c(1, 1)
  ),
  accuracy_conditional_legend,
  ncol = 1,
  rel_heights = c(1, 0.16)
)
save_plot(
  accuracy_conditional_correlation_plot,
  "figure4_method_accuracy_conditional_correlation",
  7.2,
  3.2
)

accuracy_difference_tex <- accuracy_difference_summary %>%
  filter(stratum == "Overall") %>%
  mutate(
    estimate_ci = paste0(fmt(estimate, 3), " [", fmt(conf.low, 3), ", ", fmt(conf.high, 3), "]")
  ) %>%
  select(outcome, comparison, estimate_ci, n_clusters)
accuracy_difference_lines <- c(
  "\\begin{tabular}{llrl}",
  "\\toprule",
  "Metric & Contrast & Difference [95\\% CI] & Clusters \\\\",
  "\\midrule",
  apply(
    accuracy_difference_tex,
    1,
    function(row) paste(latex_escape(as.character(unname(row))), collapse = " & ") |> paste0(" \\\\")
  ),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(accuracy_difference_lines, file.path(out_dir, "figure4_method_accuracy_reliable_differences.tex"))

design_block_levels <- c(
  "Base\nN50 T100 p4",
  "Larger N\nN100 T100 p4",
  "Longer T\nN50 T300 p4",
  "More items\nT100 p6"
)

classify_design_block <- function(N, TimeP, NVAR) {
  case_when(
    as.character(N) == "50" & as.character(TimeP) == "100" & as.character(NVAR) == "4" ~ design_block_levels[1],
    as.character(N) == "100" & as.character(TimeP) == "100" & as.character(NVAR) == "4" ~ design_block_levels[2],
    as.character(N) == "50" & as.character(TimeP) == "300" & as.character(NVAR) == "4" ~ design_block_levels[3],
    as.character(TimeP) == "100" & as.character(NVAR) == "6" ~ design_block_levels[4],
    TRUE ~ "Other"
  )
}

runtime_main <- bind_rows(
  dynega %>% filter(scenario_label %in% main_scenarios) %>% mutate(method_group = "DynEGA"),
  graphicalvar %>% filter(scenario_label %in% main_scenarios) %>% mutate(method_group = "GraphicalVAR"),
  gimme %>% filter(scenario_label %in% main_scenarios) %>% mutate(method_group = "GIMME")
) %>%
  mutate(
    method_group = factor(method_group, levels = c("DynEGA", "GraphicalVAR", "GIMME")),
    true_dim_label = factor(true_dim_label, levels = c("2D", "3D", "4D")),
    design_block = factor(classify_design_block(N, TimeP, NVAR), levels = design_block_levels)
  )

runtime_summary <- runtime_main %>%
  group_by(method_group, true_dim_label, design_block) %>%
  summarise(
    n_runs = n(),
    median_sec = median(runtime_sec, na.rm = TRUE),
    q25_sec = unname(stats::quantile(runtime_sec, 0.25, na.rm = TRUE)),
    q75_sec = unname(stats::quantile(runtime_sec, 0.75, na.rm = TRUE)),
    p95_sec = unname(stats::quantile(runtime_sec, 0.95, na.rm = TRUE)),
    max_sec = max(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(runtime_summary, file.path(out_dir, "figure4_method_runtime_summary.csv"))

runtime_common_summary <- runtime_summary %>%
  filter(design_block == design_block_levels[1])
readr::write_csv(runtime_common_summary, file.path(out_dir, "figure4_method_runtime_common_base_summary.csv"))

full_design <- dynega %>%
  filter(scenario_label %in% main_scenarios) %>%
  distinct(
    scenario, scenario_label, true_dim_label, N, TimeP, NVAR,
    Error_sd, Loadings, rep, community_algorithm
  ) %>%
  mutate(design_block = factor(classify_design_block(N, TimeP, NVAR), levels = design_block_levels)) %>%
  filter(!is.na(design_block))

full_counts <- full_design %>%
  count(true_dim_label, design_block, name = "full_cells")

coverage_template <- expand_grid(
  method_group = factor(c("DynEGA", "GraphicalVAR", "GIMME"), levels = c("DynEGA", "GraphicalVAR", "GIMME")),
  true_dim_label = factor(c("2D", "3D", "4D"), levels = c("2D", "3D", "4D")),
  design_block = factor(design_block_levels, levels = design_block_levels)
)

runtime_coverage <- runtime_main %>%
  distinct(
    method_group, scenario, scenario_label, true_dim_label, N, TimeP, NVAR,
    Error_sd, Loadings, rep, community_algorithm, design_block
  ) %>%
  filter(!is.na(design_block)) %>%
  count(method_group, true_dim_label, design_block, name = "ok_cells") %>%
  right_join(coverage_template, by = c("method_group", "true_dim_label", "design_block")) %>%
  left_join(full_counts, by = c("true_dim_label", "design_block")) %>%
  mutate(
    ok_cells = replace_na(ok_cells, 0L),
    coverage = ok_cells / full_cells,
    coverage_status = case_when(
      ok_cells == 0L ~ "Not run",
      coverage >= 0.999 ~ "Ran",
      TRUE ~ "Partial grid"
    ),
    coverage_status = factor(coverage_status, levels = c("Ran", "Partial grid", "Not run")),
    coverage_label = case_when(
      ok_cells == 0L ~ "not run",
      coverage >= 0.999 ~ paste0("ran\n", ok_cells, "/", full_cells),
      TRUE ~ paste0("partial\n", ok_cells, "/", full_cells)
    )
  )
readr::write_csv(runtime_coverage, file.path(out_dir, "figure4_method_runtime_coverage.csv"))

runtime_speedup <- runtime_summary %>%
  filter(design_block == design_block_levels[1]) %>%
  filter(!is.na(median_sec)) %>%
  select(method_group, true_dim_label, median_sec) %>%
  pivot_wider(names_from = method_group, values_from = median_sec) %>%
  mutate(
    graphicalvar_vs_dynega = GraphicalVAR / DynEGA,
    gimme_vs_dynega = GIMME / DynEGA
  )
readr::write_csv(runtime_speedup, file.path(out_dir, "figure4_method_runtime_speedup.csv"))

runtime_cols <- method_cols
runtime_shapes <- c("DynEGA" = 16, "GraphicalVAR" = 17, "GIMME" = 15)
runtime_plot <- ggplot(
  runtime_common_summary,
  aes(x = true_dim_label, y = median_sec, color = method_group, shape = method_group)
) +
  geom_linerange(
    aes(ymin = q25_sec, ymax = q75_sec),
    position = position_dodge(width = 0.55),
    linewidth = 0.55
  ) +
  geom_point(position = position_dodge(width = 0.55), size = 2.1) +
  geom_text(
    data = runtime_coverage %>%
      filter(method_group == "GIMME", design_block == design_block_levels[1], ok_cells == 0L) %>%
      distinct(true_dim_label),
    aes(x = true_dim_label, y = 620, label = "GIMME\nnot run"),
    inherit.aes = FALSE,
    color = "grey35",
    size = 2.4,
    lineheight = 0.9
  ) +
  scale_color_manual(values = runtime_cols, name = NULL) +
  scale_shape_manual(values = runtime_shapes, name = NULL) +
  scale_y_log10(
    breaks = c(1, 3, 10, 30, 60, 180, 600),
    labels = c("1 s", "3 s", "10 s", "30 s", "1 min", "3 min", "10 min")
  ) +
  labs(x = "True dimensionality", y = "Common base-grid time, median [IQR]") +
  theme_rr(8.3) +
  theme(panel.grid.minor = element_blank())
save_plot(runtime_plot, "figure4_method_runtime_successful", 5.8, 3.0)

coverage_fill <- c("Ran" = okabe[["blue"]], "Partial grid" = "#F0E442", "Not run" = "grey88")
coverage_plot <- ggplot(
  runtime_coverage,
  aes(x = design_block, y = method_group, fill = coverage_status)
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = coverage_label), size = 2.45, lineheight = 0.92, color = "grey12") +
  facet_wrap(~ true_dim_label, nrow = 1) +
  scale_fill_manual(values = coverage_fill, name = NULL) +
  labs(x = "Design block (N, T, p)", y = NULL) +
  theme_rr(8.0) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(lineheight = 0.88, size = rel(0.78)),
    legend.position = "bottom"
  )
save_plot(coverage_plot, "figure4_method_runtime_coverage", 7.2, 2.8)

runtime_feasibility_plot <- cowplot::plot_grid(
  runtime_plot + theme(legend.position = "bottom") + labs(tag = "A"),
  coverage_plot + theme(legend.position = "bottom") + labs(tag = "B"),
  ncol = 1,
  rel_heights = c(1.05, 1)
)
save_plot(runtime_feasibility_plot, "figure4_method_runtime_feasibility", 7.2, 5.7)

caption_main_hit <- c(
  "Main DynEGA hit-rate figure.",
  "The old hard scenario is excluded to the supplement. Rows show the renumbered main-text Scenarios A-C, selected for the clearest exact-recovery and/or factor-correlation variation.",
  "Columns show true dimensionality; the x-axis is measurement-error standard deviation and line color is loading strength."
)
writeLines(caption_main_hit, file.path(out_dir, "figure4_main_non_a_dynega_hit_rate_caption.txt"))

caption_main_cor <- c(
  "Main DynEGA factor-score correlation figure.",
  "The old hard scenario is excluded to the supplement. Rows show the renumbered main-text Scenarios A-C; columns show true dimensionality.",
  "The x-axis is measurement-error standard deviation and line color is loading strength."
)
writeLines(caption_main_cor, file.path(out_dir, "figure4_main_non_a_dynega_factor_cor_caption.txt"))

caption_method <- c(
  "Method-comparison suite for the clearest bounded overlap after excluding the supplementary hard scenario.",
  "Rows show main-text Scenarios A-C crossed with the 2D and 3D dimensions covered by the staged GIMME run; columns show loading strength.",
  "The x-axis is measurement-error standard deviation and line color is method. The old hard scenario is retained only as a supplementary edge case."
)
writeLines(caption_method, file.path(out_dir, "figure4_method_bcd_overlap_caption.txt"))

caption_accuracy <- c(
  "Reliable method-comparison hit-rate summary.",
  "The left panel shows hit rates with 95% cluster-bootstrap intervals across the full bounded GIMME-overlap subset after excluding the supplementary hard scenario.",
  "The right panel shows paired DynEGA-minus-comparator hit-rate differences using the same matched simulation cells. Intervals are clustered by simulation structure cell.",
  "Separate `levels` and `differences` versions are provided for cleaner manuscript layout if the combined legend is too dense."
)
writeLines(caption_accuracy, file.path(out_dir, "figure4_method_accuracy_reliable_caption.txt"))

caption_accuracy_cor <- c(
  "Method accuracy and conditional latent-correlation summary.",
  "The left panel shows hit rates with 95% cluster-bootstrap intervals across the bounded GIMME-overlap subset after excluding the supplementary hard scenario.",
  "The right panel shows the latent-score correlation only for rows in which the same method recovered the correct number of dimensions (`n_dim_exact = 1`).",
  "Intervals are clustered by simulation structure cell; sample sizes therefore differ across methods in the conditional correlation panel."
)
writeLines(caption_accuracy_cor, file.path(out_dir, "figure4_method_accuracy_conditional_correlation_caption.txt"))

caption_runtime <- c(
  "Runtime and feasibility comparison.",
  "Panel A shows median execution time with interquartile ranges on the common base grid where methods are directly comparable: N = 50, T = 100, p = 4.",
  "Panel B shows which design blocks were actually executed, using the DynEGA full grid as the reference. Not-run cells indicate conditions excluded from the staged comparator simulation as computationally impractical."
)
writeLines(caption_runtime, file.path(out_dir, "figure4_method_runtime_feasibility_caption.txt"))

readme <- c(
  "# Main Figure 4 Suite Excluding the Supplementary Hard Scenario",
  "",
  "The previous hard scenario is treated as a supplementary edge case. The main-text scenarios are labeled Scenarios A-C.",
  "",
  "## Effect Screen",
  "",
  paste0(
    "- Exact recovery top term: `",
    effect_screen %>% filter(response == "exact_recovery") %>% slice_max(partial_eta2, n = 1) %>% pull(term),
    "`."
  ),
  paste0(
    "- Factor-score correlation top term: `",
    effect_screen %>% filter(response == "factor_score_correlation") %>% slice_max(partial_eta2, n = 1) %>% pull(term),
    "`."
  ),
  "",
  "## Figures",
  "",
  "- `figure4_main_non_a_dynega_hit_rate.{png,pdf}`",
  "- `figure4_main_non_a_dynega_factor_cor.{png,pdf}`",
  "- `figure4_method_accuracy_reliable.{png,pdf}`",
  "- `figure4_method_accuracy_levels.{png,pdf}`",
  "- `figure4_method_accuracy_differences.{png,pdf}`",
  "- `figure4_method_accuracy_conditional_correlation.{png,pdf}`",
  "- `figure4_method_runtime_feasibility.{png,pdf}`",
  "- `figure4_method_runtime_successful.{png,pdf}`",
  "- `figure4_method_runtime_coverage.{png,pdf}`",
  "- `figure4_method_bcd_overlap_hit_rate.{png,pdf}`",
  "- `figure4_method_bcd_overlap_factor_cor.{png,pdf}`",
  "",
  "## Tables",
  "",
  "- `figure4_non_a_effect_screen.csv`",
  "- `figure4_non_a_scenario_spread.csv`",
  "- `figure4_main_non_a_dynega_summary.csv`",
  "- `figure4_method_bcd_overlap_summary.csv`",
  "- `figure4_method_bcd_overlap_condition_gaps.csv`",
  "- `figure4_method_accuracy_reliable_summary.csv`",
  "- `figure4_method_accuracy_reliable_differences.{csv,tex}`",
  "- `figure4_method_accuracy_conditional_correlation_summary.csv`",
  "- `figure4_method_runtime_summary.csv`",
  "- `figure4_method_runtime_common_base_summary.csv`",
  "- `figure4_method_runtime_coverage.csv`",
  "- `figure4_method_runtime_speedup.csv`",
  "- `figure4_method_performance_assessment.{csv,tex}`"
)
writeLines(readme, file.path(out_dir, "README_non_a_main_suite.md"))

cat("Saved main suite excluding supplementary hard scenario to ", out_dir, "\n", sep = "")
