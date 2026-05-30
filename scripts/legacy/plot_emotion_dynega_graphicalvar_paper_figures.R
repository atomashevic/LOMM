#!/usr/bin/env Rscript

## Paper-style DynEGA vs GraphicalVAR figures for the reset emotion benchmark.
## GraphicalVAR ok rows define the comparable analytic subset.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

root <- normalizePath(".", mustWork = TRUE)
dynega_dir <- Sys.getenv(
  "EMOTION_DYNEGA_DIR",
  file.path(root, "benchmark_outputs_emotion_dynega")
)
graphicalvar_dir <- Sys.getenv(
  "EMOTION_GRAPHICALVAR_DIR",
  file.path(root, "benchmark_outputs_emotion_graphicalvar")
)
out_dir <- Sys.getenv(
  "EMOTION_COMPARISON_OUT_DIR",
  file.path(root, "comparison_emotion_dynega_graphicalvar_paper")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_result_csv <- function(path) {
  candidates <- c(
    file.path(path, "benchmark_results.csv"),
    file.path(path, "checkpoint_results.csv")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    stop("No benchmark result CSV found in: ", path, call. = FALSE)
  }
  readr::read_csv(existing[[1]], show_col_types = FALSE)
}

dynega <- read_result_csv(dynega_dir)
graphicalvar <- read_result_csv(graphicalvar_dir)

scenario_levels <- c(
  "old_low_single_stable",
  "old_high_single_stable",
  "old_high_switch_stable",
  "old_high_switch_volatile",
  "oai_ranked_persistence",
  "oai_broad_geometry"
)
scenario_labels <- c(
  "old_low_single_stable" = "Old: low signal, single stable",
  "old_high_single_stable" = "Old: high signal, single stable",
  "old_high_switch_stable" = "Old: high signal, switch stable",
  "old_high_switch_volatile" = "Old: high signal, switch volatile",
  "oai_ranked_persistence" = "OpenAI-inspired: ranked persistence",
  "oai_broad_geometry" = "OpenAI-inspired: broad geometry"
)
method_levels <- c("DynEGA", "GraphicalVAR")
method_cols <- c("DynEGA" = "#0072B2", "GraphicalVAR" = "#D55E00")
community_levels <- c("louvain", "leiden", "walktrap")
community_labels <- c(
  "louvain" = "Louvain",
  "leiden" = "Leiden",
  "walktrap" = "Walktrap"
)

prepare_results <- function(df) {
  if (!"Error_sd" %in% names(df)) df$Error_sd <- df$Error
  if (!"Error_variance" %in% names(df)) df$Error_variance <- df$Error_sd^2
  df %>%
    mutate(
      method_group = case_when(
        method_family == "dynEGA" ~ "DynEGA",
        method_family == "graphicalVAR" ~ "GraphicalVAR",
        TRUE ~ method_label
      ),
      method_group = factor(method_group, levels = method_levels),
      community_algorithm = factor(as.character(community_algorithm), levels = community_levels),
      community_label = factor(
        community_labels[as.character(community_algorithm)],
        levels = unname(community_labels)
      ),
      scenario_family = factor(as.character(scenario_family), levels = scenario_levels),
      scenario_label = factor(
        scenario_labels[as.character(scenario_family)],
        levels = unname(scenario_labels)
      ),
      scenario_class = if_else(grepl("^oai_", as.character(scenario_family)), "OpenAI-inspired", "Old generator"),
      true_factors = factor(paste0(scenario_nfac, " factors"), levels = paste0(c(2, 3, 4), " factors")),
      condition_label = paste0(
        scenario_nfac, " factors\n",
        "eta=", eta, "\n",
        "zeta=", zeta
      ),
      scenario_condition_label = paste0(
        scenario_labels[as.character(scenario_family)],
        "\n",
        scenario_nfac, " factors",
        "\neta=", eta,
        "\nzeta=", zeta
      ),
      N = factor(N),
      TimeP = factor(TimeP),
      NVAR = factor(NVAR),
      Error_sd = factor(Error_sd),
      Loadings = factor(Loadings),
      status_group = case_when(
        status == "ok" ~ "ok",
        status == "timeout" ~ "timeout",
        TRUE ~ "error"
      )
    )
}

dynega <- prepare_results(dynega)
graphicalvar <- prepare_results(graphicalvar)

key_cols <- c(
  "scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "rep", "community_algorithm"
)

gv_ok_keys <- graphicalvar %>%
  filter(status == "ok") %>%
  distinct(across(all_of(key_cols)))

comparison <- bind_rows(
  dynega %>% semi_join(gv_ok_keys, by = key_cols),
  graphicalvar %>% semi_join(gv_ok_keys, by = key_cols)
) %>%
  filter(method_group %in% method_levels) %>%
  mutate(
    n_dim_exact = as.numeric(n_dim_exact),
    assignment_accuracy = as.numeric(assignment_accuracy),
    score_cor_common = as.numeric(score_cor_common),
    score_nrmse_common = as.numeric(score_nrmse_common),
    runtime_sec = as.numeric(runtime_sec)
  )

comparison_ok <- comparison %>% filter(status == "ok")
graphicalvar_status <- graphicalvar %>%
  filter(method_group == "GraphicalVAR")

scenario_group_labels <- comparison_ok %>%
  distinct(scenario_family, scenario_label, scenario_nfac, eta, zeta) %>%
  arrange(scenario_family, scenario_nfac) %>%
  group_by(scenario_family, scenario_label) %>%
  summarise(
    scenario_group_label = paste0(
      first(as.character(scenario_label)),
      "\n",
      paste0(
        scenario_nfac,
        " factors: eta=(",
        eta,
        "); zeta=(",
        zeta,
        ")",
        collapse = "\n"
      )
    ),
    .groups = "drop"
  ) %>%
  arrange(scenario_family) %>%
  mutate(
    scenario_group_label = factor(scenario_group_label, levels = unique(scenario_group_label))
  )

comparison_ok <- comparison_ok %>%
  left_join(
    scenario_group_labels %>% select(scenario_family, scenario_label, scenario_group_label),
    by = c("scenario_family", "scenario_label")
  )

ci_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  1.96 * stats::sd(x) / sqrt(length(x))
}

save_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 300)
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height)
}

theme_paper <- function(base_size = 9) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      strip.background = element_rect(fill = "grey92", color = "grey70")
    )
}

method_dodge <- position_dodge(width = 0.35)

accuracy_by_scenario <- comparison_ok %>%
  group_by(
    method_group, community_label, scenario_label, scenario_group_label,
    scenario_nfac, true_factors, condition_label, scenario_condition_label
  ) %>%
  summarise(
    exact = mean(n_dim_exact, na.rm = TRUE),
    exact_ci = ci_mean(n_dim_exact),
    assignment = mean(assignment_accuracy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p1_overview <- ggplot(
  accuracy_by_scenario,
  aes(
    x = scenario_nfac,
    y = exact,
    color = method_group,
    group = method_group
  )
) +
  geom_hline(yintercept = 1, color = "grey80", linewidth = 0.25) +
  geom_ribbon(
    aes(
      ymin = pmax(0, exact - exact_ci),
      ymax = pmin(1, exact + exact_ci),
      fill = method_group
    ),
    alpha = 0.12,
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.5, position = method_dodge) +
  geom_point(aes(shape = method_group), size = 2.0, position = method_dodge) +
  geom_errorbar(
    aes(ymin = pmax(0, exact - exact_ci), ymax = pmin(1, exact + exact_ci)),
    width = 0.12,
    alpha = 0.55,
    position = method_dodge
  ) +
  facet_grid(community_label ~ scenario_group_label) +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_fill_manual(values = method_cols, name = NULL) +
  scale_shape_manual(values = c("DynEGA" = 16, "GraphicalVAR" = 17), name = NULL) +
  scale_x_continuous(breaks = c(2, 3, 4), labels = paste0(c(2, 3, 4), " factors")) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(
    title = "Exact n.dim recovery by scenario",
    x = "True measured factors",
    y = "Exact n.dim recovery"
  ) +
  theme_paper(8) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text.x = element_text(size = 6.2, lineheight = 0.9),
    plot.margin = margin(t = 8, r = 8, b = 16, l = 24)
  )
save_plot(p1_overview, "01_exact_ndim_recovery_by_scenario", 17, 9.0)
save_plot(p1_overview, "01_exact_ndim_recovery_overview", 17, 9.0)

make_scenario_exact_plot <- function(scenario_name) {
  plot_data <- accuracy_by_scenario %>%
    filter(as.character(scenario_label) == scenario_name)

  ggplot(
    plot_data,
    aes(
      x = method_group,
      y = exact,
      color = method_group,
      shape = method_group
    )
  ) +
    geom_hline(yintercept = 1, color = "grey80", linewidth = 0.25) +
    geom_point(size = 2.2) +
    geom_errorbar(
      aes(ymin = pmax(0, exact - exact_ci), ymax = pmin(1, exact + exact_ci)),
      width = 0.12,
      alpha = 0.55
    ) +
    facet_grid(community_label ~ condition_label) +
    scale_color_manual(values = method_cols, name = NULL) +
    scale_shape_manual(values = c("DynEGA" = 16, "GraphicalVAR" = 17), name = NULL) +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    labs(
      title = paste("Exact n.dim recovery:", scenario_name),
      x = NULL,
      y = "Exact n.dim recovery"
    ) +
    theme_paper(8) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text.x = element_text(size = 7.2, lineheight = 0.95)
    )
}

scenario_file_stems <- c(
  "Old: low signal, single stable" = "01a_exact_ndim_old_low_single_stable",
  "Old: high signal, single stable" = "01b_exact_ndim_old_high_single_stable",
  "Old: high signal, switch stable" = "01c_exact_ndim_old_high_switch_stable",
  "Old: high signal, switch volatile" = "01d_exact_ndim_old_high_switch_volatile",
  "OpenAI-inspired: ranked persistence" = "01e_exact_ndim_oai_ranked_persistence"
)

for (scenario_name in names(scenario_file_stems)) {
  if (scenario_name %in% as.character(accuracy_by_scenario$scenario_label)) {
    save_plot(
      make_scenario_exact_plot(scenario_name),
      scenario_file_stems[[scenario_name]],
      8.8,
      6.6
    )
  }
}

p1 <- ggplot(
  accuracy_by_scenario,
  aes(
    x = method_group,
    y = exact,
    color = method_group,
    shape = method_group,
    group = method_group
  )
) +
  geom_hline(yintercept = 1, color = "grey80", linewidth = 0.25) +
  geom_point(size = 2.0) +
  geom_errorbar(
    aes(ymin = pmax(0, exact - exact_ci), ymax = pmin(1, exact + exact_ci)),
    width = 0.12,
    alpha = 0.55
  ) +
  facet_grid(community_label ~ scenario_condition_label) +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_shape_manual(values = c("DynEGA" = 16, "GraphicalVAR" = 17), name = NULL) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(
    title = "Exact n.dim recovery by scenario condition",
    x = NULL,
    y = "Exact n.dim recovery"
  ) +
  theme_paper(7.5) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 5.3, lineheight = 0.95)
  )
save_plot(p1, "01z_exact_ndim_all_conditions", 18, 8.5)

factor_sensitivity <- bind_rows(
  comparison_ok %>% transmute(method_group, community_label, factor = "N", level = as.numeric(as.character(N)), n_dim_exact),
  comparison_ok %>% transmute(method_group, community_label, factor = "Error_sd", level = as.numeric(as.character(Error_sd)), n_dim_exact),
  comparison_ok %>% transmute(method_group, community_label, factor = "Loadings", level = as.numeric(as.character(Loadings)), n_dim_exact)
) %>%
  group_by(method_group, community_label, factor, level) %>%
  summarise(
    exact = mean(n_dim_exact, na.rm = TRUE),
    exact_ci = ci_mean(n_dim_exact),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(factor = factor(factor, levels = c("N", "Error_sd", "Loadings")))

p2 <- ggplot(factor_sensitivity, aes(x = level, y = exact, color = method_group, group = method_group)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, exact - exact_ci),
      ymax = pmin(1, exact + exact_ci),
      fill = method_group
    ),
    alpha = 0.14,
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.9) +
  facet_grid(community_label ~ factor, scales = "free_x") +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_fill_manual(values = method_cols, name = NULL) +
  scale_x_continuous(breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(
    title = "Sensitivity to N, measurement error, and loadings",
    x = "Factor level",
    y = "Exact n.dim recovery"
  ) +
  theme_paper(8)
save_plot(p2, "02_old_grid_factor_sensitivity", 9.8, 7.2)

score_summary <- comparison_ok %>%
  group_by(method_group, community_label, scenario_label, true_factors, condition_label) %>%
  summarise(
    score_cor = mean(score_cor_common, na.rm = TRUE),
    score_cor_ci = ci_mean(score_cor_common),
    nrmse = mean(score_nrmse_common, na.rm = TRUE),
    nrmse_ci = ci_mean(score_nrmse_common),
    .groups = "drop"
  ) %>%
  pivot_longer(
    c(score_cor, nrmse),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("score_cor", "nrmse"),
      labels = c("Latent-score correlation", "Latent-score NRMSE")
    )
  )

p3 <- ggplot(score_summary, aes(x = true_factors, y = value, color = method_group, group = method_group)) +
  geom_line(linewidth = 0.5, position = method_dodge) +
  geom_point(aes(shape = method_group), size = 1.9, position = method_dodge) +
  facet_grid(metric ~ scenario_label, scales = "free_x", space = "free_x") +
  scale_color_manual(values = method_cols, name = NULL) +
  scale_shape_manual(values = c("DynEGA" = 16, "GraphicalVAR" = 17), name = NULL) +
  labs(
    title = "Score correlation and NRMSE",
    x = "True factors and DLO parameters",
    y = NULL
  ) +
  theme_paper(8) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5.5))
save_plot(p3, "03_score_correlation_and_nrmse", 14, 7)

ndim_distribution <- comparison_ok %>%
  group_by(method_group, community_label, scenario_label, true_factors, n_dim_hat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(method_group, community_label, scenario_label, true_factors) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p4 <- ggplot(ndim_distribution, aes(x = factor(n_dim_hat), y = prop, fill = method_group)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68) +
  facet_grid(community_label + true_factors ~ scenario_label) +
  scale_fill_manual(values = method_cols, name = NULL) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Estimated dimensionality distribution",
    x = "Estimated n.dim",
    y = "Share of comparable runs"
  ) +
  theme_paper(7.5) +
  theme(axis.text.x = element_text(size = 6))
save_plot(p4, "04_estimated_ndim_distribution", 15, 10)

gv_status_summary <- graphicalvar_status %>%
  count(scenario_label, true_factors, community_label, status_group, name = "rows") %>%
  group_by(scenario_label, true_factors, community_label) %>%
  mutate(prop = rows / sum(rows)) %>%
  ungroup()

p5 <- ggplot(gv_status_summary, aes(x = true_factors, y = prop, fill = status_group)) +
  geom_col(width = 0.72) +
  facet_grid(community_label ~ scenario_label) +
  scale_fill_manual(values = c(ok = "#009E73", timeout = "#CC79A7", error = "#999999"), name = NULL) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(
    title = "GraphicalVAR run status",
    x = "True measured factors",
    y = "Share of attempted rows"
  ) +
  theme_paper(8) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p5, "05_graphicalvar_status_boundary", 14, 8.5)

runtime_summary <- comparison %>%
  group_by(method_group, community_label, status_group) %>%
  summarise(
    median_runtime = median(runtime_sec, na.rm = TRUE),
    mean_runtime = mean(runtime_sec, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  ) %>%
  filter(status_group == "ok")

p6 <- ggplot(runtime_summary, aes(x = community_label, y = median_runtime, fill = method_group)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_fill_manual(values = method_cols, name = NULL) +
  scale_y_continuous(labels = label_number()) +
  labs(
    title = "Runtime for matched successful runs",
    x = "Community detection",
    y = "Median runtime, seconds"
  ) +
  theme_paper(9)
save_plot(p6, "06_runtime_comparable_subset", 8.5, 5)

partial_eta2 <- function(data, response, predictors) {
  keep <- data %>%
    select(all_of(c(response, predictors))) %>%
    filter(if_all(everything(), ~ !is.na(.x)))
  predictors <- predictors[vapply(
    predictors,
    function(pred) length(unique(keep[[pred]])) > 1L,
    logical(1)
  )]
  if (nrow(keep) < 10L) {
    return(data.frame())
  }
  if (length(predictors) == 0L) {
    return(data.frame())
  }
  formula <- stats::as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  fit <- stats::lm(formula, data = keep)
  tab <- stats::anova(fit)
  residual_ss <- tab["Residuals", "Sum Sq"]
  out <- data.frame(
    term = rownames(tab),
    ss = tab[, "Sum Sq"],
    df = tab[, "Df"],
    stringsAsFactors = FALSE
  ) %>%
    filter(term != "Residuals") %>%
    mutate(
      response = response,
      partial_eta2 = ss / (ss + residual_ss)
    ) %>%
    select(response, term, df, ss, partial_eta2)
  rownames(out) <- NULL
  out
}

eta_predictors <- c(
  "method_group", "community_label", "scenario_family", "scenario_nfac",
  "N", "TimeP", "NVAR", "Error_sd", "Loadings"
)
eta_table <- bind_rows(
  partial_eta2(comparison_ok, "n_dim_exact", eta_predictors),
  partial_eta2(comparison_ok, "assignment_accuracy", eta_predictors),
  partial_eta2(comparison_ok, "score_cor_common", eta_predictors),
  partial_eta2(comparison_ok, "score_nrmse_common", eta_predictors)
) %>%
  mutate(
    response_label = recode(
      response,
      n_dim_exact = "Exact n.dim",
      assignment_accuracy = "Assignment accuracy",
      score_cor_common = "Score correlation",
      score_nrmse_common = "Score NRMSE"
    ),
    term_label = recode(
      term,
      method_group = "Method",
      community_label = "Community",
      scenario_family = "Scenario",
      scenario_nfac = "True factors",
      Error_sd = "Error sd"
    )
  )
readr::write_csv(eta_table, file.path(out_dir, "partial_eta2_main_effects.csv"))

p7 <- eta_table %>%
  filter(response %in% c("n_dim_exact", "score_cor_common", "score_nrmse_common")) %>%
  mutate(
    term_label = factor(
      term_label,
      levels = rev(c("Method", "Community", "Scenario", "True factors", "N", "TimeP", "NVAR", "Error sd", "Loadings"))
    )
  ) %>%
  ggplot(aes(x = partial_eta2, y = term_label, fill = response_label)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  facet_wrap(~ response_label, nrow = 1, scales = "free_x") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Main-effect screening",
    x = "Partial eta-squared",
    y = NULL
  ) +
  theme_paper(8) +
  theme(legend.position = "none")
save_plot(p7, "07_partial_eta2_main_effects", 12, 4.8)

overall_summary <- comparison %>%
  group_by(method_group, community_label) %>%
  summarise(
    rows = n(),
    ok_rows = sum(status == "ok"),
    error_rows = sum(status_group == "error"),
    timeout_rows = sum(status_group == "timeout"),
    exact_ndim_ok = mean(n_dim_exact[status == "ok"], na.rm = TRUE),
    assignment_ok = mean(assignment_accuracy[status == "ok"], na.rm = TRUE),
    score_cor_ok = mean(score_cor_common[status == "ok"], na.rm = TRUE),
    score_nrmse_ok = mean(score_nrmse_common[status == "ok"], na.rm = TRUE),
    median_runtime_ok = median(runtime_sec[status == "ok"], na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(overall_summary, file.path(out_dir, "overall_method_summary.csv"))
readr::write_csv(accuracy_by_scenario, file.path(out_dir, "accuracy_by_scenario_factor.csv"))
readr::write_csv(factor_sensitivity, file.path(out_dir, "old_grid_factor_sensitivity.csv"))
readr::write_csv(score_summary, file.path(out_dir, "score_recovery_summary.csv"))
readr::write_csv(gv_status_summary, file.path(out_dir, "graphicalvar_status_summary.csv"))
scenario_parameter_labels <- comparison_ok %>%
  distinct(scenario_family, scenario_label, scenario_nfac, eta, zeta, condition_label) %>%
  arrange(scenario_family, scenario_nfac)
readr::write_csv(scenario_parameter_labels, file.path(out_dir, "scenario_parameter_labels.csv"))

design_summary <- data.frame(
  quantity = c(
    "DynEGA source rows",
    "GraphicalVAR source rows",
    "GraphicalVAR ok rows",
    "Comparable structure/community rows per method",
    "Comparable total rows",
    "Comparable unique structure fits"
  ),
  value = c(
    nrow(dynega),
    nrow(graphicalvar),
    sum(graphicalvar$status == "ok"),
    nrow(gv_ok_keys),
    nrow(comparison),
    nrow(gv_ok_keys) / length(community_levels)
  )
)
readr::write_csv(design_summary, file.path(out_dir, "comparison_design_summary.csv"))

format_summary_table <- function(df) {
  lines <- apply(df, 1, function(row) paste(row, collapse = " | "))
  header <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  c(header, sep, lines)
}

report_overall <- overall_summary %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    method_group = as.character(method_group),
    community_label = as.character(community_label)
  )

readme <- c(
  "# Emotion DynEGA vs GraphicalVAR Paper-Style Figures",
  "",
  "## Scope",
  "",
  "This bundle mirrors the old `Paper_emotion_dynamics_HGolino.Rmd` simulation-results structure: hit-rate/accuracy, manipulated-factor sensitivity, score recovery, estimated dimensionality, run status, runtime, and partial eta-squared screening.",
  "",
  "Figures 01-04 use matched successful GraphicalVAR rows. Figure 05 shows the GraphicalVAR rows that were not successful.",
  "",
  "The saved benchmark has `score_nrmse_common`, not RMSEA. Figures therefore report NRMSE explicitly rather than relabeling it as RMSEA.",
  "",
  "The main exact-recovery figure groups columns by scenario. Each column title lists the factor-specific DLO `eta` and `zeta` values; the x-axis stays on the true measured-factor count.",
  "",
  "Figure 02 reports only `N`, `Error_sd`, and `Loadings`; `TimeP` and `NVAR` are omitted from that sensitivity plot. Figures 01 and 02 use shaded 95% CI bands.",
  "",
  "## Sources",
  "",
  paste0("- DynEGA: `", dynega_dir, "`"),
  paste0("- GraphicalVAR: `", graphicalvar_dir, "`"),
  "",
  "## Matched Successful-Run Summary",
  "",
  format_summary_table(design_summary),
  "",
  "## Overall Method Summary",
  "",
  format_summary_table(report_overall),
  "",
  "## Figure Files",
  "",
  "- `01_exact_ndim_recovery_by_scenario.{png,pdf}`",
  "- `01_exact_ndim_recovery_overview.{png,pdf}`",
  "- `01a_exact_ndim_old_low_single_stable.{png,pdf}`",
  "- `01b_exact_ndim_old_high_single_stable.{png,pdf}`",
  "- `01c_exact_ndim_old_high_switch_stable.{png,pdf}`",
  "- `01d_exact_ndim_old_high_switch_volatile.{png,pdf}`",
  "- `01e_exact_ndim_oai_ranked_persistence.{png,pdf}`",
  "- `01z_exact_ndim_all_conditions.{png,pdf}`",
  "- `02_old_grid_factor_sensitivity.{png,pdf}`",
  "- `03_score_correlation_and_nrmse.{png,pdf}`",
  "- `04_estimated_ndim_distribution.{png,pdf}`",
  "- `05_graphicalvar_status_boundary.{png,pdf}`",
  "- `06_runtime_comparable_subset.{png,pdf}`",
  "- `07_partial_eta2_main_effects.{png,pdf}`",
  "",
  "## Tables",
  "",
  "- `overall_method_summary.csv`",
  "- `accuracy_by_scenario_factor.csv`",
  "- `old_grid_factor_sensitivity.csv`",
  "- `score_recovery_summary.csv`",
  "- `graphicalvar_status_summary.csv`",
  "- `scenario_parameter_labels.csv`",
  "- `partial_eta2_main_effects.csv`",
  "- `comparison_design_summary.csv`"
)
writeLines(readme, file.path(out_dir, "README.md"))

cat("Saved paper-style DynEGA vs GraphicalVAR figures to ", out_dir, "\n", sep = "")
