#!/usr/bin/env Rscript

## RR Figure 4 from the latest emotion DynEGA reset simulation.
## The report-facing figure names the source-defined scenario families as
## neutral Scenario A-F labels, but displays only the informative Scenario A-B
## rows crossed with true dimensions, measurement error, and loading strength.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

repo_root <- normalizePath(".", mustWork = TRUE)

input_dir <- Sys.getenv(
  "DYNEGA_RR_FIGURE_INPUT_DIR",
  file.path(repo_root, "simulation_outputs", "emotion_dynega")
)
graphicalvar_dir <- Sys.getenv(
  "DYNEGA_RR_GRAPHICALVAR_INPUT_DIR",
  file.path(repo_root, "simulation_outputs", "emotion_graphicalvar")
)
gimme_dir <- Sys.getenv(
  "DYNEGA_RR_GIMME_INPUT_DIR",
  file.path(repo_root, "simulation_outputs", "emotion_gimme")
)
out_dir <- Sys.getenv(
  "DYNEGA_RR_FIGURE_OUT_DIR",
  file.path(repo_root, "simulation_outputs", "method_comparison")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

input_csv <- file.path(input_dir, "benchmark_results.csv")
metadata_file <- file.path(input_dir, "benchmark_metadata.txt")
if (!file.exists(input_csv)) {
  stop("Missing DynEGA simulation results: ", input_csv, call. = FALSE)
}
graphicalvar_csv_candidates <- c(
  file.path(graphicalvar_dir, "benchmark_results.csv"),
  file.path(graphicalvar_dir, "benchmark_results_partial.csv"),
  file.path(graphicalvar_dir, "checkpoint_results.csv")
)
graphicalvar_csv_existing <- graphicalvar_csv_candidates[file.exists(graphicalvar_csv_candidates)]
graphicalvar_csv <- if (length(graphicalvar_csv_existing) > 0L) graphicalvar_csv_existing[[1]] else NA_character_
gimme_csv_candidates <- c(
  file.path(gimme_dir, "benchmark_results.csv"),
  file.path(gimme_dir, "benchmark_results_partial.csv"),
  file.path(gimme_dir, "checkpoint_results.csv")
)
gimme_csv_existing <- gimme_csv_candidates[file.exists(gimme_csv_candidates)]
gimme_csv <- if (length(gimme_csv_existing) > 0L) gimme_csv_existing[[1]] else NA_character_

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
      strip.text = element_text(size = rel(0.82), margin = margin(b = 2)),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.spacing.x = grid::unit(0.55, "lines"),
      plot.margin = margin(7, 8, 6, 7)
    )
}

save_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 320)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ci95 <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  1.96 * stats::sd(x) / sqrt(length(x))
}

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

scenario_labels <- c(
  old_low_single_stable = "Scenario A",
  old_high_single_stable = "Scenario B",
  old_high_switch_stable = "Scenario C",
  old_high_switch_volatile = "Scenario D",
  oai_ranked_persistence = "Scenario E",
  oai_broad_geometry = "Scenario F"
)

scenario_definitions <- data.frame(
  scenario_family = names(scenario_labels),
  scenario_label = unname(scenario_labels),
  description = c(
    "old generator; sigma_scale=0.50; active_gain=0.35; background_gain=0.05; no switching",
    "old generator; sigma_scale=1.00; active_gain=1.00; background_gain=0.08; no switching",
    "old generator; sigma_scale=1.00; active_gain=1.00; background_gain=0.08; switching dwell=0.20-0.45",
    "old generator; sigma_scale=1.25; active_gain=1.25; background_gain=0.08; switching dwell=0.03-0.10",
    "OpenAI-inspired ranked-persistence generator; sigma_q=0.25; active_gain=1.00; no switching",
    "OpenAI-inspired broad-geometry generator; sigma_q=0.35; active_gain=1.10; switching dwell=0.12-0.30"
  ),
  eta_zeta_pattern = c(
    "eta = first d of (-0.30,-0.55,-0.85,-1.10); zeta = first d of (-1.70,-1.00,-0.65,-0.30)",
    "eta = first d of (-0.30,-0.55,-0.85,-1.10); zeta = first d of (-1.70,-1.00,-0.65,-0.30)",
    "eta = first d of (-0.30,-0.55,-0.85,-1.10); zeta = first d of (-1.70,-1.00,-0.65,-0.30)",
    "eta = first d of (-0.30,-0.55,-0.85,-1.10); zeta = first d of (-1.70,-1.00,-0.65,-0.30)",
    "eta_j = -0.05*j; zeta_j = 0.06 + 0.02*j",
    "eta_j = -0.05*j; zeta_j = 0.06 + 0.02*j"
  ),
  stringsAsFactors = FALSE
)

raw <- readr::read_csv(input_csv, show_col_types = FALSE)

readr::write_csv(scenario_definitions, file.path(out_dir, "figure4_scenario_definitions.csv"))
scenario_table <- c(
  "\\begin{tabular}{p{0.11\\linewidth}p{0.43\\linewidth}p{0.36\\linewidth}}",
  "\\toprule",
  "Scenario & Generator settings & $\\eta/\\zeta$ pattern \\\\",
  "\\midrule",
  apply(
    scenario_definitions[, c("scenario_label", "description", "eta_zeta_pattern")],
    1,
    function(row) paste(latex_escape(unname(row)), collapse = " & ") |> paste0(" \\\\")
  ),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(scenario_table, file.path(out_dir, "figure4_scenario_definitions.tex"))

dynega <- raw %>%
  filter(method_family == "dynEGA", status == "ok") %>%
  mutate(
    exact_recovery = as.numeric(n_dim_exact),
    Error_sd = factor(fmt(as.numeric(Error), 3), levels = fmt(sort(unique(as.numeric(Error))), 3)),
    Loadings = factor(fmt(as.numeric(Loadings), 1), levels = fmt(sort(unique(as.numeric(Loadings))), 1)),
    scenario_family = factor(as.character(scenario_family), levels = names(scenario_labels)),
    scenario_label = factor(scenario_labels[as.character(scenario_family)], levels = unname(scenario_labels)),
    scenario_nfac = factor(as.integer(scenario_nfac), levels = c(2, 3, 4)),
    true_dim_label = factor(paste0(scenario_nfac, "D"), levels = c("2D", "3D", "4D")),
    community_algorithm = factor(community_algorithm, levels = c("louvain", "leiden", "walktrap")),
    N = factor(N),
    TimeP = factor(TimeP),
    NVAR = factor(NVAR),
    benchmark_grid = factor(benchmark_grid)
  )

if (nrow(dynega) == 0L) {
  stop("No valid DynEGA rows found in ", input_csv, call. = FALSE)
}

predictors <- c(
  "scenario_family", "scenario_nfac", "community_algorithm",
  "N", "TimeP", "NVAR", "Error_sd", "Loadings", "benchmark_grid"
)

factor_effects <- bind_rows(
  partial_eta2(dynega, "exact_recovery", predictors),
  partial_eta2(dynega, "score_nrmse_common", predictors),
  partial_eta2(dynega, "score_cor_common", predictors)
)
readr::write_csv(factor_effects, file.path(out_dir, "figure4_factor_effects.csv"))

scenario_spread <- dynega %>%
  group_by(scenario_family, scenario_label, scenario_nfac, Error_sd, Loadings) %>%
  summarise(
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    score_nrmse = mean(score_nrmse_common, na.rm = TRUE),
    score_cor = mean(score_cor_common, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  ) %>%
  group_by(scenario_family, scenario_label) %>%
  summarise(
    exact_range = diff(range(exact_recovery, na.rm = TRUE)),
    exact_min = min(exact_recovery, na.rm = TRUE),
    exact_max = max(exact_recovery, na.rm = TRUE),
    nrmse_range = diff(range(score_nrmse, na.rm = TRUE)),
    cor_range = diff(range(score_cor, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(exact_range), desc(nrmse_range))
readr::write_csv(scenario_spread, file.path(out_dir, "figure4_scenario_spread.csv"))

candidate_specs <- list(
  selected_exact_error_loading = list(
    selected = TRUE,
    metric = "exact_recovery",
    x = "Error_sd",
    color = "Loadings",
    facet1 = "scenario_label",
    facet2 = "scenario_nfac",
    reason = "Largest hit-rate grouping, displayed only for informative Scenario A-B rows to avoid flat C-F panels."
  ),
  rejected_time_nvar = list(
    selected = FALSE,
    metric = "exact_recovery",
    x = "TimeP",
    color = "NVAR",
    facet1 = "scenario_nfac",
    facet2 = "scenario_family",
    reason = "Closest literal analogue to the old time-points/NVAR display, but it has weaker spread."
  ),
  secondary_nrmse_error_loading = list(
    selected = FALSE,
    metric = "score_nrmse_common",
    x = "Error_sd",
    color = "Loadings",
    facet1 = "scenario_nfac",
    facet2 = "scenario_family",
    reason = "Most sensitive continuous recovery metric, retained as a diagnostic companion rather than the main hit-rate figure."
  )
)

score_candidate <- function(name, spec) {
  vars <- unique(na.omit(unname(unlist(spec[c("x", "color", "facet1", "facet2")]))))
  summary <- dynega %>%
    group_by(across(all_of(vars))) %>%
    summarise(value = mean(.data[[spec$metric]], na.rm = TRUE), .groups = "drop")
  facet_vars <- unique(na.omit(c(spec$facet1, spec$facet2)))
  panel_ranges <- if (length(facet_vars) > 0L) {
    summary %>%
      group_by(across(all_of(facet_vars))) %>%
      summarise(panel_range = diff(range(value, na.rm = TRUE)), .groups = "drop")
  } else {
    data.frame(panel_range = diff(range(summary$value, na.rm = TRUE)))
  }
  data.frame(
    candidate = name,
    metric = spec$metric,
    x = spec$x,
    color = spec$color,
    facet_rows = spec$facet1,
    facet_cols = spec$facet2 %||% NA_character_,
    panels = nrow(panel_ranges),
    median_panel_range = median(panel_ranges$panel_range, na.rm = TRUE),
    max_panel_range = max(panel_ranges$panel_range, na.rm = TRUE),
    total_range = diff(range(summary$value, na.rm = TRUE)),
    min_value = min(summary$value, na.rm = TRUE),
    max_value = max(summary$value, na.rm = TRUE),
    selected = isTRUE(spec$selected),
    reason = spec$reason,
    stringsAsFactors = FALSE
  )
}

candidate_scores <- bind_rows(Map(score_candidate, names(candidate_specs), candidate_specs)) %>%
  arrange(desc(selected), desc(metric == "exact_recovery"), desc(total_range), desc(median_panel_range))
readr::write_csv(candidate_scores, file.path(out_dir, "figure4_candidate_scores.csv"))

selected_summary <- dynega %>%
  filter(scenario_label %in% c("Scenario A", "Scenario B")) %>%
  group_by(scenario_label, true_dim_label, Error_sd, Loadings) %>%
  summarise(
    exact_recovery = mean(exact_recovery, na.rm = TRUE),
    exact_recovery_ci = ci95(as.numeric(n_dim_exact)),
    score_nrmse = mean(score_nrmse_common, na.rm = TRUE),
    score_cor = mean(score_cor_common, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  ) %>%
  mutate(scenario_label = droplevels(scenario_label))
readr::write_csv(selected_summary, file.path(out_dir, "figure4_selected_summary.csv"))

grid_summary <- dynega %>%
  summarise(
    source_rows = n(),
    source_methods = paste(sort(unique(as.character(method))), collapse = ";"),
    source_communities = paste(sort(unique(as.character(community_algorithm))), collapse = ";"),
    exact_recovery_mean = mean(exact_recovery, na.rm = TRUE),
    exact_recovery_min = min(exact_recovery, na.rm = TRUE),
    exact_recovery_max = max(exact_recovery, na.rm = TRUE),
    displayed_factors = "scenario_family;scenario_nfac;Error_sd;Loadings"
  )
readr::write_csv(grid_summary, file.path(out_dir, "figure4_grid_summary.csv"))

loading_cols <- c(
  "0.4" = okabe[["blue"]],
  "0.6" = okabe[["orange"]],
  "0.8" = okabe[["vermillion"]]
)

selected_plot <- ggplot(
  selected_summary,
  aes(
    x = as.numeric(as.character(Error_sd)),
    y = exact_recovery,
    color = Loadings,
    group = Loadings
  )
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.9) +
  geom_errorbar(
    aes(
      ymin = pmax(0, exact_recovery - exact_recovery_ci),
      ymax = pmin(1, exact_recovery + exact_recovery_ci)
    ),
    width = 0.015,
    linewidth = 0.32,
    alpha = 0.72
  ) +
  facet_grid(scenario_label ~ true_dim_label) +
  scale_color_manual(values = loading_cols, name = "Loading") +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(
    x = "Measurement error SD",
    y = "Exact n.dim hit rate"
  ) +
  theme_original_style(8.5) +
  theme(
    axis.text.x = element_text(angle = 0),
    strip.text.y = element_text(angle = 0)
  )
save_plot(selected_plot, "figure4_dynega_rr_selected", 8.4, 4.2)

dynega_cor_effects <- factor_effects %>%
  filter(response == "score_cor_common") %>%
  arrange(desc(partial_eta2))
readr::write_csv(
  dynega_cor_effects,
  file.path(out_dir, "figure4_dynega_factor_cor_effects.csv")
)

dynega_cor_summary <- dynega %>%
  group_by(scenario_label, true_dim_label, Error_sd, Loadings) %>%
  summarise(
    score_cor = mean(score_cor_common, na.rm = TRUE),
    rows = n(),
    .groups = "drop"
  )
readr::write_csv(
  dynega_cor_summary,
  file.path(out_dir, "figure4_dynega_factor_cor_summary.csv")
)

dynega_cor_plot <- ggplot(
  dynega_cor_summary,
  aes(
    x = as.numeric(as.character(Error_sd)),
    y = score_cor,
    color = Loadings,
    group = Loadings
  )
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_grid(scenario_label ~ true_dim_label) +
  scale_color_manual(values = loading_cols, name = "Loading") +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  scale_y_continuous(labels = label_number(accuracy = 0.01), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(
    x = "Measurement error SD",
    y = "Factor-score correlation"
  ) +
  theme_original_style(8.0) +
  theme(
    axis.text.x = element_text(angle = 0),
    strip.text.y = element_text(angle = 0)
  )
save_plot(dynega_cor_plot, "figure4_dynega_factor_cor_selected", 8.4, 8.4)

dynega_cor_caption_lines <- c(
  "Figure 4 companion. DynEGA factor-score correlation in the latest full emotion-DynEGA grid.",
  "Rows show neutral generator scenarios A-F and columns show true dimensionality.",
  "The selected grouping follows the largest factor-correlation effect screen: scenario family, measurement-error standard deviation, loading strength, and true dimensionality.",
  "The x-axis is measurement-error standard deviation and line color is loading strength; points are means over sample size, time-point, NVAR, community-algorithm, and replication cells."
)
writeLines(
  dynega_cor_caption_lines,
  file.path(out_dir, "figure4_dynega_factor_cor_caption.txt")
)

method_compare_summary <- data.frame()
method_compare_design <- data.frame(
  quantity = c("GraphicalVAR source", "GIMME source", "Comparable Scenario A rows"),
  value = c(
    ifelse(is.na(graphicalvar_csv), "<missing>", graphicalvar_csv),
    ifelse(is.na(gimme_csv), "<missing>", gimme_csv),
    "0"
  ),
  stringsAsFactors = FALSE
)

if (!is.na(graphicalvar_csv)) {
  graphicalvar_raw <- readr::read_csv(graphicalvar_csv, show_col_types = FALSE)
  gimme_raw <- if (!is.na(gimme_csv)) readr::read_csv(gimme_csv, show_col_types = FALSE) else data.frame()
  dynega_raw <- raw
  if (!"Error_sd" %in% names(dynega_raw)) dynega_raw$Error_sd <- dynega_raw$Error
  if (!"Error_sd" %in% names(graphicalvar_raw)) graphicalvar_raw$Error_sd <- graphicalvar_raw$Error
  if (nrow(gimme_raw) > 0L && !"Error_sd" %in% names(gimme_raw)) gimme_raw$Error_sd <- gimme_raw$Error

  method_key_cols <- c(
    "scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "rep", "community_algorithm"
  )
  method_sources <- list(dynega_raw, graphicalvar_raw)
  if (nrow(gimme_raw) > 0L) method_sources <- c(method_sources, list(gimme_raw))
  common_method_cols <- Reduce(intersect, lapply(method_sources, names))
  missing_key_cols <- setdiff(method_key_cols, common_method_cols)

  if (length(missing_key_cols) == 0L) {
    scenario_a_keys <- graphicalvar_raw %>%
      filter(status == "ok", scenario_family == "old_low_single_stable") %>%
      distinct(across(all_of(method_key_cols)))

    gimme_scenario_a_keys <- if (nrow(gimme_raw) > 0L) {
      gimme_raw %>%
        filter(method_family == "gimme", status == "ok", scenario_family == "old_low_single_stable") %>%
        distinct(across(all_of(method_key_cols)))
    } else {
      data.frame()
    }

    gimme_scenario_a <- if (nrow(gimme_raw) > 0L) {
      gimme_raw %>%
        filter(method_family == "gimme", status == "ok", scenario_family == "old_low_single_stable") %>%
        semi_join(gimme_scenario_a_keys, by = method_key_cols) %>%
        mutate(method_group = "GIMME")
    } else {
      data.frame()
    }

    method_compare <- bind_rows(
      dynega_raw %>%
        filter(method_family == "dynEGA", status == "ok", scenario_family == "old_low_single_stable") %>%
        semi_join(scenario_a_keys, by = method_key_cols) %>%
        mutate(method_group = "DynEGA"),
      graphicalvar_raw %>%
        filter(method_family == "graphicalVAR", status == "ok", scenario_family == "old_low_single_stable") %>%
        semi_join(scenario_a_keys, by = method_key_cols) %>%
        mutate(method_group = "GraphicalVAR"),
      gimme_scenario_a
    ) %>%
      mutate(
        method_group = factor(method_group, levels = c("DynEGA", "GIMME", "GraphicalVAR")),
        exact_recovery = as.numeric(n_dim_exact),
        Error_sd = factor(fmt(as.numeric(Error_sd), 3), levels = fmt(c(0.125, 0.250, 0.500), 3)),
        Loadings = factor(fmt(as.numeric(Loadings), 1), levels = fmt(c(0.4, 0.6, 0.8), 1)),
        scenario_nfac = factor(as.integer(scenario_nfac), levels = c(2, 3, 4)),
        true_dim_label = factor(paste0(scenario_nfac, "D"), levels = c("2D", "3D", "4D"))
      )

    method_compare_summary <- method_compare %>%
      group_by(method_group, true_dim_label, Error_sd, Loadings) %>%
      summarise(
        exact_recovery = mean(exact_recovery, na.rm = TRUE),
        exact_recovery_ci = ci95(as.numeric(n_dim_exact)),
        rows = n(),
        .groups = "drop"
      )
    readr::write_csv(
      method_compare_summary,
      file.path(out_dir, "figure4_dynega_vs_graphicalvar_scenario_a_summary.csv")
    )

    method_compare_design <- data.frame(
      quantity = c(
        "DynEGA source",
        "GraphicalVAR source",
        "GIMME source",
        "Scenario A matched keys",
        "Scenario A comparable rows",
        "Scenario A GIMME rows",
        "Scenario A GIMME dimensions",
        "Compared methods"
      ),
      value = c(
        input_csv,
        graphicalvar_csv,
        ifelse(is.na(gimme_csv), "<missing>", gimme_csv),
        as.character(nrow(scenario_a_keys)),
        as.character(nrow(method_compare)),
        as.character(nrow(gimme_scenario_a)),
        if (nrow(gimme_scenario_a) > 0L) {
          paste(sort(unique(as.character(gimme_scenario_a$scenario_nfac))), collapse = ";")
        } else {
          "<none>"
        },
        paste(levels(method_compare$method_group), collapse = ";")
      ),
      stringsAsFactors = FALSE
    )
    readr::write_csv(
      method_compare_design,
      file.path(out_dir, "figure4_dynega_vs_graphicalvar_scenario_a_design.csv")
    )

    missing_method_panels <- data.frame(
      method_group = factor("GIMME", levels = c("DynEGA", "GIMME", "GraphicalVAR")),
      true_dim_label = factor("4D", levels = c("2D", "3D", "4D")),
      x = 0.25,
      y = 0.55,
      label = "GIMME\nnot run",
      stringsAsFactors = FALSE
    )

    method_compare_plot <- ggplot(
      method_compare_summary,
      aes(
        x = as.numeric(as.character(Error_sd)),
        y = exact_recovery,
        color = Loadings,
        group = Loadings
      )
    ) +
      geom_line(linewidth = 0.55) +
      geom_point(size = 1.9) +
      geom_text(
        data = missing_method_panels,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        color = "grey45",
        size = 3.0,
        lineheight = 0.92
      ) +
      facet_grid(method_group ~ true_dim_label, drop = FALSE) +
      scale_color_manual(values = loading_cols, name = "Loading") +
      scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
      labs(
        x = "Measurement error SD",
        y = "Exact n.dim hit rate"
      ) +
      theme_original_style(8.5) +
      theme(
        axis.text.x = element_text(angle = 0),
        strip.text.y = element_text(angle = 0)
      )
    save_plot(method_compare_plot, "figure4_dynega_vs_graphicalvar_scenario_a", 8.4, 5.8)

    method_caption_lines <- c(
      "Alternative Figure 4. Exact dimensional hit rate for DynEGA, GIMME, and GraphicalVAR in Scenario A.",
      "Rows show method and columns show true dimensionality; the x-axis is measurement-error standard deviation and line color is loading strength.",
      "DynEGA and GraphicalVAR use matched successful Scenario A GraphicalVAR cells. GIMME is added where the staged feasibility run has Scenario A coverage, which is limited to 2D and 3D at N=50, TimeP=100, and NVAR=4.",
      "Points are means over available matched cells."
    )
    writeLines(
      method_caption_lines,
      file.path(out_dir, "figure4_dynega_vs_graphicalvar_scenario_a_caption.txt")
    )

    method_cor_summary <- method_compare %>%
      group_by(method_group, true_dim_label, Error_sd, Loadings) %>%
      summarise(
        score_cor = mean(score_cor_common, na.rm = TRUE),
        rows = n(),
        .groups = "drop"
      )
    readr::write_csv(
      method_cor_summary,
      file.path(out_dir, "figure4_dynega_gimme_graphicalvar_scenario_a_factor_cor_summary.csv")
    )

    method_cor_plot <- ggplot(
      method_cor_summary,
      aes(
        x = as.numeric(as.character(Error_sd)),
        y = score_cor,
        color = Loadings,
        group = Loadings
      )
    ) +
      geom_line(linewidth = 0.55) +
      geom_point(size = 1.9) +
      geom_text(
        data = missing_method_panels,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        color = "grey45",
        size = 3.0,
        lineheight = 0.92
      ) +
      facet_grid(method_group ~ true_dim_label, drop = FALSE) +
      scale_color_manual(values = loading_cols, name = "Loading") +
      scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
      scale_y_continuous(labels = label_number(accuracy = 0.01), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
      labs(
        x = "Measurement error SD",
        y = "Factor-score correlation"
      ) +
      theme_original_style(8.5) +
      theme(
        axis.text.x = element_text(angle = 0),
        strip.text.y = element_text(angle = 0)
      )
    save_plot(method_cor_plot, "figure4_dynega_gimme_graphicalvar_scenario_a_factor_cor", 8.4, 5.8)

    method_cor_caption_lines <- c(
      "Alternative Figure 4. Factor-score correlation for DynEGA, GIMME, and GraphicalVAR in Scenario A.",
      "Rows show method and columns show true dimensionality; the x-axis is measurement-error standard deviation and line color is loading strength.",
      "DynEGA and GraphicalVAR use matched successful Scenario A GraphicalVAR cells. GIMME is added where the staged feasibility run has Scenario A coverage, which is limited to 2D and 3D at N=50, TimeP=100, and NVAR=4.",
      "Points are means over available matched cells."
    )
    writeLines(
      method_cor_caption_lines,
      file.path(out_dir, "figure4_dynega_gimme_graphicalvar_scenario_a_factor_cor_caption.txt")
    )

    make_method_compare <- function(keys, include_gimme = FALSE) {
      rows <- list(
        dynega_raw %>%
          filter(method_family == "dynEGA", status == "ok", scenario_family == "old_low_single_stable") %>%
          semi_join(keys, by = method_key_cols) %>%
          mutate(method_group = "DynEGA"),
        graphicalvar_raw %>%
          filter(method_family == "graphicalVAR", status == "ok", scenario_family == "old_low_single_stable") %>%
          semi_join(keys, by = method_key_cols) %>%
          mutate(method_group = "GraphicalVAR")
      )
      if (include_gimme && nrow(gimme_raw) > 0L) {
        rows <- append(
          rows,
          list(
            gimme_raw %>%
              filter(method_family == "gimme", status == "ok", scenario_family == "old_low_single_stable") %>%
              semi_join(keys, by = method_key_cols) %>%
              mutate(method_group = "GIMME")
          )
        )
      }
      bind_rows(rows) %>%
        mutate(method_group = factor(method_group, levels = c("DynEGA", "GIMME", "GraphicalVAR")))
    }

    summarise_method_table <- function(data, scope_label) {
      structure_key_cols <- setdiff(method_key_cols, "community_algorithm")
      data$structure_cell_id <- apply(data[, structure_key_cols, drop = FALSE], 1, paste, collapse = "|")
      data %>%
        group_by(method_group) %>%
        summarise(
          scope = scope_label,
          dimensions = paste(sort(unique(paste0(scenario_nfac, "D"))), collapse = ", "),
          structure_cells = n_distinct(structure_cell_id),
          method_rows = n(),
          exact_hit_rate = mean(as.numeric(n_dim_exact), na.rm = TRUE),
          assignment_accuracy = mean(as.numeric(assignment_accuracy), na.rm = TRUE),
          factor_score_correlation = mean(as.numeric(score_cor_common), na.rm = TRUE),
          factor_score_nrmse = mean(as.numeric(score_nrmse_common), na.rm = TRUE),
          median_runtime_sec = median(as.numeric(runtime_sec), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        select(scope, method_group, dimensions, structure_cells, method_rows, everything())
    }

    comparison_table <- bind_rows(
      summarise_method_table(
        make_method_compare(scenario_a_keys, include_gimme = FALSE),
        "Scenario A: GraphicalVAR-matched"
      ),
      if (nrow(gimme_scenario_a_keys) > 0L) {
        summarise_method_table(
          make_method_compare(gimme_scenario_a_keys, include_gimme = TRUE),
          "Scenario A: GIMME-overlap bounded"
        )
      } else {
        data.frame()
      }
    ) %>%
      mutate(
        scope = factor(
          scope,
          levels = c(
            "Scenario A: GraphicalVAR-matched",
            "Scenario A: GIMME-overlap bounded"
          )
        ),
        method_group = factor(method_group, levels = c("DynEGA", "GIMME", "GraphicalVAR"))
      ) %>%
      arrange(scope, method_group)
    readr::write_csv(
      comparison_table,
      file.path(out_dir, "figure4_scenario_a_method_comparison_table.csv")
    )

    gv_scope <- comparison_table %>%
      filter(scope == "Scenario A: GraphicalVAR-matched") %>%
      select(method_group, exact_hit_rate, assignment_accuracy, factor_score_correlation, factor_score_nrmse, median_runtime_sec)
    gimme_scope <- comparison_table %>%
      filter(scope == "Scenario A: GIMME-overlap bounded") %>%
      select(method_group, exact_hit_rate, assignment_accuracy, factor_score_correlation, factor_score_nrmse, median_runtime_sec)
    interp_lines <- c(
      "# Scenario A Method Comparison Interpretation",
      "",
      "The factor-score correlation panel by itself does not show a clear DynEGA advantage. In both the GraphicalVAR-matched and GIMME-overlap subsets, GraphicalVAR has slightly higher mean factor-score correlation and lower NRMSE than DynEGA.",
      "",
      "The DynEGA advantage is instead clearest on exact dimensional recovery, assignment accuracy, and runtime.",
      "",
      "## GraphicalVAR-Matched Subset",
      "",
      paste0(
        "- DynEGA: exact=", fmt(gv_scope$exact_hit_rate[gv_scope$method_group == "DynEGA"], 3),
        ", assignment=", fmt(gv_scope$assignment_accuracy[gv_scope$method_group == "DynEGA"], 3),
        ", correlation=", fmt(gv_scope$factor_score_correlation[gv_scope$method_group == "DynEGA"], 3),
        ", median runtime=", fmt(gv_scope$median_runtime_sec[gv_scope$method_group == "DynEGA"], 1), " s."
      ),
      paste0(
        "- GraphicalVAR: exact=", fmt(gv_scope$exact_hit_rate[gv_scope$method_group == "GraphicalVAR"], 3),
        ", assignment=", fmt(gv_scope$assignment_accuracy[gv_scope$method_group == "GraphicalVAR"], 3),
        ", correlation=", fmt(gv_scope$factor_score_correlation[gv_scope$method_group == "GraphicalVAR"], 3),
        ", median runtime=", fmt(gv_scope$median_runtime_sec[gv_scope$method_group == "GraphicalVAR"], 1), " s."
      ),
      "",
      "## GIMME-Overlap Bounded Subset",
      "",
      apply(
        gimme_scope,
        1,
        function(row) {
          paste0(
            "- ", row[["method_group"]],
            ": exact=", fmt(as.numeric(row[["exact_hit_rate"]]), 3),
            ", assignment=", fmt(as.numeric(row[["assignment_accuracy"]]), 3),
            ", correlation=", fmt(as.numeric(row[["factor_score_correlation"]]), 3),
            ", median runtime=", fmt(as.numeric(row[["median_runtime_sec"]]), 1), " s."
          )
        }
      )
    )
    writeLines(
      interp_lines,
      file.path(out_dir, "figure4_scenario_a_method_comparison_interpretation.md")
    )

    comparison_table_tex <- comparison_table %>%
      mutate(
        exact_hit_rate = fmt(exact_hit_rate, 3),
        assignment_accuracy = fmt(assignment_accuracy, 3),
        factor_score_correlation = fmt(factor_score_correlation, 3),
        factor_score_nrmse = fmt(factor_score_nrmse, 3),
        median_runtime_sec = fmt(median_runtime_sec, 1)
      )
    comparison_table_lines <- c(
      "\\begin{tabular}{p{0.22\\linewidth}p{0.12\\linewidth}p{0.07\\linewidth}rrrrr}",
      "\\toprule",
      "Scope & Method & Dim. & Exact & Assign. & Corr. & NRMSE & Median s \\\\",
      "\\midrule",
      apply(
        comparison_table_tex[
          ,
          c(
            "scope", "method_group", "dimensions", "exact_hit_rate", "assignment_accuracy",
            "factor_score_correlation", "factor_score_nrmse", "median_runtime_sec"
          )
        ],
        1,
        function(row) paste(latex_escape(as.character(unname(row))), collapse = " & ") |> paste0(" \\\\")
      ),
      "\\bottomrule",
      "\\end{tabular}"
    )
    writeLines(
      comparison_table_lines,
      file.path(out_dir, "figure4_scenario_a_method_comparison_table.tex")
    )
  } else {
    warning(
      "Skipping DynEGA vs GraphicalVAR Scenario A plot; missing key columns: ",
      paste(missing_key_cols, collapse = ", "),
      call. = FALSE
    )
  }
} else {
  warning("Skipping DynEGA vs GraphicalVAR Scenario A plot; GraphicalVAR CSV not found.", call. = FALSE)
}

nrmse_summary <- dynega %>%
  filter(scenario_label %in% c("Scenario A", "Scenario B")) %>%
  group_by(scenario_label, true_dim_label, Error_sd, Loadings) %>%
  summarise(
    score_nrmse = mean(score_nrmse_common, na.rm = TRUE),
    score_nrmse_ci = ci95(score_nrmse_common),
    rows = n(),
    .groups = "drop"
  ) %>%
  mutate(scenario_label = droplevels(scenario_label))

nrmse_plot <- ggplot(
  nrmse_summary,
  aes(x = as.numeric(as.character(Error_sd)), y = score_nrmse, color = Loadings, group = Loadings)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.9) +
  geom_errorbar(
    aes(ymin = pmax(0, score_nrmse - score_nrmse_ci), ymax = score_nrmse + score_nrmse_ci),
    width = 0.015,
    linewidth = 0.32,
    alpha = 0.72
  ) +
  facet_grid(scenario_label ~ true_dim_label) +
  scale_color_manual(values = loading_cols, name = "Loading") +
  scale_x_continuous(breaks = c(0.125, 0.250, 0.500), labels = c("0.125", "0.250", "0.500")) +
  labs(x = "Measurement error SD", y = "Latent-score NRMSE") +
  theme_original_style(8.5) +
  theme(strip.text.y = element_text(angle = 0))
save_plot(nrmse_plot, "figure4_candidate_score_nrmse", 8.4, 4.2)

time_nvar_summary <- dynega %>%
  group_by(true_dim_label, TimeP, NVAR) %>%
  summarise(exact_recovery = mean(exact_recovery, na.rm = TRUE), rows = n(), .groups = "drop")

time_nvar_plot <- ggplot(
  time_nvar_summary,
  aes(x = as.numeric(as.character(TimeP)), y = exact_recovery, color = NVAR, group = NVAR)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.9) +
  facet_wrap(~true_dim_label, nrow = 1) +
  scale_color_manual(values = c("4" = okabe[["orange"]], "6" = okabe[["blue"]]), name = "NVAR") +
  scale_x_continuous(breaks = c(100, 300)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.02), breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Time points / frames", y = "Exact n.dim hit rate") +
  theme_original_style(8.5)
save_plot(time_nvar_plot, "figure4_candidate_timep_nvar", 7.0, 3.25)

caption_lines <- c(
  "Figure 4. DynEGA exact dimensional hit rate in the latest full emotion-DynEGA grid.",
  "Rows show Scenario A and Scenario B, the two generator scenarios with the clearest hit-rate degradation; all six generator scenarios are defined in the accompanying table.",
  "Points are means over sample size, time-point, NVAR, community-algorithm, and replication cells; error bars show 95% confidence intervals.",
  "The x-axis is measurement-error standard deviation and line color is loading strength."
)
writeLines(caption_lines, file.path(out_dir, "figure4_dynega_rr_caption.txt"))

top_effects <- factor_effects %>%
  filter(response == "exact_recovery") %>%
  arrange(desc(partial_eta2)) %>%
  slice_head(n = 6) %>%
  transmute(term, partial_eta2 = fmt(partial_eta2, 3))

readme <- c(
  "# DynEGA RR Figure 4",
  "",
  "## Source",
  "",
  paste0("- Input CSV: `", input_csv, "`"),
  if (!is.na(graphicalvar_csv)) paste0("- GraphicalVAR CSV for companion comparison: `", graphicalvar_csv, "`") else character(0),
  if (!is.na(gimme_csv)) paste0("- GIMME CSV for companion comparison: `", gimme_csv, "`") else character(0),
  if (file.exists(metadata_file)) paste0("- Metadata: `", metadata_file, "`") else character(0),
  paste0("- Valid DynEGA rows used for selection: ", nrow(dynega)),
  paste0("- Exact n.dim hit rate across the full grid: mean=", fmt(grid_summary$exact_recovery_mean, 3),
         ", min=", fmt(grid_summary$exact_recovery_min, 3),
         ", max=", fmt(grid_summary$exact_recovery_max, 3)),
  "",
  "## Selection",
  "",
  "The closest literal remake (`TimeP` by `NVAR`) was tested but had weaker spread. The selected report-facing hit-rate display uses the largest hit-rate grouping but shows only Scenario A and Scenario B, because Scenario C-F are nearly flat at ceiling and make the plot harder to read.",
  if (nrow(method_compare_summary) > 0L) {
    "A companion DynEGA-GIMME-GraphicalVAR version keeps only Scenario A. DynEGA and GraphicalVAR use matched successful GraphicalVAR cells; GIMME is shown for its bounded staged coverage only, with 4D marked as not run."
  } else {
    character(0)
  },
  "",
  "Top main effects for exact recovery:",
  "",
  paste0("- `", top_effects$term, "`: partial eta2 = ", top_effects$partial_eta2),
  "",
  "Top main effects for factor-score correlation:",
  "",
  paste0(
    "- `",
    dynega_cor_effects$term[seq_len(min(6L, nrow(dynega_cor_effects)))],
    "`: partial eta2 = ",
    fmt(dynega_cor_effects$partial_eta2[seq_len(min(6L, nrow(dynega_cor_effects)))], 3)
  ),
  "",
  "## Files",
  "",
  "- `figure4_dynega_rr_selected.{png,pdf}`",
  "- `figure4_dynega_factor_cor_selected.{png,pdf}`",
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_vs_graphicalvar_scenario_a.{png,pdf}`" else character(0),
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_gimme_graphicalvar_scenario_a_factor_cor.{png,pdf}`" else character(0),
  "- `figure4_candidate_score_nrmse.{png,pdf}`",
  "- `figure4_candidate_timep_nvar.{png,pdf}`",
  "- `figure4_candidate_scores.csv`",
  "- `figure4_factor_effects.csv`",
  "- `figure4_scenario_definitions.csv`",
  "- `figure4_scenario_definitions.tex`",
  "- `figure4_scenario_spread.csv`",
  "- `figure4_grid_summary.csv`",
  "- `figure4_selected_summary.csv`",
  "- `figure4_dynega_factor_cor_summary.csv`",
  "- `figure4_dynega_factor_cor_effects.csv`",
  "- `figure4_dynega_factor_cor_caption.txt`",
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_vs_graphicalvar_scenario_a_summary.csv`" else character(0),
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_vs_graphicalvar_scenario_a_design.csv`" else character(0),
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_vs_graphicalvar_scenario_a_caption.txt`" else character(0),
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_gimme_graphicalvar_scenario_a_factor_cor_summary.csv`" else character(0),
  if (nrow(method_compare_summary) > 0L) "- `figure4_dynega_gimme_graphicalvar_scenario_a_factor_cor_caption.txt`" else character(0),
  if (nrow(method_compare_summary) > 0L) "- `figure4_scenario_a_method_comparison_table.{csv,tex}`" else character(0),
  "- `figure4_dynega_rr_caption.txt`"
)
writeLines(readme, file.path(out_dir, "README.md"))

cat("Saved DynEGA RR Figure 4 bundle to ", out_dir, "\n", sep = "")
