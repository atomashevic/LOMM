#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

repo_root <- normalizePath(".", mustWork = TRUE)
out_dir <- file.path(
  repo_root,
  "simulation_outputs/final_simulation_results/comparison_to_current_manuscript"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_results <- function(path, allow_checkpoint = FALSE) {
  if (file.exists(path)) {
    return(read_csv(path, show_col_types = FALSE, progress = FALSE))
  }
  if (isTRUE(allow_checkpoint)) {
    checkpoint <- file.path(dirname(path), "checkpoint_results.csv")
    if (file.exists(checkpoint)) {
      return(read_csv(checkpoint, show_col_types = FALSE, progress = FALSE))
    }
  }
  stop("Missing required result file: ", path, call. = FALSE)
}

bounded_nvar4_filter <- function(x) {
  x %>%
    filter(
      scenario_nfac %in% c(2, 3),
      TimeP == 100,
      NVAR == 4,
      N %in% c(50, 100),
      Error %in% c(0.125, 0.25, 0.5),
      Loadings %in% c(0.4, 0.6, 0.8)
    )
}

scenario_key_cols <- c(
  "scenario", "scenario_family", "scenario_nfac", "N", "TimeP", "NVAR",
  "Error", "Loadings", "Q", "base_err", "rep"
)

normalise_for_cd <- function(x, method_name) {
  if (!"uni_method" %in% names(x)) {
    x$uni_method <- NA_character_
  }
  x %>%
    mutate(
      Method = method_name,
      CD = str_to_title(as.character(community_algorithm)),
      uni_method = if_else(is.na(uni_method), NA_character_, as.character(uni_method))
    )
}

summarise_rows <- function(x, grouping = character()) {
  x %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      structural_cells = n_distinct(
        paste(scenario, N, TimeP, NVAR, Error, Loadings, Q, base_err, sep = "|")
      ),
      rows = n(),
      ok = sum(status == "ok", na.rm = TRUE),
      timeout = sum(status == "timeout", na.rm = TRUE),
      error = sum(status == "error", na.rm = TRUE),
      hit_all_pct = 100 * sum(n_dim_exact == 1, na.rm = TRUE) / n(),
      hit_ok_pct = if_else(
        ok > 0,
        100 * sum(n_dim_exact == 1 & status == "ok", na.rm = TRUE) / ok,
        NA_real_
      ),
      assignment_available_pct = 100 * mean(assignment_accuracy, na.rm = TRUE),
      score_r_available = mean(score_cor_common, na.rm = TRUE),
      score_nrmse_available = mean(score_nrmse_common, na.rm = TRUE),
      runtime_ok_median_s = median(runtime_sec[status == "ok"], na.rm = TRUE),
      runtime_all_median_s = median(runtime_sec, na.rm = TRUE),
      .groups = "drop"
    )
}

dynega_full <- read_results(
  "simulation_outputs/final_simulation_results/dynega_full/benchmark_results.csv"
)
dynega_cd <- read_results(
  "simulation_outputs/final_simulation_results/dynega_bounded_nvar4_cd/benchmark_results.csv",
  allow_checkpoint = TRUE
)
graphicalvar <- read_results(
  "simulation_outputs/final_simulation_results/graphicalvar_bounded_nvar4/benchmark_results.csv"
)
gimme <- read_results(
  "simulation_outputs/final_simulation_results/gimme_bounded_nvar4/benchmark_results.csv"
)

dynega_louvain_bounded <- dynega_full %>%
  bounded_nvar4_filter() %>%
  filter(community_algorithm == "louvain", uni_method == "expand", rep <= 100)

dynega_cd_bounded <- dynega_cd %>%
  bounded_nvar4_filter() %>%
  filter(community_algorithm %in% c("leiden", "walktrap"), uni_method == "expand")

final_cd_rows <- bind_rows(
  normalise_for_cd(dynega_louvain_bounded, "DynEGA"),
  normalise_for_cd(dynega_cd_bounded, "DynEGA"),
  normalise_for_cd(bounded_nvar4_filter(graphicalvar), "GraphicalVAR"),
  normalise_for_cd(bounded_nvar4_filter(gimme), "GIMME")
) %>%
  mutate(
    Method = factor(Method, levels = c("DynEGA", "GraphicalVAR", "GIMME")),
    CD = factor(CD, levels = c("Louvain", "Leiden", "Walktrap"))
  )

full_dynega_louvain <- dynega_full %>%
  filter(community_algorithm == "louvain", uni_method == "expand")

final_full_dynega_summary <- summarise_rows(full_dynega_louvain) %>%
  mutate(Method = "DynEGA", Scope = "Full final DynEGA Louvain/expand grid") %>%
  select(Method, Scope, everything())

final_cd_summary <- final_cd_rows %>%
  summarise_rows(c("Method", "CD")) %>%
  arrange(Method, CD)

final_method_summary <- final_cd_rows %>%
  summarise_rows("Method") %>%
  arrange(Method)

final_cd_by_scenario <- final_cd_rows %>%
  summarise_rows(c("Method", "CD", "scenario_family", "scenario_nfac")) %>%
  arrange(Method, CD, scenario_family, scenario_nfac)

final_cd_by_measurement <- final_cd_rows %>%
  summarise_rows(c("Method", "CD", "Error", "Loadings")) %>%
  arrange(Method, CD, Loadings, Error)

summarise_conditional_scores <- function(data, grouping) {
  data %>%
    mutate(correct = status == "ok" & n_dim_exact == 1) %>%
    group_by(across(all_of(c(grouping, "Error", "Loadings")))) %>%
    summarise(
      stratum_total_rows = n(),
      stratum_correct_rows = sum(correct, na.rm = TRUE),
      mean_r = mean(score_cor_common[correct], na.rm = TRUE),
      ci_width = {
        vals <- score_cor_common[correct]
        vals <- vals[is.finite(vals)]
        if (length(vals) <= 3L) {
          NA_real_
        } else {
          2 * 1.96 / sqrt(length(vals) - 3)
        }
      },
      .groups = "drop"
    ) %>%
    group_by(across(all_of(grouping))) %>%
    summarise(
      correct_rows = sum(stratum_correct_rows, na.rm = TRUE),
      total_rows = sum(stratum_total_rows, na.rm = TRUE),
      hit_rate = correct_rows / total_rows,
      mean_conditional_r = {
        vals <- mean_r[is.finite(mean_r)]
        weights <- stratum_correct_rows[is.finite(mean_r)]
        if (length(vals) == 0L || sum(weights) == 0) {
          NA_real_
        } else {
          tanh(weighted.mean(atanh(pmin(pmax(vals, -0.999999), 0.999999)), weights))
        }
      },
      min_conditional_r = suppressWarnings(min(mean_r, na.rm = TRUE)),
      max_conditional_r = suppressWarnings(max(mean_r, na.rm = TRUE)),
      min_correct_rows_per_stratum = suppressWarnings(min(stratum_correct_rows[stratum_correct_rows > 0], na.rm = TRUE)),
      max_correct_rows_per_stratum = suppressWarnings(max(stratum_correct_rows, na.rm = TRUE)),
      widest_ci = suppressWarnings(max(ci_width, na.rm = TRUE)),
      missing_strata = sum(!is.finite(mean_r)),
      .groups = "drop"
    ) %>%
    mutate(
      min_conditional_r = if_else(is.infinite(min_conditional_r), NA_real_, min_conditional_r),
      max_conditional_r = if_else(is.infinite(max_conditional_r), NA_real_, max_conditional_r),
      min_correct_rows_per_stratum = if_else(
        is.infinite(min_correct_rows_per_stratum),
        NA_real_,
        min_correct_rows_per_stratum
      ),
      max_correct_rows_per_stratum = if_else(
        is.infinite(max_correct_rows_per_stratum),
        NA_real_,
        max_correct_rows_per_stratum
      ),
      widest_ci = if_else(is.infinite(widest_ci), NA_real_, widest_ci)
    )
}

conditional_score_summary <- summarise_conditional_scores(final_cd_rows, c("Method", "CD")) %>%
  arrange(Method, CD)

conditional_method_summary <- summarise_conditional_scores(final_cd_rows, "Method") %>%
  arrange(Method)

structure_fit_status <- bind_rows(
  full_dynega_louvain %>%
    distinct(across(all_of(scenario_key_cols)), status) %>%
    mutate(Method = "DynEGA", Scope = "Full DynEGA grid"),
  dynega_louvain_bounded %>%
    distinct(across(all_of(scenario_key_cols)), status) %>%
    mutate(Method = "DynEGA", Scope = "Bounded NVAR=4 comparator"),
  graphicalvar %>%
    bounded_nvar4_filter() %>%
    distinct(across(all_of(scenario_key_cols)), status) %>%
    mutate(Method = "GraphicalVAR", Scope = "Bounded NVAR=4 comparator"),
  gimme %>%
    bounded_nvar4_filter() %>%
    distinct(across(all_of(scenario_key_cols)), status) %>%
    mutate(Method = "GIMME", Scope = "Bounded NVAR=4 comparator")
) %>%
  group_by(Method, Scope) %>%
  summarise(
    structure_fits = n(),
    ok = sum(status == "ok", na.rm = TRUE),
    timeout = sum(status == "timeout", na.rm = TRUE),
    error = sum(status == "error", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Method, Scope)

fmt_pct <- function(x) ifelse(is.na(x), NA_character_, sprintf("%.1f", x))
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}
fmt_runtime <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x < 10, sprintf("%.2f", x), sprintf("%.1f", x)))
}

paper_table1 <- final_method_summary %>%
  transmute(
    Method = as.character(Method),
    Cells = structural_cells,
    Rows = rows,
    ok,
    timeout,
    error,
    `Hit (%)` = fmt_pct(hit_all_pct),
    `Assign. (%)` = fmt_pct(assignment_available_pct),
    `Score r` = fmt_num(score_r_available, 3),
    `Score NRMSE` = fmt_num(score_nrmse_available, 3),
    `Runtime (s)` = fmt_runtime(runtime_ok_median_s)
  )

paper_table2 <- conditional_method_summary %>%
  transmute(
    Method = as.character(Method),
    Correct = paste0(correct_rows, "/", total_rows),
    `Hit (%)` = scales::percent(hit_rate, accuracy = 0.1),
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

write_csv(final_full_dynega_summary, file.path(out_dir, "final_full_dynega_summary.csv"))
write_csv(final_method_summary, file.path(out_dir, "final_method_summary.csv"))
write_csv(final_cd_summary, file.path(out_dir, "final_cd_summary.csv"))
write_csv(final_cd_by_scenario, file.path(out_dir, "final_cd_by_scenario.csv"))
write_csv(final_cd_by_measurement, file.path(out_dir, "final_cd_by_measurement.csv"))
write_csv(conditional_score_summary, file.path(out_dir, "final_cd_conditional_score_summary.csv"))
write_csv(conditional_method_summary, file.path(out_dir, "final_method_conditional_score_summary.csv"))
write_csv(structure_fit_status, file.path(out_dir, "final_structure_fit_status_accounting.csv"))
write_csv(paper_table1, file.path(repo_root, "paper", "tables", "table1_method_overall.csv"))
write_csv(paper_table2, file.path(repo_root, "paper", "tables", "table2_conditional_score_correlations.csv"))
saveRDS(final_cd_rows, file.path(out_dir, "final_cd_rows.rds"))

report_path <- file.path(out_dir, "final_vs_current_manuscript_report.md")
report <- c(
  "# Final Simulation Results Versus Current Manuscript",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Source Files",
  "",
  "- Final DynEGA full Louvain grid: `simulation_outputs/final_simulation_results/dynega_full/benchmark_results.csv`.",
  "- Final DynEGA bounded CD variants: `simulation_outputs/final_simulation_results/dynega_bounded_nvar4_cd/benchmark_results.csv`.",
  "- Final GraphicalVAR bounded CD rows: `simulation_outputs/final_simulation_results/graphicalvar_bounded_nvar4/benchmark_results.csv`.",
  "- Final GIMME bounded CD rows: `simulation_outputs/final_simulation_results/gimme_bounded_nvar4/benchmark_results.csv`.",
  "",
  "## Final Scope",
  "",
  "- Full DynEGA scalability evidence remains the completed Louvain/expand grid.",
  "- The final all-method comparator is bounded to NVAR = 4 and treats community detection as a factor.",
  "- Comparator rows combine DynEGA Louvain from the full run restricted to `rep <= 100`, DynEGA bounded Leiden/Walktrap variants, and existing GraphicalVAR/GIMME Louvain/Leiden/Walktrap rows.",
  "",
  "## Full DynEGA Summary",
  "",
  paste(capture.output(print(final_full_dynega_summary, n = Inf, width = Inf)), collapse = "\n"),
  "",
  "## Final Method x CD Summary",
  "",
  paste(capture.output(print(final_cd_summary, n = Inf, width = Inf)), collapse = "\n"),
  "",
  "## Structure-Fit Status Accounting",
  "",
  paste(capture.output(print(structure_fit_status, n = Inf, width = Inf)), collapse = "\n"),
  "",
  "## Scenario-Level Summary",
  "",
  paste(capture.output(print(final_cd_by_scenario, n = Inf, width = Inf)), collapse = "\n"),
  "",
  "## Interpretation Changes Needed",
  "",
  "1. Replace old 486-row matched-subset claims with the final bounded NVAR = 4 method x CD comparator.",
  "2. Describe community detection as an analyzed factor, not as a sensitivity-only readout.",
  "3. Keep the full DynEGA grid as separate scalability evidence because it is Louvain/expand only.",
  "4. Report GraphicalVAR and GIMME feasibility using unique structure-fit counts as well as CD readout rows.",
  "5. Keep score recovery conditional on correct dimensional recovery and show denominators for each method x CD cell."
)
writeLines(report, report_path)

cat("Wrote final CD comparison outputs to ", out_dir, "\n", sep = "")
