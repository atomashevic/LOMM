#!/usr/bin/env Rscript

library(dplyr)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

read_results <- function(path) {
  file <- file.path(path, "benchmark_results.csv")
  if (!file.exists(file)) {
    file <- file.path(path, "checkpoint_results.csv")
  }
  if (!file.exists(file)) {
    return(NULL)
  }
  utils::read.csv(file, stringsAsFactors = FALSE)
}

append_md_table <- function(df, file) {
  if (nrow(df) == 0L) {
    cat("_No rows._\n", file = file, append = TRUE)
    return(invisible(NULL))
  }

  cat(
    paste(names(df), collapse = " | "),
    "\n",
    sep = "",
    file = file,
    append = TRUE
  )
  cat(
    paste(rep("---", ncol(df)), collapse = " | "),
    "\n",
    sep = "",
    file = file,
    append = TRUE
  )
  for (row_idx in seq_len(nrow(df))) {
    cat(
      paste(as.character(df[row_idx, , drop = TRUE]), collapse = " | "),
      "\n",
      sep = "",
      file = file,
      append = TRUE
    )
  }
}

output_dir <- Sys.getenv(
  "FULL_SUITE_SUMMARY_OUT_DIR",
  file.path("simulation_outputs", "revision_summary")
)
input_dirs <- strsplit(
  Sys.getenv(
    "FULL_SUITE_INPUT_DIRS",
    paste(
      file.path("simulation_outputs", "revision_dynega"),
      file.path("simulation_outputs", "revision_graphicalvar"),
      file.path("simulation_outputs", "revision_gimme"),
      sep = ","
    )
  ),
  ",",
  fixed = TRUE
)[[1]]
input_dirs <- trimws(input_dirs)
input_dirs <- input_dirs[nzchar(input_dirs)]

results <- Filter(Negate(is.null), lapply(input_dirs, read_results))
if (length(results) == 0L) {
  stop("No benchmark_results.csv files found in FULL_SUITE_INPUT_DIRS.", call. = FALSE)
}

combined <- dplyr::bind_rows(results)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!"N" %in% names(combined)) combined$N <- NA_integer_
if (!"NVAR" %in% names(combined)) combined$NVAR <- NA_integer_
if (!"Error" %in% names(combined)) combined$Error <- combined$base_err
if (!"Q" %in% names(combined)) combined$Q <- NA_real_
if (!"Loadings" %in% names(combined)) combined$Loadings <- NA_real_
if (!"comparison_scope" %in% names(combined)) {
  combined$comparison_scope <- ifelse(combined$method_family == "gimme", "gimme_bounded", "main_full_grid")
}

combined$success <- combined$status == "ok"
combined$method_compare <- ifelse(
  combined$method_family == "dynEGA",
  paste0("DynEGA[", combined$community_algorithm, "|", combined$uni_method, "]"),
  combined$method_label
)
combined$fit_method_compare <- ifelse(
  combined$method_family == "gimme",
  "GIMME structure fit",
  ifelse(
    combined$method_family == "graphicalVAR",
    "GraphicalVAR structure fit",
    combined$method_compare
  )
)
combined$fit_key <- paste(
  combined$fit_method_compare,
  combined$scenario,
  combined$N,
  combined$TimeP,
  combined$NVAR,
  combined$Error,
  combined$Q,
  combined$Loadings,
  combined$rep,
  sep = "\r"
)
combined$status_timeout <- combined$status == "timeout"
combined$status_error <- !combined$success & !combined$status_timeout

fit_level <- combined %>%
  dplyr::group_by(
    fit_method_compare,
    method_family,
    scenario,
    scenario_family,
    scenario_mode,
    scenario_nfac,
    N,
    TimeP,
    NVAR,
    Error,
    Q,
    Loadings,
    rep,
    fit_key
  ) %>%
  dplyr::summarize(
    fit_status = dplyr::case_when(
      any(status == "ok") ~ "ok",
      any(status == "timeout") ~ "timeout",
      TRUE ~ "error"
    ),
    fit_runtime_sec = max(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  )

main_full_grid <- combined %>%
  dplyr::filter(
    method_family %in% c("dynEGA", "graphicalVAR"),
    comparison_scope == "main_full_grid"
  )

gimme_rows <- combined %>%
  dplyr::filter(method_family == "gimme", comparison_scope == "gimme_bounded")

summarize_rows_by_method <- function(df) {
  df %>%
    dplyr::group_by(method_compare, method_family, community_algorithm, uni_method) %>%
    dplyr::summarize(
      n_conditions = dplyr::n(),
      n_ok = sum(success, na.rm = TRUE),
      n_timeout = sum(status_timeout, na.rm = TRUE),
      n_error = sum(status_error, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      exact_rate_all = mean(ifelse(success, n_dim_exact, 0), na.rm = TRUE),
      exact_rate_ok = mean(n_dim_exact[success], na.rm = TRUE),
      cor_mean = mean(score_cor_common, na.rm = TRUE),
      nrmse_mean = mean(score_nrmse_common, na.rm = TRUE),
      assignment_mean = mean(assignment_accuracy, na.rm = TRUE),
      runtime_mean = mean(runtime_sec, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_rows_by_mode_n <- function(df) {
  df %>%
    dplyr::group_by(method_compare, scenario_mode, scenario_nfac) %>%
    dplyr::summarize(
      n_conditions = dplyr::n(),
      n_ok = sum(success, na.rm = TRUE),
      n_timeout = sum(status_timeout, na.rm = TRUE),
      n_error = sum(status_error, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      exact_rate_all = mean(ifelse(success, n_dim_exact, 0), na.rm = TRUE),
      cor_mean = mean(score_cor_common, na.rm = TRUE),
      nrmse_mean = mean(score_nrmse_common, na.rm = TRUE),
      runtime_mean = mean(runtime_sec, na.rm = TRUE),
      .groups = "drop"
    )
}

summary_by_method <- summarize_rows_by_method(combined)
main_summary_by_method <- summarize_rows_by_method(main_full_grid)

gimme_feasibility_by_method <- gimme_rows %>%
  dplyr::group_by(method_compare, method_family, community_algorithm, uni_method) %>%
  dplyr::summarize(
    n_conditions = dplyr::n(),
    n_ok = sum(success, na.rm = TRUE),
    n_timeout = sum(status_timeout, na.rm = TRUE),
    n_error = sum(status_error, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    exact_rate_all = mean(ifelse(success, n_dim_exact, 0), na.rm = TRUE),
    exact_rate_ok = mean(n_dim_exact[success], na.rm = TRUE),
    cor_mean = mean(score_cor_common, na.rm = TRUE),
    nrmse_mean = mean(score_nrmse_common, na.rm = TRUE),
    assignment_mean = mean(assignment_accuracy, na.rm = TRUE),
    runtime_mean = mean(runtime_sec, na.rm = TRUE),
    .groups = "drop"
  )

summary_by_family_fit <- fit_level %>%
  dplyr::group_by(fit_method_compare, method_family) %>%
  dplyr::summarize(
    n_fits = dplyr::n(),
    n_ok_fits = sum(fit_status == "ok", na.rm = TRUE),
    n_timeout_fits = sum(fit_status == "timeout", na.rm = TRUE),
    n_error_fits = sum(fit_status == "error", na.rm = TRUE),
    fit_success_rate = mean(fit_status == "ok", na.rm = TRUE),
    fit_runtime_mean = mean(fit_runtime_sec, na.rm = TRUE),
    fit_runtime_median = stats::median(fit_runtime_sec, na.rm = TRUE),
    fit_runtime_max = max(fit_runtime_sec, na.rm = TRUE),
    .groups = "drop"
  )

main_summary_by_family_fit <- summary_by_family_fit %>%
  dplyr::filter(method_family %in% c("dynEGA", "graphicalVAR"))

gimme_feasibility_by_fit <- summary_by_family_fit %>%
  dplyr::filter(method_family == "gimme")

gimme_feasibility_by_scenario <- fit_level %>%
  dplyr::filter(method_family == "gimme") %>%
  dplyr::group_by(scenario, scenario_mode, scenario_nfac, N, TimeP, NVAR, Error, Q, Loadings) %>%
  dplyr::summarize(
    n_fits = dplyr::n(),
    n_ok_fits = sum(fit_status == "ok", na.rm = TRUE),
    n_timeout_fits = sum(fit_status == "timeout", na.rm = TRUE),
    n_error_fits = sum(fit_status == "error", na.rm = TRUE),
    fit_success_rate = mean(fit_status == "ok", na.rm = TRUE),
    fit_runtime_mean = mean(fit_runtime_sec, na.rm = TRUE),
    fit_runtime_median = stats::median(fit_runtime_sec, na.rm = TRUE),
    fit_runtime_max = max(fit_runtime_sec, na.rm = TRUE),
    .groups = "drop"
  )

summary_by_mode_n <- summarize_rows_by_mode_n(combined)
main_summary_by_mode_n <- summarize_rows_by_mode_n(main_full_grid)

probe_summary_path <- Sys.getenv("GIMME_PROBE_SUMMARY", "gimme_configuration_probe_summary.csv")
gimme_probe_summary <- if (file.exists(probe_summary_path)) {
  utils::read.csv(probe_summary_path, stringsAsFactors = FALSE)
} else {
  data.frame()
}

utils::write.csv(combined, file.path(output_dir, "full_suite_combined_results.csv"), row.names = FALSE)
utils::write.csv(summary_by_method, file.path(output_dir, "full_suite_summary_by_method.csv"), row.names = FALSE)
utils::write.csv(summary_by_family_fit, file.path(output_dir, "full_suite_summary_by_family_fit.csv"), row.names = FALSE)
utils::write.csv(summary_by_mode_n, file.path(output_dir, "full_suite_summary_by_mode_n.csv"), row.names = FALSE)
utils::write.csv(main_summary_by_method, file.path(output_dir, "main_full_grid_summary_by_method.csv"), row.names = FALSE)
utils::write.csv(main_summary_by_family_fit, file.path(output_dir, "main_full_grid_summary_by_fit.csv"), row.names = FALSE)
utils::write.csv(main_summary_by_mode_n, file.path(output_dir, "main_full_grid_summary_by_mode_n.csv"), row.names = FALSE)
utils::write.csv(gimme_feasibility_by_method, file.path(output_dir, "gimme_feasibility_by_method.csv"), row.names = FALSE)
utils::write.csv(gimme_feasibility_by_fit, file.path(output_dir, "gimme_feasibility_by_fit.csv"), row.names = FALSE)
utils::write.csv(gimme_feasibility_by_scenario, file.path(output_dir, "gimme_feasibility_by_scenario.csv"), row.names = FALSE)
if (nrow(gimme_probe_summary) > 0L) {
  utils::write.csv(gimme_probe_summary, file.path(output_dir, "gimme_configuration_probe_summary.csv"), row.names = FALSE)
}

cat("# Benchmark Reporting Policy Summary\n\n", file = file.path(output_dir, "full_suite_summary.md"))
cat("Input directories: ", paste(input_dirs, collapse = ", "), "\n\n", sep = "", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("Rows: ", nrow(combined), "\n\n", sep = "", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("Policy: DynEGA and GraphicalVAR are the main full-grid analysis. GIMME is reported only as a bounded sensitivity and feasibility comparator with explicit timeout accounting.\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("## Main Full-Grid Analysis\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("This section excludes GIMME by policy.\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("### By Method\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
append_md_table(main_summary_by_method, file.path(output_dir, "full_suite_summary.md"))
cat("\n### By Method Fit\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
append_md_table(main_summary_by_family_fit, file.path(output_dir, "full_suite_summary.md"))
cat("\n### By Mode And n\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
append_md_table(main_summary_by_mode_n, file.path(output_dir, "full_suite_summary.md"))
cat("\n## GIMME Feasibility And Sensitivity\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("GIMME rows share one structure fit across community variants; fit-level tables avoid counting the same expensive fit as independent variant runs.\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("### Row-Level Status By Variant\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
append_md_table(gimme_feasibility_by_method, file.path(output_dir, "full_suite_summary.md"))
cat("\n### Fit-Level Status\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
append_md_table(gimme_feasibility_by_fit, file.path(output_dir, "full_suite_summary.md"))
cat("\n### Fit-Level Status By Scenario\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
append_md_table(gimme_feasibility_by_scenario, file.path(output_dir, "full_suite_summary.md"))
if (nrow(gimme_probe_summary) > 0L) {
  cat("\n### Configuration Probes\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
  append_md_table(gimme_probe_summary, file.path(output_dir, "full_suite_summary.md"))
}
cat("\n## Legacy Combined Tables\n\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)
cat("The CSV files `full_suite_summary_by_method.csv`, `full_suite_summary_by_family_fit.csv`, and `full_suite_summary_by_mode_n.csv` are retained for audit/backward compatibility and include all methods.\n", file = file.path(output_dir, "full_suite_summary.md"), append = TRUE)

cat("Saved full-suite summaries to ", output_dir, "\n", sep = "")
