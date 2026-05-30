#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

repo_root <- normalizePath(".", mustWork = TRUE)

source(file.path(repo_root, "extract.r"))

read_char_env <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  value
}

format_num <- function(x, digits = 2L) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

escape_latex <- function(x) {
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("_", "\\\\_", x)
  x
}

ppc_call <- function(max_discrepancy) {
  dplyr::case_when(
    max_discrepancy <= 0.33 ~ "Pass",
    max_discrepancy <= 1.00 ~ "Partial",
    TRUE ~ "Fail"
  )
}

collapse_sizes <- function(x) {
  paste(as.integer(x), collapse = "-")
}

metric_labels <- c(
  sample_entropy = "Sample entropy",
  permutation_entropy = "Permutation entropy",
  spectral_entropy = "Spectral entropy",
  run_length_entropy = "Run-length entropy",
  lag1_acf = "Lag-1 ACF",
  channel_iqr = "Channel IQR",
  std_value = "Standard deviation",
  total_variation = "Total variation",
  diff_iqr = "Derivative IQR",
  mean_abs_pairwise_cor = "Mean absolute pairwise correlation",
  zero_crossing_rate = "Zero-crossing rate",
  spectral_centroid = "Spectral centroid",
  effective_rank = "Effective rank"
)

input_dir <- read_char_env(
  "OAI_VALIDATION_INPUT_DIR",
  file.path(repo_root, "simulation_outputs", "empirical_mafw_retained100", "oai-large-100")
)
out_dir <- read_char_env(
  "OAI_VALIDATION_OUT_DIR",
  file.path(repo_root, "simulation_outputs", "empirical_realism")
)
compare_mode <- read_char_env("OAI_VALIDATION_COMPARE_MODE", "heterogeneous")
algorithms <- strsplit(read_char_env("OAI_VALIDATION_ALGORITHMS", "louvain,walktrap,leiden"), ",", fixed = TRUE)[[1]]
algorithms <- trimws(algorithms)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

empirical_panel <- utils::read.csv(file.path(input_dir, "empirical_panel_clean.csv"), stringsAsFactors = FALSE)
synthetic_panel <- utils::read.csv(file.path(input_dir, paste0(compare_mode, "_synthetic_panel.csv")), stringsAsFactors = FALSE)
score_cols <- EMPIRICAL_EMOTION_COLUMNS

raw_emp <- metric_table_from_panel(empirical_panel, series_cols = score_cols, family = "raw", dt = 1)
raw_syn <- metric_table_from_panel(synthetic_panel, series_cols = score_cols, family = "raw", dt = 1)
raw_comparison <- metric_loss_table(raw_emp, raw_syn, family = "raw")
raw_comparison$algorithm <- "raw"
raw_comparison$empirical_n_dim <- NA_integer_
raw_comparison$synthetic_n_dim <- NA_integer_
raw_comparison$empirical_community_sizes <- NA_character_
raw_comparison$synthetic_community_sizes <- NA_character_

latent_comparisons <- list()
summary_rows <- list()
failed_rows <- list()

for (algorithm in algorithms) {
  target_pair <- tryCatch(
    list(
      empirical = build_property_target_from_panel(
        empirical_panel,
        model_name = "OpenAI-large",
        score_cols = score_cols,
        ord0_embed = 10L,
        algorithm = algorithm
      ),
      synthetic = build_property_target_from_panel(
        synthetic_panel,
        model_name = paste0(compare_mode, " DLO"),
        score_cols = score_cols,
        ord0_embed = 10L,
        algorithm = algorithm
      )
    ),
    error = function(err) {
      failed_rows[[algorithm]] <<- data.frame(
        algorithm = algorithm,
        error = conditionMessage(err),
        stringsAsFactors = FALSE
      )
      warning(
        sprintf("Skipping %s validation readout: %s", algorithm, conditionMessage(err)),
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(target_pair)) {
    next
  }

  empirical_target <- target_pair$empirical
  synthetic_target <- target_pair$synthetic

  comparison <- metric_loss_table(
    empirical_target$latent_metrics,
    synthetic_target$latent_metrics,
    family = "latent"
  )
  comparison$algorithm <- algorithm
  comparison$empirical_n_dim <- empirical_target$n_dim
  comparison$synthetic_n_dim <- synthetic_target$n_dim
  comparison$empirical_community_sizes <- collapse_sizes(empirical_target$community_sizes)
  comparison$synthetic_community_sizes <- collapse_sizes(synthetic_target$community_sizes)
  latent_comparisons[[algorithm]] <- comparison

  validation_metrics <- c(
    "sample_entropy",
    "permutation_entropy",
    "spectral_entropy",
    "run_length_entropy",
    "lag1_acf",
    "std_value",
    "total_variation",
    "diff_iqr",
    "zero_crossing_rate"
  )
  validation_subset <- comparison[comparison$metric %in% validation_metrics, , drop = FALSE]
  max_discrepancy <- pmax(validation_subset$median_diff_scaled, validation_subset$wasserstein_scaled)
  calls <- ppc_call(max_discrepancy)

  summary_rows[[algorithm]] <- data.frame(
    algorithm = algorithm,
    empirical_n_dim = empirical_target$n_dim,
    empirical_sizes = collapse_sizes(empirical_target$community_sizes),
    synthetic_n_dim = synthetic_target$n_dim,
    synthetic_sizes = collapse_sizes(synthetic_target$community_sizes),
    mean_max_discrepancy = mean(max_discrepancy, na.rm = TRUE),
    pass = sum(calls == "Pass", na.rm = TRUE),
    partial = sum(calls == "Partial", na.rm = TRUE),
    fail = sum(calls == "Fail", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

comparison_all <- dplyr::bind_rows(raw_comparison, dplyr::bind_rows(latent_comparisons)) |>
  dplyr::mutate(max_discrepancy = pmax(median_diff_scaled, wasserstein_scaled))

utils::write.csv(
  comparison_all,
  file.path(out_dir, "oai_validation_sensitivity_metrics_v2.csv"),
  row.names = FALSE
)

cd_summary <- dplyr::bind_rows(summary_rows) |>
  dplyr::arrange(mean_max_discrepancy)

utils::write.csv(
  cd_summary,
  file.path(out_dir, "oai_cd_sensitivity_summary_v2.csv"),
  row.names = FALSE
)

failed_summary <- dplyr::bind_rows(failed_rows)
utils::write.csv(
  failed_summary,
  file.path(out_dir, "oai_cd_sensitivity_failures_v2.csv"),
  row.names = FALSE
)

entropy_metrics <- c("sample_entropy", "permutation_entropy", "spectral_entropy", "run_length_entropy")
raw_entropy <- comparison_all |>
  dplyr::filter(algorithm == "raw", family == "raw", metric %in% entropy_metrics) |>
  dplyr::mutate(Readout = "Raw", Algorithm = "--")

latent_entropy <- comparison_all |>
  dplyr::filter(algorithm %in% algorithms, family == "latent", metric %in% entropy_metrics) |>
  dplyr::mutate(Readout = "Latent", Algorithm = algorithm)

entropy_table_df <- dplyr::bind_rows(raw_entropy, latent_entropy) |>
  dplyr::mutate(
    Metric = metric_labels[metric],
    `OpenAI-large` = format_num(empirical_median, 3),
    `DLO simulation` = format_num(synthetic_median, 3),
    `Max discrepancy` = format_num(max_discrepancy, 2),
    Call = ppc_call(max_discrepancy),
    metric_order = factor(metric, levels = entropy_metrics),
    readout_order = factor(Readout, levels = c("Raw", "Latent"))
  ) |>
  dplyr::arrange(readout_order, Algorithm, metric_order) |>
  dplyr::select(Readout, Algorithm, Metric, `OpenAI-large`, `DLO simulation`, `Max discrepancy`, Call)

entropy_table <- c(
  "\\begin{tabular}{lllrrrl}",
  "\\toprule",
  "Readout & CD & Metric & OpenAI-large & DLO sim. & Max disc. & Call \\\\",
  "\\midrule",
  apply(entropy_table_df, 1, function(row) {
    paste(escape_latex(unname(row)), collapse = " & ") |> paste0(" \\\\")
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(entropy_table, file.path(out_dir, "oai_entropy_sensitivity_table_v2.tex"))

report_metrics <- c("lag1_acf", "diff_iqr", "run_length_entropy", "spectral_entropy", "effective_rank")
report_table_df <- comparison_all |>
  dplyr::filter(
    metric %in% report_metrics,
    (family == "raw" & algorithm == "raw") |
      (family == "latent" & algorithm == "louvain")
  ) |>
  dplyr::mutate(
    Readout = dplyr::if_else(family == "raw", "Raw", "Latent"),
    Metric = metric_labels[metric],
    `OpenAI-large` = format_num(empirical_median, 3),
    `DLO simulation` = format_num(synthetic_median, 3),
    `Max discrepancy` = format_num(max_discrepancy, 2),
    Call = ppc_call(max_discrepancy),
    readout_order = factor(Readout, levels = c("Raw", "Latent")),
    metric_order = factor(metric, levels = report_metrics)
  ) |>
  dplyr::arrange(readout_order, metric_order) |>
  dplyr::select(Readout, Metric, `OpenAI-large`, `DLO simulation`, `Max discrepancy`, Call)

report_table <- c(
  "\\begin{tabular}{llrrrl}",
  "\\toprule",
  "Readout & Metric & OpenAI-large & DLO sim. & Max disc. & Call \\\\",
  "\\midrule",
  apply(report_table_df, 1, function(row) {
    paste(escape_latex(unname(row)), collapse = " & ") |> paste0(" \\\\")
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(report_table, file.path(out_dir, "oai_louvain_validation_table_v2.tex"))

basic_comparison_metrics <- c(
  "channel_iqr",
  "mean_abs_pairwise_cor",
  "diff_iqr",
  "lag1_acf",
  "run_length_entropy",
  "spectral_entropy",
  "effective_rank"
)

basic_comparison_df <- comparison_all |>
  dplyr::filter(
    metric %in% basic_comparison_metrics,
    (family == "raw" & algorithm == "raw") |
      (family == "latent" & algorithm == "louvain")
  ) |>
  dplyr::mutate(
    Readout = dplyr::if_else(family == "raw", "Raw", "Latent"),
    Metric = metric_labels[metric],
    `OpenAI-large` = format_num(empirical_median, 3),
    `DLO simulation` = format_num(synthetic_median, 3),
    `Max discrepancy` = format_num(max_discrepancy, 2),
    Call = ppc_call(max_discrepancy),
    metric_order = factor(metric, levels = basic_comparison_metrics),
    readout_order = factor(Readout, levels = c("Raw", "Latent"))
  ) |>
  dplyr::arrange(readout_order, metric_order) |>
  dplyr::select(
    Readout,
    metric,
    Metric,
    empirical_median,
    synthetic_median,
    empirical_iqr,
    max_discrepancy,
    `OpenAI-large`,
    `DLO simulation`,
    `Max discrepancy`,
    Call
  )

utils::write.csv(
  basic_comparison_df,
  file.path(out_dir, "oai_basic_metric_comparison_v2.csv"),
  row.names = FALSE
)

basic_table_df <- basic_comparison_df |>
  dplyr::select(Readout, Metric, `OpenAI-large`, `DLO simulation`, `Max discrepancy`, Call)

basic_table <- c(
  "\\begin{tabular}{llrrrl}",
  "\\toprule",
  "Readout & Metric & OpenAI-large & DLO sim. & Max disc. & Call \\\\",
  "\\midrule",
  apply(basic_table_df, 1, function(row) {
    paste(escape_latex(unname(row)), collapse = " & ") |> paste0(" \\\\")
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(basic_table, file.path(out_dir, "oai_basic_metric_comparison_v2.tex"))

cd_table_df <- cd_summary |>
  dplyr::mutate(
    Algorithm = algorithm,
    `Emp. dims` = paste0(empirical_n_dim, " (", empirical_sizes, ")"),
    `DLO dims` = paste0(synthetic_n_dim, " (", synthetic_sizes, ")"),
    `Mean max disc.` = format_num(mean_max_discrepancy, 2),
    Calls = paste0(pass, " / ", partial, " / ", fail)
  ) |>
  dplyr::select(Algorithm, `Emp. dims`, `DLO dims`, `Mean max disc.`, Calls)

cd_table <- c(
  "\\begin{tabular}{lllll}",
  "\\toprule",
  "CD & Empirical dims & DLO dims & Mean max disc. & Pass / partial / fail \\\\",
  "\\midrule",
  apply(cd_table_df, 1, function(row) {
    paste(escape_latex(unname(row)), collapse = " & ") |> paste0(" \\\\")
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(cd_table, file.path(out_dir, "oai_cd_sensitivity_table_v2.tex"))

message("Saved OAI validation sensitivity assets to ", out_dir)
