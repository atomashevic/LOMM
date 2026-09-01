#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || args[[1L]] != "--aggregate-dir") {
  stop(
    "Usage: test_compositional_profile_artifacts.R --aggregate-dir DIR",
    call. = FALSE
  )
}

test_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)),
  mustWork = TRUE
)
repo_root <- normalizePath(file.path(dirname(test_path), "..", ".."), mustWork = TRUE)
builder <- file.path(repo_root, "scripts", "figures", "build_compositional_profile_artifacts.R")
source(builder, local = .GlobalEnv)
run_artifact_build(args)

table_path <- file.path(repo_root, "paper", "tables", "compositional_profile_discrepancy.csv")
all_views_table_path <- file.path(
  repo_root, "paper", "tables", "compositional_profile_discrepancy_all_views.csv"
)
selected_metric_table_path <- file.path(
  repo_root, "paper", "tables", "compositional_profile_selected_metrics.csv"
)
caption_path <- file.path(
  repo_root, "paper", "tables", "compositional_profile_discrepancy_caption.txt"
)
all_views_caption_path <- file.path(
  repo_root, "paper", "tables",
  "compositional_profile_discrepancy_all_views_caption.txt"
)
selected_metric_caption_path <- file.path(
  repo_root, "paper", "tables",
  "compositional_profile_selected_metrics_caption.txt"
)
figure_paths <- file.path(
  repo_root, "paper", "figures",
  c(
    "compositional_profile_discrepancy.pdf",
    "compositional_profile_discrepancy.png",
    "compositional_profile_discrepancy_all_representations.pdf",
    "compositional_profile_discrepancy_all_representations.png"
  )
)
table <- utils::read.csv(table_path, stringsAsFactors = FALSE, check.names = FALSE)
all_views_table <- utils::read.csv(
  all_views_table_path, stringsAsFactors = FALSE, check.names = FALSE
)
selected_metric_table <- utils::read.csv(
  selected_metric_table_path, stringsAsFactors = FALSE, check.names = FALSE
)
caption <- paste(readLines(caption_path, warn = FALSE), collapse = " ")
all_views_caption <- paste(
  readLines(all_views_caption_path, warn = FALSE), collapse = " "
)
selected_metric_caption <- paste(
  readLines(selected_metric_caption_path, warn = FALSE), collapse = " "
)
forbidden <- paste(
  "lomm[_ -]?[0-9]|revision|config_hash|profile_id|analysis_view|",
  "complete_case|ilr_gap2",
  sep = ""
)
displayed <- c(
  names(table), unlist(table[vapply(table, is.character, logical(1))]),
  names(all_views_table),
  unlist(all_views_table[vapply(all_views_table, is.character, logical(1))]),
  names(selected_metric_table),
  unlist(selected_metric_table[
    vapply(selected_metric_table, is.character, logical(1))
  ]),
  caption, all_views_caption, selected_metric_caption
)

if (nrow(table) != 16L || length(unique(table$Configuration)) != 8L ||
    length(unique(table[["Representation"]])) != 2L) {
  stop("The manuscript-facing configuration table is incomplete.", call. = FALSE)
}
if (nrow(all_views_table) != 32L ||
    length(unique(all_views_table[["Analysis"]])) != 2L ||
    length(unique(all_views_table$Configuration)) != 8L ||
    length(unique(all_views_table[["Representation"]])) != 2L) {
  stop("The all-view manuscript-facing configuration table is incomplete.", call. = FALSE)
}
if (nrow(selected_metric_table) != 32L ||
    length(unique(selected_metric_table[["Analysis"]])) != 2L ||
    length(unique(selected_metric_table$Metric)) != 8L ||
    length(unique(selected_metric_table[["Representation"]])) != 2L) {
  stop("The selected-configuration statistic table is incomplete.", call. = FALSE)
}
if (any(grepl(forbidden, displayed, ignore.case = TRUE, perl = TRUE))) {
  stop("A manuscript-facing artifact contains an internal identifier.", call. = FALSE)
}
if (any(table$Configuration %in% LOMM11_PAIRED_VARIANTS)) {
  stop("A manuscript-facing configuration uses an internal factorial code.", call. = FALSE)
}
if (any(all_views_table$Configuration %in% LOMM11_PAIRED_VARIANTS)) {
  stop("An all-analysis configuration uses an internal factorial code.", call. = FALSE)
}
if (any(grepl("\u2014|---", c(
  caption, all_views_caption, selected_metric_caption
), perl = TRUE))) {
  stop("A manuscript-facing caption contains an em dash.", call. = FALSE)
}
if (!grepl("Bootstrap intervals are not shown for individual configurations", caption, fixed = TRUE) ||
    grepl("Horizontal bars", caption, fixed = TRUE) ||
    any(grepl("primary bias-corrected", c(
      all_views_caption, selected_metric_caption
    ), fixed = TRUE))) {
  stop("The figure or table captions misstate the bootstrap decision.", call. = FALSE)
}
if (any(!file.exists(figure_paths)) || any(file.info(figure_paths)$size < 1000L)) {
  stop("One or more figure files are missing or empty.", call. = FALSE)
}
if (any(table[["Basic 95% interval: lower"]] >
        table[["Basic 95% interval: upper"]]) ||
    any(table[["Percentile 95% interval: lower"]] >
        table[["Percentile 95% interval: upper"]])) {
  stop("The manuscript-facing interval endpoints are inverted.", call. = FALSE)
}
for (frame in list(all_views_table, selected_metric_table)) {
  if (any(frame[["Basic 95% interval: lower"]] >
          frame[["Basic 95% interval: upper"]]) ||
      any(frame[["Percentile 95% interval: lower"]] >
          frame[["Percentile 95% interval: upper"]])) {
    stop("An extended table contains inverted interval endpoints.", call. = FALSE)
  }
}
if (any(selected_metric_table[["Empirical units per fold: minimum"]] <= 0L) ||
    any(selected_metric_table[["Simulated units per fold: minimum"]] <= 0L)) {
  stop("The metric table contains invalid denominators.", call. = FALSE)
}

cat("Compositional-profile artifact tests: PASS\n")
