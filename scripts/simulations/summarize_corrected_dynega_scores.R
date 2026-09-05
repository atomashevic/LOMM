#!/usr/bin/env Rscript

# Rebuild score summaries from an explicit correction overlay; source results stay intact.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L || arguments[[1L]] != "--corrections-dir") {
  stop("Usage: Rscript scripts/simulations/summarize_corrected_dynega_scores.R --corrections-dir DIRECTORY",
    call. = FALSE)
}
corrections_dir <- normalizePath(arguments[[2L]], mustWork = TRUE)
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
root <- dirname(dirname(dirname(normalizePath(sub("^--file=", "", script_argument)))))
aggregate_dir <- file.path(root, "simulation_outputs/revision_v2/aggregate_results/e63eea5bdca6")
implementation <- new.env(parent = globalenv())
Sys.setenv(REVISION_RESULTS_NO_MAIN = "1")
source(file.path(root, "scripts/simulations/revision_spec.R"), local = implementation)
source(file.path(root, "scripts/simulations/build_revision_results.R"), local = implementation)
source(file.path(root, "scripts/figures/build_revision_v2_artifacts.R"), local = implementation)
workers <- implementation$rr_worker_count()
if (workers > 4L) stop("Use at most four aggregation workers.", call. = FALSE)

read_csv <- function(path) {
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NaN"))
}
write_csv <- function(value, path) {
  temporary <- paste0(path, ".tmp")
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) stop("Could not publish ", path, call. = FALSE)
}
replace_rows <- function(original, replacement, keys) {
  original_key <- implementation$rr_frame_key(original, keys)
  replacement_key <- implementation$rr_frame_key(replacement, keys)
  if (anyDuplicated(original_key) || anyDuplicated(replacement_key)) {
    stop("Summary keys must be unique.", call. = FALSE)
  }
  location <- match(replacement_key, original_key)
  if (anyNA(location)) stop("Correction contains an unknown summary row.", call. = FALSE)
  columns <- intersect(names(original), names(replacement))
  original[location, columns] <- replacement[, columns, drop = FALSE]
  original
}
write_replaced_csv <- function(source, updated, indices, destination) {
  lines <- readLines(source, warn = FALSE)
  if (length(lines) != nrow(updated) + 1L) {
    stop("Expected one physical CSV line per summary row.", call. = FALSE)
  }
  replacement <- capture.output(utils::write.table(updated[indices, , drop = FALSE],
    row.names = FALSE, col.names = FALSE, sep = ",", na = "", qmethod = "double"))
  lines[indices + 1L] <- replacement
  temporary <- paste0(destination, ".tmp")
  writeLines(lines, temporary)
  if (!file.rename(temporary, destination)) stop("Could not publish ", destination, call. = FALSE)
}

overlay_path <- file.path(corrections_dir, "checkpoint_results.csv")
worker_status_path <- file.path(corrections_dir, "status.json")
worker_status <- jsonlite::fromJSON(worker_status_path, simplifyVector = TRUE)
sha256 <- function(path) digest::digest(file = path, algo = "sha256")
if (!identical(worker_status$status, "complete") ||
    !identical(worker_status$checkpoint_sha256, sha256(overlay_path)) ||
    worker_status$completed_readouts != 144000L || worker_status$completed_panels != 48000L) {
  stop("A complete worker receipt matching the correction checkpoint is required.", call. = FALSE)
}
source_results <- file.path("simulation_outputs/revision_v2",
  c("t100_dynega/966643c4ba53", "t25_dynega/2205c374057a", "challenge_dynega/fc6333c2fdab"),
  "results.csv")
if (!all(source_results %in% names(worker_status$hashes))) {
  stop("Worker receipt must identify all original source results.", call. = FALSE)
}
for (path in names(worker_status$hashes)) {
  resolved <- if (startsWith(path, "/")) path else file.path(root, path)
  if (!file.exists(resolved) || !identical(sha256(resolved), worker_status$hashes[[path]])) {
    stop("Worker source hash mismatch: ", path, call. = FALSE)
  }
}
overlay <- read_csv(overlay_path)
required <- c("source_job", "unit_key", "panel_key", "old_score_cor_common",
  "old_score_nrmse_common", "score_cor_common", "score_nrmse_common")
if (length(setdiff(required, names(overlay))) || anyDuplicated(overlay$unit_key)) {
  stop("Correction overlay has missing columns or duplicate units.", call. = FALSE)
}
message("Reading selected columns from the original aggregate.")
long <- implementation$revision_v2_read_csv(aggregate_dir, "revision_results_long.csv",
  implementation$revision_v2_method_columns())
long <- long[!is.na(long$method_id) & nzchar(long$method_id), , drop = FALSE]
expected <- long$method_id == "dynEGA"
expected_counts <- c(t100_dynega = 64800L, t25_dynega = 64800L, challenge_dynega = 14400L)
if (!setequal(overlay$unit_key, long$unit_key[expected]) ||
    !identical(as.integer(table(factor(overlay$source_job, levels = names(expected_counts)))),
      unname(expected_counts))) {
  stop("Correction overlay must contain every expected DynEGA readout exactly once.", call. = FALSE)
}
location <- match(overlay$unit_key, long$unit_key)
if (any(overlay$source_job != long$.source_job[location]) ||
    any(overlay$panel_key != long$panel_key[location])) {
  stop("Correction identities disagree with the aggregate.", call. = FALSE)
}
for (metric in c("score_cor_common", "score_nrmse_common")) {
  old <- overlay[[paste0("old_", metric)]]
  if (!isTRUE(all.equal(old, long[[metric]][location], tolerance = 1e-12,
      check.attributes = FALSE))) {
    stop("Stored metric mismatch: ", metric, call. = FALSE)
  }
  if (any(!is.finite(overlay[[metric]]))) stop("Corrected scores must be finite.", call. = FALSE)
  long[[metric]][location] <- overlay[[metric]]
}

# These are the score definitions and pairing keys from rr_canonical_method_results.
# Partition metrics are outside this correction and do not need recomputation.
data <- long
data$method_id[data$method_id %in% c("static_correlation", "static_EGA_glasso")] <- "static_EGA"
data$community_algorithm <- tolower(data$community_algorithm)
data$status <- tolower(data$status)
data$status[data$status %in% c("success", "complete", "completed")] <- "ok"
data$analysis_scope <- ifelse(grepl("^challenge_", data$.source_job), "challenge", "matched")
for (column in c("TimeP", "N", "NVAR", "Error_sd", "Loadings", "Q", "rep")) {
  data[[column]] <- as.numeric(data[[column]])
}
data$challenge_id[is.na(data$challenge_id) | !nzchar(data$challenge_id)] <- "none"
data$representation[is.na(data$representation) | !nzchar(data$representation)] <- "linear_z"
data$panel_cluster_key <- data$panel_key
signature <- c("scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "Q", "rep",
  "challenge_id", "representation")
data$method_pair_key <- implementation$rr_frame_key(data, c(signature, "community_algorithm"))
data$horizon_pair_key <- implementation$rr_frame_key(data,
  setdiff(c(signature, "community_algorithm"), "TimeP"))
data$challenge_pair_key <- implementation$rr_frame_key(data,
  c("scenario", "N", "NVAR", "Error_sd", "Loadings", "Q", "rep", "method_id", "community_algorithm"))
data$representation_pair_key <- implementation$rr_frame_key(data,
  c("scenario", "challenge_id", "rep", "method_id", "community_algorithm"))
usable <- data$status == "ok" & is.finite(data$n_dim_hat)
data$value_score_correlation_marginal <- ifelse(usable & is.finite(data$score_cor_common),
  data$score_cor_common, 0)
data$value_score_nrmse_marginal <- ifelse(usable & is.finite(data$score_nrmse_common),
  data$score_nrmse_common, 2)
score_ids <- c("score_correlation_marginal", "score_nrmse_marginal")
catalog <- implementation$rr_metric_catalog()
catalog <- catalog[catalog$estimand_id %in% score_ids, , drop = FALSE]
implementation$rr_metric_catalog <- function() catalog

estimands_path <- file.path(aggregate_dir, "revision_estimand_results.csv")
original_estimands <- read_csv(estimands_path)
estimand_keys <- c("summary_level", "analysis_scope", "method_id", "community_algorithm",
  "scenario_family", "stability_class", "TimeP", "challenge_id", "representation", "estimand_id")
pairs_path <- file.path(aggregate_dir, "revision_paired_differences.csv")
original_pairs <- read_csv(pairs_path)
pair_keys <- c("comparison_type", "left_label", "right_label", "analysis_scope",
  "community_algorithm", "stability_class", "TimeP", "challenge_id", "representation", "estimand_id")
original_pair_function <- implementation$rr_summarize_pair
implementation$rr_summarize_pair <- function(left, right, key_column, comparison_type,
    left_label, right_label, reps = 2000L) {
  if (!any(left$method_id == "dynEGA") && !any(right$method_id == "dynEGA")) {
    return(data.frame())
  }
  original_pair_function(left, right, key_column, comparison_type, left_label, right_label, reps)
}
original_estimator <- implementation$rr_estimate_values
implementation$rr_estimate_values <- function(values, clusters, statistic_name, key, reps = 2000L) {
  keep <- is.finite(values) & !is.na(clusters) & nzchar(clusters)
  values <- values[keep]
  clusters <- clusters[keep]
  list(estimate = if (length(values)) mean(values) else NA_real_, monte_carlo_se = NA_real_,
    bootstrap_se = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
    n_rows = length(values), n_panels = length(unique(clusters)), bootstrap_reps = reps,
    bootstrap_seed = implementation$rr_seed_from_text(key))
}
baseline <- data
baseline$value_score_correlation_marginal[location] <- overlay$old_score_cor_common
baseline$value_score_nrmse_marginal[location] <- overlay$old_score_nrmse_common
check_baseline <- function(actual, original, keys, label, expected) {
  if (!setequal(implementation$rr_frame_key(actual, keys),
      implementation$rr_frame_key(original[expected, , drop = FALSE], keys))) {
    stop("Affected summary coverage is incomplete: ", label, call. = FALSE)
  }
  index <- match(implementation$rr_frame_key(actual, keys), implementation$rr_frame_key(original, keys))
  if (anyNA(index)) stop("Baseline summary keys disagree: ", label, call. = FALSE)
  for (column in c("estimate", "n_rows", "n_panels", "bootstrap_reps", "bootstrap_seed")) {
    if (!isTRUE(all.equal(as.numeric(actual[[column]]), as.numeric(original[[column]][index]),
        tolerance = 1e-12, check.attributes = FALSE))) {
      stop("Baseline summary mismatch in ", label, ": ", column, call. = FALSE)
    }
  }
  message("Baseline parity verified for ", nrow(actual), " ", label, " rows.")
}
message("Verifying original point estimates, denominators, and bootstrap seeds.")
check_baseline(implementation$rr_build_method_estimands(baseline[baseline$method_id == "dynEGA", ]),
  original_estimands, estimand_keys, "estimand",
  !is.na(original_estimands$method_id) & original_estimands$method_id == "dynEGA" &
    original_estimands$estimand_id %in% score_ids)
check_baseline(implementation$rr_build_paired_differences(baseline),
  original_pairs, pair_keys, "paired-difference", original_pairs$estimand_id %in% score_ids &
    (grepl("dynEGA", original_pairs$left_label, fixed = TRUE) |
      grepl("dynEGA", original_pairs$right_label, fixed = TRUE) |
      original_pairs$comparison_type %in% c("challenge_minus_ideal", "softmax_clr_minus_raw")))
original_table_path <- file.path(corrections_dir, "original_tables",
  "supplementary_score_quality_readout_sensitivity.csv")
if (!file.exists(original_table_path)) {
  original_table_path <- file.path(root, "paper/tables/supplementary_score_quality_readout_sensitivity.csv")
}
baseline_table <- read_csv(original_table_path)
baseline_table <- baseline_table[baseline_table$method_id == "dynEGA" &
  baseline_table$estimand_id %in% c("score_correlation_correct_dimension", "score_nrmse_correct_dimension"), ]
for (index in seq_len(nrow(baseline_table))) {
  row <- baseline_table[index, ]
  keep <- baseline$method_id == "dynEGA" & baseline$analysis_scope == "matched" &
    baseline$TimeP == row$time_points & baseline$community_algorithm == row$community_algorithm &
    baseline$status == "ok" & is.finite(baseline$n_dim_hat) & baseline$n_dim_hat == baseline$n_dim_true
  column <- if (row$estimand_id == "score_correlation_correct_dimension") {
    "value_score_correlation_marginal"
  } else "value_score_nrmse_marginal"
  value <- baseline[[column]][keep]
  panels <- baseline$panel_key[keep]
  panel_means <- vapply(split(value, panels), mean, numeric(1))
  if (abs(mean(panel_means) - row$estimate) > 1e-12 || length(panel_means) != row$n_panels) {
    stop("Baseline correct-dimensional table disagrees with stored rows.", call. = FALSE)
  }
}
message("Baseline parity verified for ", nrow(baseline_table), " correct-dimensional table rows.")
implementation$rr_estimate_values <- original_estimator
rm(baseline)

message("Recomputing DynEGA score estimands with the original 2,000-resample procedure.")
new_estimands <- implementation$rr_build_method_estimands(data[data$method_id == "dynEGA", ])
# Empirical summaries have additional descriptors and are copied unchanged.
method_rows <- !is.na(original_estimands$method_id) & nzchar(original_estimands$method_id)
updated_estimands <- original_estimands
updated_estimands[method_rows, ] <- replace_rows(original_estimands[method_rows, ],
  new_estimands, estimand_keys)
estimand_indices <- match(implementation$rr_frame_key(new_estimands, estimand_keys),
  implementation$rr_frame_key(original_estimands, estimand_keys))
write_replaced_csv(estimands_path, updated_estimands, estimand_indices,
  file.path(corrections_dir, "revision_estimand_results.csv"))

message("Recomputing affected paired score differences.")
new_pairs <- implementation$rr_build_paired_differences(data)
updated_pairs <- replace_rows(original_pairs, new_pairs, pair_keys)
pair_indices <- match(implementation$rr_frame_key(new_pairs, pair_keys),
  implementation$rr_frame_key(original_pairs, pair_keys))
write_replaced_csv(pairs_path, updated_pairs, pair_indices,
  file.path(corrections_dir, "revision_paired_differences.csv"))

message("Rebuilding the two reported score tables.")
canonical <- implementation$revision_v2_canonical_method_rows(long)
correct <- implementation$revision_v2_prepare_correct_dimension_trajectories(canonical, methods = "dynEGA")
marginal <- implementation$revision_v2_prepare_horizon_estimands(updated_estimands, score_ids,
  "Corrected marginal trajectory estimates", allow_undefined = TRUE)
replacement <- rbind(correct, marginal)
replacement <- replacement[replacement$method_id == "dynEGA", , drop = FALSE]
table_keys <- c("time_points", "method_id", "community_algorithm", "estimand_id")
table_names <- c("table2_score_quality_marginal_means.csv",
  "supplementary_score_quality_readout_sensitivity.csv")
dir.create(file.path(corrections_dir, "tables"), showWarnings = FALSE)
dir.create(file.path(corrections_dir, "original_tables"), showWarnings = FALSE)
changes <- list()
for (filename in table_names) {
  snapshot <- file.path(corrections_dir, "original_tables", filename)
  if (!file.exists(snapshot)) file.copy(file.path(root, "paper/tables", filename), snapshot)
  original <- read_csv(snapshot)
  selected <- replacement
  if (filename == table_names[[1L]]) selected <- selected[selected$community_algorithm == "louvain", ]
  updated <- replace_rows(original, selected, table_keys)
  implementation$revision_v2_validate_table(updated, filename)
  indices <- match(implementation$rr_frame_key(selected, table_keys),
    implementation$rr_frame_key(original, table_keys))
  write_replaced_csv(snapshot, updated, indices, file.path(corrections_dir, "tables", filename))
  changed <- which(original$estimate != updated$estimate)
  changes[[filename]] <- cbind(table = rep(filename, length(changed)), original[changed, table_keys],
    old_estimate = original$estimate[changed], estimate = updated$estimate[changed],
    monte_carlo_se = updated$monte_carlo_se[changed], conf_low = updated$conf_low[changed],
    conf_high = updated$conf_high[changed], n_panels = updated$n_panels[changed])
}
write_csv(do.call(rbind, changes), file.path(corrections_dir, "score_summary_changes.csv"))
for (filename in c("revision_estimand_results.csv", "revision_paired_differences.csv")) {
  snapshot <- file.path(corrections_dir, paste0("original_", filename))
  if (!file.exists(snapshot)) file.copy(file.path(aggregate_dir, filename), snapshot)
}
input_paths <- c(file.path(aggregate_dir, c("revision_results_long.csv",
  "revision_estimand_results.csv", "revision_paired_differences.csv")),
  overlay_path, worker_status_path)
output_paths <- c("revision_estimand_results.csv", "revision_paired_differences.csv",
  file.path("tables", table_names), "score_summary_changes.csv")
receipt <- list(status = "complete", source_aggregate_dir = aggregate_dir,
  corrected_readouts = nrow(overlay),
  input_hashes = as.list(setNames(vapply(input_paths, sha256, character(1)), input_paths)),
  output_hashes = as.list(setNames(vapply(file.path(corrections_dir, output_paths),
    sha256, character(1)), output_paths)),
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
jsonlite::write_json(receipt, file.path(corrections_dir, "summary_status.json"),
  auto_unbox = TRUE, pretty = TRUE)
message("Completed corrected aggregate overlays and score tables in ", corrections_dir)
