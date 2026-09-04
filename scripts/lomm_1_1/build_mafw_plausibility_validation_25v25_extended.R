#!/usr/bin/env Rscript

mafw_extended_script_path <- function() {
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[
    !is.na(frames) &
      basename(frames) == "build_mafw_plausibility_validation_25v25_extended.R"
  ]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Could not locate the extended script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

MAFW_EXTENDED_SCRIPT_PATH <- mafw_extended_script_path()
source(file.path(
  dirname(MAFW_EXTENDED_SCRIPT_PATH),
  "build_mafw_plausibility_validation_25v25.R"
), local = .GlobalEnv)

MAFW_PLAUSIBILITY_OUTPUT <- file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "mafw_plausibility_validation_25v25_extended"
)
MAFW_EXTENDED_METRICS_PATH <- file.path(
  dirname(MAFW_EXTENDED_SCRIPT_PATH), "extended_realism_metrics.R"
)
MAFW_METRIC_LABELS <- c(
  level_median = "Median level",
  level_iqr = "Level IQR",
  difference_iqr = "Change IQR",
  lag1_correlation = "Lag-1 correlation",
  run_length_entropy = "Run-length entropy",
  sample_entropy = "Sample entropy",
  permutation_entropy = "Permutation entropy",
  spectral_entropy = "Spectral entropy",
  effective_rank = "Effective rank",
  robust_level_variance = "Robust level variance",
  robust_change_variance = "Robust change variance"
)
MAFW_TEMPORAL_SELECTION_METRICS <- c(
  "difference_iqr", "lag1_correlation", "run_length_entropy",
  "sample_entropy", "permutation_entropy", "spectral_entropy"
)

mafw_prepare_25v25_standard <- mafw_prepare

mafw_prepare <- function(paths = mafw_paths()) {
  prepared <- mafw_prepare_25v25_standard(paths)
  source(MAFW_EXTENDED_METRICS_PATH, local = .GlobalEnv)
  lomm11_run_extended_metric_checks()
  lomm11_runner_atomic_json(list(
    schema_version = 1L,
    analysis_role = "main candidate",
    design = "25 simulated versus 25 held-out MAFW; 25 versus 25 disjoint MAFW",
    folds = MAFW_FOLD_COUNT,
    training_clips_per_fold = 75L,
    clips_per_comparison_group = MAFW_HELDOUT_CLIPS_PER_FOLD,
    real_real_groups_disjoint = TRUE,
    stratify_by = "truth_label",
    order_by = c("complete_rows", "clip"),
    heldout_replicates = MAFW_HELDOUT_REPLICATES,
    real_real_reference_replicates = MAFW_REFERENCE_REPLICATES,
    metric_cells = length(LOMM11_METRIC_FAMILIES) * length(LOMM11_METRICS),
    univariate_metrics = as.list(LOMM11_UNIVARIATE_METRICS),
    multivariate_metrics = as.list(LOMM11_MULTIVARIATE_METRICS),
    entropy_parameters = list(
      sample_entropy_order = 2L,
      sample_entropy_tolerance_sd = 0.20,
      sample_entropy_match_regularization = "(extended matches + 0.5) / (base matches + 1)",
      permutation_entropy_order = 3L
    ),
    variance_definitions = list(
      robust_level_variance = paste(
        "10 percent trimmed mean squared distance from the clip center",
        "divided by coordinate dimension"
      ),
      robust_change_variance = paste(
        "10 percent trimmed mean squared within-segment first difference",
        "divided by twice the coordinate dimension"
      ),
      logratio_geometry = "ILR coordinates"
    ),
    output_directory = MAFW_PLAUSIBILITY_OUTPUT
  ), file.path(MAFW_PLAUSIBILITY_OUTPUT, "design.json"))
  prepared
}

if (sys.nframe() == 0L) mafw_main()
