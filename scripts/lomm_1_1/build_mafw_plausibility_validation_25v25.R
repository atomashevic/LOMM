#!/usr/bin/env Rscript

mafw25_script_path <- function() {
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[
    !is.na(frames) &
      basename(frames) == "build_mafw_plausibility_validation_25v25.R"
  ]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Could not locate the 25v25 script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

MAFW25_SCRIPT_PATH <- mafw25_script_path()
MAFW_PLAUSIBILITY_SCRIPT_PATH <- normalizePath(
  file.path(dirname(MAFW25_SCRIPT_PATH), "build_mafw_plausibility_validation.R"),
  mustWork = TRUE
)
source(MAFW_PLAUSIBILITY_SCRIPT_PATH, local = .GlobalEnv)

MAFW_FOLD_COUNT <- 4L
MAFW_HELDOUT_CLIPS_PER_FOLD <- 25L
MAFW_PLAUSIBILITY_OUTPUT <- file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "mafw_plausibility_validation_25v25"
)

mafw_paths <- function() {
  list(
    runner = file.path(
      MAFW_PLAUSIBILITY_ROOT, "scripts", "lomm_1_1", "run_realism_ablation.R"
    ),
    training_losses = file.path(MAFW_PLAUSIBILITY_OUTPUT, "training_losses.csv"),
    selected_settings = file.path(MAFW_PLAUSIBILITY_OUTPUT, "selected_settings.csv"),
    geometry = file.path(MAFW_PLAUSIBILITY_OUTPUT, "fold_geometry.csv"),
    metric_results = file.path(MAFW_PLAUSIBILITY_OUTPUT, "metric_results.csv"),
    composite_results = file.path(MAFW_PLAUSIBILITY_OUTPUT, "composite_results.csv"),
    example_series = file.path(MAFW_PLAUSIBILITY_OUTPUT, "example_series.csv"),
    success = file.path(MAFW_PLAUSIBILITY_OUTPUT, "_SUCCESS.json"),
    table_metric = file.path(MAFW_PLAUSIBILITY_OUTPUT, "metric_summary.csv"),
    table_composite = file.path(MAFW_PLAUSIBILITY_OUTPUT, "composite_summary.csv"),
    table_deviations = file.path(MAFW_PLAUSIBILITY_OUTPUT, "deviations.csv"),
    table_settings = file.path(MAFW_PLAUSIBILITY_OUTPUT, "fold_settings.csv"),
    table_example = file.path(MAFW_PLAUSIBILITY_OUTPUT, "representative_example.csv"),
    caption = file.path(MAFW_PLAUSIBILITY_OUTPUT, "representative_example_caption.txt"),
    figure_pdf = file.path(MAFW_PLAUSIBILITY_OUTPUT, "representative_example.pdf"),
    figure_png = file.path(MAFW_PLAUSIBILITY_OUTPUT, "representative_example.png")
  )
}

mafw25_assign_folds <- function(clip_qc) {
  required <- c(
    "clip", "truth_label", "complete_rows", "primary_segments",
    "sensitivity_segments", "primary_max_segment_length",
    "sensitivity_max_segment_length"
  )
  if (!all(required %in% names(clip_qc)) || nrow(clip_qc) != 100L ||
      anyDuplicated(clip_qc$clip)) {
    mafw_stop("Clip QC does not satisfy the 25v25 fold input contract.")
  }
  folds <- integer(nrow(clip_qc))
  offset <- 0L
  for (stratum in sort(unique(as.character(clip_qc$truth_label)))) {
    indices <- which(as.character(clip_qc$truth_label) == stratum)
    ordered <- indices[order(
      as.integer(clip_qc$complete_rows[indices]),
      as.character(clip_qc$clip[indices])
    )]
    folds[ordered] <- ((seq_along(ordered) - 1L + offset) %% MAFW_FOLD_COUNT) + 1L
    offset <- (offset + length(indices)) %% MAFW_FOLD_COUNT
  }
  assignments <- data.frame(
    clip = as.character(clip_qc$clip),
    truth_label = as.character(clip_qc$truth_label),
    complete_rows = as.integer(clip_qc$complete_rows),
    primary_segments = as.integer(clip_qc$primary_segments),
    sensitivity_segments = as.integer(clip_qc$sensitivity_segments),
    primary_max_segment_length = as.integer(clip_qc$primary_max_segment_length),
    sensitivity_max_segment_length = as.integer(clip_qc$sensitivity_max_segment_length),
    fold = as.integer(folds),
    stringsAsFactors = FALSE
  )
  assignments <- assignments[
    order(assignments$fold, assignments$truth_label, assignments$clip),
  ]
  rownames(assignments) <- NULL
  fold_sizes <- as.integer(table(factor(
    assignments$fold, levels = seq_len(MAFW_FOLD_COUNT)
  )))
  if (!identical(fold_sizes, rep(MAFW_HELDOUT_CLIPS_PER_FOLD, MAFW_FOLD_COUNT))) {
    mafw_stop("The four folds must contain exactly 25 clips each.")
  }
  for (fold in seq_len(MAFW_FOLD_COUNT)) {
    heldout <- assignments[assignments$fold == fold, , drop = FALSE]
    training <- assignments[assignments$fold != fold, , drop = FALSE]
    target <- table(heldout$truth_label)
    available <- table(factor(training$truth_label, levels = names(target)))
    if (any(as.integer(available) < 2L * as.integer(target))) {
      mafw_stop("Fold ", fold, " cannot support two disjoint stratified groups.")
    }
  }
  assignments
}

mafw25_training_plan <- function(candidate_grid) {
  candidate_ids <- as.character(candidate_grid$candidate_id[
    as.character(candidate_grid$variant_id) == MAFW_PROFILE_ID
  ])
  if (length(candidate_ids) != 27L || anyDuplicated(candidate_ids)) {
    mafw_stop("The canonical profile must have 27 seed candidates.")
  }
  rows <- expand.grid(
    fold = seq_len(MAFW_FOLD_COUNT),
    candidate_id = candidate_ids,
    training_rep = seq_len(MAFW_TRAINING_REPLICATES),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  rows$variant_id <- MAFW_PROFILE_ID
  rows$seed_group <- sprintf(
    "mafw25v25|training|F%02d|%s|R%03d",
    rows$fold, rows$candidate_id, rows$training_rep
  )
  rows$seed_base <- vapply(
    rows$seed_group, lomm11_seed_from_key, integer(1), MAFW_REFERENCE_SEED
  )
  rows[order(rows$fold, rows$candidate_id, rows$training_rep), ]
}

mafw25_heldout_plan <- function() {
  rows <- expand.grid(
    fold = seq_len(MAFW_FOLD_COUNT),
    heldout_rep = seq_len(MAFW_HELDOUT_REPLICATES),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  rows$variant_id <- MAFW_PROFILE_ID
  rows$seed_group <- sprintf(
    "mafw25v25|heldout|F%02d|R%03d", rows$fold, rows$heldout_rep
  )
  rows$seed_base <- vapply(
    rows$seed_group, lomm11_seed_from_key, integer(1), MAFW_REFERENCE_SEED
  )
  rows[order(rows$fold, rows$heldout_rep), ]
}

mafw_prepare_20v20 <- mafw_prepare

mafw_prepare <- function(paths = mafw_paths()) {
  prepared <- mafw_prepare_20v20(paths)
  assignments <- mafw25_assign_folds(prepared$input_tables[["clip_qc.csv"]])
  training_plan <- mafw25_training_plan(
    prepared$context$plan$candidate_grid.csv
  )
  heldout_plan <- mafw25_heldout_plan()
  prepared$context$config$folds$count <- MAFW_FOLD_COUNT
  prepared$context$config$folds$heldout_clips_per_fold <-
    MAFW_HELDOUT_CLIPS_PER_FOLD
  prepared$context$plan$fold_assignments.csv <- assignments
  prepared$context$plan$training_plan.csv <- training_plan
  prepared$context$plan$heldout_plan.csv <- heldout_plan

  dir.create(MAFW_PLAUSIBILITY_OUTPUT, recursive = TRUE, showWarnings = FALSE)
  mafw_atomic_csv(
    assignments, file.path(MAFW_PLAUSIBILITY_OUTPUT, "fold_assignments.csv")
  )
  mafw_atomic_csv(
    training_plan, file.path(MAFW_PLAUSIBILITY_OUTPUT, "training_plan.csv")
  )
  mafw_atomic_csv(
    heldout_plan, file.path(MAFW_PLAUSIBILITY_OUTPUT, "heldout_plan.csv")
  )
  lomm11_runner_atomic_json(list(
    schema_version = 1L,
    analysis_role = "main",
    design = "25 simulated versus 25 held-out MAFW; 25 versus 25 disjoint MAFW",
    folds = MAFW_FOLD_COUNT,
    training_clips_per_fold = 75L,
    clips_per_comparison_group = MAFW_HELDOUT_CLIPS_PER_FOLD,
    real_real_groups_disjoint = TRUE,
    stratify_by = "truth_label",
    order_by = c("complete_rows", "clip"),
    heldout_replicates = MAFW_HELDOUT_REPLICATES,
    real_real_reference_replicates = MAFW_REFERENCE_REPLICATES,
    output_directory = MAFW_PLAUSIBILITY_OUTPUT
  ), file.path(MAFW_PLAUSIBILITY_OUTPUT, "design.json"))
  prepared
}

if (sys.nframe() == 0L) mafw_main()
