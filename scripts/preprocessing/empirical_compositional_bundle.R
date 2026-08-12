if (!exists("lomm_prepare_compositional_panel", mode = "function")) {
  stop("Source the compositional math and view modules first.", call. = FALSE)
}

LOMM_EMPIRICAL_SCORE_COLUMNS <- c(
  "anger", "disgust", "fear", "happiness", "neutral", "sadness",
  "surprise", "contempt", "anxiety", "helplessness", "disappointment"
)
LOMM_EMPIRICAL_INPUT_SHA256 <-
  "a6416f69f2090770f9fba5b13359883483ec4c689a34046a1c6c121c5843b67a"
LOMM_EMPIRICAL_EXPECTED_COUNTS <- list(
  source_rows = 3831L,
  clips = 100L,
  complete_rows = 3557L,
  all_missing_rows = 274L,
  partial_missing_rows = 0L,
  imputed_rows = 75L,
  imputed_clips = 29L,
  unresolved_rows = 199L,
  primary_rows = 3557L,
  sensitivity_rows = 3632L,
  primary_segments = 188L,
  sensitivity_segments = 129L
)
LOMM_VIEW_METADATA_COLUMNS <- c(
  "observation_key", "analysis_variant", "frame_key", "source_row", "clip",
  "frame_index", "truth_label", "sample_order", "model", "predicted_label",
  "source_complete", "is_imputed", "imputation_method", "segment_id",
  "segment_position", "segment_length"
)
LOMM_EMPIRICAL_EXPECTED_OUTPUT_DIMENSIONS <- list(
  frame_ledger.csv = c(rows = 3831L, columns = 31L),
  probability_view.csv = c(rows = 7189L, columns = 27L),
  clr_view.csv = c(rows = 7189L, columns = 27L),
  ilr_view.csv = c(rows = 7189L, columns = 26L),
  ilr_basis.csv = c(rows = 11L, columns = 12L),
  clip_qc.csv = c(rows = 100L, columns = 13L)
)

lomm_expected_output_schemas <- function() {
  list(
    frame_ledger.csv = c(
      "frame_key", "source_row", "clip", "frame_index", "truth_label",
      "sample_order", "model", "predicted_label", "missingness_class",
      "source_complete", "source_row_sum", "normalization_factor",
      "closure_deviation", "renormalized", "zero_replaced", "primary_included",
      "sensitivity_included", "is_imputed", "imputation_method",
      "imputation_gap_length", "interpolation_weight", "left_source_row",
      "right_source_row", "imputation_block_id", "primary_segment_id",
      "primary_segment_position", "primary_segment_length", "sensitivity_segment_id",
      "sensitivity_segment_position", "sensitivity_segment_length", "exclusion_reason"
    ),
    probability_view.csv = c(LOMM_VIEW_METADATA_COLUMNS, LOMM_EMPIRICAL_SCORE_COLUMNS),
    clr_view.csv = c(
      LOMM_VIEW_METADATA_COLUMNS, paste0("clr_", LOMM_EMPIRICAL_SCORE_COLUMNS)
    ),
    ilr_view.csv = c(LOMM_VIEW_METADATA_COLUMNS, sprintf("ilr_%02d", 1:10)),
    ilr_basis.csv = c("channel", "channel_order", sprintf("ilr_%02d", 1:10)),
    clip_qc.csv = c(
      "clip", "truth_label", "model", "source_rows", "complete_rows",
      "all_missing_rows", "partial_missing_rows", "imputed_rows",
      "unfilled_missing_rows", "primary_segments", "sensitivity_segments",
      "primary_max_segment_length", "sensitivity_max_segment_length"
    )
  )
}

lomm_validate_empirical_source <- function(panel) {
  metadata <- c(
    "frame_index", "clip", "truth_label", "sample_order", "model",
    "predicted_label"
  )
  required <- c(LOMM_EMPIRICAL_SCORE_COLUMNS, metadata)
  missing <- setdiff(required, names(panel))
  if (length(missing) > 0L) {
    stop("Empirical source is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  score_order <- names(panel)[names(panel) %in% LOMM_EMPIRICAL_SCORE_COLUMNS]
  if (!identical(score_order, LOMM_EMPIRICAL_SCORE_COLUMNS)) {
    stop("Empirical score-column order differs from the locked order.", call. = FALSE)
  }
  if (anyDuplicated(paste(panel$clip, panel$frame_index, sep = "\r"))) {
    stop("Empirical source contains duplicate clip/frame keys.", call. = FALSE)
  }
  scores <- as.matrix(panel[LOMM_EMPIRICAL_SCORE_COLUMNS])
  missing_counts <- rowSums(is.na(scores))
  if (any(!missing_counts %in% c(0L, ncol(scores)))) {
    stop("Empirical source contains partially missing compositions.", call. = FALSE)
  }
  complete <- missing_counts == 0L
  predicted <- LOMM_EMPIRICAL_SCORE_COLUMNS[
    max.col(scores[complete, , drop = FALSE], ties.method = "first")
  ]
  if (!identical(predicted, as.character(panel$predicted_label[complete]))) {
    stop("Predicted labels do not match score argmax values.", call. = FALSE)
  }
  for (field in c("truth_label", "sample_order", "model")) {
    counts <- vapply(split(panel[[field]], panel$clip), function(value) {
      length(unique(value))
    }, integer(1))
    if (any(counts != 1L)) {
      stop("Field '", field, "' varies within a clip.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

lomm_frame_key <- function(clip, frame_index) {
  clip <- as.character(clip)
  if (any(grepl("|", clip, fixed = TRUE)) || any(grepl("\r", clip, fixed = TRUE))) {
    stop("Clip identifiers cannot contain frame-key separators.", call. = FALSE)
  }
  formatted_index <- vapply(
    as.numeric(frame_index), function(value) sprintf("%.17g", value), character(1)
  )
  paste(clip, formatted_index, sep = "|")
}

lomm_build_frame_ledger <- function(primary, sensitivity) {
  if (!identical(primary$source_rows, sensitivity$source_rows)) {
    stop("Primary and sensitivity source rows do not align.", call. = FALSE)
  }
  panel <- primary$panel
  source_flags <- primary$source_flags
  imputation <- sensitivity$imputation_flags
  ledger <- data.frame(
    frame_key = lomm_frame_key(panel$clip, panel$frame_index),
    source_row = primary$source_rows,
    clip = as.character(panel$clip),
    frame_index = panel$frame_index,
    truth_label = as.character(panel$truth_label),
    sample_order = panel$sample_order,
    model = as.character(panel$model),
    predicted_label = as.character(panel$predicted_label),
    missingness_class = source_flags$missingness_class,
    source_complete = source_flags$source_complete,
    source_row_sum = source_flags$source_row_sum,
    normalization_factor = source_flags$normalization_factor,
    closure_deviation = source_flags$closure_deviation,
    renormalized = source_flags$renormalized,
    zero_replaced = source_flags$zero_replaced,
    primary_included = primary$included,
    sensitivity_included = sensitivity$included,
    is_imputed = imputation$ilr_imputed,
    imputation_method = ifelse(imputation$ilr_imputed, "linear_ilr", "none"),
    imputation_gap_length = imputation$imputation_gap_length,
    interpolation_weight = imputation$interpolation_weight,
    left_source_row = imputation$left_source_row,
    right_source_row = imputation$right_source_row,
    imputation_block_id = imputation$imputation_block_id,
    primary_segment_id = primary$analysis_segments$segment_id,
    primary_segment_position = primary$analysis_segments$segment_position,
    primary_segment_length = primary$analysis_segments$segment_length,
    sensitivity_segment_id = sensitivity$analysis_segments$segment_id,
    sensitivity_segment_position = sensitivity$analysis_segments$segment_position,
    sensitivity_segment_length = sensitivity$analysis_segments$segment_length,
    exclusion_reason = ifelse(
      imputation$unresolved_missing,
      paste0("unresolved_", source_flags$missingness_class),
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(ledger$frame_key)) {
    stop("Frame ledger contains duplicate keys.", call. = FALSE)
  }
  ledger
}

lomm_view_metadata <- function(prepared, variant, ledger) {
  indices <- which(prepared$included)
  frame_keys <- lomm_frame_key(
    prepared$panel$clip[indices], prepared$panel$frame_index[indices]
  )
  ledger_rows <- match(frame_keys, ledger$frame_key)
  data.frame(
    observation_key = paste(variant, frame_keys, sep = "|"),
    analysis_variant = variant,
    frame_key = frame_keys,
    source_row = prepared$source_rows[indices],
    clip = as.character(prepared$panel$clip[indices]),
    frame_index = prepared$panel$frame_index[indices],
    truth_label = as.character(prepared$panel$truth_label[indices]),
    sample_order = prepared$panel$sample_order[indices],
    model = as.character(prepared$panel$model[indices]),
    predicted_label = as.character(prepared$panel$predicted_label[indices]),
    source_complete = prepared$source_flags$source_complete[indices],
    is_imputed = prepared$imputation_flags$ilr_imputed[indices],
    imputation_method = ledger$imputation_method[ledger_rows],
    segment_id = prepared$analysis_segments$segment_id[indices],
    segment_position = prepared$analysis_segments$segment_position[indices],
    segment_length = prepared$analysis_segments$segment_length[indices],
    stringsAsFactors = FALSE
  )
}

lomm_build_one_view <- function(prepared, variant, ledger, representation) {
  metadata <- lomm_view_metadata(prepared, variant, ledger)
  indices <- which(prepared$included)
  values <- switch(
    representation,
    probability = prepared$probabilities[indices, , drop = FALSE],
    clr = prepared$clr[indices, , drop = FALSE],
    ilr = prepared$ilr[indices, , drop = FALSE],
    stop("Unknown compositional representation.", call. = FALSE)
  )
  if (identical(representation, "clr")) {
    colnames(values) <- paste0("clr_", colnames(values))
  }
  cbind(metadata, as.data.frame(values, stringsAsFactors = FALSE))
}

lomm_bind_variant_views <- function(primary, sensitivity, ledger, representation) {
  rbind(
    lomm_build_one_view(primary, "complete_case", ledger, representation),
    lomm_build_one_view(sensitivity, "ilr_gap2", ledger, representation)
  )
}

lomm_clip_qc_row <- function(ledger, indices) {
  primary_ids <- unique(stats::na.omit(ledger$primary_segment_id[indices]))
  sensitivity_ids <- unique(stats::na.omit(ledger$sensitivity_segment_id[indices]))
  data.frame(
    clip = ledger$clip[[indices[[1L]]]],
    truth_label = ledger$truth_label[[indices[[1L]]]],
    model = ledger$model[[indices[[1L]]]],
    source_rows = length(indices),
    complete_rows = sum(ledger$source_complete[indices]),
    all_missing_rows = sum(ledger$missingness_class[indices] == "all_missing"),
    partial_missing_rows = sum(ledger$missingness_class[indices] == "partial_missing"),
    imputed_rows = sum(ledger$is_imputed[indices]),
    unfilled_missing_rows = sum(!ledger$sensitivity_included[indices]),
    primary_segments = length(primary_ids),
    sensitivity_segments = length(sensitivity_ids),
    primary_max_segment_length = max(c(0L, ledger$primary_segment_length[indices]), na.rm = TRUE),
    sensitivity_max_segment_length = max(c(0L, ledger$sensitivity_segment_length[indices]), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

lomm_build_clip_qc <- function(ledger) {
  indices <- split(
    seq_len(nrow(ledger)), factor(ledger$clip, levels = unique(ledger$clip))
  )
  do.call(rbind, lapply(indices, function(index) lomm_clip_qc_row(ledger, index)))
}

lomm_observed_counts <- function(ledger, clip_qc) {
  list(
    source_rows = nrow(ledger),
    clips = length(unique(ledger$clip)),
    complete_rows = sum(ledger$source_complete),
    all_missing_rows = sum(ledger$missingness_class == "all_missing"),
    partial_missing_rows = sum(ledger$missingness_class == "partial_missing"),
    imputed_rows = sum(ledger$is_imputed),
    imputed_clips = sum(clip_qc$imputed_rows > 0L),
    unresolved_rows = sum(!ledger$sensitivity_included),
    primary_rows = sum(ledger$primary_included),
    sensitivity_rows = sum(ledger$sensitivity_included),
    primary_segments = length(unique(stats::na.omit(ledger$primary_segment_id))),
    sensitivity_segments = length(unique(stats::na.omit(ledger$sensitivity_segment_id)))
  )
}

lomm_validate_expected_counts <- function(observed) {
  for (name in names(LOMM_EMPIRICAL_EXPECTED_COUNTS)) {
    if (!identical(as.integer(observed[[name]]), LOMM_EMPIRICAL_EXPECTED_COUNTS[[name]])) {
      stop(
        "Observed count mismatch for ", name, ": ", observed[[name]], " != ",
        LOMM_EMPIRICAL_EXPECTED_COUNTS[[name]], ".",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

lomm_validate_views <- function(bundle) {
  keys <- lapply(bundle$views, function(view) view$observation_key)
  if (!identical(keys$probability, keys$clr) || !identical(keys$probability, keys$ilr) ||
      anyDuplicated(keys$probability)) {
    stop("Compositional view keys do not align exactly.", call. = FALSE)
  }
  probability_matrix <- as.matrix(bundle$views$probability[LOMM_EMPIRICAL_SCORE_COLUMNS])
  lomm_validate_positive_closed(probability_matrix, tolerance = 1e-12)
  clr_columns <- paste0("clr_", LOMM_EMPIRICAL_SCORE_COLUMNS)
  if (max(abs(rowSums(as.matrix(bundle$views$clr[clr_columns])))) > 1e-12) {
    stop("CLR view rows do not sum to zero.", call. = FALSE)
  }
  ilr_matrix <- as.matrix(bundle$views$ilr[colnames(bundle$basis)])
  recovered <- lomm_ilr_inverse(ilr_matrix, bundle$basis)
  if (max(abs(recovered - probability_matrix)) > 1e-12) {
    stop("ILR view does not round-trip to the probability view.", call. = FALSE)
  }
  if (any(bundle$views$probability$analysis_variant == "complete_case" &
          bundle$views$probability$is_imputed)) {
    stop("Primary complete-case view contains imputed rows.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_build_bundle <- function(panel) {
  common <- list(
    panel = panel,
    score_cols = LOMM_EMPIRICAL_SCORE_COLUMNS,
    clip_col = "clip",
    frame_col = "frame_index",
    closure_tolerance = 1e-6,
    zero_policy = "error",
    expected_step = 1
  )
  primary <- do.call(lomm_prepare_compositional_panel, c(common, list(max_gap = 0L)))
  sensitivity <- do.call(lomm_prepare_compositional_panel, c(common, list(max_gap = 2L)))
  ledger <- lomm_build_frame_ledger(primary, sensitivity)
  clip_qc <- lomm_build_clip_qc(ledger)
  basis <- primary$basis
  basis_table <- data.frame(
    channel = rownames(basis), channel_order = seq_len(nrow(basis)),
    as.data.frame(basis, stringsAsFactors = FALSE), stringsAsFactors = FALSE
  )
  bundle <- list(
    source_column_count = ncol(panel),
    source_column_names = names(panel),
    frame_ledger = ledger,
    views = list(
      probability = lomm_bind_variant_views(primary, sensitivity, ledger, "probability"),
      clr = lomm_bind_variant_views(primary, sensitivity, ledger, "clr"),
      ilr = lomm_bind_variant_views(primary, sensitivity, ledger, "ilr")
    ),
    basis = basis,
    basis_table = basis_table,
    clip_qc = clip_qc
  )
  bundle$observed_counts <- lomm_observed_counts(ledger, clip_qc)
  lomm_validate_expected_counts(bundle$observed_counts)
  lomm_validate_views(bundle)
  bundle
}

lomm_bundle_tables <- function(bundle) {
  list(
    frame_ledger.csv = bundle$frame_ledger,
    probability_view.csv = bundle$views$probability,
    clr_view.csv = bundle$views$clr,
    ilr_view.csv = bundle$views$ilr,
    ilr_basis.csv = bundle$basis_table,
    clip_qc.csv = bundle$clip_qc
  )
}

lomm_basis_specification <- function(basis_table) {
  coordinate_columns <- sprintf("ilr_%02d", 1:10)
  list(
    channels = as.list(as.character(basis_table$channel)),
    channel_order = as.list(as.integer(basis_table$channel_order)),
    coordinates = lapply(seq_len(nrow(basis_table)), function(index) {
      as.list(as.numeric(basis_table[index, coordinate_columns]))
    })
  )
}
