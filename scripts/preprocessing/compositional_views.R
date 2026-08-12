if (!exists("lomm_ilr", mode = "function")) {
  stop("Source compositional_math.R before compositional_views.R.", call. = FALSE)
}

lomm_validate_series_keys <- function(groups, times) {
  if (length(groups) != length(times) || anyNA(groups) || any(!nzchar(groups)) ||
      any(!is.finite(times))) {
    lomm_comp_stop("Series groups and frame indices must be complete and finite.")
  }
  if (anyDuplicated(paste(groups, times, sep = "\r"))) {
    lomm_comp_stop("Clip and frame-index keys must be unique.")
  }
  for (indices in split(seq_along(groups), factor(groups, levels = unique(groups)))) {
    if (any(diff(times[indices]) <= 0)) {
      lomm_comp_stop("Frame indices must increase strictly within each clip.")
    }
  }
  invisible(TRUE)
}

lomm_cadence_matches <- function(differences, expected_step) {
  if (is.null(expected_step)) {
    return(rep(TRUE, length(differences)))
  }
  tolerance <- 32 * .Machine$double.eps * pmax(
    1, abs(differences), abs(expected_step)
  )
  abs(differences - expected_step) <= tolerance
}

lomm_segment_metadata <- function(groups, times, included, expected_step = 1) {
  groups <- as.character(groups)
  times <- as.numeric(times)
  included <- as.logical(included)
  if (length(included) != length(groups) || anyNA(included)) {
    lomm_comp_stop("Segment inclusion flags must be complete and row-aligned.")
  }
  if (!is.null(expected_step) &&
      (length(expected_step) != 1L || !is.finite(expected_step) || expected_step <= 0)) {
    lomm_comp_stop("Expected frame step must be NULL or a positive finite scalar.")
  }
  lomm_validate_series_keys(groups, times)
  segment_ids <- rep(NA_character_, length(groups))
  positions <- rep(NA_integer_, length(groups))
  for (indices in split(seq_along(groups), factor(groups, levels = unique(groups)))) {
    segment_number <- 0L
    position <- 0L
    previous_index <- NA_integer_
    for (index in indices) {
      if (!included[[index]]) {
        previous_index <- index
        position <- 0L
        next
      }
      cadence_break <- !is.na(previous_index) && !lomm_cadence_matches(
        times[[index]] - times[[previous_index]], expected_step
      )
      starts_segment <- is.na(previous_index) || !included[[previous_index]] || cadence_break
      if (starts_segment) {
        segment_number <- segment_number + 1L
        position <- 1L
      } else {
        position <- position + 1L
      }
      segment_ids[[index]] <- sprintf("%s::seg%03d", groups[[index]], segment_number)
      positions[[index]] <- position
      previous_index <- index
    }
  }
  lengths <- rep(NA_integer_, length(groups))
  valid_ids <- !is.na(segment_ids)
  segment_lengths <- table(segment_ids[valid_ids])
  lengths[valid_ids] <- as.integer(segment_lengths[segment_ids[valid_ids]])
  data.frame(
    segment_id = segment_ids,
    segment_position = positions,
    segment_length = lengths,
    segment_start = !is.na(positions) & positions == 1L,
    stringsAsFactors = FALSE
  )
}

lomm_missing_runs <- function(missing_mask) {
  runs <- rle(missing_mask)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  data.frame(
    start = starts[runs$values],
    end = ends[runs$values],
    length = runs$lengths[runs$values]
  )
}

lomm_initialize_interpolation <- function(probabilities, source_complete, basis) {
  coordinates <- matrix(
    NA_real_, nrow = nrow(probabilities), ncol = ncol(basis),
    dimnames = list(NULL, colnames(basis))
  )
  coordinates[source_complete, ] <- lomm_ilr(
    probabilities[source_complete, , drop = FALSE], basis
  )
  flags <- data.frame(
    ilr_imputed = rep(FALSE, nrow(probabilities)),
    unresolved_missing = !source_complete,
    imputation_gap_length = rep(NA_integer_, nrow(probabilities)),
    interpolation_weight = rep(NA_real_, nrow(probabilities)),
    left_source_row = rep(NA_integer_, nrow(probabilities)),
    right_source_row = rep(NA_integer_, nrow(probabilities)),
    imputation_block_id = rep(NA_character_, nrow(probabilities)),
    stringsAsFactors = FALSE
  )
  list(coordinates = coordinates, flags = flags)
}

lomm_fill_one_ilr_gap <- function(state,
                                  indices,
                                  gap,
                                  gap_index,
                                  source_complete,
                                  groups,
                                  times,
                                  max_gap,
                                  expected_step,
                                  source_rows) {
  local_missing <- seq.int(gap$start, gap$end)
  missing_indices <- indices[local_missing]
  state$flags$imputation_gap_length[missing_indices] <- gap$length
  state$flags$imputation_block_id[missing_indices] <- sprintf(
    "%s::gap%03d", groups[[indices[[1L]]]], gap_index
  )
  bounded <- gap$start > 1L && gap$end < length(indices)
  if (!bounded || gap$length > max_gap) {
    return(state)
  }
  left_index <- indices[[gap$start - 1L]]
  right_index <- indices[[gap$end + 1L]]
  interval_indices <- c(left_index, missing_indices, right_index)
  cadence_ok <- all(lomm_cadence_matches(diff(times[interval_indices]), expected_step))
  if (!source_complete[[left_index]] || !source_complete[[right_index]] || !cadence_ok) {
    return(state)
  }
  denominator <- times[[right_index]] - times[[left_index]]
  weights <- (times[missing_indices] - times[[left_index]]) / denominator
  left_values <- state$coordinates[left_index, ]
  right_values <- state$coordinates[right_index, ]
  state$coordinates[missing_indices, ] <-
    outer(1 - weights, left_values) + outer(weights, right_values)
  state$flags$ilr_imputed[missing_indices] <- TRUE
  state$flags$unresolved_missing[missing_indices] <- FALSE
  state$flags$interpolation_weight[missing_indices] <- weights
  state$flags$left_source_row[missing_indices] <- source_rows[[left_index]]
  state$flags$right_source_row[missing_indices] <- source_rows[[right_index]]
  state
}

lomm_interpolate_ilr_gaps <- function(probabilities,
                                      source_complete,
                                      groups,
                                      times,
                                      basis,
                                      max_gap = 0L,
                                      expected_step = 1,
                                      source_rows = seq_along(groups)) {
  probabilities <- as.matrix(probabilities)
  source_complete <- as.logical(source_complete)
  groups <- as.character(groups)
  times <- as.numeric(times)
  row_count <- nrow(probabilities)
  if (length(source_complete) != row_count || length(groups) != row_count ||
      length(times) != row_count || length(source_rows) != row_count ||
      anyNA(source_complete)) {
    lomm_comp_stop("Interpolation inputs must be complete and row-aligned.")
  }
  lomm_validate_ilr_basis(basis, colnames(probabilities))
  if (length(max_gap) != 1L || !is.numeric(max_gap) || !is.finite(max_gap) ||
      max_gap < 0 || max_gap > .Machine$integer.max || max_gap != floor(max_gap)) {
    lomm_comp_stop("Maximum interpolation gap must be a non-negative integer.")
  }
  max_gap <- as.integer(max_gap)
  lomm_validate_series_keys(groups, times)
  state <- lomm_initialize_interpolation(probabilities, source_complete, basis)
  for (indices in split(seq_along(groups), factor(groups, levels = unique(groups)))) {
    gaps <- lomm_missing_runs(!source_complete[indices])
    for (gap_index in seq_len(nrow(gaps))) {
      state <- lomm_fill_one_ilr_gap(
        state, indices, gaps[gap_index, ], gap_index, source_complete,
        groups, times, max_gap, expected_step, source_rows
      )
    }
  }
  filled <- probabilities
  if (any(state$flags$ilr_imputed)) {
    filled[state$flags$ilr_imputed, ] <- lomm_ilr_inverse(
      state$coordinates[state$flags$ilr_imputed, , drop = FALSE], basis
    )
  }
  list(probabilities = filled, coordinates = state$coordinates, flags = state$flags)
}

lomm_validate_panel_input <- function(panel, score_cols, clip_col, frame_col) {
  required <- c(score_cols, clip_col, frame_col)
  if (!is.data.frame(panel) || length(setdiff(required, names(panel))) > 0L) {
    lomm_comp_stop("Input panel is missing required composition or series columns.")
  }
  observed_score_order <- names(panel)[names(panel) %in% score_cols]
  if (!identical(observed_score_order, score_cols)) {
    lomm_comp_stop("Score columns do not match the locked component order.")
  }
  if (any(!vapply(panel[score_cols], is.numeric, logical(1)))) {
    lomm_comp_stop("All score columns must be numeric.")
  }
  if (!is.numeric(panel[[frame_col]])) {
    lomm_comp_stop("Frame-index column must be numeric.")
  }
  invisible(TRUE)
}

lomm_prepare_compositional_panel <- function(panel,
                                             score_cols,
                                             clip_col = "clip",
                                             frame_col = "frame_index",
                                             max_gap = 0L,
                                             closure_tolerance = 1e-6,
                                             zero_policy = "error",
                                             zero_replacement = NULL,
                                             expected_step = 1) {
  lomm_validate_panel_input(panel, score_cols, clip_col, frame_col)
  source_rows <- seq_len(nrow(panel))
  clip_order <- match(panel[[clip_col]], unique(panel[[clip_col]]))
  ordering <- order(clip_order, panel[[frame_col]], source_rows)
  ordered <- panel[ordering, , drop = FALSE]
  ordered_source_rows <- source_rows[ordering]
  groups <- as.character(ordered[[clip_col]])
  times <- as.numeric(ordered[[frame_col]])
  lomm_validate_series_keys(groups, times)
  closed <- lomm_close_probabilities(
    ordered[score_cols], closure_tolerance, zero_policy, zero_replacement
  )
  basis <- lomm_ilr_basis(score_cols)
  source_segments <- lomm_segment_metadata(
    groups, times, closed$flags$source_complete, expected_step
  )
  interpolated <- lomm_interpolate_ilr_gaps(
    closed$probabilities, closed$flags$source_complete, groups, times, basis,
    max_gap, expected_step, ordered_source_rows
  )
  included <- closed$flags$source_complete | interpolated$flags$ilr_imputed
  analysis_segments <- lomm_segment_metadata(groups, times, included, expected_step)
  clr_values <- matrix(
    NA_real_, nrow = nrow(ordered), ncol = length(score_cols),
    dimnames = list(NULL, score_cols)
  )
  clr_values[included, ] <- lomm_clr(interpolated$probabilities[included, , drop = FALSE])
  list(
    panel = ordered,
    source_rows = ordered_source_rows,
    score_cols = score_cols,
    basis = basis,
    probabilities = interpolated$probabilities,
    clr = clr_values,
    ilr = interpolated$coordinates,
    included = included,
    source_flags = closed$flags,
    imputation_flags = interpolated$flags,
    source_segments = source_segments,
    analysis_segments = analysis_segments
  )
}
