LOMM_COMPOSITION_SCHEMA_VERSION <- 1L

lomm_comp_stop <- function(...) {
  stop(..., call. = FALSE)
}

lomm_require_numeric_matrix <- function(values, minimum_columns = 2L) {
  matrix_values <- as.matrix(values)
  if (!is.numeric(matrix_values)) {
    lomm_comp_stop("Compositions must be numeric.")
  }
  if (ncol(matrix_values) < minimum_columns) {
    lomm_comp_stop("Compositions require at least two components.")
  }
  component_names <- colnames(matrix_values)
  if (is.null(component_names) || anyNA(component_names) ||
      any(!nzchar(component_names)) || anyDuplicated(component_names)) {
    lomm_comp_stop("Composition component names must be present and unique.")
  }
  storage.mode(matrix_values) <- "double"
  matrix_values
}

lomm_validate_positive_closed <- function(probabilities, tolerance = 1e-10) {
  probabilities <- lomm_require_numeric_matrix(probabilities)
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) {
    lomm_comp_stop("Closure validation tolerance must be a non-negative finite scalar.")
  }
  if (any(!is.finite(probabilities)) || any(probabilities <= 0) ||
      any(probabilities > 1)) {
    lomm_comp_stop("Closed compositions must be finite and strictly inside (0, 1).")
  }
  if (any(abs(rowSums(probabilities) - 1) > tolerance)) {
    lomm_comp_stop("Composition rows are not closed within tolerance.")
  }
  invisible(TRUE)
}

lomm_replace_zeros <- function(probabilities, zero_replacement) {
  if (length(zero_replacement) != 1L || !is.finite(zero_replacement) ||
      zero_replacement <= 0) {
    lomm_comp_stop("Multiplicative zero replacement requires a positive finite delta.")
  }
  replaced <- probabilities
  zero_counts <- rowSums(replaced == 0)
  for (row_index in which(zero_counts > 0L)) {
    zero_count <- zero_counts[[row_index]]
    if (zero_count * zero_replacement >= 1) {
      lomm_comp_stop("Zero replacement delta is too large for a composition row.")
    }
    zero_mask <- replaced[row_index, ] == 0
    replaced[row_index, zero_mask] <- zero_replacement
    replaced[row_index, !zero_mask] <-
      replaced[row_index, !zero_mask] * (1 - zero_count * zero_replacement)
  }
  replaced
}

lomm_close_probabilities <- function(values,
                                     closure_tolerance = 1e-6,
                                     zero_policy = c("error", "multiplicative"),
                                     zero_replacement = NULL) {
  probabilities <- lomm_require_numeric_matrix(values)
  zero_policy <- match.arg(zero_policy)
  if (length(closure_tolerance) != 1L || !is.finite(closure_tolerance) ||
      closure_tolerance < 0) {
    lomm_comp_stop("Closure tolerance must be a non-negative finite scalar.")
  }
  if (any(is.infinite(probabilities)) || any(is.nan(probabilities))) {
    lomm_comp_stop("Probability input contains NaN or infinite values.")
  }
  observed_values <- probabilities[!is.na(probabilities)]
  if (any(observed_values < 0) || any(observed_values > 1)) {
    lomm_comp_stop("Observed probability values must remain inside [0, 1].")
  }
  missing_counts <- rowSums(is.na(probabilities))
  source_complete <- missing_counts == 0L
  missingness_class <- ifelse(
    source_complete,
    "complete",
    ifelse(missing_counts == ncol(probabilities), "all_missing", "partial_missing")
  )
  closed <- matrix(
    NA_real_, nrow = nrow(probabilities), ncol = ncol(probabilities),
    dimnames = dimnames(probabilities)
  )
  source_sums <- rep(NA_real_, nrow(probabilities))
  zero_counts <- rep(NA_integer_, nrow(probabilities))
  if (any(source_complete)) {
    observed <- probabilities[source_complete, , drop = FALSE]
    if (any(observed < 0) || any(observed > 1)) {
      lomm_comp_stop("Complete probability rows contain values outside [0, 1].")
    }
    observed_sums <- rowSums(observed)
    if (any(!is.finite(observed_sums)) || any(observed_sums <= 0)) {
      lomm_comp_stop("Complete probability rows must have a positive finite sum.")
    }
    if (any(abs(observed_sums - 1) > closure_tolerance)) {
      lomm_comp_stop("Complete probability rows exceed the closure tolerance.")
    }
    observed <- observed / observed_sums
    observed_zero_counts <- rowSums(observed == 0)
    if (any(observed_zero_counts > 0L) && identical(zero_policy, "error")) {
      lomm_comp_stop("Zero probabilities require an explicit replacement policy.")
    }
    if (any(observed_zero_counts > 0L)) {
      observed <- lomm_replace_zeros(observed, zero_replacement)
    }
    closed[source_complete, ] <- observed
    source_sums[source_complete] <- observed_sums
    zero_counts[source_complete] <- observed_zero_counts
  }
  closure_deviation <- abs(source_sums - 1)
  list(
    probabilities = closed,
    flags = data.frame(
      source_complete = source_complete,
      missingness_class = missingness_class,
      source_row_sum = source_sums,
      normalization_factor = 1 / source_sums,
      closure_deviation = closure_deviation,
      renormalized = source_complete & closure_deviation > 0,
      zero_count = zero_counts,
      zero_replaced = !is.na(zero_counts) & zero_counts > 0L,
      stringsAsFactors = FALSE
    )
  )
}

lomm_softmax_rows <- function(values) {
  values <- lomm_require_numeric_matrix(values)
  if (any(!is.finite(values))) {
    lomm_comp_stop("Softmax input must be finite.")
  }
  shifted <- values - apply(values, 1L, max)
  exponentiated <- exp(shifted)
  probabilities <- exponentiated / rowSums(exponentiated)
  if (any(probabilities <= 0) || any(!is.finite(probabilities))) {
    lomm_comp_stop("Softmax produced a non-positive or non-finite composition.")
  }
  probabilities
}

lomm_clr <- function(probabilities, tolerance = 1e-10) {
  lomm_validate_positive_closed(probabilities, tolerance)
  log_values <- log(as.matrix(probabilities))
  output <- log_values - rowMeans(log_values)
  colnames(output) <- colnames(probabilities)
  output
}

lomm_clr_inverse <- function(clr_values, tolerance = 1e-10) {
  clr_values <- lomm_require_numeric_matrix(clr_values)
  if (any(!is.finite(clr_values)) || any(abs(rowSums(clr_values)) > tolerance)) {
    lomm_comp_stop("CLR rows must be finite and sum to zero within tolerance.")
  }
  lomm_softmax_rows(clr_values - rowMeans(clr_values))
}

lomm_ilr_basis <- function(component_names) {
  component_names <- as.character(component_names)
  if (length(component_names) < 2L || anyNA(component_names) ||
      any(!nzchar(component_names)) || anyDuplicated(component_names)) {
    lomm_comp_stop("ILR basis component names must be present and unique.")
  }
  dimension <- length(component_names)
  basis <- matrix(0, nrow = dimension, ncol = dimension - 1L)
  for (coordinate in seq_len(dimension - 1L)) {
    denominator <- sqrt(coordinate * (coordinate + 1))
    basis[seq_len(coordinate), coordinate] <- 1 / denominator
    basis[coordinate + 1L, coordinate] <- -coordinate / denominator
  }
  rownames(basis) <- component_names
  colnames(basis) <- sprintf("ilr_%02d", seq_len(ncol(basis)))
  basis
}

lomm_validate_ilr_basis <- function(basis, component_names, tolerance = 1e-12) {
  basis <- as.matrix(basis)
  component_names <- as.character(component_names)
  expected_dimensions <- c(length(component_names), length(component_names) - 1L)
  if (!is.numeric(basis) || !identical(dim(basis), expected_dimensions) ||
      any(!is.finite(basis))) {
    lomm_comp_stop("ILR basis has invalid dimensions or values.")
  }
  if (!identical(rownames(basis), component_names) ||
      is.null(colnames(basis)) || anyDuplicated(colnames(basis))) {
    lomm_comp_stop("ILR basis names do not match the locked component order.")
  }
  if (max(abs(colSums(basis))) > tolerance ||
      max(abs(crossprod(basis) - diag(ncol(basis)))) > tolerance) {
    lomm_comp_stop("ILR basis is not an orthonormal contrast basis.")
  }
  invisible(TRUE)
}

lomm_ilr <- function(probabilities, basis, tolerance = 1e-10) {
  lomm_validate_positive_closed(probabilities, tolerance)
  lomm_validate_ilr_basis(basis, colnames(probabilities))
  output <- log(as.matrix(probabilities)) %*% basis
  colnames(output) <- colnames(basis)
  output
}

lomm_ilr_inverse <- function(ilr_values, basis) {
  ilr_values <- lomm_require_numeric_matrix(ilr_values, minimum_columns = 1L)
  lomm_validate_ilr_basis(basis, rownames(basis))
  if (!identical(colnames(ilr_values), colnames(basis)) ||
      ncol(ilr_values) != ncol(basis) || any(!is.finite(ilr_values))) {
    lomm_comp_stop("ILR coordinates do not match the locked basis.")
  }
  clr_values <- ilr_values %*% t(basis)
  colnames(clr_values) <- rownames(basis)
  lomm_softmax_rows(clr_values)
}
