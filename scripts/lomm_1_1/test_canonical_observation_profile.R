#!/usr/bin/env Rscript

lomm11_canonical_test_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the canonical observation-profile tests.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_CANONICAL_TEST_PATH <- lomm11_canonical_test_path()
LOMM11_CANONICAL_TEST_DIRECTORY <- dirname(LOMM11_CANONICAL_TEST_PATH)
LOMM11_CANONICAL_TEST_REPO <- normalizePath(
  file.path(LOMM11_CANONICAL_TEST_DIRECTORY, "..", ".."), mustWork = TRUE
)
LOMM11_CANONICAL_IMPLEMENTATION <- file.path(
  LOMM11_CANONICAL_TEST_DIRECTORY, "canonical_observation_profile.R"
)
LOMM11_CANONICAL_CONFIG <- file.path(
  LOMM11_CANONICAL_TEST_DIRECTORY, "canonical_observation_profile_config.json"
)

source(LOMM11_CANONICAL_IMPLEMENTATION)

lomm11_canonical_test_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

lomm11_canonical_test_error <- function(expression, pattern = NULL) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  if (is.null(error)) stop("Expected an error.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    stop("Unexpected error: ", conditionMessage(error), call. = FALSE)
  }
  invisible(error)
}

lomm11_canonical_test_clone <- function(value) {
  unserialize(serialize(value, NULL))
}

lomm11_canonical_test_basis <- function() {
  channels <- c(
    "anger", "disgust", "fear", "happiness", "neutral", "sadness",
    "surprise", "contempt", "anxiety", "helplessness", "disappointment"
  )
  basis <- stats::contr.helmert(length(channels))
  basis <- sweep(basis, 2L, sqrt(colSums(basis^2)), "/")
  rownames(basis) <- channels
  colnames(basis) <- sprintf("ilr_%02d", seq_len(ncol(basis)))
  basis
}

lomm11_canonical_test_fixture <- function() {
  basis <- lomm11_canonical_test_basis()
  coordinate_names <- colnames(basis)
  factor_names <- sprintf("factor_%02d", 1:3)
  loadings <- matrix(
    c(
      1.00, 0.00, 0.00,
      0.25, 0.85, 0.00,
      0.00, 0.20, 1.10,
      0.35, 0.00, 0.15,
      0.00, 0.30, 0.10,
      0.15, 0.15, 0.00,
      0.10, 0.00, 0.25,
      0.00, 0.20, 0.20,
      0.20, 0.10, 0.00,
      0.05, 0.15, 0.20
    ),
    nrow = length(coordinate_names), ncol = length(factor_names), byrow = TRUE,
    dimnames = list(coordinate_names, factor_names)
  )
  latent <- matrix(
    c(
      -0.50, 0.20, 0.10,
      -0.10, 0.40, 0.25,
      0.30, 0.15, 0.40,
      0.55, -0.10, 0.20,
      0.20, -0.35, -0.10,
      -0.25, -0.20, -0.30
    ),
    nrow = 6L, ncol = 3L, byrow = TRUE,
    dimnames = list(sprintf("frame_%02d", 1:6), factor_names)
  )
  residuals <- outer(seq_len(nrow(latent)), seq_along(coordinate_names), function(i, j) {
    (i - j) / 1000
  })
  dimnames(residuals) <- list(rownames(latent), coordinate_names)
  center <- stats::setNames(seq(-0.09, 0.09, length.out = 10L), coordinate_names)
  baseline <- stats::setNames(seq(0.04, -0.04, length.out = 10L), coordinate_names)
  list(
    basis = basis,
    loadings = loadings,
    latent = latent,
    residuals = residuals,
    center = center,
    baseline = baseline
  )
}

lomm11_canonical_test_config_contract <- function(config) {
  preset <- config$calibrated_preset
  lomm11_canonical_test_assert(
    identical(preset$variant_id, "110") && isTRUE(preset$B) &&
      isTRUE(preset$S) && identical(preset$G, FALSE) &&
      identical(preset$period_frames, 12L) && identical(preset$damping_ratio, 0.5) &&
      identical(preset$kappa, 1) && identical(preset$core_candidate_id, "CORE017"),
    "The MAFW/CLIP B+S preset is not locked to P=12, xi=0.5, kappa=1."
  )
  changed <- lomm11_canonical_test_clone(config)
  changed$calibrated_preset$G <- TRUE
  lomm11_canonical_test_error(
    lomm11_canonical_validate_config(changed), "Preset component flags"
  )
  changed <- lomm11_canonical_test_clone(config)
  changed$calibrated_preset$kappa <- 0.5
  lomm11_canonical_test_error(lomm11_canonical_validate_config(changed), "Preset kappa")
  changed <- lomm11_canonical_test_clone(config)
  changed$observation_model$clip_scale$lower <- 0.4
  lomm11_canonical_test_error(
    lomm11_canonical_validate_config(changed), "mean one"
  )
  changed <- lomm11_canonical_test_clone(config)
  changed$unexpected <- TRUE
  lomm11_canonical_test_error(
    lomm11_canonical_validate_config(changed), "Canonical profile fields"
  )
  invisible(TRUE)
}

lomm11_canonical_test_identified_link <- function(fixture) {
  lomm11_canonical_validate_basis(fixture$basis)
  output <- lomm11_canonical_observe(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    fixture$baseline, clip_scale = 1.25, fixture$basis, kappa = 1, tau = 1
  )
  lomm11_canonical_test_assert(
    max(abs(rowSums(output$probability) - 1)) < 1e-14 &&
      all(output$probability > 0) && max(abs(rowSums(output$clr))) < 1e-12,
    "The bounded output is not a positive closed composition."
  )
  roundtrip <- lomm11_canonical_probability_ilr(output$probability, fixture$basis)
  lomm11_canonical_test_assert(
    max(abs(roundtrip - output$ilr)) < 1e-12,
    "The probability-to-ILR round trip is not identified."
  )
  logits <- output$ilr %*% t(fixture$basis)
  colnames(logits) <- rownames(fixture$basis)
  unshifted <- lomm11_canonical_softmax(logits)
  shifted <- lomm11_canonical_softmax(logits, common_shift = seq(-4, 4, length.out = 6L))
  lomm11_canonical_test_assert(
    max(abs(unshifted - shifted)) < 1e-15,
    "Softmax changed under a row-specific common-logit shift."
  )
  invalid_basis <- fixture$basis
  invalid_basis[1L, 1L] <- invalid_basis[1L, 1L] + 0.01
  lomm11_canonical_test_error(
    lomm11_canonical_validate_basis(invalid_basis), "orthonormal contrast"
  )
  invisible(TRUE)
}

lomm11_canonical_test_nested_profile <- function(fixture) {
  zero_baseline <- stats::setNames(rep(0, length(fixture$center)), names(fixture$center))
  homogeneous <- lomm11_canonical_contrast_profile(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    zero_baseline, clip_scale = 1, kappa = 1
  )
  expected <- fixture$latent %*% t(fixture$loadings) + fixture$residuals
  expected <- sweep(expected, 2L, fixture$center, "+")
  lomm11_canonical_test_assert(
    max(abs(homogeneous - expected)) < 1e-15,
    "The homogeneous LOMM is not exactly nested at b_i=0 and s_i=1."
  )
  scaled <- lomm11_canonical_contrast_profile(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    zero_baseline, clip_scale = 1.25, kappa = 1
  )
  combined <- lomm11_canonical_contrast_profile(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    fixture$baseline, clip_scale = 1.25, kappa = 1
  )
  baseline_matrix <- matrix(
    fixture$baseline, nrow = nrow(combined), ncol = ncol(combined), byrow = TRUE
  )
  lomm11_canonical_test_assert(
    max(abs((combined - scaled) - baseline_matrix)) < 1e-15,
    "The additive baseline was incorrectly absorbed into clip scale."
  )
  homogeneous_centered <- sweep(homogeneous, 2L, colMeans(homogeneous), "-")
  scaled_centered <- sweep(scaled, 2L, colMeans(scaled), "-")
  lomm11_canonical_test_assert(
    max(abs(scaled_centered - 1.25 * homogeneous_centered)) < 1e-14,
    "Clip scale does not act on within-clip profile variation."
  )
  lomm11_canonical_test_error(
    lomm11_canonical_contrast_profile(
      fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
      zero_baseline, clip_scale = 0, kappa = 1
    ),
    "Clip scale"
  )
  invisible(TRUE)
}

lomm11_canonical_test_block_preservation <- function(fixture) {
  beta <- matrix(
    c(
      1.0, 0.0, -1.0,
      -1.0, 0.0, 1.0,
      0.0, 1.0, -0.5,
      0.0, -1.0, 0.5
    ),
    nrow = 4L, byrow = TRUE,
    dimnames = list(sprintf("clip_%02d", 1:4), colnames(fixture$loadings))
  )
  baseline <- lomm11_canonical_block_baseline(fixture$loadings, beta)
  expected <- beta %*% t(fixture$loadings)
  scales <- stats::setNames(c(0.5, 0.75, 1.25, 1.5), rownames(baseline))
  lomm11_canonical_test_assert(
    max(abs(baseline - expected)) < 1e-15 && max(abs(colMeans(baseline))) < 1e-15,
    "The loading-space baseline does not equal Lambda times beta."
  )
  lomm11_canonical_validate_realized_effects(baseline, scales)
  q_complete <- qr.Q(qr(fixture$loadings), complete = TRUE)
  invalid <- baseline
  invalid[1L, ] <- invalid[1L, ] + q_complete[, ncol(fixture$loadings) + 1L]
  lomm11_canonical_test_error(
    lomm11_canonical_validate_block_baseline(invalid, fixture$loadings),
    "column space"
  )
  lomm11_canonical_test_error(
    lomm11_canonical_validate_realized_effects(baseline, scales * 2),
    "mean-one-scale"
  )
  invalid_loadings <- fixture$loadings
  invalid_loadings[, 3L] <- invalid_loadings[, 2L]
  lomm11_canonical_test_error(
    lomm11_canonical_validate_loadings(
      invalid_loadings, rownames(invalid_loadings)
    ),
    "full column rank"
  )
  invisible(TRUE)
}

lomm11_canonical_test_provenance <- function(config) {
  lomm11_canonical_validate_provenance(config, LOMM11_CANONICAL_TEST_REPO)
  source_rows <- lomm11_profile_provenance_rows(config)
  basis_path <- source_rows$path[source_rows$role == "locked_ilr_basis"]
  saved_basis <- lomm11_canonical_read_basis(
    file.path(LOMM11_CANONICAL_TEST_REPO, basis_path)
  )
  lomm11_canonical_test_assert(
    identical(dim(saved_basis), c(11L, 10L)),
    "The provenance-locked ILR basis does not have 11-by-10 dimensions."
  )
  implementation_hash <- lomm11_profile_file_sha256(LOMM11_CANONICAL_IMPLEMENTATION)
  lomm11_canonical_test_assert(
    identical(implementation_hash, config$provenance$implementation$sha256),
    "The implementation digest is not locked in the profile config."
  )
  changed <- lomm11_canonical_test_clone(config)
  changed$provenance$implementation$sha256 <- paste(rep("0", 64L), collapse = "")
  lomm11_canonical_test_error(
    lomm11_canonical_validate_provenance(changed, LOMM11_CANONICAL_TEST_REPO),
    "implementation hash"
  )
  invisible(TRUE)
}

config <- lomm11_canonical_read_config(LOMM11_CANONICAL_CONFIG)
fixture <- lomm11_canonical_test_fixture()

lomm11_canonical_test_config_contract(config)
lomm11_canonical_test_identified_link(fixture)
lomm11_canonical_test_nested_profile(fixture)
lomm11_canonical_test_block_preservation(fixture)
lomm11_canonical_test_provenance(config)

cat("LOMM 1.1 canonical observation-profile tests: PASS\n")
