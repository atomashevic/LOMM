#!/usr/bin/env Rscript

lomm_test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the public LOMM observation-profile tests.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM_TEST_PATH <- lomm_test_script_path()
LOMM_TEST_DIRECTORY <- dirname(LOMM_TEST_PATH)
LOMM_TEST_REPO <- normalizePath(
  file.path(LOMM_TEST_DIRECTORY, "..", ".."), mustWork = TRUE
)
source(file.path(LOMM_TEST_DIRECTORY, "observation_profile.R"))

lomm_test_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

lomm_test_error <- function(expression, pattern = NULL) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = identity)
  if (is.null(error)) stop("Expected an error.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    stop("Unexpected error: ", conditionMessage(error), call. = FALSE)
  }
  invisible(error)
}

lomm_test_clone <- function(value) {
  unserialize(serialize(value, NULL))
}

lomm_test_fixture <- function() {
  component_names <- c(
    "anger", "disgust", "fear", "happiness", "neutral", "sadness",
    "surprise", "contempt", "anxiety", "helplessness", "disappointment"
  )
  basis <- stats::contr.helmert(length(component_names))
  basis <- sweep(basis, 2L, sqrt(colSums(basis^2)), "/")
  rownames(basis) <- component_names
  colnames(basis) <- sprintf("ilr_%02d", seq_len(ncol(basis)))
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
  empirical_covariance <- diag(seq(0.03, 0.12, length.out = 10L))
  dimnames(empirical_covariance) <- list(coordinate_names, coordinate_names)
  list(
    basis = basis,
    loadings = loadings,
    latent = latent,
    residuals = residuals,
    center = center,
    empirical_covariance = empirical_covariance
  )
}

lomm_test_config_contract <- function(config) {
  lomm_validate_observation_profile_config(
    config, LOMM_TEST_REPO, validate_internal_hashes = TRUE
  )
  default <- lomm_default_observation_profile(config)
  lomm_test_assert(
    identical(default$profile, "compositional_bs") &&
      identical(default$baseline_mode, "empirical") &&
      isTRUE(default$baseline_enabled) &&
      isTRUE(default$clip_scale_enabled) &&
      identical(default$activity_gate_enabled, FALSE) &&
      identical(default$rng_state_version, LOMM_RNG_STATE_VERSION) &&
      identical(default$internal_profile_id, LOMM_INTERNAL_PROFILE_ID) &&
      identical(default$internal_profile_role, "calibration_ancestry"),
    "The public default is not compositional B+S with the retained internal ID."
  )
  loading <- lomm_resolve_observation_profile("loading_aligned_bs", config = config)
  homogeneous <- lomm_resolve_observation_profile("homogeneous", config = config)
  s_only <- lomm_resolve_observation_profile(
    "compositional_bs", baseline_mode = "none", config = config
  )
  lomm_test_assert(
    identical(loading$baseline_mode, "loading_aligned") &&
      isTRUE(loading$clip_scale_enabled) &&
      identical(homogeneous$baseline_mode, "none") &&
      identical(homogeneous$clip_scale_enabled, FALSE) &&
      identical(s_only$baseline_enabled, FALSE) &&
      isTRUE(s_only$clip_scale_enabled),
    "The loading-aligned, homogeneous, or S-only resolution is invalid."
  )
  lomm_test_error(
    lomm_resolve_observation_profile("missing", config = config),
    "Unknown public LOMM observation profile"
  )
  lomm_test_error(
    lomm_resolve_observation_profile(
      "compositional_bs", baseline_mode = "loading_space", config = config
    ),
    "Unknown LOMM baseline mode"
  )
  changed <- lomm_test_clone(config)
  changed$default_profile <- "homogeneous"
  lomm_test_error(
    lomm_validate_observation_profile_config(changed), "Default profile"
  )
  changed <- lomm_test_clone(config)
  changed$baseline_modes$empirical$law <- "laplace"
  lomm_test_error(
    lomm_validate_observation_profile_config(changed), "Baseline mode empirical"
  )
  changed <- lomm_test_clone(config)
  changed$clip_scale$lower <- 0.1
  changed$clip_scale$upper <- 1.9
  lomm_test_error(
    lomm_validate_observation_profile_config(changed), "Clip-scale bounds"
  )
  changed <- lomm_test_clone(config)
  changed$clip_scale <- append(changed$clip_scale, list(upper = 99))
  lomm_test_error(
    lomm_validate_observation_profile_config(changed),
    "Clip-scale contract fields differ from the public schema"
  )
  changed <- lomm_test_clone(config)
  changed$internal_source_contract$calibration_run_config_hash <-
    paste(rep("0", 64L), collapse = "")
  lomm_test_error(
    lomm_validate_observation_profile_config(changed),
    "Calibration run config hash"
  )
  changed <- lomm_test_clone(config)
  changed$clip_scale <- changed$clip_scale[c(
    "upper", "lower", "law", "population_mean", "placement"
  )]
  lomm_test_error(
    lomm_resolve_observation_profile(config = changed),
    "loaded public observation-profile config changed in memory"
  )
  invisible(TRUE)
}

lomm_test_empirical_effects <- function(config, fixture) {
  first <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-001",
    master_seed = 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  second <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-001",
    master_seed = 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  collision_candidate <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-36981",
    master_seed = 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  lomm_test_assert(
    identical(first, second) && any(abs(first$baseline) > 0) &&
      first$clip_scale >= 0.5 && first$clip_scale <= 1.5 &&
      !identical(first$baseline_rng_state, collision_candidate$baseline_rng_state),
    "Empirical B+S effects are not deterministic, active, and in range."
  )
  expected_baseline_state <- c(
    10407L, 1512252661L, -623395706L, 1573136511L, 773281170L,
    -359807536L, 415756503L
  )
  expected_scale_state <- c(
    10407L, 1584580826L, 2018733980L, -47007774L, -858930860L,
    -987292841L, -1585470213L
  )
  expected_baseline <- c(
    0.0849308274402505, -0.00898387581730958, 0.0184705278987285,
    0.135346012302175, -0.0165426671241254, 0.0997634333997771,
    0.307718306860862, -0.0986050667226749, 0.362944225922990,
    0.576175474146567
  )
  lomm_test_assert(
    identical(first$baseline_rng_state, expected_baseline_state) &&
      identical(first$clip_scale_rng_state, expected_scale_state) &&
      max(abs(unname(first$baseline) - expected_baseline)) < 1e-14 &&
      abs(first$clip_scale - 0.554311823634631) < 1e-14,
    "The versioned public RNG stream or golden B/S draw changed."
  )
  changed_covariance <- fixture$empirical_covariance
  changed_covariance[1L, 1L] <- changed_covariance[1L, 1L] + 0.01
  changed_geometry <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-001",
    master_seed = 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = changed_covariance,
    config = config
  )
  lomm_test_assert(
    !identical(
      first$baseline_geometry_sha256,
      changed_geometry$baseline_geometry_sha256
    ),
    "The baseline-geometry hash did not identify a covariance change."
  )
  permutation <- rev(seq_along(first$baseline))
  permuted_covariance <- fixture$empirical_covariance[
    permutation, permutation, drop = FALSE
  ]
  permuted <- lomm_draw_clip_effects(
    coordinate_names = rownames(permuted_covariance),
    clip_key = "clip-001",
    master_seed = 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = permuted_covariance,
    config = config
  )
  lomm_test_assert(
    max(abs(first$baseline - permuted$baseline[names(first$baseline)])) < 1e-14 &&
      identical(
        first$baseline_geometry_sha256,
        permuted$baseline_geometry_sha256
      ),
    "Named empirical effects changed under a consistent coordinate permutation."
  )
  set.seed(20260812L)
  caller_state <- .Random.seed
  invisible(lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "rng-restoration",
    master_seed = 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  ))
  lomm_test_assert(
    identical(.Random.seed, caller_state),
    "The public observation-profile draw changed the caller RNG state."
  )
  invalid_names <- fixture$empirical_covariance
  rownames(invalid_names)[1L] <- "wrong"
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "clip-001", 20260812L,
      profile = "compositional_bs",
      between_clip_covariance = invalid_names,
      config = config
    ),
    "named, finite, symmetric covariance"
  )
  indefinite <- fixture$empirical_covariance
  indefinite[1L, 1L] <- -1
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "clip-001", 20260812L,
      profile = "compositional_bs",
      between_clip_covariance = indefinite,
      config = config
    ),
    "positive semidefinite"
  )
  invisible(first)
}

lomm_test_paired_homogeneous <- function(config, fixture, empirical_effects) {
  homogeneous <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-001",
    master_seed = 20260812L,
    profile = "homogeneous",
    between_clip_covariance = fixture$empirical_covariance,
    paired_reference_mode = "empirical",
    config = config
  )
  lomm_test_assert(
    identical(homogeneous$baseline_draw, empirical_effects$baseline_draw) &&
      identical(homogeneous$clip_scale_draw, empirical_effects$clip_scale_draw) &&
      identical(homogeneous$baseline_reference_mode, "empirical") &&
      identical(
        homogeneous$baseline_geometry_sha256,
        empirical_effects$baseline_geometry_sha256
      ) &&
      all(homogeneous$baseline == 0) && identical(homogeneous$clip_scale, 1),
    "The homogeneous profile does not retain paired B/S draws while disabling them."
  )
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "clip-001", 20260812L,
      profile = "compositional_bs",
      between_clip_covariance = fixture$empirical_covariance,
      paired_reference_mode = "empirical",
      config = config
    ),
    "only valid when baseline mode is none"
  )
  invisible(TRUE)
}

lomm_test_loading_aligned <- function(config, fixture) {
  effects <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-loading",
    master_seed = 20260812L,
    profile = "loading_aligned_bs",
    loadings = fixture$loadings,
    beta_sd = 0.4,
    config = config
  )
  projector <- fixture$loadings %*%
    solve(crossprod(fixture$loadings)) %*% t(fixture$loadings)
  residual <- effects$baseline - as.numeric(effects$baseline %*% projector)
  lomm_test_assert(
    max(abs(residual)) < 1e-12 && !is.null(effects$beta),
    "The loading-aligned baseline leaves the loading column space."
  )
  factor_permutation <- rev(seq_len(ncol(fixture$loadings)))
  permuted_loadings <- fixture$loadings[, factor_permutation, drop = FALSE]
  permuted_effects <- lomm_draw_clip_effects(
    coordinate_names = rownames(permuted_loadings),
    clip_key = "clip-loading",
    master_seed = 20260812L,
    profile = "loading_aligned_bs",
    loadings = permuted_loadings,
    beta_sd = 0.4,
    config = config
  )
  lomm_test_assert(
    max(abs(effects$baseline - permuted_effects$baseline)) < 1e-14 &&
      identical(
        effects$baseline_geometry_sha256,
        permuted_effects$baseline_geometry_sha256
      ),
    "Loading-aligned effects changed under a consistent factor permutation."
  )
  data_frame_effects <- lomm_draw_clip_effects(
    coordinate_names = rownames(fixture$loadings),
    clip_key = "clip-loading",
    master_seed = 20260812L,
    profile = "loading_aligned_bs",
    loadings = as.data.frame(fixture$loadings),
    beta_sd = 0.4,
    config = config
  )
  lomm_test_assert(
    identical(effects$baseline, data_frame_effects$baseline),
    "Numeric data-frame loadings were not normalized to the public matrix form."
  )
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "clip-loading", 20260812L,
      profile = "loading_aligned_bs", loadings = fixture$loadings,
      config = config
    ),
    "requires beta_covariance or beta_sd"
  )
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "clip-loading", 20260812L,
      profile = "loading_aligned_bs", loadings = fixture$loadings,
      beta_sd = stats::setNames(rep(0.4, 3L), rev(colnames(fixture$loadings))),
      config = config
    ),
    "named in loading-factor order"
  )
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "clip-loading", 20260812L,
      profile = "loading_aligned_bs", loadings = fixture$loadings,
      beta_sd = factor("0.4"), config = config
    ),
    "Beta SD must be numeric"
  )
  invisible(TRUE)
}

lomm_test_observation_surfaces <- function(config, fixture) {
  bounded <- lomm_observe(
    latent_positions = fixture$latent,
    loadings = fixture$loadings,
    residuals = fixture$residuals,
    center = fixture$center,
    basis = fixture$basis,
    clip_key = "clip-bounded",
    master_seed = 20260812L,
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  roundtrip <- lomm11_canonical_probability_ilr(
    bounded$probability, fixture$basis
  )
  required_metadata <- c(
    "model_name", "public_schema_version", "public_api_version",
    "public_config_source_sha256", "public_effective_config_sha256",
    "public_implementation_sha256", "public_profile", "internal_profile_id",
    "internal_profile_role", "internal_schema_version", "internal_config_sha256",
    "internal_implementation_sha256", "baseline_mode",
    "baseline_enabled", "clip_scale_enabled", "activity_gate_enabled",
    "loading_aligned_truth", "observation_surface",
    "default_observation_surface", "rng_state_version", "rng_state_derivation", "clip_key",
    "master_seed", "baseline_rng_key", "baseline_rng_state",
    "clip_scale_rng_key", "clip_scale_rng_state", "baseline_reference_mode",
    "coordinate_names_sha256", "loading_matrix_sha256", "basis_sha256",
    "baseline_geometry_sha256", "kappa", "tau"
  )
  lomm_test_assert(
    max(abs(rowSums(bounded$probability) - 1)) < 1e-14 &&
      all(bounded$probability > 0) &&
      max(abs(roundtrip - bounded$ilr)) < 1e-12 &&
      identical(names(bounded$metadata), required_metadata) &&
      identical(bounded$metadata$public_profile, "compositional_bs") &&
      identical(bounded$metadata$internal_profile_role, "calibration_ancestry") &&
      identical(bounded$metadata$rng_state_version, LOMM_RNG_STATE_VERSION) &&
      identical(bounded$metadata$observation_surface, "bounded_composition"),
    "The default bounded surface or mandatory metadata is invalid."
  )
  flipped_basis <- fixture$basis
  flipped_basis[, 1L] <- -flipped_basis[, 1L]
  flipped <- lomm_observe(
    latent_positions = fixture$latent,
    loadings = fixture$loadings,
    residuals = fixture$residuals,
    center = fixture$center,
    basis = flipped_basis,
    clip_key = "clip-bounded",
    master_seed = 20260812L,
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  lomm_test_assert(
    !identical(bounded$metadata$basis_sha256, flipped$metadata$basis_sha256),
    "The ILR-basis hash did not identify a valid sign change."
  )
  data_frame_basis <- lomm_observe(
    latent_positions = fixture$latent,
    loadings = as.data.frame(fixture$loadings),
    residuals = fixture$residuals,
    center = fixture$center,
    basis = as.data.frame(fixture$basis),
    clip_key = "clip-bounded",
    master_seed = 20260812L,
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  lomm_test_assert(
    identical(bounded$probability, data_frame_basis$probability),
    "Numeric data-frame basis or loadings were not normalized to matrices."
  )
  homogeneous <- lomm_observe(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    profile = "homogeneous", clip_key = "clip-homogeneous",
    master_seed = 20260812L, config = config
  )
  expected <- fixture$latent %*% t(fixture$loadings) + fixture$residuals
  expected <- sweep(expected, 2L, fixture$center, "+")
  lomm_test_assert(
    max(abs(homogeneous$contrast - expected)) < 1e-15 &&
      identical(homogeneous$metadata$observation_surface, "linear_contrast"),
    "The homogeneous profile is not the exact nested linear contrast."
  )
  lomm_test_error(
    lomm_observe(
      fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
      clip_key = "missing-basis", master_seed = 20260812L,
      between_clip_covariance = fixture$empirical_covariance, config = config
    ),
    "bounded_composition surface requires an ILR basis"
  )
  lomm_test_error(
    lomm_observe(
      fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
      basis = fixture$basis, profile = "homogeneous", clip_key = "wrong-surface",
      master_seed = 20260812L, config = config
    ),
    "linear_contrast surface does not accept an ILR basis"
  )
  s_only <- lomm_observe(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    profile = "compositional_bs", baseline_mode = "none",
    clip_scale_enabled = TRUE, observation_surface = "linear_contrast",
    clip_key = "clip-scale",
    master_seed = 20260812L, config = config
  )
  unscaled <- lomm_observe(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    profile = "homogeneous", clip_key = "clip-scale",
    master_seed = 20260812L, config = config
  )
  centered_s <- sweep(s_only$contrast, 2L, fixture$center, "-")
  centered_unscaled <- sweep(unscaled$contrast, 2L, fixture$center, "-")
  lomm_test_assert(
    max(abs(centered_s - s_only$effects$clip_scale * centered_unscaled)) < 1e-14,
    "Clip scale does not multiply both dynamic signal and residual variation."
  )
  loading <- lomm_observe(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    profile = "loading_aligned_bs", clip_key = "clip-loading",
    master_seed = 20260812L, beta_sd = 0.4, config = config
  )
  lomm_test_assert(
    isTRUE(loading$metadata$loading_aligned_truth) &&
      identical(loading$metadata$observation_surface, "linear_contrast"),
    "Loading-aligned metadata does not expose the known-truth geometry."
  )
  lomm_test_error(
    lomm_observe(
      fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
      profile = "homogeneous", clip_key = "bad-temperature",
      master_seed = 20260812L, tau = 0, config = config
    ),
    "Temperature must be one positive finite number"
  )
  lomm_test_error(
    lomm_observe(
      fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
      profile = lomm_default_observation_profile(config),
      baseline_mode = "none", clip_key = "conflict", master_seed = 1L,
      config = config
    ),
    "Resolved profiles cannot be combined with mode overrides"
  )
  resolved <- lomm_default_observation_profile(config)
  resolved$baseline_enabled <- FALSE
  lomm_test_error(
    lomm_draw_clip_effects(
      rownames(fixture$loadings), "mutated-resolved", 20260812L,
      profile = resolved,
      between_clip_covariance = fixture$empirical_covariance,
      config = config
    ),
    "Resolved profile differs from the loaded public contract"
  )
  lomm_test_error(
    lomm_observe(
      fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
      basis = fixture$basis, profile = resolved, clip_key = "mutated-resolved",
      master_seed = 20260812L,
      between_clip_covariance = fixture$empirical_covariance,
      config = config
    ),
    "Resolved profile differs from the loaded public contract"
  )
  for (field in c(
    "rng_state_version", "internal_profile_id", "public_effective_config_sha256"
  )) {
    changed_resolved <- lomm_default_observation_profile(config)
    changed_resolved[[field]] <- paste0("changed-", field)
    lomm_test_error(
      lomm_draw_clip_effects(
        rownames(fixture$loadings), paste0("mutated-", field), 20260812L,
        profile = changed_resolved,
        between_clip_covariance = fixture$empirical_covariance,
        config = config
      ),
      "Resolved profile differs from the loaded public contract"
    )
  }
  invisible(TRUE)
}

lomm_test_loaded_provenance <- function(config, fixture) {
  lomm_test_assert(
    identical(
      LOMM_PUBLIC_IMPLEMENTATION_SHA256,
      digest::digest(file = LOMM_OBSERVATION_PROFILE_SCRIPT, algo = "sha256")
    ),
    "The public implementation hash was not captured at module load."
  )
  temporary_config <- tempfile(fileext = ".json")
  file.copy(lomm_default_profile_config_path(), temporary_config)
  on.exit(unlink(temporary_config), add = TRUE)
  loaded <- lomm_read_observation_profile_config(temporary_config)
  loaded_source_sha256 <- attr(loaded, "source_sha256", exact = TRUE)
  write(" ", file = temporary_config, append = TRUE)
  observed <- lomm_observe(
    fixture$latent, fixture$loadings, fixture$residuals, fixture$center,
    basis = fixture$basis, clip_key = "loaded-config", master_seed = 20260812L,
    between_clip_covariance = fixture$empirical_covariance, config = loaded
  )
  lomm_test_assert(
    identical(
      observed$metadata$public_config_source_sha256, loaded_source_sha256
    ) && !identical(
      loaded_source_sha256,
      digest::digest(file = temporary_config, algo = "sha256")
    ),
    "Observation metadata did not retain the loaded config-byte hash."
  )
  forged <- lomm_test_clone(loaded)
  attr(forged, "source_sha256") <- paste(rep("0", 64L), collapse = "")
  lomm_test_error(
    lomm_default_observation_profile(forged),
    "loaded public observation-profile identity changed"
  )
  invisible(TRUE)
}

lomm_test_rejects_unverified_internal_source <- function(config, fixture) {
  temporary_repo <- tempfile("lomm-unverified-")
  wrapper_directory <- file.path(temporary_repo, "scripts", "lomm")
  internal_directory <- file.path(temporary_repo, "scripts", "lomm_1_1")
  dir.create(wrapper_directory, recursive = TRUE)
  dir.create(internal_directory, recursive = TRUE)
  on.exit(unlink(temporary_repo, recursive = TRUE), add = TRUE)
  copied <- c(
    file.copy(
      LOMM_OBSERVATION_PROFILE_SCRIPT,
      file.path(wrapper_directory, "observation_profile.R")
    ),
    file.copy(
      LOMM_INTERNAL_IMPLEMENTATION,
      file.path(internal_directory, "canonical_observation_profile.R")
    ),
    file.copy(
      lomm_default_profile_config_path(),
      file.path(wrapper_directory, "observation_profile_config.json")
    ),
    file.copy(
      LOMM_INTERNAL_CONFIG,
      file.path(internal_directory, "canonical_observation_profile_config.json")
    )
  )
  lomm_test_assert(
    all(copied), "Could not construct the unverified-source test fixture."
  )
  verified_environment <- new.env(parent = globalenv())
  sys.source(
    file.path(wrapper_directory, "observation_profile.R"),
    envir = verified_environment
  )
  captured_public_hash <- get(
    "LOMM_PUBLIC_IMPLEMENTATION_SHA256",
    envir = verified_environment,
    inherits = FALSE
  )
  lomm_test_assert(
    identical(
      get("LOMM_REPO_ROOT", envir = verified_environment, inherits = FALSE),
      normalizePath(temporary_repo, mustWork = TRUE)
    ),
    "A sourced wrapper did not bind to its own repository root."
  )
  isolated_config <- verified_environment$lomm_read_observation_profile_config()
  isolated <- verified_environment$lomm_draw_clip_effects(
    rownames(fixture$loadings), "receipt-free", 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = fixture$empirical_covariance,
    config = isolated_config
  )
  original <- lomm_draw_clip_effects(
    rownames(fixture$loadings), "receipt-free", 20260812L,
    profile = "compositional_bs",
    between_clip_covariance = fixture$empirical_covariance,
    config = config
  )
  lomm_test_assert(
    identical(isolated, original),
    "The isolated public model differs without local evidence or run outputs."
  )
  chdir_environment <- new.env(parent = globalenv())
  source(
    file.path(wrapper_directory, "observation_profile.R"),
    local = chdir_environment,
    chdir = TRUE
  )
  lomm_test_assert(
    identical(
      get("LOMM_REPO_ROOT", envir = chdir_environment, inherits = FALSE),
      normalizePath(temporary_repo, mustWork = TRUE)
    ),
    "A chdir source call did not bind to its own repository root."
  )
  write(
    "# post-load file change",
    file = file.path(wrapper_directory, "observation_profile.R"),
    append = TRUE
  )
  lomm_test_assert(
    !identical(
      captured_public_hash,
      digest::digest(
        file = file.path(wrapper_directory, "observation_profile.R"),
        algo = "sha256"
      )
    ),
    "The test fixture did not distinguish loaded and later wrapper bytes."
  )
  marker <- ".lomm_unverified_internal_executed"
  if (exists(marker, envir = .GlobalEnv, inherits = FALSE)) {
    rm(list = marker, envir = .GlobalEnv)
  }
  write(
    paste0("assign(\"", marker, "\", TRUE, envir = .GlobalEnv)"),
    file = file.path(internal_directory, "canonical_observation_profile.R"),
    append = TRUE
  )
  previous_directory <- getwd()
  setwd(temporary_repo)
  on.exit(setwd(previous_directory), add = TRUE)
  lomm_test_error(
    sys.source(
      file.path(wrapper_directory, "observation_profile.R"),
      envir = new.env(parent = globalenv())
    ),
    "retained internal canonical observation source changed"
  )
  lomm_test_assert(
    !exists(marker, envir = .GlobalEnv, inherits = FALSE),
    "An unverified internal implementation executed before its hash check."
  )
  invisible(TRUE)
}

config <- lomm_read_observation_profile_config()
fixture <- lomm_test_fixture()
lomm_test_config_contract(config)
empirical_effects <- lomm_test_empirical_effects(config, fixture)
lomm_test_paired_homogeneous(config, fixture, empirical_effects)
lomm_test_loading_aligned(config, fixture)
lomm_test_observation_surfaces(config, fixture)
lomm_test_loaded_provenance(config, fixture)
lomm_test_rejects_unverified_internal_source(config, fixture)

module_environment <- new.env(parent = globalenv())
sys.source(file.path(LOMM_TEST_DIRECTORY, "observation_profile.R"),
           envir = module_environment)
lomm_test_assert(
  exists("lomm_observe", envir = module_environment, inherits = FALSE) &&
    exists("lomm11_canonical_observe", envir = module_environment, inherits = FALSE),
  "The public wrapper is not module-safe under sys.source()."
)

cat("Public LOMM observation-profile tests: PASS\n")
