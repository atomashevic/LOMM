LOMM11_CANONICAL_PROFILE_SCHEMA_VERSION <- 1L
LOMM11_CANONICAL_PROFILE_ID <- "lomm_1_1_mafw_clip_bs_v1"

LOMM11_CANONICAL_PROVENANCE_FILES <- c(
  "scripts/lomm_1_1/realism_generator.R" =
    "3e43cf3a018e1735dda81f028f78632ee957f62687d56ec17c3ee217552adcaa",
  "scripts/lomm_1_1/realism_ablation_config.json" =
    "46b36c838818d88c77eb9f99fdc085b3a2075cd3f1f1b6ab43573a1d1f8d4259",
  "simulation_outputs/lomm_1_1/empirical_compositional_preprocess/0cdc11154d8a/ilr_basis.csv" =
    "f5a9a2aa842fe5ab49332e7d4bcfb7095a79522c53d2fc9bc9b8b29d720ebdf9",
  "simulation_outputs/lomm_1_1/empirical_realism_ablation_runs/854e8722d647/selected_candidates.csv" =
    "f6a07a0eb6b2f867b5d84cb9df0d0849691f5c014ad7e5c04fab8c6778084278",
  "simulation_outputs/lomm_1_1/empirical_realism_ablation_runs/854e8722d647/_SUCCESS.json" =
    "63db6991f23b706da4e0e0aba001e379374ba131b1bf08746c88b4860ef96460",
  "simulation_outputs/lomm_1_1/empirical_realism_ablation_runs/854e8722d647/run_manifest.json" =
    "84a4f471d312c3379affa96b0d354aca92ebb2f66780a4f39a45f23b1c9d306a"
)

LOMM11_CANONICAL_PROVENANCE_ROLES <- c(
  "calibration_generator",
  "calibration_design",
  "locked_ilr_basis",
  "training_only_selections",
  "completed_run_receipt",
  "completed_run_manifest"
)

lomm11_profile_stop <- function(...) {
  stop(..., call. = FALSE)
}

lomm11_profile_require_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      lomm11_profile_stop("Package '", package, "' is required.")
    }
  }
  invisible(TRUE)
}

lomm11_profile_file_sha256 <- function(path) {
  lomm11_profile_require_packages()
  if (length(path) != 1L || !is.character(path) || !file.exists(path) ||
      isTRUE(file.info(path)$isdir)) {
    lomm11_profile_stop("A readable file is required for SHA-256 validation.")
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm11_profile_require_fields <- function(value, expected, label) {
  if (!is.list(value) || !setequal(names(value), expected)) {
    lomm11_profile_stop(label, " fields differ from the canonical schema.")
  }
  invisible(TRUE)
}

lomm11_profile_scalar_number <- function(value, label, positive = FALSE) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
      (positive && value <= 0)) {
    qualifier <- if (positive) "positive " else ""
    lomm11_profile_stop(label, " must be one ", qualifier, "finite number.")
  }
  as.numeric(value)
}

lomm11_profile_scalar_string <- function(value, label) {
  if (length(value) != 1L || !is.character(value) || is.na(value) ||
      !nzchar(value)) {
    lomm11_profile_stop(label, " must be one non-empty string.")
  }
  value
}

lomm11_profile_scalar_logical <- function(value, label) {
  if (length(value) != 1L || !is.logical(value) || is.na(value)) {
    lomm11_profile_stop(label, " must be one logical value.")
  }
  value
}

lomm11_profile_character_vector <- function(value, label) {
  output <- unname(unlist(value, recursive = FALSE, use.names = FALSE))
  if (!is.character(output) || length(output) < 1L || anyNA(output) ||
      any(!nzchar(output))) {
    lomm11_profile_stop(label, " must contain non-empty strings.")
  }
  output
}

lomm11_profile_validate_relative_path <- function(path, label) {
  path <- lomm11_profile_scalar_string(path, label)
  invalid <- startsWith(path, "/") || grepl("(^|/)\\.\\.(/|$)", path) ||
    grepl("(^|/)\\.(/|$)", path) || grepl("\n", path, fixed = TRUE) ||
    grepl("\r", path, fixed = TRUE)
  if (invalid) {
    lomm11_profile_stop(label, " must be a normalized repository-relative path.")
  }
  invisible(TRUE)
}

lomm11_profile_validate_sha256 <- function(value, label) {
  value <- lomm11_profile_scalar_string(value, label)
  if (!grepl("^[0-9a-f]{64}$", value)) {
    lomm11_profile_stop(label, " must be a lowercase SHA-256 digest.")
  }
  invisible(TRUE)
}

lomm11_profile_expect_number <- function(value, expected, label,
                                         tolerance = 1e-12) {
  observed <- lomm11_profile_scalar_number(value, label)
  if (abs(observed - expected) > tolerance) {
    lomm11_profile_stop(label, " differs from the canonical value.")
  }
  invisible(TRUE)
}

lomm11_profile_expect_value <- function(value, expected, label) {
  if (!identical(value, expected)) {
    lomm11_profile_stop(label, " differs from the canonical value.")
  }
  invisible(TRUE)
}

lomm11_profile_validate_coordinate_contract <- function(coordinates) {
  lomm11_profile_require_fields(
    coordinates,
    c(
      "component_count", "contrast_dimension", "basis",
      "basis_constraints", "identified_coordinate", "bounded_inverse",
      "common_logit_shift"
    ),
    "Coordinate contract"
  )
  lomm11_profile_expect_number(coordinates$component_count, 11, "Component count")
  lomm11_profile_expect_number(coordinates$contrast_dimension, 10, "Contrast dimension")
  lomm11_profile_expect_value(
    coordinates$basis, "locked_orthonormal_ilr", "Coordinate basis"
  )
  constraints <- lomm11_profile_character_vector(
    coordinates$basis_constraints, "Basis constraints"
  )
  expected <- c("crossprod(V)=I", "crossprod(V,1)=0", "fixed_component_order")
  lomm11_profile_expect_value(constraints, expected, "Basis constraints")
  lomm11_profile_expect_value(
    coordinates$identified_coordinate,
    "ilr_V(p_it)=V^T*log(p_it)",
    "Identified coordinate"
  )
  lomm11_profile_expect_value(
    coordinates$bounded_inverse,
    "p_it=softmax(V*u_it/tau)",
    "Bounded inverse"
  )
  lomm11_profile_expect_value(
    coordinates$common_logit_shift,
    "excluded_unidentified_direction",
    "Common-logit-shift rule"
  )
  invisible(TRUE)
}

lomm11_profile_validate_observation_contract <- function(model) {
  lomm11_profile_require_fields(
    model,
    c(
      "contrast_equation", "center", "clip_baseline", "clip_scale",
      "residual_placement", "homogeneous_nested_case"
    ),
    "Observation model"
  )
  lomm11_profile_expect_value(
    model$contrast_equation,
    "u_it=mu+b_i+kappa*s_i*(Lambda*x_it+epsilon_it)",
    "Contrast equation"
  )
  lomm11_profile_require_fields(
    model$center, c("symbol", "constraint", "source"), "Center contract"
  )
  lomm11_profile_expect_value(model$center$symbol, "mu", "Center symbol")
  lomm11_profile_expect_value(
    model$center$constraint, "fixed_in_locked_ilr_basis", "Center constraint"
  )
  lomm11_profile_expect_value(
    model$center$source, "complete_case_training_mean_ilr", "Center source"
  )
  lomm11_profile_require_fields(
    model$clip_baseline,
    c("symbol", "space", "population_mean", "calibration_covariance"),
    "Clip-baseline contract"
  )
  lomm11_profile_expect_value(model$clip_baseline$symbol, "b_i", "Baseline symbol")
  lomm11_profile_expect_value(model$clip_baseline$space, "ilr", "Baseline space")
  lomm11_profile_expect_value(
    model$clip_baseline$population_mean, "zero", "Baseline mean"
  )
  lomm11_profile_expect_value(
    model$clip_baseline$calibration_covariance,
    "half_full_half_diagonal_clip_mean_ilr_covariance",
    "Baseline covariance"
  )
  lomm11_profile_require_fields(
    model$clip_scale,
    c("symbol", "support", "population_mean", "family", "lower", "upper"),
    "Clip-scale contract"
  )
  lomm11_profile_expect_value(model$clip_scale$symbol, "s_i", "Scale symbol")
  lomm11_profile_expect_value(
    model$clip_scale$support, "strictly_positive", "Scale support"
  )
  lomm11_profile_expect_number(model$clip_scale$population_mean, 1, "Scale mean")
  lomm11_profile_expect_value(model$clip_scale$family, "uniform", "Scale family")
  lomm11_canonical_validate_scale_law(
    model$clip_scale$lower, model$clip_scale$upper,
    model$clip_scale$population_mean
  )
  lomm11_profile_expect_value(
    model$residual_placement,
    "epsilon_it_is_inside_kappa_times_s_i",
    "Residual placement"
  )
  lomm11_profile_require_fields(
    model$homogeneous_nested_case,
    c("baseline", "scale", "equation"),
    "Homogeneous nested case"
  )
  lomm11_profile_expect_value(
    model$homogeneous_nested_case$baseline, "b_i=0", "Homogeneous baseline"
  )
  lomm11_profile_expect_value(
    model$homogeneous_nested_case$scale, "s_i=1", "Homogeneous scale"
  )
  lomm11_profile_expect_value(
    model$homogeneous_nested_case$equation,
    "u_it=mu+kappa*(Lambda*x_it+epsilon_it)",
    "Homogeneous equation"
  )
  invisible(TRUE)
}

lomm11_profile_validate_preset <- function(preset) {
  lomm11_profile_require_fields(
    preset,
    c(
      "label", "measurement_surface", "variant_id", "B", "S", "G",
      "period_frames", "damping_ratio", "kappa", "tau", "residual_sd",
      "selected_folds", "core_candidate_id", "latent_position_variance"
    ),
    "Calibrated preset"
  )
  lomm11_profile_expect_value(
    preset$label, "MAFW_retained100_CLIP_oai_large", "Preset label"
  )
  lomm11_profile_expect_value(
    preset$measurement_surface,
    "11_channel_prompt_relative_classifier_probabilities",
    "Measurement surface"
  )
  lomm11_profile_expect_value(preset$variant_id, "110", "Preset variant")
  lomm11_profile_expect_value(
    vapply(c("B", "S", "G"), function(flag) {
      lomm11_profile_scalar_logical(preset[[flag]], paste0(flag, " flag"))
    }, logical(1)),
    c(B = TRUE, S = TRUE, G = FALSE),
    "Preset component flags"
  )
  lomm11_profile_expect_number(preset$period_frames, 12, "Preset period")
  lomm11_profile_expect_number(preset$damping_ratio, 0.5, "Preset damping ratio")
  lomm11_profile_expect_number(preset$kappa, 1, "Preset kappa")
  lomm11_profile_expect_number(preset$tau, 1, "Preset tau")
  lomm11_profile_expect_number(preset$residual_sd, 0.25, "Preset residual SD")
  folds <- as.integer(unlist(preset$selected_folds, use.names = FALSE))
  lomm11_profile_expect_value(folds, 1:5, "Preset selected folds")
  lomm11_profile_expect_value(
    preset$core_candidate_id, "CORE017", "Preset candidate"
  )
  lomm11_profile_expect_number(
    preset$latent_position_variance, 1, "Latent-position variance"
  )
  invisible(TRUE)
}

lomm11_profile_validate_known_truth <- function(known_truth) {
  lomm11_profile_require_fields(
    known_truth,
    c(
      "baseline_mode", "equation", "beta_population_mean",
      "loading_requirement", "block_preservation"
    ),
    "Known-truth contract"
  )
  lomm11_profile_expect_value(
    known_truth$baseline_mode, "loading_space", "Known-truth baseline mode"
  )
  lomm11_profile_expect_value(
    known_truth$equation, "b_i=Lambda*beta_i", "Known-truth equation"
  )
  lomm11_profile_expect_value(
    known_truth$beta_population_mean, "zero", "Known-truth beta mean"
  )
  lomm11_profile_expect_value(
    known_truth$loading_requirement,
    "fixed_full_column_rank_with_locked_orientation",
    "Known-truth loading requirement"
  )
  lomm11_profile_expect_value(
    known_truth$block_preservation,
    "b_i_lies_in_column_space_of_Lambda",
    "Known-truth block-preservation rule"
  )
  invisible(TRUE)
}

lomm11_profile_validate_provenance_contract <- function(provenance) {
  lomm11_profile_require_fields(
    provenance,
    c(
      "adopted_on", "calibration_run_config_hash", "source_files",
      "implementation"
    ),
    "Provenance contract"
  )
  lomm11_profile_expect_value(provenance$adopted_on, "2026-08-10", "Adoption date")
  lomm11_profile_expect_value(
    provenance$calibration_run_config_hash,
    "854e8722d647ae1564a58459b66f66c87abf91217258fb04d0360ed26d8a4729",
    "Calibration run hash"
  )
  files <- provenance$source_files
  if (!is.list(files) || length(files) != length(LOMM11_CANONICAL_PROVENANCE_FILES)) {
    lomm11_profile_stop("Provenance source-file count differs from the canonical contract.")
  }
  observed <- lapply(seq_along(files), function(index) {
    item <- files[[index]]
    lomm11_profile_require_fields(item, c("path", "sha256", "role"), "Source file")
    lomm11_profile_validate_relative_path(item$path, "Source path")
    lomm11_profile_validate_sha256(item$sha256, "Source SHA-256")
    c(path = item$path, sha256 = item$sha256, role = item$role)
  })
  paths <- vapply(observed, `[[`, character(1), "path")
  hashes <- vapply(observed, `[[`, character(1), "sha256")
  roles <- vapply(observed, `[[`, character(1), "role")
  if (!identical(paths, names(LOMM11_CANONICAL_PROVENANCE_FILES)) ||
      !identical(unname(hashes), unname(LOMM11_CANONICAL_PROVENANCE_FILES)) ||
      !identical(roles, LOMM11_CANONICAL_PROVENANCE_ROLES)) {
    lomm11_profile_stop("Provenance source files differ from the canonical contract.")
  }
  lomm11_profile_require_fields(
    provenance$implementation, c("path", "sha256"), "Implementation provenance"
  )
  lomm11_profile_expect_value(
    provenance$implementation$path,
    "scripts/lomm_1_1/canonical_observation_profile.R",
    "Implementation path"
  )
  lomm11_profile_validate_sha256(
    provenance$implementation$sha256, "Implementation SHA-256"
  )
  invisible(TRUE)
}

lomm11_profile_validate_claim_scope <- function(claim_scope) {
  lomm11_profile_require_fields(
    claim_scope, c("supported", "excluded"), "Claim scope"
  )
  supported <- lomm11_profile_character_vector(claim_scope$supported, "Supported scope")
  excluded <- lomm11_profile_character_vector(claim_scope$excluded, "Excluded scope")
  expected_supported <- c(
    "nested_LOMM_observation_profile",
    "heldout_similarity_to_same_100_clip_MAFW_CLIP_score_sample",
    "known_truth_block_preserving_simulation_option"
  )
  expected_excluded <- c(
    "emotion_factor_validity",
    "classifier_validity",
    "external_dataset_generalization",
    "causal_affect_mechanism"
  )
  lomm11_profile_expect_value(supported, expected_supported, "Supported scope")
  lomm11_profile_expect_value(excluded, expected_excluded, "Excluded scope")
  invisible(TRUE)
}

lomm11_canonical_validate_config <- function(config) {
  lomm11_profile_require_fields(
    config,
    c(
      "schema_version", "profile_id", "status", "coordinate_system",
      "observation_model", "calibrated_preset", "known_truth_recovery",
      "provenance", "claim_scope"
    ),
    "Canonical profile"
  )
  lomm11_profile_expect_number(
    config$schema_version, LOMM11_CANONICAL_PROFILE_SCHEMA_VERSION,
    "Profile schema version"
  )
  lomm11_profile_expect_value(
    config$profile_id, LOMM11_CANONICAL_PROFILE_ID, "Profile ID"
  )
  lomm11_profile_expect_value(
    config$status, "canonical_nested_specification", "Profile status"
  )
  lomm11_profile_validate_coordinate_contract(config$coordinate_system)
  lomm11_profile_validate_observation_contract(config$observation_model)
  lomm11_profile_validate_preset(config$calibrated_preset)
  lomm11_profile_validate_known_truth(config$known_truth_recovery)
  lomm11_profile_validate_provenance_contract(config$provenance)
  lomm11_profile_validate_claim_scope(config$claim_scope)
  invisible(TRUE)
}

lomm11_canonical_read_config <- function(path) {
  lomm11_profile_require_packages()
  config <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  lomm11_canonical_validate_config(config)
  config
}

lomm11_profile_numeric_matrix <- function(value, label) {
  output <- as.matrix(value)
  if (!is.numeric(output) || length(dim(output)) != 2L ||
      any(dim(output) < 1L) || any(!is.finite(output))) {
    lomm11_profile_stop(label, " must be a non-empty finite numeric matrix.")
  }
  storage.mode(output) <- "double"
  output
}

lomm11_profile_named_vector <- function(value, expected_names, label) {
  if (!is.numeric(value) || length(value) != length(expected_names) ||
      any(!is.finite(value)) || !identical(names(value), expected_names)) {
    lomm11_profile_stop(label, " must be finite and match the locked coordinates.")
  }
  stats::setNames(as.numeric(value), expected_names)
}

lomm11_canonical_validate_scale_law <- function(lower, upper,
                                                 expected_mean = 1,
                                                 tolerance = 1e-12) {
  lower <- lomm11_profile_scalar_number(lower, "Scale lower bound", positive = TRUE)
  upper <- lomm11_profile_scalar_number(upper, "Scale upper bound", positive = TRUE)
  expected_mean <- lomm11_profile_scalar_number(
    expected_mean, "Expected scale mean", positive = TRUE
  )
  if (upper <= lower || abs((lower + upper) / 2 - expected_mean) > tolerance) {
    lomm11_profile_stop("Uniform scale law must be positive and mean one.")
  }
  invisible(TRUE)
}

lomm11_canonical_validate_basis <- function(basis, component_names = rownames(basis),
                                            tolerance = 1e-12) {
  basis <- lomm11_profile_numeric_matrix(basis, "ILR basis")
  component_names <- as.character(component_names)
  valid_names <- length(component_names) == nrow(basis) && !anyNA(component_names) &&
    all(nzchar(component_names)) && !anyDuplicated(component_names)
  valid_columns <- !is.null(colnames(basis)) && all(nzchar(colnames(basis))) &&
    !anyDuplicated(colnames(basis))
  if (nrow(basis) != ncol(basis) + 1L || !valid_names || !valid_columns ||
      !identical(rownames(basis), component_names)) {
    lomm11_profile_stop("ILR basis dimensions or names are invalid.")
  }
  tolerance <- lomm11_profile_scalar_number(tolerance, "Basis tolerance", positive = TRUE)
  projection <- diag(nrow(basis)) - matrix(1 / nrow(basis), nrow(basis), nrow(basis))
  if (max(abs(colSums(basis))) > tolerance ||
      max(abs(crossprod(basis) - diag(ncol(basis)))) > tolerance ||
      max(abs(basis %*% t(basis) - projection)) > 10 * tolerance) {
    lomm11_profile_stop("ILR basis is not the required orthonormal contrast basis.")
  }
  invisible(TRUE)
}

lomm11_canonical_read_basis <- function(path) {
  frame <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  coordinate_columns <- grep("^ilr_[0-9]{2}$", names(frame), value = TRUE)
  valid <- all(c("channel", "channel_order") %in% names(frame)) &&
    length(coordinate_columns) == nrow(frame) - 1L &&
    identical(frame$channel_order, seq_len(nrow(frame))) &&
    !anyNA(frame$channel) && all(nzchar(frame$channel)) &&
    !anyDuplicated(frame$channel)
  if (!valid) {
    lomm11_profile_stop("Saved ILR basis schema or channel order is invalid.")
  }
  basis <- as.matrix(frame[, coordinate_columns, drop = FALSE])
  storage.mode(basis) <- "double"
  rownames(basis) <- frame$channel
  lomm11_canonical_validate_basis(basis)
  basis
}

lomm11_canonical_validate_loadings <- function(loadings, coordinate_names,
                                                tolerance = 1e-10) {
  loadings <- lomm11_profile_numeric_matrix(loadings, "Loading matrix")
  factor_names <- colnames(loadings)
  valid_factors <- !is.null(factor_names) && all(nzchar(factor_names)) &&
    !anyDuplicated(factor_names)
  if (nrow(loadings) != length(coordinate_names) || ncol(loadings) >= nrow(loadings) ||
      !identical(rownames(loadings), coordinate_names) || !valid_factors) {
    lomm11_profile_stop("Loading matrix dimensions or names are invalid.")
  }
  tolerance <- lomm11_profile_scalar_number(
    tolerance, "Loading-rank tolerance", positive = TRUE
  )
  if (qr(loadings, tol = tolerance)$rank != ncol(loadings)) {
    lomm11_profile_stop("Loading matrix must have full column rank.")
  }
  invisible(TRUE)
}

lomm11_canonical_softmax <- function(logits, common_shift = 0) {
  logits <- lomm11_profile_numeric_matrix(logits, "Logits")
  if (!is.numeric(common_shift) || any(!is.finite(common_shift)) ||
      !(length(common_shift) %in% c(1L, nrow(logits)))) {
    lomm11_profile_stop("Common shift must be finite and scalar or row-specific.")
  }
  shifted_logits <- logits + rep(common_shift, length.out = nrow(logits))
  shifted_logits <- shifted_logits - apply(shifted_logits, 1L, max)
  exponentiated <- exp(shifted_logits)
  probabilities <- exponentiated / rowSums(exponentiated)
  if (any(!is.finite(probabilities)) || any(probabilities <= 0)) {
    lomm11_profile_stop("Softmax produced invalid simplex values.")
  }
  colnames(probabilities) <- colnames(logits)
  rownames(probabilities) <- rownames(logits)
  probabilities
}

lomm11_canonical_ilr_inverse <- function(ilr, basis) {
  ilr <- lomm11_profile_numeric_matrix(ilr, "ILR coordinates")
  lomm11_canonical_validate_basis(basis)
  if (ncol(ilr) != ncol(basis) || !identical(colnames(ilr), colnames(basis))) {
    lomm11_profile_stop("ILR coordinates do not match the locked basis.")
  }
  logits <- ilr %*% t(basis)
  colnames(logits) <- rownames(basis)
  lomm11_canonical_softmax(logits)
}

lomm11_canonical_probability_ilr <- function(probability, basis,
                                              tolerance = 1e-10) {
  probability <- lomm11_profile_numeric_matrix(probability, "Probabilities")
  lomm11_canonical_validate_basis(basis)
  tolerance <- lomm11_profile_scalar_number(
    tolerance, "Probability tolerance", positive = TRUE
  )
  if (!identical(colnames(probability), rownames(basis)) ||
      any(probability <= 0) || any(probability >= 1) ||
      max(abs(rowSums(probability) - 1)) > tolerance) {
    lomm11_profile_stop("Probabilities must be positive closed rows in basis order.")
  }
  output <- log(probability) %*% basis
  colnames(output) <- colnames(basis)
  rownames(output) <- rownames(probability)
  output
}

lomm11_canonical_contrast_profile <- function(
    latent_positions, loadings, residuals, center, baseline, clip_scale,
    kappa = 1) {
  latent_positions <- lomm11_profile_numeric_matrix(
    latent_positions, "Latent positions"
  )
  residuals <- lomm11_profile_numeric_matrix(residuals, "Residuals")
  coordinate_names <- rownames(loadings)
  lomm11_canonical_validate_loadings(loadings, coordinate_names)
  valid_rows <- !is.null(rownames(latent_positions)) &&
    !anyDuplicated(rownames(latent_positions)) &&
    identical(rownames(latent_positions), rownames(residuals))
  if (!identical(colnames(latent_positions), colnames(loadings)) ||
      nrow(residuals) != nrow(latent_positions) ||
      !identical(colnames(residuals), coordinate_names) || !valid_rows) {
    lomm11_profile_stop("Latent, loading, and residual coordinates do not agree.")
  }
  center <- lomm11_profile_named_vector(center, coordinate_names, "Center")
  baseline <- lomm11_profile_named_vector(baseline, coordinate_names, "Baseline")
  clip_scale <- lomm11_profile_scalar_number(clip_scale, "Clip scale", positive = TRUE)
  kappa <- lomm11_profile_scalar_number(kappa, "Kappa", positive = TRUE)
  signal <- latent_positions %*% t(loadings)
  output <- kappa * clip_scale * (signal + residuals)
  output <- sweep(output, 2L, center + baseline, "+")
  colnames(output) <- coordinate_names
  rownames(output) <- rownames(latent_positions)
  output
}

lomm11_canonical_observe <- function(
    latent_positions, loadings, residuals, center, baseline, clip_scale,
    basis, kappa = 1, tau = 1) {
  lomm11_canonical_validate_basis(basis)
  if (!identical(rownames(loadings), colnames(basis))) {
    lomm11_profile_stop("Loading coordinates do not match the locked ILR basis.")
  }
  tau <- lomm11_profile_scalar_number(tau, "Temperature", positive = TRUE)
  contrast <- lomm11_canonical_contrast_profile(
    latent_positions, loadings, residuals, center, baseline, clip_scale, kappa
  )
  ilr <- contrast / tau
  probability <- lomm11_canonical_ilr_inverse(ilr, basis)
  clr <- ilr %*% t(basis)
  colnames(clr) <- rownames(basis)
  roundtrip <- lomm11_canonical_probability_ilr(probability, basis)
  if (max(abs(roundtrip - ilr)) > 1e-10) {
    lomm11_profile_stop("Bounded observation failed the ILR round trip.")
  }
  list(
    contrast = contrast,
    ilr = ilr,
    clr = clr,
    probability = probability,
    clip_scale = clip_scale,
    kappa = kappa,
    tau = tau
  )
}

lomm11_canonical_block_baseline <- function(loadings, beta,
                                             tolerance = 1e-10) {
  coordinate_names <- rownames(loadings)
  lomm11_canonical_validate_loadings(loadings, coordinate_names, tolerance)
  beta <- lomm11_profile_numeric_matrix(beta, "Baseline coefficients")
  valid_clips <- !is.null(rownames(beta)) && all(nzchar(rownames(beta))) &&
    !anyDuplicated(rownames(beta))
  if (!identical(colnames(beta), colnames(loadings)) || !valid_clips) {
    lomm11_profile_stop("Baseline coefficients do not match the loading factors.")
  }
  baseline <- beta %*% t(loadings)
  colnames(baseline) <- coordinate_names
  rownames(baseline) <- rownames(beta)
  lomm11_canonical_validate_block_baseline(baseline, loadings, tolerance)
  baseline
}

lomm11_canonical_validate_block_baseline <- function(baseline, loadings,
                                                      tolerance = 1e-10) {
  coordinate_names <- rownames(loadings)
  lomm11_canonical_validate_loadings(loadings, coordinate_names, tolerance)
  baseline <- lomm11_profile_numeric_matrix(baseline, "Block baseline")
  if (ncol(baseline) != nrow(loadings) ||
      !identical(colnames(baseline), coordinate_names)) {
    lomm11_profile_stop("Block baseline does not match the loading coordinates.")
  }
  q_basis <- qr.Q(qr(loadings, tol = tolerance))[, seq_len(ncol(loadings)), drop = FALSE]
  residual <- baseline - baseline %*% q_basis %*% t(q_basis)
  if (max(abs(residual)) > tolerance) {
    lomm11_profile_stop("Baseline is not in the column space of Lambda.")
  }
  invisible(TRUE)
}

lomm11_canonical_validate_realized_effects <- function(baselines, scales,
                                                        tolerance = 1e-10) {
  baselines <- lomm11_profile_numeric_matrix(baselines, "Realized baselines")
  valid_clips <- !is.null(rownames(baselines)) && all(nzchar(rownames(baselines))) &&
    !anyDuplicated(rownames(baselines)) && !is.null(names(scales)) &&
    identical(names(scales), rownames(baselines))
  if (!is.numeric(scales) || length(scales) != nrow(baselines) ||
      any(!is.finite(scales)) || any(scales <= 0) || !valid_clips) {
    lomm11_profile_stop("Realized scales must be finite, positive, and clip-aligned.")
  }
  tolerance <- lomm11_profile_scalar_number(
    tolerance, "Realized-effect tolerance", positive = TRUE
  )
  if (max(abs(colMeans(baselines))) > tolerance || abs(mean(scales) - 1) > tolerance) {
    lomm11_profile_stop("Realized effects violate zero-baseline or mean-one-scale constraints.")
  }
  invisible(TRUE)
}

lomm11_profile_provenance_rows <- function(config) {
  files <- config$provenance$source_files
  data.frame(
    path = vapply(files, `[[`, character(1), "path"),
    sha256 = vapply(files, `[[`, character(1), "sha256"),
    role = vapply(files, `[[`, character(1), "role"),
    stringsAsFactors = FALSE
  )
}

lomm11_profile_validate_selected_preset <- function(config, repo_root) {
  rows <- lomm11_profile_provenance_rows(config)
  path <- rows$path[rows$role == "training_only_selections"]
  selected <- utils::read.csv(
    file.path(repo_root, path), stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(variant_id = "character")
  )
  selected <- selected[selected$variant_id == config$calibrated_preset$variant_id, ]
  valid <- nrow(selected) == 5L && identical(sort(selected$fold), 1:5) &&
    all(selected$period_frames == 12) && all(selected$damping_ratio == 0.5) &&
    all(selected$kappa == 1) && all(selected$core_candidate_id == "CORE017") &&
    all(selected$training_units == 3L) && all(selected$selected_before_heldout) &&
    all(selected$config_hash == config$provenance$calibration_run_config_hash)
  if (!valid) {
    lomm11_profile_stop("Calibration selections do not support the canonical preset.")
  }
  invisible(TRUE)
}

lomm11_profile_validate_run_receipts <- function(config, repo_root) {
  rows <- lomm11_profile_provenance_rows(config)
  success_path <- rows$path[rows$role == "completed_run_receipt"]
  manifest_path <- rows$path[rows$role == "completed_run_manifest"]
  success <- jsonlite::fromJSON(file.path(repo_root, success_path), simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(file.path(repo_root, manifest_path), simplifyVector = FALSE)
  config_hash <- config$provenance$calibration_run_config_hash
  selected_hash <- rows$sha256[rows$role == "training_only_selections"]
  manifest_hash <- rows$sha256[rows$role == "completed_run_manifest"]
  output_paths <- vapply(success$outputs, `[[`, character(1), "path")
  output_hashes <- vapply(success$outputs, `[[`, character(1), "sha256")
  selected_receipt_hash <- output_hashes[output_paths == "selected_candidates.csv"]
  success_valid <- identical(success$status, "complete") &&
    identical(success$config_hash, config_hash) &&
    identical(success$run_manifest_sha256, manifest_hash) &&
    identical(selected_receipt_hash, selected_hash) &&
    identical(success$failed_panel_attempts, 0L)
  source_hashes <- setNames(rows$sha256, rows$role)
  manifest_valid <- identical(manifest$status, "complete") &&
    identical(manifest$stage, "complete") &&
    identical(manifest$config_hash, config_hash) &&
    identical(manifest$progress$error, 0L) &&
    identical(manifest$progress$timeout, 0L) &&
    identical(
      manifest$source_hashes$generator,
      unname(source_hashes[["calibration_generator"]])
    ) && identical(
      manifest$source_hashes$config,
      unname(source_hashes[["calibration_design"]])
    )
  if (!success_valid || !manifest_valid) {
    lomm11_profile_stop("Completed calibration receipts failed provenance validation.")
  }
  invisible(TRUE)
}

lomm11_canonical_validate_provenance <- function(config, repo_root) {
  lomm11_canonical_validate_config(config)
  repo_root <- normalizePath(repo_root, mustWork = TRUE)
  rows <- lomm11_profile_provenance_rows(config)
  for (index in seq_len(nrow(rows))) {
    observed <- lomm11_profile_file_sha256(file.path(repo_root, rows$path[[index]]))
    if (!identical(observed, rows$sha256[[index]])) {
      lomm11_profile_stop("Provenance hash mismatch for ", rows$path[[index]], ".")
    }
  }
  implementation <- config$provenance$implementation
  observed <- lomm11_profile_file_sha256(file.path(repo_root, implementation$path))
  if (!identical(observed, implementation$sha256)) {
    lomm11_profile_stop("Canonical implementation hash does not match its contract.")
  }
  lomm11_profile_validate_selected_preset(config, repo_root)
  lomm11_profile_validate_run_receipts(config, repo_root)
  invisible(TRUE)
}
