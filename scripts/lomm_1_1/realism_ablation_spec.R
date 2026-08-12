lomm11_stop <- function(...) {
  stop(..., call. = FALSE)
}

lomm11_require_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      lomm11_stop("Package '", package, "' is required.")
    }
  }
}

lomm11_file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm11_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

lomm11_read_config <- function(path) {
  lomm11_require_packages()
  config <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  lomm11_validate_config(config)
  config
}

lomm11_require_exact_fields <- function(value, fields, label) {
  if (!setequal(names(value), fields)) {
    lomm11_stop(label, " fields differ from the locked schema.")
  }
  invisible(TRUE)
}

lomm11_scalar_integer <- function(value, label) {
  if (length(value) != 1L || !is.integer(value) || is.na(value)) {
    lomm11_stop(label, " must be one JSON integer.")
  }
  value
}

lomm11_scalar_number <- function(value, label) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value)) {
    lomm11_stop(label, " must be one finite JSON number.")
  }
  as.numeric(value)
}

lomm11_scalar_string <- function(value, label) {
  if (length(value) != 1L || !is.character(value) || is.na(value) || !nzchar(value)) {
    lomm11_stop(label, " must be one non-empty JSON string.")
  }
  value
}

lomm11_validate_input_contract <- function(input_bundle) {
  lomm11_require_exact_fields(
    input_bundle,
    c("artifact_id", "repo_relative_path", "config_hash", "files"),
    "Input bundle"
  )
  expected_files <- c(
    `_SUCCESS.json` = "0517998bd320ebfb46c2efc7e433223dd8353843f4a885e7e70bb6ef18ff3ba8",
    preprocessing_manifest.json = "60c3ca4a9198e01d2f87f0136d5e1b1ec9a2dfddd90d2b47c082e0c8bd2c9880",
    frame_ledger.csv = "0f0bdea7c26c1e929634e93a29148e6ee7f2ca44d7150780631671490cf52ef0",
    probability_view.csv = "23752e2d917fe125a2ca4688e001825e4ca1a846711fbe8a81609dcd4861c3d3",
    clr_view.csv = "0a4eee7f3ae5a5e95c20de304c57c0dee9e2ea84643c307a7a8fcd2cfb334146",
    ilr_view.csv = "e74f82770c0e4fa001b8f0190adb9d67d99b1e4cf20b51e3a9e28ab6b0915052",
    ilr_basis.csv = "f5a9a2aa842fe5ab49332e7d4bcfb7095a79522c53d2fc9bc9b8b29d720ebdf9",
    clip_qc.csv = "eb2603cf8140466e6c7cad07b681e4ffaede1763393f2891838bf6faa87db8a2"
  )
  observed_files <- unlist(input_bundle$files, use.names = TRUE)
  valid <- identical(input_bundle$artifact_id, "lomm_1_1_compositional_0cdc11154d8a") &&
    identical(
      input_bundle$repo_relative_path,
      "simulation_outputs/lomm_1_1/empirical_compositional_preprocess/0cdc11154d8a"
    ) &&
    identical(input_bundle$config_hash, "0cdc11154d8a") &&
    identical(observed_files, expected_files)
  if (!valid) lomm11_stop("Input bundle differs from the certified contract.")
  invisible(TRUE)
}

lomm11_validate_fold_contract <- function(folds) {
  lomm11_require_exact_fields(
    folds,
    c(
      "count", "unit", "stratify_by", "order_by", "heldout_clips_per_fold",
      "selection_view", "sensitivity_view"
    ),
    "Fold contract"
  )
  valid <- identical(folds$count, 5L) && identical(folds$unit, "clip") &&
    identical(folds$stratify_by, "truth_label") &&
    identical(unlist(folds$order_by, use.names = FALSE), c("complete_rows", "clip")) &&
    identical(folds$heldout_clips_per_fold, 20L) &&
    identical(folds$selection_view, "complete_case") &&
    identical(folds$sensitivity_view, "ilr_gap2")
  if (!valid) lomm11_stop("Fold contract differs from the locked design.")
  invisible(TRUE)
}

lomm11_validate_dynamics_contract <- function(dynamics) {
  lomm11_require_exact_fields(
    dynamics,
    c(
      "dt", "omega_rule", "eta_rule", "zeta_rule", "transition",
      "initialization", "innovation_normalization", "lyapunov_tolerance",
      "anti_damped_allowed"
    ),
    "Dynamics contract"
  )
  valid <- identical(dynamics$dt, 1) &&
    identical(dynamics$omega_rule, "2*pi/period_frames") &&
    identical(dynamics$eta_rule, "-omega^2") &&
    identical(dynamics$zeta_rule, "-2*damping_ratio*omega") &&
    identical(dynamics$transition, "exact_zero_order_hold") &&
    identical(dynamics$initialization, "independent_stationary_gaussian") &&
    identical(
      dynamics$innovation_normalization,
      "discrete_lyapunov_unit_position_variance"
    ) && identical(dynamics$lyapunov_tolerance, 1e-10) &&
    identical(dynamics$anti_damped_allowed, FALSE)
  if (!valid) lomm11_stop("Dynamics contract differs from the locked design.")
  invisible(TRUE)
}

lomm11_validate_measurement_contract <- function(measurement) {
  expected_names <- c(
    "coordinate_system", "n_latent_directions", "training_center",
    "loading_basis", "loading_relative_scale", "loading_sign_rule",
    "between_clip_covariance", "between_clip_psd_floor", "residual_sd", "tau",
    "common_logit_shift", "clip_scale_distribution", "gate_low", "gate_high",
    "gate_initial_high_probability", "gate_dwell_rule",
    "gate_transition_observations"
  )
  lomm11_require_exact_fields(measurement, expected_names, "Measurement contract")
  expected <- list(
    coordinate_system = "locked_10d_ilr",
    n_latent_directions = 3L,
    training_center = "complete_case_training_mean_ilr",
    loading_basis = "top3_within_clip_centered_ilr_eigenvectors",
    loading_relative_scale = "sqrt_eigenvalue_divided_by_root_mean_eigenvalue",
    loading_sign_rule = "largest_absolute_entry_positive",
    between_clip_covariance = "half_full_half_diagonal_clip_mean_ilr_covariance",
    between_clip_psd_floor = 1e-8,
    residual_sd = 0.25,
    tau = 1,
    common_logit_shift = 0,
    clip_scale_distribution = "Uniform(0.5,1.5)",
    gate_low = 0.25,
    gate_high = 1,
    gate_initial_high_probability = 0.5,
    gate_dwell_rule = "min(24,4+Geometric(p=1/8))",
    gate_transition_observations = 3L
  )
  if (!identical(measurement, expected)) {
    lomm11_stop("Measurement contract differs from the locked design.")
  }
  invisible(TRUE)
}

lomm11_validate_randomness_contract <- function(randomness) {
  expected_names <- c(
    "seed_origin", "training_replicates", "heldout_replicates", "substreams",
    "training_pairing", "heldout_pairing", "stream_state_derivation",
    "seed_base_role", "component_substreams_consumed_when_disabled"
  )
  lomm11_require_exact_fields(randomness, expected_names, "Randomness contract")
  expected_streams <- c(
    "between_clip_baseline", "clip_scale", "gate_schedule", "initial_state",
    "innovations", "residuals"
  )
  valid <- identical(randomness$seed_origin, 11082026L) &&
    identical(randomness$training_replicates, 3L) &&
    identical(randomness$heldout_replicates, 100L) &&
    identical(unlist(randomness$substreams, use.names = FALSE), expected_streams) &&
    identical(randomness$training_pairing, "fold_core_candidate_replicate") &&
    identical(randomness$heldout_pairing, "fold_replicate") &&
    identical(
      randomness$stream_state_derivation,
      "sha256_full_key_to_lecuyer_cmrg_v1"
    ) && identical(
      randomness$seed_base_role,
      "auditable_pairing_namespace_checksum"
    ) &&
    identical(randomness$component_substreams_consumed_when_disabled, TRUE)
  if (!valid) lomm11_stop("Randomness contract differs from the locked design.")
  invisible(TRUE)
}

lomm11_validate_metric_contract <- function(metrics) {
  expected_names <- c(
    "families", "univariate", "multivariate", "probability_columns",
    "logratio_named_columns", "logratio_multivariate_coordinates",
    "segment_boundary_rule", "scale_fallback", "metric_discrepancy",
    "family_weights"
  )
  lomm11_require_exact_fields(metrics, expected_names, "Metric contract")
  valid <- identical(unlist(metrics$families, use.names = FALSE), c("probability", "logratio")) &&
    identical(
      unlist(metrics$univariate, use.names = FALSE),
      c(
        "level_median", "level_iqr", "difference_iqr", "lag1_correlation",
        "run_length_entropy", "spectral_entropy"
      )
    ) &&
    identical(unlist(metrics$multivariate, use.names = FALSE), c("effective_rank", "total_variance")) &&
    identical(metrics$probability_columns, 11L) &&
    identical(metrics$logratio_named_columns, 11L) &&
    identical(metrics$logratio_multivariate_coordinates, 10L) &&
    identical(metrics$segment_boundary_rule, "never_bridge") &&
    identical(unlist(metrics$scale_fallback, use.names = FALSE), c("IQR", "1.4826*MAD", "SD", "1")) &&
    identical(
      metrics$metric_discrepancy,
      "max(abs_median_difference_scaled,wasserstein1_scaled)"
    ) && identical(unlist(metrics$family_weights), c(probability = 0.5, logratio = 0.5))
  if (!valid) lomm11_stop("Metric contract differs from the locked design.")
  invisible(TRUE)
}

lomm11_validate_selection_execution_contract <- function(config) {
  expected_order <- c(
    "selection_loss_ascending", "max_metric_discrepancy_ascending",
    "absolute_kappa_minus_one_ascending", "damping_ratio_descending",
    "candidate_id_ascending"
  )
  selection <- config$selection
  execution <- config$future_execution
  lomm11_require_exact_fields(
    selection,
    c(
      "scope", "separate_by", "candidate_count_per_selection",
      "replicate_pooling", "order", "heldout_access", "sensitivity_access"
    ),
    "Selection contract"
  )
  lomm11_require_exact_fields(
    execution,
    c(
      "worker_limit", "blas_threads_per_worker", "job_timeout_seconds",
      "panel_timeout_seconds", "authorization_token",
      "requires_full_prospective_manifest_sha256", "automatic_progression"
    ),
    "Future-execution contract"
  )
  selection_valid <- identical(selection$scope, "complete_case_training_clips_only") &&
    identical(unlist(selection$separate_by, use.names = FALSE), c("fold", "variant_id")) &&
    identical(selection$candidate_count_per_selection, 27L) &&
    identical(selection$replicate_pooling, "three_paired_training_panels") &&
    identical(unlist(selection$order, use.names = FALSE), expected_order) &&
    identical(selection$heldout_access, FALSE) && identical(selection$sensitivity_access, FALSE)
  execution_valid <- identical(execution$worker_limit, 12L) &&
    identical(execution$blas_threads_per_worker, 1L) &&
    identical(execution$job_timeout_seconds, 21600L) &&
    identical(execution$panel_timeout_seconds, 300L) &&
    identical(execution$authorization_token, "LOMM-1.1-REALISM-AUTHORIZED") &&
    identical(execution$requires_full_prospective_manifest_sha256, TRUE) &&
    identical(execution$automatic_progression, FALSE)
  if (!selection_valid || !execution_valid) {
    lomm11_stop("Selection or future-execution contract differs from the locked design.")
  }
  invisible(TRUE)
}

lomm11_validate_claim_contract <- function(claim_scope) {
  lomm11_require_exact_fields(
    claim_scope, c("supported_target", "excluded_claims"), "Claim contract"
  )
  expected_exclusions <- c(
    "emotion_factor_validity", "affect_mechanism_identification",
    "classifier_validity", "external_dataset_generalization",
    "dimensional_recovery_performance", "anti_damped_superiority_or_inferiority"
  )
  valid <- identical(
    claim_scope$supported_target,
    "heldout_similarity_to_the_same_100_clip_mafw_clip_score_sample"
  ) && identical(
    unlist(claim_scope$excluded_claims, use.names = FALSE), expected_exclusions
  )
  if (!valid) lomm11_stop("Claim contract differs from the locked design.")
  invisible(TRUE)
}

lomm11_validate_config <- function(config) {
  required <- c(
    "schema_version", "pipeline_id", "specification_status", "input_bundle",
    "folds", "variants", "axes", "dynamics", "measurement", "randomness",
    "metrics", "selection", "future_execution", "expected_counts", "claim_scope"
  )
  lomm11_require_exact_fields(config, required, "Ablation configuration")
  if (!identical(config$schema_version, 1L) ||
      !identical(config$pipeline_id, "lomm_1_1_empirical_realism_ablation") ||
      !identical(config$specification_status, "planned")) {
    lomm11_stop("Ablation configuration header is invalid.")
  }
  lomm11_validate_input_contract(config$input_bundle)
  lomm11_validate_fold_contract(config$folds)
  lomm11_validate_dynamics_contract(config$dynamics)
  lomm11_validate_measurement_contract(config$measurement)
  lomm11_validate_randomness_contract(config$randomness)
  lomm11_validate_metric_contract(config$metrics)
  lomm11_validate_selection_execution_contract(config)
  lomm11_validate_claim_contract(config$claim_scope)
  axes <- lapply(config$axes, unlist, use.names = FALSE)
  if (!identical(as.integer(axes$period_frames), c(8L, 12L, 24L)) ||
      !identical(as.numeric(axes$damping_ratio), c(0.05, 0.20, 0.50)) ||
      !identical(as.numeric(axes$kappa), c(0.75, 1.00, 1.25))) {
    lomm11_stop("Ablation axes differ from the locked design.")
  }
  variants <- lomm11_variant_table(config)
  expected_variant_ids <- c("000", "100", "010", "001", "110", "101", "011", "111")
  if (!identical(variants$variant_id, expected_variant_ids) ||
      any(variants$B + variants$S + variants$G < 0L) ||
      any(!variants$B %in% 0:1) || any(!variants$S %in% 0:1) ||
      any(!variants$G %in% 0:1)) {
    lomm11_stop("Ablation variants differ from the locked factorial design.")
  }
  if (!identical(config$dynamics$anti_damped_allowed, FALSE) ||
      !identical(config$dynamics$dt, 1) ||
      !identical(config$measurement$tau, 1) ||
      !identical(config$folds$selection_view, "complete_case") ||
      !identical(config$folds$sensitivity_view, "ilr_gap2")) {
    lomm11_stop("A locked stability, scale, or analysis-view constraint changed.")
  }
  expected_counts <- c(
    candidates = 216L, fold_candidates = 1080L, training_panels = 3240L,
    selections = 40L, heldout_panels = 4000L, heldout_view_evaluations = 8000L,
    generated_panels = 7240L, checkpoint_units = 11240L,
    training_metric_discrepancies = 51840L,
    heldout_metric_discrepancies = 128000L,
    clip_metric_contributions = 21760000L, paired_ablation_effects = 272L,
    primary_frames = 3557L, primary_segments = 188L,
    sensitivity_frames = 3632L, sensitivity_segments = 129L,
    imputed_sensitivity_frames = 75L
  )
  observed_counts <- unlist(config$expected_counts, use.names = TRUE)
  if (!identical(names(observed_counts), names(expected_counts)) ||
      !all(vapply(config$expected_counts, is.integer, logical(1))) ||
      !identical(as.integer(observed_counts), as.integer(expected_counts))) {
    lomm11_stop("Expected counts differ from the locked design.")
  }
  invisible(TRUE)
}

lomm11_variant_table <- function(config) {
  rows <- lapply(config$variants, function(value) {
    lomm11_require_exact_fields(
      value, c("variant_id", "variant_label", "B", "S", "G"), "Variant"
    )
    data.frame(
      variant_id = lomm11_scalar_string(value$variant_id, "Variant ID"),
      variant_label = lomm11_scalar_string(value$variant_label, "Variant label"),
      B = lomm11_scalar_integer(value$B, "B"),
      S = lomm11_scalar_integer(value$S, "S"),
      G = lomm11_scalar_integer(value$G, "G"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

lomm11_hash_int <- function(value, modulus = 2147483629) {
  codepoints <- utf8ToInt(enc2utf8(as.character(value)))
  hash <- 0
  for (codepoint in codepoints) {
    hash <- (hash * 131 + codepoint) %% modulus
  }
  as.integer(hash)
}

lomm11_seed_from_key <- function(key, origin) {
  modulus <- 2147483646
  as.integer((as.double(origin) + as.double(lomm11_hash_int(key))) %% modulus + 1)
}

lomm11_component_rng_key <- function(seed_group, clip, segment, component,
                                     master_seed) {
  if (length(master_seed) != 1L || !is.integer(master_seed) ||
      is.na(master_seed) || master_seed < 1L || master_seed > 2147483646L) {
    lomm11_stop("Component RNG master seed is invalid.")
  }
  labels <- c(seed_group, clip, segment, component)
  if (anyNA(labels) || any(!nzchar(as.character(labels)))) {
    lomm11_stop("Component RNG identifiers are incomplete.")
  }
  paste(
    "lomm11_lecuyer_cmrg_v1",
    format(master_seed, scientific = FALSE, trim = TRUE),
    enc2utf8(as.character(seed_group)), enc2utf8(as.character(clip)),
    enc2utf8(as.character(segment)), enc2utf8(as.character(component)),
    sep = "\037"
  )
}

lomm11_lecuyer_state_from_key <- function(key) {
  lomm11_require_packages()
  if (length(key) != 1L || is.na(key) || !nzchar(key)) {
    lomm11_stop("Component RNG key is invalid.")
  }
  bytes <- as.integer(digest::digest(
    key, algo = "sha256", serialize = FALSE, raw = TRUE
  ))
  words <- colSums(
    matrix(bytes[seq_len(24L)], nrow = 4L) * c(2^24, 2^16, 2^8, 1)
  )
  moduli <- c(rep(4294967087, 3L), rep(4294944443, 3L))
  unsigned <- (words %% (moduli - 2)) + 1
  unsigned[unsigned >= 2^31] <- unsigned[unsigned >= 2^31] + 1
  signed <- unsigned
  signed[signed > 2147483647] <- signed[signed > 2147483647] - 2^32
  state <- c(10407L, as.integer(signed))
  if (length(state) != 7L || anyNA(state)) {
    lomm11_stop("Component RNG state derivation failed.")
  }
  state
}

lomm11_component_rng_state <- function(seed_group, clip, segment, component,
                                       master_seed) {
  lomm11_lecuyer_state_from_key(lomm11_component_rng_key(
    seed_group, clip, segment, component, master_seed
  ))
}

lomm11_candidate_grid <- function(config) {
  axes <- lapply(config$axes, unlist, use.names = FALSE)
  core <- expand.grid(
    period_frames = as.integer(axes$period_frames),
    damping_ratio = as.numeric(axes$damping_ratio),
    kappa = as.numeric(axes$kappa),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  core <- core[order(core$period_frames, core$damping_ratio, core$kappa), , drop = FALSE]
  core$core_candidate_index <- seq_len(nrow(core))
  core$core_candidate_id <- sprintf("CORE%03d", core$core_candidate_index)
  variants <- lomm11_variant_table(config)
  rows <- lapply(seq_len(nrow(variants)), function(index) {
    frame <- cbind(variants[rep(index, nrow(core)), , drop = FALSE], core)
    frame$candidate_id <- sprintf(
      "V%s-C%03d", frame$variant_id, frame$core_candidate_index
    )
    frame
  })
  candidates <- do.call(rbind, rows)
  candidates$dt <- as.numeric(config$dynamics$dt)
  candidates$omega <- 2 * pi / candidates$period_frames
  candidates$eta <- -(candidates$omega^2)
  candidates$zeta <- -2 * candidates$damping_ratio * candidates$omega
  candidates$tau <- as.numeric(config$measurement$tau)
  candidates$initialization <- config$dynamics$initialization
  candidates$innovation_normalization <- config$dynamics$innovation_normalization
  candidates$stability_class <- "asymptotically_stable"
  candidates$selection_eligible <- TRUE
  candidates <- candidates[, c(
    "candidate_id", "core_candidate_id", "core_candidate_index", "variant_id",
    "variant_label", "B", "S", "G", "period_frames", "damping_ratio",
    "kappa", "dt", "omega", "eta", "zeta", "tau", "initialization",
    "innovation_normalization", "stability_class", "selection_eligible"
  )]
  rownames(candidates) <- NULL
  candidates
}

lomm11_assign_folds <- function(clip_qc, config) {
  required <- c(
    "clip", "truth_label", "complete_rows", "primary_segments",
    "sensitivity_segments", "primary_max_segment_length",
    "sensitivity_max_segment_length"
  )
  if (!all(required %in% names(clip_qc)) || nrow(clip_qc) != 100L ||
      anyDuplicated(clip_qc$clip)) {
    lomm11_stop("Clip QC does not satisfy the locked fold input contract.")
  }
  n_folds <- as.integer(config$folds$count)
  folds <- integer(nrow(clip_qc))
  offset <- 0L
  for (stratum in sort(unique(as.character(clip_qc$truth_label)))) {
    indices <- which(as.character(clip_qc$truth_label) == stratum)
    ordered <- indices[order(
      as.integer(clip_qc$complete_rows[indices]),
      as.character(clip_qc$clip[indices])
    )]
    folds[ordered] <- ((seq_along(ordered) - 1L + offset) %% n_folds) + 1L
    offset <- (offset + length(indices)) %% n_folds
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
  assignments <- assignments[order(assignments$fold, assignments$truth_label, assignments$clip), ]
  rownames(assignments) <- NULL
  fold_sizes <- as.integer(table(factor(assignments$fold, levels = seq_len(n_folds))))
  if (!identical(fold_sizes, rep(20L, 5L))) {
    lomm11_stop("Fold assignment must yield exactly 20 held-out clips per fold.")
  }
  assignments
}

lomm11_segment_manifest <- function(probability_view, assignments) {
  required <- c(
    "analysis_variant", "clip", "frame_index", "truth_label", "source_complete",
    "is_imputed", "segment_id", "segment_position", "segment_length"
  )
  if (!all(required %in% names(probability_view))) {
    lomm11_stop("Probability view is missing segment-manifest fields.")
  }
  groups <- split(
    seq_len(nrow(probability_view)),
    paste(probability_view$analysis_variant, probability_view$segment_id, sep = "\r")
  )
  rows <- lapply(groups, function(indices) {
    frame <- probability_view[indices, , drop = FALSE]
    data.frame(
      analysis_variant = as.character(frame$analysis_variant[[1]]),
      clip = as.character(frame$clip[[1]]),
      truth_label = as.character(frame$truth_label[[1]]),
      segment_id = as.character(frame$segment_id[[1]]),
      segment_position_start = min(as.integer(frame$segment_position)),
      segment_position_end = max(as.integer(frame$segment_position)),
      segment_length = nrow(frame),
      frame_index_start = min(as.numeric(frame$frame_index)),
      frame_index_end = max(as.numeric(frame$frame_index)),
      source_complete_rows = sum(as.logical(frame$source_complete)),
      imputed_rows = sum(as.logical(frame$is_imputed)),
      stringsAsFactors = FALSE
    )
  })
  segments <- do.call(rbind, rows)
  segments$fold <- assignments$fold[match(segments$clip, assignments$clip)]
  segments <- segments[order(
    segments$analysis_variant, segments$fold, segments$clip,
    segments$frame_index_start
  ), ]
  rownames(segments) <- NULL
  expected <- c(complete_case = 188L, ilr_gap2 = 129L)
  observed <- table(factor(segments$analysis_variant, levels = names(expected)))
  if (!identical(as.integer(observed), as.integer(expected)) ||
      any(segments$segment_position_start != 1L) ||
      any(segments$segment_position_end != segments$segment_length) ||
      anyNA(segments$fold)) {
    lomm11_stop("Segment manifest differs from the certified input contract.")
  }
  segments
}

lomm11_fold_view_counts <- function(assignments, segments, probability_view) {
  views <- c("complete_case", "ilr_gap2")
  rows <- list()
  row_index <- 1L
  for (fold in seq_len(5L)) {
    heldout <- assignments$clip[assignments$fold == fold]
    for (view in views) {
      frame_rows <- probability_view$analysis_variant == view &
        probability_view$clip %in% heldout
      segment_rows <- segments$analysis_variant == view & segments$clip %in% heldout
      rows[[row_index]] <- data.frame(
        fold = fold,
        analysis_view = view,
        heldout_clips = length(heldout),
        heldout_frames = sum(frame_rows),
        heldout_segments = sum(segment_rows),
        stringsAsFactors = FALSE
      )
      row_index <- row_index + 1L
    }
  }
  counts <- do.call(rbind, rows)
  primary <- counts[counts$analysis_view == "complete_case", ]
  sensitivity <- counts[counts$analysis_view == "ilr_gap2", ]
  if (!identical(primary$heldout_frames, c(700L, 697L, 706L, 766L, 688L)) ||
      !identical(primary$heldout_segments, c(39L, 36L, 30L, 43L, 40L)) ||
      !identical(sensitivity$heldout_frames, c(716L, 709L, 716L, 789L, 702L)) ||
      !identical(sensitivity$heldout_segments, c(28L, 27L, 22L, 24L, 28L))) {
    lomm11_stop("Fold frame or segment counts differ from the locked input geometry.")
  }
  counts
}

lomm11_training_plan <- function(candidates, fold_counts, config) {
  rows <- merge(
    expand.grid(
      fold = seq_len(5L),
      training_rep = seq_len(as.integer(config$randomness$training_replicates)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    candidates,
    by = NULL,
    sort = FALSE
  )
  rows <- rows[order(
    rows$fold, rows$variant_id, rows$core_candidate_index, rows$training_rep
  ), ]
  rows$stage <- "training"
  rows$analysis_view <- "complete_case"
  rows$selection_key <- sprintf("F%02d|V%s", rows$fold, rows$variant_id)
  rows$seed_group <- sprintf(
    "training|F%02d|%s|R%03d",
    rows$fold, rows$core_candidate_id, rows$training_rep
  )
  origin <- as.integer(config$randomness$seed_origin)
  rows$seed_base <- vapply(rows$seed_group, lomm11_seed_from_key, integer(1), origin)
  rows$panel_key <- sprintf(
    "training|F%02d|V%s|%s|R%03d",
    rows$fold, rows$variant_id, rows$core_candidate_id, rows$training_rep
  )
  rows$unit_key <- rows$panel_key
  primary <- fold_counts[fold_counts$analysis_view == "complete_case", ]
  rows$expected_training_clips <- 80L
  rows$expected_training_primary_frames <- 3557L - primary$heldout_frames[rows$fold]
  rows$expected_training_primary_segments <- 188L - primary$heldout_segments[rows$fold]
  rows[, c(
    "unit_key", "panel_key", "stage", "fold", "variant_id", "candidate_id",
    "core_candidate_id", "training_rep", "analysis_view", "selection_key",
    "seed_group", "seed_base", "expected_training_clips",
    "expected_training_primary_frames", "expected_training_primary_segments"
  )]
}

lomm11_selection_plan <- function(config) {
  variants <- lomm11_variant_table(config)
  rows <- merge(
    data.frame(fold = seq_len(5L)), variants, by = NULL, sort = FALSE
  )
  rows <- rows[order(rows$fold, rows$variant_id), ]
  rows$selection_key <- sprintf("F%02d|V%s", rows$fold, rows$variant_id)
  rows$candidate_count <- 27L
  rows$training_replicates <- as.integer(config$randomness$training_replicates)
  rows$training_panel_count <- rows$candidate_count * rows$training_replicates
  rows$selection_view <- "complete_case"
  rows$selection_rule <- paste(unlist(config$selection$order), collapse = ";")
  rows$heldout_access <- FALSE
  rows$sensitivity_access <- FALSE
  rows[, c(
    "selection_key", "fold", "variant_id", "variant_label", "candidate_count",
    "training_replicates", "training_panel_count", "selection_view",
    "selection_rule", "heldout_access", "sensitivity_access"
  )]
}

lomm11_heldout_plan <- function(config, fold_counts) {
  variants <- lomm11_variant_table(config)
  rows <- merge(
    expand.grid(
      fold = seq_len(5L),
      heldout_rep = seq_len(as.integer(config$randomness$heldout_replicates)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    variants,
    by = NULL,
    sort = FALSE
  )
  rows <- rows[order(rows$fold, rows$variant_id, rows$heldout_rep), ]
  rows$stage <- "heldout"
  rows$selection_key <- sprintf("F%02d|V%s", rows$fold, rows$variant_id)
  rows$seed_group <- sprintf("heldout|F%02d|R%03d", rows$fold, rows$heldout_rep)
  origin <- as.integer(config$randomness$seed_origin)
  rows$seed_base <- vapply(rows$seed_group, lomm11_seed_from_key, integer(1), origin)
  rows$panel_key <- sprintf(
    "heldout|F%02d|V%s|R%03d", rows$fold, rows$variant_id, rows$heldout_rep
  )
  support <- fold_counts[fold_counts$analysis_view == "ilr_gap2", ]
  rows$expected_heldout_clips <- 20L
  rows$support_view <- "ilr_gap2"
  rows$expected_support_frames <- support$heldout_frames[rows$fold]
  rows$expected_support_segments <- support$heldout_segments[rows$fold]
  rows[, c(
    "panel_key", "stage", "fold", "variant_id", "variant_label", "heldout_rep",
    "selection_key", "seed_group", "seed_base", "expected_heldout_clips",
    "support_view", "expected_support_frames", "expected_support_segments"
  )]
}

lomm11_heldout_evaluation_plan <- function(heldout, fold_counts) {
  views <- data.frame(
    analysis_view = c("complete_case", "ilr_gap2"),
    stringsAsFactors = FALSE
  )
  rows <- merge(heldout, views, by = NULL, sort = FALSE)
  rows <- rows[order(rows$fold, rows$variant_id, rows$heldout_rep, rows$analysis_view), ]
  count_key <- paste(fold_counts$fold, fold_counts$analysis_view, sep = "|")
  row_key <- paste(rows$fold, rows$analysis_view, sep = "|")
  index <- match(row_key, count_key)
  rows$unit_key <- paste(rows$panel_key, rows$analysis_view, sep = "|")
  rows$expected_clips <- fold_counts$heldout_clips[index]
  rows$expected_frames <- fold_counts$heldout_frames[index]
  rows$expected_segments <- fold_counts$heldout_segments[index]
  rows$simulation_reused_across_views <- TRUE
  rows[, c(
    "unit_key", "panel_key", "stage", "fold", "variant_id", "heldout_rep",
    "analysis_view", "selection_key", "seed_group", "seed_base",
    "expected_clips", "expected_frames", "expected_segments",
    "simulation_reused_across_views"
  )]
}

lomm11_table_schema <- function(frame, primary_key) {
  column_type <- function(value) {
    if (is.character(value)) return("character")
    if (is.integer(value)) return("integer")
    if (is.logical(value)) return("logical")
    if (is.numeric(value)) return("double")
    lomm11_stop("Unsupported plan-column type.")
  }
  list(
    rows = nrow(frame),
    columns = ncol(frame),
    column_names = as.list(names(frame)),
    column_types = as.list(vapply(frame, column_type, character(1))),
    primary_key = as.list(primary_key)
  )
}

lomm11_future_output_contract <- function(config) {
  list(
    fold_nuisance_manifest.csv = list(
      rows = 5L,
      primary_key = list("fold"),
      columns = list(
        fold = "integer", n_training_clips = "integer",
        training_clips_sha256 = "character", primary_rows = "integer",
        primary_segments = "integer", center_sha256 = "character",
        directions_sha256 = "character", loadings_sha256 = "character",
        between_clip_covariance_sha256 = "character",
        metric_scales_sha256 = "character", nuisance_rds_path = "character",
        nuisance_rds_sha256 = "character", config_hash = "character",
        input_hash = "character"
      )
    ),
    checkpoint_results.csv = list(
      rows = 11240L,
      primary_key = list("unit_key"),
      columns = list(
        unit_key = "character", panel_key = "character", stage = "character",
        analysis_view = "character", fold = "integer", variant_id = "character",
        candidate_id = "character", replicate = "integer", status = "character",
        error_message = "character", probability_loss = "double",
        logratio_loss = "double", selection_loss = "double",
        max_metric_discrepancy = "double", seed_group = "character",
        seed_base = "integer", runtime_sec = "double", config_hash = "character",
        input_hash = "character", panel_hash = "character"
      )
    ),
    selected_candidates.csv = list(
      rows = 40L,
      primary_key = list("selection_key"),
      columns = list(
        selection_key = "character", fold = "integer", variant_id = "character",
        candidate_id = "character", core_candidate_id = "character",
        period_frames = "integer", damping_ratio = "double", kappa = "double",
        probability_loss = "double", logratio_loss = "double",
        selection_loss = "double", max_metric_discrepancy = "double",
        training_units = "integer", selected_before_heldout = "logical",
        config_hash = "character", input_hash = "character"
      )
    ),
    training_metric_discrepancies.parquet = list(
      rows = 51840L,
      primary_key = list("unit_key", "family", "metric"),
      columns = list(
        unit_key = "character", panel_key = "character", fold = "integer",
        variant_id = "character", candidate_id = "character", train_rep = "integer",
        family = "character", metric = "character", median_diff_scaled = "double",
        wasserstein_scaled = "double", max_discrepancy = "double",
        empirical_n = "integer", synthetic_n = "integer", metric_scale = "double",
        status = "character", error_message = "character", seed_group = "character",
        config_hash = "character", input_hash = "character", panel_hash = "character"
      )
    ),
    heldout_metric_discrepancies.csv = list(
      rows = 128000L,
      primary_key = list("unit_key", "family", "metric"),
      columns = list(
        unit_key = "character", panel_key = "character", fold = "integer",
        variant_id = "character", heldout_rep = "integer",
        analysis_view = "character", family = "character", metric = "character",
        median_diff_scaled = "double", wasserstein_scaled = "double",
        max_discrepancy = "double", empirical_n = "integer", synthetic_n = "integer",
        metric_scale = "double", status = "character", error_message = "character",
        seed_group = "character", config_hash = "character", input_hash = "character",
        panel_hash = "character"
      )
    ),
    clip_metric_contributions.parquet = list(
      rows = 21760000L,
      primary_key = list("contribution_key"),
      partition_columns = list("analysis_view", "fold", "variant_id", "family"),
      columns = list(
        contribution_key = "character", unit_key = "character",
        panel_key = "character", fold = "integer", variant_id = "character",
        heldout_rep = "integer", analysis_view = "character", clip = "character",
        family = "character", metric = "character", unit = "character",
        empirical_value = "double", synthetic_value = "double",
        training_scale = "double", status = "character", error_message = "character",
        seed_group = "character", config_hash = "character", input_hash = "character",
        panel_hash = "character"
      )
    ),
    run_manifest.json = list(rows = 1L, primary_key = list("config_hash")),
    `_SUCCESS.json` = list(rows = 1L, primary_key = list("config_hash")),
    paired_ablation_effects.csv = list(
      completion_required = FALSE,
      rows = 272L,
      primary_key = list("contrast_id", "analysis_view", "family", "metric"),
      columns = list(
        contrast_id = "character", contrast_definition = "character",
        analysis_view = "character", family = "character", metric = "character",
        estimand = "character", direction = "character", estimate = "double",
        monte_carlo_se = "double", bootstrap_se = "double", conf_low = "double",
        conf_high = "double", n_folds = "integer", n_reps = "integer",
        n_clips = "integer", bootstrap_reps = "integer", bootstrap_seed = "integer",
        source_config_hash = "character"
      )
    )
  )
}

lomm11_rng_provenance_contract <- function(config) {
  list(
    state_derivation_version = "lomm11_lecuyer_cmrg_v1",
    stream_state_derivation = config$randomness$stream_state_derivation,
    generator = "L'Ecuyer-CMRG",
    state_words = 6L,
    state_identity_fields = as.list(c(
      "state_derivation_version", "seed_base", "seed_group", "clip",
      "segment", "component"
    )),
    seed_base_role = config$randomness$seed_base_role,
    clip_shared_segment = "__clip_shared__",
    clip_shared_components = as.list(c(
      "between_clip_baseline", "clip_scale"
    )),
    segment_components = as.list(c(
      "gate_schedule", "initial_state", "innovations", "residuals"
    )),
    component_substreams_consumed_when_disabled =
      config$randomness$component_substreams_consumed_when_disabled
  )
}

lomm11_target_contract <- function(config, fold_counts) {
  list(
    schema_version = 1L,
    selection_view = "complete_case",
    sensitivity_view = "ilr_gap2",
    clip_is_independent_unit = TRUE,
    segment_boundaries_are_hard = TRUE,
    nuisance_scope = "complete_case_training_clips_only",
    nuisance_fields = as.list(c(
      "mean_ilr", "within_clip_loading_basis", "between_clip_covariance",
      "metric_scales", "run_thresholds"
    )),
    families = config$metrics$families,
    univariate_metrics = config$metrics$univariate,
    multivariate_metrics = config$metrics$multivariate,
    discrepancy = config$metrics$metric_discrepancy,
    scale_fallback = config$metrics$scale_fallback,
    family_weights = config$metrics$family_weights,
    randomness = lomm11_rng_provenance_contract(config),
    fold_view_counts = lapply(seq_len(nrow(fold_counts)), function(index) {
      as.list(fold_counts[index, , drop = FALSE])
    }),
    claim_scope = config$claim_scope
  )
}

lomm11_build_plan <- function(config, clip_qc, probability_view) {
  candidates <- lomm11_candidate_grid(config)
  folds <- lomm11_assign_folds(clip_qc, config)
  segments <- lomm11_segment_manifest(probability_view, folds)
  fold_counts <- lomm11_fold_view_counts(folds, segments, probability_view)
  training <- lomm11_training_plan(candidates, fold_counts, config)
  selections <- lomm11_selection_plan(config)
  heldout <- lomm11_heldout_plan(config, fold_counts)
  evaluations <- lomm11_heldout_evaluation_plan(heldout, fold_counts)
  tables <- list(
    candidate_grid.csv = candidates,
    fold_assignments.csv = folds,
    segment_manifest.csv = segments,
    training_plan.csv = training,
    selection_plan.csv = selections,
    heldout_plan.csv = heldout,
    heldout_evaluation_plan.csv = evaluations
  )
  schemas <- list(
    candidate_grid.csv = lomm11_table_schema(candidates, "candidate_id"),
    fold_assignments.csv = lomm11_table_schema(folds, "clip"),
    segment_manifest.csv = lomm11_table_schema(
      segments, c("analysis_variant", "segment_id")
    ),
    training_plan.csv = lomm11_table_schema(training, "unit_key"),
    selection_plan.csv = lomm11_table_schema(selections, "selection_key"),
    heldout_plan.csv = lomm11_table_schema(heldout, "panel_key"),
    heldout_evaluation_plan.csv = lomm11_table_schema(evaluations, "unit_key")
  )
  plan <- list(
    tables = tables,
    schemas = schemas,
    fold_counts = fold_counts,
    target_contract = lomm11_target_contract(config, fold_counts),
    future_output_contract = lomm11_future_output_contract(config)
  )
  lomm11_validate_plan(plan, config)
  plan
}

lomm11_validate_plan <- function(plan, config) {
  tables <- plan$tables
  expected_rows <- c(
    candidate_grid.csv = 216L,
    fold_assignments.csv = 100L,
    segment_manifest.csv = 317L,
    training_plan.csv = 3240L,
    selection_plan.csv = 40L,
    heldout_plan.csv = 4000L,
    heldout_evaluation_plan.csv = 8000L
  )
  observed_rows <- vapply(tables, nrow, integer(1))
  if (!identical(observed_rows, expected_rows)) {
    lomm11_stop("Plan-table row counts differ from the locked design.")
  }
  for (name in names(tables)) {
    key <- unlist(plan$schemas[[name]]$primary_key, use.names = FALSE)
    values <- do.call(paste, c(tables[[name]][key], sep = "\r"))
    if (anyDuplicated(values)) {
      lomm11_stop("Plan table has duplicate primary keys: ", name)
    }
  }
  training <- tables$training_plan.csv
  training_groups <- table(training$seed_group)
  if (length(training_groups) != 405L || !all(training_groups == 8L) ||
      anyDuplicated(training$unit_key)) {
    lomm11_stop("Training seed pairing or unit keys are invalid.")
  }
  heldout <- tables$heldout_plan.csv
  heldout_groups <- table(heldout$seed_group)
  if (length(heldout_groups) != 500L || !all(heldout_groups == 8L) ||
      anyDuplicated(heldout$panel_key)) {
    lomm11_stop("Held-out seed pairing or panel keys are invalid.")
  }
  evaluations <- tables$heldout_evaluation_plan.csv
  evaluation_groups <- table(evaluations$panel_key)
  evaluation_seed_groups <- table(evaluations$seed_group)
  if (!all(evaluation_groups == 2L) || length(evaluation_seed_groups) != 500L ||
      !all(evaluation_seed_groups == 16L) || anyDuplicated(evaluations$unit_key) ||
      !all(evaluations$simulation_reused_across_views)) {
    lomm11_stop("Held-out view reuse or seed pairing is invalid.")
  }
  candidates <- tables$candidate_grid.csv
  if (any(candidates$eta >= 0) || any(candidates$zeta >= 0) ||
      any(candidates$tau != 1) || any(candidates$dt != 1) ||
      !all(candidates$stability_class == "asymptotically_stable")) {
    lomm11_stop("Candidate grid violates stable fixed-scale constraints.")
  }
  selections <- tables$selection_plan.csv
  if (any(selections$heldout_access) || any(selections$sensitivity_access) ||
      !all(selections$training_panel_count == 81L)) {
    lomm11_stop("Selection plan permits leakage or has wrong dependencies.")
  }
  expected_checkpoint <- nrow(training) + nrow(evaluations)
  expected_generated <- nrow(training) + nrow(heldout)
  if (expected_checkpoint != as.integer(config$expected_counts$checkpoint_units) ||
      expected_generated != as.integer(config$expected_counts$generated_panels)) {
    lomm11_stop("Generated-panel or checkpoint denominator is invalid.")
  }
  if (!identical(
    plan$target_contract$randomness,
    lomm11_rng_provenance_contract(config)
  )) {
    lomm11_stop("Scientific-contract RNG provenance is invalid.")
  }
  invisible(TRUE)
}
