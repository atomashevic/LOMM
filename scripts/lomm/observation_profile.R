LOMM_OBSERVATION_PROFILE_SCHEMA_VERSION <- 2L
LOMM_OBSERVATION_PROFILES <- c(
  "compositional_bs", "loading_aligned_bs", "homogeneous"
)
LOMM_BASELINE_MODES <- c("empirical", "loading_aligned", "none")
LOMM_INTERNAL_PROFILE_ID <- "lomm_1_1_mafw_clip_bs_v1"
LOMM_INTERNAL_CONFIG_SHA256 <-
  "f889b21161a3b201c29e6272972adb05aa410c80341fb1c2e1ccff4f330c2e83"
LOMM_INTERNAL_IMPLEMENTATION_SHA256 <-
  "07ee2ff1896a0bb9985df5ca0242db83a39cf0a1ca181e0ba73e1962ba0d11c0"
LOMM_CALIBRATION_RUN_CONFIG_HASH <-
  "854e8722d647ae1564a58459b66f66c87abf91217258fb04d0360ed26d8a4729"
LOMM_RNG_STATE_VERSION <- "lomm11_lecuyer_cmrg_v1"
LOMM_RNG_STATE_DERIVATION <- "sha256_full_key_to_lecuyer_cmrg_v1"
LOMM_RNG_SHARED_SEGMENT <- "__clip_shared__"
LOMM_RNG_COMPONENTS <- c("between_clip_baseline", "clip_scale")

lomm_profile_stop <- function(...) {
  stop(..., call. = FALSE)
}

lomm_profile_detect_script_path <- function() {
  frame_paths <- unlist(lapply(sys.frames(), function(frame) {
    paths <- c(frame$ofile, frame$file)
    as.character(paths[lengths(paths) == 1L])
  }), use.names = FALSE)
  candidates <- frame_paths[nzchar(frame_paths)]
  matches <- candidates[basename(candidates) == "observation_profile.R"]
  if (length(matches) > 0L) {
    resolved_matches <- vapply(matches, function(path) {
      if (file.exists(path)) return(normalizePath(path, mustWork = TRUE))
      local_path <- file.path(getwd(), basename(path))
      if (file.exists(local_path)) {
        return(normalizePath(local_path, mustWork = TRUE))
      }
      NA_character_
    }, character(1))
    matches <- resolved_matches[!is.na(resolved_matches)]
  }
  if (length(matches) == 0L) {
    fallback <- file.path(getwd(), "scripts", "lomm", "observation_profile.R")
    if (file.exists(fallback)) matches <- fallback
  }
  if (length(matches) == 0L) {
    lomm_profile_stop("Cannot locate scripts/lomm/observation_profile.R.")
  }
  normalizePath(utils::tail(matches, 1L), mustWork = TRUE)
}

LOMM_OBSERVATION_PROFILE_SCRIPT <- lomm_profile_detect_script_path()
LOMM_REPO_ROOT <- normalizePath(
  file.path(dirname(LOMM_OBSERVATION_PROFILE_SCRIPT), "..", ".."),
  mustWork = TRUE
)
LOMM_INTERNAL_IMPLEMENTATION <- file.path(
  LOMM_REPO_ROOT, "scripts", "lomm_1_1", "canonical_observation_profile.R"
)
LOMM_INTERNAL_CONFIG <- file.path(
  LOMM_REPO_ROOT, "scripts", "lomm_1_1", "canonical_observation_profile_config.json"
)

if (!requireNamespace("digest", quietly = TRUE)) {
  lomm_profile_stop("Package 'digest' is required.")
}
if (!file.exists(LOMM_INTERNAL_IMPLEMENTATION) || !file.exists(LOMM_INTERNAL_CONFIG)) {
  lomm_profile_stop("A retained internal canonical observation source is missing.")
}
LOMM_PUBLIC_IMPLEMENTATION_SHA256 <- digest::digest(
  file = LOMM_OBSERVATION_PROFILE_SCRIPT, algo = "sha256"
)
LOMM_INTERNAL_IMPLEMENTATION_BYTES <- readBin(
  LOMM_INTERNAL_IMPLEMENTATION, what = "raw",
  n = file.info(LOMM_INTERNAL_IMPLEMENTATION)$size
)
LOMM_INTERNAL_IMPLEMENTATION_OBSERVED_SHA256 <- digest::digest(
  LOMM_INTERNAL_IMPLEMENTATION_BYTES, algo = "sha256", serialize = FALSE
)
LOMM_INTERNAL_CONFIG_OBSERVED_SHA256 <- digest::digest(
  file = LOMM_INTERNAL_CONFIG, algo = "sha256"
)
if (!identical(
    LOMM_INTERNAL_IMPLEMENTATION_OBSERVED_SHA256,
    LOMM_INTERNAL_IMPLEMENTATION_SHA256
  ) || !identical(LOMM_INTERNAL_CONFIG_OBSERVED_SHA256, LOMM_INTERNAL_CONFIG_SHA256)) {
  lomm_profile_stop("A retained internal canonical observation source changed.")
}
eval(
  parse(
    text = rawToChar(LOMM_INTERNAL_IMPLEMENTATION_BYTES),
    srcfile = LOMM_INTERNAL_IMPLEMENTATION
  ),
  envir = environment()
)
rm(LOMM_INTERNAL_IMPLEMENTATION_BYTES)

lomm_profile_require_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      lomm_profile_stop("Package '", package, "' is required.")
    }
  }
  invisible(TRUE)
}

lomm_profile_require_fields <- function(value, expected, label) {
  fields <- names(value)
  if (!is.list(value) || length(fields) != length(expected) ||
      anyDuplicated(fields) || !setequal(fields, expected)) {
    lomm_profile_stop(label, " fields differ from the public schema.")
  }
  invisible(TRUE)
}

lomm_profile_scalar_string <- function(value, label) {
  if (length(value) != 1L || !is.character(value) || is.na(value) ||
      !nzchar(value)) {
    lomm_profile_stop(label, " must be one non-empty string.")
  }
  value
}

lomm_profile_scalar_number <- function(value, label, positive = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L && is.finite(value)
  if (valid && positive) valid <- value > 0
  if (!valid) {
    qualifier <- if (positive) "positive " else ""
    lomm_profile_stop(label, " must be one ", qualifier, "finite number.")
  }
  as.numeric(value)
}

lomm_profile_scalar_logical <- function(value, label) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    lomm_profile_stop(label, " must be one logical value.")
  }
  value
}

lomm_profile_expect <- function(value, expected, label) {
  if (!identical(value, expected)) {
    lomm_profile_stop(label, " differs from the public contract.")
  }
  invisible(TRUE)
}

lomm_profile_character_vector <- function(value, label) {
  output <- unname(unlist(value, recursive = FALSE, use.names = FALSE))
  if (!is.character(output) || length(output) == 0L || anyNA(output) ||
      any(!nzchar(output))) {
    lomm_profile_stop(label, " must contain non-empty strings.")
  }
  output
}

lomm_profile_validate_sha256 <- function(value, label) {
  value <- lomm_profile_scalar_string(value, label)
  if (!grepl("^[0-9a-f]{64}$", value)) {
    lomm_profile_stop(label, " must be a lowercase SHA-256 digest.")
  }
  invisible(TRUE)
}

lomm_profile_validate_relative_path <- function(value, label) {
  value <- lomm_profile_scalar_string(value, label)
  invalid <- startsWith(value, "/") || grepl("(^|/)\\.\\.(/|$)", value) ||
    grepl("[\r\n]", value)
  if (invalid) {
    lomm_profile_stop(label, " must be a normalized repository-relative path.")
  }
  invisible(TRUE)
}

lomm_default_profile_config_path <- function() {
  file.path(dirname(LOMM_OBSERVATION_PROFILE_SCRIPT), "observation_profile_config.json")
}

lomm_profile_validate_public_api <- function(api) {
  expected <- list(
    api_version = 1L,
    rng_state_version = LOMM_RNG_STATE_VERSION,
    rng_state_derivation = LOMM_RNG_STATE_DERIVATION,
    shared_segment = LOMM_RNG_SHARED_SEGMENT,
    component_streams = as.list(LOMM_RNG_COMPONENTS)
  )
  lomm_profile_require_fields(api, names(expected), "Public API contract")
  lomm_profile_expect(api, expected, "Public API contract")
  invisible(TRUE)
}

lomm_profile_validate_profiles <- function(config) {
  if (!is.list(config$profiles) ||
      !identical(names(config$profiles), LOMM_OBSERVATION_PROFILES)) {
    lomm_profile_stop("Public profile names or order are invalid.")
  }
  expected <- list(
    compositional_bs = list(
      baseline_mode = "empirical", clip_scale_enabled = TRUE,
      activity_gate_enabled = FALSE,
      default_observation_surface = "bounded_composition"
    ),
    loading_aligned_bs = list(
      baseline_mode = "loading_aligned", clip_scale_enabled = TRUE,
      activity_gate_enabled = FALSE,
      default_observation_surface = "linear_contrast"
    ),
    homogeneous = list(
      baseline_mode = "none", clip_scale_enabled = FALSE,
      activity_gate_enabled = FALSE,
      default_observation_surface = "linear_contrast"
    )
  )
  for (profile_name in names(expected)) {
    profile <- config$profiles[[profile_name]]
    lomm_profile_require_fields(
      profile,
      c(
        "baseline_mode", "clip_scale_enabled", "activity_gate_enabled",
        "default_observation_surface"
      ),
      paste("Profile", profile_name)
    )
    lomm_profile_expect(profile, expected[[profile_name]], paste("Profile", profile_name))
  }
  invisible(TRUE)
}

lomm_profile_validate_baseline_modes <- function(config) {
  if (!is.list(config$baseline_modes) ||
      !identical(names(config$baseline_modes), LOMM_BASELINE_MODES)) {
    lomm_profile_stop("Baseline-mode names or order are invalid.")
  }
  expected <- list(
    empirical = list(
      law = "zero_mean_multivariate_gaussian",
      space = "identified_contrast",
      requires = "named_positive_semidefinite_between_clip_covariance"
    ),
    loading_aligned = list(
      law = "zero_mean_loading_coefficient_gaussian",
      space = "column_space_of_Lambda",
      requires = "full_column_rank_loadings_and_beta_covariance_or_beta_sd",
      equation = "b_i=Lambda*beta_i"
    ),
    none = list(
      law = "point_mass_zero", space = "identified_contrast", requires = "none"
    )
  )
  for (mode in names(expected)) {
    lomm_profile_require_fields(
      config$baseline_modes[[mode]], names(expected[[mode]]),
      paste("Baseline mode", mode)
    )
    lomm_profile_expect(
      config$baseline_modes[[mode]], expected[[mode]], paste("Baseline mode", mode)
    )
  }
  invisible(TRUE)
}

lomm_profile_validate_scale <- function(scale) {
  lomm_profile_require_fields(
    scale, c("law", "lower", "upper", "population_mean", "placement"),
    "Clip-scale contract"
  )
  lomm_profile_expect(scale$law, "uniform", "Clip-scale law")
  lower <- lomm_profile_scalar_number(scale$lower, "Clip-scale lower", TRUE)
  upper <- lomm_profile_scalar_number(scale$upper, "Clip-scale upper", TRUE)
  mean_value <- lomm_profile_scalar_number(
    scale$population_mean, "Clip-scale population mean", TRUE
  )
  if (abs(lower - 0.5) > 1e-12 || abs(upper - 1.5) > 1e-12 ||
      abs(mean_value - 1) > 1e-12) {
    lomm_profile_stop("Clip-scale bounds or mean differ from the public contract.")
  }
  lomm_profile_expect(
    scale$placement, "multiplies_dynamic_signal_plus_residual",
    "Clip-scale placement"
  )
  invisible(TRUE)
}

lomm_profile_validate_calibrated_defaults <- function(defaults) {
  lomm_profile_require_fields(
    defaults,
    c(
      "period_frames", "damping_ratio", "kappa", "tau", "residual_sd",
      "activity_gate_enabled"
    ),
    "Calibrated defaults"
  )
  expected <- c(
    period_frames = 12, damping_ratio = 0.5, kappa = 1,
    tau = 1, residual_sd = 0.25
  )
  for (field in names(expected)) {
    observed <- lomm_profile_scalar_number(defaults[[field]], paste("Default", field))
    if (abs(observed - expected[[field]]) > 1e-12) {
      lomm_profile_stop("Default ", field, " differs from the calibrated preset.")
    }
  }
  if (lomm_profile_scalar_logical(
      defaults$activity_gate_enabled, "Default activity gate"
    )) {
    lomm_profile_stop("The calibrated default must keep activity gates disabled.")
  }
  invisible(TRUE)
}

lomm_profile_validate_internal_source <- function(contract) {
  lomm_profile_require_fields(
    contract,
    c(
      "schema_version", "profile_id", "role", "config_path", "config_sha256",
      "implementation_path", "implementation_sha256",
      "calibration_run_config_hash"
    ),
    "Internal source contract"
  )
  lomm_profile_expect(contract$schema_version, 1L, "Internal schema version")
  lomm_profile_expect(contract$profile_id, LOMM_INTERNAL_PROFILE_ID, "Internal profile ID")
  lomm_profile_expect(contract$role, "calibration_ancestry", "Internal profile role")
  lomm_profile_expect(
    contract$config_path,
    "scripts/lomm_1_1/canonical_observation_profile_config.json",
    "Internal config path"
  )
  lomm_profile_expect(
    contract$config_sha256, LOMM_INTERNAL_CONFIG_SHA256, "Internal config hash"
  )
  lomm_profile_expect(
    contract$implementation_path,
    "scripts/lomm_1_1/canonical_observation_profile.R",
    "Internal implementation path"
  )
  lomm_profile_expect(
    contract$implementation_sha256, LOMM_INTERNAL_IMPLEMENTATION_SHA256,
    "Internal implementation hash"
  )
  for (field in c(
    "config_path", "implementation_path"
  )) lomm_profile_validate_relative_path(contract[[field]], paste("Internal", field))
  for (field in c(
    "config_sha256", "implementation_sha256", "calibration_run_config_hash"
  )) lomm_profile_validate_sha256(contract[[field]], paste("Internal", field))
  lomm_profile_expect(
    contract$calibration_run_config_hash, LOMM_CALIBRATION_RUN_CONFIG_HASH,
    "Calibration run config hash"
  )
  invisible(TRUE)
}

lomm_profile_validate_claim_scope <- function(scope) {
  lomm_profile_require_fields(scope, c("supported", "excluded"), "Claim scope")
  supported <- lomm_profile_character_vector(scope$supported, "Supported claims")
  excluded <- lomm_profile_character_vector(scope$excluded, "Excluded claims")
  expected_supported <- c(
    "calibrated_default_for_the_same_retained_100_clip_MAFW_CLIP_score_surface",
    "exact_homogeneous_nested_sensitivity",
    "loading_aligned_known_truth_recovery_option"
  )
  expected_excluded <- c(
    "bounded_probability_recovery_validation", "emotion_factor_validity",
    "classifier_validity", "external_dataset_generalization",
    "causal_affect_mechanism"
  )
  lomm_profile_expect(supported, expected_supported, "Supported claim scope")
  lomm_profile_expect(excluded, expected_excluded, "Excluded claim scope")
  invisible(TRUE)
}

lomm_profile_validate_hashes <- function(config, repo_root) {
  repo_root <- normalizePath(repo_root, mustWork = TRUE)
  contract <- config$internal_source_contract
  files <- c(
    config = contract$config_path,
    implementation = contract$implementation_path
  )
  expected <- c(
    config = contract$config_sha256,
    implementation = contract$implementation_sha256
  )
  absolute <- file.path(repo_root, unname(files))
  if (any(!file.exists(absolute))) {
    lomm_profile_stop("A provenance-locked source file is missing.")
  }
  observed <- vapply(absolute, lomm11_profile_file_sha256, character(1))
  if (!identical(unname(observed), unname(expected))) {
    lomm_profile_stop("A provenance-locked source hash changed.")
  }
  invisible(TRUE)
}

lomm_validate_observation_profile_config <- function(
    config, repo_root = LOMM_REPO_ROOT, validate_internal_hashes = FALSE) {
  lomm_profile_require_fields(
    config,
    c(
      "schema_version", "model_name", "default_profile", "public_api", "profiles",
      "baseline_modes", "clip_scale", "calibrated_defaults",
      "internal_source_contract", "claim_scope"
    ),
    "Public observation profile"
  )
  lomm_profile_expect(
    config$schema_version, LOMM_OBSERVATION_PROFILE_SCHEMA_VERSION,
    "Public schema version"
  )
  lomm_profile_expect(config$model_name, "LOMM", "Public model name")
  lomm_profile_expect(config$default_profile, "compositional_bs", "Default profile")
  lomm_profile_validate_public_api(config$public_api)
  lomm_profile_validate_profiles(config)
  lomm_profile_validate_baseline_modes(config)
  lomm_profile_validate_scale(config$clip_scale)
  lomm_profile_validate_calibrated_defaults(config$calibrated_defaults)
  lomm_profile_validate_internal_source(config$internal_source_contract)
  lomm_profile_validate_claim_scope(config$claim_scope)
  if (validate_internal_hashes) {
    lomm_profile_validate_hashes(config, repo_root)
  }
  invisible(TRUE)
}

lomm_profile_effective_config_hash <- function(config) {
  lomm_profile_require_packages()
  payload <- config
  for (attribute_name in c(
    "source_path", "source_sha256", "effective_sha256",
    "loaded_identity_sha256"
  )) attr(payload, attribute_name) <- NULL
  serialized <- jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE
  )
  digest::digest(serialized, algo = "sha256", serialize = FALSE)
}

lomm_profile_config_identity <- function(config) {
  lomm_validate_observation_profile_config(config)
  effective_sha256 <- lomm_profile_effective_config_hash(config)
  loaded_effective_sha256 <- attr(config, "effective_sha256", exact = TRUE)
  if (!is.null(loaded_effective_sha256) &&
      !identical(loaded_effective_sha256, effective_sha256)) {
    lomm_profile_stop("The loaded public observation-profile config changed in memory.")
  }
  source_sha256 <- attr(config, "source_sha256", exact = TRUE)
  source_path <- attr(config, "source_path", exact = TRUE)
  loaded_identity_sha256 <- attr(config, "loaded_identity_sha256", exact = TRUE)
  if (xor(is.null(source_sha256), is.null(loaded_identity_sha256))) {
    lomm_profile_stop("The loaded public observation-profile identity is incomplete.")
  }
  if (!is.null(source_sha256)) {
    lomm_profile_validate_sha256(source_sha256, "Loaded public config hash")
  }
  if (!is.null(loaded_identity_sha256)) {
    observed_identity_sha256 <- lomm_profile_object_hash(list(
      source_path = source_path,
      source_sha256 = source_sha256,
      effective_sha256 = loaded_effective_sha256
    ))
    if (!identical(loaded_identity_sha256, observed_identity_sha256)) {
      lomm_profile_stop("The loaded public observation-profile identity changed.")
    }
  }
  list(
    source_sha256 = if (is.null(source_sha256)) NA_character_ else source_sha256,
    effective_sha256 = effective_sha256
  )
}

lomm_read_observation_profile_config <- function(
    path = lomm_default_profile_config_path(), validate_internal_hashes = TRUE) {
  lomm_profile_require_packages()
  path <- normalizePath(path, mustWork = TRUE)
  config_bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  source_sha256 <- digest::digest(
    config_bytes, algo = "sha256", serialize = FALSE
  )
  config <- jsonlite::fromJSON(rawToChar(config_bytes), simplifyVector = FALSE)
  attr(config, "source_path") <- path
  attr(config, "source_sha256") <- source_sha256
  attr(config, "effective_sha256") <- lomm_profile_effective_config_hash(config)
  attr(config, "loaded_identity_sha256") <- lomm_profile_object_hash(list(
    source_path = path,
    source_sha256 = source_sha256,
    effective_sha256 = attr(config, "effective_sha256", exact = TRUE)
  ))
  lomm_validate_observation_profile_config(
    config,
    validate_internal_hashes = validate_internal_hashes
  )
  config
}

lomm_resolve_observation_profile <- function(
    profile = NULL, baseline_mode = NULL, clip_scale_enabled = NULL,
    config = lomm_read_observation_profile_config()) {
  config_identity <- lomm_profile_config_identity(config)
  profile_name <- if (is.null(profile)) config$default_profile else
    lomm_profile_scalar_string(profile, "Profile")
  if (!profile_name %in% names(config$profiles)) {
    lomm_profile_stop("Unknown public LOMM observation profile: ", profile_name, ".")
  }
  resolved <- config$profiles[[profile_name]]
  if (!is.null(baseline_mode)) {
    baseline_mode <- lomm_profile_scalar_string(baseline_mode, "Baseline mode")
    if (!baseline_mode %in% LOMM_BASELINE_MODES) {
      lomm_profile_stop("Unknown LOMM baseline mode: ", baseline_mode, ".")
    }
    resolved$baseline_mode <- baseline_mode
  }
  if (!is.null(clip_scale_enabled)) {
    resolved$clip_scale_enabled <- lomm_profile_scalar_logical(
      clip_scale_enabled, "Clip-scale override"
    )
  }
  resolved$profile <- profile_name
  resolved$baseline_enabled <- !identical(resolved$baseline_mode, "none")
  resolved$public_schema_version <- config$schema_version
  resolved$public_api_version <- config$public_api$api_version
  resolved$rng_state_version <- config$public_api$rng_state_version
  resolved$rng_state_derivation <- config$public_api$rng_state_derivation
  resolved$internal_profile_id <- config$internal_source_contract$profile_id
  resolved$internal_profile_role <- config$internal_source_contract$role
  resolved$internal_schema_version <- config$internal_source_contract$schema_version
  resolved$public_config_source_sha256 <- config_identity$source_sha256
  resolved$public_effective_config_sha256 <- config_identity$effective_sha256
  class(resolved) <- c("lomm_observation_profile", "list")
  resolved
}

lomm_default_observation_profile <- function(
    config = lomm_read_observation_profile_config()) {
  lomm_resolve_observation_profile(config = config)
}

lomm_profile_validate_resolved <- function(profile, config) {
  if (!inherits(profile, "lomm_observation_profile")) {
    lomm_profile_stop("A resolved observation profile must use the public profile class.")
  }
  expected_fields <- c(
    "baseline_mode", "clip_scale_enabled", "activity_gate_enabled",
    "default_observation_surface", "profile", "baseline_enabled",
    "public_schema_version", "public_api_version", "rng_state_version",
    "rng_state_derivation", "internal_profile_id", "internal_profile_role",
    "internal_schema_version", "public_config_source_sha256",
    "public_effective_config_sha256"
  )
  lomm_profile_require_fields(unclass(profile), expected_fields, "Resolved profile")
  if (!profile$profile %in% names(config$profiles) ||
      !profile$baseline_mode %in% LOMM_BASELINE_MODES) {
    lomm_profile_stop("Resolved profile names or modes are invalid.")
  }
  base_profile <- config$profiles[[profile$profile]]
  config_identity <- lomm_profile_config_identity(config)
  valid <- is.logical(profile$clip_scale_enabled) &&
    length(profile$clip_scale_enabled) == 1L && !is.na(profile$clip_scale_enabled) &&
    identical(profile$activity_gate_enabled, base_profile$activity_gate_enabled) &&
    identical(
      profile$default_observation_surface, base_profile$default_observation_surface
    ) &&
    identical(profile$baseline_enabled, !identical(profile$baseline_mode, "none")) &&
    identical(profile$public_schema_version, config$schema_version) &&
    identical(profile$public_api_version, config$public_api$api_version) &&
    identical(profile$rng_state_version, config$public_api$rng_state_version) &&
    identical(
      profile$rng_state_derivation, config$public_api$rng_state_derivation
    ) &&
    identical(profile$internal_profile_id, config$internal_source_contract$profile_id) &&
    identical(profile$internal_profile_role, config$internal_source_contract$role) &&
    identical(
      profile$internal_schema_version, config$internal_source_contract$schema_version
    ) &&
    identical(
      profile$public_config_source_sha256, config_identity$source_sha256
    ) &&
    identical(
      profile$public_effective_config_sha256, config_identity$effective_sha256
    )
  if (!valid) lomm_profile_stop("Resolved profile differs from the loaded public contract.")
  invisible(TRUE)
}

lomm_profile_resolve_input <- function(profile, config) {
  lomm_profile_config_identity(config)
  if (inherits(profile, "lomm_observation_profile")) {
    lomm_profile_validate_resolved(profile, config)
    return(profile)
  }
  lomm_resolve_observation_profile(profile = profile, config = config)
}

lomm_profile_object_hash <- function(value) {
  lomm_profile_require_packages()
  digest::digest(
    value, algo = "sha256", serialize = TRUE, serializeVersion = 3
  )
}

lomm_profile_name_order <- function(names) {
  names <- enc2utf8(as.character(names))
  byte_keys <- vapply(names, function(name) {
    paste(sprintf("%02x", as.integer(charToRaw(name))), collapse = "")
  }, character(1))
  order(byte_keys, method = "radix")
}

lomm_profile_canonical_covariance <- function(covariance) {
  index <- lomm_profile_name_order(rownames(covariance))
  covariance[index, index, drop = FALSE]
}

lomm_profile_rng_key <- function(master_seed, clip_key, component) {
  lomm_profile_require_packages()
  master_seed <- lomm_profile_scalar_number(master_seed, "Master seed", TRUE)
  if (master_seed != as.integer(master_seed) || master_seed >= 2147483647) {
    lomm_profile_stop("Master seed must be a positive 32-bit integer.")
  }
  clip_key <- lomm_profile_scalar_string(as.character(clip_key), "Clip key")
  component <- lomm_profile_scalar_string(component, "Seed component")
  if (!component %in% LOMM_RNG_COMPONENTS) {
    lomm_profile_stop("Unknown observation-profile RNG component.")
  }
  paste(
    LOMM_RNG_STATE_VERSION,
    format(master_seed, scientific = FALSE, trim = TRUE),
    LOMM_INTERNAL_PROFILE_ID,
    enc2utf8(clip_key),
    LOMM_RNG_SHARED_SEGMENT,
    enc2utf8(component),
    sep = "\037"
  )
}

lomm_profile_rng_state <- function(key) {
  key <- lomm_profile_scalar_string(key, "RNG key")
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
    lomm_profile_stop("Observation-profile RNG state derivation failed.")
  }
  state
}

lomm_profile_validate_rng_state <- function(state) {
  moduli <- c(rep(4294967087, 3L), rep(4294944443, 3L))
  unsigned <- as.numeric(state[-1L])
  negative <- !is.na(unsigned) & unsigned < 0
  unsigned[negative] <- unsigned[negative] + 2^32
  valid <- is.integer(state) && length(state) == 7L && !anyNA(state) &&
    identical(state[[1L]], 10407L) && all(unsigned > 0) &&
    all(unsigned < moduli) && all(unsigned != 2^31)
  if (!valid) lomm_profile_stop("Invalid observation-profile RNG state.")
  invisible(TRUE)
}

lomm_profile_with_seed <- function(state, expression) {
  lomm_profile_validate_rng_state(state)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  assign(".Random.seed", state, envir = .GlobalEnv)
  force(expression)
}

lomm_profile_validate_covariance <- function(
    covariance, expected_names, label, tolerance = 1e-10) {
  covariance <- as.matrix(covariance)
  expected_names <- as.character(expected_names)
  valid <- is.numeric(covariance) &&
    identical(dim(covariance), rep(length(expected_names), 2L)) &&
    identical(rownames(covariance), expected_names) &&
    identical(colnames(covariance), expected_names) &&
    all(is.finite(covariance))
  if (!valid || max(abs(covariance - t(covariance))) > tolerance) {
    lomm_profile_stop(label, " must be a named, finite, symmetric covariance matrix.")
  }
  covariance <- (covariance + t(covariance)) / 2
  if (min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) < -tolerance) {
    lomm_profile_stop(label, " must be positive semidefinite.")
  }
  covariance
}

lomm_profile_draw_gaussian <- function(covariance, state) {
  decomposition <- eigen(covariance, symmetric = TRUE)
  standard <- lomm_profile_with_seed(state, stats::rnorm(nrow(covariance)))
  root <- sweep(
    decomposition$vectors, 2L, sqrt(pmax(decomposition$values, 0)), "*"
  ) %*% t(decomposition$vectors)
  as.numeric(root %*% standard)
}

lomm_profile_beta_covariance <- function(loadings, beta_covariance, beta_sd) {
  factor_names <- colnames(loadings)
  if (!is.null(beta_covariance) && !is.null(beta_sd)) {
    lomm_profile_stop("Supply beta_covariance or beta_sd, not both.")
  }
  if (!is.null(beta_covariance)) {
    return(lomm_profile_validate_covariance(
      beta_covariance, factor_names, "Beta covariance"
    ))
  }
  if (is.null(beta_sd)) {
    lomm_profile_stop("The loading_aligned mode requires beta_covariance or beta_sd.")
  }
  if (!is.numeric(beta_sd)) {
    lomm_profile_stop("Beta SD must be numeric.")
  }
  if (length(beta_sd) == 1L) {
    beta_sd <- rep(as.numeric(beta_sd), length(factor_names))
  } else {
    if (!identical(names(beta_sd), factor_names)) {
      lomm_profile_stop("Vector beta SD must be named in loading-factor order.")
    }
    beta_sd <- as.numeric(beta_sd)
  }
  if (length(beta_sd) != length(factor_names) || any(!is.finite(beta_sd)) ||
      any(beta_sd < 0)) {
    lomm_profile_stop("Beta SD must be finite, non-negative, and factor-aligned.")
  }
  covariance <- diag(beta_sd^2, nrow = length(beta_sd))
  dimnames(covariance) <- list(factor_names, factor_names)
  covariance
}

lomm_profile_draw_empirical_baseline <- function(
    coordinate_names, covariance, state) {
  covariance <- lomm_profile_validate_covariance(
    covariance, coordinate_names, "Between-clip covariance"
  )
  canonical_covariance <- lomm_profile_canonical_covariance(covariance)
  draw <- stats::setNames(
    lomm_profile_draw_gaussian(canonical_covariance, state),
    rownames(canonical_covariance)
  )
  draw[coordinate_names]
}

lomm_profile_draw_loading_aligned_baseline <- function(
    coordinate_names, loadings, beta_covariance, beta_sd, state) {
  lomm11_canonical_validate_loadings(loadings, coordinate_names)
  covariance <- lomm_profile_beta_covariance(loadings, beta_covariance, beta_sd)
  canonical_covariance <- lomm_profile_canonical_covariance(covariance)
  beta <- stats::setNames(
    lomm_profile_draw_gaussian(canonical_covariance, state),
    rownames(canonical_covariance)
  )[colnames(loadings)]
  baseline <- as.numeric(loadings %*% beta)
  list(
    baseline = stats::setNames(baseline, coordinate_names),
    beta = beta
  )
}

lomm_draw_clip_effects <- function(
    coordinate_names, clip_key, master_seed, profile = NULL,
    between_clip_covariance = NULL, loadings = NULL, beta_covariance = NULL,
    beta_sd = NULL, paired_reference_mode = NULL,
    config = lomm_read_observation_profile_config()) {
  lomm_profile_config_identity(config)
  coordinate_names <- as.character(coordinate_names)
  if (!is.null(loadings)) loadings <- as.matrix(loadings)
  if (length(coordinate_names) == 0L || anyNA(coordinate_names) ||
      any(!nzchar(coordinate_names)) || anyDuplicated(coordinate_names)) {
    lomm_profile_stop("Coordinate names must be unique non-empty strings.")
  }
  resolved <- lomm_profile_resolve_input(profile, config)
  reference_mode <- resolved$baseline_mode
  if (!is.null(paired_reference_mode)) {
    paired_reference_mode <- lomm_profile_scalar_string(
      paired_reference_mode, "Paired reference mode"
    )
    if (!paired_reference_mode %in% setdiff(LOMM_BASELINE_MODES, "none")) {
      lomm_profile_stop("Paired reference mode must be empirical or loading_aligned.")
    }
    if (!identical(resolved$baseline_mode, "none")) {
      lomm_profile_stop("Paired reference mode is only valid when baseline mode is none.")
    }
    reference_mode <- paired_reference_mode
  }
  baseline_rng_key <- lomm_profile_rng_key(
    master_seed, clip_key, "between_clip_baseline"
  )
  clip_scale_rng_key <- lomm_profile_rng_key(master_seed, clip_key, "clip_scale")
  baseline_rng_state <- lomm_profile_rng_state(baseline_rng_key)
  clip_scale_rng_state <- lomm_profile_rng_state(clip_scale_rng_key)
  beta <- NULL
  baseline_draw <- switch(
    reference_mode,
    empirical = lomm_profile_draw_empirical_baseline(
      coordinate_names, between_clip_covariance, baseline_rng_state
    ),
    loading_aligned = {
      draw <- lomm_profile_draw_loading_aligned_baseline(
        coordinate_names, loadings, beta_covariance, beta_sd, baseline_rng_state
      )
      beta <- draw$beta
      draw$baseline
    },
    none = stats::setNames(rep(0, length(coordinate_names)), coordinate_names),
    lomm_profile_stop("Unsupported baseline mode.")
  )
  scale_draw <- lomm_profile_with_seed(
    clip_scale_rng_state,
    stats::runif(1L, config$clip_scale$lower, config$clip_scale$upper)
  )
  baseline <- if (resolved$baseline_enabled) baseline_draw else
    stats::setNames(rep(0, length(coordinate_names)), coordinate_names)
  scale <- if (resolved$clip_scale_enabled) scale_draw else 1
  baseline_geometry <- switch(
    reference_mode,
    empirical = list(
      mode = reference_mode,
      covariance = lomm_profile_canonical_covariance(
        lomm_profile_validate_covariance(
          between_clip_covariance, coordinate_names, "Between-clip covariance"
        )
      )
    ),
    loading_aligned = {
      loading_matrix <- as.matrix(loadings)
      coordinate_order <- lomm_profile_name_order(rownames(loading_matrix))
      factor_order <- lomm_profile_name_order(colnames(loading_matrix))
      list(
        mode = reference_mode,
        loadings = loading_matrix[
          coordinate_order, factor_order, drop = FALSE
        ],
        beta_covariance = lomm_profile_canonical_covariance(
          lomm_profile_beta_covariance(
            loadings, beta_covariance, beta_sd
          )
        )
      )
    },
    none = list(mode = reference_mode)
  )
  list(
    baseline = baseline,
    baseline_draw = baseline_draw,
    beta = beta,
    clip_scale = as.numeric(scale),
    clip_scale_draw = as.numeric(scale_draw),
    baseline_rng_key = baseline_rng_key,
    baseline_rng_state = baseline_rng_state,
    clip_scale_rng_key = clip_scale_rng_key,
    clip_scale_rng_state = clip_scale_rng_state,
    baseline_reference_mode = reference_mode,
    coordinate_names_sha256 = lomm_profile_object_hash(coordinate_names),
    baseline_geometry_sha256 = lomm_profile_object_hash(baseline_geometry),
    profile = resolved
  )
}

lomm_observe <- function(
    latent_positions, loadings, residuals, center, basis = NULL,
    profile = NULL, baseline_mode = NULL, clip_scale_enabled = NULL,
    observation_surface = NULL,
    clip_key, master_seed, between_clip_covariance = NULL,
    beta_covariance = NULL, beta_sd = NULL, paired_reference_mode = NULL,
    kappa = NULL, tau = NULL,
    config = lomm_read_observation_profile_config()) {
  config_identity <- lomm_profile_config_identity(config)
  loadings <- as.matrix(loadings)
  if (!is.null(basis)) basis <- as.matrix(basis)
  resolved <- if (inherits(profile, "lomm_observation_profile")) {
    if (!is.null(baseline_mode) || !is.null(clip_scale_enabled)) {
      lomm_profile_stop("Resolved profiles cannot be combined with mode overrides.")
    }
    lomm_profile_validate_resolved(profile, config)
    profile
  } else {
    lomm_resolve_observation_profile(
      profile, baseline_mode, clip_scale_enabled, config
    )
  }
  coordinate_names <- rownames(loadings)
  effects <- lomm_draw_clip_effects(
    coordinate_names = coordinate_names,
    clip_key = clip_key,
    master_seed = master_seed,
    profile = resolved,
    between_clip_covariance = between_clip_covariance,
    loadings = loadings,
    beta_covariance = beta_covariance,
    beta_sd = beta_sd,
    paired_reference_mode = paired_reference_mode,
    config = config
  )
  if (is.null(kappa)) kappa <- config$calibrated_defaults$kappa
  if (is.null(tau)) tau <- config$calibrated_defaults$tau
  kappa <- lomm11_profile_scalar_number(kappa, "Kappa", positive = TRUE)
  tau <- lomm11_profile_scalar_number(tau, "Temperature", positive = TRUE)
  if (is.null(observation_surface)) {
    observation_surface <- resolved$default_observation_surface
  } else {
    observation_surface <- lomm_profile_scalar_string(
      observation_surface, "Observation surface"
    )
  }
  if (!observation_surface %in% c("bounded_composition", "linear_contrast")) {
    lomm_profile_stop("Observation surface must be bounded_composition or linear_contrast.")
  }
  if (identical(observation_surface, "bounded_composition") && is.null(basis)) {
    lomm_profile_stop("The bounded_composition surface requires an ILR basis.")
  }
  if (identical(observation_surface, "linear_contrast") && !is.null(basis)) {
    lomm_profile_stop("The linear_contrast surface does not accept an ILR basis.")
  }
  if (identical(observation_surface, "linear_contrast")) {
    output <- list(contrast = lomm11_canonical_contrast_profile(
      latent_positions, loadings, residuals, center, effects$baseline,
      effects$clip_scale, kappa
    ))
  } else {
    output <- lomm11_canonical_observe(
      latent_positions, loadings, residuals, center, effects$baseline,
      effects$clip_scale, basis, kappa, tau
    )
  }
  output$effects <- effects[c(
    "baseline", "baseline_draw", "beta", "clip_scale", "clip_scale_draw"
  )]
  output$metadata <- list(
    model_name = config$model_name,
    public_schema_version = config$schema_version,
    public_api_version = config$public_api$api_version,
    public_config_source_sha256 = config_identity$source_sha256,
    public_effective_config_sha256 = config_identity$effective_sha256,
    public_implementation_sha256 = LOMM_PUBLIC_IMPLEMENTATION_SHA256,
    public_profile = resolved$profile,
    internal_profile_id = resolved$internal_profile_id,
    internal_profile_role = resolved$internal_profile_role,
    internal_schema_version = resolved$internal_schema_version,
    internal_config_sha256 = config$internal_source_contract$config_sha256,
    internal_implementation_sha256 =
      config$internal_source_contract$implementation_sha256,
    baseline_mode = resolved$baseline_mode,
    baseline_enabled = resolved$baseline_enabled,
    clip_scale_enabled = resolved$clip_scale_enabled,
    activity_gate_enabled = resolved$activity_gate_enabled,
    loading_aligned_truth = identical(resolved$baseline_mode, "loading_aligned"),
    observation_surface = observation_surface,
    default_observation_surface = resolved$default_observation_surface,
    rng_state_version = resolved$rng_state_version,
    rng_state_derivation = resolved$rng_state_derivation,
    clip_key = as.character(clip_key),
    master_seed = as.integer(master_seed),
    baseline_rng_key = effects$baseline_rng_key,
    baseline_rng_state = effects$baseline_rng_state,
    clip_scale_rng_key = effects$clip_scale_rng_key,
    clip_scale_rng_state = effects$clip_scale_rng_state,
    baseline_reference_mode = effects$baseline_reference_mode,
    coordinate_names_sha256 = effects$coordinate_names_sha256,
    loading_matrix_sha256 = lomm_profile_object_hash(loadings),
    basis_sha256 = if (is.null(basis)) NA_character_ else
      lomm_profile_object_hash(basis),
    baseline_geometry_sha256 = effects$baseline_geometry_sha256,
    kappa = as.numeric(kappa),
    tau = as.numeric(tau)
  )
  output
}
