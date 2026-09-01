LOMM11_BRIDGE_SCHEMA_VERSION <- 1L
LOMM11_BRIDGE_PROFILES <- c("homogeneous", "between_clip_scale")
LOMM11_BRIDGE_METHODS <- c(
  "dynEGA", "graphicalVAR", "gimme", "static_EGA", "oracle_blocks"
)
LOMM11_BRIDGE_ALGORITHMS <- c("louvain", "leiden", "walktrap")

lomm11_bridge_stop <- function(...) stop(..., call. = FALSE)

lomm11_bridge_repo_root <- function() {
  configured <- Sys.getenv("LOMM11_BRIDGE_REPO_ROOT", "")
  candidates <- c(configured, getwd())
  candidates <- candidates[nzchar(candidates)]
  for (candidate in candidates) {
    root <- normalizePath(candidate, mustWork = FALSE)
    if (file.exists(file.path(root, "README.md")) &&
        file.exists(file.path(root, "scripts", "lomm_1_1",
                              "bridge_recovery_config.json"))) {
      return(normalizePath(root, mustWork = TRUE))
    }
  }
  lomm11_bridge_stop("Cannot locate the LOMM repository root.")
}

lomm11_bridge_require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    lomm11_bridge_stop("Missing required R packages: ", paste(missing, collapse = ", "), ".")
  }
}

lomm11_bridge_sha256 <- function(path) {
  if (!file.exists(path)) lomm11_bridge_stop("Cannot hash missing path: ", path)
  if (dir.exists(path)) {
    files <- sort(list.files(path, recursive = TRUE, full.names = TRUE,
                             include.dirs = FALSE, all.files = TRUE, no.. = TRUE))
    records <- vapply(files, function(file) {
      paste(sub(paste0("^", path, "/?"), "", file), lomm11_bridge_sha256(file), sep = "|")
    }, character(1))
    return(lomm11_bridge_hash_text(paste(records, collapse = "\n")))
  }
  output <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) lomm11_bridge_stop("sha256sum failed for ", path)
  strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
}

lomm11_bridge_hash_text <- function(value) {
  path <- tempfile("lomm11-bridge-hash-")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeChar(enc2utf8(as.character(value)), path, eos = NULL, useBytes = TRUE)
  lomm11_bridge_sha256(path)
}

lomm11_bridge_object_hash <- function(value) {
  path <- tempfile("lomm11-bridge-object-", fileext = ".rds")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  saveRDS(value, path, version = 3)
  lomm11_bridge_sha256(path)
}

lomm11_bridge_read_config <- function(root = lomm11_bridge_repo_root()) {
  lomm11_bridge_require_packages("jsonlite")
  path <- file.path(root, "scripts", "lomm_1_1", "bridge_recovery_config.json")
  config <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  lomm11_bridge_validate_config(config)
  attr(config, "path") <- normalizePath(path, mustWork = TRUE)
  config
}

lomm11_bridge_expect_fields <- function(value, expected, label) {
  if (!is.list(value) || !identical(names(value), expected)) {
    lomm11_bridge_stop(label, " fields or order changed.")
  }
  invisible(TRUE)
}

lomm11_bridge_validate_config <- function(config) {
  lomm11_bridge_expect_fields(config, c(
    "schema_version", "pipeline_id", "purpose", "source_realism_run",
    "canonical_profile", "design", "between_clip", "clip_scale", "randomness",
    "methods", "execution", "analysis", "expected_counts", "claim_scope"
  ), "Bridge configuration")
  lomm11_bridge_expect_fields(
    config$source_realism_run, c("path", "config_hash", "success_sha256"),
    "Source-realism contract"
  )
  lomm11_bridge_expect_fields(config$canonical_profile, c(
    "profile_id", "config_path", "config_sha256", "implementation_path",
    "implementation_sha256"
  ), "Canonical-profile contract")
  lomm11_bridge_expect_fields(config$design, c(
    "scenario_families", "true_dimensions", "replicates", "profiles",
    "videos_per_panel", "observations_per_video", "indicators_per_dimension",
    "primary_loading", "measurement_error_sd", "initialization",
    "representation", "community_algorithms"
  ), "Bridge design")
  lomm11_bridge_expect_fields(config$between_clip, c(
    "enabled_profile", "constraint", "factor_distribution", "factor_sd",
    "calibration_rule", "mean_fold_trace_between_clip_covariance"
  ), "Between-clip contract")
  lomm11_bridge_expect_fields(config$clip_scale, c(
    "enabled_profile", "distribution", "minimum", "maximum", "expected_value",
    "applies_to"
  ), "Clip-scale contract")
  lomm11_bridge_expect_fields(config$randomness, c(
    "profile_component_seed_origin", "base_panel_seed_origin", "pairing",
    "state_derivation",
    "profile_substreams_consumed_when_disabled", "fit_seed_pairing",
    "readout_seed_pairing"
  ), "Randomness contract")
  lomm11_bridge_expect_fields(config$execution, c(
    "workers", "blas_threads_per_worker", "graphicalvar_timeout_seconds",
    "gimme_timeout_seconds", "job_timeout_seconds", "max_attempts_per_fit",
    "authorization_token", "output_root", "run_output_root"
  ), "Execution contract")
  lomm11_bridge_expect_fields(config$analysis, c(
    "primary_outcome", "secondary_outcomes", "estimand_definitions",
    "paired_contrast", "bootstrap_replicates", "bootstrap_unit",
    "bootstrap_strata", "runtime_cap_seconds", "status_inclusive_denominator"
  ), "Analysis contract")
  lomm11_bridge_expect_fields(config$analysis$estimand_definitions, c(
    "usable", "usable_and_correct_dimension", "conditional_recovery",
    "restricted_runtime_seconds"
  ), "Estimand definitions")
  lomm11_bridge_expect_fields(config$expected_counts, c(
    "scenario_dimension_cells", "paired_panel_sets", "generated_panels",
    "structure_fits", "result_rows"
  ), "Expected-count contract")
  lomm11_bridge_expect_fields(
    config$claim_scope, c("supported", "excluded"), "Claim-scope contract"
  )

  checks <- c(
    schema_version = identical(config$schema_version, LOMM11_BRIDGE_SCHEMA_VERSION),
    pipeline_id = identical(config$pipeline_id, "lomm_1_1_block_recovery_bridge"),
    purpose = identical(
      config$purpose,
      paste(
        "Paired screening sensitivity for the effect of the canonical B+S",
        "observation profile on known-block recovery."
      )
    ),
    source_path = identical(
      config$source_realism_run$path,
      "simulation_outputs/lomm_1_1/empirical_realism_ablation_runs/854e8722d647"
    ),
    source_hash = identical(
      config$source_realism_run$config_hash,
      "854e8722d647ae1564a58459b66f66c87abf91217258fb04d0360ed26d8a4729"
    ),
    source_success = identical(
      config$source_realism_run$success_sha256,
      "63db6991f23b706da4e0e0aba001e379374ba131b1bf08746c88b4860ef96460"
    ),
    canonical_id = identical(
      config$canonical_profile$profile_id, "lomm_1_1_mafw_clip_bs_v1"
    ),
    canonical_config_path = identical(
      config$canonical_profile$config_path,
      "scripts/lomm_1_1/canonical_observation_profile_config.json"
    ),
    canonical_config_hash = identical(
      config$canonical_profile$config_sha256,
      "f889b21161a3b201c29e6272972adb05aa410c80341fb1c2e1ccff4f330c2e83"
    ),
    canonical_implementation_path = identical(
      config$canonical_profile$implementation_path,
      "scripts/lomm_1_1/canonical_observation_profile.R"
    ),
    canonical_implementation_hash = identical(
      config$canonical_profile$implementation_sha256,
      "07ee2ff1896a0bb9985df5ca0242db83a39cf0a1ca181e0ba73e1962ba0d11c0"
    ),
    scenario_families = identical(config$design$scenario_families, c(
      "old_low_single_stable", "old_high_single_stable",
      "old_high_switch_stable", "old_high_switch_volatile"
    )),
    true_dimensions = identical(config$design$true_dimensions, c(2L, 3L)),
    replicates = identical(config$design$replicates, 10L),
    profiles = identical(config$design$profiles, LOMM11_BRIDGE_PROFILES),
    panel_geometry = identical(
      unname(unlist(config$design[c(
        "videos_per_panel", "observations_per_video", "indicators_per_dimension"
      )])), c(100L, 100L, 4L)
    ),
    measurement = identical(
      unname(unlist(config$design[c("primary_loading", "measurement_error_sd")])),
      c(0.6, 0.25)
    ),
    initialization = identical(config$design$initialization, "independent_stationary"),
    representation = identical(config$design$representation, "linear_z"),
    algorithms = identical(
      config$design$community_algorithms, LOMM11_BRIDGE_ALGORITHMS
    ),
    methods = identical(config$methods, LOMM11_BRIDGE_METHODS),
    between_clip = identical(config$between_clip, list(
      enabled_profile = "between_clip_scale",
      constraint = "loading_span",
      factor_distribution = "isotropic_gaussian",
      factor_sd = 1.848702551941885,
      calibration_rule =
        "sqrt(mean_fold_trace_between_clip_covariance/10)/primary_loading",
      mean_fold_trace_between_clip_covariance = 12.303724052003171
    )),
    clip_scale = identical(config$clip_scale, list(
      enabled_profile = "between_clip_scale", distribution = "uniform",
      minimum = 0.5, maximum = 1.5, expected_value = 1,
      applies_to = "within_clip_signal_plus_measurement_residual"
    )),
    randomness = identical(config$randomness, list(
      profile_component_seed_origin = 11082026L,
      base_panel_seed_origin = 20260715L,
      pairing = "scenario_dimension_replicate",
      state_derivation = "sha256_full_key_to_lecuyer_cmrg_v1",
      profile_substreams_consumed_when_disabled = TRUE,
      fit_seed_pairing = "same_pair_method_seed_across_profiles_and_attempts",
      readout_seed_pairing =
        "same_pair_method_algorithm_seed_across_profiles_and_attempts"
    )),
    execution = identical(config$execution, list(
      workers = 8L, blas_threads_per_worker = 1L,
      graphicalvar_timeout_seconds = 300L, gimme_timeout_seconds = 600L,
      job_timeout_seconds = 21600L, max_attempts_per_fit = 2L,
      authorization_token = "LOMM-1.1-BRIDGE-AUTHORIZED",
      output_root = "simulation_outputs/lomm_1_1/recovery_bridge",
      run_output_root = "simulation_outputs/lomm_1_1/recovery_bridge_runs"
    )),
    primary_outcome = identical(
      config$analysis$primary_outcome, "usable_and_correct_dimension"
    ),
    secondary_outcomes = identical(config$analysis$secondary_outcomes, c(
      "usable", "dimension_exact_conditional_on_usable", "assignment_accuracy",
      "adjusted_rand_index", "variation_information", "restricted_runtime_seconds"
    )),
    estimands = identical(config$analysis$estimand_definitions, list(
      usable = paste(
        "status_ok_and_finite_dimension; all 80 paired panels remain in each",
        "profile denominator"
      ),
      usable_and_correct_dimension = paste(
        "status_ok_and_dimension_exact; all 80 paired panels remain in each",
        "profile denominator"
      ),
      conditional_recovery = paste(
        "profile summaries condition on that profile being usable; paired effects",
        "use only pair keys with finite values in both profiles"
      ),
      restricted_runtime_seconds = paste(
        "min(recorded_runtime_seconds,900); missing or nonfinite runtime is assigned",
        "900 seconds"
      )
    )),
    analysis_controls = identical(config$analysis[c(
      "paired_contrast", "bootstrap_replicates", "bootstrap_unit",
      "bootstrap_strata", "runtime_cap_seconds", "status_inclusive_denominator"
    )], list(
      paired_contrast = "between_clip_scale_minus_homogeneous",
      bootstrap_replicates = 2000L, bootstrap_unit = "paired_panel",
      bootstrap_strata = "scenario_family_by_true_dimension",
      runtime_cap_seconds = 900L, status_inclusive_denominator = TRUE
    )),
    counts = identical(unname(unlist(config$expected_counts)), c(
      8L, 80L, 160L, 800L, 2400L
    )),
    supported_scope = identical(
      config$claim_scope$supported,
      paste(
        "screening evidence about whether block-preserving B+S changes known-block",
        "recovery in the selected stable T=100 conditions"
      )
    ),
    excluded_scope = identical(config$claim_scope$excluded, c(
      "external_dataset_generalization", "classifier_validity",
      "affect_mechanism_identification", "equivalence across the complete revision_v2 grid",
      "causal claims about empirical emotion dynamics"
    ))
  )
  failed <- names(checks)[!checks]
  if (length(failed) > 0L) {
    lomm11_bridge_stop(
      "Bridge configuration violates locked fields: ", paste(failed, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

lomm11_bridge_validate_source_run <- function(config,
                                               root = lomm11_bridge_repo_root()) {
  canonical_paths <- c(
    config = config$canonical_profile$config_path,
    implementation = config$canonical_profile$implementation_path
  )
  canonical_hashes <- c(
    config = config$canonical_profile$config_sha256,
    implementation = config$canonical_profile$implementation_sha256
  )
  observed_canonical_hashes <- vapply(
    file.path(root, canonical_paths), lomm11_bridge_sha256, character(1)
  )
  if (!identical(unname(observed_canonical_hashes), unname(canonical_hashes))) {
    lomm11_bridge_stop("The canonical B+S profile source or contract changed.")
  }
  if (!exists("lomm11_canonical_validate_provenance", mode = "function")) {
    source(file.path(root, config$canonical_profile$implementation_path))
  }
  canonical_config <- lomm11_canonical_read_config(
    file.path(root, config$canonical_profile$config_path)
  )
  if (!identical(canonical_config$profile_id, config$canonical_profile$profile_id)) {
    lomm11_bridge_stop("The canonical profile identifier changed.")
  }
  lomm11_canonical_validate_provenance(canonical_config, root)
  source_dir <- file.path(root, config$source_realism_run$path)
  success <- file.path(source_dir, "_SUCCESS.json")
  if (!file.exists(success) ||
      !identical(lomm11_bridge_sha256(success), config$source_realism_run$success_sha256)) {
    lomm11_bridge_stop("The certified realism success receipt is missing or changed.")
  }
  lomm11_bridge_require_packages("jsonlite")
  receipt <- jsonlite::fromJSON(success, simplifyVector = TRUE)
  if (!identical(receipt$status, "complete") ||
      !identical(receipt$config_hash, config$source_realism_run$config_hash) ||
      !identical(as.integer(receipt$checkpoint_units), 11240L) ||
      !identical(as.integer(receipt$failed_panel_attempts), 0L)) {
    lomm11_bridge_stop("The source realism receipt does not satisfy the bridge contract.")
  }
  invisible(receipt)
}

lomm11_bridge_source_dependencies <- function(root = lomm11_bridge_repo_root()) {
  files <- c(
    "scripts/lomm_1_1/bridge_recovery_config.json",
    "scripts/lomm_1_1/bridge_recovery_spec.R",
    "scripts/lomm_1_1/build_bridge_recovery_plan.R",
    "scripts/lomm_1_1/run_bridge_recovery.R",
    "scripts/lomm_1_1/test_bridge_recovery.R",
    "scripts/lomm_1_1/canonical_observation_profile.R",
    "scripts/lomm_1_1/canonical_observation_profile_config.json",
    "scripts/lomm_1_1/realism_ablation_spec.R",
    "scripts/lomm_1_1/realism_generator.R",
    "scripts/simulations/revision_spec.R",
    "scripts/simulations/run_revision_job.R",
    "scripts/simulations/run_method_simulation.R",
    "scripts/run_external_structure_fit.R",
    "scripts/extract.r"
  )
  existing <- file.exists(file.path(root, files))
  if (any(!existing)) {
    lomm11_bridge_stop("Bridge dependency files are missing: ",
                       paste(files[!existing], collapse = ", "))
  }
  stats::setNames(vapply(file.path(root, files), lomm11_bridge_sha256, character(1)), files)
}

lomm11_bridge_load_pure_dependencies <- function(root = lomm11_bridge_repo_root()) {
  if (!exists("revision_challenge_base_grid", mode = "function")) {
    source(file.path(root, "scripts", "simulations", "revision_spec.R"))
  }
  if (!exists("lomm11_component_rng_state", mode = "function")) {
    source(file.path(root, "scripts", "lomm_1_1", "realism_ablation_spec.R"))
  }
  if (!exists("lomm11_with_seed", mode = "function")) {
    source(file.path(root, "scripts", "lomm_1_1", "realism_generator.R"))
  }
  invisible(TRUE)
}

lomm11_bridge_pair_plan <- function(config = lomm11_bridge_read_config(),
                                    root = lomm11_bridge_repo_root()) {
  lomm11_bridge_load_pure_dependencies(root)
  base <- revision_challenge_base_grid(as.integer(config$design$replicates))
  keep <- base$challenge_id == "independent_initialization" &
    base$scenario_family %in% config$design$scenario_families &
    base$scenario_nfac %in% config$design$true_dimensions
  plan <- base[keep, , drop = FALSE]
  plan$pair_key <- sprintf("bridge|%s|R%03d", plan$scenario, plan$rep)
  plan$challenge_pair_key <- plan$pair_key
  plan$seed_base <- vapply(plan$pair_key, revision_seed_from_key, integer(1))
  plan$profile_count <- length(LOMM11_BRIDGE_PROFILES)
  plan$method_count <- length(LOMM11_BRIDGE_METHODS)
  plan$readouts_per_pair <- length(LOMM11_BRIDGE_PROFILES) *
    length(LOMM11_BRIDGE_METHODS) * length(LOMM11_BRIDGE_ALGORITHMS)
  plan <- plan[order(plan$scenario_family, plan$scenario_nfac, plan$rep), , drop = FALSE]
  rownames(plan) <- NULL
  cells <- table(
    factor(plan$scenario_family, levels = config$design$scenario_families),
    factor(plan$scenario_nfac, levels = config$design$true_dimensions)
  )
  replicate_sets <- split(
    plan$rep, interaction(plan$scenario_family, plan$scenario_nfac, drop = TRUE)
  )
  expected_scenarios <- as.vector(outer(
    config$design$scenario_families,
    paste0(config$design$true_dimensions, "d"),
    paste,
    sep = "_"
  ))
  design_key <- paste(plan$scenario_family, plan$scenario_nfac, plan$rep, sep = "|")
  valid_contract <- nrow(plan) == config$expected_counts$paired_panel_sets &&
    !anyDuplicated(plan$pair_key) && !anyDuplicated(design_key) &&
    all(cells == config$design$replicates) &&
    all(vapply(replicate_sets, function(value) {
      identical(sort(as.integer(value)), seq_len(config$design$replicates))
    }, logical(1))) &&
    identical(sort(unique(plan$scenario)), sort(expected_scenarios)) &&
    identical(REVISION_SEED_ORIGIN, config$randomness$base_panel_seed_origin) &&
    all(plan$challenge_id == "independent_initialization") &&
    all(plan$stability_class == "asymptotically_stable") &&
    all(plan$representation == "linear_z") && all(plan$N == 100L) &&
    all(plan$TimeP == 100L) && all(plan$NVAR == 4L) &&
    all(plan$Error_sd == 0.25) && all(plan$Loadings == 0.6) &&
    all(plan$readouts_per_pair == 30L) &&
    identical(
      plan$pair_key,
      sprintf("bridge|%s|R%03d", plan$scenario, plan$rep)
    )
  if (!valid_contract) {
    lomm11_bridge_stop("Bridge pair-plan construction failed its exact-count contract.")
  }
  plan
}

lomm11_bridge_flatten_plan <- function(plan) {
  output <- plan
  list_columns <- names(output)[vapply(output, is.list, logical(1))]
  for (column in list_columns) {
    output[[column]] <- vapply(output[[column]], function(value) {
      paste(as.character(value), collapse = ";")
    }, character(1))
  }
  output
}

lomm11_bridge_profile_row <- function(pair_row, profile, config_hash) {
  if (!profile %in% LOMM11_BRIDGE_PROFILES) lomm11_bridge_stop("Unknown bridge profile.")
  row <- pair_row
  row$profile_id <- profile
  row$panel_key <- paste(row$pair_key, profile, sep = "|")
  row$representation <- "linear_z"
  row$representation_key <- paste(row$panel_key, row$representation, sep = "|")
  row$config_hash <- config_hash
  row
}

lomm11_bridge_config_identity <- function(config, dependency_hashes, pair_plan) {
  config_payload <- unclass(config)
  attr(config_payload, "path") <- NULL
  payload <- list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    config = config_payload,
    dependencies = as.list(dependency_hashes),
    pair_plan_hash = lomm11_bridge_object_hash(pair_plan)
  )
  lomm11_bridge_object_hash(payload)
}

lomm11_bridge_draw_profile_components <- function(pair_key, video, n_factors,
                                                   config) {
  master_seed <- as.integer(config$randomness$profile_component_seed_origin)
  segment <- LOMM11_CLIP_SHARED_SEGMENT
  baseline_seed <- lomm11_component_rng_state(
    pair_key, as.character(video), segment, "between_clip_baseline", master_seed
  )
  scale_seed <- lomm11_component_rng_state(
    pair_key, as.character(video), segment, "clip_scale", master_seed
  )
  beta <- lomm11_with_seed(baseline_seed, function() {
    stats::rnorm(n_factors, sd = as.numeric(config$between_clip$factor_sd))
  })
  scale <- lomm11_with_seed(scale_seed, function() {
    stats::runif(
      1L, as.numeric(config$clip_scale$minimum), as.numeric(config$clip_scale$maximum)
    )
  })
  list(beta = beta, scale = scale,
       baseline_seed = baseline_seed, scale_seed = scale_seed)
}

lomm11_bridge_apply_profile <- function(sim_data, pair_row, profile, config) {
  lomm11_bridge_load_pure_dependencies()
  if (!profile %in% LOMM11_BRIDGE_PROFILES) lomm11_bridge_stop("Unknown bridge profile.")
  required <- c(
    "panel", "subject_list", "latents", "item_names", "true_membership",
    "NFAC_true", "NVAR", "N_videos"
  )
  if (!is.list(sim_data) || any(!required %in% names(sim_data))) {
    lomm11_bridge_stop("Bridge simulation input is incomplete.")
  }
  n_factors <- as.integer(sim_data$NFAC_true)
  loadings <- revision_loading_matrix(
    n_factors, as.integer(sim_data$NVAR), as.numeric(config$design$primary_loading)
  )
  transformed <- sim_data
  beta_draws <- matrix(NA_real_, nrow = sim_data$N_videos, ncol = n_factors)
  scales <- numeric(sim_data$N_videos)
  baselines <- matrix(NA_real_, nrow = sim_data$N_videos, ncol = length(sim_data$item_names))
  baseline_states <- vector("list", sim_data$N_videos)
  scale_states <- vector("list", sim_data$N_videos)
  for (video in seq_len(sim_data$N_videos)) {
    draws <- lomm11_bridge_draw_profile_components(
      pair_row$pair_key[[1L]], video, n_factors, config
    )
    baseline <- as.numeric(loadings %*% draws$beta)
    source <- as.matrix(sim_data$subject_list[[video]])
    source_latent <- as.matrix(sim_data$latents[[video]])
    if (identical(profile, "between_clip_scale")) {
      observed <- sweep(draws$scale * source, 2L, baseline, "+")
      latent <- sweep(draws$scale * source_latent, 2L, draws$beta, "+")
      colnames(observed) <- sim_data$item_names
      transformed$subject_list[[video]] <- observed
      transformed$latents[[video]] <- latent
      row_index <- transformed$panel$id == video
      transformed$panel[row_index, sim_data$item_names] <- observed
    }
    beta_draws[video, ] <- draws$beta
    scales[[video]] <- draws$scale
    baselines[video, ] <- baseline
    baseline_states[[video]] <- draws$baseline_seed
    scale_states[[video]] <- draws$scale_seed
  }
  transformed$bridge_truth <- list(
    pair_key = pair_row$pair_key[[1L]],
    profile_id = profile,
    loadings = loadings,
    factor_baselines = beta_draws,
    observed_baselines = baselines,
    clip_scales = scales,
    baseline_states = baseline_states,
    scale_states = scale_states,
    source_panel_hash = lomm11_bridge_object_hash(sim_data$panel),
    transformed_panel_hash = lomm11_bridge_object_hash(transformed$panel),
    source_latent_hash = lomm11_bridge_object_hash(sim_data$latents),
    transformed_latent_hash = lomm11_bridge_object_hash(transformed$latents),
    true_membership_hash = lomm11_bridge_object_hash(sim_data$true_membership)
  )
  transformed
}

lomm11_bridge_validate_profile_pair <- function(core, bs, pair_row, config,
                                                tolerance = 1e-10) {
  if (!identical(core$bridge_truth$source_panel_hash,
                 core$bridge_truth$transformed_panel_hash) ||
      !identical(core$bridge_truth$source_latent_hash,
                 core$bridge_truth$transformed_latent_hash) ||
      !identical(core$bridge_truth$pair_key, pair_row$pair_key[[1L]]) ||
      !identical(bs$bridge_truth$pair_key, pair_row$pair_key[[1L]]) ||
      !identical(
        core$bridge_truth$true_membership_hash,
        bs$bridge_truth$true_membership_hash
      ) ||
      !identical(
        core$bridge_truth$true_membership_hash,
        lomm11_bridge_object_hash(core$true_membership)
      ) ||
      !identical(core$true_membership, bs$true_membership)) {
    lomm11_bridge_stop("The homogeneous bridge profile is not exactly nested.")
  }
  if (!identical(core$bridge_truth$factor_baselines,
                 bs$bridge_truth$factor_baselines) ||
      !identical(core$bridge_truth$clip_scales, bs$bridge_truth$clip_scales)) {
    lomm11_bridge_stop("Bridge profiles did not consume identical paired component draws.")
  }
  loadings <- bs$bridge_truth$loadings
  projector <- loadings %*% solve(crossprod(loadings)) %*% t(loadings)
  residual <- bs$bridge_truth$observed_baselines -
    bs$bridge_truth$observed_baselines %*% projector
  transformation_error <- vapply(seq_len(core$N_videos), function(video) {
    expected_observed <- sweep(
      bs$bridge_truth$clip_scales[[video]] * core$subject_list[[video]],
      2L,
      bs$bridge_truth$observed_baselines[video, ],
      "+"
    )
    expected_latent <- sweep(
      bs$bridge_truth$clip_scales[[video]] * core$latents[[video]],
      2L,
      bs$bridge_truth$factor_baselines[video, ],
      "+"
    )
    max(
      abs(expected_observed - bs$subject_list[[video]]),
      abs(expected_latent - bs$latents[[video]])
    )
  }, numeric(1))
  if (max(abs(residual)) > tolerance ||
      max(transformation_error) > tolerance ||
      any(bs$bridge_truth$clip_scales < config$clip_scale$minimum) ||
      any(bs$bridge_truth$clip_scales > config$clip_scale$maximum)) {
    lomm11_bridge_stop("The B+S profile violates its block-preserving truth contract.")
  }
  invisible(TRUE)
}

lomm11_bridge_plan_summary <- function(config, pair_plan, config_hash) {
  list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    status = "planned",
    pipeline_id = config$pipeline_id,
    config_hash = config_hash,
    config_prefix = substr(config_hash, 1L, 12L),
    expected_counts = as.list(config$expected_counts),
    design = list(
      pair_rows = nrow(pair_plan),
      profiles = LOMM11_BRIDGE_PROFILES,
      methods = LOMM11_BRIDGE_METHODS,
      community_algorithms = LOMM11_BRIDGE_ALGORITHMS,
      scenario_families = config$design$scenario_families,
      true_dimensions = as.integer(config$design$true_dimensions)
    ),
    safety = list(
      estimator_calls = FALSE,
      simulation_executed = FALSE,
      tmux_started = FALSE,
      revision_v2_modified = FALSE
    )
  )
}
