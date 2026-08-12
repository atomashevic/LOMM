#!/usr/bin/env Rscript

lomm11_validator_script_path <- function() {
  if (exists("LOMM11_DEPENDENCY_PATHS", inherits = TRUE)) {
    paths <- get("LOMM11_DEPENDENCY_PATHS", inherits = TRUE)
    if ("validator" %in% names(paths)) {
      return(normalizePath(paths[["validator"]], mustWork = TRUE))
    }
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the LOMM 1.1 plan validator.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_VALIDATOR_PATH <- lomm11_validator_script_path()
LOMM11_VALIDATOR_DIRECTORY <- dirname(LOMM11_VALIDATOR_PATH)
LOMM11_VALIDATOR_REPO_ROOT <- normalizePath(
  file.path(LOMM11_VALIDATOR_DIRECTORY, "..", ".."), mustWork = TRUE
)

lomm11_validator_require_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is required.", call. = FALSE)
    }
  }
}

lomm11_validator_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm11_validator_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

lomm11_validator_exact_fields <- function(value, fields, label) {
  if (!is.list(value) || !identical(sort(names(value)), sort(fields))) {
    stop(label, " fields differ from the locked schema.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validator_scalar_string <- function(value, label) {
  if (length(value) != 1L || !is.character(value) || is.na(value) || !nzchar(value)) {
    stop(label, " must be one non-empty string.", call. = FALSE)
  }
  value
}

lomm11_validator_canonical_path <- function(path, must_work = TRUE) {
  path <- path.expand(path)
  if (!startsWith(path, .Platform$file.sep)) {
    path <- file.path(getwd(), path)
  }
  if (must_work) {
    return(normalizePath(path, mustWork = TRUE))
  }
  existing <- path
  suffix <- character()
  while (!file.exists(existing)) {
    parent <- dirname(existing)
    if (identical(parent, existing)) {
      stop("Cannot resolve path: ", path, call. = FALSE)
    }
    suffix <- c(basename(existing), suffix)
    existing <- parent
  }
  resolved <- normalizePath(existing, mustWork = TRUE)
  if (length(suffix) == 0L) resolved else do.call(file.path, as.list(c(resolved, suffix)))
}

lomm11_validator_is_within <- function(path, root) {
  identical(path, root) || startsWith(path, paste0(root, .Platform$file.sep))
}

lomm11_validator_lexical_path <- function(path) {
  path <- lomm11_validator_scalar_string(path, "Plan directory")
  path <- path.expand(path)
  if (!startsWith(path, .Platform$file.sep)) {
    path <- file.path(getwd(), path)
  }
  parts <- strsplit(path, paste0(.Platform$file.sep, "+"))[[1L]]
  normalized <- character()
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) next
    if (identical(part, "..")) {
      if (length(normalized) > 0L) normalized <- normalized[-length(normalized)]
      next
    }
    normalized <- c(normalized, part)
  }
  paste0(.Platform$file.sep, paste(normalized, collapse = .Platform$file.sep))
}

lomm11_validator_locked_input <- function() {
  list(
    artifact_id = "lomm_1_1_compositional_0cdc11154d8a",
    repo_relative_path = paste(
      "simulation_outputs/lomm_1_1/empirical_compositional_preprocess",
      "0cdc11154d8a",
      sep = "/"
    ),
    config_hash = "0cdc11154d8a",
    files = as.list(c(
      `_SUCCESS.json` = "0517998bd320ebfb46c2efc7e433223dd8353843f4a885e7e70bb6ef18ff3ba8",
      preprocessing_manifest.json =
        "60c3ca4a9198e01d2f87f0136d5e1b1ec9a2dfddd90d2b47c082e0c8bd2c9880",
      frame_ledger.csv =
        "0f0bdea7c26c1e929634e93a29148e6ee7f2ca44d7150780631671490cf52ef0",
      probability_view.csv =
        "23752e2d917fe125a2ca4688e001825e4ca1a846711fbe8a81609dcd4861c3d3",
      clr_view.csv =
        "0a4eee7f3ae5a5e95c20de304c57c0dee9e2ea84643c307a7a8fcd2cfb334146",
      ilr_view.csv =
        "e74f82770c0e4fa001b8f0190adb9d67d99b1e4cf20b51e3a9e28ab6b0915052",
      ilr_basis.csv =
        "f5a9a2aa842fe5ab49332e7d4bcfb7095a79522c53d2fc9bc9b8b29d720ebdf9",
      clip_qc.csv =
        "eb2603cf8140466e6c7cad07b681e4ffaede1763393f2891838bf6faa87db8a2"
    ))
  )
}

lomm11_validator_expected_counts <- function() {
  list(
    candidates = 216L, fold_candidates = 1080L, training_panels = 3240L,
    selections = 40L, heldout_panels = 4000L,
    heldout_view_evaluations = 8000L, generated_panels = 7240L,
    checkpoint_units = 11240L, training_metric_discrepancies = 51840L,
    heldout_metric_discrepancies = 128000L,
    clip_metric_contributions = 21760000L, paired_ablation_effects = 272L,
    primary_frames = 3557L, primary_segments = 188L,
    sensitivity_frames = 3632L, sensitivity_segments = 129L,
    imputed_sensitivity_frames = 75L
  )
}

lomm11_validator_resource_contract <- function() {
  list(
    worker_limit = 12L,
    blas_threads_per_worker = 1L,
    job_timeout_seconds = 21600L,
    panel_timeout_seconds = 300L,
    authorization_token = "LOMM-1.1-REALISM-AUTHORIZED",
    requires_full_prospective_manifest_sha256 = TRUE,
    automatic_progression = FALSE
  )
}

lomm11_validator_randomness_contract <- function() {
  list(
    seed_origin = 11082026L,
    training_replicates = 3L,
    heldout_replicates = 100L,
    substreams = as.list(c(
      "between_clip_baseline", "clip_scale", "gate_schedule", "initial_state",
      "innovations", "residuals"
    )),
    training_pairing = "fold_core_candidate_replicate",
    heldout_pairing = "fold_replicate",
    stream_state_derivation = "sha256_full_key_to_lecuyer_cmrg_v1",
    seed_base_role = "auditable_pairing_namespace_checksum",
    component_substreams_consumed_when_disabled = TRUE
  )
}

lomm11_validator_rng_provenance_contract <- function() {
  list(
    state_derivation_version = "lomm11_lecuyer_cmrg_v1",
    stream_state_derivation = "sha256_full_key_to_lecuyer_cmrg_v1",
    generator = "L'Ecuyer-CMRG",
    state_words = 6L,
    state_identity_fields = as.list(c(
      "state_derivation_version", "seed_base", "seed_group", "clip",
      "segment", "component"
    )),
    seed_base_role = "auditable_pairing_namespace_checksum",
    clip_shared_segment = "__clip_shared__",
    clip_shared_components = as.list(c(
      "between_clip_baseline", "clip_scale"
    )),
    segment_components = as.list(c(
      "gate_schedule", "initial_state", "innovations", "residuals"
    )),
    component_substreams_consumed_when_disabled = TRUE
  )
}

lomm11_validator_fold_nuisance_contract <- function() {
  list(
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
  )
}

lomm11_validator_check_future_contract <- function(contract) {
  expected_names <- c(
    "fold_nuisance_manifest.csv", "checkpoint_results.csv",
    "selected_candidates.csv", "training_metric_discrepancies.parquet",
    "heldout_metric_discrepancies.csv", "clip_metric_contributions.parquet",
    "run_manifest.json", "_SUCCESS.json", "paired_ablation_effects.csv"
  )
  if (!is.list(contract) || !identical(names(contract), expected_names)) {
    stop("Future output artifact set differs from the locked contract.", call. = FALSE)
  }
  expected_rows <- c(
    fold_nuisance_manifest.csv = 5L,
    checkpoint_results.csv = 11240L,
    selected_candidates.csv = 40L,
    training_metric_discrepancies.parquet = 51840L,
    heldout_metric_discrepancies.csv = 128000L,
    clip_metric_contributions.parquet = 21760000L,
    run_manifest.json = 1L,
    `_SUCCESS.json` = 1L,
    paired_ablation_effects.csv = 272L
  )
  observed_rows <- vapply(contract, function(artifact) {
    if (length(artifact$rows) != 1L || !is.integer(artifact$rows)) NA_integer_
    else artifact$rows
  }, integer(1))
  if (!identical(observed_rows, expected_rows)) {
    stop("Future output row counts differ from the locked contract.", call. = FALSE)
  }
  lomm11_validator_json_equal(
    contract[["fold_nuisance_manifest.csv"]],
    lomm11_validator_fold_nuisance_contract(),
    "Fold-nuisance manifest contract"
  )
  paired <- contract[["paired_ablation_effects.csv"]]
  if (!identical(paired$completion_required, FALSE) ||
      !identical(paired$rows, 272L)) {
    stop("Paired ablation effects must remain downstream-only.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validator_paths <- function() {
  c(
    config = file.path(LOMM11_VALIDATOR_DIRECTORY, "realism_ablation_config.json"),
    spec = file.path(LOMM11_VALIDATOR_DIRECTORY, "realism_ablation_spec.R"),
    generator = file.path(LOMM11_VALIDATOR_DIRECTORY, "realism_generator.R"),
    metrics = file.path(LOMM11_VALIDATOR_DIRECTORY, "realism_metrics.R"),
    validator = LOMM11_VALIDATOR_PATH,
    planner = file.path(LOMM11_VALIDATOR_DIRECTORY, "build_realism_ablation_plan.R"),
    runner = file.path(LOMM11_VALIDATOR_DIRECTORY, "run_realism_ablation.R"),
    test = file.path(LOMM11_VALIDATOR_DIRECTORY, "test_realism_ablation.R"),
    revision_spec = file.path(
      LOMM11_VALIDATOR_REPO_ROOT, "scripts", "simulations", "revision_spec.R"
    ),
    compositional_math = file.path(
      LOMM11_VALIDATOR_REPO_ROOT, "scripts", "preprocessing", "compositional_math.R"
    ),
    compositional_views = file.path(
      LOMM11_VALIDATOR_REPO_ROOT, "scripts", "preprocessing", "compositional_views.R"
    ),
    compositional_bundle = file.path(
      LOMM11_VALIDATOR_REPO_ROOT, "scripts", "preprocessing",
      "empirical_compositional_bundle.R"
    ),
    compositional_validator = file.path(
      LOMM11_VALIDATOR_REPO_ROOT, "scripts", "preprocessing",
      "validate_published_bundle.R"
    )
  )
}

lomm11_validator_dependency_records <- function(paths = lomm11_validator_paths()) {
  if (any(!file.exists(paths))) {
    stop(
      "LOMM 1.1 validator dependencies are missing: ",
      paste(names(paths)[!file.exists(paths)], collapse = ", "), ".",
      call. = FALSE
    )
  }
  hashes <- vapply(paths, lomm11_validator_sha256, character(1))
  lapply(seq_along(paths), function(index) {
    path <- normalizePath(paths[[index]], mustWork = TRUE)
    repo_prefix <- paste0(LOMM11_VALIDATOR_REPO_ROOT, .Platform$file.sep)
    logical_path <- if (startsWith(path, repo_prefix)) {
      substring(path, nchar(repo_prefix) + 1L)
    } else {
      basename(path)
    }
    list(
      id = names(paths)[[index]],
      path = logical_path,
      sha256 = unname(hashes[[index]])
    )
  })
}

lomm11_validator_load_contract <- function() {
  if (exists("lomm11_build_plan", mode = "function", inherits = TRUE) &&
      exists("lomm_validation_read_tables", mode = "function", inherits = TRUE)) {
    return(invisible(TRUE))
  }
  paths <- lomm11_validator_paths()
  source(paths[["compositional_math"]], local = .GlobalEnv)
  source(paths[["compositional_views"]], local = .GlobalEnv)
  source(paths[["compositional_bundle"]], local = .GlobalEnv)
  assign(
    "LOMM_BUILDER_PATH",
    file.path(
      LOMM11_VALIDATOR_REPO_ROOT, "scripts", "preprocessing",
      "build_empirical_compositional_views.R"
    ),
    envir = .GlobalEnv
  )
  source(paths[["compositional_validator"]], local = .GlobalEnv)
  source(paths[["spec"]], local = .GlobalEnv)
  source(paths[["revision_spec"]], local = .GlobalEnv)
  invisible(TRUE)
}

lomm11_validator_json_equal <- function(actual, expected, label) {
  if (!identical(
    lomm11_validator_object_sha256(actual),
    lomm11_validator_object_sha256(expected)
  )) {
    stop(label, " differs from the independently rebuilt contract.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validator_schema_colclasses <- function(schema) {
  types <- unlist(schema$column_types, use.names = FALSE)
  mapped <- c(
    character = "character", integer = "integer", logical = "logical",
    double = "numeric"
  )[types]
  if (length(mapped) != length(types) || anyNA(mapped)) {
    stop("Plan schema contains an unsupported column type.", call. = FALSE)
  }
  unname(mapped)
}

lomm11_validator_read_plan_table <- function(path, schema) {
  columns <- unlist(schema$column_names, use.names = FALSE)
  table <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "",
    colClasses = lomm11_validator_schema_colclasses(schema)
  )
  if (!identical(names(table), columns) ||
      nrow(table) != as.integer(schema$rows) ||
      ncol(table) != as.integer(schema$columns)) {
    stop("Serialized plan table has the wrong schema: ", basename(path), call. = FALSE)
  }
  table
}

lomm11_validator_compare_table <- function(actual, expected, schema, label) {
  if (!identical(names(actual), names(expected)) || nrow(actual) != nrow(expected)) {
    stop(label, " differs from the rebuilt table.", call. = FALSE)
  }
  types <- unlist(schema$column_types, use.names = FALSE)
  for (index in seq_along(expected)) {
    observed <- actual[[index]]
    target <- expected[[index]]
    matches <- if (identical(types[[index]], "double")) {
      identical(is.na(observed), is.na(target)) && {
        usable <- !is.na(target)
        differences <- abs(as.numeric(observed[usable]) - as.numeric(target[usable]))
        length(differences) == 0L ||
          (all(is.finite(differences)) && max(differences) <= 1e-12)
      }
    } else if (identical(types[[index]], "integer")) {
      identical(as.integer(observed), as.integer(target))
    } else {
      identical(observed, target)
    }
    if (!isTRUE(matches)) {
      stop(
        label, " differs from the rebuilt table in column ", names(expected)[[index]], ".",
        call. = FALSE
      )
    }
  }
  keys <- unlist(schema$primary_key, use.names = FALSE)
  key_values <- do.call(paste, c(actual[keys], sep = "\r"))
  if (anyNA(actual[keys]) || anyDuplicated(key_values)) {
    stop(label, " has missing or duplicate primary keys.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validator_plan_hashes <- function(plan) {
  lapply(names(plan$tables), function(name) list(
    path = name,
    sha256 = lomm11_validator_object_sha256(plan$tables[[name]]),
    schema_sha256 = lomm11_validator_object_sha256(plan$schemas[[name]])
  ))
}

lomm11_validator_identity <- function(config, input_hashes, plan) {
  logical_input <- list(
    artifact_id = config$input_bundle$artifact_id,
    repo_relative_path = config$input_bundle$repo_relative_path,
    preprocessing_config_hash = config$input_bundle$config_hash,
    files = as.list(input_hashes)
  )
  payload <- list(
    schema_version = config$schema_version,
    pipeline_id = config$pipeline_id,
    config_file_sha256 = lomm11_validator_sha256(
      lomm11_validator_paths()[["config"]]
    ),
    logical_input = logical_input,
    dependencies = lomm11_validator_dependency_records(),
    plan_tables = lomm11_validator_plan_hashes(plan),
    target_contract_sha256 = lomm11_validator_object_sha256(plan$target_contract),
    output_contract_sha256 = lomm11_validator_object_sha256(plan$future_output_contract)
  )
  full_hash <- lomm11_validator_object_sha256(payload)
  list(payload = payload, full = full_hash, prefix = substr(full_hash, 1L, 12L))
}

lomm11_validator_prospective_outputs <- function(plan) {
  tables <- lapply(names(plan$tables), function(name) list(
    path = name,
    format = "csv",
    rows = nrow(plan$tables[[name]]),
    columns = ncol(plan$tables[[name]]),
    schema = plan$schemas[[name]]
  ))
  c(tables, list(
    list(path = "ablation_config.json", format = "json", rows = 1L),
    list(path = "target_contract.json", format = "json", rows = 1L),
    list(path = "output_schema.json", format = "json", rows = 1L),
    list(path = "prospective_manifest.json", format = "json", rows = 1L),
    list(path = "_PLANNED.json", format = "json", rows = 1L)
  ))
}

lomm11_validator_expected_limitations <- function() {
  as.list(c(
    "The plan targets one 100-clip MAFW sample and one CLIP scoring surface.",
    paste(
      "Training-derived contrast directions are measurement directions rather",
      "than emotion factors."
    ),
    "The ilr_gap2 view is a missing-frame sensitivity analysis.",
    "No simulation or estimator result is represented by this planned receipt."
  ))
}

lomm11_validate_prospective_manifest <- function(manifest, config, plan) {
  fields <- c(
    "schema_version", "status", "pipeline_id", "config_hash", "config_prefix",
    "configuration_identity", "input_bundle", "generator_files",
    "expected_counts", "observed_plan_counts", "output_schema",
    "future_output_contract", "prospective_outputs", "authorization",
    "resource_contract", "scientific_contract", "execution_state", "randomness",
    "software_versions", "git", "command", "limitations", "planned_at"
  )
  lomm11_validator_exact_fields(manifest, fields, "Prospective manifest")
  lomm11_validator_json_equal(
    config$input_bundle, lomm11_validator_locked_input(),
    "Checked-in certified-input configuration"
  )
  lomm11_validator_json_equal(
    config$expected_counts, lomm11_validator_expected_counts(),
    "Checked-in expected counts"
  )
  lomm11_validator_json_equal(
    config$future_execution, lomm11_validator_resource_contract(),
    "Checked-in resource contract"
  )
  lomm11_validator_json_equal(
    config$randomness, lomm11_validator_randomness_contract(),
    "Checked-in randomness contract"
  )
  header_valid <- identical(manifest$schema_version, 1L) &&
    identical(manifest$status, "planned") &&
    identical(manifest$pipeline_id, config$pipeline_id) &&
    grepl("^[0-9a-f]{64}$", manifest$config_hash) &&
    grepl("^[0-9a-f]{12}$", manifest$config_prefix) &&
    identical(manifest$config_prefix, substr(manifest$config_hash, 1L, 12L))
  if (!header_valid) {
    stop("Prospective manifest header is invalid.", call. = FALSE)
  }
  input_hashes <- unlist(manifest$input_bundle$files, use.names = TRUE)
  identity <- lomm11_validator_identity(config, input_hashes, plan)
  if (!identical(manifest$config_hash, identity$full) ||
      !identical(manifest$config_prefix, identity$prefix)) {
    stored_parts <- vapply(
      manifest$configuration_identity,
      lomm11_validator_object_sha256,
      character(1)
    )
    rebuilt_parts <- vapply(
      identity$payload,
      lomm11_validator_object_sha256,
      character(1)
    )
    differing <- names(rebuilt_parts)[
      is.na(match(names(rebuilt_parts), names(stored_parts))) |
        rebuilt_parts != stored_parts[names(rebuilt_parts)]
    ]
    stop(
      "Prospective manifest configuration hash is not reproducible",
      if (length(differing) > 0L) {
        paste0("; differing identity fields: ", paste(differing, collapse = ", "))
      } else {
        "; serialized identity hashes match but the full hash differs"
      },
      ".",
      call. = FALSE
    )
  }
  lomm11_validator_json_equal(
    manifest$configuration_identity, identity$payload, "Configuration identity"
  )
  lomm11_validator_json_equal(
    manifest$generator_files, identity$payload$dependencies, "Generator records"
  )
  lomm11_validator_json_equal(
    manifest$expected_counts, lomm11_validator_expected_counts(), "Expected counts"
  )
  expected_observed <- list(
    candidates = 216L, fold_assignments = 100L, segments = 317L,
    training_panels = 3240L, selections = 40L, heldout_panels = 4000L,
    heldout_view_evaluations = 8000L
  )
  lomm11_validator_json_equal(
    manifest$observed_plan_counts, expected_observed, "Observed plan counts"
  )
  lomm11_validator_json_equal(manifest$output_schema, plan$schemas, "Output schema")
  lomm11_validator_json_equal(
    manifest$future_output_contract, plan$future_output_contract,
    "Future output contract"
  )
  lomm11_validator_check_future_contract(manifest$future_output_contract)
  lomm11_validator_json_equal(
    manifest$prospective_outputs, lomm11_validator_prospective_outputs(plan),
    "Prospective output records"
  )
  lomm11_validator_json_equal(
    manifest$scientific_contract, plan$target_contract, "Scientific contract"
  )
  lomm11_validator_json_equal(
    manifest$scientific_contract$randomness,
    lomm11_validator_rng_provenance_contract(),
    "Scientific RNG provenance contract"
  )
  expected_authorization <- list(
    execution_token = lomm11_validator_resource_contract()$authorization_token,
    full_manifest_sha256_required = TRUE,
    automatic_progression = FALSE
  )
  expected_execution <- list(
    simulation_executed = FALSE, rng_called = FALSE,
    parallel_backend_started = FALSE, estimator_calls = FALSE, tmux_started = FALSE
  )
  locked_randomness <- lomm11_validator_randomness_contract()
  expected_randomness <- list(
    plan_generation = "none",
    future_substreams = locked_randomness$substreams,
    training_pairing = locked_randomness$training_pairing,
    heldout_pairing = locked_randomness$heldout_pairing,
    stream_state_derivation = locked_randomness$stream_state_derivation,
    seed_base_role = locked_randomness$seed_base_role,
    component_substreams_consumed_when_disabled =
      locked_randomness$component_substreams_consumed_when_disabled
  )
  lomm11_validator_json_equal(
    manifest$authorization, expected_authorization, "Authorization contract"
  )
  lomm11_validator_json_equal(
    manifest$resource_contract, lomm11_validator_resource_contract(),
    "Resource contract"
  )
  lomm11_validator_json_equal(
    manifest$execution_state, expected_execution, "Execution state"
  )
  lomm11_validator_json_equal(
    manifest$randomness, expected_randomness, "Randomness contract"
  )
  lomm11_validator_json_equal(
    manifest$limitations, lomm11_validator_expected_limitations(), "Limitations"
  )
  lomm11_validator_exact_fields(
    manifest$software_versions, c("R", "digest", "jsonlite"), "Software versions"
  )
  expected_software <- list(
    R = R.version.string,
    digest = as.character(utils::packageVersion("digest")),
    jsonlite = as.character(utils::packageVersion("jsonlite"))
  )
  lomm11_validator_json_equal(
    manifest$software_versions, expected_software, "Software versions"
  )
  lomm11_validator_exact_fields(manifest$git, c("commit", "dirty"), "Git record")
  if (!grepl("^[0-9a-f]{40}$", manifest$git$commit) ||
      length(manifest$git$dirty) != 1L || !is.logical(manifest$git$dirty) ||
      is.na(manifest$git$dirty)) {
    stop("Git record is invalid.", call. = FALSE)
  }
  command <- lomm11_validator_scalar_string(manifest$command, "Planner command")
  command_tokens <- strsplit(command, "[[:space:]]+")[[1L]]
  mode_tokens <- command_tokens[command_tokens %in% c("--dry-run", "--materialize-plan")]
  forbidden_tokens <- c(
    "--execute", "launch", "resume", "tmux", "DynEGA", "GraphicalVAR",
    "GIMME", "static-EGA"
  )
  if (!grepl("build_realism_ablation_plan[.]R", command) ||
      length(mode_tokens) != 1L ||
      any(vapply(forbidden_tokens, grepl, logical(1), x = command, fixed = TRUE))) {
    stop("Planner command does not describe exactly one planned-only mode.", call. = FALSE)
  }
  timestamp <- lomm11_validator_scalar_string(manifest$planned_at, "Planned timestamp")
  parsed_timestamp <- as.POSIXct(
    timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  )
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", timestamp) ||
      is.na(parsed_timestamp) ||
      !identical(format(parsed_timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), timestamp)) {
    stop("Planned timestamp is not strict UTC ISO-8601.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validator_input_contract <- function(manifest, config) {
  fields <- c(
    "artifact_id", "repo_relative_path", "resolved_path",
    "preprocessing_config_hash", "files"
  )
  lomm11_validator_exact_fields(manifest$input_bundle, fields, "Input bundle")
  locked <- lomm11_validator_locked_input()
  lomm11_validator_json_equal(
    config$input_bundle, locked, "Checked-in certified-input configuration"
  )
  expected_directory <- normalizePath(
    file.path(LOMM11_VALIDATOR_REPO_ROOT, locked$repo_relative_path),
    mustWork = TRUE
  )
  resolved <- normalizePath(manifest$input_bundle$resolved_path, mustWork = TRUE)
  expected_hashes <- unlist(locked$files, use.names = TRUE)
  stored_hashes <- unlist(manifest$input_bundle$files, use.names = TRUE)
  valid <- identical(manifest$input_bundle$artifact_id, locked$artifact_id) &&
    identical(
      manifest$input_bundle$repo_relative_path,
      locked$repo_relative_path
    ) &&
    identical(manifest$input_bundle$preprocessing_config_hash, locked$config_hash) &&
    identical(manifest$input_bundle$resolved_path, expected_directory) &&
    identical(resolved, expected_directory) && identical(stored_hashes, expected_hashes)
  if (!valid) {
    stop("Input bundle does not match the locked certified artifact.", call. = FALSE)
  }
  paths <- file.path(expected_directory, names(expected_hashes))
  if (any(!file.exists(paths)) || any(nzchar(Sys.readlink(paths)))) {
    stop("Certified input bundle is incomplete or contains linked files.", call. = FALSE)
  }
  observed <- vapply(paths, lomm11_validator_sha256, character(1))
  if (!identical(unname(observed), unname(expected_hashes))) {
    stop("Certified input bundle hashes differ from the locked artifact.", call. = FALSE)
  }
  list(directory = expected_directory, hashes = setNames(observed, names(expected_hashes)))
}

lomm11_validator_validate_directory_path <- function(directory, prefix, allow_staging) {
  resolved <- normalizePath(directory, mustWork = TRUE)
  root <- lomm11_validator_canonical_path(file.path(
    LOMM11_VALIDATOR_REPO_ROOT, "simulation_outputs", "lomm_1_1",
    "empirical_realism_ablation"
  ), must_work = FALSE)
  revision <- lomm11_validator_canonical_path(file.path(
    LOMM11_VALIDATOR_REPO_ROOT, "simulation_outputs", "revision_v2"
  ), must_work = FALSE)
  if (!lomm11_validator_is_within(resolved, root) ||
      lomm11_validator_is_within(resolved, revision)) {
    stop("Plan directory escapes the dedicated LOMM 1.1 namespace.", call. = FALSE)
  }
  staging_pattern <- paste0("^", prefix, "[.]staging-[0-9]+$")
  valid_name <- identical(basename(resolved), prefix) ||
    (isTRUE(allow_staging) && grepl(staging_pattern, basename(resolved)))
  if (!valid_name) {
    stop("Plan directory name differs from the configuration prefix.", call. = FALSE)
  }
  resolved
}

lomm11_validator_expected_files <- function() {
  c(
    "ablation_config.json", "candidate_grid.csv", "fold_assignments.csv",
    "segment_manifest.csv", "training_plan.csv", "selection_plan.csv",
    "heldout_plan.csv", "heldout_evaluation_plan.csv", "target_contract.json",
    "output_schema.json", "prospective_manifest.json", "_PLANNED.json"
  )
}

lomm11_validator_file_snapshot <- function(directory) {
  files <- lomm11_validator_expected_files()
  paths <- file.path(directory, files)
  if (any(!file.exists(paths))) {
    stop("Plan directory is missing required files.", call. = FALSE)
  }
  setNames(vapply(paths, lomm11_validator_sha256, character(1)), files)
}

lomm11_validator_check_receipt <- function(receipt, manifest, manifest_path) {
  lomm11_validator_exact_fields(
    receipt,
    c(
      "schema_version", "status", "config_hash", "config_prefix",
      "prospective_manifest_sha256", "simulation_executed", "estimator_calls"
    ),
    "Planned receipt"
  )
  valid <- identical(receipt$schema_version, 1L) &&
    identical(receipt$status, "planned") &&
    identical(receipt$config_hash, manifest$config_hash) &&
    identical(receipt$config_prefix, manifest$config_prefix) &&
    identical(
      receipt$prospective_manifest_sha256,
      lomm11_validator_sha256(manifest_path)
    ) &&
    identical(receipt$simulation_executed, FALSE) &&
    identical(receipt$estimator_calls, FALSE)
  if (!valid) {
    stop("Planned receipt is invalid or records execution.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validate_realism_ablation_plan <- function(directory, allow_staging = FALSE) {
  lomm11_validator_require_packages()
  lomm11_validator_load_contract()
  lexical_directory <- lomm11_validator_lexical_path(directory)
  lexical_root <- file.path(
    LOMM11_VALIDATOR_REPO_ROOT, "simulation_outputs", "lomm_1_1",
    "empirical_realism_ablation"
  )
  if (!lomm11_validator_is_within(lexical_directory, lexical_root) ||
      nzchar(Sys.readlink(lexical_directory))) {
    stop("Plan directory uses an unauthorized namespace or alias.", call. = FALSE)
  }
  directory <- normalizePath(directory, mustWork = TRUE)
  expected_files <- lomm11_validator_expected_files()
  entries <- list.files(directory, all.files = TRUE, no.. = TRUE)
  if (!identical(sort(entries), sort(expected_files)) ||
      any(file.info(file.path(directory, entries))$isdir) ||
      any(nzchar(Sys.readlink(file.path(directory, entries))))) {
    stop(
      "Plan directory contains missing, unexpected, nested, or linked artifacts.",
      call. = FALSE
    )
  }
  before <- lomm11_validator_file_snapshot(directory)
  config_path <- lomm11_validator_paths()[["config"]]
  if (!identical(
    lomm11_validator_sha256(file.path(directory, "ablation_config.json")),
    lomm11_validator_sha256(config_path)
  )) {
    stop("Published ablation configuration differs from the locked file.", call. = FALSE)
  }
  config <- lomm11_read_config(config_path)
  manifest <- jsonlite::fromJSON(
    file.path(directory, "prospective_manifest.json"), simplifyVector = FALSE
  )
  directory <- lomm11_validator_validate_directory_path(
    directory, manifest$config_prefix, allow_staging
  )
  input <- lomm11_validator_input_contract(manifest, config)
  input_hash_before <- input$hashes
  dependency_before <- vapply(
    lomm11_validator_paths(), lomm11_validator_sha256, character(1)
  )
  lomm_validate_published_bundle(input$directory)
  input_tables <- lomm_validation_read_tables(input$directory)
  rebuilt <- lomm11_build_plan(
    config, input_tables[["clip_qc.csv"]], input_tables[["probability_view.csv"]]
  )
  lomm11_validate_prospective_manifest(manifest, config, rebuilt)
  if (!grepl(
    "(^|[[:space:]])--materialize-plan($|[[:space:]])",
    manifest$command,
    perl = TRUE
  ) || grepl("(^|[[:space:]])--dry-run($|[[:space:]])", manifest$command, perl = TRUE)) {
    stop("Published plan manifest must record --materialize-plan.", call. = FALSE)
  }
  for (name in names(rebuilt$tables)) {
    schema <- rebuilt$schemas[[name]]
    serialized <- lomm11_validator_read_plan_table(file.path(directory, name), schema)
    lomm11_validator_compare_table(
      serialized, rebuilt$tables[[name]], schema, paste("Plan table", name)
    )
  }
  target <- jsonlite::fromJSON(
    file.path(directory, "target_contract.json"), simplifyVector = FALSE
  )
  output <- jsonlite::fromJSON(
    file.path(directory, "output_schema.json"), simplifyVector = FALSE
  )
  lomm11_validator_json_equal(target, rebuilt$target_contract, "Target contract")
  lomm11_validator_json_equal(
    output, rebuilt$future_output_contract, "Future output contract file"
  )
  receipt <- jsonlite::fromJSON(
    file.path(directory, "_PLANNED.json"), simplifyVector = FALSE
  )
  lomm11_validator_check_receipt(
    receipt, manifest, file.path(directory, "prospective_manifest.json")
  )
  input_after <- vapply(
    file.path(input$directory, names(input_hash_before)),
    lomm11_validator_sha256,
    character(1)
  )
  dependency_after <- vapply(
    lomm11_validator_paths(), lomm11_validator_sha256, character(1)
  )
  after <- lomm11_validator_file_snapshot(directory)
  if (!identical(unname(input_hash_before), unname(input_after)) ||
      !identical(dependency_before, dependency_after) ||
      !identical(before, after)) {
    stop("Plan, input, or validator dependencies changed during validation.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_validator_main <- function(args) {
  if (length(args) != 2L || !identical(args[[1L]], "--directory") ||
      startsWith(args[[2L]], "--")) {
    stop(
      "Usage: validate_realism_ablation_plan.R --directory DIR",
      call. = FALSE
    )
  }
  lomm11_validate_realism_ablation_plan(args[[2L]])
  cat(
    "LOMM 1.1 empirical-realism ablation plan validation: PASS\n",
    normalizePath(args[[2L]], mustWork = TRUE), "\n"
  )
}

if (sys.nframe() == 0L) {
  lomm11_validator_main(commandArgs(trailingOnly = TRUE))
}
