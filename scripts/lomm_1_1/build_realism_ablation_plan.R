#!/usr/bin/env Rscript

lomm11_planner_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the LOMM 1.1 ablation planner.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_PLANNER_PATH <- lomm11_planner_script_path()
LOMM11_DIRECTORY <- dirname(LOMM11_PLANNER_PATH)
LOMM11_REPO_ROOT <- normalizePath(file.path(LOMM11_DIRECTORY, "..", ".."))

lomm11_planner_require_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is required.", call. = FALSE)
    }
  }
}

lomm11_planner_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm11_planner_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

lomm11_planner_require_packages()

LOMM11_CONFIG_PATH <- file.path(LOMM11_DIRECTORY, "realism_ablation_config.json")
LOMM11_DEPENDENCY_PATHS <- c(
  config = LOMM11_CONFIG_PATH,
  spec = file.path(LOMM11_DIRECTORY, "realism_ablation_spec.R"),
  generator = file.path(LOMM11_DIRECTORY, "realism_generator.R"),
  metrics = file.path(LOMM11_DIRECTORY, "realism_metrics.R"),
  validator = file.path(LOMM11_DIRECTORY, "validate_realism_ablation_plan.R"),
  planner = LOMM11_PLANNER_PATH,
  runner = file.path(LOMM11_DIRECTORY, "run_realism_ablation.R"),
  test = file.path(LOMM11_DIRECTORY, "test_realism_ablation.R"),
  revision_spec = file.path(
    LOMM11_REPO_ROOT, "scripts", "simulations", "revision_spec.R"
  ),
  compositional_math = file.path(
    LOMM11_REPO_ROOT, "scripts", "preprocessing", "compositional_math.R"
  ),
  compositional_views = file.path(
    LOMM11_REPO_ROOT, "scripts", "preprocessing", "compositional_views.R"
  ),
  compositional_bundle = file.path(
    LOMM11_REPO_ROOT, "scripts", "preprocessing", "empirical_compositional_bundle.R"
  ),
  compositional_validator = file.path(
    LOMM11_REPO_ROOT, "scripts", "preprocessing", "validate_published_bundle.R"
  )
)

lomm11_missing_dependencies <- names(LOMM11_DEPENDENCY_PATHS)[
  !file.exists(LOMM11_DEPENDENCY_PATHS)
]
if (length(lomm11_missing_dependencies) > 0L) {
  stop(
    "Missing LOMM 1.1 planner dependencies: ",
    paste(lomm11_missing_dependencies, collapse = ", "),
    call. = FALSE
  )
}

LOMM11_STARTUP_DEPENDENCY_HASHES <- vapply(
  LOMM11_DEPENDENCY_PATHS, lomm11_planner_sha256, character(1)
)

source(LOMM11_DEPENDENCY_PATHS[["compositional_math"]])
source(LOMM11_DEPENDENCY_PATHS[["compositional_views"]])
source(LOMM11_DEPENDENCY_PATHS[["compositional_bundle"]])
LOMM_BUILDER_PATH <- file.path(
  LOMM11_REPO_ROOT, "scripts", "preprocessing", "build_empirical_compositional_views.R"
)
source(LOMM11_DEPENDENCY_PATHS[["compositional_validator"]])
source(LOMM11_DEPENDENCY_PATHS[["spec"]])
source(LOMM11_DEPENDENCY_PATHS[["revision_spec"]])
source(LOMM11_DEPENDENCY_PATHS[["generator"]])
source(LOMM11_DEPENDENCY_PATHS[["metrics"]])
source(LOMM11_DEPENDENCY_PATHS[["validator"]])

lomm11_assert_dependency_stability <- function() {
  current <- vapply(LOMM11_DEPENDENCY_PATHS, lomm11_planner_sha256, character(1))
  if (!identical(current, LOMM11_STARTUP_DEPENDENCY_HASHES)) {
    stop("LOMM 1.1 planner dependencies changed during execution.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_assert_dependency_stability()

lomm11_parse_planner_args <- function(args) {
  parsed <- list(
    input_bundle = file.path(
      LOMM11_REPO_ROOT, "simulation_outputs", "lomm_1_1",
      "empirical_compositional_preprocess", "0cdc11154d8a"
    ),
    output_root = file.path(
      LOMM11_REPO_ROOT, "simulation_outputs", "lomm_1_1",
      "empirical_realism_ablation"
    ),
    config = LOMM11_CONFIG_PATH,
    mode = NULL
  )
  value_flags <- c("--input-bundle", "--output-root", "--config")
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token %in% c("--dry-run", "--materialize-plan")) {
      if (!is.null(parsed$mode)) {
        stop("Choose exactly one planner mode.", call. = FALSE)
      }
      parsed$mode <- sub("^--", "", token)
      index <- index + 1L
      next
    }
    if (token %in% value_flags) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        stop("Missing value after ", token, ".", call. = FALSE)
      }
      name <- gsub("-", "_", sub("^--", "", token))
      parsed[[name]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    stop("Unknown argument: ", token, call. = FALSE)
  }
  if (is.null(parsed$mode)) {
    stop("Specify --dry-run or --materialize-plan.", call. = FALSE)
  }
  parsed
}

lomm11_lexically_normalize_path <- function(path) {
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

lomm11_canonical_future_path <- function(path) {
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) expanded <- file.path(getwd(), expanded)
  existing <- expanded
  suffix <- character()
  while (!file.exists(existing)) {
    parent <- dirname(existing)
    if (identical(parent, existing)) stop("Cannot resolve path.", call. = FALSE)
    suffix <- c(basename(existing), suffix)
    existing <- parent
  }
  resolved <- normalizePath(existing, mustWork = TRUE)
  combined <- if (length(suffix) == 0L) {
    resolved
  } else {
    do.call(file.path, as.list(c(resolved, suffix)))
  }
  lomm11_lexically_normalize_path(combined)
}

lomm11_validate_plan_root <- function(output_root) {
  absolute <- lomm11_canonical_future_path(output_root)
  expected_root <- normalizePath(
    file.path(
      LOMM11_REPO_ROOT, "simulation_outputs", "lomm_1_1",
      "empirical_realism_ablation"
    ),
    mustWork = FALSE
  )
  revision_root <- normalizePath(
    file.path(LOMM11_REPO_ROOT, "simulation_outputs", "revision_v2"),
    mustWork = FALSE
  )
  under_revision <- identical(absolute, revision_root) ||
    startsWith(absolute, paste0(revision_root, .Platform$file.sep))
  if (under_revision) {
    stop("LOMM 1.1 ablation plans cannot be written under revision_v2.", call. = FALSE)
  }
  if (!identical(absolute, expected_root)) {
    stop("LOMM 1.1 ablation plans require the dedicated output root.", call. = FALSE)
  }
  absolute
}

lomm11_input_hashes <- function(bundle_directory, config) {
  expected <- unlist(config$input_bundle$files, use.names = TRUE)
  paths <- file.path(bundle_directory, names(expected))
  if (any(!file.exists(paths))) {
    stop("Certified compositional input files are missing.", call. = FALSE)
  }
  observed <- vapply(paths, lomm11_planner_sha256, character(1))
  if (!identical(unname(observed), unname(expected))) {
    stop("Certified compositional input hashes differ from the locked config.", call. = FALSE)
  }
  setNames(observed, names(expected))
}

lomm11_dependency_records <- function() {
  logical_paths <- vapply(LOMM11_DEPENDENCY_PATHS, function(path) {
    relative <- substring(path, nchar(LOMM11_REPO_ROOT) + 2L)
    if (startsWith(path, paste0(LOMM11_REPO_ROOT, .Platform$file.sep))) {
      relative
    } else {
      basename(path)
    }
  }, character(1))
  lapply(seq_along(logical_paths), function(index) list(
    id = names(logical_paths)[[index]],
    path = unname(logical_paths[[index]]),
    sha256 = unname(LOMM11_STARTUP_DEPENDENCY_HASHES[[index]])
  ))
}

lomm11_plan_table_hashes <- function(plan) {
  lapply(names(plan$tables), function(name) list(
    path = name,
    sha256 = lomm11_planner_object_sha256(plan$tables[[name]]),
    schema_sha256 = lomm11_planner_object_sha256(plan$schemas[[name]])
  ))
}

lomm11_git_state <- function() {
  commit <- system2(
    "git", c("-C", LOMM11_REPO_ROOT, "rev-parse", "HEAD"), stdout = TRUE
  )
  dirty <- length(system2(
    "git", c("-C", LOMM11_REPO_ROOT, "status", "--porcelain"), stdout = TRUE
  )) > 0L
  list(commit = unname(commit[[1L]]), dirty = dirty)
}

lomm11_config_identity <- function(config, input_hashes, plan) {
  dependency_records <- lomm11_dependency_records()
  logical_input <- list(
    artifact_id = config$input_bundle$artifact_id,
    repo_relative_path = config$input_bundle$repo_relative_path,
    preprocessing_config_hash = config$input_bundle$config_hash,
    files = as.list(input_hashes)
  )
  payload <- list(
    schema_version = config$schema_version,
    pipeline_id = config$pipeline_id,
    config_file_sha256 = lomm11_planner_sha256(LOMM11_CONFIG_PATH),
    logical_input = logical_input,
    dependencies = dependency_records,
    plan_tables = lomm11_plan_table_hashes(plan),
    target_contract_sha256 = lomm11_planner_object_sha256(plan$target_contract),
    output_contract_sha256 = lomm11_planner_object_sha256(plan$future_output_contract)
  )
  full_hash <- lomm11_planner_object_sha256(payload)
  list(payload = payload, full = full_hash, prefix = substr(full_hash, 1L, 12L))
}

lomm11_prospective_output_records <- function(plan) {
  table_records <- lapply(names(plan$tables), function(name) list(
    path = name,
    format = "csv",
    rows = nrow(plan$tables[[name]]),
    columns = ncol(plan$tables[[name]]),
    schema = plan$schemas[[name]]
  ))
  c(table_records, list(
    list(path = "ablation_config.json", format = "json", rows = 1L),
    list(path = "target_contract.json", format = "json", rows = 1L),
    list(path = "output_schema.json", format = "json", rows = 1L),
    list(path = "prospective_manifest.json", format = "json", rows = 1L),
    list(path = "_PLANNED.json", format = "json", rows = 1L)
  ))
}

lomm11_build_prospective_manifest <- function(
  config, plan, identity, input_directory, input_hashes
) {
  list(
    schema_version = 1L,
    status = "planned",
    pipeline_id = config$pipeline_id,
    config_hash = identity$full,
    config_prefix = identity$prefix,
    configuration_identity = identity$payload,
    input_bundle = list(
      artifact_id = config$input_bundle$artifact_id,
      repo_relative_path = config$input_bundle$repo_relative_path,
      resolved_path = input_directory,
      preprocessing_config_hash = config$input_bundle$config_hash,
      files = as.list(input_hashes)
    ),
    generator_files = lomm11_dependency_records(),
    expected_counts = config$expected_counts,
    observed_plan_counts = list(
      candidates = nrow(plan$tables$candidate_grid.csv),
      fold_assignments = nrow(plan$tables$fold_assignments.csv),
      segments = nrow(plan$tables$segment_manifest.csv),
      training_panels = nrow(plan$tables$training_plan.csv),
      selections = nrow(plan$tables$selection_plan.csv),
      heldout_panels = nrow(plan$tables$heldout_plan.csv),
      heldout_view_evaluations = nrow(plan$tables$heldout_evaluation_plan.csv)
    ),
    output_schema = plan$schemas,
    future_output_contract = plan$future_output_contract,
    prospective_outputs = lomm11_prospective_output_records(plan),
    authorization = list(
      execution_token = config$future_execution$authorization_token,
      full_manifest_sha256_required = TRUE,
      automatic_progression = FALSE
    ),
    resource_contract = config$future_execution,
    scientific_contract = plan$target_contract,
    execution_state = list(
      simulation_executed = FALSE,
      rng_called = FALSE,
      parallel_backend_started = FALSE,
      estimator_calls = FALSE,
      tmux_started = FALSE
    ),
    randomness = list(
      plan_generation = "none",
      future_substreams = config$randomness$substreams,
      training_pairing = config$randomness$training_pairing,
      heldout_pairing = config$randomness$heldout_pairing,
      stream_state_derivation = config$randomness$stream_state_derivation,
      seed_base_role = config$randomness$seed_base_role,
      component_substreams_consumed_when_disabled =
        config$randomness$component_substreams_consumed_when_disabled
    ),
    software_versions = list(
      R = R.version.string,
      digest = as.character(utils::packageVersion("digest")),
      jsonlite = as.character(utils::packageVersion("jsonlite"))
    ),
    git = lomm11_git_state(),
    command = paste(commandArgs(), collapse = " "),
    limitations = as.list(c(
      "The plan targets one 100-clip MAFW sample and one CLIP scoring surface.",
      "Training-derived contrast directions are measurement directions rather than emotion factors.",
      "The ilr_gap2 view is a missing-frame sensitivity analysis.",
      "No simulation or estimator result is represented by this planned receipt."
    )),
    planned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

lomm11_write_json <- function(value, path) {
  jsonlite::write_json(
    value, path, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
  )
}

lomm11_write_plan_tables <- function(plan, directory) {
  for (name in names(plan$tables)) {
    utils::write.csv(
      plan$tables[[name]], file.path(directory, name), row.names = FALSE, na = ""
    )
  }
}

lomm11_materialize_plan <- function(config, plan, manifest, output_root) {
  final_directory <- file.path(output_root, manifest$config_prefix)
  if (dir.exists(final_directory)) {
    stop("Plan namespace already exists: ", final_directory, call. = FALSE)
  }
  staging <- paste0(final_directory, ".staging-", Sys.getpid())
  if (dir.exists(staging)) stop("Plan staging directory already exists.", call. = FALSE)
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  file.copy(LOMM11_CONFIG_PATH, file.path(staging, "ablation_config.json"))
  lomm11_write_plan_tables(plan, staging)
  lomm11_write_json(plan$target_contract, file.path(staging, "target_contract.json"))
  lomm11_write_json(plan$future_output_contract, file.path(staging, "output_schema.json"))
  manifest_path <- file.path(staging, "prospective_manifest.json")
  lomm11_write_json(manifest, manifest_path)
  receipt <- list(
    schema_version = 1L,
    status = "planned",
    config_hash = manifest$config_hash,
    config_prefix = manifest$config_prefix,
    prospective_manifest_sha256 = lomm11_planner_sha256(manifest_path),
    simulation_executed = FALSE,
    estimator_calls = FALSE
  )
  lomm11_write_json(receipt, file.path(staging, "_PLANNED.json"))
  lomm11_validate_realism_ablation_plan(staging, allow_staging = TRUE)
  dir.create(dirname(final_directory), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(staging, final_directory)) {
    stop("Failed to publish the atomic LOMM 1.1 ablation plan.", call. = FALSE)
  }
  lomm11_validate_realism_ablation_plan(final_directory)
  final_directory
}

lomm11_planner_main <- function() {
  args <- lomm11_parse_planner_args(commandArgs(trailingOnly = TRUE))
  output_root <- lomm11_validate_plan_root(args$output_root)
  input_directory <- normalizePath(args$input_bundle, mustWork = TRUE)
  config_path <- normalizePath(args$config, mustWork = TRUE)
  if (!identical(config_path, normalizePath(LOMM11_CONFIG_PATH, mustWork = TRUE))) {
    stop("The planner accepts only the checked-in locked configuration.", call. = FALSE)
  }
  lomm11_assert_dependency_stability()
  config <- lomm11_read_config(config_path)
  input_hashes <- lomm11_input_hashes(input_directory, config)
  lomm_validate_published_bundle(input_directory)
  lomm11_assert_dependency_stability()
  if (!identical(input_hashes, lomm11_input_hashes(input_directory, config))) {
    stop("Certified input files changed during plan construction.", call. = FALSE)
  }
  tables <- lomm_validation_read_tables(input_directory)
  plan <- lomm11_build_plan(
    config, tables[["clip_qc.csv"]], tables[["probability_view.csv"]]
  )
  identity <- lomm11_config_identity(config, input_hashes, plan)
  manifest <- lomm11_build_prospective_manifest(
    config, plan, identity, input_directory, input_hashes
  )
  lomm11_validate_prospective_manifest(manifest, config, plan)
  lomm11_assert_dependency_stability()
  if (!identical(input_hashes, lomm11_input_hashes(input_directory, config))) {
    stop("Certified input files changed during plan construction.", call. = FALSE)
  }
  if (identical(args$mode, "dry-run")) {
    cat(jsonlite::toJSON(
      manifest, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
    ), "\n")
    return(invisible(manifest))
  }
  output_directory <- lomm11_materialize_plan(config, plan, manifest, output_root)
  cat("Published LOMM 1.1 empirical-realism ablation plan:\n", output_directory, "\n")
  invisible(output_directory)
}

if (sys.nframe() == 0L) {
  lomm11_planner_main()
}
