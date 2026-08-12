#!/usr/bin/env Rscript

lomm_builder_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the compositional-view builder.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM_BUILDER_PATH <- lomm_builder_script_path()
LOMM_REPO_ROOT <- normalizePath(file.path(dirname(LOMM_BUILDER_PATH), "..", ".."))

lomm_require_builder_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is required.", call. = FALSE)
    }
  }
}

lomm_file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

lomm_require_builder_packages()
LOMM_GENERATOR_PATHS <- c(
  math = file.path(dirname(LOMM_BUILDER_PATH), "compositional_math.R"),
  views = file.path(dirname(LOMM_BUILDER_PATH), "compositional_views.R"),
  bundle = file.path(dirname(LOMM_BUILDER_PATH), "empirical_compositional_bundle.R"),
  validator = file.path(dirname(LOMM_BUILDER_PATH), "validate_published_bundle.R"),
  builder = LOMM_BUILDER_PATH
)
LOMM_STARTUP_GENERATOR_FILES <- as.list(vapply(
  LOMM_GENERATOR_PATHS, lomm_file_sha256, character(1)
))
LOMM_STARTUP_GENERATOR_HASH <- lomm_object_sha256(LOMM_STARTUP_GENERATOR_FILES)

source(LOMM_GENERATOR_PATHS[["math"]])
source(LOMM_GENERATOR_PATHS[["views"]])
source(LOMM_GENERATOR_PATHS[["bundle"]])
source(LOMM_GENERATOR_PATHS[["validator"]])

lomm_parse_builder_args <- function(args) {
  parsed <- list(
    input = NULL,
    output_root = file.path(
      LOMM_REPO_ROOT, "simulation_outputs", "lomm_1_1",
      "empirical_compositional_preprocess"
    ),
    dry_run = FALSE
  )
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (identical(token, "--dry-run")) {
      parsed$dry_run <- TRUE
      index <- index + 1L
      next
    }
    if (token %in% c("--input", "--output-root")) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        stop("Missing value after ", token, ".", call. = FALSE)
      }
      parsed[[sub("-", "_", sub("^--", "", token))]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    stop("Unknown argument: ", token, call. = FALSE)
  }
  if (is.null(parsed$input)) {
    stop(
      "Usage: build_empirical_compositional_views.R --input FILE ",
      "[--output-root DIR] [--dry-run]",
      call. = FALSE
    )
  }
  parsed
}

lomm_generator_hashes <- function() {
  current <- as.list(vapply(LOMM_GENERATOR_PATHS, lomm_file_sha256, character(1)))
  if (!identical(current, LOMM_STARTUP_GENERATOR_FILES)) {
    stop("Preprocessing generator files changed during startup or execution.", call. = FALSE)
  }
  list(files = LOMM_STARTUP_GENERATOR_FILES, combined = LOMM_STARTUP_GENERATOR_HASH)
}

lomm_manifest_parameters <- function() {
  list(
    ordered_score_columns = as.list(LOMM_EMPIRICAL_SCORE_COLUMNS),
    metadata_columns = as.list(c(
      "frame_index", "clip", "truth_label", "sample_order", "model", "predicted_label"
    )),
    variants = list(
      complete_case = list(max_gap = 0L, interpolation = "none"),
      ilr_gap2 = list(max_gap = 2L, interpolation = "linear_ilr_by_frame_index")
    ),
    closure_tolerance = 1e-6,
    zero_policy = "error",
    expected_frame_step = 1L,
    ilr_basis = "sequential_helmert_v1"
  )
}

lomm_manifest_plan <- function(input_path, input_hash, bundle, generator) {
  parameters <- lomm_manifest_parameters()
  parameters$ilr_basis_sha256 <- lomm_object_sha256(
    lomm_basis_specification(bundle$basis_table)
  )
  tables <- lomm_bundle_tables(bundle)
  schemas <- lomm_expected_output_schemas()
  if (!identical(lapply(tables, names), schemas)) {
    stop("Prospective output schemas differ from the locked contract.", call. = FALSE)
  }
  for (name in names(tables)) {
    observed_dimension <- c(rows = nrow(tables[[name]]), columns = ncol(tables[[name]]))
    expected_dimension <- LOMM_EMPIRICAL_EXPECTED_OUTPUT_DIMENSIONS[[name]]
    if (!identical(as.integer(observed_dimension), as.integer(expected_dimension))) {
      stop("Prospective output dimensions differ from the locked contract: ", name, call. = FALSE)
    }
  }
  prospective_outputs <- lapply(names(tables), function(name) list(
    path = name,
    rows = nrow(tables[[name]]),
    columns = ncol(tables[[name]])
  ))
  input_spec_hash <- lomm_object_sha256(list(
    input_sha256 = input_hash,
    parameters = parameters,
    schema_version = LOMM_COMPOSITION_SCHEMA_VERSION
  ))
  config_hash <- substr(lomm_object_sha256(list(
    input_spec_hash = input_spec_hash,
    generator_hash = generator$combined
  )), 1L, 12L)
  list(
    schema_version = LOMM_COMPOSITION_SCHEMA_VERSION,
    status = "planned",
    dry_run = TRUE,
    pipeline_id = "lomm_v1_1_empirical_compositional_preprocess",
    config_hash = config_hash,
    input_spec_hash = input_spec_hash,
    generator_hash = generator$combined,
    generator_files = generator$files,
    input_artifact = list(
      path = normalizePath(input_path, mustWork = TRUE),
      sha256 = input_hash,
      rows = nrow(bundle$frame_ledger),
      columns = bundle$source_column_count,
      column_names = as.list(bundle$source_column_names)
    ),
    parameters = parameters,
    expected_counts = LOMM_EMPIRICAL_EXPECTED_COUNTS,
    observed_counts = bundle$observed_counts,
    randomness = "none",
    planned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    command = paste(commandArgs(), collapse = " "),
    software_versions = list(
      R = R.version.string,
      digest = as.character(utils::packageVersion("digest")),
      jsonlite = as.character(utils::packageVersion("jsonlite"))
    ),
    git = lomm_git_state(),
    output_schema = schemas,
    prospective_outputs = prospective_outputs,
    expected_outputs = as.list(c(
      "frame_ledger.csv", "probability_view.csv", "clr_view.csv", "ilr_view.csv",
      "ilr_basis.csv", "clip_qc.csv", "preprocessing_manifest.json", "_SUCCESS.json"
    )),
    checks = as.list(c(
      "locked input hash", "exact source counts", "strict simplex validation",
      "orthonormal ILR basis", "CLR zero-sum", "ILR round-trip",
      "aligned view keys", "deterministic segment IDs", "no estimator calls"
    ))
  )
}

lomm_write_json <- function(value, path) {
  jsonlite::write_json(
    value, path, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
  )
}

lomm_write_bundle_files <- function(bundle, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  files <- lomm_bundle_tables(bundle)
  for (name in names(files)) {
    utils::write.csv(files[[name]], file.path(directory, name), row.names = FALSE, na = "")
  }
  files
}

lomm_validate_serialized_tables <- function(directory, expected, basis) {
  observed <- lomm_validation_read_tables(directory)
  for (name in names(expected)) {
    if (!identical(names(observed[[name]]), names(expected[[name]])) ||
        nrow(observed[[name]]) != nrow(expected[[name]])) {
      stop("Serialized table schema mismatch: ", name, call. = FALSE)
    }
  }
  keys <- lapply(
    observed[c("probability_view.csv", "clr_view.csv", "ilr_view.csv")],
    function(view) view$observation_key
  )
  if (!identical(keys[[1L]], keys[[2L]]) || !identical(keys[[1L]], keys[[3L]]) ||
      anyDuplicated(keys[[1L]])) {
    stop("Serialized compositional view keys do not align.", call. = FALSE)
  }
  probabilities <- as.matrix(observed$probability_view.csv[LOMM_EMPIRICAL_SCORE_COLUMNS])
  lomm_validate_positive_closed(probabilities, tolerance = 1e-12)
  clr_columns <- paste0("clr_", LOMM_EMPIRICAL_SCORE_COLUMNS)
  clr_values <- as.matrix(observed$clr_view.csv[clr_columns])
  if (max(abs(rowSums(clr_values))) > 1e-12) {
    stop("Serialized CLR rows do not sum to zero.", call. = FALSE)
  }
  basis_table <- observed$ilr_basis.csv
  serialized_basis <- as.matrix(basis_table[colnames(basis)])
  rownames(serialized_basis) <- basis_table$channel
  lomm_validate_ilr_basis(serialized_basis, LOMM_EMPIRICAL_SCORE_COLUMNS)
  if (max(abs(serialized_basis - basis)) > 1e-15) {
    stop("Serialized ILR basis differs from the in-memory basis.", call. = FALSE)
  }
  ilr_values <- as.matrix(observed$ilr_view.csv[colnames(basis)])
  if (max(abs(lomm_ilr_inverse(ilr_values, serialized_basis) - probabilities)) > 1e-12) {
    stop("Serialized ILR values do not round-trip to probabilities.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_git_state <- function() {
  commit <- system2("git", c("-C", LOMM_REPO_ROOT, "rev-parse", "HEAD"), stdout = TRUE)
  dirty <- length(system2(
    "git", c("-C", LOMM_REPO_ROOT, "status", "--porcelain"), stdout = TRUE
  )) > 0L
  list(commit = unname(commit[[1L]]), dirty = dirty)
}

lomm_lexically_normalize_absolute_path <- function(path) {
  components <- strsplit(path, paste0(.Platform$file.sep, "+"))[[1L]]
  normalized <- character()
  for (component in components) {
    if (!nzchar(component) || identical(component, ".")) {
      next
    }
    if (identical(component, "..")) {
      if (length(normalized) > 0L) {
        normalized <- normalized[-length(normalized)]
      }
      next
    }
    normalized <- c(normalized, component)
  }
  paste0(.Platform$file.sep, paste(normalized, collapse = .Platform$file.sep))
}

lomm_canonical_output_path <- function(output_root) {
  expanded <- path.expand(output_root)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
  existing <- expanded
  suffix <- character()
  while (!file.exists(existing)) {
    parent <- dirname(existing)
    if (identical(parent, existing)) {
      stop("Cannot resolve the output-root path.", call. = FALSE)
    }
    suffix <- c(basename(existing), suffix)
    existing <- parent
  }
  resolved <- normalizePath(existing, mustWork = TRUE)
  combined <- if (length(suffix) == 0L) {
    resolved
  } else {
    do.call(file.path, as.list(c(resolved, suffix)))
  }
  lomm_lexically_normalize_absolute_path(combined)
}

lomm_validate_output_root <- function(output_root) {
  absolute <- lomm_canonical_output_path(output_root)
  revision_root <- normalizePath(
    file.path(LOMM_REPO_ROOT, "simulation_outputs", "revision_v2"),
    mustWork = FALSE
  )
  nested_revision_path <- identical(absolute, revision_root) ||
    startsWith(absolute, paste0(revision_root, .Platform$file.sep))
  if (nested_revision_path) {
    stop("LOMM 1.1 preprocessing cannot publish under revision_v2.", call. = FALSE)
  }
  absolute
}

lomm_assert_no_source_drift <- function(input_path, input_hash, generator) {
  if (!identical(lomm_file_sha256(input_path), input_hash)) {
    stop("Empirical input changed during preprocessing.", call. = FALSE)
  }
  current_generator <- lomm_generator_hashes()
  if (!identical(current_generator$files, generator$files) ||
      !identical(current_generator$combined, generator$combined)) {
    stop("Preprocessing generator files changed during execution.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_output_records <- function(tables, directory) {
  paths <- file.path(directory, names(tables))
  lapply(seq_along(paths), function(index) list(
    path = names(tables)[[index]],
    sha256 = lomm_file_sha256(paths[[index]]),
    rows = nrow(tables[[index]]),
    columns = ncol(tables[[index]])
  ))
}

lomm_complete_manifest <- function(plan, bundle, tables, outputs) {
  manifest <- plan
  manifest$status <- "complete"
  manifest$dry_run <- FALSE
  manifest$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  manifest$command <- paste(commandArgs(), collapse = " ")
  manifest$output_schema <- lapply(tables, names)
  manifest$outputs <- outputs
  manifest$software_versions <- list(
    R = R.version.string,
    digest = as.character(utils::packageVersion("digest")),
    jsonlite = as.character(utils::packageVersion("jsonlite"))
  )
  manifest$git <- lomm_git_state()
  manifest$limitations <- as.list(c(
    "Historical scoring-time package and checkpoint versions were not recorded.",
    "ILR-interpolated rows are sensitivity values rather than direct classifier outputs.",
    "The measurement surface is specific to the fixed 11-label prompt set."
  ))
  manifest
}

lomm_publish_bundle <- function(bundle, plan, output_root) {
  final_directory <- file.path(output_root, plan$config_hash)
  if (dir.exists(final_directory)) {
    stop("Output namespace already exists: ", final_directory, call. = FALSE)
  }
  staging <- paste0(final_directory, ".staging-", Sys.getpid())
  if (dir.exists(staging)) {
    stop("Staging namespace already exists: ", staging, call. = FALSE)
  }
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
  tables <- lomm_write_bundle_files(bundle, staging)
  lomm_validate_serialized_tables(staging, tables, bundle$basis)
  outputs <- lomm_output_records(tables, staging)
  manifest <- lomm_complete_manifest(plan, bundle, tables, outputs)
  manifest_path <- file.path(staging, "preprocessing_manifest.json")
  lomm_write_json(manifest, manifest_path)
  success <- list(
    schema_version = LOMM_COMPOSITION_SCHEMA_VERSION,
    status = "complete",
    config_hash = plan$config_hash,
    manifest_sha256 = lomm_file_sha256(manifest_path),
    outputs = outputs,
    observed_counts = bundle$observed_counts
  )
  lomm_write_json(success, file.path(staging, "_SUCCESS.json"))
  lomm_validate_published_bundle(staging, allow_staging = TRUE)
  dir.create(dirname(final_directory), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(staging, final_directory)) {
    stop("Failed to publish the atomic preprocessing bundle.", call. = FALSE)
  }
  final_directory
}

lomm_builder_main <- function() {
  lomm_require_builder_packages()
  args <- lomm_parse_builder_args(commandArgs(trailingOnly = TRUE))
  args$output_root <- lomm_validate_output_root(args$output_root)
  generator <- lomm_generator_hashes()
  input_path <- normalizePath(args$input, mustWork = TRUE)
  input_hash <- lomm_file_sha256(input_path)
  if (!identical(input_hash, LOMM_EMPIRICAL_INPUT_SHA256)) {
    stop("Empirical input hash differs from the locked retained-100 artifact.", call. = FALSE)
  }
  panel <- utils::read.csv(input_path, stringsAsFactors = FALSE)
  lomm_assert_no_source_drift(input_path, input_hash, generator)
  lomm_validate_empirical_source(panel)
  bundle <- lomm_build_bundle(panel)
  lomm_assert_no_source_drift(input_path, input_hash, generator)
  plan <- lomm_manifest_plan(input_path, input_hash, bundle, generator)
  if (isTRUE(args$dry_run)) {
    lomm_assert_no_source_drift(input_path, input_hash, generator)
    cat(jsonlite::toJSON(
      plan, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
    ), "\n")
    return(invisible(plan))
  }
  lomm_assert_no_source_drift(input_path, input_hash, generator)
  output_directory <- lomm_publish_bundle(bundle, plan, args$output_root)
  lomm_validate_published_bundle(output_directory)
  cat("Published LOMM 1.1 compositional preprocessing bundle:\n", output_directory, "\n")
  invisible(output_directory)
}

if (sys.nframe() == 0L) {
  lomm_builder_main()
}
