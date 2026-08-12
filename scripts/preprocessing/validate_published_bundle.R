#!/usr/bin/env Rscript

lomm_validator_script_path <- function() {
  if (exists("LOMM_BUILDER_PATH", inherits = TRUE)) {
    return(file.path(dirname(LOMM_BUILDER_PATH), "validate_published_bundle.R"))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the compositional-bundle validator.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM_VALIDATOR_PATH <- lomm_validator_script_path()
LOMM_VALIDATOR_DIRECTORY <- dirname(LOMM_VALIDATOR_PATH)
if (!exists("lomm_build_bundle", mode = "function")) {
  source(file.path(LOMM_VALIDATOR_DIRECTORY, "compositional_math.R"))
  source(file.path(LOMM_VALIDATOR_DIRECTORY, "compositional_views.R"))
  source(file.path(LOMM_VALIDATOR_DIRECTORY, "empirical_compositional_bundle.R"))
}
if (!exists("LOMM_REPO_ROOT", inherits = TRUE)) {
  LOMM_REPO_ROOT <- normalizePath(file.path(LOMM_VALIDATOR_DIRECTORY, "..", ".."))
}

lomm_validator_require_packages <- function() {
  for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is required.", call. = FALSE)
    }
  }
}

lomm_validation_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm_validation_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

lomm_validation_csv_classes <- function(table_name) {
  schema <- lomm_expected_output_schemas()[[table_name]]
  classes <- rep("numeric", length(schema))
  names(classes) <- schema
  character_columns <- c(
    "frame_key", "clip", "truth_label", "model", "predicted_label",
    "missingness_class", "imputation_method", "imputation_block_id",
    "primary_segment_id", "sensitivity_segment_id", "exclusion_reason",
    "observation_key", "analysis_variant", "segment_id", "channel"
  )
  logical_columns <- c(
    "source_complete", "renormalized", "zero_replaced", "primary_included",
    "sensitivity_included", "is_imputed"
  )
  classes[names(classes) %in% character_columns] <- "character"
  classes[names(classes) %in% logical_columns] <- "logical"
  unname(classes)
}

lomm_validation_read_tables <- function(directory) {
  table_names <- names(lomm_expected_output_schemas())
  setNames(lapply(table_names, function(name) {
    utils::read.csv(
      file.path(directory, name),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = lomm_validation_csv_classes(name),
      na.strings = ""
    )
  }), table_names)
}

lomm_validation_require_exact_fields <- function(value, fields, label) {
  if (!setequal(names(value), fields)) {
    stop(label, " fields differ from the locked schema.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_validation_scalar_integer <- function(value, label) {
  if (length(value) != 1L || !is.integer(value) || is.na(value)) {
    stop(label, " must be one JSON integer.", call. = FALSE)
  }
  value
}

lomm_validation_scalar_string <- function(value, label) {
  if (length(value) != 1L || !is.character(value) || is.na(value) || !nzchar(value)) {
    stop(label, " must be one non-empty JSON string.", call. = FALSE)
  }
  value
}

lomm_validation_counts <- function(value) {
  setNames(vapply(names(value), function(name) {
    lomm_validation_scalar_integer(value[[name]], paste("Count", name))
  }, integer(1)), names(value))
}

lomm_validation_records <- function(records) {
  rows <- lapply(records, function(record) {
    lomm_validation_require_exact_fields(
      record, c("path", "sha256", "rows", "columns"), "Output record"
    )
    data.frame(
      path = lomm_validation_scalar_string(record$path, "Output path"),
      sha256 = lomm_validation_scalar_string(record$sha256, "Output SHA-256"),
      rows = lomm_validation_scalar_integer(record$rows, "Output rows"),
      columns = lomm_validation_scalar_integer(record$columns, "Output columns"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

lomm_validation_prospective_records <- function(records) {
  rows <- lapply(records, function(record) {
    lomm_validation_require_exact_fields(
      record, c("path", "rows", "columns"), "Prospective output record"
    )
    data.frame(
      path = lomm_validation_scalar_string(record$path, "Prospective output path"),
      rows = lomm_validation_scalar_integer(record$rows, "Prospective output rows"),
      columns = lomm_validation_scalar_integer(record$columns, "Prospective output columns"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

lomm_validation_generator_paths <- function() {
  c(
    math = file.path(LOMM_VALIDATOR_DIRECTORY, "compositional_math.R"),
    views = file.path(LOMM_VALIDATOR_DIRECTORY, "compositional_views.R"),
    bundle = file.path(LOMM_VALIDATOR_DIRECTORY, "empirical_compositional_bundle.R"),
    validator = LOMM_VALIDATOR_PATH,
    builder = file.path(LOMM_VALIDATOR_DIRECTORY, "build_empirical_compositional_views.R")
  )
}

lomm_validate_receipt_headers <- function(manifest, success) {
  manifest_fields <- c(
    "schema_version", "status", "dry_run", "pipeline_id", "config_hash",
    "input_spec_hash", "generator_hash", "generator_files", "input_artifact",
    "parameters", "expected_counts", "observed_counts", "randomness",
    "planned_at", "command", "software_versions", "git", "output_schema",
    "prospective_outputs", "expected_outputs", "checks", "created_at",
    "outputs", "limitations"
  )
  success_fields <- c(
    "schema_version", "status", "config_hash", "manifest_sha256", "outputs",
    "observed_counts"
  )
  lomm_validation_require_exact_fields(
    manifest,
    manifest_fields,
    "Preprocessing manifest"
  )
  lomm_validation_require_exact_fields(
    success,
    success_fields,
    "Success receipt"
  )
  valid <- identical(manifest$schema_version, LOMM_COMPOSITION_SCHEMA_VERSION) &&
    identical(success$schema_version, LOMM_COMPOSITION_SCHEMA_VERSION) &&
    identical(manifest$status, "complete") && identical(success$status, "complete") &&
    identical(manifest$dry_run, FALSE) &&
    identical(manifest$pipeline_id, "lomm_v1_1_empirical_compositional_preprocess") &&
    identical(manifest$config_hash, success$config_hash) &&
    identical(manifest$randomness, "none")
  if (!valid) {
    stop("Published receipt headers are inconsistent.", call. = FALSE)
  }
  string_fields <- c(
    "config_hash", "input_spec_hash", "generator_hash", "planned_at", "created_at",
    "command"
  )
  for (field in string_fields) {
    lomm_validation_scalar_string(manifest[[field]], paste("Manifest", field))
  }
  lomm_validation_scalar_string(success$manifest_sha256, "Success manifest SHA-256")
  if (!grepl("^[0-9a-f]{12}$", manifest$config_hash) ||
      !grepl("^[0-9a-f]{64}$", manifest$input_spec_hash) ||
      !grepl("^[0-9a-f]{64}$", manifest$generator_hash) ||
      !grepl("^[0-9a-f]{64}$", success$manifest_sha256)) {
    stop("Published receipt hashes have invalid formats.", call. = FALSE)
  }
  lomm_validation_require_exact_fields(
    manifest$software_versions, c("R", "digest", "jsonlite"), "Software versions"
  )
  if (any(!vapply(manifest$software_versions, function(value) {
    length(value) == 1L && is.character(value) && !is.na(value) && nzchar(value)
  }, logical(1)))) {
    stop("Software versions must be non-empty strings.", call. = FALSE)
  }
  lomm_validation_require_exact_fields(manifest$git, c("commit", "dirty"), "Git record")
  if (!grepl("^[0-9a-f]{40}$", lomm_validation_scalar_string(
    manifest$git$commit, "Git commit"
  )) || length(manifest$git$dirty) != 1L || !is.logical(manifest$git$dirty) ||
      is.na(manifest$git$dirty)) {
    stop("Git provenance record is invalid.", call. = FALSE)
  }
  for (field in c("checks", "limitations")) {
    values <- unlist(manifest[[field]], use.names = FALSE)
    if (length(values) == 0L || !is.character(values) || anyNA(values) || any(!nzchar(values))) {
      stop("Manifest ", field, " must contain non-empty strings.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

lomm_validate_parameter_contract <- function(manifest) {
  parameters <- manifest$parameters
  expected_fields <- c(
    "ordered_score_columns", "metadata_columns", "variants", "closure_tolerance",
    "zero_policy", "expected_frame_step", "ilr_basis", "ilr_basis_sha256"
  )
  lomm_validation_require_exact_fields(parameters, expected_fields, "Parameters")
  score_columns <- unlist(parameters$ordered_score_columns, use.names = FALSE)
  metadata_columns <- unlist(parameters$metadata_columns, use.names = FALSE)
  expected_metadata <- c(
    "frame_index", "clip", "truth_label", "sample_order", "model", "predicted_label"
  )
  valid <- identical(score_columns, LOMM_EMPIRICAL_SCORE_COLUMNS) &&
    identical(metadata_columns, expected_metadata) &&
    identical(parameters$closure_tolerance, 1e-6) &&
    identical(parameters$zero_policy, "error") &&
    identical(parameters$expected_frame_step, 1L) &&
    identical(parameters$ilr_basis, "sequential_helmert_v1") &&
    grepl(
      "^[0-9a-f]{64}$",
      lomm_validation_scalar_string(parameters$ilr_basis_sha256, "ILR basis SHA-256")
    )
  if (!valid) {
    stop("Manifest parameters differ from the locked preprocessing specification.", call. = FALSE)
  }
  lomm_validation_require_exact_fields(
    parameters$variants, c("complete_case", "ilr_gap2"), "Analysis variants"
  )
  lomm_validation_require_exact_fields(
    parameters$variants$complete_case, c("max_gap", "interpolation"),
    "Complete-case variant"
  )
  lomm_validation_require_exact_fields(
    parameters$variants$ilr_gap2, c("max_gap", "interpolation"), "ILR-gap variant"
  )
  variants_valid <- identical(parameters$variants$complete_case$max_gap, 0L) &&
    identical(parameters$variants$complete_case$interpolation, "none") &&
    identical(parameters$variants$ilr_gap2$max_gap, 2L) &&
    identical(
      parameters$variants$ilr_gap2$interpolation, "linear_ilr_by_frame_index"
    )
  if (!variants_valid) {
    stop("Analysis variants differ from the locked preprocessing specification.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_validate_count_contract <- function(manifest, success, tables) {
  expected <- setNames(
    as.integer(unlist(LOMM_EMPIRICAL_EXPECTED_COUNTS, use.names = FALSE)),
    names(LOMM_EMPIRICAL_EXPECTED_COUNTS)
  )
  manifest_expected <- lomm_validation_counts(manifest$expected_counts)
  manifest_observed <- lomm_validation_counts(manifest$observed_counts)
  success_observed <- lomm_validation_counts(success$observed_counts)
  derived <- unlist(
    lomm_observed_counts(tables$frame_ledger.csv, tables$clip_qc.csv),
    use.names = TRUE
  )
  derived <- setNames(as.integer(derived), names(derived))
  if (!identical(manifest_expected, expected) ||
      !identical(manifest_observed, expected) ||
      !identical(success_observed, expected) ||
      !identical(derived, expected)) {
    stop("Published expected, observed, or derived counts differ.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_validate_output_contract <- function(
    directory, manifest, success, tables, allow_staging = FALSE) {
  schemas <- lomm_expected_output_schemas()
  dimensions <- LOMM_EMPIRICAL_EXPECTED_OUTPUT_DIMENSIONS
  expected_files <- c(names(schemas), "preprocessing_manifest.json", "_SUCCESS.json")
  serialized_expected_files <- unlist(manifest$expected_outputs, use.names = FALSE)
  staging_pattern <- paste0("^", manifest$config_hash, "[.]staging-[0-9]+$")
  valid_directory_name <- identical(basename(directory), manifest$config_hash) ||
    (isTRUE(allow_staging) && grepl(staging_pattern, basename(directory)))
  if (!valid_directory_name) {
    stop("Published directory name differs from the configuration hash.", call. = FALSE)
  }
  if (!identical(sort(list.files(directory)), sort(expected_files))) {
    stop("Published bundle contains missing or unexpected files.", call. = FALSE)
  }
  if (!identical(serialized_expected_files, expected_files)) {
    stop("Manifest expected outputs differ from the locked file contract.", call. = FALSE)
  }
  for (name in names(schemas)) {
    observed_dimensions <- c(rows = nrow(tables[[name]]), columns = ncol(tables[[name]]))
    if (!identical(names(tables[[name]]), schemas[[name]]) ||
        !identical(as.integer(observed_dimensions), as.integer(dimensions[[name]]))) {
      stop("Published table schema mismatch: ", name, call. = FALSE)
    }
  }
  serialized_schemas <- lapply(manifest$output_schema, unlist, use.names = FALSE)
  if (!identical(serialized_schemas, schemas)) {
    stop("Manifest output schemas differ from the locked contract.", call. = FALSE)
  }
  prospective_records <- lomm_validation_prospective_records(manifest$prospective_outputs)
  prospective_dimensions <- do.call(rbind, lapply(dimensions, function(value) {
    data.frame(
      rows = as.integer(value[["rows"]]),
      columns = as.integer(value[["columns"]])
    )
  }))
  prospective_expected <- data.frame(
    path = names(dimensions),
    prospective_dimensions,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  if (!identical(prospective_records, prospective_expected)) {
    stop("Manifest prospective outputs differ from the locked table contract.", call. = FALSE)
  }
  manifest_records <- lomm_validation_records(manifest$outputs)
  success_records <- lomm_validation_records(success$outputs)
  if (!identical(manifest_records, success_records) ||
      !identical(manifest_records$path, names(schemas))) {
    stop("Manifest and success output records differ.", call. = FALSE)
  }
  for (index in seq_len(nrow(manifest_records))) {
    name <- manifest_records$path[[index]]
    path <- file.path(directory, name)
    expected_dimension <- dimensions[[name]]
    valid <- identical(manifest_records$sha256[[index]], lomm_validation_sha256(path)) &&
      identical(manifest_records$rows[[index]], as.integer(expected_dimension[["rows"]])) &&
      identical(manifest_records$columns[[index]], as.integer(expected_dimension[["columns"]]))
    if (!valid) {
      stop("Published output record mismatch: ", name, call. = FALSE)
    }
  }
  invisible(TRUE)
}

lomm_validate_numeric_views <- function(tables, manifest) {
  keys <- lapply(
    tables[c("probability_view.csv", "clr_view.csv", "ilr_view.csv")],
    function(view) view$observation_key
  )
  if (!identical(keys[[1L]], keys[[2L]]) || !identical(keys[[1L]], keys[[3L]]) ||
      anyDuplicated(keys[[1L]])) {
    stop("Published compositional view keys do not align.", call. = FALSE)
  }
  probabilities <- as.matrix(tables$probability_view.csv[LOMM_EMPIRICAL_SCORE_COLUMNS])
  lomm_validate_positive_closed(probabilities, tolerance = 1e-12)
  clr <- as.matrix(tables$clr_view.csv[paste0("clr_", LOMM_EMPIRICAL_SCORE_COLUMNS)])
  if (max(abs(rowSums(clr))) > 1e-12) {
    stop("Published CLR rows do not sum to zero.", call. = FALSE)
  }
  basis_table <- tables$ilr_basis.csv
  basis <- as.matrix(basis_table[sprintf("ilr_%02d", 1:10)])
  rownames(basis) <- basis_table$channel
  lomm_validate_ilr_basis(basis, LOMM_EMPIRICAL_SCORE_COLUMNS)
  ilr <- as.matrix(tables$ilr_view.csv[colnames(basis)])
  if (max(abs(lomm_ilr_inverse(ilr, basis) - probabilities)) > 1e-12) {
    stop("Published ILR values do not round-trip to probabilities.", call. = FALSE)
  }
  basis_hash <- lomm_validation_object_sha256(
    lomm_basis_specification(tables$ilr_basis.csv)
  )
  if (!identical(manifest$parameters$ilr_basis_sha256, basis_hash)) {
    stop("Published ILR basis hash differs from the manifest parameter.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_validate_segment_contract <- function(ledger) {
  if (anyDuplicated(ledger$frame_key) ||
      !identical(sort(as.integer(ledger$source_row)), seq_len(nrow(ledger)))) {
    stop("Published frame ledger keys or source rows are invalid.", call. = FALSE)
  }
  primary <- lomm_segment_metadata(
    ledger$clip, ledger$frame_index, ledger$primary_included, expected_step = 1
  )
  sensitivity <- lomm_segment_metadata(
    ledger$clip, ledger$frame_index, ledger$sensitivity_included, expected_step = 1
  )
  primary_valid <- identical(primary$segment_id, ledger$primary_segment_id) &&
    identical(primary$segment_position, as.integer(ledger$primary_segment_position)) &&
    identical(primary$segment_length, as.integer(ledger$primary_segment_length))
  sensitivity_valid <- identical(sensitivity$segment_id, ledger$sensitivity_segment_id) &&
    identical(sensitivity$segment_position, as.integer(ledger$sensitivity_segment_position)) &&
    identical(sensitivity$segment_length, as.integer(ledger$sensitivity_segment_length))
  if (!primary_valid || !sensitivity_valid) {
    stop("Published segment identifiers or positions are not reproducible.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_validate_imputation_contract <- function(tables) {
  ledger <- tables$frame_ledger.csv
  imputed <- which(ledger$is_imputed)
  valid_flags <- length(imputed) == LOMM_EMPIRICAL_EXPECTED_COUNTS$imputed_rows &&
    all(!ledger$source_complete[imputed]) && all(ledger$sensitivity_included[imputed]) &&
    all(!ledger$primary_included[imputed]) &&
    all(ledger$imputation_gap_length[imputed] %in% c(1, 2)) &&
    all(ledger$interpolation_weight[imputed] > 0 & ledger$interpolation_weight[imputed] < 1)
  if (!valid_flags) {
    stop("Published imputation flags violate the bounded-gap contract.", call. = FALSE)
  }
  ilr <- tables$ilr_view.csv
  sensitivity <- ilr[ilr$analysis_variant == "ilr_gap2", , drop = FALSE]
  row_lookup <- setNames(seq_len(nrow(sensitivity)), as.character(sensitivity$source_row))
  coordinates <- as.matrix(sensitivity[sprintf("ilr_%02d", 1:10)])
  for (ledger_index in imputed) {
    target <- row_lookup[[as.character(ledger$source_row[[ledger_index]])]]
    left <- row_lookup[[as.character(ledger$left_source_row[[ledger_index]])]]
    right <- row_lookup[[as.character(ledger$right_source_row[[ledger_index]])]]
    weight <- ledger$interpolation_weight[[ledger_index]]
    expected <- (1 - weight) * coordinates[left, ] + weight * coordinates[right, ]
    if (max(abs(coordinates[target, ] - expected)) > 1e-12) {
      stop("Published ILR interpolation is not affine between source endpoints.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

lomm_validation_discrete_numeric_columns <- function() {
  c(
    "source_row", "frame_index", "sample_order", "imputation_gap_length",
    "left_source_row", "right_source_row", "primary_segment_position",
    "primary_segment_length", "sensitivity_segment_position",
    "sensitivity_segment_length", "segment_position", "segment_length",
    "channel_order", "source_rows", "complete_rows", "all_missing_rows",
    "partial_missing_rows", "imputed_rows", "unfilled_missing_rows",
    "primary_segments", "sensitivity_segments", "primary_max_segment_length",
    "sensitivity_max_segment_length"
  )
}

lomm_validation_numeric_column_matches <- function(actual, target, column_name) {
  if (!is.numeric(actual) || !identical(is.na(actual), is.na(target))) {
    return(FALSE)
  }
  observed <- !is.na(target)
  if (column_name %in% lomm_validation_discrete_numeric_columns()) {
    return(identical(as.numeric(actual[observed]), as.numeric(target[observed])))
  }
  differences <- abs(actual[observed] - target[observed])
  length(differences) == 0L ||
    (all(is.finite(differences)) && max(differences) <= 1e-12)
}

lomm_validate_rebuilt_tables <- function(tables, source) {
  lomm_validate_empirical_source(source)
  expected <- lomm_bundle_tables(lomm_build_bundle(source))
  for (table_name in names(expected)) {
    actual_table <- tables[[table_name]]
    expected_table <- expected[[table_name]]
    for (column_name in names(expected_table)) {
      actual <- actual_table[[column_name]]
      target <- expected_table[[column_name]]
      matches <- if (is.numeric(target)) {
        lomm_validation_numeric_column_matches(actual, target, column_name)
      } else if (is.character(target)) {
        target[target == ""] <- NA_character_
        identical(actual, target)
      } else {
        identical(actual, target)
      }
      if (!matches) {
        stop(
          "Published table differs from the deterministic source rebuild: ",
          table_name, "$", column_name, call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

lomm_validate_hash_contract <- function(directory, manifest, success, tables) {
  manifest_path <- file.path(directory, "preprocessing_manifest.json")
  if (!identical(success$manifest_sha256, lomm_validation_sha256(manifest_path))) {
    stop("Published manifest hash differs from the success receipt.", call. = FALSE)
  }
  input <- manifest$input_artifact
  lomm_validation_require_exact_fields(
    input, c("path", "sha256", "rows", "columns", "column_names"), "Input artifact"
  )
  input_path <- lomm_validation_scalar_string(input$path, "Input path")
  input_hash <- lomm_validation_scalar_string(input$sha256, "Input SHA-256")
  input_rows <- lomm_validation_scalar_integer(input$rows, "Input rows")
  input_columns <- lomm_validation_scalar_integer(input$columns, "Input columns")
  input_names <- unlist(input$column_names, use.names = FALSE)
  if (!identical(input_hash, LOMM_EMPIRICAL_INPUT_SHA256)) {
    stop("Published input hash differs from the locked empirical artifact.", call. = FALSE)
  }
  if (!file.exists(input_path) || !identical(lomm_validation_sha256(input_path), input_hash)) {
    stop("Published input artifact is missing or hash-mismatched.", call. = FALSE)
  }
  source <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(source) != input_rows || ncol(source) != input_columns ||
      !identical(names(source), input_names)) {
    stop("Published input artifact dimensions or columns differ.", call. = FALSE)
  }
  lomm_validate_rebuilt_tables(tables, source)
  lomm_validation_require_exact_fields(
    manifest$generator_files, names(lomm_validation_generator_paths()), "Generator files"
  )
  stored_generator <- unlist(manifest$generator_files, use.names = TRUE)
  if (!is.character(stored_generator) || any(!grepl("^[0-9a-f]{64}$", stored_generator))) {
    stop("Published generator hashes have invalid formats.", call. = FALSE)
  }
  generator_paths <- lomm_validation_generator_paths()
  current_generator <- vapply(generator_paths, lomm_validation_sha256, character(1))
  if (!identical(stored_generator, current_generator) ||
      !identical(manifest$generator_hash, lomm_validation_object_sha256(as.list(stored_generator)))) {
    stop("Published generator hashes differ from current source files.", call. = FALSE)
  }
  input_spec_hash <- lomm_validation_object_sha256(list(
    input_sha256 = input_hash,
    parameters = manifest$parameters,
    schema_version = manifest$schema_version
  ))
  config_hash <- substr(lomm_validation_object_sha256(list(
    input_spec_hash = input_spec_hash,
    generator_hash = manifest$generator_hash
  )), 1L, 12L)
  if (!identical(manifest$input_spec_hash, input_spec_hash) ||
      !identical(manifest$config_hash, config_hash)) {
    stop("Published input-spec or configuration hash is not reproducible.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm_validate_published_bundle <- function(directory, allow_staging = FALSE) {
  lomm_validator_require_packages()
  directory <- normalizePath(directory, mustWork = TRUE)
  manifest_path <- file.path(directory, "preprocessing_manifest.json")
  success_path <- file.path(directory, "_SUCCESS.json")
  if (!file.exists(manifest_path) || !file.exists(success_path)) {
    stop("Published bundle is missing completion receipts.", call. = FALSE)
  }
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  success <- jsonlite::fromJSON(success_path, simplifyVector = FALSE)
  lomm_validate_receipt_headers(manifest, success)
  lomm_validate_parameter_contract(manifest)
  tables <- lomm_validation_read_tables(directory)
  lomm_validate_output_contract(
    directory, manifest, success, tables, allow_staging = allow_staging
  )
  lomm_validate_count_contract(manifest, success, tables)
  lomm_validate_numeric_views(tables, manifest)
  lomm_validate_segment_contract(tables$frame_ledger.csv)
  lomm_validate_imputation_contract(tables)
  lomm_validate_hash_contract(directory, manifest, success, tables)
  invisible(TRUE)
}

lomm_validator_main <- function(args) {
  if (length(args) != 2L || !identical(args[[1L]], "--directory") ||
      startsWith(args[[2L]], "--")) {
    stop("Usage: validate_published_bundle.R --directory DIR", call. = FALSE)
  }
  directory <- normalizePath(args[[2L]], mustWork = TRUE)
  lomm_validate_published_bundle(directory)
  cat("LOMM compositional preprocessing bundle validation: PASS\n", directory, "\n")
}

if (sys.nframe() == 0L) {
  lomm_validator_main(commandArgs(trailingOnly = TRUE))
}
