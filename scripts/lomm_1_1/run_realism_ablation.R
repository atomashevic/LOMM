#!/usr/bin/env Rscript

lomm11_runner_script_path <- function() {
  if (exists("LOMM11_RUNNER_PATH", inherits = TRUE)) {
    return(normalizePath(get("LOMM11_RUNNER_PATH", inherits = TRUE), mustWork = TRUE))
  }
  frame_paths <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frame_paths <- frame_paths[!is.na(frame_paths) & nzchar(frame_paths)]
  if (length(frame_paths) > 0L) {
    return(normalizePath(tail(frame_paths, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the LOMM 1.1 ablation runner.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_RUNNER_PATH <- lomm11_runner_script_path()
LOMM11_RUNNER_DIRECTORY <- dirname(LOMM11_RUNNER_PATH)
LOMM11_RUNNER_REPO_ROOT <- normalizePath(
  file.path(LOMM11_RUNNER_DIRECTORY, "..", ".."), mustWork = TRUE
)
LOMM11_RUNNER_TOKEN <- "LOMM-1.1-REALISM-AUTHORIZED"
LOMM11_RUNNER_PLAN_ROOT <- file.path(
  LOMM11_RUNNER_REPO_ROOT, "simulation_outputs", "lomm_1_1",
  "empirical_realism_ablation"
)
LOMM11_RUNNER_RESULT_ROOT <- file.path(
  LOMM11_RUNNER_REPO_ROOT, "simulation_outputs", "lomm_1_1",
  "empirical_realism_ablation_runs"
)
LOMM11_RUNNER_WORKER_DYNAMIC_EXPORTS <- c(
  "LOMM11_WORKER_CACHES", "LOMM11_WORKER_CONTEXT",
  "LOMM11_WORKER_CANDIDATES", "LOMM11_WORKER_EVALUATIONS"
)
LOMM11_RUNNER_WORKER_DEFERRED_EXPORTS <- "LOMM11_WORKER_SELECTED"

lomm11_runner_require_core_packages <- function() {
  for (package in c("codetools", "digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Package '", package, "' is required.", call. = FALSE)
    }
  }
}

lomm11_runner_require_core_packages()

LOMM11_RUNNER_DEPENDENCIES <- c(
  runner = LOMM11_RUNNER_PATH,
  config = file.path(LOMM11_RUNNER_DIRECTORY, "realism_ablation_config.json"),
  spec = file.path(LOMM11_RUNNER_DIRECTORY, "realism_ablation_spec.R"),
  generator = file.path(LOMM11_RUNNER_DIRECTORY, "realism_generator.R"),
  metrics = file.path(LOMM11_RUNNER_DIRECTORY, "realism_metrics.R"),
  planner = file.path(LOMM11_RUNNER_DIRECTORY, "build_realism_ablation_plan.R"),
  tests = file.path(LOMM11_RUNNER_DIRECTORY, "test_realism_ablation.R"),
  validator = file.path(
    LOMM11_RUNNER_DIRECTORY, "validate_realism_ablation_plan.R"
  ),
  revision_spec = file.path(
    LOMM11_RUNNER_REPO_ROOT, "scripts", "simulations", "revision_spec.R"
  ),
  compositional_math = file.path(
    LOMM11_RUNNER_REPO_ROOT, "scripts", "preprocessing", "compositional_math.R"
  ),
  compositional_views = file.path(
    LOMM11_RUNNER_REPO_ROOT, "scripts", "preprocessing", "compositional_views.R"
  ),
  compositional_bundle = file.path(
    LOMM11_RUNNER_REPO_ROOT, "scripts", "preprocessing",
    "empirical_compositional_bundle.R"
  ),
  compositional_validator = file.path(
    LOMM11_RUNNER_REPO_ROOT, "scripts", "preprocessing",
    "validate_published_bundle.R"
  )
)

lomm11_runner_missing_dependencies <- names(LOMM11_RUNNER_DEPENDENCIES)[
  !file.exists(LOMM11_RUNNER_DEPENDENCIES)
]
if (length(lomm11_runner_missing_dependencies) > 0L) {
  stop(
    "Missing LOMM 1.1 runner dependencies: ",
    paste(lomm11_runner_missing_dependencies, collapse = ", "), ".",
    call. = FALSE
  )
}

lomm11_runner_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

lomm11_runner_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

LOMM11_RUNNER_STARTUP_HASHES <- vapply(
  LOMM11_RUNNER_DEPENDENCIES, lomm11_runner_sha256, character(1)
)

# The plan validator discovers its own path through this shared binding.
LOMM11_DEPENDENCY_PATHS <- LOMM11_RUNNER_DEPENDENCIES
source(LOMM11_RUNNER_DEPENDENCIES[["compositional_math"]])
source(LOMM11_RUNNER_DEPENDENCIES[["compositional_views"]])
source(LOMM11_RUNNER_DEPENDENCIES[["compositional_bundle"]])
LOMM_BUILDER_PATH <- file.path(
  LOMM11_RUNNER_REPO_ROOT, "scripts", "preprocessing",
  "build_empirical_compositional_views.R"
)
source(LOMM11_RUNNER_DEPENDENCIES[["compositional_validator"]])
source(LOMM11_RUNNER_DEPENDENCIES[["spec"]])
source(LOMM11_RUNNER_DEPENDENCIES[["revision_spec"]])
source(LOMM11_RUNNER_DEPENDENCIES[["generator"]])
source(LOMM11_RUNNER_DEPENDENCIES[["metrics"]])
source(LOMM11_RUNNER_DEPENDENCIES[["validator"]])

lomm11_runner_assert_dependency_stability <- function() {
  observed <- vapply(
    LOMM11_RUNNER_DEPENDENCIES, lomm11_runner_sha256, character(1)
  )
  if (!identical(observed, LOMM11_RUNNER_STARTUP_HASHES)) {
    stop("LOMM 1.1 runner dependencies changed during execution.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_usage <- function() {
  paste(
    "Usage:",
    "  run_realism_ablation.R [--plan-dir DIR] [--output-dir DIR] [--dry-run]",
    "  run_realism_ablation.R --plan-dir DIR --execute [--resume]",
    "    --ack-long-running LOMM-1.1-REALISM-AUTHORIZED",
    "    --prospective-manifest-sha256 FULL_SHA256 [--workers N]",
    sep = "\n"
  )
}

lomm11_runner_parse_positive_integer <- function(value, label) {
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 1L ||
      !identical(as.character(parsed), value)) {
    stop(label, " must be a positive integer.", call. = FALSE)
  }
  parsed
}

lomm11_runner_parse_args <- function(args) {
  parsed <- list(
    plan_dir = NULL, output_dir = NULL, mode = "dry-run", resume = FALSE,
    acknowledgement = NULL, manifest_sha256 = NULL, workers = NULL,
    help = FALSE
  )
  value_flags <- c(
    "--plan-dir", "--output-dir", "--ack-long-running",
    "--prospective-manifest-sha256", "--workers"
  )
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (identical(token, "--help")) {
      parsed$help <- TRUE
      index <- index + 1L
      next
    }
    if (identical(token, "--dry-run")) {
      if (identical(parsed$mode, "execute")) {
        stop("--dry-run and --execute are mutually exclusive.", call. = FALSE)
      }
      parsed$mode <- "dry-run"
      index <- index + 1L
      next
    }
    if (identical(token, "--execute")) {
      if ("--dry-run" %in% args) {
        stop("--dry-run and --execute are mutually exclusive.", call. = FALSE)
      }
      parsed$mode <- "execute"
      index <- index + 1L
      next
    }
    if (identical(token, "--resume")) {
      parsed$resume <- TRUE
      index <- index + 1L
      next
    }
    if (token %in% value_flags) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        stop("Missing value after ", token, ".", call. = FALSE)
      }
      field <- switch(
        token,
        `--plan-dir` = "plan_dir",
        `--output-dir` = "output_dir",
        `--ack-long-running` = "acknowledgement",
        `--prospective-manifest-sha256` = "manifest_sha256",
        `--workers` = "workers"
      )
      parsed[[field]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    stop("Unknown argument: ", token, call. = FALSE)
  }
  if (isTRUE(parsed$help)) return(parsed)
  if (isTRUE(parsed$resume) && !identical(parsed$mode, "execute")) {
    stop("--resume also requires --execute.", call. = FALSE)
  }
  if (identical(parsed$mode, "dry-run") &&
      (!is.null(parsed$acknowledgement) || !is.null(parsed$manifest_sha256))) {
    stop("Execution acknowledgements are accepted only with --execute.", call. = FALSE)
  }
  if (!is.null(parsed$workers)) {
    parsed$workers <- lomm11_runner_parse_positive_integer(
      parsed$workers, "--workers"
    )
  }
  parsed
}

lomm11_runner_discover_plan <- function(plan_dir) {
  if (!is.null(plan_dir)) return(normalizePath(plan_dir, mustWork = TRUE))
  if (!dir.exists(LOMM11_RUNNER_PLAN_ROOT)) {
    stop("No materialized LOMM 1.1 ablation plan exists.", call. = FALSE)
  }
  candidates <- list.dirs(
    LOMM11_RUNNER_PLAN_ROOT, full.names = TRUE, recursive = FALSE
  )
  candidates <- candidates[
    grepl("^[0-9a-f]{12}$", basename(candidates)) &
      file.exists(file.path(candidates, "_PLANNED.json"))
  ]
  if (length(candidates) != 1L) {
    stop(
      "Specify --plan-dir; automatic discovery requires exactly one materialized plan.",
      call. = FALSE
    )
  }
  normalizePath(candidates[[1L]], mustWork = TRUE)
}

lomm11_runner_lexical_path <- function(path) {
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
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
  parts <- strsplit(combined, paste0(.Platform$file.sep, "+"))[[1L]]
  normalized <- character()
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) next
    if (identical(part, "..")) {
      if (length(normalized) > 0L) normalized <- normalized[-length(normalized)]
    } else {
      normalized <- c(normalized, part)
    }
  }
  paste0(.Platform$file.sep, paste(normalized, collapse = .Platform$file.sep))
}

lomm11_runner_is_within <- function(path, root) {
  identical(path, root) || startsWith(path, paste0(root, .Platform$file.sep))
}

lomm11_runner_result_directory <- function(output_dir, prefix, plan_dir) {
  expected <- lomm11_runner_lexical_path(file.path(LOMM11_RUNNER_RESULT_ROOT, prefix))
  chosen <- if (is.null(output_dir)) expected else lomm11_runner_lexical_path(output_dir)
  revision_root <- lomm11_runner_lexical_path(
    file.path(LOMM11_RUNNER_REPO_ROOT, "simulation_outputs", "revision_v2")
  )
  plan_path <- normalizePath(plan_dir, mustWork = TRUE)
  input_path <- lomm11_runner_lexical_path(file.path(
    LOMM11_RUNNER_REPO_ROOT, "simulation_outputs", "lomm_1_1",
    "empirical_compositional_preprocess", "0cdc11154d8a"
  ))
  if (!identical(chosen, expected)) {
    stop("The runner requires the hash-specific LOMM 1.1 result directory.", call. = FALSE)
  }
  forbidden <- lomm11_runner_is_within(chosen, revision_root) ||
    lomm11_runner_is_within(chosen, plan_path) ||
    lomm11_runner_is_within(chosen, input_path)
  if (forbidden) {
    stop("The result directory overlaps a protected artifact namespace.", call. = FALSE)
  }
  chosen
}

lomm11_runner_read_json <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

lomm11_runner_plan_snapshot <- function(plan_dir) {
  files <- lomm11_validator_expected_files()
  paths <- file.path(plan_dir, files)
  if (any(!file.exists(paths))) {
    stop("Materialized plan files are missing.", call. = FALSE)
  }
  setNames(vapply(paths, lomm11_runner_sha256, character(1)), files)
}

lomm11_runner_input_snapshot <- function(manifest) {
  directory <- normalizePath(manifest$input_bundle$resolved_path, mustWork = TRUE)
  expected <- unlist(manifest$input_bundle$files, use.names = TRUE)
  paths <- file.path(directory, names(expected))
  observed <- setNames(vapply(paths, lomm11_runner_sha256, character(1)), names(expected))
  if (!identical(unname(observed), unname(expected))) {
    stop("Certified input hashes differ from the materialized plan.", call. = FALSE)
  }
  observed
}

lomm11_runner_assert_artifact_stability <- function(context) {
  lomm11_runner_assert_dependency_stability()
  if (!identical(
    context$plan_snapshot, lomm11_runner_plan_snapshot(context$plan_dir)
  )) {
    stop("Materialized plan files changed during execution.", call. = FALSE)
  }
  if (!identical(
    context$input_snapshot, lomm11_runner_input_snapshot(context$manifest)
  )) {
    stop("Certified input files changed during execution.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_load_plan_tables <- function(plan_dir, manifest) {
  schema <- manifest$output_schema
  table_names <- c(
    "candidate_grid.csv", "fold_assignments.csv", "segment_manifest.csv",
    "training_plan.csv", "selection_plan.csv", "heldout_plan.csv",
    "heldout_evaluation_plan.csv"
  )
  setNames(lapply(table_names, function(name) {
    lomm11_validator_read_plan_table(file.path(plan_dir, name), schema[[name]])
  }), table_names)
}

lomm11_runner_locked_output_contract <- function() {
  list(
    fold_nuisance_manifest.csv = list(
      rows = 5L,
      primary_key = "fold",
      columns = c(
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
      primary_key = "unit_key",
      columns = c(
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
      primary_key = "selection_key",
      columns = c(
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
      primary_key = c("unit_key", "family", "metric"),
      columns = c(
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
      primary_key = c("unit_key", "family", "metric"),
      columns = c(
        unit_key = "character", panel_key = "character", fold = "integer",
        variant_id = "character", heldout_rep = "integer",
        analysis_view = "character", family = "character", metric = "character",
        median_diff_scaled = "double", wasserstein_scaled = "double",
        max_discrepancy = "double", empirical_n = "integer",
        synthetic_n = "integer", metric_scale = "double", status = "character",
        error_message = "character", seed_group = "character",
        config_hash = "character", input_hash = "character", panel_hash = "character"
      )
    ),
    clip_metric_contributions.parquet = list(
      rows = 21760000L,
      primary_key = "contribution_key",
      partition_columns = c("analysis_view", "fold", "variant_id", "family"),
      columns = c(
        contribution_key = "character", unit_key = "character",
        panel_key = "character", fold = "integer", variant_id = "character",
        heldout_rep = "integer", analysis_view = "character", clip = "character",
        family = "character", metric = "character", unit = "character",
        empirical_value = "double", synthetic_value = "double",
        training_scale = "double", status = "character",
        error_message = "character", seed_group = "character",
        config_hash = "character", input_hash = "character", panel_hash = "character"
      )
    )
  )
}

lomm11_runner_validate_output_contract <- function(manifest) {
  expected <- lomm11_runner_locked_output_contract()
  observed <- manifest$future_output_contract
  required_names <- c(
    names(expected), "run_manifest.json", "_SUCCESS.json",
    "paired_ablation_effects.csv"
  )
  if (!setequal(names(observed), required_names)) {
    stop("Future output artifact names differ from the locked runner contract.", call. = FALSE)
  }
  for (name in names(expected)) {
    target <- expected[[name]]
    contract <- observed[[name]]
    columns <- unlist(contract$columns, use.names = TRUE)
    keys <- unlist(contract$primary_key, use.names = FALSE)
    partitions <- unlist(contract$partition_columns, use.names = FALSE)
    partition_match <- if (is.null(target$partition_columns)) {
      length(partitions) == 0L
    } else {
      identical(partitions, target$partition_columns)
    }
    if (!identical(contract$rows, target$rows) ||
        !identical(keys, target$primary_key) ||
        !identical(columns, target$columns) || !partition_match) {
      stop("Future output contract differs for ", name, ".", call. = FALSE)
    }
  }
  expected_counts <- c(
    checkpoint_units = 11240L, training_metric_discrepancies = 51840L,
    heldout_metric_discrepancies = 128000L,
    clip_metric_contributions = 21760000L
  )
  observed_counts <- unlist(manifest$expected_counts[names(expected_counts)])
  if (!identical(as.integer(observed_counts), as.integer(expected_counts))) {
    stop("Manifest output counts differ from the locked execution contract.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_load_context <- function(args) {
  plan_dir <- lomm11_runner_discover_plan(args$plan_dir)
  lomm11_validate_realism_ablation_plan(plan_dir)
  manifest_path <- file.path(plan_dir, "prospective_manifest.json")
  manifest <- lomm11_runner_read_json(manifest_path)
  manifest_sha256 <- lomm11_runner_sha256(manifest_path)
  config <- lomm11_read_config(file.path(plan_dir, "ablation_config.json"))
  lomm11_runner_validate_output_contract(manifest)
  plan_tables <- lomm11_runner_load_plan_tables(plan_dir, manifest)
  rebuilt <- list(
    tables = plan_tables,
    schemas = manifest$output_schema,
    target_contract = manifest$scientific_contract,
    future_output_contract = manifest$future_output_contract
  )
  lomm11_validate_plan(rebuilt, config)
  output_dir <- lomm11_runner_result_directory(
    args$output_dir, manifest$config_prefix, plan_dir
  )
  input_snapshot <- lomm11_runner_input_snapshot(manifest)
  context <- list(
    args = args, plan_dir = plan_dir, output_dir = output_dir,
    manifest = manifest, manifest_sha256 = manifest_sha256, config = config,
    plan = plan_tables, plan_snapshot = lomm11_runner_plan_snapshot(plan_dir),
    input_snapshot = input_snapshot,
    input_hash = lomm11_runner_object_sha256(
      manifest$configuration_identity$logical_input
    )
  )
  lomm11_runner_assert_artifact_stability(context)
  context
}

lomm11_runner_validate_execution_authority <- function(context) {
  args <- context$args
  if (!identical(args$acknowledgement, LOMM11_RUNNER_TOKEN) ||
      !identical(args$acknowledgement, context$config$future_execution$authorization_token)) {
    stop("Execution requires the literal long-running acknowledgement token.", call. = FALSE)
  }
  if (is.null(args$manifest_sha256) ||
      !grepl("^[0-9a-f]{64}$", args$manifest_sha256) ||
      !identical(args$manifest_sha256, context$manifest_sha256)) {
    stop("Execution requires the exact full prospective-manifest SHA-256.", call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required only for execute mode.", call. = FALSE)
  }
  workers <- if (is.null(args$workers)) {
    min(4L, as.integer(context$config$future_execution$worker_limit))
  } else {
    as.integer(args$workers)
  }
  if (workers > as.integer(context$config$future_execution$worker_limit)) {
    stop("--workers exceeds the locked worker limit.", call. = FALSE)
  }
  workers
}

lomm11_runner_schema_frame <- function(contract, rows = 0L) {
  columns <- contract$columns
  frame <- lapply(columns, function(type) {
    switch(
      type,
      character = rep(NA_character_, rows),
      integer = rep(NA_integer_, rows),
      double = rep(NA_real_, rows),
      logical = rep(NA, rows),
      stop("Unsupported result-column type: ", type, call. = FALSE)
    )
  })
  as.data.frame(frame, stringsAsFactors = FALSE, check.names = FALSE)
}

lomm11_runner_coerce_frame <- function(frame, contract, label) {
  expected <- names(contract$columns)
  if (!is.data.frame(frame) || !setequal(names(frame), expected)) {
    stop(label, " columns differ from the locked schema.", call. = FALSE)
  }
  output <- frame[, expected, drop = FALSE]
  for (name in expected) {
    output[[name]] <- switch(
      contract$columns[[name]],
      character = as.character(output[[name]]),
      integer = as.integer(output[[name]]),
      double = as.numeric(output[[name]]),
      logical = as.logical(output[[name]]),
      stop("Unsupported result-column type.", call. = FALSE)
    )
  }
  keys <- contract$primary_key
  key <- do.call(paste, c(output[keys], sep = "\r"))
  if (nrow(output) > 0L && (anyNA(output[keys]) || anyDuplicated(key))) {
    stop(label, " has missing or duplicate primary keys.", call. = FALSE)
  }
  output
}

lomm11_runner_atomic_csv <- function(frame, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(
    dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid())
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Failed to publish atomic CSV: ", path, call. = FALSE)
  }
  invisible(path)
}

lomm11_runner_atomic_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(
    dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid())
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
  )
  if (!file.rename(temporary, path)) {
    stop("Failed to publish atomic JSON: ", path, call. = FALSE)
  }
  invisible(path)
}

lomm11_runner_read_csv_contract <- function(path, contract, label) {
  classes <- unname(c(
    character = "character", integer = "integer", double = "numeric",
    logical = "logical"
  )[unlist(contract$columns, use.names = FALSE)])
  frame <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = "",
    colClasses = classes
  )
  lomm11_runner_coerce_frame(frame, contract, label)
}

lomm11_runner_tree_sha256 <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files <- files[!file.info(files)$isdir]
  relative <- substring(files, nchar(normalizePath(path, mustWork = TRUE)) + 2L)
  order_index <- order(relative)
  records <- lapply(order_index, function(index) list(
    path = relative[[index]], sha256 = lomm11_runner_sha256(files[[index]])
  ))
  lomm11_runner_object_sha256(records)
}

lomm11_runner_partition_value <- function(value) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !grepl("^[A-Za-z0-9_.-]+$", value)) {
    stop("Unsafe Parquet partition value.", call. = FALSE)
  }
  value
}

lomm11_runner_parquet_path <- function(root, unit_key, partitions = list()) {
  directory <- root
  if (length(partitions) > 0L) {
    for (name in names(partitions)) {
      directory <- file.path(
        directory,
        paste0(name, "-", lomm11_runner_partition_value(partitions[[name]]))
      )
    }
  }
  filename <- paste0(
    "part-", substr(lomm11_runner_object_sha256(unit_key), 1L, 32L), ".parquet"
  )
  file.path(directory, filename)
}

lomm11_runner_atomic_parquet <- function(frame, path, partition_columns = character()) {
  if (!all(partition_columns %in% names(frame))) {
    stop("Parquet partition columns are absent from the unit frame.", call. = FALSE)
  }
  stored <- frame
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) {
    existing <- as.data.frame(arrow::read_parquet(path))
    if (nrow(existing) != nrow(stored) || !identical(names(existing), names(stored))) {
      stop("Existing Parquet unit artifact is incompatible: ", path, call. = FALSE)
    }
    key_columns <- if ("contribution_key" %in% names(stored)) {
      "contribution_key"
    } else {
      c("unit_key", "family", "metric")
    }
    provenance_columns <- intersect(
      c(
        key_columns, "status", "config_hash", "input_hash", "panel_hash",
        "seed_group"
      ),
      names(stored)
    )
    key <- function(value) do.call(paste, c(value[key_columns], sep = "\r"))
    existing_order <- order(key(existing))
    stored_order <- order(key(stored))
    normalize <- function(value) {
      value <- value[, provenance_columns, drop = FALSE]
      value[] <- lapply(value, as.character)
      rownames(value) <- NULL
      value
    }
    if (!identical(
      normalize(existing[existing_order, , drop = FALSE]),
      normalize(stored[stored_order, , drop = FALSE])
    )) {
      stop("Existing Parquet unit keys or provenance differ: ", path, call. = FALSE)
    }
    return(invisible(path))
  }
  temporary <- file.path(
    dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid())
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  arrow::write_parquet(stored, temporary, compression = "zstd")
  if (!file.rename(temporary, path)) {
    stop("Failed to publish atomic Parquet unit: ", path, call. = FALSE)
  }
  invisible(path)
}

lomm11_runner_status_from_error <- function(condition) {
  message <- conditionMessage(condition)
  status <- if (grepl("elapsed time limit|time limit", message, ignore.case = TRUE)) {
    "timeout"
  } else {
    "error"
  }
  list(status = status, error_message = substr(message, 1L, 2000L))
}

lomm11_runner_with_timeout <- function(seconds, callback) {
  start <- proc.time()[["elapsed"]]
  output <- tryCatch({
    setTimeLimit(elapsed = seconds, transient = TRUE)
    value <- callback()
    list(ok = TRUE, value = value, status = "ok", error_message = NA_character_)
  }, error = function(condition) {
    failure <- lomm11_runner_status_from_error(condition)
    list(
      ok = FALSE, value = NULL, status = failure$status,
      error_message = failure$error_message
    )
  }, finally = {
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  output$runtime_sec <- as.numeric(proc.time()[["elapsed"]] - start)
  output
}

lomm11_runner_lock_owner <- function(context, pid = Sys.getpid(),
                                     host = Sys.info()[["nodename"]]) {
  list(
    schema_version = 1L,
    pid = as.integer(pid),
    host = unname(as.character(host)),
    config_hash = context$manifest$config_hash,
    prospective_manifest_sha256 = context$manifest_sha256,
    output_directory = context$output_dir,
    acquired_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

lomm11_runner_validate_lock_owner <- function(owner, context) {
  fields <- c(
    "schema_version", "pid", "host", "config_hash",
    "prospective_manifest_sha256", "output_directory", "acquired_at"
  )
  valid <- is.list(owner) && setequal(names(owner), fields) &&
    identical(owner$schema_version, 1L) && is.integer(owner$pid) &&
    length(owner$pid) == 1L && !is.na(owner$pid) && owner$pid > 0L &&
    is.character(owner$host) && length(owner$host) == 1L && nzchar(owner$host) &&
    identical(owner$config_hash, context$manifest$config_hash) &&
    identical(
      owner$prospective_manifest_sha256, context$manifest_sha256
    ) && identical(owner$output_directory, context$output_dir) &&
    is.character(owner$acquired_at) && length(owner$acquired_at) == 1L &&
    nzchar(owner$acquired_at)
  if (!valid) {
    stop("Run lock owner record is missing or differs from this plan.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_acquire_lock <- function(context) {
  output_dir <- context$output_dir
  lock <- paste0(output_dir, ".lock")
  dir.create(dirname(lock), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(lock)) {
    record_path <- file.path(lock, "owner.json")
    if (!file.exists(record_path)) {
      stop("Run lock has no owner record and cannot be reclaimed.", call. = FALSE)
    }
    owner <- lomm11_runner_read_json(record_path)
    lomm11_runner_validate_lock_owner(owner, context)
    local_host <- unname(Sys.info()[["nodename"]])
    if (!identical(owner$host, local_host)) {
      stop("A foreign-host run lock cannot be reclaimed.", call. = FALSE)
    }
    alive <- file.exists(file.path("/proc", as.character(owner$pid)))
    if (!isTRUE(context$args$resume) || alive) {
      stop("A live or non-resume run lock cannot be reclaimed.", call. = FALSE)
    }
    unlink(lock, recursive = TRUE)
  }
  if (!dir.create(lock, recursive = FALSE, showWarnings = FALSE)) {
    stop("Failed to acquire the LOMM 1.1 run lock.", call. = FALSE)
  }
  lomm11_runner_atomic_json(
    lomm11_runner_lock_owner(context), file.path(lock, "owner.json")
  )
  lock
}

lomm11_runner_release_lock <- function(lock, context) {
  if (dir.exists(lock)) {
    record <- lomm11_runner_read_json(file.path(lock, "owner.json"))
    lomm11_runner_validate_lock_owner(record, context)
    if (!identical(record$pid, as.integer(Sys.getpid())) ||
        !identical(record$host, unname(Sys.info()[["nodename"]]))) {
      stop("Refusing to release a run lock owned by another process.", call. = FALSE)
    }
    unlink(lock, recursive = TRUE)
  }
  invisible(TRUE)
}

lomm11_runner_set_thread_limits <- function() {
  variables <- c(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  do.call(Sys.setenv, as.list(variables))
  invisible(TRUE)
}

lomm11_runner_basis <- function(input_tables) {
  table <- input_tables[["ilr_basis.csv"]]
  coordinate_columns <- sprintf("ilr_%02d", 1:10)
  basis <- as.matrix(table[coordinate_columns])
  storage.mode(basis) <- "double"
  rownames(basis) <- as.character(table$channel)
  colnames(basis) <- coordinate_columns
  lomm_validate_ilr_basis(basis, rownames(basis))
  basis
}

lomm11_runner_filter_view <- function(table, analysis_view, clips) {
  output <- table[
    table$analysis_variant == analysis_view & as.character(table$clip) %in% clips,
    , drop = FALSE
  ]
  output[order(output$clip, output$frame_index), , drop = FALSE]
}

lomm11_runner_empirical_contributions <- function(input_tables, analysis_view, clips) {
  probability <- lomm11_runner_filter_view(
    input_tables[["probability_view.csv"]], analysis_view, clips
  )
  clr <- lomm11_runner_filter_view(
    input_tables[["clr_view.csv"]], analysis_view, clips
  )
  ilr <- lomm11_runner_filter_view(
    input_tables[["ilr_view.csv"]], analysis_view, clips
  )
  lomm11_metric_contributions(probability, clr, ilr)
}

lomm11_runner_generation_plan <- function(frame_ledger, clips) {
  selected <- frame_ledger[
    as.character(frame_ledger$clip) %in% clips &
      as.logical(frame_ledger$sensitivity_included),
    , drop = FALSE
  ]
  lomm11_prepare_generation_frame_plan(selected)
}

lomm11_runner_fold_cache <- function(fold, context, input_tables, basis, view_index) {
  assignments <- context$plan$fold_assignments.csv
  training_clips <- as.character(assignments$clip[assignments$fold != fold])
  heldout_clips <- as.character(assignments$clip[assignments$fold == fold])
  primary_training <- lomm11_runner_empirical_contributions(
    input_tables, "complete_case", training_clips
  )
  primary_rows <- lomm11_runner_filter_view(
    input_tables[["probability_view.csv"]], "complete_case", training_clips
  )
  training_plan <- context$plan$training_plan.csv[
    context$plan$training_plan.csv$fold == fold, , drop = FALSE
  ]
  expected_rows <- unique(training_plan$expected_training_primary_frames)
  expected_segments <- unique(training_plan$expected_training_primary_segments)
  observed_segments <- length(unique(primary_rows$segment_id))
  if (length(expected_rows) != 1L || length(expected_segments) != 1L ||
      nrow(primary_rows) != expected_rows || observed_segments != expected_segments) {
    stop("Fold training rows or segments differ from the materialized plan.", call. = FALSE)
  }
  geometry <- lomm11_fit_training_geometry(
    input_tables[["ilr_view.csv"]], training_clips,
    n_directions = as.integer(context$config$measurement$n_latent_directions),
    psd_floor = as.numeric(context$config$measurement$between_clip_psd_floor)
  )
  scales <- lomm11_fit_metric_scales(primary_training, training_clips)
  heldout <- setNames(lapply(c("complete_case", "ilr_gap2"), function(view) {
    lomm11_runner_empirical_contributions(input_tables, view, heldout_clips)
  }), c("complete_case", "ilr_gap2"))
  list(
    fold = as.integer(fold), training_clips = training_clips,
    heldout_clips = heldout_clips, geometry = geometry, scales = scales,
    empirical_training = primary_training, empirical_heldout = heldout,
    primary_rows = as.integer(nrow(primary_rows)),
    primary_segments = as.integer(observed_segments),
    training_generation_plan = lomm11_runner_generation_plan(
      input_tables[["frame_ledger.csv"]], training_clips
    ),
    heldout_generation_plan = lomm11_runner_generation_plan(
      input_tables[["frame_ledger.csv"]], heldout_clips
    ),
    training_view_index = view_index[view_index$clip %in% training_clips, , drop = FALSE],
    heldout_view_index = view_index[view_index$clip %in% heldout_clips, , drop = FALSE],
    basis = basis,
    nuisance_hash = lomm11_runner_object_sha256(list(
      center = geometry$center, directions = geometry$directions,
      relative_scales = geometry$relative_scales,
      between_clip_covariance = geometry$between_clip_covariance,
      metric_scales = scales
    ))
  )
}

lomm11_runner_build_fold_caches <- function(context) {
  input_directory <- normalizePath(
    context$manifest$input_bundle$resolved_path, mustWork = TRUE
  )
  input_tables <- lomm_validation_read_tables(input_directory)
  basis <- lomm11_runner_basis(input_tables)
  view_index <- lomm11_view_index_from_ledger(input_tables[["frame_ledger.csv"]])
  caches <- lapply(seq_len(5L), function(fold) {
    lomm11_runner_fold_cache(fold, context, input_tables, basis, view_index)
  })
  names(caches) <- as.character(seq_len(5L))
  caches
}

lomm11_runner_generation_arguments <- function(candidate, cache, row, context) {
  measurement <- context$config$measurement
  list(
    geometry = cache$geometry,
    ilr_basis = cache$basis,
    period_frames = as.integer(candidate$period_frames[[1L]]),
    damping_ratio = as.numeric(candidate$damping_ratio[[1L]]),
    kappa = as.numeric(candidate$kappa[[1L]]),
    variant = candidate[, c("B", "S", "G"), drop = FALSE],
    master_seed = as.integer(row$seed_base[[1L]]),
    pairing_key = as.character(row$seed_group[[1L]]),
    dt = as.numeric(context$config$dynamics$dt),
    residual_sd = as.numeric(measurement$residual_sd),
    tau = as.numeric(measurement$tau),
    common_logit_shift = as.numeric(measurement$common_logit_shift),
    gate_low = as.numeric(measurement$gate_low),
    gate_high = as.numeric(measurement$gate_high),
    gate_initial_high_probability = as.numeric(
      measurement$gate_initial_high_probability
    ),
    gate_transition_observations = as.integer(
      measurement$gate_transition_observations
    )
  )
}

lomm11_runner_generate <- function(frame_plan, candidate, cache, row, context) {
  arguments <- c(
    list(frame_plan = frame_plan),
    lomm11_runner_generation_arguments(candidate, cache, row, context)
  )
  do.call(lomm11_generate_panel, arguments)
}

lomm11_runner_panel_hash <- function(generated) {
  digest::digest(generated$panel, algo = "sha256", serialize = TRUE)
}

lomm11_runner_metric_rows <- function(discrepancies, metadata, contract) {
  frame <- data.frame(
    unit_key = metadata$unit_key,
    panel_key = metadata$panel_key,
    fold = metadata$fold,
    variant_id = metadata$variant_id,
    candidate_id = metadata$candidate_id,
    train_rep = metadata$train_rep,
    heldout_rep = metadata$heldout_rep,
    analysis_view = metadata$analysis_view,
    family = discrepancies$family,
    metric = discrepancies$metric,
    median_diff_scaled = discrepancies$signed_median_difference_scaled,
    wasserstein_scaled = discrepancies$wasserstein1_scaled,
    max_discrepancy = discrepancies$discrepancy,
    empirical_n = discrepancies$n_empirical,
    synthetic_n = discrepancies$n_generated,
    metric_scale = discrepancies$scale,
    status = metadata$status,
    error_message = metadata$error_message,
    seed_group = metadata$seed_group,
    config_hash = metadata$config_hash,
    input_hash = metadata$input_hash,
    panel_hash = metadata$panel_hash,
    stringsAsFactors = FALSE
  )
  frame <- frame[, intersect(names(frame), names(contract$columns)), drop = FALSE]
  lomm11_runner_coerce_frame(frame, contract, "Metric discrepancy rows")
}

lomm11_runner_error_discrepancies <- function() {
  groups <- lomm11_metric_groups()
  data.frame(
    family = groups$family,
    metric = groups$metric,
    scale = NA_real_,
    n_empirical = 0L,
    n_generated = 0L,
    empirical_median = NA_real_,
    generated_median = NA_real_,
    signed_median_difference_scaled = NA_real_,
    abs_median_difference_scaled = NA_real_,
    wasserstein1_scaled = NA_real_,
    discrepancy = NA_real_,
    stringsAsFactors = FALSE
  )
}

lomm11_runner_loss_values <- function(discrepancies, config) {
  weights <- unlist(config$metrics$family_weights, use.names = TRUE)
  loss <- lomm11_selection_loss(discrepancies, weights)
  family_mean <- loss$family_mean
  list(
    probability_loss = if (is.null(family_mean)) Inf else family_mean[["probability"]],
    logratio_loss = if (is.null(family_mean)) Inf else family_mean[["logratio"]],
    selection_loss = loss$selection_loss,
    max_metric_discrepancy = loss$max_metric_discrepancy
  )
}

lomm11_runner_checkpoint_row <- function(metadata, losses, runtime_sec, contract) {
  frame <- data.frame(
    unit_key = metadata$unit_key,
    panel_key = metadata$panel_key,
    stage = metadata$stage,
    analysis_view = metadata$analysis_view,
    fold = metadata$fold,
    variant_id = metadata$variant_id,
    candidate_id = metadata$candidate_id,
    replicate = metadata$replicate,
    status = metadata$status,
    error_message = metadata$error_message,
    probability_loss = losses$probability_loss,
    logratio_loss = losses$logratio_loss,
    selection_loss = losses$selection_loss,
    max_metric_discrepancy = losses$max_metric_discrepancy,
    seed_group = metadata$seed_group,
    seed_base = metadata$seed_base,
    runtime_sec = runtime_sec,
    config_hash = metadata$config_hash,
    input_hash = metadata$input_hash,
    panel_hash = metadata$panel_hash,
    stringsAsFactors = FALSE
  )
  lomm11_runner_coerce_frame(frame, contract, "Checkpoint row")
}

lomm11_runner_training_metadata <- function(row, context, status, error_message,
                                            panel_hash) {
  list(
    unit_key = as.character(row$unit_key[[1L]]),
    panel_key = as.character(row$panel_key[[1L]]),
    stage = "training",
    analysis_view = "complete_case",
    fold = as.integer(row$fold[[1L]]),
    variant_id = as.character(row$variant_id[[1L]]),
    candidate_id = as.character(row$candidate_id[[1L]]),
    replicate = as.integer(row$training_rep[[1L]]),
    train_rep = as.integer(row$training_rep[[1L]]),
    heldout_rep = NA_integer_,
    status = status,
    error_message = error_message,
    seed_group = as.character(row$seed_group[[1L]]),
    seed_base = as.integer(row$seed_base[[1L]]),
    config_hash = context$manifest$config_hash,
    input_hash = context$input_hash,
    panel_hash = panel_hash
  )
}

lomm11_runner_training_unit <- function(row, candidate_grid, caches, context) {
  checkpoint_contract <- lomm11_runner_locked_output_contract()$checkpoint_results.csv
  metric_contract <- lomm11_runner_locked_output_contract()$training_metric_discrepancies.parquet
  candidate <- candidate_grid[candidate_grid$candidate_id == row$candidate_id[[1L]], , drop = FALSE]
  cache <- caches[[as.character(row$fold[[1L]])]]
  timeout <- as.numeric(context$config$future_execution$panel_timeout_seconds)
  attempt <- lomm11_runner_with_timeout(timeout, function() {
    generated <- lomm11_runner_generate(
      cache$training_generation_plan, candidate, cache, row, context
    )
    generated_view <- lomm11_subset_panel_view(
      generated, cache$training_view_index, "complete_case"
    )
    generated_contributions <- lomm11_metric_contributions(
      generated_view, generated_view, generated_view
    )
    discrepancies <- lomm11_metric_discrepancies(
      cache$empirical_training, generated_contributions, cache$scales
    )
    list(
      discrepancies = discrepancies,
      panel_hash = lomm11_runner_panel_hash(generated)
    )
  })
  if (attempt$ok) {
    metadata <- lomm11_runner_training_metadata(
      row, context, "ok", NA_character_, attempt$value$panel_hash
    )
    discrepancies <- attempt$value$discrepancies
    losses <- lomm11_runner_loss_values(discrepancies, context$config)
  } else {
    metadata <- lomm11_runner_training_metadata(
      row, context, attempt$status, attempt$error_message, NA_character_
    )
    discrepancies <- lomm11_runner_error_discrepancies()
    losses <- list(
      probability_loss = Inf, logratio_loss = Inf, selection_loss = Inf,
      max_metric_discrepancy = Inf
    )
  }
  list(
    checkpoint = lomm11_runner_checkpoint_row(
      metadata, losses, attempt$runtime_sec, checkpoint_contract
    ),
    metrics = lomm11_runner_metric_rows(discrepancies, metadata, metric_contract)
  )
}

lomm11_runner_contribution_rows <- function(empirical, generated, scales,
                                            metadata, contract) {
  if (is.null(generated)) {
    paired <- empirical[, c(
      "contribution_key", "analysis_variant", "clip", "family", "unit",
      "metric", "value"
    ), drop = FALSE]
    paired$empirical_value <- paired$value
    paired$generated_value <- NA_real_
    paired$scale <- mapply(
      function(family, metric) lomm11_scale_lookup(scales, family, metric),
      paired$family, paired$metric
    )
  } else {
    paired <- lomm11_pair_metric_contributions(empirical, generated, scales)
  }
  frame <- data.frame(
    contribution_key = paste(metadata$unit_key, paired$contribution_key, sep = "|"),
    unit_key = metadata$unit_key,
    panel_key = metadata$panel_key,
    fold = metadata$fold,
    variant_id = metadata$variant_id,
    heldout_rep = metadata$heldout_rep,
    analysis_view = metadata$analysis_view,
    clip = paired$clip,
    family = paired$family,
    metric = paired$metric,
    unit = paired$unit,
    empirical_value = paired$empirical_value,
    synthetic_value = paired$generated_value,
    training_scale = paired$scale,
    status = metadata$status,
    error_message = metadata$error_message,
    seed_group = metadata$seed_group,
    config_hash = metadata$config_hash,
    input_hash = metadata$input_hash,
    panel_hash = metadata$panel_hash,
    stringsAsFactors = FALSE
  )
  lomm11_runner_coerce_frame(frame, contract, "Held-out contribution rows")
}

lomm11_runner_heldout_metadata <- function(row, evaluation, candidate, context,
                                           status, error_message, panel_hash) {
  list(
    unit_key = as.character(evaluation$unit_key[[1L]]),
    panel_key = as.character(row$panel_key[[1L]]),
    stage = "heldout",
    analysis_view = as.character(evaluation$analysis_view[[1L]]),
    fold = as.integer(row$fold[[1L]]),
    variant_id = as.character(row$variant_id[[1L]]),
    candidate_id = as.character(candidate$candidate_id[[1L]]),
    replicate = as.integer(row$heldout_rep[[1L]]),
    train_rep = NA_integer_,
    heldout_rep = as.integer(row$heldout_rep[[1L]]),
    status = status,
    error_message = error_message,
    seed_group = as.character(row$seed_group[[1L]]),
    seed_base = as.integer(row$seed_base[[1L]]),
    config_hash = context$manifest$config_hash,
    input_hash = context$input_hash,
    panel_hash = panel_hash
  )
}

lomm11_runner_one_heldout_view <- function(view, generated_views, cache, row,
                                           evaluation, candidate, context,
                                           panel_hash) {
  metric_contract <- lomm11_runner_locked_output_contract()$heldout_metric_discrepancies.csv
  contribution_contract <- lomm11_runner_locked_output_contract()$clip_metric_contributions.parquet
  attempt <- tryCatch({
    generated <- lomm11_metric_contributions(
      generated_views[[view]], generated_views[[view]], generated_views[[view]]
    )
    discrepancies <- lomm11_metric_discrepancies(
      cache$empirical_heldout[[view]], generated, cache$scales
    )
    list(ok = TRUE, generated = generated, discrepancies = discrepancies)
  }, error = function(condition) {
    failure <- lomm11_runner_status_from_error(condition)
    list(
      ok = FALSE, generated = NULL,
      discrepancies = lomm11_runner_error_discrepancies(),
      status = failure$status, error_message = failure$error_message
    )
  })
  status <- if (attempt$ok) "ok" else attempt$status
  error_message <- if (attempt$ok) NA_character_ else attempt$error_message
  metadata <- lomm11_runner_heldout_metadata(
    row, evaluation, candidate, context, status, error_message, panel_hash
  )
  losses <- if (attempt$ok) {
    lomm11_runner_loss_values(attempt$discrepancies, context$config)
  } else {
    list(
      probability_loss = Inf, logratio_loss = Inf, selection_loss = Inf,
      max_metric_discrepancy = Inf
    )
  }
  list(
    metadata = metadata,
    losses = losses,
    discrepancies = lomm11_runner_metric_rows(
      attempt$discrepancies, metadata, metric_contract
    ),
    contributions = lomm11_runner_contribution_rows(
      cache$empirical_heldout[[view]], attempt$generated, cache$scales,
      metadata, contribution_contract
    )
  )
}

lomm11_runner_heldout_failure <- function(row, evaluations, candidate, cache,
                                          context, attempt) {
  metric_contract <- lomm11_runner_locked_output_contract()$heldout_metric_discrepancies.csv
  contribution_contract <- lomm11_runner_locked_output_contract()$clip_metric_contributions.parquet
  lapply(seq_len(nrow(evaluations)), function(index) {
    evaluation <- evaluations[index, , drop = FALSE]
    view <- as.character(evaluation$analysis_view[[1L]])
    metadata <- lomm11_runner_heldout_metadata(
      row, evaluation, candidate, context, attempt$status,
      attempt$error_message, NA_character_
    )
    list(
      metadata = metadata,
      losses = list(
        probability_loss = Inf, logratio_loss = Inf, selection_loss = Inf,
        max_metric_discrepancy = Inf
      ),
      discrepancies = lomm11_runner_metric_rows(
        lomm11_runner_error_discrepancies(), metadata, metric_contract
      ),
      contributions = lomm11_runner_contribution_rows(
        cache$empirical_heldout[[view]], NULL, cache$scales, metadata,
        contribution_contract
      )
    )
  })
}

lomm11_runner_heldout_panel <- function(row, selected, evaluations, caches, context) {
  cache <- caches[[as.character(row$fold[[1L]])]]
  candidate <- selected[selected$selection_key == row$selection_key[[1L]], , drop = FALSE]
  source_candidate <- context$plan$candidate_grid.csv[
    context$plan$candidate_grid.csv$candidate_id == candidate$candidate_id[[1L]],
    , drop = FALSE
  ]
  timeout <- as.numeric(context$config$future_execution$panel_timeout_seconds)
  attempt <- lomm11_runner_with_timeout(timeout, function() {
    generated <- lomm11_runner_generate(
      cache$heldout_generation_plan, source_candidate, cache, row, context
    )
    views <- lomm11_subset_panel_views(
      generated, cache$heldout_view_index, c("complete_case", "ilr_gap2")
    )
    panel_hash <- lomm11_runner_panel_hash(generated)
    results <- lapply(seq_len(nrow(evaluations)), function(index) {
      evaluation <- evaluations[index, , drop = FALSE]
      view <- as.character(evaluation$analysis_view[[1L]])
      lomm11_runner_one_heldout_view(
        view, views, cache, row, evaluation, candidate, context, panel_hash
      )
    })
    list(results = results, panel_hash = panel_hash)
  })
  results <- if (attempt$ok) {
    attempt$value$results
  } else {
    lomm11_runner_heldout_failure(
      row, evaluations, candidate, cache, context, attempt
    )
  }
  checkpoint_contract <- lomm11_runner_locked_output_contract()$checkpoint_results.csv
  for (index in seq_along(results)) {
    results[[index]]$checkpoint <- lomm11_runner_checkpoint_row(
      results[[index]]$metadata, results[[index]]$losses,
      attempt$runtime_sec, checkpoint_contract
    )
  }
  list(results = results, panel_key = as.character(row$panel_key[[1L]]))
}

lomm11_runner_nuisance_payload <- function(cache, context) {
  geometry <- cache$geometry
  list(
    schema_version = 1L,
    fold = cache$fold,
    training_clips = sort(cache$training_clips),
    n_training_clips = as.integer(length(cache$training_clips)),
    primary_rows = cache$primary_rows,
    primary_segments = cache$primary_segments,
    coordinate_columns = geometry$coordinate_columns,
    center = geometry$center,
    directions = geometry$directions,
    direction_eigenvalues = geometry$direction_eigenvalues,
    relative_scales = geometry$relative_scales,
    loadings = geometry$loadings,
    between_clip_covariance = geometry$between_clip_covariance,
    metric_scales = cache$scales,
    config_hash = context$manifest$config_hash,
    input_hash = context$input_hash
  )
}

lomm11_runner_atomic_rds <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) {
    existing <- readRDS(path)
    if (!identical(
      lomm11_runner_object_sha256(existing), lomm11_runner_object_sha256(value)
    )) {
      stop("Existing fold nuisance RDS differs from the rebuilt artifact.", call. = FALSE)
    }
    return(invisible(path))
  }
  temporary <- file.path(
    dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid())
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3L, compress = "xz")
  if (!file.rename(temporary, path)) {
    stop("Failed to publish fold nuisance RDS.", call. = FALSE)
  }
  invisible(path)
}

lomm11_runner_write_nuisance <- function(caches, context) {
  contract <- lomm11_runner_locked_output_contract()$fold_nuisance_manifest.csv
  rows <- lapply(caches, function(cache) {
    payload <- lomm11_runner_nuisance_payload(cache, context)
    relative <- file.path("fold_nuisance", sprintf("fold_%02d.rds", cache$fold))
    path <- file.path(context$output_dir, relative)
    lomm11_runner_atomic_rds(payload, path)
    data.frame(
      fold = cache$fold,
      n_training_clips = length(cache$training_clips),
      training_clips_sha256 = lomm11_runner_object_sha256(sort(cache$training_clips)),
      primary_rows = cache$primary_rows,
      primary_segments = cache$primary_segments,
      center_sha256 = lomm11_runner_object_sha256(cache$geometry$center),
      directions_sha256 = lomm11_runner_object_sha256(cache$geometry$directions),
      loadings_sha256 = lomm11_runner_object_sha256(cache$geometry$loadings),
      between_clip_covariance_sha256 = lomm11_runner_object_sha256(
        cache$geometry$between_clip_covariance
      ),
      metric_scales_sha256 = lomm11_runner_object_sha256(cache$scales),
      nuisance_rds_path = relative,
      nuisance_rds_sha256 = lomm11_runner_sha256(path),
      config_hash = context$manifest$config_hash,
      input_hash = context$input_hash,
      stringsAsFactors = FALSE
    )
  })
  manifest <- lomm11_runner_coerce_frame(
    do.call(rbind, rows), contract, "Fold nuisance manifest"
  )
  path <- file.path(context$output_dir, "fold_nuisance_manifest.csv")
  if (file.exists(path)) {
    existing <- lomm11_runner_read_csv_contract(path, contract, "Fold nuisance manifest")
    if (!identical(
      lomm11_runner_object_sha256(existing), lomm11_runner_object_sha256(manifest)
    )) {
      stop("Existing fold nuisance manifest differs from the rebuilt artifact.", call. = FALSE)
    }
  } else {
    lomm11_runner_atomic_csv(manifest, path)
  }
  manifest
}

lomm11_runner_checkpoint_path <- function(context) {
  file.path(context$output_dir, "checkpoint_results.csv")
}

lomm11_runner_state_directory <- function(context) {
  file.path(context$output_dir, "_state")
}

lomm11_runner_finalizing_directory <- function(context) {
  file.path(
    dirname(context$output_dir),
    paste0(
      ".", basename(context$output_dir), ".finalizing-",
      substr(context$manifest$config_hash, 1L, 16L)
    )
  )
}

lomm11_runner_pending_success_path <- function(context) {
  file.path(
    dirname(context$output_dir),
    paste0(
      ".", basename(context$output_dir), ".success-pending-",
      substr(context$manifest$config_hash, 1L, 16L), ".json"
    )
  )
}

lomm11_runner_finalizing_marker <- function(context) {
  list(
    schema_version = 1L,
    status = "finalizing",
    config_hash = context$manifest$config_hash,
    prospective_manifest_sha256 = context$manifest_sha256,
    output_directory = context$output_dir,
    checkpoint_sha256 = lomm11_runner_sha256(
      lomm11_runner_checkpoint_path(context)
    ),
    prepared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

lomm11_runner_validate_finalizing_marker <- function(directory, context) {
  path <- file.path(directory, "_FINALIZING.json")
  if (!file.exists(path)) {
    stop("Finalizing state has no provenance marker.", call. = FALSE)
  }
  marker <- lomm11_runner_read_json(path)
  fields <- c(
    "schema_version", "status", "config_hash",
    "prospective_manifest_sha256", "output_directory", "checkpoint_sha256",
    "prepared_at"
  )
  valid <- setequal(names(marker), fields) &&
    identical(marker$schema_version, 1L) &&
    identical(marker$status, "finalizing") &&
    identical(marker$config_hash, context$manifest$config_hash) &&
    identical(marker$prospective_manifest_sha256, context$manifest_sha256) &&
    identical(marker$output_directory, context$output_dir) &&
    identical(
      marker$checkpoint_sha256,
      lomm11_runner_sha256(lomm11_runner_checkpoint_path(context))
    ) &&
    is.character(marker$prepared_at) && length(marker$prepared_at) == 1L &&
    nzchar(marker$prepared_at)
  if (!valid) {
    stop("Finalizing-state marker differs from this authorized run.", call. = FALSE)
  }
  marker
}

lomm11_runner_validate_pending_identity <- function(path, context) {
  pending <- lomm11_runner_read_json(path)
  if (!identical(pending$config_hash, context$manifest$config_hash) ||
      !identical(
        pending$prospective_manifest_sha256, context$manifest_sha256
      )) {
    stop("Pending success receipt differs from this authorized run.", call. = FALSE)
  }
  invisible(pending)
}

lomm11_runner_restore_finalizing_state <- function(context) {
  finalizing <- lomm11_runner_finalizing_directory(context)
  state <- lomm11_runner_state_directory(context)
  pending <- lomm11_runner_pending_success_path(context)
  if (file.exists(file.path(context$output_dir, "_SUCCESS.json"))) {
    return(invisible(FALSE))
  }
  if (dir.exists(finalizing)) {
    if (dir.exists(state)) {
      stop("Both active and finalizing state directories exist.", call. = FALSE)
    }
    lomm11_runner_validate_finalizing_marker(finalizing, context)
    if (!file.rename(finalizing, state)) {
      stop("Failed to restore finalizing state for resume.", call. = FALSE)
    }
    marker <- file.path(state, "_FINALIZING.json")
    if (file.exists(marker)) unlink(marker)
  }
  if (file.exists(pending)) {
    lomm11_runner_validate_pending_identity(pending, context)
    unlink(pending)
  }
  invisible(dir.exists(state))
}

lomm11_runner_move_state_to_finalizing <- function(context) {
  state <- lomm11_runner_state_directory(context)
  finalizing <- lomm11_runner_finalizing_directory(context)
  if (!dir.exists(state) || dir.exists(finalizing) ||
      file.exists(file.path(context$output_dir, "_SUCCESS.json"))) {
    stop("Finalization state transition is not safe to start.", call. = FALSE)
  }
  lomm11_runner_atomic_json(
    lomm11_runner_finalizing_marker(context),
    file.path(state, "_FINALIZING.json")
  )
  if (!file.rename(state, finalizing)) {
    stop("Failed to atomically move state into finalization.", call. = FALSE)
  }
  if (dir.exists(state)) {
    stop("Internal state remained inside the output directory.", call. = FALSE)
  }
  finalizing
}

lomm11_runner_cleanup_finalizing <- function(context) {
  finalizing <- lomm11_runner_finalizing_directory(context)
  if (!dir.exists(finalizing)) return(invisible(FALSE))
  if (!file.exists(file.path(context$output_dir, "_SUCCESS.json"))) {
    stop("Finalizing state can be cleaned only after strict success validation.",
         call. = FALSE)
  }
  lomm11_runner_validate_success_receipt(context)
  lomm11_runner_validate_finalizing_marker(finalizing, context)
  unlink(finalizing, recursive = TRUE)
  if (dir.exists(finalizing)) {
    stop("Failed to clean validated finalizing state.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_metric_chunk_path <- function(context, unit_key) {
  lomm11_runner_metric_chunk_file(
    lomm11_runner_state_directory(context), unit_key
  )
}

lomm11_runner_metric_chunk_file <- function(state_directory, unit_key) {
  file.path(
    state_directory, "heldout_metric_chunks",
    paste0(substr(lomm11_runner_object_sha256(unit_key), 1L, 32L), ".csv")
  )
}

lomm11_runner_training_parquet_path <- function(context, metrics) {
  lomm11_runner_parquet_path(
    file.path(context$output_dir, "training_metric_discrepancies.parquet"),
    metrics$unit_key[[1L]],
    list(fold = sprintf("%02d", metrics$fold[[1L]]),
         variant_id = metrics$variant_id[[1L]])
  )
}

lomm11_runner_contribution_parquet_path <- function(context, contributions) {
  lomm11_runner_parquet_path(
    file.path(context$output_dir, "clip_metric_contributions.parquet"),
    paste(contributions$unit_key[[1L]], contributions$family[[1L]], sep = "|"),
    list(
      analysis_view = contributions$analysis_view[[1L]],
      fold = sprintf("%02d", contributions$fold[[1L]]),
      variant_id = contributions$variant_id[[1L]],
      family = contributions$family[[1L]]
    )
  )
}

lomm11_runner_expected_checkpoint_keys <- function(context) {
  c(
    as.character(context$plan$training_plan.csv$unit_key),
    as.character(context$plan$heldout_evaluation_plan.csv$unit_key)
  )
}

lomm11_runner_read_selected_for_checkpoint <- function(context, checkpoint,
                                                       selected = NULL) {
  if (!any(checkpoint$unit_key %in%
           context$plan$heldout_evaluation_plan.csv$unit_key)) {
    return(NULL)
  }
  contract <- lomm11_runner_locked_output_contract()$selected_candidates.csv
  if (is.null(selected)) {
    path <- file.path(context$output_dir, "selected_candidates.csv")
    if (!file.exists(path)) {
      stop("Held-out checkpoint rows require selected_candidates.csv.", call. = FALSE)
    }
    selected <- lomm11_runner_read_csv_contract(
      path, contract, "Checkpoint selection map"
    )
  } else {
    selected <- lomm11_runner_coerce_frame(
      selected, contract, "Checkpoint selection map"
    )
  }
  selection_plan <- context$plan$selection_plan.csv
  candidate_grid <- context$plan$candidate_grid.csv
  if (nrow(selected) != nrow(selection_plan) ||
      !setequal(selected$selection_key, selection_plan$selection_key)) {
    stop("Checkpoint selection map differs from the 40 planned selections.", call. = FALSE)
  }
  plan_index <- match(selected$selection_key, selection_plan$selection_key)
  candidate_index <- match(selected$candidate_id, candidate_grid$candidate_id)
  valid <- !anyNA(c(plan_index, candidate_index)) &&
    identical(selected$fold, selection_plan$fold[plan_index]) &&
    identical(selected$variant_id, selection_plan$variant_id[plan_index]) &&
    identical(selected$variant_id, candidate_grid$variant_id[candidate_index]) &&
    identical(
      selected$core_candidate_id,
      candidate_grid$core_candidate_id[candidate_index]
    ) && identical(selected$period_frames, candidate_grid$period_frames[candidate_index]) &&
    identical(selected$damping_ratio, candidate_grid$damping_ratio[candidate_index]) &&
    identical(selected$kappa, candidate_grid$kappa[candidate_index]) &&
    all(selected$config_hash == context$manifest$config_hash) &&
    all(selected$input_hash == context$input_hash)
  if (!valid) {
    stop("Checkpoint selection map contains inconsistent plan-derived IDs.", call. = FALSE)
  }
  selected
}

lomm11_runner_expected_checkpoint_metadata <- function(context, checkpoint,
                                                       selected = NULL) {
  training <- context$plan$training_plan.csv
  training_expected <- data.frame(
    unit_key = training$unit_key,
    panel_key = training$panel_key,
    stage = training$stage,
    analysis_view = training$analysis_view,
    fold = training$fold,
    variant_id = training$variant_id,
    candidate_id = training$candidate_id,
    replicate = training$training_rep,
    seed_group = training$seed_group,
    seed_base = training$seed_base,
    stringsAsFactors = FALSE
  )
  evaluation <- context$plan$heldout_evaluation_plan.csv
  selection_map <- lomm11_runner_read_selected_for_checkpoint(
    context, checkpoint, selected
  )
  heldout_candidate <- rep(NA_character_, nrow(evaluation))
  if (!is.null(selection_map)) {
    heldout_candidate <- selection_map$candidate_id[
      match(evaluation$selection_key, selection_map$selection_key)
    ]
  }
  heldout_expected <- data.frame(
    unit_key = evaluation$unit_key,
    panel_key = evaluation$panel_key,
    stage = evaluation$stage,
    analysis_view = evaluation$analysis_view,
    fold = evaluation$fold,
    variant_id = evaluation$variant_id,
    candidate_id = heldout_candidate,
    replicate = evaluation$heldout_rep,
    seed_group = evaluation$seed_group,
    seed_base = evaluation$seed_base,
    stringsAsFactors = FALSE
  )
  rbind(training_expected, heldout_expected)
}

lomm11_runner_validate_checkpoint <- function(checkpoint, context,
                                               complete = FALSE, selected = NULL) {
  contract <- lomm11_runner_locked_output_contract()$checkpoint_results.csv
  checkpoint <- lomm11_runner_coerce_frame(checkpoint, contract, "Checkpoint")
  expected <- lomm11_runner_expected_checkpoint_keys(context)
  if (any(!checkpoint$unit_key %in% expected) ||
      any(!checkpoint$status %in% c("ok", "error", "timeout")) ||
      any(checkpoint$config_hash != context$manifest$config_hash) ||
      any(checkpoint$input_hash != context$input_hash)) {
    stop("Checkpoint rows violate the plan or provenance contract.", call. = FALSE)
  }
  metadata <- lomm11_runner_expected_checkpoint_metadata(
    context, checkpoint, selected
  )
  metadata <- metadata[match(checkpoint$unit_key, metadata$unit_key), , drop = FALSE]
  immutable <- setdiff(names(metadata), "unit_key")
  mismatched <- immutable[!vapply(immutable, function(field) {
    identical(checkpoint[[field]], metadata[[field]])
  }, logical(1))]
  if (length(mismatched) > 0L) {
    stop(
      "Checkpoint immutable metadata differ from the materialized plan: ",
      paste(mismatched, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (isTRUE(complete) &&
      (!identical(nrow(checkpoint), length(expected)) ||
       !setequal(checkpoint$unit_key, expected))) {
    stop("Complete checkpoint does not contain all 11,240 planned units.", call. = FALSE)
  }
  checkpoint
}

lomm11_runner_validate_heldout_pairing <- function(checkpoint, context) {
  heldout <- checkpoint[checkpoint$stage == "heldout", , drop = FALSE]
  expected_panels <- context$plan$heldout_plan.csv$panel_key
  if (!setequal(unique(heldout$panel_key), expected_panels)) {
    stop("Held-out checkpoint panels differ from the materialized plan.", call. = FALSE)
  }
  valid <- vapply(expected_panels, function(panel_key) {
    rows <- heldout[heldout$panel_key == panel_key, , drop = FALSE]
    hashes <- as.character(rows$panel_hash)
    missing_hash <- is.na(hashes) | !nzchar(hashes)
    shared_generation_failure <- all(missing_hash) &&
      all(rows$status %in% c("error", "timeout")) &&
      length(unique(rows$status)) == 1L &&
      length(unique(as.character(rows$error_message))) == 1L
    shared_panel <- !any(missing_hash) && length(unique(hashes)) == 1L
    nrow(rows) == 2L &&
      setequal(rows$analysis_view, c("complete_case", "ilr_gap2")) &&
      length(unique(rows$seed_base)) == 1L &&
      length(unique(rows$seed_group)) == 1L &&
      length(unique(rows$candidate_id)) == 1L &&
      (shared_panel || shared_generation_failure)
  }, logical(1))
  if (!all(valid)) {
    stop("Held-out checkpoint rows do not prove paired-view panel reuse.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_load_checkpoint <- function(context) {
  path <- lomm11_runner_checkpoint_path(context)
  contract <- lomm11_runner_locked_output_contract()$checkpoint_results.csv
  if (!file.exists(path)) {
    empty <- lomm11_runner_schema_frame(contract)
    lomm11_runner_atomic_csv(empty, path)
    return(empty)
  }
  lomm11_runner_validate_checkpoint(
    lomm11_runner_read_csv_contract(path, contract, "Checkpoint"), context
  )
}

lomm11_runner_append_checkpoint <- function(checkpoint, additions, context) {
  if (length(additions) == 0L) return(checkpoint)
  additions <- do.call(rbind, additions)
  additions <- additions[!additions$unit_key %in% checkpoint$unit_key, , drop = FALSE]
  if (nrow(additions) == 0L) return(checkpoint)
  combined <- lomm11_runner_validate_checkpoint(rbind(checkpoint, additions), context)
  order_index <- match(
    combined$unit_key, lomm11_runner_expected_checkpoint_keys(context)
  )
  combined <- combined[order(order_index), , drop = FALSE]
  lomm11_runner_atomic_csv(combined, lomm11_runner_checkpoint_path(context))
  combined
}

lomm11_runner_validate_unit_provenance <- function(frame, checkpoint_row,
                                                   expected_rows, label) {
  unit_key <- checkpoint_row$unit_key[[1L]]
  matches <- nrow(frame) == expected_rows && all(frame$unit_key == unit_key) &&
    all(frame$status == checkpoint_row$status[[1L]]) &&
    all(frame$config_hash == checkpoint_row$config_hash[[1L]]) &&
    all(frame$input_hash == checkpoint_row$input_hash[[1L]]) &&
    identical(
      as.character(frame$panel_hash),
      rep(as.character(checkpoint_row$panel_hash[[1L]]), nrow(frame))
    )
  if (!matches) {
    stop(label, " rows or provenance differ from the checkpoint.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_validate_existing_training_artifact <- function(checkpoint_row,
                                                               context) {
  unit_key <- checkpoint_row$unit_key[[1L]]
  row <- context$plan$training_plan.csv[
    context$plan$training_plan.csv$unit_key == unit_key, , drop = FALSE
  ]
  template <- data.frame(
    unit_key = unit_key, fold = row$fold, variant_id = row$variant_id,
    stringsAsFactors = FALSE
  )
  path <- lomm11_runner_training_parquet_path(context, template)
  if (!file.exists(path)) {
    stop("Checkpointed training unit is missing its Parquet artifact.", call. = FALSE)
  }
  contract <- lomm11_runner_locked_output_contract()$training_metric_discrepancies.parquet
  metrics <- lomm11_runner_coerce_frame(
    as.data.frame(arrow::read_parquet(path)), contract,
    "Checkpointed training metric artifact"
  )
  lomm11_runner_validate_unit_provenance(
    metrics, checkpoint_row, 16L, "Checkpointed training metric artifact"
  )
  expected_groups <- paste(
    lomm11_metric_groups()$family, lomm11_metric_groups()$metric, sep = "|"
  )
  if (!setequal(paste(metrics$family, metrics$metric, sep = "|"), expected_groups)) {
    stop("Checkpointed training metric keys are incomplete.", call. = FALSE)
  }
  invisible(TRUE)
}

lomm11_runner_validate_existing_heldout_artifact <- function(checkpoint_row,
                                                              context) {
  unit_key <- checkpoint_row$unit_key[[1L]]
  evaluation <- context$plan$heldout_evaluation_plan.csv[
    context$plan$heldout_evaluation_plan.csv$unit_key == unit_key, , drop = FALSE
  ]
  chunk <- lomm11_runner_metric_chunk_path(context, unit_key)
  if (!file.exists(chunk)) {
    stop("Checkpointed held-out unit is missing its metric chunk.", call. = FALSE)
  }
  metric_contract <- lomm11_runner_locked_output_contract()$heldout_metric_discrepancies.csv
  metrics <- lomm11_runner_read_csv_contract(
    chunk, metric_contract, "Checkpointed held-out metric artifact"
  )
  lomm11_runner_validate_unit_provenance(
    metrics, checkpoint_row, 16L, "Checkpointed held-out metric artifact"
  )
  expected_groups <- paste(
    lomm11_metric_groups()$family, lomm11_metric_groups()$metric, sep = "|"
  )
  if (!setequal(paste(metrics$family, metrics$metric, sep = "|"), expected_groups)) {
    stop("Checkpointed held-out metric keys are incomplete.", call. = FALSE)
  }
  contribution_contract <- lomm11_runner_locked_output_contract()$clip_metric_contributions.parquet
  for (family in c("probability", "logratio")) {
    template <- data.frame(
      unit_key = unit_key,
      analysis_view = evaluation$analysis_view,
      fold = evaluation$fold,
      variant_id = evaluation$variant_id,
      family = family,
      stringsAsFactors = FALSE
    )
    path <- lomm11_runner_contribution_parquet_path(context, template)
    if (!file.exists(path)) {
      stop("Checkpointed held-out unit is missing a contribution partition.", call. = FALSE)
    }
    contributions <- lomm11_runner_coerce_frame(
      as.data.frame(arrow::read_parquet(path)), contribution_contract,
      "Checkpointed contribution artifact"
    )
    lomm11_runner_validate_unit_provenance(
      contributions, checkpoint_row, 1360L, "Checkpointed contribution artifact"
    )
    if (any(contributions$family != family) ||
        any(contributions$analysis_view != evaluation$analysis_view[[1L]]) ||
        any(contributions$fold != evaluation$fold[[1L]]) ||
        any(contributions$variant_id != evaluation$variant_id[[1L]])) {
      stop("Checkpointed contribution partition keys are invalid.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

lomm11_runner_validate_checkpoint_artifacts <- function(checkpoint, context) {
  training <- checkpoint[checkpoint$stage == "training", , drop = FALSE]
  heldout <- checkpoint[checkpoint$stage == "heldout", , drop = FALSE]
  invisible(lapply(seq_len(nrow(training)), function(index) {
    lomm11_runner_validate_existing_training_artifact(
      training[index, , drop = FALSE], context
    )
  }))
  invisible(lapply(seq_len(nrow(heldout)), function(index) {
    lomm11_runner_validate_existing_heldout_artifact(
      heldout[index, , drop = FALSE], context
    )
  }))
  invisible(TRUE)
}

lomm11_runner_write_training_result <- function(result, context) {
  metrics <- result$metrics
  if (nrow(metrics) != 16L) {
    stop("A training unit must write exactly 16 metric rows.", call. = FALSE)
  }
  path <- lomm11_runner_training_parquet_path(context, metrics)
  lomm11_runner_atomic_parquet(metrics, path, c("fold", "variant_id"))
  result$checkpoint
}

lomm11_runner_write_heldout_view <- function(result, context) {
  metrics <- result$discrepancies
  contributions <- result$contributions
  if (nrow(metrics) != 16L || nrow(contributions) != 2720L) {
    stop("Held-out unit row counts differ from the 16/2,720 contract.", call. = FALSE)
  }
  metric_contract <- lomm11_runner_locked_output_contract()$heldout_metric_discrepancies.csv
  chunk_path <- lomm11_runner_metric_chunk_path(context, metrics$unit_key[[1L]])
  if (file.exists(chunk_path)) {
    existing <- lomm11_runner_read_csv_contract(
      chunk_path, metric_contract, "Held-out metric chunk"
    )
    if (nrow(existing) != 16L || !identical(existing$unit_key, metrics$unit_key)) {
      stop("Existing held-out metric chunk is incompatible.", call. = FALSE)
    }
  } else {
    lomm11_runner_atomic_csv(metrics, chunk_path)
  }
  for (family in c("probability", "logratio")) {
    rows <- contributions[contributions$family == family, , drop = FALSE]
    if (nrow(rows) != 1360L) {
      stop("Held-out family contribution count must equal 1,360.", call. = FALSE)
    }
    path <- lomm11_runner_contribution_parquet_path(context, rows)
    lomm11_runner_atomic_parquet(
      rows, path, c("analysis_view", "fold", "variant_id", "family")
    )
  }
  result$checkpoint
}

lomm11_runner_read_training_metrics <- function(context) {
  root <- file.path(context$output_dir, "training_metric_discrepancies.parquet")
  dataset <- arrow::open_dataset(root, format = "parquet")
  scanner_builder <- dataset$NewScan()
  scanner_builder$UseThreads(FALSE)
  frame <- as.data.frame(scanner_builder$Finish()$ToTable())
  contract <- lomm11_runner_locked_output_contract()$training_metric_discrepancies.parquet
  frame <- lomm11_runner_coerce_frame(frame, contract, "Training metrics")
  if (nrow(frame) != contract$rows) {
    stop("Training metric dataset does not contain 51,840 rows.", call. = FALSE)
  }
  frame
}

lomm11_runner_candidate_losses <- function(metrics, candidate_grid, fold,
                                            variant_id) {
  candidates <- candidate_grid[
    candidate_grid$variant_id == variant_id, , drop = FALSE
  ]
  rows <- lapply(seq_len(nrow(candidates)), function(index) {
    candidate <- candidates[index, , drop = FALSE]
    selected <- metrics[
      metrics$fold == fold & metrics$candidate_id == candidate$candidate_id,
      , drop = FALSE
    ]
    expected_units <- length(unique(selected$unit_key)) == 3L
    groups <- split(selected, paste(selected$family, selected$metric, sep = "|"))
    pooled <- vapply(groups, function(group) {
      if (!expected_units || nrow(group) != 3L || any(group$status != "ok") ||
          any(!is.finite(group$max_discrepancy))) Inf else mean(group$max_discrepancy)
    }, numeric(1))
    probability <- pooled[startsWith(names(pooled), "probability|")]
    logratio <- pooled[startsWith(names(pooled), "logratio|")]
    probability_loss <- if (length(probability) == 8L) mean(probability) else Inf
    logratio_loss <- if (length(logratio) == 8L) mean(logratio) else Inf
    data.frame(
      candidate_id = candidate$candidate_id,
      probability_loss = probability_loss,
      logratio_loss = logratio_loss,
      selection_loss = 0.5 * probability_loss + 0.5 * logratio_loss,
      max_metric_discrepancy = if (length(pooled) == 16L) max(pooled) else Inf,
      training_units = as.integer(length(unique(selected$unit_key))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

lomm11_runner_select_candidates <- function(context, metrics) {
  candidate_grid <- context$plan$candidate_grid.csv
  selection_plan <- context$plan$selection_plan.csv
  rows <- lapply(seq_len(nrow(selection_plan)), function(index) {
    selection <- selection_plan[index, , drop = FALSE]
    losses <- lomm11_runner_candidate_losses(
      metrics, candidate_grid, selection$fold[[1L]], selection$variant_id[[1L]]
    )
    candidates <- merge(
      candidate_grid[candidate_grid$variant_id == selection$variant_id[[1L]], ],
      losses, by = "candidate_id", sort = FALSE
    )
    eligible <- is.finite(candidates$selection_loss) &
      is.finite(candidates$max_metric_discrepancy) &
      candidates$selection_eligible
    if (!any(eligible)) {
      stop(
        "No finite eligible candidate for selection key ",
        selection$selection_key[[1L]], ".",
        call. = FALSE
      )
    }
    candidates <- candidates[eligible, , drop = FALSE]
    order_index <- order(
      candidates$selection_loss,
      candidates$max_metric_discrepancy,
      abs(candidates$kappa - 1),
      -candidates$damping_ratio,
      candidates$candidate_id,
      na.last = TRUE
    )
    winner <- candidates[order_index[[1L]], , drop = FALSE]
    data.frame(
      selection_key = selection$selection_key,
      fold = selection$fold,
      variant_id = selection$variant_id,
      candidate_id = winner$candidate_id,
      core_candidate_id = winner$core_candidate_id,
      period_frames = winner$period_frames,
      damping_ratio = winner$damping_ratio,
      kappa = winner$kappa,
      probability_loss = winner$probability_loss,
      logratio_loss = winner$logratio_loss,
      selection_loss = winner$selection_loss,
      max_metric_discrepancy = winner$max_metric_discrepancy,
      training_units = winner$training_units,
      selected_before_heldout = TRUE,
      config_hash = context$manifest$config_hash,
      input_hash = context$input_hash,
      stringsAsFactors = FALSE
    )
  })
  contract <- lomm11_runner_locked_output_contract()$selected_candidates.csv
  selected <- lomm11_runner_coerce_frame(
    do.call(rbind, rows), contract, "Selected candidates"
  )
  if (nrow(selected) != 40L || any(selected$training_units != 3L)) {
    stop("Selection did not produce 40 candidates from three training units each.", call. = FALSE)
  }
  selected
}

lomm11_runner_publish_selected <- function(selected, context) {
  path <- file.path(context$output_dir, "selected_candidates.csv")
  contract <- lomm11_runner_locked_output_contract()$selected_candidates.csv
  if (file.exists(path)) {
    existing <- lomm11_runner_read_csv_contract(path, contract, "Selected candidates")
    if (!identical(
      lomm11_runner_object_sha256(existing), lomm11_runner_object_sha256(selected)
    )) {
      stop("Existing selections differ from training-only recomputation.", call. = FALSE)
    }
  } else {
    lomm11_runner_atomic_csv(selected, path)
  }
  selected
}

lomm11_runner_progress <- function(checkpoint) {
  counts <- table(factor(
    checkpoint$status, levels = c("ok", "error", "timeout")
  ))
  list(
    checkpoint_units = as.integer(nrow(checkpoint)),
    training_units = as.integer(sum(checkpoint$stage == "training")),
    heldout_view_units = as.integer(sum(checkpoint$stage == "heldout")),
    ok = as.integer(counts[["ok"]]),
    error = as.integer(counts[["error"]]),
    timeout = as.integer(counts[["timeout"]])
  )
}

lomm11_runner_git_state <- function() {
  commit <- system2(
    "git", c("-C", LOMM11_RUNNER_REPO_ROOT, "rev-parse", "HEAD"),
    stdout = TRUE
  )
  dirty <- length(system2(
    "git", c("-C", LOMM11_RUNNER_REPO_ROOT, "status", "--porcelain"),
    stdout = TRUE
  )) > 0L
  list(commit = unname(commit[[1L]]), dirty = dirty)
}

lomm11_runner_new_manifest <- function(context, workers, preflight) {
  lomm11_runner_validate_execution_preflight(preflight)
  list(
    schema_version = 1L,
    status = "running",
    stage = "preparing",
    pipeline_id = context$config$pipeline_id,
    config_hash = context$manifest$config_hash,
    config_prefix = context$manifest$config_prefix,
    prospective_manifest_sha256 = context$manifest_sha256,
    runner_sha256 = lomm11_runner_sha256(LOMM11_RUNNER_PATH),
    input_hash = context$input_hash,
    source_hashes = as.list(LOMM11_RUNNER_STARTUP_HASHES),
    plan_hashes = as.list(context$plan_snapshot),
    input_hashes = as.list(context$input_snapshot),
    workers = as.integer(workers),
    worker_backend = "PSOCK",
    worker_export = lomm11_runner_worker_export_identity(),
    execution_preflight = preflight,
    blas_threads_per_worker = 1L,
    panel_timeout_seconds = as.integer(
      context$config$future_execution$panel_timeout_seconds
    ),
    job_timeout_seconds = as.integer(
      context$config$future_execution$job_timeout_seconds
    ),
    progress = lomm11_runner_progress(
      lomm11_runner_schema_frame(
        lomm11_runner_locked_output_contract()$checkpoint_results.csv
      )
    ),
    nuisance = list(status = "pending", fold_hashes = list()),
    resume_count = 0L,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    completed_at = NULL,
    command = paste(commandArgs(), collapse = " "),
    software_versions = list(
      R = R.version.string,
      codetools = as.character(utils::packageVersion("codetools")),
      digest = as.character(utils::packageVersion("digest")),
      jsonlite = as.character(utils::packageVersion("jsonlite")),
      arrow = as.character(utils::packageVersion("arrow"))
    ),
    git = lomm11_runner_git_state(),
    outputs = list()
  )
}

lomm11_runner_initialize_result <- function(context, workers, preflight) {
  lomm11_runner_validate_execution_preflight(preflight)
  manifest_path <- file.path(context$output_dir, "run_manifest.json")
  if (isTRUE(context$args$resume)) {
    if (!dir.exists(context$output_dir) || !file.exists(manifest_path)) {
      stop("--resume requires an existing result directory and run manifest.", call. = FALSE)
    }
    manifest <- lomm11_runner_read_json(manifest_path)
    if (!identical(manifest$config_hash, context$manifest$config_hash) ||
        !identical(manifest$prospective_manifest_sha256, context$manifest_sha256) ||
        !identical(manifest$input_hash, context$input_hash) ||
        !identical(manifest$worker_backend, "PSOCK") ||
        !identical(
          lomm11_runner_object_sha256(manifest$worker_export),
          lomm11_runner_object_sha256(lomm11_runner_worker_export_identity())
        ) || !identical(
          manifest$software_versions$codetools,
          as.character(utils::packageVersion("codetools"))
        )) {
      stop("Existing run manifest differs from the authorized plan.", call. = FALSE)
    }
    if (file.exists(file.path(context$output_dir, "_SUCCESS.json"))) {
      if (!identical(manifest$status, "complete")) {
        stop("Success receipt exists for a non-complete run manifest.", call. = FALSE)
      }
      return(manifest)
    }
    manifest$resume_count <- as.integer(manifest$resume_count) + 1L
    manifest$status <- "running"
    manifest$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    manifest$workers <- as.integer(workers)
    manifest$execution_preflight <- preflight
    lomm11_runner_atomic_json(manifest, manifest_path)
    return(manifest)
  }
  if (file.exists(context$output_dir) || dir.exists(context$output_dir)) {
    stop("Fresh execution refuses an existing result directory; use --resume.", call. = FALSE)
  }
  dir.create(context$output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(lomm11_runner_state_directory(context), recursive = TRUE, showWarnings = FALSE)
  manifest <- lomm11_runner_new_manifest(context, workers, preflight)
  lomm11_runner_atomic_json(manifest, manifest_path)
  manifest
}

lomm11_runner_update_manifest <- function(manifest, context, checkpoint, stage,
                                          status = "running", nuisance = NULL,
                                          outputs = NULL) {
  manifest$status <- status
  manifest$stage <- stage
  manifest$progress <- lomm11_runner_progress(checkpoint)
  manifest$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (!is.null(nuisance)) manifest$nuisance <- nuisance
  if (!is.null(outputs)) manifest$outputs <- outputs
  if (identical(status, "complete")) {
    manifest$completed_at <- manifest$updated_at
  }
  lomm11_runner_atomic_json(
    manifest, file.path(context$output_dir, "run_manifest.json")
  )
  manifest
}

lomm11_runner_worker_dependency_names <- function(root_functions,
                                                   environment = .GlobalEnv) {
  is_global_function <- function(name) {
    exists(name, envir = environment, inherits = FALSE) &&
      is.function(get(name, envir = environment, inherits = FALSE))
  }
  if (any(!vapply(root_functions, is_global_function, logical(1)))) {
    stop("A declared PSOCK worker root function is unavailable.", call. = FALSE)
  }
  resolved_functions <- character()
  resolved_variables <- character()
  pending <- unique(root_functions)
  while (length(pending) > 0L) {
    name <- pending[[1L]]
    pending <- pending[-1L]
    if (name %in% resolved_functions) next
    callback <- get(name, envir = environment, inherits = FALSE)
    globals <- codetools::findGlobals(callback, merge = FALSE)
    local_functions <- globals$functions[
      vapply(globals$functions, is_global_function, logical(1))
    ]
    local_variables <- globals$variables[vapply(globals$variables, function(variable) {
      exists(variable, envir = environment, inherits = FALSE)
    }, logical(1))]
    resolved_functions <- c(resolved_functions, name)
    resolved_variables <- union(resolved_variables, local_variables)
    pending <- union(
      pending, setdiff(local_functions, resolved_functions)
    )
  }
  sort(unique(c(resolved_functions, resolved_variables)))
}

lomm11_runner_capture_worker_function_roots <- function(
    environment = .GlobalEnv) {
  global_names <- ls(envir = environment, all.names = TRUE)
  global_names[
    grepl("^(lomm11_|lomm_)", global_names) &
      !grepl("^lomm11_test_", global_names) &
      global_names != "lomm11_run_pure_integrity_tests" &
      vapply(global_names, function(name) {
        is.function(get(name, envir = environment, inherits = FALSE))
      }, logical(1))
  ]
}

lomm11_runner_worker_names <- function() {
  global_names <- ls(envir = .GlobalEnv, all.names = TRUE)
  runtime_functions <- LOMM11_RUNNER_WORKER_FUNCTION_ROOTS
  dynamic_roots <- "revision_exact_transition"
  constants <- global_names[
    grepl("^LOMM11_", global_names) & !grepl("^LOMM11_TEST_", global_names)
  ]
  sort(unique(c(
    constants, LOMM11_RUNNER_WORKER_DYNAMIC_EXPORTS,
    lomm11_runner_worker_dependency_names(
      c(runtime_functions, dynamic_roots), .GlobalEnv
    )
  )))
}

lomm11_runner_worker_export_identity <- function() {
  export_names <- lomm11_runner_worker_names()
  list(
    schema_version = 1L,
    initial_export_count = as.integer(length(export_names)),
    initial_export_sha256 = lomm11_runner_object_sha256(as.list(export_names)),
    deferred_export_names = as.list(LOMM11_RUNNER_WORKER_DEFERRED_EXPORTS)
  )
}

lomm11_runner_preflight_export_environment <- function(export_names) {
  environment <- new.env(parent = emptyenv())
  static_names <- setdiff(export_names, LOMM11_RUNNER_WORKER_DYNAMIC_EXPORTS)
  missing <- static_names[!vapply(static_names, function(name) {
    exists(name, envir = .GlobalEnv, inherits = FALSE)
  }, logical(1))]
  if (length(missing) > 0L) {
    stop(
      "PSOCK preflight exports are unavailable: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  for (name in static_names) {
    assign(name, get(name, envir = .GlobalEnv, inherits = FALSE), envir = environment)
  }
  placeholders <- list(
    LOMM11_WORKER_CACHES = list(),
    LOMM11_WORKER_CONTEXT = list(preflight = TRUE),
    LOMM11_WORKER_CANDIDATES = data.frame(),
    LOMM11_WORKER_EVALUATIONS = data.frame()
  )
  for (name in names(placeholders)) {
    assign(name, placeholders[[name]], envir = environment)
  }
  environment
}

lomm11_runner_with_preserved_rng <- function(callback) {
  if (!is.function(callback)) {
    stop("RNG-preserving preflight requires a callback.", call. = FALSE)
  }
  previous_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  callback()
}

lomm11_runner_arrow_zstd_preflight <- function() {
  if (!requireNamespace("arrow", quietly = TRUE) ||
      !isTRUE(arrow::codec_is_available("zstd"))) {
    stop("Execute mode requires an Arrow build with the zstd codec.", call. = FALSE)
  }
  path <- tempfile("lomm11-arrow-zstd-preflight-", fileext = ".parquet")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  fixture <- data.frame(preflight_key = "lomm11", value = 1L)
  arrow::write_parquet(fixture, path, compression = "zstd")
  observed <- as.data.frame(arrow::read_parquet(path))
  if (!identical(observed, fixture)) {
    stop("Arrow zstd preflight round-trip failed.", call. = FALSE)
  }
  list(
    arrow_version = as.character(utils::packageVersion("arrow")),
    zstd_available = TRUE,
    zstd_roundtrip = TRUE
  )
}

lomm11_runner_psock_transport_preflight <- function() {
  export_names <- lomm11_runner_worker_names()
  export_environment <- lomm11_runner_preflight_export_environment(export_names)
  cluster <- parallel::makePSOCKcluster(1L, outfile = "")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(cluster, export_names, envir = export_environment)
  expected <- revision_exact_transition(-1, -0.2, 1)
  observed <- parallel::clusterCall(cluster, function() {
    revision_exact_transition(-1, -0.2, 1)
  })[[1L]]
  if (!identical(observed, expected)) {
    stop("PSOCK exact-transition transport preflight failed.", call. = FALSE)
  }
  list(
    workers = 1L,
    exact_transition_transport = TRUE,
    exact_transition_sha256 = lomm11_runner_object_sha256(observed)
  )
}

lomm11_runner_execution_preflight <- function() {
  lomm11_runner_with_preserved_rng(function() {
    arrow <- lomm11_runner_arrow_zstd_preflight()
    psock <- lomm11_runner_psock_transport_preflight()
    list(
      schema_version = 1L,
      status = "passed",
      arrow = arrow,
      psock = psock,
      worker_export = lomm11_runner_worker_export_identity(),
      caller_rng_preserved = TRUE,
      checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  })
}

lomm11_runner_validate_worker_export_identity <- function(identity) {
  expected <- lomm11_runner_worker_export_identity()
  if (!identical(
    lomm11_runner_object_sha256(identity),
    lomm11_runner_object_sha256(expected)
  )) {
    stop("PSOCK worker-export identity differs from the runner contract.", call. = FALSE)
  }
  invisible(identity)
}

lomm11_runner_validate_execution_preflight <- function(preflight) {
  fields <- c(
    "schema_version", "status", "arrow", "psock", "worker_export",
    "caller_rng_preserved", "checked_at"
  )
  arrow_fields <- c("arrow_version", "zstd_available", "zstd_roundtrip")
  psock_fields <- c(
    "workers", "exact_transition_transport", "exact_transition_sha256"
  )
  expected_transition_hash <- lomm11_runner_object_sha256(
    revision_exact_transition(-1, -0.2, 1)
  )
  valid <- is.list(preflight) && setequal(names(preflight), fields) &&
    identical(preflight$schema_version, 1L) &&
    identical(preflight$status, "passed") &&
    setequal(names(preflight$arrow), arrow_fields) &&
    identical(
      preflight$arrow$arrow_version,
      as.character(utils::packageVersion("arrow"))
    ) &&
    identical(preflight$arrow$zstd_available, TRUE) &&
    identical(preflight$arrow$zstd_roundtrip, TRUE) &&
    setequal(names(preflight$psock), psock_fields) &&
    identical(preflight$psock$workers, 1L) &&
    identical(preflight$psock$exact_transition_transport, TRUE) &&
    identical(preflight$psock$exact_transition_sha256, expected_transition_hash) &&
    identical(preflight$caller_rng_preserved, TRUE) &&
    is.character(preflight$checked_at) && length(preflight$checked_at) == 1L &&
    nzchar(preflight$checked_at)
  if (!valid) {
    stop("Execution preflight record is incomplete or invalid.", call. = FALSE)
  }
  lomm11_runner_validate_worker_export_identity(preflight$worker_export)
  invisible(preflight)
}

lomm11_runner_start_cluster <- function(workers, caches, context) {
  cluster <- parallel::makePSOCKcluster(workers, outfile = "")
  initialized <- FALSE
  on.exit({
    if (!initialized) lomm11_runner_stop_cluster(cluster)
  }, add = TRUE)
  assign("LOMM11_WORKER_CACHES", caches, envir = .GlobalEnv)
  assign("LOMM11_WORKER_CONTEXT", context, envir = .GlobalEnv)
  assign(
    "LOMM11_WORKER_CANDIDATES", context$plan$candidate_grid.csv,
    envir = .GlobalEnv
  )
  assign(
    "LOMM11_WORKER_EVALUATIONS", context$plan$heldout_evaluation_plan.csv,
    envir = .GlobalEnv
  )
  parallel::clusterExport(
    cluster, lomm11_runner_worker_names(), envir = .GlobalEnv
  )
  parallel::clusterCall(cluster, function() {
    Sys.setenv(
      OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
      VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
    )
    TRUE
  })
  initialized <- TRUE
  cluster
}

lomm11_runner_stop_cluster <- function(cluster) {
  if (!is.null(cluster)) parallel::stopCluster(cluster)
  variables <- c(
    "LOMM11_WORKER_CACHES", "LOMM11_WORKER_CONTEXT",
    "LOMM11_WORKER_CANDIDATES", "LOMM11_WORKER_EVALUATIONS",
    "LOMM11_WORKER_SELECTED"
  )
  existing <- variables[vapply(
    variables, exists, logical(1), envir = .GlobalEnv, inherits = FALSE
  )]
  if (length(existing) > 0L) rm(list = existing, envir = .GlobalEnv)
  invisible(TRUE)
}

lomm11_runner_worker_training <- function(row) {
  lomm11_runner_training_unit(
    row, LOMM11_WORKER_CANDIDATES, LOMM11_WORKER_CACHES,
    LOMM11_WORKER_CONTEXT
  )
}

lomm11_runner_worker_heldout <- function(row) {
  evaluations <- LOMM11_WORKER_EVALUATIONS[
    LOMM11_WORKER_EVALUATIONS$panel_key == row$panel_key[[1L]], , drop = FALSE
  ]
  lomm11_runner_heldout_panel(
    row, LOMM11_WORKER_SELECTED, evaluations, LOMM11_WORKER_CACHES,
    LOMM11_WORKER_CONTEXT
  )
}

lomm11_runner_batches <- function(frame, size) {
  if (nrow(frame) == 0L) return(list())
  group <- ceiling(seq_len(nrow(frame)) / size)
  split(frame, group)
}

lomm11_runner_row_tasks <- function(frame) {
  lapply(seq_len(nrow(frame)), function(index) frame[index, , drop = FALSE])
}

lomm11_runner_deadline_reached <- function(started, timeout_seconds) {
  as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout_seconds
}

lomm11_runner_batch_checkpoint_rows <- function(results, stage) {
  rows <- if (identical(stage, "training")) {
    lapply(results, function(result) result$checkpoint)
  } else if (identical(stage, "heldout")) {
    unlist(lapply(results, function(result) {
      lapply(result$results, function(view) view$checkpoint)
    }), recursive = FALSE)
  } else {
    stop("Unknown batch-integrity stage.", call. = FALSE)
  }
  if (length(rows) == 0L) return(data.frame())
  do.call(rbind, rows)
}

lomm11_runner_assert_batch_integrity <- function(results, stage, full_batch) {
  checkpoint <- lomm11_runner_batch_checkpoint_rows(results, stage)
  if (!isTRUE(full_batch) || nrow(checkpoint) == 0L) return(invisible(TRUE))
  messages <- trimws(as.character(checkpoint$error_message))
  systematic_failure <- !any(checkpoint$status == "ok") &&
    all(!is.na(messages) & nzchar(messages)) && length(unique(messages)) == 1L
  if (systematic_failure) {
    stop(
      "Systematic ", stage, " batch failure before artifact commit: ",
      unique(messages),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

lomm11_runner_commit_training_batch <- function(results, checkpoint, context,
                                                 full_batch) {
  lomm11_runner_assert_batch_integrity(results, "training", full_batch)
  additions <- lapply(results, lomm11_runner_write_training_result, context)
  lomm11_runner_append_checkpoint(checkpoint, additions, context)
}

lomm11_runner_commit_heldout_batch <- function(results, checkpoint, context,
                                                full_batch) {
  lomm11_runner_assert_batch_integrity(results, "heldout", full_batch)
  additions <- unlist(lapply(results, function(result) {
    lomm11_runner_write_heldout_panel_result(result, checkpoint, context)
  }), recursive = FALSE)
  lomm11_runner_append_checkpoint(checkpoint, additions, context)
}

lomm11_runner_run_training <- function(cluster, checkpoint, context, manifest,
                                       started) {
  plan <- context$plan$training_plan.csv
  pending <- plan[!plan$unit_key %in% checkpoint$unit_key, , drop = FALSE]
  timeout <- as.numeric(context$config$future_execution$job_timeout_seconds)
  batch_size <- max(1L, context$workers * 2L)
  for (batch in lomm11_runner_batches(pending, batch_size)) {
    if (lomm11_runner_deadline_reached(started, timeout)) break
    results <- parallel::parLapplyLB(
      cluster, lomm11_runner_row_tasks(batch), lomm11_runner_worker_training
    )
    checkpoint <- lomm11_runner_commit_training_batch(
      results, checkpoint, context, nrow(batch) == batch_size
    )
    lomm11_runner_assert_artifact_stability(context)
    manifest <- lomm11_runner_update_manifest(
      manifest, context, checkpoint, "training"
    )
  }
  list(
    checkpoint = checkpoint, manifest = manifest,
    complete = all(plan$unit_key %in% checkpoint$unit_key)
  )
}

lomm11_runner_export_selected <- function(cluster, selected) {
  assign("LOMM11_WORKER_SELECTED", selected, envir = .GlobalEnv)
  parallel::clusterExport(cluster, "LOMM11_WORKER_SELECTED", envir = .GlobalEnv)
  invisible(TRUE)
}

lomm11_runner_write_heldout_panel_result <- function(result, checkpoint, context) {
  additions <- list()
  for (view_result in result$results) {
    unit_key <- view_result$checkpoint$unit_key[[1L]]
    if (unit_key %in% checkpoint$unit_key) next
    additions[[length(additions) + 1L]] <- lomm11_runner_write_heldout_view(
      view_result, context
    )
  }
  additions
}

lomm11_runner_run_heldout <- function(cluster, checkpoint, selected, context,
                                      manifest, started) {
  lomm11_runner_export_selected(cluster, selected)
  panels <- context$plan$heldout_plan.csv
  evaluations <- context$plan$heldout_evaluation_plan.csv
  complete_panel <- vapply(panels$panel_key, function(panel_key) {
    keys <- evaluations$unit_key[evaluations$panel_key == panel_key]
    all(keys %in% checkpoint$unit_key)
  }, logical(1))
  pending <- panels[!complete_panel, , drop = FALSE]
  timeout <- as.numeric(context$config$future_execution$job_timeout_seconds)
  batch_size <- max(1L, context$workers)
  for (batch in lomm11_runner_batches(pending, batch_size)) {
    if (lomm11_runner_deadline_reached(started, timeout)) break
    results <- parallel::parLapplyLB(
      cluster, lomm11_runner_row_tasks(batch), lomm11_runner_worker_heldout
    )
    checkpoint <- lomm11_runner_commit_heldout_batch(
      results, checkpoint, context, nrow(batch) == batch_size
    )
    lomm11_runner_assert_artifact_stability(context)
    manifest <- lomm11_runner_update_manifest(
      manifest, context, checkpoint, "heldout"
    )
  }
  list(
    checkpoint = checkpoint, manifest = manifest,
    complete = all(evaluations$unit_key %in% checkpoint$unit_key)
  )
}

lomm11_runner_rebuild_heldout_metric_chunks <- function(state_directory, units,
                                                         contract) {
  chunk_directory <- file.path(
    state_directory, "heldout_metric_chunks"
  )
  expected_chunks <- vapply(units, function(unit_key) {
    lomm11_runner_metric_chunk_file(state_directory, unit_key)
  }, character(1))
  observed_chunks <- list.files(
    chunk_directory, pattern = "[.]csv$", full.names = TRUE
  )
  if (any(!file.exists(expected_chunks)) ||
      !setequal(normalizePath(observed_chunks), normalizePath(expected_chunks))) {
    stop(
      "Held-out metric chunk set differs from all ",
      format(length(units), big.mark = ","), " planned units.",
      call. = FALSE
    )
  }
  chunks <- lapply(units, function(unit_key) {
    chunk_path <- lomm11_runner_metric_chunk_file(state_directory, unit_key)
    if (!file.exists(chunk_path)) {
      stop("Held-out metric chunk is missing for unit: ", unit_key, call. = FALSE)
    }
    chunk <- lomm11_runner_read_csv_contract(
      chunk_path, contract, "Held-out metric chunk"
    )
    if (nrow(chunk) != 16L || any(chunk$unit_key != unit_key)) {
      stop("Held-out metric chunk violates its unit contract.", call. = FALSE)
    }
    chunk
  })
  output <- lomm11_runner_coerce_frame(
    do.call(rbind, chunks), contract, "Held-out metrics"
  )
  expected_rows <- as.integer(length(units) * 16L)
  if (nrow(output) != expected_rows) {
    stop("Held-out metric chunk rebuild has the wrong row count.", call. = FALSE)
  }
  output
}

lomm11_runner_compare_or_publish_heldout_metrics <- function(output, path) {
  output_directory <- dirname(path)
  canonical <- file.path(
    dirname(output_directory),
    paste0(
      ".", basename(output_directory), ".", basename(path), ".rebuild-",
      Sys.getpid(), ".csv"
    )
  )
  on.exit(if (file.exists(canonical)) unlink(canonical), add = TRUE)
  utils::write.csv(output, canonical, row.names = FALSE, na = "")
  if (file.exists(path)) {
    if (!identical(
      lomm11_runner_sha256(path), lomm11_runner_sha256(canonical)
    )) {
      stop(
        "Held-out metric output differs from the complete per-unit chunk rebuild.",
        call. = FALSE
      )
    }
  } else if (!file.rename(canonical, path)) {
    stop("Failed to publish rebuilt held-out metric output.", call. = FALSE)
  }
  output
}

lomm11_runner_consolidate_heldout_metrics <- function(
    context, state_directory = lomm11_runner_state_directory(context)) {
  contract <- lomm11_runner_locked_output_contract()$heldout_metric_discrepancies.csv
  path <- file.path(context$output_dir, "heldout_metric_discrepancies.csv")
  units <- context$plan$heldout_evaluation_plan.csv$unit_key
  output <- lomm11_runner_rebuild_heldout_metric_chunks(
    state_directory, units, contract
  )
  if (nrow(output) != contract$rows) {
    stop("Held-out metric output does not contain 128,000 rows.", call. = FALSE)
  }
  lomm11_runner_compare_or_publish_heldout_metrics(output, path)
}

lomm11_runner_dataset_count <- function(path, contract, expected_files) {
  files <- list.files(path, pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
  if (length(files) != expected_files) {
    stop("Parquet dataset has the wrong number of unit files: ", path, call. = FALSE)
  }
  first <- arrow::read_parquet(files[[1L]], as_data_frame = FALSE)
  if (!setequal(first$schema$names, names(contract$columns))) {
    stop("Parquet dataset columns differ from the locked schema: ", path, call. = FALSE)
  }
  count <- sum(vapply(files, function(file) {
    as.numeric(arrow::read_parquet(file, as_data_frame = FALSE)$num_rows)
  }, numeric(1)))
  if (length(count) != 1L || !is.finite(count) || count != contract$rows) {
    stop("Parquet dataset row count differs from the locked schema: ", path, call. = FALSE)
  }
  as.integer(count)
}

lomm11_runner_validate_nuisance_outputs <- function(context) {
  contract <- lomm11_runner_locked_output_contract()$fold_nuisance_manifest.csv
  path <- file.path(context$output_dir, "fold_nuisance_manifest.csv")
  manifest <- lomm11_runner_read_csv_contract(path, contract, "Fold nuisance manifest")
  if (nrow(manifest) != 5L || !identical(manifest$fold, 1:5) ||
      any(manifest$n_training_clips != 80L) ||
      any(manifest$config_hash != context$manifest$config_hash) ||
      any(manifest$input_hash != context$input_hash)) {
    stop("Fold nuisance manifest violates its 5-fold provenance contract.", call. = FALSE)
  }
  expected_primary_rows <- 3557L - c(700L, 697L, 706L, 766L, 688L)
  expected_primary_segments <- 188L - c(39L, 36L, 30L, 43L, 40L)
  if (!identical(manifest$primary_rows, expected_primary_rows) ||
      !identical(manifest$primary_segments, expected_primary_segments)) {
    stop("Fold nuisance training row or segment counts are invalid.", call. = FALSE)
  }
  expected_files <- sprintf("fold_%02d.rds", 1:5)
  expected_paths <- file.path("fold_nuisance", expected_files)
  directory <- file.path(context$output_dir, "fold_nuisance")
  if (!identical(sort(list.files(directory)), expected_files) ||
      !identical(as.character(manifest$nuisance_rds_path), expected_paths)) {
    stop("Fold nuisance directory has missing or unexpected files.", call. = FALSE)
  }
  for (index in seq_len(nrow(manifest))) {
    rds_path <- file.path(context$output_dir, manifest$nuisance_rds_path[[index]])
    if (!file.exists(rds_path) ||
        !identical(lomm11_runner_sha256(rds_path), manifest$nuisance_rds_sha256[[index]])) {
      stop("Fold nuisance RDS hash is invalid.", call. = FALSE)
    }
    payload <- readRDS(rds_path)
    fold <- manifest$fold[[index]]
    expected_clips <- sort(as.character(
      context$plan$fold_assignments.csv$clip[
        context$plan$fold_assignments.csv$fold != fold
      ]
    ))
    expected_clip_hash <- lomm11_runner_object_sha256(expected_clips)
    payload_valid <- identical(payload$schema_version, 1L) &&
      identical(payload$fold, fold) &&
      identical(sort(as.character(payload$training_clips)), expected_clips) &&
      identical(payload$n_training_clips, 80L) &&
      identical(payload$primary_rows, manifest$primary_rows[[index]]) &&
      identical(payload$primary_segments, manifest$primary_segments[[index]]) &&
      identical(payload$config_hash, context$manifest$config_hash) &&
      identical(payload$input_hash, context$input_hash) &&
      identical(manifest$training_clips_sha256[[index]], expected_clip_hash)
    if (!payload_valid) {
      stop("Fold nuisance RDS identifiers or training clips are invalid.", call. = FALSE)
    }
    hashes <- c(
      center_sha256 = lomm11_runner_object_sha256(payload$center),
      directions_sha256 = lomm11_runner_object_sha256(payload$directions),
      loadings_sha256 = lomm11_runner_object_sha256(payload$loadings),
      between_clip_covariance_sha256 = lomm11_runner_object_sha256(
        payload$between_clip_covariance
      ),
      metric_scales_sha256 = lomm11_runner_object_sha256(payload$metric_scales)
    )
    if (!identical(
      unname(hashes), unname(as.character(manifest[index, names(hashes)]))
    )) {
      stop("Fold nuisance component hash is invalid.", call. = FALSE)
    }
  }
  manifest
}

lomm11_runner_validate_complete_outputs <- function(context) {
  contracts <- lomm11_runner_locked_output_contract()
  checkpoint <- lomm11_runner_load_checkpoint(context)
  checkpoint <- lomm11_runner_validate_checkpoint(checkpoint, context, complete = TRUE)
  lomm11_runner_validate_heldout_pairing(checkpoint, context)
  lomm11_runner_validate_checkpoint_artifacts(checkpoint, context)
  selected <- lomm11_runner_read_csv_contract(
    file.path(context$output_dir, "selected_candidates.csv"),
    contracts$selected_candidates.csv, "Selected candidates"
  )
  if (nrow(selected) != 40L || !all(selected$selected_before_heldout)) {
    stop("Selected-candidate output violates the training-only selection contract.", call. = FALSE)
  }
  heldout <- lomm11_runner_read_csv_contract(
    file.path(context$output_dir, "heldout_metric_discrepancies.csv"),
    contracts$heldout_metric_discrepancies.csv, "Held-out metrics"
  )
  if (nrow(heldout) != contracts$heldout_metric_discrepancies.csv$rows) {
    stop("Held-out metric output has the wrong row count.", call. = FALSE)
  }
  nuisance <- lomm11_runner_validate_nuisance_outputs(context)
  training_metrics <- lomm11_runner_read_training_metrics(context)
  recomputed <- lomm11_runner_select_candidates(context, training_metrics)
  if (!identical(
    lomm11_runner_object_sha256(selected),
    lomm11_runner_object_sha256(recomputed)
  )) {
    stop("Selected candidates differ from training-only recomputation.", call. = FALSE)
  }
  lomm11_runner_dataset_count(
    file.path(context$output_dir, "training_metric_discrepancies.parquet"),
    contracts$training_metric_discrepancies.parquet, 3240L
  )
  lomm11_runner_dataset_count(
    file.path(context$output_dir, "clip_metric_contributions.parquet"),
    contracts$clip_metric_contributions.parquet, 16000L
  )
  list(
    checkpoint = checkpoint, selected = selected, heldout = heldout,
    nuisance = nuisance
  )
}

lomm11_runner_output_records <- function(context) {
  file_record <- function(name, rows) list(
    path = name,
    sha256 = lomm11_runner_sha256(file.path(context$output_dir, name)),
    rows = as.integer(rows)
  )
  tree_record <- function(name, rows) list(
    path = name,
    sha256 = lomm11_runner_tree_sha256(file.path(context$output_dir, name)),
    rows = as.integer(rows)
  )
  list(
    file_record("fold_nuisance_manifest.csv", 5L),
    tree_record("fold_nuisance", 5L),
    file_record("checkpoint_results.csv", 11240L),
    file_record("selected_candidates.csv", 40L),
    tree_record("training_metric_discrepancies.parquet", 51840L),
    file_record("heldout_metric_discrepancies.csv", 128000L),
    tree_record("clip_metric_contributions.parquet", 21760000L)
  )
}

lomm11_runner_validate_success_receipt <- function(context) {
  path <- file.path(context$output_dir, "_SUCCESS.json")
  success <- lomm11_runner_read_json(path)
  expected_fields <- c(
    "schema_version", "status", "config_hash", "config_prefix",
    "prospective_manifest_sha256", "run_manifest_sha256", "checkpoint_units",
    "planned_panel_attempts", "panels_with_values", "failed_panel_attempts",
    "heldout_panel_values_reused_across_views", "outputs", "completed_at"
  )
  valid_header <- setequal(names(success), expected_fields) &&
    identical(success$schema_version, 1L) && identical(success$status, "complete") &&
    identical(success$config_hash, context$manifest$config_hash) &&
    identical(success$config_prefix, context$manifest$config_prefix) &&
    identical(success$prospective_manifest_sha256, context$manifest_sha256) &&
    identical(success$checkpoint_units, 11240L) &&
    identical(success$planned_panel_attempts, 7240L) &&
    is.integer(success$panels_with_values) &&
    success$panels_with_values >= 0L && success$panels_with_values <= 7240L &&
    identical(
      success$failed_panel_attempts,
      success$planned_panel_attempts - success$panels_with_values
    ) &&
    identical(success$heldout_panel_values_reused_across_views, TRUE)
  if (!valid_header) {
    stop("Completed success receipt has an invalid header.", call. = FALSE)
  }
  manifest_path <- file.path(context$output_dir, "run_manifest.json")
  if (!identical(success$run_manifest_sha256, lomm11_runner_sha256(manifest_path))) {
    stop("Completed run manifest hash differs from the success receipt.", call. = FALSE)
  }
  run_manifest <- lomm11_runner_read_json(manifest_path)
  if (!identical(run_manifest$status, "complete") ||
      !identical(run_manifest$config_hash, context$manifest$config_hash) ||
      !identical(
        run_manifest$prospective_manifest_sha256, context$manifest_sha256
      ) || !identical(run_manifest$progress$checkpoint_units, 11240L)) {
    stop("Completed run manifest header or counts are invalid.", call. = FALSE)
  }
  lomm11_runner_validate_worker_export_identity(run_manifest$worker_export)
  lomm11_runner_validate_execution_preflight(run_manifest$execution_preflight)
  if (!identical(
    run_manifest$software_versions$codetools,
    as.character(utils::packageVersion("codetools"))
  )) {
    stop("Completed run manifest codetools version is invalid.", call. = FALSE)
  }
  checkpoint <- lomm11_runner_load_checkpoint(context)
  checkpoint <- lomm11_runner_validate_checkpoint(checkpoint, context, complete = TRUE)
  lomm11_runner_validate_heldout_pairing(checkpoint, context)
  panels_with_values <- length(unique(
    checkpoint$panel_key[!is.na(checkpoint$panel_hash) & nzchar(checkpoint$panel_hash)]
  ))
  if (!identical(success$panels_with_values, as.integer(panels_with_values))) {
    stop("Success receipt panel-value counts differ from the checkpoint.", call. = FALSE)
  }
  training_metrics <- lomm11_runner_read_training_metrics(context)
  selected_contract <- lomm11_runner_locked_output_contract()$selected_candidates.csv
  selected <- lomm11_runner_read_csv_contract(
    file.path(context$output_dir, "selected_candidates.csv"),
    selected_contract, "Selected candidates"
  )
  recomputed <- lomm11_runner_select_candidates(context, training_metrics)
  if (!identical(
    lomm11_runner_object_sha256(selected),
    lomm11_runner_object_sha256(recomputed)
  )) {
    stop("Completed selections differ from training-only recomputation.", call. = FALSE)
  }
  lomm11_runner_validate_nuisance_outputs(context)
  contracts <- lomm11_runner_locked_output_contract()
  heldout <- lomm11_runner_read_csv_contract(
    file.path(context$output_dir, "heldout_metric_discrepancies.csv"),
    contracts$heldout_metric_discrepancies.csv, "Held-out metrics"
  )
  if (nrow(heldout) != contracts$heldout_metric_discrepancies.csv$rows) {
    stop("Completed held-out metric row count is invalid.", call. = FALSE)
  }
  finalizing <- lomm11_runner_finalizing_directory(context)
  if (dir.exists(finalizing)) {
    lomm11_runner_validate_finalizing_marker(finalizing, context)
    lomm11_runner_consolidate_heldout_metrics(context, finalizing)
  }
  lomm11_runner_dataset_count(
    file.path(context$output_dir, "training_metric_discrepancies.parquet"),
    contracts$training_metric_discrepancies.parquet, 3240L
  )
  lomm11_runner_dataset_count(
    file.path(context$output_dir, "clip_metric_contributions.parquet"),
    contracts$clip_metric_contributions.parquet, 16000L
  )
  observed <- success$outputs
  rebuilt <- lomm11_runner_output_records(context)
  if (!identical(
    lomm11_runner_object_sha256(observed), lomm11_runner_object_sha256(rebuilt)
  )) {
    stop("Completed output hashes differ from the success receipt.", call. = FALSE)
  }
  expected_entries <- sort(c(
    "fold_nuisance", "fold_nuisance_manifest.csv", "checkpoint_results.csv",
    "selected_candidates.csv", "training_metric_discrepancies.parquet",
    "heldout_metric_discrepancies.csv", "clip_metric_contributions.parquet",
    "run_manifest.json", "_SUCCESS.json"
  ))
  if (!identical(
    sort(list.files(context$output_dir, all.files = TRUE, no.. = TRUE)),
    expected_entries
  )) {
    stop("Completed result directory contains unexpected artifacts.", call. = FALSE)
  }
  invisible(success)
}

lomm11_runner_publish_success <- function(context, manifest, validation) {
  outputs <- lomm11_runner_output_records(context)
  manifest <- lomm11_runner_update_manifest(
    manifest, context, validation$checkpoint, "complete", "complete",
    nuisance = list(
      status = "complete",
      fold_hashes = as.list(validation$nuisance$nuisance_rds_sha256)
    ),
    outputs = outputs
  )
  manifest_path <- file.path(context$output_dir, "run_manifest.json")
  panels_with_values <- length(unique(
    validation$checkpoint$panel_key[
      !is.na(validation$checkpoint$panel_hash) &
        nzchar(validation$checkpoint$panel_hash)
    ]
  ))
  success <- list(
    schema_version = 1L,
    status = "complete",
    config_hash = context$manifest$config_hash,
    config_prefix = context$manifest$config_prefix,
    prospective_manifest_sha256 = context$manifest_sha256,
    run_manifest_sha256 = lomm11_runner_sha256(manifest_path),
    checkpoint_units = 11240L,
    planned_panel_attempts = 7240L,
    panels_with_values = as.integer(panels_with_values),
    failed_panel_attempts = as.integer(7240L - panels_with_values),
    heldout_panel_values_reused_across_views = TRUE,
    outputs = outputs,
    completed_at = manifest$completed_at
  )
  pending <- lomm11_runner_pending_success_path(context)
  if (file.exists(pending)) {
    stop("A pending success receipt already exists.", call. = FALSE)
  }
  jsonlite::write_json(
    success, pending, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
  )
  lomm11_runner_move_state_to_finalizing(context)
  success_path <- file.path(context$output_dir, "_SUCCESS.json")
  if (!file.rename(pending, success_path)) {
    stop("Failed to publish the atomic success receipt.", call. = FALSE)
  }
  lomm11_runner_cleanup_finalizing(context)
  manifest
}

lomm11_runner_dry_run_report <- function(context) {
  plan <- context$plan
  list(
    schema_version = 1L,
    status = "validated_dry_run",
    plan_directory = context$plan_dir,
    prospective_result_directory = context$output_dir,
    config_hash = context$manifest$config_hash,
    prospective_manifest_sha256 = context$manifest_sha256,
    counts = list(
      candidates = nrow(plan$candidate_grid.csv),
      training_panels = nrow(plan$training_plan.csv),
      selections = nrow(plan$selection_plan.csv),
      heldout_panels = nrow(plan$heldout_plan.csv),
      heldout_view_evaluations = nrow(plan$heldout_evaluation_plan.csv),
      checkpoint_units = 11240L,
      fold_nuisance_artifacts = 5L
    ),
    execution_guards = list(
      execute_flag_required = TRUE,
      resume_also_requires_execute = TRUE,
      acknowledgement_token = LOMM11_RUNNER_TOKEN,
      exact_full_prospective_manifest_sha256_required = TRUE,
      arrow_required_only_in_execute_mode = TRUE,
      automatic_progression = FALSE
    ),
    software_versions = list(
      codetools = as.character(utils::packageVersion("codetools"))
    ),
    prospective_worker_export = lomm11_runner_worker_export_identity(),
    execute_preflight_contract = list(
      arrow_zstd_roundtrip_before_result_mutation = TRUE,
      one_worker_psock_exact_transition_before_result_mutation = TRUE,
      caller_rng_preserved = TRUE
    ),
    execution_state = list(
      simulation_executed = FALSE,
      rng_called = FALSE,
      psock_backend_started = FALSE,
      arrow_loaded = FALSE,
      tmux_started = FALSE
    ),
    future_launch_command = paste(
      "Rscript scripts/lomm_1_1/run_realism_ablation.R",
      "--plan-dir", shQuote(context$plan_dir), "--execute",
      "--ack-long-running", LOMM11_RUNNER_TOKEN,
      "--prospective-manifest-sha256", context$manifest_sha256
    ),
    future_resume_command = paste(
      "Rscript scripts/lomm_1_1/run_realism_ablation.R",
      "--plan-dir", shQuote(context$plan_dir), "--execute --resume",
      "--ack-long-running", LOMM11_RUNNER_TOKEN,
      "--prospective-manifest-sha256", context$manifest_sha256
    )
  )
}

lomm11_runner_execute <- function(context, workers, preflight) {
  lomm11_runner_validate_execution_preflight(preflight)
  context$workers <- as.integer(workers)
  lock <- lomm11_runner_acquire_lock(context)
  on.exit(lomm11_runner_release_lock(lock, context), add = TRUE)
  lomm11_runner_set_thread_limits()
  manifest <- lomm11_runner_initialize_result(context, workers, preflight)
  if (file.exists(file.path(context$output_dir, "_SUCCESS.json"))) {
    lomm11_runner_validate_success_receipt(context)
    lomm11_runner_cleanup_finalizing(context)
    return(list(complete = TRUE, manifest = manifest))
  }
  lomm11_runner_restore_finalizing_state(context)
  started <- Sys.time()
  checkpoint <- lomm11_runner_load_checkpoint(context)
  lomm11_runner_validate_checkpoint_artifacts(checkpoint, context)
  caches <- lomm11_runner_build_fold_caches(context)
  nuisance <- lomm11_runner_write_nuisance(caches, context)
  manifest <- lomm11_runner_update_manifest(
    manifest, context, checkpoint, "preparing", nuisance = list(
      status = "complete", fold_hashes = as.list(nuisance$nuisance_rds_sha256)
    )
  )
  lomm11_runner_assert_artifact_stability(context)
  cluster <- lomm11_runner_start_cluster(workers, caches, context)
  on.exit(lomm11_runner_stop_cluster(cluster), add = TRUE)
  training <- lomm11_runner_run_training(
    cluster, checkpoint, context, manifest, started
  )
  checkpoint <- training$checkpoint
  manifest <- training$manifest
  if (!training$complete) {
    manifest <- lomm11_runner_update_manifest(
      manifest, context, checkpoint, "training", "partial"
    )
    return(list(complete = FALSE, stage = "training", manifest = manifest))
  }
  training_metrics <- lomm11_runner_read_training_metrics(context)
  selected <- lomm11_runner_publish_selected(
    lomm11_runner_select_candidates(context, training_metrics), context
  )
  manifest <- lomm11_runner_update_manifest(
    manifest, context, checkpoint, "selection"
  )
  heldout <- lomm11_runner_run_heldout(
    cluster, checkpoint, selected, context, manifest, started
  )
  checkpoint <- heldout$checkpoint
  manifest <- heldout$manifest
  if (!heldout$complete) {
    manifest <- lomm11_runner_update_manifest(
      manifest, context, checkpoint, "heldout", "partial"
    )
    return(list(complete = FALSE, stage = "heldout", manifest = manifest))
  }
  lomm11_runner_consolidate_heldout_metrics(context)
  validation <- lomm11_runner_validate_complete_outputs(context)
  manifest <- lomm11_runner_publish_success(context, manifest, validation)
  lomm11_runner_assert_artifact_stability(context)
  list(complete = TRUE, stage = "complete", manifest = manifest)
}

lomm11_runner_main <- function() {
  args <- lomm11_runner_parse_args(commandArgs(trailingOnly = TRUE))
  if (isTRUE(args$help)) {
    cat(lomm11_runner_usage(), "\n")
    return(invisible(NULL))
  }
  context <- lomm11_runner_load_context(args)
  if (identical(args$mode, "dry-run")) {
    report <- lomm11_runner_dry_run_report(context)
    cat(jsonlite::toJSON(
      report, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
    ), "\n")
    return(invisible(report))
  }
  workers <- lomm11_runner_validate_execution_authority(context)
  preflight <- lomm11_runner_execution_preflight()
  lomm11_runner_assert_artifact_stability(context)
  result <- lomm11_runner_execute(context, workers, preflight)
  cat(jsonlite::toJSON(
    list(
      complete = result$complete,
      stage = if (is.null(result$stage)) "complete" else result$stage,
      result_directory = context$output_dir,
      checkpoint_units = result$manifest$progress$checkpoint_units,
      resume_required = !isTRUE(result$complete)
    ),
    pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
  ), "\n")
  invisible(result)
}

LOMM11_RUNNER_WORKER_FUNCTION_ROOTS <-
  lomm11_runner_capture_worker_function_roots(.GlobalEnv)

if (sys.nframe() == 0L) {
  lomm11_runner_main()
}
