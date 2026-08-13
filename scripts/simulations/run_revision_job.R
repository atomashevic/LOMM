#!/usr/bin/env Rscript

revision_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) != 1L) {
    stop("Cannot locate run_revision_job.R.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
}

REVISION_RUNNER_PATH <- revision_script_path()
REVISION_REPO_ROOT <- normalizePath(
  file.path(dirname(REVISION_RUNNER_PATH), "..", ".."),
  mustWork = TRUE
)
source(file.path(REVISION_REPO_ROOT, "scripts", "simulations", "revision_spec.R"))

revision_require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required by the revision dispatcher.", call. = FALSE)
  }
}

revision_parse_cli <- function(args) {
  parsed <- list(job = NULL, config = NULL, dry_run = FALSE)
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (identical(token, "--dry-run")) {
      parsed$dry_run <- TRUE
      index <- index + 1L
      next
    }
    if (token %in% c("--job", "--config")) {
      if (index == length(args)) {
        stop("Missing value after ", token, ".", call. = FALSE)
      }
      parsed[[sub("^--", "", token)]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    stop("Unknown argument: ", token, call. = FALSE)
  }
  if (is.null(parsed$job) || is.null(parsed$config)) {
    stop("Usage: run_revision_job.R --job JOB --config FILE [--dry-run]", call. = FALSE)
  }
  parsed
}

revision_absolute_path <- function(path) {
  if (grepl("^/", path)) {
    return(path)
  }
  file.path(REVISION_REPO_ROOT, path)
}

revision_read_config <- function(path) {
  revision_require_jsonlite()
  absolute_path <- revision_absolute_path(path)
  if (!file.exists(absolute_path)) {
    stop("Revision config does not exist: ", absolute_path, call. = FALSE)
  }
  config <- jsonlite::fromJSON(absolute_path, simplifyVector = FALSE)
  if (!is.list(config) || !is.list(config$defaults) || !is.list(config$jobs)) {
    stop("Revision config must contain defaults and jobs objects.", call. = FALSE)
  }
  list(value = config, path = normalizePath(absolute_path, mustWork = TRUE))
}

revision_config_job <- function(config, job_id) {
  indices <- which(vapply(
    config$jobs,
    function(job) identical(job$id, job_id),
    logical(1)
  ))
  if (length(indices) != 1L) {
    stop("Config must contain exactly one job named ", job_id, ".", call. = FALSE)
  }
  config$jobs[[indices]]
}

revision_validate_dynega_config <- function(job_id, job) {
  if (!grepl("dynega", job_id, fixed = TRUE)) {
    return(invisible(TRUE))
  }
  configured_delta <- job$parameters$dynega_observation_delta
  if (length(configured_delta) != 1L ||
      !is.numeric(configured_delta) ||
      !is.finite(configured_delta) ||
      configured_delta != REVISION_DYNEGA_OBSERVATION_DELTA) {
    stop(
      "DynEGA jobs must configure dynega_observation_delta = ",
      REVISION_DYNEGA_OBSERVATION_DELTA,
      ".",
      call. = FALSE
    )
  }
  configured_units <- job$parameters$dynega_delta_units
  if (length(configured_units) != 1L ||
      !identical(configured_units, REVISION_DYNEGA_DELTA_UNITS)) {
    stop(
      "DynEGA jobs must configure dynega_delta_units = ",
      REVISION_DYNEGA_DELTA_UNITS,
      ".",
      call. = FALSE
    )
  }
  configured_physical_dt <- job$parameters$physical_dt
  if (length(configured_physical_dt) != 1L ||
      !is.numeric(configured_physical_dt) ||
      !is.finite(configured_physical_dt) ||
      configured_physical_dt != REVISION_STRUCTURE_PHYSICAL_DT) {
    stop(
      "DynEGA jobs must configure physical_dt = ",
      REVISION_STRUCTURE_PHYSICAL_DT,
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_validate_empirical_config <- function(job_id, job) {
  if (!identical(job_id, "empirical_cv")) {
    return(invisible(TRUE))
  }
  ncores <- job$parameters$frozen_dynega_ncores
  if (length(ncores) != 1L ||
      !is.numeric(ncores) ||
      !is.finite(ncores) ||
      ncores != 1) {
    stop(
      "Empirical CV must configure frozen_dynega_ncores = 1.",
      call. = FALSE
    )
  }
  backend <- job$parameters$outer_parallel_backend
  if (length(backend) != 1L || !identical(backend, "psock")) {
    stop(
      "Empirical CV must configure outer_parallel_backend = psock.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_validate_publication_config <- function(job_id, job) {
  if (!identical(job_id, "rebuild_figures_tables")) {
    return(invisible(TRUE))
  }
  revision <- job$parameters$publication_schema_revision
  if (length(revision) != 1L || !is.numeric(revision) ||
      !is.finite(revision) || revision != 4) {
    stop(
      "Figure/table rebuild must configure publication_schema_revision = 4.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_expected_counts <- function(job) {
  names_to_keep <- grep("^expected_", names(job), value = TRUE)
  counts <- job[names_to_keep]
  if (length(counts) == 0L || is.null(counts$expected_units)) {
    stop("Job config must declare expected_units.", call. = FALSE)
  }
  invalid <- !vapply(
    counts,
    function(value) length(value) == 1L && is.numeric(value) && value > 0,
    logical(1)
  )
  if (any(invalid)) {
    stop("Expected counts must be positive numeric scalars.", call. = FALSE)
  }
  lapply(counts, as.integer)
}

revision_env_inputs <- function() {
  value <- Sys.getenv("REVISION_JOB_INPUTS_JSON", "{}")
  revision_require_jsonlite()
  parsed <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  if (!is.list(parsed)) {
    stop("REVISION_JOB_INPUTS_JSON must be a JSON object.", call. = FALSE)
  }
  parsed
}

revision_env_numeric <- function(name, fallback) {
  raw <- Sys.getenv(name, "")
  value <- if (nzchar(raw)) suppressWarnings(as.numeric(raw)) else as.numeric(fallback)
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    return(as.numeric(fallback))
  }
  value
}

revision_runtime_context <- function(job_id, job, config, config_path) {
  env_job <- Sys.getenv("REVISION_JOB_ID", job_id)
  if (!identical(env_job, job_id)) {
    stop("REVISION_JOB_ID does not match --job.", call. = FALSE)
  }
  config_hash <- Sys.getenv(
    "REVISION_JOB_CONFIG_HASH",
    revision_default_config_hash(job_id)
  )
  if (!grepl("^[a-f0-9]{12}$", config_hash)) {
    stop("REVISION_JOB_CONFIG_HASH must contain 12 lowercase hexadecimal characters.", call. = FALSE)
  }
  default_output <- file.path(
    REVISION_REPO_ROOT,
    config$defaults$output_root,
    job_id,
    config_hash
  )
  output_dir <- Sys.getenv("REVISION_JOB_OUTPUT_DIR", default_output)
  if (!identical(normalizePath(dirname(output_dir), mustWork = FALSE),
                 normalizePath(dirname(default_output), mustWork = FALSE)) ||
      !identical(basename(output_dir), basename(default_output))) {
    stop("REVISION_JOB_OUTPUT_DIR differs from the locked hashed output directory.", call. = FALSE)
  }
  workers <- suppressWarnings(as.integer(Sys.getenv(
    "REVISION_JOB_WORKERS",
    as.character(job$workers)
  )))
  if (!is.finite(workers) || workers < 1L || workers > 24L) {
    stop("REVISION_JOB_WORKERS must be between 1 and 24.", call. = FALSE)
  }
  list(
    job_id = job_id,
    config_hash = config_hash,
    output_dir = output_dir,
    workers = workers,
    job_timeout_seconds = revision_env_numeric(
      "REVISION_JOB_TIMEOUT_SECONDS",
      revision_scalar_or(job$contract$job_timeout_seconds, 900)
    ),
    per_unit_timeout_seconds = revision_env_numeric(
      "REVISION_JOB_PER_UNIT_TIMEOUT_SECONDS",
      revision_scalar_or(
        job$contract$per_unit_timeout_seconds,
        revision_scalar_or(job$contract$job_timeout_seconds, 900)
      )
    ),
    budget_seconds = revision_env_numeric(
      "REVISION_JOB_BUDGET_SECONDS",
      revision_scalar_or(
        job$contract$budget_seconds,
        revision_scalar_or(job$contract$job_timeout_seconds, 900)
      )
    ),
    resume = identical(Sys.getenv("REVISION_JOB_RESUME", "0"), "1"),
    validate = identical(Sys.getenv("REVISION_JOB_VALIDATE", "0"), "1"),
    inputs = revision_env_inputs(),
    config_path = config_path
  )
}

revision_write_json <- function(value, path) {
  revision_require_jsonlite()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value,
    temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  if (!file.rename(temporary, path)) {
    stop("Could not atomically write ", path, ".", call. = FALSE)
  }
  invisible(path)
}

revision_detect_version <- function(package) {
  installed <- utils::installed.packages()
  if (!package %in% rownames(installed)) {
    return("not_installed")
  }
  as.character(installed[package, "Version"])
}

revision_relevant_packages <- function(job_id) {
  packages <- c("jsonlite")
  if (grepl("dynega", job_id) || identical(job_id, "empirical_cv")) {
    packages <- c(packages, "EGAnet", "clue", "igraph")
  }
  if (grepl("graphicalvar", job_id)) {
    packages <- c(packages, "graphicalVAR", "clue", "igraph")
  }
  if (grepl("gimme", job_id)) {
    packages <- c(packages, "gimme", "clue", "igraph")
  }
  if (grepl("static_oracle", job_id)) {
    packages <- c(packages, "EGAnet", "clue", "igraph")
  }
  unique(packages)
}

revision_software_versions <- function(job_id) {
  packages <- revision_relevant_packages(job_id)
  package_versions <- setNames(
    lapply(packages, revision_detect_version),
    packages
  )
  list(
    R = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    packages = package_versions
  )
}

revision_parameter_summary <- function(job_id, plan) {
  if (identical(job_id, "empirical_cv")) {
    axes <- revision_empirical_calibration_axes()
    return(list(
      folds = 5L,
      candidate_count = 900L,
      calibration_regimes = c("unconstrained", "stable_negative_zeta"),
      eta = axes$eta,
      zeta = list(
        unconstrained = axes$unconstrained_zeta,
        stable_negative_zeta = axes$stable_negative_zeta
      ),
      sigma_q = axes$sigma_q,
      dt = axes$dt,
      tau = axes$tau,
      channel_intercepts = "estimated_from_training_fold_only",
      alpha = "selected_or_fixed_from_training_fold_only",
      summarizer = paste(
        "training-frozen six-metric mean of scaled absolute median",
        "difference and one-dimensional Wasserstein distance"
      ),
      heldout_access_during_selection = FALSE,
      clips_per_fold = 20L,
      training_clips_per_fold = 80L,
      heldout_clips_per_fold = 20L,
      training_evaluations_per_fold = 2700L,
      heldout_replicates_per_fold = 100L
    ))
  }
  if (grepl("^t(25|100)_", job_id)) {
    panels <- plan$panels
    return(list(
      scenario_families = unique(panels$scenario_family),
      source_dimensions = sort(unique(panels$scenario_nfac)),
      N = sort(unique(panels$N)),
      TimeP = unique(panels$TimeP),
      NVAR = unique(panels$NVAR),
      Error_sd = sort(unique(panels$Error_sd)),
      Loadings = sort(unique(panels$Loadings)),
      Q = unique(panels$Q),
      replications = max(panels$rep),
      dt = unique(panels$dt),
      dynega_observation_delta = REVISION_DYNEGA_OBSERVATION_DELTA,
      dynega_delta_units = REVISION_DYNEGA_DELTA_UNITS,
      physical_dt = unique(panels$dt),
      n_embed_candidates = unique(panels$n_embed_candidates),
      community_algorithms = REVISION_COMMUNITY_ALGORITHMS,
      stability_classes = sort(unique(panels$stability_class)),
      partition_source = "one_frozen_structure_per_method_panel"
    ))
  }
  if (grepl("^challenge_", job_id)) {
    panels <- plan$panels
    return(list(
      challenge_variants = split(
        revision_challenge_variants(),
        revision_challenge_variants()$challenge_id
      ),
      scenario_families = unique(panels$scenario_family),
      representations = sort(unique(panels$representation)),
      N = 100L,
      TimeP = 100L,
      NVAR = 4L,
      Error_sd = 0.25,
      Loadings = 0.60,
      Q = 0.75,
      replications = 50L,
      dynega_observation_delta = REVISION_DYNEGA_OBSERVATION_DELTA,
      dynega_delta_units = REVISION_DYNEGA_DELTA_UNITS,
      physical_dt = unique(panels$dt),
      community_algorithms = REVISION_COMMUNITY_ALGORITHMS
    ))
  }
  list(analysis = plan$job$runner_kind[[1]])
}

revision_required_inputs <- function(job_id, job, context) {
  external <- unlist(
    job$contract$external_required_inputs,
    use.names = FALSE
  )
  resolved <- external
  if (identical(job_id, "empirical_cv")) {
    resolved <- Sys.getenv(
      "REVISION_EMPIRICAL_FRAME_SCORES",
      if (length(external) == 0L) {
        "required_at_execution:MAFW-derived frame_scores.csv"
      } else {
        external[[1L]]
      }
    )
  }
  list(
    external_artifacts = as.list(external),
    resolved_external_artifacts = as.list(resolved),
    dependency_outputs = context$inputs
  )
}

revision_expected_outputs <- function(job_id) {
  if (identical(job_id, "empirical_cv")) {
    return(c(
      "checkpoint_results.csv",
      "results.csv",
      "fold_balance.csv",
      "clip_contributions.csv",
      "run_manifest.json",
      "_SUCCESS.json"
    ))
  }
  if (identical(job_id, "t100_reuse_audit")) {
    return(c(
      "reuse_audit.json",
      "checkpoint_results.csv",
      "run_manifest.json",
      "_SUCCESS.json"
    ))
  }
  if (identical(job_id, "aggregate_results")) {
    return(c(
      "revision_estimand_spec.csv",
      "upstream_artifact_manifest.csv",
      "revision_results_long.csv",
      "identity_validation.csv",
      "partition_metric_recomputation_audit.csv",
      "revision_estimand_results.csv",
      "revision_paired_differences.csv",
      "checkpoint_results.csv",
      "run_manifest.json",
      "_SUCCESS.json"
    ))
  }
  if (identical(job_id, "rebuild_figures_tables")) {
    return(c(
      "checkpoint_results.csv",
      "results.csv",
      "publication_artifacts.json",
      "run_manifest.json",
      "_SUCCESS.json"
    ))
  }
  if (identical(job_id, "integrate_manuscript")) {
    return(c(
      "checkpoint_results.csv",
      "results.csv",
      "manuscript_artifacts.json",
      "run_manifest.json",
      "_SUCCESS.json"
    ))
  }
  c("checkpoint_results.csv", "results.csv", "run_manifest.json", "_SUCCESS.json")
}

revision_assert_plan_counts <- function(job_id, plan, expected_counts) {
  if (identical(job_id, "empirical_cv")) {
    actual <- list(
      expected_train_units = nrow(plan$panels$training),
      expected_heldout_units = nrow(plan$panels$heldout),
      expected_units = nrow(plan$panels$training) + nrow(plan$panels$heldout)
    )
  } else if (identical(job_id, "t100_reuse_audit")) {
    actual <- list(
      expected_panels = nrow(plan$panels),
      expected_units = nrow(plan$panels)
    )
  } else if (!is.null(plan$panels)) {
    base_panels <- length(unique(plan$panels$panel_key))
    representation_panels <- nrow(plan$panels)
    readouts <- representation_panels * nrow(plan$readouts)
    actual <- list(expected_units = readouts)
    if (!is.null(expected_counts$expected_panels)) {
      actual$expected_panels <- representation_panels
    }
    if (!is.null(expected_counts$expected_base_panels)) {
      actual$expected_base_panels <- base_panels
    }
    if (!is.null(expected_counts$expected_representations)) {
      actual$expected_representations <- representation_panels
    }
    if (!is.null(expected_counts$expected_method_panels)) {
      actual$expected_method_panels <- representation_panels * length(plan$methods)
    }
    if (!is.null(expected_counts$expected_readouts)) {
      actual$expected_readouts <- readouts
    }
  } else {
    actual <- list(expected_units = 1L)
  }
  for (name in names(expected_counts)) {
    if (is.null(actual[[name]]) || !identical(as.integer(actual[[name]]), expected_counts[[name]])) {
      stop(
        job_id, " planned ", name, " does not match the job config.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

revision_manifest_counts <- function(plan, expected_counts) {
  counts <- expected_counts
  counts$expected_base_panels <- as.integer(if (is.null(plan$panels)) {
    0L
  } else if (is.list(plan$panels) && !is.data.frame(plan$panels)) {
    nrow(plan$panels$training) + nrow(plan$panels$heldout)
  } else {
    length(unique(plan$panels$panel_key))
  })
  counts$expected_representation_panels <- as.integer(if (is.null(plan$panels)) {
    0L
  } else if (is.list(plan$panels) && !is.data.frame(plan$panels)) {
    nrow(plan$panels$training) + nrow(plan$panels$heldout)
  } else {
    nrow(plan$panels)
  })
  counts$expected_structure_fits <- as.integer(plan$job$structure_fits[[1]])
  counts$expected_readouts <- as.integer(plan$job$community_readouts[[1]])
  counts
}

revision_output_schema <- function(job_id) {
  if (identical(job_id, "empirical_cv")) {
    return(list(
      results = c(
        "unit_key", "panel_key", "stage", "status", "fold", "candidate_id",
        "heldout_rep", "calibration_regime", "eta", "zeta", "sigma_q", "dt",
        "tau", "continuous_loss", "secondary_frozen_dynega_loss", "seed_base",
        "config_hash", "generator_hash", "input_spec_hash", "input_data_hash",
        "panel_input_hash", "frozen_summary_hash", "stability_class",
        "min_observed_horizon", "mean_observed_horizon", "max_observed_horizon"
      ),
      clip_contributions = c(
        "contribution_key", "fold", "heldout_rep", "clip", "unit", "metric", "family",
        "empirical", "synthetic", "scale", "signed_discrepancy",
        "absolute_discrepancy"
      ),
      fold_balance = c(
        "fold", "truth_label", "clip_count", "min_length", "mean_length",
        "max_length"
      )
    ))
  }
  if (grepl("^(t25|t100|challenge)_", job_id) &&
      !identical(job_id, "t100_reuse_audit")) {
    return(list(results = c(
      "unit_key", "panel_key", "representation_key", "structure_fit_key",
      "structure_fit_status", "structure_hash", "method_id",
      "community_algorithm", "status", "true_membership", "estimated_membership",
      "n_dim_true", "n_dim_hat", "n_dim_exact", "assignment_accuracy",
      "adjusted_rand_index", "variation_information", "score_cor_common",
      "score_nrmse_common", "structure_fit_runtime_sec", "readout_runtime_sec",
      "runtime_sec", "completed_within_900", "design_range_status", "seed_base",
      "config_hash", "generator_hash", "input_spec_hash", "panel_input_hash",
      "dt", "dynega_delta", "dynega_delta_units", "n_transitions",
      "observed_horizon", "stability_class"
    )))
  }
  if (identical(job_id, "t100_reuse_audit")) {
    return(list(
      checkpoint = c(
        "unit_key", "panel_key", "seed_base", "stage", "status",
        "graphicalvar_present", "gimme_present"
      ),
      reuse_audit = c(
        "job_id", "config_hash", "canonical_panel_count",
        "canonical_panel_keys_valid", "canonical_seed_origin", "methods"
      )
    ))
  }
  list(checkpoint = c("unit_key", "panel_key", "stage", "status", "artifact"))
}

revision_plan_seed_rows <- function(job_id, plan) {
  if (identical(job_id, "empirical_cv")) {
    return(rbind(
      plan$panels$training[, c("panel_key", "seed_base"), drop = FALSE],
      plan$panels$heldout[, c("panel_key", "seed_base"), drop = FALSE]
    ))
  }
  if (is.data.frame(plan$panels)) {
    return(plan$panels[, c("panel_key", "seed_base"), drop = FALSE])
  }
  data.frame(
    panel_key = paste(job_id, "singleton", sep = "|"),
    seed_base = revision_seed_from_key(paste(job_id, "singleton", sep = "|")),
    stringsAsFactors = FALSE
  )
}

revision_seed_summary <- function(job_id, plan) {
  rows <- revision_plan_seed_rows(job_id, plan)
  first_indices <- seq_len(min(3L, nrow(rows)))
  last_indices <- seq.int(max(1L, nrow(rows) - 2L), nrow(rows))
  list(
    rule = "seed = 1 + (origin + stable_base131_hash(pair_key)) mod 2147483646",
    seed_origin = REVISION_SEED_ORIGIN,
    total_enumerated = nrow(rows),
    unique_panel_keys = length(unique(rows$panel_key)),
    unique_seeds = length(unique(rows$seed_base)),
    enumeration_hash = revision_object_sha256(rows),
    first_samples = split(rows[first_indices, , drop = FALSE], first_indices),
    last_samples = split(rows[last_indices, , drop = FALSE], last_indices)
  )
}

revision_write_prospective_manifests <- function(job_id,
                                                 job,
                                                 config,
                                                 context,
                                                 plan,
                                                 expected_counts) {
  revision_validate_job_plan(plan)
  revision_assert_plan_counts(job_id, plan, expected_counts)
  manifest_counts <- revision_manifest_counts(plan, expected_counts)
  command <- unlist(job$command, use.names = FALSE)
  grid_specification <- revision_parameter_summary(job_id, plan)
  input_spec_hash <- revision_object_sha256(list(
    job_id = job_id,
    config_hash = context$config_hash,
    configured = job$parameters,
    contract = job$contract,
    prospective_grid = grid_specification,
    expected_counts = expected_counts,
    seed_origin = REVISION_SEED_ORIGIN
  ))
  generator_hash <- context$generator_hash
  output_schema <- revision_output_schema(job_id)
  seed_summary <- revision_seed_summary(job_id, plan)
  prospective <- list(
    status = "planned",
    dry_run = TRUE,
    job_id = job_id,
    config_hash = context$config_hash,
    command = command,
    parameters = list(
      configured = job$parameters,
      contract = job$contract,
      prospective_grid = grid_specification
    ),
    input_spec_hash = input_spec_hash,
    generator_hash = generator_hash,
    output_schema = output_schema,
    seed_enumeration = seed_summary,
    seed = list(
      origin = REVISION_SEED_ORIGIN,
      rule = "seed = 1 + (origin + stable_base131_hash(pair_key)) mod 2147483646",
      panel_keys_are_method_independent = TRUE,
      matched_horizons_share_pair_keys = TRUE,
      challenge_variants_share_pair_keys = TRUE
    ),
    required_inputs = revision_required_inputs(job_id, job, context),
    expected_outputs = revision_expected_outputs(job_id),
    software_versions = revision_software_versions(job_id),
    config_path = context$config_path,
    expected_counts = expected_counts
  )
  dry_manifest <- c(
    list(
      status = "planned",
      dry_run = TRUE,
      job_id = job_id,
      config_hash = context$config_hash,
      input_spec_hash = input_spec_hash,
      generator_hash = generator_hash,
      output_schema = output_schema,
      seed_enumeration = seed_summary,
      parameters = grid_specification,
      command = command,
      expected_counts = manifest_counts
    ),
    manifest_counts
  )
  dir.create(context$output_dir, recursive = TRUE, showWarnings = FALSE)
  revision_write_json(
    dry_manifest,
    file.path(context$output_dir, config$defaults$dry_run_manifest)
  )
  revision_write_json(
    prospective,
    file.path(context$output_dir, config$defaults$prospective_manifest)
  )
  forbidden <- file.path(context$output_dir, config$defaults$success_marker)
  if (file.exists(forbidden) && context$validate) {
    stop("A dry-run validation directory already contains _SUCCESS.json.", call. = FALSE)
  }
  invisible(list(dry = dry_manifest, prospective = prospective))
}

revision_sha256 <- function(path) {
  if (!file.exists(path)) {
    stop("Cannot hash missing file: ", path, call. = FALSE)
  }
  output <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("sha256sum failed for ", path, ".", call. = FALSE)
  }
  strsplit(output[[1]], "[[:space:]]+")[[1]][[1]]
}

revision_object_sha256 <- function(value) {
  path <- tempfile("revision-object-", fileext = ".rds")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  saveRDS(value, path, version = 3)
  revision_sha256(path)
}

revision_generator_hash <- function() {
  paths <- c(
    file.path(REVISION_REPO_ROOT, "scripts", "simulations", "revision_spec.R"),
    file.path(REVISION_REPO_ROOT, "scripts", "simulations", "run_revision_job.R"),
    file.path(REVISION_REPO_ROOT, "scripts", "extract.r")
  )
  revision_object_sha256(setNames(vapply(paths, revision_sha256, character(1)), paths))
}

revision_write_run_manifest <- function(context, config, status, details = list()) {
  job <- revision_config_job(config, context$job_id)
  value <- c(
    list(
      job_id = context$job_id,
      config_hash = context$config_hash,
      status = status,
      command = unlist(job$command, use.names = FALSE),
      dry_run = FALSE,
      started_or_updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      workers = context$workers,
      job_timeout_seconds = context$job_timeout_seconds,
      per_unit_timeout_seconds = context$per_unit_timeout_seconds,
      budget_seconds = context$budget_seconds,
      resume = context$resume,
      input_directories = context$inputs,
      software_versions = revision_software_versions(context$job_id)
    ),
    details
  )
  path <- file.path(context$output_dir, config$defaults$run_manifest)
  revision_write_json(value, path)
  invisible(value)
}

revision_checkpoint_path <- function(context, config) {
  file.path(context$output_dir, config$defaults$checkpoint_file)
}

revision_read_checkpoint <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  rows <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"unit_key" %in% names(rows)) {
    stop("Checkpoint is missing unit_key: ", path, call. = FALSE)
  }
  if (anyDuplicated(rows$unit_key)) {
    stop("Checkpoint contains duplicate unit_key values: ", path, call. = FALSE)
  }
  rows
}

revision_align_checkpoint_columns <- function(rows, path) {
  if (!file.exists(path)) {
    return(rows)
  }
  header <- names(utils::read.csv(
    path,
    nrows = 0L,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))
  if (anyDuplicated(header) || anyDuplicated(names(rows))) {
    stop(
      "Checkpoint append schema contains duplicate column names.",
      call. = FALSE
    )
  }
  missing <- setdiff(header, names(rows))
  extra <- setdiff(names(rows), header)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(
      "Checkpoint append schema differs from the existing header; missing: ",
      paste(missing, collapse = ", "),
      "; extra: ",
      paste(extra, collapse = ", "),
      call. = FALSE
    )
  }
  rows[, header, drop = FALSE]
}

revision_append_checkpoint <- function(rows, path) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) {
    return(invisible(path))
  }
  append <- file.exists(path)
  if (append) {
    rows <- revision_align_checkpoint_columns(rows, path)
  }
  utils::write.table(
    rows,
    path,
    sep = ",",
    row.names = FALSE,
    col.names = !append,
    append = append,
    quote = TRUE,
    na = ""
  )
  invisible(path)
}

revision_assert_fresh_or_resume <- function(context, checkpoint_path) {
  if (file.exists(checkpoint_path) && !context$resume) {
    stop(
      "Checkpoint already exists. Relaunch with the orchestrator resume command.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_configured_result_paths <- function(method) {
  base <- file.path(
    REVISION_REPO_ROOT,
    "simulation_outputs",
    "final_simulation_results"
  )
  directory <- switch(
    method,
    graphicalvar = "graphicalvar_bounded_nvar4",
    gimme = "gimme_bounded_nvar4",
    stop("Unknown reuse-audit method: ", method, call. = FALSE)
  )
  list(
    results = file.path(base, directory, "benchmark_results.csv"),
    grid = file.path(base, directory, "benchmark_grid_main_full.csv"),
    metadata = file.path(base, directory, "benchmark_metadata.txt")
  )
}

revision_signature_key <- function(data) {
  paste(
    data$scenario,
    data$N,
    data$TimeP,
    data$NVAR,
    sprintf("%.3f", data$Error_sd),
    sprintf("%.3f", data$Loadings),
    sprintf("%.2f", data$Q),
    data$rep,
    sep = "|"
  )
}

revision_audit_prior_method <- function(method, canonical) {
  paths <- revision_configured_result_paths(method)
  required_schema <- c(
    "scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "Q", "rep",
    "method_family", "community_algorithm", "status", "n_dim_hat",
    "n_dim_exact", "assignment_accuracy", "score_cor_common",
    "score_nrmse_common", "runtime_sec", "config_hash", "generator_hash",
    "input_spec_hash", "panel_input_hash"
  )
  missing_files <- names(paths)[!vapply(paths, file.exists, logical(1))]
  if (length(missing_files) > 0L) {
    return(list(
      reusable = FALSE,
      reason = paste("missing files:", paste(missing_files, collapse = ", ")),
      checks = list(files_present = FALSE)
    ))
  }
  grid <- utils::read.csv(paths$grid, check.names = FALSE, stringsAsFactors = FALSE)
  results <- utils::read.csv(paths$results, check.names = FALSE, stringsAsFactors = FALSE)
  schema_ok <- all(required_schema %in% names(results))
  reporting_schema_ok <- all(c(
    "adjusted_rand_index",
    "variation_information"
  ) %in% names(results)) || all(c(
    "true_membership",
    "estimated_membership"
  ) %in% names(results))
  canonical_signatures <- sort(unique(revision_signature_key(canonical)))
  grid_signatures <- sort(unique(revision_signature_key(grid)))
  keys_ok <- identical(canonical_signatures, grid_signatures)
  algorithms <- sort(unique(results$community_algorithm))
  partition_ok <- identical(algorithms, sort(REVISION_COMMUNITY_ALGORITHMS)) &&
    nrow(results) == nrow(canonical) * length(REVISION_COMMUNITY_ALGORITHMS)
  results_panels_ok <- identical(
    canonical_signatures,
    sort(unique(revision_signature_key(results)))
  )
  result_signatures <- revision_signature_key(results)
  valid_panel_hashes <- "panel_input_hash" %in% names(results) &&
    all(grepl("^[0-9a-f]{64}$", results$panel_input_hash))
  panel_hash_groups <- if (valid_panel_hashes) {
    split(results$panel_input_hash, result_signatures)
  } else {
    list()
  }
  panel_input_hash_equivalence <- valid_panel_hashes &&
    identical(sort(names(panel_hash_groups)), canonical_signatures) &&
    all(vapply(panel_hash_groups, function(hashes) {
      length(unique(hashes)) == 1L
    }, logical(1)))
  expected_seeds <- canonical$seed_base[match(
    revision_signature_key(grid),
    revision_signature_key(canonical)
  )]
  seeds_ok <- "seed_base" %in% names(grid) &&
    !anyNA(expected_seeds) &&
    identical(as.integer(grid$seed_base), as.integer(expected_seeds))
  metadata <- paste(readLines(paths$metadata, warn = FALSE), collapse = "\n")
  config_hash_ok <- grepl("config_hash:", metadata, fixed = TRUE) &&
    grepl("input_hash", metadata, fixed = TRUE)
  reusable <- schema_ok && reporting_schema_ok && keys_ok &&
    partition_ok && results_panels_ok &&
    seeds_ok && config_hash_ok && panel_input_hash_equivalence
  failed <- names(which(!unlist(list(
    output_schema = schema_ok,
    reporting_schema = reporting_schema_ok,
    canonical_panel_keys = keys_ok,
    required_partition_conversion = partition_ok,
    result_panel_keys = results_panels_ok,
    canonical_seed_rule = seeds_ok,
    config_and_input_hashes = config_hash_ok,
    panel_input_hash_equivalence = panel_input_hash_equivalence
  ))))
  list(
    reusable = reusable,
    reason = if (reusable) {
      "all reuse checks passed"
    } else {
      paste("failed checks:", paste(failed, collapse = ", "))
    },
    checks = list(
      output_schema = schema_ok,
      reporting_schema = reporting_schema_ok,
      canonical_panel_keys = keys_ok,
      canonical_panel_count = length(grid_signatures),
      required_partition_conversion = partition_ok,
      result_panel_keys = results_panels_ok,
      result_rows = nrow(results),
      canonical_seed_rule = seeds_ok,
      config_and_input_hashes = config_hash_ok,
      panel_input_hash_equivalence = panel_input_hash_equivalence
    ),
    input_hashes = lapply(paths, revision_sha256)
  )
}

revision_run_reuse_audit <- function(plan, context, config) {
  checkpoint_path <- revision_checkpoint_path(context, config)
  revision_assert_fresh_or_resume(context, checkpoint_path)
  canonical <- plan$panels
  methods <- list(
    graphicalvar = revision_audit_prior_method("graphicalvar", canonical),
    gimme = revision_audit_prior_method("gimme", canonical)
  )
  audit <- list(
    job_id = context$job_id,
    config_hash = context$config_hash,
    canonical_panel_count = nrow(canonical),
    canonical_panel_keys_valid = !anyDuplicated(canonical$panel_key) &&
      nrow(canonical) == 21600L,
    canonical_seed_origin = REVISION_SEED_ORIGIN,
    methods = methods
  )
  revision_write_json(audit, file.path(context$output_dir, "reuse_audit.json"))
  if (!file.exists(checkpoint_path)) {
    checkpoint <- data.frame(
      unit_key = canonical$panel_key,
      panel_key = canonical$panel_key,
      seed_base = canonical$seed_base,
      stage = "reuse_audit",
      status = "audited",
      graphicalvar_present = isTRUE(methods$graphicalvar$checks$result_panel_keys),
      gimme_present = isTRUE(methods$gimme$checks$result_panel_keys),
      stringsAsFactors = FALSE
    )
    revision_append_checkpoint(checkpoint, checkpoint_path)
  }
  revision_read_checkpoint(checkpoint_path)
}

revision_source_estimation_core <- function() {
  Sys.setenv(
    RUN_METHOD_BENCHMARK = "0",
    RUN_METHOD_BENCHMARK_SMOKE = "0"
  )
  source(file.path(
    REVISION_REPO_ROOT,
    "scripts",
    "simulations",
    "run_method_simulation.R"
  ))
  invisible(TRUE)
}

revision_partition_weight_matrix <- function(weight_matrix,
                                             algorithm,
                                             resolution = 1) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for common partitioning.", call. = FALSE)
  }
  matrix_value <- as.matrix(weight_matrix)
  if (nrow(matrix_value) != ncol(matrix_value)) {
    stop("Frozen method structure must be square.", call. = FALSE)
  }
  positive <- revision_symmetrize_weights(matrix_value)
  item_names <- rownames(matrix_value)
  if (is.null(item_names)) {
    item_names <- paste0("Y", seq_len(nrow(matrix_value)))
  }
  if (sum(positive) == 0) {
    return(setNames(seq_len(nrow(matrix_value)), item_names))
  }
  dimnames(positive) <- list(item_names, item_names)
  graph <- igraph::graph_from_adjacency_matrix(
    positive,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  weights <- igraph::E(graph)$weight
  membership <- switch(
    algorithm,
    louvain = igraph::cluster_louvain(
      graph,
      weights = weights,
      resolution = resolution
    )$membership,
    leiden = igraph::cluster_leiden(
      graph,
      objective_function = "modularity",
      weights = weights,
      resolution = resolution
    )$membership,
    walktrap = igraph::cluster_walktrap(graph, weights = weights)$membership,
    stop("Unknown community algorithm: ", algorithm, call. = FALSE)
  )
  names(membership) <- item_names
  normalize_community_map(membership)
}

revision_oracle_structure <- function(membership) {
  structure <- outer(membership, membership, FUN = "==") * 1
  diag(structure) <- 0
  dimnames(structure) <- list(names(membership), names(membership))
  structure
}

revision_encode_membership <- function(membership) {
  paste(paste(names(membership), as.integer(membership), sep = "="), collapse = ";")
}

revision_align_memberships <- function(truth, estimate) {
  if (is.null(names(truth))) names(truth) <- as.character(seq_along(truth))
  if (is.null(names(estimate))) names(estimate) <- as.character(seq_along(estimate))
  common <- intersect(names(truth), names(estimate))
  normalize <- function(values) {
    as.integer(factor(as.character(values), levels = unique(as.character(values))))
  }
  list(
    true = normalize(truth[common]),
    est = normalize(estimate[common]),
    items = common
  )
}

revision_adjusted_rand_index <- function(truth, estimate) {
  aligned <- revision_align_memberships(truth, estimate)
  contingency <- table(aligned$true, aligned$est)
  choose2 <- function(values) values * (values - 1) / 2
  observed <- sum(choose2(contingency))
  row_pairs <- sum(choose2(rowSums(contingency)))
  column_pairs <- sum(choose2(colSums(contingency)))
  total_pairs <- choose2(sum(contingency))
  if (total_pairs == 0) {
    return(NA_real_)
  }
  expected <- row_pairs * column_pairs / total_pairs
  denominator <- (row_pairs + column_pairs) / 2 - expected
  if (denominator == 0) {
    return(as.numeric(identical(aligned$true, aligned$est)))
  }
  (observed - expected) / denominator
}

revision_variation_information <- function(truth, estimate) {
  aligned <- revision_align_memberships(truth, estimate)
  contingency <- unclass(table(aligned$true, aligned$est))
  total <- sum(contingency)
  if (total == 0) {
    return(NA_real_)
  }
  joint <- contingency / total
  truth_probability <- rowSums(joint)
  estimate_probability <- colSums(joint)
  entropy <- function(probability) {
    probability <- probability[probability > 0]
    -sum(probability * log(probability))
  }
  mutual_information <- 0
  nonzero <- which(joint > 0, arr.ind = TRUE)
  for (index in seq_len(nrow(nonzero))) {
    row <- nonzero[index, 1L]
    column <- nonzero[index, 2L]
    mutual_information <- mutual_information + joint[row, column] * log(
      joint[row, column] /
        (truth_probability[[row]] * estimate_probability[[column]])
    )
  }
  entropy(truth_probability) + entropy(estimate_probability) -
    2 * mutual_information
}

revision_static_ega_structure <- function(sim_data) {
  fit <- quiet_eval(EGAnet::EGA(
    data = sim_data$panel[, sim_data$item_names, drop = FALSE],
    model = "glasso",
    algorithm = "louvain"
  ))
  structure <- fit$network
  if (is.null(structure) && !is.null(fit$EGA$network)) {
    structure <- fit$EGA$network
  }
  if (is.null(structure)) {
    stop("Static EGA did not expose a frozen network matrix.", call. = FALSE)
  }
  as.matrix(structure)
}

revision_tefi_value <- function(fit) {
  raw <- suppressWarnings(as.numeric(fit$dynEGA$population$TEFI))
  if (length(raw) == 0L) {
    return(NA_real_)
  }
  value <- raw[[1]]
  if (!is.finite(value)) NA_real_ else value
}

revision_fit_frozen_dynega <- function(sim_data, scen_row, cfg) {
  candidates <- revision_parse_numeric_vector(scen_row$n_embed_candidates[[1]])
  candidates <- as.integer(candidates[candidates >= 3L &
    candidates < nrow(sim_data$subject_list[[1]])])
  if (length(candidates) == 0L) {
    stop("No valid DynEGA embedding candidates for this panel.", call. = FALSE)
  }
  fits <- vector("list", length(candidates))
  tefi <- rep(NA_real_, length(candidates))
  errors <- character()
  for (index in seq_along(candidates)) {
    attempt <- tryCatch(
      quiet_eval(EGAnet::dynEGA(
        data = sim_data$panel[, c(sim_data$item_names, "id"), drop = FALSE],
        n.embed = candidates[[index]],
        tau = 1,
        delta = REVISION_DYNEGA_OBSERVATION_DELTA,
        id = length(sim_data$item_names) + 1L,
        model = "glasso",
        algorithm = "louvain",
        uni.method = "expand",
        use.derivatives = 1,
        ncores = cfg$dynEGA_ncores
      )),
      error = function(error) error
    )
    if (inherits(attempt, "error")) {
      errors <- c(errors, conditionMessage(attempt))
      next
    }
    fits[[index]] <- attempt
    tefi[[index]] <- revision_tefi_value(attempt)
  }
  valid <- which(!vapply(fits, is.null, logical(1)))
  if (length(valid) == 0L) {
    stop(
      "All frozen DynEGA embedding fits failed: ",
      paste(unique(errors), collapse = " | "),
      call. = FALSE
    )
  }
  finite <- valid[is.finite(tefi[valid])]
  selected <- if (length(finite) > 0L) {
    finite[[which.min(tefi[finite])]]
  } else {
    valid[[1L]]
  }
  fit <- fits[[selected]]
  population <- fit$dynEGA$population
  network <- population$network
  if (is.null(network)) {
    network <- population$correlation
  }
  if (is.null(network)) {
    stop("Frozen DynEGA fit did not expose a network matrix.", call. = FALSE)
  }
  correlation <- population$correlation
  if (is.null(correlation)) {
    correlation <- suppressWarnings(stats::cor(
      sim_data$panel[, sim_data$item_names, drop = FALSE],
      use = "pairwise.complete.obs"
    ))
  }
  list(
    structure = as.matrix(network),
    correlation = as.matrix(correlation),
    native_membership = extract_dynega_membership(fit, sim_data$item_names),
    n_embed_best = candidates[[selected]],
    tefi_best = tefi[[selected]]
  )
}

revision_fit_frozen_structure <- function(method_id, sim_data, scen_row, cfg) {
  start <- proc.time()[["elapsed"]]
  value <- tryCatch(
    switch(
      method_id,
      dynEGA = revision_fit_frozen_dynega(sim_data, scen_row, cfg),
      static_EGA = list(
        structure = revision_static_ega_structure(sim_data),
        correlation = NULL,
        native_membership = NULL,
        n_embed_best = NA_integer_,
        tefi_best = NA_real_
      ),
      oracle_blocks = list(
        structure = revision_oracle_structure(sim_data$true_membership),
        correlation = NULL,
        native_membership = sim_data$true_membership,
        n_embed_best = NA_integer_,
        tefi_best = NA_real_
      ),
      graphicalVAR = list(
        structure = run_external_structure_fit(
          method = "graphicalVAR",
          sim_data = sim_data,
          cfg = cfg,
          scenario_label = scen_row$panel_key[[1]],
          limit_sec = cfg$graphical_var_timeout_sec
        ),
        correlation = NULL,
        native_membership = NULL,
        n_embed_best = NA_integer_,
        tefi_best = NA_real_
      ),
      gimme = list(
        structure = revision_gimme_positive_structure(
          run_external_structure_fit(
            method = "gimme",
            sim_data = sim_data,
            cfg = cfg,
            scenario_label = scen_row$panel_key[[1]],
            limit_sec = cfg$gimme_timeout_sec
          )
        ),
        correlation = NULL,
        native_membership = NULL,
        n_embed_best = NA_integer_,
        tefi_best = NA_real_
      ),
      stop("Unknown frozen method: ", method_id, call. = FALSE)
    ),
    error = function(error) error
  )
  elapsed <- proc.time()[["elapsed"]] - start
  if (inherits(value, "error")) {
    status <- if (is_timeout_message(conditionMessage(value))) "timeout" else "error"
    return(list(
      status = status,
      error_message = conditionMessage(value),
      runtime_sec = elapsed
    ))
  }
  list(
    status = "ok",
    error_message = NA_character_,
    runtime_sec = elapsed,
    value = value
  )
}

revision_panel_result_base <- function(scen_row, method_id, algorithm) {
  optional_value <- function(name, fallback = NA) {
    if (name %in% names(scen_row)) scen_row[[name]][[1]] else fallback
  }
  representation_key <- optional_value("representation_key", scen_row$panel_key[[1]])
  data.frame(
    unit_key = paste(representation_key, method_id, algorithm, sep = "|"),
    panel_key = scen_row$panel_key[[1]],
    representation_key = representation_key,
    structure_fit_key = paste(representation_key, method_id, sep = "|"),
    seed_base = scen_row$seed_base[[1]],
    config_hash = optional_value("config_hash", NA_character_),
    generator_hash = optional_value("generator_hash", NA_character_),
    input_spec_hash = optional_value("input_spec_hash", NA_character_),
    panel_input_hash = optional_value("panel_input_hash", NA_character_),
    input_data_hash = optional_value("panel_input_hash", NA_character_),
    component_hash_latent_standard_normals = optional_value(
      "component_hash_latent_standard_normals",
      NA_character_
    ),
    component_hash_residual_standard_normals = optional_value(
      "component_hash_residual_standard_normals",
      NA_character_
    ),
    component_hash_loading_basis = optional_value(
      "component_hash_loading_basis",
      NA_character_
    ),
    component_hash_activity_schedule = optional_value(
      "component_hash_activity_schedule",
      NA_character_
    ),
    component_hash_linear_z = optional_value(
      "component_hash_linear_z",
      NA_character_
    ),
    stage = optional_value("grid_scope", "matched"),
    method_id = method_id,
    community_algorithm = algorithm,
    primary_readout = algorithm == "louvain",
    partition_source = "frozen_method_structure",
    scenario = scen_row$scenario[[1]],
    scenario_family = scen_row$scenario_family[[1]],
    challenge_id = optional_value("challenge_id", NA_character_),
    representation = optional_value("representation", "linear_z"),
    N = scen_row$N[[1]],
    TimeP = scen_row$TimeP[[1]],
    NVAR = scen_row$NVAR[[1]],
    Error_sd = scen_row$Error_sd[[1]],
    Loadings = scen_row$Loadings[[1]],
    Q = scen_row$Q[[1]],
    rep = scen_row$rep[[1]],
    dt = scen_row$dt[[1]],
    dynega_delta = if (identical(method_id, "dynEGA")) {
      REVISION_DYNEGA_OBSERVATION_DELTA
    } else {
      NA_real_
    },
    dynega_delta_units = if (identical(method_id, "dynEGA")) {
      REVISION_DYNEGA_DELTA_UNITS
    } else {
      NA_character_
    },
    n_transitions = scen_row$n_transitions[[1]],
    observed_horizon = scen_row$observed_horizon[[1]],
    stability_class = scen_row$stability_class[[1]],
    design_range_status = if (identical(method_id, "gimme") &&
      scen_row$TimeP[[1]] == 25L) {
      "out_of_range_short_record"
    } else {
      "within_prespecified_range"
    },
    continuous_max_real_eigenvalue = scen_row$continuous_max_real_eigenvalue[[1]],
    discrete_spectral_radius = scen_row$discrete_spectral_radius[[1]],
    n_dim_true = scen_row$scenario_nfac[[1]],
    stringsAsFactors = FALSE
  )
}

revision_set_missing_recovery_metrics <- function(row) {
  row$n_dim_hat <- NA_integer_
  row$n_dim_exact <- NA_real_
  row$assignment_accuracy <- NA_real_
  row$adjusted_rand_index <- NA_real_
  row$variation_information <- NA_real_
  row$true_membership <- NA_character_
  row$estimated_membership <- NA_character_
  row$score_cor_common <- NA_real_
  row$score_nrmse_common <- NA_real_
  row$n_embed_best <- NA_integer_
  row$tefi_best <- NA_real_
  row$native_expand_n_dim <- NA_integer_
  row$native_expand_assignment_accuracy <- NA_real_
  row$native_expand_status <- NA_character_
  row$native_expand_error_message <- NA_character_
  row$correlation_louvain_095_n_dim <- NA_integer_
  row$correlation_louvain_095_assignment_accuracy <- NA_real_
  row$correlation_louvain_095_status <- NA_character_
  row$correlation_louvain_095_error_message <- NA_character_
  row
}

revision_error_readouts <- function(scen_row, method_id, fit_result) {
  rows <- lapply(REVISION_COMMUNITY_ALGORITHMS, function(algorithm) {
    row <- revision_panel_result_base(scen_row, method_id, algorithm)
    row$status <- fit_result$status
    row$error_message <- fit_result$error_message
    row$structure_fit_status <- fit_result$status
    row$structure_hash <- NA_character_
    row$structure_fit_runtime_sec <- fit_result$runtime_sec
    row$readout_runtime_sec <- NA_real_
    row$runtime_sec <- fit_result$runtime_sec
    row$completed_within_900 <- FALSE
    revision_set_missing_recovery_metrics(row)
  })
  do.call(rbind, rows)
}

revision_native_sensitivity <- function(frozen, sim_data, method_id) {
  if (!identical(method_id, "dynEGA")) {
    return(list(
      status = "not_applicable",
      error_message = NA_character_,
      n_dim = NA_integer_,
      assignment_accuracy = NA_real_
    ))
  }
  tryCatch({
    membership <- frozen$native_membership
    if (is.null(membership)) {
      stop("Frozen DynEGA fit did not expose expand membership.", call. = FALSE)
    }
    list(
      status = "ok",
      error_message = NA_character_,
      n_dim = length(unique(membership)),
      assignment_accuracy = compute_assignment_accuracy(
        sim_data$true_membership,
        membership
      )
    )
  }, error = function(error) list(
    status = "error",
    error_message = conditionMessage(error),
    n_dim = NA_integer_,
    assignment_accuracy = NA_real_
  ))
}

revision_correlation_sensitivity <- function(frozen, sim_data, method_id) {
  if (!identical(method_id, "dynEGA")) {
    return(list(
      status = "not_applicable",
      error_message = NA_character_,
      n_dim = NA_integer_,
      assignment_accuracy = NA_real_
    ))
  }
  tryCatch({
    membership <- revision_partition_weight_matrix(
      frozen$correlation,
      "louvain",
      0.95
    )
    list(
      status = "ok",
      error_message = NA_character_,
      n_dim = length(unique(membership)),
      assignment_accuracy = compute_assignment_accuracy(
        sim_data$true_membership,
        membership
      )
    )
  }, error = function(error) list(
    status = "error",
    error_message = conditionMessage(error),
    n_dim = NA_integer_,
    assignment_accuracy = NA_real_
  ))
}

revision_evaluate_structure_readout <- function(scen_row,
                                                sim_data,
                                                method_id,
                                                algorithm,
                                                fit_result,
                                                structure_hash,
                                                native,
                                                correlation) {
  start <- proc.time()[["elapsed"]]
  row <- revision_panel_result_base(scen_row, method_id, algorithm)
  row$structure_fit_status <- "ok"
  row$structure_hash <- structure_hash
  row$structure_fit_runtime_sec <- fit_result$runtime_sec
  row$native_expand_n_dim <- native$n_dim
  row$native_expand_assignment_accuracy <- native$assignment_accuracy
  row$native_expand_status <- native$status
  row$native_expand_error_message <- native$error_message
  row$correlation_louvain_095_n_dim <- correlation$n_dim
  row$correlation_louvain_095_assignment_accuracy <- correlation$assignment_accuracy
  row$correlation_louvain_095_status <- correlation$status
  row$correlation_louvain_095_error_message <- correlation$error_message
  evaluated <- tryCatch({
    membership <- revision_partition_weight_matrix(
      fit_result$value$structure,
      algorithm,
      resolution = 1
    )
    scores <- build_common_scores(sim_data$panel, sim_data$item_names, membership)
    list(
      membership = membership,
      n_dim_hat = length(unique(membership)),
      assignment_accuracy = compute_assignment_accuracy(
        sim_data$true_membership,
        membership
      ),
      adjusted_rand_index = revision_adjusted_rand_index(
        sim_data$true_membership,
        membership
      ),
      variation_information = revision_variation_information(
        sim_data$true_membership,
        membership
      ),
      score_cor_common = compute_latent_score_alignment(
        scores,
        sim_data$latents,
        sim_data$panel$id
      ),
      score_nrmse_common = compute_score_nrmse_alignment(
        scores,
        sim_data$latents,
        sim_data$panel$id
      )
    )
  }, error = function(error) error)
  row$readout_runtime_sec <- proc.time()[["elapsed"]] - start
  row$runtime_sec <- row$structure_fit_runtime_sec + row$readout_runtime_sec
  if (inherits(evaluated, "error")) {
    row$status <- "error"
    row$error_message <- paste("readout failed:", conditionMessage(evaluated))
    row$completed_within_900 <- FALSE
    row <- revision_set_missing_recovery_metrics(row)
    row$native_expand_n_dim <- native$n_dim
    row$native_expand_assignment_accuracy <- native$assignment_accuracy
    row$native_expand_status <- native$status
    row$native_expand_error_message <- native$error_message
    row$correlation_louvain_095_n_dim <- correlation$n_dim
    row$correlation_louvain_095_assignment_accuracy <- correlation$assignment_accuracy
    row$correlation_louvain_095_status <- correlation$status
    row$correlation_louvain_095_error_message <- correlation$error_message
    return(row)
  }
  membership <- evaluated$membership
  row$status <- "ok"
  row$error_message <- NA_character_
  row$completed_within_900 <- row$runtime_sec <= 900
  row$n_dim_hat <- evaluated$n_dim_hat
  row$n_dim_exact <- as.numeric(row$n_dim_hat == sim_data$NFAC_true)
  row$assignment_accuracy <- evaluated$assignment_accuracy
  row$adjusted_rand_index <- evaluated$adjusted_rand_index
  row$variation_information <- evaluated$variation_information
  row$true_membership <- revision_encode_membership(sim_data$true_membership)
  row$estimated_membership <- revision_encode_membership(membership)
  row$score_cor_common <- evaluated$score_cor_common
  row$score_nrmse_common <- evaluated$score_nrmse_common
  row$n_embed_best <- fit_result$value$n_embed_best
  row$tefi_best <- fit_result$value$tefi_best
  row
}

revision_evaluate_frozen_structure <- function(scen_row,
                                               sim_data,
                                               method_id,
                                               fit_result) {
  if (!identical(fit_result$status, "ok")) {
    return(revision_error_readouts(scen_row, method_id, fit_result))
  }
  frozen <- fit_result$value
  structure_hash <- tryCatch(
    revision_object_sha256(as.matrix(frozen$structure)),
    error = function(error) error
  )
  if (inherits(structure_hash, "error")) {
    return(revision_error_readouts(scen_row, method_id, list(
      status = "error",
      error_message = paste(
        "frozen structure hash failed:",
        conditionMessage(structure_hash)
      ),
      runtime_sec = fit_result$runtime_sec
    )))
  }
  native <- revision_native_sensitivity(frozen, sim_data, method_id)
  correlation <- revision_correlation_sensitivity(frozen, sim_data, method_id)
  rows <- lapply(REVISION_COMMUNITY_ALGORITHMS, function(algorithm) {
    revision_evaluate_structure_readout(
      scen_row,
      sim_data,
      method_id,
      algorithm,
      fit_result,
      structure_hash,
      native,
      correlation
    )
  })
  do.call(rbind, rows)
}

revision_transform_standard_normals <- function(standard, covariance) {
  decomposition <- eigen(covariance, symmetric = TRUE)
  values <- pmax(decomposition$values, 0)
  as.matrix(standard) %*% diag(sqrt(values)) %*% t(decomposition$vectors)
}

revision_component_seed <- function(pair_key, video, component) {
  revision_seed_from_key(paste(pair_key, video, component, sep = "|"))
}

revision_simulate_challenge_panel <- function(scen_row) {
  set.seed(scen_row$seed_base[[1]])
  n_factors <- as.integer(scen_row$scenario_nfac[[1]])
  indicators <- as.integer(scen_row$NVAR[[1]])
  n_videos <- as.integer(scen_row$N[[1]])
  time_points <- as.integer(scen_row$TimeP[[1]])
  eta <- scen_row$eta[[1]]
  zeta <- scen_row$zeta[[1]]
  sigma_q <- scen_row$sigma_q[[1]]
  item_names <- paste0("Y", seq_len(n_factors * indicators))
  membership <- setNames(rep(seq_len(n_factors), each = indicators), item_names)
  panels <- vector("list", n_videos)
  subjects <- vector("list", n_videos)
  latents <- vector("list", n_videos)
  schedules <- vector("list", n_videos)
  latent_standard_hashes <- character(n_videos)
  residual_standard_hashes <- character(n_videos)
  loading_basis_hashes <- character(n_videos)
  schedule_hashes <- character(n_videos)
  linear_z_hashes <- character(n_videos)
  pair_key <- scen_row$challenge_pair_key[[1]]
  for (video in seq_len(n_videos)) {
    independent <- identical(scen_row$challenge_id[[1]], "independent_initialization")
    burn_in <- 0L
    total <- time_points + burn_in
    state <- if (independent) {
      revision_initial_states(
        eta,
        zeta,
        sigma_q,
        scen_row$dt[[1]],
        revision_component_seed(pair_key, video, "initialization")
      )
    } else {
      cbind(rep(0.25, n_factors), rep(2, n_factors))
    }
    transitions <- lapply(seq_len(n_factors), function(index) {
      revision_exact_transition(eta[[index]], zeta[[index]], scen_row$dt[[1]])
    })
    shock_covariance <- revision_equicorrelation_covariance(
      sigma_q,
      scen_row$latent_shock_rho[[1]]
    )
    set.seed(revision_component_seed(pair_key, video, "latent_innovations"))
    latent_standard <- matrix(stats::rnorm(total * n_factors), nrow = total)
    shocks <- revision_transform_standard_normals(latent_standard, shock_covariance)
    latent_standard_hashes[[video]] <- revision_object_sha256(latent_standard)
    latent <- matrix(NA_real_, nrow = total, ncol = n_factors)
    latent[1L, ] <- state[, 1L]
    velocity <- state[, 2L]
    if (total > 1L) {
      for (time in 2:total) {
        for (factor in seq_len(n_factors)) {
          updated <- transitions[[factor]]$transition %*%
            c(latent[time - 1L, factor], velocity[[factor]]) +
            transitions[[factor]]$innovation_map * shocks[time, factor]
          latent[time, factor] <- updated[[1L]]
          velocity[[factor]] <- updated[[2L]]
        }
      }
    }
    if (burn_in > 0L) {
      latent <- latent[(burn_in + 1L):total, , drop = FALSE]
    }
    set.seed(revision_component_seed(pair_key, video, "activity_schedule"))
    schedule <- build_activation_schedule(time_points, n_factors, scen_row, video)
    schedule_hashes[[video]] <- revision_object_sha256(schedule$active_dim)
    envelope <- if (identical(scen_row$activity_gate[[1]], "half_cosine")) {
      revision_smooth_activity_envelope(
        schedule$active_dim,
        n_factors,
        scen_row$active_gain[[1]],
        scen_row$background_gain[[1]],
        scen_row$gate_transition_observations[[1]]
      )
    } else {
      schedule$envelope
    }
    latent <- latent * envelope
    set.seed(revision_component_seed(pair_key, video, "loading_basis"))
    loading_basis <- matrix(
      stats::rnorm(length(item_names) * n_factors, sd = 0.05),
      nrow = length(item_names),
      ncol = n_factors
    )
    loading_basis_hashes[[video]] <- revision_object_sha256(loading_basis)
    loadings <- revision_loading_matrix(
      n_factors,
      indicators,
      scen_row$Loadings[[1]],
      identical(scen_row$challenge_id[[1]], "cross_loadings"),
      loading_basis
    )
    residual_covariance <- revision_equicorrelation_covariance(
      rep(scen_row$Error_sd[[1]], length(item_names)),
      scen_row$residual_rho[[1]]
    )
    set.seed(revision_component_seed(pair_key, video, "residual_innovations"))
    residual_standard <- matrix(
      stats::rnorm(time_points * length(item_names)),
      nrow = time_points
    )
    residual_standard_hashes[[video]] <- revision_object_sha256(residual_standard)
    residual <- revision_transform_standard_normals(
      residual_standard,
      residual_covariance
    )
    z <- latent %*% t(loadings) + residual
    colnames(z) <- item_names
    linear_z_hashes[[video]] <- revision_object_sha256(z)
    observed <- if (identical(scen_row$challenge_id[[1]], "softmax_bounded")) {
      probabilities <- revision_row_softmax(
        z,
        scen_row$softmax_alpha[[1]],
        scen_row$softmax_tau[[1]]
      )
      if (identical(scen_row$representation[[1]], "softmax_clr")) {
        revision_clr(probabilities)
      } else {
        probabilities
      }
    } else {
      z
    }
    panel <- as.data.frame(observed, stringsAsFactors = FALSE)
    panel$time <- seq_len(time_points)
    panel$id <- video
    panels[[video]] <- panel
    subjects[[video]] <- observed
    latents[[video]] <- latent
    schedules[[video]] <- data.frame(
      id = video,
      switch_count = schedule$switch_count,
      mean_dwell = schedule$mean_dwell,
      stringsAsFactors = FALSE
    )
  }
  schedule_frame <- do.call(rbind, schedules)
  names(subjects) <- paste0("subj", seq_len(n_videos))
  list(
    panel = do.call(rbind, panels),
    subject_list = subjects,
    latents = latents,
    item_names = item_names,
    true_membership = normalize_community_map(membership),
    NFAC_true = n_factors,
    NVAR = indicators,
    N_videos = n_videos,
    dt = scen_row$dt[[1]],
    schedule = schedule_frame,
    switch_count_mean = mean(schedule_frame$switch_count),
    mean_dwell = mean(schedule_frame$mean_dwell),
    component_hashes = list(
      latent_standard_normals = revision_object_sha256(latent_standard_hashes),
      residual_standard_normals = revision_object_sha256(residual_standard_hashes),
      loading_basis = revision_object_sha256(loading_basis_hashes),
      activity_schedule = revision_object_sha256(schedule_hashes),
      linear_z = revision_object_sha256(linear_z_hashes)
    )
  )
}

revision_simulate_planned_panel <- function(scen_row, challenge) {
  if (challenge) {
    return(revision_simulate_challenge_panel(scen_row))
  }
  simulate_benchmark_panel(scen_row, REVISION_RUNTIME_CFG, scen_row$seed_base[[1]])
}

revision_process_structure_panel <- function(scen_row, methods, challenge) {
  sim_data <- tryCatch(
    revision_simulate_planned_panel(scen_row, challenge),
    error = function(error) error
  )
  if (inherits(sim_data, "error")) {
    fit_error <- list(
      status = "error",
      error_message = paste("simulation failed:", conditionMessage(sim_data)),
      runtime_sec = NA_real_
    )
    return(do.call(rbind, lapply(methods, function(method_id) {
      revision_error_readouts(scen_row, method_id, fit_error)
    })))
  }
  scen_row$panel_input_hash <- revision_object_sha256(list(
    panel = sim_data$panel,
    latents = sim_data$latents,
    true_membership = sim_data$true_membership
  ))
  if (!is.null(sim_data$component_hashes)) {
    for (name in names(sim_data$component_hashes)) {
      scen_row[[paste0("component_hash_", name)]] <- sim_data$component_hashes[[name]]
    }
  }
  rows <- lapply(methods, function(method_id) {
    tryCatch({
      method_seed <- as.integer((
        as.double(scen_row$seed_base[[1]]) +
          match(method_id, methods) * 100003
      ) %% 2147483646 + 1)
      set.seed(method_seed)
      fit <- revision_fit_frozen_structure(
        method_id,
        sim_data,
        scen_row,
        REVISION_RUNTIME_CFG
      )
      revision_evaluate_frozen_structure(scen_row, sim_data, method_id, fit)
    }, error = function(error) {
      revision_error_readouts(scen_row, method_id, list(
        status = "error",
        error_message = paste(
          "unexpected post-simulation failure:",
          conditionMessage(error)
        ),
        runtime_sec = NA_real_
      ))
    })
  })
  combined <- do.call(rbind, rows)
  revision_validate_structure_fit_contract(combined)
  combined
}

revision_validate_structure_fit_contract <- function(rows,
                                                      expected_fit_keys = NULL) {
  required <- c(
    "unit_key", "structure_fit_key", "structure_fit_status",
    "structure_hash", "community_algorithm"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    stop(
      "Structure readouts omit contract columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(expected_fit_keys)) {
    observed_fit_keys <- sort(unique(as.character(rows$structure_fit_key)))
    expected_fit_keys <- sort(unique(as.character(expected_fit_keys)))
    if (!identical(observed_fit_keys, expected_fit_keys)) {
      missing_keys <- setdiff(expected_fit_keys, observed_fit_keys)
      extra_keys <- setdiff(observed_fit_keys, expected_fit_keys)
      stop(
        "Structure-fit key mismatch; missing: ",
        paste(head(missing_keys, 5L), collapse = ", "),
        "; extra: ",
        paste(head(extra_keys, 5L), collapse = ", "),
        call. = FALSE
      )
    }
  }
  groups <- split(seq_len(nrow(rows)), rows$structure_fit_key)
  invalid <- vapply(groups, function(index) {
    group <- rows[index, , drop = FALSE]
    algorithms_ok <- nrow(group) == length(REVISION_COMMUNITY_ALGORITHMS) &&
      identical(
        sort(as.character(group$community_algorithm)),
        sort(REVISION_COMMUNITY_ALGORITHMS)
      )
    status <- unique(as.character(group$structure_fit_status))
    status_ok <- length(status) == 1L && status %in% c("ok", "error", "timeout")
    hashes <- unique(as.character(group$structure_hash[
      !is.na(group$structure_hash) & nzchar(group$structure_hash)
    ]))
    hash_ok <- if (length(status) == 1L && identical(status, "ok")) {
      length(hashes) == 1L && grepl("^[0-9a-f]{64}$", hashes)
    } else {
      length(hashes) == 0L
    }
    !(algorithms_ok && status_ok && hash_ok)
  }, logical(1))
  if (any(invalid)) {
    stop(
      "Frozen-structure contract failed for fit keys: ",
      paste(head(names(groups)[invalid], 10L), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_runtime_config <- function(context) {
  config <- build_benchmark_config(smoke = FALSE)
  config$integrator <- "linear_exact"
  config$parallel_workers <- context$workers
  config$dynEGA_ncores <- 1L
  config$graphical_var_timeout_sec <- as.numeric(Sys.getenv(
    "REVISION_GRAPHICALVAR_TIMEOUT_SEC",
    as.character(context$per_unit_timeout_seconds)
  ))
  config$gimme_timeout_sec <- as.numeric(Sys.getenv(
    "REVISION_GIMME_TIMEOUT_SEC",
    as.character(context$per_unit_timeout_seconds)
  ))
  config
}

revision_run_structure_job <- function(plan, context, config) {
  revision_source_estimation_core()
  assign("REVISION_RUNTIME_CFG", revision_runtime_config(context), envir = .GlobalEnv)
  on.exit(rm("REVISION_RUNTIME_CFG", envir = .GlobalEnv), add = TRUE)
  panels <- revision_core_panel_grid(plan$panels)
  panels$config_hash <- context$config_hash
  panels$generator_hash <- context$generator_hash
  panels$input_spec_hash <- revision_object_sha256(list(
    job_id = context$job_id,
    config_hash = context$config_hash,
    parameters = revision_parameter_summary(context$job_id, plan),
    expected_rows = plan$job$community_readouts[[1]],
    seed_origin = REVISION_SEED_ORIGIN
  ))
  methods <- plan$methods
  checkpoint_path <- revision_checkpoint_path(context, config)
  revision_assert_fresh_or_resume(context, checkpoint_path)
  existing <- revision_read_checkpoint(checkpoint_path)
  completed <- if (nrow(existing) == 0L) character() else existing$unit_key
  expected_by_panel <- length(methods) * length(REVISION_COMMUNITY_ALGORITHMS)
  pending <- vapply(seq_len(nrow(panels)), function(index) {
    prefix <- panels$representation_key[[index]]
    sum(startsWith(completed, paste0(prefix, "|"))) < expected_by_panel
  }, logical(1))
  panel_indices <- which(pending)
  if (length(panel_indices) == 0L) {
    return(existing)
  }
  challenge <- grepl("^challenge_", context$job_id)
  batch_size <- max(context$workers, 1L)
  for (start in seq.int(1L, length(panel_indices), by = batch_size)) {
    batch <- panel_indices[seq.int(start, min(length(panel_indices), start + batch_size - 1L))]
    rows <- parallel::mclapply(
      batch,
      function(index) {
        scen_row <- panels[index, , drop = FALSE]
        tryCatch(
          revision_process_structure_panel(scen_row, methods, challenge),
          error = function(error) do.call(rbind, lapply(methods, function(method_id) {
            revision_error_readouts(scen_row, method_id, list(
              status = "error",
              error_message = paste(
                "unexpected panel failure:",
                conditionMessage(error)
              ),
              runtime_sec = NA_real_
            ))
          }))
        )
      },
      mc.cores = min(context$workers, length(batch)),
      mc.preschedule = FALSE
    )
    for (frame in rows) {
      frame <- frame[!frame$unit_key %in% completed, , drop = FALSE]
      revision_append_checkpoint(frame, checkpoint_path)
      completed <- c(completed, frame$unit_key)
    }
  }
  revision_read_checkpoint(checkpoint_path)
}

revision_empirical_metric_distribution <- function(panel, score_columns) {
  clips <- unique(panel$clip)
  rows <- list()
  for (clip in clips) {
    clip_panel <- panel[panel$clip == clip, , drop = FALSE]
    for (column in score_columns) {
      series <- as.numeric(clip_panel[[column]])
      lag_one <- if (length(series) < 3L || stats::sd(series) == 0) {
        NA_real_
      } else {
        stats::cor(series[-length(series)], series[-1L])
      }
      values <- c(
        channel_iqr = stats::IQR(series),
        derivative_iqr = stats::IQR(diff(series)),
        lag1_acf = lag_one,
        run_length_entropy = compute_run_length_entropy(series),
        spectral_entropy = compute_spectral_entropy(series)
      )
      rows[[length(rows) + 1L]] <- data.frame(
        clip = clip,
        unit = column,
        metric = names(values),
        value = as.numeric(values),
        stringsAsFactors = FALSE
      )
    }
    correlation <- suppressWarnings(stats::cor(
      clip_panel[, score_columns, drop = FALSE],
      use = "pairwise.complete.obs"
    ))
    rank_value <- if (is.matrix(correlation) && all(is.finite(correlation))) {
      safe_effective_rank(correlation)
    } else {
      NA_real_
    }
    rows[[length(rows) + 1L]] <- data.frame(
      clip = clip,
      unit = "clip_panel",
      metric = "effective_rank",
      value = rank_value,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

revision_metric_scale <- function(values) {
  scale <- stats::IQR(values, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-8) {
    scale <- stats::sd(values, na.rm = TRUE)
  }
  if (!is.finite(scale) || scale <= 1e-8) 1 else scale
}

revision_wasserstein_1d <- function(first, second) {
  first <- sort(first[is.finite(first)])
  second <- sort(second[is.finite(second)])
  if (length(first) == 0L || length(second) == 0L) {
    return(NA_real_)
  }
  probabilities <- seq(0, 1, length.out = max(length(first), length(second)))
  first_quantiles <- stats::quantile(first, probabilities, names = FALSE, type = 8)
  second_quantiles <- stats::quantile(second, probabilities, names = FALSE, type = 8)
  mean(abs(first_quantiles - second_quantiles))
}

revision_empirical_continuous_loss <- function(target, synthetic) {
  metrics <- intersect(unique(target$metric), unique(synthetic$metric))
  components <- vapply(metrics, function(metric) {
    empirical <- target$value[target$metric == metric]
    generated <- synthetic$value[synthetic$metric == metric]
    empirical <- empirical[is.finite(empirical)]
    generated <- generated[is.finite(generated)]
    if (length(empirical) == 0L || length(generated) == 0L) {
      return(NA_real_)
    }
    scale <- revision_metric_scale(empirical)
    median_difference <- abs(stats::median(generated) - stats::median(empirical)) / scale
    wasserstein <- revision_wasserstein_1d(empirical, generated) / scale
    median_difference + wasserstein
  }, numeric(1))
  if (length(components) == 0L || any(!is.finite(components))) Inf else mean(components)
}

revision_fit_training_frozen_summary <- function(training_panel,
                                                  score_columns,
                                                  ncores,
                                                  seed) {
  fit <- fit_pooled_ord0_dynega(
    training_panel,
    score_cols = score_columns,
    n_embed = 10L,
    tau = 1,
    algorithm = "louvain",
    ncores = ncores,
    seed = seed,
    verbose = FALSE
  )
  center <- vapply(
    training_panel[, score_columns, drop = FALSE],
    mean,
    numeric(1),
    na.rm = TRUE
  )
  scale <- vapply(
    training_panel[, score_columns, drop = FALSE],
    stats::sd,
    numeric(1),
    na.rm = TRUE
  )
  scale[!is.finite(scale) | scale <= 0] <- 1
  list(
    community_map = fit$community_map[score_columns],
    center = center,
    scale = scale,
    n_dim = fit$n_dim
  )
}

revision_empirical_fold_summary <- function(training_panel,
                                            score_columns,
                                            fold,
                                            context,
                                            ncores) {
  directory <- file.path(context$output_dir, "empirical_fold_summaries")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  model_path <- file.path(directory, sprintf("fold_%02d.rds", fold))
  metadata_path <- file.path(directory, sprintf("fold_%02d.json", fold))
  training_hash <- revision_object_sha256(
    training_panel[, c("clip", "frame_index", score_columns), drop = FALSE]
  )
  if (file.exists(model_path) || file.exists(metadata_path)) {
    if (!file.exists(model_path) || !file.exists(metadata_path)) {
      stop("Empirical fold summarizer is only partially persisted.", call. = FALSE)
    }
    metadata <- jsonlite::fromJSON(metadata_path, simplifyVector = FALSE)
    actual_hash <- revision_sha256(model_path)
    if (!identical(metadata$config_hash, context$config_hash) ||
        !identical(as.integer(metadata$fold), as.integer(fold)) ||
        !identical(metadata$training_data_hash, training_hash) ||
        !identical(metadata$model_hash, actual_hash)) {
      stop("Persisted empirical fold summarizer failed provenance validation.", call. = FALSE)
    }
    return(readRDS(model_path))
  }
  seed <- revision_seed_from_key(paste(
    context$config_hash,
    fold,
    "training_frozen_dynega",
    sep = "|"
  ))
  set.seed(seed)
  model <- revision_fit_training_frozen_summary(
    training_panel,
    score_columns,
    ncores,
    seed
  )
  model$fold <- fold
  model$config_hash <- context$config_hash
  model$training_data_hash <- training_hash
  model$seed <- seed
  temporary <- tempfile("fold-summary-", tmpdir = directory, fileext = ".rds")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(model, temporary, version = 3)
  if (!file.rename(temporary, model_path)) {
    stop("Could not persist empirical fold summarizer.", call. = FALSE)
  }
  revision_write_json(
    list(
      fold = fold,
      config_hash = context$config_hash,
      training_data_hash = training_hash,
      seed = seed,
      model_hash = revision_sha256(model_path)
    ),
    metadata_path
  )
  model
}

revision_apply_training_frozen_summary <- function(panel,
                                                   model,
                                                   score_columns) {
  standardized <- sweep(
    as.matrix(panel[, score_columns, drop = FALSE]),
    2L,
    model$center[score_columns],
    "-"
  )
  standardized <- sweep(
    standardized,
    2L,
    model$scale[score_columns],
    "/"
  )
  communities <- sort(unique(model$community_map))
  scores <- vapply(communities, function(community) {
    columns <- names(model$community_map)[model$community_map == community]
    rowMeans(standardized[, columns, drop = FALSE])
  }, numeric(nrow(standardized)))
  scores <- as.matrix(scores)
  colnames(scores) <- paste0("frozen_dim_", seq_len(ncol(scores)))
  output <- panel[, c("clip", "frame_index"), drop = FALSE]
  cbind(output, as.data.frame(scores, stringsAsFactors = FALSE))
}

revision_clip_metric_contributions <- function(empirical_panel,
                                               synthetic_panel,
                                               columns,
                                               fold,
                                               heldout_rep,
                                               family) {
  empirical <- revision_empirical_metric_distribution(empirical_panel, columns)
  synthetic <- revision_empirical_metric_distribution(synthetic_panel, columns)
  names(empirical)[names(empirical) == "value"] <- "empirical"
  names(synthetic)[names(synthetic) == "value"] <- "synthetic"
  rows <- merge(
    empirical,
    synthetic,
    by = c("clip", "unit", "metric"),
    sort = FALSE
  )
  scales <- vapply(rows$metric, function(metric) {
    revision_metric_scale(rows$empirical[rows$metric == metric])
  }, numeric(1))
  signed <- (rows$synthetic - rows$empirical) / scales
  data.frame(
    contribution_key = paste(
      fold,
      heldout_rep,
      rows$clip,
      rows$unit,
      rows$metric,
      family,
      sep = "|"
    ),
    fold = fold,
    heldout_rep = heldout_rep,
    clip = rows$clip,
    unit = rows$unit,
    metric = rows$metric,
    family = family,
    empirical = rows$empirical,
    synthetic = rows$synthetic,
    scale = scales,
    signed_discrepancy = signed,
    absolute_discrepancy = abs(signed),
    stringsAsFactors = FALSE
  )
}

revision_write_contribution_part <- function(rows, panel_key, output_dir) {
  directory <- file.path(output_dir, "clip_contribution_parts")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, paste0(revision_hash_int(panel_key), ".csv"))
  if (file.exists(path)) {
    existing <- utils::read.csv(path, stringsAsFactors = FALSE)
    if (!identical(existing$contribution_key, rows$contribution_key)) {
      stop("Existing clip-contribution part does not match its panel key.", call. = FALSE)
    }
    return(invisible(path))
  }
  temporary <- tempfile("contribution-", tmpdir = directory, fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(rows, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Could not publish clip-contribution part.", call. = FALSE)
  }
  invisible(path)
}

revision_publish_clip_contributions <- function(output_dir) {
  paths <- list.files(
    file.path(output_dir, "clip_contribution_parts"),
    pattern = "[.]csv$",
    full.names = TRUE
  )
  if (length(paths) != 500L) {
    stop("Expected 500 held-out clip-contribution parts.", call. = FALSE)
  }
  rows <- do.call(rbind, lapply(paths, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }))
  if (anyDuplicated(rows$contribution_key)) {
    stop("Clip-contribution keys must be unique.", call. = FALSE)
  }
  destination <- file.path(output_dir, "clip_contributions.csv")
  if (file.exists(destination)) {
    existing <- utils::read.csv(
      destination,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (!identical(existing$contribution_key, rows$contribution_key) ||
        !isTRUE(all.equal(existing, rows, check.attributes = FALSE))) {
      stop("Existing clip_contributions.csv differs from persisted parts.", call. = FALSE)
    }
    return(invisible(destination))
  }
  temporary <- tempfile("clip-contributions-", tmpdir = output_dir, fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(rows, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, destination)) {
    stop("Could not publish clip_contributions.csv.", call. = FALSE)
  }
  invisible(destination)
}

revision_simulate_empirical_panel <- function(parameters,
                                              clip_lengths,
                                              score_columns,
                                              channel_intercepts,
                                              seed) {
  set.seed(seed)
  panel_rows <- vector("list", length(clip_lengths))
  transition <- revision_exact_transition(
    parameters$eta,
    parameters$zeta,
    parameters$dt
  )
  stability <- revision_stability(
    parameters$eta,
    parameters$zeta,
    parameters$dt
  )
  for (clip_index in seq_along(clip_lengths)) {
    length_value <- as.integer(clip_lengths[[clip_index]])
    dimension <- length(score_columns)
    states <- revision_initial_states(
      rep(parameters$eta, dimension),
      rep(parameters$zeta, dimension),
      rep(parameters$sigma_q, dimension),
      parameters$dt,
      as.integer((as.double(seed) + clip_index) %% 2147483646 + 1)
    )
    latent <- matrix(NA_real_, nrow = length_value, ncol = dimension)
    latent[1L, ] <- states[, 1L]
    velocity <- states[, 2L]
    if (length_value > 1L) {
      for (time in 2:length_value) {
        shocks <- stats::rnorm(dimension, sd = parameters$sigma_q)
        for (channel in seq_len(dimension)) {
          updated <- transition$transition %*%
            c(latent[time - 1L, channel], velocity[[channel]]) +
            transition$innovation_map * shocks[[channel]]
          latent[time, channel] <- updated[[1L]]
          velocity[[channel]] <- updated[[2L]]
        }
      }
    }
    linked <- revision_row_softmax(
      sweep(latent, 2L, channel_intercepts, "+"),
      alpha = 0,
      tau = parameters$cv_tau
    )
    frame <- as.data.frame(linked, stringsAsFactors = FALSE)
    names(frame) <- score_columns
    clip_ids <- names(clip_lengths)
    frame$clip <- if (is.null(clip_ids) || !nzchar(clip_ids[[clip_index]])) {
      paste0("synthetic_", clip_index)
    } else {
      clip_ids[[clip_index]]
    }
    frame$frame_index <- seq_len(length_value)
    panel_rows[[clip_index]] <- frame
  }
  panel <- do.call(rbind, panel_rows)
  list(
    panel = panel,
    stability_class = stability$stability_class[[1]],
    spectral_radius = stability$discrete_spectral_radius[[1]],
    observed_horizon = (as.numeric(clip_lengths) - 1) * parameters$dt
  )
}

revision_fold_balance_table <- function(assignments) {
  keys <- unique(assignments[, c("fold", "stratum"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(index) {
    selected <- assignments$fold == keys$fold[[index]] &
      assignments$stratum == keys$stratum[[index]]
    lengths <- assignments$clip_length[selected]
    data.frame(
      fold = keys$fold[[index]],
      truth_label = keys$stratum[[index]],
      clip_count = sum(selected),
      min_length = min(lengths),
      mean_length = mean(lengths),
      max_length = max(lengths),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

revision_empirical_result_row <- function(plan_row,
                                          stage,
                                          loss,
                                          secondary_loss,
                                          simulated,
                                          context,
                                          source_hash,
                                          input_spec_hash,
                                          panel_hash,
                                          frozen_summary_hash,
                                          selected = FALSE) {
  candidate_value <- function(name, fallback = NA) {
    if (name %in% names(plan_row)) plan_row[[name]][[1]] else fallback
  }
  horizons <- simulated$observed_horizon
  data.frame(
    unit_key = plan_row$panel_key[[1]],
    panel_key = plan_row$panel_key[[1]],
    stage = stage,
    status = "ok",
    error_message = NA_character_,
    fold = plan_row$fold[[1]],
    candidate_id = candidate_value("candidate_id", NA_integer_),
    heldout_rep = candidate_value("heldout_rep", NA_integer_),
    calibration_regime = candidate_value(
      "calibration_regime",
      candidate_value("selected_calibration_regime", NA_character_)
    ),
    eta = candidate_value("eta", NA_real_),
    zeta = candidate_value("zeta", NA_real_),
    sigma_q = candidate_value("sigma_q", NA_real_),
    dt = candidate_value("dt", NA_real_),
    tau = candidate_value("cv_tau", candidate_value("selected_cv_tau", NA_real_)),
    alpha = 0,
    summarizer = "mean_scaled_median_plus_wasserstein_six_metrics",
    continuous_loss = loss,
    secondary_frozen_dynega_loss = secondary_loss,
    selected_by_training_loss = selected,
    selection_locked_before_heldout = stage == "heldout",
    heldout_access_during_selection = FALSE,
    seed_base = plan_row$seed_base[[1]],
    config_hash = context$config_hash,
    generator_hash = context$generator_hash,
    input_spec_hash = input_spec_hash,
    input_data_hash = source_hash,
    panel_input_hash = panel_hash,
    frozen_summary_hash = frozen_summary_hash,
    stability_class = simulated$stability_class,
    discrete_spectral_radius = simulated$spectral_radius,
    min_observed_horizon = min(horizons),
    mean_observed_horizon = mean(horizons),
    max_observed_horizon = max(horizons),
    stringsAsFactors = FALSE
  )
}

revision_evaluate_empirical_training <- function(row,
                                                 score_columns,
                                                 clip_lengths,
                                                 intercepts,
                                                 target,
                                                 frozen_model,
                                                 secondary_target,
                                                 context,
                                                 source_hash,
                                                 input_spec_hash) {
  parameters <- as.list(row[1L, c(
    "eta", "zeta", "sigma_q", "dt", "cv_tau"
  ), drop = FALSE])
  simulated <- revision_simulate_empirical_panel(
    parameters,
    clip_lengths,
    score_columns,
    intercepts,
    row$seed_base[[1]]
  )
  synthetic_metrics <- revision_empirical_metric_distribution(
    simulated$panel,
    score_columns
  )
  loss <- revision_empirical_continuous_loss(target, synthetic_metrics)
  frozen_panel <- revision_apply_training_frozen_summary(
    simulated$panel,
    frozen_model,
    score_columns
  )
  frozen_columns <- setdiff(names(frozen_panel), c("clip", "frame_index"))
  secondary_loss <- revision_empirical_continuous_loss(
    secondary_target,
    revision_empirical_metric_distribution(frozen_panel, frozen_columns)
  )
  revision_empirical_result_row(
    row,
    "training",
    loss,
    secondary_loss,
    simulated,
    context,
    source_hash,
    input_spec_hash,
    revision_object_sha256(simulated$panel),
    revision_object_sha256(frozen_model)
  )
}

revision_empirical_worker_exports <- function() {
  candidates <- ls(envir = .GlobalEnv)
  grep(
    "^(revision_|compute_run_length_entropy$|compute_spectral_entropy$|safe_effective_rank$)",
    candidates,
    value = TRUE
  )
}

revision_start_empirical_cluster <- function(workers) {
  workers <- as.integer(workers)
  if (workers <= 1L) {
    return(NULL)
  }
  cluster <- parallel::makePSOCKcluster(workers)
  initialized <- FALSE
  on.exit({
    if (!initialized) {
      parallel::stopCluster(cluster)
    }
  }, add = TRUE)
  parallel::clusterExport(
    cluster,
    revision_empirical_worker_exports(),
    envir = .GlobalEnv
  )
  invisible(parallel::clusterCall(cluster, function() {
    Sys.setenv(
      OMP_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1"
    )
    TRUE
  }))
  initialized <- TRUE
  cluster
}

revision_stop_empirical_cluster <- function(cluster) {
  if (!is.null(cluster)) {
    parallel::stopCluster(cluster)
  }
  invisible(TRUE)
}

revision_empirical_worker_inputs <- function(rows,
                                             score_columns,
                                             clip_lengths,
                                             intercepts,
                                             target,
                                             frozen_model,
                                             secondary_target,
                                             context,
                                             source_hash,
                                             input_spec_hash) {
  list(
    rows = rows,
    score_columns = score_columns,
    clip_lengths = clip_lengths,
    intercepts = intercepts,
    target = target,
    frozen_model = frozen_model,
    secondary_target = secondary_target,
    context = context,
    source_hash = source_hash,
    input_spec_hash = input_spec_hash
  )
}

revision_evaluate_empirical_worker_input <- function(index, inputs) {
  revision_evaluate_empirical_training(
    inputs$rows[index, , drop = FALSE],
    inputs$score_columns,
    inputs$clip_lengths,
    inputs$intercepts,
    inputs$target,
    inputs$frozen_model,
    inputs$secondary_target,
    inputs$context,
    inputs$source_hash,
    inputs$input_spec_hash
  )
}

revision_evaluate_empirical_psock_index <- function(index) {
  inputs <- get("REVISION_EMPIRICAL_WORKER_INPUTS", envir = .GlobalEnv)
  revision_evaluate_empirical_worker_input(index, inputs)
}

revision_configure_empirical_cluster <- function(cluster, inputs) {
  if (is.null(cluster)) {
    return(invisible(TRUE))
  }
  invisible(parallel::clusterCall(cluster, function(value) {
    assign("REVISION_EMPIRICAL_WORKER_INPUTS", value, envir = .GlobalEnv)
    TRUE
  }, inputs))
}

revision_evaluate_empirical_batch <- function(cluster, indices, inputs) {
  if (is.null(cluster)) {
    return(lapply(
      indices,
      revision_evaluate_empirical_worker_input,
      inputs = inputs
    ))
  }
  parallel::parLapplyLB(
    cluster,
    indices,
    revision_evaluate_empirical_psock_index
  )
}

revision_run_empirical_cv <- function(plan, context, config) {
  source_path <- Sys.getenv("REVISION_EMPIRICAL_FRAME_SCORES", "")
  if (!nzchar(source_path) || !file.exists(source_path)) {
    stop(
      "Set REVISION_EMPIRICAL_FRAME_SCORES to the MAFW-derived frame-score CSV.",
      call. = FALSE
    )
  }
  job <- revision_config_job(config, context$job_id)
  frozen_dynega_ncores <- as.integer(job$parameters$frozen_dynega_ncores)
  revision_source_estimation_core()
  cluster <- revision_start_empirical_cluster(context$workers)
  on.exit(revision_stop_empirical_cluster(cluster), add = TRUE)
  cleaned <- clean_empirical_frame_scores(
    source_path,
    model_name = "revision_v2"
  )
  panel <- cleaned$panel
  usable <- cleaned$qc[!cleaned$qc$excluded, , drop = FALSE]
  if (nrow(usable) != 100L) {
    stop("Empirical CV requires exactly 100 usable clips.", call. = FALSE)
  }
  assignments <- revision_assign_clip_folds(
    usable$clip,
    usable$truth_label,
    usable$usable_length,
    5L
  )
  fold_counts <- table(assignments$fold)
  if (!identical(as.integer(fold_counts), rep(20L, 5L))) {
    stop("Deterministic fold assignment must yield 20 clips per fold.", call. = FALSE)
  }
  utils::write.csv(
    revision_fold_balance_table(assignments),
    file.path(context$output_dir, "fold_balance.csv"),
    row.names = FALSE,
    na = ""
  )
  checkpoint_path <- revision_checkpoint_path(context, config)
  revision_assert_fresh_or_resume(context, checkpoint_path)
  existing <- revision_read_checkpoint(checkpoint_path)
  completed <- if (nrow(existing) == 0L) character() else existing$unit_key
  score_columns <- intersect(EMPIRICAL_EMOTION_COLUMNS, names(panel))
  source_hash <- revision_sha256(source_path)
  input_spec_hash <- revision_object_sha256(list(
    job_id = context$job_id,
    config_hash = context$config_hash,
    axes = revision_empirical_calibration_axes(),
    fold_assignments = assignments,
    source_hash = source_hash
  ))
  for (fold in seq_len(5L)) {
    heldout_ids <- assignments$clip_id[assignments$fold == fold]
    training_panel <- panel[!panel$clip %in% heldout_ids, , drop = FALSE]
    training_lengths <- table(training_panel$clip)
    training_intercepts <- log(pmax(
      colMeans(training_panel[, score_columns, drop = FALSE]),
      1e-8
    ))
    training_intercepts <- training_intercepts - mean(training_intercepts)
    target <- revision_empirical_metric_distribution(training_panel, score_columns)
    frozen_model <- revision_empirical_fold_summary(
      training_panel,
      score_columns,
      fold,
      context,
      frozen_dynega_ncores
    )
    frozen_training <- revision_apply_training_frozen_summary(
      training_panel,
      frozen_model,
      score_columns
    )
    frozen_columns <- setdiff(names(frozen_training), c("clip", "frame_index"))
    secondary_target <- revision_empirical_metric_distribution(
      frozen_training,
      frozen_columns
    )
    rows <- plan$panels$training[plan$panels$training$fold == fold, , drop = FALSE]
    rows <- rows[!rows$panel_key %in% completed, , drop = FALSE]
    if (nrow(rows) > 0L) {
      worker_inputs <- revision_empirical_worker_inputs(
        rows,
        score_columns,
        training_lengths,
        training_intercepts,
        target,
        frozen_model,
        secondary_target,
        context,
        source_hash,
        input_spec_hash
      )
      revision_configure_empirical_cluster(cluster, worker_inputs)
      batches <- split(seq_len(nrow(rows)), ceiling(seq_len(nrow(rows)) / context$workers))
      for (indices in batches) {
        evaluated <- revision_evaluate_empirical_batch(
          cluster,
          indices,
          worker_inputs
        )
        frame <- do.call(rbind, evaluated)
        revision_append_checkpoint(frame, checkpoint_path)
        completed <- c(completed, frame$unit_key)
      }
    }
    training_results <- revision_read_checkpoint(checkpoint_path)
    training_results <- training_results[
      training_results$stage == "training" & training_results$fold == fold,
      , drop = FALSE
    ]
    if (nrow(training_results) != 2700L || any(!is.finite(training_results$continuous_loss))) {
      stop("A fold must complete 2,700 finite training losses before selection.", call. = FALSE)
    }
    selected <- training_results[which.min(training_results$continuous_loss), , drop = FALSE]
    selected_candidate <- plan$panels$candidates[
      plan$panels$candidates$candidate_id == selected$candidate_id[[1]],
      , drop = FALSE
    ]
    heldout_panel <- panel[panel$clip %in% heldout_ids, , drop = FALSE]
    heldout_lengths <- table(heldout_panel$clip)
    heldout_target <- revision_empirical_metric_distribution(
      heldout_panel,
      score_columns
    )
    frozen_heldout <- revision_apply_training_frozen_summary(
      heldout_panel,
      frozen_model,
      score_columns
    )
    heldout_secondary_target <- revision_empirical_metric_distribution(
      frozen_heldout,
      frozen_columns
    )
    heldout_rows <- plan$panels$heldout[plan$panels$heldout$fold == fold, , drop = FALSE]
    heldout_rows <- heldout_rows[!heldout_rows$panel_key %in% completed, , drop = FALSE]
    for (index in seq_len(nrow(heldout_rows))) {
      row <- heldout_rows[index, , drop = FALSE]
      row$candidate_id <- selected_candidate$candidate_id[[1]]
      row$calibration_regime <- selected_candidate$calibration_regime[[1]]
      row$eta <- selected_candidate$eta[[1]]
      row$zeta <- selected_candidate$zeta[[1]]
      row$sigma_q <- selected_candidate$sigma_q[[1]]
      row$dt <- selected_candidate$dt[[1]]
      row$cv_tau <- selected$tau[[1]]
      parameters <- as.list(row[1L, c(
        "eta", "zeta", "sigma_q", "dt", "cv_tau"
      ), drop = FALSE])
      simulated <- revision_simulate_empirical_panel(
        parameters,
        heldout_lengths,
        score_columns,
        training_intercepts,
        row$seed_base[[1]]
      )
      loss <- revision_empirical_continuous_loss(
        heldout_target,
        revision_empirical_metric_distribution(simulated$panel, score_columns)
      )
      frozen_synthetic <- revision_apply_training_frozen_summary(
        simulated$panel,
        frozen_model,
        score_columns
      )
      secondary_loss <- revision_empirical_continuous_loss(
        heldout_secondary_target,
        revision_empirical_metric_distribution(frozen_synthetic, frozen_columns)
      )
      contributions <- rbind(
        revision_clip_metric_contributions(
          heldout_panel,
          simulated$panel,
          score_columns,
          fold,
          row$heldout_rep[[1]],
          "observable"
        ),
        revision_clip_metric_contributions(
          frozen_heldout,
          frozen_synthetic,
          frozen_columns,
          fold,
          row$heldout_rep[[1]],
          "secondary_frozen_dynega"
        )
      )
      revision_write_contribution_part(
        contributions,
        row$panel_key[[1]],
        context$output_dir
      )
      result <- revision_empirical_result_row(
        row,
        "heldout",
        loss,
        secondary_loss,
        simulated,
        context,
        source_hash,
        input_spec_hash,
        revision_object_sha256(simulated$panel),
        revision_object_sha256(frozen_model),
        selected = TRUE
      )
      revision_append_checkpoint(result, checkpoint_path)
      completed <- c(completed, result$unit_key)
    }
  }
  revision_publish_clip_contributions(context$output_dir)
  revision_read_checkpoint(checkpoint_path)
}



revision_publish_checkpoint_results <- function(checkpoint_path, output_dir) {
  destination <- file.path(output_dir, "results.csv")
  if (file.exists(destination)) {
    if (!identical(revision_sha256(destination), revision_sha256(checkpoint_path))) {
      stop("Existing results.csv differs from its checkpoint.", call. = FALSE)
    }
    return(destination)
  }
  temporary <- tempfile("results-", tmpdir = output_dir, fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  if (!file.copy(checkpoint_path, temporary, overwrite = FALSE) ||
      !file.rename(temporary, destination)) {
    stop("Could not publish results.csv atomically.", call. = FALSE)
  }
  destination
}

revision_expected_completion_identity <- function(plan, context) {
  job_id <- context$job_id
  panels <- plan$panels
  if (identical(job_id, "empirical_cv")) {
    unit_keys <- c(panels$training$panel_key, panels$heldout$panel_key)
    return(list(
      unit_keys = unit_keys,
      base_panel_keys = unit_keys,
      representation_keys = unit_keys,
      structure_fit_keys = character()
    ))
  }
  if (identical(job_id, "t100_reuse_audit")) {
    return(list(
      unit_keys = panels$panel_key,
      base_panel_keys = panels$panel_key,
      representation_keys = panels$panel_key,
      structure_fit_keys = character()
    ))
  }
  if (is.data.frame(panels)) {
    representation_keys <- as.character(panels$representation_key)
    structure_fit_keys <- unlist(lapply(representation_keys, function(key) {
      paste(key, plan$methods, sep = "|")
    }), use.names = FALSE)
    unit_keys <- unlist(lapply(structure_fit_keys, function(key) {
      paste(key, REVISION_COMMUNITY_ALGORITHMS, sep = "|")
    }), use.names = FALSE)
    return(list(
      unit_keys = unit_keys,
      base_panel_keys = unique(as.character(panels$panel_key)),
      representation_keys = representation_keys,
      structure_fit_keys = structure_fit_keys
    ))
  }
  list(
    unit_keys = paste(job_id, context$config_hash, sep = "|"),
    base_panel_keys = character(),
    representation_keys = character(),
    structure_fit_keys = character()
  )
}

revision_identity_mismatch <- function(label, expected, observed) {
  expected <- sort(unique(as.character(expected)))
  observed <- sort(unique(as.character(observed)))
  if (identical(expected, observed)) {
    return(NULL)
  }
  missing <- setdiff(expected, observed)
  extra <- setdiff(observed, expected)
  paste0(
    label,
    " mismatch; missing(", length(missing), ")=",
    paste(head(missing, 5L), collapse = ", "),
    "; extra(", length(extra), ")=",
    paste(head(extra, 5L), collapse = ", ")
  )
}

revision_validate_completion_rows <- function(rows, identity) {
  required <- c("unit_key", "panel_key")
  if (!is.data.frame(rows) || !all(required %in% names(rows))) {
    stop("Completion rows must contain unit_key and panel_key.", call. = FALSE)
  }
  unit_keys <- as.character(rows$unit_key)
  if (anyNA(unit_keys) || any(!nzchar(unit_keys)) || anyDuplicated(unit_keys)) {
    stop("Completion unit keys must be nonempty and unique.", call. = FALSE)
  }
  issues <- Filter(Negate(is.null), list(
    revision_identity_mismatch("unit_key", identity$unit_keys, unit_keys)
  ))
  if (length(identity$base_panel_keys) > 0L) {
    issues <- c(issues, Filter(Negate(is.null), list(
      revision_identity_mismatch(
        "base panel_key",
        identity$base_panel_keys,
        rows$panel_key
      )
    )))
  }
  observed_representation_keys <- character()
  if (length(identity$representation_keys) > 0L) {
    observed_representation_keys <- if ("representation_key" %in% names(rows)) {
      rows$representation_key
    } else {
      rows$panel_key
    }
    issues <- c(issues, Filter(Negate(is.null), list(
      revision_identity_mismatch(
        "representation_key",
        identity$representation_keys,
        observed_representation_keys
      )
    )))
  }
  if (length(identity$structure_fit_keys) > 0L) {
    if (!"structure_fit_key" %in% names(rows)) {
      issues <- c(issues, "structure_fit_key column is missing")
    } else {
      issues <- c(issues, Filter(Negate(is.null), list(
        revision_identity_mismatch(
          "structure_fit_key",
          identity$structure_fit_keys,
          rows$structure_fit_key
        )
      )))
      revision_validate_structure_fit_contract(
        rows,
        identity$structure_fit_keys
      )
    }
  }
  if (length(issues) > 0L) {
    stop(
      "Completion identity validation failed: ",
      paste(issues, collapse = " | "),
      call. = FALSE
    )
  }
  list(
    observed_unique_units = length(unique(unit_keys)),
    observed_unique_base_panels = length(unique(identity$base_panel_keys)),
    observed_unique_representation_panels = length(
      unique(observed_representation_keys)
    ),
    observed_unique_structure_fits = length(unique(identity$structure_fit_keys))
  )
}

revision_complete_job <- function(rows,
                                  context,
                                  config,
                                  expected_counts,
                                  plan,
                                  publish_results = TRUE) {
  expected_units <- expected_counts$expected_units
  identity <- revision_expected_completion_identity(plan, context)
  observed <- revision_validate_completion_rows(rows, identity)
  if (!identical(observed$observed_unique_units, expected_units)) {
    stop(
      "Completion expected_units differs from the prospective unit-key set.",
      call. = FALSE
    )
  }
  checkpoint_path <- revision_checkpoint_path(context, config)
  if (!file.exists(checkpoint_path)) {
    stop("Completion checkpoint is missing.", call. = FALSE)
  }
  if (publish_results) {
    revision_publish_checkpoint_results(checkpoint_path, context$output_dir)
  }
  fold_summaries <- list.files(
    file.path(context$output_dir, "empirical_fold_summaries"),
    pattern = "[.]rds$",
    full.names = TRUE
  )
  fold_summary_hashes <- if (length(fold_summaries) == 0L) {
    list()
  } else {
    setNames(lapply(fold_summaries, revision_sha256), basename(fold_summaries))
  }
  revision_write_run_manifest(
    context,
    config,
    "complete",
    list(
      expected_units = expected_units,
      observed_unique_units = observed$observed_unique_units,
      observed_unique_base_panels = observed$observed_unique_base_panels,
      observed_unique_representation_panels =
        observed$observed_unique_representation_panels,
      observed_unique_structure_fits = observed$observed_unique_structure_fits,
      empirical_fold_summarizer_hashes = fold_summary_hashes,
      completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
  expected_outputs <- revision_expected_outputs(context$job_id)
  hash_names <- setdiff(expected_outputs, config$defaults$success_marker)
  missing <- hash_names[!file.exists(file.path(context$output_dir, hash_names))]
  if (length(missing) > 0L) {
    stop(
      "Cannot complete job; expected outputs are missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  output_hashes <- setNames(
    lapply(file.path(context$output_dir, hash_names), revision_sha256),
    hash_names
  )
  marker <- list(
    job_id = context$job_id,
    config_hash = context$config_hash,
    expected_units = expected_units,
    observed_unique_units = observed$observed_unique_units,
    prospective_manifest_valid = TRUE,
    run_manifest_valid = TRUE,
    output_hashes = output_hashes
  )
  failure_path <- file.path(context$output_dir, config$defaults$failure_marker)
  if (file.exists(failure_path)) {
    archived_failure <- file.path(
      context$output_dir,
      paste0(
        config$defaults$failure_marker,
        ".previous.",
        format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
        ".json"
      )
    )
    if (!file.rename(failure_path, archived_failure)) {
      stop("Could not archive the prior failure marker.", call. = FALSE)
    }
  }
  revision_write_json(
    marker,
    file.path(context$output_dir, config$defaults$success_marker)
  )
  invisible(marker)
}

revision_write_failure <- function(error, context, config) {
  revision_write_run_manifest(
    context,
    config,
    "failed",
    list(error_message = conditionMessage(error))
  )
  revision_write_json(
    list(
      job_id = context$job_id,
      config_hash = context$config_hash,
      status = "failed",
      error_message = conditionMessage(error)
    ),
    file.path(context$output_dir, config$defaults$failure_marker)
  )
  invisible(error)
}

revision_singleton_checkpoint <- function(context, config, stage, artifact) {
  checkpoint_path <- revision_checkpoint_path(context, config)
  revision_assert_fresh_or_resume(context, checkpoint_path)
  if (!file.exists(checkpoint_path)) {
    row <- data.frame(
      unit_key = paste(context$job_id, context$config_hash, sep = "|"),
      panel_key = paste(context$job_id, context$config_hash, sep = "|"),
      stage = stage,
      status = "ok",
      artifact = artifact,
      config_hash = context$config_hash,
      stringsAsFactors = FALSE
    )
    revision_append_checkpoint(row, checkpoint_path)
  }
  revision_read_checkpoint(checkpoint_path)
}

revision_run_aggregate_job <- function(context, config) {
  script <- file.path(
    REVISION_REPO_ROOT,
    "scripts",
    "simulations",
    "build_revision_results.R"
  )
  status <- system2(
    "Rscript",
    c(script, "--config", context$config_path),
    stdout = "",
    stderr = ""
  )
  if (!identical(as.integer(status), 0L)) {
    stop("Revision result builder failed with exit status ", status, ".", call. = FALSE)
  }
  revision_singleton_checkpoint(
    context,
    config,
    "aggregate_results",
    "revision_results_long.csv"
  )
}

revision_run_checked_command <- function(command, arguments) {
  status <- system2(command, arguments, stdout = "", stderr = "")
  if (!identical(as.integer(status), 0L)) {
    stop(
      command, " failed with exit status ", status, ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_dependency_output_dir <- function(context, job_id) {
  path <- context$inputs[[job_id]]
  if (is.null(path) || length(path) != 1L || !nzchar(path)) {
    stop("Missing resolved dependency output for ", job_id, ".", call. = FALSE)
  }
  path <- revision_absolute_path(path)
  if (!dir.exists(path)) {
    stop("Resolved dependency output does not exist: ", path, call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

revision_publication_artifact_names <- function() {
  c(
    "figure4_empirical_realism.pdf",
    "figure4_empirical_realism.png",
    "figure5_dimension_recovery.pdf",
    "figure5_dimension_recovery.png",
    "figure6_factor_effects.pdf",
    "figure6_factor_effects.png",
    "figure7_runtime_feasibility.pdf",
    "figure7_runtime_feasibility.png",
    "figureS1_dimension_recovery_readout_sensitivity.pdf",
    "figureS1_dimension_recovery_readout_sensitivity.png",
    "figureS2_factor_effects_readout_sensitivity.pdf",
    "figureS2_factor_effects_readout_sensitivity.png",
    "empirical_realism_table.csv",
    "table1_method_overall.csv",
    "figure5_condition_key.csv",
    "figure6_factor_effects_table.csv",
    "figure6_factor_effects_eta_screen.csv",
    "table2_score_quality_marginal_means.csv",
    "table3_runtime_feasibility.csv",
    "table3_runtime_feasibility_accounting.csv",
    "supplementary_method_readout_sensitivity.csv",
    "supplementary_score_quality_readout_sensitivity.csv",
    "supplementary_factor_effects_readout_sensitivity.csv"
  )
}

revision_publication_destinations <- function(names) {
  directory <- ifelse(grepl("\\.csv$", names), "tables", "figures")
  file.path(REVISION_REPO_ROOT, "paper", directory, names)
}

revision_publish_hashed_artifacts <- function(sources, destinations) {
  if (length(sources) == 0L || length(sources) != length(destinations) ||
      anyDuplicated(destinations) ||
      any(!file.exists(sources) | file.info(sources)$size <= 0L)) {
    stop("Publication transaction received invalid source artifacts.", call. = FALSE)
  }
  for (directory in unique(dirname(destinations))) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  suffix <- paste0(".revision-v2-", Sys.getpid())
  staged <- paste0(destinations, suffix, ".new")
  backups <- paste0(destinations, suffix, ".previous")
  if (any(file.exists(c(staged, backups)))) {
    stop("Publication transaction staging paths already exist.", call. = FALSE)
  }
  copied <- file.copy(sources, staged, overwrite = FALSE)
  if (!all(copied) || !identical(
    unname(vapply(sources, revision_sha256, character(1))),
    unname(vapply(staged, revision_sha256, character(1)))
  )) {
    unlink(staged[file.exists(staged)])
    stop("Could not stage a hash-identical publication artifact set.", call. = FALSE)
  }
  had_previous <- file.exists(destinations)
  moved_previous <- rep(FALSE, length(destinations))
  installed <- rep(FALSE, length(destinations))
  rollback <- function() {
    unlink(destinations[installed & file.exists(destinations)])
    for (index in rev(which(moved_previous))) {
      if (file.exists(backups[[index]])) {
        file.rename(backups[[index]], destinations[[index]])
      }
    }
    unlink(staged[file.exists(staged)])
    unlink(backups[file.exists(backups)])
  }
  outcome <- tryCatch({
    for (index in seq_along(destinations)) {
      if (had_previous[[index]]) {
        if (!file.rename(destinations[[index]], backups[[index]])) {
          stop("Could not preserve prior artifact: ", destinations[[index]])
        }
        moved_previous[[index]] <- TRUE
      }
      if (!file.rename(staged[[index]], destinations[[index]])) {
        stop("Could not install rebuilt artifact: ", destinations[[index]])
      }
      installed[[index]] <- TRUE
    }
    if (!identical(
      unname(vapply(sources, revision_sha256, character(1))),
      unname(vapply(destinations, revision_sha256, character(1)))
    )) {
      stop("Published artifact hashes differ from the rebuilt set.")
    }
    TRUE
  }, error = function(error) error)
  if (inherits(outcome, "error")) {
    rollback()
    stop(conditionMessage(outcome), call. = FALSE)
  }
  unlink(backups[file.exists(backups)])
  invisible(destinations)
}

revision_validate_published_artifact_hashes <- function(publication) {
  artifact_names <- revision_publication_artifact_names()
  required_metadata <- c(
    "artifact_schema_version", "job_id", "config_hash", "published_root",
    "aggregate_output_dir", "aggregate_success_hash", "builder_hash"
  )
  schema_version <- if (is.list(publication)) {
    publication$artifact_schema_version
  } else {
    NULL
  }
  if (!is.list(publication) || length(schema_version) != 1L ||
      !identical(as.integer(schema_version), 4L) ||
      !identical(publication$published_root, "paper") ||
      !setequal(names(publication), c(required_metadata, artifact_names))) {
    stop("Publication artifact manifest has an invalid schema.", call. = FALSE)
  }
  expected_hashes <- unlist(publication[artifact_names], use.names = FALSE)
  if (length(expected_hashes) != length(artifact_names) ||
      any(!grepl("^[0-9a-f]{64}$", expected_hashes))) {
    stop("Publication artifact manifest contains an invalid SHA-256.", call. = FALSE)
  }
  destinations <- revision_publication_destinations(artifact_names)
  if (any(!file.exists(destinations) | file.info(destinations)$size <= 0L)) {
    stop("A manuscript-facing figure or table is missing.", call. = FALSE)
  }
  observed_hashes <- vapply(destinations, revision_sha256, character(1))
  if (!identical(unname(observed_hashes), unname(expected_hashes))) {
    stop(
      "Manuscript-facing figure/table hashes differ from the completed rebuild.",
      call. = FALSE
    )
  }
  invisible(destinations)
}

revision_run_rebuild_job <- function(context, config) {
  aggregate_dir <- revision_dependency_output_dir(context, "aggregate_results")
  aggregate_files <- c(
    "revision_estimand_spec.csv",
    "upstream_artifact_manifest.csv",
    "revision_results_long.csv",
    "identity_validation.csv",
    "partition_metric_recomputation_audit.csv",
    "revision_estimand_results.csv",
    "revision_paired_differences.csv",
    "_SUCCESS.json"
  )
  if (any(!file.exists(file.path(aggregate_dir, aggregate_files)))) {
    stop("Selected aggregate_results output is incomplete.", call. = FALSE)
  }
  script <- file.path(
    REVISION_REPO_ROOT,
    "scripts",
    "figures",
    "build_revision_v2_artifacts.R"
  )
  if (!file.exists(script)) {
    stop("The revision-v2 figure/table builder is missing.", call. = FALSE)
  }
  publication_root <- file.path(context$output_dir, "revision_v2_publication")
  revision_run_checked_command(
    "Rscript",
    c(
      script,
      "--aggregate-dir",
      aggregate_dir,
      "--output-root",
      publication_root
    )
  )
  generated <- file.path(
    publication_root,
    c(
      rep("figures", 12L),
      rep("tables", 11L)
    ),
    c(
      "figure4_empirical_realism.pdf",
      "figure4_empirical_realism.png",
      "figure5_dimension_recovery.pdf",
      "figure5_dimension_recovery.png",
      "figure6_factor_effects.pdf",
      "figure6_factor_effects.png",
      "figure7_runtime_feasibility.pdf",
      "figure7_runtime_feasibility.png",
      "figureS1_dimension_recovery_readout_sensitivity.pdf",
      "figureS1_dimension_recovery_readout_sensitivity.png",
      "figureS2_factor_effects_readout_sensitivity.pdf",
      "figureS2_factor_effects_readout_sensitivity.png",
      "empirical_realism_table.csv",
      "table1_method_overall.csv",
      "figure5_condition_key.csv",
      "figure6_factor_effects_table.csv",
      "figure6_factor_effects_eta_screen.csv",
      "table2_score_quality_marginal_means.csv",
      "table3_runtime_feasibility.csv",
      "table3_runtime_feasibility_accounting.csv",
      "supplementary_method_readout_sensitivity.csv",
      "supplementary_score_quality_readout_sensitivity.csv",
      "supplementary_factor_effects_readout_sensitivity.csv"
    )
  )
  if (any(!file.exists(generated))) {
    stop("Figure/table rebuild did not produce every declared artifact.", call. = FALSE)
  }
  artifact_names <- revision_publication_artifact_names()
  if (!identical(basename(generated), artifact_names) ||
      any(file.info(generated)$size <= 0L)) {
    stop("Rebuilt artifact names or file sizes violate the publication contract.", call. = FALSE)
  }
  destinations <- revision_publication_destinations(artifact_names)
  revision_publish_hashed_artifacts(generated, destinations)
  artifacts <- c(
    list(
      artifact_schema_version = 4L,
      job_id = context$job_id,
      config_hash = context$config_hash,
      published_root = "paper",
      aggregate_output_dir = aggregate_dir,
      aggregate_success_hash = revision_sha256(
        file.path(aggregate_dir, "_SUCCESS.json")
      ),
      builder_hash = revision_sha256(script)
    ),
    setNames(lapply(destinations, revision_sha256), basename(destinations))
  )
  revision_write_json(
    artifacts,
    file.path(context$output_dir, "publication_artifacts.json")
  )
  revision_validate_published_artifact_hashes(artifacts)
  revision_singleton_checkpoint(
    context,
    config,
    "rebuild_figures_tables",
    "publication_artifacts.json"
  )
}

revision_assert_no_pending_markers <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) != length(paths)) {
    stop("Manuscript integration surfaces are missing.", call. = FALSE)
  }
  findings <- unlist(lapply(existing, function(path) {
    lines <- readLines(path, warn = FALSE)
    indices <- grep("REVISION_[A-Z_]+_PENDING", lines, perl = TRUE)
    if (length(indices) == 0L) {
      return(character())
    }
    paste0(basename(path), ":", indices)
  }), use.names = FALSE)
  if (length(findings) > 0L) {
    stop(
      "Manuscript integration remains gated by revision markers at ",
      paste(head(findings, 20L), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_claim_ledger_statuses <- function(path, claim_ids) {
  if (!file.exists(path)) {
    stop("Claim ledger is missing: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^- id:[[:space:]]+", lines)
  parsed_ids <- trimws(sub("^- id:[[:space:]]+", "", lines[starts]))
  statuses <- setNames(rep(NA_character_, length(claim_ids)), claim_ids)
  for (claim_id in claim_ids) {
    match_index <- which(parsed_ids == claim_id)
    if (length(match_index) != 1L) {
      next
    }
    start <- starts[[match_index]]
    end <- if (match_index < length(starts)) starts[[match_index + 1L]] - 1L else length(lines)
    block <- lines[start:end]
    status_line <- grep("^[[:space:]]+status:[[:space:]]+", block, value = TRUE)
    if (length(status_line) == 1L) {
      statuses[[claim_id]] <- trimws(sub("^[[:space:]]+status:[[:space:]]+", "", status_line))
    }
  }
  statuses
}

revision_validate_claim_ledger <- function(path) {
  claim_ids <- c(
    "clm-v2-held-out-realism",
    "clm-v2-matched-recovery",
    "clm-v2-short-record",
    "clm-v2-measurement-challenges",
    "clm-v2-runtime-feasibility"
  )
  statuses <- revision_claim_ledger_statuses(path, claim_ids)
  blocked <- is.na(statuses) | !nzchar(statuses) | statuses == "needs-evidence"
  if (any(blocked)) {
    stop(
      "Claim-ledger evidence gate is incomplete for: ",
      paste(names(statuses)[blocked], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(statuses)
}

revision_readiness_dependency_hashes <- function(context, config) {
  rebuild_dir <- revision_dependency_output_dir(context, "rebuild_figures_tables")
  publication_path <- file.path(rebuild_dir, "publication_artifacts.json")
  rebuild_success <- file.path(rebuild_dir, config$defaults$success_marker)
  if (!file.exists(publication_path) || !file.exists(rebuild_success)) {
    stop("Rebuild dependency lacks completion evidence.", call. = FALSE)
  }
  publication <- jsonlite::fromJSON(publication_path, simplifyVector = FALSE)
  revision_validate_published_artifact_hashes(publication)
  aggregate_dir <- publication$aggregate_output_dir
  if (is.null(aggregate_dir) || length(aggregate_dir) != 1L || !dir.exists(aggregate_dir)) {
    stop("Rebuild provenance lacks its aggregate output directory.", call. = FALSE)
  }
  aggregate_success <- file.path(aggregate_dir, config$defaults$success_marker)
  if (!file.exists(aggregate_success)) {
    stop("Aggregate dependency lacks completion evidence.", call. = FALSE)
  }
  list(
    aggregate_results = revision_sha256(aggregate_success),
    rebuild_figures_tables = revision_sha256(rebuild_success)
  )
}

revision_validate_integration_receipt <- function(context, config) {
  job <- revision_config_job(config, context$job_id)
  specification <- job$manual_readiness_receipt
  if (!is.list(specification) || is.null(specification$filename)) {
    stop("Integration readiness receipt is not configured.", call. = FALSE)
  }
  path <- file.path(context$output_dir, specification$filename)
  if (!file.exists(path)) {
    stop("Manual integration readiness receipt is absent: ", path, call. = FALSE)
  }
  receipt <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  required <- c(
    "job_id", "config_hash", "status", "reviewer", "reviewed_at",
    "reviewed_dependency_hashes"
  )
  if (!identical(sort(names(receipt)), sort(required))) {
    stop("Manual integration readiness receipt has an invalid schema.", call. = FALSE)
  }
  if (!identical(receipt$job_id, context$job_id) ||
      !identical(receipt$config_hash, context$config_hash) ||
      !identical(receipt$status, specification$required_status) ||
      !is.character(receipt$reviewer) || !nzchar(trimws(receipt$reviewer)) ||
      !is.character(receipt$reviewed_at) || !nzchar(receipt$reviewed_at)) {
    stop("Manual integration readiness receipt fields do not match the plan.", call. = FALSE)
  }
  expected_hashes <- revision_readiness_dependency_hashes(context, config)
  reviewed_hashes <- receipt$reviewed_dependency_hashes
  if (!is.list(reviewed_hashes) ||
      !identical(sort(names(reviewed_hashes)), sort(names(expected_hashes))) ||
      !identical(
        unname(unlist(reviewed_hashes[names(expected_hashes)])),
        unname(unlist(expected_hashes))
      )) {
    stop(
      "Manual integration receipt does not match aggregate/rebuild success hashes.",
      call. = FALSE
    )
  }
  invisible(path)
}

revision_run_integration_job <- function(context, config) {
  manuscript <- file.path(
    REVISION_REPO_ROOT,
    "paper",
    "Paper_emotion_dynamics_HGolino_RR.qmd"
  )
  response <- file.path(REVISION_REPO_ROOT, "paper", "response_letter.tex")
  revision_assert_no_pending_markers(c(manuscript, response))
  revision_validate_claim_ledger(
    file.path(REVISION_REPO_ROOT, "research", "claim-ledger.yaml")
  )
  receipt <- revision_validate_integration_receipt(context, config)
  revision_run_checked_command("quarto", c("render", manuscript, "--to", "pdf"))
  revision_run_checked_command(
    "pdflatex",
    c(
      "-interaction=nonstopmode",
      "-halt-on-error",
      paste0("-output-directory=", dirname(response)),
      response
    )
  )
  outputs <- c(
    manuscript = sub("\\.qmd$", ".pdf", manuscript),
    response_letter = sub("\\.tex$", ".pdf", response)
  )
  if (any(!file.exists(outputs))) {
    stop("Manuscript integration did not produce both PDFs.", call. = FALSE)
  }
  revision_write_json(
    c(
      list(job_id = context$job_id, config_hash = context$config_hash),
      list(readiness_receipt_hash = revision_sha256(receipt)),
      setNames(lapply(outputs, revision_sha256), names(outputs))
    ),
    file.path(context$output_dir, "manuscript_artifacts.json")
  )
  revision_singleton_checkpoint(
    context,
    config,
    "integrate_manuscript",
    "manuscript_artifacts.json"
  )
}

revision_dispatch_full <- function(job_id,
                                   plan,
                                   context,
                                   config,
                                   expected_counts) {
  revision_write_run_manifest(context, config, "running")
  outcome <- tryCatch(
    {
      if (identical(job_id, "empirical_cv")) {
        revision_run_empirical_cv(plan, context, config)
      } else if (identical(job_id, "t100_reuse_audit")) {
        revision_run_reuse_audit(plan, context, config)
      } else if (grepl("^(t25|t100|challenge)_", job_id)) {
        revision_run_structure_job(plan, context, config)
      } else if (identical(job_id, "aggregate_results")) {
        revision_run_aggregate_job(context, config)
      } else if (identical(job_id, "rebuild_figures_tables")) {
        revision_run_rebuild_job(context, config)
      } else if (identical(job_id, "integrate_manuscript")) {
        revision_run_integration_job(context, config)
      } else {
        stop("No execution adapter for ", job_id, ".", call. = FALSE)
      }
    },
    error = function(error) error
  )
  if (inherits(outcome, "error")) {
    revision_write_failure(outcome, context, config)
    stop(conditionMessage(outcome), call. = FALSE)
  }
  publish_results <- "results.csv" %in% revision_expected_outputs(job_id)
  revision_complete_job(
    outcome,
    context,
    config,
    expected_counts,
    plan,
    publish_results
  )
}

revision_main <- function() {
  cli <- revision_parse_cli(commandArgs(trailingOnly = TRUE))
  revision_assert_job_id(cli$job)
  config_input <- revision_read_config(cli$config)
  config <- config_input$value
  job <- revision_config_job(config, cli$job)
  revision_validate_dynega_config(cli$job, job)
  revision_validate_empirical_config(cli$job, job)
  revision_validate_publication_config(cli$job, job)
  expected_counts <- revision_expected_counts(job)
  context <- revision_runtime_context(
    cli$job,
    job,
    config,
    config_input$path
  )
  context$generator_hash <- revision_generator_hash()
  plan <- revision_job_plan(cli$job, REVISION_REPO_ROOT)
  plan$config_hash <- context$config_hash
  revision_write_prospective_manifests(
    cli$job,
    job,
    config,
    context,
    plan,
    expected_counts
  )
  if (cli$dry_run) {
    cat(
      "Validated dry-run plan for ", cli$job,
      " at ", context$output_dir, "\n",
      sep = ""
    )
    return(invisible(0L))
  }
  revision_dispatch_full(
    cli$job,
    plan,
    context,
    config,
    expected_counts
  )
  invisible(0L)
}

if (sys.nframe() == 0L) {
  revision_main()
}
