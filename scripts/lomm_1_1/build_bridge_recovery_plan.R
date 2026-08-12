#!/usr/bin/env Rscript

lomm11_bridge_planner_path <- function() {
  configured <- Sys.getenv("LOMM11_BRIDGE_PLANNER_PATH", "")
  if (nzchar(configured) && file.exists(configured)) {
    return(normalizePath(configured, mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  candidate <- if (length(argument) == 1L) {
    file.path(dirname(normalizePath(sub("^--file=", "", argument), mustWork = TRUE)),
              "build_bridge_recovery_plan.R")
  } else {
    file.path(getwd(), "scripts", "lomm_1_1", "build_bridge_recovery_plan.R")
  }
  normalizePath(candidate, mustWork = TRUE)
}

LOMM11_BRIDGE_PLANNER_PATH <- lomm11_bridge_planner_path()
LOMM11_BRIDGE_DIRECTORY <- dirname(LOMM11_BRIDGE_PLANNER_PATH)
LOMM11_BRIDGE_ROOT <- normalizePath(file.path(LOMM11_BRIDGE_DIRECTORY, "..", ".."),
                                    mustWork = TRUE)
LOMM11_BRIDGE_CONFIG_PATH <- file.path(
  LOMM11_BRIDGE_DIRECTORY, "bridge_recovery_config.json"
)
LOMM11_BRIDGE_SPEC_PATH <- file.path(LOMM11_BRIDGE_DIRECTORY, "bridge_recovery_spec.R")

source(LOMM11_BRIDGE_SPEC_PATH)

lomm11_bridge_planner_require_packages <- function() {
  lomm11_bridge_require_packages("jsonlite")
}

lomm11_bridge_planner_require_packages()

lomm11_bridge_path_is_within <- function(path, root) {
  identical(path, root) || startsWith(path, paste0(root, .Platform$file.sep))
}

lomm11_bridge_future_path <- function(path) {
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) expanded <- file.path(getwd(), expanded)
  existing <- expanded
  suffix <- character()
  while (!file.exists(existing)) {
    parent <- dirname(existing)
    if (identical(parent, existing)) lomm11_bridge_stop("Cannot resolve path: ", path)
    suffix <- c(basename(existing), suffix)
    existing <- parent
  }
  base <- normalizePath(existing, mustWork = TRUE)
  if (length(suffix) == 0L) return(base)
  do.call(file.path, as.list(c(base, suffix)))
}

lomm11_bridge_locked_plan_root <- function(path) {
  observed <- lomm11_bridge_future_path(path)
  expected <- lomm11_bridge_future_path(file.path(
    LOMM11_BRIDGE_ROOT, "simulation_outputs", "lomm_1_1", "recovery_bridge"
  ))
  revision <- lomm11_bridge_future_path(file.path(
    LOMM11_BRIDGE_ROOT, "simulation_outputs", "revision_v2"
  ))
  if (lomm11_bridge_path_is_within(observed, revision)) {
    lomm11_bridge_stop("Recovery-bridge plans cannot use revision_v2.")
  }
  if (!identical(observed, expected)) {
    lomm11_bridge_stop("Recovery-bridge plans require the dedicated output root.")
  }
  observed
}

lomm11_bridge_parse_planner_args <- function(args) {
  parsed <- list(
    mode = NULL,
    config = LOMM11_BRIDGE_CONFIG_PATH,
    output_root = file.path(
      LOMM11_BRIDGE_ROOT, "simulation_outputs", "lomm_1_1", "recovery_bridge"
    )
  )
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token %in% c("--dry-run", "--materialize-plan")) {
      if (!is.null(parsed$mode)) lomm11_bridge_stop("Choose exactly one planner mode.")
      parsed$mode <- sub("^--", "", token)
      index <- index + 1L
      next
    }
    if (token %in% c("--config", "--output-root")) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        lomm11_bridge_stop("Missing value after ", token, ".")
      }
      name <- gsub("-", "_", sub("^--", "", token))
      parsed[[name]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    lomm11_bridge_stop("Unknown planner argument: ", token)
  }
  if (is.null(parsed$mode)) lomm11_bridge_stop("Specify --dry-run or --materialize-plan.")
  config_path <- normalizePath(parsed$config, mustWork = TRUE)
  if (!identical(config_path, normalizePath(LOMM11_BRIDGE_CONFIG_PATH, mustWork = TRUE))) {
    lomm11_bridge_stop("The recovery bridge requires the locked configuration file.")
  }
  parsed$config <- config_path
  parsed$output_root <- lomm11_bridge_locked_plan_root(parsed$output_root)
  parsed
}

lomm11_bridge_plan_tables <- function(config, pair_plan, config_hash) {
  panel_rows <- lapply(seq_len(nrow(pair_plan)), function(index) {
    pair_row <- pair_plan[index, , drop = FALSE]
    do.call(rbind, lapply(LOMM11_BRIDGE_PROFILES, function(profile) {
      row <- lomm11_bridge_profile_row(pair_row, profile, config_hash)
      data.frame(
        pair_key = row$pair_key[[1L]],
        panel_key = row$panel_key[[1L]],
        representation_key = row$representation_key[[1L]],
        scenario = row$scenario[[1L]],
        scenario_family = row$scenario_family[[1L]],
        true_dimension = as.integer(row$scenario_nfac[[1L]]),
        replicate = as.integer(row$rep[[1L]]),
        profile_id = profile,
        seed_base = as.integer(row$seed_base[[1L]]),
        config_hash = config_hash,
        stringsAsFactors = FALSE
      )
    }))
  })
  panels <- do.call(rbind, panel_rows)
  fits <- do.call(rbind, lapply(seq_len(nrow(panels)), function(index) {
    panel <- panels[index, , drop = FALSE]
    do.call(rbind, lapply(LOMM11_BRIDGE_METHODS, function(method) {
      data.frame(
        fit_key = paste(panel$representation_key[[1L]], method, sep = "|"),
        pair_key = panel$pair_key[[1L]],
        panel_key = panel$panel_key[[1L]],
        representation_key = panel$representation_key[[1L]],
        profile_id = panel$profile_id[[1L]],
        method_id = method,
        expected_readouts = length(LOMM11_BRIDGE_ALGORITHMS),
        config_hash = config_hash,
        stringsAsFactors = FALSE
      )
    }))
  }))
  readouts <- do.call(rbind, lapply(seq_len(nrow(fits)), function(index) {
    fit <- fits[index, , drop = FALSE]
    data.frame(
      readout_key = paste(fit$fit_key[[1L]], LOMM11_BRIDGE_ALGORITHMS, sep = "|"),
      fit_key = fit$fit_key[[1L]],
      pair_key = fit$pair_key[[1L]],
      panel_key = fit$panel_key[[1L]],
      profile_id = fit$profile_id[[1L]],
      method_id = fit$method_id[[1L]],
      community_algorithm = LOMM11_BRIDGE_ALGORITHMS,
      primary_readout = LOMM11_BRIDGE_ALGORITHMS == "louvain",
      config_hash = config_hash,
      stringsAsFactors = FALSE
    )
  }))
  panels <- panels[order(panels$pair_key, match(panels$profile_id,
                                                LOMM11_BRIDGE_PROFILES)), ]
  fits <- fits[order(fits$pair_key, match(fits$profile_id, LOMM11_BRIDGE_PROFILES),
                     match(fits$method_id, LOMM11_BRIDGE_METHODS)), ]
  readouts <- readouts[order(
    readouts$pair_key, match(readouts$profile_id, LOMM11_BRIDGE_PROFILES),
    match(readouts$method_id, LOMM11_BRIDGE_METHODS),
    match(readouts$community_algorithm, LOMM11_BRIDGE_ALGORITHMS)
  ), ]
  rownames(panels) <- rownames(fits) <- rownames(readouts) <- NULL
  flattened_pairs <- lomm11_bridge_flatten_plan(pair_plan)
  flattened_pairs$config_hash <- config_hash
  tables <- list(
    pair_plan = flattened_pairs,
    panel_plan = panels,
    fit_plan = fits,
    readout_plan = readouts
  )
  expected <- config$expected_counts
  observed <- c(nrow(flattened_pairs), nrow(panels), nrow(fits), nrow(readouts))
  required <- c(
    as.integer(expected$paired_panel_sets), as.integer(expected$generated_panels),
    as.integer(expected$structure_fits), as.integer(expected$result_rows)
  )
  keys <- c(flattened_pairs$pair_key, panels$panel_key, fits$fit_key,
            readouts$readout_key)
  if (!identical(observed, required) || any(vapply(
    list(flattened_pairs$pair_key, panels$panel_key, fits$fit_key,
         readouts$readout_key), anyDuplicated, integer(1)
  ) != 0L) || any(!nzchar(keys))) {
    lomm11_bridge_stop("Recovery-bridge plan tables violate exact counts or keys.")
  }
  tables
}

lomm11_bridge_csv_sha256 <- function(frame) {
  path <- tempfile("lomm11-bridge-plan-", fileext = ".csv")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  lomm11_bridge_sha256(path)
}

lomm11_bridge_output_schema <- function(config, config_hash) {
  list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    config_hash = config_hash,
    checkpoint_results = list(
      key = c("fit_key", "attempt"),
      columns = c(
        "attempt_key", "fit_key", "pair_key", "panel_key", "profile_id",
        "method_id", "attempt", "status", "error_message", "panel_hash",
        "source_panel_hash", "structure_hash", "runtime_seconds", "started_at",
        "finished_at", "worker_pid", "worker_host", "config_hash",
        "prospective_manifest_sha256", "attempt_artifact",
        "attempt_receipt_sha256"
      ),
      allowed_status = c("ok", "error", "timeout"),
      append_only = TRUE,
      resume_completed_status = "ok"
    ),
    fit_results = list(exact_rows = as.integer(config$expected_counts$structure_fits),
                       unique_key = "fit_key"),
    recovery_results = list(exact_rows = as.integer(config$expected_counts$result_rows),
                            unique_key = "readout_key"),
    bridge_summary = list(exact_rows = 210L,
                          dimensions = c("profile_id", "method_id",
                                         "community_algorithm", "outcome")),
    paired_bootstrap_effects = list(
      exact_rows = 105L,
      dimensions = c("method_id", "community_algorithm", "outcome"),
      bootstrap_replicates = as.integer(config$analysis$bootstrap_replicates),
      bootstrap_unit = config$analysis$bootstrap_unit
    ),
    completion = list(
      terminal_fit_keys = as.integer(config$expected_counts$structure_fits),
      max_attempts_per_fit = as.integer(config$execution$max_attempts_per_fit),
      result_rows = as.integer(config$expected_counts$result_rows),
      required_files = c(
        "checkpoint_results.csv", "fit_results.csv", "recovery_results.csv",
        "bridge_summary.csv", "paired_bootstrap_effects.csv", "run_manifest.json",
        "_FINALIZATION.json", "_SUCCESS.json"
      )
    )
  )
}

lomm11_bridge_plan_manifest <- function(config, pair_plan, tables,
                                        dependency_hashes, config_hash) {
  plan_records <- lapply(names(tables), function(name) {
    list(path = paste0(name, ".csv"), rows = nrow(tables[[name]]),
         sha256 = lomm11_bridge_csv_sha256(tables[[name]]))
  })
  dependencies <- lapply(names(dependency_hashes), function(path) {
    list(path = path, sha256 = unname(dependency_hashes[[path]]))
  })
  summary <- lomm11_bridge_plan_summary(config, pair_plan, config_hash)
  summary$source_realism_run <- unclass(config$source_realism_run)
  summary$dependencies <- dependencies
  summary$plan_artifacts <- plan_records
  summary$output_schema <- lomm11_bridge_output_schema(config, config_hash)
  summary$execution <- list(
    workers_maximum = min(8L, as.integer(config$execution$workers)),
    blas_threads_per_worker = as.integer(config$execution$blas_threads_per_worker),
    graphicalvar_timeout_seconds = as.numeric(
      config$execution$graphicalvar_timeout_seconds
    ),
    gimme_timeout_seconds = as.numeric(config$execution$gimme_timeout_seconds),
    job_timeout_seconds = as.numeric(config$execution$job_timeout_seconds),
    run_output_root = config$execution$run_output_root,
    authorization_token_sha256 = lomm11_bridge_hash_text(
      config$execution$authorization_token
    )
  )
  summary$future_launch <- paste(
    "Rscript scripts/lomm_1_1/run_bridge_recovery.R",
    "--plan-dir", file.path(config$execution$output_root, substr(config_hash, 1L, 12L)),
    "--execute --ack-long-running", config$execution$authorization_token,
    "--prospective-manifest-sha256 <PROSPECTIVE_MANIFEST_SHA256>"
  )
  summary
}

lomm11_bridge_write_json_atomic <- function(value, path) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), tmpdir = directory)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null", digits = NA)
  if (!file.rename(temporary, path)) lomm11_bridge_stop("Atomic JSON publish failed: ", path)
  invisible(path)
}

lomm11_bridge_write_csv <- function(frame, path) {
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  invisible(path)
}

lomm11_bridge_json_sha256 <- function(value) {
  path <- tempfile("lomm11-bridge-manifest-", fileext = ".json")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, path, auto_unbox = TRUE, pretty = TRUE, null = "null",
    na = "null", digits = NA
  )
  lomm11_bridge_sha256(path)
}

lomm11_bridge_materialize_plan <- function(config, tables, manifest, output_root) {
  prefix <- manifest$config_prefix
  target <- file.path(output_root, prefix)
  if (file.exists(target)) lomm11_bridge_stop("Plan namespace already exists: ", target)
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  staging <- file.path(output_root, paste0(".", prefix, ".staging-", Sys.getpid()))
  if (file.exists(staging)) lomm11_bridge_stop("Plan staging namespace already exists.")
  dir.create(staging, recursive = FALSE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  for (name in names(tables)) {
    lomm11_bridge_write_csv(tables[[name]], file.path(staging, paste0(name, ".csv")))
  }
  lomm11_bridge_write_json_atomic(
    manifest$output_schema, file.path(staging, "output_schema.json")
  )
  lomm11_bridge_write_json_atomic(manifest, file.path(staging, "prospective_manifest.json"))
  manifest_sha <- lomm11_bridge_sha256(file.path(staging, "prospective_manifest.json"))
  receipt <- list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    status = "planned",
    config_hash = manifest$config_hash,
    config_prefix = prefix,
    prospective_manifest_sha256 = manifest_sha,
    estimator_calls = FALSE,
    simulation_executed = FALSE
  )
  lomm11_bridge_write_json_atomic(receipt, file.path(staging, "_PLANNED.json"))
  expected_hashes <- stats::setNames(
    vapply(manifest$plan_artifacts, `[[`, character(1), "sha256"),
    vapply(manifest$plan_artifacts, `[[`, character(1), "path")
  )
  observed_hashes <- vapply(file.path(staging, names(expected_hashes)),
                            lomm11_bridge_sha256, character(1))
  if (!identical(unname(observed_hashes), unname(expected_hashes))) {
    lomm11_bridge_stop("Materialized bridge plan differs from its manifest.")
  }
  if (!file.rename(staging, target)) lomm11_bridge_stop("Atomic plan publish failed.")
  list(directory = target, manifest_sha256 = manifest_sha)
}

lomm11_bridge_build_plan <- function() {
  config <- lomm11_bridge_read_config(LOMM11_BRIDGE_ROOT)
  lomm11_bridge_validate_source_run(config, LOMM11_BRIDGE_ROOT)
  pair_plan <- lomm11_bridge_pair_plan(config, LOMM11_BRIDGE_ROOT)
  dependency_hashes <- lomm11_bridge_source_dependencies(LOMM11_BRIDGE_ROOT)
  config_hash <- lomm11_bridge_config_identity(config, dependency_hashes, pair_plan)
  tables <- lomm11_bridge_plan_tables(config, pair_plan, config_hash)
  manifest <- lomm11_bridge_plan_manifest(
    config, pair_plan, tables, dependency_hashes, config_hash
  )
  list(config = config, pair_plan = pair_plan, dependencies = dependency_hashes,
       config_hash = config_hash, tables = tables, manifest = manifest)
}

lomm11_bridge_planner_main <- function() {
  args <- lomm11_bridge_parse_planner_args(commandArgs(trailingOnly = TRUE))
  before <- lomm11_bridge_source_dependencies(LOMM11_BRIDGE_ROOT)
  plan <- lomm11_bridge_build_plan()
  after <- lomm11_bridge_source_dependencies(LOMM11_BRIDGE_ROOT)
  if (!identical(before, after) || !identical(after, plan$dependencies)) {
    lomm11_bridge_stop("Recovery-bridge dependencies changed during planning.")
  }
  if (identical(args$mode, "dry-run")) {
    report <- plan$manifest
    report$prospective_manifest_sha256 <- lomm11_bridge_json_sha256(plan$manifest)
    cat(jsonlite::toJSON(report, auto_unbox = TRUE, pretty = TRUE, null = "null",
                         na = "null", digits = NA), "\n")
    return(invisible(report))
  }
  published <- lomm11_bridge_materialize_plan(
    plan$config, plan$tables, plan$manifest, args$output_root
  )
  cat("Materialized recovery-bridge plan at ", published$directory, "\n",
      "config_hash: ", plan$config_hash, "\n",
      "prospective_manifest_sha256: ", published$manifest_sha256, "\n", sep = "")
  invisible(published)
}

if (sys.nframe() == 0L) lomm11_bridge_planner_main()
