#!/usr/bin/env Rscript

lomm11_bridge_runner_path <- function() {
  configured <- Sys.getenv("LOMM11_BRIDGE_RUNNER_PATH", "")
  if (nzchar(configured) && file.exists(configured)) {
    return(normalizePath(configured, mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  candidate <- if (length(argument) == 1L) {
    normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
  } else {
    file.path(getwd(), "scripts", "lomm_1_1", "run_bridge_recovery.R")
  }
  normalizePath(candidate, mustWork = TRUE)
}

LOMM11_BRIDGE_RUNNER_PATH <- lomm11_bridge_runner_path()
LOMM11_BRIDGE_RUNNER_DIRECTORY <- dirname(LOMM11_BRIDGE_RUNNER_PATH)
LOMM11_BRIDGE_RUNNER_ROOT <- normalizePath(
  file.path(LOMM11_BRIDGE_RUNNER_DIRECTORY, "..", ".."), mustWork = TRUE
)

source(file.path(LOMM11_BRIDGE_RUNNER_DIRECTORY, "bridge_recovery_spec.R"))
source(file.path(LOMM11_BRIDGE_RUNNER_DIRECTORY, "build_bridge_recovery_plan.R"))

LOMM11_BRIDGE_CHECKPOINT_COLUMNS <- c(
  "attempt_key", "fit_key", "pair_key", "panel_key", "profile_id",
  "method_id", "attempt", "status", "error_message", "panel_hash",
  "source_panel_hash", "structure_hash", "runtime_seconds", "started_at",
  "finished_at", "worker_pid", "worker_host", "config_hash",
  "prospective_manifest_sha256", "attempt_artifact", "attempt_receipt_sha256"
)

lomm11_bridge_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

lomm11_bridge_parse_runner_args <- function(args) {
  parsed <- list(
    mode = NULL, plan_dir = NULL, execute = FALSE, resume = FALSE,
    acknowledgement = NULL, manifest_sha256 = NULL
  )
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (identical(token, "--dry-run")) {
      if (!is.null(parsed$mode)) lomm11_bridge_stop("Choose exactly one runner mode.")
      parsed$mode <- "dry-run"
      index <- index + 1L
      next
    }
    if (identical(token, "--execute")) {
      if (!is.null(parsed$mode)) lomm11_bridge_stop("Choose exactly one runner mode.")
      parsed$mode <- "execute"
      parsed$execute <- TRUE
      index <- index + 1L
      next
    }
    if (identical(token, "--resume")) {
      parsed$resume <- TRUE
      index <- index + 1L
      next
    }
    if (token %in% c(
      "--plan-dir", "--ack-long-running", "--prospective-manifest-sha256"
    )) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        lomm11_bridge_stop("Missing value after ", token, ".")
      }
      destination <- switch(
        token,
        `--plan-dir` = "plan_dir",
        `--ack-long-running` = "acknowledgement",
        `--prospective-manifest-sha256` = "manifest_sha256"
      )
      parsed[[destination]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    lomm11_bridge_stop("Unknown runner argument: ", token)
  }
  if (is.null(parsed$mode) || is.null(parsed$plan_dir)) {
    lomm11_bridge_stop("Specify --plan-dir and exactly one of --dry-run or --execute.")
  }
  if (parsed$resume && !parsed$execute) {
    lomm11_bridge_stop("--resume is valid only with --execute.")
  }
  if (!is.null(parsed$manifest_sha256) &&
      !grepl("^[0-9a-f]{64}$", parsed$manifest_sha256)) {
    lomm11_bridge_stop("The prospective-manifest SHA must contain 64 hex characters.")
  }
  parsed
}

lomm11_bridge_read_json <- function(path) {
  if (!file.exists(path)) lomm11_bridge_stop("Missing JSON artifact: ", path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

lomm11_bridge_read_csv <- function(path) {
  if (!file.exists(path)) lomm11_bridge_stop("Missing CSV artifact: ", path)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

lomm11_bridge_validate_plan_directory <- function(plan_dir, config) {
  observed <- normalizePath(plan_dir, mustWork = TRUE)
  expected_root <- normalizePath(
    file.path(LOMM11_BRIDGE_RUNNER_ROOT, config$execution$output_root),
    mustWork = TRUE
  )
  if (!lomm11_bridge_path_is_within(observed, expected_root) ||
      identical(observed, expected_root)) {
    lomm11_bridge_stop("The plan must use its isolated hashed plan namespace.")
  }
  revision_root <- lomm11_bridge_future_path(file.path(
    LOMM11_BRIDGE_RUNNER_ROOT, "simulation_outputs", "revision_v2"
  ))
  if (lomm11_bridge_path_is_within(observed, revision_root)) {
    lomm11_bridge_stop("The recovery bridge cannot read or write revision_v2.")
  }
  observed
}

lomm11_bridge_validate_plan <- function(plan_dir, supplied_manifest_sha = NULL) {
  config <- lomm11_bridge_read_config(LOMM11_BRIDGE_RUNNER_ROOT)
  lomm11_bridge_validate_source_run(config, LOMM11_BRIDGE_RUNNER_ROOT)
  directory <- lomm11_bridge_validate_plan_directory(plan_dir, config)
  rebuilt <- lomm11_bridge_build_plan()
  manifest_path <- file.path(directory, "prospective_manifest.json")
  manifest_sha <- lomm11_bridge_sha256(manifest_path)
  expected_manifest_sha <- lomm11_bridge_json_sha256(rebuilt$manifest)
  if (!identical(manifest_sha, expected_manifest_sha)) {
    lomm11_bridge_stop("The prospective manifest is stale or has changed.")
  }
  if (!is.null(supplied_manifest_sha) &&
      !identical(manifest_sha, supplied_manifest_sha)) {
    lomm11_bridge_stop("The supplied prospective-manifest SHA does not match.")
  }
  expected_directory <- normalizePath(file.path(
    LOMM11_BRIDGE_RUNNER_ROOT, config$execution$output_root,
    substr(rebuilt$config_hash, 1L, 12L)
  ), mustWork = TRUE)
  if (!identical(directory, expected_directory)) {
    lomm11_bridge_stop("The plan directory does not match the current configuration hash.")
  }
  for (name in names(rebuilt$tables)) {
    path <- file.path(directory, paste0(name, ".csv"))
    expected_hash <- lomm11_bridge_csv_sha256(rebuilt$tables[[name]])
    if (!identical(lomm11_bridge_sha256(path), expected_hash)) {
      lomm11_bridge_stop("Plan artifact differs from its prospective version: ", name)
    }
  }
  schema_path <- file.path(directory, "output_schema.json")
  if (!identical(
    lomm11_bridge_sha256(schema_path),
    lomm11_bridge_json_sha256(rebuilt$manifest$output_schema)
  )) {
    lomm11_bridge_stop("The materialized output schema is stale or changed.")
  }
  receipt <- lomm11_bridge_read_json(file.path(directory, "_PLANNED.json"))
  if (!identical(receipt$status, "planned") ||
      !identical(receipt$config_hash, rebuilt$config_hash) ||
      !identical(receipt$prospective_manifest_sha256, manifest_sha) ||
      !identical(receipt$estimator_calls, FALSE) ||
      !identical(receipt$simulation_executed, FALSE)) {
    lomm11_bridge_stop("The planned-run receipt does not satisfy the launch contract.")
  }
  list(
    config = config,
    config_hash = rebuilt$config_hash,
    config_prefix = substr(rebuilt$config_hash, 1L, 12L),
    manifest = rebuilt$manifest,
    manifest_sha256 = manifest_sha,
    directory = directory,
    tables = lapply(names(rebuilt$tables), function(name) {
      lomm11_bridge_read_csv(file.path(directory, paste0(name, ".csv")))
    }) |> stats::setNames(names(rebuilt$tables))
  )
}

lomm11_bridge_validate_authorization <- function(args, plan) {
  expected <- plan$config$execution$authorization_token
  if (!args$execute || !identical(args$acknowledgement, expected) ||
      is.null(args$manifest_sha256) ||
      !identical(args$manifest_sha256, plan$manifest_sha256)) {
    lomm11_bridge_stop(
      "Execution requires --execute, the literal acknowledgement token, and ",
      "the exact prospective-manifest SHA."
    )
  }
  invisible(TRUE)
}

lomm11_bridge_run_directory <- function(plan) {
  run_root <- lomm11_bridge_future_path(file.path(
    LOMM11_BRIDGE_RUNNER_ROOT, plan$config$execution$run_output_root
  ))
  revision_root <- lomm11_bridge_future_path(file.path(
    LOMM11_BRIDGE_RUNNER_ROOT, "simulation_outputs", "revision_v2"
  ))
  if (lomm11_bridge_path_is_within(run_root, revision_root)) {
    lomm11_bridge_stop("The recovery bridge cannot write revision_v2.")
  }
  file.path(run_root, plan$config_prefix)
}

lomm11_bridge_process_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (length(pid) != 1L || is.na(pid) || pid < 1L) return(FALSE)
  identical(system2("kill", c("-0", as.character(pid)), stdout = FALSE,
                    stderr = FALSE), 0L)
}

lomm11_bridge_acquire_lock <- function(run_dir) {
  lock_dir <- file.path(run_dir, ".runner.lock")
  if (dir.exists(lock_dir)) {
    lock_path <- file.path(lock_dir, "owner.json")
    owner <- tryCatch(lomm11_bridge_read_json(lock_path), error = function(error) NULL)
    if (is.null(owner) || is.null(owner$host) || is.null(owner$pid)) {
      lomm11_bridge_stop("An unreadable runner lock requires manual inspection.")
    }
    same_host <- identical(owner$host, Sys.info()[["nodename"]])
    if (!same_host) {
      lomm11_bridge_stop("A foreign-host runner lock cannot be reclaimed automatically.")
    }
    if (lomm11_bridge_process_alive(owner$pid)) {
      lomm11_bridge_stop("A recovery-bridge runner already owns this namespace.")
    }
    stale <- paste0(lock_dir, ".stale-", format(Sys.time(), "%Y%m%dT%H%M%S"),
                    "-", Sys.getpid())
    if (!file.rename(lock_dir, stale)) {
      lomm11_bridge_stop("Could not preserve and replace the stale runner lock.")
    }
  }
  if (!dir.create(lock_dir, recursive = FALSE, showWarnings = FALSE)) {
    lomm11_bridge_stop("Could not acquire the recovery-bridge runner lock.")
  }
  lomm11_bridge_write_json_atomic(list(
    pid = Sys.getpid(), host = Sys.info()[["nodename"]], acquired_at = lomm11_bridge_timestamp()
  ), file.path(lock_dir, "owner.json"))
  lock_dir
}

lomm11_bridge_write_csv_atomic <- function(frame, path, replace = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) && !replace) {
    lomm11_bridge_stop("Refusing to overwrite existing scientific artifact: ", path)
  }
  temporary <- tempfile(paste0(".", basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) lomm11_bridge_stop("Atomic CSV publish failed: ", path)
  invisible(path)
}

lomm11_bridge_frame_csv_sha256 <- function(frame) {
  path <- tempfile("lomm11-bridge-frame-", fileext = ".csv")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  lomm11_bridge_sha256(path)
}

lomm11_bridge_publish_csv_idempotent <- function(frame, path) {
  expected <- lomm11_bridge_frame_csv_sha256(frame)
  if (file.exists(path)) {
    if (!identical(lomm11_bridge_sha256(path), expected)) {
      lomm11_bridge_stop("Existing final artifact differs from canonical recovery: ", path)
    }
    return(invisible(path))
  }
  lomm11_bridge_write_csv_atomic(frame, path)
  if (!identical(lomm11_bridge_sha256(path), expected)) {
    lomm11_bridge_stop("Published final artifact failed its byte check: ", path)
  }
  invisible(path)
}

lomm11_bridge_publish_json_idempotent <- function(value, path) {
  expected <- lomm11_bridge_json_sha256(value)
  if (file.exists(path)) {
    if (!identical(lomm11_bridge_sha256(path), expected)) {
      lomm11_bridge_stop("Existing finalization receipt differs: ", path)
    }
    return(invisible(path))
  }
  lomm11_bridge_write_json_atomic(value, path)
  if (!identical(lomm11_bridge_sha256(path), expected)) {
    lomm11_bridge_stop("Published finalization receipt failed its byte check.")
  }
  invisible(path)
}

lomm11_bridge_checkpoint_path <- function(run_dir) {
  file.path(run_dir, "checkpoint_results.csv")
}

lomm11_bridge_empty_checkpoint <- function() {
  values <- stats::setNames(replicate(
    length(LOMM11_BRIDGE_CHECKPOINT_COLUMNS), character(), simplify = FALSE
  ), LOMM11_BRIDGE_CHECKPOINT_COLUMNS)
  values$attempt <- integer()
  values$runtime_seconds <- numeric()
  values$worker_pid <- integer()
  as.data.frame(values, stringsAsFactors = FALSE)
}

lomm11_bridge_read_checkpoint <- function(path) {
  if (!file.exists(path)) return(lomm11_bridge_empty_checkpoint())
  rows <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!identical(names(rows), LOMM11_BRIDGE_CHECKPOINT_COLUMNS)) {
    lomm11_bridge_stop("The bridge checkpoint schema changed.")
  }
  rows$attempt <- as.integer(rows$attempt)
  rows$runtime_seconds <- as.numeric(rows$runtime_seconds)
  rows$worker_pid <- as.integer(rows$worker_pid)
  rows
}

lomm11_bridge_append_checkpoint <- function(rows, path) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) return(invisible(path))
  rows <- rows[, LOMM11_BRIDGE_CHECKPOINT_COLUMNS, drop = FALSE]
  existing <- lomm11_bridge_read_checkpoint(path)
  combined <- rbind(existing, rows)
  if (anyDuplicated(combined$attempt_key) ||
      anyDuplicated(paste(combined$fit_key, combined$attempt, sep = "|"))) {
    lomm11_bridge_stop("Attempt checkpoint keys must be append-only and unique.")
  }
  if (nrow(existing) > 0L &&
      !isTRUE(all.equal(existing, combined[seq_len(nrow(existing)), , drop = FALSE],
                       check.attributes = FALSE))) {
    lomm11_bridge_stop("Checkpoint append would alter prior attempts.")
  }
  lomm11_bridge_write_csv_atomic(combined, path, replace = file.exists(path))
}

lomm11_bridge_attempt_artifact <- function(run_dir, attempt) {
  directory <- file.path(run_dir, "attempts")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".attempt-", tmpdir = directory, fileext = ".rds")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(attempt, temporary, version = 3)
  hash <- lomm11_bridge_sha256(temporary)
  target <- file.path(directory, paste0(
    substr(lomm11_bridge_hash_text(attempt$checkpoint$attempt_key), 1L, 16L),
    "-", substr(hash, 1L, 16L), ".rds"
  ))
  if (file.exists(target)) {
    if (!identical(lomm11_bridge_sha256(target), hash)) {
      lomm11_bridge_stop("Attempt artifact hash collision.")
    }
  } else if (!file.rename(temporary, target)) {
    lomm11_bridge_stop("Atomic attempt-artifact publish failed.")
  }
  list(path = file.path("attempts", basename(target)), sha256 = hash)
}

lomm11_bridge_validate_attempt_artifact <- function(row, run_dir, plan) {
  relative <- row$attempt_artifact[[1L]]
  if (is.na(relative) || !grepl("^attempts/[A-Za-z0-9-]+[.]rds$", relative)) {
    return(FALSE)
  }
  artifact <- file.path(run_dir, row$attempt_artifact[[1L]])
  attempts_root <- normalizePath(file.path(run_dir, "attempts"), mustWork = TRUE)
  observed_path <- normalizePath(artifact, mustWork = FALSE)
  if (!lomm11_bridge_path_is_within(observed_path, attempts_root)) return(FALSE)
  if (!file.exists(artifact) ||
      !identical(lomm11_bridge_sha256(artifact), row$attempt_receipt_sha256[[1L]])) {
    return(FALSE)
  }
  payload <- tryCatch(readRDS(artifact), error = function(error) NULL)
  if (!is.list(payload) || !is.data.frame(payload$checkpoint) ||
      !is.data.frame(payload$readouts) || nrow(payload$checkpoint) != 1L ||
      nrow(payload$readouts) != length(LOMM11_BRIDGE_ALGORITHMS) ||
      any(!c(
        "readout_key", "fit_key", "status", "structure_fit_status",
        "structure_hash", "community_algorithm", "config_hash"
      ) %in% names(payload$readouts))) {
    return(FALSE)
  }
  identity_columns <- setdiff(
    LOMM11_BRIDGE_CHECKPOINT_COLUMNS,
    c("attempt_artifact", "attempt_receipt_sha256")
  )
  expected <- row[, identity_columns, drop = FALSE]
  observed <- payload$checkpoint[, identity_columns, drop = FALSE]
  identity_ok <- all(vapply(identity_columns, function(column) {
    expected_value <- ifelse(
      is.na(expected[[column]]), "<NA>", as.character(expected[[column]])
    )
    observed_value <- ifelse(
      is.na(observed[[column]]), "<NA>", as.character(observed[[column]])
    )
    identical(expected_value, observed_value)
  }, logical(1)))
  algorithms_ok <- identical(
    sort(as.character(payload$readouts$community_algorithm)),
    sort(LOMM11_BRIDGE_ALGORITHMS)
  )
  keys_ok <- all(payload$readouts$fit_key == row$fit_key[[1L]]) &&
    all(payload$readouts$config_hash == plan$config_hash) &&
    identical(
      sort(as.character(payload$readouts$readout_key)),
      sort(plan$tables$readout_plan$readout_key[
        plan$tables$readout_plan$fit_key == row$fit_key[[1L]]
      ])
    )
  status_ok <- if (identical(row$status[[1L]], "ok")) {
    all(payload$readouts$structure_fit_status == "ok") &&
      all(payload$readouts$status %in% c("ok", "error")) &&
      grepl("^[0-9a-f]{64}$", row$structure_hash[[1L]]) &&
      all(payload$readouts$structure_hash == row$structure_hash[[1L]])
  } else {
    all(payload$readouts$structure_fit_status == row$status[[1L]]) &&
      all(payload$readouts$status == row$status[[1L]]) &&
      is.na(row$structure_hash[[1L]]) &&
      all(is.na(payload$readouts$structure_hash))
  }
  identity_ok && algorithms_ok && keys_ok && status_ok
}

lomm11_bridge_validate_checkpoint <- function(rows, run_dir, plan) {
  if (nrow(rows) == 0L) return(invisible(TRUE))
  fits <- plan$tables$fit_plan
  fit_index <- match(rows$fit_key, fits$fit_key)
  mapped <- !is.na(fit_index)
  identity_ok <- mapped &
    rows$pair_key == fits$pair_key[fit_index] &
    rows$panel_key == fits$panel_key[fit_index] &
    rows$profile_id == fits$profile_id[fit_index] &
    rows$method_id == fits$method_id[fit_index]
  if (any(!identity_ok) ||
      any(!rows$status %in% c("ok", "error", "timeout")) ||
      any(rows$attempt < 1L) ||
      any(rows$attempt > plan$config$execution$max_attempts_per_fit) ||
      any(rows$config_hash != plan$config_hash) ||
      any(rows$prospective_manifest_sha256 != plan$manifest_sha256) ||
      anyDuplicated(rows$attempt_key) ||
      anyDuplicated(paste(rows$fit_key, rows$attempt, sep = "|"))) {
    lomm11_bridge_stop("The bridge checkpoint violates its key or provenance contract.")
  }
  groups <- split(rows$attempt, rows$fit_key)
  contiguous <- vapply(groups, function(value) {
    identical(sort(as.integer(value)), seq_len(max(as.integer(value))))
  }, logical(1))
  if (any(!contiguous)) lomm11_bridge_stop("Fit attempts must be contiguous from one.")
  status_groups <- split(seq_len(nrow(rows)), rows$fit_key)
  retry_after_ok <- vapply(status_groups, function(index) {
    ordered <- index[order(rows$attempt[index])]
    ok <- which(rows$status[ordered] == "ok")
    length(ok) > 0L && ok[[1L]] < length(ordered)
  }, logical(1))
  if (any(retry_after_ok)) lomm11_bridge_stop("A successful fit cannot be retried.")
  valid <- vapply(seq_len(nrow(rows)), function(index) {
    lomm11_bridge_validate_attempt_artifact(rows[index, , drop = FALSE], run_dir, plan)
  }, logical(1))
  if (any(!valid)) lomm11_bridge_stop("A checkpoint attempt artifact is missing or invalid.")
  invisible(TRUE)
}

lomm11_bridge_latest_attempts <- function(rows) {
  if (nrow(rows) == 0L) return(rows)
  index <- vapply(split(seq_len(nrow(rows)), rows$fit_key), function(group) {
    group[[which.max(rows$attempt[group])]]
  }, integer(1))
  output <- rows[unname(index), , drop = FALSE]
  output[order(output$fit_key), , drop = FALSE]
}

lomm11_bridge_pending_fits <- function(fit_plan, checkpoint, max_attempts) {
  latest <- lomm11_bridge_latest_attempts(checkpoint)
  match_index <- match(fit_plan$fit_key, latest$fit_key)
  missing <- is.na(match_index)
  status <- rep(NA_character_, nrow(fit_plan))
  attempt <- integer(nrow(fit_plan))
  status[!missing] <- latest$status[match_index[!missing]]
  attempt[!missing] <- latest$attempt[match_index[!missing]]
  eligible <- missing | (status != "ok" & attempt < max_attempts)
  output <- fit_plan[eligible, , drop = FALSE]
  output$next_attempt <- attempt[eligible] + 1L
  output
}

lomm11_bridge_source_estimation_functions <- function() {
  previous_directory <- getwd()
  on.exit(setwd(previous_directory), add = TRUE)
  setwd(LOMM11_BRIDGE_RUNNER_ROOT)
  if (!exists("revision_source_estimation_core", mode = "function")) {
    source(file.path(
      LOMM11_BRIDGE_RUNNER_ROOT,
      "scripts", "simulations", "run_revision_job.R"
    ))
  }
  revision_source_estimation_core()
  invisible(TRUE)
}

lomm11_bridge_fit_config <- function(config, fit_seed, diagnostic_dir) {
  ambient <- names(Sys.getenv())
  benchmark <- grep("^BENCHMARK_", ambient, value = TRUE)
  previous <- if (length(benchmark) == 0L) {
    character()
  } else {
    Sys.getenv(benchmark, unset = NA_character_, names = TRUE)
  }
  on.exit({
    Sys.unsetenv(benchmark)
    restore <- previous[!is.na(previous)]
    if (length(restore) > 0L) do.call(Sys.setenv, as.list(restore))
  }, add = TRUE)
  if (length(benchmark) > 0L) Sys.unsetenv(benchmark)
  value <- build_benchmark_config(smoke = FALSE)
  value$integrator <- "linear_exact"
  value$parallel_workers <- 1L
  value$dynEGA_ncores <- 1L
  value$graphical_var_workers <- 1L
  value$gimme_workers <- 1L
  value$graphical_var_timeout_sec <- as.numeric(
    config$execution$graphicalvar_timeout_seconds
  )
  value$gimme_timeout_sec <- as.numeric(config$execution$gimme_timeout_seconds)
  value$lomm11_bridge_fit_seed <- as.integer(fit_seed)
  value$out_dir <- diagnostic_dir
  value
}

lomm11_bridge_fit_seed <- function(pair_key, method_id) {
  revision_seed_from_key(paste(pair_key, method_id, "paired_fit", sep = "|"))
}

lomm11_bridge_readout_seed <- function(pair_key, method_id, algorithm) {
  revision_seed_from_key(paste(pair_key, method_id, algorithm, "paired_readout", sep = "|"))
}

lomm11_bridge_evaluate_fit <- function(scen_row, sim_data, method_id, fit_result) {
  if (!identical(fit_result$status, "ok")) {
    return(list(status = fit_result$status, structure_hash = NA_character_,
                readouts = revision_error_readouts(scen_row, method_id, fit_result)))
  }
  structure_hash <- tryCatch(
    revision_object_sha256(as.matrix(fit_result$value$structure)),
    error = function(error) error
  )
  if (inherits(structure_hash, "error")) {
    failure <- list(
      status = "error",
      error_message = paste("frozen structure hash failed:", conditionMessage(structure_hash)),
      runtime_sec = fit_result$runtime_sec
    )
    return(list(status = "error", structure_hash = NA_character_,
                readouts = revision_error_readouts(scen_row, method_id, failure)))
  }
  pair_key <- scen_row$pair_key[[1L]]
  set.seed(lomm11_bridge_readout_seed(pair_key, method_id, "native"))
  native <- revision_native_sensitivity(fit_result$value, sim_data, method_id)
  set.seed(lomm11_bridge_readout_seed(pair_key, method_id, "correlation_louvain_095"))
  correlation <- revision_correlation_sensitivity(fit_result$value, sim_data, method_id)
  readouts <- lapply(LOMM11_BRIDGE_ALGORITHMS, function(algorithm) {
    set.seed(lomm11_bridge_readout_seed(pair_key, method_id, algorithm))
    revision_evaluate_structure_readout(
      scen_row, sim_data, method_id, algorithm, fit_result, structure_hash,
      native, correlation
    )
  })
  list(status = "ok", structure_hash = structure_hash,
       readouts = do.call(rbind, readouts))
}

lomm11_bridge_enrich_readouts <- function(rows, fit_row, attempt, plan,
                                          sim_data, fit_seed) {
  rows$fit_key <- fit_row$fit_key[[1L]]
  rows$readout_key <- paste(
    fit_row$fit_key[[1L]], rows$community_algorithm, sep = "|"
  )
  rows$pair_key <- fit_row$pair_key[[1L]]
  rows$profile_id <- fit_row$profile_id[[1L]]
  rows$attempt <- as.integer(attempt)
  rows$fit_seed <- as.integer(fit_seed)
  rows$config_hash <- plan$config_hash
  rows$prospective_manifest_sha256 <- plan$manifest_sha256
  rows$source_panel_hash <- sim_data$bridge_truth$source_panel_hash
  rows$panel_hash <- sim_data$bridge_truth$transformed_panel_hash
  rows$oscillator_latent_hash <- sim_data$bridge_truth$source_latent_hash
  rows$evaluation_latent_hash <- sim_data$bridge_truth$transformed_latent_hash
  rows
}

lomm11_bridge_attempt_checkpoint <- function(fit_row, attempt, evaluated,
                                             readouts, plan, sim_data,
                                             started_at, finished_at) {
  error_values <- unique(readouts$error_message[
    !is.na(readouts$error_message) & nzchar(readouts$error_message)
  ])
  structure_values <- unique(readouts$structure_hash[
    !is.na(readouts$structure_hash) & nzchar(readouts$structure_hash)
  ])
  runtime_values <- readouts$structure_fit_runtime_sec[
    is.finite(readouts$structure_fit_runtime_sec)
  ]
  data.frame(
    attempt_key = paste0(fit_row$fit_key[[1L]], "|attempt=", sprintf("%02d", attempt)),
    fit_key = fit_row$fit_key[[1L]],
    pair_key = fit_row$pair_key[[1L]],
    panel_key = fit_row$panel_key[[1L]],
    profile_id = fit_row$profile_id[[1L]],
    method_id = fit_row$method_id[[1L]],
    attempt = as.integer(attempt),
    status = evaluated$status,
    error_message = if (length(error_values) == 0L) NA_character_ else error_values[[1L]],
    panel_hash = sim_data$bridge_truth$transformed_panel_hash,
    source_panel_hash = sim_data$bridge_truth$source_panel_hash,
    structure_hash = if (length(structure_values) == 1L) structure_values else NA_character_,
    runtime_seconds = if (length(runtime_values) == 0L) NA_real_ else
      max(runtime_values),
    started_at = started_at,
    finished_at = finished_at,
    worker_pid = Sys.getpid(),
    worker_host = Sys.info()[["nodename"]],
    config_hash = plan$config_hash,
    prospective_manifest_sha256 = plan$manifest_sha256,
    attempt_artifact = NA_character_,
    attempt_receipt_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
}

lomm11_bridge_prepare_profiles <- function(pair_row, plan) {
  source_panel <- revision_simulate_challenge_panel(pair_row)
  source_panel$oscillator_latents <- source_panel$latents
  profiles <- lapply(LOMM11_BRIDGE_PROFILES, function(profile) {
    transformed <- lomm11_bridge_apply_profile(
      source_panel, pair_row, profile, plan$config
    )
    transformed$oscillator_latents <- source_panel$oscillator_latents
    transformed$evaluation_latents <- transformed$latents
    transformed
  })
  names(profiles) <- LOMM11_BRIDGE_PROFILES
  lomm11_bridge_validate_profile_pair(
    profiles$homogeneous, profiles$between_clip_scale, pair_row, plan$config
  )
  profiles
}

lomm11_bridge_scenario_row <- function(pair_row, fit_row, plan, sim_data) {
  row <- lomm11_bridge_profile_row(
    pair_row, fit_row$profile_id[[1L]], plan$config_hash
  )
  row$generator_hash <- plan$config_hash
  row$input_spec_hash <- plan$config_hash
  row$panel_input_hash <- sim_data$bridge_truth$transformed_panel_hash
  row$component_hash_latent_standard_normals <-
    sim_data$component_hashes$latent_standard_normals
  row$component_hash_residual_standard_normals <-
    sim_data$component_hashes$residual_standard_normals
  row$component_hash_loading_basis <- sim_data$component_hashes$loading_basis
  row$component_hash_activity_schedule <- sim_data$component_hashes$activity_schedule
  row$component_hash_linear_z <- sim_data$component_hashes$linear_z
  row$grid_scope <- "lomm_1_1_recovery_bridge"
  row
}

lomm11_bridge_run_one_attempt <- function(pair_row, fit_row, attempt, plan,
                                          sim_data, run_dir) {
  started_at <- lomm11_bridge_timestamp()
  method_id <- fit_row$method_id[[1L]]
  fit_seed <- lomm11_bridge_fit_seed(pair_row$pair_key[[1L]], method_id)
  diagnostic_dir <- file.path(
    run_dir, "method_failure_diagnostics",
    substr(lomm11_bridge_hash_text(paste(fit_row$fit_key[[1L]], attempt)), 1L, 16L)
  )
  dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)
  cfg <- lomm11_bridge_fit_config(plan$config, fit_seed, diagnostic_dir)
  scen_row <- lomm11_bridge_scenario_row(pair_row, fit_row, plan, sim_data)
  set.seed(fit_seed)
  fit_result <- revision_fit_frozen_structure(method_id, sim_data, scen_row, cfg)
  evaluated <- lomm11_bridge_evaluate_fit(scen_row, sim_data, method_id, fit_result)
  readouts <- lomm11_bridge_enrich_readouts(
    evaluated$readouts, fit_row, attempt, plan, sim_data, fit_seed
  )
  finished_at <- lomm11_bridge_timestamp()
  checkpoint <- lomm11_bridge_attempt_checkpoint(
    fit_row, attempt, evaluated, readouts, plan, sim_data, started_at, finished_at
  )
  list(checkpoint = checkpoint, readouts = readouts)
}

lomm11_bridge_generation_failure <- function(pair_row, fit_row, attempt, plan,
                                              message) {
  profile <- fit_row$profile_id[[1L]]
  scen_row <- lomm11_bridge_profile_row(pair_row, profile, plan$config_hash)
  failure <- list(status = "error", error_message = message, runtime_sec = 0)
  rows <- revision_error_readouts(scen_row, fit_row$method_id[[1L]], failure)
  truth <- list(
    source_panel_hash = NA_character_, transformed_panel_hash = NA_character_,
    source_latent_hash = NA_character_, transformed_latent_hash = NA_character_
  )
  sim_data <- list(bridge_truth = truth)
  rows <- lomm11_bridge_enrich_readouts(
    rows, fit_row, attempt, plan, sim_data,
    lomm11_bridge_fit_seed(pair_row$pair_key[[1L]], fit_row$method_id[[1L]])
  )
  checkpoint <- lomm11_bridge_attempt_checkpoint(
    fit_row, attempt,
    list(status = "error", structure_hash = NA_character_),
    rows, plan, sim_data, lomm11_bridge_timestamp(), lomm11_bridge_timestamp()
  )
  list(checkpoint = checkpoint, readouts = rows)
}

lomm11_bridge_run_pair <- function(pair_row, pending, plan, run_dir) {
  profiles <- tryCatch(
    lomm11_bridge_prepare_profiles(pair_row, plan),
    error = function(error) error
  )
  attempts <- list()
  cursor <- 1L
  maximum <- as.integer(plan$config$execution$max_attempts_per_fit)
  for (index in seq_len(nrow(pending))) {
    fit_row <- pending[index, , drop = FALSE]
    attempt <- as.integer(fit_row$next_attempt[[1L]])
    repeat {
      result <- if (inherits(profiles, "error")) {
        lomm11_bridge_generation_failure(
          pair_row, fit_row, attempt, plan, conditionMessage(profiles)
        )
      } else {
        profile_data <- profiles[[fit_row$profile_id[[1L]]]]
        tryCatch(
          lomm11_bridge_run_one_attempt(
            pair_row, fit_row, attempt, plan, profile_data, run_dir
          ),
          error = function(error) lomm11_bridge_generation_failure(
            pair_row, fit_row, attempt, plan,
            paste("bridge attempt failed:", conditionMessage(error))
          )
        )
      }
      attempts[[cursor]] <- result
      cursor <- cursor + 1L
      if (identical(result$checkpoint$status[[1L]], "ok") || attempt >= maximum) break
      attempt <- attempt + 1L
    }
  }
  attempts
}

lomm11_bridge_publish_attempts <- function(attempts, run_dir, checkpoint_path) {
  if (length(attempts) == 0L) return(invisible(0L))
  rows <- vector("list", length(attempts))
  for (index in seq_along(attempts)) {
    published <- lomm11_bridge_attempt_artifact(run_dir, attempts[[index]])
    row <- attempts[[index]]$checkpoint
    row$attempt_artifact <- published$path
    row$attempt_receipt_sha256 <- published$sha256
    rows[[index]] <- row
  }
  checkpoint_rows <- do.call(rbind, rows)
  checkpoint_rows <- checkpoint_rows[order(
    checkpoint_rows$pair_key, checkpoint_rows$fit_key, checkpoint_rows$attempt
  ), , drop = FALSE]
  lomm11_bridge_append_checkpoint(checkpoint_rows, checkpoint_path)
  invisible(nrow(checkpoint_rows))
}

lomm11_bridge_validate_worker_results <- function(results, batch, pending) {
  if (!is.list(results) || length(results) != length(batch) ||
      any(vapply(results, inherits, logical(1), "try-error")) ||
      any(!vapply(results, is.list, logical(1)))) {
    lomm11_bridge_stop("A parallel bridge worker did not return a valid pair result.")
  }
  for (index in seq_along(results)) {
    expected_fits <- pending$fit_key[pending$pair_key == batch[[index]]]
    attempts <- results[[index]]
    if (length(attempts) == 0L || any(!vapply(attempts, function(value) {
      is.list(value) && is.data.frame(value$checkpoint) &&
        is.data.frame(value$readouts) && nrow(value$checkpoint) == 1L &&
        nrow(value$readouts) == length(LOMM11_BRIDGE_ALGORITHMS)
    }, logical(1)))) {
      lomm11_bridge_stop("A bridge worker returned a malformed attempt payload.")
    }
    observed_fits <- unique(vapply(
      attempts, function(value) value$checkpoint$fit_key[[1L]], character(1)
    ))
    if (!identical(sort(observed_fits), sort(expected_fits))) {
      lomm11_bridge_stop("A bridge worker returned unexpected fit keys.")
    }
  }
  invisible(TRUE)
}

lomm11_bridge_attempt_readouts <- function(checkpoint, run_dir) {
  rows <- lapply(seq_len(nrow(checkpoint)), function(index) {
    readRDS(file.path(run_dir, checkpoint$attempt_artifact[[index]]))$readouts
  })
  do.call(rbind, rows)
}

lomm11_bridge_terminal_results <- function(checkpoint, run_dir, plan) {
  latest <- lomm11_bridge_latest_attempts(checkpoint)
  expected <- sort(plan$tables$fit_plan$fit_key)
  if (!identical(sort(latest$fit_key), expected)) {
    lomm11_bridge_stop("Terminal fit keys do not match the prospective fit plan.")
  }
  maximum <- as.integer(plan$config$execution$max_attempts_per_fit)
  terminal <- latest$status == "ok" |
    (latest$status %in% c("error", "timeout") & latest$attempt == maximum)
  if (any(!terminal)) lomm11_bridge_stop("Some bridge fits are not terminal.")
  recovery <- lomm11_bridge_attempt_readouts(latest, run_dir)
  recovery <- recovery[order(recovery$readout_key), , drop = FALSE]
  if (nrow(recovery) != plan$config$expected_counts$result_rows ||
      anyDuplicated(recovery$readout_key) ||
      !identical(sort(recovery$readout_key),
                 sort(plan$tables$readout_plan$readout_key))) {
    lomm11_bridge_stop("Canonical recovery readouts violate their exact contract.")
  }
  list(fits = latest, recovery = recovery)
}

lomm11_bridge_outcome_values <- function(rows, config) {
  usable <- rows$status == "ok" & is.finite(rows$n_dim_hat)
  runtime <- suppressWarnings(as.numeric(rows$runtime_sec))
  cap <- as.numeric(config$analysis$runtime_cap_seconds)
  runtime[!is.finite(runtime)] <- cap
  runtime <- pmin(runtime, cap)
  list(
    usable_and_correct_dimension = as.numeric(usable & rows$n_dim_exact == 1),
    usable = as.numeric(usable),
    dimension_exact_conditional_on_usable = ifelse(usable, rows$n_dim_exact, NA_real_),
    assignment_accuracy = ifelse(usable, rows$assignment_accuracy, NA_real_),
    adjusted_rand_index = ifelse(usable, rows$adjusted_rand_index, NA_real_),
    variation_information = ifelse(usable, rows$variation_information, NA_real_),
    restricted_runtime_seconds = runtime
  )
}

lomm11_bridge_outcome_frame <- function(recovery, config) {
  values <- lomm11_bridge_outcome_values(recovery, config)
  base <- recovery[, c(
    "pair_key", "profile_id", "method_id", "community_algorithm",
    "scenario_family", "n_dim_true"
  ), drop = FALSE]
  rows <- lapply(names(values), function(outcome) {
    output <- base
    output$outcome <- outcome
    output$value <- values[[outcome]]
    output
  })
  do.call(rbind, rows)
}

lomm11_bridge_mean_se <- function(values) {
  finite <- values[is.finite(values)]
  estimate <- if (length(finite) == 0L) NA_real_ else mean(finite)
  se <- if (length(finite) < 2L) NA_real_ else stats::sd(finite) / sqrt(length(finite))
  c(estimate = estimate, monte_carlo_se = se, n_finite = length(finite))
}

lomm11_bridge_build_summary <- function(recovery, config) {
  long <- lomm11_bridge_outcome_frame(recovery, config)
  groups <- split(seq_len(nrow(long)), interaction(
    long$profile_id, long$method_id, long$community_algorithm, long$outcome,
    drop = TRUE, lex.order = TRUE
  ))
  rows <- lapply(groups, function(index) {
    group <- long[index, , drop = FALSE]
    estimate <- lomm11_bridge_mean_se(group$value)
    data.frame(
      profile_id = group$profile_id[[1L]],
      method_id = group$method_id[[1L]],
      community_algorithm = group$community_algorithm[[1L]],
      outcome = group$outcome[[1L]],
      estimate = estimate[["estimate"]],
      monte_carlo_se = estimate[["monte_carlo_se"]],
      n_total = nrow(group),
      n_finite = as.integer(estimate[["n_finite"]]),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  if (nrow(output) != 210L) lomm11_bridge_stop("Bridge summary must have 210 rows.")
  output
}

lomm11_bridge_stratified_bootstrap <- function(differences, strata,
                                                replicates, seed) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  groups <- split(seq_along(differences), strata)
  vapply(seq_len(replicates), function(index) {
    sampled <- unlist(lapply(groups, function(indices) {
      sample(indices, length(indices), replace = TRUE)
    }), use.names = FALSE)
    mean(differences[sampled])
  }, numeric(1))
}

lomm11_bridge_build_paired_effects <- function(recovery, config,
                                               bootstrap_replicates = NULL) {
  long <- lomm11_bridge_outcome_frame(recovery, config)
  replicates <- if (is.null(bootstrap_replicates)) {
    as.integer(config$analysis$bootstrap_replicates)
  } else {
    as.integer(bootstrap_replicates)
  }
  group_key <- interaction(
    long$method_id, long$community_algorithm, long$outcome,
    drop = TRUE, lex.order = TRUE
  )
  rows <- lapply(split(seq_len(nrow(long)), group_key), function(index) {
    group <- long[index, , drop = FALSE]
    core <- group[group$profile_id == "homogeneous", , drop = FALSE]
    bs <- group[group$profile_id == "between_clip_scale", , drop = FALSE]
    paired <- merge(
      core, bs,
      by = c("pair_key", "method_id", "community_algorithm", "outcome",
             "scenario_family", "n_dim_true"),
      suffixes = c("_core", "_bs"), sort = TRUE
    )
    finite <- is.finite(paired$value_core) & is.finite(paired$value_bs)
    paired <- paired[finite, , drop = FALSE]
    differences <- paired$value_bs - paired$value_core
    strata <- paste(paired$scenario_family, paired$n_dim_true, sep = "|")
    bootstrap_seed <- revision_seed_from_key(paste(
      "bridge_bootstrap", group$method_id[[1L]],
      group$community_algorithm[[1L]], group$outcome[[1L]], sep = "|"
    ))
    bootstrap <- if (length(differences) == 0L) {
      rep(NA_real_, replicates)
    } else {
      lomm11_bridge_stratified_bootstrap(
        differences, strata, replicates, bootstrap_seed
      )
    }
    finite_bootstrap <- bootstrap[is.finite(bootstrap)]
    estimate <- if (length(differences) == 0L) NA_real_ else mean(differences)
    data.frame(
      method_id = group$method_id[[1L]],
      community_algorithm = group$community_algorithm[[1L]],
      outcome = group$outcome[[1L]],
      contrast = "between_clip_scale_minus_homogeneous",
      estimate = estimate,
      panel_monte_carlo_se = if (length(differences) < 2L) NA_real_ else
        stats::sd(differences) / sqrt(length(differences)),
      bootstrap_se = if (length(finite_bootstrap) < 2L) NA_real_ else
        stats::sd(finite_bootstrap),
      conf_low = if (length(finite_bootstrap) == 0L) NA_real_ else
        unname(stats::quantile(
          finite_bootstrap, 0.025, names = FALSE, type = 8
        )),
      conf_high = if (length(finite_bootstrap) == 0L) NA_real_ else
        unname(stats::quantile(
          finite_bootstrap, 0.975, names = FALSE, type = 8
        )),
      n_paired = length(differences),
      bootstrap_replicates = as.integer(replicates),
      bootstrap_seed = as.integer(bootstrap_seed),
      bootstrap_seed_rule = paste0(
        "revision_seed_from_key(bridge_bootstrap|method|algorithm|outcome); ",
        "base131_plus_REVISION_SEED_ORIGIN"
      ),
      bootstrap_strata = "scenario_family_by_true_dimension",
      bootstrap_quantile_type = 8L,
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  if (nrow(output) != 105L) {
    lomm11_bridge_stop("Paired recovery effects must have 105 rows.")
  }
  output
}

lomm11_bridge_write_run_manifest <- function(path, plan, status, checkpoint,
                                              invocation_started, details = list()) {
  latest <- lomm11_bridge_latest_attempts(checkpoint)
  previous <- if (file.exists(path)) {
    tryCatch(lomm11_bridge_read_json(path), error = function(error) NULL)
  } else {
    NULL
  }
  history <- if (is.list(previous) && is.list(previous$invocation_history)) {
    previous$invocation_history
  } else {
    list()
  }
  prior_index <- which(vapply(history, function(value) {
    identical(value$invocation_started_at, invocation_started)
  }, logical(1)))
  resume_value <- if ("resume" %in% names(details)) {
    isTRUE(details$resume)
  } else if (length(prior_index) > 0L) {
    isTRUE(history[[prior_index[[1L]]]]$resume)
  } else {
    FALSE
  }
  event_details <- details[setdiff(names(details), "resume")]
  event <- c(list(
    invocation_started_at = invocation_started,
    status = status,
    updated_at = lomm11_bridge_timestamp(),
    resume = resume_value
  ), event_details)
  if (length(prior_index) == 0L) {
    history[[length(history) + 1L]] <- event
  } else {
    history[[prior_index[[1L]]]] <- event
  }
  value <- c(list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    pipeline_id = plan$config$pipeline_id,
    status = status,
    config_hash = plan$config_hash,
    prospective_manifest_sha256 = plan$manifest_sha256,
    invocation_started_at = invocation_started,
    updated_at = lomm11_bridge_timestamp(),
    workers = min(4L, as.integer(plan$config$execution$workers)),
    attempts_recorded = nrow(checkpoint),
    unique_fits_recorded = length(unique(checkpoint$fit_key)),
    latest_status_counts = as.list(table(latest$status, useNA = "ifany")),
    max_attempts_per_fit = as.integer(plan$config$execution$max_attempts_per_fit),
    command = paste(
      "Rscript scripts/lomm_1_1/run_bridge_recovery.R",
      paste(commandArgs(trailingOnly = TRUE), collapse = " ")
    ),
    plan_directory = plan$directory,
    source_realism_run = unclass(plan$config$source_realism_run),
    canonical_profile = unclass(plan$config$canonical_profile),
    dependency_hashes = plan$manifest$dependencies,
    randomness_contract = unclass(plan$config$randomness),
    analysis_contract = unclass(plan$config$analysis),
    claim_scope = unclass(plan$config$claim_scope),
    invocation_history = history,
    software_versions = list(
      R = paste(R.version$major, R.version$minor, sep = "."),
      platform = R.version$platform,
      packages = stats::setNames(lapply(
        c("jsonlite", "EGAnet", "igraph", "clue", "graphicalVAR", "gimme"),
        function(package) {
          if (requireNamespace(package, quietly = TRUE)) {
            as.character(utils::packageVersion(package))
          } else {
            "not_installed"
          }
        }
      ), c("jsonlite", "EGAnet", "igraph", "clue", "graphicalVAR", "gimme"))
    )
  ), details)
  lomm11_bridge_write_json_atomic(value, path)
  invisible(value)
}

lomm11_bridge_publish_completion <- function(run_dir, plan, terminal,
                                              invocation_started,
                                              resume = FALSE) {
  summary <- lomm11_bridge_build_summary(terminal$recovery, plan$config)
  effects <- lomm11_bridge_build_paired_effects(terminal$recovery, plan$config)
  paths <- c(
    fit_results = "fit_results.csv",
    recovery_results = "recovery_results.csv",
    bridge_summary = "bridge_summary.csv",
    paired_bootstrap_effects = "paired_bootstrap_effects.csv"
  )
  frames <- list(terminal$fits, terminal$recovery, summary, effects)
  names(frames) <- names(paths)
  finalization_path <- file.path(run_dir, "_FINALIZATION.json")
  finalization <- list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    status = "prepared",
    config_hash = plan$config_hash,
    prospective_manifest_sha256 = plan$manifest_sha256,
    artifacts = lapply(names(frames), function(name) {
      list(path = paths[[name]], sha256 = lomm11_bridge_frame_csv_sha256(frames[[name]]))
    })
  )
  names(finalization$artifacts) <- names(frames)
  lomm11_bridge_publish_json_idempotent(finalization, finalization_path)
  for (index in seq_along(paths)) {
    lomm11_bridge_publish_csv_idempotent(
      frames[[index]], file.path(run_dir, paths[[index]])
    )
  }
  manifest_path <- file.path(run_dir, "run_manifest.json")
  checkpoint <- lomm11_bridge_read_checkpoint(lomm11_bridge_checkpoint_path(run_dir))
  lomm11_bridge_write_run_manifest(
    manifest_path, plan, "complete", checkpoint, invocation_started,
    list(
      terminal_fits = nrow(terminal$fits),
      recovery_rows = nrow(terminal$recovery),
      fit_status_counts = as.list(table(terminal$fits$status)),
      readout_status_counts = as.list(table(terminal$recovery$status)),
      resume = resume
    )
  )
  artifact_paths <- c(
    checkpoint_results = lomm11_bridge_checkpoint_path(run_dir),
    stats::setNames(file.path(run_dir, unname(paths)), names(paths)),
    finalization = finalization_path,
    run_manifest = manifest_path
  )
  receipt <- list(
    schema_version = LOMM11_BRIDGE_SCHEMA_VERSION,
    status = "complete",
    config_hash = plan$config_hash,
    prospective_manifest_sha256 = plan$manifest_sha256,
    completed_at = lomm11_bridge_timestamp(),
    terminal_fit_keys = nrow(terminal$fits),
    result_rows = nrow(terminal$recovery),
    fit_status_counts = as.list(table(terminal$fits$status)),
    readout_status_counts = as.list(table(terminal$recovery$status)),
    attempts_tree_sha256 = lomm11_bridge_sha256(file.path(run_dir, "attempts")),
    attempt_artifact_count = length(list.files(
      file.path(run_dir, "attempts"), pattern = "[.]rds$", full.names = TRUE
    )),
    checkpoint_attempt_count = nrow(checkpoint),
    artifacts = lapply(names(artifact_paths), function(name) {
      list(path = basename(artifact_paths[[name]]),
           sha256 = lomm11_bridge_sha256(artifact_paths[[name]]))
    })
  )
  names(receipt$artifacts) <- names(artifact_paths)
  lomm11_bridge_write_json_atomic(receipt, file.path(run_dir, "_SUCCESS.json"))
  lomm11_bridge_validate_success(run_dir, plan)
  invisible(receipt)
}

lomm11_bridge_validate_success <- function(run_dir, plan) {
  success_path <- file.path(run_dir, "_SUCCESS.json")
  receipt <- lomm11_bridge_read_json(success_path)
  checkpoint <- lomm11_bridge_read_checkpoint(lomm11_bridge_checkpoint_path(run_dir))
  lomm11_bridge_validate_checkpoint(checkpoint, run_dir, plan)
  terminal <- lomm11_bridge_terminal_results(checkpoint, run_dir, plan)
  expected <- list(
    fit_results = terminal$fits,
    recovery_results = terminal$recovery,
    bridge_summary = lomm11_bridge_build_summary(terminal$recovery, plan$config),
    paired_bootstrap_effects = lomm11_bridge_build_paired_effects(
      terminal$recovery, plan$config
    )
  )
  final_paths <- stats::setNames(
    file.path(run_dir, paste0(names(expected), ".csv")), names(expected)
  )
  final_ok <- vapply(names(expected), function(name) {
    file.exists(final_paths[[name]]) && identical(
      lomm11_bridge_sha256(final_paths[[name]]),
      lomm11_bridge_frame_csv_sha256(expected[[name]])
    )
  }, logical(1))
  if (any(!final_ok)) lomm11_bridge_stop("A final bridge CSV failed strict validation.")
  manifest_path <- file.path(run_dir, "run_manifest.json")
  finalization_path <- file.path(run_dir, "_FINALIZATION.json")
  manifest <- lomm11_bridge_read_json(manifest_path)
  dependency_ok <- is.list(manifest$dependency_hashes) &&
    length(manifest$dependency_hashes) == length(plan$manifest$dependencies) &&
    all(vapply(seq_along(plan$manifest$dependencies), function(index) {
      identical(manifest$dependency_hashes[[index]]$path,
                plan$manifest$dependencies[[index]]$path) &&
        identical(manifest$dependency_hashes[[index]]$sha256,
                  plan$manifest$dependencies[[index]]$sha256)
    }, logical(1)))
  manifest_ok <- identical(manifest$status, "complete") &&
    identical(manifest$config_hash, plan$config_hash) &&
    identical(manifest$prospective_manifest_sha256, plan$manifest_sha256) &&
    !is.null(manifest$command) && !is.null(manifest$software_versions) &&
    !is.null(manifest$source_realism_run) && !is.null(manifest$dependency_hashes) &&
    !is.null(manifest$randomness_contract) && !is.null(manifest$analysis_contract) &&
    !is.null(manifest$claim_scope) && length(manifest$invocation_history) >= 1L &&
    identical(manifest$source_realism_run$config_hash,
              plan$config$source_realism_run$config_hash) &&
    identical(manifest$canonical_profile$profile_id,
              plan$config$canonical_profile$profile_id) &&
    identical(manifest$randomness_contract$fit_seed_pairing,
              plan$config$randomness$fit_seed_pairing) && dependency_ok
  artifact_paths <- c(
    checkpoint_results = lomm11_bridge_checkpoint_path(run_dir),
    final_paths,
    finalization = finalization_path,
    run_manifest = manifest_path
  )
  artifact_ok <- is.list(receipt$artifacts) &&
    identical(sort(names(receipt$artifacts)), sort(names(artifact_paths))) &&
    all(vapply(names(artifact_paths), function(name) {
      record <- receipt$artifacts[[name]]
      identical(record$path, basename(artifact_paths[[name]])) &&
        identical(record$sha256, lomm11_bridge_sha256(artifact_paths[[name]]))
    }, logical(1)))
  attempts_dir <- file.path(run_dir, "attempts")
  finalization <- lomm11_bridge_read_json(finalization_path)
  finalization_ok <- identical(finalization$status, "prepared") &&
    identical(finalization$config_hash, plan$config_hash) &&
    identical(finalization$prospective_manifest_sha256, plan$manifest_sha256) &&
    identical(sort(names(finalization$artifacts)), sort(names(expected))) &&
    all(vapply(names(expected), function(name) {
      record <- finalization$artifacts[[name]]
      identical(record$path, basename(final_paths[[name]])) &&
        identical(record$sha256, lomm11_bridge_sha256(final_paths[[name]]))
    }, logical(1)))
  receipt_ok <- identical(receipt$status, "complete") &&
    identical(receipt$config_hash, plan$config_hash) &&
    identical(receipt$prospective_manifest_sha256, plan$manifest_sha256) &&
    identical(as.integer(receipt$terminal_fit_keys),
              as.integer(plan$config$expected_counts$structure_fits)) &&
    identical(as.integer(receipt$result_rows),
              as.integer(plan$config$expected_counts$result_rows)) &&
    identical(as.integer(receipt$checkpoint_attempt_count), nrow(checkpoint)) &&
    identical(as.integer(receipt$attempt_artifact_count), length(list.files(
      attempts_dir, pattern = "[.]rds$", full.names = TRUE
    ))) &&
    identical(receipt$attempts_tree_sha256, lomm11_bridge_sha256(attempts_dir))
  if (!manifest_ok || !finalization_ok || !artifact_ok || !receipt_ok) {
    lomm11_bridge_stop("The bridge success receipt failed strict validation.")
  }
  invisible(TRUE)
}

lomm11_bridge_validate_run_mode <- function(args, run_dir, plan) {
  checkpoint <- lomm11_bridge_checkpoint_path(run_dir)
  success <- file.path(run_dir, "_SUCCESS.json")
  if (file.exists(success)) {
    lomm11_bridge_validate_success(run_dir, plan)
    lomm11_bridge_stop("This bridge namespace is already complete and validated.")
  }
  if (args$resume) {
    if (!dir.exists(run_dir) || !file.exists(checkpoint)) {
      lomm11_bridge_stop("Resume requires an existing bridge checkpoint.")
    }
  } else if (dir.exists(run_dir) &&
             length(list.files(run_dir, all.files = TRUE, no.. = TRUE)) > 0L) {
    lomm11_bridge_stop("Fresh execution refuses a nonempty run namespace.")
  }
  invisible(TRUE)
}

lomm11_bridge_limit_numeric_threads <- function() {
  variables <- c(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1"
  )
  do.call(Sys.setenv, as.list(variables))
  invisible(TRUE)
}

lomm11_bridge_execute <- function(args, plan) {
  lomm11_bridge_validate_authorization(args, plan)
  run_dir <- lomm11_bridge_run_directory(plan)
  lomm11_bridge_validate_run_mode(args, run_dir, plan)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  lock_dir <- lomm11_bridge_acquire_lock(run_dir)
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)
  lomm11_bridge_limit_numeric_threads()
  checkpoint_path <- lomm11_bridge_checkpoint_path(run_dir)
  if (!file.exists(checkpoint_path)) {
    lomm11_bridge_write_csv_atomic(
      lomm11_bridge_empty_checkpoint(), checkpoint_path
    )
  }
  lomm11_bridge_source_estimation_functions()
  checkpoint <- lomm11_bridge_read_checkpoint(checkpoint_path)
  lomm11_bridge_validate_checkpoint(checkpoint, run_dir, plan)
  invocation_started <- lomm11_bridge_timestamp()
  manifest_path <- file.path(run_dir, "run_manifest.json")
  lomm11_bridge_write_run_manifest(
    manifest_path, plan, "running", checkpoint, invocation_started,
    list(resume = args$resume)
  )
  pair_plan <- revision_core_panel_grid(plan$tables$pair_plan)
  pending <- lomm11_bridge_pending_fits(
    plan$tables$fit_plan, checkpoint,
    as.integer(plan$config$execution$max_attempts_per_fit)
  )
  pending_pairs <- unique(pending$pair_key)
  workers <- min(8L, as.integer(plan$config$execution$workers))
  started <- proc.time()[["elapsed"]]
  batches <- split(pending_pairs, ceiling(seq_along(pending_pairs) / workers))
  for (batch in batches) {
    if (proc.time()[["elapsed"]] - started >= plan$config$execution$job_timeout_seconds) {
      checkpoint <- lomm11_bridge_read_checkpoint(checkpoint_path)
      lomm11_bridge_write_run_manifest(
        manifest_path, plan, "partial", checkpoint, invocation_started,
        list(reason = "job_timeout_reached_at_batch_boundary", resume = args$resume)
      )
      lomm11_bridge_stop("Bridge job time limit reached; resume from the checkpoint.")
    }
    remaining <- plan$config$execution$job_timeout_seconds -
      (proc.time()[["elapsed"]] - started)
    results <- tryCatch(
      run_with_hard_timeout(remaining, parallel::mclapply(
        batch,
        function(pair_key) {
          pair_row <- pair_plan[pair_plan$pair_key == pair_key, , drop = FALSE]
          pair_pending <- pending[pending$pair_key == pair_key, , drop = FALSE]
          lomm11_bridge_run_pair(pair_row, pair_pending, plan, run_dir)
        },
        mc.cores = min(workers, length(batch)), mc.preschedule = FALSE,
        mc.set.seed = FALSE
      )),
      error = function(error) error
    )
    if (inherits(results, "error")) {
      checkpoint <- lomm11_bridge_read_checkpoint(checkpoint_path)
      lomm11_bridge_write_run_manifest(
        manifest_path, plan, "partial", checkpoint, invocation_started,
        list(reason = conditionMessage(results), resume = args$resume)
      )
      lomm11_bridge_stop(
        "Bridge batch stopped before checkpoint publication: ",
        conditionMessage(results), ". Resume is safe."
      )
    }
    lomm11_bridge_validate_worker_results(results, batch, pending)
    attempts <- unlist(results, recursive = FALSE)
    lomm11_bridge_publish_attempts(attempts, run_dir, checkpoint_path)
    checkpoint <- lomm11_bridge_read_checkpoint(checkpoint_path)
    lomm11_bridge_validate_checkpoint(checkpoint, run_dir, plan)
    lomm11_bridge_write_run_manifest(
      manifest_path, plan, "running", checkpoint, invocation_started,
      list(resume = args$resume)
    )
  }
  checkpoint <- lomm11_bridge_read_checkpoint(checkpoint_path)
  terminal <- lomm11_bridge_terminal_results(checkpoint, run_dir, plan)
  receipt <- lomm11_bridge_publish_completion(
    run_dir, plan, terminal, invocation_started, resume = args$resume
  )
  cat(
    "Completed the LOMM 1.1 recovery bridge at ", run_dir, "\n",
    "terminal fits: ", receipt$terminal_fit_keys, "\n",
    "recovery rows: ", receipt$result_rows, "\n", sep = ""
  )
  invisible(receipt)
}

lomm11_bridge_runner_main <- function() {
  args <- lomm11_bridge_parse_runner_args(commandArgs(trailingOnly = TRUE))
  plan <- lomm11_bridge_validate_plan(args$plan_dir, args$manifest_sha256)
  if (identical(args$mode, "dry-run")) {
    report <- list(
      status = "validated",
      dry_run = TRUE,
      config_hash = plan$config_hash,
      prospective_manifest_sha256 = plan$manifest_sha256,
      expected_counts = as.list(plan$config$expected_counts),
      maximum_workers = min(8L, as.integer(plan$config$execution$workers)),
      max_attempts_per_fit = as.integer(plan$config$execution$max_attempts_per_fit),
      estimator_calls = FALSE,
      simulation_executed = FALSE,
      run_output = lomm11_bridge_run_directory(plan)
    )
    cat(jsonlite::toJSON(report, auto_unbox = TRUE, pretty = TRUE), "\n")
    return(invisible(report))
  }
  lomm11_bridge_execute(args, plan)
}

if (sys.nframe() == 0L) lomm11_bridge_runner_main()
