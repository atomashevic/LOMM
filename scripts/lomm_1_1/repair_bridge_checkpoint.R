#!/usr/bin/env Rscript

lomm11_repair_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the bridge checkpoint repair script.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_REPAIR_SCRIPT_PATH <- lomm11_repair_script_path()
LOMM11_REPAIR_DIRECTORY <- dirname(LOMM11_REPAIR_SCRIPT_PATH)
LOMM11_REPAIR_ROOT <- normalizePath(
  file.path(LOMM11_REPAIR_DIRECTORY, "..", ".."), mustWork = TRUE
)
LOMM11_REPAIR_RUNNER_PATH <- file.path(
  LOMM11_REPAIR_DIRECTORY, "run_bridge_recovery.R"
)
LOMM11_REPAIR_RESUME_PATH <- file.path(
  LOMM11_REPAIR_DIRECTORY, "resume_bridge_recovery.R"
)

Sys.setenv(
  LOMM11_BRIDGE_RESUME_PATH = LOMM11_REPAIR_RESUME_PATH,
  LOMM11_BRIDGE_RUNNER_PATH = LOMM11_REPAIR_RUNNER_PATH,
  LOMM11_BRIDGE_PLANNER_PATH = file.path(
    LOMM11_REPAIR_DIRECTORY, "build_bridge_recovery_plan.R"
  ),
  LOMM11_BRIDGE_REPO_ROOT = LOMM11_REPAIR_ROOT
)
source(LOMM11_REPAIR_RESUME_PATH)

lomm11_repair_parse_args <- function(args) {
  parsed <- list(
    mode = NULL, plan_dir = NULL, run_dir = NULL,
    acknowledgement = NULL, manifest_sha256 = NULL
  )
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token %in% c("--dry-run", "--execute")) {
      if (!is.null(parsed$mode)) lomm11_bridge_stop("Choose one repair mode.")
      parsed$mode <- sub("^--", "", token)
      index <- index + 1L
      next
    }
    if (token %in% c(
      "--plan-dir", "--run-dir", "--ack-long-running",
      "--prospective-manifest-sha256"
    )) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        lomm11_bridge_stop("Missing value after ", token, ".")
      }
      destination <- switch(
        token,
        `--plan-dir` = "plan_dir",
        `--run-dir` = "run_dir",
        `--ack-long-running` = "acknowledgement",
        `--prospective-manifest-sha256` = "manifest_sha256"
      )
      parsed[[destination]] <- args[[index + 1L]]
      index <- index + 2L
      next
    }
    lomm11_bridge_stop("Unknown repair argument: ", token)
  }
  required <- c("mode", "plan_dir", "run_dir", "manifest_sha256")
  if (any(vapply(parsed[required], is.null, logical(1)))) {
    lomm11_bridge_stop(
      "Specify plan, run, manifest SHA, and exactly one repair mode."
    )
  }
  parsed
}

lomm11_repair_artifact_rows <- function(run_dir) {
  paths <- sort(list.files(
    file.path(run_dir, "attempts"), pattern = "[.]rds$", full.names = TRUE
  ))
  rows <- lapply(paths, function(path) {
    payload <- readRDS(path)
    if (!is.list(payload) || !is.data.frame(payload$checkpoint) ||
        nrow(payload$checkpoint) != 1L) {
      lomm11_bridge_stop("Malformed attempt receipt: ", path)
    }
    row <- payload$checkpoint
    row$attempt_artifact <- file.path("attempts", basename(path))
    row$attempt_receipt_sha256 <- lomm11_bridge_sha256(path)
    row
  })
  if (length(rows) == 0L) return(lomm11_bridge_empty_checkpoint())
  do.call(rbind, rows)
}

lomm11_repair_prepare <- function(args) {
  plan <- lomm11_bridge_validate_plan(args$plan_dir, args$manifest_sha256)
  run_dir <- normalizePath(args$run_dir, mustWork = TRUE)
  expected_run_dir <- normalizePath(lomm11_bridge_run_directory(plan), mustWork = TRUE)
  if (!identical(run_dir, expected_run_dir)) {
    lomm11_bridge_stop("The repair run directory does not match the plan.")
  }
  checkpoint_path <- lomm11_bridge_checkpoint_path(run_dir)
  if (!file.exists(checkpoint_path) || file.exists(file.path(run_dir, "_SUCCESS.json"))) {
    lomm11_bridge_stop("Repair requires an incomplete run with a checkpoint.")
  }
  checkpoint <- lomm11_bridge_read_checkpoint(checkpoint_path)
  lomm11_bridge_validate_checkpoint(checkpoint, run_dir, plan)
  before_sha256 <- lomm11_bridge_sha256(checkpoint_path)
  if (!identical(lomm11_bridge_frame_csv_sha256(checkpoint), before_sha256)) {
    lomm11_bridge_stop("Typed checkpoint reading would alter prior attempt bytes.")
  }
  artifact_rows <- lomm11_repair_artifact_rows(run_dir)
  additions <- artifact_rows[
    !artifact_rows$attempt_artifact %in% checkpoint$attempt_artifact,
    , drop = FALSE
  ]
  additions <- additions[order(
    additions$pair_key, additions$fit_key, additions$attempt
  ), , drop = FALSE]
  combined <- rbind(checkpoint, additions)
  if (nrow(additions) == 0L) {
    lomm11_bridge_stop("No orphan attempt receipts are available to repair.")
  }
  if (any(!vapply(seq_len(nrow(additions)), function(index) {
    lomm11_bridge_validate_attempt_artifact(
      additions[index, , drop = FALSE], run_dir, plan
    )
  }, logical(1)))) {
    lomm11_bridge_stop("An orphan attempt receipt failed strict validation.")
  }
  lomm11_bridge_validate_checkpoint(combined, run_dir, plan)
  temporary <- tempfile(".checkpoint-repair-", tmpdir = run_dir, fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(combined, temporary, row.names = FALSE, na = "")
  compatible <- lomm11_bridge_read_checkpoint(temporary)
  lomm11_bridge_validate_checkpoint(compatible, run_dir, plan)
  list(
    plan = plan, run_dir = run_dir, checkpoint_path = checkpoint_path,
    checkpoint = checkpoint, additions = additions, combined = combined,
    before_sha256 = before_sha256
  )
}

lomm11_repair_report <- function(prepared, status) {
  additions <- prepared$additions
  list(
    schema_version = 1L,
    status = status,
    config_hash = prepared$plan$config_hash,
    prospective_manifest_sha256 = prepared$plan$manifest_sha256,
    runner_sha256 = lomm11_bridge_sha256(LOMM11_REPAIR_RUNNER_PATH),
    repair_script_sha256 = lomm11_bridge_sha256(LOMM11_REPAIR_SCRIPT_PATH),
    resume_entrypoint = file.path(
      "scripts", "lomm_1_1", basename(LOMM11_REPAIR_RESUME_PATH)
    ),
    resume_entrypoint_sha256 = lomm11_bridge_sha256(LOMM11_REPAIR_RESUME_PATH),
    checkpoint_before_sha256 = prepared$before_sha256,
    checkpoint_rows_before = nrow(prepared$checkpoint),
    recovered_attempt_rows = nrow(additions),
    checkpoint_rows_after = nrow(prepared$combined),
    unique_fits_after = length(unique(prepared$combined$fit_key)),
    recovered_status_counts = as.list(table(additions$status)),
    recovered_attempt_counts = as.list(table(additions$attempt)),
    recovered_artifacts = lapply(seq_len(nrow(additions)), function(index) {
      list(
        path = additions$attempt_artifact[[index]],
        sha256 = additions$attempt_receipt_sha256[[index]]
      )
    })
  )
}

lomm11_repair_main <- function() {
  args <- lomm11_repair_parse_args(commandArgs(trailingOnly = TRUE))
  prepared <- lomm11_repair_prepare(args)
  if (identical(args$mode, "dry-run")) {
    cat(jsonlite::toJSON(
      lomm11_repair_report(prepared, "validated"),
      auto_unbox = TRUE, pretty = TRUE
    ), "\n")
    return(invisible(TRUE))
  }
  if (!identical(
    args$acknowledgement, prepared$plan$config$execution$authorization_token
  )) {
    lomm11_bridge_stop("Execution requires the literal acknowledgement token.")
  }
  lock_dir <- lomm11_bridge_acquire_lock(prepared$run_dir)
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)
  prepared <- lomm11_repair_prepare(args)
  receipt_path <- file.path(prepared$run_dir, "_CHECKPOINT_REPAIR.json")
  if (file.exists(receipt_path)) {
    lomm11_bridge_stop("A checkpoint repair receipt already exists.")
  }
  lomm11_bridge_write_csv_atomic(
    prepared$combined, prepared$checkpoint_path, replace = TRUE
  )
  repaired <- lomm11_bridge_read_checkpoint(prepared$checkpoint_path)
  lomm11_bridge_validate_checkpoint(repaired, prepared$run_dir, prepared$plan)
  receipt <- lomm11_repair_report(prepared, "repaired")
  receipt$repaired_at <- lomm11_bridge_timestamp()
  receipt$checkpoint_after_sha256 <- lomm11_bridge_sha256(
    prepared$checkpoint_path
  )
  lomm11_bridge_write_json_atomic(receipt, receipt_path)
  cat(jsonlite::toJSON(receipt, auto_unbox = TRUE, pretty = TRUE), "\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) lomm11_repair_main()
