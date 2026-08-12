#!/usr/bin/env Rscript

lomm11_resume_script_path <- function() {
  configured <- Sys.getenv("LOMM11_BRIDGE_RESUME_PATH", "")
  if (nzchar(configured) && file.exists(configured)) {
    return(normalizePath(configured, mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the bridge resume entrypoint.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_RESUME_SCRIPT_PATH <- lomm11_resume_script_path()
LOMM11_RESUME_DIRECTORY <- dirname(LOMM11_RESUME_SCRIPT_PATH)
LOMM11_RESUME_ROOT <- normalizePath(
  file.path(LOMM11_RESUME_DIRECTORY, "..", ".."), mustWork = TRUE
)
LOMM11_RESUME_RUNNER_PATH <- file.path(
  LOMM11_RESUME_DIRECTORY, "run_bridge_recovery.R"
)
LOMM11_RESUME_PINNED_RUNNER_SHA256 <- paste0(
  "210d540f6b6377f16579d2454a27b5d2",
  "8c815b6af4a0f508c2bc61c3409dbb3d"
)

Sys.setenv(
  LOMM11_BRIDGE_RUNNER_PATH = LOMM11_RESUME_RUNNER_PATH,
  LOMM11_BRIDGE_PLANNER_PATH = file.path(
    LOMM11_RESUME_DIRECTORY, "build_bridge_recovery_plan.R"
  ),
  LOMM11_BRIDGE_REPO_ROOT = LOMM11_RESUME_ROOT
)
source(LOMM11_RESUME_RUNNER_PATH)

if (!identical(
  lomm11_bridge_sha256(LOMM11_RESUME_RUNNER_PATH),
  LOMM11_RESUME_PINNED_RUNNER_SHA256
)) {
  lomm11_bridge_stop("The resume entrypoint requires the pinned bridge runner.")
}

LOMM11_RESUME_WRITE_RUN_MANIFEST <- lomm11_bridge_write_run_manifest

lomm11_bridge_write_run_manifest <- function(path, plan, status, checkpoint,
                                               invocation_started,
                                               details = list()) {
  LOMM11_RESUME_WRITE_RUN_MANIFEST(
    path, plan, status, checkpoint, invocation_started, details
  )
  manifest <- lomm11_bridge_read_json(path)
  manifest$command <- paste(
    "Rscript scripts/lomm_1_1/resume_bridge_recovery.R",
    paste(commandArgs(trailingOnly = TRUE), collapse = " ")
  )
  manifest$resume_io_patch <- list(
    entrypoint = "scripts/lomm_1_1/resume_bridge_recovery.R",
    entrypoint_sha256 = lomm11_bridge_sha256(LOMM11_RESUME_SCRIPT_PATH),
    pinned_runner_sha256 = LOMM11_RESUME_PINNED_RUNNER_SHA256,
    checkpoint_column_types = "explicit",
    blank_fields = "NA"
  )
  lomm11_bridge_write_json_atomic(manifest, path)
  invisible(manifest)
}

lomm11_bridge_read_checkpoint <- function(path) {
  if (!file.exists(path)) return(lomm11_bridge_empty_checkpoint())
  classes <- stats::setNames(
    rep("character", length(LOMM11_BRIDGE_CHECKPOINT_COLUMNS)),
    LOMM11_BRIDGE_CHECKPOINT_COLUMNS
  )
  classes[c("attempt", "worker_pid")] <- "integer"
  classes[["runtime_seconds"]] <- "numeric"
  rows <- utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE,
    colClasses = unname(classes), na.strings = ""
  )
  if (!identical(names(rows), LOMM11_BRIDGE_CHECKPOINT_COLUMNS)) {
    lomm11_bridge_stop("The bridge checkpoint schema changed.")
  }
  rows
}

if (sys.nframe() == 0L) lomm11_bridge_runner_main()
