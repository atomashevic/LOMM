#!/usr/bin/env Rscript

lomm11_bridge_test_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot locate bridge test script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_BRIDGE_TEST_PATH <- lomm11_bridge_test_path()
LOMM11_BRIDGE_TEST_DIRECTORY <- dirname(LOMM11_BRIDGE_TEST_PATH)
LOMM11_BRIDGE_TEST_ROOT <- normalizePath(
  file.path(LOMM11_BRIDGE_TEST_DIRECTORY, "..", ".."), mustWork = TRUE
)
Sys.setenv(LOMM11_BRIDGE_RUNNER_PATH = file.path(
  LOMM11_BRIDGE_TEST_DIRECTORY, "run_bridge_recovery.R"
))
source(file.path(LOMM11_BRIDGE_TEST_DIRECTORY, "run_bridge_recovery.R"))

lomm11_bridge_expect_true <- function(value, label) {
  if (!isTRUE(value)) stop("Bridge test failed: ", label, call. = FALSE)
}

lomm11_bridge_expect_error <- function(expression, pattern, label) {
  observed <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(error) conditionMessage(error)
  )
  if (is.null(observed) || !grepl(pattern, observed, fixed = TRUE)) {
    stop("Bridge test failed: ", label, call. = FALSE)
  }
}

lomm11_bridge_test_plan <- function() {
  plan <- lomm11_bridge_build_plan()
  counts <- vapply(plan$tables, nrow, integer(1))
  lomm11_bridge_expect_true(
    identical(unname(counts), c(80L, 160L, 800L, 2400L)),
    "prospective plan counts"
  )
  fits <- plan$tables$fit_plan
  readouts <- plan$tables$readout_plan
  lomm11_bridge_expect_true(
    all(startsWith(fits$fit_key, paste0(fits$representation_key, "|"))),
    "fit keys use native representation keys"
  )
  lomm11_bridge_expect_true(
    all(startsWith(readouts$readout_key, paste0(readouts$fit_key, "|"))),
    "readout keys extend fit keys"
  )
  lomm11_bridge_expect_true(
    identical(lomm11_bridge_json_sha256(plan$manifest),
              lomm11_bridge_json_sha256(plan$manifest)),
    "manifest serialization is byte-stable"
  )
  plan
}

lomm11_bridge_test_cli_and_authorization <- function(plan) {
  dry <- lomm11_bridge_parse_runner_args(c(
    "--plan-dir", "plan", "--dry-run"
  ))
  lomm11_bridge_expect_true(identical(dry$mode, "dry-run"), "dry-run parsing")
  execute <- lomm11_bridge_parse_runner_args(c(
    "--plan-dir", "plan", "--execute", "--resume",
    "--ack-long-running", plan$config$execution$authorization_token,
    "--prospective-manifest-sha256", paste(rep("a", 64L), collapse = "")
  ))
  lomm11_bridge_expect_true(
    execute$execute && execute$resume, "execute and resume parsing"
  )
  lomm11_bridge_expect_error(
    lomm11_bridge_parse_runner_args(c(
      "--plan-dir", "plan", "--dry-run", "--resume"
    )),
    "--resume is valid only with --execute", "resume gate"
  )
  authorization_plan <- list(
    config = plan$config,
    manifest_sha256 = paste(rep("b", 64L), collapse = "")
  )
  invalid <- list(
    execute = TRUE, acknowledgement = "wrong",
    manifest_sha256 = authorization_plan$manifest_sha256
  )
  lomm11_bridge_expect_error(
    lomm11_bridge_validate_authorization(invalid, authorization_plan),
    "literal acknowledgement token", "literal authorization token"
  )
  valid <- invalid
  valid$acknowledgement <- plan$config$execution$authorization_token
  lomm11_bridge_validate_authorization(valid, authorization_plan)
  invisible(TRUE)
}

lomm11_bridge_test_resume_selection <- function(plan) {
  fits <- plan$tables$fit_plan[seq_len(4L), , drop = FALSE]
  checkpoint <- data.frame(
    fit_key = c(fits$fit_key[[1L]], fits$fit_key[[2L]], fits$fit_key[[3L]],
                fits$fit_key[[3L]]),
    status = c("ok", "error", "error", "timeout"),
    attempt = c(1L, 1L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  pending <- lomm11_bridge_pending_fits(
    fits, checkpoint, plan$config$execution$max_attempts_per_fit
  )
  lomm11_bridge_expect_true(
    identical(pending$fit_key, c(fits$fit_key[[2L]], fits$fit_key[[4L]])),
    "resume skips ok and terminal second failures"
  )
  lomm11_bridge_expect_true(
    identical(pending$next_attempt, c(2L, 1L)),
    "resume attempt numbers"
  )
  invisible(TRUE)
}

lomm11_bridge_fixture_recovery <- function(plan) {
  readouts <- plan$tables$readout_plan
  panels <- plan$tables$panel_plan[, c(
    "panel_key", "scenario_family", "true_dimension"
  )]
  output <- merge(readouts, panels, by = "panel_key", sort = FALSE)
  output$n_dim_true <- output$true_dimension
  output$status <- "ok"
  output$n_dim_hat <- output$n_dim_true
  output$n_dim_exact <- 1
  output$assignment_accuracy <- 1
  output$adjusted_rand_index <- 1
  output$variation_information <- 0
  output$runtime_sec <- 10
  output
}

lomm11_bridge_test_estimands <- function(plan) {
  recovery <- lomm11_bridge_fixture_recovery(plan)
  summary <- lomm11_bridge_build_summary(recovery, plan$config)
  effects <- lomm11_bridge_build_paired_effects(
    recovery, plan$config, bootstrap_replicates = 20L
  )
  lomm11_bridge_expect_true(nrow(summary) == 210L, "summary row count")
  lomm11_bridge_expect_true(nrow(effects) == 105L, "effect row count")
  lomm11_bridge_expect_true(
    all(summary$n_total == 80L), "status-inclusive profile denominators"
  )
  lomm11_bridge_expect_true(
    all(abs(effects$estimate) < 1e-12), "paired zero-effect fixture"
  )
  lomm11_bridge_expect_true(
    all(effects$bootstrap_replicates == 20L), "bootstrap fixture count"
  )
  lomm11_bridge_expect_true(
    all(effects$bootstrap_quantile_type == 8L), "bootstrap quantile rule"
  )
  expected_seed <- mapply(function(method, algorithm, outcome) {
    revision_seed_from_key(paste(
      "bridge_bootstrap", method, algorithm, outcome, sep = "|"
    ))
  }, effects$method_id, effects$community_algorithm, effects$outcome)
  lomm11_bridge_expect_true(
    identical(as.integer(effects$bootstrap_seed), as.integer(expected_seed)) &&
      all(grepl("base131_plus_REVISION_SEED_ORIGIN",
                effects$bootstrap_seed_rule, fixed = TRUE)),
    "bootstrap seed provenance"
  )
  set.seed(991L)
  before <- .Random.seed
  invisible(lomm11_bridge_stratified_bootstrap(
    seq_len(8L), rep(letters[1:4], each = 2L), 5L, 123L
  ))
  lomm11_bridge_expect_true(
    identical(before, .Random.seed), "bootstrap preserves caller RNG"
  )
  invisible(TRUE)
}

lomm11_bridge_test_attempt_receipt <- function(plan) {
  run_dir <- tempfile("lomm11-bridge-attempt-test-")
  dir.create(run_dir)
  on.exit(unlink(run_dir, recursive = TRUE, force = TRUE), add = TRUE)
  fit <- plan$tables$fit_plan[1L, , drop = FALSE]
  planned_readouts <- plan$tables$readout_plan[
    plan$tables$readout_plan$fit_key == fit$fit_key[[1L]], , drop = FALSE
  ]
  readouts <- planned_readouts
  readouts$config_hash <- plan$config_hash
  readouts$status <- "ok"
  readouts$structure_fit_status <- "ok"
  readouts$structure_hash <- paste(rep("a", 64L), collapse = "")
  checkpoint <- as.data.frame(stats::setNames(
    replicate(length(LOMM11_BRIDGE_CHECKPOINT_COLUMNS), NA, simplify = FALSE),
    LOMM11_BRIDGE_CHECKPOINT_COLUMNS
  ), stringsAsFactors = FALSE)
  checkpoint$attempt_key <- paste0(fit$fit_key[[1L]], "|attempt=01")
  checkpoint$fit_key <- fit$fit_key[[1L]]
  checkpoint$pair_key <- fit$pair_key[[1L]]
  checkpoint$panel_key <- fit$panel_key[[1L]]
  checkpoint$profile_id <- fit$profile_id[[1L]]
  checkpoint$method_id <- fit$method_id[[1L]]
  checkpoint$attempt <- 1L
  checkpoint$status <- "ok"
  checkpoint$structure_hash <- paste(rep("a", 64L), collapse = "")
  checkpoint$runtime_seconds <- 1
  checkpoint$started_at <- "2026-08-11T00:00:00.000Z"
  checkpoint$finished_at <- "2026-08-11T00:00:01.000Z"
  checkpoint$worker_pid <- Sys.getpid()
  checkpoint$worker_host <- Sys.info()[["nodename"]]
  checkpoint$config_hash <- plan$config_hash
  checkpoint$prospective_manifest_sha256 <- plan$manifest_sha256
  payload <- list(checkpoint = checkpoint, readouts = readouts)
  published <- lomm11_bridge_attempt_artifact(run_dir, payload)
  checkpoint$attempt_artifact <- published$path
  checkpoint$attempt_receipt_sha256 <- published$sha256
  lomm11_bridge_expect_true(
    lomm11_bridge_validate_attempt_artifact(checkpoint, run_dir, plan),
    "immutable attempt receipt"
  )
  checkpoint_path <- lomm11_bridge_checkpoint_path(run_dir)
  lomm11_bridge_append_checkpoint(checkpoint, checkpoint_path)
  reread <- lomm11_bridge_read_checkpoint(checkpoint_path)
  lomm11_bridge_validate_checkpoint(reread, run_dir, plan)
  escaped <- reread
  escaped$attempt_artifact <- "../escape.rds"
  lomm11_bridge_expect_true(
    !lomm11_bridge_validate_attempt_artifact(escaped, run_dir, plan),
    "attempt artifacts cannot escape the run namespace"
  )
  wrong_identity <- reread
  wrong_identity$pair_key <- "foreign-pair"
  lomm11_bridge_expect_error(
    lomm11_bridge_validate_checkpoint(wrong_identity, run_dir, plan),
    "key or provenance contract", "checkpoint fit identity mapping"
  )
  lomm11_bridge_expect_error(
    lomm11_bridge_append_checkpoint(checkpoint, checkpoint_path),
    "append-only and unique", "duplicate attempt rejection"
  )
  retry_after_ok <- rbind(reread, reread)
  retry_after_ok$attempt[[2L]] <- 2L
  retry_after_ok$attempt_key[[2L]] <- paste0(fit$fit_key[[1L]], "|attempt=02")
  retry_after_ok$status[[2L]] <- "error"
  lomm11_bridge_expect_error(
    lomm11_bridge_validate_checkpoint(retry_after_ok, run_dir, plan),
    "successful fit cannot be retried", "no retry after a valid ok fit"
  )
  invisible(TRUE)
}

lomm11_bridge_test_lock_and_finalization <- function() {
  run_dir <- tempfile("lomm11-bridge-lock-test-")
  dir.create(run_dir)
  on.exit(unlink(run_dir, recursive = TRUE, force = TRUE), add = TRUE)
  lock_dir <- file.path(run_dir, ".runner.lock")
  dir.create(lock_dir)
  lomm11_bridge_write_json_atomic(list(
    pid = 1L, host = "foreign-host", acquired_at = "fixture"
  ), file.path(lock_dir, "owner.json"))
  lomm11_bridge_expect_error(
    lomm11_bridge_acquire_lock(run_dir),
    "foreign-host runner lock", "foreign lock is unreclaimable"
  )
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  artifact <- file.path(run_dir, "final.csv")
  frame <- data.frame(value = 1:3)
  lomm11_bridge_publish_csv_idempotent(frame, artifact)
  lomm11_bridge_publish_csv_idempotent(frame, artifact)
  lomm11_bridge_expect_error(
    lomm11_bridge_publish_csv_idempotent(data.frame(value = 4:6), artifact),
    "differs from canonical recovery", "crash-safe final artifact mismatch"
  )
  malformed <- list(structure("worker failure", class = "try-error"))
  lomm11_bridge_expect_error(
    lomm11_bridge_validate_worker_results(
      malformed, "pair", data.frame(pair_key = "pair", fit_key = "fit")
    ),
    "did not return a valid pair result", "parallel worker failure gate"
  )
  empty_dir <- file.path(run_dir, "empty-resume")
  dir.create(empty_dir)
  empty_checkpoint <- lomm11_bridge_checkpoint_path(empty_dir)
  lomm11_bridge_write_csv_atomic(lomm11_bridge_empty_checkpoint(), empty_checkpoint)
  empty <- lomm11_bridge_read_checkpoint(empty_checkpoint)
  lomm11_bridge_expect_true(nrow(empty) == 0L, "empty checkpoint header round trip")
  lomm11_bridge_validate_run_mode(list(resume = TRUE), empty_dir, list())
  invisible(TRUE)
}

lomm11_bridge_test_manifest_resume_history <- function(plan) {
  path <- tempfile("lomm11-bridge-manifest-", fileext = ".json")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  plan$directory <- "fixture-plan-directory"
  checkpoint <- lomm11_bridge_empty_checkpoint()
  started <- "2026-08-11T01:00:00.000Z"
  lomm11_bridge_write_run_manifest(
    path, plan, "running", checkpoint, started, list(resume = TRUE)
  )
  lomm11_bridge_write_run_manifest(
    path, plan, "complete", checkpoint, started, list(reason = "fixture")
  )
  manifest <- lomm11_bridge_read_json(path)
  lomm11_bridge_expect_true(
    length(manifest$invocation_history) == 1L &&
      isTRUE(manifest$invocation_history[[1L]]$resume),
    "resume provenance survives current-invocation status updates"
  )
  invisible(TRUE)
}

lomm11_bridge_test_full_pair_generation <- function(plan) {
  previous_directory <- getwd()
  outside_directory <- tempfile("lomm11-bridge-outside-cwd-")
  dir.create(outside_directory)
  setwd(outside_directory)
  lomm11_bridge_source_estimation_functions()
  lomm11_bridge_expect_true(
    identical(getwd(), outside_directory), "estimation preflight restores caller cwd"
  )
  setwd(previous_directory)
  unlink(outside_directory, recursive = TRUE, force = TRUE)
  lomm11_bridge_expect_true(
    exists("revision_fit_frozen_structure", mode = "function") &&
      identical(normalizePath(repo_root, mustWork = TRUE), LOMM11_BRIDGE_TEST_ROOT),
    "clean execution preflight sources the revision core at repository root"
  )
  pair <- revision_core_panel_grid(plan$tables$pair_plan[1L, , drop = FALSE])
  lomm11_bridge_expect_true(
    is.numeric(pair$eta[[1L]]) && is.numeric(pair$zeta[[1L]]) &&
      is.numeric(pair$sigma_q[[1L]]),
    "semicolon-packed execution parameters are normalized"
  )
  profiles <- lomm11_bridge_prepare_profiles(pair, plan)
  lomm11_bridge_expect_true(
    identical(profiles$homogeneous$panel,
              profiles$between_clip_scale$panel) == FALSE,
    "B+S changes the observed panel"
  )
  lomm11_bridge_expect_true(
    identical(profiles$homogeneous$oscillator_latents,
              profiles$between_clip_scale$oscillator_latents),
    "original oscillator latents remain paired"
  )
  fit <- plan$tables$fit_plan[
    plan$tables$fit_plan$pair_key == pair$pair_key[[1L]] &
      plan$tables$fit_plan$profile_id == "homogeneous" &
      plan$tables$fit_plan$method_id == "dynEGA", , drop = FALSE
  ]
  scenario <- lomm11_bridge_scenario_row(
    pair, fit, plan, profiles$homogeneous
  )
  native <- revision_panel_result_base(scenario, "dynEGA", "louvain")
  lomm11_bridge_expect_true(
    identical(native$structure_fit_key[[1L]], fit$fit_key[[1L]]),
    "native fit key matches prospective fit key"
  )
  expected_readout <- paste(fit$fit_key[[1L]], "louvain", sep = "|")
  lomm11_bridge_expect_true(
    identical(native$unit_key[[1L]], expected_readout),
    "native readout key matches prospective readout key"
  )
  Sys.setenv(BENCHMARK_GIMME_ALPHA = "0.99")
  diagnostic_dir <- tempfile("bridge-diagnostics-")
  cfg <- lomm11_bridge_fit_config(plan$config, 123L, diagnostic_dir)
  lomm11_bridge_expect_true(
    identical(cfg$gimme_alpha, 0.05), "ambient benchmark settings are ignored"
  )
  lomm11_bridge_expect_true(
    identical(Sys.getenv("BENCHMARK_GIMME_ALPHA"), "0.99"),
    "ambient benchmark settings are restored"
  )
  Sys.unsetenv("BENCHMARK_GIMME_ALPHA")
  lomm11_bridge_limit_numeric_threads()
  lomm11_bridge_expect_true(
    all(Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")) == "1"),
    "numeric thread limits"
  )
  lomm11_bridge_expect_error(
    run_with_hard_timeout(0.1, Sys.sleep(1)),
    "Timed out after", "hard job-timeout primitive"
  )
  invisible(TRUE)
}

lomm11_bridge_fixture_checkpoint_row <- function(fit, plan, structure_hash) {
  row <- as.data.frame(stats::setNames(
    replicate(length(LOMM11_BRIDGE_CHECKPOINT_COLUMNS), NA_character_, simplify = FALSE),
    LOMM11_BRIDGE_CHECKPOINT_COLUMNS
  ), stringsAsFactors = FALSE)
  row$attempt_key <- paste0(fit$fit_key[[1L]], "|attempt=01")
  row$fit_key <- fit$fit_key[[1L]]
  row$pair_key <- fit$pair_key[[1L]]
  row$panel_key <- fit$panel_key[[1L]]
  row$profile_id <- fit$profile_id[[1L]]
  row$method_id <- fit$method_id[[1L]]
  row$attempt <- 1L
  row$status <- "ok"
  row$panel_hash <- lomm11_bridge_hash_text(fit$panel_key[[1L]])
  row$source_panel_hash <- lomm11_bridge_hash_text(fit$pair_key[[1L]])
  row$structure_hash <- structure_hash
  row$runtime_seconds <- 1
  row$started_at <- "2026-08-11T00:00:00.000Z"
  row$finished_at <- "2026-08-11T00:00:01.000Z"
  row$worker_pid <- Sys.getpid()
  row$worker_host <- Sys.info()[["nodename"]]
  row$config_hash <- plan$config_hash
  row$prospective_manifest_sha256 <- plan$manifest_sha256
  row
}

lomm11_bridge_test_success_validation <- function(plan) {
  run_dir <- tempfile("lomm11-bridge-success-test-")
  dir.create(run_dir)
  on.exit(unlink(run_dir, recursive = TRUE, force = TRUE), add = TRUE)
  recovery <- lomm11_bridge_fixture_recovery(plan)
  recovery$structure_fit_status <- "ok"
  recovery$structure_fit_runtime_sec <- 1
  recovery$readout_runtime_sec <- 0
  rows <- vector("list", nrow(plan$tables$fit_plan))
  for (index in seq_len(nrow(plan$tables$fit_plan))) {
    fit <- plan$tables$fit_plan[index, , drop = FALSE]
    structure_hash <- lomm11_bridge_hash_text(fit$fit_key[[1L]])
    readouts <- recovery[recovery$fit_key == fit$fit_key[[1L]], , drop = FALSE]
    readouts$structure_hash <- structure_hash
    readouts$config_hash <- plan$config_hash
    checkpoint <- lomm11_bridge_fixture_checkpoint_row(fit, plan, structure_hash)
    payload <- list(checkpoint = checkpoint, readouts = readouts)
    published <- lomm11_bridge_attempt_artifact(run_dir, payload)
    checkpoint$attempt_artifact <- published$path
    checkpoint$attempt_receipt_sha256 <- published$sha256
    rows[[index]] <- checkpoint
  }
  checkpoint <- do.call(rbind, rows)
  lomm11_bridge_write_csv_atomic(
    checkpoint, lomm11_bridge_checkpoint_path(run_dir)
  )
  lomm11_bridge_validate_checkpoint(checkpoint, run_dir, plan)
  terminal <- lomm11_bridge_terminal_results(checkpoint, run_dir, plan)
  plan$directory <- "fixture-plan-directory"
  lomm11_bridge_publish_completion(
    run_dir, plan, terminal, "2026-08-11T00:00:00.000Z"
  )
  lomm11_bridge_validate_success(run_dir, plan)
  summary_path <- file.path(run_dir, "bridge_summary.csv")
  write("tamper", file = summary_path, append = TRUE)
  lomm11_bridge_expect_error(
    lomm11_bridge_validate_success(run_dir, plan),
    "final bridge CSV failed strict validation", "post-publication tamper detection"
  )
  invisible(TRUE)
}

lomm11_bridge_test_main <- function() {
  plan <- lomm11_bridge_test_plan()
  plan$manifest_sha256 <- lomm11_bridge_json_sha256(plan$manifest)
  lomm11_bridge_test_cli_and_authorization(plan)
  lomm11_bridge_test_resume_selection(plan)
  lomm11_bridge_test_estimands(plan)
  lomm11_bridge_test_attempt_receipt(plan)
  lomm11_bridge_test_lock_and_finalization()
  lomm11_bridge_test_manifest_resume_history(plan)
  lomm11_bridge_test_full_pair_generation(plan)
  lomm11_bridge_test_success_validation(plan)
  cat("PASS: LOMM 1.1 recovery bridge pure and simulation-only tests\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) lomm11_bridge_test_main()
