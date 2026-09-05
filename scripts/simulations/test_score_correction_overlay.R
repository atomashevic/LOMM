#!/usr/bin/env Rscript

# Small fixtures exercise correction admission and artifact-builder routing.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
root <- dirname(dirname(dirname(normalizePath(sub("^--file=", "", argument)))))
implementation <- new.env(parent = globalenv())
source(file.path(root, "scripts/figures/build_revision_v2_artifacts.R"), local = implementation)

expect <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}
expect_error <- function(expression, label) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  expect(inherits(error, "error"), paste("Expected rejection:", label))
}
sha256 <- function(path) digest::digest(file = path, algo = "sha256")

run_tests <- function() {
  temporary <- tempfile("score-correction-overlay-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  aggregate <- file.path(temporary, "aggregate_results", "fixture_hash")
  correction_dir <- file.path(temporary, "trajectory_score_correction", "fixture_run")
  dir.create(aggregate, recursive = TRUE)
  dir.create(correction_dir, recursive = TRUE)
  long <- data.frame(
    unit_key = c("dyn1", "other", "dyn2"),
    panel_key = c("panel1", "panel1", "panel2"),
    .source_job = c("t100_dynega", "t100_static", "t25_dynega"),
    method_id = c("dynEGA", "static_EGA", "dynEGA"),
    score_cor_common = c(0.5, 0.7, 0.6),
    score_nrmse_common = c(1.4, 0.4, 1.5),
    unrelated = c("a", "b", "c"), stringsAsFactors = FALSE
  )
  correction <- data.frame(
    unit_key = c("dyn2", "dyn1"),
    panel_key = c("panel2", "panel1"),
    source_job = c("t25_dynega", "t100_dynega"),
    old_score_cor_common = c(0.6, 0.5),
    old_score_nrmse_common = c(1.5, 1.4),
    score_cor_common = c(0.9, 0.95),
    score_nrmse_common = c(0.2, 0.1), stringsAsFactors = FALSE
  )
  input_names <- c("revision_results_long.csv", "revision_estimand_results.csv",
    "revision_paired_differences.csv")
  output_names <- input_names[-1L]
  utils::write.csv(long, file.path(aggregate, input_names[[1L]]), row.names = FALSE)
  for (filename in output_names) {
    utils::write.csv(data.frame(source = "original"), file.path(aggregate, filename), row.names = FALSE)
    utils::write.csv(data.frame(source = "corrected"), file.path(correction_dir, filename), row.names = FALSE)
  }
  checkpoint <- file.path(correction_dir, "checkpoint_results.csv")
  receipt_path <- file.path(correction_dir, "summary_status.json")
  make_receipt <- function(frame = correction) {
    utils::write.csv(frame, checkpoint, row.names = FALSE)
    inputs <- c(file.path(aggregate, input_names), checkpoint)
    list(status = "complete", source_aggregate_dir = aggregate,
      corrected_readouts = nrow(frame),
      input_hashes = as.list(setNames(vapply(inputs, sha256, character(1)), inputs)),
      output_hashes = as.list(setNames(vapply(file.path(correction_dir, output_names),
        sha256, character(1)), output_names)))
  }
  save_receipt <- function(receipt) {
    jsonlite::write_json(receipt, receipt_path, auto_unbox = TRUE)
  }
  reset <- function(frame = correction) {
    receipt <- make_receipt(frame)
    save_receipt(receipt)
    receipt
  }
  load <- function(data = long) {
    suppressMessages(implementation$revision_v2_corrected_scores(aggregate, correction_dir, data))
  }
  receipt <- reset()
  expected <- long
  expected$score_cor_common[c(1L, 3L)] <- c(0.95, 0.9)
  expected$score_nrmse_common[c(1L, 3L)] <- c(0.1, 0.2)
  expect(identical(load(), expected), "Only the matched DynEGA metric cells should change.")
  unlink(receipt_path)
  expect_error(load(), "missing receipt")
  receipt$status <- "running"
  save_receipt(receipt)
  expect_error(load(), "unfinished receipt")
  receipt <- reset()
  receipt$input_hashes[[file.path(aggregate, input_names[[1L]])]] <- strrep("0", 64L)
  save_receipt(receipt)
  expect_error(load(), "wrong aggregate hash")
  receipt <- reset()
  receipt$output_hashes[[output_names[[1L]]]] <- strrep("0", 64L)
  save_receipt(receipt)
  expect_error(load(), "wrong corrected summary hash")
  receipt <- reset()
  receipt$input_hashes[[checkpoint]] <- strrep("0", 64L)
  save_receipt(receipt)
  expect_error(load(), "wrong checkpoint hash")
  reset(correction[1L, ])
  expect_error(load(), "missing unit")
  reset(rbind(correction, correction[1L, ]))
  expect_error(load(), "duplicate unit")
  bad <- correction
  bad$unit_key[[1L]] <- "unknown"
  reset(bad)
  expect_error(load(), "unknown unit")
  for (column in c("source_job", "panel_key")) {
    bad <- correction
    bad[[column]][[1L]] <- "wrong"
    reset(bad)
    expect_error(load(), paste("mismatched", column))
    bad[[column]] <- NULL
    reset(bad)
    expect_error(load(), paste("missing", column))
  }
  for (metric in c("score_cor_common", "score_nrmse_common")) {
    bad <- correction
    bad[[paste0("old_", metric)]][[1L]] <- 0.123
    reset(bad)
    expect_error(load(), paste("mismatched old", metric))
    bad <- correction
    bad[[metric]][[1L]] <- Inf
    reset(bad)
    expect_error(load(), paste("nonfinite corrected", metric))
  }
  receipt <- reset()
  receipt$corrected_readouts <- receipt$corrected_readouts + 1L
  save_receipt(receipt)
  expect_error(load(), "incorrect receipt count")
  reset()
  expect_error(load(rbind(long, long[1L, ])), "ambiguous aggregate unit")

  parsed <- implementation$revision_v2_parse_cli(c("--aggregate-dir", aggregate,
    "--output-root", "output", "--score-corrections", correction_dir))
  expect(identical(parsed$score_corrections, correction_dir), "CLI correction directory.")
  expect(is.null(implementation$revision_v2_parse_cli(c("--aggregate-dir", aggregate,
    "--output-root", "output"))$score_corrections), "Default CLI routing.")
  expect_error(implementation$revision_v2_parse_cli(c("--aggregate-dir", aggregate,
    "--output-root", "output", "--score-corrections")), "missing CLI value")

  # Capture inputs immediately before preparation, so routing exercises no render.
  routing <- new.env(parent = implementation)
  routing$revision_v2_build <- implementation$revision_v2_build
  environment(routing$revision_v2_build) <- routing
  routing$revision_v2_require_aggregate_contract <- function(...) invisible(TRUE)
  routing$revision_v2_read_csv <- function(directory, filename, columns) {
    utils::read.csv(file.path(directory, filename), stringsAsFactors = FALSE)
  }
  routing$revision_v2_prepare_artifacts <- function(estimands, paired, long) {
    routing$captured <- list(estimands = estimands, paired = paired, long = long)
    stop("fixture: captured before rendering", call. = FALSE)
  }
  build_until_captured <- function(explicit = NULL) {
    routing$captured <- NULL
    error <- tryCatch({
      suppressMessages(routing$revision_v2_build(aggregate, file.path(temporary, "unused"), explicit))
      NULL
    }, error = identity)
    expect(inherits(error, "error") && identical(conditionMessage(error),
      "fixture: captured before rendering"), "Builder must reach the capture point.")
    routing$captured
  }
  captured <- build_until_captured()
  expect(identical(captured$long, long) && captured$estimands$source == "original" &&
    captured$paired$source == "original", "No correction preserves original routing.")
  captured <- build_until_captured(correction_dir)
  expect(identical(captured$long, expected) && captured$estimands$source == "corrected" &&
    captured$paired$source == "corrected", "Explicit correction routes all three artifacts.")
  active <- file.path(dirname(correction_dir), "active")
  expect(file.symlink(correction_dir, active), "Create temporary active link.")
  captured <- build_until_captured()
  expect(identical(captured$long, expected) && captured$estimands$source == "corrected",
    "Active correction must be loaded automatically.")
  receipt <- reset()
  receipt$source_aggregate_dir <- file.path(dirname(aggregate), "other_hash")
  save_receipt(receipt)
  captured <- build_until_captured()
  expect(identical(captured$long, long) && captured$estimands$source == "original",
    "An active correction for another aggregate must not apply.")
  receipt <- reset()
  receipt$source_aggregate_dir <- NULL
  save_receipt(receipt)
  expect_error(suppressMessages(routing$revision_v2_build(aggregate, "unused")),
    "active correction without an aggregate identity")
  receipt <- reset()
  receipt$status <- "running"
  save_receipt(receipt)
  expect_error(suppressMessages(routing$revision_v2_build(aggregate, "unused")),
    "active correction without completed summaries")
  reset()
  unlink(receipt_path)
  expect_error(suppressMessages(routing$revision_v2_build(aggregate, "unused")),
    "unfinished active correction")

  # Existing tiny constant-value fixtures make the 2,000-draw checks inexpensive.
  fixture <- implementation$revision_v2_fixture_long()
  failed <- fixture$method_id == "gimme" & fixture$TimeP == 25L
  fixture$status[failed] <- "error"
  fixture$n_dim_hat[failed] <- NA_real_
  canonical <- implementation$revision_v2_canonical_method_rows(fixture)
  all_methods <- implementation$revision_v2_prepare_correct_dimension_trajectories(canonical)
  subset <- implementation$revision_v2_prepare_correct_dimension_trajectories(canonical, methods = "dynEGA")
  reference <- all_methods[all_methods$method_id == "dynEGA", ]
  rownames(reference) <- rownames(subset) <- NULL
  expect(nrow(all_methods) == 54L && nrow(subset) == 12L && identical(reference, subset),
    "DynEGA subset must equal the default summary's 12 DynEGA rows.")
  expect_error(implementation$revision_v2_prepare_correct_dimension_trajectories(canonical,
    methods = c("dynEGA", "dynEGA")), "duplicate method selection")
  expect_error(implementation$revision_v2_prepare_correct_dimension_trajectories(canonical,
    methods = "unknown"), "unknown method selection")
  cat("score correction overlay tests: PASS\n")
}

run_tests()
