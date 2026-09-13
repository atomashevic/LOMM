#!/usr/bin/env Rscript

test_stop <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) {
    test_stop(message)
  }
}

expect_equal <- function(actual, expected, tolerance = 1e-10, message = NULL) {
  equal <- isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))
  if (!equal) {
    test_stop(if (is.null(message)) "Values differ." else message)
  }
}

expect_error <- function(expression, pattern, message) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = function(error) error)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    test_stop(message)
  }
}

test_script_path <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", file_argument[[1]]), mustWork = TRUE)
}

test_root <- normalizePath(file.path(dirname(test_script_path()), "..", ".."), mustWork = TRUE)
source(file.path(test_root, "scripts", "simulations", "revision_spec.R"))
source(file.path(test_root, "scripts", "simulations", "run_revision_job.R"))
source(file.path(test_root, "scripts", "figures", "build_revision_v2_artifacts.R"))

if (!exists("normalize_community_map", mode = "function")) {
  normalize_community_map <- function(membership) {
    item_names <- names(membership)
    membership <- as.integer(factor(membership, levels = unique(membership)))
    names(membership) <- item_names
    membership
  }
}

test_transition_and_stability <- function() {
  transition <- revision_exact_transition(-1, 0, pi / 2)
  expect_equal(
    transition$transition,
    matrix(c(0, 1, -1, 0), nrow = 2, byrow = TRUE),
    tolerance = 1e-9,
    message = "Exact harmonic-oscillator transition is incorrect."
  )
  expect_equal(
    as.numeric(transition$innovation_map),
    c(1, 1),
    tolerance = 1e-9,
    message = "Exact zero-order-hold input map is incorrect."
  )
  stable <- revision_stability(-1, -0.2, 0.4)
  anti_damped <- revision_stability(-0.1, 0.1, 0.4)
  expect_true(stable$stability_class[[1]] == "asymptotically_stable", "Stable regime mislabeled.")
  expect_true(anti_damped$stability_class[[1]] == "anti_damped", "Anti-damped regime mislabeled.")
  roots <- revision_oscillator_roots(c(-1, 0, -1), c(-0.2, -1, 0.2))
  expect_true(max(Re(roots[1, ])) < 0, "Stable roots are incorrect.")
  expect_equal(max(Re(roots[2, ])), 0, message = "Critical roots are incorrect.")
  expect_true(max(Re(roots[3, ])) > 0, "Anti-damped roots are incorrect.")
  expect_equal(revision_horizon_metadata(25, 0.4)$observed_horizon[[1]], 9.6)
  expect_equal(revision_horizon_metadata(100, 0.4)$observed_horizon[[1]], 39.6)

  covariance <- revision_stationary_state_covariance(-1, -0.2, 0.3, 0.4)
  exact <- revision_exact_transition(-1, -0.2, 0.4)
  residual <- covariance - exact$transition %*% covariance %*% t(exact$transition) -
    0.3^2 * tcrossprod(exact$innovation_map)
  expect_true(max(abs(residual)) < 1e-8, "Stationary covariance fails the Lyapunov equation.")
}

test_observation_helpers <- function() {
  values <- matrix(c(-1, 0, 1, 2, 0, -2), nrow = 2, byrow = TRUE)
  probabilities <- revision_row_softmax(values, alpha = c(-0.1, 0, 0.1), tau = 0.75)
  shifted <- revision_row_softmax(values + 7, alpha = c(-0.1, 0, 0.1), tau = 0.75)
  rescaled <- revision_row_softmax(values * 2, alpha = c(-0.2, 0, 0.2), tau = 1.5)
  expect_equal(rowSums(probabilities), rep(1, 2), tolerance = 1e-12)
  expect_equal(probabilities, shifted, tolerance = 1e-12, message = "Softmax shift invariance failed.")
  expect_equal(probabilities, rescaled, tolerance = 1e-12, message = "Temperature-scale ambiguity failed.")
  expect_equal(rowSums(revision_clr(probabilities)), rep(0, 2), tolerance = 1e-12)

  schedule <- c(rep(1L, 3), rep(2L, 7))
  envelope <- revision_smooth_activity_envelope(schedule, 2, 1, 0.1, 5)
  expect_equal(envelope[4, ], c(1, 0.1), tolerance = 1e-12)
  expect_equal(envelope[8, ], c(0.1, 1), tolerance = 1e-12)
  expect_true(all(envelope >= 0.1 & envelope <= 1), "Smooth gate left its configured bounds.")
}

test_dynega_time_scale_contract <- function() {
  config <- revision_read_config(
    file.path(test_root, "scripts", "simulations", "revision_jobs.json")
  )$value
  dynega_jobs <- c("t100_dynega", "t25_dynega", "challenge_dynega")
  for (job_id in dynega_jobs) {
    job <- revision_config_job(config, job_id)
    expect_true(
      isTRUE(revision_validate_dynega_config(job_id, job)),
      paste("DynEGA time-scale contract failed for", job_id)
    )
    expect_equal(
      job$parameters$physical_dt,
      REVISION_STRUCTURE_PHYSICAL_DT,
      message = paste("Physical cadence is incorrect for", job_id)
    )
    expect_equal(
      job$parameters$dynega_observation_delta,
      REVISION_DYNEGA_OBSERVATION_DELTA,
      message = paste("Observation-unit delta is incorrect for", job_id)
    )
  }
  invalid <- revision_config_job(config, "t100_dynega")
  invalid$parameters$dynega_observation_delta <- 0.4
  expect_error(
    revision_validate_dynega_config("t100_dynega", invalid),
    "dynega_observation_delta = 1",
    "DynEGA accepted the physical cadence as its observation-unit delta."
  )

  set.seed(20260715)
  levels <- matrix(stats::rnorm(400), ncol = 4L)
  observation_differences <- levels[-1L, , drop = FALSE] -
    levels[-nrow(levels), , drop = FALSE]
  physical_time_derivatives <- observation_differences /
    REVISION_STRUCTURE_PHYSICAL_DT
  expect_equal(
    stats::cor(observation_differences),
    stats::cor(physical_time_derivatives),
    tolerance = 1e-12,
    message = "Common physical-time rescaling changed the derivative correlation matrix."
  )

  schema <- revision_output_schema("t100_dynega")$results
  expect_true(
    all(c("dt", "dynega_delta", "dynega_delta_units") %in% schema),
    "DynEGA output schema does not distinguish physical and observation-unit time."
  )
  runner_text <- readLines(
    file.path(test_root, "scripts", "simulations", "run_revision_job.R"),
    warn = FALSE
  )
  expect_true(
    any(grepl("delta = REVISION_DYNEGA_OBSERVATION_DELTA", runner_text, fixed = TRUE)),
    "DynEGA runner does not use the locked observation-unit delta."
  )
  expect_true(
    !any(grepl("delta = scen_row$dt", runner_text, fixed = TRUE)),
    "DynEGA runner still passes the physical cadence to EGAnet delta."
  )
  if (requireNamespace("EGAnet", quietly = TRUE)) {
    validator <- getFromNamespace("dynEGA_errors", "EGAnet")
    fixture <- matrix(seq_len(20L), ncol = 2L)
    validated <- validator(
      fixture,
      id = NULL,
      group = NULL,
      n.embed = 3,
      tau = 1,
      delta = REVISION_DYNEGA_OBSERVATION_DELTA,
      use.derivatives = 1,
      ncores = 1,
      verbose = FALSE
    )
    expect_equal(validated, fixture, message = "EGAnet rejected delta = 1.")
    expect_error(
      validator(
        fixture,
        id = NULL,
        group = NULL,
        n.embed = 3,
        tau = 1,
        delta = REVISION_STRUCTURE_PHYSICAL_DT,
        use.derivatives = 1,
        ncores = 1,
        verbose = FALSE
      ),
      "Minimum value of 'delta' (0.4)",
      "EGAnet validation no longer rejects physical dt = 0.4 as delta."
    )
  }
}

test_empirical_dynega_parallel_contract <- function() {
  config <- revision_read_config(
    file.path(test_root, "scripts", "simulations", "revision_jobs.json")
  )$value
  job <- revision_config_job(config, "empirical_cv")
  expect_true(
    isTRUE(revision_validate_empirical_config("empirical_cv", job)),
    "Empirical DynEGA parallel contract rejected the locked configuration."
  )
  invalid <- job
  invalid$parameters$frozen_dynega_ncores <- 2L
  expect_error(
    revision_validate_empirical_config("empirical_cv", invalid),
    "frozen_dynega_ncores = 1",
    "Empirical CV accepted internal DynEGA parallelism."
  )
  invalid <- job
  invalid$parameters$outer_parallel_backend <- "fork"
  expect_error(
    revision_validate_empirical_config("empirical_cv", invalid),
    "outer_parallel_backend = psock",
    "Empirical CV accepted a fork backend after DynEGA."
  )

  extract_environment <- new.env(parent = globalenv())
  previous_option <- getOption("run_extract_pipeline")
  on.exit(options(run_extract_pipeline = previous_option), add = TRUE)
  options(run_extract_pipeline = FALSE)
  sys.source(
    file.path(test_root, "scripts", "extract.r"),
    envir = extract_environment
  )
  wrapper <- extract_environment$fit_pooled_ord0_dynega
  expect_equal(
    formals(wrapper)$ncores,
    1L,
    message = "Pooled DynEGA does not default to one internal core."
  )
  wrapper_body <- paste(deparse(body(wrapper)), collapse = "\n")
  expect_true(
    grepl("ncores = ncores", wrapper_body, fixed = TRUE),
    "Pooled DynEGA does not forward its locked ncores value."
  )
  expect_true(
    grepl("seed = seed", wrapper_body, fixed = TRUE),
    "Pooled DynEGA does not forward its reproducibility seed."
  )
  runner_body <- paste(deparse(body(revision_run_empirical_cv)), collapse = "\n")
  expect_true(
    !grepl("mclapply", runner_body, fixed = TRUE),
    "Empirical CV still forks after fitting DynEGA."
  )
  expect_true(
    grepl("revision_start_empirical_cluster", runner_body, fixed = TRUE),
    "Empirical CV does not initialize its PSOCK worker pool."
  )
}

test_empirical_psock_evaluation <- function() {
  revision_source_estimation_core()
  score_columns <- c("anger", "fear")
  clip_lengths <- c(clip_a = 12L, clip_b = 12L)
  empirical_panel <- data.frame(
    anger = c(seq(0.1, 0.7, length.out = 12L), seq(0.2, 0.8, length.out = 12L)),
    fear = c(seq(0.8, 0.2, length.out = 12L), seq(0.7, 0.1, length.out = 12L)),
    clip = rep(names(clip_lengths), clip_lengths),
    frame_index = rep(seq_len(12L), 2L),
    stringsAsFactors = FALSE
  )
  target <- revision_empirical_metric_distribution(empirical_panel, score_columns)
  frozen_model <- list(
    community_map = setNames(seq_along(score_columns), score_columns),
    center = vapply(empirical_panel[, score_columns], mean, numeric(1)),
    scale = vapply(empirical_panel[, score_columns], stats::sd, numeric(1)),
    n_dim = length(score_columns)
  )
  frozen_panel <- revision_apply_training_frozen_summary(
    empirical_panel,
    frozen_model,
    score_columns
  )
  frozen_columns <- setdiff(names(frozen_panel), c("clip", "frame_index"))
  secondary_target <- revision_empirical_metric_distribution(
    frozen_panel,
    frozen_columns
  )
  rows <- data.frame(
    panel_key = c("psock-test-1", "psock-test-2"),
    fold = c(1L, 1L),
    candidate_id = c(1L, 2L),
    calibration_regime = c("stable", "stable"),
    eta = c(-1, -0.8),
    zeta = c(-0.2, -0.2),
    sigma_q = c(0.1, 0.1),
    dt = c(0.4, 0.4),
    cv_tau = c(1, 1),
    seed_base = c(1001L, 1002L),
    stringsAsFactors = FALSE
  )
  context <- list(config_hash = "psock-test", generator_hash = "psock-test")
  inputs <- revision_empirical_worker_inputs(
    rows,
    score_columns,
    clip_lengths,
    c(anger = 0, fear = 0),
    target,
    frozen_model,
    secondary_target,
    context,
    "source-test",
    "input-test"
  )
  cluster <- revision_start_empirical_cluster(2L)
  on.exit(revision_stop_empirical_cluster(cluster), add = TRUE)
  worker_pids <- unlist(parallel::clusterCall(cluster, Sys.getpid), use.names = FALSE)
  expect_true(
    length(unique(worker_pids)) == 2L && !Sys.getpid() %in% worker_pids,
    "Empirical PSOCK workers are not independent R processes."
  )
  revision_configure_empirical_cluster(cluster, inputs)
  evaluated <- revision_evaluate_empirical_batch(cluster, seq_len(nrow(rows)), inputs)
  expect_equal(length(evaluated), 2L)
  expect_true(
    all(vapply(evaluated, function(row) row$status[[1]] == "ok", logical(1))),
    "Empirical PSOCK evaluation did not return successful rows."
  )
  expect_true(
    all(vapply(
      evaluated,
      function(row) is.finite(row$continuous_loss[[1]]),
      logical(1)
    )),
    "Empirical PSOCK evaluation returned a non-finite loss."
  )
}

test_grids_and_folds <- function() {
  t100 <- revision_matched_panel_grid(100)
  t25 <- revision_matched_panel_grid(25)
  challenge <- revision_challenge_panel_grid()
  empirical <- revision_empirical_cv_plan()
  expect_true(nrow(t100) == 21600L && !anyDuplicated(t100$panel_key), "T=100 grid is invalid.")
  expect_true(nrow(t25) == 21600L && !anyDuplicated(t25$panel_key), "T=25 grid is invalid.")
  pairing_columns <- c("scenario", "N", "NVAR", "Error_sd", "Loadings", "Q", "rep")
  pairing_key <- function(data) do.call(paste, c(data[pairing_columns], sep = "|"))
  expect_equal(
    t25$seed_base[order(pairing_key(t25))],
    t100$seed_base[order(pairing_key(t100))],
    message = "Matched horizons do not share paired seeds."
  )
  expect_true(
    all(t25$panel_key != t100$panel_key),
    "Matched horizons must retain distinct panel keys."
  )
  expect_true(length(unique(challenge$panel_key)) == 4200L, "Challenge base count is invalid.")
  expect_true(nrow(challenge) == 4800L, "Challenge representation count is invalid.")
  challenge_base <- revision_challenge_base_grid(reps = 2L)
  challenge_groups <- split(
    challenge_base$seed_base,
    paste(challenge_base$scenario, challenge_base$rep, sep = "|")
  )
  expect_true(
    all(vapply(challenge_groups, function(seeds) length(unique(seeds)) == 1L, logical(1))),
    "Challenge variants do not share paired seeds."
  )
  expect_true(nrow(empirical$training) == 13500L, "Empirical training count is invalid.")
  expect_true(nrow(empirical$heldout) == 500L, "Empirical held-out count is invalid.")

  clip_ids <- sprintf("clip-%02d", seq_len(20))
  labels <- c(rep("a", 3), rep("b", 7), rep("c", 10))
  lengths <- rep(c(20, 10, 30, 15, 25), 4)
  first <- revision_assign_clip_folds(clip_ids, labels, lengths)
  second <- revision_assign_clip_folds(clip_ids, labels, lengths)
  expect_equal(first, second, message = "Fold assignment is not deterministic.")
  fold_sizes <- as.integer(table(factor(first$fold, levels = seq_len(5))))
  expect_true(max(fold_sizes) - min(fold_sizes) <= 1L, "Deterministic folds are not balanced.")
}

test_partition_and_checkpoint_helpers <- function() {
  asymmetric <- matrix(c(0, -2, 4, NA), nrow = 2)
  expect_equal(
    revision_symmetrize_weights(asymmetric),
    matrix(c(0, 1, 1, 0), nrow = 2),
    message = "Common weight symmetrization is incorrect."
  )
  negative_gimme <- matrix(c(0, -2, -4, 0), nrow = 2)
  expect_equal(
    revision_gimme_positive_structure(negative_gimme),
    matrix(0, nrow = 2, ncol = 2),
    message = "GIMME positive-part mapping resurrected negative paths."
  )
  pair_key <- "challenge-pair"
  expect_equal(
    revision_component_seed(pair_key, 1L, "residual_innovations"),
    revision_component_seed(pair_key, 1L, "residual_innovations"),
    message = "Challenge component seeds are not deterministic."
  )
  truth <- setNames(c(1L, 1L, 2L, 2L), paste0("Y", 1:4))
  expect_equal(revision_adjusted_rand_index(truth, truth), 1)
  expect_equal(revision_variation_information(truth, truth), 0)
  if (requireNamespace("igraph", quietly = TRUE)) {
    membership <- setNames(rep(1:2, each = 4), paste0("Y", 1:8))
    oracle <- revision_oracle_structure(membership)
    oracle[1, 2] <- 0.7
    oracle[2, 1] <- 1.0
    estimated <- revision_partition_weight_matrix(oracle, "louvain", 1)
    expect_true(length(unique(estimated)) == 2L, "Oracle block partition failed.")

    singleton <- revision_partition_weight_matrix(matrix(0, 3, 3), "louvain", 1)
    expect_true(length(unique(singleton)) == 3L, "Singleton handling failed.")
  }

  checkpoint <- tempfile("revision-checkpoint-", fileext = ".csv")
  on.exit(unlink(checkpoint), add = TRUE)
  rows <- data.frame(unit_key = c("a", "b"), status = c("ok", "timeout"))
  revision_append_checkpoint(rows, checkpoint)
  expect_equal(revision_read_checkpoint(checkpoint), rows)

  reordered <- data.frame(
    status = "error",
    unit_key = "c",
    stringsAsFactors = FALSE
  )
  revision_append_checkpoint(reordered, checkpoint)
  aligned <- revision_read_checkpoint(checkpoint)
  expect_equal(
    aligned$unit_key,
    c("a", "b", "c"),
    message = "Checkpoint append did not follow the existing column order."
  )
  expect_equal(
    aligned$status,
    c("ok", "timeout", "error"),
    message = "Checkpoint append shifted values across columns."
  )
  expect_error(
    revision_append_checkpoint(
      data.frame(unit_key = "d", unexpected = "value"),
      checkpoint
    ),
    "Checkpoint append schema differs from the existing header",
    "Checkpoint append accepted missing and extra columns."
  )
  duplicated <- data.frame(unit_key = "d", status = "ok")
  names(duplicated) <- c("unit_key", "unit_key")
  expect_error(
    revision_append_checkpoint(duplicated, checkpoint),
    "Checkpoint append schema contains duplicate column names",
    "Checkpoint append accepted duplicate columns."
  )
}

test_identity_and_manifest_contracts <- function() {
  algorithms <- REVISION_COMMUNITY_ALGORITHMS
  fit_key <- "representation|dynEGA"
  rows <- data.frame(
    unit_key = paste(fit_key, algorithms, sep = "|"),
    panel_key = rep("panel", length(algorithms)),
    representation_key = rep("representation", length(algorithms)),
    structure_fit_key = rep(fit_key, length(algorithms)),
    structure_fit_status = rep("ok", length(algorithms)),
    structure_hash = rep(paste(rep("a", 64L), collapse = ""), length(algorithms)),
    community_algorithm = algorithms,
    stringsAsFactors = FALSE
  )
  identity <- list(
    unit_keys = rows$unit_key,
    base_panel_keys = "panel",
    representation_keys = "representation",
    structure_fit_keys = fit_key
  )
  observed <- revision_validate_completion_rows(rows, identity)
  expect_equal(observed$observed_unique_units, 3L)
  expect_equal(observed$observed_unique_base_panels, 1L)
  expect_equal(observed$observed_unique_representation_panels, 1L)
  expect_equal(observed$observed_unique_structure_fits, 1L)
  substituted <- rows
  substituted$unit_key[[1L]] <- "substituted"
  expect_error(
    revision_validate_completion_rows(substituted, identity),
    "unit_key mismatch",
    "Completion accepted a substituted unit key."
  )

  config <- revision_read_config(
    file.path(test_root, "scripts", "simulations", "revision_jobs.json")
  )$value
  audit_job <- revision_config_job(config, "t100_reuse_audit")
  inputs <- revision_required_inputs(
    "t100_reuse_audit",
    audit_job,
    list(inputs = list())
  )
  expect_equal(
    length(inputs$external_artifacts),
    6L,
    message = "Reuse manifest omits declared external artifacts."
  )

  empirical_fixture <- data.frame(
    analysis_scope = rep("empirical_cv", 4L),
    summary_level = rep("heldout_clip_cluster", 4L),
    contribution = c(
      "signed_scaled_median_difference",
      "absolute_scaled_median_difference",
      "scaled_wasserstein_distance",
      "max_discrepancy"
    ),
    stringsAsFactors = FALSE
  )
  selected <- revision_v2_select(empirical_fixture, list(
    analysis_scope = "empirical_cv",
    summary_level = "heldout_clip_cluster",
    contribution = "max_discrepancy"
  ))
  expect_equal(nrow(selected), 1L)
  expect_equal(selected$contribution, "max_discrepancy")
}

test_revision_v2_artifact_builder <- function() {
  config <- revision_read_config(
    file.path(test_root, "scripts", "simulations", "revision_jobs.json")
  )$value
  rebuild_job <- revision_config_job(config, "rebuild_figures_tables")
  expect_true(
    isTRUE(revision_validate_publication_config("rebuild_figures_tables", rebuild_job)),
    "Publication schema revision contract rejected the configured revision."
  )
  invalid_rebuild <- rebuild_job
  invalid_rebuild$parameters$publication_schema_revision <- 1
  expect_error(
    revision_validate_publication_config("rebuild_figures_tables", invalid_rebuild),
    "publication_schema_revision = 4",
    "Publication schema revision contract accepted a stale revision."
  )
  expect_true(
    isTRUE(revision_v2_run_fixture_tests()),
    "Revision-v2 artifact fixture tests failed."
  )
  root <- tempfile("revision-publication-transaction-")
  dir.create(file.path(root, "source"), recursive = TRUE)
  dir.create(file.path(root, "destination"), recursive = TRUE)
  sources <- file.path(root, "source", c("first.txt", "second.txt"))
  destinations <- file.path(
    root,
    "destination",
    c("first.txt", "second.txt")
  )
  writeLines("new first", sources[[1L]])
  writeLines("new second", sources[[2L]])
  writeLines("old first", destinations[[1L]])
  writeLines("old second", destinations[[2L]])
  revision_publish_hashed_artifacts(sources, destinations)
  expect_equal(
    vapply(destinations, readLines, character(1)),
    c("new first", "new second"),
    message = "Publication transaction did not install the rebuilt artifacts."
  )
  leftovers <- list.files(root, recursive = TRUE, all.files = TRUE)
  expect_true(
    !any(grepl("revision-v2-.*\\.(new|previous)$", leftovers)),
    "Publication transaction left staging or backup files behind."
  )
  unlink(root, recursive = TRUE, force = TRUE)
}

test_aggregate_builder_fixture <- function() {
  environment_name <- "REVISION_RESULTS_NO_MAIN"
  previous <- Sys.getenv(environment_name, unset = NA_character_)
  Sys.setenv(REVISION_RESULTS_NO_MAIN = "1")
  on.exit({
    if (is.na(previous)) {
      Sys.unsetenv(environment_name)
    } else {
      do.call(Sys.setenv, setNames(list(previous), environment_name))
    }
  }, add = TRUE)
  aggregate_environment <- new.env(parent = globalenv())
  source(
    file.path(test_root, "scripts", "simulations", "build_revision_results.R"),
    local = aggregate_environment
  )
  expect_true(
    isTRUE(aggregate_environment$rr_run_fixture_tests(reps = 20L)),
    "Aggregate result-builder fixture tests failed."
  )
  scoped <- new.env(parent = aggregate_environment)
  scoped$rr_main <- aggregate_environment$rr_main
  environment(scoped$rr_main) <- scoped
  scoped$commandArgs <- function(trailingOnly = FALSE) c("--study2-only", "--dry-run")
  scoped$rr_find_repo_root <- function() test_root
  plan <- aggregate_environment$rr_parse_json_text(
    paste(capture.output(suppressMessages(scoped$rr_main())), collapse = "\n"),
    "Study 2 dry run"
  )
  expect_true(
    !"empirical_cv" %in% unlist(plan$required_upstream_jobs) &&
      !any(grepl("^empirical_", unlist(plan$estimand_ids))),
    "Study 2 aggregation still requires empirical inputs or estimands."
  )
}

cli <- revision_parse_cli(c("--job", "aggregate_results", "--study2-only", "--resume"))
expect_true(cli$study2_only && cli$resume &&
  identical(cli$config, "scripts/simulations/revision_jobs.json"),
  "Direct reproduction CLI did not resolve its defaults and flags.")
expect_error(revision_parse_cli(c("--job", "t100_dynega", "--study2-only")),
  "--study2-only applies to aggregate_results", "Study 2 scope accepted a fitting job.")

test_transition_and_stability()
test_observation_helpers()
test_dynega_time_scale_contract()
test_empirical_dynega_parallel_contract()
test_empirical_psock_evaluation()
test_grids_and_folds()
test_partition_and_checkpoint_helpers()
test_identity_and_manifest_contracts()
test_revision_v2_artifact_builder()
test_aggregate_builder_fixture()
cat("revision specification pure tests: PASS\n")
