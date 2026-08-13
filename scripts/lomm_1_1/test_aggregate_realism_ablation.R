#!/usr/bin/env Rscript

test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot locate paired-ablation tests.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

TEST_PATH <- test_script_path()
TEST_DIRECTORY <- dirname(TEST_PATH)
LOMM11_PAIRED_AGGREGATOR_PATH <- file.path(
  TEST_DIRECTORY, "aggregate_realism_ablation.R"
)
source(LOMM11_PAIRED_AGGREGATOR_PATH)

test_wasserstein <- function(first, second) {
  first <- sort(first[is.finite(first)])
  second <- sort(second[is.finite(second)])
  support <- sort(unique(c(first, second)))
  if (length(support) < 2L) return(0)
  left <- support[-length(support)]
  sum(abs(
    findInterval(left, first) / length(first) -
      findInterval(left, second) / length(second)
  ) * diff(support))
}

test_expanded_discrepancy <- function(empirical, synthetic, clip_index, counts, scale) {
  expanded <- rep(seq_along(clip_index), counts[clip_index + 1L])
  empirical_values <- empirical[expanded]
  synthetic_values <- synthetic[expanded]
  empirical_values <- empirical_values[is.finite(empirical_values)]
  synthetic_values <- synthetic_values[is.finite(synthetic_values)]
  max(
    abs(stats::median(synthetic_values) - stats::median(empirical_values)),
    test_wasserstein(empirical_values, synthetic_values)
  ) / scale
}

test_contrasts <- function() {
  spec <- pa_validate_contrast_spec()
  bits <- do.call(rbind, strsplit(LOMM11_PAIRED_VARIANTS, "", fixed = TRUE))
  storage.mode(bits) <- "integer"
  B <- bits[, 1L]
  S <- bits[, 2L]
  G <- bits[, 3L]
  values <- 19 + 2 * B + 3 * S + 5 * G + 7 * B * S + 11 * B * G +
    13 * S * G + 17 * B * S * G
  observed <- as.numeric(spec$coefficients %*% values)
  expected <- c(
    2 + 7 / 2 + 11 / 2 + 17 / 4,
    3 + 7 / 2 + 13 / 2 + 17 / 4,
    5 + 11 / 2 + 13 / 2 + 17 / 4,
    7 + 17 / 2,
    11 + 17 / 2,
    13 + 17 / 2,
    17,
    2 + 3 + 5 + 7 + 11 + 13 + 17
  )
  stopifnot(isTRUE(all.equal(observed, expected, tolerance = 1e-14)))
}

test_bootstrap_design <- function() {
  set.seed(71)
  before <- .Random.seed
  first <- pa_bootstrap_design(25L, 771L)
  stopifnot(identical(before, .Random.seed))
  second <- pa_bootstrap_design(25L, 771L)
  stopifnot(
    identical(first, second), length(first) == 5L,
    all(vapply(first, nrow, integer(1)) == 26L),
    all(vapply(first, ncol, integer(1)) == 20L),
    all(vapply(first, function(value) all(value[1L, ] == 1L), logical(1))),
    all(vapply(first, function(value) all(rowSums(value) == 20L), logical(1)))
  )
}

test_quantile_contract <- function() {
  values <- c(-3.2, -1.1, 0.4, 0.8, 2.7, 4.9, 7.3, 11.2)
  for (probability in c(0.025, 0.5, 0.975)) {
    expected <- stats::quantile(
      values, probability, names = FALSE, type = 8
    )
    stopifnot(identical(pa_quantile(values, probability), expected))
  }
}

test_interval_summary <- function() {
  draws <- setNames(c(7, 8, 9, 10, 12, 14), paste0("draw", seq_len(6L)))
  estimate <- c(contrast = 9.5)
  observed <- pa_interval_summary(estimate, draws)
  expected_mean <- mean(unname(draws))
  stopifnot(
    identical(names(observed), c(
      "bootstrap_se", "bootstrap_mean", "bootstrap_bias", "basic_conf_low",
      "basic_conf_high", "percentile_conf_low", "percentile_conf_high"
    )),
    isTRUE(all.equal(observed[["bootstrap_mean"]], expected_mean)),
    isTRUE(all.equal(
      observed[["bootstrap_bias"]], expected_mean - unname(estimate)
    )),
    isTRUE(all.equal(
      observed[["basic_conf_low"]],
      2 * unname(estimate) - observed[["percentile_conf_high"]]
    )),
    isTRUE(all.equal(
      observed[["basic_conf_high"]],
      2 * unname(estimate) - observed[["percentile_conf_low"]]
    ))
  )
}

test_profile_labels <- function() {
  stopifnot(
    identical(names(LOMM11_PROFILE_LABELS), LOMM11_PAIRED_VARIANTS),
    !any(unname(LOMM11_PROFILE_LABELS) %in% LOMM11_PAIRED_VARIANTS)
  )
}

test_cpp_fixture <- function() {
  pa_compile_engine(2L)
  empirical <- c(-1, -0.5, 0, 0.5, 1, 1.5, NA_real_, 2.5)
  clip_index <- rep(0:3, each = 2L)
  variants <- 8L
  reps <- 2L
  panel_count <- variants * reps
  synthetic <- vapply(seq_len(panel_count), function(panel) {
    variant <- (panel - 1L) %/% reps
    replicate <- (panel - 1L) %% reps
    empirical + variant / 10 + c(-0.2, 0.1) * (replicate + 1L)
  }, numeric(length(empirical)))
  synthetic[7L, seq(2L, panel_count, by = 3L)] <- NA_real_
  variant_index <- rep(0:7, each = reps)
  draws <- rbind(
    c(1L, 1L, 1L, 1L),
    c(2L, 0L, 1L, 1L),
    c(0L, 2L, 2L, 0L),
    c(1L, 0L, 0L, 3L)
  )
  scale <- 1.7
  observed <- lomm11_bootstrap_metric_group_cpp(
    empirical, synthetic, clip_index, variant_index, draws, scale, variants
  )
  expected <- matrix(NA_real_, nrow(draws), variants)
  for (draw in seq_len(nrow(draws))) {
    for (variant in seq_len(variants)) {
      panels <- which(variant_index == variant - 1L)
      expected[draw, variant] <- mean(vapply(panels, function(panel) {
        test_expanded_discrepancy(
          empirical, synthetic[, panel], clip_index, draws[draw, ], scale
        )
      }, numeric(1)))
    }
  }
  stopifnot(isTRUE(all.equal(observed, expected, tolerance = 2e-13)))
  invalid <- try(lomm11_bootstrap_metric_group_cpp(
    empirical, synthetic, clip_index, variant_index,
    rbind(c(1L, 1L, 1L, 0L)), scale, variants
  ), silent = TRUE)
  stopifnot(inherits(invalid, "try-error"))
}

test_cli_contract <- function() {
  parsed <- pa_parse_args(c("--source-run-dir", "/tmp/source", "--execute", "--workers", "2"))
  stopifnot(
    identical(parsed$source_run_dir, "/tmp/source"),
    identical(parsed$mode, "execute"), identical(parsed$workers, "2")
  )
  bad <- try(pa_parse_args(c("--unknown")), silent = TRUE)
  stopifnot(inherits(bad, "try-error"))
}

test_published_output <- function(output_dir) {
  validated <- pa_validate_published(output_dir)
  primary <- validated$profiles[
    validated$profiles$analysis_view == "complete_case", , drop = FALSE
  ]
  stopifnot(
    nrow(primary) == 16L,
    length(unique(primary$profile_id)) == 8L,
    length(unique(primary$family)) == 2L,
    all(primary$basic_conf_low <= primary$basic_conf_high),
    all(primary$percentile_conf_low <= primary$percentile_conf_high)
  )
  baseline_scale <- primary[primary$profile_id == "110", , drop = FALSE]
  expected <- c(probability = 0.2217709, logratio = 0.2915386)
  observed <- setNames(baseline_scale$estimate, baseline_scale$family)
  stopifnot(isTRUE(all.equal(
    unname(observed[names(expected)]), unname(expected), tolerance = 5e-7
  )))
  selected_metrics <- validated$profile_metrics[
    validated$profile_metrics$profile_id == "110", , drop = FALSE
  ]
  stopifnot(
    nrow(selected_metrics) == 32L,
    length(unique(selected_metrics$analysis_view)) == 2L,
    length(unique(selected_metrics$family)) == 2L,
    length(unique(selected_metrics$metric)) == 8L,
    all(selected_metrics$empirical_units_min > 0L),
    all(selected_metrics$empirical_units_min <= selected_metrics$empirical_units_max),
    all(selected_metrics$synthetic_units_min <= selected_metrics$synthetic_units_max)
  )
}

test_args <- commandArgs(trailingOnly = TRUE)
if (length(test_args) > 0L &&
    (length(test_args) != 2L || test_args[[1L]] != "--output-dir")) {
  stop("Usage: test_aggregate_realism_ablation.R [--output-dir DIR]", call. = FALSE)
}

pa_require_packages()
test_contrasts()
test_bootstrap_design()
test_quantile_contract()
test_interval_summary()
test_profile_labels()
test_cpp_fixture()
test_cli_contract()
if (length(test_args) == 2L) test_published_output(test_args[[2L]])
cat("LOMM 1.1 paired-ablation aggregation tests: PASS\n")
