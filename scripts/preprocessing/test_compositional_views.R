#!/usr/bin/env Rscript

lomm_test_stop <- function(message) {
  stop(message, call. = FALSE)
}

lomm_expect_true <- function(value, message) {
  if (!isTRUE(value)) {
    lomm_test_stop(message)
  }
}

lomm_expect_equal <- function(actual, expected, tolerance = 1e-10, message = "Values differ.") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    lomm_test_stop(message)
  }
}

lomm_expect_error <- function(code, pattern, message) {
  error <- tryCatch({
    force(code)
    NULL
  }, error = function(condition) condition)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    lomm_test_stop(message)
  }
}

lomm_test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
}

LOMM_TEST_DIRECTORY <- dirname(lomm_test_script_path())
source(file.path(LOMM_TEST_DIRECTORY, "compositional_math.R"))
source(file.path(LOMM_TEST_DIRECTORY, "compositional_views.R"))
source(file.path(LOMM_TEST_DIRECTORY, "empirical_compositional_bundle.R"))
LOMM_BUILDER_PATH <- file.path(
  LOMM_TEST_DIRECTORY, "build_empirical_compositional_views.R"
)
source(file.path(LOMM_TEST_DIRECTORY, "validate_published_bundle.R"))

lomm_test_simplex_and_zero_policy <- function() {
  drifted <- matrix(c(0.2, 0.3, 0.5000001), nrow = 1L)
  colnames(drifted) <- c("a", "b", "c")
  closed <- lomm_close_probabilities(drifted)
  lomm_expect_equal(rowSums(closed$probabilities), 1, tolerance = 1e-14)
  lomm_expect_true(closed$flags$renormalized, "Closure adjustment was not recorded.")
  tiny_drift <- matrix(c(0.2, 0.3, 0.500000001), nrow = 1L)
  colnames(tiny_drift) <- c("a", "b", "c")
  lomm_expect_true(
    lomm_close_probabilities(tiny_drift)$flags$renormalized,
    "Small closure adjustment was not recorded."
  )

  missing <- rbind(c(NA, NA, NA), c(0.2, NA, 0.8))
  colnames(missing) <- c("a", "b", "c")
  prepared_missing <- lomm_close_probabilities(missing)
  lomm_expect_equal(
    prepared_missing$flags$missingness_class,
    c("all_missing", "partial_missing")
  )
  lomm_expect_true(
    all(is.na(prepared_missing$probabilities)),
    "Incomplete rows were not treated as whole missing compositions."
  )

  zero <- matrix(c(0, 0.3, 0.7), nrow = 1L, dimnames = list(NULL, c("a", "b", "c")))
  lomm_expect_error(
    lomm_close_probabilities(zero),
    "Zero probabilities require",
    "Default zero policy accepted a zero."
  )
  replaced <- lomm_close_probabilities(
    zero, zero_policy = "multiplicative", zero_replacement = 0.01
  )
  lomm_expect_equal(replaced$probabilities, matrix(c(0.01, 0.297, 0.693), nrow = 1L))
  lomm_expect_equal(
    replaced$probabilities[1, 2] / replaced$probabilities[1, 3],
    0.3 / 0.7
  )
  lomm_expect_true(replaced$flags$zero_replaced, "Zero replacement flag is false.")

  invalid <- list(
    matrix(c(-0.1, 0.4, 0.7), nrow = 1L),
    matrix(c(1.1, 0.1, -0.2), nrow = 1L),
    matrix(c(0.2, 0.2, 0.2), nrow = 1L),
    matrix(c(Inf, 0.4, 0.6), nrow = 1L),
    matrix(c(NaN, 0.4, 0.6), nrow = 1L),
    matrix(c(0, 0, 0), nrow = 1L)
  )
  for (value in invalid) {
    colnames(value) <- c("a", "b", "c")
    lomm_expect_error(
      lomm_close_probabilities(value),
      "",
      "Invalid probability input did not fail."
    )
  }
  partial_invalid <- matrix(c(-5, NA, 6), nrow = 1L)
  colnames(partial_invalid) <- c("a", "b", "c")
  lomm_expect_error(
    lomm_close_probabilities(partial_invalid),
    "inside [0, 1]",
    "Invalid observed values in a partial row were masked as missing."
  )
}

lomm_test_logratio_roundtrips <- function() {
  probabilities <- rbind(c(0.2, 0.3, 0.5), c(0.6, 0.1, 0.3))
  colnames(probabilities) <- c("a", "b", "c")
  basis <- lomm_ilr_basis(colnames(probabilities))
  expected_basis <- matrix(c(
    1 / sqrt(2), 1 / sqrt(6),
    -1 / sqrt(2), 1 / sqrt(6),
    0, -2 / sqrt(6)
  ), nrow = 3L, byrow = TRUE)
  lomm_expect_equal(basis, expected_basis, tolerance = 1e-12)
  lomm_expect_equal(colSums(basis), c(0, 0), tolerance = 1e-12)
  lomm_expect_equal(crossprod(basis), diag(2L), tolerance = 1e-12)

  clr_values <- lomm_clr(probabilities)
  ilr_values <- lomm_ilr(probabilities, basis)
  lomm_expect_equal(rowSums(clr_values), c(0, 0), tolerance = 1e-12)
  lomm_expect_equal(lomm_clr_inverse(clr_values), probabilities, tolerance = 1e-12)
  lomm_expect_equal(lomm_ilr_inverse(ilr_values, basis), probabilities, tolerance = 1e-12)
  lomm_expect_equal(ilr_values %*% t(basis), clr_values, tolerance = 1e-12)

  logits <- matrix(c(-1, 0, 2, 4, -2, 1), nrow = 2L, byrow = TRUE)
  colnames(logits) <- colnames(probabilities)
  softmax <- lomm_softmax_rows(logits)
  lomm_expect_equal(lomm_clr(softmax), logits - rowMeans(logits), tolerance = 1e-12)

  wrong_basis <- basis
  rownames(wrong_basis) <- rev(rownames(wrong_basis))
  lomm_expect_error(
    lomm_ilr(probabilities, wrong_basis),
    "locked component order",
    "ILR accepted a basis with mismatched channel names."
  )
}

lomm_test_segments <- function() {
  included <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  segments <- lomm_segment_metadata(rep("clip", 7L), seq_len(7L), included)
  lomm_expect_equal(
    segments$segment_id,
    c("clip::seg001", "clip::seg001", NA, NA, "clip::seg002", NA, "clip::seg003")
  )
  lomm_expect_equal(segments$segment_position, c(1L, 2L, NA, NA, 1L, NA, 1L))
  lomm_expect_equal(segments$segment_length, c(2L, 2L, NA, NA, 1L, NA, 1L))

  cadence <- lomm_segment_metadata(rep("clip", 3L), c(1, 2, 4), rep(TRUE, 3L))
  lomm_expect_equal(
    cadence$segment_id,
    c("clip::seg001", "clip::seg001", "clip::seg002")
  )
  fractional <- lomm_segment_metadata(
    rep("clip", 3L), c(0.1, 0.2, 0.3), rep(TRUE, 3L), expected_step = 0.1
  )
  lomm_expect_true(
    length(unique(fractional$segment_id)) == 1L,
    "Floating-point cadence created a false segment break."
  )
  lomm_expect_error(
    lomm_segment_metadata(rep("clip", 2L), c(1, 1), rep(TRUE, 2L)),
    "must be unique",
    "Duplicate frame indices did not fail."
  )
}

lomm_fixture_panel <- function(values, clip = "clip", frames = seq_len(nrow(values))) {
  values <- as.data.frame(values, stringsAsFactors = FALSE)
  names(values) <- c("a", "b", "c")
  values$clip <- clip
  values$frame_index <- frames
  values
}

lomm_test_ilr_interpolation <- function() {
  left <- c(0.6, 0.3, 0.1)
  right <- c(0.1, 0.3, 0.6)
  panel <- lomm_fixture_panel(rbind(left, c(NA, NA, NA), right))
  result <- lomm_prepare_compositional_panel(
    panel, c("a", "b", "c"), max_gap = 1L
  )
  expected <- sqrt(left * right)
  expected <- expected / sum(expected)
  lomm_expect_true(result$imputation_flags$ilr_imputed[[2L]], "Interior gap was not imputed.")
  lomm_expect_equal(result$probabilities[2L, ], expected, tolerance = 1e-12)
  lomm_expect_equal(
    result$ilr[2L, ], (result$ilr[1L, ] + result$ilr[3L, ]) / 2,
    tolerance = 1e-12
  )
  lomm_expect_equal(result$probabilities[c(1L, 3L), ], rbind(left, right))
  lomm_expect_equal(
    result$source_segments$segment_id,
    c("clip::seg001", NA, "clip::seg002")
  )
  lomm_expect_true(
    length(unique(stats::na.omit(result$analysis_segments$segment_id))) == 1L,
    "Imputed gap did not join the sensitivity segment."
  )

  partial <- lomm_fixture_panel(rbind(left, c(NA, 0.2, 0.8), right))
  partial_result <- lomm_prepare_compositional_panel(
    partial, c("a", "b", "c"), max_gap = 1L
  )
  lomm_expect_true(
    partial_result$imputation_flags$ilr_imputed[[2L]],
    "Partial composition was not imputed jointly."
  )

  long_gap <- lomm_fixture_panel(rbind(
    left, c(NA, NA, NA), c(NA, NA, NA), c(NA, NA, NA), right
  ))
  unresolved <- lomm_prepare_compositional_panel(
    long_gap, c("a", "b", "c"), max_gap = 2L
  )
  lomm_expect_true(
    all(unresolved$imputation_flags$unresolved_missing[2:4]),
    "Over-limit gap was imputed."
  )

  edge_gap <- lomm_fixture_panel(rbind(c(NA, NA, NA), left, right, c(NA, NA, NA)))
  edge_result <- lomm_prepare_compositional_panel(
    edge_gap, c("a", "b", "c"), max_gap = 2L
  )
  lomm_expect_true(
    all(edge_result$imputation_flags$unresolved_missing[c(1L, 4L)]),
    "Leading or trailing gap was extrapolated."
  )

  cadence_gap <- lomm_fixture_panel(
    rbind(left, c(NA, NA, NA), right), frames = c(1, 2, 4)
  )
  cadence_result <- lomm_prepare_compositional_panel(
    cadence_gap, c("a", "b", "c"), max_gap = 1L
  )
  lomm_expect_true(
    cadence_result$imputation_flags$unresolved_missing[[2L]],
    "Nonconsecutive frame interval was bridged."
  )
  lomm_expect_error(
    lomm_prepare_compositional_panel(
      panel, c("a", "b", "c"), max_gap = 1.9
    ),
    "non-negative integer",
    "Fractional maximum gap was silently truncated."
  )
}

lomm_test_empirical_key_and_tie_rules <- function() {
  keys <- lomm_frame_key(c("clip", "clip"), c(1.00000001, 1.00000002))
  lomm_expect_true(length(unique(keys)) == 2L, "Frame-key formatting collapsed distinct times.")

  scores <- matrix(0, nrow = 1L, ncol = length(LOMM_EMPIRICAL_SCORE_COLUMNS))
  colnames(scores) <- LOMM_EMPIRICAL_SCORE_COLUMNS
  scores[1, 1:2] <- 0.5
  fixture <- as.data.frame(scores)
  fixture$frame_index <- 1
  fixture$clip <- "clip"
  fixture$truth_label <- "anger"
  fixture$sample_order <- 1
  fixture$model <- "model"
  fixture$predicted_label <- "anger"
  for (seed in seq_len(20L)) {
    set.seed(seed)
    lomm_expect_true(
      isTRUE(lomm_validate_empirical_source(fixture)),
      "Tie handling in predicted-label validation was not deterministic."
    )
  }
}

lomm_test_no_estimator_dependency <- function() {
  paths <- file.path(LOMM_TEST_DIRECTORY, c(
    "compositional_math.R", "compositional_views.R",
    "empirical_compositional_bundle.R", "build_empirical_compositional_views.R"
  ))
  text <- paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")
  forbidden <- c("qlogis(", "dynEGA(", "graphicalVAR(", "gimme(", "EGA(")
  for (token in forbidden) {
    lomm_expect_true(
      !grepl(token, text, fixed = TRUE),
      paste("Preprocessing source contains forbidden estimator or qlogis call:", token)
    )
  }
}

lomm_test_rebuild_numeric_comparisons <- function() {
  lomm_expect_true(
    !lomm_validation_numeric_column_matches(6 + 5e-12, 6, "source_row"),
    "Discrete numeric changes were accepted by the rebuild check."
  )
  lomm_expect_true(
    lomm_validation_numeric_column_matches(2e-5 + 1e-16, 2e-5, "anger"),
    "Negligible absolute serialization error was rejected."
  )
  lomm_expect_true(
    !lomm_validation_numeric_column_matches(1 + 2e-12, 1, "anger"),
    "Continuous error above the absolute tolerance was accepted."
  )
  lomm_expect_true(
    !lomm_validation_numeric_column_matches(c(1, NA), c(1, 2), "anger"),
    "A changed numeric missingness mask was accepted."
  )
}

lomm_test_builder_cli_guards <- function() {
  repository_root <- normalizePath(file.path(LOMM_TEST_DIRECTORY, "..", ".."))
  builder <- file.path(LOMM_TEST_DIRECTORY, "build_empirical_compositional_views.R")
  original_directory <- getwd()
  on.exit(setwd(original_directory), add = TRUE)
  setwd(repository_root)

  forbidden_roots <- c(
    "simulation_outputs/revision_v2/adversarial",
    "./simulation_outputs/revision_v2/adversarial",
    "scripts/../simulation_outputs/revision_v2/adversarial"
  )
  for (output_root in forbidden_roots) {
    result <- suppressWarnings(system2(
      "Rscript",
      c(
        builder, "--input", "missing-input.csv", "--output-root",
        output_root, "--dry-run"
      ),
      stdout = TRUE,
      stderr = TRUE
    ))
    lomm_expect_true(
      identical(attr(result, "status"), 1L) &&
        any(grepl("cannot publish under revision_v2", result, fixed = TRUE)),
      paste("Builder accepted a revision_v2 output-root alias:", output_root)
    )
  }

  malformed <- list(
    c(builder, "--input", "missing-input.csv", "--output-root", "--dry-run"),
    c(builder, "--input", "--dry-run")
  )
  for (arguments in malformed) {
    result <- suppressWarnings(system2(
      "Rscript", arguments, stdout = TRUE, stderr = TRUE
    ))
    lomm_expect_true(
      identical(attr(result, "status"), 1L) &&
        any(grepl("Missing value after", result, fixed = TRUE)),
      "Builder accepted a flag in place of a required argument value."
    )
  }
}

lomm_test_simplex_and_zero_policy()
lomm_test_logratio_roundtrips()
lomm_test_segments()
lomm_test_ilr_interpolation()
lomm_test_empirical_key_and_tie_rules()
lomm_test_no_estimator_dependency()
lomm_test_rebuild_numeric_comparisons()
lomm_test_builder_cli_guards()
cat("LOMM compositional preprocessing tests: PASS\n")
