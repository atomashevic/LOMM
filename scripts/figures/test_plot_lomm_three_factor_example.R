#!/usr/bin/env Rscript

figure_test_stop <- function(message) {
  stop(message, call. = FALSE)
}

figure_expect_true <- function(value, message) {
  if (!isTRUE(value)) {
    figure_test_stop(message)
  }
}

figure_expect_equal <- function(actual, expected, tolerance = 1e-12, message = "Values differ.") {
  equal <- isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))
  if (!equal) {
    figure_test_stop(message)
  }
}

figure_test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
}

figure_test_root <- normalizePath(
  file.path(dirname(figure_test_script_path()), "..", ".."),
  mustWork = TRUE
)
figure_environment <- new.env(parent = globalenv())
sys.source(
  file.path(figure_test_root, "scripts", "figures", "plot_lomm_three_factor_example.R"),
  envir = figure_environment
)

data_first <- figure_environment$lomm_figure1_data(seed = 1729L)
data_repeat <- figure_environment$lomm_figure1_data(seed = 1729L)
data_changed <- figure_environment$lomm_figure1_data(seed = 1730L)

figure_expect_true(identical(data_first, data_repeat), "A fixed seed did not reproduce Figure 1 data.")
figure_expect_equal(nrow(data_first), 2L * 3L * 100L, message = "Figure 1 has an unexpected row count.")
figure_expect_true(all(is.finite(data_first$position)), "Figure 1 contains a non-finite position.")

deterministic_first <- subset(data_first, regime == "Exact transition, no shocks")
deterministic_changed <- subset(data_changed, regime == "Exact transition, no shocks")
stochastic_first <- subset(data_first, regime == "Exact transition with per-step shocks")
stochastic_changed <- subset(data_changed, regime == "Exact transition with per-step shocks")
figure_expect_equal(
  deterministic_first$position,
  deterministic_changed$position,
  message = "The no-shock trajectory depends on the random seed."
)
figure_expect_true(
  !isTRUE(all.equal(stochastic_first$position, stochastic_changed$position)),
  "Changing the seed did not change the stochastic trajectory."
)

parameters <- figure_environment$lomm_figure1_parameters()
exact <- figure_environment$revision_exact_transition(parameters$eta[[1L]], parameters$zeta[[1L]], 0.4)
expected_second_state <- exact$transition %*% matrix(c(0.25, 2), ncol = 1L)
first_factor <- subset(
  deterministic_first,
  factor == parameters$factor_label[[1L]] & observation == 2L
)
figure_expect_equal(
  c(first_factor$position, first_factor$velocity),
  as.numeric(expected_second_state),
  tolerance = 1e-13,
  message = "Figure 1 does not use the production exact transition."
)

set.seed(91L)
expected_rng <- stats::runif(5L)
set.seed(91L)
invisible(figure_environment$lomm_figure1_data(seed = 1729L))
observed_rng <- stats::runif(5L)
figure_expect_equal(observed_rng, expected_rng, message = "Figure generation changed the caller RNG state.")

first_pdf <- tempfile(fileext = ".pdf")
second_pdf <- tempfile(fileext = ".pdf")
on.exit(unlink(c(first_pdf, second_pdf), force = TRUE), add = TRUE)
figure_environment$lomm_figure1_write(first_pdf, seed = 1729L)
figure_environment$lomm_figure1_write(second_pdf, seed = 1729L)
figure_expect_true(
  identical(unname(tools::md5sum(first_pdf)), unname(tools::md5sum(second_pdf))),
  "Repeated Figure 1 rendering produced different PDF bytes."
)

process_first <- figure_environment$lomm_process_data(seed = 1729L)
process_repeat <- figure_environment$lomm_process_data(seed = 1729L)
figure_expect_true(
  identical(process_first, process_repeat),
  "A fixed seed did not reproduce the worked LOMM process data."
)
figure_expect_true(
  identical(
    levels(process_first$panel),
    c("Latent process", "Activity", "Expressed signal", "Observed indicators")
  ),
  "The worked LOMM process panels are incomplete or out of order."
)
figure_expect_true(
  all(is.finite(process_first$value)),
  "The worked LOMM process contains a non-finite value."
)

process_pdf <- tempfile(fileext = ".pdf")
on.exit(unlink(process_pdf, force = TRUE), add = TRUE)
figure_environment$lomm_process_write(process_pdf, seed = 1729L)
figure_expect_true(
  file.exists(process_pdf) && file.info(process_pdf)$size > 0L,
  "The worked LOMM process PDF was not created."
)

cat("LOMM three-factor Figure 1 tests: PASS\n")
