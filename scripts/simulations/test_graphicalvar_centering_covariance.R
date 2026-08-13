#!/usr/bin/env Rscript

centering_test_stop <- function(message) {
  stop(message, call. = FALSE)
}

centering_expect_true <- function(value, message) {
  if (!isTRUE(value)) {
    centering_test_stop(message)
  }
}

centering_expect_equal <- function(actual, expected, tolerance = 1e-10, message = "Values differ.") {
  equal <- isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))
  if (!equal) {
    centering_test_stop(message)
  }
}

centering_test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
}

centering_test_root <- normalizePath(
  file.path(dirname(centering_test_script_path()), "..", ".."),
  mustWork = TRUE
)
source(
  file.path(
    centering_test_root,
    "scripts",
    "simulations",
    "graphicalvar_centering_covariance.R"
  )
)

time_points <- 12L
indicators <- 4L
clip_count <- 50000L
error_variance <- 0.25^2
loading <- 0.6
set.seed(314159L)

latent <- matrix(stats::rnorm(clip_count * time_points), nrow = clip_count)
latent <- latent - rowMeans(latent)
errors <- array(
  stats::rnorm(clip_count * time_points * indicators, sd = sqrt(error_variance)),
  dim = c(clip_count, time_points, indicators)
)

pair_count <- clip_count * (time_points - 1L)
previous <- matrix(NA_real_, nrow = pair_count, ncol = indicators)
current <- matrix(NA_real_, nrow = pair_count, ncol = indicators)
for (indicator in seq_len(indicators)) {
  observed <- loading * latent + errors[, , indicator]
  observed <- observed - rowMeans(observed)
  previous[, indicator] <- as.vector(observed[, seq_len(time_points - 1L)])
  current[, indicator] <- as.vector(observed[, 2:time_points])
}

latent_previous <- as.vector(latent[, seq_len(time_points - 1L)])
latent_current <- as.vector(latent[, 2:time_points])
analytic <- graphicalvar_centered_block_moments(
  loading = loading,
  variance_previous = mean(latent_previous^2),
  variance_current = mean(latent_current^2),
  lag1_covariance = mean(latent_current * latent_previous),
  error_variance = error_variance,
  time_points = time_points,
  indicators = indicators
)

expected_error_variance <- error_variance * (1 - 1 / time_points)
expected_error_lag <- -error_variance / time_points
centering_expect_equal(
  analytic$centered_error_variance,
  expected_error_variance,
  message = "Finite-clip centered error variance is incorrect."
)
centering_expect_equal(
  analytic$centered_error_lag1_covariance,
  expected_error_lag,
  message = "Finite-clip centered lag covariance is incorrect."
)

empirical_previous <- crossprod(previous) / pair_count
empirical_lag <- crossprod(current, previous) / pair_count
empirical_current <- crossprod(current) / pair_count
centering_expect_true(
  max(abs(empirical_previous - analytic$previous_covariance)) < 0.0015,
  "Monte Carlo previous-state covariance disagrees with the centered formula."
)
centering_expect_true(
  max(abs(empirical_lag - analytic$lag_covariance)) < 0.0015,
  "Monte Carlo lag covariance disagrees with the centered formula."
)
centering_expect_true(
  max(abs(empirical_current - analytic$current_covariance)) < 0.0015,
  "Monte Carlo current-state covariance disagrees with the centered formula."
)

direct_residual <- analytic$current_covariance -
  analytic$lag_covariance %*%
    solve(analytic$previous_covariance, t(analytic$lag_covariance))
centering_expect_equal(
  analytic$residual_covariance,
  direct_residual,
  tolerance = 1e-12,
  message = "Closed-form residual covariance disagrees with direct projection."
)

empirical_residual <- empirical_current -
  empirical_lag %*% solve(empirical_previous, t(empirical_lag))
centering_expect_true(
  max(abs(empirical_residual - analytic$residual_covariance)) < 0.0015,
  "Monte Carlo residual covariance disagrees with the centered formula."
)

cat("GraphicalVAR finite-clip centering covariance tests: PASS\n")
