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

full_residual <- graphicalvar_schur_residual(
  analytic$previous_covariance,
  analytic$current_covariance,
  analytic$lag_covariance
)
centering_expect_equal(
  full_residual,
  analytic$residual_covariance,
  tolerance = 1e-12,
  message = "Full-matrix Schur residual disagrees with the equal-block formula."
)

orthogonal <- graphicalvar_orthogonal_residual_variance(error_variance, time_points)
centering_expect_equal(
  orthogonal,
  error_variance * (time_points - 2) / (time_points - 1),
  message = "Contrast-subspace residual variance r_perp is incorrect."
)
centering_expect_equal(
  graphicalvar_orthogonal_residual_variance(error_variance, 100L),
  error_variance * 98 / 99,
  message = "T=100 r_perp must equal psi (T-2)/(T-1)."
)

error_matrices <- graphicalvar_centered_error_matrices(
  diag(error_variance, indicators),
  time_points
)
centering_expect_equal(
  diag(error_matrices$contemporaneous),
  rep(error_variance * (1 - 1 / time_points), indicators),
  message = "Full-matrix centered error variance is incorrect."
)
centering_expect_equal(
  diag(error_matrices$lag1),
  rep(-error_variance / time_points, indicators),
  message = "Full-matrix centered lag-1 error covariance is incorrect."
)

cat("GraphicalVAR finite-clip centering covariance tests: PASS\n")
