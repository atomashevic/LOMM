graphicalvar_centered_error_moments <- function(error_variance, time_points) {
  if (length(error_variance) != 1L || !is.finite(error_variance) || error_variance <= 0) {
    stop("error_variance must be a positive finite scalar.", call. = FALSE)
  }
  if (
    length(time_points) != 1L || !is.finite(time_points) ||
      time_points < 2 || time_points != as.integer(time_points)
  ) {
    stop("time_points must be an integer of at least 2.", call. = FALSE)
  }
  time_points <- as.integer(time_points)
  list(
    contemporaneous_variance = error_variance * (1 - 1 / time_points),
    lag1_covariance = -error_variance / time_points
  )
}

graphicalvar_centered_block_moments <- function(
  loading,
  variance_previous,
  variance_current,
  lag1_covariance,
  error_variance,
  time_points,
  indicators
) {
  scalar_values <- c(loading, variance_previous, variance_current, lag1_covariance)
  if (any(!is.finite(scalar_values)) || variance_previous < 0 || variance_current < 0) {
    stop("Loading and latent moments must be finite, with nonnegative variances.", call. = FALSE)
  }
  if (
    length(indicators) != 1L || !is.finite(indicators) ||
      indicators < 1 || indicators != as.integer(indicators)
  ) {
    stop("indicators must be a positive integer.", call. = FALSE)
  }
  indicators <- as.integer(indicators)
  error_moments <- graphicalvar_centered_error_moments(error_variance, time_points)
  identity_matrix <- diag(indicators)
  common_matrix <- matrix(1, nrow = indicators, ncol = indicators)
  b_previous <- error_moments$contemporaneous_variance
  b_current <- error_moments$contemporaneous_variance
  d <- error_moments$lag1_covariance
  a_previous <- loading^2 * variance_previous
  a_current <- loading^2 * variance_current
  c_lag <- loading^2 * lag1_covariance

  previous_covariance <- a_previous * common_matrix + b_previous * identity_matrix
  lag_covariance <- c_lag * common_matrix + d * identity_matrix
  current_covariance <- a_current * common_matrix + b_current * identity_matrix

  residual_orthogonal <- b_current - d^2 / b_previous
  residual_common <- b_current + indicators * a_current -
    (d + indicators * c_lag)^2 / (b_previous + indicators * a_previous)
  residual_covariance <- residual_orthogonal * identity_matrix +
    (residual_common - residual_orthogonal) / indicators * common_matrix

  list(
    previous_covariance = previous_covariance,
    lag_covariance = lag_covariance,
    current_covariance = current_covariance,
    residual_covariance = residual_covariance,
    residual_orthogonal = residual_orthogonal,
    residual_common = residual_common,
    centered_error_variance = b_previous,
    centered_error_lag1_covariance = d
  )
}
