LOMM11_RNG_SUBSTREAMS <- c(
  "between_clip_baseline",
  "clip_scale",
  "gate_schedule",
  "initial_state",
  "innovations",
  "residuals"
)

LOMM11_DEFAULT_CHANNELS <- c(
  "anger", "disgust", "fear", "happiness", "neutral", "sadness",
  "surprise", "contempt", "anxiety", "helplessness", "disappointment"
)

LOMM11_CLIP_SHARED_SEGMENT <- "__clip_shared__"

if (!exists("lomm11_stop", mode = "function", inherits = TRUE)) {
  lomm11_stop <- function(...) {
    stop(..., call. = FALSE)
  }
}

lomm11_assert_scalar <- function(value, label, lower = -Inf, upper = Inf,
                                 lower_open = FALSE, upper_open = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L && is.finite(value)
  if (valid) {
    valid <- if (lower_open) value > lower else value >= lower
    valid <- valid && if (upper_open) value < upper else value <= upper
  }
  if (!valid) {
    lomm11_stop(label, " is outside its permitted finite scalar range.")
  }
  invisible(TRUE)
}

lomm11_require_helper <- function(name) {
  if (!exists(name, mode = "function", inherits = TRUE)) {
    lomm11_stop("Required pure helper is unavailable: ", name, ".")
  }
  get(name, mode = "function", inherits = TRUE)
}

lomm11_period_damping_map <- function(period_frames, damping_ratio) {
  lomm11_assert_scalar(period_frames, "period_frames", lower = 0, lower_open = TRUE)
  lomm11_assert_scalar(
    damping_ratio, "damping_ratio", lower = 0, upper = 1,
    lower_open = TRUE, upper_open = TRUE
  )
  omega <- 2 * pi / period_frames
  eta <- -(omega^2)
  zeta <- -2 * damping_ratio * omega
  roots <- c(
    -damping_ratio * omega + 1i * omega * sqrt(1 - damping_ratio^2),
    -damping_ratio * omega - 1i * omega * sqrt(1 - damping_ratio^2)
  )
  list(
    period_frames = as.numeric(period_frames),
    damping_ratio = as.numeric(damping_ratio),
    omega = omega,
    eta = eta,
    zeta = zeta,
    roots = roots
  )
}

lomm11_solve_discrete_lyapunov <- function(transition, innovation_covariance) {
  transition <- as.matrix(transition)
  innovation_covariance <- as.matrix(innovation_covariance)
  if (!is.numeric(transition) || nrow(transition) != ncol(transition) ||
      !identical(dim(transition), dim(innovation_covariance)) ||
      any(!is.finite(transition)) || any(!is.finite(innovation_covariance))) {
    lomm11_stop("Lyapunov inputs must be aligned finite square numeric matrices.")
  }
  dimension <- nrow(transition)
  operator <- diag(dimension^2) - kronecker(transition, transition)
  covariance <- matrix(
    solve(operator, as.vector(innovation_covariance)),
    nrow = dimension
  )
  covariance <- (covariance + t(covariance)) / 2
  if (any(!is.finite(covariance))) {
    lomm11_stop("The discrete Lyapunov solution is non-finite.")
  }
  covariance
}

lomm11_build_unit_position_system <- function(period_frames, damping_ratio,
                                              dt = 1, tolerance = 1e-10) {
  lomm11_assert_scalar(dt, "dt", lower = 0, lower_open = TRUE)
  lomm11_assert_scalar(tolerance, "tolerance", lower = 0, lower_open = TRUE)
  parameters <- lomm11_period_damping_map(period_frames, damping_ratio)
  exact_transition <- lomm11_require_helper("revision_exact_transition")
  exact <- exact_transition(parameters$eta, parameters$zeta, dt)
  transition <- exact$transition
  raw_map <- exact$innovation_map
  radius <- max(Mod(eigen(transition, only.values = TRUE)$values))
  if (!is.finite(radius) || radius >= 1) {
    lomm11_stop("Unit-position normalization requires a stable transition.")
  }
  raw_covariance <- lomm11_solve_discrete_lyapunov(
    transition,
    tcrossprod(raw_map)
  )
  if (!is.finite(raw_covariance[1L, 1L]) || raw_covariance[1L, 1L] <= 0) {
    lomm11_stop("The unscaled stationary position variance is not positive.")
  }
  innovation_sd <- 1 / sqrt(raw_covariance[1L, 1L])
  innovation_map <- raw_map * innovation_sd
  innovation_covariance <- tcrossprod(innovation_map)
  stationary_covariance <- lomm11_solve_discrete_lyapunov(
    transition,
    innovation_covariance
  )
  residual <- stationary_covariance -
    transition %*% stationary_covariance %*% t(transition) -
    innovation_covariance
  system <- c(parameters, list(
    dt = as.numeric(dt),
    transition = transition,
    innovation_map = innovation_map,
    innovation_sd = innovation_sd,
    innovation_covariance = innovation_covariance,
    stationary_covariance = stationary_covariance,
    spectral_radius = radius,
    lyapunov_residual = max(abs(residual))
  ))
  lomm11_validate_unit_position_system(system, tolerance)
  system
}

lomm11_validate_unit_position_system <- function(system, tolerance = 1e-10) {
  required <- c(
    "eta", "zeta", "roots", "transition", "innovation_map",
    "innovation_covariance", "stationary_covariance", "spectral_radius",
    "lyapunov_residual"
  )
  if (!is.list(system) || length(setdiff(required, names(system))) > 0L) {
    lomm11_stop("The oscillator system is missing required fields.")
  }
  if (max(Re(system$roots)) >= 0 || system$spectral_radius >= 1) {
    lomm11_stop("The oscillator system is not asymptotically stable.")
  }
  if (abs(system$stationary_covariance[1L, 1L] - 1) > tolerance ||
      system$lyapunov_residual > tolerance) {
    lomm11_stop("The unit-position Lyapunov normalization failed validation.")
  }
  invisible(TRUE)
}

lomm11_named_substream_seeds <- function(master_seed, pairing_key, clip,
                                         segment = LOMM11_CLIP_SHARED_SEGMENT) {
  lomm11_segment_substream_seeds(master_seed, pairing_key, clip, segment)
}

lomm11_component_substream_seed <- function(master_seed, pairing_key, clip,
                                            segment, substream) {
  lomm11_assert_scalar(master_seed, "master_seed", lower = 1, upper = 2147483646)
  labels <- c(pairing_key, clip, segment, substream)
  if (anyNA(labels) || any(!nzchar(as.character(labels))) ||
      !substream %in% LOMM11_RNG_SUBSTREAMS) {
    lomm11_stop("Component substream identifiers are incomplete or invalid.")
  }
  component_state <- lomm11_require_helper("lomm11_component_rng_state")
  component_state(pairing_key, clip, segment, substream, master_seed)
}

lomm11_segment_substream_seeds <- function(master_seed, pairing_key, clip,
                                           segment) {
  seeds <- lapply(LOMM11_RNG_SUBSTREAMS, function(substream) {
    lomm11_component_substream_seed(
      master_seed, pairing_key, clip, segment, substream
    )
  })
  stats::setNames(seeds, LOMM11_RNG_SUBSTREAMS)
}

lomm11_validate_rng_state <- function(state) {
  moduli <- c(rep(4294967087, 3L), rep(4294944443, 3L))
  unsigned <- as.numeric(state[-1L])
  negative <- !is.na(unsigned) & unsigned < 0
  unsigned[negative] <- unsigned[negative] + 2^32
  valid <- is.integer(state) && length(state) == 7L && !anyNA(state) &&
    identical(state[[1L]], 10407L) && all(unsigned > 0) &&
    all(unsigned < moduli) && all(unsigned != 2^31)
  if (!valid) {
    lomm11_stop("A component RNG state is not a valid L'Ecuyer-CMRG state.")
  }
  invisible(TRUE)
}

lomm11_with_seed <- function(seed, callback) {
  if (!is.function(callback)) {
    lomm11_stop("A seeded callback must be a function.")
  }
  lomm11_validate_rng_state(seed)
  previous_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  assign(".Random.seed", seed, envir = .GlobalEnv)
  callback()
}

lomm11_psd_floor <- function(covariance, floor = 1e-8) {
  covariance <- as.matrix(covariance)
  lomm11_assert_scalar(floor, "PSD floor", lower = 0, lower_open = TRUE)
  if (!is.numeric(covariance) || nrow(covariance) != ncol(covariance) ||
      any(!is.finite(covariance))) {
    lomm11_stop("Covariance must be a finite square numeric matrix.")
  }
  decomposition <- eigen((covariance + t(covariance)) / 2, symmetric = TRUE)
  output <- decomposition$vectors %*%
    diag(pmax(decomposition$values, floor), nrow = nrow(covariance)) %*%
    t(decomposition$vectors)
  dimnames(output) <- dimnames(covariance)
  (output + t(output)) / 2
}

lomm11_draw_gaussian <- function(n, covariance, seed, center = NULL) {
  covariance <- as.matrix(covariance)
  if (length(n) != 1L || !is.finite(n) || n < 1L || n != as.integer(n)) {
    lomm11_stop("Gaussian draw count must be a positive integer.")
  }
  decomposition <- eigen((covariance + t(covariance)) / 2, symmetric = TRUE)
  if (min(decomposition$values) < -1e-8) {
    lomm11_stop("Gaussian covariance is not positive semidefinite.")
  }
  root <- decomposition$vectors %*%
    diag(sqrt(pmax(decomposition$values, 0)), nrow = nrow(covariance))
  draws <- lomm11_with_seed(seed, function() {
    matrix(stats::rnorm(n * nrow(covariance)), nrow = n) %*% t(root)
  })
  if (!is.null(center)) {
    if (length(center) != ncol(draws) || any(!is.finite(center))) {
      lomm11_stop("Gaussian center does not match the covariance.")
    }
    draws <- sweep(draws, 2L, center, "+")
  }
  colnames(draws) <- colnames(covariance)
  draws
}

lomm11_orient_direction <- function(direction) {
  pivot <- which.max(abs(direction))
  if (direction[[pivot]] < 0) -direction else direction
}

lomm11_training_rows <- function(ilr_view, training_clips, coordinate_columns) {
  required <- c("clip", coordinate_columns)
  missing <- setdiff(required, names(ilr_view))
  if (length(missing) > 0L) {
    lomm11_stop("Training ILR view is missing columns: ", paste(missing, collapse = ", "), ".")
  }
  training_clips <- unique(as.character(training_clips))
  if (length(training_clips) < 2L || anyNA(training_clips) || any(!nzchar(training_clips))) {
    lomm11_stop("At least two explicit training clip identifiers are required.")
  }
  rows <- as.character(ilr_view$clip) %in% training_clips
  if ("analysis_variant" %in% names(ilr_view)) {
    rows <- rows & ilr_view$analysis_variant == "complete_case"
  }
  training <- ilr_view[rows, required, drop = FALSE]
  absent <- setdiff(training_clips, unique(as.character(training$clip)))
  if (length(absent) > 0L) {
    lomm11_stop("Training clips are absent from the complete-case ILR view.")
  }
  values <- as.matrix(training[coordinate_columns])
  storage.mode(values) <- "double"
  if (any(!is.finite(values))) {
    lomm11_stop("Training ILR coordinates must be finite.")
  }
  list(data = training, values = values, clips = training_clips)
}

lomm11_fit_training_geometry <- function(ilr_view, training_clips,
                                         coordinate_columns = sprintf("ilr_%02d", 1:10),
                                         n_directions = 3L, psd_floor = 1e-8) {
  if (length(n_directions) != 1L || n_directions < 1L ||
      n_directions >= length(coordinate_columns)) {
    lomm11_stop("n_directions must be smaller than the ILR dimension.")
  }
  prepared <- lomm11_training_rows(ilr_view, training_clips, coordinate_columns)
  center <- colMeans(prepared$values)
  clip_indices <- split(seq_len(nrow(prepared$data)), prepared$data$clip)
  clip_means <- t(vapply(clip_indices, function(index) {
    colMeans(prepared$values[index, , drop = FALSE])
  }, numeric(length(coordinate_columns))))
  colnames(clip_means) <- coordinate_columns
  within <- prepared$values - clip_means[match(prepared$data$clip, rownames(clip_means)), ]
  within_covariance <- stats::cov(within)
  decomposition <- eigen(within_covariance, symmetric = TRUE)
  indices <- seq_len(n_directions)
  eigenvalues <- pmax(decomposition$values[indices], 0)
  if (any(eigenvalues <= 0)) {
    lomm11_stop("Training ILR data do not support the requested directions.")
  }
  directions <- apply(
    decomposition$vectors[, indices, drop = FALSE], 2L, lomm11_orient_direction
  )
  if (is.null(dim(directions))) {
    directions <- matrix(directions, ncol = 1L)
  }
  rownames(directions) <- coordinate_columns
  colnames(directions) <- sprintf("factor_%02d", indices)
  relative_scales <- sqrt(eigenvalues) / sqrt(mean(eigenvalues))
  loadings <- sweep(directions, 2L, relative_scales, "*")
  baseline_deviations <- sweep(clip_means, 2L, center, "-")
  baseline_full <- stats::cov(baseline_deviations)
  baseline_shrunk <- 0.5 * baseline_full + 0.5 * diag(diag(baseline_full))
  dimnames(baseline_shrunk) <- list(coordinate_columns, coordinate_columns)
  geometry <- structure(list(
    coordinate_columns = coordinate_columns,
    n_directions = as.integer(n_directions),
    training_clips = sort(prepared$clips),
    n_training_rows = nrow(prepared$data),
    center = stats::setNames(center, coordinate_columns),
    directions = directions,
    direction_eigenvalues = stats::setNames(eigenvalues, colnames(directions)),
    relative_scales = stats::setNames(relative_scales, colnames(directions)),
    loadings = loadings,
    between_clip_covariance = lomm11_psd_floor(baseline_shrunk, psd_floor),
    within_clip_covariance = within_covariance,
    psd_floor = psd_floor
  ), class = "lomm11_training_geometry")
  lomm11_validate_training_geometry(geometry)
  geometry
}

lomm11_validate_training_geometry <- function(geometry, tolerance = 1e-8) {
  required <- c(
    "coordinate_columns", "n_directions", "training_clips", "center",
    "directions", "direction_eigenvalues", "relative_scales", "loadings",
    "between_clip_covariance"
  )
  if (!is.list(geometry) || length(setdiff(required, names(geometry))) > 0L) {
    lomm11_stop("Training geometry is missing required fields.")
  }
  dimension <- length(geometry$coordinate_columns)
  expected <- c(dimension, geometry$n_directions)
  if (!identical(dim(geometry$directions), expected) ||
      !identical(dim(geometry$loadings), expected) ||
      length(geometry$center) != dimension || any(!is.finite(geometry$center))) {
    lomm11_stop("Training geometry dimensions do not agree.")
  }
  if (max(abs(crossprod(geometry$directions) - diag(geometry$n_directions))) > tolerance) {
    lomm11_stop("Training directions are not orthonormal.")
  }
  oriented <- apply(geometry$directions, 2L, function(direction) {
    direction[[which.max(abs(direction))]] >= 0
  })
  if (!all(oriented) || min(eigen(geometry$between_clip_covariance,
                                  symmetric = TRUE, only.values = TRUE)$values) <= 0) {
    lomm11_stop("Training direction signs or between-clip covariance are invalid.")
  }
  invisible(TRUE)
}

lomm11_semimarkov_hard_states <- function(n_observations, n_factors,
                                          initial_high_probability = 0.5,
                                          geometric_probability = 1 / 8,
                                          minimum_dwell = 4L,
                                          maximum_dwell = 24L,
                                          seed) {
  if (n_observations < 1L || n_factors < 1L) {
    lomm11_stop("Gate dimensions must be positive.")
  }
  lomm11_assert_scalar(
    initial_high_probability, "initial_high_probability",
    lower = 0, upper = 1
  )
  lomm11_assert_scalar(
    geometric_probability, "geometric_probability",
    lower = 0, upper = 1, lower_open = TRUE
  )
  if (minimum_dwell < 1L || maximum_dwell < minimum_dwell) {
    lomm11_stop("Gate dwell bounds are invalid.")
  }
  lomm11_with_seed(seed, function() {
    states <- matrix(FALSE, nrow = n_observations, ncol = n_factors)
    for (factor_index in seq_len(n_factors)) {
      position <- 1L
      high <- stats::runif(1L) < initial_high_probability
      while (position <= n_observations) {
        dwell <- min(
          maximum_dwell,
          minimum_dwell + stats::rgeom(1L, geometric_probability)
        )
        endpoint <- min(n_observations, position + dwell - 1L)
        states[position:endpoint, factor_index] <- high
        position <- endpoint + 1L
        high <- !high
      }
    }
    colnames(states) <- sprintf("factor_%02d", seq_len(n_factors))
    states
  })
}

lomm11_half_cosine_gates <- function(hard_states, low = 0.25, high = 1,
                                     transition_observations = 3L) {
  hard_states <- as.matrix(hard_states)
  if (!is.logical(hard_states) || anyNA(hard_states)) {
    lomm11_stop("Hard gate states must be a complete logical matrix.")
  }
  lomm11_assert_scalar(low, "gate low", lower = 0)
  lomm11_assert_scalar(high, "gate high", lower = low, lower_open = TRUE)
  if (transition_observations < 1L ||
      transition_observations != as.integer(transition_observations)) {
    lomm11_stop("Gate transition length must be a positive integer.")
  }
  hard_values <- ifelse(hard_states, high, low)
  gates <- hard_values
  if (transition_observations == 1L || nrow(gates) < 2L) {
    return(gates)
  }
  phase <- seq(0, 1, length.out = transition_observations)
  blend <- (1 - cos(pi * phase)) / 2
  for (factor_index in seq_len(ncol(gates))) {
    changes <- which(hard_states[-1L, factor_index] !=
                       hard_states[-nrow(hard_states), factor_index]) + 1L
    for (start in changes) {
      index <- seq.int(start, min(nrow(gates), start + transition_observations - 1L))
      old <- hard_values[start - 1L, factor_index]
      new <- hard_values[start, factor_index]
      gates[index, factor_index] <- old + (new - old) * blend[seq_along(index)]
    }
  }
  dimnames(gates) <- dimnames(hard_states)
  gates
}

lomm11_draw_gate_schedule <- function(n_observations, n_factors, seed,
                                      low = 0.25, high = 1,
                                      initial_high_probability = 0.5,
                                      geometric_probability = 1 / 8,
                                      minimum_dwell = 4L,
                                      maximum_dwell = 24L,
                                      transition_observations = 3L) {
  hard_states <- lomm11_semimarkov_hard_states(
    n_observations, n_factors, initial_high_probability,
    geometric_probability, minimum_dwell, maximum_dwell, seed
  )
  list(
    hard_states = hard_states,
    gates = lomm11_half_cosine_gates(
      hard_states, low, high, transition_observations
    )
  )
}

lomm11_simulate_latent_factors <- function(n_observations, n_factors, system,
                                           initial_seed, innovation_seed) {
  lomm11_validate_unit_position_system(system)
  initial <- lomm11_draw_gaussian(
    n_factors, system$stationary_covariance, initial_seed
  )
  innovations <- lomm11_with_seed(innovation_seed, function() {
    matrix(
      stats::rnorm(max(0L, n_observations - 1L) * n_factors),
      nrow = max(0L, n_observations - 1L), ncol = n_factors
    )
  })
  state <- array(NA_real_, dim = c(n_observations, 2L, n_factors))
  state[1L, , ] <- t(initial)
  if (n_observations > 1L) {
    for (time_index in 2:n_observations) {
      for (factor_index in seq_len(n_factors)) {
        state[time_index, , factor_index] <-
          system$transition %*% state[time_index - 1L, , factor_index] +
          system$innovation_map * innovations[time_index - 1L, factor_index]
      }
    }
  }
  factor_names <- sprintf("factor_%02d", seq_len(n_factors))
  position <- matrix(state[, 1L, ], nrow = n_observations,
                     dimnames = list(NULL, factor_names))
  velocity <- matrix(state[, 2L, ], nrow = n_observations,
                     dimnames = list(NULL, factor_names))
  list(position = position, velocity = velocity, innovations = innovations)
}

lomm11_variant_flags <- function(variant) {
  if (is.data.frame(variant)) {
    if (nrow(variant) != 1L) lomm11_stop("Variant data must have one row.")
    variant <- as.list(variant[1L, , drop = FALSE])
  }
  if (!is.list(variant) || length(setdiff(c("B", "S", "G"), names(variant))) > 0L) {
    lomm11_stop("Variant must define B, S, and G flags.")
  }
  flags <- vapply(variant[c("B", "S", "G")], function(value) {
    if (length(value) != 1L || is.na(value) || !value %in% c(0, 1, FALSE, TRUE)) {
      lomm11_stop("B, S, and G must be binary flags.")
    }
    as.logical(value)
  }, logical(1))
  flags
}

lomm11_normalize_frame_plan <- function(frame_plan) {
  required <- c("clip", "frame_index")
  missing <- setdiff(required, names(frame_plan))
  if (length(missing) > 0L || nrow(frame_plan) == 0L) {
    lomm11_stop("Frame plan requires non-empty clip and frame_index columns.")
  }
  plan <- frame_plan
  plan$clip <- as.character(plan$clip)
  if (anyNA(plan$clip) || any(!nzchar(plan$clip)) ||
      any(!is.finite(plan$frame_index)) ||
      any(plan$frame_index != as.integer(plan$frame_index))) {
    lomm11_stop("Frame plan clip identifiers and integer frame indices are invalid.")
  }
  if (!"frame_key" %in% names(plan)) {
    plan$frame_key <- paste(plan$clip, plan$frame_index, sep = "|")
  }
  if (anyDuplicated(plan$frame_key) ||
      anyDuplicated(paste(plan$clip, plan$frame_index, sep = "\037"))) {
    lomm11_stop("Frame plan keys must be unique.")
  }
  plan[order(plan$clip, plan$frame_index), , drop = FALSE]
}

lomm11_infer_generation_segments <- function(plan) {
  segment <- character(nrow(plan))
  clip_indices <- split(seq_len(nrow(plan)), factor(plan$clip, levels = unique(plan$clip)))
  for (clip in names(clip_indices)) {
    index <- clip_indices[[clip]]
    breaks <- c(TRUE, diff(plan$frame_index[index]) != 1)
    segment[index] <- sprintf("%s::generation%03d", clip, cumsum(breaks))
  }
  segment
}

lomm11_prepare_generation_frame_plan <- function(frame_plan) {
  plan <- frame_plan
  if ("analysis_variant" %in% names(plan) &&
      any(plan$analysis_variant == "ilr_gap2")) {
    plan <- plan[plan$analysis_variant == "ilr_gap2", , drop = FALSE]
  }
  if ("sensitivity_included" %in% names(plan)) {
    included <- !is.na(plan$sensitivity_included) & as.logical(plan$sensitivity_included)
    plan <- plan[included, , drop = FALSE]
  }
  plan <- lomm11_normalize_frame_plan(plan)
  segment_source <- if ("generation_segment_id" %in% names(plan)) {
    plan$generation_segment_id
  } else if ("sensitivity_segment_id" %in% names(plan)) {
    plan$sensitivity_segment_id
  } else if ("segment_id" %in% names(plan) &&
      (!"analysis_variant" %in% names(plan) ||
       all(plan$analysis_variant == "ilr_gap2"))) {
    plan$segment_id
  } else {
    lomm11_infer_generation_segments(plan)
  }
  if (anyNA(segment_source) || any(!nzchar(as.character(segment_source)))) {
    lomm11_stop("Generation segments must exclude unresolved ilr_gap2 gaps.")
  }
  plan$generation_segment_id <- as.character(segment_source)
  plan$generation_segment_position <- rep(NA_integer_, nrow(plan))
  plan$generation_segment_length <- rep(NA_integer_, nrow(plan))
  segment_indices <- split(
    seq_len(nrow(plan)),
    factor(plan$generation_segment_id, levels = unique(plan$generation_segment_id))
  )
  for (index in segment_indices) {
    if (length(unique(plan$clip[index])) != 1L ||
        any(diff(plan$frame_index[index]) != 1)) {
      lomm11_stop("Each generation segment must be one contiguous clip interval.")
    }
    plan$generation_segment_position[index] <- seq_along(index)
    plan$generation_segment_length[index] <- length(index)
  }
  plan$generation_segment_position <- as.integer(plan$generation_segment_position)
  plan$generation_segment_length <- as.integer(plan$generation_segment_length)
  plan
}

lomm11_clip_metadata <- function(clip_plan) {
  excluded <- c(
    LOMM11_DEFAULT_CHANNELS,
    paste0("clr_", LOMM11_DEFAULT_CHANNELS),
    sprintf("ilr_%02d", 1:10),
    "predicted_label", "analysis_variant", "observation_key", "segment_id",
    "segment_position", "segment_length", "source_complete", "is_imputed",
    "imputation_method"
  )
  clip_plan[, setdiff(names(clip_plan), excluded), drop = FALSE]
}

lomm11_generate_segment_process <- function(segment_plan, geometry, system,
                                            seeds, flags, kappa, clip_scale,
                                            baseline, residual_sd, gate_low,
                                            gate_high,
                                            gate_initial_high_probability,
                                            gate_transition_observations) {
  n_observations <- nrow(segment_plan)
  n_factors <- geometry$n_directions
  latent <- lomm11_simulate_latent_factors(
    n_observations, n_factors, system,
    seeds[["initial_state"]], seeds[["innovations"]]
  )
  gate_draw <- lomm11_draw_gate_schedule(
    n_observations, n_factors, seeds[["gate_schedule"]],
    gate_low, gate_high, gate_initial_high_probability,
    transition_observations = gate_transition_observations
  )
  residuals <- lomm11_with_seed(seeds[["residuals"]], function() {
    matrix(
      stats::rnorm(n_observations * length(geometry$center), sd = residual_sd),
      nrow = n_observations, ncol = length(geometry$center)
    )
  })
  gates <- if (flags[["G"]]) gate_draw$gates else matrix(
    1, nrow = n_observations, ncol = n_factors,
    dimnames = dimnames(gate_draw$gates)
  )
  signal <- (latent$position * gates) %*% t(geometry$loadings)
  ilr <- kappa * clip_scale * (signal + residuals)
  ilr <- sweep(ilr, 2L, baseline, "+")
  ilr <- sweep(ilr, 2L, geometry$center, "+")
  colnames(ilr) <- geometry$coordinate_columns
  list(
    ilr = ilr,
    latent_position = latent$position,
    latent_velocity = latent$velocity,
    innovations = latent$innovations,
    gates = gates,
    hard_gate_states = gate_draw$hard_states,
    residuals = residuals,
    substream_seeds = seeds
  )
}

lomm11_segment_panel <- function(segment_plan, process, ilr_basis, tau,
                                 common_logit_shift) {
  ilr <- process$ilr / tau
  clr <- ilr %*% t(ilr_basis)
  colnames(clr) <- paste0("clr_", rownames(ilr_basis))
  softmax <- lomm11_require_helper("lomm_softmax_rows")
  probability <- softmax(clr + common_logit_shift / tau)
  colnames(probability) <- rownames(ilr_basis)
  metadata <- lomm11_clip_metadata(segment_plan)
  predicted <- colnames(probability)[max.col(probability, ties.method = "first")]
  panel <- cbind(
    metadata,
    predicted_label = predicted,
    as.data.frame(probability, stringsAsFactors = FALSE),
    as.data.frame(clr, stringsAsFactors = FALSE),
    as.data.frame(ilr, stringsAsFactors = FALSE)
  )
  rownames(panel) <- NULL
  panel
}

lomm11_generate_clip_panel <- function(clip_plan, geometry, ilr_basis,
                                       period_frames, damping_ratio, kappa,
                                       variant, master_seed, pairing_key,
                                       dt = 1, residual_sd = 0.25, tau = 1,
                                       common_logit_shift = 0,
                                       gate_low = 0.25, gate_high = 1,
                                       gate_initial_high_probability = 0.5,
                                       gate_transition_observations = 3L) {
  lomm11_validate_training_geometry(geometry)
  flags <- lomm11_variant_flags(variant)
  lomm11_assert_scalar(kappa, "kappa", lower = 0, lower_open = TRUE)
  lomm11_assert_scalar(residual_sd, "residual_sd", lower = 0)
  lomm11_assert_scalar(tau, "tau", lower = 0, lower_open = TRUE)
  lomm11_assert_scalar(common_logit_shift, "common_logit_shift")
  clip_plan <- lomm11_prepare_generation_frame_plan(clip_plan)
  clips <- unique(clip_plan$clip)
  if (length(clips) != 1L) lomm11_stop("A clip generator call must contain one clip.")
  basis_validator <- lomm11_require_helper("lomm_validate_ilr_basis")
  basis_validator(ilr_basis, rownames(ilr_basis))
  if (!identical(geometry$coordinate_columns, colnames(ilr_basis))) {
    lomm11_stop("Generated ILR coordinates do not match the locked basis.")
  }
  shared_seeds <- lomm11_segment_substream_seeds(
    master_seed, pairing_key, clips, LOMM11_CLIP_SHARED_SEGMENT
  )
  baseline_draw <- as.numeric(lomm11_draw_gaussian(
    1L, geometry$between_clip_covariance,
    shared_seeds[["between_clip_baseline"]]
  ))
  scale_draw <- lomm11_with_seed(
    shared_seeds[["clip_scale"]], function() stats::runif(1L, 0.5, 1.5)
  )
  baseline <- if (flags[["B"]]) baseline_draw else rep(0, length(baseline_draw))
  clip_scale <- if (flags[["S"]]) scale_draw else 1
  system <- lomm11_build_unit_position_system(period_frames, damping_ratio, dt)
  segment_plans <- split(
    clip_plan,
    factor(
      clip_plan$generation_segment_id,
      levels = unique(clip_plan$generation_segment_id)
    )
  )
  segment_processes <- lapply(names(segment_plans), function(segment_id) {
    seeds <- lomm11_segment_substream_seeds(
      master_seed, pairing_key, clips, segment_id
    )
    lomm11_generate_segment_process(
      segment_plans[[segment_id]], geometry, system, seeds, flags, kappa,
      clip_scale, baseline, residual_sd, gate_low, gate_high,
      gate_initial_high_probability, gate_transition_observations
    )
  })
  names(segment_processes) <- names(segment_plans)
  segment_panels <- Map(function(segment_plan, process) {
    lomm11_segment_panel(
      segment_plan, process, ilr_basis, tau, common_logit_shift
    )
  }, segment_plans, segment_processes)
  panel <- do.call(rbind, segment_panels)
  panel <- panel[order(panel$frame_index), , drop = FALSE]
  rownames(panel) <- NULL
  output <- list(
    panel = panel,
    segments = segment_processes,
    component_draws = list(
      between_clip_baseline = baseline_draw,
      clip_scale = scale_draw,
      shared_substream_seeds = shared_seeds
    ),
    system = system,
    generated_frame_index = panel$frame_index
  )
  lomm11_validate_generated_panel(output$panel, ilr_basis)
  output
}

lomm11_generate_panel <- function(frame_plan, geometry, ilr_basis,
                                  period_frames, damping_ratio, kappa, variant,
                                  master_seed, pairing_key, ...) {
  plan <- lomm11_prepare_generation_frame_plan(frame_plan)
  clip_plans <- split(plan, factor(plan$clip, levels = unique(plan$clip)))
  generated <- lapply(clip_plans, function(clip_plan) {
    lomm11_generate_clip_panel(
      clip_plan, geometry, ilr_basis, period_frames, damping_ratio, kappa,
      variant, master_seed, pairing_key, ...
    )
  })
  panel <- do.call(rbind, lapply(generated, `[[`, "panel"))
  rownames(panel) <- NULL
  list(panel = panel, clips = generated)
}

lomm11_validate_generated_panel <- function(panel, ilr_basis,
                                            tolerance = 1e-10) {
  probability_columns <- rownames(ilr_basis)
  clr_columns <- paste0("clr_", probability_columns)
  ilr_columns <- colnames(ilr_basis)
  required <- c("frame_key", "clip", "frame_index", probability_columns,
                clr_columns, ilr_columns)
  if (length(setdiff(required, names(panel))) > 0L || anyDuplicated(panel$frame_key)) {
    lomm11_stop("Generated panel schema or frame keys are invalid.")
  }
  probability <- as.matrix(panel[probability_columns])
  clr <- as.matrix(panel[clr_columns])
  ilr <- as.matrix(panel[ilr_columns])
  if (any(!is.finite(c(probability, clr, ilr))) ||
      max(abs(rowSums(probability) - 1)) > tolerance ||
      max(abs(rowSums(clr))) > tolerance || any(probability <= 0)) {
    lomm11_stop("Generated compositional values failed validation.")
  }
  reconstructed <- ilr %*% t(ilr_basis)
  if (max(abs(unname(reconstructed) - unname(clr))) > tolerance) {
    lomm11_stop("Generated ILR and CLR coordinates do not agree.")
  }
  probability_clr <- log(probability) - rowMeans(log(probability))
  if (max(abs(unname(probability_clr) - unname(clr))) > tolerance) {
    lomm11_stop("Generated probabilities and log-ratio coordinates do not agree.")
  }
  invisible(TRUE)
}

lomm11_panel_data <- function(generated) {
  if (is.data.frame(generated)) return(generated)
  if (is.list(generated) && is.data.frame(generated$panel)) return(generated$panel)
  lomm11_stop("Generated input must be a panel or a generator result.")
}

lomm11_view_index_from_ledger <- function(frame_ledger) {
  required <- c(
    "frame_key", "source_row", "clip", "frame_index", "truth_label",
    "sample_order", "model", "source_complete", "is_imputed",
    "imputation_method", "primary_included", "sensitivity_included",
    "primary_segment_id", "primary_segment_position", "primary_segment_length",
    "sensitivity_segment_id", "sensitivity_segment_position",
    "sensitivity_segment_length"
  )
  if (length(setdiff(required, names(frame_ledger))) > 0L) {
    lomm11_stop("Frame ledger is missing view-index fields.")
  }
  make_view <- function(included, variant, prefix) {
    indices <- which(included)
    data.frame(
      observation_key = paste(variant, frame_ledger$frame_key[indices], sep = "|"),
      analysis_variant = variant,
      frame_key = frame_ledger$frame_key[indices],
      source_row = frame_ledger$source_row[indices],
      clip = frame_ledger$clip[indices],
      frame_index = frame_ledger$frame_index[indices],
      truth_label = frame_ledger$truth_label[indices],
      sample_order = frame_ledger$sample_order[indices],
      model = frame_ledger$model[indices],
      source_complete = frame_ledger$source_complete[indices],
      is_imputed = frame_ledger$is_imputed[indices],
      imputation_method = frame_ledger$imputation_method[indices],
      segment_id = frame_ledger[[paste0(prefix, "_segment_id")]][indices],
      segment_position = frame_ledger[[paste0(prefix, "_segment_position")]][indices],
      segment_length = frame_ledger[[paste0(prefix, "_segment_length")]][indices],
      stringsAsFactors = FALSE
    )
  }
  rbind(
    make_view(frame_ledger$primary_included, "complete_case", "primary"),
    make_view(frame_ledger$sensitivity_included, "ilr_gap2", "sensitivity")
  )
}

lomm11_subset_panel_view <- function(generated, view_index, analysis_variant) {
  panel <- lomm11_panel_data(generated)
  index <- view_index[view_index$analysis_variant == analysis_variant, , drop = FALSE]
  if (nrow(index) == 0L || anyDuplicated(index$frame_key)) {
    lomm11_stop("Requested view is empty or has duplicate frame keys.")
  }
  matched <- match(index$frame_key, panel$frame_key)
  if (anyNA(matched)) {
    lomm11_stop("Generated panel does not contain every requested view frame.")
  }
  values <- panel[matched, setdiff(names(panel), names(index)), drop = FALSE]
  output <- cbind(index, values)
  rownames(output) <- NULL
  output
}

lomm11_subset_panel_views <- function(generated, view_index,
                                      variants = c("complete_case", "ilr_gap2")) {
  views <- lapply(variants, function(variant) {
    lomm11_subset_panel_view(generated, view_index, variant)
  })
  names(views) <- variants
  lomm11_validate_same_panel_views(views)
  views
}

lomm11_validate_same_panel_views <- function(views) {
  if (!is.list(views) || length(views) < 2L) {
    lomm11_stop("At least two generated views are required.")
  }
  value_columns <- c(
    LOMM11_DEFAULT_CHANNELS,
    paste0("clr_", LOMM11_DEFAULT_CHANNELS),
    sprintf("ilr_%02d", 1:10)
  )
  reference <- views[[1L]]
  for (candidate in views[-1L]) {
    overlap <- intersect(reference$frame_key, candidate$frame_key)
    left <- reference[match(overlap, reference$frame_key), value_columns, drop = FALSE]
    right <- candidate[match(overlap, candidate$frame_key), value_columns, drop = FALSE]
    if (!identical(unname(as.matrix(left)), unname(as.matrix(right)))) {
      lomm11_stop("Generated views do not reuse the same panel values.")
    }
  }
  invisible(TRUE)
}

lomm11_run_internal_generator_checks <- function(tolerance = 1e-9) {
  system <- lomm11_build_unit_position_system(12, 0.2, dt = 1)
  expected <- lomm11_period_damping_map(12, 0.2)
  if (abs(system$eta - expected$eta) > tolerance ||
      abs(system$zeta - expected$zeta) > tolerance) {
    lomm11_stop("Period/damping mapping check failed.")
  }
  seeds <- lomm11_named_substream_seeds(11082026L, "fixture", "clip")
  if (anyDuplicated(seeds) || !identical(
    seeds, lomm11_named_substream_seeds(11082026L, "fixture", "clip")
  )) {
    lomm11_stop("Named RNG substream check failed.")
  }
  hard <- matrix(c(FALSE, FALSE, TRUE, TRUE, TRUE), ncol = 1L)
  gate <- lomm11_half_cosine_gates(hard, transition_observations = 3L)
  if (min(gate) < 0.25 || max(gate) > 1 ||
      abs(gate[3L, 1L] - 0.25) > tolerance ||
      abs(gate[5L, 1L] - 1) > tolerance) {
    lomm11_stop("Half-cosine gate check failed.")
  }
  invisible(TRUE)
}
