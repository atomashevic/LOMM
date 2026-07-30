REVISION_JOB_IDS <- c(
  "empirical_cv",
  "t100_reuse_audit",
  "t100_dynega",
  "t100_static_oracle",
  "t100_graphicalvar_fallback",
  "t100_gimme_fallback",
  "t25_dynega",
  "t25_graphicalvar",
  "t25_gimme",
  "t25_static_oracle",
  "challenge_dynega",
  "challenge_graphicalvar",
  "challenge_static_oracle",
  "aggregate_results",
  "rebuild_figures_tables",
  "integrate_manuscript"
)

REVISION_COMMUNITY_ALGORITHMS <- c("louvain", "leiden", "walktrap")
REVISION_T100_N_EMBED <- c(5L, 10L, 15L, 25L, 50L)
REVISION_T25_N_EMBED <- c(5L, 6L, 10L, 12L)
REVISION_SEED_ORIGIN <- 20260715L
REVISION_CONFIG_VERSION <- "revision_v2"
REVISION_STRUCTURE_PHYSICAL_DT <- 0.4
REVISION_DYNEGA_OBSERVATION_DELTA <- 1
REVISION_DYNEGA_DELTA_UNITS <- "observation_interval"

revision_scalar_or <- function(value, fallback) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1]])) {
    return(fallback)
  }
  value
}

revision_collapse <- function(values) {
  paste(values, collapse = ";")
}

revision_parse_numeric_vector <- function(value) {
  as.numeric(strsplit(as.character(value), ";", fixed = TRUE)[[1]])
}

revision_parse_character_vector <- function(value) {
  strsplit(as.character(value), ";", fixed = TRUE)[[1]]
}

revision_factor_archetypes <- function() {
  data.frame(
    factor_archetype = c(
      "smooth_slow",
      "moderate_bounded",
      "reactive_bounded",
      "volatile_bounded"
    ),
    eta = c(-0.30, -0.55, -0.85, -1.10),
    zeta = c(-1.70, -1.00, -0.65, -0.30),
    sigma_q = c(0.12, 0.30, 0.45, 0.65),
    stringsAsFactors = FALSE
  )
}

revision_scenario_families <- function() {
  data.frame(
    scenario_family = c(
      "old_low_single_stable",
      "old_high_single_stable",
      "old_high_switch_stable",
      "old_high_switch_volatile",
      "oai_ranked_persistence",
      "oai_broad_geometry"
    ),
    sigma_rule = c("scaled", "scaled", "scaled", "scaled", "fixed", "fixed"),
    sigma_value = c(0.50, 1.00, 1.00, 1.25, 0.25, 0.35),
    active_gain = c(0.35, 1.00, 1.00, 1.25, 1.00, 1.10),
    background_gain = c(0.05, 0.08, 0.08, 0.08, 0.08, 0.10),
    dwell_min_frac = c(1.00, 1.00, 0.20, 0.03, 1.00, 0.12),
    dwell_max_frac = c(1.00, 1.00, 0.45, 0.10, 1.00, 0.30),
    switching = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

revision_factor_sigma <- function(family_row, archetypes, n_factors) {
  if (identical(family_row$sigma_rule[[1]], "fixed")) {
    return(rep(family_row$sigma_value[[1]], n_factors))
  }
  archetypes$sigma_q[seq_len(n_factors)] * family_row$sigma_value[[1]]
}

revision_oscillator_roots <- function(eta, zeta) {
  eta <- as.numeric(eta)
  zeta <- as.numeric(zeta)
  if (length(eta) != length(zeta) || length(eta) == 0L ||
      any(!is.finite(eta)) || any(!is.finite(zeta))) {
    stop("eta and zeta must be aligned finite numeric vectors.", call. = FALSE)
  }
  discriminant_root <- sqrt(as.complex(zeta^2 + 4 * eta))
  roots <- cbind(
    r_minus = (zeta - discriminant_root) / 2,
    r_plus = (zeta + discriminant_root) / 2
  )
  rownames(roots) <- paste0("factor_", seq_along(eta))
  roots
}

revision_stability <- function(eta, zeta, dt) {
  roots <- revision_oscillator_roots(eta, zeta)
  max_real <- max(Re(roots))
  spectral_radius <- exp(dt * max_real)
  anti_damped <- any(eta < 0 & zeta > 0)
  stability_class <- if (anti_damped) {
    "anti_damped"
  } else if (max_real < 0 && spectral_radius < 1) {
    "asymptotically_stable"
  } else {
    "unstable"
  }
  data.frame(
    continuous_max_real_eigenvalue = max_real,
    discrete_spectral_radius = spectral_radius,
    continuous_stable = max_real < 0,
    discrete_stable = spectral_radius < 1,
    anti_damped = anti_damped,
    stability_class = stability_class,
    stringsAsFactors = FALSE
  )
}

revision_integrated_input <- function(system_matrix, transition, dt, eta) {
  if (abs(eta) > 1e-10) {
    return(matrix(c((transition[1L, 1L] - 1) / eta, transition[2L, 1L] / eta), ncol = 1L))
  }
  input <- matrix(c(0, 1), ncol = 1L)
  term <- input * dt
  integrated <- term
  for (order in seq_len(40L)) {
    term <- system_matrix %*% term * dt / (order + 1L)
    integrated <- integrated + term
    if (max(abs(term)) < 1e-14) {
      break
    }
  }
  integrated
}

revision_exact_transition <- function(eta, zeta, dt) {
  system_matrix <- matrix(c(0, 1, eta, zeta), nrow = 2L, byrow = TRUE)
  half_trace <- zeta / 2
  centered <- system_matrix - diag(half_trace, nrow = 2L)
  discriminant <- half_trace^2 + eta
  tolerance <- 1e-12
  if (discriminant > tolerance) {
    root <- sqrt(discriminant)
    coefficient <- sinh(root * dt) / root
    base <- cosh(root * dt)
  } else if (discriminant < -tolerance) {
    root <- sqrt(-discriminant)
    coefficient <- sin(root * dt) / root
    base <- cos(root * dt)
  } else {
    coefficient <- dt
    base <- 1
  }
  transition <- exp(half_trace * dt) * (base * diag(2L) + coefficient * centered)
  innovation_map <- revision_integrated_input(system_matrix, transition, dt, eta)
  list(transition = transition, innovation_map = innovation_map)
}

revision_stationary_state_covariance <- function(eta, zeta, sigma_q, dt) {
  stability <- revision_stability(eta, zeta, dt)
  if (!isTRUE(stability$discrete_stable[[1]])) {
    stop("Stationary initialization requires a stable transition.", call. = FALSE)
  }
  transition <- revision_exact_transition(eta, zeta, dt)
  innovation_covariance <- sigma_q^2 *
    tcrossprod(transition$innovation_map)
  system <- diag(4L) - kronecker(transition$transition, transition$transition)
  covariance <- matrix(solve(system, as.vector(innovation_covariance)), nrow = 2L)
  (covariance + t(covariance)) / 2
}

revision_sample_gaussian <- function(covariance) {
  decomposition <- eigen(covariance, symmetric = TRUE)
  values <- pmax(Re(decomposition$values), 0)
  as.numeric(decomposition$vectors %*% (sqrt(values) * stats::rnorm(length(values))))
}

revision_initial_states <- function(eta, zeta, sigma_q, dt, seed) {
  set.seed(seed)
  states <- matrix(NA_real_, nrow = length(eta), ncol = 2L)
  for (factor_index in seq_along(eta)) {
    factor_stability <- revision_stability(
      eta[[factor_index]],
      zeta[[factor_index]],
      dt
    )
    if (isTRUE(factor_stability$discrete_stable[[1]])) {
      covariance <- revision_stationary_state_covariance(
        eta[[factor_index]],
        zeta[[factor_index]],
        sigma_q[[factor_index]],
        dt
      )
      states[factor_index, ] <- revision_sample_gaussian(covariance)
    } else {
      states[factor_index, ] <- c(
        stats::runif(1L, -0.25, 0.25),
        stats::runif(1L, -2, 2)
      )
    }
  }
  colnames(states) <- c("x0", "v0")
  states
}

revision_smooth_activity_envelope <- function(active_dimension,
                                              n_factors,
                                              active_gain,
                                              background_gain,
                                              transition_width = 5L) {
  active_dimension <- as.integer(active_dimension)
  envelope <- matrix(
    background_gain,
    nrow = length(active_dimension),
    ncol = n_factors
  )
  envelope[cbind(seq_along(active_dimension), active_dimension)] <- active_gain
  changes <- which(active_dimension[-1L] != active_dimension[-length(active_dimension)]) + 1L
  if (length(changes) == 0L || transition_width < 1L) {
    return(envelope)
  }

  for (change in changes) {
    old_factor <- active_dimension[[change - 1L]]
    new_factor <- active_dimension[[change]]
    indices <- seq.int(change, min(nrow(envelope), change + transition_width - 1L))
    phase <- if (length(indices) == 1L) {
      1
    } else {
      seq(0, 1, length.out = length(indices))
    }
    new_weight <- 0.5 * (1 - cos(pi * phase))
    old_weight <- 1 - new_weight
    envelope[indices, old_factor] <- background_gain +
      (active_gain - background_gain) * old_weight
    envelope[indices, new_factor] <- background_gain +
      (active_gain - background_gain) * new_weight
  }
  envelope
}

revision_loading_matrix <- function(n_factors,
                                    indicators_per_factor,
                                    primary_loading,
                                    cross_loadings = FALSE,
                                    secondary_draws = NULL) {
  item_count <- n_factors * indicators_per_factor
  membership <- rep(seq_len(n_factors), each = indicators_per_factor)
  loadings <- matrix(0, nrow = item_count, ncol = n_factors)
  loadings[cbind(seq_len(item_count), membership)] <- primary_loading
  if (isTRUE(cross_loadings)) {
    secondary <- if (is.null(secondary_draws)) {
      matrix(
        stats::rnorm(item_count * n_factors, sd = 0.05),
        nrow = item_count,
        ncol = n_factors
      )
    } else {
      matrix(as.numeric(secondary_draws), nrow = item_count, ncol = n_factors)
    }
    secondary <- pmin(pmax(secondary, -0.10), 0.10)
    secondary[cbind(seq_len(item_count), membership)] <- primary_loading
    loadings <- secondary
  }
  secondary_magnitudes <- abs(loadings)
  secondary_magnitudes[cbind(seq_len(item_count), membership)] <- -Inf
  if (any(abs(loadings[cbind(seq_len(item_count), membership)]) <=
          apply(secondary_magnitudes, 1L, max))) {
    stop("Primary loadings must remain largest after cross-loading generation.", call. = FALSE)
  }
  rownames(loadings) <- paste0("Y", seq_len(item_count))
  colnames(loadings) <- paste0("F", seq_len(n_factors))
  loadings
}

revision_equicorrelation_covariance <- function(standard_deviations, rho) {
  standard_deviations <- as.numeric(standard_deviations)
  dimension <- length(standard_deviations)
  lower_bound <- -1 / max(dimension - 1L, 1L)
  if (!is.finite(rho) || rho <= lower_bound || rho >= 1) {
    stop("Equicorrelation rho is outside the positive-definite interval.", call. = FALSE)
  }
  correlation <- matrix(rho, nrow = dimension, ncol = dimension)
  diag(correlation) <- 1
  scales <- diag(standard_deviations, nrow = dimension)
  scales %*% correlation %*% scales
}

revision_row_softmax <- function(values, alpha = 0, tau = 1) {
  values <- as.matrix(values)
  if (!is.finite(tau) || tau <= 0) {
    stop("Softmax tau must be positive.", call. = FALSE)
  }
  intercept <- rep_len(as.numeric(alpha), ncol(values))
  shifted <- sweep(values, 2L, intercept, "+") / tau
  shifted <- shifted - apply(shifted, 1L, max)
  exponentiated <- exp(shifted)
  exponentiated / rowSums(exponentiated)
}

revision_clr <- function(probabilities, epsilon = 1e-12) {
  probabilities <- pmax(as.matrix(probabilities), epsilon)
  log_values <- log(probabilities)
  log_values - rowMeans(log_values)
}

revision_symmetrize_weights <- function(weight_matrix) {
  weights <- as.matrix(weight_matrix)
  if (nrow(weights) != ncol(weights)) {
    stop("Weight matrix must be square.", call. = FALSE)
  }
  weights[!is.finite(weights)] <- 0
  weights <- (weights + t(weights)) / 2
  diag(weights) <- 0
  positive <- pmax(weights, 0)
  if (sum(positive) == 0) {
    positive <- abs(weights)
  }
  dimnames(positive) <- dimnames(weights)
  positive
}

revision_gimme_positive_structure <- function(weight_matrix) {
  weights <- as.matrix(weight_matrix)
  if (nrow(weights) != ncol(weights)) {
    stop("GIMME weight matrix must be square.", call. = FALSE)
  }
  weights[!is.finite(weights)] <- 0
  weights <- (weights + t(weights)) / 2
  diag(weights) <- 0
  positive <- pmax(weights, 0)
  dimnames(positive) <- dimnames(weights)
  positive
}

revision_assign_clip_folds <- function(clip_ids,
                                       strata = NULL,
                                       clip_lengths = NULL,
                                       n_folds = 5L) {
  clip_ids <- as.character(clip_ids)
  if (anyDuplicated(clip_ids)) {
    stop("clip_ids must contain one row per clip.", call. = FALSE)
  }
  strata <- if (is.null(strata)) rep("all", length(clip_ids)) else as.character(strata)
  if (length(strata) != length(clip_ids)) {
    stop("strata must align with clip_ids.", call. = FALSE)
  }
  clip_lengths <- if (is.null(clip_lengths)) {
    rep(NA_integer_, length(clip_ids))
  } else {
    as.integer(clip_lengths)
  }
  if (length(clip_lengths) != length(clip_ids)) {
    stop("clip_lengths must align with clip_ids.", call. = FALSE)
  }
  folds <- integer(length(clip_ids))
  offset <- 0L
  for (stratum in sort(unique(strata))) {
    indices <- which(strata == stratum)
    ordered <- indices[order(clip_lengths[indices], clip_ids[indices], na.last = TRUE)]
    folds[ordered] <- ((seq_along(ordered) - 1L + offset) %% n_folds) + 1L
    offset <- (offset + length(indices)) %% n_folds
  }
  data.frame(
    clip_id = clip_ids,
    stratum = strata,
    clip_length = clip_lengths,
    fold = folds,
    stringsAsFactors = FALSE
  )
}

revision_scenario_row <- function(family_row, n_factors, dt) {
  archetypes <- revision_factor_archetypes()
  factor_rows <- archetypes[seq_len(n_factors), , drop = FALSE]
  eta <- if (grepl("^oai_", family_row$scenario_family[[1]])) {
    -0.05 * seq_len(n_factors)
  } else {
    factor_rows$eta
  }
  zeta <- if (grepl("^oai_", family_row$scenario_family[[1]])) {
    0.06 + 0.02 * seq_len(n_factors)
  } else {
    factor_rows$zeta
  }
  sigma_q <- revision_factor_sigma(family_row, archetypes, n_factors)
  stability <- revision_stability(eta, zeta, dt)

  cbind(
    data.frame(
      scenario = paste0(family_row$scenario_family[[1]], "_", n_factors, "d"),
      scenario_family = family_row$scenario_family[[1]],
      scenario_mode = family_row$scenario_family[[1]],
      scenario_nfac = as.integer(n_factors),
      Q2 = c(`2` = 1.5, `3` = 2.5)[[as.character(n_factors)]],
      eta = revision_collapse(eta),
      zeta = revision_collapse(zeta),
      sigma_q = revision_collapse(sigma_q),
      factor_archetypes = revision_collapse(factor_rows$factor_archetype),
      active_gain = family_row$active_gain[[1]],
      background_gain = family_row$background_gain[[1]],
      dwell_min_frac = family_row$dwell_min_frac[[1]],
      dwell_max_frac = family_row$dwell_max_frac[[1]],
      switching = family_row$switching[[1]],
      stringsAsFactors = FALSE
    ),
    stability
  )
}

revision_scenarios <- function(dt = 0.4) {
  families <- revision_scenario_families()
  rows <- vector("list", nrow(families) * 2L)
  row_index <- 1L
  for (family_index in seq_len(nrow(families))) {
    for (n_factors in c(2L, 3L)) {
      rows[[row_index]] <- revision_scenario_row(
        families[family_index, , drop = FALSE],
        n_factors,
        dt
      )
      row_index <- row_index + 1L
    }
  }
  do.call(rbind, rows)
}

revision_challenge_variants <- function() {
  data.frame(
    challenge_id = c(
      "ideal_baseline",
      "independent_initialization",
      "smooth_activity_gate",
      "cross_loadings",
      "residual_equicorrelation",
      "latent_shock_equicorrelation",
      "softmax_bounded"
    ),
    initialization = c(
      "legacy_fixed",
      "stationary_if_stable_else_uniform",
      rep("legacy_fixed", 5L)
    ),
    stable_burn_in = rep(0L, 7L),
    antidamped_x0 = c(NA, "U[-0.25,0.25]", rep(NA, 5L)),
    antidamped_v0 = c(NA, "U[-2,2]", rep(NA, 5L)),
    activity_gate = c("step", "step", "half_cosine", rep("step", 4L)),
    gate_transition_observations = c(0L, 0L, 5L, rep(0L, 4L)),
    secondary_loading_distribution = c(rep(NA, 3L), "N(0,0.05^2)", rep(NA, 3L)),
    secondary_loading_limit = c(rep(NA, 3L), "[-0.10,0.10]", rep(NA, 3L)),
    primary_loading_constraint = c(rep(NA, 3L), "primary_largest", rep(NA, 3L)),
    residual_rho = c(rep(0, 4L), 0.10, 0, 0),
    latent_shock_rho = c(rep(0, 5L), 0.15, 0),
    observation_link = c(rep("linear", 6L), "softmax"),
    softmax_alpha = c(rep(NA_real_, 6L), 0),
    softmax_tau = c(rep(NA_real_, 6L), 1),
    logit_reference = c(rep(NA, 6L), "ideal_z_panel"),
    representations = c(rep("linear_z", 6L), "softmax_raw;softmax_clr"),
    stringsAsFactors = FALSE
  )
}

revision_empirical_calibration_axes <- function() {
  list(
    eta = c(-0.05, -0.10, -0.20, -0.40, -0.60, -0.80),
    sigma_q = c(0.05, 0.10, 0.20, 0.40, 0.60),
    dt = c(0.25, 0.50, 1.00),
    unconstrained_zeta = c(0.05, 0.10, 0.20, 0.40, 0.60),
    stable_negative_zeta = c(-0.05, -0.10, -0.20, -0.40, -0.60),
    tau = c(0.75, 1.00, 1.25)
  )
}

revision_empirical_candidates <- function() {
  axes <- revision_empirical_calibration_axes()
  regimes <- rbind(
    data.frame(
      calibration_regime = "unconstrained",
      zeta = axes$unconstrained_zeta,
      stringsAsFactors = FALSE
    ),
    data.frame(
      calibration_regime = "stable_negative_zeta",
      zeta = axes$stable_negative_zeta,
      stringsAsFactors = FALSE
    )
  )
  candidates <- merge(
    expand.grid(
      eta = axes$eta,
      sigma_q = axes$sigma_q,
      dt = axes$dt,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    regimes,
    by = NULL,
    sort = FALSE
  )
  candidates <- candidates[order(
    candidates$calibration_regime,
    candidates$eta,
    candidates$sigma_q,
    candidates$dt,
    candidates$zeta
  ), , drop = FALSE]
  candidates$candidate_id <- seq_len(nrow(candidates))
  candidates$candidate_label <- sprintf(
    "%s_eta_%.2f_zeta_%+.2f_sigma_%.2f_dt_%.2f",
    candidates$calibration_regime,
    candidates$eta,
    candidates$zeta,
    candidates$sigma_q,
    candidates$dt
  )
  candidates$observation_link <- "softmax"
  candidates$channel_intercept_rule <- "estimated_from_training_clips"
  candidates$calibration_scope <- "training_fold_only"
  rownames(candidates) <- NULL
  candidates
}

revision_hash_int <- function(value, modulus = 2147483629) {
  codepoints <- utf8ToInt(enc2utf8(as.character(value)))
  hash <- 0
  for (codepoint in codepoints) {
    hash <- (hash * 131 + codepoint) %% modulus
  }
  as.integer(hash)
}

revision_seed_from_key <- function(panel_key) {
  modulus <- 2147483646
  as.integer(
    (as.double(REVISION_SEED_ORIGIN) + as.double(revision_hash_int(panel_key))) %%
      modulus + 1
  )
}

revision_default_config_hash <- function(job_id) {
  revision_assert_job_id(job_id)
  material <- paste(
    REVISION_CONFIG_VERSION,
    job_id,
    revision_collapse(REVISION_COMMUNITY_ALGORITHMS),
    revision_collapse(REVISION_T100_N_EMBED),
    revision_collapse(REVISION_T25_N_EMBED),
    REVISION_SEED_ORIGIN,
    sep = "|"
  )
  paste0(
    sprintf("%08x", revision_hash_int(material)),
    sprintf("%04x", revision_hash_int(paste0(material, "|secondary")) %% 65536L)
  )
}

revision_assert_job_id <- function(job_id) {
  if (length(job_id) != 1L || is.na(job_id) || !(job_id %in% REVISION_JOB_IDS)) {
    stop("Unknown revision job: ", paste(job_id, collapse = ", "), call. = FALSE)
  }
  invisible(job_id)
}

revision_horizon_metadata <- function(time_points, dt) {
  data.frame(
    n_timepoints = as.integer(time_points),
    n_transitions = as.integer(time_points - 1L),
    dt = as.numeric(dt),
    nominal_horizon = as.numeric(time_points * dt),
    observed_horizon = as.numeric((time_points - 1L) * dt),
    stringsAsFactors = FALSE
  )
}

revision_panel_keys <- function(grid, scope) {
  paste(
    REVISION_CONFIG_VERSION,
    scope,
    grid$scenario,
    paste0("N", grid$N),
    paste0("T", grid$TimeP),
    paste0("K", grid$NVAR),
    sprintf("E%.3f", grid$Error_sd),
    sprintf("L%.3f", grid$Loadings),
    sprintf("Q%.2f", grid$Q),
    paste0("R", sprintf("%03d", grid$rep)),
    sep = "|"
  )
}

revision_pair_keys <- function(grid, scope) {
  paste(
    REVISION_CONFIG_VERSION,
    scope,
    grid$scenario,
    paste0("N", grid$N),
    paste0("K", grid$NVAR),
    sprintf("E%.3f", grid$Error_sd),
    sprintf("L%.3f", grid$Loadings),
    sprintf("Q%.2f", grid$Q),
    paste0("R", sprintf("%03d", grid$rep)),
    sep = "|"
  )
}

revision_add_panel_identity <- function(grid, scope) {
  grid$panel_key <- revision_panel_keys(grid, scope)
  grid$horizon_pair_key <- revision_pair_keys(grid, "matched_horizon_pair")
  grid$representation_key <- paste(grid$panel_key, grid$representation, sep = "|")
  grid$seed_base <- vapply(grid$horizon_pair_key, revision_seed_from_key, integer(1))
  grid
}

revision_expand_scenarios <- function(design, scenarios) {
  scenario_index <- match(design$scenario, scenarios$scenario)
  scenario_columns <- setdiff(names(scenarios), "scenario")
  for (column in scenario_columns) {
    design[[column]] <- scenarios[[column]][scenario_index]
  }
  design
}

revision_add_common_panel_columns <- function(grid, time_points, grid_scope) {
  horizon <- revision_horizon_metadata(time_points, 0.4)
  grid$TimeP <- as.integer(time_points)
  grid$NVAR <- 4L
  grid$Error <- as.numeric(grid$Error_sd)
  grid$Error_variance <- as.numeric(grid$Error_sd)^2
  grid$Q <- 0.75
  grid$base_err <- as.numeric(grid$Error_sd)
  grid$benchmark_grid <- grid_scope
  grid$grid_scope <- grid_scope
  grid$n_timepoints <- horizon$n_timepoints[[1]]
  grid$n_transitions <- horizon$n_transitions[[1]]
  grid$dt <- horizon$dt[[1]]
  grid$nominal_horizon <- horizon$nominal_horizon[[1]]
  grid$observed_horizon <- horizon$observed_horizon[[1]]
  grid$n_embed_candidates <- revision_collapse(
    if (time_points == 25L) REVISION_T25_N_EMBED else REVISION_T100_N_EMBED
  )
  grid
}

revision_matched_panel_grid <- function(time_points, reps = 100L) {
  if (!(time_points %in% c(25L, 100L))) {
    stop("Matched revision grid supports TimeP 25 or 100.", call. = FALSE)
  }
  scenarios <- revision_scenarios(dt = 0.4)
  design <- expand.grid(
    scenario = scenarios$scenario,
    N = c(50L, 100L),
    Error_sd = c(0.125, 0.25, 0.50),
    Loadings = c(0.4, 0.6, 0.8),
    rep = seq_len(as.integer(reps)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  design <- revision_expand_scenarios(design, scenarios)
  design <- revision_add_common_panel_columns(
    design,
    time_points,
    paste0("revision_matched_t", time_points)
  )
  design$representation <- "linear_z"
  design <- revision_add_panel_identity(design, "matched")
  design[order(design$scenario, design$N, design$Error_sd, design$Loadings, design$rep), ]
}

revision_challenge_base_grid <- function(reps = 50L) {
  scenarios <- revision_scenarios(dt = 0.4)
  variants <- revision_challenge_variants()
  design <- expand.grid(
    scenario = scenarios$scenario,
    challenge_id = variants$challenge_id,
    rep = seq_len(as.integer(reps)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  design$N <- 100L
  design$Error_sd <- 0.25
  design$Loadings <- 0.6
  design <- revision_expand_scenarios(design, scenarios)
  variant_index <- match(design$challenge_id, variants$challenge_id)
  for (column in setdiff(names(variants), "challenge_id")) {
    design[[column]] <- variants[[column]][variant_index]
  }
  design <- revision_add_common_panel_columns(design, 100L, "revision_challenge")
  design$representation <- "linear_z"
  design$panel_key <- paste(
    revision_panel_keys(design, "challenge"),
    design$challenge_id,
    sep = "|"
  )
  design$challenge_pair_key <- revision_pair_keys(design, "challenge_variant_pair")
  design$seed_base <- vapply(
    design$challenge_pair_key,
    revision_seed_from_key,
    integer(1)
  )
  design
}

revision_challenge_panel_grid <- function(reps = 50L) {
  base <- revision_challenge_base_grid(reps)
  ordinary <- base[base$challenge_id != "softmax_bounded", , drop = FALSE]
  softmax <- base[base$challenge_id == "softmax_bounded", , drop = FALSE]
  softmax_raw <- softmax
  softmax_raw$representation <- "softmax_raw"
  softmax_clr <- softmax
  softmax_clr$representation <- "softmax_clr"
  combined <- rbind(ordinary, softmax_raw, softmax_clr)
  combined$representation_key <- paste(
    combined$panel_key,
    combined$representation,
    sep = "|"
  )
  combined[order(
    combined$scenario,
    combined$challenge_id,
    combined$rep,
    combined$representation
  ), ]
}

revision_empirical_cv_plan <- function(repo_root = ".") {
  candidates <- revision_empirical_candidates()
  axes <- revision_empirical_calibration_axes()
  training <- merge(
    expand.grid(
      fold = seq_len(5L),
      cv_tau = axes$tau,
      candidate_id = candidates$candidate_id,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    candidates,
    by = "candidate_id",
    sort = FALSE
  )
  training$training_clip_count <- 80L
  training$heldout_clip_count <- 20L
  training$training_panel_replicates <- 1L
  training$selection_rule <- "minimum_training_only_continuous_loss"
  training$calibration_selection_scope <- "training_fold_only"
  training$channel_intercept_scope <- "estimated_from_training_fold_only"
  training$alpha_selection_scope <- "training_fold_only"
  training$tau_selection_scope <- "training_fold_only"
  training$summarizer_selection_scope <- "training_fold_only"
  training$heldout_access_during_selection <- FALSE
  training$panel_key <- sprintf(
    "%s|empirical_cv|train|F%d|C%02d|TAU%.2f",
    REVISION_CONFIG_VERSION,
    training$fold,
    training$candidate_id,
    training$cv_tau
  )
  training$seed_base <- vapply(training$panel_key, revision_seed_from_key, integer(1))

  heldout <- expand.grid(
    fold = seq_len(5L),
    heldout_rep = seq_len(100L),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  heldout$training_clip_count <- 80L
  heldout$heldout_clip_count <- 20L
  heldout$selected_candidate_id <- NA_integer_
  heldout$selected_cv_tau <- NA_real_
  heldout$selected_calibration_regime <- NA_character_
  heldout$selected_alpha <- NA_real_
  heldout$selected_summarizer <- NA_character_
  heldout$selection_locked_before_heldout <- TRUE
  heldout$panel_key <- sprintf(
    "%s|empirical_cv|heldout|F%d|R%03d",
    REVISION_CONFIG_VERSION,
    heldout$fold,
    heldout$heldout_rep
  )
  heldout$seed_base <- vapply(heldout$panel_key, revision_seed_from_key, integer(1))

  list(
    candidates = candidates,
    training = training,
    heldout = heldout,
    training_only_fields = c(
      "eta",
      "zeta",
      "sigma_q",
      "dt",
      "channel_intercepts",
      "alpha",
      "tau",
      "summarizer"
    )
  )
}

revision_method_ids <- function(job_id) {
  if (grepl("dynega$", job_id)) {
    return("dynEGA")
  }
  if (grepl("graphicalvar", job_id)) {
    return("graphicalVAR")
  }
  if (grepl("gimme", job_id)) {
    return("gimme")
  }
  if (grepl("static_oracle$", job_id)) {
    return(c("static_EGA", "oracle_blocks"))
  }
  character()
}

revision_readout_specs <- function(method_ids) {
  if (length(method_ids) == 0L) {
    return(data.frame())
  }
  specs <- expand.grid(
    method_id = method_ids,
    community_algorithm = REVISION_COMMUNITY_ALGORITHMS,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  specs$primary_readout <- specs$community_algorithm == "louvain"
  specs$resolution <- ifelse(specs$primary_readout, 1.0, NA_real_)
  specs$partition_source <- "frozen_method_structure"
  specs$dynEGA_sensitivity_uni_method <- ifelse(
    specs$method_id == "dynEGA",
    "expand",
    NA
  )
  specs$correlation_louvain_sensitivity_resolution <- ifelse(
    specs$method_id == "dynEGA",
    0.95,
    NA_real_
  )
  specs
}

revision_job_catalog <- function() {
  data.frame(
    job_id = REVISION_JOB_IDS,
    stage = c(
      "empirical",
      rep("t100", 5L),
      rep("t25", 4L),
      rep("challenge", 3L),
      "aggregate",
      "publish",
      "manuscript"
    ),
    dependencies = c(
      "",
      "",
      "t100_reuse_audit",
      "t100_reuse_audit",
      "t100_reuse_audit",
      "t100_reuse_audit",
      rep("", 7L),
      paste(REVISION_JOB_IDS[seq_len(13L)], collapse = ";"),
      "aggregate_results",
      "rebuild_figures_tables"
    ),
    runner_kind = c(
      "planned_empirical_cv",
      "reuse_audit",
      "existing_core",
      "planned_static_oracle",
      "existing_core",
      "existing_core",
      "existing_core",
      "existing_core",
      "existing_core",
      "planned_static_oracle",
      "planned_challenge",
      "planned_challenge",
      "planned_challenge",
      "result_builder",
      "planned_rebuild",
      "planned_manuscript_integration"
    ),
    base_panels = c(
      14000L,
      21600L,
      rep(21600L, 8L),
      rep(4200L, 3L),
      0L,
      0L,
      0L
    ),
    representation_panels = c(
      14000L,
      21600L,
      rep(21600L, 8L),
      rep(4800L, 3L),
      0L,
      0L,
      0L
    ),
    structure_fits = c(
      0L,
      0L,
      21600L,
      43200L,
      21600L,
      21600L,
      21600L,
      21600L,
      21600L,
      43200L,
      4800L,
      4800L,
      9600L,
      0L,
      0L,
      0L
    ),
    community_readouts = c(
      0L,
      0L,
      64800L,
      129600L,
      64800L,
      64800L,
      64800L,
      64800L,
      64800L,
      129600L,
      14400L,
      14400L,
      28800L,
      0L,
      0L,
      0L
    ),
    stringsAsFactors = FALSE
  )
}

revision_job_row <- function(job_id) {
  revision_assert_job_id(job_id)
  catalog <- revision_job_catalog()
  catalog[catalog$job_id == job_id, , drop = FALSE]
}

revision_job_panels <- function(job_id, repo_root = ".") {
  revision_assert_job_id(job_id)
  if (identical(job_id, "empirical_cv")) {
    return(revision_empirical_cv_plan(repo_root))
  }
  if (grepl("^t100_", job_id)) {
    return(revision_matched_panel_grid(100L))
  }
  if (grepl("^t25_", job_id)) {
    return(revision_matched_panel_grid(25L))
  }
  if (grepl("^challenge_", job_id)) {
    return(revision_challenge_panel_grid())
  }
  NULL
}

revision_job_plan <- function(job_id, repo_root = ".") {
  job <- revision_job_row(job_id)
  methods <- revision_method_ids(job_id)
  list(
    job = job,
    methods = methods,
    readouts = revision_readout_specs(methods),
    panels = revision_job_panels(job_id, repo_root),
    config_hash = revision_default_config_hash(job_id),
    seed_origin = REVISION_SEED_ORIGIN
  )
}

revision_validate_empirical_plan <- function(plan) {
  if (nrow(plan$candidates) != 900L) {
    stop("Empirical CV must contain 900 latent calibration candidates.", call. = FALSE)
  }
  if (nrow(plan$training) != 13500L) {
    stop("Empirical CV must contain 13,500 training evaluations.", call. = FALSE)
  }
  if (nrow(plan$heldout) != 500L) {
    stop("Empirical CV must contain 500 held-out evaluations.", call. = FALSE)
  }
  regimes <- sort(unique(plan$candidates$calibration_regime))
  if (!identical(regimes, c("stable_negative_zeta", "unconstrained"))) {
    stop("Empirical CV must contain both calibration regimes.", call. = FALSE)
  }
  stable <- plan$candidates$calibration_regime == "stable_negative_zeta"
  if (any(plan$candidates$zeta[stable] >= 0)) {
    stop("Stable-negative-zeta candidates must enforce zeta < 0.", call. = FALSE)
  }
  scope_columns <- grep("_scope$", names(plan$training), value = TRUE)
  if (length(scope_columns) < 5L || any(vapply(
    plan$training[scope_columns],
    function(values) any(values != "training_fold_only" &
      values != "estimated_from_training_fold_only"),
    logical(1)
  ))) {
    stop("All empirical CV choices must be training-fold only.", call. = FALSE)
  }
  if (any(plan$training$heldout_access_during_selection)) {
    stop("Held-out data cannot be accessed during empirical selection.", call. = FALSE)
  }
  invisible(TRUE)
}

revision_validate_panel_plan <- function(job_id, plan) {
  panel_grid <- plan$panels
  expected <- plan$job$representation_panels[[1]]
  if (nrow(panel_grid) != expected) {
    stop(
      sprintf("%s has %d representation panels; expected %d.", job_id, nrow(panel_grid), expected),
      call. = FALSE
    )
  }
  base_count <- length(unique(panel_grid$panel_key))
  if (base_count != plan$job$base_panels[[1]]) {
    stop(
      sprintf("%s has %d base panels; expected %d.", job_id, base_count, plan$job$base_panels[[1]]),
      call. = FALSE
    )
  }
  expected_readouts <- nrow(panel_grid) * nrow(plan$readouts)
  if (expected_readouts != plan$job$community_readouts[[1]]) {
    stop(
      sprintf(
        "%s has %d planned readouts; expected %d.",
        job_id,
        expected_readouts,
        plan$job$community_readouts[[1]]
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_validate_job_plan <- function(plan) {
  job_id <- plan$job$job_id[[1]]
  if (identical(job_id, "empirical_cv")) {
    return(revision_validate_empirical_plan(plan$panels))
  }
  if (!is.null(plan$panels) && !identical(job_id, "t100_reuse_audit")) {
    return(revision_validate_panel_plan(job_id, plan))
  }
  if (identical(job_id, "t100_reuse_audit") && nrow(plan$panels) != 21600L) {
    stop("T100 reuse audit must address 21,600 expected panels.", call. = FALSE)
  }
  invisible(TRUE)
}

revision_core_scenarios <- function() {
  scenarios <- revision_scenarios(dt = 0.4)
  lapply(seq_len(nrow(scenarios)), function(index) {
    row <- scenarios[index, , drop = FALSE]
    list(
      scenario = row$scenario[[1]],
      scenario_family = row$scenario_family[[1]],
      scenario_mode = row$scenario_mode[[1]],
      scenario_nfac = as.integer(row$scenario_nfac[[1]]),
      Q2 = as.numeric(row$Q2[[1]]),
      eta = revision_parse_numeric_vector(row$eta[[1]]),
      zeta = revision_parse_numeric_vector(row$zeta[[1]]),
      sigma_q = revision_parse_numeric_vector(row$sigma_q[[1]]),
      factor_archetypes = revision_parse_character_vector(row$factor_archetypes[[1]]),
      active_gain = as.numeric(row$active_gain[[1]]),
      background_gain = as.numeric(row$background_gain[[1]]),
      dwell_min_frac = as.numeric(row$dwell_min_frac[[1]]),
      dwell_max_frac = as.numeric(row$dwell_max_frac[[1]]),
      switching = isTRUE(row$switching[[1]])
    )
  })
}

revision_core_panel_grid <- function(panel_grid) {
  core <- panel_grid
  core$eta <- lapply(core$eta, revision_parse_numeric_vector)
  core$zeta <- lapply(core$zeta, revision_parse_numeric_vector)
  core$sigma_q <- lapply(core$sigma_q, revision_parse_numeric_vector)
  core$factor_archetypes <- lapply(
    core$factor_archetypes,
    revision_parse_character_vector
  )
  core
}

revision_required_upstream_jobs <- function() {
  REVISION_JOB_IDS[seq_len(13L)]
}
