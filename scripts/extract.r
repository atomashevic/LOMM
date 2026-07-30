## ============================================================
## LOMM-DynEGA Pipeline: Simulation → Calibration → Production
## Golino / dynEGA latent oscillator measurement prototype (shareable version)
## ============================================================

## -----------------------------
## 0. Libraries
## -----------------------------
library(EGAnet)
library(dplyr)
library(tidyr)
library(purrr)
library(MASS)
library(ggplot2)

## ============================================================
## 1. LATENT OSCILLATOR SIMULATION + MEASUREMENT MODEL
## ============================================================

# Map Q2 "energy" to number of latent factors
map_n_factors_scalar <- function(Q2) {
  if (!is.finite(Q2) || is.na(Q2)) return(1L)
  
  nf <- if (Q2 < 1) {
    1
  } else if (Q2 < 2) {
    2
  } else if (Q2 < 3) {
    3
  } else if (Q2 < 5) {
    4
  } else if (Q2 < 7) {
    5
  } else if (Q2 < 10) {
    6
  } else if (Q2 < 15) {
    7
  } else if (Q2 < 25) {
    8
  } else if (Q2 < 40) {
    9
  } else if (Q2 < 70) {
    10
  } else if (Q2 < 120) {
    11
  } else {
    12
  }
  
  as.integer(nf)
}

# Q2 → loading strength (psychometric heuristic)
loading_fun <- function(Q2) {
  lam <- 0.75 / (1 + 0.10 * log1p(Q2))
  pmin(pmax(lam, 0.40), 0.85)
}

# Q2 → factor correlation (not used directly here, but kept for completeness)
corr_fun <- function(Q2) {
  rho <- 0.40 / (1 + 0.15 * log1p(Q2))
  pmin(pmax(rho, 0.10), 0.60)
}

# Recycle scalar factor parameters or validate factor-specific vectors
normalize_factor_parameter <- function(x, NFAC, name) {
  x <- as.numeric(x)
  if (length(x) == 1L) {
    x <- rep(x, NFAC)
  }
  if (length(x) != NFAC) {
    stop(sprintf("%s must have length 1 or NFAC (%d); got %d.", name, NFAC, length(x)), call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop(sprintf("%s contains non-finite values.", name), call. = FALSE)
  }
  x
}

# Latent noise innovations q_t for NFAC oscillator factors
generate_q_multi <- function(num_steps, NFAC, sigma_q, rho_q = 0, q_cov = NULL) {
  if (!is.null(q_cov)) {
    q_cov <- as.matrix(q_cov)
    if (!all(dim(q_cov) == c(NFAC, NFAC))) {
      stop("q_cov must be NFAC x NFAC.", call. = FALSE)
    }
    return(MASS::mvrnorm(
      n = num_steps,
      mu = rep(0, NFAC),
      Sigma = q_cov
    ))
  }

  sigma_vec <- normalize_factor_parameter(sigma_q, NFAC, "sigma_q")
  rho_min <- -1 / max(NFAC - 1, 1)
  rho_q <- min(max(as.numeric(rho_q), rho_min + 1e-6), 0.999)

  R <- matrix(rho_q, nrow = NFAC, ncol = NFAC)
  diag(R) <- 1

  D <- diag(sigma_vec, nrow = NFAC, ncol = NFAC)
  Sigma <- D %*% R %*% D

  MASS::mvrnorm(
    n = num_steps,
    mu = rep(0, NFAC),
    Sigma = Sigma
  )
}

# Generic acceleration for the second-order system
compute_accel <- function(x, dx, eta, zeta, noise) {
  eta * x + zeta * dx + noise
}

# Small utility: exponential of a 2x2 matrix via eigen decomposition
expm_2x2 <- function(M) {
  ev <- eigen(M)
  V  <- ev$vectors
  Vinv <- tryCatch(solve(V), error = function(e) MASS::ginv(V))
  Re(V %*% diag(exp(ev$values)) %*% Vinv)
}

# Precompute exact linear transition matrices for constant-coefficient system
build_exact_transition <- function(eta, zeta, dt) {
  A <- matrix(c(0, 1, eta, zeta), nrow = 2, byrow = TRUE)
  Phi <- expm_2x2(A * dt)
  
  # Handle singular A with pseudo-inverse fallback
  A_inv <- tryCatch(solve(A), error = function(e) MASS::ginv(A))
  B     <- matrix(c(0, 1), nrow = 2)
  Gamma <- (Phi - diag(2)) %*% A_inv %*% B
  
  list(Phi = Phi, Gamma = Gamma)
}

# One-step integrator dispatcher
integrate_step <- function(method, x_old, dx_old, noise, dt, eta, zeta, exact_cache = NULL) {
  accel_now <- compute_accel(x_old, dx_old, eta, zeta, noise)
  
  if (method == "euler_explicit") {
    dx_new <- dx_old + accel_now * dt
    x_new  <- x_old  + dx_old  * dt
    
  } else if (method == "euler_semi_implicit") {
    dx_new <- dx_old + accel_now * dt
    x_new  <- x_old  + dx_new   * dt
    
  } else if (method == "velocity_verlet") {
    x_new    <- x_old + dx_old * dt + 0.5 * accel_now * dt^2
    dx_half  <- dx_old + 0.5 * accel_now * dt
    accel_new <- compute_accel(x_new, dx_half, eta, zeta, noise)
    dx_new   <- dx_half + 0.5 * accel_new * dt
    
  } else if (method == "rk4") {
    f_accel <- function(x, dx) compute_accel(x, dx, eta, zeta, noise)
    
    k1_x <- dx_old
    k1_v <- f_accel(x_old, dx_old)
    
    k2_x <- dx_old + 0.5 * dt * k1_v
    k2_v <- f_accel(x_old + 0.5 * dt * k1_x, dx_old + 0.5 * dt * k1_v)
    
    k3_x <- dx_old + 0.5 * dt * k2_v
    k3_v <- f_accel(x_old + 0.5 * dt * k2_x, dx_old + 0.5 * dt * k2_v)
    
    k4_x <- dx_old + dt * k3_v
    k4_v <- f_accel(x_old + dt * k3_x, dx_old + dt * k3_v)
    
    x_new  <- x_old + (dt / 6) * (k1_x + 2 * k2_x + 2 * k3_x + k4_x)
    dx_new <- dx_old + (dt / 6) * (k1_v + 2 * k2_v + 2 * k3_v + k4_v)
    
  } else if (method == "linear_exact") {
    if (is.null(exact_cache)) {
      stop("Exact integrator requested but no transition cache supplied.")
    }
    state_old <- matrix(c(x_old, dx_old), ncol = 1)
    state_new <- exact_cache$Phi %*% state_old + exact_cache$Gamma * noise
    x_new  <- state_new[1, 1]
    dx_new <- state_new[2, 1]
    
  } else {
    stop(sprintf("Unknown integrator method: %s", method))
  }
  
  list(x = x_new, dx = dx_new)
}

# Simulate latent oscillator trajectories (second-order) for NFAC factors
# Equation: x'' = eta * x + zeta * x' + q
simulate_latent_multi <- function(dt, num_steps, NFAC, sigma_q, eta, zeta,
                                  integrator = "euler_explicit",
                                  rho_q = 0,
                                  q_cov = NULL,
                                  init_x_sd = NULL,
                                  init_dx_sd = NULL,
                                  burn_in = 0L) {
  requested_steps <- as.integer(num_steps)
  burn_in <- max(0L, as.integer(burn_in))
  total_steps <- requested_steps + burn_in

  x    <- matrix(0, nrow = total_steps, ncol = NFAC)
  dxdt <- matrix(0, nrow = total_steps, ncol = NFAC)
  eta_vec <- normalize_factor_parameter(eta, NFAC, "eta")
  zeta_vec <- normalize_factor_parameter(zeta, NFAC, "zeta")
  
  if (is.null(init_x_sd)) {
    x[1, ] <- 0.25
  } else {
    x[1, ] <- stats::rnorm(NFAC, mean = 0, sd = as.numeric(init_x_sd))
  }

  if (is.null(init_dx_sd)) {
    dxdt[1, ] <- 2
  } else {
    dxdt[1, ] <- stats::rnorm(NFAC, mean = 0, sd = as.numeric(init_dx_sd))
  }
  
  q_t <- generate_q_multi(
    num_steps = total_steps,
    NFAC = NFAC,
    sigma_q = sigma_q,
    rho_q = rho_q,
    q_cov = q_cov
  )
  
  exact_cache <- if (integrator == "linear_exact") {
    lapply(seq_len(NFAC), function(f) {
      build_exact_transition(eta_vec[[f]], zeta_vec[[f]], dt)
    })
  } else {
    NULL
  }
  
  if (total_steps > 1L) {
    for (i in 2:total_steps) {
      for (f in seq_len(NFAC)) {
        x_old  <- x[i - 1, f]
        dx_old <- dxdt[i - 1, f]
        noise_i <- q_t[i, f]

        step <- integrate_step(
          method      = integrator,
          x_old       = x_old,
          dx_old      = dx_old,
          noise       = noise_i,
          dt          = dt,
          eta         = eta_vec[[f]],
          zeta        = zeta_vec[[f]],
          exact_cache = if (is.null(exact_cache)) NULL else exact_cache[[f]]
        )

        x[i, f]    <- step$x
        dxdt[i, f] <- step$dx
      }
    }
  }

  if (burn_in > 0L) {
    x[(burn_in + 1L):total_steps, , drop = FALSE]
  } else {
    x[seq_len(requested_steps), , drop = FALSE]
  }
}

# Build measurement model and generate observables
generate_observables_multi <- function(latent, NFAC, NVAR,
                                       error, loadings = 0.8) {
  num_steps <- nrow(latent)
  n_items   <- NFAC * NVAR
  
  # Block loading matrix: each factor → NVAR items
  L <- matrix(0, nrow = n_items, ncol = NFAC)
  for (f in seq_len(NFAC)) {
    idx <- ((f - 1) * NVAR + 1):(f * NVAR)
    L[idx, f] <- loadings + runif(NVAR, -0.15, 0.15)
  }
  
  # Measurement noise covariance
  var_vec <- rep(error, n_items) + runif(n_items, -0.01, 0.01)
  Sigma   <- diag(var_vec, nrow = n_items, ncol = n_items)
  
  e <- MASS::mvrnorm(
    n   = num_steps,
    mu  = rep(0, n_items),
    Sigma = Sigma
  )
  
  obs <- latent %*% t(L) + e
  obs <- as.data.frame(obs)
  colnames(obs) <- paste0("Y", seq_len(n_items))
  
  obs
}

# One simulated panel: latent NFAC oscillator trajectories -> NFAC * NVAR items (smoothed)
simulate_video_multi <- function(dt, num_steps,
                                 NFAC, NVAR,
                                 eta, zeta,
                                 sigma_q,
                                 sd_observable,
                                 loadings,
                                 integrator = "euler_explicit",
                                 rho_q = 0,
                                 q_cov = NULL,
                                 init_x_sd = NULL,
                                 init_dx_sd = NULL,
                                 burn_in = 0L) {
  
  latent <- simulate_latent_multi(
    dt        = dt,
    num_steps = num_steps,
    NFAC      = NFAC,
    sigma_q   = sigma_q,
    eta       = eta,
    zeta      = zeta,
    integrator = integrator,
    rho_q = rho_q,
    q_cov = q_cov,
    init_x_sd = init_x_sd,
    init_dx_sd = init_dx_sd,
    burn_in = burn_in
  )
  
  obs <- generate_observables_multi(
    latent   = latent,
    NFAC     = NFAC,
    NVAR     = NVAR,
    error    = sd_observable,
    loadings = loadings
  )
  
  list(observables = obs, latent = latent)
}

## ============================================================
## 2. dynEGA WRAPPER WITH FIXED η, ζ
## ============================================================

run_dynEGA_network_fixed_params <- function(
    p,
    Q2,
    TimeP      = 250,
    N_videos   = 30,
    sigma_q    = 0.5,
    base_err   = 0.02,
    uni.method = "expand",
    cd.algorithm = "louvain",
    n_embed    = 25,
    dt         = NULL,
    eta_fixed,
    zeta_fixed,
    seed       = NULL,
    integrator = "euler_explicit",
    return_latent = FALSE,
    ncores = 12
) {
  if (!is.null(seed)) set.seed(seed)
  
  # map Q2 → NFAC, items per factor
  NFAC <- map_n_factors_scalar(Q2)
  NVAR <- p
  
  if (is.null(dt)) {
    dt <- ifelse(TimeP <= 150, 0.4, 0.25)
  }
  
  loadings <- loading_fun(Q2)
  meas_err <- base_err
  
  videos <- NULL
  latent_list <- list()
  
  for (i in seq_len(N_videos)) {
    vid <- simulate_video_multi(
      dt             = dt,
      num_steps      = TimeP,
      NFAC           = NFAC,
      NVAR           = NVAR,
      eta            = eta_fixed,
      zeta           = zeta_fixed,
      sigma_q        = sigma_q,
      sd_observable  = meas_err,
      loadings       = loadings,
      integrator     = integrator
    )
    
    dat_i <- vid$observables
    dat_i$id <- i
    videos <- rbind(videos, dat_i)
    if (return_latent) {
      latent_list[[i]] <- vid$latent
    }
  }
  
  sim.data     <- videos
  id_col_index <- ncol(sim.data)
  
  dy <- EGAnet::dynEGA(
    data            = sim.data,
    n.embed         = n_embed,
    tau             = 1,
    delta           = 1,        # MUST be >= 1 (EGAnet constraint)
    id              = id_col_index,
    model           = "glasso",
    algorithm       = cd.algorithm,
    uni.method      = uni.method,
    use.derivatives = 1,
    ncores          = ncores
  )
  
  list(
    dynEGA_obj = dy,
    NFAC_true  = NFAC,
    NVAR       = NVAR,
    Q2         = Q2,
    p          = p,
    n_embed    = n_embed,
    dt_used    = dt,
    data       = sim.data,
    true_eta   = eta_fixed,
    true_zeta  = zeta_fixed,
    integrator = integrator,
    latents    = if (return_latent) latent_list else NULL
  )
}

## ============================================================
## 3. LATENT OSCILLATOR PARAMETER ESTIMATION FROM TIME SERIES
## ============================================================
## NOTE: This version uses a simple finite-difference scheme on the
## observed data (x, x', x'') to estimate a *global* (eta, zeta)
## via OLS: x'' ~ eta * x + zeta * x'.
## It ignores dynEGA_obj internals, but keeps the same API and
## return structure you used in your calibration code.

estimate_dlo_from_dynEGA <- function(dynEGA_obj,
                                     data,
                                     dt      = 0.25,
                                     dt_eta  = 0.25,
                                     min_valid = 30,
                                     verbose = FALSE) {
  
  # Identify id column (assume last if not explicitly named "id")
  if ("id" %in% names(data)) {
    id_var <- "id"
  } else {
    id_var <- names(data)[ncol(data)]
  }
  
  # Wide → per-id processing (variables = all columns except id)
  vars <- setdiff(names(data), id_var)
  
  all_rows <- list()
  idx <- 1
  
  for (id_val in unique(data[[id_var]])) {
    dat_i <- data[data[[id_var]] == id_val, vars, drop = FALSE]
    T_i   <- nrow(dat_i)
    if (T_i < (min_valid + 2L)) next
    
    # For each variable, standardize and compute derivatives
    for (v in vars) {
      x_raw <- as.numeric(dat_i[[v]])
      if (all(is.na(x_raw))) next
      
      x_std <- as.numeric(scale(x_raw))
      
      # central differences (ignore first & last timepoint)
      # x'(t) ≈ [x(t+1) - x(t-1)] / (2 dt)
      # x''(t) ≈ [x(t+1) - 2 x(t) + x(t-1)] / (dt^2)
      x_lag  <- x_std[1:(T_i - 2L)]
      x_mid  <- x_std[2:(T_i - 1L)]
      x_lead <- x_std[3:T_i]
      
      dx  <- (x_lead - x_lag) / (2 * dt)
      d2x <- (x_lead - 2 * x_mid + x_lag) / (dt^2)
      
      df_v <- data.frame(
        id     = id_val,
        var    = v,
        x      = x_mid,
        dx     = dx,
        d2x    = d2x
      )
      all_rows[[idx]] <- df_v
      idx <- idx + 1L
    }
  }
  
  if (length(all_rows) == 0L) {
    stop("No valid time series segments with enough points to estimate latent oscillator parameters.")
  }
  
  big_df <- do.call(rbind, all_rows)
  n_obs  <- nrow(big_df)
  
  if (verbose) {
    message("Total derivative rows used for latent oscillator regression: ", n_obs)
  }
  
  # Global regression: x'' = eta * x + zeta * x' + noise
  fit <- lm(d2x ~ x + dx, data = big_df)
  coefs <- coef(fit)
  
  eta_hat  <- unname(coefs["x"])
  zeta_hat <- unname(coefs["dx"])
  R2       <- summary(fit)$r.squared
  
  # Summaries (keep structure compatible with your code)
  eta_summary <- data.frame(
    method      = "dynEGA_direct",
    eta_mean    = eta_hat,
    eta_median  = eta_hat,
    eta_sd      = NA_real_,
    eta_25      = NA_real_,
    eta_75      = NA_real_,
    zeta_mean   = zeta_hat,
    zeta_median = zeta_hat,
    zeta_sd     = NA_real_,
    zeta_25     = NA_real_,
    zeta_75     = NA_real_,
    R2_mean     = R2,
    n_estimates = n_obs
  )
  
  list(
    eta = list(
      summary = eta_summary,
      fit     = fit
    ),
    zeta_fixed = zeta_hat,
    dt         = dt,
    dt_eta     = dt_eta
  )
}

## ============================================================
## 4. CALIBRATION: GRID OF TRUE (η, ζ) → RAW (η_raw, ζ_raw)
## ============================================================

calibrate_dlo_scaling <- function(
    eta_vals   = c(-0.05, -0.10, -0.20, -0.40),
    zeta_vals  = c( 0.10,  0.20,  0.40,  0.80),
    p          = 4,
    Q2         = 6,
    TimeP      = 250,
    N_videos   = 100,
    sigma_q    = 0.25,
    base_err   = 0.05,
    n_embed    = 100,
    dt_ref     = 0.25,
    seed_start = 202500
) {
  set.seed(seed_start)
  
  calib_rows <- list()
  row_id <- 1
  
  for (eta_true in eta_vals) {
    for (zeta_true in zeta_vals) {
      message(sprintf("Calibrating eta=%.2f, zeta=%.2f ...",
                      eta_true, zeta_true))
      
      sim <- run_dynEGA_network_fixed_params(
        p         = p,
        Q2        = Q2,
        n_embed   = n_embed,
        sigma_q   = sigma_q,
        base_err  = base_err,
        TimeP     = TimeP,
        N_videos  = N_videos,
        eta_fixed = eta_true,
        zeta_fixed = zeta_true,
        seed      = seed_start + row_id
      )
      
      # Latent oscillator estimation in "production" style
      est <- estimate_dlo_from_dynEGA(
        dynEGA_obj = sim$dynEGA_obj,
        data       = sim$data,
        dt         = dt_ref,
        dt_eta     = dt_ref,
        verbose    = FALSE
      )
      
      eta_raw  <- est$eta$summary$eta_median
      zeta_raw <- est$zeta_fixed
      
      calib_rows[[row_id]] <- data.frame(
        eta_true  = eta_true,
        zeta_true = zeta_true,
        eta_raw   = eta_raw,
        zeta_raw  = zeta_raw
      )
      row_id <- row_id + 1
    }
  }
  
  calib_df <- do.call(rbind, calib_rows)
  
  # --- Learn scaling factors ----------------------------------
  # 1) Eta: factor per |eta_raw| bin (small / medium / large)
  calib_df <- calib_df %>%
    dplyr::mutate(
      abs_eta_raw = abs(eta_raw),
      eta_bin = cut(
        abs_eta_raw,
        breaks = c(0, 0.08, 0.12, Inf),
        labels = c("small", "medium", "large"),
        include.lowest = TRUE
      ),
      # avoid dividing by ~0
      eta_ratio = ifelse(abs(eta_raw) > 1e-6, eta_true / eta_raw, NA_real_)
    )
  
  eta_scaling <- calib_df %>%
    dplyr::group_by(eta_bin) %>%
    dplyr::summarise(
      eta_factor = mean(eta_ratio, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  # 2) Zeta: simple global scaling factor
  calib_df <- calib_df %>%
    dplyr::mutate(
      zeta_ratio = ifelse(abs(zeta_raw) > 1e-6, zeta_true / zeta_raw, NA_real_)
    )
  
  zeta_factor <- mean(calib_df$zeta_ratio, na.rm = TRUE)
  
  list(
    calib_df    = calib_df,
    eta_scaling = eta_scaling,
    zeta_factor = zeta_factor,
    dt_ref      = dt_ref,
    grid_eta    = eta_vals,
    grid_zeta   = zeta_vals
  )
}

## ============================================================
## 5. HELPER: PICK ETA SCALING FACTOR BY BIN
## ============================================================

pick_eta_factor <- function(eta_raw, eta_scaling_table) {
  abs_val <- abs(eta_raw)
  bin <- cut(
    abs_val,
    breaks = c(0, 0.08, 0.12, Inf),
    labels = c("small", "medium", "large"),
    include.lowest = TRUE
  )
  bin <- as.character(bin)
  
  f <- eta_scaling_table$eta_factor[match(bin, eta_scaling_table$eta_bin)]
  
  # fallback: if something is NA, use global mean factor
  if (any(is.na(f))) {
    f[is.na(f)] <- mean(eta_scaling_table$eta_factor, na.rm = TRUE)
  }
  f
}

## ============================================================
## 6. PRODUCTION ESTIMATOR (USING CALIBRATION)
## ============================================================

estimate_dlo_parameters_production <- function(dynEGA_obj, data, 
                                               calib_prod,
                                               confidence_intervals = TRUE) {
  
  cat("=== LATENT OSCILLATOR PARAMETER ESTIMATION FROM DYNEGA (Production, calibrated) ===\n\n")
  
  dt_ref <- calib_prod$dt_ref
  
  # Step 1: Estimate at reference dt
  res_base <- estimate_dlo_from_dynEGA(
    dynEGA_obj = dynEGA_obj,
    data       = data,
    dt         = dt_ref,
    dt_eta     = dt_ref,
    verbose    = FALSE
  )
  
  eta_raw  <- res_base$eta$summary$eta_median
  zeta_raw <- res_base$zeta_fixed
  
  # Step 2: Apply *empirical* corrections learned from calib_prod
  eta_factor  <- pick_eta_factor(eta_raw, calib_prod$eta_scaling)
  zeta_factor <- calib_prod$zeta_factor
  
  eta_corrected  <- eta_raw  * eta_factor
  zeta_corrected <- zeta_raw * zeta_factor
  
  # Step 3: Uncertainty (heuristic)
  if (confidence_intervals) {
    eta_ci_lower <- eta_corrected * 0.85
    eta_ci_upper <- eta_corrected * 1.15
    
    zeta_ci_lower <- zeta_corrected * 0.5
    zeta_ci_upper <- zeta_corrected * 1.5
  } else {
    eta_ci_lower <- eta_ci_upper <- NA_real_
    zeta_ci_lower <- zeta_ci_upper <- NA_real_
  }
  
  # Step 4: Dynamic-regime checks
  warnings <- character()
  if (eta_corrected > 0) {
    warnings <- c(warnings, "Eta > 0: restoring force is absent; trajectory may grow")
  }
  if (zeta_corrected > 0) {
    warnings <- c(warnings, "Zeta > 0: positive velocity feedback can sustain or amplify change")
  }
  
  # Step 5: Compile results
  results <- list(
    estimates = data.frame(
      parameter = c("eta", "zeta"),
      raw       = c(eta_raw,  zeta_raw),
      corrected = c(eta_corrected, zeta_corrected),
      ci_lower  = c(eta_ci_lower, zeta_ci_lower),
      ci_upper  = c(eta_ci_upper, zeta_ci_upper)
    ),
    dynamics_type = if (eta_corrected < 0 && zeta_corrected < 0) {
      "Restoring with damped velocity feedback"
    } else if (eta_corrected < 0 && zeta_corrected == 0) {
      "Restoring without velocity feedback"
    } else if (eta_corrected < 0 && zeta_corrected > 0) {
      "Restoring with positive velocity feedback"
    } else {
      "Potentially unstable"
    },
    quality_metrics = list(
      eta_confidence  = "Heuristic (±15%)",
      zeta_confidence = "Heuristic (±50%)",
      n_estimates     = res_base$eta$summary$n_estimates
    ),
    warnings = warnings,
    method   = "Calibrated dynEGA with empirical scaling",
    reference = sprintf("dt = %.2f; scaling learned from calibration grid", dt_ref)
  )
  
  cat("PARAMETER ESTIMATES:\n")
  cat(sprintf("Eta:  %.3f [%.3f, %.3f]\n", 
              eta_corrected, eta_ci_lower, eta_ci_upper))
  cat(sprintf("Zeta: %.3f [%.3f, %.3f]\n", 
              zeta_corrected, zeta_ci_lower, zeta_ci_upper))
  cat(sprintf("\nDynamics type: %s\n", results$dynamics_type))
  
  if (length(warnings) > 0) {
    cat("\nWarnings:\n")
    for (w in warnings) cat(paste("-", w, "\n"))
  }
  
  invisible(results)
}

## ============================================================
## 7. BENCHMARK HELPERS (INTEGRATORS, CORRELATIONS)
## ============================================================

compute_latent_obs_correlation <- function(latents, data, NVAR) {
  if (is.null(latents) || length(latents) == 0L) return(NA_real_)
  
  id_var <- if ("id" %in% names(data)) "id" else names(data)[ncol(data)]
  obs_cols <- setdiff(names(data), id_var)
  NFAC <- ncol(latents[[1]])
  
  cor_vals <- c()
  
  for (i in seq_along(latents)) {
    latent_i <- latents[[i]]
    obs_i <- data[data[[id_var]] == i, obs_cols, drop = FALSE]
    len <- min(nrow(latent_i), nrow(obs_i))
    if (len < 3L) next
    
    latent_i <- latent_i[seq_len(len), , drop = FALSE]
    obs_mat  <- as.matrix(obs_i[seq_len(len), , drop = FALSE])
    
    for (f in seq_len(NFAC)) {
      cols_f <- ((f - 1) * NVAR + 1):(f * NVAR)
      for (j in cols_f) {
        r_val <- suppressWarnings(
          cor(latent_i[, f], obs_mat[, j], use = "pairwise.complete.obs")
        )
        if (is.finite(r_val)) cor_vals <- c(cor_vals, r_val)
      }
    }
  }
  
  if (length(cor_vals) == 0L) return(NA_real_)
  mean(abs(cor_vals), na.rm = TRUE)
}

benchmark_integrators <- function(
    integrators = c("euler_explicit", "euler_semi_implicit",
                    "velocity_verlet", "rk4", "linear_exact"),
    eta_true    = -0.5,
    zeta_true   = 0.2,
    p           = 4,
    Q2          = 6,
    TimeP       = 200,
    N_videos    = 20,
    sigma_q     = 0.5,
    base_err    = 0.02,
    n_embed     = 35,
    seed        = 202700,
    ncores      = 12) {
  
  rows <- vector("list", length(integrators))
  
  for (idx in seq_along(integrators)) {
    method <- integrators[[idx]]
    
    sim <- run_dynEGA_network_fixed_params(
      p           = p,
      Q2          = Q2,
      n_embed     = n_embed,
      sigma_q     = sigma_q,
      base_err    = base_err,
      TimeP       = TimeP,
      N_videos    = N_videos,
      eta_fixed   = eta_true,
      zeta_fixed  = zeta_true,
      seed        = seed + idx * 5L,
      integrator  = method,
      return_latent = TRUE,
      ncores      = ncores
    )
    
    latent_obs_cor <- compute_latent_obs_correlation(
      latents = sim$latents,
      data    = sim$data,
      NVAR    = sim$NVAR
    )
    
    est <- estimate_dlo_from_dynEGA(
      dynEGA_obj = sim$dynEGA_obj,
      data       = sim$data,
      dt         = sim$dt_used,
      dt_eta     = sim$dt_used,
      verbose    = FALSE
    )
    
    eta_hat  <- est$eta$summary$eta_median
    zeta_hat <- est$zeta_fixed
    
    rows[[idx]] <- data.frame(
      integrator      = method,
      eta_hat         = eta_hat,
      zeta_hat        = zeta_hat,
      eta_error       = eta_hat - eta_true,
      zeta_error      = zeta_hat - zeta_true,
      latent_obs_cor  = latent_obs_cor,
      n_estimates     = est$eta$summary$n_estimates,
      est_factors     = sim$dynEGA_obj$dynEGA$population$n.dim,
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(rows)
}

# Plot a small panel of integrator performance metrics and save to disk
plot_benchmark_integrators <- function(
    bench_df,
    output_dir = ".",
    prefix = "integrator_benchmark",
    width = 7,
    height = 4) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  bench_df <- bench_df %>%
    dplyr::mutate(integrator = factor(integrator, levels = integrator))
  
  # Error bars for eta / zeta
  err_long <- bench_df %>%
    dplyr::select(integrator, eta_error, zeta_error) %>%
    tidyr::pivot_longer(cols = c(eta_error, zeta_error), names_to = "parameter", values_to = "error")
  
  p_err <- ggplot(err_long, aes(x = integrator, y = error, fill = parameter)) +
    geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = c("#3b82f6", "#f97316"), labels = c("eta", "zeta")) +
    labs(title = "Parameter Error by Integrator", y = "Error (estimate - truth)", x = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  # Raw estimates
  hat_long <- bench_df %>%
    dplyr::select(integrator, eta_hat, zeta_hat) %>%
    tidyr::pivot_longer(cols = c(eta_hat, zeta_hat), names_to = "parameter", values_to = "estimate")
  
  p_hat <- ggplot(hat_long, aes(x = integrator, y = estimate, color = parameter, group = parameter)) +
    geom_hline(yintercept = c(-0.5, 0.2), linetype = "dashed", color = c("#3b82f6", "#f97316"), linewidth = 0.5) +
    geom_point(size = 2.4) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = c("#3b82f6", "#f97316"), labels = c("eta", "zeta")) +
    labs(title = "Estimated Parameters vs. Truth", y = "Estimate", x = NULL, color = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  # Latent-observable correlation
  p_cor <- ggplot(bench_df, aes(x = integrator, y = latent_obs_cor)) +
    geom_hline(yintercept = 1, color = "gray60", linetype = "dashed", linewidth = 0.5) +
    geom_col(fill = "#10b981", width = 0.6) +
    labs(title = "Latent–Observable Coupling", y = "Mean |cor|", x = NULL) +
    coord_cartesian(ylim = c(0.95, 1.01)) +
    theme_minimal(base_size = 12)
  
  files <- list(
    errors   = file.path(output_dir, paste0(prefix, "_errors.png")),
    estimates = file.path(output_dir, paste0(prefix, "_estimates.png")),
    coupling = file.path(output_dir, paste0(prefix, "_coupling.png"))
  )
  
  ggsave(files$errors, p_err, width = width, height = height, dpi = 160)
  ggsave(files$estimates, p_hat, width = width, height = height, dpi = 160)
  ggsave(files$coupling, p_cor, width = width, height = height, dpi = 160)
  
  list(
    plots = list(errors = p_err, estimates = p_hat, coupling = p_cor),
    files = files
  )
}

## ============================================================
## 8. DYNEGA SCORE-BASED LATENT OSCILLATOR ESTIMATION (PROTOTYPE)
## ============================================================

find_first_named <- function(obj, target) {
  if (is.null(obj) || !is.list(obj)) return(NULL)
  if (!is.null(obj[[target]])) return(obj[[target]])
  for (el in obj) {
    if (is.list(el)) {
      res <- find_first_named(el, target)
      if (!is.null(res)) return(res)
    }
  }
  NULL
}

compute_net_scores_safe <- function(dynEGA_obj, data_no_id) {
  net <- find_first_named(dynEGA_obj, "network")
  wc  <- find_first_named(dynEGA_obj, "wc")
  
  if (is.null(net) || is.null(wc)) return(NULL)
  
  wc_vec <- if (is.null(dim(wc))) as.vector(wc) else as.vector(wc[1, ])
  colnames(net) <- colnames(data_no_id)
  names(wc_vec) <- colnames(data_no_id)
  
  tryCatch(
    EGAnet::net.scores(
      data = data_no_id,
      A    = net,
      wc   = wc_vec
    )$scores$std.scores,
    error = function(e) {
      warning("net.scores failed: ", conditionMessage(e))
      NULL
    }
  )
}

estimate_dlo_from_net_scores <- function(dynEGA_obj,
                                         data,
                                         dt = 0.25,
                                         min_valid = 30,
                                         verbose = FALSE) {
  id_var <- if ("id" %in% names(data)) "id" else names(data)[ncol(data)]
  obs_cols <- setdiff(names(data), id_var)
  data_no_id <- data[, obs_cols, drop = FALSE]
  
  scores <- compute_net_scores_safe(dynEGA_obj, data_no_id)
  if (is.null(scores)) {
    stop("Could not compute net scores for dynEGA_obj.")
  }
  
  if (nrow(scores) != nrow(data)) {
    stop("net.scores rows do not match data rows.")
  }
  
  id_vector <- data[[id_var]]
  all_rows <- list()
  idx <- 1L
  
  for (id_val in unique(id_vector)) {
    rows <- which(id_vector == id_val)
    scores_i <- scores[rows, , drop = FALSE]
    T_i <- nrow(scores_i)
    if (T_i < (min_valid + 2L)) next
    
    for (f in seq_len(ncol(scores_i))) {
      x_raw <- as.numeric(scores_i[, f])
      if (all(is.na(x_raw))) next
      
      x_std <- as.numeric(scale(x_raw))
      
      x_lag  <- x_std[1:(T_i - 2L)]
      x_mid  <- x_std[2:(T_i - 1L)]
      x_lead <- x_std[3:T_i]
      
      dx  <- (x_lead - x_lag) / (2 * dt)
      d2x <- (x_lead - 2 * x_mid + x_lag) / (dt^2)
      
      df_v <- data.frame(
        id  = id_val,
        dim = f,
        x   = x_mid,
        dx  = dx,
        d2x = d2x
      )
      all_rows[[idx]] <- df_v
      idx <- idx + 1L
    }
  }
  
  if (length(all_rows) == 0L) {
    stop("No valid score series to estimate latent oscillator parameters.")
  }
  
  big_df <- do.call(rbind, all_rows)
  if (verbose) {
    message("Total score-derivative rows: ", nrow(big_df))
  }
  
  fit <- lm(d2x ~ x + dx, data = big_df)
  coefs <- coef(fit)
  
  eta_hat  <- unname(coefs["x"])
  zeta_hat <- unname(coefs["dx"])
  R2       <- summary(fit)$r.squared
  
  data.frame(
    eta_hat      = eta_hat,
    zeta_hat     = zeta_hat,
    R2           = R2,
    n_rows       = nrow(big_df),
    n_dimensions = ncol(scores)
  )
}

compute_latent_score_alignment <- function(scores, latents, id_vector) {
  if (is.null(latents) || is.null(scores)) return(NA_real_)
  if (length(latents) == 0L) return(NA_real_)
  
  cor_vals <- c()
  for (i in seq_along(latents)) {
    rows <- which(id_vector == i)
    if (length(rows) == 0L) next
    scores_i <- scores[rows, , drop = FALSE]
    latent_i <- latents[[i]]
    len <- min(nrow(scores_i), nrow(latent_i))
    if (len < 3L) next
    scores_i <- scores_i[seq_len(len), , drop = FALSE]
    latent_i <- latent_i[seq_len(len), , drop = FALSE]
    cor_mat <- cor(latent_i, scores_i, use = "pairwise.complete.obs")
    if (nrow(cor_mat) == 0L || ncol(cor_mat) == 0L) next
    diag_cor <- apply(abs(cor_mat), 1, max, na.rm = TRUE)
    cor_vals <- c(cor_vals, diag_cor)
  }
  if (length(cor_vals) == 0L) return(NA_real_)
  mean(cor_vals, na.rm = TRUE)
}

benchmark_integrators_with_scores <- function(
    integrators = c("euler_explicit", "euler_semi_implicit",
                    "velocity_verlet", "rk4", "linear_exact"),
    eta_true    = -0.5,
    zeta_true   = 0.2,
    p           = 4,
    Q2          = 6,
    TimeP       = 200,
    N_videos    = 20,
    sigma_q     = 0.5,
    base_err    = 0.02,
    n_embed     = 35,
    seed        = 202700,
    ncores      = 12) {
  
  rows <- vector("list", length(integrators))
  
  for (idx in seq_along(integrators)) {
    method <- integrators[[idx]]
    
    sim <- run_dynEGA_network_fixed_params(
      p            = p,
      Q2           = Q2,
      n_embed      = n_embed,
      sigma_q      = sigma_q,
      base_err     = base_err,
      TimeP        = TimeP,
      N_videos     = N_videos,
      eta_fixed    = eta_true,
      zeta_fixed   = zeta_true,
      seed         = seed + idx * 5L,
      integrator   = method,
      return_latent = TRUE,
      ncores       = ncores
    )
    
    latent_obs_cor <- compute_latent_obs_correlation(
      latents = sim$latents,
      data    = sim$data,
      NVAR    = sim$NVAR
    )
    
    est_raw <- estimate_dlo_from_dynEGA(
      dynEGA_obj = sim$dynEGA_obj,
      data       = sim$data,
      dt         = sim$dt_used,
      dt_eta     = sim$dt_used,
      verbose    = FALSE
    )
    
    # Scores-based estimation
    id_var <- if ("id" %in% names(sim$data)) "id" else names(sim$data)[ncol(sim$data)]
    scores <- compute_net_scores_safe(sim$dynEGA_obj, sim$data[, setdiff(names(sim$data), id_var), drop = FALSE])
    
    est_scores <- tryCatch(
      estimate_dlo_from_net_scores(
        dynEGA_obj = sim$dynEGA_obj,
        data       = sim$data,
        dt         = sim$dt_used,
        min_valid  = 30,
        verbose    = FALSE
      ),
      error = function(e) NULL
    )
    
    score_latent_cor <- compute_latent_score_alignment(
      scores    = scores,
      latents   = sim$latents,
      id_vector = sim$data[[id_var]]
    )
    
    rows[[idx]] <- data.frame(
      integrator           = method,
      eta_hat_raw          = est_raw$eta$summary$eta_median,
      zeta_hat_raw         = est_raw$zeta_fixed,
      eta_error_raw        = est_raw$eta$summary$eta_median - eta_true,
      zeta_error_raw       = est_raw$zeta_fixed - zeta_true,
      eta_hat_scores       = if (!is.null(est_scores)) est_scores$eta_hat else NA_real_,
      zeta_hat_scores      = if (!is.null(est_scores)) est_scores$zeta_hat else NA_real_,
      eta_error_scores     = if (!is.null(est_scores)) est_scores$eta_hat - eta_true else NA_real_,
      zeta_error_scores    = if (!is.null(est_scores)) est_scores$zeta_hat - zeta_true else NA_real_,
      latent_obs_cor       = latent_obs_cor,
      latent_score_cor     = score_latent_cor,
      n_estimates_raw      = est_raw$eta$summary$n_estimates,
      n_estimates_scores   = if (!is.null(est_scores)) est_scores$n_rows else NA_integer_,
      est_factors          = sim$dynEGA_obj$dynEGA$population$n.dim,
      score_dimensions     = if (!is.null(est_scores)) est_scores$n_dimensions else NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(rows)
}

## ============================================================
## 9. SIMPLE STATE-SPACE FIT USING DYNEGA COMMUNITIES
## ============================================================

build_membership <- function(dynEGA_obj, data_no_id) {
  wc  <- find_first_named(dynEGA_obj, "wc")
  if (is.null(wc)) return(NULL)
  wc_vec <- if (is.null(dim(wc))) as.vector(wc) else as.vector(wc[1, ])
  names(wc_vec) <- colnames(data_no_id)
  wc_vec
}

kalman_nll_dlo <- function(par, Y, dt, loadings, ids = NULL) {
  eta <- par[1]
  zeta <- par[2]
  q_var <- exp(par[3])
  r_var <- exp(par[4])
  
  F <- matrix(c(1, dt,
                eta * dt, 1 + zeta * dt), nrow = 2, byrow = TRUE)
  eps <- 1e-6
  Q <- matrix(c(eps, 0,
                0, q_var + eps), nrow = 2, byrow = TRUE)
  
  H <- cbind(loadings, rep(0, length(loadings)))
  R <- diag(r_var + eps, nrow = length(loadings))
  
  reset_state <- function() {
    list(m = c(0, 0), C = diag(1e3, 2))
  }
  st <- reset_state()
  prev_id <- if (is.null(ids)) NA else ids[1]
  
  nll <- 0
  for (t in seq_len(nrow(Y))) {
    if (!is.null(ids) && !is.na(prev_id) && ids[t] != prev_id) {
      st <- reset_state()
      prev_id <- ids[t]
    }
    y_t <- as.numeric(Y[t, ])
    if (any(!is.finite(y_t))) next
    
    a <- F %*% st$m
    P <- F %*% st$C %*% t(F) + Q
    
    S <- H %*% P %*% t(H) + R
    S_inv <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
    innov <- y_t - as.numeric(H %*% a)
    K <- P %*% t(H) %*% S_inv
    st$m <- a + K %*% innov
    st$C <- (diag(2) - K %*% H) %*% P
    
    logdet <- tryCatch(as.numeric(determinant(S, logarithm = TRUE)$modulus), error = function(e) NA_real_)
    if (!is.finite(logdet)) logdet <- 50
    nll <- nll + 0.5 * (logdet + t(innov) %*% S_inv %*% innov)
  }
  as.numeric(nll)
}

estimate_dlo_state_space <- function(dynEGA_obj,
                                     data,
                                     dt = 0.25,
                                     min_valid = 30,
                                     verbose = FALSE) {
  id_var <- if ("id" %in% names(data)) "id" else names(data)[ncol(data)]
  obs_cols <- setdiff(names(data), id_var)
  data_no_id <- data[, obs_cols, drop = FALSE]
  
  membership <- build_membership(dynEGA_obj, data_no_id)
  if (is.null(membership)) stop("No community membership found in dynEGA_obj.")
  comms <- sort(unique(membership))
  
  eta_hats <- c()
  zeta_hats <- c()
  n_used <- 0L
  
  for (comm in comms) {
    vars <- names(membership)[membership == comm]
    if (length(vars) < 2L) next
    
    Y <- data_no_id[, vars, drop = FALSE]
    ids <- data[[id_var]]
    
    # standardize per id to control scaling
    Y_mat <- matrix(NA_real_, nrow = nrow(Y), ncol = ncol(Y))
    colnames(Y_mat) <- colnames(Y)
    for (id_val in unique(ids)) {
      idx_id <- which(ids == id_val)
      scaled_block <- scale(Y[idx_id, , drop = FALSE])
      scaled_block[is.na(scaled_block)] <- 0
      Y_mat[idx_id, ] <- scaled_block
    }
    
    if (nrow(Y_mat) < min_valid) next
    
    loadings <- rep(1, length(vars))
    
    par0 <- c(-0.2, 0.2, log(0.1), log(0.05))
    fit <- tryCatch(
      optim(
        par = par0,
        fn  = kalman_nll_dlo,
        Y   = Y_mat,
        dt  = dt,
        loadings = loadings,
        ids = ids,
        method = "BFGS",
        control = list(maxit = 200, reltol = 1e-6)
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit) || any(!is.finite(fit$par))) next
    
    eta_hats <- c(eta_hats, fit$par[1])
    zeta_hats <- c(zeta_hats, fit$par[2])
    n_used <- n_used + nrow(Y_mat)
  }
  
  if (length(eta_hats) == 0L) {
    return(data.frame(
      eta_hat_mean = NA_real_,
      eta_hat_median = NA_real_,
      zeta_hat_mean = NA_real_,
      zeta_hat_median = NA_real_,
      n_comms = 0,
      n_obs = 0
    ))
  }
  
  data.frame(
    eta_hat_mean  = mean(eta_hats),
    eta_hat_median = median(eta_hats),
    zeta_hat_mean = mean(zeta_hats),
    zeta_hat_median = median(zeta_hats),
    n_comms = length(eta_hats),
    n_obs   = n_used
  )
}

benchmark_state_space <- function(
    integrators = c("euler_explicit", "euler_semi_implicit",
                    "velocity_verlet", "rk4", "linear_exact"),
    eta_true    = -0.5,
    zeta_true   = 0.2,
    p           = 4,
    Q2          = 6,
    TimeP       = 200,
    N_videos    = 20,
    sigma_q     = 0.5,
    base_err    = 0.02,
    n_embed     = 35,
    seed        = 202750,
    ncores      = 12) {
  
  rows <- vector("list", length(integrators))
  
  for (idx in seq_along(integrators)) {
    method <- integrators[[idx]]
    
    sim <- run_dynEGA_network_fixed_params(
      p            = p,
      Q2           = Q2,
      n_embed      = n_embed,
      sigma_q      = sigma_q,
      base_err     = base_err,
      TimeP        = TimeP,
      N_videos     = N_videos,
      eta_fixed    = eta_true,
      zeta_fixed   = zeta_true,
      seed         = seed + idx * 7L,
      integrator   = method,
      return_latent = TRUE,
      ncores       = ncores
    )
    
    est_ss <- tryCatch(
      estimate_dlo_state_space(
        dynEGA_obj = sim$dynEGA_obj,
        data       = sim$data,
        dt         = sim$dt_used,
        min_valid  = 30,
        verbose    = FALSE
      ),
      error = function(e) NULL
    )
    
    rows[[idx]] <- data.frame(
      integrator     = method,
      eta_hat_ss     = if (!is.null(est_ss)) est_ss$eta_hat_mean else NA_real_,
      zeta_hat_ss    = if (!is.null(est_ss)) est_ss$zeta_hat_mean else NA_real_,
      eta_err_ss     = if (!is.null(est_ss)) est_ss$eta_hat_mean - eta_true else NA_real_,
      zeta_err_ss    = if (!is.null(est_ss)) est_ss$zeta_hat_mean - zeta_true else NA_real_,
      n_comms        = if (!is.null(est_ss)) est_ss$n_comms else NA_integer_,
      n_obs          = if (!is.null(est_ss)) est_ss$n_obs else NA_integer_,
      est_factors    = sim$dynEGA_obj$dynEGA$population$n.dim,
      latent_obs_cor = compute_latent_obs_correlation(
        latents = sim$latents,
        data    = sim$data,
        NVAR    = sim$NVAR
      ),
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(rows)
}

## ============================================================
## 8. EMPIRICAL REALISM PIPELINE
## ============================================================

EMPIRICAL_EMOTION_COLUMNS <- c(
  "anger", "disgust", "fear", "happiness", "neutral", "sadness",
  "surprise", "contempt", "anxiety", "helplessness", "disappointment"
)

EMPIRICAL_PRIMARY_METRICS <- c(
  "mean_value", "std_value", "channel_iqr", "lag1_acf", "total_variation", "diff_iqr",
  "zero_crossing_rate", "dominant_frequency", "spectral_centroid",
  "sample_entropy", "permutation_entropy", "spectral_entropy", "run_length_entropy",
  "mean_pairwise_cor", "mean_abs_pairwise_cor", "sd_pairwise_cor",
  "first_eigen_share", "effective_rank"
)

normalize_community_map <- function(community_map) {
  if (is.null(community_map) || length(community_map) == 0L) {
    return(setNames(integer(0), character(0)))
  }

  original_names <- names(community_map)
  community_map <- as.integer(community_map)
  names(community_map) <- original_names
  levels_sorted <- sort(unique(community_map))
  normalized <- match(community_map, levels_sorted)
  names(normalized) <- original_names
  normalized
}

interpolate_short_na_runs <- function(x, max_gap = 2L) {
  x_out <- as.numeric(x)
  na_idx <- is.na(x_out)
  imputed <- rep(FALSE, length(x_out))

  if (!any(na_idx)) {
    return(list(values = x_out, imputed = imputed))
  }

  runs <- rle(na_idx)
  run_ends <- cumsum(runs$lengths)
  run_starts <- run_ends - runs$lengths + 1L

  for (idx in seq_along(runs$values)) {
    if (!runs$values[[idx]]) {
      next
    }

    gap_len <- runs$lengths[[idx]]
    start_idx <- run_starts[[idx]]
    end_idx <- run_ends[[idx]]

    if (gap_len > max_gap || start_idx == 1L || end_idx == length(x_out)) {
      next
    }

    left_value <- x_out[[start_idx - 1L]]
    right_value <- x_out[[end_idx + 1L]]
    if (!is.finite(left_value) || !is.finite(right_value)) {
      next
    }

    interp_positions <- seq.int(left_value, right_value, length.out = gap_len + 2L)
    x_out[start_idx:end_idx] <- interp_positions[2:(gap_len + 1L)]
    imputed[start_idx:end_idx] <- TRUE
  }

  list(values = x_out, imputed = imputed)
}

renormalize_probability_rows <- function(df, score_cols) {
  if (nrow(df) == 0L) {
    return(df)
  }

  clipped <- df
  clipped[score_cols] <- lapply(clipped[score_cols], function(col) pmin(pmax(col, 0), 1))
  row_sums <- rowSums(clipped[score_cols])
  keep <- is.finite(row_sums) & row_sums > 0

  clipped <- clipped[keep, , drop = FALSE]
  row_sums <- row_sums[keep]

  if (nrow(clipped) == 0L) {
    return(clipped)
  }

  clipped[score_cols] <- clipped[score_cols] / row_sums
  clipped
}

clean_empirical_frame_scores <- function(frame_scores_path,
                                         model_name,
                                         score_cols = EMPIRICAL_EMOTION_COLUMNS,
                                         max_gap = 2L,
                                         min_complete = 0.60,
                                         min_length = 15L) {
  raw_df <- utils::read.csv(frame_scores_path, stringsAsFactors = FALSE)
  required_cols <- c(score_cols, "clip", "frame_index", "truth_label")
  missing_cols <- setdiff(required_cols, names(raw_df))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required empirical columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  clip_ids <- unique(raw_df$clip)
  qc_rows <- vector("list", length(clip_ids))
  cleaned_rows <- vector("list", length(clip_ids))

  for (idx in seq_along(clip_ids)) {
    clip_id <- clip_ids[[idx]]
    clip_df <- raw_df[raw_df$clip == clip_id, c("clip", "frame_index", "truth_label", score_cols), drop = FALSE]
    clip_df <- clip_df[order(clip_df$frame_index), , drop = FALSE]

    interp_flags <- matrix(FALSE, nrow = nrow(clip_df), ncol = length(score_cols))
    colnames(interp_flags) <- score_cols

    for (col_idx in seq_along(score_cols)) {
      col_name <- score_cols[[col_idx]]
      interp <- interpolate_short_na_runs(clip_df[[col_name]], max_gap = max_gap)
      clip_df[[col_name]] <- interp$values
      interp_flags[, col_idx] <- interp$imputed
    }

    complete_mask <- complete.cases(clip_df[, score_cols, drop = FALSE])
    completeness <- mean(complete_mask)
    usable_df <- clip_df[complete_mask, , drop = FALSE]
    usable_df$model <- model_name
    usable_df <- renormalize_probability_rows(usable_df, score_cols)
    usable_length <- nrow(usable_df)

    exclude_reason <- NA_character_
    if (completeness < min_complete) {
      exclude_reason <- "low_completeness"
    } else if (usable_length < min_length) {
      exclude_reason <- "too_short"
    }

    excluded <- !is.na(exclude_reason)

    qc_rows[[idx]] <- data.frame(
      clip = clip_id,
      truth_label = clip_df$truth_label[[1]],
      model = model_name,
      raw_length = nrow(clip_df),
      complete_fraction = completeness,
      usable_length = usable_length,
      imputed_rows = sum(rowSums(interp_flags) > 0L),
      excluded = excluded,
      exclude_reason = exclude_reason,
      stringsAsFactors = FALSE
    )

    if (!excluded && usable_length > 0L) {
      cleaned_rows[[idx]] <- usable_df
    }
  }

  panel <- dplyr::bind_rows(cleaned_rows)
  qc <- dplyr::bind_rows(qc_rows)

  list(
    panel = panel,
    qc = qc
  )
}

compute_sample_entropy <- function(series, m = 2L, r = NULL) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]

  if (length(series) <= (m + 2L)) {
    return(NA_real_)
  }

  if (is.null(r)) {
    r <- 0.2 * stats::sd(series)
  }

  if (!is.finite(r) || r <= 0) {
    return(NA_real_)
  }

  phi_count <- function(embed_dim) {
    n_templates <- length(series) - embed_dim + 1L
    if (n_templates <= 1L) {
      return(NA_real_)
    }

    templates <- lapply(seq_len(n_templates), function(i) series[i:(i + embed_dim - 1L)])
    counts <- numeric(n_templates)

    for (i in seq_len(n_templates)) {
      counts[[i]] <- sum(vapply(
        templates,
        function(candidate) max(abs(templates[[i]] - candidate)) <= r,
        logical(1)
      ))
    }

    mean(counts / n_templates)
  }

  phi_m <- phi_count(m)
  phi_m1 <- phi_count(m + 1L)

  if (!is.finite(phi_m) || !is.finite(phi_m1) || phi_m <= 0 || phi_m1 <= 0) {
    return(NA_real_)
  }

  -log(phi_m1 / phi_m)
}

compute_permutation_entropy <- function(series, m = 3L) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]
  n <- length(series)

  if (n < m + 1L) {
    return(NA_real_)
  }

  patterns <- vapply(seq_len(n - m + 1L), function(i) {
    paste(order(series[i:(i + m - 1L)]), collapse = ",")
  }, character(1))
  freq <- table(patterns) / length(patterns)

  -sum(freq * log2(freq)) / log2(factorial(m))
}

compute_spectral_entropy <- function(series) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]
  n <- length(series)

  if (n < 8L) {
    return(NA_real_)
  }

  power <- Mod(stats::fft(series - mean(series)))^2
  power <- power[2:(floor(n / 2L) + 1L)]
  power <- power / sum(power)
  power <- power[is.finite(power) & power > 0]

  if (length(power) <= 1L) {
    return(NA_real_)
  }

  -sum(power * log2(power)) / log2(length(power))
}

compute_run_length_entropy <- function(series, max_run = 8L) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]

  if (length(series) < 4L) {
    return(NA_real_)
  }

  signs <- sign(diff(series))
  signs <- signs[signs != 0]

  if (length(signs) < 2L) {
    return(0)
  }

  run_lengths <- pmin(rle(signs)$lengths, as.integer(max_run))
  probs <- as.numeric(table(run_lengths))
  probs <- probs / sum(probs)

  if (length(probs) <= 1L) {
    return(0)
  }

  -sum(probs * log2(probs)) / log2(length(probs))
}

compute_spectral_properties_r <- function(series, dt = 1) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]

  if (length(series) < 4L) {
    return(list(dominant_frequency = NA_real_, spectral_centroid = NA_real_))
  }

  centered <- series - mean(series)
  n <- length(centered)
  spectrum <- stats::fft(centered)
  power <- Mod(spectrum)^2
  freq <- seq.int(0, n - 1L) / (n * dt)
  keep <- seq_len(floor(n / 2L) + 1L)

  freq <- freq[keep]
  power <- power[keep]

  if (length(power) <= 1L || !any(is.finite(power[-1L]))) {
    return(list(dominant_frequency = NA_real_, spectral_centroid = NA_real_))
  }

  pos_freq <- freq[-1L]
  pos_power <- power[-1L]
  dominant_frequency <- pos_freq[[which.max(pos_power)]]
  spectral_centroid <- sum(pos_freq * pos_power) / sum(pos_power)

  list(
    dominant_frequency = dominant_frequency,
    spectral_centroid = spectral_centroid
  )
}

compute_series_properties <- function(series, dt = 1) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]

  if (length(series) < 3L) {
    return(data.frame(
      mean_value = NA_real_,
      std_value = NA_real_,
      channel_iqr = NA_real_,
      lag1_acf = NA_real_,
      total_variation = NA_real_,
      diff_iqr = NA_real_,
      zero_crossing_rate = NA_real_,
      dominant_frequency = NA_real_,
      spectral_centroid = NA_real_,
      sample_entropy = NA_real_,
      permutation_entropy = NA_real_,
      spectral_entropy = NA_real_,
      run_length_entropy = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  centered <- series - mean(series)
  signs <- sign(centered)
  zero_crossings <- sum(signs[-1L] * signs[-length(signs)] < 0, na.rm = TRUE)
  lag1_acf <- suppressWarnings(stats::acf(series, lag.max = 1, plot = FALSE, na.action = stats::na.pass)$acf[[2]])
  spectral <- compute_spectral_properties_r(series, dt = dt)
  increments <- diff(series)

  data.frame(
    mean_value = mean(series),
    std_value = stats::sd(series),
    channel_iqr = stats::IQR(series, na.rm = TRUE),
    lag1_acf = lag1_acf,
    total_variation = mean(abs(diff(series))),
    diff_iqr = stats::IQR(increments, na.rm = TRUE),
    zero_crossing_rate = zero_crossings / length(series),
    dominant_frequency = spectral$dominant_frequency,
    spectral_centroid = spectral$spectral_centroid,
    sample_entropy = compute_sample_entropy(series),
    permutation_entropy = compute_permutation_entropy(series),
    spectral_entropy = compute_spectral_entropy(series),
    run_length_entropy = compute_run_length_entropy(series),
    stringsAsFactors = FALSE
  )
}

safe_effective_rank <- function(cor_mat) {
  eigenvalues <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  eigenvalues <- eigenvalues[is.finite(eigenvalues) & eigenvalues > 0]
  if (length(eigenvalues) == 0L) {
    return(NA_real_)
  }

  probs <- eigenvalues / sum(eigenvalues)
  exp(-sum(probs * log(probs)))
}

compute_panel_properties <- function(mat) {
  mat <- as.matrix(mat)
  cor_mat <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs"))
  if (!is.matrix(cor_mat) || any(!is.finite(cor_mat))) {
    return(data.frame(
      mean_pairwise_cor = NA_real_,
      mean_abs_pairwise_cor = NA_real_,
      sd_pairwise_cor = NA_real_,
      first_eigen_share = NA_real_,
      effective_rank = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  upper_values <- cor_mat[upper.tri(cor_mat)]
  eigenvalues <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values

  data.frame(
    mean_pairwise_cor = mean(upper_values, na.rm = TRUE),
    mean_abs_pairwise_cor = mean(abs(upper_values), na.rm = TRUE),
    sd_pairwise_cor = stats::sd(upper_values, na.rm = TRUE),
    first_eigen_share = max(eigenvalues, na.rm = TRUE) / sum(eigenvalues, na.rm = TRUE),
    effective_rank = safe_effective_rank(cor_mat),
    stringsAsFactors = FALSE
  )
}

metric_table_from_panel <- function(panel, series_cols, family, dt = 1) {
  clip_ids <- unique(panel$clip)
  series_rows <- list()
  panel_rows <- list()
  row_id <- 1L

  for (clip_id in clip_ids) {
    clip_df <- panel[panel$clip == clip_id, , drop = FALSE]
    truth_label <- if ("truth_label" %in% names(clip_df)) clip_df$truth_label[[1]] else NA_character_

    for (series_name in series_cols) {
      prop_row <- compute_series_properties(clip_df[[series_name]], dt = dt)
      prop_row$family <- family
      prop_row$component <- "series"
      prop_row$clip <- clip_id
      prop_row$unit <- series_name
      prop_row$truth_label <- truth_label
      series_rows[[row_id]] <- prop_row
      row_id <- row_id + 1L
    }

    panel_prop <- compute_panel_properties(clip_df[, series_cols, drop = FALSE])
    panel_prop$family <- family
    panel_prop$component <- "panel"
    panel_prop$clip <- clip_id
    panel_prop$unit <- "clip_panel"
    panel_prop$truth_label <- truth_label
    panel_rows[[length(panel_rows) + 1L]] <- panel_prop
  }

  combined <- dplyr::bind_rows(series_rows, panel_rows)
  metric_cols <- intersect(EMPIRICAL_PRIMARY_METRICS, names(combined))

  combined %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_cols),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::arrange(component, clip, unit, metric)
}

extract_mean_correlation_matrix <- function(panel, series_cols) {
  mats <- lapply(unique(panel$clip), function(clip_id) {
    clip_df <- panel[panel$clip == clip_id, series_cols, drop = FALSE]
    if (nrow(clip_df) < 3L) {
      return(NULL)
    }
    suppressWarnings(stats::cor(as.matrix(clip_df), use = "pairwise.complete.obs"))
  })

  mats <- mats[!vapply(mats, is.null, logical(1))]
  if (length(mats) == 0L) {
    return(matrix(NA_real_, nrow = length(series_cols), ncol = length(series_cols), dimnames = list(series_cols, series_cols)))
  }

  acc <- Reduce(`+`, mats)
  acc / length(mats)
}

fit_pooled_ord0_dynega <- function(panel,
                                   score_cols = EMPIRICAL_EMOTION_COLUMNS,
                                   id_col = "clip",
                                   n_embed = 10L,
                                   tau = 1L,
                                   algorithm = "louvain") {
  required_cols <- c(score_cols, id_col)
  missing_cols <- setdiff(required_cols, names(panel))
  if (length(missing_cols) > 0L) {
    stop(
      "Pooled DynEGA panel is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  fit_data <- panel[, required_cols, drop = FALSE]
  fit_data[[id_col]] <- match(fit_data[[id_col]], unique(fit_data[[id_col]]))

  fit <- EGAnet::dynEGA(
    data = fit_data,
    level = "population",
    n.embed = n_embed,
    tau = tau,
    id = length(score_cols) + 1L,
    model = "glasso",
    use.derivatives = 0,
    algorithm = algorithm
  )

  score_obj <- fit$dynEGA$population

  community_map <- score_obj$wc
  if (is.matrix(community_map)) {
    community_map <- community_map[1, ]
  }
  names(community_map) <- gsub("\\.Ord0$", "", names(community_map))
  community_map <- normalize_community_map(community_map[score_cols])
  community_sizes <- sort(as.integer(table(community_map)), decreasing = TRUE)
  n_dim <- length(unique(community_map))

  list(
    fit = fit,
    score_obj = score_obj,
    community_map = community_map,
    community_sizes = community_sizes,
    n_dim = n_dim
  )
}

extract_latent_scores <- function(dynega_fit, panel, score_cols = EMPIRICAL_EMOTION_COLUMNS) {
  scores <- compute_net_scores_safe(dynega_fit$score_obj, panel[, score_cols, drop = FALSE])
  if (is.null(scores)) {
    stop("Could not extract latent network scores from pooled dynEGA.", call. = FALSE)
  }

  scores <- as.data.frame(scores, stringsAsFactors = FALSE)
  names(scores) <- paste0("dim_", seq_len(ncol(scores)))
  cbind(panel[, c("clip", "frame_index", "truth_label", "model"), drop = FALSE], scores)
}

estimate_named_loadings <- function(panel,
                                    community_map,
                                    score_cols = EMPIRICAL_EMOTION_COLUMNS) {
  loadings <- rep(0.8, length(score_cols))
  names(loadings) <- score_cols

  for (community_id in sort(unique(community_map))) {
    vars <- names(community_map)[community_map == community_id]
    block <- panel[, vars, drop = FALSE]

    if (length(vars) == 1L) {
      loadings[vars] <- 0.8
      next
    }

    pc <- tryCatch(stats::prcomp(block, center = TRUE, scale. = TRUE), error = function(e) NULL)
    if (is.null(pc)) {
      loadings[vars] <- 0.8
      next
    }

    weights <- abs(pc$rotation[, 1])
    if (!all(is.finite(weights)) || max(weights) <= 0) {
      loadings[vars] <- 0.8
      next
    }

    scaled <- 0.4 + 0.5 * (weights / max(weights))
    loadings[vars] <- pmin(pmax(scaled, 0.4), 0.9)
  }

  loadings
}

build_property_target_from_panel <- function(panel,
                                             model_name,
                                             score_cols = EMPIRICAL_EMOTION_COLUMNS,
                                             ord0_embed = 10L,
                                             algorithm = "louvain") {
  raw_metrics <- metric_table_from_panel(panel, series_cols = score_cols, family = "raw", dt = 1)
  latent_fit <- fit_pooled_ord0_dynega(
    panel,
    score_cols = score_cols,
    n_embed = ord0_embed,
    algorithm = algorithm
  )
  latent_panel <- extract_latent_scores(latent_fit, panel, score_cols = score_cols)
  latent_cols <- setdiff(names(latent_panel), c("clip", "frame_index", "truth_label", "model"))
  latent_metrics <- metric_table_from_panel(latent_panel, series_cols = latent_cols, family = "latent", dt = 1)
  loadings <- estimate_named_loadings(panel, latent_fit$community_map, score_cols = score_cols)
  measurement_var <- pmax(0.05, 1 - loadings^2)
  names(measurement_var) <- names(loadings)

  list(
    model = model_name,
    panel = panel,
    latent_panel = latent_panel,
    raw_metrics = raw_metrics,
    latent_metrics = latent_metrics,
    raw_cor = extract_mean_correlation_matrix(panel, series_cols = score_cols),
    latent_cor = extract_mean_correlation_matrix(latent_panel, series_cols = latent_cols),
    score_cols = score_cols,
    latent_cols = latent_cols,
    community_map = latent_fit$community_map,
    community_sizes = latent_fit$community_sizes,
    n_dim = latent_fit$n_dim,
    loadings = loadings,
    measurement_var = measurement_var
  )
}

build_empirical_target <- function(frame_scores_path,
                                   model_name,
                                   score_cols = EMPIRICAL_EMOTION_COLUMNS,
                                   max_gap = 2L,
                                   min_complete = 0.60,
                                   min_length = 15L,
                                   ord0_embed = 10L) {
  cleaned <- clean_empirical_frame_scores(
    frame_scores_path = frame_scores_path,
    model_name = model_name,
    score_cols = score_cols,
    max_gap = max_gap,
    min_complete = min_complete,
    min_length = min_length
  )

  target <- build_property_target_from_panel(
    panel = cleaned$panel,
    model_name = model_name,
    score_cols = score_cols,
    ord0_embed = ord0_embed
  )

  target$qc <- cleaned$qc
  target$clip_lengths <- cleaned$qc$usable_length[!cleaned$qc$excluded]
  names(target$clip_lengths) <- cleaned$qc$clip[!cleaned$qc$excluded]
  target
}

make_residual_cov <- function(var_vec, resid_rho = 0, eps = 1e-8) {
  var_vec <- pmax(as.numeric(var_vec), eps)
  n <- length(var_vec)

  rho_min <- -1 / max(n - 1, 1)
  resid_rho <- min(max(as.numeric(resid_rho), rho_min + 1e-6), 0.999)

  R <- matrix(resid_rho, nrow = n, ncol = n)
  diag(R) <- 1

  D <- diag(sqrt(var_vec), nrow = n, ncol = n)
  D %*% R %*% D
}

simulate_ar1 <- function(num_steps, phi = 0.6, sd = 1.0) {
  phi <- min(max(as.numeric(phi), -0.999), 0.999)
  sd <- max(as.numeric(sd), 0)
  g <- numeric(num_steps)

  if (num_steps == 0L || sd == 0) {
    return(g)
  }

  g[[1]] <- stats::rnorm(1, 0, sd / sqrt(max(1 - phi^2, 1e-6)))
  if (num_steps > 1L) {
    for (t in 2:num_steps) {
      g[[t]] <- phi * g[[t - 1L]] + stats::rnorm(1, 0, sd)
    }
  }
  g
}

apply_score_link <- function(z,
                             link = c("softmax", "identity", "logistic", "logistic_renorm"),
                             temperature = 1.0,
                             channel_intercepts = NULL,
                             eps = 1e-8) {
  link <- match.arg(link)
  z <- as.matrix(z)
  temperature <- max(as.numeric(temperature), eps)

  if (!is.null(channel_intercepts)) {
    if (!is.null(names(channel_intercepts)) && !is.null(colnames(z))) {
      missing_intercepts <- setdiff(colnames(z), names(channel_intercepts))
      if (length(missing_intercepts) > 0L) {
        stop(
          "Missing channel intercepts for: ",
          paste(missing_intercepts, collapse = ", "),
          call. = FALSE
        )
      }
      intercepts <- as.numeric(channel_intercepts[colnames(z)])
    } else {
      intercepts <- as.numeric(channel_intercepts)
      if (length(intercepts) == 1L) {
        intercepts <- rep(intercepts, ncol(z))
      }
      if (length(intercepts) != ncol(z)) {
        stop(
          "channel_intercepts must be length 1, length ncol(z), or named by score column.",
          call. = FALSE
        )
      }
    }
    z <- sweep(z, 2, intercepts, "+")
  }

  if (link == "identity") {
    return(z)
  }

  if (link == "logistic") {
    return(plogis(z / temperature))
  }

  if (link == "logistic_renorm") {
    p <- plogis(z / temperature)
    return(p / pmax(rowSums(p), eps))
  }

  z_scaled <- z / temperature
  z_scaled <- sweep(z_scaled, 1, apply(z_scaled, 1, max, na.rm = TRUE), "-")
  ez <- exp(z_scaled)
  ez / pmax(rowSums(ez), eps)
}

robust_rescale_panel <- function(synthetic_panel,
                                 empirical_panel,
                                 score_cols,
                                 shrink = 1.0,
                                 eps = 1e-8) {
  syn <- synthetic_panel
  emp_center <- vapply(empirical_panel[score_cols], stats::median, numeric(1), na.rm = TRUE)
  emp_iqr <- vapply(empirical_panel[score_cols], stats::IQR, numeric(1), na.rm = TRUE)

  syn_center <- vapply(syn[score_cols], stats::median, numeric(1), na.rm = TRUE)
  syn_iqr <- vapply(syn[score_cols], stats::IQR, numeric(1), na.rm = TRUE)

  scale <- shrink * emp_iqr / pmax(syn_iqr, eps)

  syn[score_cols] <- sweep(syn[score_cols], 2, syn_center, "-")
  syn[score_cols] <- sweep(syn[score_cols], 2, scale, "*")
  syn[score_cols] <- sweep(syn[score_cols], 2, emp_center, "+")
  syn
}

amplitude_decomp <- function(panel,
                             score_cols,
                             clip_col_candidates = c("clip", "clip_id", "video_id", "video"),
                             eps = 1e-8) {
  clip_col <- intersect(clip_col_candidates, names(panel))[1]
  if (is.na(clip_col)) {
    stop("No clip column found. Supply or rename clip identifier.", call. = FALSE)
  }

  long <- panel |>
    dplyr::select(dplyr::all_of(c(clip_col, score_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(score_cols),
      names_to = "channel",
      values_to = "value"
    )

  pooled <- long |>
    dplyr::group_by(channel) |>
    dplyr::summarise(
      pooled_iqr = stats::IQR(value, na.rm = TRUE),
      pooled_sd = stats::sd(value, na.rm = TRUE),
      .groups = "drop"
    )

  within_clip <- long |>
    dplyr::group_by(.data[[clip_col]], channel) |>
    dplyr::summarise(
      within_iqr = stats::IQR(value, na.rm = TRUE),
      clip_median = stats::median(value, na.rm = TRUE),
      .groups = "drop"
    )

  within_summary <- within_clip |>
    dplyr::group_by(channel) |>
    dplyr::summarise(
      mean_within_iqr = mean(within_iqr, na.rm = TRUE),
      median_within_iqr = stats::median(within_iqr, na.rm = TRUE),
      between_clip_median_iqr = stats::IQR(clip_median, na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::left_join(pooled, within_summary, by = "channel") |>
    dplyr::mutate(
      pooled_iqr = pmax(pooled_iqr, eps),
      mean_within_iqr = pmax(mean_within_iqr, eps),
      median_within_iqr = pmax(median_within_iqr, eps),
      between_clip_median_iqr = pmax(between_clip_median_iqr, eps)
    )
}

compare_amplitude_decomp <- function(empirical_panel,
                                     synthetic_panel,
                                     score_cols,
                                     eps = 1e-8) {
  emp_amp <- amplitude_decomp(empirical_panel, score_cols = score_cols, eps = eps)
  syn_amp <- amplitude_decomp(synthetic_panel, score_cols = score_cols, eps = eps)

  emp_amp |>
    dplyr::rename_with(~ paste0("emp_", .x), -channel) |>
    dplyr::inner_join(
      syn_amp |> dplyr::rename_with(~ paste0("syn_", .x), -channel),
      by = "channel"
    ) |>
    dplyr::mutate(
      pooled_iqr_ratio = syn_pooled_iqr / pmax(emp_pooled_iqr, eps),
      within_iqr_ratio = syn_mean_within_iqr / pmax(emp_mean_within_iqr, eps),
      between_median_iqr_ratio =
        syn_between_clip_median_iqr / pmax(emp_between_clip_median_iqr, eps)
    )
}

compress_tanh <- function(x, center, radius, slope = 1.0, eps = 1e-8) {
  radius <- pmax(as.numeric(radius), eps)
  center + radius * tanh(as.numeric(slope) * (x - center) / radius)
}

apply_empirical_tanh_link <- function(synthetic_panel,
                                      empirical_panel,
                                      score_cols,
                                      radius_multiplier = 1.5,
                                      slope = 1.0,
                                      eps = 1e-8) {
  out <- synthetic_panel

  centers <- vapply(empirical_panel[score_cols], stats::median, numeric(1), na.rm = TRUE)
  radii <- as.numeric(radius_multiplier) *
    vapply(empirical_panel[score_cols], stats::IQR, numeric(1), na.rm = TRUE)

  for (ch in score_cols) {
    out[[ch]] <- compress_tanh(
      x = out[[ch]],
      center = centers[[ch]],
      radius = radii[[ch]],
      slope = slope,
      eps = eps
    )
  }

  out
}

clamp_to_empirical_range <- function(panel,
                                     empirical_panel,
                                     score_cols) {
  out <- panel
  lower <- vapply(empirical_panel[score_cols], min, numeric(1), na.rm = TRUE)
  upper <- vapply(empirical_panel[score_cols], max, numeric(1), na.rm = TRUE)

  for (ch in score_cols) {
    out[[ch]] <- pmin(pmax(out[[ch]], lower[[ch]]), upper[[ch]])
  }

  out
}

logit_transform_score_panel <- function(panel,
                                        score_cols,
                                        eps = 1e-4) {
  out <- panel
  eps <- min(max(as.numeric(eps), .Machine$double.eps), 0.49)

  for (ch in score_cols) {
    p <- as.numeric(out[[ch]])
    out[[ch]] <- stats::qlogis(pmin(pmax(p, eps), 1 - eps))
  }

  out
}

affine_iqr_normalize_panel <- function(synthetic_panel,
                                       empirical_panel,
                                       score_cols,
                                       alpha = 1.0,
                                       clamp = TRUE,
                                       eps = 1e-8) {
  normalized <- robust_rescale_panel(
    synthetic_panel = synthetic_panel,
    empirical_panel = empirical_panel,
    score_cols = score_cols,
    shrink = 1.0,
    eps = eps
  )

  out <- synthetic_panel
  alpha <- min(max(as.numeric(alpha), 0), 1)
  out[score_cols] <- (1 - alpha) * synthetic_panel[score_cols] + alpha * normalized[score_cols]

  if (clamp) {
    out <- clamp_to_empirical_range(out, empirical_panel, score_cols)
  }

  out
}

empirical_quantile_normalize_panel <- function(synthetic_panel,
                                               empirical_panel,
                                               score_cols,
                                               alpha = 1.0,
                                               n_prob = 1001L,
                                               clamp = TRUE) {
  out <- synthetic_panel
  alpha <- min(max(as.numeric(alpha), 0), 1)
  probs <- seq(0, 1, length.out = as.integer(n_prob))

  for (ch in score_cols) {
    syn <- as.numeric(synthetic_panel[[ch]])
    emp <- as.numeric(empirical_panel[[ch]])
    emp <- emp[is.finite(emp)]
    finite <- is.finite(syn)

    mapped <- syn
    if (sum(finite) > 1L && length(emp) > 1L) {
      ranks <- rank(syn[finite], ties.method = "average")
      rank_probs <- (ranks - 0.5) / length(ranks)
      emp_quantiles <- stats::quantile(emp, probs = probs, names = FALSE, type = 8, na.rm = TRUE)
      mapped[finite] <- stats::approx(
        x = probs,
        y = emp_quantiles,
        xout = rank_probs,
        rule = 2,
        ties = "ordered"
      )$y
    }

    out[[ch]] <- (1 - alpha) * syn + alpha * mapped
  }

  if (clamp) {
    out <- clamp_to_empirical_range(out, empirical_panel, score_cols)
  }

  out
}

candidate_channel_iqr_frontier <- function(candidate_summary,
                                           metric_comparison_long) {
  candidate_metrics <- metric_comparison_long |>
    dplyr::mutate(
      max_discrepancy = pmax(median_diff_scaled, wasserstein_scaled, na.rm = TRUE)
    )

  channel_frontier <- candidate_metrics |>
    dplyr::filter(metric == "channel_iqr") |>
    dplyr::group_by(candidate_id) |>
    dplyr::summarise(
      channel_iqr_worst = max(max_discrepancy, na.rm = TRUE),
      raw_channel_iqr = max(max_discrepancy[family == "raw"], na.rm = TRUE),
      latent_channel_iqr = max(max_discrepancy[family == "latent"], na.rm = TRUE),
      .groups = "drop"
    )

  diff_frontier <- candidate_metrics |>
    dplyr::filter(metric == "diff_iqr") |>
    dplyr::group_by(candidate_id) |>
    dplyr::summarise(
      diff_iqr_worst = max(max_discrepancy, na.rm = TRUE),
      raw_diff_iqr = max(max_discrepancy[family == "raw"], na.rm = TRUE),
      latent_diff_iqr = max(max_discrepancy[family == "latent"], na.rm = TRUE),
      .groups = "drop"
    )

  channel_frontier |>
    dplyr::inner_join(diff_frontier, by = "candidate_id") |>
    dplyr::inner_join(candidate_summary, by = "candidate_id") |>
    dplyr::arrange(channel_iqr_worst, total_loss)
}

cor_sign_summary <- function(panel, score_cols, label = NA_character_) {
  C <- suppressWarnings(stats::cor(panel[score_cols], use = "pairwise.complete.obs"))
  if (!is.matrix(C) || any(!is.finite(C))) {
    return(data.frame(
      label = label,
      mean_pairwise_cor = NA_real_,
      mean_abs_pairwise_cor = NA_real_,
      positive_cor_fraction = NA_real_,
      negative_cor_fraction = NA_real_,
      first_eigen_share = NA_real_,
      effective_rank = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  vals <- C[upper.tri(C)]
  eigenvalues <- eigen(C, symmetric = TRUE, only.values = TRUE)$values

  data.frame(
    label = label,
    mean_pairwise_cor = mean(vals, na.rm = TRUE),
    mean_abs_pairwise_cor = mean(abs(vals), na.rm = TRUE),
    positive_cor_fraction = mean(vals > 0, na.rm = TRUE),
    negative_cor_fraction = mean(vals < 0, na.rm = TRUE),
    first_eigen_share = max(eigenvalues, na.rm = TRUE) / sum(eigenvalues, na.rm = TRUE),
    effective_rank = safe_effective_rank(C),
    stringsAsFactors = FALSE
  )
}

simulate_named_observables <- function(latent,
                                       community_map,
                                       loadings,
                                       measurement_var,
                                       obs_scale = 1.0,
                                       resid_rho = 0,
                                       common_sd = 0,
                                       common_phi = 0.6,
                                       cross_loading_sd = 0,
                                       link = c("softmax", "identity", "logistic", "logistic_renorm"),
                                       temperature = 1.0,
                                       channel_intercepts = NULL) {
  link <- match.arg(link)
  emotion_names <- names(community_map)
  n_steps <- nrow(latent)
  n_emotions <- length(emotion_names)
  n_fac <- ncol(latent)

  L <- matrix(0, nrow = n_emotions, ncol = n_fac)
  rownames(L) <- emotion_names

  if (cross_loading_sd > 0) {
    L[,] <- stats::rnorm(n_emotions * n_fac, mean = 0, sd = cross_loading_sd)
  }

  for (emotion in emotion_names) {
    fac <- community_map[[emotion]]
    L[emotion, fac] <- loadings[[emotion]]
  }

  Sigma <- make_residual_cov(measurement_var[emotion_names], resid_rho = resid_rho)
  e <- MASS::mvrnorm(
    n = n_steps,
    mu = rep(0, n_emotions),
    Sigma = Sigma
  )

  g <- simulate_ar1(n_steps, phi = common_phi, sd = common_sd)
  obs_star <- obs_scale * (latent %*% t(L) + outer(g, rep(1, n_emotions))) + e
  colnames(obs_star) <- emotion_names

  obs <- apply_score_link(
    obs_star,
    link = link,
    temperature = temperature,
    channel_intercepts = channel_intercepts
  )
  obs <- as.data.frame(obs, stringsAsFactors = FALSE)
  names(obs) <- emotion_names
  obs
}

simulate_empirical_dlo_panel <- function(clip_lengths,
                                         community_map,
                                         loadings,
                                         measurement_var,
                                         eta,
                                         zeta,
                                         sigma_q,
                                         dt,
                                         n_replicates = 1L,
                                         model_name = "synthetic",
                                         integrator = "linear_exact",
                                         seed = NULL,
                                         rho_q = 0,
                                         q_cov = NULL,
                                         obs_scale = 1.0,
                                         resid_rho = 0,
                                         common_sd = 0,
                                         common_phi = 0.6,
                                         cross_loading_sd = 0,
                                         link = "softmax",
                                         temperature = 1.0,
                                         channel_intercepts = NULL,
                                         init_x_sd = NULL,
                                         init_dx_sd = NULL,
                                         burn_in = 0L) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  community_map <- normalize_community_map(community_map)
  n_fac <- length(unique(community_map))
  clip_names <- names(clip_lengths)
  rows <- list()
  row_id <- 1L

  for (rep_idx in seq_len(n_replicates)) {
    for (clip_idx in seq_along(clip_lengths)) {
      clip_length <- as.integer(clip_lengths[[clip_idx]])
      latent <- simulate_latent_multi(
        dt = dt,
        num_steps = clip_length,
        NFAC = n_fac,
        sigma_q = sigma_q,
        eta = eta,
        zeta = zeta,
        integrator = integrator,
        rho_q = rho_q,
        q_cov = q_cov,
        init_x_sd = init_x_sd,
        init_dx_sd = init_dx_sd,
        burn_in = burn_in
      )

      observables <- simulate_named_observables(
        latent = latent,
        community_map = community_map,
        loadings = loadings,
        measurement_var = measurement_var,
        obs_scale = obs_scale,
        resid_rho = resid_rho,
        common_sd = common_sd,
        common_phi = common_phi,
        cross_loading_sd = cross_loading_sd,
        link = link,
        temperature = temperature,
        channel_intercepts = channel_intercepts
      )

      observables$clip <- sprintf("rep%03d_%s", rep_idx, clip_names[[clip_idx]])
      observables$frame_index <- seq_len(clip_length)
      observables$truth_label <- NA_character_
      observables$model <- model_name
      rows[[row_id]] <- observables[, c(names(community_map), "frame_index", "clip", "truth_label", "model"), drop = FALSE]
      row_id <- row_id + 1L
    }
  }

  dplyr::bind_rows(rows)
}

wasserstein_1d <- function(x, y, n_prob = 101L) {
  x <- sort(x[is.finite(x)])
  y <- sort(y[is.finite(y)])
  if (length(x) == 0L || length(y) == 0L) {
    return(Inf)
  }

  probs <- seq(0, 1, length.out = n_prob)
  qx <- stats::quantile(x, probs = probs, names = FALSE, type = 8)
  qy <- stats::quantile(y, probs = probs, names = FALSE, type = 8)
  mean(abs(qx - qy))
}

metric_loss_table <- function(empirical_metrics, synthetic_metrics, family) {
  empirical_family <- empirical_metrics[empirical_metrics$family == family, , drop = FALSE]
  synthetic_family <- synthetic_metrics[synthetic_metrics$family == family, , drop = FALSE]
  common_metrics <- intersect(unique(empirical_family$metric), unique(synthetic_family$metric))

  rows <- vector("list", length(common_metrics))
  for (idx in seq_along(common_metrics)) {
    metric_name <- common_metrics[[idx]]
    x <- empirical_family$value[empirical_family$metric == metric_name]
    y <- synthetic_family$value[synthetic_family$metric == metric_name]
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]

    empirical_iqr <- stats::IQR(x, na.rm = TRUE)
    if (!is.finite(empirical_iqr) || empirical_iqr <= 1e-6) {
      empirical_iqr <- max(stats::sd(x, na.rm = TRUE), 1e-3)
    }

    median_diff_scaled <- abs(stats::median(x, na.rm = TRUE) - stats::median(y, na.rm = TRUE)) / empirical_iqr
    wasserstein_scaled <- wasserstein_1d(x, y) / empirical_iqr

    rows[[idx]] <- data.frame(
      family = family,
      metric = metric_name,
      empirical_median = stats::median(x, na.rm = TRUE),
      synthetic_median = stats::median(y, na.rm = TRUE),
      empirical_iqr = empirical_iqr,
      median_diff_scaled = median_diff_scaled,
      wasserstein_scaled = wasserstein_scaled,
      metric_loss = median_diff_scaled + wasserstein_scaled,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(rows)
}

permute_indices <- function(x) {
  if (length(x) <= 1L) {
    return(list(x))
  }

  out <- list()
  out_id <- 1L
  for (idx in seq_along(x)) {
    rest <- x[-idx]
    for (perm in permute_indices(rest)) {
      out[[out_id]] <- c(x[[idx]], perm)
      out_id <- out_id + 1L
    }
  }
  out
}

optimal_community_size_loss <- function(empirical_sizes, synthetic_sizes) {
  k <- max(length(empirical_sizes), length(synthetic_sizes))
  empirical_sizes <- c(empirical_sizes, rep(0, k - length(empirical_sizes)))
  synthetic_sizes <- c(synthetic_sizes, rep(0, k - length(synthetic_sizes)))

  if (k > 7L) {
    return(mean(abs(sort(empirical_sizes, decreasing = TRUE) - sort(synthetic_sizes, decreasing = TRUE))))
  }

  perms <- permute_indices(seq_len(k))
  losses <- vapply(perms, function(perm) {
    mean(abs(empirical_sizes - synthetic_sizes[perm]))
  }, numeric(1))
  min(losses)
}

score_candidate_loss <- function(empirical_target, synthetic_target) {
  raw_table <- metric_loss_table(empirical_target$raw_metrics, synthetic_target$raw_metrics, family = "raw")
  latent_table <- metric_loss_table(empirical_target$latent_metrics, synthetic_target$latent_metrics, family = "latent")

  n_dim_penalty <- abs(empirical_target$n_dim - synthetic_target$n_dim)
  community_penalty <- optimal_community_size_loss(empirical_target$community_sizes, synthetic_target$community_sizes) / 11

  raw_loss <- mean(raw_table$metric_loss, na.rm = TRUE)
  latent_loss <- mean(latent_table$metric_loss, na.rm = TRUE) + n_dim_penalty + community_penalty

  comparison <- dplyr::bind_rows(
    raw_table,
    latent_table,
    data.frame(
      family = "latent",
      metric = "n_dim_penalty",
      empirical_median = empirical_target$n_dim,
      synthetic_median = synthetic_target$n_dim,
      empirical_iqr = NA_real_,
      median_diff_scaled = n_dim_penalty,
      wasserstein_scaled = 0,
      metric_loss = n_dim_penalty,
      stringsAsFactors = FALSE
    ),
    data.frame(
      family = "latent",
      metric = "community_size_penalty",
      empirical_median = sum(empirical_target$community_sizes),
      synthetic_median = sum(synthetic_target$community_sizes),
      empirical_iqr = NA_real_,
      median_diff_scaled = community_penalty,
      wasserstein_scaled = 0,
      metric_loss = community_penalty,
      stringsAsFactors = FALSE
    )
  )

  list(
    raw_loss = raw_loss,
    latent_loss = latent_loss,
    total_loss = 0.5 * raw_loss + 0.5 * latent_loss,
    comparison = comparison
  )
}

evaluate_empirical_candidate <- function(empirical_target,
                                         eta,
                                         zeta,
                                         sigma_q,
                                         dt,
                                         n_replicates = 50L,
                                         integrator = "linear_exact",
                                         ord0_embed = 10L,
                                         seed = NULL) {
  synthetic_panel <- simulate_empirical_dlo_panel(
    clip_lengths = empirical_target$clip_lengths,
    community_map = empirical_target$community_map,
    loadings = empirical_target$loadings,
    measurement_var = empirical_target$measurement_var,
    eta = eta,
    zeta = zeta,
    sigma_q = sigma_q,
    dt = dt,
    n_replicates = n_replicates,
    model_name = paste0(empirical_target$model, "_synthetic"),
    integrator = integrator,
    seed = seed
  )

  synthetic_target <- build_property_target_from_panel(
    panel = synthetic_panel,
    model_name = paste0(empirical_target$model, "_synthetic"),
    score_cols = empirical_target$score_cols,
    ord0_embed = ord0_embed
  )

  loss <- score_candidate_loss(empirical_target, synthetic_target)

  list(
    params = data.frame(
      eta = eta,
      zeta = zeta,
      sigma_q = sigma_q,
      dt = dt,
      n_replicates = n_replicates,
      raw_loss = loss$raw_loss,
      latent_loss = loss$latent_loss,
      total_loss = loss$total_loss,
      synthetic_n_dim = synthetic_target$n_dim,
      stringsAsFactors = FALSE
    ),
    target = synthetic_target,
    panel = synthetic_panel,
    comparison = loss$comparison
  )
}

build_refinement_grid <- function(top_rows, eta_vals, zeta_vals, sigma_q_vals) {
  eta_step <- min(abs(diff(sort(unique(eta_vals)))))
  zeta_step <- min(abs(diff(sort(unique(zeta_vals)))))
  sigma_step <- min(abs(diff(sort(unique(sigma_q_vals)))))

  refine_rows <- list()
  row_id <- 1L

  for (idx in seq_len(nrow(top_rows))) {
    row <- top_rows[idx, , drop = FALSE]
    eta_grid <- sort(unique(c(row$eta - eta_step / 2, row$eta, row$eta + eta_step / 2)))
    zeta_grid <- sort(unique(c(row$zeta - zeta_step / 2, row$zeta, row$zeta + zeta_step / 2)))
    sigma_grid <- sort(unique(c(row$sigma_q - sigma_step / 2, row$sigma_q, row$sigma_q + sigma_step / 2)))

    eta_grid <- eta_grid[eta_grid < 0]
    zeta_grid <- zeta_grid[zeta_grid > 0]
    sigma_grid <- sigma_grid[sigma_grid > 0]

    expand <- expand.grid(
      eta = eta_grid,
      zeta = zeta_grid,
      sigma_q = sigma_grid,
      dt = row$dt,
      stringsAsFactors = FALSE
    )

    for (grid_idx in seq_len(nrow(expand))) {
      refine_rows[[row_id]] <- expand[grid_idx, , drop = FALSE]
      row_id <- row_id + 1L
    }
  }

  dplyr::distinct(dplyr::bind_rows(refine_rows))
}

calibrate_empirical_dlo_realism <- function(empirical_target,
                                            eta_vals = c(-0.05, -0.10, -0.20, -0.40, -0.60, -0.80),
                                            zeta_vals = c(0.05, 0.10, 0.20, 0.40, 0.60),
                                            sigma_q_vals = c(0.05, 0.10, 0.20, 0.40, 0.60),
                                            dt_vals = c(0.25, 0.50, 1.00),
                                            coarse_replicates = 50L,
                                            refine_top_n = 5L,
                                            integrator = "linear_exact",
                                            ord0_embed = 10L,
                                            seed = 20260403L) {
  coarse_grid <- expand.grid(
    eta = eta_vals,
    zeta = zeta_vals,
    sigma_q = sigma_q_vals,
    dt = dt_vals,
    stringsAsFactors = FALSE
  )

  coarse_rows <- vector("list", nrow(coarse_grid))
  coarse_comparisons <- vector("list", nrow(coarse_grid))

  for (idx in seq_len(nrow(coarse_grid))) {
    params <- coarse_grid[idx, , drop = FALSE]
    candidate <- tryCatch(
      evaluate_empirical_candidate(
        empirical_target = empirical_target,
        eta = params$eta,
        zeta = params$zeta,
        sigma_q = params$sigma_q,
        dt = params$dt,
        n_replicates = coarse_replicates,
        integrator = integrator,
        ord0_embed = ord0_embed,
        seed = seed + idx
      ),
      error = function(e) list(
        params = data.frame(
          eta = params$eta,
          zeta = params$zeta,
          sigma_q = params$sigma_q,
          dt = params$dt,
          n_replicates = coarse_replicates,
          raw_loss = Inf,
          latent_loss = Inf,
          total_loss = Inf,
          synthetic_n_dim = NA_integer_,
          stringsAsFactors = FALSE
        ),
        comparison = data.frame(
          family = NA_character_,
          metric = NA_character_,
          empirical_median = NA_real_,
          synthetic_median = NA_real_,
          empirical_iqr = NA_real_,
          median_diff_scaled = NA_real_,
          wasserstein_scaled = NA_real_,
          metric_loss = Inf,
          stringsAsFactors = FALSE
        ),
        error = conditionMessage(e)
      )
    )

    row <- candidate$params
    row$stage <- "coarse"
    row$error <- if (!is.null(candidate$error)) candidate$error else NA_character_
    coarse_rows[[idx]] <- row

    comparison <- candidate$comparison
    comparison$stage <- "coarse"
    comparison$eta <- params$eta
    comparison$zeta <- params$zeta
    comparison$sigma_q <- params$sigma_q
    comparison$dt <- params$dt
    coarse_comparisons[[idx]] <- comparison
  }

  coarse_df <- dplyr::bind_rows(coarse_rows) %>% dplyr::arrange(total_loss)
  top_rows <- utils::head(coarse_df[is.finite(coarse_df$total_loss), , drop = FALSE], refine_top_n)
  refine_grid <- build_refinement_grid(top_rows, eta_vals, zeta_vals, sigma_q_vals)

  refine_rows <- vector("list", nrow(refine_grid))
  refine_comparisons <- vector("list", nrow(refine_grid))

  for (idx in seq_len(nrow(refine_grid))) {
    params <- refine_grid[idx, , drop = FALSE]
    candidate <- tryCatch(
      evaluate_empirical_candidate(
        empirical_target = empirical_target,
        eta = params$eta,
        zeta = params$zeta,
        sigma_q = params$sigma_q,
        dt = params$dt,
        n_replicates = coarse_replicates,
        integrator = integrator,
        ord0_embed = ord0_embed,
        seed = seed + 100000L + idx
      ),
      error = function(e) list(
        params = data.frame(
          eta = params$eta,
          zeta = params$zeta,
          sigma_q = params$sigma_q,
          dt = params$dt,
          n_replicates = coarse_replicates,
          raw_loss = Inf,
          latent_loss = Inf,
          total_loss = Inf,
          synthetic_n_dim = NA_integer_,
          stringsAsFactors = FALSE
        ),
        comparison = data.frame(
          family = NA_character_,
          metric = NA_character_,
          empirical_median = NA_real_,
          synthetic_median = NA_real_,
          empirical_iqr = NA_real_,
          median_diff_scaled = NA_real_,
          wasserstein_scaled = NA_real_,
          metric_loss = Inf,
          stringsAsFactors = FALSE
        ),
        error = conditionMessage(e)
      )
    )

    row <- candidate$params
    row$stage <- "refine"
    row$error <- if (!is.null(candidate$error)) candidate$error else NA_character_
    refine_rows[[idx]] <- row

    comparison <- candidate$comparison
    comparison$stage <- "refine"
    comparison$eta <- params$eta
    comparison$zeta <- params$zeta
    comparison$sigma_q <- params$sigma_q
    comparison$dt <- params$dt
    refine_comparisons[[idx]] <- comparison
  }

  candidates <- dplyr::bind_rows(coarse_rows, refine_rows) %>% dplyr::arrange(total_loss)
  comparison_table <- dplyr::bind_rows(coarse_comparisons, refine_comparisons)
  best <- candidates[which.min(candidates$total_loss), , drop = FALSE]

  list(
    candidates = candidates,
    comparison_table = comparison_table,
    best = best
  )
}

run_best_empirical_simulation <- function(empirical_target,
                                          best_params,
                                          final_replicates = 50L,
                                          integrator = "linear_exact",
                                          ord0_embed = 10L,
                                          seed = 20270403L) {
  panel <- simulate_empirical_dlo_panel(
    clip_lengths = empirical_target$clip_lengths,
    community_map = empirical_target$community_map,
    loadings = empirical_target$loadings,
    measurement_var = empirical_target$measurement_var,
    eta = best_params$eta[[1]],
    zeta = best_params$zeta[[1]],
    sigma_q = best_params$sigma_q[[1]],
    dt = best_params$dt[[1]],
    n_replicates = final_replicates,
    model_name = paste0(empirical_target$model, "_synthetic_final"),
    integrator = integrator,
    seed = seed
  )

  build_property_target_from_panel(
    panel = panel,
    model_name = paste0(empirical_target$model, "_synthetic_final"),
    score_cols = empirical_target$score_cols,
    ord0_embed = ord0_embed
  )
}

make_metric_heatmap <- function(comparison_table, output_path, title_text) {
  plot_df <- comparison_table[comparison_table$metric %in% EMPIRICAL_PRIMARY_METRICS, , drop = FALSE]
  gg <- ggplot2::ggplot(plot_df, ggplot2::aes(x = metric, y = family, fill = metric_loss)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient(low = "#f3f4f6", high = "#b91c1c") +
    ggplot2::labs(title = title_text, x = NULL, y = NULL, fill = "Loss") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(output_path, gg, width = 9, height = 3.5, dpi = 150)
}

make_distribution_plot <- function(empirical_metrics,
                                   synthetic_metrics,
                                   family,
                                   output_path,
                                   title_text) {
  metric_subset <- c("lag1_acf", "total_variation", "sample_entropy", "dominant_frequency")
  plot_df <- dplyr::bind_rows(
    empirical_metrics[empirical_metrics$family == family, , drop = FALSE] %>% dplyr::mutate(dataset = "empirical"),
    synthetic_metrics[synthetic_metrics$family == family, , drop = FALSE] %>% dplyr::mutate(dataset = "synthetic")
  )
  plot_df <- plot_df[plot_df$metric %in% metric_subset & is.finite(plot_df$value), , drop = FALSE]

  gg <- ggplot2::ggplot(plot_df, ggplot2::aes(x = value, fill = dataset, color = dataset)) +
    ggplot2::geom_density(alpha = 0.25, adjust = 1.1) +
    ggplot2::facet_wrap(~metric, scales = "free", ncol = 2) +
    ggplot2::labs(title = title_text, x = NULL, y = "Density") +
    ggplot2::theme_bw(base_size = 11)
  ggplot2::ggsave(output_path, gg, width = 9, height = 6, dpi = 150)
}

make_correlation_matrix_plot <- function(empirical_cor,
                                         synthetic_cor,
                                         output_path,
                                         title_text) {
  emp_df <- as.data.frame(as.table(empirical_cor), stringsAsFactors = FALSE)
  syn_df <- as.data.frame(as.table(synthetic_cor), stringsAsFactors = FALSE)
  names(emp_df) <- c("row", "col", "value")
  names(syn_df) <- c("row", "col", "value")

  emp_df$dataset <- "empirical"
  syn_df$dataset <- "synthetic"
  diff_df <- emp_df
  diff_df$value <- syn_df$value - emp_df$value
  diff_df$dataset <- "difference"

  plot_df <- dplyr::bind_rows(emp_df, syn_df, diff_df)
  gg <- ggplot2::ggplot(plot_df, ggplot2::aes(x = row, y = col, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::facet_wrap(~dataset) +
    ggplot2::scale_fill_gradient2(low = "#1d4ed8", mid = "white", high = "#b91c1c", midpoint = 0) +
    ggplot2::labs(title = title_text, x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  ggplot2::ggsave(output_path, gg, width = 12, height = 4.5, dpi = 150)
}

run_empirical_dlo_realism_pipeline <- function(frame_scores_path,
                                               model_name,
                                               output_dir,
                                               score_cols = EMPIRICAL_EMOTION_COLUMNS,
                                               max_gap = 2L,
                                               min_complete = 0.60,
                                               min_length = 15L,
                                               ord0_embed = 10L,
                                               eta_vals = c(-0.05, -0.10, -0.20, -0.40, -0.60, -0.80),
                                               zeta_vals = c(0.05, 0.10, 0.20, 0.40, 0.60),
                                               sigma_q_vals = c(0.05, 0.10, 0.20, 0.40, 0.60),
                                               dt_vals = c(0.25, 0.50, 1.00),
                                               coarse_replicates = 50L,
                                               final_replicates = 50L,
                                               refine_top_n = 5L,
                                               integrator = "linear_exact",
                                               seed = 20260403L) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  empirical_target <- build_empirical_target(
    frame_scores_path = frame_scores_path,
    model_name = model_name,
    score_cols = score_cols,
    max_gap = max_gap,
    min_complete = min_complete,
    min_length = min_length,
    ord0_embed = ord0_embed
  )

  calibration <- calibrate_empirical_dlo_realism(
    empirical_target = empirical_target,
    eta_vals = eta_vals,
    zeta_vals = zeta_vals,
    sigma_q_vals = sigma_q_vals,
    dt_vals = dt_vals,
    coarse_replicates = coarse_replicates,
    refine_top_n = refine_top_n,
    integrator = integrator,
    ord0_embed = ord0_embed,
    seed = seed
  )

  synthetic_target <- run_best_empirical_simulation(
    empirical_target = empirical_target,
    best_params = calibration$best,
    final_replicates = final_replicates,
    integrator = integrator,
    ord0_embed = ord0_embed,
    seed = seed + 999999L
  )

  final_loss <- score_candidate_loss(empirical_target, synthetic_target)
  comparison_table <- final_loss$comparison

  utils::write.csv(empirical_target$panel, file.path(output_dir, "empirical_panel_clean.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$qc, file.path(output_dir, "empirical_qc.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$raw_metrics, file.path(output_dir, "empirical_raw_metrics.csv"), row.names = FALSE)
  utils::write.csv(empirical_target$latent_metrics, file.path(output_dir, "empirical_latent_metrics.csv"), row.names = FALSE)
  utils::write.csv(calibration$candidates, file.path(output_dir, "calibration_candidates.csv"), row.names = FALSE)
  utils::write.csv(calibration$comparison_table, file.path(output_dir, "calibration_metric_losses.csv"), row.names = FALSE)
  utils::write.csv(synthetic_target$panel, file.path(output_dir, "synthetic_panel.csv"), row.names = FALSE)
  utils::write.csv(synthetic_target$raw_metrics, file.path(output_dir, "synthetic_raw_metrics.csv"), row.names = FALSE)
  utils::write.csv(synthetic_target$latent_metrics, file.path(output_dir, "synthetic_latent_metrics.csv"), row.names = FALSE)
  utils::write.csv(comparison_table, file.path(output_dir, "final_metric_comparison.csv"), row.names = FALSE)
  saveRDS(
    list(
      empirical = empirical_target,
      calibration = calibration,
      synthetic = synthetic_target,
      final_loss = final_loss
    ),
    file.path(output_dir, "pipeline_bundle.rds")
  )

  make_metric_heatmap(
    comparison_table = comparison_table,
    output_path = file.path(output_dir, "metric_loss_heatmap.png"),
    title_text = sprintf("%s: empirical vs synthetic metric loss", model_name)
  )
  make_distribution_plot(
    empirical_metrics = empirical_target$raw_metrics,
    synthetic_metrics = synthetic_target$raw_metrics,
    family = "raw",
    output_path = file.path(output_dir, "raw_metric_distributions.png"),
    title_text = sprintf("%s: raw metric distributions", model_name)
  )
  make_distribution_plot(
    empirical_metrics = empirical_target$latent_metrics,
    synthetic_metrics = synthetic_target$latent_metrics,
    family = "latent",
    output_path = file.path(output_dir, "latent_metric_distributions.png"),
    title_text = sprintf("%s: latent metric distributions", model_name)
  )
  make_correlation_matrix_plot(
    empirical_cor = empirical_target$raw_cor,
    synthetic_cor = synthetic_target$raw_cor,
    output_path = file.path(output_dir, "raw_correlation_comparison.png"),
    title_text = sprintf("%s: raw correlation matrices", model_name)
  )

  list(
    empirical = empirical_target,
    calibration = calibration,
    synthetic = synthetic_target,
    final_loss = final_loss
  )
}

## ============================================================
## 9. PIPELINE
## ============================================================

if (isTRUE(getOption("run_extract_pipeline", TRUE))) {
  # 1) Build calibration from the current pipeline
  #    (this can take some time)
  calib_prod <- calibrate_dlo_scaling()
  calib_prod$eta_scaling
  calib_prod$zeta_factor
  
  # 2) Simulate data with fixed eta,zeta
  res_fixed_proper <- run_dynEGA_network_fixed_params(
    p          = 4,
    Q2         = 6,
    n_embed    = 100,
    sigma_q    = 0.5,
    base_err   = 0.02,
    TimeP      = 250,
    N_videos   = 100,
    eta_fixed  = -0.5,
    zeta_fixed = 0.2,
    seed       = 202502
  )
  
  # 3) Production-style estimate
  final_results <- estimate_dlo_parameters_production(
    dynEGA_obj = res_fixed_proper$dynEGA_obj,
    data       = res_fixed_proper$data,
    calib_prod = calib_prod
  )
  
  final_results$estimates
}
