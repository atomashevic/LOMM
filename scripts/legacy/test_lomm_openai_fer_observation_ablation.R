#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)
source("extract.r")

score_cols <- c("anger", "fear", "happiness", "sadness")
community_map <- c(anger = 1, fear = 1, happiness = 2, sadness = 2)
loadings <- c(anger = 0.7, fear = 0.8, happiness = 0.75, sadness = 0.65)
measurement_var <- c(anger = 0.30, fear = 0.25, happiness = 0.28, sadness = 0.32)

set.seed(20260519)

q <- generate_q_multi(num_steps = 20, NFAC = 3, sigma_q = c(0.1, 0.2, 0.3), rho_q = 0.25)
stopifnot(is.matrix(q), all(dim(q) == c(20, 3)), all(is.finite(q)))

Sigma <- make_residual_cov(measurement_var, resid_rho = 0.25)
stopifnot(all(dim(Sigma) == c(4, 4)))
stopifnot(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values) > 0)

latent <- simulate_latent_multi(
  dt = 0.5,
  num_steps = 12,
  NFAC = 2,
  sigma_q = 0.1,
  eta = c(-0.05, -0.10),
  zeta = c(0.08, 0.10),
  integrator = "linear_exact",
  rho_q = 0.3,
  init_x_sd = 0.1,
  init_dx_sd = 0.5
)
stopifnot(is.matrix(latent), all(dim(latent) == c(12, 2)), all(is.finite(latent)))

obs <- simulate_named_observables(
  latent = latent,
  community_map = community_map,
  loadings = loadings,
  measurement_var = measurement_var,
  obs_scale = 0.65,
  resid_rho = 0.2,
  common_sd = 0.05,
  common_phi = 0.5,
  cross_loading_sd = 0.025,
  link = "softmax",
  temperature = 1.0
)
stopifnot(all(names(obs) == score_cols))
stopifnot(max(abs(rowSums(obs) - 1)) < 1e-8)

panel <- simulate_empirical_dlo_panel(
  clip_lengths = c(clip_a = 8, clip_b = 9),
  community_map = community_map,
  loadings = loadings,
  measurement_var = measurement_var,
  eta = c(-0.05, -0.10),
  zeta = c(0.08, 0.10),
  sigma_q = 0.15,
  dt = 0.5,
  n_replicates = 1,
  model_name = "test",
  rho_q = 0.2,
  obs_scale = 0.7,
  resid_rho = 0.1,
  common_sd = 0.05,
  cross_loading_sd = 0.025,
  seed = 20260519
)
stopifnot(nrow(panel) == 17)
stopifnot(max(abs(rowSums(panel[score_cols]) - 1)) < 1e-8)

rescaled <- robust_rescale_panel(panel, panel, score_cols, shrink = 1.0)
stopifnot(all(dim(rescaled) == dim(panel)))

cors <- cor_sign_summary(panel, score_cols, label = "test")
stopifnot(nrow(cors) == 1, is.finite(cors$mean_abs_pairwise_cor))

message("LOMM OpenAI-FER observation ablation tests passed.")
