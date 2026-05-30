#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)
source("extract.r")
source("run_lomm_openai_fer_range_geometry_diagnostics.R")

score_cols <- c("anger", "fear", "happiness", "sadness")
community_map <- c(anger = 1, fear = 1, happiness = 2, sadness = 2)
loadings <- c(anger = 0.7, fear = 0.8, happiness = 0.75, sadness = 0.65)
measurement_var <- c(anger = 0.30, fear = 0.25, happiness = 0.28, sadness = 0.32)

set.seed(20260520)

latent_burn <- simulate_latent_multi(
  dt = 0.5,
  num_steps = 10,
  NFAC = 2,
  sigma_q = 0.1,
  eta = c(-0.05, -0.10),
  zeta = c(0.08, 0.10),
  integrator = "linear_exact",
  burn_in = 25L
)
stopifnot(is.matrix(latent_burn), all(dim(latent_burn) == c(10, 2)), all(is.finite(latent_burn)))

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
  resid_rho = 0.1,
  burn_in = 20L,
  seed = 20260520
)
stopifnot(nrow(panel) == 17, max(abs(rowSums(panel[score_cols]) - 1)) < 1e-8)

amp <- amplitude_decomp(panel, score_cols)
stopifnot(nrow(amp) == length(score_cols))
stopifnot(all(is.finite(amp$pooled_iqr)), all(is.finite(amp$mean_within_iqr)))

amp_cmp <- compare_amplitude_decomp(panel, panel, score_cols)
stopifnot(nrow(amp_cmp) == length(score_cols))
stopifnot(max(abs(amp_cmp$pooled_iqr_ratio - 1)) < 1e-8)

tanh_panel <- apply_empirical_tanh_link(
  synthetic_panel = panel,
  empirical_panel = panel,
  score_cols = score_cols,
  radius_multiplier = 1.5,
  slope = 1.0
)
stopifnot(all(dim(tanh_panel) == dim(panel)))
stopifnot(all(is.finite(as.matrix(tanh_panel[score_cols]))))

candidate_summary <- data.frame(
  candidate_id = 1:2,
  stage = c("a", "b"),
  total_loss = c(1.0, 2.0),
  raw_loss = c(1.0, 2.0),
  latent_loss = c(1.0, 2.0),
  diagnostic = FALSE
)
metric_comparison_long <- data.frame(
  candidate_id = rep(1:2, each = 4),
  family = rep(c("raw", "latent"), times = 4),
  metric = rep(c("channel_iqr", "channel_iqr", "diff_iqr", "diff_iqr"), times = 2),
  median_diff_scaled = c(0.2, 0.4, 0.1, 0.3, 0.8, 1.2, 0.2, 0.5),
  wasserstein_scaled = c(0.3, 0.5, 0.2, 0.4, 0.9, 1.3, 0.3, 0.6)
)
frontier <- candidate_channel_iqr_frontier(candidate_summary, metric_comparison_long)
stopifnot(nrow(frontier) == 2)
stopifnot(frontier$candidate_id[[1]] == 1)

with_calls <- add_paper_calls(metric_comparison_long)
stopifnot(all(c("max_discrepancy", "paper_call") %in% names(with_calls)))

message("LOMM OpenAI-FER range-geometry diagnostics tests passed.")
