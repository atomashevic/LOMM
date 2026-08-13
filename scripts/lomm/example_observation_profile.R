source("scripts/lomm/observation_profile.R")

component_names <- c("anger", "fear", "happiness", "neutral")
basis <- stats::contr.helmert(length(component_names))
basis <- sweep(basis, 2L, sqrt(colSums(basis^2)), "/")
rownames(basis) <- component_names
colnames(basis) <- sprintf("ilr_%02d", seq_len(ncol(basis)))

factor_names <- c("approach", "threat")
loadings <- matrix(
  c(0.80, 0.00, 0.20, 0.65, 0.45, 0.25),
  nrow = ncol(basis),
  byrow = TRUE,
  dimnames = list(colnames(basis), factor_names)
)
latent_positions <- cbind(
  approach = sin(seq(0, pi, length.out = 12L)),
  threat = cos(seq(0, pi, length.out = 12L))
)
rownames(latent_positions) <- sprintf("frame_%02d", seq_len(nrow(latent_positions)))
residuals <- matrix(
  0,
  nrow = nrow(latent_positions),
  ncol = nrow(loadings),
  dimnames = list(rownames(latent_positions), rownames(loadings))
)
center <- stats::setNames(rep(0, nrow(loadings)), rownames(loadings))
between_clip_covariance <- diag(rep(0.05, nrow(loadings)))
dimnames(between_clip_covariance) <- list(rownames(loadings), rownames(loadings))

observed <- lomm_observe(
  latent_positions = latent_positions,
  loadings = loadings,
  residuals = residuals,
  center = center,
  basis = basis,
  clip_key = "worked-example",
  master_seed = 20260813L,
  between_clip_covariance = between_clip_covariance
)

stopifnot(
  all(abs(rowSums(observed$probability) - 1) < 1e-12),
  identical(observed$metadata$observation_surface, "bounded_composition")
)

print(round(utils::head(observed$probability, 4L), 3L))
print(observed$metadata[c("public_profile", "baseline_mode", "observation_surface")])
