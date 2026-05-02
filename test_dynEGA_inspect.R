# Lightweight dynEGA sanity script for manual inspection
# - Uses small simulation to keep runtime short
# - Produces a dynEGA object you can explore (str(), View(), etc.)
# - Saves the full object to an RDS for reuse

options(run_extract_pipeline = FALSE)
source("extract.r")

# Small sim settings
sim_res <- run_dynEGA_network_fixed_params(
  p            = 4,
  Q2           = 6,
  n_embed      = 15,
  sigma_q      = 0.5,
  base_err     = 0.02,
  TimeP        = 120,
  N_videos     = 5,
  eta_fixed    = -0.5,
  zeta_fixed   = 0.2,
  seed         = 1234,
  integrator   = "rk4",
  return_latent = TRUE
)

dyn_obj <- sim_res$dynEGA_obj

cat("dynEGA_obj top-level names:\n")
print(names(dyn_obj))

cat("\nPopulation slot names (if available):\n")
pop <- dyn_obj$dynEGA$population
if (!is.null(pop)) {
  print(names(pop))
  if (!is.null(pop$wc)) {
    cat("Population wc length:", length(pop$wc), "\n")
  }
  if (!is.null(pop$network)) {
    cat("Population network dim:", paste(dim(pop$network), collapse = "x"), "\n")
  }
}

cat("\nSaving dyn_obj to dynEGA_small.rds\n")
saveRDS(dyn_obj, file = "dynEGA_small.rds")

cat("\nTo explore manually in R:\n")
cat("  dyn_obj <- readRDS('dynEGA_small.rds')\n")
cat("  str(dyn_obj, max.level = 3)\n")
