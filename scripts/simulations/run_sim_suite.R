# Comprehensive simulation suite for dynEGA/DLO recovery
# - Explores multiple integrators, latent dimensions (via Q2), eta/zeta pairs,
#   time lengths, and number of videos
# - Runs recovery with three estimators: raw finite-difference, dynEGA scores, state-space
# - Saves raw results, summaries, and plots

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

options(run_extract_pipeline = FALSE)
source("extract.r")

# Configuration --------------------------------------------------------------
sim_config <- list(
  integrators   = c("euler_explicit", "euler_semi_implicit",
                    "velocity_verlet", "rk4", "linear_exact"),
  Q2_vals       = c(1.5, 4, 10),    # roughly 2,4,6 factors
  eta_vals      = c(-0.2, -0.5),
  zeta_vals     = c(0.1, 0.3),
  TimeP_vals    = 300,
  N_video_vals  = 30,
  seeds_per     = 2,               # 2 reps per scenario → ~480 runs with 5 integrators
  p             = 4,
  sigma_q       = 0.5,
  base_err      = 0.02,
  n_embed       = 35,
  dt            = NULL,            # let wrapper choose
  ncores_dynEGA = 12,
  out_dir       = file.path("simulation_outputs", "sim_suite")
)

# Utility to build scenario grid (without integrator dimension)
build_scenarios <- function(cfg) {
  base <- expand.grid(
    Q2      = cfg$Q2_vals,
    eta     = cfg$eta_vals,
    zeta    = cfg$zeta_vals,
    TimeP   = cfg$TimeP_vals,
    N_video = cfg$N_video_vals,
    rep     = seq_len(cfg$seeds_per),
    stringsAsFactors = FALSE
  )
  base$seed_base <- 100000 + seq_len(nrow(base))
  base
}

# Run a single scenario for one integrator
run_one_sim <- function(cfg, scen_row, integrator) {
  seed <- scen_row$seed_base + match(integrator, cfg$integrators) * 1000
  
  sim <- tryCatch(
    run_dynEGA_network_fixed_params(
      p            = cfg$p,
      Q2           = scen_row$Q2,
      n_embed      = cfg$n_embed,
      sigma_q      = cfg$sigma_q,
      base_err     = cfg$base_err,
      TimeP        = scen_row$TimeP,
      N_videos     = scen_row$N_video,
      eta_fixed    = scen_row$eta,
      zeta_fixed   = scen_row$zeta,
      seed         = seed,
      integrator   = integrator,
      return_latent = TRUE,
      ncores       = cfg$ncores_dynEGA
    ),
    error = function(e) NULL
  )
  if (is.null(sim)) return(NULL)
  
  id_var <- if ("id" %in% names(sim$data)) "id" else names(sim$data)[ncol(sim$data)]
  
  latent_obs_cor <- compute_latent_obs_correlation(
    latents = sim$latents,
    data    = sim$data,
    NVAR    = sim$NVAR
  )
  
  est_raw <- tryCatch(
    estimate_dlo_from_dynEGA(
      dynEGA_obj = sim$dynEGA_obj,
      data       = sim$data,
      dt         = sim$dt_used,
      dt_eta     = sim$dt_used,
      verbose    = FALSE
    ),
    error = function(e) NULL
  )
  
  scores <- compute_net_scores_safe(
    sim$dynEGA_obj,
    sim$data[, setdiff(names(sim$data), id_var), drop = FALSE]
  )
  
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
  
  score_latent_cor <- compute_latent_score_alignment(
    scores    = scores,
    latents   = sim$latents,
    id_vector = sim$data[[id_var]]
  )
  
  data.frame(
    integrator      = integrator,
    Q2              = scen_row$Q2,
    eta_true        = scen_row$eta,
    zeta_true       = scen_row$zeta,
    TimeP           = scen_row$TimeP,
    N_video         = scen_row$N_video,
    rep             = scen_row$rep,
    latent_obs_cor  = latent_obs_cor,
    score_latent_cor = score_latent_cor,
    est_factors     = sim$dynEGA_obj$dynEGA$population$n.dim,
    eta_hat_raw     = if (!is.null(est_raw)) est_raw$eta$summary$eta_median else NA_real_,
    zeta_hat_raw    = if (!is.null(est_raw)) est_raw$zeta_fixed else NA_real_,
    n_est_raw       = if (!is.null(est_raw)) est_raw$eta$summary$n_estimates else NA_integer_,
    eta_hat_scores  = if (!is.null(est_scores)) est_scores$eta_hat else NA_real_,
    zeta_hat_scores = if (!is.null(est_scores)) est_scores$zeta_hat else NA_real_,
    n_est_scores    = if (!is.null(est_scores)) est_scores$n_rows else NA_integer_,
    eta_hat_ss      = if (!is.null(est_ss)) est_ss$eta_hat_mean else NA_real_,
    zeta_hat_ss     = if (!is.null(est_ss)) est_ss$zeta_hat_mean else NA_real_,
    n_est_ss        = if (!is.null(est_ss)) est_ss$n_obs else NA_integer_,
    stringsAsFactors = FALSE
  )
}

# Run full grid
run_sim_suite <- function(cfg, small = FALSE) {
  scenarios <- build_scenarios(cfg)
  if (small) {
    scenarios <- scenarios %>% dplyr::slice_head(n = 5)
    cfg$integrators <- cfg$integrators[1:2]
  }
  
  results <- list()
  idx <- 1L
  for (i in seq_len(nrow(scenarios))) {
    scen <- scenarios[i, ]
    for (method in cfg$integrators) {
      res <- run_one_sim(cfg, scen, method)
      results[[idx]] <- res
      idx <- idx + 1L
    }
  }
  
  dplyr::bind_rows(results)
}

# Summaries and plots -------------------------------------------------------
summarize_results <- function(df) {
  df %>%
    mutate(
      eta_err_raw    = eta_hat_raw    - eta_true,
      zeta_err_raw   = zeta_hat_raw   - zeta_true,
      eta_err_scores = eta_hat_scores - eta_true,
      zeta_err_scores= zeta_hat_scores- zeta_true,
      eta_err_ss     = eta_hat_ss     - eta_true,
      zeta_err_ss    = zeta_hat_ss    - zeta_true
    ) %>%
    pivot_longer(
      cols = c(eta_err_raw, zeta_err_raw,
               eta_err_scores, zeta_err_scores,
               eta_err_ss, zeta_err_ss),
      names_to = c("param", "method"),
      names_pattern = "(eta|zeta)_err_(raw|scores|ss)",
      values_to = "error"
    ) %>%
    group_by(integrator, param, method, Q2, TimeP, N_video) %>%
    summarize(
      mean_err = mean(error, na.rm = TRUE),
      sd_err   = sd(error, na.rm = TRUE),
      rmse     = sqrt(mean(error^2, na.rm = TRUE)),
      n        = sum(!is.na(error)),
      ci_lower = mean_err - 1.96 * sd_err / sqrt(pmax(n, 1)),
      ci_upper = mean_err + 1.96 * sd_err / sqrt(pmax(n, 1)),
      .groups = "drop"
    )
}

plot_error_summary <- function(summary_df, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  p <- ggplot(summary_df, aes(x = integrator, y = mean_err, color = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2,
                  position = position_dodge(width = 0.6)) +
    facet_grid(param ~ TimeP + N_video + Q2, scales = "free_y") +
    labs(title = "Bias with 95% CI by integrator and recovery method",
         y = "Mean error (estimate - truth)", x = NULL, color = "Recovery") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, "sim_bias_ci.png"), p, width = 14, height = 8, dpi = 160)
  p
}

plot_rmse <- function(summary_df, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  p <- ggplot(summary_df, aes(x = integrator, y = rmse, fill = method)) +
    geom_col(position = position_dodge(width = 0.6)) +
    facet_grid(param ~ TimeP + N_video + Q2, scales = "free_y") +
    labs(title = "RMSE by integrator and recovery method",
         y = "RMSE", x = NULL, fill = "Recovery") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, "sim_rmse.png"), p, width = 14, height = 8, dpi = 160)
  p
}

# Driver --------------------------------------------------------------------
if (identical(tolower(Sys.getenv("RUN_SIM_SUITE", "0")), "1")) {
  cfg <- sim_config
  if (!dir.exists(cfg$out_dir)) dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Running simulation suite... this may take time.\n")
  res <- run_sim_suite(cfg, small = FALSE)
  saveRDS(res, file.path(cfg$out_dir, "sim_results.rds"))
  
  summary_df <- summarize_results(res)
  write.csv(summary_df, file.path(cfg$out_dir, "sim_summary.csv"), row.names = FALSE)
  
  plot_error_summary(summary_df, cfg$out_dir)
  plot_rmse(summary_df, cfg$out_dir)
  
  cat("Saved outputs to", cfg$out_dir, "\n")
} else if (identical(tolower(Sys.getenv("RUN_SIM_SUITE_SMOKE", "0")), "1")) {
  cfg <- sim_config
  cfg$seeds_per <- 1
  cfg$integrators <- cfg$integrators[1:2]
  cfg$Q2_vals <- cfg$Q2_vals[1]
  cfg$eta_vals <- cfg$eta_vals[1]
  cfg$zeta_vals <- cfg$zeta_vals[1]
  cfg$TimeP_vals <- 100
  cfg$N_video_vals <- 5
  cfg$n_embed <- 20
  cfg$ncores_dynEGA <- 8
  cfg$out_dir <- file.path("simulation_outputs", "sim_suite_smoke")
  if (!dir.exists(cfg$out_dir)) dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Running SMOKE simulation suite (small)...\n")
  res <- run_sim_suite(cfg, small = TRUE)
  saveRDS(res, file.path(cfg$out_dir, "sim_results_smoke.rds"))
  
  summary_df <- summarize_results(res)
  write.csv(summary_df, file.path(cfg$out_dir, "sim_summary_smoke.csv"), row.names = FALSE)
  
  plot_error_summary(summary_df, cfg$out_dir)
  plot_rmse(summary_df, cfg$out_dir)
  
  cat("Saved smoke outputs to", cfg$out_dir, "\n")
}
