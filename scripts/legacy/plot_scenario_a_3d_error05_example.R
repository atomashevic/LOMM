#!/usr/bin/env Rscript

## Illustrative Scenario A / 3D / Error SD 0.5 diagnostic panel.
## This visualizes one generated benchmark condition rather than method output.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(patchwork)
})

old_grid_only <- Sys.getenv("EMOTION_BENCHMARK_GRID_ONLY", "")
Sys.setenv(EMOTION_BENCHMARK_GRID_ONLY = "1")
source("run_emotion_dynega_benchmark.R")
if (nzchar(old_grid_only)) {
  Sys.setenv(EMOTION_BENCHMARK_GRID_ONLY = old_grid_only)
} else {
  Sys.unsetenv("EMOTION_BENCHMARK_GRID_ONLY")
}

out_dir <- file.path(getwd(), "comparison_dynega_rr_figure4")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cfg <- build_benchmark_config(smoke = FALSE)
grid <- build_emotion_dynega_grid(reps = 3L, smoke = FALSE)

scenario_row <- grid %>%
  filter(
    scenario_family == "old_low_single_stable",
    scenario_nfac == 3L,
    N == 50L,
    TimeP == 100L,
    NVAR == 4L,
    abs(Error_sd - 0.5) < 1e-10,
    abs(Loadings - 0.6) < 1e-10,
    rep == 1L
  ) %>%
  slice(1)

if (nrow(scenario_row) != 1L) {
  stop("Could not identify the requested Scenario A 3D Error SD 0.5 grid row.", call. = FALSE)
}

sim_data <- simulate_benchmark_panel(
  scen_row = scenario_row,
  cfg = cfg,
  seed = scenario_row$seed_base[[1]]
)

item_factor <- data.frame(
  item = sim_data$item_names,
  factor = rep(seq_len(sim_data$NFAC_true), each = sim_data$NVAR),
  stringsAsFactors = FALSE
)

active_subjects <- sim_data$schedule %>%
  group_by(active_dim_start) %>%
  summarise(id = first(id), .groups = "drop") %>%
  arrange(active_dim_start)

trace_rows <- lapply(seq_len(nrow(active_subjects)), function(idx) {
  id <- active_subjects$id[[idx]]
  active_factor <- active_subjects$active_dim_start[[idx]]
  rows <- which(sim_data$panel$id == id)
  item_names <- item_factor$item[item_factor$factor == active_factor]

  obs_mat <- as.matrix(sim_data$panel[rows, item_names, drop = FALSE])
  latent <- sim_data$latents[[id]][, active_factor]
  block_mean <- rowMeans(obs_mat)

  item_df <- as.data.frame(obs_mat)
  item_df$time <- seq_len(nrow(item_df))
  item_long <- item_df %>%
    pivot_longer(
      all_of(item_names),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(
      series_type = "Observed item",
      subject = paste0("Subject ", id),
      active_factor = paste0("Active factor ", active_factor)
    )

  bind_rows(
    item_long,
    data.frame(
      time = seq_along(latent),
      series = "True latent",
      value = latent,
      series_type = "True latent",
      subject = paste0("Subject ", id),
      active_factor = paste0("Active factor ", active_factor),
      stringsAsFactors = FALSE
    ),
    data.frame(
      time = seq_along(block_mean),
      series = "Observed block mean",
      value = block_mean,
      series_type = "Observed block mean",
      subject = paste0("Subject ", id),
      active_factor = paste0("Active factor ", active_factor),
      stringsAsFactors = FALSE
    )
  )
})

trace_df <- bind_rows(trace_rows) %>%
  mutate(
    series_type = factor(series_type, levels = c("Observed item", "Observed block mean", "True latent")),
    active_factor = factor(active_factor, levels = paste0("Active factor ", seq_len(sim_data$NFAC_true)))
  )

readr::write_csv(
  trace_df,
  file.path(out_dir, "figure4_scenario_a_3d_error05_example_traces.csv")
)

cor_mat <- stats::cor(
  sim_data$panel[, sim_data$item_names, drop = FALSE],
  use = "pairwise.complete.obs"
)
cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
names(cor_df) <- c("item_x", "item_y", "correlation")
cor_df <- cor_df %>%
  mutate(
    item_x = factor(item_x, levels = sim_data$item_names),
    item_y = factor(item_y, levels = rev(sim_data$item_names))
  )

readr::write_csv(
  cor_df,
  file.path(out_dir, "figure4_scenario_a_3d_error05_example_item_correlations.csv")
)

block_cor <- lapply(seq_len(sim_data$NFAC_true), function(fac) {
  rows <- which(sim_data$panel$id %in% active_subjects$id)
  item_names <- item_factor$item[item_factor$factor == fac]
  data.frame(
    factor = fac,
    mean_abs_item_latent_cor = mean(abs(stats::cor(
      sim_data$panel[rows, item_names, drop = FALSE],
      unlist(lapply(active_subjects$id, function(id) sim_data$latents[[id]][, fac])),
      use = "pairwise.complete.obs"
    )), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

metadata <- data.frame(
  field = c(
    "scenario_label", "scenario_family", "scenario_nfac", "Error_sd", "Loadings",
    "N", "TimeP", "NVAR", "rep", "seed_base", "eta", "zeta", "sigma_q",
    "active_gain", "background_gain", "switching"
  ),
  value = c(
    "Scenario A",
    as.character(scenario_row$scenario_family[[1]]),
    as.character(scenario_row$scenario_nfac[[1]]),
    as.character(scenario_row$Error_sd[[1]]),
    as.character(scenario_row$Loadings[[1]]),
    as.character(scenario_row$N[[1]]),
    as.character(scenario_row$TimeP[[1]]),
    as.character(scenario_row$NVAR[[1]]),
    as.character(scenario_row$rep[[1]]),
    as.character(scenario_row$seed_base[[1]]),
    paste(scenario_row$eta[[1]], collapse = ";"),
    paste(scenario_row$zeta[[1]], collapse = ";"),
    paste(scenario_row$sigma_q[[1]], collapse = ";"),
    as.character(scenario_row$active_gain[[1]]),
    as.character(scenario_row$background_gain[[1]]),
    as.character(scenario_row$switching[[1]])
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(metadata, file.path(out_dir, "figure4_scenario_a_3d_error05_example_metadata.csv"))
readr::write_csv(block_cor, file.path(out_dir, "figure4_scenario_a_3d_error05_example_block_cor.csv"))

okabe <- c(
  blue = "#0072B2",
  orange = "#E69F00",
  vermillion = "#D55E00",
  black = "#000000"
)

theme_example <- function(base_size = 8.5) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "grey88", linewidth = 0.22),
      panel.grid.minor = element_line(color = "grey94", linewidth = 0.16),
      strip.background = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.margin = margin(7, 8, 6, 7)
    )
}

trace_plot <- ggplot(trace_df, aes(x = time, y = value, group = series)) +
  geom_line(
    data = trace_df %>% filter(series_type == "Observed item"),
    color = "grey65",
    alpha = 0.35,
    linewidth = 0.28
  ) +
  geom_line(
    data = trace_df %>% filter(series_type == "Observed block mean"),
    aes(color = series_type),
    linewidth = 0.55
  ) +
  geom_line(
    data = trace_df %>% filter(series_type == "True latent"),
    aes(color = series_type),
    linewidth = 0.65
  ) +
  facet_wrap(~ active_factor, nrow = 1) +
  scale_color_manual(
    values = c("Observed block mean" = okabe[["orange"]], "True latent" = okabe[["black"]]),
    name = NULL
  ) +
  labs(
    x = "Time point",
    y = "Raw value",
    title = "Scenario A, 3D, measurement-error SD 0.5",
    subtitle = "One generated benchmark condition; grey lines are the four observed indicators for the active factor"
  ) +
  theme_example(8.5)

heat_plot <- ggplot(cor_df, aes(x = item_x, y = item_y, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.12) +
  geom_vline(xintercept = c(4.5, 8.5), color = "grey20", linewidth = 0.35) +
  geom_hline(yintercept = c(4.5, 8.5), color = "grey20", linewidth = 0.35) +
  coord_equal() +
  scale_fill_gradient2(
    low = okabe[["blue"]],
    mid = "white",
    high = okabe[["vermillion"]],
    midpoint = 0,
    limits = c(-1, 1),
    name = "r"
  ) +
  labs(
    x = "Observed item",
    y = "Observed item",
    title = "Observed item correlations across the full simulated panel",
    subtitle = "Block boundaries mark the three true factors"
  ) +
  theme_example(8.0) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

combined <- trace_plot / heat_plot + plot_layout(heights = c(1.0, 1.15))

ggsave(
  file.path(out_dir, "figure4_scenario_a_3d_error05_example.png"),
  combined,
  width = 8.4,
  height = 7.2,
  dpi = 320
)
ggsave(
  file.path(out_dir, "figure4_scenario_a_3d_error05_example.pdf"),
  combined,
  width = 8.4,
  height = 7.2,
  device = cairo_pdf
)

caption <- c(
  "Scenario A 3D example with measurement-error SD 0.5 and loading 0.6.",
  "Top panels show one active-factor trajectory for three representative subjects from the same generated condition; grey lines are individual observed indicators, orange is their block mean, and black is the true latent factor.",
  "Bottom panel shows the observed-item correlation matrix for the full simulated panel of 50 subjects and 100 time points.",
  "The weak block structure illustrates why Scenario A is difficult: low latent innovation, low active gain, strong damping, and high measurement error leave little recoverable measurement-block signal."
)
writeLines(caption, file.path(out_dir, "figure4_scenario_a_3d_error05_example_caption.txt"))

cat("Saved Scenario A 3D Error SD 0.5 example to ", out_dir, "\n", sep = "")
