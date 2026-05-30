#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(patchwork)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_dlo_figure2_latent_observable_examples.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

options(run_extract_pipeline = FALSE)
source(file.path(repo_root, "extract.r"))

out_dir <- Sys.getenv(
  "DLO_FIGURE2_OUT_DIR",
  file.path(repo_root, "comparison_revision_report_v2")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

factor_cols <- c(
  `Factor 1` = "#D62728", # red
  `Factor 2` = "#0072B2", # blue
  `Factor 3` = "#006D2C"  # dark green
)

observable_cols <- c(
  F1a = "#D62728",
  F1b = "#FB6A4A",
  F1c = "#A50F15",
  F2a = "#0072B2",
  F2b = "#56B4E9",
  F2c = "#084081",
  F3a = "#006D2C",
  F3b = "#31A354",
  F3c = "#00441B"
)

theme_figure2 <- function(base_size = 8) {
  theme_classic(base_size = base_size) +
    theme(
      axis.line = element_line(linewidth = 0.25, color = "grey20"),
      axis.ticks = element_line(linewidth = 0.25, color = "grey20"),
      legend.position = "right",
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      plot.title = element_blank(),
      plot.margin = margin(4, 6, 4, 4)
    )
}

unit_deviation <- function(x) {
  x <- x - mean(x, na.rm = TRUE)
  scale <- max(abs(x), na.rm = TRUE)
  if (!is.finite(scale) || scale <= .Machine$double.eps) {
    return(rep(0, length(x)))
  }
  x / scale
}

clamp01 <- function(x) {
  pmin(pmax(x, 0), 1)
}

centered_moving_average <- function(x, window = 11L) {
  radius <- floor(window / 2L)
  vapply(seq_along(x), function(i) {
    idx <- max(1L, i - radius):min(length(x), i + radius)
    mean(x[idx], na.rm = TRUE)
  }, numeric(1L))
}

simulate_latent_scores <- function(seed = 20260517L) {
  set.seed(seed)

  num_steps <- 300L
  burn_in <- 90L
  n_factors <- 3L

  raw_latent <- simulate_latent_multi(
    dt = 0.4,
    num_steps = num_steps + burn_in,
    NFAC = n_factors,
    eta = c(-0.40, -0.55, -0.75),
    zeta = c(-1.20, -0.95, -0.70),
    sigma_q = c(0.32, 0.42, 0.22),
    integrator = "linear_exact"
  )
  raw_latent <- raw_latent[(burn_in + 1L):(burn_in + num_steps), , drop = FALSE]

  factor_levels <- c(0.48, 0.36, 0.24)
  factor_amplitudes <- c(0.15, 0.14, 0.10)

  latent <- do.call(cbind, lapply(seq_len(n_factors), function(factor_idx) {
    clamp01(
      factor_levels[[factor_idx]] +
        factor_amplitudes[[factor_idx]] * unit_deviation(raw_latent[, factor_idx])
    )
  }))
  colnames(latent) <- paste0("Factor ", seq_len(n_factors))

  list(
    latent = latent,
    raw_latent = raw_latent,
    num_steps = num_steps,
    eta = c(-0.40, -0.55, -0.75),
    zeta = c(-1.20, -0.95, -0.70),
    sigma_q = c(0.32, 0.42, 0.22),
    dt = 0.4
  )
}

generate_observable_scores <- function(latent, seed = 4240L) {
  set.seed(seed)

  n_factors <- ncol(latent)
  n_items_per_factor <- 3L

  observable_matrix <- generate_observables_multi(
    latent = latent,
    NFAC = n_factors,
    NVAR = n_items_per_factor,
    error = 0.0105,
    loadings = 0.50
  )
  colnames(observable_matrix) <- names(observable_cols)

  as.data.frame(observable_matrix, stringsAsFactors = FALSE) %>%
    mutate(frame = seq_len(n())) %>%
    pivot_longer(
      cols = all_of(names(observable_cols)),
      names_to = "observable",
      values_to = "score"
    ) %>%
    mutate(
      factor_index = ceiling(match(observable, names(observable_cols)) / n_items_per_factor),
      factor = paste0("Factor ", factor_index),
      score = clamp01(score)
    ) %>%
    dplyr::select(frame, observable, factor, score)
}

simulation <- simulate_latent_scores()

latent_data <- as.data.frame(simulation$latent, stringsAsFactors = FALSE) %>%
  mutate(frame = seq_len(n())) %>%
  pivot_longer(
    cols = starts_with("Factor "),
    names_to = "factor",
    values_to = "score"
  ) %>%
  mutate(
    factor = factor(factor, levels = names(factor_cols)),
    panel = "Latent scores",
    raw_score = score
  )

observable_data <- generate_observable_scores(simulation$latent) %>%
  mutate(
    factor = factor(factor, levels = names(factor_cols)),
    observable = factor(observable, levels = names(observable_cols)),
    panel = "Observable scores",
    raw_score = score
  ) %>%
  arrange(observable, frame) %>%
  group_by(observable) %>%
  mutate(score = centered_moving_average(raw_score, window = 11L)) %>%
  ungroup()

figure_data <- bind_rows(
  latent_data %>%
    transmute(
      panel, frame, series = as.character(factor), factor = as.character(factor),
      raw_score, score, series_type = "latent"
    ),
  observable_data %>%
    transmute(
      panel, frame, series = as.character(observable), factor = as.character(factor),
      raw_score, score, series_type = "observable"
    )
)

latent_plot <- ggplot(latent_data, aes(x = frame, y = score, color = factor)) +
  geom_line(linewidth = 0.36, alpha = 0.68) +
  scale_color_manual(values = factor_cols, name = "Latent score") +
  scale_x_continuous(breaks = c(1, 100, 200, 300), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = label_number(accuracy = 0.25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = "Frame", y = "Score") +
  theme_figure2()

observable_plot <- ggplot(observable_data, aes(x = frame, y = score, color = observable)) +
  geom_line(linewidth = 0.32, alpha = 0.36) +
  scale_color_manual(values = observable_cols, name = "Observable score") +
  scale_x_continuous(breaks = c(1, 100, 200, 300), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    limits = c(0, 0.65),
    breaks = c(0, 0.2, 0.4, 0.6),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = "Frame", y = "Score") +
  theme_figure2()

figure2 <- latent_plot / observable_plot +
  plot_layout(heights = c(0.9, 1.1))

readr::write_csv(
  figure_data,
  file.path(out_dir, "figure2_dlo_latent_observable_examples_data.csv")
)

ggsave(
  file.path(out_dir, "figure2_dlo_latent_observable_examples.pdf"),
  figure2,
  width = 7.2,
  height = 4.8,
  device = cairo_pdf
)

ggsave(
  file.path(out_dir, "figure2_dlo_latent_observable_examples.png"),
  figure2,
  width = 7.2,
  height = 4.8,
  dpi = 320
)
