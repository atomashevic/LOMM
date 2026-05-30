#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_dlo_figure1_model_examples.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

options(run_extract_pipeline = FALSE)
source(file.path(repo_root, "extract.r"))

out_dir <- Sys.getenv(
  "DLO_FIGURE1_OUT_DIR",
  file.path(repo_root, "comparison_revision_report_v2")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

okabe <- c(
  blue = "#0072B2",
  vermillion = "#D55E00",
  green = "#009E73",
  orange = "#E69F00",
  purple = "#CC79A7",
  sky = "#56B4E9"
)

factor_cols <- c(
  Affiliative = okabe[["blue"]],
  Threat = okabe[["vermillion"]],
  Orienting = okabe[["green"]]
)

theme_figure1 <- function(base_size = 8) {
  theme_classic(base_size = base_size) +
    theme(
      axis.line = element_line(linewidth = 0.25, color = "grey20"),
      axis.ticks = element_line(linewidth = 0.25, color = "grey20"),
      legend.position = "right",
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      panel.spacing = grid::unit(0.36, "lines"),
      strip.background = element_blank(),
      strip.text.x = element_text(face = "bold", size = base_size + 0.25),
      strip.text.y = element_blank(),
      plot.margin = margin(5, 7, 5, 5),
      plot.title.position = "plot"
    )
}

rescale_series_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) <= .Machine$double.eps) {
    return(rep(0.5, length(x)))
  }
  (x - rng[[1]]) / diff(rng)
}

rolling_mean <- function(x, window = 11L) {
  n <- length(x)
  half_window <- floor(window / 2)
  vapply(seq_len(n), function(idx) {
    lo <- max(1L, idx - half_window)
    hi <- min(n, idx + half_window)
    mean(x[lo:hi], na.rm = TRUE)
  }, numeric(1))
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

format_param <- function(x) {
  formatC(x, format = "f", digits = 2)
}

deterministic_curve <- function(frame, start, target, rate) {
  target + (start - target) * exp(-rate * (frame - 1L))
}

simulate_case <- function(case) {
  set.seed(case$seed)

  latent <- simulate_latent_multi(
    dt = case$dt,
    num_steps = case$num_steps + case$burn_in,
    NFAC = case$n_factors,
    eta = case$eta,
    zeta = case$zeta,
    sigma_q = case$sigma_q,
    integrator = case$integrator
  )
  latent <- latent[(case$burn_in + 1L):(case$burn_in + case$num_steps), , drop = FALSE]

  stochastic_long <- bind_rows(lapply(seq_len(case$n_factors), function(factor_idx) {
    raw_dlo <- latent[, factor_idx]
    score <- clamp01(
      case$stochastic_level[[factor_idx]] +
        case$stochastic_amplitude[[factor_idx]] * unit_deviation(raw_dlo)
    )

    data.frame(
      time = seq_len(case$num_steps),
      factor = case$factor_names[[factor_idx]],
      raw_value = score,
      smooth_value = score,
      value = score,
      dlo_value = latent[, factor_idx],
      smooth_dlo_value = raw_dlo,
      eta = case$eta[[factor_idx]],
      zeta = case$zeta[[factor_idx]],
      sigma_q = case$sigma_q[[factor_idx]],
      panel = "Simulated linear oscillator trajectories",
      curve_source = "simulate_latent_multi",
      stringsAsFactors = FALSE
    )
  }))

  deterministic_long <- bind_rows(lapply(seq_len(case$n_factors), function(factor_idx) {
    data.frame(
      time = seq_len(case$num_steps),
      factor = case$factor_names[[factor_idx]],
      raw_value = deterministic_curve(
        frame = seq_len(case$num_steps),
        start = case$deterministic_start[[factor_idx]],
        target = case$deterministic_target[[factor_idx]],
        rate = case$deterministic_rate[[factor_idx]]
      ),
      dlo_value = NA_real_,
      smooth_dlo_value = NA_real_,
      eta = case$eta[[factor_idx]],
      zeta = case$zeta[[factor_idx]],
      sigma_q = 0,
      stringsAsFactors = FALSE
    )
  })) %>%
    mutate(
      panel = "Reference trajectories",
      curve_source = "conceptual",
      smooth_value = raw_value,
      value = raw_value
    ) %>%
    ungroup()

  bind_rows(deterministic_long, stochastic_long) %>%
    mutate(
      model = case$model,
      model_label = case$model_label,
      dt = case$dt,
      integrator = case$integrator,
      n_factors = case$n_factors,
      rolling_window = case$rolling_window
    )
}

cases <- list(
  list(
    model = "3_factor_plutchik_informed",
    model_label = "Three latent expressive dimensions",
    n_factors = 3L,
    factor_names = c(
      "Affiliative",
      "Threat",
      "Orienting"
    ),
    eta = c(-0.30, -0.55, -0.85),
    zeta = c(-1.70, -1.00, -0.65),
    sigma_q = c(0.12, 0.30, 0.45),
    seed = 20260518L,
    num_steps = 300L,
    burn_in = 80L,
    dt = 0.4,
    integrator = "linear_exact",
    rolling_window = 11L,
    deterministic_start = c(0.24, 0.30, 0.18),
    deterministic_target = c(0.62, 0.44, 0.34),
    deterministic_rate = c(0.024, 0.030, 0.038),
    stochastic_level = c(0.50, 0.38, 0.30),
    stochastic_amplitude = c(0.16, 0.14, 0.11)
  )
)

factor_levels <- unique(unlist(lapply(cases, `[[`, "factor_names"), use.names = FALSE))

plot_data <- bind_rows(lapply(cases, simulate_case)) %>%
  mutate(
    model_label = factor(model_label, levels = vapply(cases, `[[`, character(1), "model_label")),
    panel = factor(panel, levels = c("Reference trajectories", "Simulated linear oscillator trajectories")),
    factor = factor(factor, levels = factor_levels)
  )

annotation_data <- plot_data %>%
  distinct(model_label, panel, n_factors, integrator, dt) %>%
  mutate(
    label = if_else(
      panel == "Reference trajectories",
      paste0(
        "m = ", n_factors,
        "\nconceptual paths",
        "\nσ_q = 0"
      ),
      paste0(
        "m = ", n_factors,
        "\n", integrator,
        "\nfactor-specific η, ζ, σ_q"
      )
    ),
    label_x = 178,
    label_y = 0.98
  )

figure1 <- ggplot(plot_data, aes(x = time, y = value, color = factor)) +
  geom_line(linewidth = 0.35, alpha = 0.94) +
  geom_label(
    data = annotation_data,
    aes(x = label_x, y = label_y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 2.45,
    lineheight = 0.95,
    color = "grey15",
    fill = scales::alpha("white", 0.95),
    linewidth = 0,
    label.padding = grid::unit(0.08, "lines")
  ) +
  facet_grid(model_label ~ panel) +
  scale_color_manual(values = factor_cols, drop = FALSE, name = "Latent dimension") +
  scale_x_continuous(breaks = c(1, 100, 200, 300), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.25),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    limits = c(-0.03, 1.03),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Time", y = "Scaled latent score") +
  guides(color = guide_legend(override.aes = list(linewidth = 0.65, alpha = 1))) +
  theme_figure1()

readr::write_csv(
  plot_data,
  file.path(out_dir, "figure1_dlo_model_examples_data.csv")
)

ggsave(
  file.path(out_dir, "figure1_dlo_model_examples.pdf"),
  figure1,
  width = 7.2,
  height = 3.4,
  device = cairo_pdf
)

ggsave(
  file.path(out_dir, "figure1_dlo_model_examples.png"),
  figure1,
  width = 7.2,
  height = 3.4,
  dpi = 320
)
