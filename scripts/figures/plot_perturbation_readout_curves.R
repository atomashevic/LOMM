#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

repo_root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"))) {
  stop("Run this script from the repository root.")
}

figure_dir <- file.path(repo_root, "paper", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Population readout maps for the symmetric block target (Appendix A).
# Marginal readout: w^M(theta) = theta / (1 + theta).
# Partial readout:  w^P(theta) = theta / (1 + (k - 1) * theta), k = 4.
k_indicators <- 4L

theta_grid <- 10^seq(-2, 3, length.out = 600)
curves <- bind_rows(
  tibble(
    theta = theta_grid,
    edge = theta_grid / (1 + theta_grid),
    Readout = "Marginal readout"
  ),
  tibble(
    theta = theta_grid,
    edge = theta_grid / (1 + (k_indicators - 1) * theta_grid),
    Readout = "Partial readout"
  )
) |>
  mutate(Readout = factor(Readout, levels = c("Marginal readout", "Partial readout")))

# Matched-grid measurement settings: loadings and error standard deviations.
# theta_{L,f} = lambda^2 * V_{L,f} / psi_L with psi = s_eps^2; the shaded band
# marks the lambda^2 / s_eps^2 span of the grid, i.e., theta at V_{L,f} = 1.
loading_values <- c(0.4, 0.6, 0.8)
error_sd_values <- c(0.125, 0.25, 0.5)
ratio_grid <- as.vector(outer(loading_values^2, error_sd_values^2, "/"))
band_min <- min(ratio_grid)
band_max <- max(ratio_grid)

readout_colors <- c(
  "Marginal readout" = "#0072B2",
  "Partial readout" = "#E69F00"
)

ceiling_value <- 1 / (k_indicators - 1)

figure <- ggplot(curves, aes(x = theta, y = edge, color = Readout, linetype = Readout)) +
  annotate(
    "rect",
    xmin = band_min, xmax = band_max, ymin = -Inf, ymax = Inf,
    fill = "grey80", alpha = 0.35
  ) +
  geom_hline(yintercept = ceiling_value, linetype = "dotted", color = "grey35", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  annotate(
    "text",
    x = 10^2.7, y = ceiling_value + 0.045,
    label = "Partial-readout ceiling 1/(k - 1) = 1/3",
    size = 3.0, hjust = 1, color = "grey25"
  ) +
  annotate(
    "text",
    x = sqrt(band_min * band_max), y = 0.97,
    label = "'Matched-grid '*lambda^2/s[epsilon]^2*' span'",
    parse = TRUE,
    size = 3.0, color = "grey25"
  ) +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000),
    labels = label_number(drop0trailing = TRUE)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = readout_colors) +
  scale_linetype_manual(values = c("Marginal readout" = "solid", "Partial readout" = "longdash")) +
  labs(
    x = expression("Filtered signal-to-noise ratio " * theta[L]),
    y = "Population within-block edge weight",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

pdf_path <- file.path(figure_dir, "perturbation_readout_curves.pdf")
png_path <- file.path(figure_dir, "perturbation_readout_curves.png")
ggsave(pdf_path, figure, width = 6.6, height = 3.6, device = cairo_pdf)
ggsave(png_path, figure, width = 6.6, height = 3.6, dpi = 450, bg = "white")

cat("Wrote", pdf_path, "and", png_path, "\n")
cat(sprintf(
  "Grid lambda^2/s_eps^2 span: %.3g to %.3g (V_{L,f} multiplies theta).\n",
  band_min, band_max
))
