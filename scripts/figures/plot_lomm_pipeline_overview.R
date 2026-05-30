#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(grid)
})

repo_root <- normalizePath(".", mustWork = TRUE)

out_dir <- file.path(repo_root, "paper", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pdf_path <- file.path(out_dir, "lomm_pipeline_overview.pdf")
png_path <- file.path(out_dir, "lomm_pipeline_overview.png")
visual_abstract_png_path <- file.path(out_dir, "lomm_pipeline_visual_abstract.png")

okabe <- c(
  blue = "#0072B2",
  sky = "#56B4E9",
  green = "#009E73",
  orange = "#E69F00",
  vermillion = "#D55E00",
  purple = "#CC79A7",
  grey = "#5F5F5F"
)

npc <- function(x) unit(x, "npc")

draw_arrow <- function(x0, y0, x1, y1, col = "grey35", lty = 1, lwd = 1.1) {
  grid.lines(
    x = npc(c(x0, x1)),
    y = npc(c(y0, y1)),
    arrow = arrow(type = "closed", length = unit(0.07, "inches")),
    gp = gpar(col = col, lwd = lwd, lty = lty)
  )
}

draw_poly_arrow <- function(xs, ys, col = "grey35", lty = 2, lwd = 0.95) {
  grid.lines(
    x = npc(xs),
    y = npc(ys),
    arrow = arrow(type = "closed", length = unit(0.07, "inches")),
    gp = gpar(col = col, lwd = lwd, lty = lty)
  )
}

draw_row_header <- function(x0, x1, y, label, col) {
  grid.lines(
    x = npc(c(x0, x1)),
    y = npc(c(y, y)),
    gp = gpar(col = col, lwd = 2.2)
  )
  grid.text(
    label,
    x = npc(x0),
    y = npc(y + 0.028),
    just = "left",
    gp = gpar(col = "grey12", fontsize = 8.2, fontface = "bold")
  )
}

draw_node <- function(x, y, w, h, title, accent, fill, label = NULL) {
  grid.roundrect(
    x = npc(x),
    y = npc(y),
    width = npc(w),
    height = npc(h),
    r = unit(0.014, "snpc"),
    gp = gpar(fill = fill, col = "grey35", lwd = 0.65)
  )
  grid.rect(
    x = npc(x),
    y = npc(y + h / 2 - 0.028),
    width = npc(w),
    height = npc(0.056),
    gp = gpar(fill = accent, col = NA)
  )
  if (!is.null(label)) {
    grid.circle(
      x = npc(x - w / 2 + 0.023),
      y = npc(y + h / 2 - 0.028),
      r = npc(0.014),
      gp = gpar(fill = "white", col = NA)
    )
    grid.text(
      label,
      x = npc(x - w / 2 + 0.023),
      y = npc(y + h / 2 - 0.028),
      just = "center",
      gp = gpar(col = accent, fontsize = 6.6, fontface = "bold")
    )
  }
  grid.text(
    title,
    x = npc(x),
    y = npc(y + h / 2 - 0.028),
    just = "center",
    gp = gpar(col = "white", fontsize = 7.3, fontface = "bold")
  )
}

draw_video_icon <- function(x, y) {
  for (i in 0:2) {
    grid.roundrect(
      x = npc(x - 0.035 + i * 0.033),
      y = npc(y + 0.004 + i * 0.004),
      width = npc(0.058),
      height = npc(0.069),
      r = unit(0.004, "snpc"),
      gp = gpar(fill = "white", col = "grey35", lwd = 0.55)
    )
    grid.rect(
      x = npc(x - 0.035 + i * 0.033),
      y = npc(y + 0.028 + i * 0.004),
      width = npc(0.052),
      height = npc(0.011),
      gp = gpar(fill = okabe[["sky"]], col = NA)
    )
  }
  grid.text(
    "frames",
    x = npc(x),
    y = npc(y - 0.073),
    gp = gpar(col = "grey18", fontsize = 6.7)
  )
}

draw_line_icon <- function(x, y, w = 0.115, h = 0.105, noisy = FALSE) {
  grid.lines(
    x = npc(c(x - w / 2, x - w / 2, x + w / 2)),
    y = npc(c(y + h / 2, y - h / 2, y - h / 2)),
    gp = gpar(col = "grey62", lwd = 0.5)
  )
  t <- seq(0, 1, length.out = 80)
  curves <- list(
    0.40 + 0.20 * sin(2 * pi * (t + 0.05)),
    0.47 + 0.16 * sin(2 * pi * (t + 0.35)),
    0.28 + 0.13 * cos(3 * pi * t)
  )
  cols <- c(okabe[["blue"]], okabe[["vermillion"]], okabe[["green"]])
  for (idx in seq_along(curves)) {
    yy <- curves[[idx]]
    if (noisy) {
      yy <- yy + 0.025 * sin(28 * pi * t + idx)
    }
    grid.lines(
      x = npc(x - w / 2 + t * w),
      y = npc(y - h / 2 + yy * h),
      gp = gpar(col = cols[[idx]], lwd = 1.0)
    )
  }
}

draw_realism_icon <- function(x, y) {
  metrics <- c(
    "Lag-1 ACF",
    "Channel IQR",
    "Derivative IQR",
    "Run-length entropy",
    "Spectral entropy",
    "Effective rank"
  )
  y0 <- y + 0.057
  for (idx in seq_along(metrics)) {
    yy <- y0 - (idx - 1L) * 0.0235
    grid.circle(
      x = npc(x - 0.066),
      y = npc(yy),
      r = npc(0.0045),
      gp = gpar(fill = okabe[["blue"]], col = NA)
    )
    grid.text(
      metrics[[idx]],
      x = npc(x - 0.055),
      y = npc(yy),
      just = "left",
      gp = gpar(col = "grey16", fontsize = 5.8)
    )
  }
}

draw_scenario_icon <- function(x, y) {
  chip_y <- y + c(0.052, 0.006, -0.047)
  chip_labels <- c("dynamics", "activity", "measurement")
  for (idx in seq_along(chip_y)) {
    grid.roundrect(
      x = npc(x - 0.047),
      y = npc(chip_y[[idx]]),
      width = npc(0.080),
      height = npc(0.030),
      r = unit(0.006, "snpc"),
      gp = gpar(fill = "#FFF7E6", col = okabe[["orange"]], lwd = 0.5)
    )
    grid.text(
      chip_labels[[idx]],
      x = npc(x - 0.047),
      y = npc(chip_y[[idx]]),
      gp = gpar(col = "grey18", fontsize = 5.9)
    )
  }
  grid.text(
    expression(eta*", "*zeta*", "*sigma[q]),
    x = npc(x + 0.046),
    y = npc(chip_y[[1]]),
    gp = gpar(col = "grey12", fontsize = 8.5)
  )
  grid.text(
    "dominance\nswitching\nlow expression",
    x = npc(x + 0.046),
    y = npc(chip_y[[2]]),
    gp = gpar(col = "grey12", fontsize = 5.6, lineheight = 0.92)
  )
  grid.text(
    expression(Lambda*", "*Sigma[epsilon]),
    x = npc(x + 0.046),
    y = npc(chip_y[[3]]),
    gp = gpar(col = "grey12", fontsize = 8.5)
  )
}

draw_latent_icon <- function(x, y) {
  draw_line_icon(x, y + 0.015, w = 0.115, h = 0.085, noisy = FALSE)
  grid.text(
    expression(x(t)*", "*v(t)),
    x = npc(x),
    y = npc(y - 0.061),
    gp = gpar(col = "grey12", fontsize = 6.7)
  )
}

draw_measurement_icon <- function(x, y) {
  x0 <- x - 0.046
  y0 <- y + 0.033
  cell <- 0.015
  for (r in 0:5) {
    for (c in 0:2) {
      is_block <- (c == floor(r / 2))
      grid.rect(
        x = npc(x0 + c * cell),
        y = npc(y0 - r * cell),
        width = npc(cell * 0.9),
        height = npc(cell * 0.9),
        gp = gpar(
          fill = if (is_block) okabe[["green"]] else "grey92",
          col = "white",
          lwd = 0.2
        )
      )
    }
  }
  grid.text(
    expression(y == Lambda*tilde(x) + epsilon),
    x = npc(x + 0.024),
    y = npc(y - 0.04),
    gp = gpar(col = "grey12", fontsize = 6.5)
  )
}

draw_recovery_icon <- function(x, y) {
  node_x <- x + c(-0.045, -0.015, 0.018, 0.045, 0.000)
  node_y <- y + c(0.030, 0.055, 0.028, 0.055, -0.010)
  edge_pairs <- matrix(c(1, 2, 2, 3, 3, 4, 1, 5, 3, 5), ncol = 2, byrow = TRUE)
  for (idx in seq_len(nrow(edge_pairs))) {
    a <- edge_pairs[idx, 1]
    b <- edge_pairs[idx, 2]
    grid.lines(
      x = npc(c(node_x[[a]], node_x[[b]])),
      y = npc(c(node_y[[a]], node_y[[b]])),
      gp = gpar(col = "grey55", lwd = 0.55)
    )
  }
  node_cols <- c(okabe[["blue"]], okabe[["blue"]], okabe[["green"]], okabe[["green"]], okabe[["vermillion"]])
  for (idx in seq_along(node_x)) {
    grid.circle(
      x = npc(node_x[[idx]]),
      y = npc(node_y[[idx]]),
      r = npc(0.011),
      gp = gpar(fill = node_cols[[idx]], col = "white", lwd = 0.45)
    )
  }
  grid.text(
    expression(hat(m)*", blocks"),
    x = npc(x),
    y = npc(y - 0.052),
    gp = gpar(col = "grey12", fontsize = 6.5)
  )
}

draw_figure <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))

  grid.text(
    "LOMM simulation pipeline",
    x = npc(0.50),
    y = npc(0.955),
    just = "center",
    gp = gpar(col = "grey10", fontsize = 11.2, fontface = "bold")
  )

  draw_row_header(0.055, 0.945, 0.858, "Empirical calibration", okabe[["blue"]])
  draw_row_header(0.055, 0.945, 0.462, "Known-truth simulation study", okabe[["green"]])

  xs <- c(0.13, 0.37, 0.61, 0.85)
  top_y <- 0.665
  bottom_y <- 0.270
  box_w <- 0.18
  box_h <- 0.235

  draw_node(xs[[1]], top_y, box_w, box_h, "Video data", okabe[["blue"]], "#F7FBFF", "A")
  draw_video_icon(xs[[1]], top_y - 0.012)

  draw_node(xs[[2]], top_y, box_w, box_h, "FER-like scores", okabe[["blue"]], "#F7FBFF", "B")
  draw_line_icon(xs[[2]], top_y + 0.005, w = 0.120, h = 0.095, noisy = TRUE)
  grid.text("label channels", x = npc(xs[[2]]), y = npc(top_y - 0.073), gp = gpar(col = "grey18", fontsize = 6.7))

  draw_node(xs[[3]], top_y, box_w, box_h, "Realism targets", okabe[["blue"]], "#F7FBFF", "C")
  draw_realism_icon(xs[[3]], top_y)

  draw_node(xs[[4]], top_y, box_w, box_h, "Scenario selection", okabe[["orange"]], "#FFFDF5", "D")
  draw_scenario_icon(xs[[4]], top_y - 0.003)

  draw_node(xs[[4]], bottom_y, box_w, box_h, "Latent source truth", okabe[["green"]], "#F7FFF9", "E")
  draw_latent_icon(xs[[4]], bottom_y)

  draw_node(xs[[3]], bottom_y, box_w, box_h, "Observation layer", okabe[["green"]], "#F7FFF9", "F")
  draw_measurement_icon(xs[[3]], bottom_y)

  draw_node(xs[[2]], bottom_y, box_w, box_h, "Simulated panels", okabe[["green"]], "#F7FFF9", "G")
  draw_line_icon(xs[[2]], bottom_y + 0.005, w = 0.120, h = 0.095, noisy = TRUE)
  grid.text("known blocks", x = npc(xs[[2]]), y = npc(bottom_y - 0.073), gp = gpar(col = "grey18", fontsize = 6.7))

  draw_node(xs[[1]], bottom_y, box_w, box_h, "Recovery test", okabe[["green"]], "#F7FFF9", "H")
  draw_recovery_icon(xs[[1]], bottom_y)

  for (idx in seq_len(length(xs) - 1L)) {
    draw_arrow(xs[[idx]] + box_w / 2 + 0.012, top_y, xs[[idx + 1L]] - box_w / 2 - 0.012, top_y)
  }
  for (idx in length(xs):2L) {
    draw_arrow(xs[[idx]] - box_w / 2 - 0.012, bottom_y, xs[[idx - 1L]] + box_w / 2 + 0.012, bottom_y)
  }

  draw_poly_arrow(
    c(xs[[4]], xs[[4]]),
    c(top_y - box_h / 2 - 0.014, bottom_y + box_h / 2 + 0.014),
    col = okabe[["orange"]],
    lty = 1,
    lwd = 1.15
  )
  grid.text(
    "calibrates",
    x = npc(xs[[4]] + 0.037),
    y = npc(0.498),
    gp = gpar(col = okabe[["orange"]], fontsize = 6.8, fontface = "italic")
  )

  draw_poly_arrow(
    c(xs[[4]], xs[[4]], xs[[1]], xs[[1]]),
    c(bottom_y - box_h / 2 - 0.016, 0.070, 0.070, bottom_y - box_h / 2 - 0.016),
    col = "grey35",
    lty = 2,
    lwd = 0.9
  )
  grid.text(
    "compare estimates with known m and blocks",
    x = npc(0.50),
    y = npc(0.037),
    gp = gpar(col = "grey25", fontsize = 7.2, fontface = "italic")
  )
}

if (capabilities("cairo")) {
  cairo_pdf(pdf_path, width = 10.8, height = 5.25, family = "sans")
} else {
  pdf(pdf_path, width = 10.8, height = 5.25, useDingbats = FALSE)
}
draw_figure()
dev.off()

png(png_path, width = 3240, height = 1575, res = 300, type = "cairo-png")
draw_figure()
dev.off()

png(visual_abstract_png_path, width = 3240, height = 1575, res = 300, type = "cairo-png")
draw_figure()
dev.off()

message("Wrote: ", pdf_path)
message("Wrote: ", png_path)
message("Wrote: ", visual_abstract_png_path)
