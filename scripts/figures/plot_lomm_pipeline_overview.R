#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(grid)
})

repo_root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"))) {
  stop("Run this script from the repository root.")
}

out_dir <- file.path(repo_root, "paper", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

png_path <- file.path(out_dir, "lomm_pipeline_visual_abstract.png")

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
    gp = gpar(col = "grey55", lwd = 0.55)
  )
  xx <- seq(-0.45, 0.45, length.out = 45)
  waves <- list(
    0.21 * sin(7 * xx) + 0.09,
    0.18 * cos(8 * xx + 0.5) - 0.02,
    0.16 * sin(9 * xx + 1.4) - 0.13
  )
  cols <- c(okabe[["blue"]], okabe[["vermillion"]], okabe[["green"]])
  if (noisy) {
    waves <- lapply(seq_along(waves), function(idx) {
      waves[[idx]] + 0.035 * sin(seq_along(xx) * (idx + 1.2))
    })
  }
  for (idx in seq_along(waves)) {
    grid.lines(
      x = npc(x + xx * w),
      y = npc(y + waves[[idx]] * h),
      gp = gpar(col = cols[[idx]], lwd = 1.0)
    )
  }
}

draw_realism_icon <- function(x, y) {
  labels <- c("IQR", "ACF", "Rank")
  vals <- c(0.35, 0.55, 0.74)
  for (idx in seq_along(vals)) {
    grid.rect(
      x = npc(x - 0.050 + idx * 0.035),
      y = npc(y - 0.035 + vals[[idx]] * 0.075),
      width = npc(0.024),
      height = npc(vals[[idx]] * 0.150),
      gp = gpar(fill = okabe[[c("blue", "purple", "green")[[idx]]]], col = NA)
    )
    grid.text(
      labels[[idx]],
      x = npc(x - 0.050 + idx * 0.035),
      y = npc(y - 0.073),
      gp = gpar(col = "grey18", fontsize = 5.9)
    )
  }
}

draw_scenario_icon <- function(x, y) {
  grid.circle(
    x = npc(x),
    y = npc(y + 0.025),
    r = npc(0.052),
    gp = gpar(fill = "white", col = okabe[["orange"]], lwd = 1.1)
  )
  for (angle in seq(0, 300, by = 60)) {
    x1 <- x + 0.052 * cos(angle * pi / 180)
    y1 <- y + 0.025 + 0.052 * sin(angle * pi / 180)
    grid.lines(
      x = npc(c(x, x1)),
      y = npc(c(y + 0.025, y1)),
      gp = gpar(col = okabe[["orange"]], lwd = 0.8)
    )
  }
  grid.text(
    "dynamics + activity",
    x = npc(x),
    y = npc(y - 0.073),
    gp = gpar(col = "grey18", fontsize = 6.4)
  )
}

draw_latent_icon <- function(x, y) {
  draw_line_icon(x, y + 0.008, w = 0.118, h = 0.100, noisy = FALSE)
  grid.text(
    "generating dimensions",
    x = npc(x),
    y = npc(y - 0.073),
    gp = gpar(col = "grey18", fontsize = 6.4)
  )
}

draw_measurement_icon <- function(x, y) {
  for (row in 1:3) {
    for (col in 1:4) {
      fill <- if (col <= 2 && row == 1) {
        okabe[["blue"]]
      } else if (col >= 3 && row == 2) {
        okabe[["green"]]
      } else if (col == row) {
        okabe[["vermillion"]]
      } else {
        "grey88"
      }
      grid.rect(
        x = npc(x - 0.048 + col * 0.020),
        y = npc(y + 0.050 - row * 0.023),
        width = npc(0.016),
        height = npc(0.016),
        gp = gpar(fill = fill, col = "white", lwd = 0.3)
      )
    }
  }
  grid.text(
    "B+S default",
    x = npc(x),
    y = npc(y - 0.036),
    gp = gpar(col = "grey18", fontsize = 6.2, fontface = "bold")
  )
  grid.text(
    "empirical | loading-aligned | none",
    x = npc(x),
    y = npc(y - 0.059),
    gp = gpar(col = "grey18", fontsize = 4.8)
  )
  grid.text(
    "surface declared per profile",
    x = npc(x),
    y = npc(y - 0.081),
    gp = gpar(col = "grey18", fontsize = 5.5)
  )
}

draw_recovery_icon <- function(x, y) {
  node_x <- x + c(-0.047, -0.018, 0.020, 0.050, 0.000)
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
  draw_row_header(0.055, 0.945, 0.462, "Recovery simulation study", okabe[["green"]])

  xs <- c(0.13, 0.37, 0.61, 0.85)
  top_y <- 0.665
  bottom_y <- 0.270
  box_w <- 0.18
  box_h <- 0.235

  draw_node(xs[[1]], top_y, box_w, box_h, "Video data", okabe[["blue"]], "#F7FBFF", "A")
  draw_video_icon(xs[[1]], top_y - 0.012)

  draw_node(xs[[2]], top_y, box_w, box_h, "Classifier-output scores", okabe[["blue"]], "#F7FBFF", "B")
  draw_line_icon(xs[[2]], top_y + 0.005, w = 0.120, h = 0.095, noisy = TRUE)
  grid.text("score channels", x = npc(xs[[2]]), y = npc(top_y - 0.073), gp = gpar(col = "grey18", fontsize = 6.7))

  draw_node(xs[[3]], top_y, box_w, box_h, "Realism targets", okabe[["blue"]], "#F7FBFF", "C")
  draw_realism_icon(xs[[3]], top_y)

  draw_node(xs[[4]], top_y, box_w, box_h, "Scenario selection", okabe[["orange"]], "#FFFDF5", "D")
  draw_scenario_icon(xs[[4]], top_y - 0.003)

  draw_node(xs[[1]], bottom_y, box_w, box_h, "Latent trajectories", okabe[["green"]], "#F7FFF9", "E")
  draw_latent_icon(xs[[1]], bottom_y)

  draw_node(xs[[2]], bottom_y, box_w, box_h, "Observation profiles", okabe[["green"]], "#F7FFF9", "F")
  draw_measurement_icon(xs[[2]], bottom_y)

  draw_node(xs[[3]], bottom_y, box_w, box_h, "Versioned outputs", okabe[["green"]], "#F7FFF9", "G")
  draw_line_icon(xs[[3]], bottom_y + 0.014, w = 0.120, h = 0.070, noisy = TRUE)
  grid.text(
    "bounded composition -> realism",
    x = npc(xs[[3]]),
    y = npc(bottom_y - 0.053),
    gp = gpar(col = "grey18", fontsize = 5.2)
  )
  grid.text(
    "linear_z -> recovery",
    x = npc(xs[[3]]),
    y = npc(bottom_y - 0.077),
    gp = gpar(col = "grey18", fontsize = 5.2)
  )

  draw_node(xs[[4]], bottom_y, box_w, box_h, "Recovery test", okabe[["green"]], "#F7FFF9", "H")
  draw_recovery_icon(xs[[4]], bottom_y)
  grid.text(
    "declared linear surface",
    x = npc(xs[[4]]),
    y = npc(bottom_y - 0.078),
    gp = gpar(col = "grey18", fontsize = 5.3)
  )

  for (idx in seq_len(length(xs) - 1L)) {
    draw_arrow(xs[[idx]] + box_w / 2 + 0.012, top_y, xs[[idx + 1L]] - box_w / 2 - 0.012, top_y)
  }
  for (idx in 1:2) {
    draw_arrow(
      xs[[idx]] + box_w / 2 + 0.012, bottom_y,
      xs[[idx + 1L]] - box_w / 2 - 0.012, bottom_y
    )
  }
  draw_arrow(
    xs[[3]] + box_w / 2 + 0.012, bottom_y - 0.050,
    xs[[4]] - box_w / 2 - 0.012, bottom_y - 0.050,
    col = okabe[["green"]]
  )

  draw_arrow(
    xs[[3]], bottom_y + box_h / 2 + 0.014,
    xs[[3]], top_y - box_h / 2 - 0.014,
    col = okabe[["blue"]], lty = 2
  )
  grid.text(
    "realism check",
    x = npc(xs[[3]] + 0.040),
    y = npc(0.498),
    gp = gpar(col = okabe[["blue"]], fontsize = 6.5, fontface = "italic")
  )

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
    c(xs[[1]], xs[[1]], xs[[4]], xs[[4]]),
    c(bottom_y - box_h / 2 - 0.016, 0.070, 0.070, bottom_y - box_h / 2 - 0.016),
    col = "grey35",
    lty = 2,
    lwd = 0.9
  )
  grid.text(
    "compare recovered m and blocks with the simulation design",
    x = npc(0.50),
    y = npc(0.037),
    gp = gpar(col = "grey25", fontsize = 7.2, fontface = "italic")
  )
}

png(png_path, width = 3240, height = 1575, res = 300, type = "cairo-png")
draw_figure()
dev.off()

message("Wrote: ", png_path)
