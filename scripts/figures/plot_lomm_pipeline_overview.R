#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(grid))

repo_root <- normalizePath(".", mustWork = TRUE)
source_path <- file.path(
  repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"
)
if (!file.exists(source_path)) {
  stop("Run this script from the repository root.", call. = FALSE)
}

output_path <- file.path(
  repo_root, "paper", "figures", "lomm_pipeline_visual_abstract.png"
)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

colors <- c(
  blue = "#0072B2",
  green = "#009E73",
  orange = "#E69F00",
  light_blue = "#F2F8FC",
  light_green = "#F1FAF6",
  light_orange = "#FFF8E8",
  grey = "#5F5F5F"
)

npc <- function(value) unit(value, "npc")

draw_box <- function(x, y, width, height, label, fill, border, fontsize = 9) {
  grid.roundrect(
    x = npc(x), y = npc(y), width = npc(width), height = npc(height),
    r = unit(0.018, "snpc"),
    gp = gpar(fill = fill, col = border, lwd = 1.2)
  )
  grid.text(
    label, x = npc(x), y = npc(y),
    gp = gpar(col = "grey12", fontsize = fontsize, lineheight = 1.05)
  )
}

draw_arrow <- function(x0, y0, x1, y1, color = "grey40", dashed = FALSE) {
  grid.lines(
    x = npc(c(x0, x1)), y = npc(c(y0, y1)),
    arrow = arrow(type = "closed", length = unit(0.075, "inches")),
    gp = gpar(col = color, lwd = 1.25, lty = if (dashed) 2 else 1)
  )
}

draw_panel <- function(x, y, width, height, label, accent) {
  grid.roundrect(
    x = npc(x), y = npc(y), width = npc(width), height = npc(height),
    r = unit(0.018, "snpc"),
    gp = gpar(fill = "white", col = "grey72", lwd = 0.9)
  )
  grid.rect(
    x = npc(x), y = npc(y + height / 2 - 0.043),
    width = npc(width), height = npc(0.086),
    gp = gpar(fill = accent, col = NA)
  )
  grid.text(
    label, x = npc(x - width / 2 + 0.025),
    y = npc(y + height / 2 - 0.043), just = "left",
    gp = gpar(col = "white", fontsize = 10.5, fontface = "bold")
  )
}

draw_figure <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))

  draw_panel(0.255, 0.52, 0.46, 0.84, "Study 1: resemblance", colors[["blue"]])
  draw_panel(0.745, 0.52, 0.46, 0.84, "Study 2: recovery", colors[["green"]])

  draw_box(
    0.255, 0.785, 0.34, 0.12,
    "Real classifier scores\n100 MAFW clips",
    colors[["light_blue"]], colors[["blue"]], 9.2
  )
  draw_box(
    0.255, 0.585, 0.34, 0.12,
    "LOMM bounded scores\n8 candidate configurations",
    colors[["light_blue"]], colors[["blue"]], 9.2
  )
  draw_box(
    0.255, 0.385, 0.34, 0.12,
    "Compare 8 statistics on\nscores and log-ratio coordinates",
    colors[["light_orange"]], colors[["orange"]], 8.8
  )
  draw_box(
    0.255, 0.185, 0.34, 0.12,
    "Outcome\nsimulated-versus-real discrepancy",
    colors[["light_blue"]], colors[["blue"]], 8.8
  )
  draw_arrow(0.255, 0.72, 0.255, 0.655, colors[["blue"]], TRUE)
  draw_arrow(0.255, 0.52, 0.255, 0.455, colors[["orange"]])
  draw_arrow(0.255, 0.32, 0.255, 0.255, colors[["blue"]])

  draw_box(
    0.68, 0.785, 0.28, 0.12,
    "Prespecified factor structure\ndimensions and indicator assignments",
    colors[["light_green"]], colors[["green"]], 8.8
  )
  draw_box(
    0.68, 0.585, 0.28, 0.12,
    "LOMM continuous indicators\n6 activity-dynamics families",
    colors[["light_green"]], colors[["green"]], 9.0
  )
  draw_box(
    0.68, 0.385, 0.28, 0.12,
    "DynEGA, static EGA,\nGraphicalVAR, and GIMME",
    colors[["light_orange"]], colors[["orange"]], 9.0
  )
  draw_box(
    0.68, 0.185, 0.28, 0.12,
    "Outcome\nrecover dimensions and assignments",
    colors[["light_green"]], colors[["green"]], 8.8
  )
  draw_arrow(0.68, 0.72, 0.68, 0.655, colors[["green"]])
  draw_arrow(0.68, 0.52, 0.68, 0.455, colors[["orange"]])
  draw_arrow(0.68, 0.32, 0.68, 0.255, colors[["green"]])

  draw_box(
    0.89, 0.585, 0.13, 0.115,
    "Bounded-score\nboundary check",
    colors[["light_orange"]], colors[["orange"]], 7.2
  )
  draw_arrow(0.80, 0.585, 0.825, 0.585, colors[["orange"]], TRUE)
}

grDevices::png(
  output_path, width = 2100, height = 1080, res = 300,
  type = "cairo-png", bg = "white"
)
draw_figure()
grDevices::dev.off()

message("Wrote: ", output_path)
