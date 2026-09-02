#!/usr/bin/env Rscript

# Figure 1: schematic of how real data, LOMM, and the recovery methods feed
# Study 1 (simulated vs real) and Study 2 (recovered vs known). Glyphs carry
# the content; text is limited to panel titles, one line of design numbers,
# and arrow verbs.

suppressPackageStartupMessages(library(grid))

repo_root <- normalizePath(".", mustWork = TRUE)
source_path <- file.path(
  repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"
)
if (!file.exists(source_path)) {
  stop("Run this script from the repository root.", call. = FALSE)
}

output_stem <- file.path(repo_root, "paper", "figures", "lomm_pipeline_visual_abstract")
dir.create(dirname(output_stem), recursive = TRUE, showWarnings = FALSE)

col <- c(
  real = "#0072B2",
  lomm = "#009E73",
  dim2 = "#E69F00",
  method = "#5F5F5F",
  ink = "grey15",
  muted = "grey45",
  frame = "grey70",
  fill = "grey97"
)
channel_cols <- c(
  "#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00",
  "#F0E442", "#999999", "#7A5195", "#2F4B7C", "#A05195"
)

W <- 6.6
H <- 3.9
Y0 <- 0.22  # bottom crop; layout coordinates below Y0 are not drawn

nat <- function(v) unit(v, "native")

with_vp <- function(x, y, w, h, xs = c(0, 1), ys = c(0, 1), expr) {
  pushViewport(viewport(
    x = nat(x), y = nat(y), width = nat(w), height = nat(h),
    xscale = xs, yscale = ys, clip = "off"
  ))
  on.exit(popViewport())
  force(expr)
}

txt <- function(label, x, y, size = 7, face = "plain", colour = col[["ink"]],
                just = "centre", rot = 0) {
  grid.text(label, x = nat(x), y = nat(y), just = just, rot = rot,
            gp = gpar(fontsize = size, fontface = face, col = colour,
                      fontfamily = "sans", lineheight = 0.95))
}

arrow_seg <- function(x, y, colour = col[["method"]], lty = 1, lwd = 0.9) {
  grid.lines(x = nat(x), y = nat(y),
             arrow = arrow(type = "closed", angle = 22,
                           length = unit(0.055, "inches")),
             gp = gpar(col = colour, fill = colour, lwd = lwd, lty = lty))
}

panel_frame <- function(x0, y0, x1, y1, letter, title, subtitle, accent) {
  grid.roundrect(x = nat((x0 + x1) / 2), y = nat((y0 + y1) / 2),
                 width = nat(x1 - x0), height = nat(y1 - y0),
                 r = unit(0.03, "inches"),
                 gp = gpar(fill = "white", col = col[["frame"]], lwd = 0.7))
  grid.rect(x = nat(x0 + 0.02), y = nat((y0 + y1) / 2), width = nat(0.04),
            height = nat(y1 - y0 - 0.06), gp = gpar(fill = accent, col = NA))
  txt(letter, x0 + 0.10, y1 - 0.10, size = 9, face = "bold", just = c("left", "top"))
  txt(title, x0 + 0.27, y1 - 0.10, size = 8.2, face = "bold", just = c("left", "top"))
  if (!is.null(subtitle)) {
    txt(subtitle, x0 + 0.10, y0 + 0.07, size = 6, colour = col[["muted"]],
        just = c("left", "bottom"))
  }
}

# ---- glyphs --------------------------------------------------------------

glyph_frames <- function(x, y, w, h) {
  with_vp(x, y, w, h, xs = c(0, 3), ys = c(0, 1), {
    for (k in 0:2) {
      grid.rect(x = nat(k + 0.5), y = nat(0.5), width = nat(0.92),
                height = nat(0.92), gp = gpar(fill = "grey30", col = NA))
      grid.circle(x = nat(k + 0.5), y = nat(0.56), r = unit(0.17, "npc"),
                  gp = gpar(fill = "grey78", col = NA))
      grid.rect(x = nat(k + 0.5), y = nat(0.20), width = nat(0.44),
                height = nat(0.22), gp = gpar(fill = "grey78", col = NA))
    }
  })
}

smooth_series <- function(n, seed, scale = 1) {
  set.seed(seed)
  z <- cumsum(rnorm(n))
  z <- stats::filter(z, rep(1 / 5, 5), sides = 2)
  z <- as.numeric(z)
  ok <- which(!is.na(z))
  z[seq_len(ok[1] - 1)] <- z[ok[1]]
  z[seq(ok[length(ok)] + 1, length.out = length(z) - ok[length(ok)])] <- z[ok[length(ok)]]
  z <- (z - min(z)) / (max(z) - min(z) + 1e-9)
  z * scale
}

glyph_score_series <- function(x, y, w, h, seed, palette) {
  with_vp(x, y, w, h, xs = c(1, 40), ys = c(0, 1), {
    grid.rect(gp = gpar(fill = col[["fill"]], col = NA))
    for (k in seq_along(palette)) {
      s <- smooth_series(40, seed + k, scale = 0.55) + 0.05 + 0.07 * (k %% 2)
      grid.lines(x = nat(1:40), y = nat(s),
                 gp = gpar(col = palette[k], lwd = 1.1))
    }
  })
}

glyph_oscillator <- function(x, y, w, h) {
  with_vp(x, y, w, h, xs = c(0, 30), ys = c(-1.15, 1.15), {
    t <- seq(0, 30, length.out = 200)
    grid.lines(x = nat(c(0, 30)), y = nat(c(0, 0)), gp = gpar(col = "grey80", lwd = 0.5))
    grid.lines(x = nat(t), y = nat(exp(-0.07 * t) * cos(0.9 * t)),
               gp = gpar(col = col[["lomm"]], lwd = 1.2))
    grid.lines(x = nat(t), y = nat(0.8 * exp(-0.05 * t) * cos(0.55 * t + 1.6)),
               gp = gpar(col = col[["dim2"]], lwd = 1.2, lty = 2))
  })
}

glyph_activity <- function(x, y, w, h) {
  with_vp(x, y, w, h, xs = c(0, 30), ys = c(-0.1, 1.2), {
    grid.lines(x = nat(c(0, 15, 15, 30)), y = nat(c(1, 1, 0.08, 0.08)),
               gp = gpar(col = col[["lomm"]], lwd = 1.2))
    grid.lines(x = nat(c(0, 15, 15, 30)), y = nat(c(0.08, 0.08, 1, 1)),
               gp = gpar(col = col[["dim2"]], lwd = 1.2, lty = 2))
  })
}

glyph_loading_grid <- function(x, y, w, h, p = 8, m = 2) {
  with_vp(x, y, w, h, xs = c(0, m), ys = c(0, p), {
    k <- p / m
    for (j in 1:p) {
      for (f in 1:m) {
        on <- ceiling(j / k) == f
        fill <- if (on) c(col[["lomm"]], col[["dim2"]])[f] else "grey92"
        grid.rect(x = nat(f - 0.5), y = nat(p - j + 0.5), width = nat(0.9),
                  height = nat(0.9), gp = gpar(fill = fill, col = NA))
      }
    }
  })
}

glyph_indicators <- function(x, y, w, h, seed = 11) {
  with_vp(x, y, w, h, xs = c(1, 40), ys = c(-1.6, 1.6), {
    grid.rect(gp = gpar(fill = col[["fill"]], col = NA))
    set.seed(seed)
    t <- 1:40
    base1 <- exp(-0.07 * t / 2) * cos(0.45 * t) * (t <= 20) * 0.9
    base2 <- 0.9 * exp(-0.05 * (t - 20) / 2) * cos(0.35 * (t - 20) + 1.6) * (t > 20)
    for (i in 1:4) {
      grid.lines(x = nat(t), y = nat(base1 + rnorm(40, 0, 0.18) + 0.55),
                 gp = gpar(col = adjustcolor(col[["lomm"]], 0.7), lwd = 0.6))
      grid.lines(x = nat(t), y = nat(base2 + rnorm(40, 0, 0.18) - 0.55),
                 gp = gpar(col = adjustcolor(col[["dim2"]], 0.7), lwd = 0.6))
    }
  })
}

glyph_network <- function(x, y, w, h, seed = 3, coloured = TRUE) {
  with_vp(x, y, w, h, xs = c(-1.2, 3.2), ys = c(-1.2, 1.2), {
    set.seed(seed)
    ang <- seq(0, 2 * pi, length.out = 5)[-5]
    g1 <- cbind(0 + 0.7 * cos(ang), 0.7 * sin(ang))
    g2 <- cbind(2 + 0.7 * cos(ang + 0.4), 0.7 * sin(ang + 0.4))
    edges <- function(g, colour) {
      for (a in 1:3) for (b in (a + 1):4) {
        grid.lines(x = nat(g[c(a, b), 1]), y = nat(g[c(a, b), 2]),
                   gp = gpar(col = colour, lwd = 0.8))
      }
    }
    edges(g1, if (coloured) adjustcolor(col[["lomm"]], 0.6) else "grey60")
    edges(g2, if (coloured) adjustcolor(col[["dim2"]], 0.6) else "grey60")
    grid.lines(x = nat(c(g1[1, 1], g2[3, 1])), y = nat(c(g1[1, 2], g2[3, 2])),
               gp = gpar(col = "grey75", lwd = 0.5, lty = 3))
    grid.points(x = nat(g1[, 1]), y = nat(g1[, 2]), pch = 21, size = unit(0.09, "inches"),
                gp = gpar(fill = if (coloured) col[["lomm"]] else col[["method"]], col = "white", lwd = 0.5))
    grid.points(x = nat(g2[, 1]), y = nat(g2[, 2]), pch = 21, size = unit(0.09, "inches"),
                gp = gpar(fill = if (coloured) col[["dim2"]] else col[["method"]], col = "white", lwd = 0.5))
  })
}

# Estimated network before community detection: eight indicators on a ring,
# strong ties within each block and weak ties between blocks, all uncoloured.
glyph_network_full <- function(x, y, w, h) {
  with_vp(x, y, w, h, xs = c(-1.25, 1.25), ys = c(-1.25, 1.25), {
    ang <- seq(0, 2 * pi, length.out = 9)[-9] + pi / 8
    pts <- cbind(cos(ang), sin(ang))
    block <- rep(1:2, each = 4)
    for (a in 1:7) for (b in (a + 1):8) {
      same <- block[a] == block[b]
      grid.lines(x = nat(pts[c(a, b), 1]), y = nat(pts[c(a, b), 2]),
                 gp = gpar(col = if (same) "grey45" else "grey78",
                           lwd = if (same) 1.0 else 0.4))
    }
    grid.points(x = nat(pts[, 1]), y = nat(pts[, 2]), pch = 21,
                size = unit(0.085, "inches"),
                gp = gpar(fill = col[["method"]], col = "white", lwd = 0.5))
  })
}

compare_box <- function(x0, y0, x1, y1, title, question, accent) {
  grid.roundrect(x = nat((x0 + x1) / 2), y = nat((y0 + y1) / 2),
                 width = nat(x1 - x0), height = nat(y1 - y0),
                 r = unit(0.03, "inches"),
                 gp = gpar(fill = "white", col = accent, lwd = 1.1))
  txt(title, x0 + 0.10, y1 - 0.09, size = 8.2, face = "bold", colour = accent,
      just = c("left", "top"))
  txt(question, x0 + 0.10 + 0.55, y1 - 0.09, size = 7.6, face = "italic",
      just = c("left", "top"))
}

# ---- layout ----------------------------------------------------------------

draw_figure <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(Y0, H)))
  grid.rect(gp = gpar(fill = "white", col = NA))

  # Top row panels
  ay <- c(2.15, 3.80)
  ax <- c(0.12, 1.92)
  bx <- c(2.32, 4.28)
  cx <- c(4.68, 6.48)

  panel_frame(ax[1], ay[1], ax[2], ay[2], "A", "Real data", NULL, col[["real"]])
  panel_frame(bx[1], ay[1], bx[2], ay[2], "B", "LOMM", NULL, col[["lomm"]])
  panel_frame(cx[1], ay[1], cx[2], ay[2], "C", "Recovery methods", NULL, col[["method"]])

  # A: frames -> score series
  glyph_frames(1.02, 3.30, 1.40, 0.36)
  arrow_seg(c(1.02, 1.02), c(3.11, 3.00), colour = col[["real"]])
  glyph_score_series(1.02, 2.70, 1.40, 0.54, seed = 21,
                     palette = channel_cols[c(1, 2, 4, 3)])

  # B: three layers
  lab_x <- bx[1] + 0.12
  gx <- bx[1] + 0.62
  gw <- bx[2] - gx - 0.12
  txt("latent", lab_x, 3.42, size = 6.2, colour = col[["muted"]], just = c("left", "centre"))
  glyph_oscillator(gx + gw / 2, 3.42, gw, 0.30)
  txt("activity", lab_x, 3.05, size = 6.2, colour = col[["muted"]], just = c("left", "centre"))
  glyph_activity(gx + gw / 2, 3.05, gw, 0.26)
  txt("observed", lab_x, 2.62, size = 6.2, colour = col[["muted"]], just = c("left", "centre"))
  glyph_loading_grid(gx + 0.16, 2.62, 0.24, 0.46)
  glyph_indicators(gx + 0.42 + (gw - 0.42) / 2, 2.62, gw - 0.42, 0.46)
  arrow_seg(c(gx + gw * 0.5, gx + gw * 0.5), c(3.26, 3.19), lwd = 0.6)
  arrow_seg(c(gx + gw * 0.5, gx + gw * 0.5), c(2.91, 2.86), lwd = 0.6)

  # C: estimated network -> communities coloured by estimated dimension
  glyph_network_full(5.10, 3.08, 0.66, 0.66)
  arrow_seg(c(5.47, 5.62), c(3.08, 3.08), colour = col[["method"]])
  glyph_network(5.98, 3.08, 0.86, 0.50, coloured = TRUE)
  txt("network", 5.10, 2.70, size = 6, colour = col[["muted"]])
  txt("communities", 5.98, 2.70, size = 6, colour = col[["muted"]])
  methods <- c("DynEGA", "static EGA", "GraphicalVAR", "GIMME")
  mx <- c(cx[1] + 0.22, cx[1] + 0.22, cx[1] + 1.00, cx[1] + 1.00)
  my <- c(2.52, 2.32, 2.52, 2.32)
  for (i in seq_along(methods)) {
    grid.points(x = nat(mx[i] - 0.09), y = nat(my[i]), pch = 15,
                size = unit(0.06, "inches"), gp = gpar(col = col[["method"]]))
    txt(methods[i], mx[i], my[i], size = 6.6, just = c("left", "centre"))
  }

  # A -> B (calibration, dashed)
  arrow_seg(c(ax[2] + 0.02, bx[1] - 0.02), c(3.15, 3.15), colour = col[["real"]], lty = 2)
  txt("calibrate", (ax[2] + bx[1]) / 2, 3.24, size = 6, colour = col[["real"]])

  # B -> C (continuous indicators)
  arrow_seg(c(bx[2] + 0.02, cx[1] - 0.02), c(3.15, 3.15), colour = col[["lomm"]])
  txt("simulate", (bx[2] + cx[1]) / 2, 3.24, size = 6, colour = col[["lomm"]])
  txt("fit", (bx[2] + cx[1]) / 2, 3.05, size = 6, colour = col[["method"]])

  # Bottom row comparison boxes
  sy <- c(0.36, 1.55)
  s1x <- c(0.12, 3.15)
  s2x <- c(3.45, 6.48)

  compare_box(s1x[1], sy[1], s1x[2], sy[2], "Study 1",
              "Do simulated score series resemble real ones?", col[["real"]])
  glyph_score_series(0.95, 0.90, 1.05, 0.50, seed = 21,
                     palette = channel_cols[c(1, 2, 4, 3)])
  txt("real", 0.95, 0.57, size = 6, colour = col[["real"]])
  txt("\u2248 ?", 1.635, 0.90, size = 11, colour = col[["ink"]])
  glyph_score_series(2.32, 0.90, 1.05, 0.50, seed = 33,
                     palette = channel_cols[c(1, 2, 4, 3)])
  txt("simulated", 2.32, 0.57, size = 6, colour = col[["lomm"]])

  compare_box(s2x[1], sy[1], s2x[2], sy[2], "Study 2",
              "Is the known structure recovered?", col[["lomm"]])
  glyph_loading_grid(4.30, 0.92, 0.26, 0.50)
  txt("known", 4.30, 0.57, size = 6, colour = col[["lomm"]])
  txt("= ?", 4.965, 0.90, size = 11, colour = col[["ink"]])
  glyph_network(5.65, 0.92, 1.05, 0.56, coloured = TRUE)
  txt("recovered", 5.65, 0.57, size = 6, colour = col[["method"]])

  # Vertical flows into the studies
  arrow_seg(c(1.02, 1.02), c(ay[1] - 0.02, sy[2] + 0.02), colour = col[["real"]])
  arrow_seg(c(bx[1] + 0.45, 2.32), c(ay[1] - 0.02, sy[2] + 0.02), colour = col[["lomm"]])
  txt("softmax", 2.62, 1.86, size = 6, colour = col[["lomm"]], just = c("left", "centre"))
  arrow_seg(c(bx[2] - 0.30, 4.30), c(ay[1] - 0.02, sy[2] + 0.02), colour = col[["lomm"]])
  arrow_seg(c(5.65, 5.65), c(ay[1] - 0.02, sy[2] + 0.02), colour = col[["method"]])

  popViewport()
}

grDevices::cairo_pdf(paste0(output_stem, ".pdf"), width = W, height = H - Y0, bg = "white")
draw_figure()
invisible(grDevices::dev.off())

grDevices::png(paste0(output_stem, ".png"), width = W, height = H - Y0, units = "in",
               res = 300, type = "cairo-png", bg = "white")
draw_figure()
invisible(grDevices::dev.off())

message("Wrote: ", output_stem, ".pdf and .png")
