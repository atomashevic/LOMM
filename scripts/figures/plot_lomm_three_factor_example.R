#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

lomm_figure1_find_root <- function(start = getwd()) {
  candidate <- normalizePath(start, mustWork = TRUE)
  repeat {
    marker <- file.path(candidate, "scripts", "simulations", "revision_spec.R")
    if (file.exists(marker)) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      stop("Could not locate the repository root.", call. = FALSE)
    }
    candidate <- parent
  }
}

lomm_figure1_root <- lomm_figure1_find_root()
source(
  file.path(lomm_figure1_root, "scripts", "simulations", "revision_spec.R"),
  local = TRUE
)

lomm_figure1_with_seed <- function(seed, callback) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  callback()
}

lomm_figure1_parameters <- function() {
  parameters <- revision_factor_archetypes()[seq_len(3L), , drop = FALSE]
  parameters$factor_label <- c("Smooth/slow", "Moderate", "Reactive")
  parameters
}

lomm_figure1_simulate <- function(
  parameters,
  time_points = 100L,
  dt = 0.4,
  initial_state = c(0.25, 2),
  stochastic = FALSE,
  seed = 20260812L
) {
  time_points <- as.integer(time_points)
  if (time_points < 2L || !is.finite(dt) || dt <= 0) {
    stop("time_points must be at least 2 and dt must be positive.", call. = FALSE)
  }
  if (length(initial_state) != 2L || any(!is.finite(initial_state))) {
    stop("initial_state must contain finite position and velocity values.", call. = FALSE)
  }
  required <- c("eta", "zeta", "sigma_q", "factor_label")
  if (!all(required %in% names(parameters)) || nrow(parameters) < 1L) {
    stop("parameters must define eta, zeta, sigma_q, and factor_label.", call. = FALSE)
  }

  factor_count <- nrow(parameters)
  shocks <- matrix(0, nrow = time_points, ncol = factor_count)
  if (isTRUE(stochastic)) {
    shocks[-1L, ] <- lomm_figure1_with_seed(seed, function() {
      draws <- matrix(
        stats::rnorm((time_points - 1L) * factor_count),
        nrow = time_points - 1L,
        ncol = factor_count
      )
      sweep(draws, 2L, parameters$sigma_q, `*`)
    })
  }

  position <- matrix(NA_real_, nrow = time_points, ncol = factor_count)
  velocity <- matrix(NA_real_, nrow = time_points, ncol = factor_count)
  position[1L, ] <- initial_state[[1L]]
  velocity[1L, ] <- initial_state[[2L]]

  for (factor_index in seq_len(factor_count)) {
    exact <- revision_exact_transition(
      parameters$eta[[factor_index]],
      parameters$zeta[[factor_index]],
      dt
    )
    state <- matrix(initial_state, ncol = 1L)
    for (time_index in 2:time_points) {
      state <- exact$transition %*% state +
        exact$innovation_map * shocks[time_index, factor_index]
      position[time_index, factor_index] <- state[[1L]]
      velocity[time_index, factor_index] <- state[[2L]]
    }
  }

  data.frame(
    observation = rep(seq_len(time_points), times = factor_count),
    physical_time = rep((seq_len(time_points) - 1L) * dt, times = factor_count),
    factor = rep(parameters$factor_label, each = time_points),
    position = as.vector(position),
    velocity = as.vector(velocity),
    shock = as.vector(shocks),
    stringsAsFactors = FALSE
  )
}

lomm_figure1_data <- function(seed = 20260812L, time_points = 100L, dt = 0.4) {
  parameters <- lomm_figure1_parameters()
  deterministic <- lomm_figure1_simulate(
    parameters,
    time_points = time_points,
    dt = dt,
    stochastic = FALSE
  )
  stochastic <- lomm_figure1_simulate(
    parameters,
    time_points = time_points,
    dt = dt,
    stochastic = TRUE,
    seed = seed
  )
  deterministic$regime <- "Exact transition, no shocks"
  stochastic$regime <- "Exact transition with per-step shocks"
  output <- rbind(deterministic, stochastic)
  output$factor <- factor(output$factor, levels = parameters$factor_label)
  output$regime <- factor(
    output$regime,
    levels = c("Exact transition, no shocks", "Exact transition with per-step shocks")
  )
  output
}

lomm_figure1_plot <- function(data) {
  factor_colors <- c(
    "Smooth/slow" = "#0072B2",
    "Moderate" = "#D55E00",
    "Reactive" = "#009E73"
  )
  ggplot(data, aes(x = physical_time, y = position, color = factor, linetype = factor)) +
    geom_hline(yintercept = 0, color = "grey75", linewidth = 0.35) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~regime, nrow = 1L) +
    scale_color_manual(values = factor_colors, drop = FALSE) +
    scale_linetype_manual(values = c("solid", "longdash", "dotdash"), drop = FALSE) +
    labs(
      x = "Physical time",
      y = "Latent position",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.spacing.x = grid::unit(1, "lines"),
      strip.text = element_text(face = "bold")
    )
}

lomm_figure1_write <- function(
  output_path = file.path(
    lomm_figure1_root,
    "paper",
    "figures",
    "lomm_three_factor_example.pdf"
  ),
  seed = 20260812L
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  figure <- lomm_figure1_plot(lomm_figure1_data(seed = seed))
  ggsave(
    output_path,
    figure,
    width = 7.15,
    height = 3.4,
    units = "in",
    device = cairo_pdf,
    bg = "white"
  )
  invisible(normalizePath(output_path, mustWork = TRUE))
}

# Worked LOMM example (main-text Figure 2): three uncoupled dimensions share an
# initial state, one dimension is active at a time with the family C gains
# (1.00 active / 0.08 background), and four indicators load on each dimension.
lomm_process_settings <- function() {
  list(
    active_gain = 1.00,
    background_gain = 0.08,
    # Dominant dimension per dwell segment and the segment boundaries as
    # fractions of the record (dwell fractions lie in the family C range).
    dominant = c(1L, 2L, 3L, 1L),
    boundaries = c(0, 0.20, 0.50, 0.80, 1),
    loadings = c(0.65, 0.75, 0.85, 0.70),
    residual_sd = 0.20
  )
}

lomm_process_dimension_labels <- function() {
  parameters <- lomm_figure1_parameters()
  sprintf(
    "Dimension %d (\u03b7 = %.2f, \u03b6 = %.2f)",
    seq_len(nrow(parameters)),
    parameters$eta,
    parameters$zeta
  )
}

lomm_process_activity <- function(time_points, settings = lomm_process_settings()) {
  progress <- (seq_len(time_points) - 1L) / (time_points - 1L)
  segment <- findInterval(progress, settings$boundaries, rightmost.closed = TRUE)
  dominant <- settings$dominant[segment]
  activity <- matrix(settings$background_gain, nrow = time_points, ncol = 3L)
  activity[cbind(seq_len(time_points), dominant)] <- settings$active_gain
  list(activity = activity, dominant = dominant)
}

lomm_process_data <- function(seed = 20260901L, time_points = 100L, dt = 0.4) {
  settings <- lomm_process_settings()
  parameters <- lomm_figure1_parameters()
  dimension_labels <- lomm_process_dimension_labels()
  parameters$factor_label <- dimension_labels
  latent_inputs <- lomm_figure1_simulate(
    parameters,
    time_points = time_points,
    dt = dt,
    stochastic = TRUE,
    seed = seed
  )
  latent_no_inputs <- lomm_figure1_simulate(
    parameters,
    time_points = time_points,
    dt = dt,
    stochastic = FALSE
  )
  schedule <- lomm_process_activity(time_points, settings)
  position <- matrix(latent_inputs$position, nrow = time_points)
  expressed <- position * schedule$activity
  loadings <- settings$loadings
  errors <- lomm_figure1_with_seed(seed + 1L, function() {
    matrix(
      stats::rnorm(time_points * length(loadings) * 3L, sd = settings$residual_sd),
      nrow = time_points
    )
  })
  physical_time <- (seq_len(time_points) - 1L) * dt
  rows <- list()
  for (dimension in 1:3) {
    label <- dimension_labels[[dimension]]
    rows[[length(rows) + 1L]] <- data.frame(
      physical_time = physical_time,
      panel = "Latent process",
      dimension = label,
      series = "With acceleration inputs",
      value = position[, dimension],
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
      physical_time = physical_time,
      panel = "Latent process",
      dimension = label,
      series = "No acceleration inputs",
      value = matrix(latent_no_inputs$position, nrow = time_points)[, dimension],
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
      physical_time = physical_time,
      panel = "Activity",
      dimension = label,
      series = "Activity",
      value = schedule$activity[, dimension],
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
      physical_time = physical_time,
      panel = "Expressed signal",
      dimension = label,
      series = "Expressed signal",
      value = expressed[, dimension],
      stringsAsFactors = FALSE
    )
    for (index in seq_along(loadings)) {
      column <- (dimension - 1L) * length(loadings) + index
      rows[[length(rows) + 1L]] <- data.frame(
        physical_time = physical_time,
        panel = "Observed indicators",
        dimension = label,
        series = sprintf("z%d", column),
        value = loadings[[index]] * expressed[, dimension] + errors[, column],
        stringsAsFactors = FALSE
      )
    }
  }
  output <- do.call(rbind, rows)
  output$panel <- factor(
    output$panel,
    levels = c("Latent process", "Activity", "Expressed signal", "Observed indicators")
  )
  output$dimension <- factor(output$dimension, levels = dimension_labels)
  output
}

lomm_process_bands <- function(data) {
  activity <- subset(data, panel == "Activity")
  settings <- lomm_process_settings()
  active <- activity[activity$value == settings$active_gain, ]
  active <- active[order(active$physical_time), ]
  dt <- diff(sort(unique(data$physical_time)))[[1L]]
  change <- c(TRUE, active$dimension[-1L] != active$dimension[-nrow(active)])
  starts <- which(change)
  ends <- c(starts[-1L] - 1L, nrow(active))
  data.frame(
    dimension = active$dimension[starts],
    start = active$physical_time[starts] - dt / 2,
    end = active$physical_time[ends] + dt / 2
  )
}

lomm_process_correlation <- function(data) {
  indicators <- subset(data, panel == "Observed indicators")
  series_levels <- unique(indicators$series)
  wide <- sapply(series_levels, function(name) {
    indicators$value[indicators$series == name]
  })
  correlation <- stats::cor(wide)
  dimension_of <- vapply(series_levels, function(name) {
    as.character(indicators$dimension[indicators$series == name][[1L]])
  }, character(1))
  grid <- expand.grid(row = series_levels, column = series_levels, stringsAsFactors = FALSE)
  grid$value <- as.vector(correlation)
  grid$row <- factor(grid$row, levels = rev(series_levels))
  grid$column <- factor(grid$column, levels = series_levels)
  attr(grid, "dimension_of") <- dimension_of
  grid
}

lomm_process_plot <- function(data) {
  dimension_labels <- levels(data$dimension)
  dimension_colors <- stats::setNames(c("#0072B2", "#D55E00", "#009E73"), dimension_labels)
  dimension_linetypes <- stats::setNames(c("solid", "longdash", "dotdash"), dimension_labels)
  short_labels <- stats::setNames(paste("Dim.", 1:3), dimension_labels)
  bands <- lomm_process_bands(data)
  settings <- lomm_process_settings()
  x_limits <- range(data$physical_time) + c(-0.2, 0.2)

  base_theme <- theme_minimal(base_size = 8.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 8.5, face = "bold", margin = margin(b = 2)),
      plot.title.position = "plot",
      strip.text.y = element_text(size = 7.5, angle = 0, hjust = 0),
      axis.title.y = element_text(size = 9),
      axis.title.x = element_text(size = 8.5),
      legend.position = "bottom",
      legend.text = element_text(size = 7.5),
      plot.margin = margin(2, 4, 2, 4)
    )
  hide_x <- theme(legend.position = "none", axis.text.x = element_blank())
  band_layer <- geom_rect(
    data = bands,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = dimension),
    inherit.aes = FALSE,
    alpha = 0.10,
    show.legend = FALSE
  )
  dimension_scales <- list(
    scale_color_manual(values = dimension_colors, drop = FALSE),
    scale_fill_manual(values = dimension_colors, drop = FALSE),
    scale_x_continuous(limits = x_limits, expand = c(0, 0))
  )
  linetype_scale <- scale_linetype_manual(values = dimension_linetypes, drop = FALSE)

  latent <- subset(data, panel == "Latent process")
  panel_a <- ggplot(
    subset(latent, series == "With acceleration inputs"),
    aes(x = physical_time, y = value, color = dimension, linetype = dimension)
  ) +
    band_layer +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
    geom_line(
      data = subset(latent, series == "No acceleration inputs"),
      aes(group = dimension),
      color = "grey62",
      linewidth = 0.45
    ) +
    geom_line(linewidth = 0.7) +
    geom_text(
      data = bands,
      aes(
        x = (start + end) / 2,
        y = Inf,
        label = paste(short_labels[as.character(dimension)], "active"),
        color = dimension
      ),
      inherit.aes = FALSE,
      vjust = 1.4,
      size = 2.5,
      show.legend = FALSE
    ) +
    dimension_scales +
    linetype_scale +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    labs(
      title = expression(
        bold("A  Latent process ") * x[f * ",n"] * ": damped oscillator with acceleration inputs (grey: without inputs)"
      ),
      x = NULL,
      y = expression(x[f]),
      color = NULL,
      linetype = NULL
    ) +
    base_theme +
    hide_x

  activity <- subset(data, panel == "Activity")
  panel_b <- ggplot(
    activity,
    aes(x = physical_time, y = value, color = dimension, linetype = dimension)
  ) +
    band_layer +
    geom_step(linewidth = 0.7, direction = "mid") +
    facet_grid(dimension ~ ., labeller = as_labeller(short_labels)) +
    dimension_scales +
    linetype_scale +
    scale_y_continuous(
      breaks = c(settings$background_gain, settings$active_gain),
      labels = c("0.08", "1.00"),
      limits = c(-0.12, 1.15)
    ) +
    labs(
      title = expression(
        bold("B  Activity ") * a[f * ",n"] * ": 1.00 for the active dimension, 0.08 for the others"
      ),
      x = NULL,
      y = expression(a[f]),
      color = NULL,
      linetype = NULL
    ) +
    base_theme +
    hide_x +
    theme(panel.spacing.y = grid::unit(1.5, "pt"))

  expressed <- subset(data, panel == "Expressed signal")
  panel_c <- ggplot(
    expressed,
    aes(x = physical_time, y = value, color = dimension, linetype = dimension)
  ) +
    band_layer +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    dimension_scales +
    linetype_scale +
    labs(
      title = expression(
        bold("C  Expressed signal ") * tilde(x)[f * ",n"] == a[f * ",n"] * x[f * ",n"]
      ),
      x = NULL,
      y = expression(tilde(x)[f]),
      color = NULL,
      linetype = NULL
    ) +
    base_theme +
    hide_x

  indicators <- subset(data, panel == "Observed indicators")
  loaded_signal <- transform(
    expressed,
    value = value * mean(settings$loadings)
  )
  panel_d <- ggplot(
    indicators,
    aes(x = physical_time, y = value, color = dimension, group = series)
  ) +
    band_layer +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
    geom_line(linewidth = 0.3, alpha = 0.55) +
    geom_line(
      data = loaded_signal,
      aes(x = physical_time, y = value, group = dimension),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.7
    ) +
    facet_grid(dimension ~ ., labeller = as_labeller(short_labels)) +
    dimension_scales +
    labs(
      title = expression(
        bold("D  Observed indicators ") * z[j * ",n"] == lambda[j] * tilde(x)[f * ",n"] + epsilon[j * ",n"] *
          ", four per dimension (black: " * bar(lambda) * tilde(x)[f * ",n"] * ")"
      ),
      x = expression("Time " * t[n] ~ "(" * Delta == 0.4 * ")"),
      y = expression(z[j]),
      color = NULL
    ) +
    base_theme +
    theme(legend.position = "none", panel.spacing.y = grid::unit(1.5, "pt"))

  correlation <- lomm_process_correlation(data)
  dimension_of <- attr(correlation, "dimension_of")
  block_edges <- cumsum(rle(unname(dimension_of))$lengths)[-3L] + 0.5
  axis_colors <- unname(dimension_colors[dimension_of])
  panel_e <- ggplot(correlation, aes(x = column, y = row, fill = value)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_hline(yintercept = length(dimension_of) - block_edges + 1, color = "grey20", linewidth = 0.5) +
    geom_vline(xintercept = block_edges, color = "grey20", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#2166AC",
      midpoint = 0, limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      name = "r"
    ) +
    scale_x_discrete(labels = function(x) sub("^z", "", x)) +
    scale_y_discrete(labels = function(x) sub("^z", "", x)) +
    coord_equal() +
    labs(
      title = expression(bold("E  Indicator correlations")),
      x = expression("Indicator " * j),
      y = NULL
    ) +
    base_theme +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 6.5, color = axis_colors),
      axis.text.y = element_text(size = 6.5, color = rev(axis_colors)),
      legend.position = "right",
      legend.key.height = grid::unit(10, "pt"),
      legend.key.width = grid::unit(5, "pt"),
      legend.title = element_text(size = 7.5),
      legend.text = element_text(size = 6.5)
    )

  legend_source <- ggplot(
    subset(latent, series == "With acceleration inputs"),
    aes(x = physical_time, y = value, color = dimension, linetype = dimension)
  ) +
    geom_line() +
    scale_color_manual(values = dimension_colors, drop = FALSE) +
    scale_linetype_manual(values = dimension_linetypes, drop = FALSE) +
    labs(color = NULL, linetype = NULL) +
    theme_minimal(base_size = 8.5) +
    theme(
      legend.position = "top",
      legend.direction = "vertical",
      legend.text = element_text(size = 7.5),
      legend.key.width = grid::unit(18, "pt")
    )
  legend_grob <- cowplot::get_plot_component(legend_source, "guide-box-top", return_all = TRUE)
  panel_legend <- patchwork::wrap_elements(full = legend_grob)

  left <- patchwork::wrap_plots(
    panel_a, panel_b, panel_c, panel_d,
    ncol = 1L, heights = c(1.45, 1.15, 1.1, 2.2)
  )
  right <- patchwork::wrap_plots(
    panel_legend, panel_e, patchwork::plot_spacer(),
    ncol = 1L, heights = c(0.8, 2.3, 0.6)
  )
  patchwork::wrap_plots(left, right, ncol = 2L, widths = c(2.8, 1.1))
}

lomm_process_write <- function(
  output_path = file.path(
    lomm_figure1_root,
    "paper",
    "figures",
    "lomm_process_overview.pdf"
  ),
  # Chosen so that dimensions 2 and 3 have visible latent amplitude when the
  # schedule switches them on; the illustration is otherwise an ordinary draw.
  seed = 20260906L
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  figure <- lomm_process_plot(lomm_process_data(seed = seed))
  ggsave(
    output_path,
    figure,
    width = 7.15,
    height = 6.6,
    units = "in",
    device = cairo_pdf,
    bg = "white"
  )
  invisible(normalizePath(output_path, mustWork = TRUE))
}

lomm_figure1_invoked_directly <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_argument) != 1L) {
    return(FALSE)
  }
  invoked <- normalizePath(sub("^--file=", "", file_argument), mustWork = FALSE)
  expected <- normalizePath(
    file.path(lomm_figure1_root, "scripts", "figures", "plot_lomm_three_factor_example.R"),
    mustWork = TRUE
  )
  identical(invoked, expected)
}

if (lomm_figure1_invoked_directly()) {
  written <- lomm_figure1_write()
  cat("Wrote", written, "\n")
  process_written <- lomm_process_write()
  cat("Wrote", process_written, "\n")
}
