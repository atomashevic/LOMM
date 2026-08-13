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
}
