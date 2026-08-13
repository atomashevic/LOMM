#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

lomm_mafw_stop <- function(...) stop(..., call. = FALSE)

lomm_mafw_script_path <- function() {
  if (exists("LOMM_MAFW_SCRIPT_PATH", inherits = TRUE)) {
    return(normalizePath(
      get("LOMM_MAFW_SCRIPT_PATH", inherits = TRUE), mustWork = TRUE
    ))
  }
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[
    !is.na(frames) &
      basename(frames) == "plot_lomm_mafw_closest_timeseries.R"
  ]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) lomm_mafw_stop("Could not locate the figure script.")
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM_MAFW_SCRIPT_PATH <- lomm_mafw_script_path()
LOMM_MAFW_ROOT <- normalizePath(
  file.path(dirname(LOMM_MAFW_SCRIPT_PATH), "..", ".."), mustWork = TRUE
)
LOMM_MAFW_CLIP <- "02875.mp4"
LOMM_MAFW_PROFILE_ID <- "110"
LOMM_MAFW_SMOOTH_WINDOW <- 7L
LOMM_MAFW_EMOTION_LABELS <- c(
  anger = "Anger",
  disgust = "Disgust",
  fear = "Fear",
  happiness = "Happiness",
  neutral = "Neutral",
  sadness = "Sadness",
  surprise = "Surprise",
  contempt = "Contempt",
  anxiety = "Anxiety",
  helplessness = "Helplessness",
  disappointment = "Disappointment"
)

lomm_mafw_paths <- function(root = LOMM_MAFW_ROOT) {
  plan_dir <- file.path(
    root, "simulation_outputs", "lomm_1_1", "empirical_realism_ablation",
    "854e8722d647"
  )
  run_dir <- file.path(
    root, "simulation_outputs", "lomm_1_1", "empirical_realism_ablation_runs",
    "854e8722d647"
  )
  input_dir <- file.path(
    root, "simulation_outputs", "lomm_1_1", "empirical_compositional_preprocess",
    "0cdc11154d8a"
  )
  list(
    runner = file.path(root, "scripts", "lomm_1_1", "run_realism_ablation.R"),
    plan_dir = plan_dir,
    run_dir = run_dir,
    selected_candidates = file.path(run_dir, "selected_candidates.csv"),
    probabilities = file.path(input_dir, "probability_view.csv"),
    output_pdf = file.path(root, "paper", "figures", "lomm_mafw_closest_timeseries.pdf"),
    output_png = file.path(root, "paper", "figures", "lomm_mafw_closest_timeseries.png"),
    output_series = file.path(root, "paper", "tables", "lomm_mafw_closest_timeseries.csv"),
    output_ranking = file.path(root, "paper", "tables", "lomm_mafw_closest_timeseries_ranking.csv"),
    output_summary = file.path(root, "paper", "tables", "lomm_mafw_closest_timeseries_summary.csv"),
    output_caption = file.path(root, "paper", "tables", "lomm_mafw_closest_timeseries_caption.txt")
  )
}

lomm_mafw_require_paths <- function(paths) {
  inputs <- c(
    paths$runner, paths$plan_dir, paths$selected_candidates, paths$probabilities
  )
  missing <- inputs[!file.exists(inputs)]
  if (length(missing) > 0L) {
    lomm_mafw_stop("Missing required input: ", paste(missing, collapse = ", "))
  }
  invisible(paths)
}

lomm_mafw_runner_environment <- function(paths) {
  assign(
    "LOMM11_RUNNER_PATH",
    normalizePath(paths$runner, mustWork = TRUE),
    envir = .GlobalEnv
  )
  source(paths$runner, local = .GlobalEnv)
  .GlobalEnv
}

lomm_mafw_prepare <- function(paths = lomm_mafw_paths()) {
  lomm_mafw_require_paths(paths)
  runner <- lomm_mafw_runner_environment(paths)
  arguments <- runner$lomm11_runner_parse_args(c(
    "--plan-dir", normalizePath(paths$plan_dir, mustWork = TRUE), "--dry-run"
  ))
  context <- runner$lomm11_runner_load_context(arguments)
  caches <- runner$lomm11_runner_build_fold_caches(context)
  assignments <- context$plan$fold_assignments.csv
  assignment <- assignments[assignments$clip == LOMM_MAFW_CLIP, , drop = FALSE]
  if (nrow(assignment) != 1L) {
    lomm_mafw_stop("The fixed MAFW clip does not have one fold assignment.")
  }
  fold <- as.integer(assignment$fold[[1L]])
  cache <- caches[[as.character(fold)]]
  clip_plan <- cache$heldout_generation_plan[
    cache$heldout_generation_plan$clip == LOMM_MAFW_CLIP, , drop = FALSE
  ]
  if (nrow(clip_plan) < 2L || length(unique(clip_plan$generation_segment_id)) != 1L) {
    lomm_mafw_stop("The fixed clip must contain one contiguous generation segment.")
  }

  selected <- utils::read.csv(paths$selected_candidates, stringsAsFactors = FALSE)
  heldout_rows <- context$plan$heldout_plan.csv
  heldout_rows <- heldout_rows[
    heldout_rows$fold == fold &
      as.character(heldout_rows$variant_id) == LOMM_MAFW_PROFILE_ID,
    , drop = FALSE
  ]
  heldout_rows <- heldout_rows[order(heldout_rows$heldout_rep), , drop = FALSE]
  if (nrow(heldout_rows) != 100L || !identical(heldout_rows$heldout_rep, 1:100)) {
    lomm_mafw_stop("Expected exactly 100 ordered held-out draws for the selected profile.")
  }
  selected_row <- selected[
    selected$selection_key == unique(heldout_rows$selection_key), , drop = FALSE
  ]
  if (nrow(selected_row) != 1L) {
    lomm_mafw_stop("The selected configuration has no unique candidate.")
  }
  candidate <- context$plan$candidate_grid.csv[
    context$plan$candidate_grid.csv$candidate_id == selected_row$candidate_id[[1L]],
    , drop = FALSE
  ]
  if (nrow(candidate) != 1L ||
      !isTRUE(as.logical(candidate$B[[1L]])) ||
      !isTRUE(as.logical(candidate$S[[1L]])) ||
      isTRUE(as.logical(candidate$G[[1L]]))) {
    lomm_mafw_stop("The selected candidate is not empirical baseline plus clip scale.")
  }

  empirical <- utils::read.csv(paths$probabilities, stringsAsFactors = FALSE)
  empirical <- empirical[
    empirical$analysis_variant == "complete_case" &
      empirical$clip == LOMM_MAFW_CLIP,
    , drop = FALSE
  ]
  empirical <- empirical[order(empirical$frame_index), , drop = FALSE]
  emotions <- rownames(cache$basis)
  required <- c("frame_key", "frame_index", emotions)
  if (length(setdiff(required, names(empirical))) > 0L ||
      nrow(empirical) != nrow(clip_plan) || anyDuplicated(empirical$frame_key)) {
    lomm_mafw_stop("The empirical clip and generation plan do not align.")
  }
  empirical_matrix <- as.matrix(empirical[, emotions, drop = FALSE])
  if (any(!is.finite(empirical_matrix)) || any(empirical_matrix <= 0) ||
      max(abs(rowSums(empirical_matrix) - 1)) > 1e-8) {
    lomm_mafw_stop("The empirical score composition is invalid.")
  }
  list(
    runner = runner,
    context = context,
    cache = cache,
    candidate = candidate,
    heldout_rows = heldout_rows,
    clip_plan = clip_plan,
    empirical = empirical,
    emotions = emotions,
    fold = fold,
    selected_candidate = selected_row
  )
}

lomm_mafw_generate_draw <- function(prepared, draw_row) {
  generated <- prepared$runner$lomm11_runner_generate(
    prepared$clip_plan,
    prepared$candidate,
    prepared$cache,
    draw_row,
    prepared$context
  )
  panel <- generated$panel[order(generated$panel$frame_index), , drop = FALSE]
  if (!identical(panel$frame_key, prepared$empirical$frame_key)) {
    lomm_mafw_stop("A generated draw does not align with the empirical frame keys.")
  }
  probability <- as.matrix(panel[, prepared$emotions, drop = FALSE])
  if (any(!is.finite(probability)) || any(probability <= 0) ||
      max(abs(rowSums(probability) - 1)) > 1e-10) {
    lomm_mafw_stop("A generated score composition is invalid.")
  }
  list(panel = panel, probability = probability, generated = generated)
}

lomm_mafw_rank_draws <- function(prepared) {
  empirical <- as.matrix(prepared$empirical[, prepared$emotions, drop = FALSE])
  empirical_difference <- apply(empirical, 2L, diff)
  level_scale <- stats::sd(as.vector(empirical))
  change_scale <- stats::sd(as.vector(empirical_difference))
  if (!is.finite(level_scale) || !is.finite(change_scale) ||
      level_scale <= 0 || change_scale <= 0) {
    lomm_mafw_stop("The empirical level or change scale is invalid.")
  }

  generated_draws <- vector("list", nrow(prepared$heldout_rows))
  ranking_rows <- vector("list", nrow(prepared$heldout_rows))
  for (index in seq_len(nrow(prepared$heldout_rows))) {
    row <- prepared$heldout_rows[index, , drop = FALSE]
    draw <- lomm_mafw_generate_draw(prepared, row)
    generated_draws[[index]] <- draw
    difference <- apply(draw$probability, 2L, diff)
    level_rmse <- sqrt(mean((draw$probability - empirical)^2))
    change_rmse <- sqrt(mean((difference - empirical_difference)^2))
    emotion_correlations <- vapply(seq_along(prepared$emotions), function(column) {
      stats::cor(empirical[, column], draw$probability[, column])
    }, numeric(1))
    ranking_rows[[index]] <- data.frame(
      draw = as.integer(row$heldout_rep[[1L]]),
      seed = as.integer(row$seed_base[[1L]]),
      level_rmse = level_rmse,
      change_rmse = change_rmse,
      normalized_level_rmse = level_rmse / level_scale,
      normalized_change_rmse = change_rmse / change_scale,
      trajectory_distance = 0.5 * level_rmse / level_scale +
        0.5 * change_rmse / change_scale,
      mean_emotion_correlation = mean(emotion_correlations),
      minimum_emotion_correlation = min(emotion_correlations),
      maximum_emotion_correlation = max(emotion_correlations),
      stringsAsFactors = FALSE
    )
  }
  ranking <- do.call(rbind, ranking_rows)
  ordering <- order(
    ranking$trajectory_distance, ranking$level_rmse,
    ranking$change_rmse, ranking$draw
  )
  ranking <- ranking[ordering, , drop = FALSE]
  ranking$rank <- seq_len(nrow(ranking))
  ranking <- ranking[, c("rank", setdiff(names(ranking), "rank")), drop = FALSE]
  selected_index <- match(ranking$draw[[1L]], prepared$heldout_rows$heldout_rep)
  list(
    ranking = ranking,
    selected = generated_draws[[selected_index]],
    level_scale = level_scale,
    change_scale = change_scale
  )
}

lomm_mafw_smooth <- function(values, window = LOMM_MAFW_SMOOTH_WINDOW) {
  window <- as.integer(window)
  half_width <- floor(window / 2L)
  vapply(seq_along(values), function(index) {
    lower <- max(1L, index - half_width)
    upper <- min(length(values), index + half_width)
    mean(values[lower:upper])
  }, numeric(1))
}

lomm_mafw_long_data <- function(prepared, ranked) {
  selected_draw <- ranked$ranking$draw[[1L]]
  sources <- stats::setNames(
    list(prepared$empirical, ranked$selected$panel),
    c(
      "A  MAFW CLIP classifier scores",
      "B  LOMM realization"
    )
  )
  rows <- lapply(names(sources), function(source) {
    frame <- sources[[source]]
    do.call(rbind, lapply(prepared$emotions, function(emotion) {
      values <- as.numeric(frame[[emotion]])
      data.frame(
        source = source,
        frame_index = as.integer(frame$frame_index),
        emotion = emotion,
        score = values,
        smoothed_score = lomm_mafw_smooth(values),
        stringsAsFactors = FALSE
      )
    }))
  })
  output <- do.call(rbind, rows)
  empirical_means <- colMeans(
    prepared$empirical[, prepared$emotions, drop = FALSE]
  )
  highlighted <- names(sort(empirical_means, decreasing = TRUE))[seq_len(4L)]
  output$highlighted <- output$emotion %in% highlighted
  output$source <- factor(output$source, levels = names(sources))
  output$emotion <- factor(output$emotion, levels = prepared$emotions)
  list(data = output, highlighted = highlighted)
}

lomm_mafw_plot <- function(prepared, ranked) {
  display <- lomm_mafw_long_data(prepared, ranked)
  highlighted <- display$highlighted
  colors <- stats::setNames(
    c("#0072B2", "#D55E00", "#CC79A7", "#009E73"), highlighted
  )
  linetypes <- stats::setNames(c("solid", "longdash", "dotdash", "dashed"), highlighted)
  plot_data <- display$data
  other <- plot_data[!plot_data$highlighted, , drop = FALSE]
  focus <- plot_data[plot_data$highlighted, , drop = FALSE]
  upper <- max(0.7, ceiling(max(plot_data$score) * 10) / 10)

  ggplot() +
    geom_line(
      data = other,
      aes(x = frame_index, y = score, group = interaction(source, emotion)),
      color = "#A6A6A6", linewidth = 0.28, alpha = 0.16
    ) +
    geom_line(
      data = focus,
      aes(x = frame_index, y = score, color = emotion, linetype = emotion),
      linewidth = 0.30, alpha = 0.25, show.legend = FALSE
    ) +
    geom_line(
      data = focus,
      aes(
        x = frame_index, y = smoothed_score,
        color = emotion, linetype = emotion
      ),
      linewidth = 0.92, alpha = 0.98
    ) +
    facet_wrap(~source, ncol = 1L, scales = "fixed") +
    scale_color_manual(
      values = colors,
      breaks = highlighted,
      labels = unname(LOMM_MAFW_EMOTION_LABELS[highlighted]),
      name = NULL
    ) +
    scale_linetype_manual(
      values = linetypes,
      breaks = highlighted,
      labels = unname(LOMM_MAFW_EMOTION_LABELS[highlighted]),
      name = NULL
    ) +
    scale_x_continuous(
      breaks = c(1, 10, 20, 30, 40, 49),
      limits = range(prepared$empirical$frame_index),
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      limits = c(0, upper), breaks = seq(0, upper, by = 0.1),
      expand = expansion(mult = c(0, 0.015))
    ) +
    labs(
      title = "MAFW CLIP classifier scores and a matched LOMM realization",
      subtitle = paste(
        "Fixed 49-frame clip; closest of 100 prespecified simulations using",
        "all 11 unsmoothed emotions"
      ),
      x = "Frame",
      y = "Classifier-output score"
    ) +
    guides(
      color = guide_legend(
        nrow = 1L, byrow = TRUE,
        override.aes = list(linewidth = 1.1, alpha = 1)
      ),
      linetype = guide_legend(nrow = 1L, byrow = TRUE)
    ) +
    theme_classic(base_size = 8.5, base_family = "sans") +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 7.8),
      legend.key.width = grid::unit(13, "pt"),
      legend.margin = margin(0, 0, 1, 0),
      axis.text = element_text(size = 7.4),
      axis.title = element_text(size = 8.6),
      axis.line = element_line(linewidth = 0.28),
      axis.ticks = element_line(linewidth = 0.28),
      strip.background = element_rect(fill = "#F2F2F2", color = NA),
      strip.text = element_text(size = 8.2, face = "bold", hjust = 0),
      panel.spacing.y = grid::unit(0.55, "lines"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 10.2, face = "bold"),
      plot.subtitle = element_text(size = 8.0),
      plot.margin = margin(5, 7, 5, 7)
    )
}

lomm_mafw_caption <- function(ranked) {
  winner <- ranked$ranking[1L, , drop = FALSE]
  paste0(
    "Real MAFW classifier scores for a fixed 49-frame clip compared with the ",
    "closest of 100 LOMM realizations generated with the clip baseline + scale ",
    "configuration. The realization shown minimized an equally weighted average ",
    "of the root mean squared error in the scores and in their frame-to-frame ",
    "changes, each standardized by the corresponding scale in the real data, ",
    "across all 11 emotions (realization ", winner$draw,
    "; distance = ", sprintf("%.3f", winner$trajectory_distance), "). ",
    "Selection used unsmoothed values; the plotted highlighted trajectories use ",
    "a seven-frame moving average. Highlighted emotions were fixed from the four ",
    "highest empirical emotion means and applied unchanged to both panels. This ",
    "post hoc best-case match was made for illustration and does not describe ",
    "typical simulation fit."
  )
}

lomm_mafw_atomic_csv <- function(frame, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = dirname(path), fileext = ".csv"
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) lomm_mafw_stop("Failed to publish ", path)
}

lomm_mafw_atomic_text <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".caption-", tmpdir = dirname(path), fileext = ".txt")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(text, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) lomm_mafw_stop("Failed to publish ", path)
}

lomm_mafw_atomic_figure <- function(figure, path, device) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = dirname(path), fileext = paste0(".", device)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  if (identical(device, "pdf")) {
    ggsave(
      temporary, figure, width = 7.15, height = 4.8, units = "in",
      device = grDevices::cairo_pdf, bg = "white"
    )
  } else {
    ggsave(
      temporary, figure, width = 7.15, height = 4.8, units = "in",
      dpi = 450, bg = "white"
    )
  }
  if (!file.rename(temporary, path)) lomm_mafw_stop("Failed to publish ", path)
}

lomm_mafw_output_series <- function(prepared, ranked) {
  selected_draw <- ranked$ranking$draw[[1L]]
  empirical <- prepared$empirical[, c("frame_index", prepared$emotions), drop = FALSE]
  generated <- ranked$selected$panel[, c("frame_index", prepared$emotions), drop = FALSE]
  empirical$series <- "MAFW CLIP classifier scores"
  generated$series <- "LOMM realization"
  empirical$draw <- NA_integer_
  generated$draw <- selected_draw
  output <- rbind(empirical, generated)
  output[, c("series", "draw", "frame_index", prepared$emotions), drop = FALSE]
}

lomm_mafw_summary <- function(prepared, ranked) {
  winner <- ranked$ranking[1L, , drop = FALSE]
  data.frame(
    empirical_clip = LOMM_MAFW_CLIP,
    empirical_label = as.character(prepared$empirical$truth_label[[1L]]),
    frames = nrow(prepared$empirical),
    emotions = length(prepared$emotions),
    candidate_draws = nrow(ranked$ranking),
    selected_draw = winner$draw,
    selected_seed = winner$seed,
    trajectory_distance = winner$trajectory_distance,
    level_rmse = winner$level_rmse,
    change_rmse = winner$change_rmse,
    level_scale = ranked$level_scale,
    change_scale = ranked$change_scale,
    observation_profile = "Clip baseline + scale",
    selection_scope = "Illustrative best case among 100 prespecified draws",
    stringsAsFactors = FALSE
  )
}

lomm_mafw_build <- function(paths = lomm_mafw_paths()) {
  prepared <- lomm_mafw_prepare(paths)
  ranked <- lomm_mafw_rank_draws(prepared)
  figure <- lomm_mafw_plot(prepared, ranked)
  lomm_mafw_atomic_csv(lomm_mafw_output_series(prepared, ranked), paths$output_series)
  lomm_mafw_atomic_csv(ranked$ranking, paths$output_ranking)
  lomm_mafw_atomic_csv(lomm_mafw_summary(prepared, ranked), paths$output_summary)
  lomm_mafw_atomic_text(lomm_mafw_caption(ranked), paths$output_caption)
  lomm_mafw_atomic_figure(figure, paths$output_pdf, "pdf")
  lomm_mafw_atomic_figure(figure, paths$output_png, "png")
  cat(
    "Published LOMM--MAFW illustrative comparison:\n",
    paths$output_pdf, "\n", paths$output_png, "\n",
    paths$output_series, "\n", paths$output_ranking, "\n",
    paths$output_summary, "\n", paths$output_caption, "\n",
    sep = ""
  )
  invisible(list(prepared = prepared, ranked = ranked, figure = figure, paths = paths))
}

lomm_mafw_invoked_directly <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) return(FALSE)
  invoked <- normalizePath(sub("^--file=", "", argument), mustWork = FALSE)
  identical(basename(invoked), basename(LOMM_MAFW_SCRIPT_PATH))
}

if (lomm_mafw_invoked_directly()) {
  lomm_mafw_build()
}
