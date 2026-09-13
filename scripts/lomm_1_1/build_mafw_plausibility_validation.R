#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

mafw_stop <- function(...) stop(..., call. = FALSE)

mafw_script_path <- function() {
  if (exists("MAFW_PLAUSIBILITY_SCRIPT_PATH", inherits = TRUE)) {
    return(normalizePath(
      get("MAFW_PLAUSIBILITY_SCRIPT_PATH", inherits = TRUE), mustWork = TRUE
    ))
  }
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[
    !is.na(frames) & basename(frames) == "build_mafw_plausibility_validation.R"
  ]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) mafw_stop("Could not locate the validation script.")
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

MAFW_PLAUSIBILITY_SCRIPT_PATH <- mafw_script_path()
MAFW_PLAUSIBILITY_ROOT <- normalizePath(
  file.path(dirname(MAFW_PLAUSIBILITY_SCRIPT_PATH), "..", ".."), mustWork = TRUE
)
MAFW_PLAUSIBILITY_PLAN <- Sys.getenv("MAFW_PLAUSIBILITY_PLAN", unset = file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "empirical_realism_ablation", "854e8722d647"
))
MAFW_PLAUSIBILITY_OUTPUT <- file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "mafw_plausibility_validation"
)
MAFW_PROFILE_ID <- "110"
MAFW_REFERENCE_REPLICATES <- 1000L
MAFW_HELDOUT_REPLICATES <- 100L
MAFW_TRAINING_REPLICATES <- 3L
MAFW_FOLD_COUNT <- 5L
MAFW_HELDOUT_CLIPS_PER_FOLD <- 20L
MAFW_REFERENCE_SEED <- 20260904L
MAFW_EMOTION_LABELS <- c(
  anger = "Anger", disgust = "Disgust", fear = "Fear",
  happiness = "Happiness", neutral = "Neutral", sadness = "Sadness",
  surprise = "Surprise", contempt = "Contempt", anxiety = "Anxiety",
  helplessness = "Helplessness", disappointment = "Disappointment"
)
MAFW_METRIC_LABELS <- c(
  level_median = "Median level",
  level_iqr = "Level IQR",
  difference_iqr = "Change IQR",
  lag1_correlation = "Lag-1 correlation",
  run_length_entropy = "Run-length entropy",
  spectral_entropy = "Spectral entropy",
  effective_rank = "Effective rank",
  total_variance = "Total variance"
)
MAFW_TEMPORAL_SELECTION_METRICS <- c(
  "difference_iqr", "lag1_correlation", "run_length_entropy",
  "spectral_entropy"
)

mafw_paths <- function() {
  list(
    runner = file.path(
      MAFW_PLAUSIBILITY_ROOT, "scripts", "lomm_1_1", "run_realism_ablation.R"
    ),
    training_losses = file.path(MAFW_PLAUSIBILITY_OUTPUT, "training_losses.csv"),
    selected_settings = file.path(MAFW_PLAUSIBILITY_OUTPUT, "selected_settings.csv"),
    geometry = file.path(MAFW_PLAUSIBILITY_OUTPUT, "fold_geometry.csv"),
    metric_results = file.path(MAFW_PLAUSIBILITY_OUTPUT, "metric_results.csv"),
    composite_results = file.path(MAFW_PLAUSIBILITY_OUTPUT, "composite_results.csv"),
    example_series = file.path(MAFW_PLAUSIBILITY_OUTPUT, "example_series.csv"),
    success = file.path(MAFW_PLAUSIBILITY_OUTPUT, "_SUCCESS.json"),
    table_metric = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "tables", "mafw_plausibility_metrics.csv"
    ),
    table_composite = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "tables", "mafw_plausibility_composite.csv"
    ),
    table_deviations = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "tables", "mafw_plausibility_deviations.csv"
    ),
    table_settings = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "tables", "mafw_plausibility_fold_settings.csv"
    ),
    table_example = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "tables", "mafw_lomm_representative_example.csv"
    ),
    caption = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "tables",
      "mafw_lomm_representative_example_caption.txt"
    ),
    figure_pdf = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "figures",
      "mafw_lomm_representative_example.pdf"
    ),
    figure_png = file.path(
      MAFW_PLAUSIBILITY_ROOT, "paper", "figures",
      "mafw_lomm_representative_example.png"
    )
  )
}

mafw_atomic_csv <- function(frame, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = dirname(path), fileext = ".csv"
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) mafw_stop("Failed to publish ", path, ".")
  invisible(path)
}

mafw_atomic_text <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".text-", tmpdir = dirname(path), fileext = ".txt")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) mafw_stop("Failed to publish ", path, ".")
  invisible(path)
}

mafw_prepare <- function(paths = mafw_paths()) {
  bundle <- Sys.getenv("MAFW_COMPOSITIONAL_BUNDLE", unset = "")
  required <- c(paths$runner, if (nzchar(bundle)) bundle else MAFW_PLAUSIBILITY_PLAN)
  if (any(!file.exists(required))) {
    mafw_stop("Missing required input: ", paste(required[!file.exists(required)], collapse = ", "))
  }
  assign("LOMM11_RUNNER_PATH", normalizePath(paths$runner), envir = .GlobalEnv)
  source(paths$runner, local = .GlobalEnv)
  if (nzchar(bundle)) {
    input_directory <- normalizePath(bundle, mustWork = TRUE)
    # Verify the original score-file hash and rebuild the numerical tables;
    # fresh metadata and floating-point serialization need not match an old run.
    lomm_validate_published_bundle(input_directory)
    config <- lomm11_read_config(LOMM11_RUNNER_DEPENDENCIES[["config"]])
    input_tables <- lomm_validation_read_tables(input_directory)
    plan <- lomm11_build_plan(config, input_tables[["clip_qc.csv"]],
      input_tables[["probability_view.csv"]])
    context <- list(config = config, plan = plan$tables)
  } else {
    arguments <- lomm11_runner_parse_args(c(
      "--plan-dir", normalizePath(MAFW_PLAUSIBILITY_PLAN), "--dry-run"
    ))
    context <- lomm11_runner_load_context(arguments)
    input_directory <- normalizePath(
      context$manifest$input_bundle$resolved_path, mustWork = TRUE
    )
    input_tables <- lomm_validation_read_tables(input_directory)
  }
  basis <- lomm11_runner_basis(input_tables)
  view_index <- lomm11_view_index_from_ledger(input_tables[["frame_ledger.csv"]])
  list(
    paths = paths, context = context, input_tables = input_tables,
    basis = basis, view_index = view_index
  )
}

mafw_fit_geometry <- function(ilr_view, training_clips, n_directions = 3L,
                              psd_floor = 1e-8) {
  coordinate_columns <- sprintf("ilr_%02d", 1:10)
  prepared <- lomm11_training_rows(ilr_view, training_clips, coordinate_columns)
  center <- colMeans(prepared$values)
  clip_indices <- split(seq_len(nrow(prepared$data)), prepared$data$clip)
  clip_means <- t(vapply(clip_indices, function(index) {
    colMeans(prepared$values[index, , drop = FALSE])
  }, numeric(length(coordinate_columns))))
  colnames(clip_means) <- coordinate_columns
  within <- prepared$values - clip_means[match(prepared$data$clip, rownames(clip_means)), ]

  raw_scales <- vapply(clip_indices, function(index) {
    sqrt(mean(rowSums(within[index, , drop = FALSE]^2)))
  }, numeric(1))
  if (any(!is.finite(raw_scales)) || any(raw_scales <= 0)) {
    mafw_stop("Every training clip must have positive finite within-clip scale.")
  }
  scale_anchor <- stats::median(raw_scales)
  clip_scales <- raw_scales / scale_anchor
  standardized_within <- within / clip_scales[as.character(prepared$data$clip)]
  within_covariance <- stats::cov(standardized_within)
  decomposition <- eigen(within_covariance, symmetric = TRUE)
  indices <- seq_len(n_directions)
  signal_variances <- pmax(decomposition$values[indices], psd_floor)
  directions <- apply(
    decomposition$vectors[, indices, drop = FALSE], 2L, lomm11_orient_direction
  )
  if (is.null(dim(directions))) directions <- matrix(directions, ncol = 1L)
  rownames(directions) <- coordinate_columns
  colnames(directions) <- sprintf("factor_%02d", indices)
  relative_scales <- sqrt(signal_variances) / sqrt(mean(signal_variances))
  loadings <- sweep(directions, 2L, relative_scales, "*")

  baseline_deviations <- sweep(clip_means, 2L, center, "-")
  baseline_full <- stats::cov(baseline_deviations)
  baseline_shrunk <- 0.5 * baseline_full + 0.5 * diag(diag(baseline_full))
  dimnames(baseline_shrunk) <- list(coordinate_columns, coordinate_columns)
  geometry <- structure(list(
    coordinate_columns = coordinate_columns,
    n_directions = as.integer(n_directions),
    training_clips = sort(prepared$clips),
    n_training_rows = nrow(prepared$data),
    center = stats::setNames(center, coordinate_columns),
    directions = directions,
    direction_eigenvalues = stats::setNames(
      decomposition$values[indices], colnames(directions)
    ),
    relative_scales = stats::setNames(relative_scales, colnames(directions)),
    loadings = loadings,
    between_clip_covariance = lomm11_psd_floor(baseline_shrunk, psd_floor),
    within_clip_covariance = lomm11_psd_floor(within_covariance, psd_floor),
    residual_sd = NA_real_,
    empirical_clip_scales = stats::setNames(clip_scales, names(raw_scales)),
    scale_anchor = scale_anchor,
    signal_variances = stats::setNames(signal_variances, colnames(directions)),
    psd_floor = psd_floor
  ), class = "lomm11_training_geometry")
  lomm11_validate_training_geometry(geometry)
  geometry
}

mafw_fold_cache <- function(fold, prepared) {
  context <- prepared$context
  input_tables <- prepared$input_tables
  assignments <- context$plan$fold_assignments.csv
  training_clips <- as.character(assignments$clip[assignments$fold != fold])
  heldout_clips <- as.character(assignments$clip[assignments$fold == fold])
  empirical_training <- lomm11_runner_empirical_contributions(
    input_tables, "complete_case", training_clips
  )
  empirical_heldout <- lomm11_runner_empirical_contributions(
    input_tables, "complete_case", heldout_clips
  )
  geometry <- mafw_fit_geometry(
    input_tables[["ilr_view.csv"]], training_clips,
    n_directions = as.integer(context$config$measurement$n_latent_directions),
    psd_floor = as.numeric(context$config$measurement$between_clip_psd_floor)
  )
  list(
    fold = as.integer(fold),
    training_clips = training_clips,
    heldout_clips = heldout_clips,
    geometry = geometry,
    basis = prepared$basis,
    scales = lomm11_fit_metric_scales(empirical_training, training_clips),
    empirical_training = empirical_training,
    empirical_heldout = empirical_heldout,
    training_generation_plan = lomm11_runner_generation_plan(
      input_tables[["frame_ledger.csv"]], training_clips
    ),
    heldout_generation_plan = lomm11_runner_generation_plan(
      input_tables[["frame_ledger.csv"]], heldout_clips
    ),
    training_view_index = prepared$view_index[
      prepared$view_index$clip %in% training_clips, , drop = FALSE
    ],
    heldout_view_index = prepared$view_index[
      prepared$view_index$clip %in% heldout_clips, , drop = FALSE
    ]
  )
}

mafw_build_caches <- function(prepared) {
  caches <- lapply(seq_len(MAFW_FOLD_COUNT), mafw_fold_cache, prepared = prepared)
  names(caches) <- as.character(seq_len(MAFW_FOLD_COUNT))
  caches
}

mafw_shared_draws <- function(clip, geometry, master_seed, pairing_key) {
  seeds <- lomm11_segment_substream_seeds(
    master_seed, pairing_key, clip, LOMM11_CLIP_SHARED_SEGMENT
  )
  baseline <- as.numeric(lomm11_draw_gaussian(
    1L, geometry$between_clip_covariance, seeds[["between_clip_baseline"]]
  ))
  scale <- lomm11_with_seed(seeds[["clip_scale"]], function() {
    sample(unname(geometry$empirical_clip_scales), 1L, replace = TRUE)
  })
  list(baseline = baseline, scale = as.numeric(scale), seeds = seeds)
}

mafw_generate_dynamic_clip <- function(clip_plan, geometry, basis, candidate,
                                       master_seed, pairing_key) {
  clip_plan <- lomm11_prepare_generation_frame_plan(clip_plan)
  clip <- unique(as.character(clip_plan$clip))
  if (length(clip) != 1L) mafw_stop("Dynamic clip generation requires one clip.")
  shared <- mafw_shared_draws(clip, geometry, master_seed, pairing_key)
  system <- lomm11_build_unit_position_system(
    as.numeric(candidate$period_frames[[1L]]),
    as.numeric(candidate$damping_ratio[[1L]]), 1
  )
  segments <- split(
    clip_plan,
    factor(clip_plan$generation_segment_id, levels = unique(clip_plan$generation_segment_id))
  )
  processes <- lapply(names(segments), function(segment_id) {
    seeds <- lomm11_segment_substream_seeds(
      master_seed, pairing_key, clip, segment_id
    )
    lomm11_generate_segment_process(
      segments[[segment_id]], geometry, system, seeds,
      flags = c(B = TRUE, S = TRUE, G = FALSE),
      kappa = as.numeric(candidate$kappa[[1L]]),
      clip_scale = shared$scale,
      baseline = shared$baseline,
      residual_sd = as.numeric(candidate$residual_sd[[1L]]),
      gate_low = 0.25, gate_high = 1,
      gate_initial_high_probability = 0.5,
      gate_transition_observations = 3L
    )
  })
  names(processes) <- names(segments)
  panels <- Map(function(segment_plan, process) {
    lomm11_segment_panel(segment_plan, process, basis, 1, 0)
  }, segments, processes)
  panel <- do.call(rbind, panels)
  panel <- panel[order(panel$frame_index), , drop = FALSE]
  rownames(panel) <- NULL
  lomm11_validate_generated_panel(panel, basis)
  list(panel = panel, segments = processes, system = system)
}

mafw_generate_dynamic <- function(frame_plan, geometry, basis, candidate,
                                  master_seed, pairing_key) {
  plan <- lomm11_prepare_generation_frame_plan(frame_plan)
  clips <- split(plan, factor(plan$clip, levels = unique(plan$clip)))
  generated <- lapply(clips, function(clip_plan) {
    mafw_generate_dynamic_clip(
      clip_plan, geometry, basis, candidate, master_seed, pairing_key
    )
  })
  panel <- do.call(rbind, lapply(generated, `[[`, "panel"))
  rownames(panel) <- NULL
  list(panel = panel, clips = generated)
}

mafw_generate_static_clip <- function(clip_plan, geometry, basis, candidate,
                                      master_seed, pairing_key) {
  clip_plan <- lomm11_prepare_generation_frame_plan(clip_plan)
  clip <- unique(as.character(clip_plan$clip))
  if (length(clip) != 1L) mafw_stop("Static clip generation requires one clip.")
  shared <- mafw_shared_draws(clip, geometry, master_seed, pairing_key)
  segments <- split(
    clip_plan,
    factor(clip_plan$generation_segment_id, levels = unique(clip_plan$generation_segment_id))
  )
  panels <- lapply(names(segments), function(segment_id) {
    segment <- segments[[segment_id]]
    seeds <- lomm11_segment_substream_seeds(
      master_seed, pairing_key, clip, segment_id
    )
    residual_sd <- as.numeric(candidate$residual_sd[[1L]])
    static_covariance <- tcrossprod(geometry$loadings) +
      diag(residual_sd^2, nrow = length(geometry$center))
    dimnames(static_covariance) <- list(
      geometry$coordinate_columns, geometry$coordinate_columns
    )
    innovations <- lomm11_draw_gaussian(
      nrow(segment), static_covariance, seeds[["residuals"]]
    )
    ilr <- as.numeric(candidate$kappa[[1L]]) * shared$scale * innovations
    ilr <- sweep(ilr, 2L, shared$baseline, "+")
    ilr <- sweep(ilr, 2L, geometry$center, "+")
    colnames(ilr) <- geometry$coordinate_columns
    lomm11_segment_panel(segment, list(ilr = ilr), basis, 1, 0)
  })
  panel <- do.call(rbind, panels)
  panel <- panel[order(panel$frame_index), , drop = FALSE]
  rownames(panel) <- NULL
  lomm11_validate_generated_panel(panel, basis)
  list(panel = panel)
}

mafw_generate_static <- function(frame_plan, geometry, basis, candidate,
                                 master_seed, pairing_key) {
  plan <- lomm11_prepare_generation_frame_plan(frame_plan)
  clips <- split(plan, factor(plan$clip, levels = unique(plan$clip)))
  generated <- lapply(clips, function(clip_plan) {
    mafw_generate_static_clip(
      clip_plan, geometry, basis, candidate, master_seed, pairing_key
    )
  })
  panel <- do.call(rbind, lapply(generated, `[[`, "panel"))
  rownames(panel) <- NULL
  list(panel = panel, clips = generated)
}

mafw_evaluate <- function(generated, view_index, empirical, scales) {
  view <- lomm11_subset_panel_view(generated, view_index, "complete_case")
  contributions <- lomm11_metric_contributions(view, view, view)
  discrepancies <- lomm11_metric_discrepancies(empirical, contributions, scales)
  loss <- lomm11_selection_loss(discrepancies)
  list(
    view = view, contributions = contributions, discrepancies = discrepancies,
    selection_loss = loss$selection_loss,
    max_metric_discrepancy = loss$max_metric_discrepancy,
    temporal_loss = mean(discrepancies$discrepancy[
      discrepancies$metric %in% MAFW_TEMPORAL_SELECTION_METRICS
    ]),
    max_temporal_discrepancy = max(discrepancies$discrepancy[
      discrepancies$metric %in% MAFW_TEMPORAL_SELECTION_METRICS
    ])
  )
}

mafw_candidate_evaluations <- function(fold, stage, candidates, prepared, cache,
                                       training_plan) {
  rows <- vector("list", nrow(candidates) * MAFW_TRAINING_REPLICATES)
  cursor <- 0L
  for (candidate_index in seq_len(nrow(candidates))) {
    candidate <- candidates[candidate_index, , drop = FALSE]
    plans <- training_plan[
      training_plan$fold == fold &
        training_plan$candidate_id == candidate$seed_candidate_id[[1L]], , drop = FALSE
    ]
    plans <- plans[order(plans$training_rep), , drop = FALSE]
    if (nrow(plans) != MAFW_TRAINING_REPLICATES) {
      mafw_stop("Training plan does not contain three candidate replicates.")
    }
    for (replicate in seq_len(nrow(plans))) {
      plan_row <- plans[replicate, , drop = FALSE]
      generated <- mafw_generate_dynamic(
        cache$training_generation_plan, cache$geometry, cache$basis, candidate,
        as.integer(plan_row$seed_base[[1L]]),
        as.character(plan_row$seed_group[[1L]])
      )
      evaluation <- mafw_evaluate(
        generated, cache$training_view_index, cache$empirical_training,
        cache$scales
      )
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        stage = stage,
        fold = fold,
        candidate_id = as.character(candidate$candidate_id[[1L]]),
        seed_candidate_id = as.character(candidate$seed_candidate_id[[1L]]),
        period_frames = as.integer(candidate$period_frames[[1L]]),
        damping_ratio = as.numeric(candidate$damping_ratio[[1L]]),
        residual_sd = as.numeric(candidate$residual_sd[[1L]]),
        kappa = as.numeric(candidate$kappa[[1L]]),
        training_rep = as.integer(plan_row$training_rep[[1L]]),
        temporal_loss = evaluation$temporal_loss,
        max_temporal_discrepancy = evaluation$max_temporal_discrepancy,
        selection_loss = evaluation$selection_loss,
        max_metric_discrepancy = evaluation$max_metric_discrepancy,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

mafw_select_stage <- function(losses, objective, maximum) {
  formula <- stats::as.formula(paste0(
    "cbind(temporal_loss, max_temporal_discrepancy, selection_loss, ",
    "max_metric_discrepancy) ~ stage + fold + candidate_id + ",
    "seed_candidate_id + period_frames + damping_ratio + residual_sd + kappa"
  ))
  candidates <- aggregate(formula, data = losses, FUN = mean)
  order_index <- order(
    candidates[[objective]], candidates[[maximum]],
    abs(candidates$kappa - 1), candidates$residual_sd,
    -candidates$damping_ratio, candidates$candidate_id
  )
  candidates[order_index[[1L]], , drop = FALSE]
}

mafw_training_fold <- function(fold, prepared, caches, seed_ids, training_plan) {
  cache <- caches[[as.character(fold)]]
  checkpoint <- file.path(
    MAFW_PLAUSIBILITY_OUTPUT, sprintf("training_losses_v2_fold_%02d.csv", fold)
  )
  if (file.exists(checkpoint)) {
    existing <- utils::read.csv(checkpoint, stringsAsFactors = FALSE)
    if (nrow(existing) == 54L &&
        all(existing$fold == fold) && all(is.finite(existing$selection_loss))) {
      message("Training-only dynamic selection: reused fold ", fold, " checkpoint")
      return(existing)
    }
    mafw_stop("Existing training checkpoint is incomplete: ", checkpoint, ".")
  }
  message(
    "Training-only dynamic selection: fold ", fold, " of ", MAFW_FOLD_COUNT
  )
  dynamic_grid <- expand.grid(
    period_frames = c(12L, 24L, 48L),
    damping_ratio = c(0.01, 0.05, 0.20),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  dynamic_grid$candidate_id <- sprintf("D%02d", seq_len(nrow(dynamic_grid)))
  dynamic_grid$seed_candidate_id <- seed_ids[seq_len(nrow(dynamic_grid))]
  dynamic_grid$residual_sd <- 0.20
  dynamic_grid$kappa <- 1
  dynamic_losses <- mafw_candidate_evaluations(
    fold, "dynamics", dynamic_grid, prepared, cache, training_plan
  )
  dynamic_winner <- mafw_select_stage(
    dynamic_losses, "temporal_loss", "max_temporal_discrepancy"
  )

  measurement_grid <- expand.grid(
    residual_sd = c(0.10, 0.20, 0.30),
    kappa = c(0.75, 1.00, 1.25),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  measurement_grid$candidate_id <- sprintf("M%02d", seq_len(nrow(measurement_grid)))
  measurement_grid$seed_candidate_id <- seed_ids[10L + seq_len(nrow(measurement_grid))]
  measurement_grid$period_frames <- dynamic_winner$period_frames[[1L]]
  measurement_grid$damping_ratio <- dynamic_winner$damping_ratio[[1L]]
  measurement_losses <- mafw_candidate_evaluations(
    fold, "measurement", measurement_grid, prepared, cache, training_plan
  )
  output <- rbind(dynamic_losses, measurement_losses)
  mafw_atomic_csv(output, checkpoint)
  output
}

mafw_select_dynamic_settings <- function(prepared, caches) {
  original_grid <- prepared$context$plan$candidate_grid.csv
  original_grid <- original_grid[
    original_grid$variant_id == MAFW_PROFILE_ID, , drop = FALSE
  ]
  seed_ids <- as.character(original_grid$candidate_id)
  training_plan <- prepared$context$plan$training_plan.csv
  training_plan <- training_plan[
    training_plan$variant_id == MAFW_PROFILE_ID, , drop = FALSE
  ]
  fold_rows <- parallel::mclapply(
    seq_len(MAFW_FOLD_COUNT), mafw_training_fold,
    prepared = prepared, caches = caches, seed_ids = seed_ids,
    training_plan = training_plan, mc.cores = 4L, mc.preschedule = TRUE
  )
  failures <- vapply(fold_rows, inherits, logical(1), what = "try-error")
  if (any(failures)) {
    mafw_stop("At least one training fold failed: ", paste(fold_rows[failures], collapse = "; "))
  }
  losses <- do.call(rbind, fold_rows)
  selected <- do.call(rbind, lapply(seq_len(MAFW_FOLD_COUNT), function(fold) {
    fold_losses <- losses[
      losses$fold == fold & losses$stage == "measurement", , drop = FALSE
    ]
    winner <- mafw_select_stage(
      fold_losses, "selection_loss", "max_metric_discrepancy"
    )
    winner$selected_before_heldout <- TRUE
    winner$training_replicates <- MAFW_TRAINING_REPLICATES
    winner
  }))
  list(losses = losses, selected = selected)
}

mafw_metric_rows <- function(discrepancies, source, fold, replicate) {
  data.frame(
    source = source,
    fold = as.integer(fold),
    replicate = as.integer(replicate),
    family = discrepancies$family,
    metric = discrepancies$metric,
    empirical_median = discrepancies$empirical_median,
    generated_median = discrepancies$generated_median,
    signed_median_difference_scaled = discrepancies$signed_median_difference_scaled,
    wasserstein_scaled = discrepancies$wasserstein1_scaled,
    discrepancy = discrepancies$discrepancy,
    stringsAsFactors = FALSE
  )
}

mafw_complete_case_clip <- function(view, clip) {
  output <- view[view$clip == clip, , drop = FALSE]
  output[order(output$frame_index), , drop = FALSE]
}

mafw_select_example_clip <- function(prepared) {
  probability <- prepared$input_tables[["probability_view.csv"]]
  probability <- probability[probability$analysis_variant == "complete_case", , drop = FALSE]
  clip_rows <- split(seq_len(nrow(probability)), probability$clip)
  eligible <- names(clip_rows)[vapply(clip_rows, function(index) {
    nrow(probability[index, , drop = FALSE]) >= 30L &&
      length(unique(probability$segment_id[index])) == 1L
  }, logical(1))]
  contributions <- lomm11_runner_empirical_contributions(
    prepared$input_tables, "complete_case", unique(as.character(probability$clip))
  )
  summaries <- aggregate(
    value ~ clip + family + metric, data = contributions, FUN = stats::median
  )
  summaries$distance <- NA_real_
  groups <- split(seq_len(nrow(summaries)), paste(summaries$family, summaries$metric))
  for (index in groups) {
    values <- summaries$value[index]
    scale <- lomm11_metric_scale(values)$scale
    summaries$distance[index] <- abs(values - stats::median(values)) / scale
  }
  distance <- aggregate(distance ~ clip, data = summaries, FUN = mean)
  distance <- distance[distance$clip %in% eligible, , drop = FALSE]
  distance <- distance[order(distance$distance, distance$clip), , drop = FALSE]
  if (nrow(distance) == 0L) mafw_stop("No eligible representative clip was found.")
  as.character(distance$clip[[1L]])
}

mafw_heldout_validation <- function(prepared, caches, selected, example_clip) {
  heldout_plan <- prepared$context$plan$heldout_plan.csv
  heldout_plan <- heldout_plan[
    heldout_plan$variant_id == MAFW_PROFILE_ID, , drop = FALSE
  ]
  metric_rows <- vector(
    "list", 2L * MAFW_FOLD_COUNT * MAFW_HELDOUT_REPLICATES
  )
  example_draws <- vector("list", MAFW_HELDOUT_REPLICATES)
  example_distances <- rep(NA_real_, MAFW_HELDOUT_REPLICATES)
  cursor <- 0L
  for (fold in seq_len(MAFW_FOLD_COUNT)) {
    cache <- caches[[as.character(fold)]]
    setting <- selected[selected$fold == fold, , drop = FALSE]
    candidate <- setting
    plans <- heldout_plan[heldout_plan$fold == fold, , drop = FALSE]
    plans <- plans[order(plans$heldout_rep), , drop = FALSE]
    if (nrow(plans) != MAFW_HELDOUT_REPLICATES) {
      mafw_stop("Held-out plan does not contain 100 profile replicates.")
    }
    message(
      "Held-out LOMM and static comparison: fold ", fold, " of ",
      MAFW_FOLD_COUNT
    )
    for (replicate in seq_len(nrow(plans))) {
      plan_row <- plans[replicate, , drop = FALSE]
      generated_lomm <- mafw_generate_dynamic(
        cache$heldout_generation_plan, cache$geometry, cache$basis, candidate,
        as.integer(plan_row$seed_base[[1L]]),
        as.character(plan_row$seed_group[[1L]])
      )
      generated_static <- mafw_generate_static(
        cache$heldout_generation_plan, cache$geometry, cache$basis, candidate,
        as.integer(plan_row$seed_base[[1L]]),
        as.character(plan_row$seed_group[[1L]])
      )
      lomm_evaluation <- mafw_evaluate(
        generated_lomm, cache$heldout_view_index, cache$empirical_heldout,
        cache$scales
      )
      static_evaluation <- mafw_evaluate(
        generated_static, cache$heldout_view_index, cache$empirical_heldout,
        cache$scales
      )
      cursor <- cursor + 1L
      metric_rows[[cursor]] <- mafw_metric_rows(
        lomm_evaluation$discrepancies, "LOMM", fold, replicate
      )
      cursor <- cursor + 1L
      metric_rows[[cursor]] <- mafw_metric_rows(
        static_evaluation$discrepancies, "Static logistic-normal", fold, replicate
      )

      if (example_clip %in% cache$heldout_clips) {
        empirical_clip <- cache$empirical_heldout[
          cache$empirical_heldout$clip == example_clip, , drop = FALSE
        ]
        generated_clip_view <- mafw_complete_case_clip(
          lomm_evaluation$view, example_clip
        )
        generated_clip <- lomm11_metric_contributions(
          generated_clip_view, generated_clip_view, generated_clip_view
        )
        clip_discrepancy <- lomm11_metric_discrepancies(
          empirical_clip, generated_clip, cache$scales
        )
        example_draws[[replicate]] <- generated_clip_view
        example_distances[[replicate]] <- mean(clip_discrepancy$discrepancy)
      }
    }
  }
  if (any(!is.finite(example_distances)) || any(vapply(example_draws, is.null, logical(1)))) {
    mafw_stop("Representative clip draws were not retained for all 100 replicates.")
  }
  ordering <- order(example_distances, seq_along(example_distances))
  median_rank <- ceiling(length(ordering) / 2)
  selected_rep <- ordering[[median_rank]]
  list(
    metrics = do.call(rbind, metric_rows),
    example = list(
      clip = example_clip,
      replicate = selected_rep,
      distance = example_distances[[selected_rep]],
      rank = median_rank,
      distances = example_distances,
      generated = example_draws[[selected_rep]]
    )
  )
}

mafw_with_seed <- function(seed, callback) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  callback()
}

mafw_stratified_sample <- function(training_assignments, target_counts, seed,
                                   exclude = character()) {
  mafw_with_seed(seed, function() {
    sampled <- unlist(lapply(names(target_counts), function(label) {
      candidates <- as.character(
        training_assignments$clip[training_assignments$truth_label == label]
      )
      candidates <- setdiff(candidates, as.character(exclude))
      size <- as.integer(target_counts[[label]])
      if (length(candidates) < size) {
        mafw_stop("Training fold has too few clips for label ", label, ".")
      }
      sample(candidates, size = size, replace = FALSE)
    }), use.names = FALSE)
    as.character(sampled)
  })
}

mafw_real_real_reference <- function(prepared, caches) {
  assignments <- prepared$context$plan$fold_assignments.csv
  rows <- vector("list", MAFW_FOLD_COUNT * MAFW_REFERENCE_REPLICATES)
  cursor <- 0L
  for (fold in seq_len(MAFW_FOLD_COUNT)) {
    message(
      "Real-versus-real reference: fold ", fold, " of ", MAFW_FOLD_COUNT
    )
    cache <- caches[[as.character(fold)]]
    training_assignments <- assignments[assignments$fold != fold, , drop = FALSE]
    heldout_assignments <- assignments[assignments$fold == fold, , drop = FALSE]
    target_counts <- table(heldout_assignments$truth_label)
    for (replicate in seq_len(MAFW_REFERENCE_REPLICATES)) {
      seed_a <- MAFW_REFERENCE_SEED + fold * 200000L + replicate * 2L
      seed_b <- seed_a + 1L
      clips_a <- mafw_stratified_sample(training_assignments, target_counts, seed_a)
      clips_b <- mafw_stratified_sample(
        training_assignments, target_counts, seed_b, exclude = clips_a
      )
      if (length(clips_a) != MAFW_HELDOUT_CLIPS_PER_FOLD ||
          length(clips_b) != MAFW_HELDOUT_CLIPS_PER_FOLD ||
          length(intersect(clips_a, clips_b)) != 0L) {
        mafw_stop("Real-versus-real samples do not satisfy the disjoint design.")
      }
      first <- cache$empirical_training[
        cache$empirical_training$clip %in% clips_a, , drop = FALSE
      ]
      second <- cache$empirical_training[
        cache$empirical_training$clip %in% clips_b, , drop = FALSE
      ]
      discrepancies <- lomm11_metric_discrepancies(first, second, cache$scales)
      cursor <- cursor + 1L
      rows[[cursor]] <- mafw_metric_rows(
        discrepancies, "MAFW split reference", fold, replicate
      )
    }
  }
  do.call(rbind, rows)
}

mafw_aggregate_across_folds <- function(metric_results) {
  aggregate(
    cbind(
      empirical_median, generated_median, signed_median_difference_scaled,
      wasserstein_scaled, discrepancy
    ) ~ source + replicate + family + metric,
    data = metric_results, FUN = mean
  )
}

mafw_summarize_metrics <- function(metric_results) {
  aggregated <- mafw_aggregate_across_folds(metric_results)
  groups <- split(
    seq_len(nrow(aggregated)), paste(aggregated$family, aggregated$metric)
  )
  rows <- lapply(groups, function(index) {
    group <- aggregated[index, , drop = FALSE]
    lomm <- group[group$source == "LOMM", , drop = FALSE]
    static <- group[group$source == "Static logistic-normal", , drop = FALSE]
    reference <- group[group$source == "MAFW split reference", , drop = FALSE]
    lomm_median <- stats::median(lomm$discrepancy)
    reference_quantiles <- stats::quantile(
      reference$discrepancy, c(0.025, 0.5, 0.975), names = FALSE
    )
    data.frame(
      family = as.character(group$family[[1L]]),
      metric = as.character(group$metric[[1L]]),
      lomm_discrepancy = lomm_median,
      lomm_signed_difference = stats::median(
        lomm$signed_median_difference_scaled
      ),
      static_discrepancy = stats::median(static$discrepancy),
      static_signed_difference = stats::median(
        static$signed_median_difference_scaled
      ),
      reference_lower = reference_quantiles[[1L]],
      reference_median = reference_quantiles[[2L]],
      reference_upper = reference_quantiles[[3L]],
      reference_percentile = mean(reference$discrepancy <= lomm_median),
      within_reference_upper = lomm_median <= reference_quantiles[[3L]],
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output <- output[order(
    match(output$metric, names(MAFW_METRIC_LABELS)),
    match(output$family, c("probability", "logratio"))
  ), , drop = FALSE]
  rownames(output) <- NULL
  output
}

mafw_composite_results <- function(metric_results) {
  fold_composite <- aggregate(
    discrepancy ~ source + fold + replicate, data = metric_results, FUN = mean
  )
  aggregate(
    discrepancy ~ source + replicate, data = fold_composite, FUN = mean
  )
}

mafw_summarize_composite <- function(composite) {
  lomm <- composite$discrepancy[composite$source == "LOMM"]
  static <- composite$discrepancy[
    composite$source == "Static logistic-normal"
  ]
  reference <- composite$discrepancy[
    composite$source == "MAFW split reference"
  ]
  reference_quantiles <- stats::quantile(
    reference, c(0.025, 0.5, 0.975), names = FALSE
  )
  lomm_median <- stats::median(lomm)
  data.frame(
    lomm_median = lomm_median,
    lomm_q025 = stats::quantile(lomm, 0.025, names = FALSE),
    lomm_q975 = stats::quantile(lomm, 0.975, names = FALSE),
    static_median = stats::median(static),
    static_q025 = stats::quantile(static, 0.025, names = FALSE),
    static_q975 = stats::quantile(static, 0.975, names = FALSE),
    reference_lower = reference_quantiles[[1L]],
    reference_median = reference_quantiles[[2L]],
    reference_upper = reference_quantiles[[3L]],
    reference_percentile = mean(reference <= lomm_median),
    within_reference_upper = lomm_median <= reference_quantiles[[3L]],
    stringsAsFactors = FALSE
  )
}

mafw_metric_table <- function(summary) {
  rows <- lapply(names(MAFW_METRIC_LABELS), function(metric) {
    probability <- summary[
      summary$family == "probability" & summary$metric == metric, , drop = FALSE
    ]
    logratio <- summary[
      summary$family == "logratio" & summary$metric == metric, , drop = FALSE
    ]
    if (nrow(probability) != 1L || nrow(logratio) != 1L) {
      mafw_stop("Metric summary is incomplete for ", metric, ".")
    }
    data.frame(
      metric = unname(MAFW_METRIC_LABELS[[metric]]),
      lomm_scores = probability$lomm_discrepancy,
      real_real_upper_scores = probability$reference_upper,
      static_scores = probability$static_discrepancy,
      lomm_logratio = logratio$lomm_discrepancy,
      real_real_upper_logratio = logratio$reference_upper,
      static_logratio = logratio$static_discrepancy,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mafw_deviation_table <- function(summary) {
  output <- summary
  output$metric_label <- unname(MAFW_METRIC_LABELS[output$metric])
  output$representation <- ifelse(
    output$family == "probability", "Scores", "Log-ratio coordinates"
  )
  output$excess_above_reference <- output$lomm_discrepancy - output$reference_upper
  output$median_direction <- ifelse(
    output$lomm_signed_difference > 0,
    "LOMM higher than MAFW",
    ifelse(output$lomm_signed_difference < 0, "LOMM lower than MAFW", "No median shift")
  )
  output <- output[order(-output$excess_above_reference, -output$lomm_discrepancy), ]
  output[, c(
    "representation", "metric_label", "lomm_discrepancy",
    "reference_upper", "excess_above_reference", "lomm_signed_difference",
    "median_direction", "within_reference_upper"
  )]
}

mafw_example_data <- function(prepared, example) {
  empirical <- prepared$input_tables[["probability_view.csv"]]
  empirical <- empirical[
    empirical$analysis_variant == "complete_case" &
      empirical$clip == example$clip, , drop = FALSE
  ]
  empirical <- empirical[order(empirical$frame_index), , drop = FALSE]
  generated <- example$generated[order(example$generated$frame_index), , drop = FALSE]
  channels <- rownames(prepared$basis)
  if (!identical(empirical$frame_key, generated$frame_key)) {
    mafw_stop("Representative empirical and generated supports differ.")
  }
  sources <- list("MAFW classifier scores" = empirical, "LOMM realization" = generated)
  rows <- lapply(names(sources), function(source) {
    frame <- sources[[source]]
    do.call(rbind, lapply(channels, function(channel) {
      data.frame(
        source = source,
        clip = example$clip,
        replicate = if (source == "LOMM realization") example$replicate else NA_integer_,
        frame_index = as.integer(frame$frame_index),
        channel = channel,
        score = as.numeric(frame[[channel]]),
        stringsAsFactors = FALSE
      )
    }))
  })
  output <- do.call(rbind, rows)
  empirical_means <- colMeans(empirical[, channels, drop = FALSE])
  highlighted <- names(sort(empirical_means, decreasing = TRUE))[seq_len(4L)]
  output$highlighted <- output$channel %in% highlighted
  output$source <- factor(
    output$source, levels = c("MAFW classifier scores", "LOMM realization")
  )
  output$channel <- factor(output$channel, levels = channels)
  list(data = output, highlighted = highlighted, n_observations = nrow(empirical))
}

mafw_running_mean <- function(values, window = 5L) {
  half_width <- floor(window / 2L)
  vapply(seq_along(values), function(index) {
    mean(values[max(1L, index - half_width):min(length(values), index + half_width)])
  }, numeric(1))
}

mafw_example_plot <- function(example_data) {
  data <- example_data$data
  data$smoothed_score <- ave(
    data$score, interaction(data$source, data$channel, drop = TRUE),
    FUN = mafw_running_mean
  )
  highlighted <- example_data$highlighted
  colors <- stats::setNames(
    c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), highlighted
  )
  linetypes <- stats::setNames(
    c("solid", "longdash", "dashed", "dotdash"), highlighted
  )
  other <- data[!data$highlighted, , drop = FALSE]
  focus <- data[data$highlighted, , drop = FALSE]
  x_limits <- range(data$frame_index)
  x_breaks <- unique(round(seq(x_limits[[1L]], x_limits[[2L]], length.out = 5L)))
  upper <- ceiling(max(data$score) * 10) / 10

  ggplot() +
    geom_line(
      data = other,
      aes(frame_index, score, group = interaction(source, channel)),
      color = "#A7ADB4", linewidth = 0.32, alpha = 0.25
    ) +
    geom_line(
      data = focus,
      aes(frame_index, score, color = channel, linetype = channel),
      linewidth = 0.32, alpha = 0.28, show.legend = FALSE
    ) +
    geom_line(
      data = focus,
      aes(frame_index, smoothed_score, color = channel, linetype = channel),
      linewidth = 0.92
    ) +
    facet_wrap(~source, ncol = 1L) +
    scale_color_manual(
      values = colors, breaks = highlighted,
      labels = unname(MAFW_EMOTION_LABELS[highlighted]), name = NULL
    ) +
    scale_linetype_manual(
      values = linetypes, breaks = highlighted,
      labels = unname(MAFW_EMOTION_LABELS[highlighted]), name = NULL
    ) +
    scale_x_continuous(
      breaks = x_breaks, limits = x_limits, expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      limits = c(0, upper), breaks = seq(0, upper, by = 0.1),
      expand = expansion(mult = c(0, 0.015))
    ) +
    labs(x = "Sampled observation", y = "Classifier-output score") +
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
      legend.key.width = grid::unit(14, "pt"),
      legend.margin = margin(0, 0, 1, 0),
      axis.text = element_text(size = 7.4),
      axis.title = element_text(size = 8.6),
      axis.line = element_line(linewidth = 0.28),
      axis.ticks = element_line(linewidth = 0.28),
      strip.background = element_rect(fill = "#F1F4F6", color = NA),
      strip.text = element_text(size = 8.4, face = "bold", hjust = 0),
      panel.spacing.y = grid::unit(0.55, "lines"),
      panel.grid = element_blank(),
      plot.margin = margin(5, 7, 5, 7)
    )
}

mafw_geometry_table <- function(caches, selected) {
  do.call(rbind, lapply(caches, function(cache) {
    geometry <- cache$geometry
    setting <- selected[selected$fold == cache$fold, , drop = FALSE]
    if (nrow(setting) != 1L) mafw_stop("Fold geometry has no selected setting.")
    data.frame(
      fold = cache$fold,
      training_clips = length(cache$training_clips),
      training_observations = geometry$n_training_rows,
      scale_anchor = geometry$scale_anchor,
      clip_scale_min = min(geometry$empirical_clip_scales),
      clip_scale_median = stats::median(geometry$empirical_clip_scales),
      clip_scale_max = max(geometry$empirical_clip_scales),
      residual_sd = setting$residual_sd[[1L]],
      variance_first_three = sum(geometry$direction_eigenvalues) /
        sum(eigen(geometry$within_clip_covariance, symmetric = TRUE,
                  only.values = TRUE)$values),
      stringsAsFactors = FALSE
    )
  }))
}

mafw_caption <- function(example_data, example) {
  paste0(
    "Classifier-output score series for an empirically central MAFW clip and a ",
    "LOMM realization on the same ", example_data$n_observations,
    " sampled observations. The clip was selected from the empirical data before ",
    "simulation as the eligible clip closest to the center of the ",
    2L * length(LOMM11_METRICS), " prespecified ",
    "summary-metric distributions. The realization is the median-ranked, rather ",
    "than closest, of 100 held-out draws by mean standardized summary discrepancy ",
    "(replicate ", example$replicate, "; discrepancy = ",
    sprintf("%.3f", example$distance), "). Thin lines show raw scores and thick ",
    "lines show five-observation moving means for the four channels with the ",
    "highest empirical means. The other seven channels are gray. The simulation ",
    "matches observation indices and uninterrupted segment structure; it is not a ",
    "frame-level reconstruction."
  )
}

mafw_write_outputs <- function(prepared, caches, selection, validation,
                               reference_metrics) {
  paths <- prepared$paths
  metric_results <- rbind(validation$metrics, reference_metrics)
  metric_summary <- mafw_summarize_metrics(metric_results)
  composite <- mafw_composite_results(metric_results)
  composite_summary <- mafw_summarize_composite(composite)
  metric_table <- mafw_metric_table(metric_summary)
  deviations <- mafw_deviation_table(metric_summary)
  example_data <- mafw_example_data(prepared, validation$example)
  plot <- mafw_example_plot(example_data)
  geometry <- mafw_geometry_table(caches, selection$selected)

  mafw_atomic_csv(selection$losses, paths$training_losses)
  mafw_atomic_csv(selection$selected, paths$selected_settings)
  mafw_atomic_csv(geometry, paths$geometry)
  mafw_atomic_csv(metric_results, paths$metric_results)
  mafw_atomic_csv(composite, paths$composite_results)
  mafw_atomic_csv(example_data$data, paths$example_series)
  mafw_atomic_csv(metric_table, paths$table_metric)
  mafw_atomic_csv(composite_summary, paths$table_composite)
  mafw_atomic_csv(deviations, paths$table_deviations)
  mafw_atomic_csv(selection$selected, paths$table_settings)
  mafw_atomic_csv(example_data$data, paths$table_example)
  mafw_atomic_text(mafw_caption(example_data, validation$example), paths$caption)

  dir.create(dirname(paths$figure_pdf), recursive = TRUE, showWarnings = FALSE)
  ggsave(paths$figure_pdf, plot, width = 7.1, height = 4.45, device = cairo_pdf)
  ggsave(paths$figure_png, plot, width = 7.1, height = 4.45, dpi = 360, bg = "white")

  output_files <- c(
    paths$training_losses, paths$selected_settings, paths$geometry,
    paths$metric_results, paths$composite_results, paths$example_series,
    paths$table_metric, paths$table_composite, paths$table_deviations,
    paths$table_settings, paths$table_example, paths$caption,
    paths$figure_pdf, paths$figure_png
  )
  success <- list(
    schema_version = 1L,
    status = "complete",
    completed_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    canonical_profile = list(
      variant_id = MAFW_PROFILE_ID,
      clip_baseline = TRUE,
      clip_scale = TRUE,
      activity_transition = FALSE,
      selected_before_heldout = TRUE
    ),
    calibration = list(
      version = 2L,
      dynamics_selection_metrics = MAFW_TEMPORAL_SELECTION_METRICS,
      measurement_selection_parameters = c("residual_sd", "kappa")
    ),
    folds = MAFW_FOLD_COUNT,
    clips_per_sample = MAFW_HELDOUT_CLIPS_PER_FOLD,
    real_real_groups_disjoint = TRUE,
    training_replicates = MAFW_TRAINING_REPLICATES,
    heldout_replicates = MAFW_HELDOUT_REPLICATES,
    real_real_reference_replicates = MAFW_REFERENCE_REPLICATES,
    example = list(
      clip = validation$example$clip,
      heldout_replicate = validation$example$replicate,
      median_rank = validation$example$rank,
      mean_metric_discrepancy = validation$example$distance
    ),
    outputs = as.list(stats::setNames(
      vapply(output_files, lomm11_runner_sha256, character(1)),
      substring(output_files, nchar(MAFW_PLAUSIBILITY_ROOT) + 2L)
    ))
  )
  lomm11_runner_atomic_json(success, paths$success)
  list(
    metric_summary = metric_summary,
    composite_summary = composite_summary,
    deviations = deviations,
    example = validation$example,
    paths = paths
  )
}

mafw_validate_outputs <- function(result) {
  paths <- result$paths
  required <- unlist(paths, use.names = FALSE)
  required <- required[!required %in% paths$runner]
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    mafw_stop("Expected outputs are missing: ", paste(missing, collapse = ", "))
  }
  expected_metrics <- length(LOMM11_METRIC_FAMILIES) * length(LOMM11_METRICS)
  if (nrow(result$metric_summary) != expected_metrics ||
      nrow(result$composite_summary) != 1L ||
      nrow(result$deviations) != expected_metrics) {
    mafw_stop("Summary output dimensions are invalid.")
  }
  if (any(!is.finite(unlist(result$composite_summary[1, 1:10])))) {
    mafw_stop("Composite summary contains non-finite values.")
  }
  invisible(TRUE)
}

mafw_main <- function() {
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  prepared <- mafw_prepare()
  caches <- mafw_build_caches(prepared)
  selection <- mafw_select_dynamic_settings(prepared, caches)
  example_clip <- mafw_select_example_clip(prepared)
  message("Empirically selected representative clip: ", example_clip)
  validation <- mafw_heldout_validation(
    prepared, caches, selection$selected, example_clip
  )
  reference <- mafw_real_real_reference(prepared, caches)
  result <- mafw_write_outputs(
    prepared, caches, selection, validation, reference
  )
  mafw_validate_outputs(result)
  print(result$composite_summary)
  print(utils::head(result$deviations, 8L))
  message("Published MAFW plausibility validation: ", MAFW_PLAUSIBILITY_OUTPUT)
  invisible(result)
}

if (sys.nframe() == 0L) mafw_main()
