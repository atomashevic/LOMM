#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

mafw_benchmark_script_path <- function() {
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[
    !is.na(frames) & basename(frames) == "build_mafw_clip_level_benchmark.R"
  ]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Could not locate benchmark script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

MAFW_BENCHMARK_SCRIPT_PATH <- mafw_benchmark_script_path()
source(file.path(
  dirname(MAFW_BENCHMARK_SCRIPT_PATH),
  "build_mafw_plausibility_validation_25v25_extended.R"
), local = .GlobalEnv)

MAFW_STANDARD_OUTPUT <- file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "mafw_plausibility_validation_25v25"
)
MAFW_EXTENDED_OUTPUT <- file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "mafw_plausibility_validation_25v25_extended"
)
MAFW_PLAUSIBILITY_OUTPUT <- file.path(
  MAFW_PLAUSIBILITY_ROOT, "simulation_outputs", "lomm_1_1",
  "mafw_clip_level_benchmark"
)
MAFW_NEIGHBOR_QUANTILE <- 0.95

mafw_benchmark_paths <- function() {
  list(
    metric_results = file.path(MAFW_PLAUSIBILITY_OUTPUT, "metric_results.csv"),
    metric_sets = file.path(MAFW_PLAUSIBILITY_OUTPUT, "metric_set_benchmark.csv"),
    per_metric = file.path(MAFW_PLAUSIBILITY_OUTPUT, "per_metric_benchmark.csv"),
    clip_distances = file.path(MAFW_PLAUSIBILITY_OUTPUT, "clip_distances.csv"),
    clip_replicates = file.path(MAFW_PLAUSIBILITY_OUTPUT, "clip_replicate_benchmark.csv"),
    clip_summary = file.path(MAFW_PLAUSIBILITY_OUTPUT, "clip_level_benchmark.csv"),
    thresholds = file.path(MAFW_PLAUSIBILITY_OUTPUT, "clip_distance_thresholds.csv"),
    figure_pdf = file.path(MAFW_PLAUSIBILITY_OUTPUT, "clip_level_benchmark.pdf"),
    figure_png = file.path(MAFW_PLAUSIBILITY_OUTPUT, "clip_level_benchmark.png"),
    report = file.path(MAFW_PLAUSIBILITY_OUTPUT, "benchmark_summary.txt"),
    success = file.path(MAFW_PLAUSIBILITY_OUTPUT, "_SUCCESS.json")
  )
}

mafw_clip_feature_matrix <- function(contributions, feature_ids = NULL) {
  values <- aggregate(
    value ~ clip + family + metric,
    data = contributions,
    FUN = function(x) {
      x <- as.numeric(x)
      x <- x[is.finite(x)]
      if (length(x) == 0L) NA_real_ else stats::median(x)
    },
    na.action = na.pass
  )
  values$feature <- paste(values$family, values$metric, sep = "::")
  if (is.null(feature_ids)) {
    feature_ids <- as.vector(outer(
      LOMM11_METRIC_FAMILIES, LOMM11_METRICS, paste, sep = "::"
    ))
  }
  clips <- sort(unique(as.character(contributions$clip)))
  output <- matrix(
    NA_real_, nrow = length(clips), ncol = length(feature_ids),
    dimnames = list(clips, feature_ids)
  )
  row_index <- match(as.character(values$clip), clips)
  column_index <- match(as.character(values$feature), feature_ids)
  if (anyNA(column_index) || anyDuplicated(paste(row_index, column_index))) {
    mafw_stop("Clip feature cells are incomplete or duplicated.")
  }
  output[cbind(row_index, column_index)] <- values$value
  output
}

mafw_fit_clip_feature_transform <- function(contributions) {
  raw <- mafw_clip_feature_matrix(contributions)
  center <- apply(raw, 2L, function(x) stats::median(x[is.finite(x)]))
  scale <- apply(raw, 2L, function(x) lomm11_metric_scale(x)$scale)
  if (any(!is.finite(center)) || any(!is.finite(scale)) || any(scale <= 0)) {
    mafw_stop("Training-only clip feature transform is invalid.")
  }
  list(
    feature_ids = colnames(raw), center = center, scale = scale,
    training_clips = rownames(raw)
  )
}

mafw_transform_clip_features <- function(contributions, transform) {
  raw <- mafw_clip_feature_matrix(contributions, transform$feature_ids)
  for (column in seq_len(ncol(raw))) {
    missing <- !is.finite(raw[, column])
    raw[missing, column] <- transform$center[[column]]
  }
  output <- sweep(raw, 2L, transform$center, "-")
  output <- sweep(output, 2L, transform$scale, "/")
  if (any(!is.finite(output))) mafw_stop("Standardized clip features are not finite.")
  output
}

mafw_rms_distance <- function(first, second) {
  first <- as.matrix(first)
  second <- as.matrix(second)
  if (ncol(first) != ncol(second) || ncol(first) == 0L) {
    mafw_stop("Clip feature matrices do not share a nonempty feature space.")
  }
  first_norm <- rowSums(first^2)
  second_norm <- rowSums(second^2)
  squared <- outer(first_norm, second_norm, "+") - 2 * tcrossprod(first, second)
  sqrt(pmax(squared, 0) / ncol(first))
}

mafw_directional_distances <- function(first, second) {
  distances <- mafw_rms_distance(first, second)
  list(
    first_to_second = apply(distances, 1L, min),
    second_to_first = apply(distances, 2L, min)
  )
}

mafw_benchmark_settings <- function() {
  path <- file.path(MAFW_STANDARD_OUTPUT, "selected_settings.csv")
  success <- file.path(MAFW_STANDARD_OUTPUT, "_SUCCESS.json")
  if (!file.exists(path) || !file.exists(success)) {
    mafw_stop("Completed standard 25v25 settings are unavailable.")
  }
  settings <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (nrow(settings) != MAFW_FOLD_COUNT ||
      !identical(sort(as.integer(settings$fold)), seq_len(MAFW_FOLD_COUNT)) ||
      any(settings$period_frames != 12L) ||
      any(abs(settings$damping_ratio - 0.20) > 1e-12) ||
      any(abs(settings$residual_sd - 0.30) > 1e-12) ||
      any(abs(settings$kappa - 0.75) > 1e-12) ||
      !all(settings$selected_before_heldout)) {
    mafw_stop("Standard 25v25 settings do not match the frozen benchmark profile.")
  }
  settings
}

mafw_reference_clip_distances <- function(prepared, caches, transforms) {
  assignments <- prepared$context$plan$fold_assignments.csv
  rows <- vector("list", MAFW_FOLD_COUNT * MAFW_REFERENCE_REPLICATES * 2L)
  cursor <- 0L
  for (fold in seq_len(MAFW_FOLD_COUNT)) {
    message("Clip-level real reference: fold ", fold, " of ", MAFW_FOLD_COUNT)
    training_assignments <- assignments[assignments$fold != fold, , drop = FALSE]
    heldout_assignments <- assignments[assignments$fold == fold, , drop = FALSE]
    target_counts <- table(heldout_assignments$truth_label)
    training <- mafw_transform_clip_features(
      caches[[as.character(fold)]]$empirical_training,
      transforms[[as.character(fold)]]
    )
    for (replicate in seq_len(MAFW_REFERENCE_REPLICATES)) {
      seed_a <- MAFW_REFERENCE_SEED + fold * 200000L + replicate * 2L
      seed_b <- seed_a + 1L
      clips_a <- mafw_stratified_sample(training_assignments, target_counts, seed_a)
      clips_b <- mafw_stratified_sample(
        training_assignments, target_counts, seed_b, exclude = clips_a
      )
      distances <- mafw_directional_distances(
        training[clips_a, , drop = FALSE], training[clips_b, , drop = FALSE]
      )
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        source = "MAFW reference", fold = fold, replicate = replicate,
        direction = "plausibility", clip = clips_a,
        distance = distances$first_to_second, stringsAsFactors = FALSE
      )
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        source = "MAFW reference", fold = fold, replicate = replicate,
        direction = "coverage", clip = clips_b,
        distance = distances$second_to_first, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

mafw_fold_thresholds <- function(reference_distances) {
  rows <- lapply(seq_len(MAFW_FOLD_COUNT), function(fold) {
    selected <- reference_distances$distance[reference_distances$fold == fold]
    data.frame(
      fold = fold,
      quantile = MAFW_NEIGHBOR_QUANTILE,
      threshold = stats::quantile(
        selected, MAFW_NEIGHBOR_QUANTILE, names = FALSE
      ),
      reference_distances = length(selected),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mafw_generated_benchmark <- function(prepared, caches, transforms, settings,
                                     thresholds) {
  heldout_plan <- prepared$context$plan$heldout_plan.csv
  heldout_plan <- heldout_plan[
    as.character(heldout_plan$variant_id) == MAFW_PROFILE_ID, , drop = FALSE
  ]
  metric_rows <- vector(
    "list", 2L * MAFW_FOLD_COUNT * MAFW_HELDOUT_REPLICATES
  )
  distance_rows <- vector(
    "list", 4L * MAFW_FOLD_COUNT * MAFW_HELDOUT_REPLICATES
  )
  metric_cursor <- 0L
  distance_cursor <- 0L
  for (fold in seq_len(MAFW_FOLD_COUNT)) {
    message("Fixed-setting held-out benchmark: fold ", fold, " of ", MAFW_FOLD_COUNT)
    cache <- caches[[as.character(fold)]]
    transform <- transforms[[as.character(fold)]]
    setting <- settings[settings$fold == fold, , drop = FALSE]
    plans <- heldout_plan[heldout_plan$fold == fold, , drop = FALSE]
    plans <- plans[order(plans$heldout_rep), , drop = FALSE]
    empirical <- mafw_transform_clip_features(cache$empirical_heldout, transform)
    threshold <- thresholds$threshold[thresholds$fold == fold]
    if (nrow(plans) != MAFW_HELDOUT_REPLICATES || length(threshold) != 1L) {
      mafw_stop("Held-out plan or clip-distance threshold is incomplete.")
    }
    for (replicate in seq_len(nrow(plans))) {
      plan <- plans[replicate, , drop = FALSE]
      generated <- list(
        LOMM = mafw_generate_dynamic(
          cache$heldout_generation_plan, cache$geometry, cache$basis, setting,
          as.integer(plan$seed_base[[1L]]), as.character(plan$seed_group[[1L]])
        ),
        `Static logistic-normal` = mafw_generate_static(
          cache$heldout_generation_plan, cache$geometry, cache$basis, setting,
          as.integer(plan$seed_base[[1L]]), as.character(plan$seed_group[[1L]])
        )
      )
      for (source in names(generated)) {
        evaluation <- mafw_evaluate(
          generated[[source]], cache$heldout_view_index,
          cache$empirical_heldout, cache$scales
        )
        metric_cursor <- metric_cursor + 1L
        metric_rows[[metric_cursor]] <- mafw_metric_rows(
          evaluation$discrepancies, source, fold, replicate
        )
        synthetic <- mafw_transform_clip_features(
          evaluation$contributions, transform
        )
        distances <- mafw_directional_distances(synthetic, empirical)
        distance_cursor <- distance_cursor + 1L
        distance_rows[[distance_cursor]] <- data.frame(
          source = source, fold = fold, replicate = replicate,
          direction = "plausibility", clip = rownames(synthetic),
          distance = distances$first_to_second, stringsAsFactors = FALSE
        )
        distance_cursor <- distance_cursor + 1L
        distance_rows[[distance_cursor]] <- data.frame(
          source = source, fold = fold, replicate = replicate,
          direction = "coverage", clip = rownames(empirical),
          distance = distances$second_to_first, stringsAsFactors = FALSE
        )
      }
    }
  }
  list(
    metrics = do.call(rbind, metric_rows),
    distances = do.call(rbind, distance_rows)
  )
}

mafw_clip_replicate_summary <- function(distances, thresholds) {
  distances$threshold <- thresholds$threshold[match(distances$fold, thresholds$fold)]
  distances$within_reference <- distances$distance <= distances$threshold
  groups <- split(
    seq_len(nrow(distances)),
    paste(distances$source, distances$fold, distances$replicate,
          distances$direction, sep = "\r")
  )
  directional <- do.call(rbind, lapply(groups, function(index) {
    frame <- distances[index, , drop = FALSE]
    data.frame(
      source = as.character(frame$source[[1L]]),
      fold = as.integer(frame$fold[[1L]]),
      replicate = as.integer(frame$replicate[[1L]]),
      direction = as.character(frame$direction[[1L]]),
      proportion = mean(frame$within_reference),
      median_distance = stats::median(frame$distance),
      stringsAsFactors = FALSE
    )
  }))
  plausibility <- directional[directional$direction == "plausibility", ]
  coverage <- directional[directional$direction == "coverage", ]
  names(plausibility)[names(plausibility) == "proportion"] <- "plausibility"
  names(plausibility)[names(plausibility) == "median_distance"] <-
    "median_plausibility_distance"
  names(coverage)[names(coverage) == "proportion"] <- "coverage"
  names(coverage)[names(coverage) == "median_distance"] <-
    "median_coverage_distance"
  output <- merge(
    plausibility[, c(
      "source", "fold", "replicate", "plausibility",
      "median_plausibility_distance"
    )],
    coverage[, c(
      "source", "fold", "replicate", "coverage", "median_coverage_distance"
    )],
    by = c("source", "fold", "replicate"), sort = FALSE
  )
  output[order(output$source, output$fold, output$replicate), ]
}

mafw_clip_summary <- function(replicates) {
  across_folds <- aggregate(
    cbind(
      plausibility, coverage, median_plausibility_distance,
      median_coverage_distance
    ) ~ source + replicate,
    data = replicates, FUN = mean
  )
  rows <- lapply(sort(unique(across_folds$source)), function(source) {
    frame <- across_folds[across_folds$source == source, , drop = FALSE]
    summarize <- function(field) {
      stats::quantile(frame[[field]], c(0.025, 0.5, 0.975), names = FALSE)
    }
    plausibility <- summarize("plausibility")
    coverage <- summarize("coverage")
    precision_distance <- summarize("median_plausibility_distance")
    coverage_distance <- summarize("median_coverage_distance")
    data.frame(
      source = source,
      replicates = nrow(frame),
      plausibility_q025 = plausibility[[1L]],
      plausibility_median = plausibility[[2L]],
      plausibility_q975 = plausibility[[3L]],
      coverage_q025 = coverage[[1L]],
      coverage_median = coverage[[2L]],
      coverage_q975 = coverage[[3L]],
      median_plausibility_distance = precision_distance[[2L]],
      median_coverage_distance = coverage_distance[[2L]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mafw_metric_set_definitions <- function() {
  list(
    `Full 11 metrics` = LOMM11_METRICS,
    `Exclude spectral entropy` = setdiff(LOMM11_METRICS, "spectral_entropy"),
    `Time-domain expanded` = c(
      "level_median", "level_iqr", "difference_iqr", "lag1_correlation",
      "run_length_entropy", "sample_entropy", "permutation_entropy"
    ),
    `Local temporal expanded` = c(
      "difference_iqr", "lag1_correlation", "run_length_entropy",
      "sample_entropy", "permutation_entropy"
    ),
    `Original time-domain core` = c(
      "level_median", "level_iqr", "difference_iqr", "lag1_correlation",
      "run_length_entropy"
    ),
    `Original local temporal` = c(
      "difference_iqr", "lag1_correlation", "run_length_entropy"
    ),
    `Entropy family` = c(
      "run_length_entropy", "sample_entropy", "permutation_entropy",
      "spectral_entropy"
    ),
    `New entropy pair` = c("sample_entropy", "permutation_entropy"),
    `Robust variance pair` = c(
      "robust_level_variance", "robust_change_variance"
    ),
    `Structure and robust variance` = c(
      "effective_rank", "robust_level_variance", "robust_change_variance"
    ),
    `Local persistence` = c("lag1_correlation", "run_length_entropy")
  )
}

mafw_metric_set_table <- function(metric_results) {
  definitions <- mafw_metric_set_definitions()
  rows <- lapply(names(definitions), function(label) {
    metrics <- definitions[[label]]
    selected <- metric_results[metric_results$metric %in% metrics, , drop = FALSE]
    fold_composite <- aggregate(
      discrepancy ~ source + fold + replicate, selected, mean
    )
    composite <- aggregate(
      discrepancy ~ source + replicate, fold_composite, mean
    )
    values <- function(source) {
      stats::quantile(
        composite$discrepancy[composite$source == source],
        c(0.025, 0.5, 0.975), names = FALSE
      )
    }
    lomm <- values("LOMM")
    reference <- values("MAFW split reference")
    static <- values("Static logistic-normal")
    data.frame(
      metric_set = label,
      metrics = length(metrics),
      cells = 2L * length(metrics),
      lomm_q025 = lomm[[1L]], lomm_median = lomm[[2L]],
      lomm_q975 = lomm[[3L]],
      reference_q025 = reference[[1L]],
      reference_median = reference[[2L]],
      reference_q975 = reference[[3L]],
      static_median = static[[2L]],
      reference_percentile = mean(
        composite$discrepancy[composite$source == "MAFW split reference"] <=
          lomm[[2L]]
      ),
      within_reference = lomm[[2L]] <= reference[[3L]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mafw_per_metric_table <- function(metric_results) {
  summary <- mafw_summarize_metrics(metric_results)
  summary$metric_label <- unname(MAFW_METRIC_LABELS[summary$metric])
  summary$representation <- ifelse(
    summary$family == "probability", "Scores", "Log-ratio coordinates"
  )
  summary$reference_width <- summary$reference_upper - summary$reference_lower
  summary[, c(
    "representation", "metric", "metric_label", "lomm_discrepancy",
    "lomm_signed_difference", "static_discrepancy", "reference_lower",
    "reference_median", "reference_upper", "reference_width",
    "reference_percentile", "within_reference_upper"
  )]
}

mafw_clip_plot <- function(summary) {
  rows <- rbind(
    data.frame(
      source = summary$source, measure = "Plausibility",
      lower = summary$plausibility_q025, estimate = summary$plausibility_median,
      upper = summary$plausibility_q975
    ),
    data.frame(
      source = summary$source, measure = "Coverage",
      lower = summary$coverage_q025, estimate = summary$coverage_median,
      upper = summary$coverage_q975
    )
  )
  rows$source <- factor(
    rows$source,
    levels = c("MAFW reference", "LOMM", "Static logistic-normal")
  )
  colors <- c(
    `MAFW reference` = "#5B6573", LOMM = "#0072B2",
    `Static logistic-normal` = "#D55E00"
  )
  ggplot(rows, aes(estimate, source, color = source)) +
    geom_errorbar(
      aes(xmin = lower, xmax = upper), orientation = "y",
      width = 0.16, linewidth = 0.55
    ) +
    geom_point(size = 2.2) +
    facet_wrap(~measure, nrow = 1L) +
    scale_color_manual(values = colors, guide = "none") +
    scale_x_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.25),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(x = "Proportion within the training-calibrated real reference", y = NULL) +
    theme_classic(base_size = 9, base_family = "sans") +
    theme(
      axis.text = element_text(color = "#24313D"),
      axis.title = element_text(color = "#24313D"),
      strip.background = element_rect(fill = "#F1F4F6", color = NA),
      strip.text = element_text(face = "bold", hjust = 0),
      panel.spacing.x = grid::unit(1.1, "lines"),
      plot.margin = margin(6, 8, 6, 6)
    )
}

mafw_benchmark_report <- function(metric_sets, clip_summary) {
  full <- metric_sets[metric_sets$metric_set == "Full 11 metrics", ]
  lomm <- clip_summary[clip_summary$source == "LOMM", ]
  static <- clip_summary[clip_summary$source == "Static logistic-normal", ]
  reference <- clip_summary[clip_summary$source == "MAFW reference", ]
  c(
    "MAFW and LOMM cross-fitted benchmark",
    "",
    sprintf(
      "Full metric composite: LOMM %.4f [%.4f, %.4f]; real reference %.4f [%.4f, %.4f].",
      full$lomm_median, full$lomm_q025, full$lomm_q975,
      full$reference_median, full$reference_q025, full$reference_q975
    ),
    sprintf(
      "Clip plausibility: MAFW %.3f, LOMM %.3f, static %.3f.",
      reference$plausibility_median, lomm$plausibility_median,
      static$plausibility_median
    ),
    sprintf(
      "Clip coverage: MAFW %.3f, LOMM %.3f, static %.3f.",
      reference$coverage_median, lomm$coverage_median, static$coverage_median
    ),
    "",
    paste(
      "Distances are root-mean-square Euclidean distances across 22 metric cells",
      "after fold-specific median centering and robust scaling on 75 training clips."
    ),
    paste(
      "The fold-specific acceptance threshold is the 95th percentile of",
      "directional nearest-neighbor distances between two disjoint, stratified",
      "25-clip MAFW samples drawn from the training fold."
    )
  )
}

mafw_benchmark_main <- function() {
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  paths <- mafw_benchmark_paths()
  dir.create(MAFW_PLAUSIBILITY_OUTPUT, recursive = TRUE, showWarnings = FALSE)
  prepared <- mafw_prepare()
  caches <- mafw_build_caches(prepared)
  settings <- mafw_benchmark_settings()
  transforms <- lapply(caches, function(cache) {
    mafw_fit_clip_feature_transform(cache$empirical_training)
  })

  reference_distances <- mafw_reference_clip_distances(
    prepared, caches, transforms
  )
  thresholds <- mafw_fold_thresholds(reference_distances)
  generated <- mafw_generated_benchmark(
    prepared, caches, transforms, settings, thresholds
  )

  extended_metric_path <- file.path(MAFW_EXTENDED_OUTPUT, "metric_results.csv")
  extended_success <- file.path(MAFW_EXTENDED_OUTPUT, "_SUCCESS.json")
  if (!file.exists(extended_metric_path) || !file.exists(extended_success)) {
    mafw_stop("Completed extended real-reference metrics are unavailable.")
  }
  reference_metrics <- utils::read.csv(
    extended_metric_path, stringsAsFactors = FALSE
  )
  reference_metrics <- reference_metrics[
    reference_metrics$source == "MAFW split reference", , drop = FALSE
  ]
  expected_reference <-
    MAFW_FOLD_COUNT * MAFW_REFERENCE_REPLICATES *
    length(LOMM11_METRIC_FAMILIES) * length(LOMM11_METRICS)
  if (nrow(reference_metrics) != expected_reference) {
    mafw_stop("Extended real-reference metric rows are incomplete.")
  }
  metric_results <- rbind(generated$metrics, reference_metrics)
  all_distances <- rbind(reference_distances, generated$distances)
  clip_replicates <- mafw_clip_replicate_summary(all_distances, thresholds)
  clip_summary <- mafw_clip_summary(clip_replicates)
  metric_sets <- mafw_metric_set_table(metric_results)
  per_metric <- mafw_per_metric_table(metric_results)
  plot <- mafw_clip_plot(clip_summary)
  report <- mafw_benchmark_report(metric_sets, clip_summary)

  mafw_atomic_csv(metric_results, paths$metric_results)
  mafw_atomic_csv(metric_sets, paths$metric_sets)
  mafw_atomic_csv(per_metric, paths$per_metric)
  mafw_atomic_csv(all_distances, paths$clip_distances)
  mafw_atomic_csv(clip_replicates, paths$clip_replicates)
  mafw_atomic_csv(clip_summary, paths$clip_summary)
  mafw_atomic_csv(thresholds, paths$thresholds)
  mafw_atomic_text(report, paths$report)
  ggsave(paths$figure_pdf, plot, width = 7.1, height = 2.65, device = cairo_pdf)
  ggsave(paths$figure_png, plot, width = 7.1, height = 2.65, dpi = 360, bg = "white")

  output_files <- unlist(paths[names(paths) != "success"], use.names = FALSE)
  lomm11_runner_atomic_json(list(
    schema_version = 1L,
    status = "complete",
    completed_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    folds = MAFW_FOLD_COUNT,
    training_clips_per_fold = 75L,
    evaluation_clips_per_fold = 25L,
    heldout_replicates = MAFW_HELDOUT_REPLICATES,
    real_reference_replicates = MAFW_REFERENCE_REPLICATES,
    feature_cells = length(LOMM11_METRIC_FAMILIES) * length(LOMM11_METRICS),
    distance = "root mean square standardized Euclidean",
    neighbor_order = 1L,
    threshold_quantile = MAFW_NEIGHBOR_QUANTILE,
    standard_settings_sha256 = lomm11_runner_sha256(file.path(
      MAFW_STANDARD_OUTPUT, "selected_settings.csv"
    )),
    extended_metric_source_sha256 = lomm11_runner_sha256(extended_metric_path),
    outputs = as.list(stats::setNames(
      vapply(output_files, lomm11_runner_sha256, character(1)),
      basename(output_files)
    ))
  ), paths$success)
  print(metric_sets)
  print(clip_summary)
  message("Published clip-level benchmark: ", MAFW_PLAUSIBILITY_OUTPUT)
  invisible(list(
    metric_sets = metric_sets, per_metric = per_metric,
    clip_summary = clip_summary, paths = paths
  ))
}

if (sys.nframe() == 0L) mafw_benchmark_main()
