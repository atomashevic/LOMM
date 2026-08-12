LOMM11_METRIC_CHANNELS <- c(
  "anger", "disgust", "fear", "happiness", "neutral", "sadness",
  "surprise", "contempt", "anxiety", "helplessness", "disappointment"
)

LOMM11_UNIVARIATE_METRICS <- c(
  "level_median",
  "level_iqr",
  "difference_iqr",
  "lag1_correlation",
  "run_length_entropy",
  "spectral_entropy"
)

LOMM11_MULTIVARIATE_METRICS <- c("effective_rank", "total_variance")
LOMM11_METRICS <- c(LOMM11_UNIVARIATE_METRICS, LOMM11_MULTIVARIATE_METRICS)
LOMM11_METRIC_FAMILIES <- c("probability", "logratio")
LOMM11_CONTRIBUTIONS_PER_FAMILY <- 68L
LOMM11_CONTRIBUTIONS_PER_CLIP_VIEW <- 136L

lomm11_metrics_stop <- function(...) {
  stop(..., call. = FALSE)
}

lomm11_metric_groups <- function() {
  groups <- expand.grid(
    family = LOMM11_METRIC_FAMILIES,
    metric = LOMM11_METRICS,
    stringsAsFactors = FALSE
  )
  groups[order(
    match(groups$family, LOMM11_METRIC_FAMILIES),
    match(groups$metric, LOMM11_METRICS)
  ), , drop = FALSE]
}

lomm11_validate_segment_columns <- function(view) {
  required <- c(
    "clip", "frame_key", "analysis_variant", "segment_id",
    "segment_position", "segment_length"
  )
  missing <- setdiff(required, names(view))
  if (length(missing) > 0L) {
    lomm11_metrics_stop(
      "Metric view is missing columns: ", paste(missing, collapse = ", "), "."
    )
  }
  if (nrow(view) == 0L || anyNA(view[required]) || anyDuplicated(view$frame_key)) {
    lomm11_metrics_stop("Metric view keys and segment metadata must be complete and unique.")
  }
  if (length(unique(view$analysis_variant)) != 1L) {
    lomm11_metrics_stop("One metric extraction call must contain one analysis variant.")
  }
  segment_indices <- split(seq_len(nrow(view)), view$segment_id)
  valid_segments <- vapply(segment_indices, function(index) {
    positions <- view$segment_position[index]
    lengths <- unique(view$segment_length[index])
    clips <- unique(as.character(view$clip[index]))
    length(lengths) == 1L && length(clips) == 1L &&
      identical(sort(as.integer(positions)), seq_len(as.integer(lengths)))
  }, logical(1))
  if (!all(valid_segments)) {
    lomm11_metrics_stop("Segment positions, lengths, or clip membership are inconsistent.")
  }
  invisible(TRUE)
}

lomm11_segment_groups <- function(segment_id, segment_position) {
  indices <- split(seq_along(segment_id), factor(segment_id, levels = unique(segment_id)))
  lapply(indices, function(index) index[order(segment_position[index])])
}

lomm11_segment_differences <- function(series, segments) {
  differences <- lapply(segments, function(index) {
    values <- as.numeric(series[index])
    if (length(values) < 2L) numeric() else diff(values)
  })
  unlist(differences, use.names = FALSE)
}

lomm11_segment_lag_pairs <- function(series, segments) {
  pairs <- lapply(segments, function(index) {
    values <- as.numeric(series[index])
    if (length(values) < 2L) return(NULL)
    cbind(previous = values[-length(values)], current = values[-1L])
  })
  pairs <- Filter(Negate(is.null), pairs)
  if (length(pairs) == 0L) {
    return(matrix(numeric(), nrow = 0L, ncol = 2L,
                  dimnames = list(NULL, c("previous", "current"))))
  }
  do.call(rbind, pairs)
}

lomm11_entropy <- function(counts) {
  counts <- as.numeric(counts)
  counts <- counts[is.finite(counts) & counts > 0]
  if (length(counts) <= 1L) return(0)
  probability <- counts / sum(counts)
  -sum(probability * log2(probability)) / log2(length(probability))
}

lomm11_segment_run_length_entropy <- function(series, segments, max_run = 8L) {
  if (sum(vapply(segments, length, integer(1))) < 4L) return(NA_real_)
  runs <- lapply(segments, function(index) {
    signs <- sign(diff(as.numeric(series[index])))
    signs <- signs[is.finite(signs) & signs != 0]
    if (length(signs) == 0L) integer() else pmin(rle(signs)$lengths, max_run)
  })
  run_lengths <- unlist(runs, use.names = FALSE)
  if (length(run_lengths) < 2L) return(0)
  lomm11_entropy(table(run_lengths))
}

lomm11_one_spectral_entropy <- function(series) {
  series <- as.numeric(series)
  if (length(series) < 8L || any(!is.finite(series))) return(NA_real_)
  power <- Mod(stats::fft(series - mean(series)))^2
  power <- power[2:(floor(length(series) / 2L) + 1L)]
  total <- sum(power)
  if (!is.finite(total) || total <= 0) return(NA_real_)
  probability <- power / total
  probability <- probability[is.finite(probability) & probability > 0]
  if (length(probability) <= 1L) return(NA_real_)
  -sum(probability * log2(probability)) / log2(length(probability))
}

lomm11_segment_spectral_entropy <- function(series, segments) {
  values <- vapply(segments, function(index) {
    lomm11_one_spectral_entropy(series[index])
  }, numeric(1))
  weights <- pmax(vapply(segments, length, integer(1)) - 1L, 0L)
  usable <- is.finite(values)
  if (!any(usable)) return(NA_real_)
  stats::weighted.mean(values[usable], weights[usable])
}

lomm11_univariate_metric_values <- function(series, segment_id,
                                            segment_position) {
  series <- as.numeric(series)
  if (any(!is.finite(series))) {
    lomm11_metrics_stop("Metric series must be finite within an analysis view.")
  }
  segments <- lomm11_segment_groups(segment_id, segment_position)
  differences <- lomm11_segment_differences(series, segments)
  lag_pairs <- lomm11_segment_lag_pairs(series, segments)
  lag_correlation <- if (nrow(lag_pairs) < 2L ||
      stats::sd(lag_pairs[, "previous"]) == 0 ||
      stats::sd(lag_pairs[, "current"]) == 0) {
    NA_real_
  } else {
    stats::cor(lag_pairs[, "previous"], lag_pairs[, "current"])
  }
  values <- c(
    level_median = stats::median(series),
    level_iqr = stats::IQR(series),
    difference_iqr = if (length(differences) == 0L) NA_real_ else stats::IQR(differences),
    lag1_correlation = lag_correlation,
    run_length_entropy = lomm11_segment_run_length_entropy(series, segments),
    spectral_entropy = lomm11_segment_spectral_entropy(series, segments)
  )
  values[LOMM11_UNIVARIATE_METRICS]
}

lomm11_effective_rank <- function(values) {
  values <- as.matrix(values)
  if (nrow(values) < 2L || any(!is.finite(values))) return(NA_real_)
  correlation <- suppressWarnings(stats::cor(values))
  if (!is.matrix(correlation) || any(!is.finite(correlation))) return(NA_real_)
  eigenvalues <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
  eigenvalues <- eigenvalues[is.finite(eigenvalues) & eigenvalues > 0]
  if (length(eigenvalues) == 0L) return(NA_real_)
  probability <- eigenvalues / sum(eigenvalues)
  exp(-sum(probability * log(probability)))
}

lomm11_total_variance <- function(values) {
  values <- as.matrix(values)
  if (nrow(values) < 2L || any(!is.finite(values))) return(NA_real_)
  covariance <- stats::cov(values)
  if (!is.matrix(covariance) || any(!is.finite(covariance))) return(NA_real_)
  sum(diag(covariance))
}

lomm11_multivariate_metric_values <- function(values) {
  c(
    effective_rank = lomm11_effective_rank(values),
    total_variance = lomm11_total_variance(values)
  )[LOMM11_MULTIVARIATE_METRICS]
}

lomm11_clip_metadata_value <- function(view, field, fallback = NA_character_) {
  if (!field %in% names(view)) return(fallback)
  values <- unique(as.character(view[[field]]))
  if (length(values) != 1L) {
    lomm11_metrics_stop("Metric metadata field varies within a clip: ", field, ".")
  }
  values[[1L]]
}

lomm11_univariate_rows <- function(view, columns, units, family) {
  rows <- vector("list", length(columns))
  for (column_index in seq_along(columns)) {
    values <- lomm11_univariate_metric_values(
      view[[columns[[column_index]]]], view$segment_id, view$segment_position
    )
    rows[[column_index]] <- data.frame(
      family = family,
      component = "univariate",
      unit = units[[column_index]],
      metric = names(values),
      value = as.numeric(values),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

lomm11_multivariate_rows <- function(view, columns, family) {
  values <- lomm11_multivariate_metric_values(view[, columns, drop = FALSE])
  data.frame(
    family = family,
    component = "multivariate",
    unit = "clip_panel",
    metric = names(values),
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )
}

lomm11_family_clip_contributions <- function(univariate_view,
                                             multivariate_view,
                                             univariate_columns,
                                             multivariate_columns,
                                             units, family) {
  rows <- rbind(
    lomm11_univariate_rows(univariate_view, univariate_columns, units, family),
    lomm11_multivariate_rows(multivariate_view, multivariate_columns, family)
  )
  clip <- lomm11_clip_metadata_value(univariate_view, "clip")
  analysis_variant <- lomm11_clip_metadata_value(univariate_view, "analysis_variant")
  rows <- cbind(
    analysis_variant = analysis_variant,
    clip = clip,
    truth_label = lomm11_clip_metadata_value(univariate_view, "truth_label"),
    rows,
    stringsAsFactors = FALSE
  )
  rows$contribution_key <- paste(
    rows$analysis_variant, rows$clip, rows$family, rows$metric, rows$unit,
    sep = "|"
  )
  rows[, c(
    "contribution_key", "analysis_variant", "clip", "truth_label", "family",
    "component", "unit", "metric", "value"
  )]
}

lomm11_align_metric_views <- function(probability_view, clr_view, ilr_view) {
  lomm11_validate_segment_columns(probability_view)
  lomm11_validate_segment_columns(clr_view)
  lomm11_validate_segment_columns(ilr_view)
  key_column <- if ("observation_key" %in% names(probability_view) &&
      "observation_key" %in% names(clr_view) &&
      "observation_key" %in% names(ilr_view)) "observation_key" else "frame_key"
  reference <- as.character(probability_view[[key_column]])
  if (!identical(reference, as.character(clr_view[[key_column]])) ||
      !identical(reference, as.character(ilr_view[[key_column]]))) {
    lomm11_metrics_stop("Probability, CLR, and ILR metric views are not row-aligned.")
  }
  metadata <- c("clip", "analysis_variant", "segment_id", "segment_position", "segment_length")
  for (field in metadata) {
    if (!identical(probability_view[[field]], clr_view[[field]]) ||
        !identical(probability_view[[field]], ilr_view[[field]])) {
      lomm11_metrics_stop("Compositional metric metadata disagree for field: ", field, ".")
    }
  }
  invisible(TRUE)
}

lomm11_clip_contributions <- function(probability_clip, clr_clip, ilr_clip,
                                      probability_columns, clr_columns,
                                      ilr_columns) {
  probability <- lomm11_family_clip_contributions(
    probability_clip, probability_clip,
    probability_columns, probability_columns,
    probability_columns, "probability"
  )
  logratio <- lomm11_family_clip_contributions(
    clr_clip, ilr_clip, clr_columns, ilr_columns,
    sub("^clr_", "", clr_columns), "logratio"
  )
  rbind(probability, logratio)
}

lomm11_metric_contributions <- function(
    probability_view, clr_view, ilr_view,
    probability_columns = LOMM11_METRIC_CHANNELS,
    clr_columns = paste0("clr_", LOMM11_METRIC_CHANNELS),
    ilr_columns = sprintf("ilr_%02d", 1:10)) {
  lomm11_align_metric_views(probability_view, clr_view, ilr_view)
  expected_lengths <- c(
    probability = length(LOMM11_METRIC_CHANNELS),
    clr = length(LOMM11_METRIC_CHANNELS),
    ilr = length(LOMM11_METRIC_CHANNELS) - 1L
  )
  supplied_lengths <- c(
    probability = length(probability_columns), clr = length(clr_columns),
    ilr = length(ilr_columns)
  )
  all_columns <- c(probability_columns, clr_columns, ilr_columns)
  available <- c(names(probability_view), names(clr_view), names(ilr_view))
  if (!identical(supplied_lengths, expected_lengths) ||
      length(setdiff(all_columns, available)) > 0L) {
    lomm11_metrics_stop("Metric representation columns do not satisfy the 11/11/10 contract.")
  }
  clips <- unique(as.character(probability_view$clip))
  contributions <- lapply(clips, function(clip) {
    probability_clip <- probability_view[probability_view$clip == clip, , drop = FALSE]
    clr_clip <- clr_view[clr_view$clip == clip, , drop = FALSE]
    ilr_clip <- ilr_view[ilr_view$clip == clip, , drop = FALSE]
    lomm11_clip_contributions(
      probability_clip, clr_clip, ilr_clip,
      probability_columns, clr_columns, ilr_columns
    )
  })
  output <- do.call(rbind, contributions)
  rownames(output) <- NULL
  lomm11_validate_metric_contributions(output)
  output
}

lomm11_validate_metric_contributions <- function(contributions) {
  required <- c(
    "contribution_key", "analysis_variant", "clip", "family", "component",
    "unit", "metric", "value"
  )
  if (length(setdiff(required, names(contributions))) > 0L ||
      anyDuplicated(contributions$contribution_key)) {
    lomm11_metrics_stop("Metric contribution schema or keys are invalid.")
  }
  clips <- unique(as.character(contributions$clip))
  counts <- table(factor(contributions$clip, levels = clips))
  if (any(counts != LOMM11_CONTRIBUTIONS_PER_CLIP_VIEW)) {
    lomm11_metrics_stop("Each clip/view must contribute exactly 136 metric rows.")
  }
  family_counts <- table(contributions$clip, contributions$family)
  if (!setequal(colnames(family_counts), LOMM11_METRIC_FAMILIES) ||
      any(family_counts[, LOMM11_METRIC_FAMILIES, drop = FALSE] !=
            LOMM11_CONTRIBUTIONS_PER_FAMILY)) {
    lomm11_metrics_stop("Each metric family must contribute exactly 68 rows per clip/view.")
  }
  univariate <- contributions[contributions$component == "univariate", , drop = FALSE]
  multivariate <- contributions[contributions$component == "multivariate", , drop = FALSE]
  if (!setequal(unique(univariate$metric), LOMM11_UNIVARIATE_METRICS) ||
      !setequal(unique(multivariate$metric), LOMM11_MULTIVARIATE_METRICS)) {
    lomm11_metrics_stop("Metric contribution names do not match the 2 x 8 contract.")
  }
  invisible(TRUE)
}

lomm11_metric_scale <- function(values, tolerance = 1e-8) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  candidates <- c(
    IQR = if (length(values) == 0L) NA_real_ else stats::IQR(values),
    `1.4826*MAD` = if (length(values) == 0L) {
      NA_real_
    } else {
      stats::mad(values, constant = 1.4826)
    },
    SD = if (length(values) < 2L) NA_real_ else stats::sd(values),
    `1` = 1
  )
  selected <- which(is.finite(candidates) & candidates > tolerance)[[1L]]
  list(scale = unname(candidates[[selected]]), rule = names(candidates)[[selected]])
}

lomm11_fit_metric_scales <- function(contributions, training_clips) {
  lomm11_validate_metric_contributions(contributions)
  training_clips <- unique(as.character(training_clips))
  if (length(training_clips) == 0L || anyNA(training_clips)) {
    lomm11_metrics_stop("Explicit training clips are required for metric scales.")
  }
  absent <- setdiff(training_clips, unique(as.character(contributions$clip)))
  if (length(absent) > 0L) {
    lomm11_metrics_stop("Metric-scale training clips are absent from contributions.")
  }
  training <- contributions[contributions$clip %in% training_clips, , drop = FALSE]
  groups <- lomm11_metric_groups()
  rows <- lapply(seq_len(nrow(groups)), function(index) {
    selected <- training$family == groups$family[[index]] &
      training$metric == groups$metric[[index]]
    estimate <- lomm11_metric_scale(training$value[selected])
    data.frame(
      family = groups$family[[index]], metric = groups$metric[[index]],
      scale = estimate$scale, scale_rule = estimate$rule,
      n_finite = sum(is.finite(training$value[selected])),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

lomm11_scale_lookup <- function(scales, family, metric) {
  index <- which(scales$family == family & scales$metric == metric)
  if (length(index) != 1L || !is.finite(scales$scale[[index]]) ||
      scales$scale[[index]] <= 0) {
    lomm11_metrics_stop("Metric scale lookup is missing or invalid.")
  }
  scales$scale[[index]]
}

lomm11_pair_metric_contributions <- function(empirical, generated, scales) {
  lomm11_validate_metric_contributions(empirical)
  lomm11_validate_metric_contributions(generated)
  key_fields <- c("analysis_variant", "clip", "family", "component", "unit", "metric")
  key <- function(data) do.call(paste, c(data[key_fields], sep = "\037"))
  empirical_key <- key(empirical)
  generated_key <- key(generated)
  matched <- match(empirical_key, generated_key)
  if (anyNA(matched) || length(empirical_key) != length(generated_key) ||
      anyDuplicated(generated_key)) {
    lomm11_metrics_stop("Empirical and generated contribution keys do not align one-to-one.")
  }
  output <- empirical[, setdiff(names(empirical), "value"), drop = FALSE]
  output$empirical_value <- empirical$value
  output$generated_value <- generated$value[matched]
  output$scale <- mapply(
    function(family, metric) lomm11_scale_lookup(scales, family, metric),
    output$family, output$metric
  )
  output$scaled_signed_difference <-
    (output$generated_value - output$empirical_value) / output$scale
  output$scaled_absolute_difference <- abs(output$scaled_signed_difference)
  output
}

lomm11_wasserstein_1d <- function(first, second) {
  first <- sort(as.numeric(first)[is.finite(first)])
  second <- sort(as.numeric(second)[is.finite(second)])
  if (length(first) == 0L || length(second) == 0L) return(NA_real_)
  support <- sort(unique(c(first, second)))
  if (length(support) == 1L) return(0)
  left_endpoints <- support[-length(support)]
  first_cdf <- findInterval(left_endpoints, first) / length(first)
  second_cdf <- findInterval(left_endpoints, second) / length(second)
  sum(abs(first_cdf - second_cdf) * diff(support))
}

lomm11_one_metric_discrepancy <- function(empirical, generated, scale) {
  empirical <- as.numeric(empirical)
  generated <- as.numeric(generated)
  empirical <- empirical[is.finite(empirical)]
  generated <- generated[is.finite(generated)]
  if (length(empirical) == 0L || length(generated) == 0L) {
    return(c(
      empirical_median = NA_real_, generated_median = NA_real_,
      signed_median_difference_scaled = NA_real_,
      abs_median_difference_scaled = NA_real_, wasserstein1_scaled = NA_real_,
      discrepancy = NA_real_
    ))
  }
  empirical_median <- stats::median(empirical)
  generated_median <- stats::median(generated)
  signed <- (generated_median - empirical_median) / scale
  wasserstein <- lomm11_wasserstein_1d(empirical, generated) / scale
  c(
    empirical_median = empirical_median,
    generated_median = generated_median,
    signed_median_difference_scaled = signed,
    abs_median_difference_scaled = abs(signed),
    wasserstein1_scaled = wasserstein,
    discrepancy = max(abs(signed), wasserstein)
  )
}

lomm11_metric_discrepancies <- function(empirical, generated, scales) {
  lomm11_validate_metric_contributions(empirical)
  lomm11_validate_metric_contributions(generated)
  groups <- lomm11_metric_groups()
  rows <- lapply(seq_len(nrow(groups)), function(index) {
    family <- groups$family[[index]]
    metric <- groups$metric[[index]]
    empirical_values <- empirical$value[
      empirical$family == family & empirical$metric == metric
    ]
    generated_values <- generated$value[
      generated$family == family & generated$metric == metric
    ]
    scale <- lomm11_scale_lookup(scales, family, metric)
    values <- lomm11_one_metric_discrepancy(
      empirical_values, generated_values, scale
    )
    data.frame(
      family = family, metric = metric, scale = scale,
      n_empirical = sum(is.finite(empirical_values)),
      n_generated = sum(is.finite(generated_values)),
      as.list(values), stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  if (nrow(output) != 16L) {
    lomm11_metrics_stop("A metric evaluation must produce exactly 16 discrepancies.")
  }
  output
}

lomm11_selection_loss <- function(discrepancies,
                                  family_weights = c(probability = 0.5,
                                                     logratio = 0.5)) {
  if (!identical(sort(names(family_weights)), sort(LOMM11_METRIC_FAMILIES)) ||
      any(!is.finite(family_weights)) || any(family_weights < 0) ||
      sum(family_weights) <= 0) {
    lomm11_metrics_stop("Family weights must be named, finite, and non-negative.")
  }
  if (nrow(discrepancies) != 16L || any(!is.finite(discrepancies$discrepancy))) {
    return(list(selection_loss = Inf, max_metric_discrepancy = Inf))
  }
  family_mean <- vapply(LOMM11_METRIC_FAMILIES, function(family) {
    mean(discrepancies$discrepancy[discrepancies$family == family])
  }, numeric(1))
  weights <- family_weights[LOMM11_METRIC_FAMILIES]
  list(
    selection_loss = sum(family_mean * weights) / sum(weights),
    max_metric_discrepancy = max(discrepancies$discrepancy),
    family_mean = family_mean
  )
}

lomm11_run_internal_metric_checks <- function() {
  values <- c(0, 1, 2, 100, 101, 102)
  segments <- c(rep("one", 3L), rep("two", 3L))
  positions <- rep(1:3, 2L)
  metrics <- lomm11_univariate_metric_values(values, segments, positions)
  if (!identical(unname(metrics[["difference_iqr"]]), 0)) {
    lomm11_metrics_stop("Segment-boundary difference check failed.")
  }
  scale <- lomm11_metric_scale(rep(2, 5L))
  if (scale$scale != 1 || scale$rule != "1") {
    lomm11_metrics_stop("Metric scale fallback check failed.")
  }
  if (lomm11_wasserstein_1d(1:4, 1:4) != 0) {
    lomm11_metrics_stop("Wasserstein identity check failed.")
  }
  unequal <- lomm11_wasserstein_1d(c(0, 2, NA_real_), c(1, NA_real_))
  if (abs(unequal - 1) > 1e-12) {
    lomm11_metrics_stop("Exact unequal-size Wasserstein check failed.")
  }
  tied <- lomm11_wasserstein_1d(c(0, 0, 2), c(1, 3))
  if (abs(tied - 4 / 3) > 1e-12) {
    lomm11_metrics_stop("Exact tied-sample Wasserstein check failed.")
  }
  invisible(TRUE)
}
