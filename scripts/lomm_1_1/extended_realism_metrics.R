LOMM11_UNIVARIATE_METRICS <- c(
  "level_median",
  "level_iqr",
  "difference_iqr",
  "lag1_correlation",
  "run_length_entropy",
  "sample_entropy",
  "permutation_entropy",
  "spectral_entropy"
)

LOMM11_MULTIVARIATE_METRICS <- c(
  "effective_rank",
  "robust_level_variance",
  "robust_change_variance"
)
LOMM11_METRICS <- c(LOMM11_UNIVARIATE_METRICS, LOMM11_MULTIVARIATE_METRICS)
LOMM11_CONTRIBUTIONS_PER_FAMILY <-
  length(LOMM11_METRIC_CHANNELS) * length(LOMM11_UNIVARIATE_METRICS) +
  length(LOMM11_MULTIVARIATE_METRICS)
LOMM11_CONTRIBUTIONS_PER_CLIP_VIEW <-
  length(LOMM11_METRIC_FAMILIES) * LOMM11_CONTRIBUTIONS_PER_FAMILY

lomm11_segment_sample_entropy <- function(series, segments, order = 2L,
                                          tolerance_ratio = 0.20) {
  series <- as.numeric(series)
  spread <- stats::sd(series)
  if (!is.finite(spread) || spread == 0) return(0)
  templates <- lapply(segments, function(index) {
    values <- series[index]
    if (length(values) < order + 1L) return(NULL)
    stats::embed(values, order + 1L)
  })
  templates <- Filter(Negate(is.null), templates)
  if (length(templates) == 0L) return(NA_real_)
  extended <- do.call(rbind, templates)
  if (nrow(extended) < 2L) return(NA_real_)
  tolerance <- tolerance_ratio * spread
  base_distance <- as.matrix(stats::dist(
    extended[, seq_len(order), drop = FALSE], method = "maximum"
  ))
  extended_distance <- as.matrix(stats::dist(extended, method = "maximum"))
  upper <- upper.tri(base_distance)
  base_matches <- sum(base_distance[upper] <= tolerance)
  extended_matches <- sum(extended_distance[upper] <= tolerance)
  match_probability <- (extended_matches + 0.5) / (base_matches + 1)
  -log(match_probability)
}

lomm11_permutation_signature <- function(values) {
  paste(order(as.numeric(values), seq_along(values)), collapse = "")
}

lomm11_segment_permutation_entropy <- function(series, segments, order = 3L) {
  patterns <- unlist(lapply(segments, function(index) {
    values <- as.numeric(series[index])
    if (length(values) < order) return(character())
    windows <- stats::embed(values, order)
    apply(windows, 1L, lomm11_permutation_signature)
  }), use.names = FALSE)
  if (length(patterns) < 2L) return(NA_real_)
  counts <- as.numeric(table(patterns))
  probabilities <- counts / sum(counts)
  entropy <- -sum(probabilities * log2(probabilities))
  entropy / log2(factorial(order))
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
    sample_entropy = lomm11_segment_sample_entropy(series, segments),
    permutation_entropy = lomm11_segment_permutation_entropy(series, segments),
    spectral_entropy = lomm11_segment_spectral_entropy(series, segments)
  )
  values[LOMM11_UNIVARIATE_METRICS]
}

lomm11_robust_level_variance <- function(values, trim = 0.10) {
  values <- as.matrix(values)
  if (nrow(values) < 2L || any(!is.finite(values))) return(NA_real_)
  centered <- sweep(values, 2L, colMeans(values), "-")
  squared_distance <- rowSums(centered^2) / ncol(values)
  mean(squared_distance, trim = trim)
}

lomm11_robust_change_variance <- function(values, segments, trim = 0.10) {
  values <- as.matrix(values)
  if (nrow(values) < 2L || any(!is.finite(values))) return(NA_real_)
  changes <- lapply(segments, function(index) {
    if (length(index) < 2L) return(NULL)
    apply(values[index, , drop = FALSE], 2L, diff)
  })
  changes <- Filter(Negate(is.null), changes)
  if (length(changes) == 0L) return(NA_real_)
  changes <- do.call(rbind, changes)
  if (is.null(dim(changes))) changes <- matrix(changes, nrow = 1L)
  squared_distance <- rowSums(changes^2) / (2 * ncol(values))
  mean(squared_distance, trim = trim)
}

lomm11_multivariate_metric_values <- function(values, segment_id,
                                              segment_position) {
  segments <- lomm11_segment_groups(segment_id, segment_position)
  c(
    effective_rank = lomm11_effective_rank(values),
    robust_level_variance = lomm11_robust_level_variance(values),
    robust_change_variance = lomm11_robust_change_variance(values, segments)
  )[LOMM11_MULTIVARIATE_METRICS]
}

lomm11_multivariate_rows <- function(view, columns, family) {
  values <- lomm11_multivariate_metric_values(
    view[, columns, drop = FALSE], view$segment_id, view$segment_position
  )
  data.frame(
    family = family,
    component = "multivariate",
    unit = "clip_panel",
    metric = names(values),
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )
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
    lomm11_metrics_stop("Clip/view contribution counts do not match the extended contract.")
  }
  family_counts <- table(contributions$clip, contributions$family)
  if (!setequal(colnames(family_counts), LOMM11_METRIC_FAMILIES) ||
      any(family_counts[, LOMM11_METRIC_FAMILIES, drop = FALSE] !=
            LOMM11_CONTRIBUTIONS_PER_FAMILY)) {
    lomm11_metrics_stop("Family contribution counts do not match the extended contract.")
  }
  univariate <- contributions[
    contributions$component == "univariate", , drop = FALSE
  ]
  multivariate <- contributions[
    contributions$component == "multivariate", , drop = FALSE
  ]
  if (!setequal(unique(univariate$metric), LOMM11_UNIVARIATE_METRICS) ||
      !setequal(unique(multivariate$metric), LOMM11_MULTIVARIATE_METRICS)) {
    lomm11_metrics_stop("Metric contribution names do not match the extended contract.")
  }
  invisible(TRUE)
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
  expected <- length(LOMM11_METRIC_FAMILIES) * length(LOMM11_METRICS)
  if (nrow(output) != expected) {
    lomm11_metrics_stop("Metric discrepancy rows do not match the extended contract.")
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
  expected <- length(LOMM11_METRIC_FAMILIES) * length(LOMM11_METRICS)
  if (nrow(discrepancies) != expected ||
      any(!is.finite(discrepancies$discrepancy))) {
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

lomm11_run_extended_metric_checks <- function() {
  regular <- rep(1, 12L)
  segments <- list(seq_along(regular))
  if (lomm11_segment_sample_entropy(regular, segments) != 0) {
    lomm11_metrics_stop("Constant-series sample entropy must be zero.")
  }
  increasing <- seq_len(12L)
  if (abs(lomm11_segment_permutation_entropy(increasing, segments)) > 1e-12) {
    lomm11_metrics_stop("Monotone-series permutation entropy must be zero.")
  }
  alternating <- rep(c(0, 1), 6L)
  sample_value <- lomm11_segment_sample_entropy(alternating, segments)
  permutation_value <- lomm11_segment_permutation_entropy(alternating, segments)
  if (!is.finite(sample_value) || !is.finite(permutation_value) ||
      sample_value < 0 || permutation_value < 0 || permutation_value > 1) {
    lomm11_metrics_stop("Extended entropy checks failed.")
  }
  first <- matrix(rep(seq_len(12L), 3L), ncol = 3L)
  shifted <- first + 100
  if (abs(
      lomm11_robust_level_variance(first) -
        lomm11_robust_level_variance(shifted)
  ) > 1e-12) {
    lomm11_metrics_stop("Robust level variance must be shift-invariant.")
  }
  invisible(TRUE)
}
