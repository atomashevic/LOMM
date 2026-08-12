#!/usr/bin/env Rscript

REVISION_V2_ALGORITHMS <- c("louvain", "leiden", "walktrap")
REVISION_V2_METHOD_ORDER <- c(
  "dynEGA", "graphicalVAR", "gimme", "static_EGA", "oracle_blocks"
)
REVISION_V2_METHOD_LABELS <- c(
  dynEGA = "DynEGA",
  graphicalVAR = "GraphicalVAR",
  gimme = "GIMME",
  static_EGA = "Static EGA",
  oracle_blocks = "Block oracle"
)
REVISION_V2_COLORS <- c(
  dynEGA = "#0072B2",
  graphicalVAR = "#D55E00",
  gimme = "#009E73",
  static_EGA = "#CC79A7",
  oracle_blocks = "#000000"
)
REVISION_V2_ALGORITHM_PCH <- c(louvain = 21L, leiden = 22L, walktrap = 24L)
REVISION_V2_ALGORITHM_LTY <- c(louvain = 1L, leiden = 2L, walktrap = 3L)
REVISION_V2_BOOTSTRAP_REPS <- 2000L

revision_v2_table_schemas <- function() {
  list(
    empirical_realism_table.csv = c(
      "family_id", "family_label", "metric_id", "metric_label",
      "component_id", "component_label", "estimate", "conf_low", "conf_high",
      "empirical_scale", "discrepancy_band", "n_clips", "bootstrap_reps"
    ),
    table1_method_overall.csv = c(
      "horizon_design", "time_points", "method_id", "method_label",
      "community_algorithm", "estimand_id", "estimand_label", "estimate",
      "conf_low", "conf_high", "n_panels", "bootstrap_reps"
    ),
    figure5_condition_key.csv = c(
      "condition_code", "scenario_family", "scenario_label", "stability_class"
    ),
    figure6_factor_effects_table.csv = c(
      "method_id", "method_label", "community_algorithm", "challenge_id",
      "challenge_label", "representation", "estimand_id", "estimate",
      "conf_low", "conf_high", "n_pairs", "bootstrap_reps"
    ),
    figure6_factor_effects_eta_screen.csv = c(
      "horizon_design", "time_points", "method_id", "method_label",
      "community_algorithm", "stability_class", "estimand_id", "estimate",
      "conf_low", "conf_high", "n_panels", "bootstrap_reps"
    ),
    table2_score_quality_marginal_means.csv = c(
      "horizon_design", "time_points", "method_id", "method_label",
      "community_algorithm", "estimand_id", "estimand_label", "estimate",
      "conf_low", "conf_high", "n_panels", "bootstrap_reps"
    ),
    table3_runtime_feasibility.csv = c(
      "horizon_design", "time_points", "method_id", "method_label",
      "n_structure_fits", "n_successful_fits", "runtime_summary_status",
      "completion_estimate", "completion_conf_low", "completion_conf_high",
      "runtime_median_sec", "runtime_q1_sec", "runtime_q3_sec", "bootstrap_reps"
    ),
    table3_runtime_feasibility_accounting.csv = c(
      "source_job", "analysis_scope", "horizon_design", "time_points",
      "method_id", "readout_rows", "structure_fits", "ok_readouts",
      "error_readouts", "timeout_readouts", "community_algorithms"
    )
  )
}

revision_v2_figure_stems <- function() {
  c(
    "figure4_empirical_realism",
    "figure5_dimension_recovery",
    "figure6_factor_effects",
    "figure7_runtime_feasibility"
  )
}

revision_v2_parse_cli <- function(arguments) {
  value <- list(aggregate_dir = NULL, output_root = NULL, fixture_test = FALSE)
  index <- 1L
  while (index <= length(arguments)) {
    flag <- arguments[[index]]
    if (identical(flag, "--fixture-test")) {
      value$fixture_test <- TRUE
      index <- index + 1L
      next
    }
    if (!flag %in% c("--aggregate-dir", "--output-root") || index == length(arguments)) {
      stop(
        paste(
          "Usage: build_revision_v2_artifacts.R",
          "--aggregate-dir DIR --output-root DIR [--fixture-test]."
        ),
        call. = FALSE
      )
    }
    name <- gsub("-", "_", sub("^--", "", flag), fixed = TRUE)
    value[[name]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (!isTRUE(value$fixture_test) &&
      (is.null(value$aggregate_dir) || is.null(value$output_root))) {
    stop("Both --aggregate-dir and --output-root are required.", call. = FALSE)
  }
  value
}

revision_v2_read_csv <- function(directory, filename, columns) {
  path <- file.path(directory, filename)
  if (!file.exists(path) || file.info(path)$size <= 0L) {
    stop("Missing revision-v2 aggregate artifact: ", path, call. = FALSE)
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    header <- names(data.table::fread(
      path,
      nrows = 0L,
      nThread = 1L,
      showProgress = FALSE
    ))
  } else {
    header <- names(utils::read.csv(
      path,
      nrows = 0L,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  missing <- setdiff(columns, header)
  if (length(missing) > 0L) {
    stop(
      filename, " omits required columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(as.data.frame(data.table::fread(
      path,
      select = columns,
      na.strings = c("", "NA", "NaN"),
      nThread = 1L,
      showProgress = FALSE
    )))
  }
  data <- utils::read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NaN")
  )
  data[columns]
}

revision_v2_method_columns <- function() {
  c(
    ".source_job", "unit_key", "panel_key", "structure_fit_key", "method_id",
    "community_algorithm", "scenario", "scenario_family", "challenge_id",
    "representation", "TimeP", "stability_class", "N", "NVAR", "Error_sd",
    "Loadings", "Q", "rep", "n_dim_true", "n_dim_hat", "status",
    "structure_fit_status", "structure_fit_runtime_sec", "runtime_sec"
  )
}

revision_v2_require_aggregate_contract <- function(directory) {
  required <- c(
    "revision_estimand_spec.csv",
    "upstream_artifact_manifest.csv",
    "revision_results_long.csv",
    "identity_validation.csv",
    "revision_estimand_results.csv",
    "revision_paired_differences.csv",
    "_SUCCESS.json"
  )
  paths <- file.path(directory, required)
  missing <- required[!file.exists(paths) | file.info(paths)$size <= 0L]
  if (length(missing) > 0L) {
    stop(
      "The selected aggregate directory is incomplete: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(required)
}

revision_v2_select <- function(data, conditions) {
  keep <- rep(TRUE, nrow(data))
  for (name in names(conditions)) {
    if (!name %in% names(data)) {
      stop("Selection column is absent: ", name, call. = FALSE)
    }
    keep <- keep & !is.na(data[[name]]) & data[[name]] %in% conditions[[name]]
  }
  data[keep, , drop = FALSE]
}

revision_v2_nonempty <- function(data, label) {
  if (nrow(data) == 0L) {
    stop("No revision-v2 aggregate rows support ", label, ".", call. = FALSE)
  }
  data
}

revision_v2_numeric <- function(data, column, label) {
  value <- suppressWarnings(as.numeric(data[[column]]))
  if (any(!is.finite(value))) {
    stop(label, " contains non-finite ", column, " values.", call. = FALSE)
  }
  value
}

revision_v2_validate_intervals <- function(data, label, probability = FALSE) {
  if (nrow(data) == 0L) {
    stop(label, " is empty.", call. = FALSE)
  }
  estimate <- revision_v2_numeric(data, "estimate", label)
  low <- revision_v2_numeric(data, "conf_low", label)
  high <- revision_v2_numeric(data, "conf_high", label)
  if (any(low > estimate + 1e-12) || any(estimate > high + 1e-12)) {
    stop(label, " contains an interval that excludes its estimate.", call. = FALSE)
  }
  if (probability && any(c(estimate, low, high) < -1e-12 | c(estimate, low, high) > 1 + 1e-12)) {
    stop(label, " contains a probability outside [0, 1].", call. = FALSE)
  }
  invisible(TRUE)
}

revision_v2_assert_unique <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(label, " omits key columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  key <- do.call(paste, c(data[columns], sep = "\r"))
  if (anyDuplicated(key)) {
    stop(label, " contains duplicate keys.", call. = FALSE)
  }
  invisible(TRUE)
}

revision_v2_humanize <- function(value) {
  value <- gsub("_", " ", value, fixed = TRUE)
  value <- gsub("\\bacf\\b", "ACF", value, ignore.case = TRUE)
  value <- gsub("\\biqr\\b", "IQR", value, ignore.case = TRUE)
  value <- gsub("\\bnrmse\\b", "NRMSE", value, ignore.case = TRUE)
  tools::toTitleCase(value)
}

revision_v2_method_label <- function(method) {
  labels <- unname(REVISION_V2_METHOD_LABELS[method])
  missing <- is.na(labels)
  labels[missing] <- revision_v2_humanize(method[missing])
  labels
}

revision_v2_horizon_design <- function(time_points) {
  time_points <- as.integer(time_points)
  ifelse(time_points == 100L, "Primary T=100", "Stress T=25")
}

revision_v2_normalize_method <- function(method) {
  method <- as.character(method)
  method[method %in% c("static_correlation", "static_EGA_glasso")] <- "static_EGA"
  method
}

revision_v2_analysis_scope <- function(source_job) {
  ifelse(grepl("^challenge_", source_job), "challenge", "matched")
}

revision_v2_seed <- function(text) {
  codes <- utf8ToInt(enc2utf8(text))
  if (length(codes) == 0L) {
    return(1L)
  }
  weights <- (seq_along(codes) %% 1009L) + 1L
  as.integer((sum((codes * weights) %% 2147483629) %% 2147483628) + 1)
}

revision_v2_mean_interval <- function(values, clusters, key,
                                      reps = REVISION_V2_BOOTSTRAP_REPS) {
  values <- as.numeric(values)
  clusters <- as.character(clusters)
  if (length(values) == 0L || any(!is.finite(values)) ||
      any(is.na(clusters) | !nzchar(clusters))) {
    stop("Clustered mean requires finite values and nonempty cluster IDs.", call. = FALSE)
  }
  cluster_values <- vapply(
    split(values, clusters, drop = TRUE), mean, numeric(1)
  )
  count <- length(cluster_values)
  if (count < 2L) {
    stop("Clustered interval requires at least two independent panels.", call. = FALSE)
  }
  set.seed(revision_v2_seed(key))
  distinct <- sort(unique(cluster_values))
  if (length(distinct) <= 20L) {
    frequency <- tabulate(match(cluster_values, distinct), nbins = length(distinct))
    draws <- stats::rmultinom(reps, size = count, prob = frequency / count)
    bootstrap <- as.numeric(crossprod(distinct, draws)) / count
  } else {
    bootstrap <- vapply(seq_len(reps), function(index) {
      mean(sample(cluster_values, count, replace = TRUE))
    }, numeric(1))
  }
  c(
    estimate = mean(cluster_values),
    conf_low = stats::quantile(bootstrap, 0.025, names = FALSE, type = 8),
    conf_high = stats::quantile(bootstrap, 0.975, names = FALSE, type = 8),
    n_panels = count,
    bootstrap_reps = reps
  )
}

revision_v2_split_apply <- function(data, columns, callback) {
  key <- do.call(paste, c(data[columns], sep = "\r"))
  groups <- split(seq_len(nrow(data)), key, drop = TRUE)
  rows <- lapply(names(groups), function(group_key) {
    callback(data[groups[[group_key]], , drop = FALSE], group_key)
  })
  do.call(rbind, rows)
}

revision_v2_canonical_method_rows <- function(long) {
  required <- revision_v2_method_columns()
  missing <- setdiff(required, names(long))
  if (length(missing) > 0L) {
    stop(
      "revision_results_long.csv omits method fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  keep <- !is.na(long$method_id) & nzchar(as.character(long$method_id))
  data <- long[keep, required, drop = FALSE]
  data$method_id <- revision_v2_normalize_method(data$method_id)
  data$community_algorithm <- tolower(as.character(data$community_algorithm))
  data$status <- tolower(as.character(data$status))
  data$status[data$status %in% c("success", "complete", "completed")] <- "ok"
  data$structure_fit_status <- tolower(as.character(data$structure_fit_status))
  data$structure_fit_status[
    data$structure_fit_status %in% c("success", "complete", "completed")
  ] <- "ok"
  data$analysis_scope <- revision_v2_analysis_scope(data$.source_job)
  data$TimeP <- suppressWarnings(as.integer(data$TimeP))
  data$n_dim_true <- suppressWarnings(as.numeric(data$n_dim_true))
  data$n_dim_hat <- suppressWarnings(as.numeric(data$n_dim_hat))
  data$usable_and_correct <- as.numeric(
    data$status == "ok" & is.finite(data$n_dim_hat) & data$n_dim_hat == data$n_dim_true
  )
  if (any(!data$method_id %in% REVISION_V2_METHOD_ORDER) ||
      any(!data$community_algorithm %in% REVISION_V2_ALGORITHMS) ||
      any(!data$status %in% c("ok", "error", "timeout")) ||
      any(!data$TimeP %in% c(25L, 100L)) ||
      any(!is.finite(data$n_dim_true))) {
    stop("Canonical method rows violate the method/readout/status/horizon contract.", call. = FALSE)
  }
  revision_v2_assert_unique(data, "unit_key", "Canonical method rows")
  data
}

revision_v2_prepare_condition_key <- function(method_rows) {
  matched <- revision_v2_nonempty(
    method_rows[method_rows$analysis_scope == "matched", , drop = FALSE],
    "the matched condition key"
  )
  mapping <- unique(matched[, c("scenario_family", "stability_class"), drop = FALSE])
  if (any(is.na(mapping$scenario_family) | !nzchar(mapping$scenario_family)) ||
      any(is.na(mapping$stability_class) | !nzchar(mapping$stability_class))) {
    stop("Matched scenarios require family and stability labels.", call. = FALSE)
  }
  counts <- table(mapping$scenario_family)
  if (any(counts != 1L)) {
    stop("A scenario family maps to more than one stability class.", call. = FALSE)
  }
  if (!setequal(
    unique(mapping$stability_class),
    c("asymptotically_stable", "anti_damped")
  )) {
    stop("Matched results require stable and anti-damped scenario classes.", call. = FALSE)
  }
  stability_order <- match(
    mapping$stability_class,
    c("asymptotically_stable", "anti_damped")
  )
  mapping <- mapping[order(stability_order, mapping$scenario_family), , drop = FALSE]
  if (nrow(mapping) != 6L) {
    stop("Matched results require exactly six scenario families.", call. = FALSE)
  }
  data.frame(
    condition_code = sprintf("C%02d", seq_len(nrow(mapping))),
    scenario_family = mapping$scenario_family,
    scenario_label = revision_v2_humanize(mapping$scenario_family),
    stability_class = mapping$stability_class,
    stringsAsFactors = FALSE
  )
}

revision_v2_prepare_empirical <- function(estimands) {
  data <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "empirical_cv",
    summary_level = "heldout_clip_cluster",
    empirical_family = c("observable_primary", "secondary_frozen_summary")
  )), "empirical held-out realism")
  expected_components <- c(
    "signed_scaled_median_difference",
    "absolute_scaled_median_difference",
    "scaled_wasserstein_distance",
    "max_discrepancy"
  )
  if (!setequal(unique(data$contribution), expected_components)) {
    stop("Empirical realism rows omit a prespecified discrepancy component.", call. = FALSE)
  }
  expected_metrics <- c(
    "channel_iqr", "derivative_iqr", "effective_rank", "lag1_acf",
    "run_length_entropy", "spectral_entropy"
  )
  expected_rows <- 2L * length(expected_metrics) * length(expected_components)
  if (nrow(data) != expected_rows ||
      !setequal(unique(data$empirical_metric), expected_metrics)) {
    stop("Empirical realism rows do not form the complete family-metric grid.", call. = FALSE)
  }
  revision_v2_validate_intervals(data, "Empirical realism estimates")
  estimate <- as.numeric(data$estimate)
  band_value <- abs(estimate)
  band <- ifelse(
    band_value <= 0.33,
    "Small (<=0.33)",
    ifelse(band_value <= 1, "Intermediate (>0.33 to 1)", "Large (>1)")
  )
  family_labels <- c(
    observable_primary = "Observable channels (primary)",
    secondary_frozen_summary = "Frozen DynEGA summaries (secondary)"
  )
  component_labels <- c(
    signed_scaled_median_difference = "Signed scaled median difference",
    absolute_scaled_median_difference = "Absolute scaled median difference",
    scaled_wasserstein_distance = "Scaled Wasserstein distance",
    max_discrepancy = "Maximum discrepancy"
  )
  output <- data.frame(
    family_id = data$empirical_family,
    family_label = unname(family_labels[data$empirical_family]),
    metric_id = data$empirical_metric,
    metric_label = revision_v2_humanize(data$empirical_metric),
    component_id = data$contribution,
    component_label = unname(component_labels[data$contribution]),
    estimate = estimate,
    conf_low = as.numeric(data$conf_low),
    conf_high = as.numeric(data$conf_high),
    empirical_scale = revision_v2_numeric(data, "empirical_scale", "Empirical realism"),
    discrepancy_band = band,
    n_clips = as.integer(revision_v2_numeric(data, "n_panels", "Empirical realism")),
    bootstrap_reps = as.integer(revision_v2_numeric(
      data, "bootstrap_reps", "Empirical realism"
    )),
    stringsAsFactors = FALSE
  )
  output[order(output$family_id, output$metric_id, output$component_id), , drop = FALSE]
}

revision_v2_estimand_label <- function(id) {
  labels <- c(
    p_usable = "Usable fit probability",
    p_correct_given_usable = "Correct dimension given usable",
    p_usable_and_correct = "Usable and correct probability",
    signed_dimension_error = "Signed dimension error",
    absolute_dimension_error = "Absolute dimension error",
    adjusted_rand_index = "Adjusted Rand index",
    variation_information = "Variation of information",
    assignment_accuracy_conditional = "Assignment accuracy given usable",
    score_correlation_marginal = "Marginal score correlation",
    score_nrmse_marginal = "Marginal score NRMSE"
  )
  result <- unname(labels[id])
  result[is.na(result)] <- revision_v2_humanize(id[is.na(result)])
  result
}

revision_v2_prepare_horizon_estimands <- function(estimands, ids, label,
                                                  allow_undefined = FALSE) {
  data <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "horizon",
    community_algorithm = REVISION_V2_ALGORITHMS,
    estimand_id = ids
  )), label)
  finite <- is.finite(suppressWarnings(as.numeric(data$estimate))) &
    is.finite(suppressWarnings(as.numeric(data$conf_low))) &
    is.finite(suppressWarnings(as.numeric(data$conf_high)))
  if (isTRUE(allow_undefined)) {
    data <- revision_v2_nonempty(
      data[finite, , drop = FALSE],
      paste(label, "with estimable intervals")
    )
  } else if (!all(finite)) {
    stop(label, " contains an undefined prespecified estimate.", call. = FALSE)
  }
  revision_v2_validate_intervals(data, label)
  probability_ids <- c("p_usable", "p_correct_given_usable", "p_usable_and_correct")
  probability <- data$estimand_id %in% probability_ids
  if (any(probability)) {
    revision_v2_validate_intervals(data[probability, , drop = FALSE], label, TRUE)
  }
  output <- data.frame(
    horizon_design = revision_v2_horizon_design(data$TimeP),
    time_points = as.integer(data$TimeP),
    method_id = revision_v2_normalize_method(data$method_id),
    method_label = revision_v2_method_label(revision_v2_normalize_method(data$method_id)),
    community_algorithm = tolower(data$community_algorithm),
    estimand_id = data$estimand_id,
    estimand_label = revision_v2_estimand_label(data$estimand_id),
    estimate = as.numeric(data$estimate),
    conf_low = as.numeric(data$conf_low),
    conf_high = as.numeric(data$conf_high),
    n_panels = as.integer(data$n_panels),
    bootstrap_reps = as.integer(data$bootstrap_reps),
    stringsAsFactors = FALSE
  )
  revision_v2_assert_unique(
    output,
    c("time_points", "method_id", "community_algorithm", "estimand_id"),
    label
  )
  expected_rows <- 2L * length(REVISION_V2_METHOD_ORDER) *
    length(REVISION_V2_ALGORITHMS) * length(ids)
  complete_grid <- expand.grid(
    time_points = c(25L, 100L),
    method_id = REVISION_V2_METHOD_ORDER,
    community_algorithm = REVISION_V2_ALGORITHMS,
    estimand_id = ids,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  if (isTRUE(allow_undefined)) {
    undefined <- complete_grid$time_points == 25L &
      complete_grid$method_id == "gimme" &
      complete_grid$estimand_id %in% c(
        "adjusted_rand_index", "variation_information",
        "assignment_accuracy_conditional"
      )
    complete_grid <- complete_grid[!undefined, , drop = FALSE]
  }
  key_columns <- c(
    "time_points", "method_id", "community_algorithm", "estimand_id"
  )
  expected_keys <- do.call(paste, c(complete_grid[key_columns], sep = "\r"))
  observed_keys <- do.call(paste, c(output[key_columns], sep = "\r"))
  if ((!allow_undefined && nrow(output) != expected_rows) ||
      !setequal(observed_keys, expected_keys)) {
    stop(label, " does not form the complete horizon-method-readout grid.", call. = FALSE)
  }
  output[order(
    -output$time_points,
    match(output$method_id, REVISION_V2_METHOD_ORDER),
    match(output$community_algorithm, REVISION_V2_ALGORITHMS),
    output$estimand_id
  ), , drop = FALSE]
}

revision_v2_prepare_factor_screen <- function(estimands) {
  data <- revision_v2_nonempty(revision_v2_select(estimands, list(
    analysis_scope = "matched",
    summary_level = "stability_horizon",
    community_algorithm = REVISION_V2_ALGORITHMS,
    estimand_id = "p_usable_and_correct"
  )), "the stability-by-horizon factor screen")
  revision_v2_validate_intervals(data, "Factor-screen estimates", TRUE)
  output <- data.frame(
    horizon_design = revision_v2_horizon_design(data$TimeP),
    time_points = as.integer(data$TimeP),
    method_id = revision_v2_normalize_method(data$method_id),
    method_label = revision_v2_method_label(revision_v2_normalize_method(data$method_id)),
    community_algorithm = tolower(data$community_algorithm),
    stability_class = data$stability_class,
    estimand_id = data$estimand_id,
    estimate = as.numeric(data$estimate),
    conf_low = as.numeric(data$conf_low),
    conf_high = as.numeric(data$conf_high),
    n_panels = as.integer(data$n_panels),
    bootstrap_reps = as.integer(data$bootstrap_reps),
    stringsAsFactors = FALSE
  )
  revision_v2_assert_unique(
    output,
    c("time_points", "method_id", "community_algorithm", "stability_class"),
    "Factor-screen table"
  )
  expected_rows <- 2L * length(REVISION_V2_METHOD_ORDER) *
    length(REVISION_V2_ALGORITHMS) * 2L
  if (nrow(output) != expected_rows) {
    stop("Factor-screen estimates do not form the complete design grid.", call. = FALSE)
  }
  output
}

revision_v2_prepare_recovery <- function(method_rows, condition_key) {
  matched <- revision_v2_nonempty(
    method_rows[method_rows$analysis_scope == "matched", , drop = FALSE],
    "scenario-specific matched recovery"
  )
  output <- revision_v2_split_apply(
    matched,
    c("TimeP", "method_id", "community_algorithm", "scenario_family"),
    function(group, key) {
      interval <- revision_v2_mean_interval(
        group$usable_and_correct,
        group$panel_key,
        paste("recovery", key, sep = "|")
      )
      data.frame(
        time_points = group$TimeP[[1]],
        method_id = group$method_id[[1]],
        community_algorithm = group$community_algorithm[[1]],
        scenario_family = group$scenario_family[[1]],
        estimate = interval[["estimate"]],
        conf_low = interval[["conf_low"]],
        conf_high = interval[["conf_high"]],
        n_panels = as.integer(interval[["n_panels"]]),
        bootstrap_reps = as.integer(interval[["bootstrap_reps"]]),
        stringsAsFactors = FALSE
      )
    }
  )
  output <- merge(output, condition_key, by = "scenario_family", sort = FALSE)
  output$horizon_design <- revision_v2_horizon_design(output$time_points)
  output$method_label <- revision_v2_method_label(output$method_id)
  output$estimand_id <- "p_usable_and_correct"
  revision_v2_validate_intervals(output, "Scenario recovery estimates", TRUE)
  observed_algorithms <- unique(output$community_algorithm)
  if (!setequal(observed_algorithms, REVISION_V2_ALGORITHMS)) {
    stop("Scenario recovery does not retain all three community readouts.", call. = FALSE)
  }
  expected_rows <- nrow(condition_key) * 2L * length(REVISION_V2_METHOD_ORDER) *
    length(REVISION_V2_ALGORITHMS)
  revision_v2_assert_unique(
    output,
    c("time_points", "method_id", "community_algorithm", "scenario_family"),
    "Scenario recovery estimates"
  )
  if (nrow(output) != expected_rows) {
    stop("Scenario recovery estimates do not form the complete design grid.", call. = FALSE)
  }
  output
}

revision_v2_pair_key <- function(data) {
  columns <- c(
    "scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "Q", "rep",
    "method_id", "community_algorithm"
  )
  do.call(paste, c(data[columns], sep = "\r"))
}

revision_v2_prepare_challenge_effects <- function(method_rows) {
  challenge <- revision_v2_nonempty(
    method_rows[method_rows$analysis_scope == "challenge", , drop = FALSE],
    "paired challenge effects"
  )
  challenge$pair_key <- revision_v2_pair_key(challenge)
  ideal <- challenge[
    challenge$challenge_id == "ideal_baseline" & challenge$representation == "linear_z",
    c("pair_key", "usable_and_correct"),
    drop = FALSE
  ]
  revision_v2_assert_unique(ideal, "pair_key", "Ideal challenge rows")
  variants <- challenge[challenge$challenge_id != "ideal_baseline", , drop = FALSE]
  groups <- split(
    seq_len(nrow(variants)),
    do.call(paste, c(
      variants[c("method_id", "community_algorithm", "challenge_id", "representation")],
      sep = "\r"
    )),
    drop = TRUE
  )
  rows <- lapply(names(groups), function(key) {
    left <- variants[groups[[key]], , drop = FALSE]
    revision_v2_assert_unique(left, "pair_key", paste("Challenge group", key))
    paired <- merge(
      left[, c(
        "pair_key", "method_id", "community_algorithm", "challenge_id",
        "representation", "usable_and_correct"
      )],
      ideal,
      by = "pair_key",
      suffixes = c("_challenge", "_ideal"),
      sort = FALSE
    )
    if (nrow(paired) != nrow(left)) {
      stop("Challenge group lacks a one-to-one ideal-baseline match: ", key, call. = FALSE)
    }
    difference <- paired$usable_and_correct_challenge - paired$usable_and_correct_ideal
    interval <- revision_v2_mean_interval(
      difference,
      paired$pair_key,
      paste("challenge", key, sep = "|")
    )
    data.frame(
      method_id = left$method_id[[1]],
      method_label = revision_v2_method_label(left$method_id[[1]]),
      community_algorithm = left$community_algorithm[[1]],
      challenge_id = left$challenge_id[[1]],
      challenge_label = revision_v2_humanize(left$challenge_id[[1]]),
      representation = left$representation[[1]],
      estimand_id = "p_usable_and_correct_difference",
      estimate = interval[["estimate"]],
      conf_low = interval[["conf_low"]],
      conf_high = interval[["conf_high"]],
      n_pairs = as.integer(interval[["n_panels"]]),
      bootstrap_reps = as.integer(interval[["bootstrap_reps"]]),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output$challenge_label[
    output$challenge_id == "softmax_bounded" & output$representation == "softmax_raw"
  ] <- "Softmax bounded (raw)"
  output$challenge_label[
    output$challenge_id == "softmax_bounded" & output$representation == "softmax_clr"
  ] <- "Softmax bounded (CLR)"
  revision_v2_validate_intervals(output, "Paired challenge effects")
  effect_values <- unlist(output[c("estimate", "conf_low", "conf_high")], use.names = FALSE)
  if (any(effect_values < -1 - 1e-12 | effect_values > 1 + 1e-12)) {
    stop("Paired probability effects fall outside [-1, 1].", call. = FALSE)
  }
  observed_softmax <- sort(unique(output$representation[output$challenge_id == "softmax_bounded"]))
  if (!identical(observed_softmax, c("softmax_clr", "softmax_raw")) ||
      !setequal(unique(output$community_algorithm), REVISION_V2_ALGORITHMS)) {
    stop("Challenge effects omit a softmax representation or community readout.", call. = FALSE)
  }
  expected_rows <- 7L * 4L * length(REVISION_V2_ALGORITHMS)
  revision_v2_assert_unique(
    output,
    c("method_id", "community_algorithm", "challenge_id", "representation"),
    "Paired challenge effects"
  )
  if (nrow(output) != expected_rows) {
    stop("Challenge effects do not form the complete method-readout grid.", call. = FALSE)
  }
  output
}

revision_v2_unique_fits <- function(method_rows) {
  matched <- revision_v2_nonempty(
    method_rows[method_rows$analysis_scope == "matched", , drop = FALSE],
    "matched fit-level feasibility"
  )
  if (any(is.na(matched$structure_fit_key) | !nzchar(matched$structure_fit_key))) {
    stop("Matched readouts require nonempty structure-fit keys.", call. = FALSE)
  }
  first <- !duplicated(matched$structure_fit_key)
  fit_keys <- matched$structure_fit_key[first]
  first_index <- match(matched$structure_fit_key, fit_keys)
  if (nrow(matched) != length(fit_keys) * length(REVISION_V2_ALGORITHMS) ||
      anyDuplicated(paste(matched$structure_fit_key, matched$community_algorithm, sep = "\r"))) {
    stop("Shared structure fits require exactly three unique community readouts.", call. = FALSE)
  }
  identity_columns <- c(
    "panel_key", ".source_job", "TimeP", "method_id", "structure_fit_status"
  )
  inconsistent <- vapply(identity_columns, function(column) {
    observed <- as.character(matched[[column]])
    expected <- as.character(matched[[column]][first])[first_index]
    any(is.na(observed) | !nzchar(observed) | observed != expected)
  }, logical(1))
  if (any(inconsistent)) {
    stop(
      "Shared structure-fit identity differs across readouts: ",
      paste(identity_columns[inconsistent], collapse = ", "),
      call. = FALSE
    )
  }
  fit_status <- matched$structure_fit_status[first]
  if (any(!fit_status %in% c("ok", "error", "timeout"))) {
    stop("Shared structure fits contain an invalid status.", call. = FALSE)
  }
  runtime <- suppressWarnings(as.numeric(matched$structure_fit_runtime_sec))
  if (any(!is.finite(runtime)) || any(runtime < 0)) {
    stop("Shared structure fits require finite nonnegative runtimes.", call. = FALSE)
  }
  expected_runtime <- runtime[first][first_index]
  if (any(abs(runtime - expected_runtime) > 1e-8)) {
    stop("Structure-fit runtime differs across shared readouts.", call. = FALSE)
  }
  fit_runtime <- runtime[first]
  data.frame(
    structure_fit_key = fit_keys,
    panel_key = matched$panel_key[first],
    source_job = matched$.source_job[first],
    time_points = matched$TimeP[first],
    method_id = matched$method_id[first],
    fit_status = fit_status,
    runtime_sec = fit_runtime,
    completion_within_900 = as.numeric(fit_status == "ok" & fit_runtime <= 900),
    stringsAsFactors = FALSE
  )
}

revision_v2_prepare_runtime <- function(method_rows) {
  fits <- revision_v2_unique_fits(method_rows)
  output <- revision_v2_split_apply(
    fits,
    c("time_points", "method_id"),
    function(group, key) {
      completion <- revision_v2_mean_interval(
        group$completion_within_900,
        group$panel_key,
        paste("runtime", key, sep = "|")
      )
      successful <- group$runtime_sec[group$fit_status == "ok"]
      if (any(successful < 0)) {
        stop("Runtime summaries require nonnegative successful-fit times: ", key, call. = FALSE)
      }
      has_runtime <- length(successful) > 0L
      data.frame(
        horizon_design = revision_v2_horizon_design(group$time_points[[1]]),
        time_points = group$time_points[[1]],
        method_id = group$method_id[[1]],
        method_label = revision_v2_method_label(group$method_id[[1]]),
        n_structure_fits = nrow(group),
        n_successful_fits = length(successful),
        runtime_summary_status = if (has_runtime) {
          "estimated_successful_fits"
        } else {
          "not_estimable_no_successful_fits"
        },
        completion_estimate = completion[["estimate"]],
        completion_conf_low = completion[["conf_low"]],
        completion_conf_high = completion[["conf_high"]],
        runtime_median_sec = if (has_runtime) stats::median(successful) else NA_real_,
        runtime_q1_sec = if (has_runtime) {
          stats::quantile(successful, 0.25, names = FALSE, type = 8)
        } else {
          NA_real_
        },
        runtime_q3_sec = if (has_runtime) {
          stats::quantile(successful, 0.75, names = FALSE, type = 8)
        } else {
          NA_real_
        },
        bootstrap_reps = as.integer(completion[["bootstrap_reps"]]),
        stringsAsFactors = FALSE
      )
    }
  )
  probability <- unlist(output[c(
    "completion_estimate", "completion_conf_low", "completion_conf_high"
  )], use.names = FALSE)
  runtime <- unlist(output[c(
    "runtime_median_sec", "runtime_q1_sec", "runtime_q3_sec"
  )], use.names = FALSE)
  runtime_available <- output$runtime_summary_status == "estimated_successful_fits"
  if (any(!is.finite(probability)) || any(probability < 0 | probability > 1) ||
      any(!is.finite(runtime[rep(runtime_available, 3L)])) ||
      any(runtime[rep(runtime_available, 3L)] < 0) ||
      any(is.finite(runtime[rep(!runtime_available, 3L)])) ||
      any(!runtime_available & output$n_successful_fits != 0L) ||
      any(output$completion_conf_low > output$completion_estimate) ||
      any(output$completion_estimate > output$completion_conf_high)) {
    stop("Fit-level runtime table violates its probability or runtime contract.", call. = FALSE)
  }
  if (nrow(output) != 2L * length(REVISION_V2_METHOD_ORDER)) {
    stop("Fit-level runtime table omits a method-horizon group.", call. = FALSE)
  }
  output[order(
    -output$time_points,
    match(output$method_id, REVISION_V2_METHOD_ORDER)
  ), , drop = FALSE]
}

revision_v2_prepare_accounting <- function(method_rows) {
  fits <- revision_v2_unique_fits(method_rows)
  matched <- method_rows[method_rows$analysis_scope == "matched", , drop = FALSE]
  group_columns <- c(".source_job", "analysis_scope", "TimeP", "method_id")
  output <- revision_v2_split_apply(matched, group_columns, function(group, key) {
    group_fits <- fits[
      fits$source_job == group$.source_job[[1]] &
        fits$time_points == group$TimeP[[1]] &
        fits$method_id == group$method_id[[1]],
      ,
      drop = FALSE
    ]
    data.frame(
      source_job = group$.source_job[[1]],
      analysis_scope = group$analysis_scope[[1]],
      horizon_design = revision_v2_horizon_design(group$TimeP[[1]]),
      time_points = group$TimeP[[1]],
      method_id = group$method_id[[1]],
      readout_rows = nrow(group),
      structure_fits = nrow(group_fits),
      ok_readouts = sum(group$status == "ok"),
      error_readouts = sum(group$status == "error"),
      timeout_readouts = sum(group$status == "timeout"),
      community_algorithms = paste(sort(unique(group$community_algorithm)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  if (any(output$readout_rows != output$structure_fits * length(REVISION_V2_ALGORITHMS))) {
    stop("Fit/readout accounting does not contain exactly three readouts per fit.", call. = FALSE)
  }
  expected_groups <- expand.grid(
    time_points = c(25L, 100L),
    method_id = REVISION_V2_METHOD_ORDER,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  expected_keys <- paste(
    expected_groups$time_points,
    expected_groups$method_id,
    sep = "\r"
  )
  observed_keys <- paste(output$time_points, output$method_id, sep = "\r")
  if (!setequal(observed_keys, expected_keys) ||
      any(output$ok_readouts + output$error_readouts + output$timeout_readouts !=
        output$readout_rows) ||
      any(output$community_algorithms != "leiden;louvain;walktrap")) {
    stop("Fit/readout accounting violates its complete status grid.", call. = FALSE)
  }
  output
}

revision_v2_validate_table <- function(data, filename) {
  schema <- revision_v2_table_schemas()[[filename]]
  if (is.null(schema) || !identical(names(data), schema)) {
    stop("Table schema mismatch for ", filename, ".", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("Table is empty: ", filename, call. = FALSE)
  }
  permitted_missing <- character()
  if (identical(filename, "table3_runtime_feasibility.csv")) {
    runtime_columns <- c("runtime_median_sec", "runtime_q1_sec", "runtime_q3_sec")
    available <- data$runtime_summary_status == "estimated_successful_fits"
    unavailable <- data$runtime_summary_status == "not_estimable_no_successful_fits"
    if (any(is.na(available) | (!available & !unavailable)) ||
        any(!vapply(data[runtime_columns], is.numeric, logical(1))) ||
        any(!is.finite(unlist(data[available, runtime_columns, drop = FALSE]))) ||
        any(is.finite(unlist(data[unavailable, runtime_columns, drop = FALSE]))) ||
        any(unavailable & data$n_successful_fits != 0L)) {
      stop("Runtime table has an invalid conditional-summary state.", call. = FALSE)
    }
    permitted_missing <- runtime_columns
  }
  invalid <- vapply(data, function(column) {
    if (is.numeric(column)) {
      any(!is.finite(column))
    } else if (is.character(column)) {
      any(is.na(column) | !nzchar(column))
    } else {
      TRUE
    }
  }, logical(1))
  invalid[intersect(names(invalid), permitted_missing)] <- FALSE
  if (any(invalid)) {
    stop(
      "Table contains missing, non-finite, or unsupported fields: ",
      filename,
      " [",
      paste(names(data)[invalid], collapse = ", "),
      "]",
      call. = FALSE
    )
  }
  if ("bootstrap_reps" %in% names(data) &&
      any(data$bootstrap_reps != REVISION_V2_BOOTSTRAP_REPS)) {
    stop("Table does not use the prespecified 2,000 resamples: ", filename, call. = FALSE)
  }
  invisible(TRUE)
}

revision_v2_prepare_artifacts <- function(estimands, paired, long) {
  required_estimand <- c(
    "summary_level", "analysis_scope", "method_id", "community_algorithm",
    "stability_class", "TimeP", "estimand_id", "estimate", "conf_low",
    "conf_high", "n_panels", "bootstrap_reps", "empirical_family",
    "empirical_metric", "empirical_scale", "contribution"
  )
  required_pair <- c(
    "comparison_type", "left_label", "right_label", "community_algorithm",
    "estimand_id", "estimate", "conf_low", "conf_high"
  )
  if (length(setdiff(required_estimand, names(estimands))) > 0L ||
      length(setdiff(required_pair, names(paired))) > 0L) {
    stop("Aggregate estimand or paired-difference schema is incomplete.", call. = FALSE)
  }
  method_rows <- revision_v2_canonical_method_rows(long)
  condition_key <- revision_v2_prepare_condition_key(method_rows)
  empirical <- revision_v2_prepare_empirical(estimands)
  overall <- revision_v2_prepare_horizon_estimands(
    estimands,
    c("p_usable", "p_usable_and_correct"),
    "Method-level horizon estimates"
  )
  score <- revision_v2_prepare_horizon_estimands(
    estimands,
    c(
      "adjusted_rand_index", "variation_information",
      "assignment_accuracy_conditional", "score_correlation_marginal",
      "score_nrmse_marginal"
    ),
    "Score-quality horizon estimates",
    allow_undefined = TRUE
  )
  factor_screen <- revision_v2_prepare_factor_screen(estimands)
  recovery <- revision_v2_prepare_recovery(method_rows, condition_key)
  challenge <- revision_v2_prepare_challenge_effects(method_rows)
  runtime <- revision_v2_prepare_runtime(method_rows)
  accounting <- revision_v2_prepare_accounting(method_rows)
  tables <- list(
    empirical_realism_table.csv = empirical,
    table1_method_overall.csv = overall,
    figure5_condition_key.csv = condition_key,
    figure6_factor_effects_table.csv = challenge,
    figure6_factor_effects_eta_screen.csv = factor_screen,
    table2_score_quality_marginal_means.csv = score,
    table3_runtime_feasibility.csv = runtime,
    table3_runtime_feasibility_accounting.csv = accounting
  )
  for (filename in names(tables)) {
    revision_v2_validate_table(tables[[filename]], filename)
  }
  list(
    tables = tables,
    figure_data = list(
      empirical = empirical,
      recovery = recovery,
      challenge = challenge,
      runtime = runtime,
      condition_key = condition_key
    )
  )
}

revision_v2_metric_label <- function(metric) {
  labels <- c(
    channel_iqr = "Channel IQR",
    derivative_iqr = "Derivative IQR",
    effective_rank = "Effective rank",
    lag1_acf = "Lag-1 ACF",
    run_length_entropy = "Run-length entropy",
    spectral_entropy = "Spectral entropy"
  )
  result <- unname(labels[metric])
  result[is.na(result)] <- revision_v2_humanize(metric[is.na(result)])
  result
}

revision_v2_draw_discrepancy_panel <- function(data, title, max_only = FALSE) {
  if (max_only) {
    data <- data[data$component_id == "max_discrepancy", , drop = FALSE]
    labels <- revision_v2_metric_label(data$metric_id)
  } else {
    data <- data[data$component_id != "max_discrepancy", , drop = FALSE]
    short_component <- c(
      signed_scaled_median_difference = "Signed median",
      absolute_scaled_median_difference = "Absolute median",
      scaled_wasserstein_distance = "Wasserstein"
    )
    labels <- paste(
      revision_v2_metric_label(data$metric_id),
      unname(short_component[data$component_id]),
      sep = " - "
    )
  }
  order_index <- order(data$metric_id, data$component_id, decreasing = TRUE)
  data <- data[order_index, , drop = FALSE]
  labels <- labels[order_index]
  limits <- range(c(data$conf_low, data$conf_high, 0, 1), finite = TRUE)
  graphics::plot(
    NA,
    xlim = limits,
    ylim = c(0.5, nrow(data) + 0.5),
    yaxt = "n",
    ylab = "",
    xlab = "Scaled discrepancy",
    main = title,
    bty = "n"
  )
  shade <- function(left, right, color) {
    visible_left <- max(limits[[1]], left)
    visible_right <- min(limits[[2]], right)
    if (visible_left < visible_right) {
      graphics::rect(
        visible_left, 0.5, visible_right, nrow(data) + 0.5,
        col = color, border = NA
      )
    }
  }
  shade(-0.33, 0.33, "#009E7318")
  shade(-1, -0.33, "#E69F0018")
  shade(0.33, 1, "#E69F0018")
  shade(-Inf, -1, "#D55E0018")
  shade(1, Inf, "#D55E0018")
  thresholds <- c(-1, -0.33, 0.33, 1)
  graphics::abline(
    v = thresholds[thresholds >= limits[[1]] & thresholds <= limits[[2]]],
    lty = 3,
    col = "grey45"
  )
  y <- seq_len(nrow(data))
  graphics::segments(data$conf_low, y, data$conf_high, y, lwd = 1.25, col = "grey25")
  pch <- c(
    signed_scaled_median_difference = 21L,
    absolute_scaled_median_difference = 22L,
    scaled_wasserstein_distance = 24L,
    max_discrepancy = 23L
  )[data$component_id]
  graphics::points(data$estimate, y, pch = pch, bg = "#0072B2", col = "#003B5C")
  graphics::axis(2, at = y, labels = labels, las = 1, cex.axis = 0.68)
  graphics::axis(1)
}

revision_v2_plot_empirical <- function(data) {
  families <- c("observable_primary", "secondary_frozen_summary")
  layout(matrix(seq_len(4L), nrow = 2L, byrow = TRUE), heights = c(2.1, 1))
  graphics::par(oma = c(1.4, 0.4, 1.5, 0.2), family = "sans")
  for (family in families) {
    subset <- data[data$family_id == family, , drop = FALSE]
    graphics::par(mar = c(3.7, 12.2, 2.8, 0.8), cex = 0.82)
    revision_v2_draw_discrepancy_panel(
      subset,
      paste(unique(subset$family_label), "- components")
    )
  }
  for (family in families) {
    subset <- data[data$family_id == family, , drop = FALSE]
    graphics::par(mar = c(3.7, 8.2, 2.8, 0.8), cex = 0.82)
    revision_v2_draw_discrepancy_panel(
      subset,
      paste(unique(subset$family_label), "- maximum"),
      max_only = TRUE
    )
  }
  graphics::mtext(
    "Held-out empirical realism: 95% clip-cluster intervals",
    outer = TRUE,
    side = 3,
    line = 0.2,
    font = 2,
    cex = 1.05
  )
  graphics::mtext(
    "Heuristic magnitude bands: Small <=0.33; Intermediate >0.33 to 1; Large >1",
    outer = TRUE,
    side = 1,
    line = 0.2,
    cex = 0.8
  )
}

revision_v2_plot_recovery <- function(data, condition_key) {
  methods <- REVISION_V2_METHOD_ORDER
  horizons <- c(100L, 25L)
  layout(matrix(seq_len(length(methods) * length(horizons)), nrow = 2L, byrow = TRUE))
  graphics::par(oma = c(3.0, 3.5, 2.2, 0.4), family = "sans")
  for (time_points in horizons) {
    for (method in methods) {
      panel <- data[data$time_points == time_points & data$method_id == method, , drop = FALSE]
      graphics::par(mar = c(if (time_points == 25L) 4.0 else 1.3, 2.8, 2.5, 0.5), cex = 0.76)
      graphics::plot(
        NA,
        xlim = c(0.6, nrow(condition_key) + 0.4),
        ylim = c(0, 1),
        xaxt = "n",
        ylab = "",
        xlab = "",
        main = if (time_points == 100L) revision_v2_method_label(method) else "",
        bty = "n"
      )
      graphics::abline(h = c(0, 0.5, 1), col = "grey90", lty = 1)
      for (algorithm_index in seq_along(REVISION_V2_ALGORITHMS)) {
        algorithm <- REVISION_V2_ALGORITHMS[[algorithm_index]]
        values <- panel[panel$community_algorithm == algorithm, , drop = FALSE]
        values <- values[match(condition_key$condition_code, values$condition_code), , drop = FALSE]
        offset <- c(-0.16, 0, 0.16)[[algorithm_index]]
        x <- seq_len(nrow(condition_key)) + offset
        graphics::lines(
          x,
          values$estimate,
          lty = REVISION_V2_ALGORITHM_LTY[[algorithm]],
          col = REVISION_V2_COLORS[[method]],
          lwd = 1
        )
        graphics::segments(x, values$conf_low, x, values$conf_high, col = "grey35")
        graphics::points(
          x,
          values$estimate,
          pch = REVISION_V2_ALGORITHM_PCH[[algorithm]],
          bg = REVISION_V2_COLORS[[method]],
          col = "black",
          cex = 0.8
        )
      }
      if (time_points == 25L) {
        graphics::axis(1, at = seq_len(nrow(condition_key)), labels = condition_key$condition_code)
      }
      if (identical(method, methods[[1]])) {
        graphics::mtext(
          if (time_points == 100L) "Primary T=100" else "Stress T=25",
          side = 2,
          line = 2.0,
          font = 2
        )
      }
    }
  }
  graphics::mtext(
    "Status-inclusive probability of a usable and correct dimension",
    outer = TRUE,
    side = 2,
    line = 1.5
  )
  graphics::mtext("Matched recovery by scenario and readout", outer = TRUE, side = 3, line = 0.5, font = 2)
  graphics::legend(
    "bottom",
    inset = -0.01,
    xpd = NA,
    horiz = TRUE,
    legend = revision_v2_humanize(REVISION_V2_ALGORITHMS),
    pch = REVISION_V2_ALGORITHM_PCH,
    lty = REVISION_V2_ALGORITHM_LTY,
    pt.bg = "grey65",
    bty = "n",
    cex = 0.8
  )
}

revision_v2_challenge_order <- function(data) {
  preferred <- c(
    "independent_initialization", "smooth_activity_gate", "cross_loadings",
    "residual_equicorrelation", "latent_shock_equicorrelation"
  )
  labels <- c(
    independent_initialization = "Independent init.",
    smooth_activity_gate = "Smooth gate",
    cross_loadings = "Cross-loadings",
    residual_equicorrelation = "Residual corr.",
    latent_shock_equicorrelation = "Latent-shock corr."
  )
  key <- paste(data$challenge_id, data$representation, sep = "|")
  order_key <- c(
    paste(preferred, "linear_z", sep = "|"),
    "softmax_bounded|softmax_raw",
    "softmax_bounded|softmax_clr"
  )
  display <- c(unname(labels[preferred]), "Softmax raw", "Softmax CLR")
  list(key = order_key, display = display, observed = key)
}

revision_v2_plot_challenge <- function(data) {
  methods <- c("dynEGA", "graphicalVAR", "static_EGA", "oracle_blocks")
  challenge_order <- revision_v2_challenge_order(data)
  limits <- range(c(data$conf_low, data$conf_high, 0), finite = TRUE)
  padding <- max(0.03, diff(limits) * 0.05)
  limits <- limits + c(-padding, padding)
  layout(matrix(seq_len(4L), nrow = 2L, byrow = TRUE))
  graphics::par(oma = c(3.5, 4.0, 2.0, 0.4), family = "sans")
  for (method in methods) {
    panel <- data[data$method_id == method, , drop = FALSE]
    panel$key <- paste(panel$challenge_id, panel$representation, sep = "|")
    graphics::par(mar = c(4.1, 3.3, 2.6, 0.8), cex = 0.78)
    graphics::plot(
      NA,
      xlim = c(0.5, length(challenge_order$key) + 0.5),
      ylim = limits,
      xaxt = "n",
      ylab = "",
      xlab = "",
      main = revision_v2_method_label(method),
      bty = "n"
    )
    graphics::abline(h = 0, lty = 2, col = "grey35")
    for (algorithm_index in seq_along(REVISION_V2_ALGORITHMS)) {
      algorithm <- REVISION_V2_ALGORITHMS[[algorithm_index]]
      values <- panel[panel$community_algorithm == algorithm, , drop = FALSE]
      values <- values[match(challenge_order$key, values$key), , drop = FALSE]
      offset <- c(-0.16, 0, 0.16)[[algorithm_index]]
      x <- seq_along(challenge_order$key) + offset
      graphics::lines(
        x,
        values$estimate,
        col = REVISION_V2_COLORS[[method]],
        lty = REVISION_V2_ALGORITHM_LTY[[algorithm]],
        lwd = 1
      )
      graphics::segments(x, values$conf_low, x, values$conf_high, col = "grey30")
      graphics::points(
        x,
        values$estimate,
        pch = REVISION_V2_ALGORITHM_PCH[[algorithm]],
        bg = REVISION_V2_COLORS[[method]],
        col = "black"
      )
    }
    graphics::axis(
      1,
      at = seq_along(challenge_order$key),
      labels = challenge_order$display,
      las = 2,
      cex.axis = 0.7
    )
  }
  graphics::mtext(
    "Challenge minus ideal: probability of a usable and correct dimension",
    outer = TRUE,
    side = 2,
    line = 1.8
  )
  graphics::mtext("Paired measurement-challenge effects", outer = TRUE, side = 3, line = 0.4, font = 2)
  graphics::legend(
    "bottom",
    inset = -0.02,
    xpd = NA,
    horiz = TRUE,
    legend = revision_v2_humanize(REVISION_V2_ALGORITHMS),
    pch = REVISION_V2_ALGORITHM_PCH,
    lty = REVISION_V2_ALGORITHM_LTY,
    pt.bg = "grey65",
    bty = "n",
    cex = 0.8
  )
}

revision_v2_plot_runtime <- function(data) {
  methods <- REVISION_V2_METHOD_ORDER
  positions <- rev(seq_along(methods))
  color <- REVISION_V2_COLORS[data$method_id]
  point <- ifelse(data$time_points == 100L, 21L, 24L)
  offset <- ifelse(data$time_points == 100L, 0.13, -0.13)
  y <- positions[match(data$method_id, methods)] + offset
  layout(matrix(1:2, nrow = 1L), widths = c(1, 1.1))
  graphics::par(oma = c(1.2, 1.0, 2.1, 0.3), family = "sans")
  graphics::par(mar = c(4.2, 7.2, 2.6, 0.8), cex = 0.85)
  graphics::plot(
    NA,
    xlim = c(0, 1),
    ylim = c(0.5, length(methods) + 0.5),
    yaxt = "n",
    ylab = "",
    xlab = "Completion within 900 seconds",
    main = "Completion probability",
    bty = "n"
  )
  graphics::abline(v = c(0, 0.5, 1), col = "grey90")
  graphics::segments(data$completion_conf_low, y, data$completion_conf_high, y, col = "grey30")
  graphics::points(data$completion_estimate, y, pch = point, bg = color, col = "black", cex = 1.05)
  graphics::axis(2, at = positions, labels = revision_v2_method_label(methods), las = 1)

  graphics::par(mar = c(4.2, 2.0, 2.6, 1.0), cex = 0.85)
  clock_floor <- 0.001
  runtime_available <- data$runtime_summary_status == "estimated_successful_fits"
  runtime_q1 <- pmax(data$runtime_q1_sec[runtime_available], clock_floor)
  runtime_q3 <- pmax(data$runtime_q3_sec[runtime_available], clock_floor)
  runtime_median <- pmax(data$runtime_median_sec[runtime_available], clock_floor)
  runtime_y <- y[runtime_available]
  limits <- range(c(runtime_q1, runtime_q3), finite = TRUE)
  graphics::plot(
    NA,
    xlim = limits,
    ylim = c(0.5, length(methods) + 0.5),
    log = "x",
    yaxt = "n",
    ylab = "",
    xlab = "Runtime (seconds; log scale)",
    main = "Successful-fit median and IQR",
    bty = "n"
  )
  graphics::segments(runtime_q1, runtime_y, runtime_q3, runtime_y, col = "grey30", lwd = 1.5)
  graphics::points(
    runtime_median,
    runtime_y,
    pch = point[runtime_available],
    bg = color[runtime_available],
    col = "black",
    cex = 1.05
  )
  if (any(!runtime_available)) {
    graphics::text(
      rep(limits[[1]], sum(!runtime_available)),
      y[!runtime_available],
      labels = "No successful fits",
      pos = 4,
      cex = 0.72
    )
  }
  graphics::legend(
    "bottomright",
    legend = c("Primary T=100", "Stress T=25"),
    pch = c(21L, 24L),
    pt.bg = "grey65",
    bty = "n",
    cex = 0.8
  )
  graphics::mtext(
    "Fit-level feasibility (each shared structure fit counted once)",
    outer = TRUE,
    side = 3,
    line = 0.5,
    font = 2
  )
  graphics::mtext(
    "Zero-duration clock readings are displayed at 0.001 seconds.",
    outer = TRUE,
    side = 1,
    line = 0.1,
    cex = 0.75
  )
}

revision_v2_write_figure <- function(stem, output_root, width, height, plotter) {
  figure_dir <- file.path(output_root, "figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figure_dir, paste0(stem, ".png"))
  grDevices::pdf(pdf_path, width = width, height = height, family = "sans", useDingbats = FALSE)
  tryCatch(plotter(), finally = grDevices::dev.off())
  grDevices::png(
    png_path,
    width = width,
    height = height,
    units = "in",
    res = 600,
    type = "cairo",
    pointsize = 9
  )
  tryCatch(plotter(), finally = grDevices::dev.off())
  invisible(c(pdf_path, png_path))
}

revision_v2_write_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  invisible(path)
}

revision_v2_uint32 <- function(bytes) {
  if (length(bytes) != 4L) {
    stop("PNG integer fields must contain exactly four bytes.", call. = FALSE)
  }
  sum(as.double(as.integer(bytes)) * c(16777216, 65536, 256, 1))
}

revision_v2_png_resolution <- function(path) {
  size <- file.info(path)$size
  bytes <- readBin(path, what = "raw", n = size)
  signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  if (length(bytes) < 8L || !identical(bytes[seq_len(8L)], signature)) {
    stop("Generated PNG has an invalid signature: ", path, call. = FALSE)
  }
  position <- 9L
  while (position + 11L <= length(bytes)) {
    chunk_length <- revision_v2_uint32(bytes[position:(position + 3L)])
    data_start <- position + 8L
    data_end <- data_start + chunk_length - 1L
    crc_end <- data_end + 4L
    if (crc_end > length(bytes)) {
      stop("Generated PNG contains a truncated chunk: ", path, call. = FALSE)
    }
    chunk_type <- rawToChar(bytes[(position + 4L):(position + 7L)])
    if (identical(chunk_type, "pHYs")) {
      if (chunk_length != 9L) {
        stop("Generated PNG has an invalid pHYs chunk: ", path, call. = FALSE)
      }
      chunk <- bytes[data_start:data_end]
      if (as.integer(chunk[[9L]]) != 1L) {
        stop("Generated PNG resolution is not expressed per metre: ", path, call. = FALSE)
      }
      pixels_per_metre <- c(
        x = revision_v2_uint32(chunk[1:4]),
        y = revision_v2_uint32(chunk[5:8])
      )
      return(pixels_per_metre / 39.37007874015748)
    }
    position <- as.integer(crc_end + 1L)
  }
  stop("Generated PNG omits physical-resolution metadata: ", path, call. = FALSE)
}

revision_v2_validate_generated_tree <- function(root) {
  table_paths <- file.path(root, "tables", names(revision_v2_table_schemas()))
  figure_names <- as.vector(outer(revision_v2_figure_stems(), c("pdf", "png"), paste, sep = "."))
  figure_paths <- file.path(root, "figures", figure_names)
  paths <- c(table_paths, figure_paths)
  invalid <- paths[!file.exists(paths) | file.info(paths)$size <= 0L]
  if (length(invalid) > 0L) {
    stop("Generated artifact tree is incomplete: ", paste(basename(invalid), collapse = ", "), call. = FALSE)
  }
  for (filename in names(revision_v2_table_schemas())) {
    data <- utils::read.csv(
      file.path(root, "tables", filename),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    revision_v2_validate_table(data, filename)
  }
  pdf_magic <- vapply(file.path(root, "figures", paste0(revision_v2_figure_stems(), ".pdf")), function(path) {
    identical(readChar(path, 4L, useBytes = TRUE), "%PDF")
  }, logical(1))
  png_paths <- file.path(
    root,
    "figures",
    paste0(revision_v2_figure_stems(), ".png")
  )
  png_magic <- vapply(png_paths, function(path) {
    identical(readBin(path, "raw", n = 8L), as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  }, logical(1))
  if (!all(pdf_magic) || !all(png_magic)) {
    stop("Generated figure signature validation failed.", call. = FALSE)
  }
  png_resolution <- vapply(
    png_paths,
    revision_v2_png_resolution,
    numeric(2L)
  )
  if (any(!is.finite(png_resolution)) || any(png_resolution < 450)) {
    stop("Generated PNG resolution is below 450 DPI.", call. = FALSE)
  }
  invisible(paths)
}

revision_v2_publish_tree <- function(staging_root, output_root) {
  parent <- dirname(output_root)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  backup <- paste0(output_root, ".previous-", Sys.getpid())
  if (file.exists(backup)) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  had_previous <- dir.exists(output_root)
  if (had_previous && !file.rename(output_root, backup)) {
    stop("Could not preserve the prior publication artifact tree.", call. = FALSE)
  }
  installed <- file.rename(staging_root, output_root)
  if (!installed) {
    if (had_previous) {
      file.rename(backup, output_root)
    }
    stop("Could not atomically install the rebuilt artifact tree.", call. = FALSE)
  }
  if (had_previous) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  invisible(output_root)
}

revision_v2_render_prepared <- function(prepared, output_root) {
  for (filename in names(prepared$tables)) {
    revision_v2_write_csv(
      prepared$tables[[filename]],
      file.path(output_root, "tables", filename)
    )
  }
  figures <- prepared$figure_data
  revision_v2_write_figure(
    "figure4_empirical_realism", output_root, 12, 9,
    function() revision_v2_plot_empirical(figures$empirical)
  )
  revision_v2_write_figure(
    "figure5_dimension_recovery", output_root, 13, 7.5,
    function() revision_v2_plot_recovery(figures$recovery, figures$condition_key)
  )
  revision_v2_write_figure(
    "figure6_factor_effects", output_root, 11, 8,
    function() revision_v2_plot_challenge(figures$challenge)
  )
  revision_v2_write_figure(
    "figure7_runtime_feasibility", output_root, 10, 5.8,
    function() revision_v2_plot_runtime(figures$runtime)
  )
  revision_v2_validate_generated_tree(output_root)
}

revision_v2_build <- function(aggregate_dir, output_root) {
  revision_v2_require_aggregate_contract(aggregate_dir)
  estimands <- revision_v2_read_csv(
    aggregate_dir,
    "revision_estimand_results.csv",
    c(
      "summary_level", "analysis_scope", "method_id", "community_algorithm",
      "stability_class", "TimeP", "estimand_id", "estimate", "conf_low",
      "conf_high", "n_panels", "bootstrap_reps", "empirical_family",
      "empirical_metric", "empirical_scale", "contribution"
    )
  )
  paired <- revision_v2_read_csv(
    aggregate_dir,
    "revision_paired_differences.csv",
    c(
      "comparison_type", "left_label", "right_label", "community_algorithm",
      "estimand_id", "estimate", "conf_low", "conf_high"
    )
  )
  long <- revision_v2_read_csv(
    aggregate_dir,
    "revision_results_long.csv",
    revision_v2_method_columns()
  )
  prepared <- revision_v2_prepare_artifacts(estimands, paired, long)
  parent <- dirname(output_root)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  staging_root <- tempfile(
    pattern = paste0(".", basename(output_root), "-staging-"),
    tmpdir = parent
  )
  if (!dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create artifact staging directory.", call. = FALSE)
  }
  on.exit({
    if (dir.exists(staging_root)) {
      unlink(staging_root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  revision_v2_render_prepared(prepared, staging_root)
  revision_v2_publish_tree(staging_root, output_root)
}

revision_v2_fixture_estimands <- function() {
  empirical <- expand.grid(
    empirical_family = c("observable_primary", "secondary_frozen_summary"),
    empirical_metric = c(
      "channel_iqr", "derivative_iqr", "effective_rank", "lag1_acf",
      "run_length_entropy", "spectral_entropy"
    ),
    contribution = c(
      "signed_scaled_median_difference", "absolute_scaled_median_difference",
      "scaled_wasserstein_distance", "max_discrepancy"
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  empirical$summary_level <- "heldout_clip_cluster"
  empirical$analysis_scope <- "empirical_cv"
  empirical$method_id <- NA_character_
  empirical$community_algorithm <- NA_character_
  empirical$stability_class <- NA_character_
  empirical$TimeP <- NA_integer_
  empirical$estimand_id <- "empirical_discrepancy"
  empirical$estimate <- seq(0.1, 0.8, length.out = nrow(empirical))
  empirical$conf_low <- pmax(0, empirical$estimate - 0.05)
  empirical$conf_high <- empirical$estimate + 0.05
  empirical$n_panels <- 20L
  empirical$bootstrap_reps <- REVISION_V2_BOOTSTRAP_REPS
  empirical$empirical_scale <- 1

  horizon_ids <- c(
    "p_usable", "p_correct_given_usable", "p_usable_and_correct",
    "signed_dimension_error", "absolute_dimension_error", "adjusted_rand_index",
    "variation_information", "assignment_accuracy_conditional",
    "score_correlation_marginal", "score_nrmse_marginal"
  )
  horizon <- expand.grid(
    TimeP = c(25L, 100L),
    method_id = REVISION_V2_METHOD_ORDER,
    community_algorithm = REVISION_V2_ALGORITHMS,
    estimand_id = horizon_ids,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  horizon$summary_level <- "horizon"
  horizon$analysis_scope <- "matched"
  horizon$stability_class <- NA_character_
  probability <- horizon$estimand_id %in% c(
    "p_usable", "p_correct_given_usable", "p_usable_and_correct",
    "adjusted_rand_index", "assignment_accuracy_conditional", "score_correlation_marginal"
  )
  horizon$estimate <- ifelse(probability, 0.7, 0.2)
  horizon$conf_low <- horizon$estimate - 0.05
  horizon$conf_high <- horizon$estimate + 0.05
  horizon$n_panels <- 20L
  horizon$bootstrap_reps <- REVISION_V2_BOOTSTRAP_REPS
  horizon$empirical_family <- NA_character_
  horizon$empirical_metric <- NA_character_
  horizon$empirical_scale <- NA_real_
  horizon$contribution <- NA_character_

  screen <- expand.grid(
    TimeP = c(25L, 100L),
    method_id = REVISION_V2_METHOD_ORDER,
    community_algorithm = REVISION_V2_ALGORITHMS,
    stability_class = c("asymptotically_stable", "anti_damped"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  screen$summary_level <- "stability_horizon"
  screen$analysis_scope <- "matched"
  screen$estimand_id <- "p_usable_and_correct"
  screen$estimate <- 0.65
  screen$conf_low <- 0.55
  screen$conf_high <- 0.75
  screen$n_panels <- 20L
  screen$bootstrap_reps <- REVISION_V2_BOOTSTRAP_REPS
  screen$empirical_family <- NA_character_
  screen$empirical_metric <- NA_character_
  screen$empirical_scale <- NA_real_
  screen$contribution <- NA_character_
  rbind(empirical, horizon, screen)
}

revision_v2_fixture_long <- function() {
  scenarios <- data.frame(
    scenario = paste0("scenario_", seq_len(6L)),
    scenario_family = paste0("family_", seq_len(6L)),
    stability_class = rep(c("asymptotically_stable", "anti_damped"), each = 3L),
    stringsAsFactors = FALSE
  )
  matched <- expand.grid(
    scenario_index = seq_len(nrow(scenarios)),
    TimeP = c(25L, 100L),
    method_id = REVISION_V2_METHOD_ORDER,
    community_algorithm = REVISION_V2_ALGORITHMS,
    rep = 1:2,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  matched$scenario <- scenarios$scenario[matched$scenario_index]
  matched$scenario_family <- scenarios$scenario_family[matched$scenario_index]
  matched$stability_class <- scenarios$stability_class[matched$scenario_index]
  matched$challenge_id <- "none"
  matched$representation <- "linear_z"
  matched$.source_job <- paste0("t", matched$TimeP, "_fixture")
  matched$panel_key <- paste(
    matched$scenario, matched$TimeP, matched$rep, sep = "|"
  )

  variants <- data.frame(
    challenge_id = c(
      "ideal_baseline", "independent_initialization", "smooth_activity_gate",
      "cross_loadings", "residual_equicorrelation",
      "latent_shock_equicorrelation", "softmax_bounded", "softmax_bounded"
    ),
    representation = c(rep("linear_z", 6L), "softmax_raw", "softmax_clr"),
    stringsAsFactors = FALSE
  )
  challenge <- expand.grid(
    scenario_index = seq_len(nrow(scenarios)),
    method_id = setdiff(REVISION_V2_METHOD_ORDER, "gimme"),
    community_algorithm = REVISION_V2_ALGORITHMS,
    rep = 1:2,
    variant_index = seq_len(nrow(variants)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  challenge$scenario <- scenarios$scenario[challenge$scenario_index]
  challenge$scenario_family <- scenarios$scenario_family[challenge$scenario_index]
  challenge$stability_class <- scenarios$stability_class[challenge$scenario_index]
  challenge$challenge_id <- variants$challenge_id[challenge$variant_index]
  challenge$representation <- variants$representation[challenge$variant_index]
  challenge$TimeP <- 100L
  challenge$.source_job <- "challenge_fixture"
  challenge$panel_key <- paste(
    challenge$scenario, challenge$challenge_id, challenge$representation,
    challenge$rep, sep = "|"
  )

  data <- rbind(
    matched[, setdiff(names(matched), "scenario_index"), drop = FALSE],
    challenge[, setdiff(names(challenge), c("scenario_index", "variant_index")), drop = FALSE]
  )
  data$N <- 100L
  data$NVAR <- 4L
  data$Error_sd <- 0.25
  data$Loadings <- 0.6
  data$Q <- 0.75
  data$n_dim_true <- 2L
  data$n_dim_hat <- 2L
  data$status <- "ok"
  data$structure_fit_status <- "ok"
  data$structure_fit_runtime_sec <- ifelse(data$method_id == "gimme", 30, 5)
  data$runtime_sec <- data$structure_fit_runtime_sec + 0.1
  representation_key <- paste(data$panel_key, data$representation, sep = "|")
  data$structure_fit_key <- paste(representation_key, data$method_id, sep = "|")
  data$unit_key <- paste(data$structure_fit_key, data$community_algorithm, sep = "|")
  data
}

revision_v2_run_fixture_tests <- function() {
  paired <- data.frame(
    comparison_type = "challenge_minus_ideal",
    left_label = "fixture",
    right_label = "ideal",
    community_algorithm = "louvain",
    estimand_id = "p_usable_and_correct",
    estimate = 0,
    conf_low = -0.1,
    conf_high = 0.1,
    stringsAsFactors = FALSE
  )
  estimands <- revision_v2_fixture_estimands()
  undefined_score <- estimands$summary_level == "horizon" &
    estimands$TimeP == 25L & estimands$method_id == "gimme" &
    estimands$estimand_id %in% c(
      "adjusted_rand_index", "variation_information",
      "assignment_accuracy_conditional"
    )
  estimands[undefined_score, c("estimate", "conf_low", "conf_high")] <- NA_real_
  estimands$n_panels[undefined_score] <- 0L
  method_rows <- revision_v2_fixture_long()
  failed_gimme <- method_rows$TimeP == 25L & method_rows$method_id == "gimme"
  method_rows$status[failed_gimme] <- "error"
  method_rows$structure_fit_status[failed_gimme] <- "error"
  method_rows$n_dim_hat[failed_gimme] <- NA_real_
  method_rows$structure_fit_runtime_sec[failed_gimme] <- 900
  method_rows$runtime_sec[failed_gimme] <- 900
  prepared <- revision_v2_prepare_artifacts(estimands, paired, method_rows)
  stopifnot(
    identical(names(prepared$tables), names(revision_v2_table_schemas())),
    setequal(unique(prepared$figure_data$recovery$community_algorithm), REVISION_V2_ALGORITHMS),
    setequal(
      unique(prepared$figure_data$challenge$representation[
        prepared$figure_data$challenge$challenge_id == "softmax_bounded"
      ]),
      c("softmax_raw", "softmax_clr")
    ),
    all(
      prepared$tables$table3_runtime_feasibility_accounting.csv$readout_rows ==
        3L * prepared$tables$table3_runtime_feasibility_accounting.csv$structure_fits
    ),
    sum(
      prepared$tables$table3_runtime_feasibility.csv$runtime_summary_status ==
        "not_estimable_no_successful_fits"
    ) == 1L,
    !any(
      prepared$tables$table2_score_quality_marginal_means.csv$time_points == 25L &
        prepared$tables$table2_score_quality_marginal_means.csv$method_id == "gimme" &
        prepared$tables$table2_score_quality_marginal_means.csv$estimand_id %in% c(
          "adjusted_rand_index", "variation_information",
          "assignment_accuracy_conditional"
        )
    )
  )
  invalid <- prepared$tables$table1_method_overall.csv
  invalid$estimate[[1]] <- Inf
  failed <- tryCatch({
    revision_v2_validate_intervals(invalid, "Invalid fixture")
    FALSE
  }, error = function(error) TRUE)
  stopifnot(failed)
  runtime_path <- tempfile("revision-runtime-fixture-", fileext = ".csv")
  on.exit(unlink(runtime_path, force = TRUE), add = TRUE)
  revision_v2_write_csv(
    prepared$tables$table3_runtime_feasibility.csv,
    runtime_path
  )
  runtime_roundtrip <- utils::read.csv(
    runtime_path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  revision_v2_validate_table(
    runtime_roundtrip,
    "table3_runtime_feasibility.csv"
  )
  artifact_root <- tempfile("revision-artifact-fixture-")
  on.exit(unlink(artifact_root, recursive = TRUE, force = TRUE), add = TRUE)
  revision_v2_render_prepared(prepared, artifact_root)
  invisible(TRUE)
}

revision_v2_main <- function() {
  cli <- revision_v2_parse_cli(commandArgs(trailingOnly = TRUE))
  if (isTRUE(cli$fixture_test)) {
    revision_v2_run_fixture_tests()
    cat("revision-v2 artifact builder fixture tests: PASS\n")
    return(invisible(TRUE))
  }
  aggregate_dir <- normalizePath(cli$aggregate_dir, mustWork = TRUE)
  output_root <- normalizePath(cli$output_root, mustWork = FALSE)
  revision_v2_build(aggregate_dir, output_root)
}

if (sys.nframe() == 0L) {
  revision_v2_main()
}
