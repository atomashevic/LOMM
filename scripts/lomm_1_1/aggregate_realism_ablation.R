#!/usr/bin/env Rscript

pa_script_path <- function() {
  if (exists("LOMM11_PAIRED_AGGREGATOR_PATH", inherits = TRUE)) {
    return(normalizePath(
      get("LOMM11_PAIRED_AGGREGATOR_PATH", inherits = TRUE), mustWork = TRUE
    ))
  }
  frames <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  frames <- frames[!is.na(frames) & nzchar(frames)]
  if (length(frames) > 0L) {
    return(normalizePath(tail(frames, 1L), mustWork = TRUE))
  }
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    stop("Cannot locate the paired-ablation aggregator.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_PAIRED_AGGREGATOR_PATH <- pa_script_path()
LOMM11_PAIRED_DIRECTORY <- dirname(LOMM11_PAIRED_AGGREGATOR_PATH)
LOMM11_PAIRED_REPO_ROOT <- normalizePath(
  file.path(LOMM11_PAIRED_DIRECTORY, "..", ".."), mustWork = TRUE
)
LOMM11_PAIRED_CPP_PATH <- file.path(
  LOMM11_PAIRED_DIRECTORY, "paired_ablation_bootstrap.cpp"
)
LOMM11_PAIRED_DEFAULT_RUN <- file.path(
  LOMM11_PAIRED_REPO_ROOT, "simulation_outputs", "lomm_1_1",
  "empirical_realism_ablation_runs", "854e8722d647"
)
LOMM11_PAIRED_OUTPUT_ROOT <- file.path(
  LOMM11_PAIRED_REPO_ROOT, "simulation_outputs", "lomm_1_1",
  "empirical_realism_ablation_aggregates"
)
LOMM11_PAIRED_BOOTSTRAP_REPS <- 2000L
LOMM11_PAIRED_BOOTSTRAP_SEED <- 11082026L
LOMM11_PAIRED_VIEWS <- c("complete_case", "ilr_gap2")
LOMM11_PAIRED_VARIANTS <- c("000", "100", "010", "001", "110", "101", "011", "111")
LOMM11_PAIRED_FAMILIES <- c("probability", "logratio")
LOMM11_PAIRED_METRICS <- c(
  "level_median", "level_iqr", "difference_iqr", "lag1_correlation",
  "run_length_entropy", "spectral_entropy", "effective_rank", "total_variance"
)
LOMM11_PAIRED_COLUMNS <- c(
  "contrast_id", "contrast_definition", "analysis_view", "family", "metric",
  "estimand", "direction", "estimate", "monte_carlo_se", "bootstrap_se",
  "bootstrap_mean", "bootstrap_bias", "basic_conf_low", "basic_conf_high",
  "percentile_conf_low", "percentile_conf_high", "n_folds", "n_reps", "n_clips",
  "bootstrap_reps", "bootstrap_seed", "source_config_hash"
)
LOMM11_PROFILE_COLUMNS <- c(
  "profile_id", "profile", "analysis_view", "family", "metric_count",
  "estimand", "estimate", "monte_carlo_se", "bootstrap_se", "bootstrap_mean",
  "bootstrap_bias", "basic_conf_low", "basic_conf_high", "percentile_conf_low",
  "percentile_conf_high", "n_folds", "n_reps", "n_clips", "bootstrap_reps",
  "bootstrap_seed", "source_config_hash"
)
LOMM11_PROFILE_METRIC_COLUMNS <- c(
  "profile_id", "profile", "analysis_view", "family", "metric", "estimand",
  "estimate", "monte_carlo_se", "bootstrap_se", "bootstrap_mean",
  "bootstrap_bias", "basic_conf_low", "basic_conf_high", "percentile_conf_low",
  "percentile_conf_high", "empirical_units_min", "empirical_units_max",
  "synthetic_units_min", "synthetic_units_max", "n_folds", "n_reps", "n_clips",
  "bootstrap_reps", "bootstrap_seed", "source_config_hash"
)
LOMM11_PROFILE_LABELS <- c(
  `000` = "Homogeneous",
  `100` = "Clip baseline",
  `010` = "Clip scale",
  `001` = "Activity gate",
  `110` = "Clip baseline + clip scale",
  `101` = "Clip baseline + activity gate",
  `011` = "Clip scale + activity gate",
  `111` = "Clip baseline + clip scale + activity gate"
)

pa_stop <- function(...) {
  stop(..., call. = FALSE)
}

pa_require_packages <- function() {
  packages <- c(
    "arrow", "data.table", "digest", "dplyr", "jsonlite", "Rcpp",
    "RcppParallel"
  )
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    pa_stop("Required packages are unavailable: ", paste(missing, collapse = ", "), ".")
  }
  if (!file.exists(LOMM11_PAIRED_CPP_PATH)) {
    pa_stop("Paired bootstrap engine is missing: ", LOMM11_PAIRED_CPP_PATH)
  }
  invisible(TRUE)
}

pa_file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

pa_object_sha256 <- function(value) {
  encoded <- jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows"
  )
  digest::digest(encoded, algo = "sha256", serialize = FALSE)
}

pa_tree_sha256 <- function(path) {
  root <- normalizePath(path, mustWork = TRUE)
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files <- files[!file.info(files)$isdir]
  relative <- substring(files, nchar(root) + 2L)
  ordering <- order(relative, method = "radix")
  records <- lapply(ordering, function(index) list(
    path = relative[[index]], sha256 = pa_file_sha256(files[[index]])
  ))
  pa_object_sha256(records)
}

pa_read_json <- function(path) {
  if (!file.exists(path)) pa_stop("Required JSON file is missing: ", path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

pa_atomic_csv <- function(frame, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid()))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(frame, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) pa_stop("Failed to publish CSV: ", path)
  invisible(path)
}

pa_atomic_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(dirname(path), paste0(".", basename(path), ".tmp-", Sys.getpid()))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
  )
  if (!file.rename(temporary, path)) pa_stop("Failed to publish JSON: ", path)
  invisible(path)
}

pa_contrast_spec <- function() {
  bits <- do.call(rbind, strsplit(LOMM11_PAIRED_VARIANTS, "", fixed = TRUE))
  storage.mode(bits) <- "integer"
  colnames(bits) <- c("B", "S", "G")
  signs <- 2L * bits - 1L
  coefficients <- rbind(
    B = signs[, "B"] / 4,
    S = signs[, "S"] / 4,
    G = signs[, "G"] / 4,
    `B:S` = signs[, "B"] * signs[, "S"] / 2,
    `B:G` = signs[, "B"] * signs[, "G"] / 2,
    `S:G` = signs[, "S"] * signs[, "G"] / 2,
    `B:S:G` = signs[, "B"] * signs[, "S"] * signs[, "G"],
    `full-minus-core` = c(-1, 0, 0, 0, 0, 0, 0, 1)
  )
  colnames(coefficients) <- LOMM11_PAIRED_VARIANTS
  definitions <- c(
    B = "mean_{S,G}[D(1,S,G)-D(0,S,G)]",
    S = "mean_{B,G}[D(B,1,G)-D(B,0,G)]",
    G = "mean_{B,S}[D(B,S,1)-D(B,S,0)]",
    `B:S` = "mean_G[(D(1,1,G)-D(1,0,G))-(D(0,1,G)-D(0,0,G))]",
    `B:G` = "mean_S[(D(1,S,1)-D(1,S,0))-(D(0,S,1)-D(0,S,0))]",
    `S:G` = "mean_B[(D(B,1,1)-D(B,1,0))-(D(B,0,1)-D(B,0,0))]",
    `B:S:G` = "[(D111-D101)-(D011-D001)]-[(D110-D100)-(D010-D000)]",
    `full-minus-core` = "D111-D000"
  )
  directions <- c(
    B = "negative = lower discrepancy with B enabled, averaged over S and G",
    S = "negative = lower discrepancy with S enabled, averaged over B and G",
    G = "negative = lower discrepancy with G enabled, averaged over B and S",
    `B:S` = "negative = negative unscaled B-by-S difference-in-differences; inspect simple effects",
    `B:G` = "negative = negative unscaled B-by-G difference-in-differences; inspect simple effects",
    `S:G` = "negative = negative unscaled S-by-G difference-in-differences; inspect simple effects",
    `B:S:G` = "negative = negative unscaled third difference; inspect lower-order simple effects",
    `full-minus-core` = "negative = lower discrepancy for full B+S+G than core"
  )
  list(
    ids = rownames(coefficients), coefficients = coefficients,
    definitions = definitions, directions = directions
  )
}

pa_validate_contrast_spec <- function(spec = pa_contrast_spec()) {
  expected_ids <- c("B", "S", "G", "B:S", "B:G", "S:G", "B:S:G", "full-minus-core")
  valid <- identical(spec$ids, expected_ids) &&
    identical(dim(spec$coefficients), c(8L, 8L)) &&
    identical(colnames(spec$coefficients), LOMM11_PAIRED_VARIANTS) &&
    identical(names(spec$definitions), expected_ids) &&
    identical(names(spec$directions), expected_ids) &&
    all(is.finite(spec$coefficients)) &&
    all(abs(rowSums(spec$coefficients)) < 1e-14)
  if (!valid) pa_stop("The locked factorial contrast specification is invalid.")
  invisible(spec)
}

pa_source_output_hash <- function(path) {
  if (dir.exists(path)) pa_tree_sha256(path) else pa_file_sha256(path)
}

pa_validate_source_outputs <- function(run_dir, outputs) {
  expected_paths <- c(
    "fold_nuisance_manifest.csv", "fold_nuisance", "checkpoint_results.csv",
    "selected_candidates.csv", "training_metric_discrepancies.parquet",
    "heldout_metric_discrepancies.csv", "clip_metric_contributions.parquet"
  )
  observed_paths <- vapply(outputs, function(record) record$path, character(1))
  if (!identical(observed_paths, expected_paths)) {
    pa_stop("Source-run output records differ from the completed contract.")
  }
  for (record in outputs) {
    path <- file.path(run_dir, record$path)
    if (!file.exists(path) && !dir.exists(path)) pa_stop("Source output is missing: ", path)
    if (!identical(pa_source_output_hash(path), record$sha256)) {
      pa_stop("Source output hash differs from its completion receipt: ", record$path)
    }
  }
  invisible(TRUE)
}

pa_validate_plan <- function(run_dir, manifest) {
  plan_dir <- file.path(
    dirname(dirname(run_dir)), "empirical_realism_ablation", manifest$config_prefix
  )
  plan_dir <- normalizePath(plan_dir, mustWork = TRUE)
  prospective <- file.path(plan_dir, "prospective_manifest.json")
  if (!identical(pa_file_sha256(prospective), manifest$prospective_manifest_sha256)) {
    pa_stop("Prospective manifest hash differs from the completed source run.")
  }
  config_path <- file.path(plan_dir, "ablation_config.json")
  if (!identical(pa_file_sha256(config_path), manifest$plan_hashes$ablation_config.json)) {
    pa_stop("Ablation config hash differs from the completed source run.")
  }
  config <- pa_read_json(config_path)
  variants <- vapply(config$variants, function(value) value$variant_id, character(1))
  metrics <- unlist(config$metrics[c("univariate", "multivariate")], use.names = FALSE)
  valid <- identical(variants, LOMM11_PAIRED_VARIANTS) &&
    identical(metrics, LOMM11_PAIRED_METRICS) &&
    identical(unlist(config$metrics$families), LOMM11_PAIRED_FAMILIES) &&
    identical(config$folds$count, 5L) &&
    identical(config$folds$heldout_clips_per_fold, 20L) &&
    identical(config$randomness$heldout_replicates, 100L) &&
    identical(config$expected_counts$paired_ablation_effects, 272L)
  if (!valid) pa_stop("Source plan differs from the locked paired-ablation design.")
  assignments <- data.table::fread(
    file.path(plan_dir, "fold_assignments.csv"),
    colClasses = c(clip = "character", fold = "integer"), showProgress = FALSE
  )
  if (nrow(assignments) != 100L || data.table::uniqueN(assignments$clip) != 100L ||
      !identical(as.integer(table(assignments$fold)), rep(20L, 5L))) {
    pa_stop("Fold assignments do not contain five disjoint 20-clip folds.")
  }
  list(directory = plan_dir, config = config, assignments = assignments)
}

pa_validate_source_run <- function(run_dir) {
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  manifest_path <- file.path(run_dir, "run_manifest.json")
  success_path <- file.path(run_dir, "_SUCCESS.json")
  manifest <- pa_read_json(manifest_path)
  success <- pa_read_json(success_path)
  header_valid <- identical(manifest$status, "complete") &&
    identical(manifest$stage, "complete") &&
    identical(success$status, "complete") &&
    identical(manifest$config_hash, success$config_hash) &&
    identical(manifest$config_prefix, success$config_prefix) &&
    identical(success$run_manifest_sha256, pa_file_sha256(manifest_path)) &&
    identical(manifest$progress$checkpoint_units, 11240L) &&
    identical(manifest$progress$error, 0L) && identical(manifest$progress$timeout, 0L) &&
    identical(success$failed_panel_attempts, 0L)
  if (!header_valid) pa_stop("Source run is not a clean completed ablation run.")
  if (!identical(pa_object_sha256(manifest$outputs), pa_object_sha256(success$outputs))) {
    pa_stop("Source manifest and success receipt disagree about outputs.")
  }
  expected_entries <- sort(c(
    "_SUCCESS.json", "checkpoint_results.csv", "fold_nuisance",
    "fold_nuisance_manifest.csv", "heldout_metric_discrepancies.csv",
    "run_manifest.json", "selected_candidates.csv",
    "training_metric_discrepancies.parquet", "clip_metric_contributions.parquet"
  ))
  if (!identical(sort(list.files(run_dir, all.files = TRUE, no.. = TRUE)), expected_entries)) {
    pa_stop("Source run contains unexpected files; downstream aggregation is isolated.")
  }
  pa_validate_source_outputs(run_dir, success$outputs)
  plan <- pa_validate_plan(run_dir, manifest)
  list(
    directory = run_dir, manifest = manifest, success = success, plan = plan,
    run_manifest_sha256 = pa_file_sha256(manifest_path),
    success_sha256 = pa_file_sha256(success_path)
  )
}

pa_preserve_rng <- function(callback) {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  callback()
}

pa_bootstrap_design <- function(reps = LOMM11_PAIRED_BOOTSTRAP_REPS,
                                seed = LOMM11_PAIRED_BOOTSTRAP_SEED) {
  if (!identical(as.integer(reps), reps) || reps < 2L ||
      !identical(as.integer(seed), seed) || seed < 1L) {
    pa_stop("Bootstrap repetitions and seed must be positive integers.")
  }
  pa_preserve_rng(function() {
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(seed)
    lapply(seq_len(5L), function(fold) {
      counts <- matrix(0L, nrow = reps + 1L, ncol = 20L)
      counts[1L, ] <- 1L
      for (draw in seq_len(reps)) {
        counts[draw + 1L, ] <- tabulate(sample.int(20L, 20L, TRUE), nbins = 20L)
      }
      counts
    })
  })
}

pa_analysis_spec <- function(source, bootstrap_design) {
  contrasts <- pa_contrast_spec()
  list(
    schema_version = 3L,
    pipeline_id = "lomm_1_1_paired_ablation_aggregation",
    source_config_hash = source$manifest$config_hash,
    source_run_manifest_sha256 = source$run_manifest_sha256,
    source_success_sha256 = source$success_sha256,
    aggregator_sha256 = pa_file_sha256(LOMM11_PAIRED_AGGREGATOR_PATH),
    bootstrap_engine_sha256 = pa_file_sha256(LOMM11_PAIRED_CPP_PATH),
    analysis_views = as.list(LOMM11_PAIRED_VIEWS),
    variants = as.list(LOMM11_PAIRED_VARIANTS),
    families = as.list(LOMM11_PAIRED_FAMILIES),
    metrics = as.list(LOMM11_PAIRED_METRICS),
    contrasts = lapply(seq_along(contrasts$ids), function(index) list(
      contrast_id = contrasts$ids[[index]],
      definition = contrasts$definitions[[index]],
      coefficients = as.list(setNames(
        as.numeric(contrasts$coefficients[index, ]), LOMM11_PAIRED_VARIANTS
      ))
    )),
    interaction_scaling = paste(
      "main effects are marginal enabled-minus-disabled differences;",
      "two-way interactions are unscaled difference-in-differences averaged",
      "over the third factor; B:S:G is the unscaled third difference"
    ),
    discrepancy = "max(abs_median_difference_scaled,wasserstein1_scaled)",
    aggregation = "mean across 100 paired heldout replicates and five folds",
    resampling = paste(
      "2,000 fold-stratified clip-cluster draws; 20 heldout clips sampled with",
      "replacement within each fold; variants, replicates, views, families,",
      "metrics, and units remain paired; discrepancy is recomputed per panel"
    ),
    intervals = list(
      primary = "basic bootstrap interval",
      sensitivity = "percentile bootstrap interval",
      bias = "bootstrap mean minus observed estimate"
    ),
    bootstrap_reps = LOMM11_PAIRED_BOOTSTRAP_REPS,
    bootstrap_seed = LOMM11_PAIRED_BOOTSTRAP_SEED,
    bootstrap_rng = "L'Ecuyer-CMRG/Inversion/Rejection",
    bootstrap_design_sha256 = pa_object_sha256(bootstrap_design),
    expected_rows = 272L
  )
}

pa_default_output_dir <- function(source, analysis_hash) {
  file.path(
    LOMM11_PAIRED_OUTPUT_ROOT, source$manifest$config_prefix,
    substr(analysis_hash, 1L, 12L)
  )
}

pa_read_heldout <- function(source) {
  path <- file.path(source$directory, "heldout_metric_discrepancies.csv")
  columns <- c(
    "fold", "variant_id", "heldout_rep", "analysis_view", "family", "metric",
    "max_discrepancy", "empirical_n", "synthetic_n", "status", "seed_group",
    "config_hash"
  )
  frame <- data.table::fread(
    path, select = columns, colClasses = c(
      fold = "integer", variant_id = "character", heldout_rep = "integer",
      analysis_view = "character", family = "character", metric = "character",
      max_discrepancy = "numeric", empirical_n = "integer", synthetic_n = "integer",
      status = "character", seed_group = "character",
      config_hash = "character"
    ), showProgress = FALSE
  )
  key_columns <- c("fold", "variant_id", "heldout_rep", "analysis_view", "family", "metric")
  expected_seed <- sprintf("heldout|F%02d|R%03d", frame$fold, frame$heldout_rep)
  valid <- nrow(frame) == 128000L &&
    data.table::uniqueN(frame, by = key_columns) == 128000L &&
    setequal(frame$analysis_view, LOMM11_PAIRED_VIEWS) &&
    setequal(frame$variant_id, LOMM11_PAIRED_VARIANTS) &&
    setequal(frame$family, LOMM11_PAIRED_FAMILIES) &&
    setequal(frame$metric, LOMM11_PAIRED_METRICS) &&
    all(frame$status == "ok") && all(is.finite(frame$max_discrepancy)) &&
    all(frame$empirical_n > 0L) && all(frame$synthetic_n > 0L) &&
    all(frame$config_hash == source$manifest$config_hash) &&
    identical(frame$seed_group, expected_seed)
  if (!valid) pa_stop("Held-out discrepancies violate the complete paired contract.")
  frame[, variant_order := match(variant_id, LOMM11_PAIRED_VARIANTS)]
  frame
}

pa_compile_engine <- function(workers) {
  cache <- file.path(tempdir(), "lomm11-paired-bootstrap-cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  Rcpp::sourceCpp(
    LOMM11_PAIRED_CPP_PATH, rebuild = FALSE, cacheDir = cache,
    showOutput = FALSE, verbose = FALSE
  )
  RcppParallel::setThreadOptions(numThreads = workers)
  if (!exists("lomm11_bootstrap_metric_group_cpp", mode = "function")) {
    pa_stop("Compiled paired-bootstrap entry point is unavailable.")
  }
  invisible(TRUE)
}

pa_collect_metric <- function(dataset, view, fold, family, metric) {
  query <- dplyr::filter(
    dataset,
    analysis_view == !!view,
    .data$fold == !!as.integer(fold),
    .data$family == !!family,
    .data$metric == !!metric
  )
  query <- dplyr::select(
    query, "variant_id", "heldout_rep", "clip", "metric", "unit",
    "empirical_value", "synthetic_value", "training_scale"
  )
  data.table::as.data.table(dplyr::collect(query))
}

pa_expected_units <- function(metric) {
  if (metric %in% LOMM11_PAIRED_METRICS[1:6]) 11L else 1L
}

pa_prepare_metric <- function(data, metric_name, clips) {
  units <- sort(unique(as.character(data$unit)), method = "radix")
  units_per_clip <- pa_expected_units(metric_name)
  observations <- 20L * units_per_clip
  expected_rows <- 8L * 100L * observations
  key_columns <- c("variant_id", "heldout_rep", "clip", "unit")
  valid <- nrow(data) == expected_rows && length(units) == units_per_clip &&
    data.table::uniqueN(data, by = key_columns) == expected_rows &&
    all(data$metric == metric_name) &&
    setequal(data$variant_id, LOMM11_PAIRED_VARIANTS) &&
    setequal(data$heldout_rep, 1:100) && setequal(data$clip, clips)
  if (!valid) pa_stop("Contribution geometry is invalid for metric: ", metric_name)
  data[, variant_order := match(variant_id, LOMM11_PAIRED_VARIANTS)]
  data[, clip_order := match(clip, clips)]
  data[, unit_order := match(unit, units)]
  data.table::setorder(data, variant_order, heldout_rep, clip_order, unit_order)
  expected_variant <- rep(seq_len(8L), each = 100L * observations)
  expected_rep <- rep(rep(seq_len(100L), each = observations), times = 8L)
  expected_clip <- rep(rep(rep(seq_len(20L), each = units_per_clip), times = 100L), 8L)
  if (!identical(data$variant_order, expected_variant) ||
      !identical(data$heldout_rep, expected_rep) ||
      !identical(data$clip_order, expected_clip)) {
    pa_stop("Contribution rows do not form the locked panel grid: ", metric_name)
  }
  empirical <- data$empirical_value[seq_len(observations)]
  if (!identical(data$empirical_value, rep(empirical, 800L))) {
    pa_stop("Empirical contributions are not invariant across paired panels: ", metric_name)
  }
  scales <- unique(data$training_scale)
  if (length(scales) != 1L || !is.finite(scales) || scales <= 0) {
    pa_stop("Metric training scale is not one positive constant: ", metric_name)
  }
  list(
    empirical = empirical,
    synthetic = matrix(data$synthetic_value, nrow = observations, ncol = 800L),
    clip_index = rep(0:19, each = units_per_clip),
    variant_index = rep(0:7, each = 100L),
    scale = scales[[1L]]
  )
}

pa_validate_contribution_dataset <- function(dataset) {
  expected_columns <- c(
    "contribution_key", "unit_key", "panel_key", "fold", "variant_id",
    "heldout_rep", "analysis_view", "clip", "family", "metric", "unit",
    "empirical_value", "synthetic_value", "training_scale", "status",
    "error_message", "seed_group", "config_hash", "input_hash", "panel_hash"
  )
  if (!identical(names(dataset$schema), expected_columns)) {
    pa_stop("Contribution dataset columns differ from the completed contract.")
  }
  rows <- dplyr::collect(dplyr::summarise(dataset, rows = dplyr::n()))$rows[[1L]]
  if (!identical(as.integer(rows), 21760000L)) {
    pa_stop("Contribution dataset row count differs from the completed contract.")
  }
  invisible(TRUE)
}

pa_direct_variant_means <- function(heldout, view, family_name, metric_name, fold_id) {
  selected <- heldout$analysis_view == view & heldout$family == family_name &
    heldout$metric == metric_name & heldout$fold == fold_id
  data <- heldout[selected]
  data.table::setorder(data, variant_order, heldout_rep)
  if (nrow(data) != 800L) pa_stop("Held-out direct mean group is incomplete.")
  vapply(seq_len(8L), function(index) {
    mean(data$max_discrepancy[data$variant_order == index])
  }, numeric(1))
}

pa_compute_variant_draws <- function(source, heldout, bootstrap_design, workers) {
  pa_compile_engine(workers)
  dataset <- arrow::open_dataset(
    file.path(source$directory, "clip_metric_contributions.parquet"),
    format = "parquet"
  )
  pa_validate_contribution_dataset(dataset)
  assignments <- source$plan$assignments
  output <- setNames(vector("list", length(LOMM11_PAIRED_VIEWS)), LOMM11_PAIRED_VIEWS)
  completed_partitions <- 0L
  total_partitions <- 20L
  computation_started <- proc.time()[["elapsed"]]
  for (view in LOMM11_PAIRED_VIEWS) {
    view_output <- list()
    for (family in LOMM11_PAIRED_FAMILIES) {
      fold_outputs <- setNames(vector("list", 5L), as.character(seq_len(5L)))
      for (fold in seq_len(5L)) {
        clips <- sort(assignments$clip[assignments$fold == fold], method = "radix")
        metric_outputs <- list()
        for (metric in LOMM11_PAIRED_METRICS) {
          metric_data <- pa_collect_metric(dataset, view, fold, family, metric)
          prepared <- pa_prepare_metric(metric_data, metric, clips)
          draws <- lomm11_bootstrap_metric_group_cpp(
            prepared$empirical, prepared$synthetic, prepared$clip_index,
            prepared$variant_index, bootstrap_design[[fold]], prepared$scale, 8L
          )
          direct <- pa_direct_variant_means(heldout, view, family, metric, fold)
          if (!isTRUE(all.equal(as.numeric(draws[1L, ]), direct, tolerance = 2e-11))) {
            pa_stop("Recomputed source discrepancy differs from held-out output: ",
                    paste(view, fold, family, metric, sep = "/"))
          }
          metric_outputs[[metric]] <- draws
          rm(metric_data, prepared, draws)
        }
        fold_outputs[[as.character(fold)]] <- metric_outputs
        completed_partitions <- completed_partitions + 1L
        cat(sprintf(
          "Bootstrap progress: %d/%d view-fold-family partitions (%.1f min)\n",
          completed_partitions, total_partitions,
          (proc.time()[["elapsed"]] - computation_started) / 60
        ))
        flush.console()
        invisible(gc(FALSE))
      }
      for (metric in LOMM11_PAIRED_METRICS) {
        matrices <- lapply(fold_outputs, `[[`, metric)
        view_output[[paste(family, metric, sep = "|")]] <-
          Reduce(`+`, matrices) / length(matrices)
      }
      rm(fold_outputs)
    }
    output[[view]] <- view_output
  }
  output
}

pa_mcse_from_group <- function(data, coefficients) {
  fold_variances <- matrix(NA_real_, nrow = 5L, ncol = nrow(coefficients))
  for (fold_id in seq_len(5L)) {
    selected <- data[data$fold == fold_id]
    data.table::setorder(selected, heldout_rep, variant_order)
    if (nrow(selected) != 800L) pa_stop("MCSE group lacks 100 paired replicates.")
    values <- matrix(selected$max_discrepancy, nrow = 100L, ncol = 8L, byrow = TRUE)
    paired <- values %*% t(coefficients)
    fold_variances[fold_id, ] <- apply(paired, 2L, stats::var) / 100
  }
  sqrt(colSums(fold_variances) / 25)
}

pa_mcse_catalog <- function(heldout, coefficients) {
  output <- list()
  for (view in LOMM11_PAIRED_VIEWS) {
    view_output <- list()
    selected_view <- heldout[heldout$analysis_view == view]
    for (family_name in LOMM11_PAIRED_FAMILIES) {
      for (metric_name in LOMM11_PAIRED_METRICS) {
        group <- selected_view[
          selected_view$family == family_name & selected_view$metric == metric_name
        ]
        view_output[[paste(family_name, metric_name, sep = "|")]] <-
          pa_mcse_from_group(group, coefficients)
      }
    }
    composite <- selected_view[, .(
      max_discrepancy = mean(max_discrepancy), variant_order = variant_order[[1L]]
    ), by = .(fold, heldout_rep, variant_id)]
    view_output[["combined|composite"]] <- pa_mcse_from_group(composite, coefficients)
    output[[view]] <- view_output
  }
  output
}

pa_quantile <- function(values, probability) {
  stats::quantile(values, probability, names = FALSE, type = 8)
}

pa_interval_summary <- function(estimate, bootstrap) {
  estimate <- unname(as.numeric(estimate))
  bootstrap <- unname(as.numeric(bootstrap))
  if (!is.finite(estimate) || length(bootstrap) < 2L || any(!is.finite(bootstrap))) {
    pa_stop("Bootstrap interval inputs must be finite and contain at least two draws.")
  }
  percentile_low <- pa_quantile(bootstrap, 0.025)
  percentile_high <- pa_quantile(bootstrap, 0.975)
  bootstrap_mean <- mean(bootstrap)
  c(
    bootstrap_se = stats::sd(bootstrap),
    bootstrap_mean = bootstrap_mean,
    bootstrap_bias = bootstrap_mean - estimate,
    basic_conf_low = 2 * estimate - percentile_high,
    basic_conf_high = 2 * estimate - percentile_low,
    percentile_conf_low = percentile_low,
    percentile_conf_high = percentile_high
  )
}

pa_build_effect_results <- function(variant_draws, heldout, source) {
  spec <- pa_contrast_spec()
  mcse <- pa_mcse_catalog(heldout, spec$coefficients)
  rows <- list()
  for (view in LOMM11_PAIRED_VIEWS) {
    catalog <- variant_draws[[view]]
    catalog[["combined|composite"]] <- Reduce(`+`, catalog) / length(catalog)
    for (outcome in names(catalog)) {
      pieces <- strsplit(outcome, "|", fixed = TRUE)[[1L]]
      contrast_draws <- catalog[[outcome]] %*% t(spec$coefficients)
      for (index in seq_along(spec$ids)) {
        bootstrap <- contrast_draws[-1L, index]
        interval <- pa_interval_summary(contrast_draws[1L, index], bootstrap)
        rows[[length(rows) + 1L]] <- data.frame(
          contrast_id = spec$ids[[index]],
          contrast_definition = spec$definitions[[index]],
          analysis_view = view,
          family = pieces[[1L]],
          metric = pieces[[2L]],
          estimand = "mean_heldout_max_discrepancy_contrast",
          direction = spec$directions[[index]],
          estimate = contrast_draws[1L, index],
          monte_carlo_se = mcse[[view]][[outcome]][[index]],
          bootstrap_se = interval[["bootstrap_se"]],
          bootstrap_mean = interval[["bootstrap_mean"]],
          bootstrap_bias = interval[["bootstrap_bias"]],
          basic_conf_low = interval[["basic_conf_low"]],
          basic_conf_high = interval[["basic_conf_high"]],
          percentile_conf_low = interval[["percentile_conf_low"]],
          percentile_conf_high = interval[["percentile_conf_high"]],
          n_folds = 5L,
          n_reps = 100L,
          n_clips = 100L,
          bootstrap_reps = LOMM11_PAIRED_BOOTSTRAP_REPS,
          bootstrap_seed = LOMM11_PAIRED_BOOTSTRAP_SEED,
          source_config_hash = source$manifest$config_hash,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  output <- data.table::rbindlist(rows, use.names = TRUE)
  data.table::setcolorder(output, LOMM11_PAIRED_COLUMNS)
  output[, family_order := match(family, c("probability", "logratio", "combined"))]
  output[, metric_order := match(metric, c(LOMM11_PAIRED_METRICS, "composite"))]
  output[, contrast_order := match(contrast_id, pa_contrast_spec()$ids)]
  data.table::setorderv(
    output, c("analysis_view", "family_order", "metric_order", "contrast_order"),
    order = 1L, na.last = TRUE
  )
  output[, c("family_order", "metric_order", "contrast_order") := NULL]
  as.data.frame(output)
}

pa_profile_mcse <- function(heldout, view, family_name, metric_name = NULL) {
  selected <- heldout[
    heldout$analysis_view == view & heldout$family == family_name
  ]
  if (!is.null(metric_name)) {
    selected <- selected[selected$metric == metric_name]
  }
  profile_panels <- selected[, .(
    discrepancy = mean(max_discrepancy),
    variant_order = variant_order[[1L]]
  ), by = .(fold, heldout_rep, variant_id)]
  fold_variances <- matrix(NA_real_, nrow = 5L, ncol = 8L)
  for (fold_id in seq_len(5L)) {
    fold <- profile_panels[profile_panels$fold == fold_id]
    data.table::setorder(fold, heldout_rep, variant_order)
    if (nrow(fold) != 800L) pa_stop("Profile MCSE group lacks 100 paired replicates.")
    values <- matrix(fold$discrepancy, nrow = 100L, ncol = 8L, byrow = TRUE)
    fold_variances[fold_id, ] <- apply(values, 2L, stats::var) / 100
  }
  sqrt(colSums(fold_variances) / 25)
}

pa_build_profile_results <- function(variant_draws, heldout, source) {
  rows <- list()
  for (view in LOMM11_PAIRED_VIEWS) {
    for (family_name in LOMM11_PAIRED_FAMILIES) {
      outcomes <- paste(family_name, LOMM11_PAIRED_METRICS, sep = "|")
      family_draws <- Reduce(`+`, variant_draws[[view]][outcomes]) / length(outcomes)
      mcse <- pa_profile_mcse(heldout, view, family_name)
      for (index in seq_along(LOMM11_PAIRED_VARIANTS)) {
        estimate <- family_draws[1L, index]
        interval <- pa_interval_summary(estimate, family_draws[-1L, index])
        profile_id <- LOMM11_PAIRED_VARIANTS[[index]]
        rows[[length(rows) + 1L]] <- data.frame(
          profile_id = profile_id,
          profile = unname(LOMM11_PROFILE_LABELS[[profile_id]]),
          analysis_view = view,
          family = family_name,
          metric_count = length(LOMM11_PAIRED_METRICS),
          estimand = "mean_heldout_max_discrepancy",
          estimate = estimate,
          monte_carlo_se = mcse[[index]],
          bootstrap_se = interval[["bootstrap_se"]],
          bootstrap_mean = interval[["bootstrap_mean"]],
          bootstrap_bias = interval[["bootstrap_bias"]],
          basic_conf_low = interval[["basic_conf_low"]],
          basic_conf_high = interval[["basic_conf_high"]],
          percentile_conf_low = interval[["percentile_conf_low"]],
          percentile_conf_high = interval[["percentile_conf_high"]],
          n_folds = 5L,
          n_reps = 100L,
          n_clips = 100L,
          bootstrap_reps = LOMM11_PAIRED_BOOTSTRAP_REPS,
          bootstrap_seed = LOMM11_PAIRED_BOOTSTRAP_SEED,
          source_config_hash = source$manifest$config_hash,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  output <- data.table::rbindlist(rows, use.names = TRUE)
  data.table::setcolorder(output, LOMM11_PROFILE_COLUMNS)
  output[, view_order := match(analysis_view, LOMM11_PAIRED_VIEWS)]
  output[, family_order := match(family, LOMM11_PAIRED_FAMILIES)]
  output[, profile_order := match(profile_id, LOMM11_PAIRED_VARIANTS)]
  data.table::setorderv(output, c("view_order", "family_order", "profile_order"))
  output[, c("view_order", "family_order", "profile_order") := NULL]
  as.data.frame(output)
}

pa_build_profile_metric_results <- function(variant_draws, heldout, source) {
  rows <- list()
  for (view in LOMM11_PAIRED_VIEWS) {
    for (family_name in LOMM11_PAIRED_FAMILIES) {
      for (metric_name in LOMM11_PAIRED_METRICS) {
        outcome <- paste(family_name, metric_name, sep = "|")
        draws <- variant_draws[[view]][[outcome]]
        mcse <- pa_profile_mcse(heldout, view, family_name, metric_name)
        for (index in seq_along(LOMM11_PAIRED_VARIANTS)) {
          estimate <- draws[1L, index]
          interval <- pa_interval_summary(estimate, draws[-1L, index])
          profile_id <- LOMM11_PAIRED_VARIANTS[[index]]
          selected <- heldout[
            heldout$analysis_view == view & heldout$family == family_name &
              heldout$metric == metric_name & heldout$variant_id == profile_id
          ]
          if (nrow(selected) != 500L) {
            pa_stop("Profile-metric denominator group lacks 500 paired panels.")
          }
          rows[[length(rows) + 1L]] <- data.frame(
            profile_id = profile_id,
            profile = unname(LOMM11_PROFILE_LABELS[[profile_id]]),
            analysis_view = view,
            family = family_name,
            metric = metric_name,
            estimand = "mean_heldout_max_discrepancy",
            estimate = estimate,
            monte_carlo_se = mcse[[index]],
            bootstrap_se = interval[["bootstrap_se"]],
            bootstrap_mean = interval[["bootstrap_mean"]],
            bootstrap_bias = interval[["bootstrap_bias"]],
            basic_conf_low = interval[["basic_conf_low"]],
            basic_conf_high = interval[["basic_conf_high"]],
            percentile_conf_low = interval[["percentile_conf_low"]],
            percentile_conf_high = interval[["percentile_conf_high"]],
            empirical_units_min = min(selected$empirical_n),
            empirical_units_max = max(selected$empirical_n),
            synthetic_units_min = min(selected$synthetic_n),
            synthetic_units_max = max(selected$synthetic_n),
            n_folds = 5L,
            n_reps = 100L,
            n_clips = 100L,
            bootstrap_reps = LOMM11_PAIRED_BOOTSTRAP_REPS,
            bootstrap_seed = LOMM11_PAIRED_BOOTSTRAP_SEED,
            source_config_hash = source$manifest$config_hash,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  output <- data.table::rbindlist(rows, use.names = TRUE)
  data.table::setcolorder(output, LOMM11_PROFILE_METRIC_COLUMNS)
  output[, view_order := match(analysis_view, LOMM11_PAIRED_VIEWS)]
  output[, family_order := match(family, LOMM11_PAIRED_FAMILIES)]
  output[, metric_order := match(metric, LOMM11_PAIRED_METRICS)]
  output[, profile_order := match(profile_id, LOMM11_PAIRED_VARIANTS)]
  data.table::setorderv(
    output, c("view_order", "family_order", "metric_order", "profile_order")
  )
  output[, c("view_order", "family_order", "metric_order", "profile_order") := NULL]
  as.data.frame(output)
}

pa_build_results <- function(variant_draws, heldout, source) {
  list(
    effects = pa_build_effect_results(variant_draws, heldout, source),
    profiles = pa_build_profile_results(variant_draws, heldout, source),
    profile_metrics = pa_build_profile_metric_results(variant_draws, heldout, source)
  )
}

pa_validate_results <- function(frame, source_hash) {
  key <- c("contrast_id", "analysis_view", "family", "metric")
  expected_outcomes <- c(
    as.vector(outer(LOMM11_PAIRED_FAMILIES, LOMM11_PAIRED_METRICS, paste, sep = "|")),
    "combined|composite"
  )
  observed_outcomes <- unique(paste(frame$family, frame$metric, sep = "|"))
  numeric_columns <- c(
    "estimate", "monte_carlo_se", "bootstrap_se", "bootstrap_mean",
    "bootstrap_bias", "basic_conf_low", "basic_conf_high",
    "percentile_conf_low", "percentile_conf_high"
  )
  valid <- identical(names(frame), LOMM11_PAIRED_COLUMNS) && nrow(frame) == 272L &&
    data.table::uniqueN(data.table::as.data.table(frame), by = key) == 272L &&
    setequal(frame$contrast_id, pa_contrast_spec()$ids) &&
    setequal(frame$analysis_view, LOMM11_PAIRED_VIEWS) &&
    setequal(observed_outcomes, expected_outcomes) &&
    all(vapply(frame[numeric_columns], function(value) all(is.finite(value)), logical(1))) &&
    all(frame$monte_carlo_se >= 0) && all(frame$bootstrap_se >= 0) &&
    all(frame$basic_conf_low <= frame$basic_conf_high) &&
    all(frame$percentile_conf_low <= frame$percentile_conf_high) &&
    isTRUE(all.equal(frame$bootstrap_bias, frame$bootstrap_mean - frame$estimate,
                     tolerance = 1e-12)) && all(frame$n_folds == 5L) &&
    all(frame$n_reps == 100L) && all(frame$n_clips == 100L) &&
    all(frame$bootstrap_reps == LOMM11_PAIRED_BOOTSTRAP_REPS) &&
    all(frame$bootstrap_seed == LOMM11_PAIRED_BOOTSTRAP_SEED) &&
    all(frame$source_config_hash == source_hash)
  if (!valid) pa_stop("Paired ablation results violate the locked 272-row schema.")
  invisible(frame)
}

pa_validate_profiles <- function(frame, source_hash) {
  key <- c("profile_id", "analysis_view", "family")
  numeric_columns <- c(
    "estimate", "monte_carlo_se", "bootstrap_se", "bootstrap_mean",
    "bootstrap_bias", "basic_conf_low", "basic_conf_high",
    "percentile_conf_low", "percentile_conf_high"
  )
  valid <- identical(names(frame), LOMM11_PROFILE_COLUMNS) && nrow(frame) == 32L &&
    data.table::uniqueN(data.table::as.data.table(frame), by = key) == 32L &&
    setequal(frame$profile_id, LOMM11_PAIRED_VARIANTS) &&
    identical(setNames(frame$profile[match(LOMM11_PAIRED_VARIANTS, frame$profile_id)],
                       LOMM11_PAIRED_VARIANTS), LOMM11_PROFILE_LABELS) &&
    setequal(frame$analysis_view, LOMM11_PAIRED_VIEWS) &&
    setequal(frame$family, LOMM11_PAIRED_FAMILIES) &&
    all(frame$metric_count == length(LOMM11_PAIRED_METRICS)) &&
    all(vapply(frame[numeric_columns], function(value) all(is.finite(value)), logical(1))) &&
    all(frame$monte_carlo_se >= 0) && all(frame$bootstrap_se >= 0) &&
    all(frame$basic_conf_low <= frame$basic_conf_high) &&
    all(frame$percentile_conf_low <= frame$percentile_conf_high) &&
    isTRUE(all.equal(frame$bootstrap_bias, frame$bootstrap_mean - frame$estimate,
                     tolerance = 1e-12)) && all(frame$n_folds == 5L) &&
    all(frame$n_reps == 100L) && all(frame$n_clips == 100L) &&
    all(frame$bootstrap_reps == LOMM11_PAIRED_BOOTSTRAP_REPS) &&
    all(frame$bootstrap_seed == LOMM11_PAIRED_BOOTSTRAP_SEED) &&
    all(frame$source_config_hash == source_hash)
  if (!valid) pa_stop("Profile discrepancies violate the locked 32-row schema.")
  invisible(frame)
}

pa_validate_profile_metrics <- function(frame, source_hash) {
  key <- c("profile_id", "analysis_view", "family", "metric")
  numeric_columns <- c(
    "estimate", "monte_carlo_se", "bootstrap_se", "bootstrap_mean",
    "bootstrap_bias", "basic_conf_low", "basic_conf_high",
    "percentile_conf_low", "percentile_conf_high"
  )
  integer_columns <- c(
    "empirical_units_min", "empirical_units_max", "synthetic_units_min",
    "synthetic_units_max"
  )
  valid <- identical(names(frame), LOMM11_PROFILE_METRIC_COLUMNS) &&
    nrow(frame) == 256L &&
    data.table::uniqueN(data.table::as.data.table(frame), by = key) == 256L &&
    setequal(frame$profile_id, LOMM11_PAIRED_VARIANTS) &&
    setequal(frame$analysis_view, LOMM11_PAIRED_VIEWS) &&
    setequal(frame$family, LOMM11_PAIRED_FAMILIES) &&
    setequal(frame$metric, LOMM11_PAIRED_METRICS) &&
    all(vapply(frame[numeric_columns], function(value) all(is.finite(value)), logical(1))) &&
    all(vapply(frame[integer_columns], function(value) all(value > 0L), logical(1))) &&
    all(frame$empirical_units_min <= frame$empirical_units_max) &&
    all(frame$synthetic_units_min <= frame$synthetic_units_max) &&
    all(frame$monte_carlo_se >= 0) && all(frame$bootstrap_se >= 0) &&
    all(frame$basic_conf_low <= frame$basic_conf_high) &&
    all(frame$percentile_conf_low <= frame$percentile_conf_high) &&
    isTRUE(all.equal(frame$bootstrap_bias, frame$bootstrap_mean - frame$estimate,
                     tolerance = 1e-12)) && all(frame$n_folds == 5L) &&
    all(frame$n_reps == 100L) && all(frame$n_clips == 100L) &&
    all(frame$bootstrap_reps == LOMM11_PAIRED_BOOTSTRAP_REPS) &&
    all(frame$bootstrap_seed == LOMM11_PAIRED_BOOTSTRAP_SEED) &&
    all(frame$source_config_hash == source_hash)
  if (!valid) pa_stop("Profile-metric discrepancies violate the locked 256-row schema.")
  invisible(frame)
}

pa_git_state <- function() {
  command <- function(arguments) {
    suppressWarnings(system2("git", arguments, stdout = TRUE, stderr = FALSE))
  }
  commit <- command(c("-C", LOMM11_PAIRED_REPO_ROOT, "rev-parse", "HEAD"))
  status <- command(c("-C", LOMM11_PAIRED_REPO_ROOT, "status", "--porcelain"))
  list(commit = if (length(commit)) commit[[1L]] else NA_character_, dirty = length(status) > 0L)
}

pa_software_versions <- function() {
  packages <- c("arrow", "data.table", "digest", "dplyr", "jsonlite", "Rcpp", "RcppParallel")
  versions <- lapply(packages, function(package) as.character(utils::packageVersion(package)))
  names(versions) <- packages
  c(list(R = R.version.string), versions)
}

pa_manifest <- function(source, analysis_spec, analysis_hash, outputs, started_at) {
  output_records <- list(
    list(
      path = "paired_ablation_effects.csv",
      sha256 = pa_file_sha256(outputs$effects),
      rows = 272L
    ),
    list(
      path = "profile_discrepancies.csv",
      sha256 = pa_file_sha256(outputs$profiles),
      rows = 32L
    ),
    list(
      path = "profile_metric_discrepancies.csv",
      sha256 = pa_file_sha256(outputs$profile_metrics),
      rows = 256L
    )
  )
  list(
    schema_version = 3L,
    status = "complete",
    pipeline_id = "lomm_1_1_paired_ablation_aggregation",
    analysis_hash = analysis_hash,
    analysis_prefix = substr(analysis_hash, 1L, 12L),
    analysis_spec = analysis_spec,
    source = list(
      run_directory = source$directory,
      config_hash = source$manifest$config_hash,
      config_prefix = source$manifest$config_prefix,
      run_manifest_sha256 = source$run_manifest_sha256,
      success_sha256 = source$success_sha256,
      clip_contributions_sha256 = source$success$outputs[[7L]]$sha256,
      heldout_discrepancies_sha256 = source$success$outputs[[6L]]$sha256
    ),
    result_contract = list(
      effects = list(
        rows = 272L, views = 2L, outcomes_per_view = 17L, contrasts = 8L,
        primary_key = as.list(c("contrast_id", "analysis_view", "family", "metric")),
        columns = as.list(LOMM11_PAIRED_COLUMNS)
      ),
      profiles = list(
        rows = 32L, views = 2L, families = 2L, profiles = 8L,
        primary_key = as.list(c("profile_id", "analysis_view", "family")),
        columns = as.list(LOMM11_PROFILE_COLUMNS)
      ),
      profile_metrics = list(
        rows = 256L, views = 2L, families = 2L, metrics = 8L, profiles = 8L,
        primary_key = as.list(c(
          "profile_id", "analysis_view", "family", "metric"
        )),
        columns = as.list(LOMM11_PROFILE_METRIC_COLUMNS)
      )
    ),
    claim_scope = source$plan$config$claim_scope,
    outputs = output_records,
    started_at = started_at,
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    command = paste(commandArgs(), collapse = " "),
    software_versions = pa_software_versions(),
    git = pa_git_state()
  )
}

pa_success <- function(manifest_path, manifest) {
  list(
    schema_version = 3L,
    status = "complete",
    pipeline_id = manifest$pipeline_id,
    analysis_hash = manifest$analysis_hash,
    analysis_prefix = manifest$analysis_prefix,
    source_config_hash = manifest$source$config_hash,
    source_run_manifest_sha256 = manifest$source$run_manifest_sha256,
    source_success_sha256 = manifest$source$success_sha256,
    run_manifest_sha256 = pa_file_sha256(manifest_path),
    bootstrap_reps = LOMM11_PAIRED_BOOTSTRAP_REPS,
    bootstrap_seed = LOMM11_PAIRED_BOOTSTRAP_SEED,
    outputs = manifest$outputs,
    completed_at = manifest$completed_at
  )
}

pa_validate_published <- function(directory, source = NULL) {
  directory <- normalizePath(directory, mustWork = TRUE)
  entries <- sort(list.files(directory, all.files = TRUE, no.. = TRUE))
  expected_entries <- sort(c(
    "_SUCCESS.json", "paired_ablation_effects.csv", "profile_discrepancies.csv",
    "profile_metric_discrepancies.csv", "run_manifest.json"
  ))
  if (!identical(entries, expected_entries)) {
    pa_stop("Published paired-ablation directory has unexpected files.")
  }
  manifest_path <- file.path(directory, "run_manifest.json")
  success <- pa_read_json(file.path(directory, "_SUCCESS.json"))
  manifest <- pa_read_json(manifest_path)
  effects_path <- file.path(directory, "paired_ablation_effects.csv")
  profiles_path <- file.path(directory, "profile_discrepancies.csv")
  profile_metrics_path <- file.path(directory, "profile_metric_discrepancies.csv")
  effects <- utils::read.csv(
    effects_path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      contrast_id = "character", contrast_definition = "character",
      analysis_view = "character", family = "character", metric = "character",
      estimand = "character", direction = "character", estimate = "numeric",
      monte_carlo_se = "numeric", bootstrap_se = "numeric",
      bootstrap_mean = "numeric", bootstrap_bias = "numeric",
      basic_conf_low = "numeric", basic_conf_high = "numeric",
      percentile_conf_low = "numeric", percentile_conf_high = "numeric",
      n_folds = "integer", n_reps = "integer",
      n_clips = "integer", bootstrap_reps = "integer", bootstrap_seed = "integer",
      source_config_hash = "character"
    )
  )
  profiles <- utils::read.csv(
    profiles_path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      profile_id = "character", profile = "character", analysis_view = "character",
      family = "character", metric_count = "integer", estimand = "character",
      estimate = "numeric", monte_carlo_se = "numeric", bootstrap_se = "numeric",
      bootstrap_mean = "numeric", bootstrap_bias = "numeric",
      basic_conf_low = "numeric", basic_conf_high = "numeric",
      percentile_conf_low = "numeric", percentile_conf_high = "numeric",
      n_folds = "integer", n_reps = "integer", n_clips = "integer",
      bootstrap_reps = "integer", bootstrap_seed = "integer",
      source_config_hash = "character"
    )
  )
  profile_metrics <- utils::read.csv(
    profile_metrics_path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      profile_id = "character", profile = "character", analysis_view = "character",
      family = "character", metric = "character", estimand = "character",
      estimate = "numeric", monte_carlo_se = "numeric", bootstrap_se = "numeric",
      bootstrap_mean = "numeric", bootstrap_bias = "numeric",
      basic_conf_low = "numeric", basic_conf_high = "numeric",
      percentile_conf_low = "numeric", percentile_conf_high = "numeric",
      empirical_units_min = "integer", empirical_units_max = "integer",
      synthetic_units_min = "integer", synthetic_units_max = "integer",
      n_folds = "integer", n_reps = "integer", n_clips = "integer",
      bootstrap_reps = "integer", bootstrap_seed = "integer",
      source_config_hash = "character"
    )
  )
  header_valid <- identical(manifest$status, "complete") &&
    identical(success$status, "complete") &&
    identical(manifest$analysis_hash, pa_object_sha256(manifest$analysis_spec)) &&
    identical(success$analysis_hash, manifest$analysis_hash) &&
    identical(success$run_manifest_sha256, pa_file_sha256(manifest_path)) &&
    identical(success$outputs[[1L]]$sha256, pa_file_sha256(effects_path)) &&
    identical(manifest$outputs[[1L]]$sha256, pa_file_sha256(effects_path)) &&
    identical(success$outputs[[2L]]$sha256, pa_file_sha256(profiles_path)) &&
    identical(manifest$outputs[[2L]]$sha256, pa_file_sha256(profiles_path)) &&
    identical(success$outputs[[3L]]$sha256, pa_file_sha256(profile_metrics_path)) &&
    identical(manifest$outputs[[3L]]$sha256, pa_file_sha256(profile_metrics_path)) &&
    identical(success$outputs[[1L]]$rows, 272L) &&
    identical(success$outputs[[2L]]$rows, 32L) &&
    identical(success$outputs[[3L]]$rows, 256L)
  if (!header_valid) pa_stop("Published paired-ablation provenance is invalid.")
  if (!is.null(source)) {
    source_valid <- identical(manifest$source$config_hash, source$manifest$config_hash) &&
      identical(manifest$source$run_manifest_sha256, source$run_manifest_sha256) &&
      identical(manifest$source$success_sha256, source$success_sha256)
    if (!source_valid) pa_stop("Published output is not bound to the requested source run.")
  }
  pa_validate_results(effects, manifest$source$config_hash)
  pa_validate_profiles(profiles, manifest$source$config_hash)
  pa_validate_profile_metrics(profile_metrics, manifest$source$config_hash)
  invisible(list(
    manifest = manifest, success = success, results = effects, profiles = profiles,
    profile_metrics = profile_metrics
  ))
}

pa_publish <- function(results, output_dir, source, analysis_spec, analysis_hash, started_at) {
  if (dir.exists(output_dir)) {
    pa_validate_published(output_dir, source)
    return(normalizePath(output_dir, mustWork = TRUE))
  }
  parent <- dirname(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  staging <- file.path(parent, paste0(".", basename(output_dir), ".staging-", Sys.getpid()))
  if (file.exists(staging) || dir.exists(staging)) {
    pa_stop("Refusing to reuse an existing staging path: ", staging)
  }
  dir.create(staging, recursive = FALSE, showWarnings = FALSE)
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
  output_paths <- list(
    effects = file.path(staging, "paired_ablation_effects.csv"),
    profiles = file.path(staging, "profile_discrepancies.csv"),
    profile_metrics = file.path(staging, "profile_metric_discrepancies.csv")
  )
  pa_atomic_csv(results$effects, output_paths$effects)
  pa_atomic_csv(results$profiles, output_paths$profiles)
  pa_atomic_csv(results$profile_metrics, output_paths$profile_metrics)
  manifest <- pa_manifest(source, analysis_spec, analysis_hash, output_paths, started_at)
  manifest_path <- file.path(staging, "run_manifest.json")
  pa_atomic_json(manifest, manifest_path)
  pa_atomic_json(pa_success(manifest_path, manifest), file.path(staging, "_SUCCESS.json"))
  pa_validate_published(staging, source)
  if (!file.rename(staging, output_dir)) pa_stop("Failed to atomically publish: ", output_dir)
  normalizePath(output_dir, mustWork = TRUE)
}

pa_worker_limit <- function(value = NULL) {
  available <- parallel::detectCores(logical = FALSE)
  if (!is.finite(available) || available < 1L) available <- 1L
  if (is.null(value)) return(as.integer(min(12L, available)))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 1L || !identical(as.character(parsed), value)) {
    pa_stop("--workers must be a positive integer.")
  }
  as.integer(min(parsed, available))
}

pa_parse_args <- function(args) {
  parsed <- list(
    source_run_dir = LOMM11_PAIRED_DEFAULT_RUN, output_dir = NULL,
    workers = NULL, mode = "dry-run", help = FALSE
  )
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token == "--help") {
      parsed$help <- TRUE
      index <- index + 1L
    } else if (token %in% c("--execute", "--dry-run")) {
      parsed$mode <- sub("^--", "", token)
      index <- index + 1L
    } else if (token == "--validate-output") {
      parsed$mode <- "validate-output"
      index <- index + 1L
    } else if (token %in% c("--source-run-dir", "--output-dir", "--workers")) {
      if (index == length(args)) pa_stop("Missing value after ", token, ".")
      field <- sub("^--", "", token)
      field <- gsub("-", "_", field, fixed = TRUE)
      parsed[[field]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      pa_stop("Unknown argument: ", token)
    }
  }
  parsed
}

pa_usage <- function() {
  paste(
    "Usage:",
    "  aggregate_realism_ablation.R [--source-run-dir DIR] [--dry-run]",
    "  aggregate_realism_ablation.R [--source-run-dir DIR] --execute [--workers N]",
    "  aggregate_realism_ablation.R --output-dir DIR --validate-output",
    sep = "\n"
  )
}

pa_run <- function(args = commandArgs(trailingOnly = TRUE)) {
  pa_require_packages()
  parsed <- pa_parse_args(args)
  if (parsed$help) {
    cat(pa_usage(), "\n")
    return(invisible(NULL))
  }
  workers <- pa_worker_limit(parsed$workers)
  if (parsed$mode == "validate-output") {
    if (is.null(parsed$output_dir)) pa_stop("--validate-output requires --output-dir.")
    validated <- pa_validate_published(parsed$output_dir)
    cat("LOMM 1.1 paired-ablation output validation: PASS\n")
    return(invisible(validated))
  }
  source <- pa_validate_source_run(parsed$source_run_dir)
  bootstrap_design <- pa_bootstrap_design()
  analysis_spec <- pa_analysis_spec(source, bootstrap_design)
  analysis_hash <- pa_object_sha256(analysis_spec)
  output_dir <- if (is.null(parsed$output_dir)) {
    pa_default_output_dir(source, analysis_hash)
  } else {
    normalizePath(parsed$output_dir, mustWork = FALSE)
  }
  if (parsed$mode == "dry-run") {
    cat(
      "LOMM 1.1 paired-ablation aggregation dry-run: PASS\n",
      "Source: ", source$directory, "\n",
      "Output: ", output_dir, "\n",
      "Analysis SHA-256: ", analysis_hash, "\n",
      "Rows: 272 contrasts, 32 profiles, and 256 profile-metric results; ",
      "bootstrap draws: 2,000; ",
      "clips: 100 in five strata\n",
      sep = ""
    )
    return(invisible(list(
      source = source, analysis_spec = analysis_spec,
      analysis_hash = analysis_hash, output_dir = output_dir
    )))
  }
  if (!identical(parsed$mode, "execute")) pa_stop("Unsupported execution mode.")
  if (dir.exists(output_dir)) {
    validated <- pa_validate_published(output_dir, source)
    cat("Reused validated paired-ablation aggregation:\n", output_dir, "\n")
    return(invisible(validated))
  }
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  heldout <- pa_read_heldout(source)
  variant_draws <- pa_compute_variant_draws(source, heldout, bootstrap_design, workers)
  results <- pa_build_results(variant_draws, heldout, source)
  pa_validate_results(results$effects, source$manifest$config_hash)
  pa_validate_profiles(results$profiles, source$manifest$config_hash)
  pa_validate_profile_metrics(results$profile_metrics, source$manifest$config_hash)
  published <- pa_publish(
    results, output_dir, source, analysis_spec, analysis_hash, started_at
  )
  validated <- pa_validate_published(published, source)
  cat(
    "Published LOMM 1.1 paired-ablation aggregation:\n", published, "\n",
    "Rows: ", nrow(validated$results), " contrasts and ",
    nrow(validated$profiles), " profiles and ", nrow(validated$profile_metrics),
    " profile-metric results; bootstrap draws: 2,000\n", sep = ""
  )
  invisible(validated)
}

if (sys.nframe() == 0L) {
  tryCatch(
    pa_run(),
    error = function(condition) {
      message("ERROR: ", conditionMessage(condition))
      quit(save = "no", status = 1L, runLast = FALSE)
    }
  )
}
