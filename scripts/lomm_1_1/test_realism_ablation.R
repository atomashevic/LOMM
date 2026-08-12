#!/usr/bin/env Rscript

lomm11_test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot locate the LOMM 1.1 tests.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

LOMM11_TEST_PATH <- lomm11_test_script_path()
LOMM11_TEST_DIRECTORY <- dirname(LOMM11_TEST_PATH)
LOMM11_TEST_REPO <- normalizePath(file.path(LOMM11_TEST_DIRECTORY, "..", ".."))

source(file.path(LOMM11_TEST_REPO, "scripts", "preprocessing", "compositional_math.R"))
source(file.path(LOMM11_TEST_REPO, "scripts", "preprocessing", "compositional_views.R"))
source(file.path(
  LOMM11_TEST_REPO, "scripts", "preprocessing", "empirical_compositional_bundle.R"
))
LOMM_BUILDER_PATH <- file.path(
  LOMM11_TEST_REPO, "scripts", "preprocessing", "build_empirical_compositional_views.R"
)
source(file.path(
  LOMM11_TEST_REPO, "scripts", "preprocessing", "validate_published_bundle.R"
))
source(file.path(LOMM11_TEST_REPO, "scripts", "simulations", "revision_spec.R"))
source(file.path(LOMM11_TEST_DIRECTORY, "realism_ablation_spec.R"))
source(file.path(LOMM11_TEST_DIRECTORY, "realism_generator.R"))
source(file.path(LOMM11_TEST_DIRECTORY, "realism_metrics.R"))
LOMM11_RUNNER_PATH <- file.path(LOMM11_TEST_DIRECTORY, "run_realism_ablation.R")
source(LOMM11_RUNNER_PATH)

lomm11_test_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

lomm11_test_error <- function(expression, pattern = NULL) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  if (is.null(error)) stop("Expected an error.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(error))) {
    stop("Unexpected error: ", conditionMessage(error), call. = FALSE)
  }
  invisible(error)
}

lomm11_test_clone <- function(value) {
  unserialize(serialize(value, NULL))
}

lomm11_test_command <- function(arguments) {
  output <- suppressWarnings(system2(
    "Rscript",
    c(file.path(LOMM11_TEST_DIRECTORY, "build_realism_ablation_plan.R"), arguments),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(output = output, status = attr(output, "status") %||% 0L)
}

`%||%` <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

lomm11_test_config <- function() {
  path <- file.path(LOMM11_TEST_DIRECTORY, "realism_ablation_config.json")
  config <- lomm11_read_config(path)
  changed <- lomm11_test_clone(config)
  changed$measurement$residual_sd <- 0.30
  lomm11_test_error(lomm11_validate_config(changed), "Measurement contract")
  changed <- lomm11_test_clone(config)
  changed$input_bundle$files$probability_view.csv <- paste(rep("0", 64L), collapse = "")
  lomm11_test_error(lomm11_validate_config(changed), "Input bundle")
  changed <- lomm11_test_clone(config)
  changed$future_execution$worker_limit <- 13L
  lomm11_test_error(lomm11_validate_config(changed), "future-execution")
  config
}

lomm11_test_certified_plan <- function(config) {
  directory <- file.path(
    LOMM11_TEST_REPO, config$input_bundle$repo_relative_path
  )
  lomm_validate_published_bundle(directory)
  tables <- lomm_validation_read_tables(directory)
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) seed_before <- get(".Random.seed", envir = .GlobalEnv)
  plan <- lomm11_build_plan(
    config, tables[["clip_qc.csv"]], tables[["probability_view.csv"]]
  )
  if (seed_exists) {
    lomm11_test_assert(
      identical(seed_before, get(".Random.seed", envir = .GlobalEnv)),
      "Plan construction changed the RNG state."
    )
  } else {
    lomm11_test_assert(
      !exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
      "Plan construction created an RNG state."
    )
  }
  expected <- c(
    candidate_grid.csv = 216L,
    fold_assignments.csv = 100L,
    segment_manifest.csv = 317L,
    training_plan.csv = 3240L,
    selection_plan.csv = 40L,
    heldout_plan.csv = 4000L,
    heldout_evaluation_plan.csv = 8000L
  )
  lomm11_test_assert(
    identical(vapply(plan$tables, nrow, integer(1)), expected),
    "Plan row counts differ from the locked workload."
  )
  lomm11_test_assert(
    identical(
      plan$target_contract$randomness,
      lomm11_rng_provenance_contract(config)
    ) && identical(
      plan$target_contract$randomness$state_identity_fields,
      as.list(c(
        "state_derivation_version", "seed_base", "seed_group", "clip",
        "segment", "component"
      ))
    ),
    "Scientific-contract RNG provenance is incomplete."
  )
  lomm11_test_assert(
    identical(as.integer(table(plan$tables$fold_assignments.csv$fold)), rep(20L, 5L)),
    "Fold sizes differ from 20 clips each."
  )
  training_pairing <- table(plan$tables$training_plan.csv$seed_group)
  heldout_pairing <- table(plan$tables$heldout_plan.csv$seed_group)
  evaluation_pairing <- table(plan$tables$heldout_evaluation_plan.csv$seed_group)
  lomm11_test_assert(
    length(training_pairing) == 405L && all(training_pairing == 8L),
    "Training common-random-number groups are invalid."
  )
  lomm11_test_assert(
    length(heldout_pairing) == 500L && all(heldout_pairing == 8L) &&
      length(evaluation_pairing) == 500L && all(evaluation_pairing == 16L),
    "Held-out common-random-number groups are invalid."
  )
  probability <- tables[["probability_view.csv"]]
  primary <- probability$analysis_variant == "complete_case"
  sensitivity <- probability$analysis_variant == "ilr_gap2"
  lomm11_test_assert(
    sum(primary) == 3557L && !any(probability$is_imputed[primary]) &&
      sum(sensitivity) == 3632L && sum(probability$is_imputed[sensitivity]) == 75L,
    "Certified primary or sensitivity view counts are invalid."
  )
  list(plan = plan, tables = tables)
}

lomm11_test_generation_plan_contract <- function(config, certified) {
  context <- lomm11_test_runner_context(certified, tempfile("lomm11-fold-cache-"))
  context$config <- config
  basis <- lomm11_runner_basis(certified$tables)
  view_index <- lomm11_view_index_from_ledger(certified$tables[["frame_ledger.csv"]])
  cache <- lomm11_runner_fold_cache(
    1L, context, certified$tables, basis, view_index
  )
  assignments <- certified$plan$tables$fold_assignments.csv
  heldout_clips <- as.character(assignments$clip[assignments$fold == 1L])
  training_clips <- as.character(assignments$clip[assignments$fold != 1L])
  ledger <- certified$tables[["frame_ledger.csv"]]
  check_plan <- function(plan, clips, label) {
    expected <- ledger[
      as.character(ledger$clip) %in% clips &
        as.logical(ledger$sensitivity_included), , drop = FALSE
    ]
    expected <- expected[order(expected$clip, expected$frame_index), , drop = FALSE]
    segments <- split(
      seq_len(nrow(plan)),
      factor(
        plan$generation_segment_id,
        levels = unique(plan$generation_segment_id)
      )
    )
    segment_fields_valid <- all(vapply(segments, function(index) {
      identical(plan$generation_segment_position[index], seq_along(index)) &&
        all(plan$generation_segment_length[index] == length(index)) &&
        length(unique(plan$clip[index])) == 1L &&
        all(diff(plan$frame_index[index]) == 1L)
    }, logical(1)))
    lomm11_test_assert(
      nrow(plan) == nrow(expected) &&
        identical(as.character(plan$frame_key), as.character(expected$frame_key)) &&
        !anyNA(plan$generation_segment_position) &&
        !anyNA(plan$generation_segment_length) && segment_fields_valid,
      paste(label, "generation plan violates its exact frame/segment contract.")
    )
  }
  check_plan(cache$training_generation_plan, training_clips, "Training-fold")
  check_plan(cache$heldout_generation_plan, heldout_clips, "Held-out-fold")
  invisible(TRUE)
}

lomm11_test_dynamics <- function(config, candidates) {
  lomm11_test_assert(
    nrow(candidates) == 216L && all(candidates$eta < 0) &&
      all(candidates$zeta < 0) && all(candidates$selection_eligible),
    "Candidate stability constraints are invalid."
  )
  axes <- unique(candidates[c("period_frames", "damping_ratio")])
  for (index in seq_len(nrow(axes))) {
    system <- lomm11_build_unit_position_system(
      axes$period_frames[[index]], axes$damping_ratio[[index]],
      dt = config$dynamics$dt,
      tolerance = config$dynamics$lyapunov_tolerance
    )
    exact <- revision_exact_transition(system$eta, system$zeta, config$dynamics$dt)
    lomm11_test_assert(
      max(abs(system$transition - exact$transition)) <= 1e-14 &&
        abs(system$stationary_covariance[1L, 1L] - 1) <= 1e-10 &&
        system$spectral_radius < 1 && system$lyapunov_residual <= 1e-10,
      "Exact transition or stationary normalization failed."
    )
  }
  invisible(TRUE)
}

lomm11_test_random_streams_and_gates <- function() {
  kind_before <- RNGkind()
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) seed_before <- get(".Random.seed", envir = .GlobalEnv)
  first <- lomm11_named_substream_seeds(11082026L, "fold01", "clip01", "segment01")
  second <- lomm11_named_substream_seeds(11082026L, "fold01", "clip01", "segment01")
  other_segment <- lomm11_named_substream_seeds(
    11082026L, "fold01", "clip01", "segment02"
  )
  state_signatures <- vapply(first, paste, collapse = ",", character(1))
  lomm11_test_assert(
    identical(first, second) && !anyDuplicated(state_signatures) &&
      !identical(first, other_segment),
    "Named component substreams are not deterministic and separated."
  )
  fixed_state <- lomm11_component_rng_state(
    "training|F01|CORE001|R001", "15550.mp4",
    "15550.mp4::seg001", "residuals", 2053918542L
  )
  expected_state <- c(
    10407L, -426046744L, -1776379206L, -742809275L,
    -478028703L, -677225558L, -769800302L
  )
  fixed_draw <- lomm11_with_seed(fixed_state, function() stats::runif(4L))
  lomm11_test_assert(
    identical(fixed_state, expected_state) &&
      max(abs(fixed_draw - c(
        0.7104892404, 0.8797622363, 0.4937516674, 0.5987115352
      ))) < 1e-9,
    "L'Ecuyer-CMRG state derivation differs from its fixed test vector."
  )
  schedule <- lomm11_draw_gate_schedule(80L, 3L, first[["gate_schedule"]])
  lomm11_test_assert(
    all(schedule$gates >= 0.25) && all(schedule$gates <= 1) &&
      identical(dim(schedule$gates), c(80L, 3L)),
    "Semi-Markov gates violate their bounds or dimensions."
  )
  lomm11_test_error(
    lomm11_with_seed(fixed_state, function() stop("seeded fixture failure")),
    "seeded fixture failure"
  )
  lomm11_test_assert(
    identical(RNGkind(), kind_before),
    "Seeded helpers changed the caller RNG kind."
  )
  if (seed_exists) {
    lomm11_test_assert(
      identical(seed_before, get(".Random.seed", envir = .GlobalEnv)),
      "Seeded pure helpers changed the caller RNG state."
    )
  } else {
    lomm11_test_assert(
      !exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
      "Seeded pure helpers left an RNG state behind."
    )
  }
  invisible(TRUE)
}

lomm11_test_full_workload_rng_states <- function(certified) {
  plan <- certified$plan$tables
  ledger <- certified$tables[["frame_ledger.csv"]]
  ledger <- ledger[as.logical(ledger$sensitivity_included), , drop = FALSE]
  assignments <- plan$fold_assignments.csv
  component_rows <- function(clips) {
    shared <- data.frame(
      clip = rep(clips, each = 2L),
      segment = LOMM11_CLIP_SHARED_SEGMENT,
      component = rep(
        c("between_clip_baseline", "clip_scale"), length(clips)
      ),
      stringsAsFactors = FALSE
    )
    segments <- unique(ledger[
      as.character(ledger$clip) %in% clips,
      c("clip", "sensitivity_segment_id"), drop = FALSE
    ])
    segmented <- data.frame(
      clip = rep(as.character(segments$clip), each = 4L),
      segment = rep(as.character(segments$sensitivity_segment_id), each = 4L),
      component = rep(
        c("gate_schedule", "initial_state", "innovations", "residuals"),
        nrow(segments)
      ),
      stringsAsFactors = FALSE
    )
    rbind(shared, segmented)
  }
  expand_groups <- function(groups, components, stage) {
    group_index <- rep(seq_len(nrow(groups)), each = nrow(components))
    component_index <- rep(seq_len(nrow(components)), nrow(groups))
    data.frame(
      stage = stage,
      master_seed = groups$seed_base[group_index],
      seed_group = groups$seed_group[group_index],
      clip = components$clip[component_index],
      segment = components$segment[component_index],
      component = components$component[component_index],
      stringsAsFactors = FALSE
    )
  }
  rows <- unlist(lapply(seq_len(5L), function(fold) {
    heldout_clips <- as.character(assignments$clip[assignments$fold == fold])
    training_clips <- as.character(assignments$clip[assignments$fold != fold])
    training_groups <- unique(plan$training_plan.csv[
      plan$training_plan.csv$fold == fold,
      c("seed_group", "seed_base"), drop = FALSE
    ])
    heldout_groups <- unique(plan$heldout_plan.csv[
      plan$heldout_plan.csv$fold == fold,
      c("seed_group", "seed_base"), drop = FALSE
    ])
    list(
      expand_groups(
        training_groups, component_rows(training_clips), "training"
      ),
      expand_groups(heldout_groups, component_rows(heldout_clips), "heldout")
    )
  }), recursive = FALSE)
  keys <- do.call(rbind, rows)
  lomm11_test_assert(
    nrow(keys) == 303584L && sum(keys$stage == "training") == 231984L &&
      sum(keys$stage == "heldout") == 71600L,
    "Full-workload component RNG key count is invalid."
  )
  canonical <- mapply(
    lomm11_component_rng_key,
    keys$seed_group, keys$clip, keys$segment, keys$component, keys$master_seed,
    USE.NAMES = FALSE
  )
  lomm11_test_assert(
    !anyDuplicated(canonical),
    "Full-workload component RNG keys are not unique."
  )
  derive <- function() vapply(
    canonical, lomm11_lecuyer_state_from_key, integer(7L)
  )
  first <- derive()
  second <- derive()
  state_rows <- as.data.frame(t(first), stringsAsFactors = FALSE)
  unsigned <- matrix(as.numeric(first[-1L, ]), nrow = 6L)
  unsigned[unsigned < 0] <- unsigned[unsigned < 0] + 2^32
  moduli <- matrix(
    c(rep(4294967087, 3L), rep(4294944443, 3L)),
    nrow = 6L, ncol = ncol(unsigned)
  )
  lomm11_test_assert(
    identical(first, second) && !anyNA(first) &&
      first[1L, 1L] == 10407L && all(first[1L, ] == 10407L) &&
      all(unsigned > 0) && all(unsigned < moduli) &&
      all(unsigned != 2^31) && !anyDuplicated(state_rows),
    "Full-workload L'Ecuyer-CMRG states collide or are not reproducible."
  )
  invisible(TRUE)
}

lomm11_test_metric_fixture <- function() {
  channels <- LOMM11_METRIC_CHANNELS
  basis <- lomm_ilr_basis(channels)
  n <- 12L
  ilr <- outer(seq_len(n), seq_len(10L), function(row, column) {
    0.08 * sin(row / (column + 1)) + 0.01 * column
  })
  colnames(ilr) <- colnames(basis)
  probability <- lomm_ilr_inverse(ilr, basis)
  clr <- lomm_clr(probability)
  metadata <- data.frame(
    observation_key = paste0("fixture|", seq_len(n)),
    analysis_variant = "complete_case",
    frame_key = paste0("clip|", seq_len(n)),
    clip = "clip",
    frame_index = seq_len(n),
    truth_label = "fixture",
    segment_id = rep(c("clip::1", "clip::2"), each = 6L),
    segment_position = rep(seq_len(6L), 2L),
    segment_length = 6L,
    stringsAsFactors = FALSE
  )
  probability_view <- cbind(metadata, as.data.frame(probability))
  clr_view <- cbind(metadata, as.data.frame(clr))
  names(clr_view)[(ncol(metadata) + 1L):ncol(clr_view)] <- paste0("clr_", channels)
  ilr_view <- cbind(metadata, as.data.frame(ilr))
  contributions <- lomm11_metric_contributions(
    probability_view, clr_view, ilr_view
  )
  lomm11_test_assert(
    nrow(contributions) == 136L &&
      all(table(contributions$family) == 68L) &&
      !anyDuplicated(contributions$contribution_key),
    "Metric fixture does not yield the exact contribution contract."
  )
  boundary <- lomm11_univariate_metric_values(
    c(0, 1, 2, 100, 101, 102),
    rep(c("a", "b"), each = 3L),
    rep(1:3, 2L)
  )
  lomm11_test_assert(
    identical(unname(boundary[["difference_iqr"]]), 0),
    "Metric differences bridged a segment boundary."
  )
  invisible(TRUE)
}

lomm11_test_generator_fixture <- function() {
  channels <- LOMM11_DEFAULT_CHANNELS
  coordinates <- sprintf("ilr_%02d", 1:10)
  basis <- lomm_ilr_basis(channels)
  directions <- diag(10)[, 1:3, drop = FALSE]
  rownames(directions) <- coordinates
  colnames(directions) <- sprintf("factor_%02d", 1:3)
  geometry <- structure(list(
    coordinate_columns = coordinates,
    n_directions = 3L,
    training_clips = c("training_a", "training_b"),
    n_training_rows = 60L,
    center = stats::setNames(rep(0, 10L), coordinates),
    directions = directions,
    direction_eigenvalues = stats::setNames(rep(1, 3L), colnames(directions)),
    relative_scales = stats::setNames(rep(1, 3L), colnames(directions)),
    loadings = directions,
    between_clip_covariance = diag(0.04, 10L),
    within_clip_covariance = diag(10L),
    psd_floor = 1e-8
  ), class = "lomm11_training_geometry")
  dimnames(geometry$between_clip_covariance) <- list(coordinates, coordinates)
  lomm11_validate_training_geometry(geometry)
  frames <- c(1:30, 40:69)
  segments <- rep(c("clip::sensitivity001", "clip::sensitivity002"), each = 30L)
  frame_plan <- data.frame(
    analysis_variant = "ilr_gap2",
    frame_key = paste("clip", frames, sep = "|"),
    clip = "clip",
    frame_index = frames,
    truth_label = "fixture",
    segment_id = segments,
    segment_position = rep(1:30, 2L),
    segment_length = 30L,
    stringsAsFactors = FALSE
  )
  variant <- function(B, S, G) list(B = B, S = S, G = G)
  generate <- function(flags) lomm11_generate_clip_panel(
    frame_plan, geometry, basis,
    period_frames = 12, damping_ratio = 0.2, kappa = 1,
    variant = flags, master_seed = 11082026L, pairing_key = "fixture"
  )
  core <- generate(variant(0L, 0L, 0L))
  between <- generate(variant(1L, 0L, 0L))
  scaled <- generate(variant(0L, 1L, 0L))
  gated <- generate(variant(0L, 0L, 1L))
  lomm11_test_assert(
    identical(core$generated_frame_index, frames) && length(core$segments) == 2L,
    "Generator bridged the unresolved sensitivity gap."
  )
  core_ilr <- as.matrix(core$panel[coordinates])
  between_ilr <- as.matrix(between$panel[coordinates])
  scaled_ilr <- as.matrix(scaled$panel[coordinates])
  baseline <- between$component_draws$between_clip_baseline
  expected_baseline <- matrix(
    baseline, nrow = nrow(core_ilr), ncol = length(baseline), byrow = TRUE
  )
  lomm11_test_assert(
    max(abs((between_ilr - core_ilr) - expected_baseline)) <= 1e-12,
    "Between-clip baseline is not an additive shared ILR effect."
  )
  scale <- scaled$component_draws$clip_scale
  lomm11_test_assert(
    max(abs(scaled_ilr - scale * core_ilr)) <= 1e-12,
    "Clip scale does not multiply the signal and residual together."
  )
  for (segment in names(core$segments)) {
    lomm11_test_assert(
      identical(core$segments[[segment]]$latent_position,
                between$segments[[segment]]$latent_position) &&
        identical(core$segments[[segment]]$residuals,
                  scaled$segments[[segment]]$residuals) &&
        identical(core$segments[[segment]]$hard_gate_states,
                  gated$segments[[segment]]$hard_gate_states),
      "A factorial toggle shifted a non-target stochastic component."
    )
  }
  first_state <- core$segments[[1L]]$latent_position[1L, ]
  second_state <- core$segments[[2L]]$latent_position[1L, ]
  lomm11_test_assert(
    !identical(first_state, second_state) &&
      any(gated$segments[[1L]]$gates != 1),
    "Segment-specific initialization or gate activation is invalid."
  )
  invisible(TRUE)
}

lomm11_test_training_isolation <- function(certified) {
  assignments <- certified$plan$tables$fold_assignments.csv
  training_clips <- assignments$clip[assignments$fold != 1L]
  heldout_clips <- assignments$clip[assignments$fold == 1L]
  ilr <- certified$tables[["ilr_view.csv"]]
  original <- lomm11_fit_training_geometry(ilr, training_clips)
  modified <- ilr
  heldout <- modified$clip %in% heldout_clips
  coordinates <- sprintf("ilr_%02d", 1:10)
  modified[heldout, coordinates] <- modified[heldout, coordinates] + 100
  changed <- lomm11_fit_training_geometry(modified, training_clips)
  lomm11_test_assert(
    identical(original$center, changed$center) &&
      identical(original$loadings, changed$loadings) &&
      identical(original$between_clip_covariance, changed$between_clip_covariance),
    "Held-out mutations changed training-only geometry."
  )
  invisible(TRUE)
}

lomm11_test_cli_guards <- function() {
  output_root <- file.path(
    LOMM11_TEST_REPO, "simulation_outputs", "lomm_1_1", "empirical_realism_ablation"
  )
  before <- if (dir.exists(output_root)) {
    list.files(output_root, all.files = TRUE, recursive = TRUE, no.. = TRUE)
  } else {
    character()
  }
  dry <- lomm11_test_command("--dry-run")
  lomm11_test_assert(dry$status == 0L, paste(dry$output, collapse = "\n"))
  after <- if (dir.exists(output_root)) {
    list.files(output_root, all.files = TRUE, recursive = TRUE, no.. = TRUE)
  } else {
    character()
  }
  lomm11_test_assert(identical(before, after), "Dry-run changed the plan namespace.")
  revision <- lomm11_test_command(c(
    "--output-root", "./simulation_outputs/revision_v2/lomm11", "--dry-run"
  ))
  lomm11_test_assert(revision$status != 0L, "revision_v2 output-root guard failed.")
  malformed <- lomm11_test_command(c("--input-bundle", "--dry-run"))
  lomm11_test_assert(malformed$status != 0L, "Malformed CLI guard failed.")
  invisible(TRUE)
}

lomm11_test_static_runtime_scan <- function() {
  paths <- file.path(
    LOMM11_TEST_DIRECTORY,
    c("realism_generator.R", "realism_metrics.R", "run_realism_ablation.R")
  )
  source_text <- paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")
  forbidden <- "EGAnet|dynEGA|GraphicalVAR|GIMME|qlogis|makeForkCluster|mclapply"
  lomm11_test_assert(
    !grepl(forbidden, source_text, ignore.case = TRUE),
    "LOMM 1.1 runtime modules reference prohibited estimators or fork backends."
  )
  invisible(TRUE)
}

lomm11_test_runner_context <- function(certified, output_directory) {
  list(
    args = list(resume = FALSE),
    output_dir = output_directory,
    manifest = list(
      config_hash = paste(rep("a", 64L), collapse = ""),
      config_prefix = paste(rep("a", 12L), collapse = "")
    ),
    manifest_sha256 = paste(rep("b", 64L), collapse = ""),
    input_hash = paste(rep("c", 64L), collapse = ""),
    plan = certified$plan$tables
  )
}

lomm11_test_selection_fixture <- function(context) {
  plan <- context$plan$selection_plan.csv
  candidates <- context$plan$candidate_grid.csv
  rows <- lapply(seq_len(nrow(plan)), function(index) {
    selection <- plan[index, , drop = FALSE]
    candidate <- candidates[
      candidates$variant_id == selection$variant_id[[1L]], , drop = FALSE
    ][1L, , drop = FALSE]
    data.frame(
      selection_key = selection$selection_key,
      fold = selection$fold,
      variant_id = selection$variant_id,
      candidate_id = candidate$candidate_id,
      core_candidate_id = candidate$core_candidate_id,
      period_frames = candidate$period_frames,
      damping_ratio = candidate$damping_ratio,
      kappa = candidate$kappa,
      probability_loss = 0,
      logratio_loss = 0,
      selection_loss = 0,
      max_metric_discrepancy = 0,
      training_units = 3L,
      selected_before_heldout = TRUE,
      config_hash = context$manifest$config_hash,
      input_hash = context$input_hash,
      stringsAsFactors = FALSE
    )
  })
  lomm11_runner_coerce_frame(
    do.call(rbind, rows),
    lomm11_runner_locked_output_contract()$selected_candidates.csv,
    "Test selection fixture"
  )
}

lomm11_test_checkpoint_row <- function(plan_row, context, candidate_id,
                                       stage) {
  training <- identical(stage, "training")
  data.frame(
    unit_key = plan_row$unit_key,
    panel_key = plan_row$panel_key,
    stage = stage,
    analysis_view = plan_row$analysis_view,
    fold = plan_row$fold,
    variant_id = plan_row$variant_id,
    candidate_id = candidate_id,
    replicate = if (training) plan_row$training_rep else plan_row$heldout_rep,
    status = "ok",
    error_message = NA_character_,
    probability_loss = 0,
    logratio_loss = 0,
    selection_loss = 0,
    max_metric_discrepancy = 0,
    seed_group = plan_row$seed_group,
    seed_base = plan_row$seed_base,
    runtime_sec = 1,
    config_hash = context$manifest$config_hash,
    input_hash = context$input_hash,
    panel_hash = paste(rep("d", 64L), collapse = ""),
    stringsAsFactors = FALSE
  )
}

lomm11_test_checkpoint_metadata_guards <- function(certified) {
  output <- tempfile("lomm11-checkpoint-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  context <- lomm11_test_runner_context(certified, output)
  selected <- lomm11_test_selection_fixture(context)
  training_plan <- context$plan$training_plan.csv[1L, , drop = FALSE]
  training <- lomm11_test_checkpoint_row(
    training_plan, context, training_plan$candidate_id[[1L]], "training"
  )
  lomm11_runner_validate_checkpoint(training, context, selected = selected)
  mutations <- list(
    panel_key = paste0(training$panel_key, "-mutated"),
    stage = "heldout",
    analysis_view = "ilr_gap2",
    fold = training$fold + 1L,
    variant_id = "999",
    candidate_id = paste0(training$candidate_id, "-mutated"),
    replicate = training$replicate + 1L,
    seed_group = paste0(training$seed_group, "-mutated"),
    seed_base = training$seed_base + 1L
  )
  for (field in names(mutations)) {
    changed <- training
    changed[[field]] <- mutations[[field]]
    lomm11_test_error(
      lomm11_runner_validate_checkpoint(changed, context, selected = selected),
      "immutable metadata"
    )
  }
  heldout_plan <- context$plan$heldout_evaluation_plan.csv[1L, , drop = FALSE]
  selected_row <- selected[
    selected$selection_key == heldout_plan$selection_key[[1L]], , drop = FALSE
  ]
  heldout <- lomm11_test_checkpoint_row(
    heldout_plan, context, selected_row$candidate_id[[1L]], "heldout"
  )
  lomm11_runner_validate_checkpoint(heldout, context, selected = selected)
  heldout$candidate_id <- paste0(heldout$candidate_id, "-mutated")
  lomm11_test_error(
    lomm11_runner_validate_checkpoint(heldout, context, selected = selected),
    "immutable metadata"
  )
  changed_selection <- selected
  changed_selection$core_candidate_id[[1L]] <- "CORE999"
  lomm11_test_error(
    lomm11_runner_validate_checkpoint(
      lomm11_test_checkpoint_row(
        heldout_plan, context, selected_row$candidate_id[[1L]], "heldout"
      ),
      context, selected = changed_selection
    ),
    "plan-derived IDs"
  )
  invisible(TRUE)
}

lomm11_test_write_lock_owner <- function(context, owner) {
  lock <- paste0(context$output_dir, ".lock")
  dir.create(lock, recursive = TRUE)
  lomm11_runner_atomic_json(owner, file.path(lock, "owner.json"))
  lock
}

lomm11_test_lock_guards <- function(certified) {
  root <- tempfile("lomm11-lock-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  context <- lomm11_test_runner_context(certified, file.path(root, "result"))
  context$args$resume <- TRUE
  dead_pid <- 2147483646L
  foreign <- lomm11_runner_lock_owner(
    context, pid = dead_pid, host = "foreign-host.invalid"
  )
  lock <- lomm11_test_write_lock_owner(context, foreign)
  lomm11_test_error(lomm11_runner_acquire_lock(context), "foreign-host")
  lomm11_test_assert(dir.exists(lock), "Foreign-host lock was reclaimed.")
  unlink(lock, recursive = TRUE)
  local <- lomm11_runner_lock_owner(context, pid = dead_pid)
  lock <- lomm11_test_write_lock_owner(context, local)
  context$args$resume <- FALSE
  lomm11_test_error(lomm11_runner_acquire_lock(context), "non-resume")
  lomm11_test_assert(dir.exists(lock), "Local lock was reclaimed without resume.")
  context$args$resume <- TRUE
  acquired <- lomm11_runner_acquire_lock(context)
  lomm11_test_assert(identical(acquired, lock), "Dead local lock was not reclaimed.")
  lomm11_runner_release_lock(acquired, context)
  mismatch <- lomm11_runner_lock_owner(context, pid = dead_pid)
  mismatch$config_hash <- paste(rep("f", 64L), collapse = "")
  lock <- lomm11_test_write_lock_owner(context, mismatch)
  lomm11_test_error(lomm11_runner_acquire_lock(context), "differs from this plan")
  lomm11_test_assert(dir.exists(lock), "Mismatched-config lock was reclaimed.")
  invisible(TRUE)
}

lomm11_test_finalizing_recovery <- function(certified) {
  root <- tempfile("lomm11-finalizing-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  context <- lomm11_test_runner_context(certified, file.path(root, "result"))
  dir.create(context$output_dir)
  utils::write.csv(
    data.frame(unit_key = "fixture"), lomm11_runner_checkpoint_path(context),
    row.names = FALSE
  )
  state <- lomm11_runner_state_directory(context)
  dir.create(file.path(state, "heldout_metric_chunks"), recursive = TRUE)
  writeLines("fixture", file.path(state, "heldout_metric_chunks", "fixture.csv"))
  finalizing <- lomm11_runner_move_state_to_finalizing(context)
  lomm11_test_assert(
    dir.exists(finalizing) && !dir.exists(state),
    "State did not move atomically outside the output directory."
  )
  lomm11_test_error(
    lomm11_runner_cleanup_finalizing(context), "only after strict success"
  )
  pending <- lomm11_runner_pending_success_path(context)
  lomm11_runner_atomic_json(
    list(
      config_hash = context$manifest$config_hash,
      prospective_manifest_sha256 = context$manifest_sha256
    ),
    pending
  )
  lomm11_runner_restore_finalizing_state(context)
  lomm11_test_assert(
    dir.exists(state) && !dir.exists(finalizing) &&
      !file.exists(file.path(state, "_FINALIZING.json")) &&
      !file.exists(pending),
    "Finalizing state was not restored for an incomplete resume."
  )
  finalizing <- lomm11_runner_move_state_to_finalizing(context)
  lomm11_runner_atomic_json(
    list(status = "fixture"), file.path(context$output_dir, "_SUCCESS.json")
  )
  lomm11_test_error(
    lomm11_runner_cleanup_finalizing(context), "invalid header"
  )
  lomm11_test_assert(
    dir.exists(finalizing), "Invalid-success finalizing state was removed."
  )
  invisible(TRUE)
}

lomm11_test_heldout_consolidation_tamper <- function() {
  root <- tempfile("lomm11-heldout-consolidation-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- file.path(root, "state")
  dir.create(file.path(state, "heldout_metric_chunks"), recursive = TRUE)
  contract <- lomm11_runner_locked_output_contract()$heldout_metric_discrepancies.csv
  groups <- lomm11_metric_groups()
  units <- c("heldout-fixture-a", "heldout-fixture-b")
  make_chunk <- function(unit_key, index) {
    frame <- lomm11_runner_schema_frame(contract, 16L)
    frame$unit_key <- unit_key
    frame$panel_key <- paste0("panel-", index)
    frame$fold <- as.integer(index)
    frame$variant_id <- "000"
    frame$heldout_rep <- 1L
    frame$analysis_view <- if (index == 1L) "complete_case" else "ilr_gap2"
    frame$family <- groups$family
    frame$metric <- groups$metric
    frame$median_diff_scaled <- seq_len(16L) / 100
    frame$wasserstein_scaled <- seq_len(16L) / 50
    frame$max_discrepancy <- seq_len(16L) / 25
    frame$empirical_n <- 20L
    frame$synthetic_n <- 20L
    frame$metric_scale <- 1
    frame$status <- "ok"
    frame$error_message <- NA_character_
    frame$seed_group <- paste0("fixture-seed-", index)
    frame$config_hash <- paste(rep("a", 64L), collapse = "")
    frame$input_hash <- paste(rep("b", 64L), collapse = "")
    frame$panel_hash <- paste(rep("c", 64L), collapse = "")
    lomm11_runner_coerce_frame(frame, contract, "Held-out fixture chunk")
  }
  for (index in seq_along(units)) {
    lomm11_runner_atomic_csv(
      make_chunk(units[[index]], index),
      lomm11_runner_metric_chunk_file(state, units[[index]])
    )
  }
  rebuilt <- lomm11_runner_rebuild_heldout_metric_chunks(
    state, units, contract
  )
  target <- file.path(root, "heldout_metric_discrepancies.csv")
  lomm11_runner_compare_or_publish_heldout_metrics(rebuilt, target)
  tampered <- rebuilt
  tampered$median_diff_scaled[[1L]] <- tampered$median_diff_scaled[[1L]] + 1
  utils::write.csv(tampered, target, row.names = FALSE, na = "")
  lomm11_test_error(
    lomm11_runner_compare_or_publish_heldout_metrics(rebuilt, target),
    "differs from the complete per-unit chunk rebuild"
  )
  utils::write.csv(rebuilt, target, row.names = FALSE, na = "")
  first_chunk_path <- lomm11_runner_metric_chunk_file(state, units[[1L]])
  first_chunk <- lomm11_runner_read_csv_contract(
    first_chunk_path, contract, "Held-out fixture chunk"
  )
  first_chunk$wasserstein_scaled[[1L]] <-
    first_chunk$wasserstein_scaled[[1L]] + 1
  utils::write.csv(first_chunk, first_chunk_path, row.names = FALSE, na = "")
  rebuilt_tampered <- lomm11_runner_rebuild_heldout_metric_chunks(
    state, units, contract
  )
  lomm11_test_error(
    lomm11_runner_compare_or_publish_heldout_metrics(
      rebuilt_tampered, target
    ),
    "differs from the complete per-unit chunk rebuild"
  )
  invisible(TRUE)
}

lomm11_test_batch_integrity_gate <- function() {
  output <- tempfile("lomm11-batch-integrity-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  context <- list(output_dir = output)
  checkpoint <- lomm11_runner_schema_frame(
    lomm11_runner_locked_output_contract()$checkpoint_results.csv
  )
  failure <- data.frame(
    status = "error",
    error_message = "could not find function revision_integrated_input",
    stringsAsFactors = FALSE
  )
  training <- rep(list(list(checkpoint = failure)), 2L)
  lomm11_test_error(
    lomm11_runner_commit_training_batch(
      training, checkpoint, context, full_batch = TRUE
    ),
    "Systematic training batch failure before artifact commit"
  )
  heldout <- rep(list(list(results = list(
    list(checkpoint = failure), list(checkpoint = failure)
  ))), 2L)
  lomm11_test_error(
    lomm11_runner_commit_heldout_batch(
      heldout, checkpoint, context, full_batch = TRUE
    ),
    "Systematic heldout batch failure before artifact commit"
  )
  lomm11_test_assert(
    nrow(checkpoint) == 0L &&
      length(list.files(
        output, recursive = TRUE, all.files = TRUE, no.. = TRUE
      )) == 0L,
    "Systematic batch failure wrote fragments or checkpoint additions."
  )
  invisible(TRUE)
}

lomm11_test_worker_dependency_export <- function() {
  exported <- lomm11_runner_worker_names()
  required <- c("revision_exact_transition", "revision_integrated_input")
  missing_required <- setdiff(required, exported)
  leaked_tests <- exported[startsWith(exported, "lomm11_test_")]
  lomm11_test_assert(
    length(missing_required) == 0L && length(leaked_tests) == 0L,
    paste0(
      "PSOCK worker-export set is invalid; missing=",
      paste(missing_required, collapse = ","), "; test_exports=",
      paste(leaked_tests, collapse = ",")
    )
  )
  first_identity <- lomm11_runner_worker_export_identity()
  second_identity <- lomm11_runner_worker_export_identity()
  lomm11_test_assert(
    identical(first_identity, second_identity) &&
      identical(first_identity$initial_export_count, as.integer(length(exported))) &&
      identical(
        first_identity$initial_export_sha256,
        lomm11_runner_object_sha256(as.list(exported))
      ),
    "PSOCK worker-export identity is not exact and deterministic."
  )
  exported_functions <- exported[vapply(exported, function(name) {
    exists(name, envir = .GlobalEnv, inherits = FALSE) &&
      is.function(get(name, envir = .GlobalEnv, inherits = FALSE))
  }, logical(1))]
  referenced <- unique(unlist(lapply(exported_functions, function(name) {
    globals <- codetools::findGlobals(
      get(name, envir = .GlobalEnv, inherits = FALSE), merge = FALSE
    )
    c(globals$functions, globals$variables)
  }), use.names = FALSE))
  local_references <- referenced[vapply(referenced, function(name) {
    exists(name, envir = .GlobalEnv, inherits = FALSE)
  }, logical(1))]
  lomm11_test_assert(
    length(setdiff(local_references, exported)) == 0L,
    "PSOCK worker export closure has unresolved project-local globals."
  )
  isolated <- new.env(parent = baseenv())
  for (name in required) {
    callback <- get(name, envir = .GlobalEnv, inherits = FALSE)
    environment(callback) <- isolated
    assign(name, callback, envir = isolated)
  }
  expected <- revision_exact_transition(-1, -0.2, 1)
  observed <- get("revision_exact_transition", envir = isolated)(-1, -0.2, 1)
  lomm11_test_assert(
    identical(observed, expected),
    "Exact-transition worker closure fails in an isolated global environment."
  )
  invisible(TRUE)
}

lomm11_test_infrastructure_preflight <- function() {
  namespace_snapshot <- function(path) {
    if (!dir.exists(path)) return(character())
    sort(list.files(
      path, recursive = TRUE, all.files = TRUE, no.. = TRUE
    ))
  }
  plan_before <- namespace_snapshot(LOMM11_RUNNER_PLAN_ROOT)
  result_before <- namespace_snapshot(LOMM11_RUNNER_RESULT_ROOT)
  original_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    original_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(original_kind))
    if (had_seed) {
      assign(".Random.seed", original_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(424242L)
  kind_before <- RNGkind()
  seed_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  worker_exports <- lomm11_runner_worker_names()
  test_exports <- worker_exports[
    grepl("^lomm11_test_|^lomm11_run_pure_integrity_tests$",
          worker_exports)
  ]
  lomm11_test_assert(
    length(test_exports) == 0L,
    paste0(
      "Infrastructure preflight worker exports contain test functions: ",
      paste(test_exports, collapse = ",")
    )
  )
  preflight <- lomm11_runner_execution_preflight()
  lomm11_runner_validate_execution_preflight(preflight)
  lomm11_test_assert(
    identical(RNGkind(), kind_before) &&
      identical(
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE), seed_before
      ) && identical(namespace_snapshot(LOMM11_RUNNER_PLAN_ROOT), plan_before) &&
      identical(namespace_snapshot(LOMM11_RUNNER_RESULT_ROOT), result_before),
    "Infrastructure preflight changed RNG or scientific namespaces."
  )
  invisible(TRUE)
}

lomm11_test_runner_guards <- function(config) {
  default <- lomm11_runner_parse_args(character())
  lomm11_test_assert(
    identical(default$mode, "dry-run") && !default$resume,
    "Runner default mode is not read-only."
  )
  lomm11_test_error(
    lomm11_runner_parse_args(c("--resume")),
    "also requires --execute"
  )
  lomm11_test_error(
    lomm11_runner_parse_args(c("--dry-run", "--execute")),
    "mutually exclusive"
  )
  lomm11_test_error(
    lomm11_runner_parse_args(c("--plan-dir", "--dry-run")),
    "Missing value"
  )
  manifest <- list(
    future_output_contract = lomm11_future_output_contract(config),
    expected_counts = config$expected_counts
  )
  lomm11_runner_validate_output_contract(manifest)
  lomm11_test_assert(
    all(c("planner", "tests") %in% names(LOMM11_RUNNER_DEPENDENCIES)) &&
      identical(config$future_execution$worker_limit, 12L),
    "Runner dependency or worker contract is incomplete."
  )
  invisible(TRUE)
}

config <- lomm11_test_config()
certified <- lomm11_test_certified_plan(config)

lomm11_run_pure_integrity_tests <- function() {
  lomm11_test_dynamics(config, certified$plan$tables$candidate_grid.csv)
  lomm11_run_internal_generator_checks()
  lomm11_run_internal_metric_checks()
  lomm11_test_random_streams_and_gates()
  lomm11_test_full_workload_rng_states(certified)
  lomm11_test_generation_plan_contract(config, certified)
  lomm11_test_checkpoint_metadata_guards(certified)
  lomm11_test_lock_guards(certified)
  lomm11_test_finalizing_recovery(certified)
  lomm11_test_heldout_consolidation_tamper()
  lomm11_test_batch_integrity_gate()
  lomm11_test_worker_dependency_export()
  lomm11_test_static_runtime_scan()
  lomm11_test_runner_guards(config)
}

if (identical(Sys.getenv("LOMM11_PURE_SAFETY_ONLY"), "1")) {
  lomm11_run_pure_integrity_tests()
} else {
  lomm11_run_pure_integrity_tests()
  lomm11_test_generator_fixture()
  lomm11_test_metric_fixture()
  lomm11_test_training_isolation(certified)
  lomm11_test_cli_guards()
}
if (identical(Sys.getenv("LOMM11_RUN_INFRASTRUCTURE_PREFLIGHT"), "1")) {
  lomm11_test_infrastructure_preflight()
}

cat("LOMM 1.1 empirical-realism ablation tests: PASS\n")
