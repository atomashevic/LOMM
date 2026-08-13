#!/usr/bin/env Rscript

calibration_test_stop <- function(...) stop(..., call. = FALSE)

calibration_test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) {
    calibration_test_stop("Cannot locate calibration reproducibility test.")
  }
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

calibration_test_parse_args <- function(args) {
  names <- c("scoring-runner", "image-source", "video-source", "score-input")
  result <- stats::setNames(as.list(rep(NA_character_, length(names))), names)
  index <- 1L
  while (index <= length(args)) {
    key <- sub("^--", "", args[[index]])
    if (!key %in% names || index == length(args)) {
      calibration_test_stop("Unknown or incomplete argument: ", args[[index]])
    }
    result[[key]] <- args[[index + 1L]]
    index <- index + 2L
  }
  if (anyNA(unlist(result, use.names = FALSE))) {
    calibration_test_stop(
      "Provide --scoring-runner, --image-source, --video-source, and --score-input."
    )
  }
  lapply(result, normalizePath, mustWork = TRUE)
}

calibration_test_contains <- function(text, pattern, label, fixed = TRUE) {
  if (!any(grepl(pattern, text, fixed = fixed))) {
    calibration_test_stop("Missing provenance evidence for ", label, ".")
  }
}

calibration_test_external_sources <- function(paths) {
  expected_hashes <- c(
    `scoring-runner` = "a7a318cc57dad7acaec0a16690d6de7ab75c1e0c2368960726d58b45bd94bf3a",
    `image-source` = "597059288abda51f616ff4fd4658a6553e5b8316ac154a1dfd4cc5930181fa5a",
    `video-source` = "c3bbd398c37f5e87001d633aa9de8229a49ea60a146594b3dbf02b3d1044e82d"
  )
  observed_hashes <- vapply(names(expected_hashes), function(name) {
    digest::digest(paths[[name]], algo = "sha256", file = TRUE, serialize = FALSE)
  }, character(1))
  if (!identical(unname(observed_hashes), unname(expected_hashes))) {
    calibration_test_stop("An inspected scoring source changed after the audit.")
  }

  runner <- readLines(paths[["scoring-runner"]], warn = FALSE)
  image_source <- readLines(paths[["image-source"]], warn = FALSE)
  video_source <- readLines(paths[["video-source"]], warn = FALSE)
  labels <- c(
    "anger", "disgust", "fear", "happiness", "neutral", "sadness",
    "surprise", "contempt", "anxiety", "helplessness", "disappointment"
  )
  for (label in labels) {
    calibration_test_contains(runner, paste0('"', label, '"'), label)
  }
  calibration_test_contains(runner, 'read_int_env("MAFW_NFRAMES", 50L)', "frame count")
  calibration_test_contains(runner, 'read_bool_env("MAFW_UNIFORM", TRUE)', "uniform sampling")
  calibration_test_contains(
    runner, 'read_char_env("MAFW_FACE_SELECTION", "largest")', "face selection"
  )
  calibration_test_contains(runner, "classes = CLASSES", "raw prompt labels")
  calibration_test_contains(
    image_source, "self.processor(text=labels, return_tensors='pt', padding=True)",
    "direct CLIP label tokenization"
  )
  calibration_test_contains(
    image_source, "def crop_face(image, padding=50, side='largest')", "face crop"
  )
  calibration_test_contains(
    image_source, "processed_image = crop_face(pil_image, side=face)",
    "classification crop call"
  )
  calibration_test_contains(video_source, "face=side", "video-to-crop call")

  scores <- utils::read.csv(
    paths[["score-input"]], stringsAsFactors = FALSE,
    colClasses = c(model = "character")
  )
  if (!identical(unique(scores$model), "oai-large")) {
    calibration_test_stop("Retained scores do not identify only the audited model alias.")
  }
}

calibration_test_retained_design <- function(repo_root) {
  design_dir <- file.path(
    repo_root, "simulation_outputs", "lomm_1_1",
    "empirical_realism_ablation", "854e8722d647"
  )
  run_dir <- file.path(
    repo_root, "simulation_outputs", "lomm_1_1",
    "empirical_realism_ablation_runs", "854e8722d647"
  )
  preprocess_manifest <- jsonlite::read_json(file.path(
    repo_root, "simulation_outputs", "lomm_1_1",
    "empirical_compositional_preprocess", "0cdc11154d8a",
    "preprocessing_manifest.json"
  ), simplifyVector = TRUE)
  config <- jsonlite::read_json(
    file.path(design_dir, "ablation_config.json"), simplifyVector = TRUE
  )
  expected_labels <- c(
    "anger", "disgust", "fear", "happiness", "neutral", "sadness",
    "surprise", "contempt", "anxiety", "helplessness", "disappointment"
  )
  stopifnot(
    identical(config$folds$count, 5L),
    identical(config$folds$heldout_clips_per_fold, 20L),
    identical(unname(config$axes$period_frames), c(8L, 12L, 24L)),
    isTRUE(all.equal(unname(config$axes$damping_ratio), c(0.05, 0.2, 0.5))),
    isTRUE(all.equal(unname(config$axes$kappa), c(0.75, 1, 1.25))),
    identical(config$selection$candidate_count_per_selection, 27L),
    identical(config$selection$scope, "complete_case_training_clips_only"),
    identical(config$selection$heldout_access, FALSE),
    identical(config$selection$sensitivity_access, FALSE),
    isTRUE(all.equal(config$measurement$between_clip_psd_floor, 1e-8)),
    identical(
      unname(preprocess_manifest$parameters$ordered_score_columns), expected_labels
    ),
    identical(preprocess_manifest$parameters$ilr_basis, "sequential_helmert_v1"),
    identical(preprocess_manifest$observed_counts$primary_rows, 3557L),
    identical(preprocess_manifest$observed_counts$sensitivity_rows, 3632L),
    identical(preprocess_manifest$observed_counts$imputed_rows, 75L)
  )

  assignments <- utils::read.csv(
    file.path(design_dir, "fold_assignments.csv"), stringsAsFactors = FALSE
  )
  fold_counts <- table(assignments$fold)
  if (!identical(as.integer(fold_counts), rep(20L, 5L))) {
    calibration_test_stop("Held-out folds do not contain 20 clips each.")
  }

  selected <- utils::read.csv(
    file.path(run_dir, "selected_candidates.csv"), stringsAsFactors = FALSE,
    colClasses = c(variant_id = "character")
  )
  selected_profile <- selected[selected$variant_id == "110", , drop = FALSE]
  if (nrow(selected_profile) != 5L ||
      !all(selected_profile$period_frames == 12L) ||
      !all(selected_profile$damping_ratio == 0.5) ||
      !all(selected_profile$kappa == 1)) {
    calibration_test_stop("Selected baseline-plus-scale settings changed.")
  }
}

calibration_test_main <- function() {
  if (!requireNamespace("digest", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    calibration_test_stop("The digest and jsonlite packages are required.")
  }
  paths <- calibration_test_parse_args(commandArgs(trailingOnly = TRUE))
  script_path <- calibration_test_script_path()
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
  calibration_test_external_sources(paths)
  calibration_test_retained_design(repo_root)
  cat("Compositional calibration reproducibility audit tests: PASS\n")
}

tryCatch(
  calibration_test_main(),
  error = function(condition) {
    message("ERROR: ", conditionMessage(condition))
    quit(save = "no", status = 1L, runLast = FALSE)
  }
)
