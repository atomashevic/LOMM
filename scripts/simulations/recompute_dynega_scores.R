#!/usr/bin/env Rscript

# Recompute trajectory diagnostics from stored partitions without fitting networks.
# Original results are read only. A committed byte count makes checkpoint appends
# recoverable if the process stops between writing a batch and updating status.

score_recompute_args <- function(args) {
  out <- list(output_dir = NULL, workers = 4L, batch_size = 16L, resume = FALSE,
              limit = Inf, jobs = c("t100_dynega", "t25_dynega", "challenge_dynega"))
  index <- 1L
  while (index <= length(args)) {
    key <- args[[index]]
    if (identical(key, "--resume")) {
      out$resume <- TRUE
      index <- index + 1L
      next
    }
    if (index == length(args)) stop("Missing argument value: ", key)
    value <- args[[index + 1L]]
    switch(key,
      "--output-dir" = { out$output_dir <- value },
      "--workers" = { out$workers <- as.integer(value) },
      "--batch-size" = { out$batch_size <- as.integer(value) },
      "--limit" = { out$limit <- as.integer(value) },
      "--jobs" = { out$jobs <- strsplit(value, ",", fixed = TRUE)[[1L]] },
      stop("Unknown argument: ", key)
    )
    index <- index + 2L
  }
  if (is.null(out$output_dir)) stop("--output-dir is required.")
  if (is.na(out$workers) || out$workers < 1L || out$workers > 4L) stop("Workers must be 1 through 4.")
  if (is.na(out$batch_size) || out$batch_size < 1L || out$batch_size > 256L) stop("Batch size must be 1 through 256.")
  if (is.na(out$limit) || out$limit < 1) stop("Limit must be positive.")
  out
}

score_recompute_equal <- function(actual, expected, label, tolerance = 1e-10) {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(label, ": expected ", paste(expected, collapse = ","),
         "; obtained ", paste(actual, collapse = ","), call. = FALSE)
  }
}

score_recompute_membership <- function(encoded) {
  pairs <- strsplit(strsplit(encoded, ";", fixed = TRUE)[[1L]], "=", fixed = TRUE)
  if (!length(pairs) || any(lengths(pairs) != 2L)) stop("Malformed stored membership.")
  setNames(as.integer(vapply(pairs, `[[`, character(1), 2L)),
           vapply(pairs, `[[`, character(1), 1L))
}

score_recompute_diagnostics <- function(scores, sim, time_points) {
  values <- numeric()
  padding <- undefined <- undefined_pairs <- 0L
  support_min <- Inf
  support_max <- 0L
  max_identity_error <- 0
  max_pair_nrmse <- 0
  complete_pairs <- 0L
  for (id in seq_along(sim$latents)) {
    rows <- which(sim$panel$id == id)
    if (length(rows) != time_points || nrow(sim$latents[[id]]) != time_points) {
      stop("Trajectory support differs from the planned record length.")
    }
    latent <- safe_scale_matrix(sim$latents[[id]])
    recovered <- safe_scale_matrix(scores[rows, , drop = FALSE])
    n_true <- ncol(latent)
    n_est <- ncol(recovered)
    costs <- matrix(2, n_true, n_est)
    valid <- matrix(FALSE, n_true, n_est)
    for (j in seq_len(n_true)) for (k in seq_len(n_est)) {
      x <- latent[, j]
      y <- recovered[, k]
      keep <- is.finite(x) & is.finite(y)
      count <- sum(keep)
      support_min <- min(support_min, count)
      support_max <- max(support_max, count)
      if (count < 3L) {
        undefined_pairs <- undefined_pairs + 1L
        next
      }
      distance <- min(sqrt(mean((x[keep] - y[keep])^2)),
                      sqrt(mean((x[keep] + y[keep])^2)))
      costs[j, k] <- distance
      valid[j, k] <- TRUE
      max_pair_nrmse <- max(max_pair_nrmse, distance)
      if (all(keep)) {
        complete_pairs <- complete_pairs + 1L
        squared_bound <- 2 * (count - 1) / count
        identity_error <- abs(distance^2 - squared_bound * (1 - abs(stats::cor(x, y))))
        max_identity_error <- max(max_identity_error, identity_error)
        if (distance > sqrt(squared_bound) + 1e-10 || identity_error > 1e-10) {
          stop("A complete standardized comparison violates the NRMSE identity.")
        }
      }
    }
    square <- pad_square_matrix(costs, pad_value = 2)
    selected <- as.integer(clue::solve_LSAP(square))[seq_len(n_true)]
    padding <- padding + sum(selected > n_est)
    real <- which(selected <= n_est)
    undefined <- undefined + sum(!valid[cbind(real, selected[real])])
    values <- c(values, square[cbind(seq_len(n_true), selected)])
  }
  implemented <- compute_score_nrmse_alignment(scores, sim$latents, sim$panel$id)
  score_recompute_equal(implemented, mean(values), "Independent assignment calculation")
  list(
    score_cor_common = compute_latent_score_alignment(scores, sim$latents, sim$panel$id),
    score_nrmse_common = implemented,
    n_scores = ncol(scores),
    n_assigned_comparisons = length(values),
    n_padding_penalties = padding,
    n_undefined_penalties = undefined,
    n_undefined_candidate_pairs = undefined_pairs,
    n_complete_candidate_pairs = complete_pairs,
    min_pair_observations = support_min,
    max_pair_observations = support_max,
    max_pair_nrmse = max_pair_nrmse,
    max_bound_identity_error = max_identity_error
  )
}

score_recompute_panel <- function(task, sources, plans) {
  tryCatch({
    rows <- sources[[task$job]][task$rows, , drop = FALSE]
    scenario <- plans[[task$job]][task$plan, , drop = FALSE]
    first <- rows[1L, , drop = FALSE]
    for (column in c("seed_base", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "dt", "representation")) {
      score_recompute_equal(rows[[column]], rep(scenario[[column]][[1L]], nrow(rows)),
                            paste("Stored scenario field", column))
    }
    sim <- if (identical(task$job, "challenge_dynega")) {
      revision_simulate_challenge_panel(scenario)
    } else {
      simulate_benchmark_panel(scenario, list(integrator = "linear_exact"), scenario$seed_base[[1L]])
    }
    panel_hash <- revision_object_sha256(list(
      panel = sim$panel, latents = sim$latents, true_membership = sim$true_membership
    ))
    score_recompute_equal(rows$panel_input_hash, rep(panel_hash, nrow(rows)), "Stored panel hash", 0)
    if (!is.null(sim$component_hashes)) for (component in names(sim$component_hashes)) {
      score_recompute_equal(rows[[paste0("component_hash_", component)]],
        rep(sim$component_hashes[[component]], nrow(rows)), paste("Stored component", component), 0)
    }
    if (nrow(sim$panel) != scenario$N * scenario$TimeP || length(sim$latents) != scenario$N) {
      stop("Generated panel dimensions differ from the source.")
    }
    finite_indicators <- all(is.finite(as.matrix(sim$panel[, sim$item_names, drop = FALSE])))
    finite_latents <- all(vapply(sim$latents, function(x) all(is.finite(x)), logical(1)))
    memberships <- lapply(rows$estimated_membership, score_recompute_membership)
    # Every affected stored partition has derivative names. Under the original
    # name lookup, all entries became NA, triggering one group of all indicators.
    if (any(vapply(memberships, function(x) any(names(x) %in% sim$item_names), logical(1)))) {
      stop("Stored membership does not satisfy the verified legacy fallback condition.")
    }
    legacy_scores <- build_common_scores(sim$panel, sim$item_names,
      setNames(rep(1L, length(sim$item_names)), sim$item_names))
    legacy_cor <- compute_latent_score_alignment(legacy_scores, sim$latents, sim$panel$id)
    legacy_nrmse <- compute_score_nrmse_alignment(legacy_scores, sim$latents, sim$panel$id)
    score_recompute_equal(rows$score_cor_common, rep(legacy_cor, nrow(rows)), "Stored legacy correlation")
    score_recompute_equal(rows$score_nrmse_common, rep(legacy_nrmse, nrow(rows)), "Stored legacy NRMSE")
    cache <- list()
    output <- vector("list", nrow(rows))
    for (index in seq_len(nrow(rows))) {
      membership <- memberships[[index]]
      canonical <- membership
      names(canonical) <- sub("\\.Ord[01]$", "", names(canonical))
      canonical <- canonical[sim$item_names]
      cache_key <- paste(match(canonical, unique(canonical)), collapse = ";")
      if (is.null(cache[[cache_key]])) {
        scores <- build_common_scores(sim$panel, sim$item_names, membership)
        if (ncol(scores) != length(unique(membership)) || nrow(scores) != nrow(sim$panel)) {
          stop("Corrected score dimensions do not preserve the stored partition.")
        }
        cache[[cache_key]] <- c(score_recompute_diagnostics(scores, sim, scenario$TimeP[[1L]]),
          list(all_scores_finite = all(is.finite(scores))))
      }
      diagnostic <- cache[[cache_key]]
      score_recompute_equal(diagnostic$n_scores, rows$n_dim_hat[[index]], "Recorded dimension count")
      output[[index]] <- c(list(
        source_job = task$job,
        unit_key = rows$unit_key[[index]], panel_key = rows$panel_key[[index]],
        representation_key = rows$representation_key[[index]],
        community_algorithm = rows$community_algorithm[[index]],
        old_score_cor_common = rows$score_cor_common[[index]],
        old_score_nrmse_common = rows$score_nrmse_common[[index]],
        n_dim_hat = rows$n_dim_hat[[index]],
        all_indicators_finite = finite_indicators,
        all_latents_finite = finite_latents,
        legacy_cor_abs_error = abs(legacy_cor - rows$score_cor_common[[index]]),
        legacy_nrmse_abs_error = abs(legacy_nrmse - rows$score_nrmse_common[[index]]),
        panel_hash_verified = TRUE
      ), diagnostic)
    }
    list(ok = TRUE, rows = do.call(rbind, lapply(output, as.data.frame, stringsAsFactors = FALSE)))
  }, error = function(error) list(ok = FALSE, job = task$job,
    representation_key = task$key, error = conditionMessage(error)))
}

score_recompute_atomic_json <- function(value, path) {
  temporary <- tempfile("score-status-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  if (!file.rename(temporary, path)) stop("Could not commit status.")
}

score_recompute_main <- function() {
  args <- score_recompute_args(commandArgs(trailingOnly = TRUE))
  script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  script <- normalizePath(sub("^--file=", "", script_argument), mustWork = TRUE)
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
  setwd(root)
  Sys.setenv(RUN_METHOD_BENCHMARK = "0", RUN_METHOD_BENCHMARK_SMOKE = "0",
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  source("scripts/simulations/run_revision_job.R", local = globalenv())
  revision_source_estimation_core()
  paths <- c(
    t100_dynega = "simulation_outputs/revision_v2/t100_dynega/966643c4ba53/results.csv",
    t25_dynega = "simulation_outputs/revision_v2/t25_dynega/2205c374057a/results.csv",
    challenge_dynega = "simulation_outputs/revision_v2/challenge_dynega/fc6333c2fdab/results.csv"
  )
  if (anyDuplicated(args$jobs) || !all(args$jobs %in% names(paths))) stop("Invalid source jobs.")
  paths <- paths[args$jobs]
  code_paths <- c(script, "scripts/simulations/run_revision_job.R", "scripts/simulations/revision_spec.R",
                  "scripts/simulations/run_method_simulation.R", "scripts/extract.r")
  provenance_paths <- c(unname(paths), file.path(dirname(paths), "run_manifest.json"), code_paths)
  hashes <- setNames(vapply(provenance_paths, revision_sha256, character(1)), provenance_paths)
  software <- c(R = as.character(getRversion()),
    vapply(c("EGAnet", "clue", "igraph", "MASS"), function(x) as.character(utils::packageVersion(x)), character(1)))
  needed <- c("unit_key", "panel_key", "representation_key", "seed_base", "N", "TimeP", "NVAR",
    "Error_sd", "Loadings", "dt", "representation", "panel_input_hash", "estimated_membership",
    "community_algorithm", "score_cor_common", "score_nrmse_common", "n_dim_hat", "status", "method_id",
    paste0("component_hash_", c("latent_standard_normals", "residual_standard_normals", "loading_basis",
                               "activity_schedule", "linear_z")))
  sources <- lapply(paths, function(path) {
    columns <- names(utils::read.csv(path, nrows = 0L))
    if (!all(needed %in% columns)) stop("Source schema incomplete: ", path)
    classes <- setNames(ifelse(columns %in% needed, NA_character_, "NULL"), columns)
    result <- utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
    if (anyDuplicated(result$unit_key) || any(result$status != "ok") || any(result$method_id != "dynEGA")) {
      stop("Source cohort contains unexpected status, method, or duplicate unit keys: ", path)
    }
    result
  })
  plans <- setNames(lapply(args$jobs, function(job) revision_core_panel_grid(revision_job_panels(job))), args$jobs)
  tasks <- list()
  for (job in args$jobs) {
    grouped <- split(seq_len(nrow(sources[[job]])), sources[[job]]$representation_key)
    plan_index <- match(names(grouped), plans[[job]]$representation_key)
    if (anyNA(plan_index) || anyDuplicated(plans[[job]]$representation_key)) stop("Source-plan alignment failed: ", job)
    tasks <- c(tasks, lapply(seq_along(grouped), function(i) list(job = job, rows = grouped[[i]],
      plan = plan_index[[i]], key = names(grouped)[[i]])))
  }
  # A bounded pilot spans the selected source cohorts and scenario order.
  if (is.finite(args$limit) && args$limit < length(tasks)) {
    tasks <- tasks[round(seq(1L, length(tasks), length.out = args$limit))]
  }
  expected_units <- unlist(lapply(tasks, function(task) sources[[task$job]]$unit_key[task$rows]), use.names = FALSE)
  task_keys <- vapply(tasks, function(task) paste(task$job, task$key, sep = "|"), character(1))
  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(args$output_dir, mustWork = TRUE)
  checkpoint <- file.path(output_dir, "checkpoint_results.csv")
  status_path <- file.path(output_dir, "status.json")
  status <- list(status = "running", started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    hashes = as.list(hashes), software = as.list(software), jobs = args$jobs,
    completed_panels = 0L, completed_readouts = 0L, checkpoint_bytes = 0,
    total_panels = length(tasks), total_readouts = length(expected_units))
  done_keys <- character()
  if (file.exists(status_path)) {
    if (!args$resume) stop("Status exists; use --resume to continue this output directory.")
    previous <- jsonlite::read_json(status_path, simplifyVector = TRUE)
    score_recompute_equal(unlist(previous$hashes), hashes, "Input or implementation hash drift", 0)
    score_recompute_equal(unlist(previous$software), software, "Software version drift", 0)
    if (previous$total_panels != length(tasks) || previous$total_readouts != length(expected_units)) {
      stop("Resume selection differs from committed run; use another output directory.")
    }
    status <- previous
    if (file.exists(checkpoint)) {
      size <- file.info(checkpoint)$size
      if (size < status$checkpoint_bytes) stop("Checkpoint is shorter than committed status.")
      if (size > status$checkpoint_bytes) {
        connection <- file(checkpoint, open = "r+b")
        seek(connection, where = status$checkpoint_bytes, origin = "start", rw = "write")
        truncate(connection)
        close(connection)
      }
    } else if (status$checkpoint_bytes > 0) stop("Committed checkpoint is missing.")
    if (status$checkpoint_bytes > 0) {
      existing <- utils::read.csv(checkpoint, stringsAsFactors = FALSE)
      if (anyDuplicated(existing$unit_key) || !all(existing$unit_key %in% expected_units) ||
          nrow(existing) != status$completed_readouts) stop("Committed checkpoint key/count mismatch.")
      done_keys <- unique(paste(existing$source_job, existing$representation_key, sep = "|"))
      if (length(done_keys) != status$completed_panels) stop("Committed panel count mismatch.")
      completed_tasks <- match(done_keys, task_keys)
      if (anyNA(completed_tasks)) stop("Checkpoint contains an unknown representation panel.")
      completed_units <- unlist(lapply(tasks[completed_tasks], function(task) {
        sources[[task$job]]$unit_key[task$rows]
      }), use.names = FALSE)
      if (!setequal(completed_units, existing$unit_key)) stop("Partial committed panel.")
    }
  } else {
    if (args$resume) stop("--resume requires an existing status file.")
    if (file.exists(checkpoint)) stop("Checkpoint without committed status; refusing to overwrite.")
  }
  status$status <- "running"
  status$workers <- args$workers
  status$batch_size <- args$batch_size
  status$pid <- Sys.getpid()
  status$error <- NULL
  score_recompute_atomic_json(status, status_path)
  remaining <- which(!task_keys %in% done_keys)
  batches <- split(remaining, ceiling(seq_along(remaining) / args$batch_size))
  tryCatch({
    for (batch in batches) {
      started <- proc.time()[["elapsed"]]
      results <- parallel::mclapply(tasks[batch], score_recompute_panel,
        sources = sources, plans = plans, mc.cores = min(args$workers, length(batch)),
        mc.preschedule = TRUE, mc.set.seed = FALSE)
      failed <- which(!vapply(results, function(x) is.list(x) && isTRUE(x$ok), logical(1)))
      if (length(failed)) stop(paste(capture.output(str(results[failed], give.attr = FALSE)), collapse = " "))
      frame <- do.call(rbind, lapply(results, `[[`, "rows"))
      utils::write.table(frame, checkpoint, sep = ",", row.names = FALSE,
        col.names = status$checkpoint_bytes == 0, append = status$checkpoint_bytes > 0, na = "")
      status$completed_panels <- status$completed_panels + length(batch)
      status$completed_readouts <- status$completed_readouts + nrow(frame)
      status$checkpoint_bytes <- file.info(checkpoint)$size
      status$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      status$last_batch_seconds <- proc.time()[["elapsed"]] - started
      status$elapsed_seconds <- as.numeric(difftime(Sys.time(),
        as.POSIXct(status$started_at, tz = "UTC"), units = "secs"))
      status$last_representation_key <- tasks[[tail(batch, 1L)]]$key
      score_recompute_atomic_json(status, status_path)
      cat(sprintf("%s panels=%d/%d readouts=%d/%d batch_seconds=%.3f\n",
        status$updated_at, status$completed_panels, status$total_panels,
        status$completed_readouts, status$total_readouts, status$last_batch_seconds))
      flush.console()
    }
    # Check both set membership and cardinality before marking the run complete.
    final <- utils::read.csv(checkpoint, stringsAsFactors = FALSE)
    if (anyDuplicated(final$unit_key) || nrow(final) != length(expected_units) ||
        !setequal(final$unit_key, expected_units)) stop("Final checkpoint does not cover the selected cohort.")
    score_recompute_equal(setNames(vapply(provenance_paths, revision_sha256, character(1)), provenance_paths),
                          hashes, "Input or implementation changed during run", 0)
    status$status <- "complete"
    status$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    status$checkpoint_sha256 <- revision_sha256(checkpoint)
    score_recompute_atomic_json(status, status_path)
  }, error = function(error) {
    status$status <- "failed"
    status$error <- conditionMessage(error)
    status$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    score_recompute_atomic_json(status, status_path)
    stop(error)
  })
}

if (sys.nframe() == 0L) score_recompute_main()
