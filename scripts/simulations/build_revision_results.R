#!/usr/bin/env Rscript

rr_stop <- function(...) {
  stop(..., call. = FALSE)
}

rr_progress <- function(label, previous_elapsed = NULL) {
  now <- unname(proc.time()[["elapsed"]])
  suffix <- if (is.null(previous_elapsed)) {
    ""
  } else {
    sprintf(" (%.1f s)", now - previous_elapsed)
  }
  message("[aggregate] ", label, suffix)
  now
}

rr_script_path <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- arguments[grepl("^--file=", arguments)]
  if (length(file_argument) == 0L) {
    return(NA_character_)
  }
  normalizePath(sub("^--file=", "", file_argument[[1]]), mustWork = FALSE)
}

rr_find_repo_root <- function() {
  script_path <- rr_script_path()
  candidates <- character()
  if (!is.na(script_path)) {
    candidates <- c(candidates, dirname(dirname(dirname(script_path))))
  }
  current <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    candidates <- c(candidates, current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  candidates <- unique(candidates)
  matches <- candidates[file.exists(file.path(
    candidates,
    "scripts",
    "simulations",
    "revision_spec.R"
  ))]
  if (length(matches) == 0L) {
    rr_stop("Cannot locate the repository root containing revision_spec.R.")
  }
  normalizePath(matches[[1]], mustWork = TRUE)
}

rr_parse_arguments <- function(arguments) {
  result <- list(
    config = "scripts/simulations/revision_jobs.json",
    dry_run = FALSE
  )
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (identical(argument, "--dry-run")) {
      result$dry_run <- TRUE
      index <- index + 1L
      next
    }
    if (identical(argument, "--config")) {
      if (index == length(arguments)) {
        rr_stop("--config requires a path.")
      }
      result$config <- arguments[[index + 1L]]
      index <- index + 2L
      next
    }
    if (grepl("^--config=", argument)) {
      result$config <- sub("^--config=", "", argument)
      index <- index + 1L
      next
    }
    rr_stop("Unknown argument: ", argument)
  }
  result
}

rr_absolute_path <- function(path, repo_root, must_work = FALSE) {
  if (!grepl("^/", path)) {
    path <- file.path(repo_root, path)
  }
  normalizePath(path, mustWork = must_work)
}

rr_json_state <- function(text, source) {
  state <- new.env(parent = emptyenv())
  state$text <- text
  state$source <- source
  state$position <- 1L
  state$length <- nchar(text, type = "chars")
  state
}

rr_json_error <- function(state, message) {
  rr_stop(sprintf(
    "Invalid JSON in %s at character %d: %s",
    state$source,
    state$position,
    message
  ))
}

rr_json_skip_space <- function(state) {
  while (state$position <= state$length) {
    character <- substr(state$text, state$position, state$position)
    if (!grepl("[[:space:]]", character)) {
      break
    }
    state$position <- state$position + 1L
  }
  invisible(NULL)
}

rr_json_peek <- function(state) {
  if (state$position > state$length) {
    return(NA_character_)
  }
  substr(state$text, state$position, state$position)
}

rr_json_expect <- function(state, expected) {
  actual <- rr_json_peek(state)
  if (is.na(actual) || !identical(actual, expected)) {
    rr_json_error(state, paste0("expected '", expected, "'."))
  }
  state$position <- state$position + 1L
  invisible(NULL)
}

rr_json_parse_string <- function(state) {
  rr_json_expect(state, "\"")
  pieces <- character()
  while (state$position <= state$length) {
    character <- rr_json_peek(state)
    state$position <- state$position + 1L
    if (identical(character, "\"")) {
      return(paste0(pieces, collapse = ""))
    }
    if (!identical(character, "\\")) {
      if (utf8ToInt(character)[[1]] < 32L) {
        rr_json_error(state, "unescaped control character in string.")
      }
      pieces <- c(pieces, character)
      next
    }
    escape <- rr_json_peek(state)
    if (is.na(escape)) {
      rr_json_error(state, "unterminated string escape.")
    }
    state$position <- state$position + 1L
    translations <- c(
      "\"" = "\"",
      "\\" = "\\",
      "/" = "/",
      "b" = "\b",
      "f" = "\f",
      "n" = "\n",
      "r" = "\r",
      "t" = "\t"
    )
    if (escape %in% names(translations)) {
      pieces <- c(pieces, unname(translations[[escape]]))
      next
    }
    if (!identical(escape, "u")) {
      rr_json_error(state, paste0("unsupported escape \\", escape, "."))
    }
    if (state$position + 3L > state$length) {
      rr_json_error(state, "incomplete Unicode escape.")
    }
    code <- substr(state$text, state$position, state$position + 3L)
    if (!grepl("^[0-9A-Fa-f]{4}$", code)) {
      rr_json_error(state, "invalid Unicode escape.")
    }
    pieces <- c(pieces, intToUtf8(strtoi(code, base = 16L)))
    state$position <- state$position + 4L
  }
  rr_json_error(state, "unterminated string.")
}

rr_json_parse_number <- function(state) {
  remaining <- substr(state$text, state$position, state$length)
  match <- regexpr(
    "^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?",
    remaining,
    perl = TRUE
  )
  if (match[[1]] != 1L) {
    rr_json_error(state, "invalid number.")
  }
  token <- regmatches(remaining, match)
  state$position <- state$position + nchar(token)
  value <- suppressWarnings(as.numeric(token))
  if (!is.finite(value)) {
    rr_json_error(state, "number is outside the finite numeric range.")
  }
  value
}

rr_json_parse_literal <- function(state, literal, value) {
  end <- state$position + nchar(literal) - 1L
  if (end > state$length ||
      !identical(substr(state$text, state$position, end), literal)) {
    rr_json_error(state, paste0("expected ", literal, "."))
  }
  state$position <- end + 1L
  value
}

rr_json_parse_array <- function(state) {
  rr_json_expect(state, "[")
  rr_json_skip_space(state)
  result <- list()
  if (identical(rr_json_peek(state), "]")) {
    state$position <- state$position + 1L
    return(result)
  }
  repeat {
    result[length(result) + 1L] <- list(rr_json_parse_value(state))
    rr_json_skip_space(state)
    delimiter <- rr_json_peek(state)
    if (identical(delimiter, "]")) {
      state$position <- state$position + 1L
      return(result)
    }
    if (!identical(delimiter, ",")) {
      rr_json_error(state, "expected ',' or ']' in array.")
    }
    state$position <- state$position + 1L
    rr_json_skip_space(state)
  }
}

rr_json_parse_object <- function(state) {
  rr_json_expect(state, "{")
  rr_json_skip_space(state)
  result <- structure(list(), names = character())
  if (identical(rr_json_peek(state), "}")) {
    state$position <- state$position + 1L
    return(result)
  }
  repeat {
    if (!identical(rr_json_peek(state), "\"")) {
      rr_json_error(state, "object key must be a string.")
    }
    key <- rr_json_parse_string(state)
    if (key %in% names(result)) {
      rr_json_error(state, paste0("duplicate object key '", key, "'."))
    }
    rr_json_skip_space(state)
    rr_json_expect(state, ":")
    rr_json_skip_space(state)
    result[key] <- list(rr_json_parse_value(state))
    rr_json_skip_space(state)
    delimiter <- rr_json_peek(state)
    if (identical(delimiter, "}")) {
      state$position <- state$position + 1L
      return(result)
    }
    if (!identical(delimiter, ",")) {
      rr_json_error(state, "expected ',' or '}' in object.")
    }
    state$position <- state$position + 1L
    rr_json_skip_space(state)
  }
}

rr_json_parse_value <- function(state) {
  rr_json_skip_space(state)
  character <- rr_json_peek(state)
  if (is.na(character)) {
    rr_json_error(state, "expected a value.")
  }
  if (identical(character, "\"")) {
    return(rr_json_parse_string(state))
  }
  if (identical(character, "{")) {
    return(rr_json_parse_object(state))
  }
  if (identical(character, "[")) {
    return(rr_json_parse_array(state))
  }
  if (identical(character, "t")) {
    return(rr_json_parse_literal(state, "true", TRUE))
  }
  if (identical(character, "f")) {
    return(rr_json_parse_literal(state, "false", FALSE))
  }
  if (identical(character, "n")) {
    return(rr_json_parse_literal(state, "null", NULL))
  }
  rr_json_parse_number(state)
}

rr_parse_json_text <- function(text, source = "<text>") {
  state <- rr_json_state(enc2utf8(text), source)
  value <- rr_json_parse_value(state)
  rr_json_skip_space(state)
  if (state$position <= state$length) {
    rr_json_error(state, "trailing content after the root value.")
  }
  value
}

rr_read_json <- function(path) {
  if (!file.exists(path)) {
    rr_stop("Missing JSON file: ", path)
  }
  size <- file.info(path)$size[[1]]
  if (!is.finite(size) || size < 1L) {
    rr_stop("Empty JSON file: ", path)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  text <- readChar(connection, nchars = size, useBytes = TRUE)
  rr_parse_json_text(text, path)
}

rr_json_escape <- function(value) {
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  value <- gsub("\"", "\\\"", value, fixed = TRUE)
  value <- gsub("\b", "\\b", value, fixed = TRUE)
  value <- gsub("\f", "\\f", value, fixed = TRUE)
  value <- gsub("\n", "\\n", value, fixed = TRUE)
  value <- gsub("\r", "\\r", value, fixed = TRUE)
  gsub("\t", "\\t", value, fixed = TRUE)
}

rr_to_json <- function(value) {
  if (is.null(value)) {
    return("null")
  }
  if (is.data.frame(value)) {
    rows <- lapply(seq_len(nrow(value)), function(index) {
      as.list(value[index, , drop = FALSE])
    })
    return(rr_to_json(rows))
  }
  if (is.list(value)) {
    object <- !is.null(names(value)) && all(nzchar(names(value)))
    if (object) {
      fields <- Map(function(name, item) {
        paste0("\"", rr_json_escape(name), "\":", rr_to_json(item))
      }, names(value), value)
      return(paste0("{", paste0(fields, collapse = ","), "}"))
    }
    return(paste0("[", paste0(vapply(value, rr_to_json, character(1)), collapse = ","), "]"))
  }
  if (length(value) != 1L) {
    return(paste0("[", paste0(vapply(as.list(value), rr_to_json, character(1)), collapse = ","), "]"))
  }
  if (is.na(value)) {
    return("null")
  }
  if (is.character(value)) {
    return(paste0("\"", rr_json_escape(value), "\""))
  }
  if (is.logical(value)) {
    return(if (isTRUE(value)) "true" else "false")
  }
  if (is.numeric(value)) {
    if (!is.finite(value)) {
      rr_stop("Cannot serialize a non-finite number to JSON.")
    }
    return(format(value, scientific = FALSE, trim = TRUE))
  }
  rr_stop("Unsupported JSON value type: ", typeof(value))
}

revision_estimand_specification <- function() {
  method_jobs <- paste(setdiff(
    revision_required_upstream_jobs(),
    c("empirical_cv", "t100_reuse_audit")
  ), collapse = ";")
  rows <- list()
  add <- function(id, family, outcome, formula, eligible, denominator,
                  failure, comparison = "none", pairing = "none",
                  strata = "method_id;community_algorithm;stability_class;TimeP",
                  sources = method_jobs, direction = "metric-specific") {
    rows[[length(rows) + 1L]] <<- data.frame(
      estimand_id = id,
      analysis_family = family,
      outcome = outcome,
      formula = formula,
      eligible_rows = eligible,
      denominator = denominator,
      failure_rule = failure,
      comparison = comparison,
      pairing_key = pairing,
      strata = strata,
      source_jobs = sources,
      direction_of_improvement = direction,
      stringsAsFactors = FALSE
    )
  }
  add(
    "empirical_heldout_loss", "empirical_cv", "continuous_loss;secondary_frozen_dynega_loss",
    "descriptive mean held-out continuous loss for the training-fold-selected configuration",
    "held-out replicate evaluations only", "all finite held-out losses",
    "selection uses training folds only", strata = "fold",
    sources = "empirical_cv", direction = "lower"
  )
  add(
    "empirical_observable_primary_discrepancy", "empirical_cv",
    paste(
      "signed_scaled_median_difference;absolute_scaled_median_difference;",
      "scaled_wasserstein_distance;max_discrepancy",
      sep = ""
    ),
    paste(
      "signed=(median(synthetic)-median(empirical))/scale;",
      " absolute=abs(signed); Wasserstein=W1(empirical,synthetic)/scale;",
      " max=max(absolute,Wasserstein); scale is empirical IQR with SD and unit fallbacks"
    ),
    "held-out clip contributions in family observable_primary",
    "held-out clips", paste(
      "clips are resampled across folds and replicates; every bootstrap draw",
      " recomputes the empirical scale, medians, and Wasserstein distance"
    ),
    strata = "family;metric", sources = "empirical_cv",
    direction = "metric-specific"
  )
  add(
    "empirical_secondary_frozen_summary_discrepancy", "empirical_cv",
    paste(
      "signed_scaled_median_difference;absolute_scaled_median_difference;",
      "scaled_wasserstein_distance;max_discrepancy",
      sep = ""
    ),
    paste(
      "signed=(median(synthetic)-median(empirical))/scale;",
      " absolute=abs(signed); Wasserstein=W1(empirical,synthetic)/scale;",
      " max=max(absolute,Wasserstein); scale is empirical IQR with SD and unit fallbacks"
    ),
    "held-out clip contributions in family secondary_frozen_summary",
    "held-out clips", paste(
      "clips are resampled across folds and replicates; every bootstrap draw",
      " recomputes the empirical scale, medians, and Wasserstein distance"
    ),
    strata = "family;metric", sources = "empirical_cv",
    direction = "metric-specific"
  )
  add(
    "p_usable", "recovery", "usable",
    "mean(I(status == 'ok' and n_dim_hat is finite))",
    "all planned readouts", "all planned readouts",
    "error and timeout contribute zero", direction = "higher"
  )
  add(
    "p_correct_given_usable", "recovery", "n_dim_exact",
    "mean(I(n_dim_hat == n_dim_true) | usable)",
    "usable readouts", "usable readouts",
    "unusable readouts are excluded from this conditional estimand",
    direction = "higher"
  )
  add(
    "p_usable_and_correct", "recovery", "usable_and_correct",
    "mean(I(usable and n_dim_hat == n_dim_true))",
    "all planned readouts", "all planned readouts",
    "error and timeout contribute zero", direction = "higher"
  )
  add(
    "signed_dimension_error", "recovery", "n_dim_hat - n_dim_true",
    "mean(n_dim_hat - n_dim_true | usable)", "usable readouts",
    "usable readouts", "unusable readouts are excluded", direction = "zero"
  )
  add(
    "absolute_dimension_error", "recovery", "abs(n_dim_hat - n_dim_true)",
    "mean(abs(n_dim_hat - n_dim_true) | usable)", "usable readouts",
    "usable readouts", "unusable readouts are excluded", direction = "lower"
  )
  add(
    "adjusted_rand_index", "partition_recovery", "adjusted_rand_index",
    "mean(adjusted_rand_index | usable)", "usable readouts",
    "usable readouts", "unusable readouts are excluded", direction = "higher"
  )
  add(
    "variation_information", "partition_recovery", "variation_information",
    "mean(variation_information | usable)", "usable readouts",
    "usable readouts", "unusable readouts are excluded", direction = "lower"
  )
  add(
    "assignment_accuracy_conditional", "partition_recovery", "assignment_accuracy",
    "mean(assignment_accuracy | usable)", "usable readouts",
    "usable readouts", "unusable readouts are excluded", direction = "higher"
  )
  add(
    "score_correlation_marginal", "score_recovery", "score_cor_common",
    "mean(if usable and finite score correlation then score correlation else 0)",
    "all planned readouts", "all planned readouts",
    "unusable or undefined scores contribute zero", direction = "higher"
  )
  add(
    "score_nrmse_marginal", "score_recovery", "score_nrmse_common",
    "mean(if usable and finite score NRMSE then score NRMSE else 2)",
    "all planned readouts", "all planned readouts",
    "unusable or undefined scores contribute the locked penalty 2", direction = "lower"
  )
  add(
    "completion_within_900s", "feasibility", "completion_within_900s",
    "mean(I(status == 'ok' and runtime_sec <= 900))",
    "all planned readouts", "all planned readouts",
    "unfinished and failed readouts contribute zero", direction = "higher"
  )
  add(
    "restricted_compute_900s", "feasibility", "min(runtime_sec, 900)",
    "mean(min(runtime_sec, 900)); missing unfinished runtime is censored at 900",
    "all planned readouts", "all planned readouts",
    "unfinished readouts with missing runtime contribute 900 seconds", direction = "lower"
  )
  add(
    "runtime_median_successful", "feasibility", "runtime_sec",
    "median(runtime_sec | usable)", "usable readouts", "usable readouts",
    "unusable readouts are excluded", direction = "lower"
  )
  add(
    "runtime_iqr_successful", "feasibility", "runtime_sec",
    "IQR(runtime_sec | usable)", "usable readouts", "usable readouts",
    "unusable readouts are excluded", direction = "lower"
  )
  pair_key <- paste(
    c("scenario", "N", "NVAR", "Error_sd", "Loadings", "Q", "rep",
      "community_algorithm", "challenge_id", "representation"),
    collapse = ";"
  )
  add(
    "panel_paired_method_difference", "paired_comparison", "all primary outcomes",
    "paired mean or paired median of method A minus method B",
    "complete panel-level method pairs", "complete panel pairs",
    "metric-specific usability rules apply before pairing",
    comparison = "method A minus method B", pairing = pair_key
  )
  add(
    "paired_t25_minus_t100", "paired_horizon", "all primary outcomes",
    "paired T25 minus T100 difference", "complete matched-horizon pairs",
    "complete matched-horizon pairs", "metric-specific usability rules apply",
    comparison = "T25 minus T100", pairing = sub(";challenge_id;representation", "", pair_key)
  )
  add(
    "method_minus_static_gap", "target_alignment", "all primary outcomes",
    "panel-paired method minus static_EGA difference", "complete method/static pairs",
    "complete method/static pairs", "metric-specific usability rules apply",
    comparison = "method minus static_EGA", pairing = pair_key
  )
  add(
    "method_minus_oracle_gap", "target_alignment", "all primary outcomes",
    "panel-paired method minus oracle_blocks difference", "complete method/oracle pairs",
    "complete method/oracle pairs", "metric-specific usability rules apply",
    comparison = "method minus oracle_blocks", pairing = pair_key
  )
  add(
    "challenge_minus_ideal", "challenge", "all primary outcomes",
    "panel-paired challenge variant minus ideal_baseline difference",
    "complete variant/ideal pairs", "complete variant/ideal pairs",
    "metric-specific usability rules apply", comparison = "variant minus ideal",
    pairing = "scenario;rep;method_id;community_algorithm"
  )
  add(
    "softmax_clr_minus_raw", "challenge", "all primary outcomes",
    "panel-paired softmax CLR minus raw difference", "complete CLR/raw pairs",
    "complete CLR/raw pairs", "metric-specific usability rules apply",
    comparison = "softmax_clr minus softmax_raw",
    pairing = "panel_key;method_id;community_algorithm"
  )
  do.call(rbind, rows)
}

rr_validate_estimands <- function(specification) {
  required <- c(
    "estimand_id",
    "analysis_family",
    "outcome",
    "formula",
    "eligible_rows",
    "denominator",
    "failure_rule",
    "comparison",
    "pairing_key",
    "strata",
    "source_jobs",
    "direction_of_improvement"
  )
  if (!identical(names(specification), required)) {
    rr_stop("Estimand specification columns differ from the locked schema.")
  }
  if (any(!nzchar(specification$estimand_id)) || anyDuplicated(specification$estimand_id)) {
    rr_stop("Estimand identifiers must be non-empty and unique.")
  }
  required_estimands <- c(
    "empirical_heldout_loss",
    "empirical_observable_primary_discrepancy",
    "empirical_secondary_frozen_summary_discrepancy",
    "p_usable",
    "p_correct_given_usable",
    "p_usable_and_correct",
    "signed_dimension_error",
    "absolute_dimension_error",
    "adjusted_rand_index",
    "variation_information",
    "assignment_accuracy_conditional",
    "score_correlation_marginal",
    "score_nrmse_marginal",
    "completion_within_900s",
    "restricted_compute_900s",
    "runtime_median_successful",
    "runtime_iqr_successful",
    "panel_paired_method_difference",
    "paired_t25_minus_t100",
    "method_minus_static_gap",
    "method_minus_oracle_gap",
    "challenge_minus_ideal",
    "softmax_clr_minus_raw"
  )
  missing <- setdiff(required_estimands, specification$estimand_id)
  if (length(missing) > 0L) {
    rr_stop("Missing required estimands: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

rr_named_job_configuration <- function(configuration) {
  jobs <- configuration$jobs
  if (!is.list(jobs) || length(jobs) == 0L) {
    rr_stop("Configuration jobs must be a non-empty array.")
  }
  identifiers <- vapply(jobs, function(job) {
    if (!is.list(job) || !is.character(job$id) || length(job$id) != 1L) {
      rr_stop("Each configured job requires one string id.")
    }
    job$id
  }, character(1))
  if (anyDuplicated(identifiers)) {
    rr_stop("Configured job ids must be unique.")
  }
  names(jobs) <- identifiers
  jobs
}

rr_positive_integer <- function(value) {
  is.numeric(value) && length(value) == 1L && is.finite(value) &&
    value > 0 && value == floor(value)
}

rr_validate_configuration <- function(configuration) {
  if (!is.list(configuration) || is.null(names(configuration))) {
    rr_stop("Configuration root must be a JSON object.")
  }
  defaults <- configuration$defaults
  if (!is.list(defaults)) {
    rr_stop("Configuration defaults must be an object.")
  }
  required_defaults <- c(
    "output_root",
    "config_hash_length",
    "prospective_manifest",
    "run_manifest",
    "success_marker"
  )
  missing_defaults <- setdiff(required_defaults, names(defaults))
  if (length(missing_defaults) > 0L) {
    rr_stop("Missing configuration defaults: ", paste(missing_defaults, collapse = ", "))
  }
  if (!identical(defaults$output_root, "simulation_outputs/revision_v2")) {
    rr_stop("Configuration output_root must be simulation_outputs/revision_v2.")
  }
  if (!rr_positive_integer(defaults$config_hash_length)) {
    rr_stop("config_hash_length must be a positive integer.")
  }
  jobs <- rr_named_job_configuration(configuration)
  required <- revision_required_upstream_jobs()
  missing_jobs <- setdiff(required, names(jobs))
  if (length(missing_jobs) > 0L) {
    rr_stop("Configuration omits upstream jobs: ", paste(missing_jobs, collapse = ", "))
  }
  for (job_id in required) {
    if (!rr_positive_integer(jobs[[job_id]]$expected_units)) {
      rr_stop(job_id, " requires a positive integer expected_units value.")
    }
  }
  invisible(jobs)
}

rr_path_is_within <- function(path, directory) {
  path <- normalizePath(path, mustWork = TRUE)
  directory <- normalizePath(directory, mustWork = TRUE)
  identical(path, directory) || startsWith(path, paste0(directory, .Platform$file.sep))
}

rr_safe_relative_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    return(FALSE)
  }
  normalized <- gsub("\\", "/", path, fixed = TRUE)
  if (startsWith(normalized, "/") || grepl("^[A-Za-z]:/", normalized)) {
    return(FALSE)
  }
  components <- strsplit(normalized, "/", fixed = TRUE)[[1]]
  all(nzchar(components)) && !any(components %in% c(".", ".."))
}

rr_sha256 <- function(path) {
  sha256sum <- Sys.which("sha256sum")
  if (nzchar(sha256sum)) {
    output <- system2(
      unname(sha256sum),
      args = shQuote(path),
      stdout = TRUE,
      stderr = TRUE
    )
    status <- attr(output, "status")
    if (is.null(status)) {
      status <- 0L
    }
    if (identical(as.integer(status), 0L) && length(output) > 0L) {
      candidate <- sub("^([0-9A-Fa-f]{64}).*$", "\\1", output[[1]])
      if (grepl("^[0-9A-Fa-f]{64}$", candidate)) {
        return(tolower(candidate))
      }
    }
    rr_stop("sha256sum failed for ", path)
  }
  openssl <- Sys.which("openssl")
  if (nzchar(openssl)) {
    output <- system2(
      unname(openssl),
      args = c("dgst", "-sha256", shQuote(path)),
      stdout = TRUE,
      stderr = TRUE
    )
    status <- attr(output, "status")
    if (is.null(status)) {
      status <- 0L
    }
    if (identical(as.integer(status), 0L) && length(output) > 0L) {
      candidate <- sub("^.*[ =]([0-9A-Fa-f]{64})$", "\\1", output[[1]])
      if (grepl("^[0-9A-Fa-f]{64}$", candidate)) {
        return(tolower(candidate))
      }
    }
    rr_stop("openssl failed to hash ", path)
  }
  rr_stop("sha256sum or openssl is required for artifact verification.")
}

rr_aggregate_job <- function(jobs) {
  job <- jobs$aggregate_results
  if (!is.list(job)) {
    rr_stop("Configuration requires an aggregate_results job.")
  }
  required <- unlist(job$required_dependencies, use.names = FALSE)
  alternatives <- job$input_alternatives
  if (!is.character(required) || length(required) == 0L ||
      !is.list(alternatives) || length(alternatives) == 0L) {
    rr_stop("aggregate_results requires dependencies and input alternatives.")
  }
  job
}

rr_discover_success_directory <- function(job_id, configuration, repo_root,
                                          required) {
  output_root <- rr_absolute_path(configuration$defaults$output_root, repo_root)
  job_root <- file.path(output_root, job_id)
  if (!dir.exists(job_root)) {
    if (required) {
      rr_stop("Missing upstream output directory: ", job_root)
    }
    return(NA_character_)
  }
  hash_length <- as.integer(configuration$defaults$config_hash_length)
  candidates <- list.dirs(job_root, full.names = TRUE, recursive = FALSE)
  candidates <- candidates[grepl(
    paste0("^[0-9a-f]{", hash_length, "}$"),
    basename(candidates)
  )]
  candidates <- candidates[file.exists(file.path(
    candidates,
    configuration$defaults$success_marker
  ))]
  if (length(candidates) == 0L && !required) {
    return(NA_character_)
  }
  if (length(candidates) != 1L) {
    rr_stop(
      job_id,
      " requires exactly one discoverable hashed successful output; found ",
      length(candidates),
      ". Set REVISION_JOB_INPUTS_JSON to select the locked dependency hash."
    )
  }
  normalizePath(candidates[[1]], mustWork = TRUE)
}

rr_resolve_input_directories <- function(configuration, jobs, repo_root) {
  aggregate <- rr_aggregate_job(jobs)
  required <- unlist(aggregate$required_dependencies, use.names = FALSE)
  fallback <- vapply(
    aggregate$input_alternatives,
    `[[`,
    character(1),
    "fallback_job"
  )
  allowed <- unique(c(required, fallback))
  encoded_inputs <- Sys.getenv("REVISION_JOB_INPUTS_JSON", unset = "")
  if (nzchar(encoded_inputs)) {
    inputs <- rr_parse_json_text(encoded_inputs, "REVISION_JOB_INPUTS_JSON")
    if (!is.list(inputs) || is.null(names(inputs))) {
      rr_stop("REVISION_JOB_INPUTS_JSON must encode an object.")
    }
    missing <- setdiff(required, names(inputs))
    extra <- setdiff(names(inputs), allowed)
    if (length(missing) > 0L || length(extra) > 0L) {
      rr_stop(
        "REVISION_JOB_INPUTS_JSON omits required or adds unknown jobs; missing=",
        paste(missing, collapse = ","),
        "; extra=",
        paste(extra, collapse = ",")
      )
    }
    paths <- vapply(names(inputs), function(job_id) {
      value <- inputs[[job_id]]
      if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
        rr_stop("Dependency path for ", job_id, " must be one string.")
      }
      rr_absolute_path(value, repo_root, must_work = job_id %in% required)
    }, character(1))
    names(paths) <- names(inputs)
    return(paths)
  }

  paths <- vapply(allowed, function(job_id) {
    rr_discover_success_directory(
      job_id,
      configuration,
      repo_root,
      required = job_id %in% required
    )
  }, character(1))
  paths <- paths[!is.na(paths)]
  paths
}

rr_validate_input_directory <- function(path, job_id, configuration, repo_root) {
  output_root <- rr_absolute_path(configuration$defaults$output_root, repo_root, must_work = TRUE)
  job_root <- file.path(output_root, job_id)
  if (!dir.exists(job_root)) {
    return(paste0(job_id, ": missing job output root ", job_root))
  }
  normalized <- normalizePath(path, mustWork = TRUE)
  expected_parent <- normalizePath(job_root, mustWork = TRUE)
  hash_length <- as.integer(configuration$defaults$config_hash_length)
  hash_pattern <- paste0("^[0-9a-f]{", hash_length, "}$")
  errors <- character()
  if (!identical(dirname(normalized), expected_parent)) {
    errors <- c(errors, paste0(job_id, ": dependency path is outside its job output root"))
  }
  if (!grepl(hash_pattern, basename(normalized))) {
    errors <- c(errors, paste0(job_id, ": output directory name is not a locked config hash"))
  }
  errors
}

rr_csv_metadata <- function(path) {
  table <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("NA", "")
    ),
    error = function(error) error
  )
  if (inherits(table, "error")) {
    rr_stop("Cannot parse result CSV ", path, ": ", conditionMessage(table))
  }
  if (nrow(table) == 0L || ncol(table) == 0L) {
    rr_stop("Result CSV must contain at least one row and column: ", path)
  }
  list(rows = nrow(table), columns = names(table))
}

rr_validate_success <- function(job_id, path, configured_job, configuration, repo_root) {
  errors <- rr_validate_input_directory(path, job_id, configuration, repo_root)
  marker_path <- file.path(path, configuration$defaults$success_marker)
  marker <- tryCatch(rr_read_json(marker_path), error = function(error) error)
  if (inherits(marker, "error")) {
    return(list(
      errors = c(errors, paste0(job_id, ": ", conditionMessage(marker))),
      artifacts = data.frame(),
      csv_files = list()
    ))
  }
  if (!is.list(marker) || is.null(names(marker))) {
    return(list(
      errors = c(errors, paste0(job_id, ": success marker root must be an object")),
      artifacts = data.frame(),
      csv_files = list()
    ))
  }
  config_hash <- basename(normalizePath(path, mustWork = TRUE))
  expected_units <- configured_job$expected_units
  checks <- list(
    job_id = identical(marker$job_id, job_id),
    config_hash = identical(marker$config_hash, config_hash),
    expected_units = rr_positive_integer(marker$expected_units) &&
      identical(as.numeric(marker$expected_units), as.numeric(expected_units)),
    observed_unique_units = rr_positive_integer(marker$observed_unique_units) &&
      identical(as.numeric(marker$observed_unique_units), as.numeric(marker$expected_units)),
    prospective_manifest_valid = isTRUE(marker$prospective_manifest_valid),
    run_manifest_valid = isTRUE(marker$run_manifest_valid)
  )
  failed <- names(checks)[!vapply(checks, isTRUE, logical(1))]
  if (length(failed) > 0L) {
    errors <- c(errors, paste0(
      job_id,
      ": invalid success marker fields: ",
      paste(failed, collapse = ", ")
    ))
  }

  manifest_names <- c(
    configuration$defaults$prospective_manifest,
    configuration$defaults$run_manifest
  )
  for (manifest_name in manifest_names) {
    manifest_path <- file.path(path, manifest_name)
    valid <- tryCatch({
      manifest <- rr_read_json(manifest_path)
      is.list(manifest) && !is.null(names(manifest))
    }, error = function(error) FALSE)
    if (!isTRUE(valid)) {
      errors <- c(errors, paste0(job_id, ": missing or invalid ", manifest_name))
    }
  }

  output_hashes <- marker$output_hashes
  if (!is.list(output_hashes) || length(output_hashes) == 0L ||
      is.null(names(output_hashes)) || any(!nzchar(names(output_hashes)))) {
    errors <- c(errors, paste0(job_id, ": output_hashes must be a non-empty object"))
    return(list(errors = errors, artifacts = data.frame(), csv_files = list()))
  }

  artifact_rows <- list()
  csv_candidates <- list()
  for (relative_path in names(output_hashes)) {
    expected_hash <- output_hashes[[relative_path]]
    if (!rr_safe_relative_path(relative_path)) {
      errors <- c(errors, paste0(job_id, ": unsafe hashed output path ", relative_path))
      next
    }
    candidate <- file.path(path, relative_path)
    if (!file.exists(candidate) || dir.exists(candidate)) {
      errors <- c(errors, paste0(job_id, ": missing hashed output ", relative_path))
      next
    }
    if (!rr_path_is_within(candidate, path)) {
      errors <- c(errors, paste0(job_id, ": hashed output escapes its job directory"))
      next
    }
    if (!is.character(expected_hash) || length(expected_hash) != 1L ||
        !grepl("^[0-9A-Fa-f]{64}$", expected_hash)) {
      errors <- c(errors, paste0(job_id, ": invalid SHA-256 for ", relative_path))
      next
    }
    actual_hash <- tryCatch(rr_sha256(candidate), error = function(error) error)
    if (inherits(actual_hash, "error")) {
      errors <- c(errors, paste0(job_id, ": ", conditionMessage(actual_hash)))
      next
    }
    if (!identical(actual_hash, tolower(expected_hash))) {
      errors <- c(errors, paste0(job_id, ": hash mismatch for ", relative_path))
      next
    }
    is_csv <- identical(tolower(tools::file_ext(candidate)), "csv")
    metadata <- list(rows = NA_integer_, columns = character())
    if (is_csv) {
      metadata <- tryCatch(rr_csv_metadata(candidate), error = function(error) error)
      if (inherits(metadata, "error")) {
        errors <- c(errors, paste0(job_id, ": ", conditionMessage(metadata)))
        next
      }
      csv_candidates[[length(csv_candidates) + 1L]] <- list(
        job_id = job_id,
        config_hash = config_hash,
        expected_units = as.integer(expected_units),
        relative_path = relative_path,
        path = normalizePath(candidate, mustWork = TRUE),
        rows = metadata$rows,
        columns = metadata$columns
      )
    }
    artifact_rows[[length(artifact_rows) + 1L]] <- data.frame(
      job_id = job_id,
      config_hash = config_hash,
      expected_units = as.integer(expected_units),
      observed_unique_units = as.integer(marker$observed_unique_units),
      artifact_relative_path = relative_path,
      sha256 = actual_hash,
      bytes = as.numeric(file.info(candidate)$size[[1]]),
      is_result_csv = is_csv,
      result_rows = if (is_csv) metadata$rows else NA_integer_,
      result_columns = if (is_csv) length(metadata$columns) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  if (length(csv_candidates) == 0L) {
    errors <- c(errors, paste0(job_id, ": no hashed non-empty result CSV"))
  }
  artifacts <- if (length(artifact_rows) == 0L) {
    data.frame()
  } else {
    do.call(rbind, artifact_rows)
  }
  csv_files <- csv_candidates
  basenames <- vapply(csv_candidates, function(record) {
    basename(record$relative_path)
  }, character(1))
  if (identical(job_id, "empirical_cv")) {
    required_csvs <- c("results.csv", "clip_contributions.csv")
    selected <- match(required_csvs, basenames)
    counts <- vapply(required_csvs, function(name) sum(basenames == name), integer(1))
    if (anyNA(selected) || any(counts != 1L)) {
      errors <- c(errors, paste0(
        job_id,
        ": authoritative empirical outputs require results.csv and clip_contributions.csv"
      ))
      csv_files <- list()
    } else {
      csv_files <- csv_candidates[selected]
    }
  } else if (length(csv_candidates) > 1L) {
    preferred_name <- if (identical(job_id, "t100_reuse_audit")) {
      "checkpoint_results.csv"
    } else {
      "results.csv"
    }
    preferred <- which(basenames == preferred_name)
    if (length(preferred) == 1L) {
      csv_files <- csv_candidates[preferred]
    } else {
      errors <- c(errors, paste0(
        job_id,
        ": multiple hashed CSVs lack one authoritative ",
        preferred_name
      ))
      csv_files <- list()
    }
  }
  if (nrow(artifacts) > 0L) {
    selected <- vapply(csv_files, `[[`, character(1), "relative_path")
    artifacts$selected_for_aggregation <- artifacts$is_result_csv &
      artifacts$artifact_relative_path %in% selected
  }
  list(errors = errors, artifacts = artifacts, csv_files = csv_files)
}

rr_validate_all_inputs <- function(paths, jobs, configuration, repo_root) {
  validations <- Map(function(job_id, path) {
    rr_validate_success(job_id, path, jobs[[job_id]], configuration, repo_root)
  }, names(paths), unname(paths))
  errors <- unlist(lapply(validations, `[[`, "errors"), use.names = FALSE)
  if (length(errors) > 0L) {
    rr_stop(
      "Upstream validation failed before aggregation:\n- ",
      paste(errors, collapse = "\n- ")
    )
  }
  artifacts <- do.call(rbind, lapply(validations, `[[`, "artifacts"))
  csv_files <- unlist(lapply(validations, `[[`, "csv_files"), recursive = FALSE)
  represented <- unique(vapply(csv_files, `[[`, character(1), "job_id"))
  if (!setequal(represented, names(paths))) {
    rr_stop("Each selected upstream job must provide its authoritative result CSV set.")
  }
  list(artifacts = artifacts, csv_files = csv_files)
}

rr_reused_result_path <- function(method, repo_root) {
  directory <- switch(
    method,
    graphicalvar = "graphicalvar_bounded_nvar4",
    gimme = "gimme_bounded_nvar4",
    rr_stop("Unknown reuse method: ", method)
  )
  file.path(
    repo_root,
    "simulation_outputs",
    "final_simulation_results",
    directory,
    "benchmark_results.csv"
  )
}

rr_reuse_entry <- function(audit, method) {
  if (!is.list(audit$methods) || !is.list(audit$methods[[method]])) {
    rr_stop("Reuse audit lacks methods.", method, ".")
  }
  entry <- audit$methods[[method]]
  if (!is.logical(entry$reusable) || length(entry$reusable) != 1L) {
    rr_stop("Reuse audit methods.", method, ".reusable must be boolean.")
  }
  entry
}

rr_reuse_has_panel_hash_equivalence <- function(entry) {
  checks <- entry$checks
  if (!is.list(checks)) {
    checks <- list()
  }
  candidates <- c(
    "panel_input_hash_equivalence",
    "equivalent_panel_input_hashes",
    "panel_input_hashes_equivalent"
  )
  any(vapply(candidates, function(name) {
    isTRUE(checks[[name]]) || isTRUE(entry[[name]])
  }, logical(1)))
}

rr_validate_reused_result <- function(method, entry, audit_path, repo_root) {
  result_path <- rr_reused_result_path(method, repo_root)
  if (!isTRUE(entry$reusable)) {
    rr_stop("Cannot select a reusable result rejected by the audit: ", method)
  }
  if (!file.exists(result_path)) {
    rr_stop("Reuse audit accepted a missing result file: ", result_path)
  }
  expected_hash <- if (is.list(entry$input_hashes)) {
    entry$input_hashes$results
  } else {
    NULL
  }
  if (!is.character(expected_hash) || length(expected_hash) != 1L ||
      !grepl("^[0-9A-Fa-f]{64}$", expected_hash)) {
    rr_stop("Reuse audit lacks the accepted results SHA-256 for ", method, ".")
  }
  actual_hash <- rr_sha256(result_path)
  if (!identical(actual_hash, tolower(expected_hash))) {
    rr_stop("Reusable result hash changed after audit: ", result_path)
  }
  metadata <- rr_csv_metadata(result_path)
  source_job <- paste0("t100_", method, "_reused")
  config_hash <- basename(normalizePath(audit_path, mustWork = TRUE))
  relative_path <- sub(
    paste0("^", normalizePath(repo_root, mustWork = TRUE), "/"),
    "",
    normalizePath(result_path, mustWork = TRUE)
  )
  record <- list(
    job_id = source_job,
    config_hash = config_hash,
    expected_units = metadata$rows,
    relative_path = relative_path,
    path = normalizePath(result_path, mustWork = TRUE),
    rows = metadata$rows,
    columns = metadata$columns
  )
  artifact <- data.frame(
    job_id = source_job,
    config_hash = config_hash,
    expected_units = metadata$rows,
    observed_unique_units = metadata$rows,
    artifact_relative_path = relative_path,
    sha256 = actual_hash,
    bytes = as.numeric(file.info(result_path)$size[[1]]),
    is_result_csv = TRUE,
    result_rows = metadata$rows,
    result_columns = length(metadata$columns),
    selected_for_aggregation = TRUE,
    stringsAsFactors = FALSE
  )
  list(artifact = artifact, csv_file = record)
}

rr_validate_selected_inputs <- function(paths, jobs, configuration, repo_root) {
  aggregate <- rr_aggregate_job(jobs)
  required <- unlist(aggregate$required_dependencies, use.names = FALSE)
  missing <- setdiff(required, names(paths))
  if (length(missing) > 0L) {
    rr_stop("Missing required aggregate inputs: ", paste(missing, collapse = ", "))
  }
  selected <- rr_validate_all_inputs(
    paths[required],
    jobs,
    configuration,
    repo_root
  )
  selected_jobs <- required
  for (alternative in aggregate$input_alternatives) {
    audit_job <- alternative$audit_job
    audit_path <- paths[[audit_job]]
    audit_file <- file.path(audit_path, alternative$artifact)
    audit <- rr_read_json(audit_file)
    method <- alternative$method
    entry <- rr_reuse_entry(audit, method)
    if (isTRUE(entry$reusable) && rr_reuse_has_panel_hash_equivalence(entry)) {
      reused <- rr_validate_reused_result(method, entry, audit_path, repo_root)
      selected$artifacts <- rbind(selected$artifacts, reused$artifact)
      selected$csv_files[[length(selected$csv_files) + 1L]] <- reused$csv_file
      selected_jobs <- c(selected_jobs, reused$csv_file$job_id)
      next
    }
    fallback_job <- alternative$fallback_job
    fallback_path <- paths[[fallback_job]]
    if (is.null(fallback_path) || !dir.exists(fallback_path)) {
      rr_stop(
        "Reuse audit rejected ", method,
        "; completed fallback input is required: ", fallback_job
      )
    }
    fallback <- rr_validate_all_inputs(
      setNames(fallback_path, fallback_job),
      jobs,
      configuration,
      repo_root
    )
    selected$artifacts <- rbind(selected$artifacts, fallback$artifacts)
    selected$csv_files <- c(selected$csv_files, fallback$csv_files)
    selected_jobs <- c(selected_jobs, fallback_job)
  }
  selected$selected_jobs <- unique(selected_jobs)
  selected
}

rr_bind_rows_fill <- function(frames) {
  frames <- frames[vapply(frames, is.data.frame, logical(1))]
  if (length(frames) == 0L) {
    return(data.frame())
  }
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(frame) {
    for (column in setdiff(columns, names(frame))) {
      frame[[column]] <- rep(NA, nrow(frame))
    }
    frame[, columns, drop = FALSE]
  })
  do.call(rbind, frames)
}

rr_read_result_record <- function(record) {
  frame <- utils::read.csv(
    record$path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("NA", "")
  )
  if (!identical(nrow(frame), record$rows)) {
    rr_stop("Result CSV row count changed during aggregation: ", record$path)
  }
  reserved <- c(
    ".source_job",
    ".source_file",
    ".source_config_hash",
    ".source_expected_units"
  )
  if (length(intersect(names(frame), reserved)) > 0L) {
    rr_stop("Result CSV uses reserved provenance columns: ", record$path)
  }
  frame$.source_job <- record$job_id
  frame$.source_file <- record$relative_path
  frame$.source_config_hash <- record$config_hash
  frame$.source_expected_units <- record$expected_units
  frame[, c(reserved, setdiff(names(frame), reserved)), drop = FALSE]
}

rr_load_long_results <- function(csv_files) {
  rr_bind_rows_fill(lapply(csv_files, rr_read_result_record))
}

rr_alias_column <- function(data, candidates, default = NA, required = FALSE) {
  present <- candidates[candidates %in% names(data)]
  if (length(present) > 0L) {
    return(data[[present[[1]]]])
  }
  if (required) {
    rr_stop("Results require one of these columns: ", paste(candidates, collapse = ", "))
  }
  rep(default, nrow(data))
}

rr_numeric_column <- function(data, candidates, default = NA_real_, required = FALSE) {
  suppressWarnings(as.numeric(rr_alias_column(data, candidates, default, required)))
}

rr_character_column <- function(data, candidates, default = NA_character_,
                                required = FALSE) {
  as.character(rr_alias_column(data, candidates, default, required))
}

rr_frame_key <- function(data, columns) {
  values <- lapply(columns, function(column) {
    value <- if (column %in% names(data)) data[[column]] else rep(NA, nrow(data))
    value <- as.character(value)
    value[is.na(value) | !nzchar(value)] <- "<NA>"
    value
  })
  do.call(paste, c(values, sep = "\r"))
}

rr_membership_vector <- function(value) {
  if (is.list(value) && length(value) == 1L) {
    value <- value[[1]]
  }
  if (length(value) > 1L) {
    result <- as.character(value)
    names(result) <- names(value)
    return(result)
  }
  if (length(value) == 0L || is.na(value) || !nzchar(as.character(value))) {
    return(character())
  }
  tokens <- strsplit(as.character(value), "[;,|[:space:]]+", perl = TRUE)[[1]]
  if (all(grepl("=", tokens, fixed = TRUE))) {
    item <- sub("=.*$", "", tokens)
    membership <- sub("^[^=]*=", "", tokens)
    if (any(!nzchar(item)) || any(!nzchar(membership)) || anyDuplicated(item)) {
      return(character())
    }
    return(setNames(membership, item))
  }
  tokens
}

rr_canonical_membership_names <- function(membership, label) {
  if (length(membership) == 0L || is.null(names(membership))) {
    return(membership)
  }
  original <- names(membership)
  canonical <- sub("\\.Ord[01]$", "", original)
  if (any(!nzchar(canonical))) {
    rr_stop(label, " membership contains an empty canonical item name.")
  }
  duplicated <- unique(canonical[duplicated(canonical)])
  if (length(duplicated) > 0L) {
    rr_stop(
      label,
      " membership has colliding canonical item names after removing ordinal suffixes: ",
      paste(duplicated, collapse = ", ")
    )
  }
  names(membership) <- canonical
  membership
}

rr_align_memberships <- function(truth, estimate) {
  truth <- rr_canonical_membership_names(
    rr_membership_vector(truth),
    "Truth"
  )
  estimate <- rr_canonical_membership_names(
    rr_membership_vector(estimate),
    "Estimated"
  )
  if (length(truth) == 0L || length(estimate) == 0L) {
    return(NULL)
  }
  truth_named <- !is.null(names(truth))
  estimate_named <- !is.null(names(estimate))
  if (xor(truth_named, estimate_named)) {
    rr_stop("Truth and estimated memberships must both use item names.")
  }
  if (truth_named) {
    missing <- setdiff(names(truth), names(estimate))
    extra <- setdiff(names(estimate), names(truth))
    if (length(missing) > 0L || length(extra) > 0L) {
      rr_stop(
        "Membership item sets differ; missing=",
        paste(missing, collapse = ","),
        "; extra=",
        paste(extra, collapse = ",")
      )
    }
    estimate <- estimate[names(truth)]
  }
  if (length(truth) != length(estimate)) {
    rr_stop("Truth and estimated memberships differ in length.")
  }
  list(truth = unname(truth), estimate = unname(estimate))
}

rr_assignment_accuracy <- function(truth, estimate) {
  aligned <- rr_align_memberships(truth, estimate)
  if (is.null(aligned)) {
    return(NA_real_)
  }
  if (!requireNamespace("clue", quietly = TRUE)) {
    rr_stop("Package 'clue' is required for assignment accuracy.")
  }
  contingency <- unclass(table(aligned$truth, aligned$estimate))
  size <- max(dim(contingency))
  padded <- matrix(0, nrow = size, ncol = size)
  padded[seq_len(nrow(contingency)), seq_len(ncol(contingency))] <- contingency
  maximum <- if (length(padded) == 0L) 0 else max(padded)
  assignment <- clue::solve_LSAP(maximum - padded)
  matched <- sum(padded[cbind(seq_len(size), assignment)])
  matched / length(aligned$truth)
}

rr_choose_two <- function(value) {
  value * (value - 1) / 2
}

rr_adjusted_rand_index <- function(truth, estimate) {
  aligned <- rr_align_memberships(truth, estimate)
  if (is.null(aligned) || length(aligned$truth) < 2L) {
    return(NA_real_)
  }
  truth <- aligned$truth
  estimate <- aligned$estimate
  contingency <- table(truth, estimate)
  observed <- sum(rr_choose_two(contingency))
  row_pairs <- sum(rr_choose_two(rowSums(contingency)))
  column_pairs <- sum(rr_choose_two(colSums(contingency)))
  total_pairs <- rr_choose_two(length(truth))
  expected <- row_pairs * column_pairs / total_pairs
  maximum <- (row_pairs + column_pairs) / 2
  denominator <- maximum - expected
  if (abs(denominator) < .Machine$double.eps) {
    truth_relation <- outer(truth, truth, "==")
    estimate_relation <- outer(estimate, estimate, "==")
    return(as.numeric(identical(truth_relation, estimate_relation)))
  }
  (observed - expected) / denominator
}

rr_variation_information <- function(truth, estimate) {
  aligned <- rr_align_memberships(truth, estimate)
  if (is.null(aligned)) {
    return(NA_real_)
  }
  truth <- aligned$truth
  estimate <- aligned$estimate
  contingency <- table(truth, estimate)
  joint <- contingency / sum(contingency)
  truth_probability <- rowSums(joint)
  estimate_probability <- colSums(joint)
  entropy <- function(probability) {
    probability <- probability[probability > 0]
    -sum(probability * log(probability))
  }
  mutual_information <- 0
  for (row in seq_len(nrow(joint))) {
    for (column in seq_len(ncol(joint))) {
      probability <- joint[row, column]
      if (probability > 0) {
        mutual_information <- mutual_information + probability * log(
          probability / (truth_probability[[row]] * estimate_probability[[column]])
        )
      }
    }
  }
  max(0, entropy(truth_probability) + entropy(estimate_probability) -
    2 * mutual_information)
}

rr_partition_metric <- function(data, aliases, truth_column, estimate_column,
                                metric, force_recompute = rep(FALSE, nrow(data)),
                                eligible = rep(TRUE, nrow(data))) {
  present <- aliases[aliases %in% names(data)]
  result <- rep(NA_real_, nrow(data))
  if (length(present) > 0L) {
    result <- suppressWarnings(as.numeric(data[[present[[1]]]]))
  }
  if (length(force_recompute) != nrow(data) || length(eligible) != nrow(data)) {
    rr_stop("Partition-metric recomputation flags differ from the result rows.")
  }
  if (!(truth_column %in% names(data) && estimate_column %in% names(data))) {
    return(list(value = result, recomputed = rep(FALSE, nrow(data))))
  }
  targets <- which(eligible & (force_recompute | !is.finite(result)))
  truth_values <- as.character(data[[truth_column]][targets])
  estimate_values <- as.character(data[[estimate_column]][targets])
  pair_keys <- paste(truth_values, estimate_values, sep = "\r")
  unique_pair <- !duplicated(pair_keys)
  unique_targets <- targets[unique_pair]
  unique_values <- vapply(unique_targets, function(index) {
    metric(data[[truth_column]][[index]], data[[estimate_column]][[index]])
  }, numeric(1))
  result[targets] <- unique_values[match(pair_keys, pair_keys[unique_pair])]
  recomputed <- rep(FALSE, nrow(data))
  recomputed[targets] <- TRUE
  list(value = result, recomputed = recomputed)
}

rr_partition_recomputation_audit <- function(data, stored, recomputed) {
  source_job <- rr_character_column(data, ".source_job", required = TRUE)
  metrics <- names(stored)
  groups <- split(seq_len(nrow(data)), paste(source_job, data$method_id, sep = "\r"))
  rows <- lapply(groups, function(indices) {
    lapply(metrics, function(metric) {
      before <- stored[[metric]][indices]
      after <- data[[metric]][indices]
      recalculated <- recomputed[[metric]][indices]
      usable <- data$usable[indices]
      changed <- recalculated & is.finite(before) & is.finite(after) &
        abs(before - after) > 1e-12
      data.frame(
        source_job = source_job[indices[[1]]],
        method_id = data$method_id[indices[[1]]],
        metric = metric,
        rows = length(indices),
        usable_rows = sum(usable),
        stored_finite_usable = sum(usable & is.finite(before)),
        recomputed_usable = sum(usable & recalculated),
        repaired_missing_usable = sum(
          usable & recalculated & !is.finite(before) & is.finite(after)
        ),
        overwritten_finite_usable = sum(usable & recalculated & is.finite(before)),
        changed_finite_usable = sum(usable & changed),
        stringsAsFactors = FALSE
      )
    })
  })
  rr_bind_rows_fill(unlist(rows, recursive = FALSE))
}

rr_total_runtime <- function(data) {
  direct <- rr_numeric_column(data, c("runtime_sec", "elapsed_sec"))
  structure <- rr_numeric_column(data, "structure_fit_runtime_sec")
  readout <- rr_numeric_column(data, "readout_runtime_sec")
  combined <- structure
  both <- is.finite(structure) & is.finite(readout)
  combined[both] <- structure[both] + readout[both]
  direct[!is.finite(direct)] <- combined[!is.finite(direct)]
  direct
}

rr_canonical_method_results <- function(long_results) {
  source_job <- rr_character_column(long_results, ".source_job", required = TRUE)
  method <- rr_character_column(
    long_results,
    c("method_id", "method_family", "method")
  )
  keep <- !source_job %in% c("empirical_cv", "t100_reuse_audit") &
    !is.na(method) & nzchar(method)
  data <- long_results[keep, , drop = FALSE]
  if (nrow(data) == 0L) {
    rr_stop("Validated inputs contain no method-result rows.")
  }
  data$method_id <- rr_character_column(data, c("method_id", "method_family", "method"),
    required = TRUE)
  data$method_id[data$method_id %in% c("static_correlation", "static_EGA_glasso")] <-
    "static_EGA"
  data$community_algorithm <- rr_character_column(
    data,
    c("community_algorithm", "algorithm"),
    required = TRUE
  )
  data$community_algorithm <- tolower(data$community_algorithm)
  data$status <- tolower(rr_character_column(data, "status", required = TRUE))
  data$status[data$status %in% c("success", "complete", "completed")] <- "ok"
  allowed_status <- c("ok", "error", "timeout")
  if (any(!data$status %in% allowed_status)) {
    rr_stop("Method results contain unsupported status values.")
  }
  data$n_dim_true <- rr_numeric_column(data, c("n_dim_true", "scenario_nfac"),
    required = TRUE)
  data$n_dim_hat <- rr_numeric_column(data, "n_dim_hat", required = TRUE)
  if (any(!is.finite(data$n_dim_true))) {
    rr_stop("Every method result requires a finite generating dimension.")
  }
  data$usable <- data$status == "ok" & is.finite(data$n_dim_hat)
  data$correct <- data$usable & data$n_dim_hat == data$n_dim_true
  data$signed_dimension_error <- ifelse(
    data$usable,
    data$n_dim_hat - data$n_dim_true,
    NA_real_
  )
  data$absolute_dimension_error <- abs(data$signed_dimension_error)
  partition_aliases <- list(
    assignment_accuracy = "assignment_accuracy",
    adjusted_rand_index = c("adjusted_rand_index", "ari", "ARI"),
    variation_information = c("variation_information", "vi", "VI")
  )
  stored_partition <- lapply(partition_aliases, function(aliases) {
    rr_numeric_column(data, aliases)
  })
  force_partition_recompute <- tolower(data$method_id) == "dynega" & data$usable
  assignment_result <- rr_partition_metric(
    data,
    partition_aliases$assignment_accuracy,
    "true_membership",
    "estimated_membership",
    rr_assignment_accuracy,
    force_recompute = force_partition_recompute,
    eligible = data$usable
  )
  adjusted_rand_result <- rr_partition_metric(
    data,
    partition_aliases$adjusted_rand_index,
    "true_membership",
    "estimated_membership",
    rr_adjusted_rand_index,
    force_recompute = force_partition_recompute,
    eligible = data$usable
  )
  variation_result <- rr_partition_metric(
    data,
    partition_aliases$variation_information,
    "true_membership",
    "estimated_membership",
    rr_variation_information,
    force_recompute = force_partition_recompute,
    eligible = data$usable
  )
  data$assignment_accuracy <- assignment_result$value
  data$adjusted_rand_index <- adjusted_rand_result$value
  data$variation_information <- variation_result$value
  recomputed_partition <- list(
    assignment_accuracy = assignment_result$recomputed,
    adjusted_rand_index = adjusted_rand_result$recomputed,
    variation_information = variation_result$recomputed
  )
  required_partition <- c(
    "assignment_accuracy",
    "adjusted_rand_index",
    "variation_information"
  )
  invalid_partition <- vapply(required_partition, function(column) {
    any(data$usable & !is.finite(data[[column]]))
  }, logical(1))
  if (any(invalid_partition)) {
    rr_stop(
      "Usable rows lack partition metrics: ",
      paste(required_partition[invalid_partition], collapse = ", "),
      ". Supply metric columns or true_membership and estimated_membership."
    )
  }
  attr(data, "partition_metric_recomputation_audit") <-
    rr_partition_recomputation_audit(
      data,
      stored_partition,
      recomputed_partition
    )
  data$score_cor_common <- rr_numeric_column(data, "score_cor_common")
  data$score_nrmse_common <- rr_numeric_column(data, "score_nrmse_common")
  data$score_correlation_marginal <- ifelse(
    data$usable & is.finite(data$score_cor_common),
    data$score_cor_common,
    0
  )
  data$score_nrmse_marginal <- ifelse(
    data$usable & is.finite(data$score_nrmse_common),
    data$score_nrmse_common,
    2
  )
  data$runtime_sec <- rr_total_runtime(data)
  if (any(is.finite(data$runtime_sec) & data$runtime_sec < 0) ||
      any(data$usable & !is.finite(data$runtime_sec))) {
    rr_stop("Runtime must be finite and nonnegative for every usable readout.")
  }
  data$completion_within_900s <- data$status == "ok" &
    is.finite(data$runtime_sec) & data$runtime_sec <= 900
  runtime_for_restriction <- data$runtime_sec
  runtime_for_restriction[!is.finite(runtime_for_restriction)] <- 900
  data$restricted_compute_900s <- pmin(pmax(runtime_for_restriction, 0), 900)
  data$scenario <- rr_character_column(data, "scenario")
  data$scenario_family <- rr_character_column(data, "scenario_family", "unspecified")
  data$TimeP <- rr_numeric_column(data, c("TimeP", "time_points"))
  data$N <- rr_numeric_column(data, "N")
  data$NVAR <- rr_numeric_column(data, "NVAR")
  data$Error_sd <- rr_numeric_column(data, c("Error_sd", "Error"))
  data$Loadings <- rr_numeric_column(data, "Loadings")
  data$Q <- rr_numeric_column(data, "Q")
  data$rep <- rr_numeric_column(data, "rep")
  data$dt <- rr_numeric_column(data, "dt")
  data$seed_base <- rr_numeric_column(data, "seed_base")
  data$panel_input_hash <- rr_character_column(data, "panel_input_hash")
  data$structure_fit_key <- rr_character_column(data, "structure_fit_key")
  data$structure_fit_status <- tolower(rr_character_column(
    data,
    "structure_fit_status"
  ))
  data$structure_hash <- rr_character_column(data, "structure_hash")
  component_hash_fields <- c(
    "component_hash_latent_standard_normals",
    "component_hash_residual_standard_normals",
    "component_hash_loading_basis",
    "component_hash_activity_schedule",
    "component_hash_linear_z"
  )
  for (field in component_hash_fields) {
    data[[field]] <- rr_character_column(data, field)
  }
  data$challenge_id <- rr_character_column(data, "challenge_id", "none")
  data$challenge_id[is.na(data$challenge_id) | !nzchar(data$challenge_id)] <- "none"
  data$representation <- rr_character_column(data, "representation", "linear_z")
  data$representation[is.na(data$representation) | !nzchar(data$representation)] <-
    "linear_z"
  data$stability_class <- rr_character_column(data, "stability_class")
  if (any(is.na(data$stability_class) | !nzchar(data$stability_class))) {
    scenarios <- revision_scenarios()
    matched <- match(data$scenario, scenarios$scenario)
    missing <- is.na(data$stability_class) | !nzchar(data$stability_class)
    data$stability_class[missing] <- scenarios$stability_class[matched[missing]]
  }
  data$stability_class[is.na(data$stability_class)] <- "unknown"
  data$analysis_scope <- ifelse(
    grepl("^challenge_", data$.source_job),
    "challenge",
    "matched"
  )
  data$panel_cluster_key <- rr_character_column(data, "panel_key")
  missing_panel <- is.na(data$panel_cluster_key) | !nzchar(data$panel_cluster_key)
  signature_columns <- c(
    "scenario", "N", "TimeP", "NVAR", "Error_sd", "Loadings", "Q", "rep",
    "challenge_id", "representation"
  )
  data$panel_cluster_key[missing_panel] <- rr_frame_key(
    data[missing_panel, , drop = FALSE],
    signature_columns
  )
  data$method_pair_key <- rr_frame_key(data, c(signature_columns, "community_algorithm"))
  data$horizon_pair_key <- rr_frame_key(
    data,
    setdiff(c(signature_columns, "community_algorithm"), "TimeP")
  )
  data$challenge_pair_key <- rr_frame_key(
    data,
    c("scenario", "N", "NVAR", "Error_sd", "Loadings", "Q", "rep",
      "method_id", "community_algorithm")
  )
  data$challenge_common_key <- rr_frame_key(
    data,
    c("scenario", "N", "NVAR", "Error_sd", "Loadings", "Q", "rep")
  )
  data$representation_pair_key <- rr_frame_key(
    data,
    c("scenario", "challenge_id", "rep", "method_id", "community_algorithm")
  )
  contribution_columns <- list(
    p_usable = as.numeric(data$usable),
    p_correct_given_usable = ifelse(data$usable, as.numeric(data$correct), NA_real_),
    p_usable_and_correct = as.numeric(data$correct),
    signed_dimension_error = data$signed_dimension_error,
    absolute_dimension_error = data$absolute_dimension_error,
    adjusted_rand_index = ifelse(data$usable, data$adjusted_rand_index, NA_real_),
    variation_information = ifelse(data$usable, data$variation_information, NA_real_),
    assignment_accuracy_conditional = ifelse(
      data$usable,
      data$assignment_accuracy,
      NA_real_
    ),
    score_correlation_marginal = data$score_correlation_marginal,
    score_nrmse_marginal = data$score_nrmse_marginal,
    completion_within_900s = as.numeric(data$completion_within_900s),
    restricted_compute_900s = data$restricted_compute_900s,
    runtime_median_successful = ifelse(data$usable, data$runtime_sec, NA_real_),
    runtime_iqr_successful = ifelse(data$usable, data$runtime_sec, NA_real_)
  )
  for (name in names(contribution_columns)) {
    data[[paste0("value_", name)]] <- contribution_columns[[name]]
  }
  data
}

rr_stop_identity_errors <- function(label, errors) {
  if (length(errors) == 0L) {
    return(invisible(TRUE))
  }
  errors <- unique(errors)
  preview <- head(errors, 20L)
  rr_stop(
    label,
    " failed before estimation:\n- ",
    paste(preview, collapse = "\n- "),
    if (length(errors) > length(preview)) "\n- additional mismatches omitted" else ""
  )
}

rr_readout_grid_errors <- function(group, expected_methods, label) {
  expected_algorithms <- c("louvain", "leiden", "walktrap")
  errors <- character()
  methods <- as.character(group$method_id)
  algorithms <- tolower(as.character(group$community_algorithm))
  if (!setequal(unique(methods), expected_methods)) {
    errors <- c(errors, paste0(
      label,
      ": expected methods {",
      paste(expected_methods, collapse = ","),
      "}, observed {",
      paste(sort(unique(methods)), collapse = ","),
      "}"
    ))
  }
  if (!setequal(unique(algorithms), expected_algorithms)) {
    errors <- c(errors, paste0(
      label,
      ": expected algorithms {",
      paste(expected_algorithms, collapse = ","),
      "}, observed {",
      paste(sort(unique(algorithms)), collapse = ","),
      "}"
    ))
  }
  expected_rows <- length(expected_methods) * length(expected_algorithms)
  keys <- paste(methods, algorithms, sep = "\r")
  if (nrow(group) != expected_rows || length(unique(keys)) != expected_rows) {
    errors <- c(errors, paste0(
      label,
      ": expected exactly one row for each method-algorithm readout (",
      expected_rows,
      " total), observed ",
      nrow(group),
      " rows and ",
      length(unique(keys)),
      " unique readouts"
    ))
  }
  errors
}

rr_valid_sha256 <- function(value) {
  length(value) == 1L && !is.na(value) &&
    grepl("^[0-9A-Fa-f]{64}$", as.character(value))
}

rr_validate_matched_panel_identity <- function(data) {
  matched <- data[data$analysis_scope == "matched", , drop = FALSE]
  if (nrow(matched) == 0L) {
    rr_stop("Identity validation requires matched method results.")
  }
  identity_fields <- c(
    "panel_input_hash", "seed_base", "scenario", "N", "TimeP", "dt",
    "n_dim_true", "NVAR", "Loadings", "Error_sd", "Q", "rep"
  )
  groups <- rr_group_indices(matched, c("panel_cluster_key", "representation"))
  rows <- list()
  errors <- character()
  for (group_key in names(groups)) {
    group <- matched[groups[[group_key]], , drop = FALSE]
    label <- group$panel_cluster_key[[1]]
    errors <- c(errors, rr_readout_grid_errors(
      group,
      c("dynEGA", "graphicalVAR", "gimme", "static_EGA", "oracle_blocks"),
      label
    ))
    values <- list()
    for (field in identity_fields) {
      field_values <- group[[field]]
      if (is.character(field_values)) {
        valid <- !is.na(field_values) & nzchar(field_values)
      } else {
        valid <- is.finite(field_values)
      }
      unique_values <- unique(field_values[valid])
      if (!all(valid) || length(unique_values) != 1L) {
        errors <- c(errors, paste0(
          label,
          ": missing or inconsistent ",
          field
        ))
        values[[field]] <- NA
      } else {
        values[[field]] <- unique_values[[1]]
      }
    }
    hash <- as.character(values$panel_input_hash)
    if (!rr_valid_sha256(hash)) {
      errors <- c(errors, paste0(
        label,
        ": panel_input_hash must be 64 hexadecimal characters"
      ))
    }
    if (!is.finite(as.numeric(values$TimeP)) ||
        !as.numeric(values$TimeP) %in% c(25, 100)) {
      errors <- c(errors, paste0(label, ": matched TimeP must be 25 or 100"))
    }
    if (!identical(unique(group$representation), "linear_z")) {
      errors <- c(errors, paste0(label, ": matched representation must be linear_z"))
    }
    rows[[length(rows) + 1L]] <- data.frame(
      validation_type = "matched_panel",
      panel_key = label,
      representation = group$representation[[1]],
      method_count = length(unique(group$method_id)),
      methods = paste(sort(unique(group$method_id)), collapse = ";"),
      readout_rows = nrow(group),
      panel_input_hash = hash,
      seed_base = as.numeric(values$seed_base),
      scenario = as.character(values$scenario),
      N = as.numeric(values$N),
      TimeP = as.numeric(values$TimeP),
      dt = as.numeric(values$dt),
      n_dim_true = as.numeric(values$n_dim_true),
      NVAR = as.numeric(values$NVAR),
      Loadings = as.numeric(values$Loadings),
      Error_sd = as.numeric(values$Error_sd),
      Q = as.numeric(values$Q),
      rep = as.numeric(values$rep),
      identity_valid = TRUE,
      stringsAsFactors = FALSE
    )
  }
  rr_stop_identity_errors("Matched-panel identity validation", errors)
  rr_bind_rows_fill(rows)
}

rr_validate_challenge_identity <- function(data) {
  challenge <- data[data$analysis_scope == "challenge", , drop = FALSE]
  if (nrow(challenge) == 0L) {
    rr_stop("Identity validation requires challenge method results.")
  }
  expected_methods <- c("dynEGA", "graphicalVAR", "static_EGA", "oracle_blocks")
  panel_groups <- rr_group_indices(
    challenge,
    c("panel_cluster_key", "representation")
  )
  rows <- list()
  errors <- character()
  for (group_key in names(panel_groups)) {
    group <- challenge[panel_groups[[group_key]], , drop = FALSE]
    label <- paste(group$panel_cluster_key[[1]], group$representation[[1]], sep = "|")
    errors <- c(errors, rr_readout_grid_errors(group, expected_methods, label))
    panel_hash <- unique(group$panel_input_hash[
      !is.na(group$panel_input_hash) & nzchar(group$panel_input_hash)
    ])
    if (length(panel_hash) != 1L || !rr_valid_sha256(panel_hash[[1]])) {
      errors <- c(errors, paste0(label, ": inconsistent or invalid panel_input_hash"))
      panel_hash <- NA_character_
    }
    rows[[length(rows) + 1L]] <- data.frame(
      validation_type = "challenge_representation_panel",
      panel_key = group$panel_cluster_key[[1]],
      representation = group$representation[[1]],
      method_count = length(unique(group$method_id)),
      methods = paste(sort(unique(group$method_id)), collapse = ";"),
      readout_rows = nrow(group),
      panel_input_hash = panel_hash,
      scenario = group$scenario[[1]],
      rep = group$rep[[1]],
      identity_valid = TRUE,
      stringsAsFactors = FALSE
    )
  }

  expected_variants <- c(
    "ideal_baseline",
    "independent_initialization",
    "smooth_activity_gate",
    "cross_loadings",
    "residual_equicorrelation",
    "latent_shock_equicorrelation",
    "softmax_bounded"
  )
  expected_variant_representations <- c(
    paste(expected_variants[expected_variants != "softmax_bounded"], "linear_z",
      sep = "\r"
    ),
    "softmax_bounded\rsoftmax_raw",
    "softmax_bounded\rsoftmax_clr"
  )
  shared_hash_fields <- c(
    "component_hash_latent_standard_normals",
    "component_hash_residual_standard_normals",
    "component_hash_loading_basis",
    "component_hash_activity_schedule"
  )
  common_groups <- rr_group_indices(challenge, "challenge_common_key")
  for (group_key in names(common_groups)) {
    group <- challenge[common_groups[[group_key]], , drop = FALSE]
    label <- paste(group$scenario[[1]], group$rep[[1]], sep = "|rep=")
    observed_variant_representations <- unique(paste(
      group$challenge_id,
      group$representation,
      sep = "\r"
    ))
    if (!setequal(observed_variant_representations, expected_variant_representations)) {
      errors <- c(errors, paste0(
        label,
        ": challenge variants or representations are incomplete or unexpected"
      ))
    }
    hashes <- list()
    for (field in shared_hash_fields) {
      values <- unique(group[[field]][!is.na(group[[field]]) & nzchar(group[[field]])])
      if (length(values) != 1L || !rr_valid_sha256(values[[1]])) {
        errors <- c(errors, paste0(label, ": inconsistent or invalid ", field))
        hashes[[field]] <- NA_character_
      } else {
        hashes[[field]] <- values[[1]]
      }
    }
    linear_reference <- group[
      (group$challenge_id == "ideal_baseline" & group$representation == "linear_z") |
        (group$challenge_id == "softmax_bounded" &
          group$representation %in% c("softmax_raw", "softmax_clr")),
      ,
      drop = FALSE
    ]
    linear_hash <- unique(linear_reference$component_hash_linear_z[
      !is.na(linear_reference$component_hash_linear_z) &
        nzchar(linear_reference$component_hash_linear_z)
    ])
    if (length(linear_hash) != 1L || !rr_valid_sha256(linear_hash[[1]])) {
      errors <- c(errors, paste0(
        label,
        ": ideal_baseline, softmax_raw, and softmax_clr do not share a valid linear_z hash"
      ))
      linear_hash <- NA_character_
    }
    rows[[length(rows) + 1L]] <- data.frame(
      validation_type = "challenge_common_pair",
      panel_key = group$challenge_common_key[[1]],
      representation = "all_variants",
      method_count = length(unique(group$method_id)),
      methods = paste(sort(unique(group$method_id)), collapse = ";"),
      readout_rows = nrow(group),
      scenario = group$scenario[[1]],
      rep = group$rep[[1]],
      component_hash_latent_standard_normals = hashes[[
        "component_hash_latent_standard_normals"
      ]],
      component_hash_residual_standard_normals = hashes[[
        "component_hash_residual_standard_normals"
      ]],
      component_hash_loading_basis = hashes[["component_hash_loading_basis"]],
      component_hash_activity_schedule = hashes[[
        "component_hash_activity_schedule"
      ]],
      component_hash_linear_z = linear_hash,
      identity_valid = TRUE,
      stringsAsFactors = FALSE
    )
  }
  rr_stop_identity_errors("Challenge identity validation", errors)
  rr_bind_rows_fill(rows)
}

rr_validate_structure_reuse <- function(data) {
  required <- c("structure_fit_key", "structure_fit_status", "structure_hash")
  if (any(!required %in% names(data)) ||
      any(is.na(data$structure_fit_key) | !nzchar(data$structure_fit_key))) {
    rr_stop(
      "Structure-reuse validation requires structure_fit_key, ",
      "structure_fit_status, and structure_hash for every method-result row."
    )
  }
  groups <- rr_group_indices(data, "structure_fit_key")
  expected_algorithms <- c("louvain", "leiden", "walktrap")
  rows <- list()
  errors <- character()
  for (group_key in names(groups)) {
    group <- data[groups[[group_key]], , drop = FALSE]
    label <- group$structure_fit_key[[1]]
    algorithms <- tolower(as.character(group$community_algorithm))
    if (nrow(group) != 3L || anyDuplicated(algorithms) ||
        !setequal(algorithms, expected_algorithms)) {
      errors <- c(errors, paste0(
        label,
        ": structure fit must have exactly one Louvain, Leiden, and Walktrap row"
      ))
    }
    if (length(unique(group$method_id)) != 1L ||
        length(unique(group$panel_cluster_key)) != 1L ||
        length(unique(group$representation)) != 1L) {
      errors <- c(errors, paste0(
        label,
        ": structure fit key spans methods, panels, or representations"
      ))
    }
    fit_status <- unique(group$structure_fit_status[
      !is.na(group$structure_fit_status) & nzchar(group$structure_fit_status)
    ])
    if (length(fit_status) != 1L ||
        !fit_status %in% c("ok", "error", "timeout")) {
      errors <- c(errors, paste0(
        label,
        ": structure fit rows have missing or inconsistent fit status"
      ))
      fit_status <- NA_character_
    }
    fit_state <- if (identical(fit_status, "ok")) {
      hashes <- unique(group$structure_hash[
        !is.na(group$structure_hash) & nzchar(group$structure_hash)
      ])
      if (length(hashes) != 1L || !rr_valid_sha256(hashes[[1]])) {
        errors <- c(errors, paste0(
          label,
          ": usable readouts do not share one valid structure_hash"
        ))
        NA_character_
      } else {
        hashes[[1]]
      }
    } else {
      hashes <- unique(group$structure_hash[
        !is.na(group$structure_hash) & nzchar(group$structure_hash)
      ])
      if (length(hashes) > 0L) {
        errors <- c(errors, paste0(
          label,
          ": nonusable structure fit must not supply a structure_hash"
        ))
      }
      NA_character_
    }
    rows[[length(rows) + 1L]] <- data.frame(
      validation_type = "structure_reuse",
      panel_key = group$panel_cluster_key[[1]],
      representation = group$representation[[1]],
      method_count = 1L,
      methods = group$method_id[[1]],
      readout_rows = nrow(group),
      structure_fit_key = label,
      structure_hash = fit_state,
      fit_status = fit_status,
      identity_valid = TRUE,
      stringsAsFactors = FALSE
    )
  }
  rr_stop_identity_errors("Structure-reuse validation", errors)
  rr_bind_rows_fill(rows)
}

rr_validate_panel_identity <- function(data) {
  rr_bind_rows_fill(list(
    rr_validate_matched_panel_identity(data),
    rr_validate_challenge_identity(data),
    rr_validate_structure_reuse(data)
  ))
}

rr_metric_catalog <- function() {
  data.frame(
    estimand_id = c(
      "p_usable", "p_correct_given_usable", "p_usable_and_correct",
      "signed_dimension_error", "absolute_dimension_error",
      "adjusted_rand_index", "variation_information",
      "assignment_accuracy_conditional", "score_correlation_marginal",
      "score_nrmse_marginal", "completion_within_900s",
      "restricted_compute_900s", "runtime_median_successful",
      "runtime_iqr_successful"
    ),
    statistic = c(rep("mean", 12L), "median", "iqr"),
    stringsAsFactors = FALSE
  )
}

rr_statistic_function <- function(name) {
  switch(
    name,
    mean = function(value) mean(value),
    median = function(value) stats::median(value),
    iqr = function(value) stats::IQR(value, type = 8),
    rr_stop("Unknown result statistic: ", name)
  )
}

rr_worker_count <- function() {
  value <- suppressWarnings(as.integer(Sys.getenv("REVISION_JOB_WORKERS", "1")))
  if (!is.finite(value) || value < 1L) 1L else value
}

rr_parallel_lapply <- function(values, callback, workers = rr_worker_count()) {
  parallel_enabled <- length(values) > 1L && workers > 1L &&
    .Platform$OS.type != "windows"
  results <- if (parallel_enabled) {
    parallel::mclapply(
      values,
      callback,
      mc.cores = min(as.integer(workers), length(values)),
      mc.preschedule = TRUE,
      mc.set.seed = FALSE
    )
  } else {
    lapply(values, callback)
  }
  if (length(results) != length(values)) {
    rr_stop("Parallel aggregation returned an incomplete result list.")
  }
  failed <- vapply(results, function(result) {
    is.null(result) || inherits(result, "try-error")
  }, logical(1))
  if (any(failed)) {
    messages <- unique(trimws(as.character(results[failed])))
    rr_stop(
      "Parallel aggregation task failed: ",
      paste(messages, collapse = "; ")
    )
  }
  invalid <- !vapply(results, is.data.frame, logical(1))
  if (any(invalid)) {
    rr_stop("Aggregation callbacks must return one data frame per group.")
  }
  results
}

rr_seed_from_text <- function(value) {
  if (exists("revision_seed_from_key", mode = "function")) {
    return(revision_seed_from_key(paste0("revision_results|", value)))
  }
  integers <- utf8ToInt(enc2utf8(value))
  hash <- 0
  for (integer in integers) {
    hash <- (hash * 131 + integer) %% 2147483646
  }
  as.integer(hash + 1)
}

rr_complement_order_statistic <- function(sorted_values, removed_positions,
                                          rank) {
  total <- length(sorted_values)
  remaining <- total - length(removed_positions)
  rank <- as.integer(rank)
  if (rank < 1L || rank > remaining) {
    rr_stop("Complement order-statistic rank is out of range.")
  }
  lower <- rank
  upper <- min(total, rank + length(removed_positions))
  while (lower < upper) {
    middle <- as.integer(floor((lower + upper) / 2))
    retained <- middle - findInterval(middle, removed_positions)
    if (retained >= rank) {
      upper <- middle
    } else {
      lower <- middle + 1L
    }
  }
  sorted_values[[lower]]
}

rr_complement_type8_quantile <- function(sorted_values, removed_positions,
                                         probability) {
  remaining <- length(sorted_values) - length(removed_positions)
  if (remaining < 1L) {
    rr_stop("Deleting a cluster leaves no observations.")
  }
  position <- (remaining + 1 / 3) * probability + 1 / 3
  fuzz <- 4 * .Machine$double.eps
  lower_rank <- floor(position * (1 + fuzz))
  fraction <- position - lower_rank
  if (abs(fraction) < fuzz) {
    fraction <- 0
  }
  if (lower_rank < 1L) {
    return(rr_complement_order_statistic(
      sorted_values,
      removed_positions,
      1L
    ))
  }
  if (lower_rank >= remaining) {
    return(rr_complement_order_statistic(
      sorted_values,
      removed_positions,
      remaining
    ))
  }
  lower <- rr_complement_order_statistic(
    sorted_values,
    removed_positions,
    lower_rank
  )
  upper <- rr_complement_order_statistic(
    sorted_values,
    removed_positions,
    lower_rank + 1L
  )
  (1 - fraction) * lower + fraction * upper
}

rr_cluster_mcse <- function(values, clusters, statistic_name) {
  keep <- is.finite(values) & !is.na(clusters) & nzchar(clusters)
  values <- values[keep]
  clusters <- as.character(clusters[keep])
  unique_clusters <- unique(clusters)
  count <- length(unique_clusters)
  if (count < 2L) {
    return(NA_real_)
  }
  cluster_index <- match(clusters, unique_clusters)
  cluster_sizes <- tabulate(cluster_index, nbins = count)
  if (any(length(values) - cluster_sizes < 1L)) {
    return(NA_real_)
  }
  if (identical(statistic_name, "mean")) {
    cluster_sums <- as.numeric(rowsum(
      values,
      cluster_index,
      reorder = FALSE
    ))
    leave_one_out <- (
      sum(values) - cluster_sums
    ) / (length(values) - cluster_sizes)
  } else if (statistic_name %in% c("median", "iqr")) {
    ordering <- order(values, method = "radix")
    sorted_values <- values[ordering]
    sorted_clusters <- cluster_index[ordering]
    removed <- split(
      seq_along(sorted_values),
      factor(sorted_clusters, levels = seq_len(count))
    )
    leave_one_out <- vapply(removed, function(removed_positions) {
      if (identical(statistic_name, "median")) {
        return(rr_complement_type8_quantile(
          sorted_values,
          removed_positions,
          0.5
        ))
      }
      rr_complement_type8_quantile(
        sorted_values,
        removed_positions,
        0.75
      ) - rr_complement_type8_quantile(
        sorted_values,
        removed_positions,
        0.25
      )
    }, numeric(1))
  } else {
    rr_stop("Unknown result statistic: ", statistic_name)
  }
  center <- mean(leave_one_out)
  sqrt((count - 1) / count * sum((leave_one_out - center)^2))
}

rr_cluster_bootstrap <- function(values, clusters, statistic, reps = 2000L,
                                 seed = 1L) {
  if (reps < 2L) {
    rr_stop("Bootstrap reps must be at least two.")
  }
  keep <- is.finite(values) & !is.na(clusters) & nzchar(clusters)
  values <- values[keep]
  clusters <- as.character(clusters[keep])
  unique_clusters <- unique(clusters)
  if (length(unique_clusters) == 0L) {
    return(numeric())
  }
  by_cluster <- split(values, factor(clusters, levels = unique_clusters))
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  vapply(seq_len(as.integer(reps)), function(index) {
    sampled <- sample.int(length(unique_clusters), length(unique_clusters), replace = TRUE)
    statistic(unlist(by_cluster[sampled], use.names = FALSE))
  }, numeric(1))
}

rr_estimate_values <- function(values, clusters, statistic_name, key,
                               reps = 2000L) {
  keep <- is.finite(values) & !is.na(clusters) & nzchar(clusters)
  values <- values[keep]
  clusters <- as.character(clusters[keep])
  statistic <- rr_statistic_function(statistic_name)
  if (length(values) == 0L) {
    return(list(
      estimate = NA_real_, monte_carlo_se = NA_real_, bootstrap_se = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_, n_rows = 0L, n_panels = 0L,
      bootstrap_reps = as.integer(reps), bootstrap_seed = rr_seed_from_text(key)
    ))
  }
  seed <- rr_seed_from_text(key)
  bootstrap <- rr_cluster_bootstrap(values, clusters, statistic, reps, seed)
  interval <- stats::quantile(
    bootstrap,
    probs = c(0.025, 0.975),
    names = FALSE,
    type = 8,
    na.rm = TRUE
  )
  list(
    estimate = statistic(values),
    monte_carlo_se = rr_cluster_mcse(values, clusters, statistic_name),
    bootstrap_se = stats::sd(bootstrap),
    conf_low = interval[[1]],
    conf_high = interval[[2]],
    n_rows = length(values),
    n_panels = length(unique(clusters)),
    bootstrap_reps = as.integer(reps),
    bootstrap_seed = seed
  )
}

rr_empirical_metric_scale <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  scale <- stats::IQR(values, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-8) {
    scale <- stats::sd(values, na.rm = TRUE)
  }
  if (!is.finite(scale) || scale <= 1e-8) 1 else scale
}

rr_wasserstein_1d <- function(first, second) {
  first <- sort(first[is.finite(first)])
  second <- sort(second[is.finite(second)])
  if (length(first) == 0L || length(second) == 0L) {
    return(NA_real_)
  }
  probabilities <- seq(0, 1, length.out = max(length(first), length(second)))
  first_quantiles <- stats::quantile(
    first,
    probabilities,
    names = FALSE,
    type = 8
  )
  second_quantiles <- stats::quantile(
    second,
    probabilities,
    names = FALSE,
    type = 8
  )
  mean(abs(first_quantiles - second_quantiles))
}

rr_empirical_distribution_functionals <- function(data) {
  empirical <- suppressWarnings(as.numeric(data$empirical))
  synthetic <- suppressWarnings(as.numeric(data$synthetic))
  if (length(empirical) == 0L || length(empirical) != length(synthetic) ||
      any(!is.finite(empirical)) || any(!is.finite(synthetic))) {
    rr_stop("Empirical distribution functionals require aligned finite raw values.")
  }
  scale <- rr_empirical_metric_scale(empirical)
  signed_median <- (
    stats::median(synthetic) - stats::median(empirical)
  ) / scale
  absolute_median <- abs(signed_median)
  wasserstein <- rr_wasserstein_1d(empirical, synthetic) / scale
  c(
    signed_scaled_median_difference = signed_median,
    absolute_scaled_median_difference = absolute_median,
    scaled_wasserstein_distance = wasserstein,
    max_discrepancy = max(absolute_median, wasserstein)
  )
}

rr_cluster_frame_bootstrap <- function(data, clusters, statistic,
                                       reps = 2000L, seed = 1L) {
  if (reps < 2L) {
    rr_stop("Bootstrap reps must be at least two.")
  }
  clusters <- as.character(clusters)
  if (nrow(data) != length(clusters) ||
      any(is.na(clusters) | !nzchar(clusters))) {
    rr_stop("Clustered frame bootstrap requires one non-empty cluster per row.")
  }
  estimate <- statistic(data)
  statistic_names <- names(estimate)
  if (length(estimate) == 0L || is.null(statistic_names) ||
      any(!nzchar(statistic_names)) || any(!is.finite(estimate))) {
    rr_stop("Clustered frame statistic must return named finite values.")
  }
  unique_clusters <- unique(clusters)
  by_cluster <- split(
    seq_len(nrow(data)),
    factor(clusters, levels = unique_clusters)
  )
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  draws <- matrix(
    NA_real_,
    nrow = as.integer(reps),
    ncol = length(estimate),
    dimnames = list(NULL, statistic_names)
  )
  for (index in seq_len(as.integer(reps))) {
    sampled <- sample.int(
      length(unique_clusters),
      length(unique_clusters),
      replace = TRUE
    )
    row_index <- unlist(by_cluster[sampled], use.names = FALSE)
    draw <- statistic(data[row_index, , drop = FALSE])
    if (!identical(names(draw), statistic_names) || any(!is.finite(draw))) {
      rr_stop("Clustered frame statistic changed shape during bootstrap.")
    }
    draws[index, ] <- draw
  }
  draws
}

rr_cluster_frame_mcse <- function(data, clusters, statistic) {
  clusters <- as.character(clusters)
  if (nrow(data) != length(clusters) ||
      any(is.na(clusters) | !nzchar(clusters))) {
    rr_stop("Clustered frame MCSE requires one non-empty cluster per row.")
  }
  estimate <- statistic(data)
  unique_clusters <- unique(clusters)
  count <- length(unique_clusters)
  if (count < 2L) {
    return(setNames(rep(NA_real_, length(estimate)), names(estimate)))
  }
  leave_one_out <- vapply(unique_clusters, function(cluster) {
    result <- statistic(data[clusters != cluster, , drop = FALSE])
    if (!identical(names(result), names(estimate)) || any(!is.finite(result))) {
      rr_stop("Clustered frame statistic changed shape during jackknife.")
    }
    result
  }, estimate)
  if (is.null(dim(leave_one_out))) {
    leave_one_out <- matrix(leave_one_out, nrow = length(estimate))
  }
  center <- rowMeans(leave_one_out)
  result <- sqrt(
    (count - 1) / count * rowSums((leave_one_out - center)^2)
  )
  setNames(result, names(estimate))
}

rr_estimate_clustered_functionals <- function(data, clusters, statistic, key,
                                              reps = 2000L) {
  seed <- rr_seed_from_text(key)
  estimate <- statistic(data)
  bootstrap <- rr_cluster_frame_bootstrap(data, clusters, statistic, reps, seed)
  conf_low <- apply(bootstrap, 2L, function(values) {
    stats::quantile(values, 0.025, names = FALSE, type = 8)
  })
  conf_high <- apply(bootstrap, 2L, function(values) {
    stats::quantile(values, 0.975, names = FALSE, type = 8)
  })
  list(
    estimate = estimate,
    monte_carlo_se = rr_cluster_frame_mcse(data, clusters, statistic),
    bootstrap_se = apply(bootstrap, 2L, stats::sd),
    conf_low = conf_low,
    conf_high = conf_high,
    n_rows = nrow(data),
    n_panels = length(unique(clusters)),
    bootstrap_reps = as.integer(reps),
    bootstrap_seed = seed
  )
}

rr_group_indices <- function(data, columns) {
  split(seq_len(nrow(data)), rr_frame_key(data, columns), drop = TRUE)
}

rr_summary_descriptor <- function(group, level, group_columns) {
  fields <- c(
    "analysis_scope", "method_id", "community_algorithm", "scenario_family",
    "stability_class", "TimeP", "challenge_id", "representation"
  )
  values <- lapply(fields, function(field) {
    if (field %in% group_columns) {
      as.character(group[[field]][[1]])
    } else {
      NA_character_
    }
  })
  names(values) <- fields
  c(list(summary_level = level), values)
}

rr_build_group_estimates <- function(data, level, group_columns, reps = 2000L) {
  groups <- rr_group_indices(data, group_columns)
  catalog <- rr_metric_catalog()
  rows <- rr_parallel_lapply(names(groups), function(group_key) {
    group <- data[groups[[group_key]], , drop = FALSE]
    descriptor <- rr_summary_descriptor(group, level, group_columns)
    group_rows <- list()
    for (index in seq_len(nrow(catalog))) {
      estimand <- catalog$estimand_id[[index]]
      statistic <- catalog$statistic[[index]]
      result <- rr_estimate_values(
        group[[paste0("value_", estimand)]],
        group$panel_cluster_key,
        statistic,
        paste(level, group_key, estimand, sep = "|"),
        reps
      )
      group_rows[[length(group_rows) + 1L]] <- as.data.frame(c(
        descriptor,
        list(estimand_id = estimand, statistic = statistic),
        result
      ), stringsAsFactors = FALSE)
    }
    rr_bind_rows_fill(group_rows)
  })
  rr_bind_rows_fill(rows)
}

rr_build_method_estimands <- function(data, reps = 2000L) {
  base <- c("analysis_scope", "method_id", "community_algorithm")
  levels <- list(
    overall = base,
    stability = c(base, "stability_class"),
    horizon = c(base, "TimeP"),
    stability_horizon = c(base, "stability_class", "TimeP"),
    scenario_family = c(base, "scenario_family"),
    challenge = c(base, "challenge_id", "representation")
  )
  rows <- lapply(names(levels), function(level) {
    subset <- if (identical(level, "challenge")) {
      data[data$analysis_scope == "challenge", , drop = FALSE]
    } else {
      data
    }
    if (nrow(subset) == 0L) {
      return(data.frame())
    }
    rr_build_group_estimates(subset, level, levels[[level]], reps)
  })
  rr_bind_rows_fill(rows)
}

rr_pair_descriptor <- function(group, comparison_type, left_label, right_label) {
  data.frame(
    comparison_type = comparison_type,
    left_label = left_label,
    right_label = right_label,
    analysis_scope = as.character(group$analysis_scope.left[[1]]),
    community_algorithm = as.character(group$community_algorithm.left[[1]]),
    stability_class = as.character(group$stability_class.left[[1]]),
    TimeP = as.character(group$TimeP.left[[1]]),
    challenge_id = as.character(group$challenge_id.left[[1]]),
    representation = as.character(group$representation.left[[1]]),
    stringsAsFactors = FALSE
  )
}

rr_summarize_pair <- function(left, right, key_column, comparison_type,
                              left_label, right_label, reps = 2000L) {
  if (nrow(left) == 0L || nrow(right) == 0L) {
    return(data.frame())
  }
  if (anyDuplicated(left[[key_column]]) || anyDuplicated(right[[key_column]])) {
    rr_stop("Paired comparison keys must be unique for ", comparison_type, ".")
  }
  metrics <- rr_metric_catalog()
  value_columns <- paste0("value_", metrics$estimand_id)
  descriptor_columns <- c(
    "analysis_scope", "community_algorithm", "stability_class", "TimeP",
    "challenge_id", "representation"
  )
  left_columns <- unique(c(key_column, descriptor_columns, value_columns))
  right_columns <- unique(c(key_column, descriptor_columns, value_columns))
  paired <- merge(
    left[, left_columns, drop = FALSE],
    right[, right_columns, drop = FALSE],
    by = key_column,
    suffixes = c(".left", ".right"),
    sort = FALSE
  )
  if (nrow(paired) == 0L) {
    return(data.frame())
  }
  group_columns <- paste0(descriptor_columns, ".left")
  groups <- rr_group_indices(paired, group_columns)
  rows <- rr_parallel_lapply(names(groups), function(group_key) {
    group <- paired[groups[[group_key]], , drop = FALSE]
    descriptor <- rr_pair_descriptor(
      group,
      comparison_type,
      left_label,
      right_label
    )
    group_rows <- list()
    for (index in seq_len(nrow(metrics))) {
      estimand <- metrics$estimand_id[[index]]
      statistic <- metrics$statistic[[index]]
      left_value <- group[[paste0("value_", estimand, ".left")]]
      right_value <- group[[paste0("value_", estimand, ".right")]]
      difference <- left_value - right_value
      result <- rr_estimate_values(
        difference,
        group[[key_column]],
        statistic,
        paste(comparison_type, left_label, right_label, group_key, estimand, sep = "|"),
        reps
      )
      group_rows[[length(group_rows) + 1L]] <- cbind(
        descriptor,
        data.frame(
          estimand_id = estimand,
          statistic = statistic,
          estimate = result$estimate,
          monte_carlo_se = result$monte_carlo_se,
          bootstrap_se = result$bootstrap_se,
          conf_low = result$conf_low,
          conf_high = result$conf_high,
          n_rows = result$n_rows,
          n_panels = result$n_panels,
          bootstrap_reps = result$bootstrap_reps,
          bootstrap_seed = result$bootstrap_seed,
          stringsAsFactors = FALSE
        )
      )
    }
    rr_bind_rows_fill(group_rows)
  })
  rr_bind_rows_fill(rows)
}

rr_method_pairs <- function(methods) {
  if (length(methods) < 2L) {
    return(list())
  }
  pairs <- combn(sort(unique(methods)), 2L, simplify = FALSE)
  lapply(pairs, function(pair) {
    reference <- intersect(c("oracle_blocks", "static_EGA"), pair)
    if (length(reference) == 1L && !identical(pair[[2]], reference)) {
      pair <- rev(pair)
    }
    pair
  })
}

rr_build_paired_differences <- function(data, reps = 2000L) {
  rows <- list()
  pairs <- rr_method_pairs(unique(data$method_id))
  for (pair in pairs) {
    left_label <- pair[[1]]
    right_label <- pair[[2]]
    comparison <- if (identical(right_label, "oracle_blocks")) {
      "method_minus_oracle"
    } else if (identical(right_label, "static_EGA")) {
      "method_minus_static"
    } else {
      "method_difference"
    }
    rows[[length(rows) + 1L]] <- rr_summarize_pair(
      data[data$method_id == left_label, , drop = FALSE],
      data[data$method_id == right_label, , drop = FALSE],
      "method_pair_key",
      comparison,
      left_label,
      right_label,
      reps
    )
  }

  matched <- data[data$analysis_scope == "matched", , drop = FALSE]
  for (method in unique(matched$method_id)) {
    method_rows <- matched[matched$method_id == method, , drop = FALSE]
    rows[[length(rows) + 1L]] <- rr_summarize_pair(
      method_rows[method_rows$TimeP == 25, , drop = FALSE],
      method_rows[method_rows$TimeP == 100, , drop = FALSE],
      "horizon_pair_key",
      "horizon_t25_minus_t100",
      paste0(method, "_T25"),
      paste0(method, "_T100"),
      reps
    )
  }

  challenge <- data[data$analysis_scope == "challenge", , drop = FALSE]
  ideal <- challenge[challenge$challenge_id == "ideal_baseline", , drop = FALSE]
  variants <- unique(challenge$challenge_id[challenge$challenge_id != "ideal_baseline"])
  for (variant in variants) {
    representations <- unique(challenge$representation[challenge$challenge_id == variant])
    for (representation in representations) {
      variant_rows <- challenge[
        challenge$challenge_id == variant &
          challenge$representation == representation,
        ,
        drop = FALSE
      ]
      rows[[length(rows) + 1L]] <- rr_summarize_pair(
        variant_rows,
        ideal,
        "challenge_pair_key",
        "challenge_minus_ideal",
        paste(variant, representation, sep = ":"),
        "ideal_baseline",
        reps
      )
    }
  }
  softmax <- challenge[challenge$challenge_id == "softmax_bounded", , drop = FALSE]
  rows[[length(rows) + 1L]] <- rr_summarize_pair(
    softmax[softmax$representation == "softmax_clr", , drop = FALSE],
    softmax[softmax$representation == "softmax_raw", , drop = FALSE],
    "representation_pair_key",
    "softmax_clr_minus_raw",
    "softmax_clr",
    "softmax_raw",
    reps
  )
  rr_bind_rows_fill(rows)
}

rr_build_empirical_estimands <- function(long_results, reps = 2000L) {
  source_job <- rr_character_column(long_results, ".source_job", required = TRUE)
  empirical <- long_results[source_job == "empirical_cv", , drop = FALSE]
  if (nrow(empirical) == 0L) {
    rr_stop("Validated inputs contain no empirical_cv results.")
  }
  source_file <- basename(rr_character_column(empirical, ".source_file", required = TRUE))
  evaluations <- empirical[source_file == "results.csv", , drop = FALSE]
  contributions <- empirical[source_file == "clip_contributions.csv", , drop = FALSE]
  if (nrow(evaluations) == 0L || nrow(contributions) == 0L) {
    rr_stop("empirical_cv requires results.csv and clip_contributions.csv rows.")
  }
  stage <- tolower(rr_character_column(evaluations, c("analysis_stage", "stage")))
  heldout <- evaluations[grepl("heldout", stage, fixed = TRUE), , drop = FALSE]
  if (nrow(heldout) == 0L) {
    rr_stop("empirical_cv results contain no held-out evaluations.")
  }
  cluster <- rr_character_column(heldout, "panel_key")
  missing_cluster <- is.na(cluster) | !nzchar(cluster)
  if (any(missing_cluster)) {
    cluster[missing_cluster] <- rr_frame_key(
      heldout[missing_cluster, , drop = FALSE],
      c("fold", "heldout_rep")
    )
  }
  groups <- list(descriptive_overall = seq_len(nrow(heldout)))
  if ("fold" %in% names(heldout)) {
    fold_groups <- split(seq_len(nrow(heldout)), heldout$fold, drop = TRUE)
    names(fold_groups) <- paste0("descriptive_fold_", names(fold_groups))
    groups <- c(groups, fold_groups)
  }
  loss_columns <- c(
    continuous_loss = "empirical_heldout_loss",
    secondary_frozen_dynega_loss = "empirical_secondary_frozen_dynega_loss_descriptive"
  )
  rows <- list()
  for (loss_column in names(loss_columns)) {
    if (!loss_column %in% names(heldout)) {
      if (identical(loss_column, "continuous_loss")) {
        rr_stop("Held-out results require continuous_loss.")
      }
      next
    }
    loss <- suppressWarnings(as.numeric(heldout[[loss_column]]))
    for (level in names(groups)) {
      index <- groups[[level]]
      finite <- is.finite(loss[index])
      result <- list(
        estimate = if (any(finite)) mean(loss[index][finite]) else NA_real_,
        monte_carlo_se = NA_real_,
        bootstrap_se = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        n_rows = sum(finite),
        n_panels = length(unique(cluster[index][finite])),
        bootstrap_reps = 0L,
        bootstrap_seed = NA_integer_
      )
      rows[[length(rows) + 1L]] <- data.frame(
        summary_level = level,
        analysis_scope = "empirical_cv",
        method_id = NA_character_,
        community_algorithm = NA_character_,
        scenario_family = NA_character_,
        stability_class = NA_character_,
        TimeP = NA_character_,
        challenge_id = NA_character_,
        representation = NA_character_,
        empirical_family = "aggregate_evaluation_loss",
        empirical_metric = loss_column,
        empirical_scale = NA_real_,
        contribution = NA_character_,
        estimand_id = loss_columns[[loss_column]],
        statistic = "mean",
        estimate = result$estimate,
        monte_carlo_se = result$monte_carlo_se,
        bootstrap_se = result$bootstrap_se,
        conf_low = result$conf_low,
        conf_high = result$conf_high,
        n_rows = result$n_rows,
        n_panels = result$n_panels,
        bootstrap_reps = result$bootstrap_reps,
        bootstrap_seed = result$bootstrap_seed,
        stringsAsFactors = FALSE
      )
    }
  }

  required_columns <- c(
    "fold", "heldout_rep", "clip", "metric", "family", "empirical",
    "synthetic"
  )
  missing <- setdiff(required_columns, names(contributions))
  if (length(missing) > 0L) {
    rr_stop("clip_contributions.csv omits columns: ", paste(missing, collapse = ", "))
  }
  contributions$family <- gsub(
    "[^a-z0-9]+",
    "_",
    tolower(as.character(contributions$family))
  )
  contributions$family[contributions$family == "observable"] <- "observable_primary"
  contributions$family[
    contributions$family == "secondary_frozen_dynega"
  ] <- "secondary_frozen_summary"
  required_families <- c("observable_primary", "secondary_frozen_summary")
  missing_families <- setdiff(required_families, unique(contributions$family))
  if (length(missing_families) > 0L) {
    rr_stop(
      "clip_contributions.csv omits empirical families: ",
      paste(missing_families, collapse = ", ")
    )
  }
  contributions$metric <- as.character(contributions$metric)
  if (any(is.na(contributions$metric) | !nzchar(contributions$metric))) {
    rr_stop("Every empirical contribution requires a metric identifier.")
  }
  contributions$empirical <- suppressWarnings(as.numeric(contributions$empirical))
  contributions$synthetic <- suppressWarnings(as.numeric(contributions$synthetic))
  if (any(!is.finite(contributions$empirical)) ||
      any(!is.finite(contributions$synthetic))) {
    rr_stop("Every empirical contribution requires finite raw values.")
  }
  contributions$clip <- as.character(contributions$clip)
  if (any(is.na(contributions$clip) | !nzchar(contributions$clip))) {
    rr_stop("Every empirical contribution requires a clip cluster id.")
  }
  contribution_groups <- rr_group_indices(
    contributions,
    c("family", "metric")
  )
  contribution_rows <- rr_parallel_lapply(
    names(contribution_groups),
    function(group_key) {
      group <- contributions[contribution_groups[[group_key]], , drop = FALSE]
      family <- group$family[[1]]
      metric <- group$metric[[1]]
      empirical_scale <- rr_empirical_metric_scale(group$empirical)
      family_estimand <- if (identical(family, "observable_primary")) {
        "empirical_observable_primary_discrepancy"
      } else {
        "empirical_secondary_frozen_summary_discrepancy"
      }
      result <- rr_estimate_clustered_functionals(
        group,
        group$clip,
        rr_empirical_distribution_functionals,
        paste(family_estimand, metric, sep = "|"),
        reps
      )
      group_rows <- list()
      for (measure in names(result$estimate)) {
        group_rows[[length(group_rows) + 1L]] <- data.frame(
          summary_level = "heldout_clip_cluster",
          analysis_scope = "empirical_cv",
          method_id = NA_character_,
          community_algorithm = NA_character_,
          scenario_family = NA_character_,
          stability_class = NA_character_,
          TimeP = NA_character_,
          challenge_id = NA_character_,
          representation = NA_character_,
          empirical_family = family,
          empirical_metric = metric,
          empirical_scale = empirical_scale,
          contribution = measure,
          estimand_id = family_estimand,
          statistic = "distribution_functional",
          estimate = result$estimate[[measure]],
          monte_carlo_se = result$monte_carlo_se[[measure]],
          bootstrap_se = result$bootstrap_se[[measure]],
          conf_low = result$conf_low[[measure]],
          conf_high = result$conf_high[[measure]],
          n_rows = result$n_rows,
          n_panels = result$n_panels,
          bootstrap_reps = result$bootstrap_reps,
          bootstrap_seed = result$bootstrap_seed,
          stringsAsFactors = FALSE
        )
      }
      rr_bind_rows_fill(group_rows)
    }
  )
  rows <- c(rows, contribution_rows)
  rr_bind_rows_fill(rows)
}

rr_run_fixture_tests <- function(reps = 50L) {
  valid_group_results <- rr_parallel_lapply(
    list(1L, 2L),
    function(value) data.frame(value = value),
    workers = min(2L, rr_worker_count())
  )
  stopifnot(
    length(valid_group_results) == 2L,
    all(vapply(valid_group_results, is.data.frame, logical(1)))
  )
  null_group_result <- try(
    rr_parallel_lapply(list(1L), function(value) NULL, workers = 1L),
    silent = TRUE
  )
  non_frame_result <- try(
    rr_parallel_lapply(list(1L), identity, workers = 1L),
    silent = TRUE
  )
  stopifnot(
    inherits(null_group_result, "try-error"),
    inherits(non_frame_result, "try-error")
  )

  brute_cluster_mcse <- function(values, clusters, statistic_name) {
    keep <- is.finite(values) & !is.na(clusters) & nzchar(clusters)
    values <- values[keep]
    clusters <- as.character(clusters[keep])
    unique_clusters <- unique(clusters)
    if (length(unique_clusters) < 2L) {
      return(NA_real_)
    }
    statistic <- rr_statistic_function(statistic_name)
    leave_one_out <- vapply(unique_clusters, function(cluster) {
      statistic(values[clusters != cluster])
    }, numeric(1))
    center <- mean(leave_one_out)
    sqrt(
      (length(unique_clusters) - 1) / length(unique_clusters) *
        sum((leave_one_out - center)^2)
    )
  }
  set.seed(20260807)
  for (iteration in seq_len(30L)) {
    count <- sample(20:300, 1L)
    cluster_count <- sample(2:min(30L, count), 1L)
    clusters <- sample(sprintf("c%03d", seq_len(cluster_count)), count, TRUE)
    clusters[seq_len(cluster_count)] <- sprintf("c%03d", seq_len(cluster_count))
    values <- round(stats::rnorm(count), sample(0:4, 1L))
    for (statistic_name in c("mean", "median", "iqr")) {
      stopifnot(isTRUE(all.equal(
        rr_cluster_mcse(values, clusters, statistic_name),
        brute_cluster_mcse(values, clusters, statistic_name),
        tolerance = 1e-11
      )))
    }
  }
  filtered_values <- c(seq_len(20L), NA_real_, Inf, 99)
  filtered_clusters <- c(
    rep("large", 15L), paste0("s", seq_len(5L)), "large", "s1", ""
  )
  for (statistic_name in c("mean", "median", "iqr")) {
    stopifnot(isTRUE(all.equal(
      rr_cluster_mcse(filtered_values, filtered_clusters, statistic_name),
      brute_cluster_mcse(filtered_values, filtered_clusters, statistic_name),
      tolerance = 1e-11
    )))
  }
  stopifnot(is.na(rr_cluster_mcse(1:3, rep("one", 3L), "mean")))

  truth <- c(1, 1, 2, 2)
  named_truth <- "Y1=1;Y2=1;Y3=2;Y4=2"
  ordered_estimate <- "Y4.Ord1=8;Y3.Ord1=8;Y2.Ord1=7;Y1.Ord1=7"
  ord0_estimate <- "Y4.Ord0=8;Y3.Ord0=8;Y2.Ord0=7;Y1.Ord0=7"
  missing_metric_fixture <- data.frame(
    assignment_accuracy = NA_real_,
    adjusted_rand_index = NA_real_,
    variation_information = NA_real_,
    true_membership = named_truth,
    estimated_membership = ordered_estimate,
    stringsAsFactors = FALSE
  )
  stopifnot(
    identical(rr_adjusted_rand_index(truth, truth), 1),
    abs(rr_variation_information(truth, truth)) < 1e-12,
    identical(rr_adjusted_rand_index(named_truth, ordered_estimate), 1),
    identical(rr_adjusted_rand_index(named_truth, ord0_estimate), 1),
    abs(rr_variation_information(named_truth, ordered_estimate)) < 1e-12,
    identical(rr_assignment_accuracy(named_truth, ordered_estimate), 1),
    identical(
      rr_partition_metric(
        missing_metric_fixture,
        "assignment_accuracy",
        "true_membership",
        "estimated_membership",
        rr_assignment_accuracy
      )$value,
      1
    ),
    identical(
      rr_adjusted_rand_index("Y1=1;Y2=1;Y3=2;Y4=2", "Y4=8;Y3=8;Y2=7;Y1=7"),
      1
    )
  )
  collision <- try(
    rr_align_memberships(
      named_truth,
      "Y1=1;Y1.Ord1=1;Y3=2;Y4=2"
    ),
    silent = TRUE
  )
  incomplete <- try(
    rr_align_memberships(named_truth, "Y1=1;Y2=1;Y3=2"),
    silent = TRUE
  )
  stale_fixture <- missing_metric_fixture
  stale_fixture$assignment_accuracy <- 0
  forced <- rr_partition_metric(
    stale_fixture,
    "assignment_accuracy",
    "true_membership",
    "estimated_membership",
    rr_assignment_accuracy,
    force_recompute = TRUE
  )
  duplicate_fixture <- missing_metric_fixture[rep(1L, 5L), , drop = FALSE]
  metric_calls <- 0L
  cached <- rr_partition_metric(
    duplicate_fixture,
    "assignment_accuracy",
    "true_membership",
    "estimated_membership",
    function(truth_value, estimate_value) {
      metric_calls <<- metric_calls + 1L
      rr_assignment_accuracy(truth_value, estimate_value)
    }
  )
  stopifnot(
    inherits(collision, "try-error"),
    inherits(incomplete, "try-error"),
    identical(forced$value, 1),
    identical(forced$recomputed, TRUE),
    identical(metric_calls, 1L),
    identical(cached$value, rep(1, 5L)),
    all(cached$recomputed)
  )
  panels <- paste0("p", seq_len(4L))
  fixture <- expand.grid(
    method_id = c("method_a", "method_b"),
    panel_key = panels,
    stringsAsFactors = FALSE
  )
  fixture$.source_job <- "t25_dynega"
  fixture$.source_file <- "results.csv"
  fixture$.source_config_hash <- "000000000001"
  fixture$.source_expected_units <- nrow(fixture)
  fixture$community_algorithm <- "louvain"
  fixture$status <- "ok"
  fixture$status[fixture$method_id == "method_b" & fixture$panel_key == "p4"] <- "error"
  fixture$n_dim_true <- 2L
  fixture$n_dim_hat <- ifelse(fixture$status == "ok", 2L, NA)
  fixture$assignment_accuracy <- ifelse(fixture$status == "ok", 0.8, NA)
  fixture$adjusted_rand_index <- ifelse(fixture$status == "ok", 0.7, NA)
  fixture$variation_information <- ifelse(fixture$status == "ok", 0.2, NA)
  fixture$score_cor_common <- ifelse(fixture$status == "ok", 0.6, NA)
  fixture$score_nrmse_common <- ifelse(fixture$status == "ok", 0.5, NA)
  fixture$runtime_sec <- ifelse(fixture$status == "ok", 10, 900)
  fixture$scenario <- "old_low_single_stable_d2"
  fixture$scenario_family <- "old_low_single_stable"
  fixture$stability_class <- "asymptotically_stable"
  fixture$TimeP <- 25L
  fixture$N <- 50L
  fixture$NVAR <- 4L
  fixture$Error_sd <- 0.25
  fixture$Loadings <- 0.6
  fixture$Q <- 0.75
  fixture$rep <- match(fixture$panel_key, panels)
  fixture$dt <- 0.4
  fixture$seed_base <- fixture$rep + 100L
  fixture$panel_input_hash <- vapply(
    fixture$panel_key,
    function(panel) paste(rep(substr(panel, 2L, 2L), 64L), collapse = ""),
    character(1)
  )
  fixture$challenge_id <- NA_character_
  fixture$representation <- "linear_z"
  canonical <- rr_canonical_method_results(fixture)
  descriptor_fixture <- data.frame(
    analysis_scope = "challenge",
    method_id = "dynEGA",
    community_algorithm = "louvain",
    scenario_family = "old_low_single_stable",
    stability_class = "asymptotically_stable",
    TimeP = 100L,
    challenge_id = "softmax_bounded",
    representation = "clr",
    stringsAsFactors = FALSE
  )
  descriptor_fields <- c(
    "analysis_scope", "method_id", "community_algorithm", "scenario_family",
    "stability_class", "TimeP", "challenge_id", "representation"
  )
  descriptor_levels <- list(
    overall = c("analysis_scope", "method_id", "community_algorithm"),
    stability = c(
      "analysis_scope", "method_id", "community_algorithm", "stability_class"
    ),
    horizon = c(
      "analysis_scope", "method_id", "community_algorithm", "TimeP"
    ),
    stability_horizon = c(
      "analysis_scope", "method_id", "community_algorithm", "stability_class",
      "TimeP"
    ),
    scenario_family = c(
      "analysis_scope", "method_id", "community_algorithm", "scenario_family"
    ),
    challenge = c(
      "analysis_scope", "method_id", "community_algorithm", "challenge_id",
      "representation"
    )
  )
  for (summary_level in names(descriptor_levels)) {
    grouped_fields <- descriptor_levels[[summary_level]]
    descriptor <- rr_summary_descriptor(
      descriptor_fixture,
      summary_level,
      grouped_fields
    )
    stopifnot(
      identical(descriptor$summary_level, summary_level),
      all(vapply(grouped_fields, function(field) {
        identical(
          descriptor[[field]],
          as.character(descriptor_fixture[[field]][[1]])
        )
      }, logical(1))),
      all(vapply(setdiff(descriptor_fields, grouped_fields), function(field) {
        is.na(descriptor[[field]])
      }, logical(1)))
    )
  }
  estimates <- rr_build_method_estimands(canonical, reps)
  descriptor_fields <- c(
    "scenario_family", "stability_class", "TimeP", "challenge_id",
    "representation"
  )
  assert_descriptor_level <- function(level, present) {
    level_rows <- estimates[estimates$summary_level == level, , drop = FALSE]
    absent <- setdiff(descriptor_fields, present)
    stopifnot(
      nrow(level_rows) > 0L,
      all(vapply(level_rows[absent], function(value) all(is.na(value)), logical(1))),
      all(vapply(level_rows[present], function(value) all(!is.na(value)), logical(1)))
    )
  }
  assert_descriptor_level("overall", character())
  assert_descriptor_level("stability", "stability_class")
  assert_descriptor_level("horizon", "TimeP")
  assert_descriptor_level(
    "stability_horizon",
    c("stability_class", "TimeP")
  )
  assert_descriptor_level("scenario_family", "scenario_family")
  overall <- estimates[
    estimates$summary_level == "overall",
    ,
    drop = FALSE
  ]
  method_b <- overall[overall$method_id == "method_b", , drop = FALSE]
  estimate_for <- function(id) {
    as.numeric(method_b$estimate[method_b$estimand_id == id][[1]])
  }
  stopifnot(
    abs(estimate_for("p_usable") - 0.75) < 1e-12,
    abs(estimate_for("p_correct_given_usable") - 1) < 1e-12,
    abs(estimate_for("p_usable_and_correct") - 0.75) < 1e-12,
    abs(estimate_for("signed_dimension_error")) < 1e-12,
    abs(estimate_for("absolute_dimension_error")) < 1e-12,
    abs(estimate_for("adjusted_rand_index") - 0.7) < 1e-12,
    abs(estimate_for("variation_information") - 0.2) < 1e-12,
    abs(estimate_for("assignment_accuracy_conditional") - 0.8) < 1e-12,
    abs(estimate_for("score_correlation_marginal") - 0.45) < 1e-12,
    abs(estimate_for("score_nrmse_marginal") - 0.875) < 1e-12,
    abs(estimate_for("completion_within_900s") - 0.75) < 1e-12,
    abs(estimate_for("restricted_compute_900s") - 232.5) < 1e-12,
    abs(estimate_for("runtime_median_successful") - 10) < 1e-12,
    abs(estimate_for("runtime_iqr_successful")) < 1e-12
  )
  first <- rr_cluster_bootstrap(1:4, panels, mean, reps, 42L)
  second <- rr_cluster_bootstrap(1:4, panels, mean, reps, 42L)
  interval_values <- seq_len(20L)
  interval_clusters <- paste0("interval_", seq_along(interval_values))
  interval_key <- "fixture|interval|mean"
  interval_result <- rr_estimate_values(
    interval_values,
    interval_clusters,
    "mean",
    interval_key,
    reps
  )
  interval_bootstrap <- rr_cluster_bootstrap(
    interval_values,
    interval_clusters,
    mean,
    reps,
    interval_result$bootstrap_seed
  )
  expected_interval <- stats::quantile(
    interval_bootstrap,
    probs = c(0.025, 0.975),
    names = FALSE,
    type = 8
  )
  stopifnot(
    identical(first, second),
    abs(rr_cluster_mcse(1:4, panels, "mean") - stats::sd(1:4) / 2) < 1e-12,
    isTRUE(all.equal(
      c(interval_result$conf_low, interval_result$conf_high),
      expected_interval,
      tolerance = 1e-12
    )),
    interval_result$conf_low <= interval_result$estimate,
    interval_result$estimate <= interval_result$conf_high,
    identical(as.integer(formals(rr_cluster_bootstrap)$reps), 2000L)
  )
  differences <- rr_build_paired_differences(canonical, reps)
  usable_difference <- differences[
    differences$comparison_type == "method_difference" &
      differences$estimand_id == "p_usable",
    ,
    drop = FALSE
  ]
  stopifnot(
    nrow(differences) > 0L,
    nrow(usable_difference) == 1L,
    abs(usable_difference$estimate[[1]] - 0.25) < 1e-12
  )

  fixture_hash <- function(values) {
    identifiers <- match(values, unique(values))
    vapply(identifiers, function(identifier) {
      paste(rep(sprintf("%x", identifier %% 16L), 64L), collapse = "")
    }, character(1))
  }
  matched_grid <- expand.grid(
    method_id = c(
      "dynEGA", "graphicalVAR", "gimme", "static_EGA", "oracle_blocks"
    ),
    community_algorithm = c("louvain", "leiden", "walktrap"),
    panel_key = c("matched_t25", "matched_t100"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  matched_input <- fixture[rep(1L, nrow(matched_grid)), , drop = FALSE]
  matched_input$method_id <- matched_grid$method_id
  matched_input$community_algorithm <- matched_grid$community_algorithm
  matched_input$panel_key <- matched_grid$panel_key
  matched_input$.source_job <- ifelse(
    matched_grid$panel_key == "matched_t25",
    "t25_dynega",
    "t100_dynega"
  )
  matched_input$status <- "ok"
  matched_input$n_dim_hat <- 2L
  matched_input$assignment_accuracy <- 0.8
  matched_input$adjusted_rand_index <- 0.7
  matched_input$variation_information <- 0.2
  matched_input$score_cor_common <- 0.6
  matched_input$score_nrmse_common <- 0.5
  matched_input$runtime_sec <- 10
  matched_input$TimeP <- ifelse(
    matched_grid$panel_key == "matched_t25",
    25L,
    100L
  )
  matched_input$rep <- 1L
  matched_input$seed_base <- ifelse(matched_input$TimeP == 25L, 251L, 1001L)
  matched_input$panel_input_hash <- fixture_hash(matched_input$panel_key)
  matched_input$structure_fit_key <- paste(
    matched_input$panel_key,
    matched_input$method_id,
    sep = "|"
  )
  matched_input$structure_fit_status <- "ok"
  matched_input$structure_hash <- fixture_hash(matched_input$structure_fit_key)
  matched_canonical <- rr_canonical_method_results(matched_input)

  variant_representation <- data.frame(
    challenge_id = c(
      "ideal_baseline",
      "independent_initialization",
      "smooth_activity_gate",
      "cross_loadings",
      "residual_equicorrelation",
      "latent_shock_equicorrelation",
      "softmax_bounded",
      "softmax_bounded"
    ),
    representation = c(rep("linear_z", 6L), "softmax_raw", "softmax_clr"),
    stringsAsFactors = FALSE
  )
  challenge_grid <- expand.grid(
    method_id = c("dynEGA", "graphicalVAR", "static_EGA", "oracle_blocks"),
    community_algorithm = c("louvain", "leiden", "walktrap"),
    variant_index = seq_len(nrow(variant_representation)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  challenge_input <- fixture[rep(1L, nrow(challenge_grid)), , drop = FALSE]
  challenge_input$method_id <- challenge_grid$method_id
  challenge_input$community_algorithm <- challenge_grid$community_algorithm
  challenge_input$challenge_id <- variant_representation$challenge_id[
    challenge_grid$variant_index
  ]
  challenge_input$representation <- variant_representation$representation[
    challenge_grid$variant_index
  ]
  challenge_input$panel_key <- paste0("challenge_", challenge_input$challenge_id)
  challenge_input$.source_job <- "challenge_dynega"
  challenge_input$status <- "ok"
  challenge_input$n_dim_hat <- 2L
  challenge_input$assignment_accuracy <- 0.8
  challenge_input$adjusted_rand_index <- 0.7
  challenge_input$variation_information <- 0.2
  challenge_input$score_cor_common <- 0.6
  challenge_input$score_nrmse_common <- 0.5
  challenge_input$runtime_sec <- 10
  challenge_input$TimeP <- 100L
  challenge_input$N <- 100L
  challenge_input$rep <- 1L
  challenge_input$seed_base <- 777L
  challenge_panel_key <- paste(
    challenge_input$panel_key,
    challenge_input$representation,
    sep = "|"
  )
  challenge_input$panel_input_hash <- fixture_hash(challenge_panel_key)
  challenge_input$structure_fit_key <- paste(
    challenge_panel_key,
    challenge_input$method_id,
    sep = "|"
  )
  challenge_input$structure_fit_status <- "ok"
  challenge_input$structure_hash <- fixture_hash(challenge_input$structure_fit_key)
  challenge_input$component_hash_latent_standard_normals <- paste(
    rep("a", 64L), collapse = ""
  )
  challenge_input$component_hash_residual_standard_normals <- paste(
    rep("b", 64L), collapse = ""
  )
  challenge_input$component_hash_loading_basis <- paste(
    rep("c", 64L), collapse = ""
  )
  challenge_input$component_hash_activity_schedule <- paste(
    rep("d", 64L), collapse = ""
  )
  challenge_input$component_hash_linear_z <- paste(rep("e", 64L), collapse = "")
  challenge_canonical <- rr_canonical_method_results(challenge_input)
  challenge_descriptor <- rr_summary_descriptor(
    challenge_canonical[1L, , drop = FALSE],
    "challenge",
    c(
      "analysis_scope", "method_id", "community_algorithm", "challenge_id",
      "representation"
    )
  )
  stopifnot(
    !is.na(challenge_descriptor$challenge_id),
    nzchar(challenge_descriptor$challenge_id),
    !is.na(challenge_descriptor$representation),
    is.na(challenge_descriptor$scenario_family),
    is.na(challenge_descriptor$stability_class),
    is.na(challenge_descriptor$TimeP)
  )
  identity_input <- rr_bind_rows_fill(list(matched_canonical, challenge_canonical))
  identity <- rr_validate_panel_identity(identity_input)
  stopifnot(
    all(identity$identity_valid),
    sum(identity$validation_type == "matched_panel") == 2L,
    sum(identity$validation_type == "challenge_representation_panel") == 8L,
    sum(identity$validation_type == "challenge_common_pair") == 1L,
    sum(identity$validation_type == "structure_reuse") == 42L
  )
  matched_mismatch <- identity_input
  selected_mismatch <- matched_mismatch$analysis_scope == "matched" &
    matched_mismatch$panel_cluster_key == "matched_t25" &
    matched_mismatch$method_id == "dynEGA"
  matched_mismatch$seed_base[selected_mismatch] <- 999L
  matched_error <- tryCatch({
    rr_validate_matched_panel_identity(matched_mismatch)
    NULL
  }, error = function(error) error)
  challenge_mismatch <- challenge_canonical
  challenge_mismatch$component_hash_loading_basis[[1]] <- paste(
    rep("f", 64L), collapse = ""
  )
  challenge_error <- tryCatch({
    rr_validate_challenge_identity(challenge_mismatch)
    NULL
  }, error = function(error) error)
  structure_mismatch <- identity_input
  structure_mismatch$structure_hash[[1]] <- paste(rep("f", 64L), collapse = "")
  structure_error <- tryCatch({
    rr_validate_structure_reuse(structure_mismatch)
    NULL
  }, error = function(error) error)
  missing_readout_error <- tryCatch({
    rr_validate_challenge_identity(challenge_canonical[-1L, , drop = FALSE])
    NULL
  }, error = function(error) error)
  nonusable_fit <- matched_canonical[
    matched_canonical$structure_fit_key == matched_canonical$structure_fit_key[[1]],
    ,
    drop = FALSE
  ]
  nonusable_fit$usable <- FALSE
  nonusable_fit$status <- "error"
  nonusable_fit$structure_fit_status <- "error"
  nonusable_fit$structure_hash <- NA_character_
  stopifnot(nrow(rr_validate_structure_reuse(nonusable_fit)) == 1L)
  inconsistent_fit <- nonusable_fit
  inconsistent_fit$structure_fit_status[[1]] <- "timeout"
  inconsistent_fit_error <- tryCatch({
    rr_validate_structure_reuse(inconsistent_fit)
    NULL
  }, error = function(error) error)
  stopifnot(
    inherits(matched_error, "error"),
    inherits(challenge_error, "error"),
    inherits(structure_error, "error"),
    inherits(missing_readout_error, "error"),
    inherits(inconsistent_fit_error, "error")
  )

  empirical_results <- data.frame(
    .source_job = "empirical_cv",
    .source_file = "results.csv",
    stage = "heldout",
    panel_key = paste0("e", seq_len(4L)),
    continuous_loss = 1:4,
    secondary_frozen_dynega_loss = 2:5,
    fold = c(1L, 1L, 2L, 2L),
    stringsAsFactors = FALSE
  )
  empirical_contributions <- expand.grid(
    .source_job = "empirical_cv",
    .source_file = "clip_contributions.csv",
    fold = 1:2,
    heldout_rep = 1:2,
    clip = paste0("clip", 1:2),
    metric = "mean",
    family = c("observable_primary", "secondary_frozen_summary"),
    scale = 1,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  empirical_contributions$empirical <- rep(seq_len(8L), times = 2L)
  empirical_contributions$synthetic <- empirical_contributions$empirical + 0.5
  empirical_contributions$signed_discrepancy <- 99
  empirical_contributions$absolute_discrepancy <- 99
  empirical <- rr_bind_rows_fill(list(empirical_results, empirical_contributions))
  empirical_result <- rr_build_empirical_estimands(empirical, reps)
  observable <- empirical_result[
    empirical_result$empirical_family == "observable_primary",
    ,
    drop = FALSE
  ]
  observable_estimate <- setNames(observable$estimate, observable$contribution)
  stopifnot(
    abs(empirical_result$estimate[
      empirical_result$summary_level == "descriptive_overall" &
        empirical_result$estimand_id == "empirical_heldout_loss"
    ] - 2.5) < 1e-12,
    nrow(observable) == 4L,
    abs(observable_estimate[["signed_scaled_median_difference"]] - 1 / 7) < 1e-12,
    abs(observable_estimate[["absolute_scaled_median_difference"]] - 1 / 7) < 1e-12,
    abs(observable_estimate[["scaled_wasserstein_distance"]] - 1 / 7) < 1e-12,
    abs(observable_estimate[["max_discrepancy"]] - 1 / 7) < 1e-12,
    all(abs(observable$empirical_scale - 3.5) < 1e-12),
    all(observable$n_panels == 2L),
    all(observable$bootstrap_reps == reps),
    all(observable$estimate < 1)
  )
  fixture_jobs <- list(aggregate_results = list(
    required_dependencies = c("empirical_cv", "t100_reuse_audit"),
    input_alternatives = list(list(
      method = "graphicalvar",
      audit_job = "t100_reuse_audit",
      artifact = "reuse_audit.json",
      fallback_job = "t100_graphicalvar_fallback"
    ))
  ))
  aggregate <- rr_aggregate_job(fixture_jobs)
  stopifnot(
    !"t100_graphicalvar_fallback" %in% unlist(
      aggregate$required_dependencies,
      use.names = FALSE
    ),
    !rr_reuse_has_panel_hash_equivalence(list(checks = list())),
    rr_reuse_has_panel_hash_equivalence(list(
      checks = list(panel_input_hash_equivalence = TRUE)
    ))
  )
  publish_root <- tempfile("revision-publish-fixture-")
  staging <- file.path(publish_root, "staging")
  destination <- file.path(publish_root, "output")
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(publish_root, recursive = TRUE, force = TRUE), add = TRUE)
  artifact_names <- paste0("artifact_", seq_len(6L), ".csv")
  for (name in artifact_names) {
    writeLines(paste("value", name, sep = ","), file.path(staging, name))
  }
  first_publish <- rr_publish_staged(staging, destination, artifact_names)
  resumed_publish <- rr_publish_staged(staging, destination, artifact_names)
  stopifnot(identical(first_publish, resumed_publish))
  writeLines("changed", file.path(staging, artifact_names[[1]]))
  mismatch_error <- tryCatch({
    rr_publish_staged(staging, destination, artifact_names)
    NULL
  }, error = function(error) error)
  stopifnot(inherits(mismatch_error, "error"))
  invisible(TRUE)
}

rr_write_csv <- function(value, path) {
  utils::write.csv(value, path, row.names = FALSE, na = "")
  if (!file.exists(path) || file.info(path)$size[[1]] < 1L) {
    rr_stop("Failed to write staged artifact: ", path)
  }
  invisible(path)
}

rr_resolve_output_directory <- function(configuration, repo_root) {
  output_directory <- Sys.getenv("REVISION_JOB_OUTPUT_DIR", unset = "")
  config_hash <- Sys.getenv("REVISION_JOB_CONFIG_HASH", unset = "")
  job_id <- Sys.getenv("REVISION_JOB_ID", unset = "aggregate_results")
  if (!identical(job_id, "aggregate_results")) {
    rr_stop("REVISION_JOB_ID must be aggregate_results.")
  }
  if (!nzchar(output_directory) || !nzchar(config_hash)) {
    rr_stop(
      "Real aggregation requires REVISION_JOB_OUTPUT_DIR and ",
      "REVISION_JOB_CONFIG_HASH from the orchestrator."
    )
  }
  hash_length <- as.integer(configuration$defaults$config_hash_length)
  if (!grepl(paste0("^[0-9a-f]{", hash_length, "}$"), config_hash)) {
    rr_stop("REVISION_JOB_CONFIG_HASH is not a valid locked config hash.")
  }
  output_directory <- rr_absolute_path(output_directory, repo_root)
  expected_parent <- rr_absolute_path(
    file.path(configuration$defaults$output_root, "aggregate_results"),
    repo_root
  )
  if (!identical(dirname(output_directory), expected_parent) ||
      !identical(basename(output_directory), config_hash)) {
    rr_stop("Aggregate output directory differs from the locked revision_v2 path.")
  }
  output_directory
}

rr_publish_staged <- function(staging_directory, output_directory, file_names) {
  if (!dir.exists(output_directory) &&
      !dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)) {
    rr_stop("Cannot create aggregate output directory: ", output_directory)
  }
  destinations <- file.path(output_directory, file_names)
  exists <- file.exists(destinations)
  if (any(exists) && !all(exists)) {
    rr_stop(
      "Aggregate resume found a partial published artifact set: ",
      paste(destinations[exists], collapse = ", ")
    )
  }
  if (all(exists)) {
    staged_hashes <- vapply(
      file.path(staging_directory, file_names),
      rr_sha256,
      character(1)
    )
    destination_hashes <- vapply(destinations, rr_sha256, character(1))
    if (!identical(unname(staged_hashes), unname(destination_hashes))) {
      mismatched <- file_names[staged_hashes != destination_hashes]
      rr_stop(
        "Aggregate resume artifacts differ from the rebuilt staged outputs: ",
        paste(mismatched, collapse = ", ")
      )
    }
    return(invisible(destinations))
  }
  temporary <- file.path(
    output_directory,
    paste0(".", file_names, ".", Sys.getpid(), ".tmp")
  )
  published <- character()
  on.exit({
    unlink(temporary[file.exists(temporary)])
    if (length(published) != length(file_names)) {
      unlink(published[file.exists(published)])
    }
  }, add = TRUE)
  copied <- file.copy(file.path(staging_directory, file_names), temporary, overwrite = FALSE)
  if (!all(copied)) {
    rr_stop("Failed to stage aggregate artifacts in the locked output directory.")
  }
  for (index in seq_along(file_names)) {
    if (!file.rename(temporary[[index]], destinations[[index]])) {
      rr_stop("Failed to publish aggregate artifact: ", destinations[[index]])
    }
    published <- c(published, destinations[[index]])
  }
  invisible(destinations)
}

rr_dry_run_plan <- function(config_path, specification, jobs) {
  aggregate <- rr_aggregate_job(jobs)
  list(
    dry_run = TRUE,
    action = "validate_then_aggregate_revision_v2_results",
    config = config_path,
    required_upstream_jobs = unlist(
      aggregate$required_dependencies,
      use.names = FALSE
    ),
    conditional_input_alternatives = lapply(
      aggregate$input_alternatives,
      function(alternative) list(
        method = alternative$method,
        reuse_audit = alternative$audit_job,
        accepted_reuse_skips_fallback = alternative$fallback_job,
        rejected_reuse_requires_fallback = alternative$fallback_job
      )
    ),
    required_success_fields = c(
      "job_id",
      "config_hash",
      "expected_units",
      "observed_unique_units",
      "prospective_manifest_valid",
      "run_manifest_valid",
      "output_hashes"
    ),
    validation_rules = c(
      "expected_units equals configured expected_units",
      "observed_unique_units equals expected_units",
      "prospective_manifest_valid and run_manifest_valid are true",
      "every output hash is a verified SHA-256",
      "each selected job has one authoritative hashed non-empty result CSV",
      "empirical_cv supplies hashed results.csv and clip_contributions.csv",
      "accepted reusable T100 results match the SHA-256 recorded by reuse_audit",
      "reusable T100 results require audited panel-input-hash equivalence",
      "rejected or hash-unverified reusable T100 results require the completed fallback",
      "matched panels contain five methods and three unique algorithm readouts per method",
      "challenge representation panels contain four methods and three unique readouts per method",
      "challenge variants share locked component hashes and softmax panels share ideal linear_z",
      "each structure_fit_key has three readouts sharing one valid structure_hash"
    ),
    estimand_count = nrow(specification),
    estimand_ids = specification$estimand_id,
    output_artifacts = c(
      "revision_estimand_spec.csv",
      "upstream_artifact_manifest.csv",
      "revision_results_long.csv",
      "identity_validation.csv",
      "partition_metric_recomputation_audit.csv",
      "revision_estimand_results.csv",
      "revision_paired_differences.csv"
    ),
    panel_cluster_bootstrap_reps = 2000L,
    interval = paste(
      "deterministic 2.5% and 97.5% panel-cluster bootstrap quantiles;",
      " empirical clip draws recompute scale, medians, and Wasserstein distance",
      sep = ""
    ),
    monte_carlo_se = "delete-one-panel or delete-one-clip cluster jackknife",
    estimator_calls = 0L,
    writes_publish_artifacts = FALSE
  )
}

rr_main <- function() {
  started_elapsed <- rr_progress("starting")
  stage_elapsed <- started_elapsed
  arguments <- rr_parse_arguments(commandArgs(trailingOnly = TRUE))
  repo_root <- rr_find_repo_root()
  specification_path <- file.path(
    repo_root,
    "scripts",
    "simulations",
    "revision_spec.R"
  )
  source(specification_path, local = globalenv())
  config_path <- rr_absolute_path(arguments$config, repo_root, must_work = TRUE)
  configuration <- rr_read_json(config_path)
  jobs <- rr_validate_configuration(configuration)
  specification <- revision_estimand_specification()
  rr_validate_estimands(specification)
  stage_elapsed <- rr_progress("configuration and estimands validated", stage_elapsed)

  if (isTRUE(arguments$dry_run)) {
    cat(rr_to_json(rr_dry_run_plan(config_path, specification, jobs)), "\n", sep = "")
    return(invisible(TRUE))
  }

  input_directories <- rr_resolve_input_directories(
    configuration,
    jobs,
    repo_root
  )
  validated <- rr_validate_selected_inputs(
    input_directories,
    jobs,
    configuration,
    repo_root
  )
  stage_elapsed <- rr_progress("retained inputs validated", stage_elapsed)
  output_directory <- rr_resolve_output_directory(configuration, repo_root)
  long_results <- rr_load_long_results(validated$csv_files)
  stage_elapsed <- rr_progress("retained result rows loaded", stage_elapsed)
  method_results <- rr_canonical_method_results(long_results)
  stage_elapsed <- rr_progress("partition metrics canonicalized", stage_elapsed)
  partition_metric_audit <- attr(
    method_results,
    "partition_metric_recomputation_audit"
  )
  if (!is.data.frame(partition_metric_audit) || nrow(partition_metric_audit) == 0L) {
    rr_stop("Partition-metric recomputation audit is missing.")
  }
  identity_validation <- rr_validate_panel_identity(method_results)
  stage_elapsed <- rr_progress("panel and structure identities validated", stage_elapsed)
  estimand_results <- rr_bind_rows_fill(list(
    rr_build_empirical_estimands(long_results, reps = 2000L),
    rr_build_method_estimands(method_results, reps = 2000L)
  ))
  stage_elapsed <- rr_progress("estimands aggregated", stage_elapsed)
  paired_differences <- rr_build_paired_differences(
    method_results,
    reps = 2000L
  )
  stage_elapsed <- rr_progress("paired differences aggregated", stage_elapsed)

  staging_directory <- tempfile("revision-aggregate-")
  if (!dir.create(staging_directory, recursive = TRUE, showWarnings = FALSE)) {
    rr_stop("Cannot create aggregation staging directory.")
  }
  on.exit(unlink(staging_directory, recursive = TRUE, force = TRUE), add = TRUE)
  output_names <- c(
    "revision_estimand_spec.csv",
    "upstream_artifact_manifest.csv",
    "revision_results_long.csv",
    "identity_validation.csv",
    "partition_metric_recomputation_audit.csv",
    "revision_estimand_results.csv",
    "revision_paired_differences.csv"
  )
  rr_write_csv(specification, file.path(staging_directory, output_names[[1]]))
  rr_write_csv(validated$artifacts, file.path(staging_directory, output_names[[2]]))
  rr_write_csv(long_results, file.path(staging_directory, output_names[[3]]))
  rr_write_csv(identity_validation, file.path(staging_directory, output_names[[4]]))
  rr_write_csv(partition_metric_audit, file.path(staging_directory, output_names[[5]]))
  rr_write_csv(estimand_results, file.path(staging_directory, output_names[[6]]))
  rr_write_csv(paired_differences, file.path(staging_directory, output_names[[7]]))
  result_rows <- nrow(long_results)
  published <- rr_publish_staged(staging_directory, output_directory, output_names)
  stage_elapsed <- rr_progress("aggregate artifacts published", stage_elapsed)
  summary <- list(
    dry_run = FALSE,
    config_hash = basename(output_directory),
    upstream_jobs = length(validated$selected_jobs),
    validated_result_csvs = length(validated$csv_files),
    aggregated_rows = result_rows,
    identity_validated_panels = nrow(identity_validation),
    partition_metric_audit_rows = nrow(partition_metric_audit),
    estimands = nrow(specification),
    estimand_result_rows = nrow(estimand_results),
    paired_difference_rows = nrow(paired_differences),
    panel_cluster_bootstrap_reps = 2000L,
    elapsed_seconds = round(stage_elapsed - started_elapsed, 3),
    output_artifacts = basename(published)
  )
  cat(rr_to_json(summary), "\n", sep = "")
  invisible(TRUE)
}

if (!identical(Sys.getenv("REVISION_RESULTS_NO_MAIN", unset = "0"), "1")) {
  rr_main()
}
