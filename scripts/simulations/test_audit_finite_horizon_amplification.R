#!/usr/bin/env Rscript

test_script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(argument) != 1L) stop("Cannot locate test script.", call. = FALSE)
  normalizePath(sub("^--file=", "", argument), mustWork = TRUE)
}

script_path <- test_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
audit_path <- file.path(
  repo_root, "scripts", "simulations", "audit_finite_horizon_amplification.R"
)
source(audit_path, local = .GlobalEnv)

fixture <- data.frame(
  scenario = rep(c("oai_ranked_persistence", "oai_broad_geometry"), each = 4L),
  dimensions = rep(c(2L, 3L), each = 2L, times = 2L),
  time_points = rep(c(100L, 300L), times = 4L),
  horizon = rep(c(39.6, 74.75), times = 4L),
  replication = 1L,
  max_abs_activity_scaled_latent = seq(2, 9),
  activity_scaled_latent_iqr = seq(0.2, 0.9, by = 0.1),
  median_channel_iqr = seq(0.3, 1.0, by = 0.1),
  final_tenth_energy_fraction = seq(0.1, 0.8, by = 0.1),
  nonfinite_latent_values = 0L,
  nonfinite_observed_values = 0L,
  stringsAsFactors = FALSE
)
summary <- audit_summarize(fixture, 1L)
stopifnot(
  nrow(summary) == 8L,
  identical(sort(unique(summary$Scenario)), c(
    "Ranked finite-horizon amplification",
    "Switching finite-horizon amplification"
  )),
  all(summary$Replications == 1L),
  all(summary$`Nonfinite latent values` == 0L),
  all(summary$`Nonfinite observed values` == 0L),
  identical(audit_parse_args(c("--reps", "3"))$reps, 3L)
)

cat("Finite-horizon amplification audit tests: PASS\n")
