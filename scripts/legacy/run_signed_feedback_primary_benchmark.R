#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = "benchmark_outputs_signed_feedback_primary")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_REPS"))) {
  Sys.setenv(SIGNED_ZETA_REPS = "20")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_SCENARIO_FAMILY"))) {
  Sys.setenv(SIGNED_ZETA_SCENARIO_FAMILY = "signed_feedback_primary")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_DRY_RUN"))) {
  Sys.setenv(SIGNED_ZETA_DRY_RUN = "1")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_USE_FIXED_GRAPHICALVAR"))) {
  Sys.setenv(SIGNED_ZETA_USE_FIXED_GRAPHICALVAR = "0")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC = "900")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_WORKERS"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_WORKERS = "2")
}
if (!nzchar(Sys.getenv("BENCHMARK_WORKERS"))) {
  Sys.setenv(BENCHMARK_WORKERS = "4")
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("run_signed_feedback_primary_benchmark.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "run_signed_zeta_smoke_benchmark.R"))
main()
