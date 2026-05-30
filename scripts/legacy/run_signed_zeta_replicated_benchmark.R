#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = "benchmark_outputs_signed_zeta_reps5")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_REPS"))) {
  Sys.setenv(SIGNED_ZETA_REPS = "5")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_SCENARIO_FAMILY"))) {
  Sys.setenv(SIGNED_ZETA_SCENARIO_FAMILY = "signed_zeta_reps5")
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("run_signed_zeta_replicated_benchmark.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "run_signed_zeta_smoke_benchmark.R"))
main()
