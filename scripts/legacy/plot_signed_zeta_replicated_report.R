#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("SIGNED_ZETA_BENCHMARK_DIR"))) {
  Sys.setenv(SIGNED_ZETA_BENCHMARK_DIR = "benchmark_outputs_signed_zeta_reps5")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_REPORT_OUT_DIR"))) {
  Sys.setenv(SIGNED_ZETA_REPORT_OUT_DIR = "comparison_signed_zeta_reps5")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_REPORT_TITLE"))) {
  Sys.setenv(SIGNED_ZETA_REPORT_TITLE = "Signed-Zeta Replicated Benchmark Report")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_PLOT_TITLE_PREFIX"))) {
  Sys.setenv(SIGNED_ZETA_PLOT_TITLE_PREFIX = "Signed-zeta reps5")
}
if (!nzchar(Sys.getenv("SIGNED_ZETA_REPORT_FILE"))) {
  Sys.setenv(SIGNED_ZETA_REPORT_FILE = "signed_zeta_reps5_report.md")
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_signed_zeta_replicated_report.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "plot_signed_zeta_smoke_report.R"))
