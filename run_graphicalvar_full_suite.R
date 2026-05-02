#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("RUN_METHOD_BENCHMARK"))) {
  Sys.setenv(RUN_METHOD_BENCHMARK = "1")
}
Sys.setenv(BENCHMARK_METHODS = "graphicalVAR")
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = "benchmark_outputs_revision_graphicalvar")
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_WORKERS"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_WORKERS = "1")
}

source("run_method_benchmark.R")
