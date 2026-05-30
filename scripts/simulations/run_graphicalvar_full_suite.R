#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("RUN_METHOD_BENCHMARK"))) {
  Sys.setenv(RUN_METHOD_BENCHMARK = "1")
}
Sys.setenv(BENCHMARK_METHODS = "graphicalVAR")
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = file.path("simulation_outputs", "revision_graphicalvar"))
}
if (!nzchar(Sys.getenv("BENCHMARK_GRAPHICALVAR_WORKERS"))) {
  Sys.setenv(BENCHMARK_GRAPHICALVAR_WORKERS = "1")
}

source(file.path("scripts", "simulations", "run_method_simulation.R"))
