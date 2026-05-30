#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("RUN_METHOD_BENCHMARK"))) {
  Sys.setenv(RUN_METHOD_BENCHMARK = "1")
}
Sys.setenv(BENCHMARK_METHODS = "dynEGA")
if (!nzchar(Sys.getenv("BENCHMARK_VARIANT_SMOKE"))) {
  Sys.setenv(BENCHMARK_VARIANT_SMOKE = "0")
}
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = file.path("simulation_outputs", "revision_dynega"))
}

source(file.path("scripts", "simulations", "run_method_simulation.R"))
