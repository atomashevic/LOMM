#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("RUN_METHOD_BENCHMARK"))) {
  Sys.setenv(RUN_METHOD_BENCHMARK = "1")
}
Sys.setenv(BENCHMARK_METHODS = "gimme")
if (!nzchar(Sys.getenv("BENCHMARK_OUT_DIR"))) {
  Sys.setenv(BENCHMARK_OUT_DIR = file.path("simulation_outputs", "revision_gimme"))
}
if (!nzchar(Sys.getenv("BENCHMARK_GIMME_WORKERS"))) {
  Sys.setenv(BENCHMARK_GIMME_WORKERS = "1")
}
if (!nzchar(Sys.getenv("BENCHMARK_GIMME_COMMUNITY_ALGORITHMS"))) {
  Sys.setenv(BENCHMARK_GIMME_COMMUNITY_ALGORITHMS = "louvain")
}

source(file.path("scripts", "simulations", "run_method_simulation.R"))
