#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Usage: Rscript scripts/run_external_structure_fit.R <method> <input.rds> <output.rds>",
    call. = FALSE
  )
}

method <- args[[1]]
input_path <- args[[2]]
output_path <- args[[3]]

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_candidate <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  ""
}
script_path <- if (nzchar(script_candidate) && !identical(script_candidate, "-")) {
  normalizePath(script_candidate, mustWork = TRUE)
} else {
  normalizePath(file.path("scripts", "run_external_structure_fit.R"), mustWork = TRUE)
}
repo_root <- dirname(dirname(script_path))

old_flags <- Sys.getenv(c("RUN_METHOD_BENCHMARK", "RUN_METHOD_BENCHMARK_SMOKE"))
on.exit({
  for (flag_name in names(old_flags)) {
    if (nzchar(old_flags[[flag_name]])) {
      do.call(Sys.setenv, stats::setNames(as.list(old_flags[[flag_name]]), flag_name))
    } else {
      Sys.unsetenv(flag_name)
    }
  }
}, add = TRUE)
Sys.setenv(RUN_METHOD_BENCHMARK = "0", RUN_METHOD_BENCHMARK_SMOKE = "0")

source(file.path(repo_root, "scripts", "simulations", "run_method_simulation.R"))

payload <- readRDS(input_path)

result <- tryCatch(
  {
    if (identical(method, "gimme")) {
      value <- fit_gimme_structure(
        sim_data = payload$sim_data,
        cfg = payload$cfg,
        scenario_label = payload$scenario_label
      )
    } else if (identical(method, "graphicalVAR")) {
      value <- fit_graphical_var_structure(
        sim_data = payload$sim_data,
        cfg = payload$cfg
      )
    } else {
      stop("Unsupported external structure-fit method: ", method, call. = FALSE)
    }
    list(ok = TRUE, value = value)
  },
  error = function(e) {
    list(ok = FALSE, message = conditionMessage(e))
  }
)

saveRDS(result, output_path)
