# Repository Guidelines

## Project Structure & Module Organization
This repository is a flat R prototype for DLO and dynEGA simulation work rather than a packaged R library. Core modeling and estimation helpers live in `extract.r`. Scenario orchestration lives in `run_sim_suite.R`, which writes benchmark outputs to `sim_outputs/` or `sim_outputs_smoke/`. Visualization is handled by `viz.R`. Manual sanity checks live in `test_dynEGA_inspect.R`. `sim.R` is an exploratory script for direct simulation and plotting. Generated artifacts such as `*.rds`, `*.png`, and files under `sim_outputs*/` should only change when you intentionally rerun the pipeline.

## Build, Test, and Development Commands
- `Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"` loads the core functions without triggering the expensive calibration block.
- `RUN_SIM_SUITE_SMOKE=1 Rscript run_sim_suite.R` runs the reduced smoke benchmark and writes outputs to `sim_outputs_smoke/`.
- `RUN_SIM_SUITE=1 Rscript run_sim_suite.R` runs the full simulation suite and saves CSV, RDS, and plot outputs to `sim_outputs/`.
- `Rscript viz.R` regenerates filtered comparison plots from `sim_outputs/sim_summary.csv`.
- `Rscript test_dynEGA_inspect.R` runs the quickest dynEGA sanity check and refreshes `dynEGA_small.rds`.

## Coding Style & Naming Conventions
Follow the existing base R and tidyverse style already used here. Use `snake_case` for functions and objects such as `run_sim_suite` and `summary_df`. Keep indentation to two spaces inside wrapped calls and function bodies, and align long named-argument lists vertically for readability. Reserve ALL_CAPS for script-level thresholds such as `RMSE_THRESHOLD`. Add comments only where numerical choices, calibration logic, or estimator behavior are not obvious.

## Testing Guidelines
There is no `testthat` suite yet; testing is script-driven. Before merging estimator, integrator, or plotting changes, run `Rscript test_dynEGA_inspect.R` and `RUN_SIM_SUITE_SMOKE=1 Rscript run_sim_suite.R`. If summary graphics depend on the change, rerun `Rscript viz.R`. Keep new checks lightweight and name any new validation scripts with a `test_*.R` prefix.

## Commit & Pull Request Guidelines
This workspace snapshot does not include `.git`, so repository-specific commit conventions cannot be inferred from local history. Use short imperative commit subjects such as `Add smoke check for rk4 recovery`. Keep each commit scoped to one logical change. In pull requests, describe the modeling or plotting change, list the rerun commands used for validation, call out any regenerated files under `sim_outputs*/`, and include updated plot images when visual outputs change.
