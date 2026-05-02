# DLO Revision Benchmark

This repository contains the cleaned revision workspace for the Behavior Research Methods revise-and-resubmit on dynamic latent oscillator simulation and dimensionality recovery for video emotion time series.

The revision source of truth is the lab note `~/lab/methods/dlo-paper-rr.md`. That note sets the current paper direction: remove the tutorial framing, foreground the generalized DLO simulation framework, benchmark DynEGA against defensible alternatives, and use empirical OAI-large time-series checks as a cautious realism argument.

## Clean Contents

- `extract.r`: core DLO simulation, empirical-target, and estimator helpers.
- `run_method_benchmark.R`: revision benchmark grid and method runners.
- `run_dynega_full_suite.R`, `run_graphicalvar_full_suite.R`, `run_gimme_full_suite.R`: method-specific benchmark entrypoints.
- `summarize_full_suite_results.R`: combines method outputs into the final revision summary tables.
- `plot_revision_first_benchmark_figures.R`: regenerates the revision figure bundle.
- `run_empirical_mafw_sample_pipeline.R` and `make_oai_large_similarity_overview.py`: empirical OAI-large realism check.
- `benchmark_outputs_revision_summary/`: finalized combined benchmark tables.
- `comparison_revision_first_benchmark/`: finalized benchmark figures.
- `reports/revision_benchmark_report.tex` and `.pdf`: compact results scaffold for the manuscript revision.
- `paper/`: manuscript-facing working notes and source readouts from the earlier simulation scripts.

Older smoke runs, failed probes, exploratory plots, intermediate `.rds` files, and superseded output directories were moved to `archive/2026-05-02-pre-rr-tidy/` and are intentionally ignored by Git.

## Main Commands

Load the core implementation without running the expensive extract pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

Run the method-specific revision benchmarks:

```bash
BENCHMARK_WORKERS=4 BENCHMARK_CHECKPOINT_EACH=1 Rscript run_dynega_full_suite.R
BENCHMARK_GRAPHICALVAR_WORKERS=24 BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC=600 BENCHMARK_CHECKPOINT_EACH=1 Rscript run_graphicalvar_full_suite.R
BENCHMARK_GIMME_WORKERS=8 BENCHMARK_GIMME_TIMEOUT_SEC=360 BENCHMARK_CHECKPOINT_EACH=1 Rscript run_gimme_full_suite.R
```

Regenerate the final combined summary and figures:

```bash
Rscript summarize_full_suite_results.R
Rscript plot_revision_first_benchmark_figures.R
```

Rebuild the report from the repository root:

```bash
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=reports reports/revision_benchmark_report.tex
```

## Current Results

The finalized benchmark contains 1,920 full-grid rows each for DynEGA and GraphicalVAR, plus a 120-row bounded GIMME feasibility run. DynEGA completed all rows and recovered the correct dimensionality in 97.4% of rows. GraphicalVAR had similar score recovery but was much slower and had 48 timeouts. GIMME is retained only as a bounded feasibility comparator because timeouts dominate the all-row result.
