# Reproducing the LOMM paper

Run all commands from the repository root.

## Reproduction levels

This repository supports three levels of reproduction:

1. Render the manuscript and response letter from the checked-in figures and
   tables.
2. Run the LOMM example and focused validation tests.
3. Rerun the simulation studies and rebuild the result summaries.

The first two levels require no external data. The full Study 1 workflow
requires licensed MAFW data and locally generated frame-level classifier scores.

## Software

The reported analyses used R 4.5.1 and Python 3.13. Rendering also requires
Quarto and a LaTeX distribution.

Install the main R packages with:

```r
install.packages(c(
  "clue", "cowplot", "digest", "dplyr", "EGAnet", "ggplot2", "gimme",
  "graphicalVAR", "igraph", "jpeg", "jsonlite", "knitr", "lavaan", "MASS",
  "patchwork", "psych", "purrr", "qgraph", "readr", "scales", "stringr",
  "tibble", "tidyr"
))
```

## Validate the public implementation

Run the worked example and focused tests:

```bash
Rscript scripts/lomm/example_observation_profile.R
Rscript scripts/lomm/test_observation_profile.R
Rscript scripts/simulations/test_graphicalvar_centering_covariance.R
```

Load the shared implementation without starting the empirical extraction
workflow:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('scripts/extract.r')"
```

## Render the paper

The manuscript reads the checked-in files under `paper/figures/` and
`paper/tables/`.

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

## Rerun the simulations

Simulation outputs are written under `simulation_outputs/`, which is ignored by
Git. The full grids are computationally expensive. Limit numerical-library
threading before running parallel jobs:

```bash
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
```

Inspect and validate the simulation job graph without fitting any model:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py plan
python3 scripts/simulations/orchestrate_revision_jobs.py validate --structural-only
```

The plan command lists the required job order. For any listed job, print its
exact launch command with:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job JOB_ID
```

The main simulation specification and dispatcher are:

- `scripts/simulations/revision_spec.R`
- `scripts/simulations/run_revision_job.R`
- `scripts/simulations/revision_jobs.json`

After the validated Study 2 aggregate directory has been produced, rebuild its
paper-facing results with:

```bash
Rscript scripts/figures/build_revision_v2_artifacts.R \
  --aggregate-dir /path/to/study2-aggregate \
  --output-root paper
```

## MAFW-derived inputs

MAFW is a third-party dataset. Request access from
<https://mafw-database.github.io/MAFW/> and follow its research-use terms. Keep
the downloaded videos, extracted frames, and frame-level classifier scores
outside this repository.

The empirical workflow expects one row per retained frame with identifiers and
the following score channels:

```text
clip,frame_index,truth_label,anger,disgust,fear,happiness,neutral,sadness,
surprise,contempt,anxiety,helplessness,disappointment
```

With the restricted classifier-score input and Study 1 calibration plan in
place, run the four-fold 25-versus-25 workflow in this order:

```bash
Rscript scripts/lomm_1_1/build_mafw_plausibility_validation_25v25.R
Rscript scripts/lomm_1_1/build_mafw_plausibility_validation_25v25_extended.R
Rscript scripts/lomm_1_1/build_mafw_clip_level_benchmark.R
Rscript scripts/figures/build_mafw_study1_figures.R
```

The first two commands select the generator and compute the extended metric
panel. The third computes the disjoint MAFW reference, plausibility, coverage,
and metric-specific discrepancies. The final command writes the manuscript
figures and paper-facing tables.

The manuscript used the `oai-large` scoring setting. The scoring-time package
release and exact model-checkpoint revision were not archived, so exact frame
rescoring is not guaranteed. The checked-in aggregate summaries support the
reported simulated-versus-real comparison without redistributing frame-level
scores or raw videos.
