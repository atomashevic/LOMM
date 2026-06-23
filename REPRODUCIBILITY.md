# From-Scratch Reproducibility

This repository contains the manuscript source, simulation runners, figure and
table scripts, and provenance checks for the LOMM revise-and-resubmit package.
Run all commands from the repository root.

## Scope

The final method-comparison simulations can be rerun from the code in this
repository. The OSF package should also include the derived empirical realism
input at
`simulation_outputs/empirical_link_options/oai-large-100/metric_comparison_long.csv`.
That file is derived from MAFW frame-level FER-like score panels. The raw MAFW
videos remain third-party materials governed by the dataset provider.

The checked-in manuscript can be rendered from `paper/figures/` and
`paper/tables/`. A full rerun writes intermediate and final outputs under the
ignored `simulation_outputs/` directory.

## Software

The workflow uses R, Quarto, XeLaTeX, Python 3, and tmux for long runs. The main
R packages used by the retained scripts are `EGAnet`, `dplyr`, `tidyr`, `purrr`,
`MASS`, `ggplot2`, `readr`, `stringr`, `scales`, `patchwork`, `igraph`, `clue`,
`graphicalVAR`, `gimme`, `cowplot`, and `jpeg`.

The final simulation launchers limit BLAS-style thread pools. For direct
foreground runs, set these variables first if the machine uses multithreaded
linear algebra:

```bash
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
```

## Quick Integrity Checks

Load the core implementation without running the expensive extraction pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

Run the project provenance check:

```bash
python3 tools/research_ops.py check-paper
```

## Dry-Run Grid Checks

These commands validate the final grids and write only small grid/manifest
files to a temporary output tree:

```bash
FINAL_DYNEGA_DRY_RUN=1 \
FINAL_DYNEGA_OUT_DIR=simulation_outputs/repro_dry_run/dynega_full \
Rscript scripts/simulations/run_final_dynega_full.R

FINAL_DYNEGA_CD_DRY_RUN=1 \
FINAL_DYNEGA_CD_OUT_DIR=simulation_outputs/repro_dry_run/dynega_bounded_nvar4_cd \
Rscript scripts/simulations/run_final_dynega_bounded_cd.R

FINAL_GRAPHICALVAR_BOUNDED_DRY_RUN=1 \
FINAL_GRAPHICALVAR_BOUNDED_OUT_DIR=simulation_outputs/repro_dry_run/graphicalvar_bounded_nvar4 \
Rscript scripts/simulations/run_final_graphicalvar_bounded.R

FINAL_GIMME_BOUNDED_DRY_RUN=1 \
FINAL_GIMME_BOUNDED_OUT_DIR=simulation_outputs/repro_dry_run/gimme_bounded_nvar4 \
Rscript scripts/simulations/run_final_gimme_bounded.R
```

Expected dry-run grid sizes are:

- DynEGA full grid: 1,296 structural cells, 400 replications, 518,400 primary method rows.
- DynEGA bounded CD grid: 21,600 structure fits, Leiden and Walktrap readouts, 43,200 method rows.
- GraphicalVAR bounded grid: 21,600 structure fits, Louvain/Leiden/Walktrap readouts, 64,800 method rows.
- GIMME bounded grid: 21,600 structure fits, Louvain/Leiden/Walktrap readouts, 64,800 method rows.

## Full Simulation Rerun

The tmux launchers are the preferred full-run entrypoints because the final
grids are long-running jobs with checkpoint/resume behavior:

```bash
bash scripts/simulations/launch_final_dynega_tmux.sh
bash scripts/simulations/launch_final_dynega_bounded_cd_tmux.sh
bash scripts/simulations/launch_final_graphicalvar_bounded_tmux.sh
bash scripts/simulations/launch_final_gimme_bounded_tmux.sh
```

Default outputs are:

- `simulation_outputs/final_simulation_results/dynega_full/`
- `simulation_outputs/final_simulation_results/dynega_bounded_nvar4_cd/`
- `simulation_outputs/final_simulation_results/graphicalvar_bounded_nvar4/`
- `simulation_outputs/final_simulation_results/gimme_bounded_nvar4/`

The main adjustable environment variables are:

- `FINAL_DYNEGA_WORKERS`, `FINAL_DYNEGA_REPS`, `FINAL_DYNEGA_OUT_DIR`
- `FINAL_DYNEGA_CD_WORKERS`, `FINAL_DYNEGA_CD_REPS`, `FINAL_DYNEGA_CD_ALGORITHMS`, `FINAL_DYNEGA_CD_OUT_DIR`
- `FINAL_GRAPHICALVAR_BOUNDED_WORKERS`, `FINAL_GRAPHICALVAR_BOUNDED_REPS`, `FINAL_GRAPHICALVAR_BOUNDED_TIMEOUT_SEC`, `FINAL_GRAPHICALVAR_BOUNDED_OUT_DIR`
- `FINAL_GIMME_BOUNDED_WORKERS`, `FINAL_GIMME_BOUNDED_REPS`, `FINAL_GIMME_BOUNDED_TIMEOUT_SEC`, `FINAL_GIMME_BOUNDED_STOP_CRIT`, `FINAL_GIMME_BOUNDED_OUT_DIR`

The manuscript values use the default replication counts: 400 for the full
DynEGA grid and 100 for the bounded comparator grids.

## Build Final Summaries, Figures, And Tables

After the four final result directories exist, build the comparison summaries
and manuscript-facing assets:

```bash
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

`plot_empirical_realism_results.R` requires the packaged derived empirical
input:

```text
simulation_outputs/empirical_link_options/oai-large-100/metric_comparison_long.csv
```

Researchers with access to the underlying MAFW-derived frame-score CSV can
rerun the empirical realism calibration by sourcing `extract.r` with
`options(run_extract_pipeline=FALSE)` and calling
`run_empirical_dlo_realism_pipeline(frame_scores_path=..., model_name=..., output_dir=...)`.
The manuscript figure script reads the selected final comparison table above.

Figure 3 is checked in as `paper/figures/mafw_video_dynega_example.pdf`. To
regenerate it, supply the derived frame-score CSV and selected video frames used
by `scripts/figures/plot_oai_clip_video_dynega_figure.R`.

## Render The Manuscript

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

## Provenance Manifests

The current package records manuscript-facing claims in
`research/claim-ledger.yaml` and run/artifact hashes in:

- `research/runs/final-manuscript-comparison-20260623/run-manifest.json`
- `research/runs/manuscript-figures-tables-20260623/run-manifest.json`

Validate them with:

```bash
python3 tools/research_ops.py validate-run research/runs/final-manuscript-comparison-20260623/run-manifest.json
python3 tools/research_ops.py validate-run research/runs/manuscript-figures-tables-20260623/run-manifest.json
python3 tools/research_ops.py check-paper
```
