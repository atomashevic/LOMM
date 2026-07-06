# Scripts

Run commands from the repository root.

## Core Implementation

`scripts/extract.r` contains the main LOMM functions, empirical-target helpers,
and estimator wrappers. Load it without running the expensive extraction
pipeline with:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('scripts/extract.r')"
```

## Final Simulation Runners

The final method-comparison runners are:

```bash
Rscript scripts/simulations/run_final_dynega_full.R
Rscript scripts/simulations/run_final_dynega_bounded_cd.R
Rscript scripts/simulations/run_final_graphicalvar_bounded.R
Rscript scripts/simulations/run_final_gimme_bounded.R
```

For full reruns, prefer the matching tmux launchers in
`scripts/simulations/`. They set conservative thread limits and use the default
output directories under `simulation_outputs/final_simulation_results/`.

Shared scenario and estimator internals live in:

- `scripts/simulations/run_emotion_dynega_simulation.R`
- `scripts/simulations/run_method_simulation.R`
- `scripts/run_external_structure_fit.R`

Some runner internals still use `BENCHMARK_*` environment variables. Those
names remain in place so existing long-run scripts and checkpoints keep working.

## Paper Figures And Tables

The checked-in manuscript renders from files already present under
`paper/figures/` and `paper/tables/`. To rebuild those files after a full rerun,
use:

```bash
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

`plot_empirical_realism_results.R` requires:

```text
simulation_outputs/empirical_link_options/oai-large-100/metric_comparison_long.csv
```

Figure 3 is produced by
`scripts/figures/plot_oai_clip_video_dynega_figure.R` when the derived
frame-score CSV and selected video frames are available.

## Manuscript Build

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

For complete rerun instructions and dry-run grid checks, see
`REPRODUCIBILITY.md`.
