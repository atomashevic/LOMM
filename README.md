# LOMM

Reproducibility materials for the paper *A Latent Oscillator Measurement Model
for Simulating Emotional-Expression Score Dynamics in Video*.

Use this repository to inspect the current manuscript package, rerun the LOMM
simulations, and rebuild the paper-facing figures and tables. The checked-in
PDFs, figures, and tables are the current shared materials for collaborators.
Long reruns write to ignored local directories, so collaborators can reproduce
the results without changing the repository.

## Where To Start

- `paper/Paper_emotion_dynamics_HGolino_RR.qmd`: manuscript source.
- `paper/Paper_emotion_dynamics_HGolino_RR.pdf`: rendered manuscript PDF.
- `paper/response_letter.tex` and `paper/response_letter.pdf`: response letter.
- `paper/references.bib` and `paper/apa.csl`: bibliography and citation style.
- `paper/figures/` and `paper/tables/`: figures and tables used by the paper.
- `REPRODUCIBILITY.md`: full rerun instructions for the final simulation
  package.

## Code Map

- `scripts/extract.r`: core LOMM implementation, empirical-target helpers, and
  estimator wrappers.
- `scripts/simulations/`: final simulation runners, tmux launchers, shared
  runner internals, and the manuscript-comparison builder.
- `scripts/figures/`: scripts that regenerate paper-facing figures and tables.
- `scripts/run_external_structure_fit.R`: external helper used by the retained
  simulation runners.
- `scripts/README.md`: compact production map for the script directory.

The default rerun output location is `simulation_outputs/`, which is ignored by
Git. Historical local material belongs under ignored `archive/` folders and is
not part of the public reproducibility surface.

## Quick Checks

Load the core implementation without running the expensive extraction pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('scripts/extract.r')"
```

Regenerate paper-facing figures and tables after the final simulation result
directories exist:

```bash
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

Rebuild the manuscript and response letter:

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

## Data Boundary

The repository contains the manuscript source, code, and paper-facing generated
assets. Rerunning the empirical realism figure also requires the derived
MAFW-based metric file described in `REPRODUCIBILITY.md`. Raw MAFW videos remain
third-party dataset materials governed by the dataset provider.

## Terminology

Public documentation uses `LOMM`, `simulation`, `classifier-output score
channels`, and `paper-facing assets` for the active workflow. Some internal
environment variables still use `BENCHMARK` or `benchmark` for compatibility
with existing R runner logic.
