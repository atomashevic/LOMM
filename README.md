# LOMM Manuscript Simulation Workspace

This repository is the cleaned RR manuscript workspace for the Latent Oscillator Measurement Model (LOMM) paper. The visible tree is organized around the current manuscript, the rendered manuscript artifacts, and the scripts needed to audit or regenerate manuscript-facing figures and tables.

## Active Manuscript

- `paper/Paper_emotion_dynamics_HGolino_RR.qmd`: current RR manuscript source.
- `paper/Paper_emotion_dynamics_HGolino_RR.pdf`: rendered manuscript PDF.
- `paper/response_letter.tex` and `paper/response_letter.pdf`: current response letter.
- `paper/references.bib` and `paper/apa.csl`: manuscript bibliography and citation style.
- `paper/figures/` and `paper/tables/`: manuscript-facing generated assets.

Legacy drafts, old readouts, older reports, obsolete bibliography backups, diagnostic scripts, and raw result trees were moved to ignored dated folders under `archive/`.

## Clean Layout

- `extract.r`: core LOMM simulation, empirical-target, and estimator helpers.
- `run_external_structure_fit.R`: external-fit helper used by the retained simulation runner.
- `scripts/simulations/`: final simulation runners, launchers, shared runner internals, and the final comparison builder.
- `scripts/figures/`: scripts that regenerate manuscript-facing figures and tables.
- `paper/figures/` and `paper/tables/`: rendered manuscript-facing outputs.
- `archive/`: ignored local archive for raw result trees, old scripts, and superseded notes.

## Main Commands

Load the core implementation without running the expensive extract pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

Regenerate manuscript-facing figures and tables:

```bash
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

The manuscript itself renders from the checked-in `paper/figures/` and
`paper/tables/` files. The raw result trees needed to rerun the figure/table
pipeline are local archived artifacts under
`archive/2026-06-16-coauthor-tidy/results/simulation_outputs/`; restore that
directory to `simulation_outputs/` before rerunning the upstream scripts.

Rebuild the manuscript and response letter:

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

See `scripts/README.md` for the current production map.

## Terminology

Visible project documentation now uses "simulation" for the active manuscript workflow. Some internal environment variables and archived historical files still use `BENCHMARK` or `benchmark` for provenance and backward compatibility with existing R runner logic.
