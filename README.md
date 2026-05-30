# LOMM Manuscript Simulation Workspace

This repository is the cleaned RR manuscript workspace for the Latent Oscillator Measurement Model (LOMM) paper. The visible tree is organized around the current manuscript, the scripts that reproduce its figures and tables, and the active simulation evidence bundles used by those scripts.

## Active Manuscript

- `paper/Paper_emotion_dynamics_HGolino_RR.qmd`: current RR manuscript source.
- `paper/Paper_emotion_dynamics_HGolino_RR.pdf`: rendered manuscript PDF.
- `paper/response_letter.tex` and `paper/response_letter.pdf`: current response letter.
- `paper/references.bib` and `paper/apa.csl`: manuscript bibliography and citation style.
- `paper/figures/` and `paper/tables/`: manuscript-facing generated assets.

Legacy drafts, old readouts, older reports, and obsolete bibliography backups were moved to `archive/2026-05-30-manuscript-tidy/old-manuscript-backup/`.

## Clean Layout

- `extract.r`: core LOMM simulation, empirical-target, and estimator helpers.
- `scripts/simulations/`: active simulation runners and method-comparison entrypoints.
- `scripts/figures/`: scripts that regenerate manuscript-facing figures and tables.
- `scripts/empirical/`: empirical MAFW/OAI realism and diagnostic scripts.
- `scripts/reports/`: summary builders retained for audit/regeneration.
- `scripts/legacy/`: retained exploratory scripts that are not part of the active manuscript run map.
- `simulation_outputs/`: active simulation and empirical evidence bundles read by manuscript scripts.
- `archive/2026-05-30-manuscript-tidy/`: dated cleanup archive with `archive_manifest.tsv`.

## Main Commands

Load the core implementation without running the expensive extract pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

Regenerate manuscript-facing figures and tables:

```bash
Rscript scripts/figures/plot_lomm_pipeline_overview.R
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

See `scripts/README.md` for the full production map.

## Terminology

Visible project documentation now uses "simulation" for the active manuscript workflow. Some internal environment variables and archived historical files still use `BENCHMARK` or `benchmark` for provenance and backward compatibility with existing R runner logic.
