# Manuscript Production Scripts

Run these commands from the repository root.

## Core Check

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

## Manuscript Figures And Tables

```bash
Rscript scripts/figures/plot_lomm_pipeline_overview.R
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

These scripts write to `paper/figures/` and `paper/tables/`. Their active inputs are under `simulation_outputs/empirical_realism/`, `simulation_outputs/method_comparison/`, and the method-specific `simulation_outputs/emotion_*` directories.

## Upstream Simulation Evidence

```bash
Rscript scripts/simulations/run_emotion_dynega_simulation.R
Rscript scripts/simulations/run_emotion_graphicalvar_simulation.R
Rscript scripts/simulations/run_emotion_gimme_simulation.R
Rscript scripts/figures/plot_non_a_main_and_method_comparison.R
Rscript scripts/empirical/summarize_oai_validation_sensitivity.R
```

The R runner internals still use `BENCHMARK_*` environment variables where renaming would risk breaking existing code. Defaults now write visible active outputs under `simulation_outputs/`.

## Manuscript Build

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

## Legacy Scripts

Scripts in `scripts/legacy/` are retained for provenance or older exploratory branches. They are not part of the active manuscript production map and may still refer to historical `benchmark_*` paths.
