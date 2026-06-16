# Manuscript Production Scripts

Run these commands from the repository root.

## Core Check

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

## Manuscript Figures And Tables

The checked-in manuscript renders from the generated files already present
under `paper/figures/` and `paper/tables/`. To regenerate those files from raw
results on this machine, restore
`archive/2026-06-16-coauthor-tidy/results/simulation_outputs/` to
`simulation_outputs/` first.

```bash
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

These scripts write to `paper/figures/` and `paper/tables/`. The final
simulation comparison uses the archived raw result trees under
`simulation_outputs/final_simulation_results/` after restoration. The empirical
realism figure uses the archived link-option output under
`simulation_outputs/empirical_link_options/`.

## Final Simulation Evidence

```bash
Rscript scripts/simulations/run_final_dynega_full.R
Rscript scripts/simulations/run_final_dynega_bounded_cd.R
Rscript scripts/simulations/run_final_graphicalvar_bounded.R
Rscript scripts/simulations/run_final_gimme_bounded.R
```

The final runners share scenario and estimator internals from
`scripts/simulations/run_emotion_dynega_simulation.R` and
`scripts/simulations/run_method_simulation.R`. The R runner internals still use
`BENCHMARK_*` environment variables where renaming would risk breaking existing
code. Defaults write local outputs under the ignored `simulation_outputs/`
directory.

## Manuscript Build

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

## Archived Material

Older exploratory scripts, diagnostics, reports, and raw result trees were moved
to ignored dated archive folders. They are local provenance artifacts and are
not part of the co-author-facing manuscript tree.
