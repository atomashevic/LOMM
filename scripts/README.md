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

## Revision V2 Pre-Compute Runners

The revision analyses share one prospective specification and one guarded job
interface:

- `scripts/simulations/revision_spec.R`: canonical grids, panel keys,
  transformations, partition helpers, estimands, and validation contracts.
- `scripts/simulations/run_revision_job.R`: named-job dispatcher with mandatory
  dry-run support.
- `scripts/simulations/build_revision_results.R`: future aggregation and
  reporting helpers; syntax-check only during pre-compute preparation.
- `scripts/figures/build_revision_v2_artifacts.R`: future V2-only figure and
  table adapter; it reads validated aggregate artifacts and never falls back to
  the checked-in legacy result paths.
- `scripts/simulations/revision_jobs.json`: dependency graph and expected
  artifacts.
- `scripts/simulations/orchestrate_revision_jobs.py`: validation, status,
  monitoring, and guarded future launch/resume commands.
- `scripts/simulations/repair_structure_checkpoint.py`: hash-guarded,
  atomic repair for structure-job batches serialized under the error-row
  column order. It requires explicit expected row and panel counts, preserves
  the original checkpoint as a backup, and writes a repair receipt.

The safe preparation commands are:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py plan
python3 scripts/simulations/orchestrate_revision_jobs.py validate
python3 scripts/simulations/orchestrate_revision_jobs.py status
python3 scripts/simulations/orchestrate_revision_jobs.py next
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job t100_dynega
```

No command in this list fits an estimator. Scientific outputs use
`simulation_outputs/revision_v2/<job-id>/<config-hash>/`; a configuration
change therefore creates a separate checkpoint namespace. See
`REVISION_V2_HANDOFF.md` for future launch guards and manuscript integration
gates.

Revision DynEGA jobs distinguish physical cadence from the EGAnet GLLA unit:
LOMM generation and horizon metadata retain `dt = 0.4`, while DynEGA uses
`delta = 1` observation interval. Both values are recorded in prospective
manifests and method-result rows.

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
