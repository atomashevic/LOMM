# Scripts

Run commands from the repository root.

## Core Implementation

`scripts/extract.r` contains the main LOMM functions, empirical-target helpers,
and estimator wrappers. Load it without running the expensive extraction
pipeline with:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('scripts/extract.r')"
```

## Public LOMM Observation Default

New simulations should source `scripts/lomm/observation_profile.R`. Its public
model name is `LOMM`, and its default profile is `compositional_bs`: an
empirical contrast-space baseline plus positive clip scale, followed by the
identified ILR-to-simplex map. The default bounded observation call requires an
ILR basis; pre-link output requires an explicit linear-contrast surface.

```r
source("scripts/lomm/observation_profile.R")
profile <- lomm_default_observation_profile()
```

The public baseline modes are:

- `empirical`, using a named positive-semidefinite between-clip covariance;
- `loading_aligned`, using `b_i = Lambda %*% beta_i` for a declared
  known-truth loading geometry;
- `none`, using an exactly zero baseline.

Clip scale is controlled separately. The `homogeneous` profile combines
`baseline_mode = "none"` with `clip_scale_enabled = FALSE`, while an S-only
sensitivity uses baseline mode `none` with clip scale enabled. Every returned
panel records the public profile, baseline mode, actual observation surface,
RNG version, active geometry hashes, and retained internal profile/schema
identifiers. The internal profile ID is labeled as calibration ancestry because
the public wrapper accepts declared geometries beyond the locked MAFW preset.

Tracked receipts under `scripts/lomm/evidence/` preserve the hashes and scoped
summaries of the ignored realism and recovery artifacts. When those full local
artifacts are present, the test suite validates their hashes as an additional
gate.

Run the public contract and provenance regression with:

```bash
Rscript scripts/lomm/test_observation_profile.R
```

The revision-v2 and `scripts/extract.r` generators retain their original
versioned defaults. The broad and matched recovery generators remain historical
homogeneous linear surfaces; the softmax realism and bounded challenge surfaces
remain separate revision-v2 analyses. Adopting the public profile in a future
benchmark requires a new versioned job graph and output namespace.

## Internal Evidence Namespace: Compositional Preprocessing

The versioned `lomm_1_1` preprocessing layer supplies the evidence and internal
provenance contract behind the public default. It remains isolated from the
completed revision-v2 job graph:

- `scripts/preprocessing/compositional_math.R`: strict simplex closure, CLR,
  a locked sequential Helmert ILR basis, and inverse transforms.
- `scripts/preprocessing/compositional_views.R`: segment construction and
  whole-composition interpolation in ILR space.
- `scripts/preprocessing/empirical_compositional_bundle.R`: the retained-panel
  schemas, QC ledger, and source regression counts.
- `scripts/preprocessing/build_empirical_compositional_views.R`: deterministic,
  hash-namespaced materialization with dry-run support.
- `scripts/preprocessing/validate_published_bundle.R`: receipt-only validation
  that rebuilds all tables from the locked source artifact.
- `scripts/preprocessing/test_compositional_views.R`: mathematical,
  missing-data, and CLI guard fixtures.

Run the tests and inspect the prospective manifest before materialization:

```bash
Rscript scripts/preprocessing/test_compositional_views.R
export LOMM_MAFW_FRAME_SCORES=/path/to/mafw-retained100/frame_scores.csv
Rscript scripts/preprocessing/build_empirical_compositional_views.R \
  --input "$LOMM_MAFW_FRAME_SCORES" \
  --dry-run
```

Remove `--dry-run` to publish the validated bundle under:

```text
simulation_outputs/lomm_1_1/empirical_compositional_preprocess/<config-hash>/
```

Validate a published bundle independently with:

```bash
Rscript scripts/preprocessing/validate_published_bundle.R \
  --directory simulation_outputs/lomm_1_1/empirical_compositional_preprocess/<config-hash>
```

The bundle contains a complete-case primary variant and a sensitivity variant
that fills bounded internal gaps of at most two frames by linear interpolation
of the full ILR vector. Every view retains segment IDs and imputation flags.
The preprocessing scripts contain no recovery-estimator calls.

## Internal Evidence Namespace: Empirical-Realism Ablation

The versioned ablation is a separate, estimator-free calibration study built
on the certified compositional bundle. Its checked-in specification fixes eight
between-clip, clip-scale, and smooth-gate variants; 27 stable oscillator cells;
five clip-level folds; and paired complete-case and ILR-gap sensitivity views.

The planning and pure-function files are:

- `scripts/lomm_1_1/realism_ablation_config.json`: locked scientific,
  resource, seed-pairing, and claim contracts.
- `scripts/lomm_1_1/realism_ablation_spec.R`: candidate, fold, unit-key,
  dependency, and future-output schemas.
- `scripts/lomm_1_1/realism_generator.R`: exact stable transitions,
  stationary initialization, training-only contrast geometry, clip effects,
  smooth gates, and compositional observation generation.
- `scripts/lomm_1_1/realism_metrics.R`: segment-aware probability and
  log-ratio realism metrics.
- `scripts/lomm_1_1/build_realism_ablation_plan.R`: dry-run and atomic plan
  materialization.
- `scripts/lomm_1_1/validate_realism_ablation_plan.R`: receipt-only plan
  reconstruction and validation.
- `scripts/lomm_1_1/run_realism_ablation.R`: read-only dry-run validation and
  an explicitly authorized, resumable PSOCK dispatcher for future execution.
- `scripts/lomm_1_1/test_realism_ablation.R`: scientific, provenance, and CLI
  guard fixtures.

Validate the specification and inspect the full prospective manifest with:

```bash
Rscript scripts/lomm_1_1/test_realism_ablation.R
Rscript scripts/lomm_1_1/build_realism_ablation_plan.R --dry-run
```

Materialize the plan without generating any panels:

```bash
Rscript scripts/lomm_1_1/build_realism_ablation_plan.R --materialize-plan
```

The plan is published under:

```text
simulation_outputs/lomm_1_1/empirical_realism_ablation/<config-prefix>/
```

Validate the materialized plan against the future runner without generating a
panel:

```bash
Rscript scripts/lomm_1_1/run_realism_ablation.R \
  --plan-dir simulation_outputs/lomm_1_1/empirical_realism_ablation/<config-prefix> \
  --dry-run
```

The dry-run prints the exact future launch and resume commands. Future
execution requires the `arrow` R package, the literal acknowledgement token,
and the full SHA-256 of `prospective_manifest.json`. The dispatcher uses at
most 12 PSOCK workers, sets numerical-library threads to one per worker,
checkpoints every planned unit, and never advances another job automatically.

The future workload contains 216 candidates, 3,240 training panels, 40 frozen
fold-by-variant selections, 4,000 held-out panels, and 8,000 held-out view
evaluations. Plan materialization records these as future work and never calls
the simulator, a recovery estimator, a parallel backend, or a session manager.

## Internal Evidence Namespace: Canonical Profile And Recovery Bridge

The canonical bounded-score observation profile is defined in:

- `scripts/lomm_1_1/canonical_observation_profile.R`;
- `scripts/lomm_1_1/canonical_observation_profile_config.json`;
- `scripts/lomm_1_1/test_canonical_observation_profile.R`.

It activates between-clip contrast baselines and positive clip-specific scale
heterogeneity (`B+S`), keeps activity gates off, and exactly nests the
homogeneous profile at a zero baseline and unit scale. The MAFW/CLIP preset
uses `P=12`, `xi=0.5`, and `kappa=1` in the locked ILR basis.

The paired realism effects are built and validated with:

```bash
Rscript scripts/lomm_1_1/test_aggregate_realism_ablation.R
Rscript scripts/lomm_1_1/aggregate_realism_ablation.R \
  --output-dir simulation_outputs/lomm_1_1/empirical_realism_ablation_aggregates/854e8722d647/9769e6dd7fd1 \
  --validate-output
```

The completed aggregate contains 272 rows: two analysis views, 17 outcomes,
and eight paired factorial contrasts with 2,000 fold-stratified clip-cluster
bootstrap draws. Its analysis hash is
`9769e6dd7fd116389f73e17839a748abedb5cdc26e13444bb9e2dc5ce4f8e855`.

The pre-link linear known-truth recovery bridge is implemented by:

- `scripts/lomm_1_1/bridge_recovery_config.json`;
- `scripts/lomm_1_1/bridge_recovery_spec.R`;
- `scripts/lomm_1_1/build_bridge_recovery_plan.R`;
- `scripts/lomm_1_1/run_bridge_recovery.R`;
- `scripts/lomm_1_1/test_bridge_recovery.R`.

The canonical materialized plan is:

```text
simulation_outputs/lomm_1_1/recovery_bridge/991d45f9c90f/
```

Validate it without fitting an estimator from the repository root or another
working directory:

```bash
Rscript scripts/lomm_1_1/test_bridge_recovery.R
Rscript scripts/lomm_1_1/run_bridge_recovery.R \
  --plan-dir simulation_outputs/lomm_1_1/recovery_bridge/991d45f9c90f \
  --dry-run \
  --prospective-manifest-sha256 20671dab38be9b752cb3d0005ec28df0df956ae651d9817fce127d23add2b6d6
```

The plan fixes 80 paired panel sets, 160 generated panels, 800 method fits,
and 2,400 readout rows. The completed run is retained under
`simulation_outputs/lomm_1_1/recovery_bridge_runs/991d45f9c90f/`. It screens
loading-aligned B+S on the `linear_z` representation; it does not validate
recovery from bounded probabilities or CLR panels. Execution is guarded by the
exact manifest SHA and literal acknowledgement token, uses at most eight
workers with one numerical-library thread each, preserves append-only attempts,
and validates terminal artifacts before completion.

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
export REVISION_EMPIRICAL_FRAME_SCORES=/absolute/path/to/frame_scores.csv
python3 scripts/simulations/orchestrate_revision_jobs.py plan
python3 scripts/simulations/orchestrate_revision_jobs.py validate
python3 scripts/simulations/orchestrate_revision_jobs.py status
python3 scripts/simulations/orchestrate_revision_jobs.py next
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job t100_dynega
```

Full validation requires the empirical frame-score input so validation and
launch use the same content-hashed namespace, including for downstream jobs.
Use `validate --structural-only` for an input-free check of the graph and
static contracts; it does not create launch-ready dry-run manifests.

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
