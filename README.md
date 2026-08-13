# LOMM

Reproducibility materials for the paper *A Latent Oscillator Measurement Model
to Simulate Emotional-Expression Score Dynamics in Video*.

Use this repository to inspect the current manuscript package, rerun the LOMM
simulations, and rebuild the paper-facing figures and tables. The checked-in
PDFs, figures, and tables are the current shared materials for collaborators.
Long reruns write to ignored local directories, so collaborators can reproduce
the results without changing the repository.

## Default Observation Pipeline

The public LOMM observation pipeline uses the compositional B+S profile by
default. It generates an additive clip baseline (`B`) in an identified contrast
space, applies a positive clip-specific scale (`S`) to the dynamic signal and
measurement residual, and maps the result to the probability simplex when an
ILR basis is supplied. The default bounded observation call requires that
basis; callers must select the linear-contrast surface explicitly when they
need pre-link output.

```r
source("scripts/lomm/observation_profile.R")
lomm_default_observation_profile()
```

The baseline geometry is always explicit:

- `empirical`: the default, using a supplied training-derived between-clip
  covariance in the active contrast coordinates;
- `loading_aligned`: draws clip baselines inside the space spanned by the
  loadings, `b_i = Lambda %*% beta_i`, so between-clip differences stay
  consistent with the factor structure;
- `none`: a zero baseline. Clip scale remains independently configurable, and
  the `homogeneous` profile sets `baseline_mode = "none"` and disables clip
  scale exactly.

The public model name is `LOMM`. Manifests retain the internal profile ID
`lomm_1_1_mafw_clip_bs_v1` as calibration ancestry, together with its schema
version, source hashes, the active geometry hashes, and historical
`simulation_outputs/lomm_1_1/` paths for reproducibility. All revision-v2
generators retain their original versioned behavior. The completed broad and
matched recovery generators remain historical homogeneous linear surfaces;
the softmax realism and bounded challenge surfaces remain separate historical
revision-v2 analyses.

## Where To Start

- `paper/Paper_emotion_dynamics_HGolino_RR.qmd`: manuscript source.
- `paper/Paper_emotion_dynamics_HGolino_RR.pdf`: rendered manuscript PDF.
- `paper/response_letter.tex` and `paper/response_letter.pdf`: response letter.
- `paper/references.bib` and `paper/apa.csl`: bibliography and citation style.
- `paper/figures/` and `paper/tables/`: figures and tables used by the paper.
- `REPRODUCIBILITY.md`: full rerun instructions for the final simulation
  package.
- `REVISION_V2_HANDOFF.md`: dry-run validation and future launch instructions
  for the prepared revision analyses.

## Code Map

- `scripts/lomm/observation_profile.R`: public LOMM observation entrypoint;
  defaults to compositional B+S and exposes the three baseline modes.
- `scripts/lomm/observation_profile_config.json`: public defaults plus retained
  internal schema, profile, calibration, and evidence identifiers.
- `scripts/lomm/evidence/`: tracked receipts for the ignored calibration and
  bridge artifacts cited by the public default.
- `scripts/extract.r`: historical core implementation, empirical-target
  helpers, and estimator wrappers retained for completed simulations.
- `scripts/simulations/`: final simulation runners, tmux launchers, shared
  runner internals, and the manuscript-comparison builder.
- `scripts/figures/`: scripts that regenerate paper-facing figures and tables.
- `scripts/run_external_structure_fit.R`: external helper used by the retained
  simulation runners.
- `scripts/README.md`: compact production map for the script directory.

The default rerun output location is `simulation_outputs/`, which is ignored by
Git. Historical local material belongs under ignored `archive/` folders and is
not part of the public reproducibility surface.

## Revision V2 Pre-Compute Interface

The revision package is prepared through the script-and-orchestration boundary.
Its default commands inspect or validate prospective work and cannot start an
estimator. Run the complete dry-run validation with the empirical frame-score
input bound before configuration hashes are calculated:

```bash
export REVISION_EMPIRICAL_FRAME_SCORES=/absolute/path/to/frame_scores.csv
python3 scripts/simulations/orchestrate_revision_jobs.py validate
```

When that external input is unavailable, validate only the job graph and
static contracts with `validate --structural-only`; this mode does not write
hash-namespaced dry-run manifests.

Inspect the dependency graph and future launch commands with:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py plan
python3 scripts/simulations/orchestrate_revision_jobs.py next
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job t100_dynega
```

Every scientific output is namespaced by job ID and configuration hash under
`simulation_outputs/revision_v2/`. Launch and resume remain guarded pending a
separate authorization. See `REVISION_V2_HANDOFF.md` for the boundary, expected
counts, manuscript gates, and exact commands.

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

## Using MAFW

MAFW is used only for the empirical score-series illustration and realism
calibration. The raw videos are not stored in this repository. To rebuild the
MAFW-derived parts of the workflow:

1. Request access from the MAFW site:
   <https://mafw-database.github.io/MAFW/>. Complete the EULA, obtain the
   required signature, and use the approved download links from the dataset
   maintainers.
2. Store the downloaded MAFW clips and labels outside the Git repository. If the
   approved Google Drive package does not include frame data, extract frames
   from the clips; the MAFW FAQ recommends aligned 224 x 224 face frames for
   benchmark-style processing.
3. Score the selected MAFW frames with `transforEmotion` or an equivalent FER
   scoring pipeline. The CSV consumed here needs one row per frame with
   `clip`, `frame_index`, `truth_label`, and these score columns: `anger`,
   `disgust`, `fear`, `happiness`, `neutral`, `sadness`, `surprise`,
   `contempt`, `anxiety`, `helplessness`, and `disappointment`.
4. Put the derived manuscript realism file at
   `simulation_outputs/empirical_link_options/oai-large-100/metric_comparison_long.csv`
   before running `scripts/figures/plot_empirical_realism_results.R`.
5. To regenerate Figure 3 with still frames, also provide the frame-score CSV
   and selected frame images to `scripts/figures/plot_oai_clip_video_dynega_figure.R`
   through `OAI_CLIP_FIGURE_FRAME_SCORES` and
   `OAI_CLIP_FIGURE_FRAME_IMAGE_DIR`.

See `REPRODUCIBILITY.md` for the exact environment variables and commands.
