# Research Log

This is an append-only project log. Add new dated entries at the bottom. Do not rewrite or delete earlier entries; if an earlier entry becomes stale, append a correction with the newer date and source.

## 2026-05-11

### Status snapshot

- Git state before paper-as-code setup was clean on `main...origin/main`.
- Current tracked head was `30ba81b Add revision benchmark report v2`.
- No active DLO benchmark R jobs were found.
- The lab vault synced cleanly before reading DLO R&R notes.

### Current evidence base

- Latest main result scaffold is `reports/revision_benchmark_report_v2.tex` / `.pdf`.
- Current benchmark evidence is in `benchmark_outputs_revision_summary/full_suite_summary.md` and `benchmark_outputs_revision_summary/full_suite_combined_results.csv`.
- Current figure and table bundle is `comparison_revision_report_v2/`.
- Current model-notation scaffold is `paper/gdlo_working_note.tex`.
- Current manuscript strategy note is `/home/atomasevic/lab/methods/dlo-paper-rr.md`.

### Benchmark status

- Combined benchmark rows: 3,960.
- DynEGA: 1,920 ok rows, 0 timeouts, 0 errors.
- GraphicalVAR: 1,872 ok rows, 48 timeouts, 0 errors.
- GIMME: 21 ok rows, 96 timeouts, 3 errors.
- Manuscript implication: DynEGA and GraphicalVAR are the main full-grid comparison; GIMME belongs in a bounded feasibility/sensitivity subsection with timeout accounting.

### Manuscript restructuring decision

- The revised manuscript should be rebuilt as a focused methods paper.
- The core spine is: generalized DLO simulator, empirical realism check, and dimensional-recovery benchmark.
- Tutorial framing, political example framing, and package-centered exposition should move out of the manuscript center.
- PCA remains an open baseline decision because the current final results do not include PCA, even though the benchmark runner supports it.

### Verification notes

- `extract.r` loaded successfully with `options(run_extract_pipeline=FALSE)`.
- `pdftotext` was unavailable, so report v2 was read from the LaTeX source.
- Paper-as-code control files were initialized after reading the repo, latest outputs, and synced lab R&R notes.

### Repository instruction correction

- `AGENTS.md` previously said this workspace snapshot did not include `.git`, but live `git status` showed a real repository on `main...origin/main`.
- The commit guidance was corrected to recognize the git repo and to mention `benchmark_outputs*/` alongside `sim_outputs*/` for regenerated-output disclosure.

## 2026-05-11

### PCA exclusion decision

- Manuscript decision from the Hudson/Alex meeting: do not add PCA to the current revision benchmark.
- Consequence: the benchmark story is DynEGA versus GraphicalVAR as the main full-grid comparison, with GIMME as bounded feasibility/sensitivity.
- Consequence: manuscript prose should not describe PCA as missing work or as a pending baseline unless the team reopens the decision later.
- Control-file update: C11 in `claims-ledger.md` now records PCA exclusion rather than an unsupported PCA-result claim.

## 2026-05-11

### Zeta sign-convention audit

- Investigated the apparent conflict between negative `zeta` benchmark scenarios and positive `zeta` OpenAI-large empirical-realism calibration.
- Source equation in `extract.r` is `x'' = eta * x + zeta * x' + q`; the exact transition matrix uses the same convention.
- Under this equation, stability/damping requires `eta < 0` and `zeta < 0`.
- Current benchmark scenarios are stable damped regimes because they use negative `eta` and negative `zeta`.
- The saved `oai-large-100` winner uses `eta = -0.0500; -0.1500` and `zeta = 0.0800; 0.1200`; under the implemented equation, those are weakly anti-damped / velocity-amplifying regimes over short clips, not damped regimes.
- An older production-estimator section in `extract.r` has incompatible physical-validity warnings that treat negative `zeta` as unphysical and large positive `zeta` as overdamped. This should not be carried into manuscript Methods.
- Added `model-sign-convention-audit.md` and updated the claims/reviewer issue trackers.

### Stable negative-zeta sensitivity

- Ran a separate `oai-large-100` empirical-realism sensitivity constrained to negative `zeta` values.
- Output directory: `empirical_outputs_smoke/mafw-stable-zeta-sensitivity/`.
- Best constrained result: heterogeneous, `eta = -0.0500; -0.1500`, `zeta = -0.2000; -0.6000`, `sigma_q = 0.25`, `dt = 0.5`, total loss `1.6988`.
- Current positive-`zeta` winner remains better on total loss: `1.4974`, delta `0.2014`.
- The stable constrained result is not a collapse; it is plausible but weaker, with worse raw loss and slightly better latent loss.
- Manuscript implication: empirical realism can be discussed as a signed-feedback calibration result, but not as evidence that the best OpenAI-large match is a damped oscillator.

## 2026-05-11

### Signed-zeta smoke benchmark addon

- Added separate runner `run_signed_zeta_smoke_benchmark.R`; it writes only to `benchmark_outputs_signed_zeta_smoke/` and does not touch completed revision benchmark directories.
- Added prebuilt scenario-grid override support to `run_method_benchmark.R`, plus optional passthrough columns for `feedback_regime`, `eta_scalar`, `zeta_scalar`, `finite_growth`, `growth_cap`, and `growth_horizon`.
- Dry-run grid check: 112 candidate cells, 88 retained runnable cells, max retained `finite_growth = 4.874415`.
- Filtering behavior matched the plan: all negative-`zeta` and zero-`zeta` cells retained; `zeta = 0.08` retained for 50 and 100 frames; `zeta = 0.12` retained for 50 frames but excluded for 100 frames; `zeta = 0.20` excluded.
- First GraphicalVAR attempt with the default EBIC/lambda search produced 240-second timeouts, so the signed smoke now uses bounded fixed-lambda GraphicalVAR settings: `lambda_beta = 0.1`, `lambda_kappa = 0.1`, `maxit.in = 25`, `maxit.out = 25`, timeout 90 seconds. The existing revision benchmark defaults remain unchanged unless these env options are set.
- Full smoke output: 176 result rows, all `ok`; 88 DynEGA rows and 88 GraphicalVAR rows; no GIMME or PCA rows.
- Overall smoke means: DynEGA exact recovery `1.00`, assignment accuracy `1.00`, score correlation `0.9416`, NRMSE `0.4725`; fixed-lambda GraphicalVAR exact recovery `0.25`, assignment accuracy `0.5208`, score correlation `0.9374`, NRMSE `0.3952`.
- Regime-level pattern: DynEGA remained exact in damped, critical, and persistent retained cells; GraphicalVAR retained high score correlations but weak dimensional exactness, especially in critical and persistent regimes.
- Manuscript implication: this is a signed-feedback feasibility and pattern-finding smoke only. It supports adding a reviewer-facing sensitivity that positive `zeta` is finite-horizon persistence, not damping, but it should not be merged into the completed revision benchmark until inspected.

## 2026-05-11

### Signed-zeta smoke report pack

- Added `plot_signed_zeta_smoke_report.R` to turn the signed-`zeta` smoke outputs into a figure/table pack.
- Output directory: `comparison_signed_zeta_smoke/`.
- Generated heatmaps for exact recovery, assignment accuracy, score correlation, NRMSE, and runtime across `zeta x eta`, split by method, horizon, and dimension count.
- Generated a feedback-regime summary figure, growth-filter heatmap, raw CSV summaries, LaTeX table snippets, and `signed_zeta_smoke_report.md`.
- The report preserves the key guardrails: this is a smoke result, positive `zeta` is finite-horizon persistence under the growth cap, and GraphicalVAR uses the bounded fixed-lambda smoke setting.

## 2026-05-11

### Signed-zeta replicated benchmark

- Added `run_signed_zeta_replicated_benchmark.R` as a thin wrapper around the signed-`zeta` smoke runner with `SIGNED_ZETA_REPS = 5`, output directory `benchmark_outputs_signed_zeta_reps5/`, and scenario family `signed_zeta_reps5`.
- Updated `run_signed_zeta_smoke_benchmark.R` so retained cells can be expanded across replications while preserving the 88 unique finite-growth cells.
- Dry-run counts: 112 candidate cells, 88 unique retained cells, 440 runnable scenario rows, 5 replications, max retained `finite_growth = 4.874415`.
- Full replicated output: 880 result rows, all `ok`; 440 DynEGA rows and 440 GraphicalVAR rows; no GIMME or PCA rows.
- Overall replicated means: DynEGA exact recovery `1.000`, assignment accuracy `1.000`, score correlation `0.940`, NRMSE `0.472`, runtime `0.92 s`; fixed-lambda GraphicalVAR exact recovery `0.248`, assignment accuracy `0.527`, score correlation `0.936`, NRMSE `0.397`, runtime `0.28 s`.
- Regime-level pattern remains the same as the first smoke: DynEGA is exact across damped, critical, and persistent retained cells; GraphicalVAR keeps high score correlation but weak dimension recovery, especially for critical and persistent regimes.

### Signed-zeta replicated report pack

- Added `plot_signed_zeta_replicated_report.R` as a wrapper around the signed-`zeta` report generator.
- Updated `plot_signed_zeta_smoke_report.R` to support reusable report titles, output filenames, plot-title prefixes, and standard-error intervals for replicated summaries.
- Output directory: `comparison_signed_zeta_reps5/`.
- Generated `signed_zeta_reps5_report.md`, metric heatmaps, growth-filter tables/figures, and the compact multi-panel feedback-regime summary.
- The compact regime plot with mean lines and standard-error bars is the preferred future benchmark-summary style for these method-behavior snapshots.

## 2026-05-11

### Signed-zeta runtime-label correction

- Corrected the signed-`zeta` report wording after noticing that GraphicalVAR appeared faster than DynEGA in the smoke/reps5 plots.
- The signed-`zeta` GraphicalVAR runs use bounded fixed-lambda smoke settings: `lambda_beta = 0.1`, `lambda_kappa = 0.1`, `maxit.in = 25`, `maxit.out = 25`.
- Therefore the signed-`zeta` runtime column is a fixed-lambda smoke runtime only. It should not be interpreted as the full GraphicalVAR cost and should not be used for manuscript-level speed claims.
- The completed full revision benchmark remains the speed reference: DynEGA ok-row mean runtime `3.608 s`; GraphicalVAR ok-row mean runtime `180.908 s`, median `138.288 s`, with 48 GraphicalVAR timeouts at about 600 s.
- Regenerated `comparison_signed_zeta_smoke/` and `comparison_signed_zeta_reps5/` so tables and plots say `Runtime (fixed-lambda smoke)` / `Fixed-lambda runtime sec` and the markdown guardrails explicitly state that these runtime values are not comparable to the full revision benchmark runtime table.

## 2026-05-11

### Signed-zeta summary-graph revision

- Redid the compact feedback-regime summary graph to remove the runtime facet.
- The regenerated summary graph now shows only method-behavior metrics that are comparable under the signed-`zeta` smoke design: exact recovery, assignment accuracy, score correlation, and NRMSE.
- Runtime remains available in the fixed-lambda smoke tables and runtime heatmap, but is no longer part of the main compact summary figure.
- Regenerated `comparison_signed_zeta_smoke/signed_zeta_regime_summary.png` and `comparison_signed_zeta_reps5/signed_zeta_regime_summary.png`.
- Added cache-safe no-runtime filenames: `signed_zeta_regime_summary_no_runtime.png` and `.pdf` in both signed-`zeta` comparison directories.

## 2026-05-11

### Primary signed-feedback benchmark dry-run

- Added `run_signed_feedback_primary_benchmark.R` as the primary signed-feedback benchmark entry point.
- Default primary dry-run settings: `BENCHMARK_OUT_DIR = benchmark_outputs_signed_feedback_primary`, `SIGNED_ZETA_REPS = 20`, `SIGNED_ZETA_SCENARIO_FAMILY = signed_feedback_primary`, `SIGNED_ZETA_DRY_RUN = 1`.
- Updated `run_signed_zeta_smoke_benchmark.R` so fixed-lambda GraphicalVAR defaults are gated by `SIGNED_ZETA_USE_FIXED_GRAPHICALVAR`. The primary wrapper sets this to `0`, preserving proper GraphicalVAR search settings for the next phase.
- Dry-run output: 112 candidate cells, 88 retained unique cells, 1,760 runnable scenario rows, 20 replications, max retained `finite_growth = 4.874415`.
- Primary metadata confirms proper GraphicalVAR-search configuration for later execution: `graphical_var_timeout_sec = 900`, `graphical_var_lambda_beta = NA`, `graphical_var_lambda_kappa = NA`, `graphical_var_maxit_in = NA`, `graphical_var_maxit_out = NA`.
- No model-fitting rows were run in this step; this is the grid/admissibility checkpoint before DynEGA and proper GraphicalVAR execution.

## 2026-05-11

### OpenAI-large retained-100 validation refresh

- Expanded the OpenAI-large MAFW scoring run from the previous 100-manifest sample to `MAFW_SAMPLE_SIZE = 130`, producing 129 scored clips; `02940.mp4` remained the single failed clip.
- Applying the empirical QC rule (`min_complete = 0.60`, `min_length = 15`, `max_gap = 2`) retained 114 of the 129 scored clips.
- Derived `/home/atomasevic/FER-evaluation/results/clip-large/mafw-sample-130-retained100/frame_scores.csv` from the first 100 QC-retained clips by manifest order. The 100th retained clip is `02606.mp4` at sample order 115.
- Reran the empirical DLO check on the retained-100 input with the narrow signed stable-zeta settings: `eta = -0.10`, `zeta = 0.10`, `sigma_q = 0.25`, `dt = 0.50`, `eta_spread = 0.50`, `zeta_spread = 0.20`, `reps = 3`.
- The retained-100 target has empirical `n_dim = 3`; the heterogeneous mode remains the winner with synthetic `n_dim = 3` and community sizes `6-3-2`.
- Report-facing OpenAI validation now uses louvain only and reports lag-1 ACF, total variation, zero-crossing rate, and spectral entropy. All raw checks are partial; latent lag-1 ACF, zero-crossing rate, and spectral entropy are partial; latent total variation fails.
- The optional CD sensitivity script now records skipped diagnostic readouts instead of aborting. In this retained-100 run, walktrap is skipped after dropping `disappointment.Ord0`; this is not used in the report-facing OpenAI table.

## 2026-05-11

### OpenAI-large validation metric refinement

- Refined the report-facing retained-100 OpenAI validation metrics to four interpretable checks: lag-1 ACF, derivative IQR, run-length entropy, and spectral entropy.
- Updated the pass threshold from `0.25` to `0.33` maximum scaled discrepancy, interpreted as within roughly one-third of empirical dispersion; `Partial` remains `<= 1.00`, and `Fail` remains `> 1.00`.
- Added `diff_iqr` and `run_length_entropy` to the shared empirical metric calculator in `extract.r`, then reran the retained-100 empirical DLO pipeline.
- The heterogeneous mode remains the winner: empirical `n_dim = 3`, synthetic `n_dim = 3`; total loss after adding the new metrics is `1.2998`.
- Updated retained-100 louvain report table: raw lag-1 ACF, raw derivative IQR, raw run-length entropy, and latent run-length entropy pass; raw spectral entropy, latent lag-1 ACF, latent derivative IQR, and latent spectral entropy are partial; no report-facing metric fails.
- Mean total variation is no longer a headline validation metric; keep it as a stricter amplitude sensitivity check because it is more affected by large synthetic movement excursions.

## 2026-05-11

### Effective-rank geometry check added

- Added effective rank to the report-facing OpenAI validation description and tables as the multivariate geometry check.
- Effective rank is partial in both raw and louvain DynEGA score spaces: raw empirical `5.114` versus DLO `6.139`, max discrepancy `0.76`; latent empirical `2.371` versus DLO `2.812`, max discrepancy `0.79`.
- The full dynamical-system interpretation now separates univariate trajectory behavior from multivariate geometry: lag-1 ACF for persistence, derivative IQR for robust local movement, run-length entropy for directional-regime complexity, spectral entropy for frequency complexity, and effective rank for dimensional spread.
- The louvain structural readout remains matched at empirical and synthetic `n_dim = 3`, community sizes `6-3-2`.
- Dynamical-system interpretation: the calibrated DLO simulation best reproduces the qualitative regime shape of the OpenAI-large trajectories rather than exact trajectory-level or full distributional equivalence. Raw lag-1 ACF passes, so short-memory persistence is close in the observed emotion-score space; latent lag-1 ACF is partial, so latent persistence is directionally right but not exact. Derivative IQR passes in raw space and is partial in latent space, implying that typical local step-to-step movement is well approximated, while stricter amplitude summaries such as mean total variation still indicate overly large synthetic excursions. Run-length entropy passes in both raw and latent spaces, supporting the claim that the simulator captures directional episode complexity rather than producing purely alternating or purely drifting paths. Spectral entropy is partial in both spaces, so frequency-domain complexity is broadly but not exactly matched. Effective rank is partial while louvain dimensionality and community sizes match exactly, meaning the simulator recovers the coarse three-mode latent organization but spreads variance somewhat more broadly across modes than the empirical panel.
- Reviewer-facing summary: the simulated scores approximate empirical dynamics well for local persistence, typical movement scale, directional-regime complexity, broad spectral complexity, and coarse three-dimensional organization. The main limitations are amplitude tails and exact latent geometry: large movements are too strong, and variance is slightly too dispersed across modes.

## 2026-05-11

### Primary signed-feedback DynEGA pass

- Ran the first method-fitting step of the primary signed-feedback benchmark with DynEGA only:
  `SIGNED_ZETA_DRY_RUN=0 BENCHMARK_METHODS=dynEGA BENCHMARK_OUT_DIR=benchmark_outputs_signed_feedback_primary_dynega Rscript run_signed_feedback_primary_benchmark.R`.
- Output directory: `benchmark_outputs_signed_feedback_primary_dynega/`.
- Result count: 1,760 rows, all `ok`; no GraphicalVAR, GIMME, or PCA rows.
- Grid/admissibility check remained unchanged from the dry run: 112 candidate cells, 88 retained unique cells, 20 replications per retained cell, max retained `finite_growth = 4.874415`.
- Overall DynEGA means by feedback regime:
  - damped: exact recovery `1.000`, assignment accuracy `1.000`, score correlation `0.908`, NRMSE `0.655`, runtime `1.688 s`.
  - critical: exact recovery `1.000`, assignment accuracy `1.000`, score correlation `0.964`, NRMSE `0.352`, runtime `1.694 s`.
  - persistent: exact recovery `1.000`, assignment accuracy `1.000`, score correlation `0.988`, NRMSE `0.181`, runtime `1.690 s`.
- The first primary result supports the signed-feedback extension for DynEGA: retained positive-`zeta` finite-horizon persistence did not degrade dimensional recovery or assignment recovery in this grid.
- Next benchmark step should be a proper GraphicalVAR-search pilot on a representative subset before committing to the full 1,760-row GraphicalVAR run.

## 2026-05-11

### Primary signed-feedback GraphicalVAR pilot

- Added `run_signed_feedback_graphicalvar_pilot.R` as a proper-search GraphicalVAR pilot wrapper.
- The pilot subset has 12 cells: `2D/4D x 50/100` crossed with damped, critical, and persistent regimes, using `eta = -0.15`, `zeta = -0.30` for damped, `zeta = 0.00` for critical, `zeta = 0.12` for 50-frame persistent cells, and `zeta = 0.08` for 100-frame persistent cells.
- Dry-run verified that all pilot cells satisfy `finite_growth <= 5`; max pilot `finite_growth = 4.874415`.
- Metadata confirms proper GraphicalVAR search rather than fixed-lambda smoke settings: `lambda_beta = NA`, `lambda_kappa = NA`, `maxit.in = NA`, `maxit.out = NA`, timeout `900 s`.
- Launched the actual pilot and stopped it after the first parallel chunk produced enough evidence: both first cells timed out at about `900.5 s`.
- Checkpoint evidence:
  - `signed_zeta_damped_eta15_neg30_2d_t50`: timeout, `900.541 s`.
  - `signed_zeta_critical_eta15_zero_2d_t50`: timeout, `900.534 s`.
- Interpretation: a full 1,760-row primary GraphicalVAR run with proper search is not currently feasible as a routine benchmark step. The next GraphicalVAR step should be a design decision, not a blind launch: either reduce the proper-search scope to a manuscript-justified representative subset, tune/diagnose `mlGraphicalVAR()` search settings separately, or report GraphicalVAR for the signed-feedback extension as bounded feasibility rather than a full primary grid.

## 2026-05-11

### GraphicalVAR comparator decision

- Added `graphicalvar-comparator-policy.md`.
- Decision: GraphicalVAR remains in the manuscript as a comparator.
- Distinction now locked:
  - completed revision benchmark: DynEGA and GraphicalVAR are full-grid comparators.
  - signed-feedback extension: DynEGA is the full-grid primary result; GraphicalVAR is a resource-bounded comparator with timeout accounting.
- Fixed-lambda signed-zeta GraphicalVAR outputs remain sensitivity/feasibility checks only and must not be used for method-level runtime claims.
- Updated `benchmark_reporting_policy.md`, `paper-plan.md`, `reviewer-issues.md`, `claims-ledger.md`, and `analysis-map.md` so the comparator role is explicit and reviewer-facing.

## 2026-05-11

### Signed-feedback main benchmark correction

- Corrected the benchmark framing: positive `zeta` is not an extension in the strict sense. It is part of the main signed-feedback simulation benchmark.
- Added `signed-feedback-main-benchmark-plan.md`.
- New design policy: GraphicalVAR and GIMME must be attempted on standardized signed-feedback comparator cells before the manuscript can claim multi-method positive-`zeta` coverage.
- Initial feasibility grid:
  - `eta = -0.15, -0.30`
  - `zeta = -0.30, 0.00, 0.08`
  - `nfac = 2, 4`
  - `TimeP = 50, 100`
  - `N = 16`, `NVAR = 3`, `Error = 0.025`, `Loadings = 0.6`, `Q = 0.75`
  - homogeneous feedback, single-expression schedule, `finite_growth <= 5`
- This yields 24 unique cells before replications and keeps damped, critical, and positive-`zeta` persistent regimes balanced across horizons and dimensionality.
- The earlier fixed-lambda GraphicalVAR signed-zeta smoke remains useful only as sensitivity/feasibility evidence. It does not satisfy the requirement for proper comparator coverage in the main signed-feedback benchmark.
- Updated `graphicalvar-comparator-policy.md`, `benchmark_reporting_policy.md`, `paper-plan.md`, `reviewer-issues.md`, `claims-ledger.md`, and `analysis-map.md` to reflect this correction.

### Signed-feedback condition-count correction

- Rechecked `paper/Paper_emotion_dynamics_HGolino.Rmd`.
- The original manuscript simulation described 216 analysis conditions and 500 replicates; the current completed revision benchmark has 192 condition cells and 10 reps, yielding 1,920 rows per main method.
- Therefore, the 24-cell signed-feedback grid should not be described as the main benchmark. It is only a feasibility layer for slow comparators.
- Updated `signed-feedback-main-benchmark-plan.md` so the main signed-feedback design is a broader 432-cell factorial grid before replications:
  - `zeta = -0.30, 0.00, 0.08`
  - `eta = -0.05, -0.15, -0.30`
  - `nfac = 2, 3, 4`
  - `N = 50, 100`
  - `TimeP = 50, 100`
  - `NVAR = 3, 4`
  - `Error = 0.025, 0.050`
  - `Loadings = 0.6`, `Q = 0.75`
- The benchmark logic is now: broad main factorial design for scientific coverage, plus a smaller 24-cell feasibility layer to determine how far GraphicalVAR and GIMME can be run without hiding their computational limits.

### Standardized signed-feedback runner dry-run

- Added `run_signed_feedback_standardized_benchmark.R`.
- Patched `run_method_benchmark.R` so a custom prebuilt scenario grid is respected for GIMME instead of forcing GIMME back onto the old bounded grid.
- Patched result-row metadata so signed-feedback rows can carry `benchmark_grid` and use `comparison_scope = signed_feedback_main` or `signed_feedback_feasibility`.
- Dry-run command: `SIGNED_FEEDBACK_DRY_RUN=1 Rscript run_signed_feedback_standardized_benchmark.R`.
- Output directory: `benchmark_outputs_signed_feedback_standardized/`.
- Dry-run outputs:
  - main conditions: 432; main rows at 1 rep: 432.
  - feasibility conditions: 24; feasibility rows at 1 rep: 24.
  - all main and feasibility cells retained under `finite_growth <= 5`.
  - max finite growth: 4.874415.
  - main regimes balanced: 144 damped, 144 critical, 144 persistent.
  - feasibility regimes balanced: 8 damped, 8 critical, 8 persistent.
- Metadata confirms methods are `dynEGA,graphicalVAR,gimme`, proper GraphicalVAR search settings remain `NA` for fixed-lambda overrides, and GIMME timeout is 900 seconds.
- No fitting results were produced in this dry-run step.

### Feasibility-grid sample-size correction

- Stopped the first standardized feasibility run after realizing the feasibility layer had inherited `N = 16` from the old signed-zeta smoke grid.
- Correction: the feasibility layer should use `N = 50, 100` and `TimeP = 100`, preserving the 24-cell size while aligning with the original/current benchmark tradition.
- Updated `run_signed_feedback_standardized_benchmark.R` and `signed-feedback-main-benchmark-plan.md`.
- Interpretation: the earlier partially executed `N = 16` feasibility checkpoint is superseded and should not be used for manuscript conclusions.

### Corrected standardized signed-feedback feasibility run

- Reran the standardized feasibility layer on the corrected grid: `N = 50, 100`, `TimeP = 100`, `nfac = 2, 4`, `eta = -0.15, -0.30`, `zeta = -0.30, 0.00, 0.08`, `NVAR = 3`, `Error = 0.025`, `Loadings = 0.6`, `Q = 0.75`.
- Output directory: `benchmark_outputs_signed_feedback_standardized/`.
- Grid check: 24 feasibility cells, 432 main-grid cells written for later use, max feasibility `finite_growth = 4.874415`, and all feasibility rows satisfy `finite_growth <= 5`.
- Result rows: 72 total, equal to 24 corrected cells crossed with DynEGA, proper-search GraphicalVAR, and GIMME.
- DynEGA completed all 24 corrected feasibility rows with status `ok`.
- Proper-search GraphicalVAR timed out on all 24 corrected feasibility rows at the configured 900-second cap.
- GIMME timed out on all 24 corrected feasibility rows at the configured 900-second cap.
- Regime balance is preserved in the results: each method has 8 damped, 8 critical, and 8 persistent rows.
- Manuscript implication: the corrected feasibility layer now satisfies the comparator-coverage requirement at the attempt level. The evidence supports separating recovery performance from computational tractability: DynEGA is feasible on the corrected grid, while proper-search GraphicalVAR and GIMME are not feasible under the current 900-second per-cell cap.

### Comparator timeout root-cause correction

- Investigated the 100% GraphicalVAR and GIMME timeout pattern from `benchmark_outputs_signed_feedback_standardized/`.
- Root cause: the forked hard-timeout helper based on `parallel::mcparallel()` produces false comparator timeouts in this environment.
- GraphicalVAR evidence:
  - hard-timeout wrapper timed out even fixed-lambda signed-feedback calls.
  - same-process fixed-lambda GraphicalVAR completed in 1.422 seconds for `N = 16` and 0.346 seconds for `N = 50`.
  - same-process proper-search GraphicalVAR completed in 29.219 seconds for `N = 16` and 33.542 seconds for `N = 50`.
  - patched GraphicalVAR path completed a corrected `N = 50` proper-search diagnostic cell in 34.337 seconds.
  - partial corrected GraphicalVAR rerun in `benchmark_outputs_signed_feedback_standardized_graphicalvar_fixed/` produced 8 `ok` rows before being stopped as an exploratory run.
- GIMME evidence:
  - same-process GIMME completed at `N = 4` in 38.637 seconds and `N = 16` in 139.916 seconds.
  - the forked hard-timeout path timed out for both `N = 4` and `N = 16` at a 240-second cap, so those forked timeouts are false.
  - an external `Rscript` timeout path completed a GIMME `N = 4` diagnostic in 14.653 seconds.
- Code changes:
  - GraphicalVAR now uses the same-process elapsed-time path instead of the forked hard-timeout path.
  - GIMME now uses `run_external_structure_fit.R` under GNU `timeout` instead of the unsafe forked hard-timeout path.
- Correction: the previous 24 GraphicalVAR timeouts and 24 GIMME timeouts in `benchmark_outputs_signed_feedback_standardized/` should not be used as estimator-infeasibility evidence. DynEGA rows from that run remain valid.

### GraphicalVAR documentation check

- Checked the CRAN `graphicalVAR` package documentation and the installed `mlGraphicalVAR()` / `tsData()` source for the current wrapper.
- The current use of `mlGraphicalVAR()` is conceptually correct for the benchmark: pooled fixed-effect multi-subject estimation with `vars = item_names`, `idvar = "id"`, `scale = TRUE`, `centerWithin = TRUE`, `gamma = 0.5`, and `subjectNetworks = FALSE`.
- `subjectNetworks = FALSE` is appropriate because the benchmark only needs the fixed-effect structure, not per-subject networks.
- The wrapper uses `fixedPCC`, i.e. the fixed contemporaneous partial-correlation network, as the community-detection matrix. This is defensible for latent measurement-block recovery, but should be described as contemporaneous GraphicalVAR structure rather than as a combined temporal/contemporaneous result.
- Documentation check: `beepvar` is optional and is created automatically when missing; `dayvar` defaults to a single day when missing. In our simulated panel, implicit `beepvar` and explicit `beepvar = "time"` produce identical lagged/current data matrices.
- Code cleanup: `fit_graphical_var_structure()` now passes `beepvar = "time"` explicitly so the time ordering is visible in the benchmark code.
- No evidence was found that missing `beepvar` or `dayvar` caused the timeout problem. The timeout problem remains attributable to the old forked timeout backend.

## 2026-05-12

### Corrected GraphicalVAR feasibility comparator

- Completed the 12-hour guarded proper-search GraphicalVAR feasibility run well inside the wall-clock budget.
- Output directory: `benchmark_outputs_signed_feedback_standardized_graphicalvar_12h/`.
- Run scope: corrected 24-cell feasibility grid only, with `N = 50,100`, `TimeP = 100`, `nfac = 2,4`, `eta = -0.15,-0.30`, and `zeta = -0.30,0.00,0.08`.
- Result count: 24 GraphicalVAR rows, all `ok`; no timeouts and no errors.
- Runtime: started at 2026-05-11 21:10:49 and finished at 2026-05-11 22:31:09, about 1 hour 20 minutes.
- Slowest corrected GraphicalVAR cells were the 4D persistent cells: `540.1 s` for `eta = -0.30, zeta = 0.08` at `N = 100`, and `558.1 s` for the matching `N = 50` cell.
- Manuscript implication: GraphicalVAR is now a valid proper-search comparator for the corrected signed-feedback feasibility layer. The earlier timeout rows are superseded for GraphicalVAR.

### Signed-feedback DynEGA versus GraphicalVAR comparison pack

- Added `plot_signed_feedback_feasibility_comparison.R`.
- Output directory: `comparison_signed_feedback_feasibility/`.
- Inputs:
  - DynEGA valid rows from `benchmark_outputs_signed_feedback_standardized/benchmark_results.csv`.
  - GraphicalVAR valid proper-search rows from `benchmark_outputs_signed_feedback_standardized_graphicalvar_12h/benchmark_results.csv`.
- Combined comparison: 48 `ok` rows, 24 DynEGA and 24 GraphicalVAR, matched on the same corrected feasibility cells.
- Both methods achieved exact dimensional recovery and assignment accuracy of `1.000` in all corrected feasibility cells.
- Common-score correlation and NRMSE are identical across methods because both methods recovered the same item partitions: overall score correlation `0.945`, NRMSE `0.404`.
- Runtime is the differentiating result: DynEGA mean runtime `1.6 s`, max `2.2 s`; GraphicalVAR mean runtime `200.5 s`, max `558.1 s`.
- GraphicalVAR runtime multiplier over DynEGA ranged from `24.0x` to `402.4x`, with the largest ratios in 4D persistent cells.
- Generated plots:
  - `signed_feedback_feasibility_regime_summary.png`
  - `signed_feedback_feasibility_runtime.png`
  - `signed_feedback_feasibility_runtime_ratio_heatmap.png`
  - `signed_feedback_feasibility_cell_metric_heatmap.png`
- Interpretation: the corrected feasibility layer now supports the substantive claim that GraphicalVAR can recover the signed-feedback feasibility structure under proper search, but at much higher computational cost than DynEGA.

### Pre-DynEGA-432 simulation output archive

- Archived prior simulation benchmark outputs before starting the clean 432-cell signed-feedback DynEGA grid.
- Archive directory: `archives/2026-05-12-pre-dynega-432-simulation-results/`.
- Archived root-level generated simulation directories matching `benchmark_outputs*`, `comparison_signed*`, and `sim_outputs*`.
- Archived directories include the completed revision benchmark outputs, signed-zeta smoke/reps outputs, standardized signed-feedback feasibility outputs, corrected GraphicalVAR feasibility outputs, and signed-feedback comparison plot packs.
- Wrote archive indexes:
  - `archives/2026-05-12-pre-dynega-432-simulation-results/archive_manifest.tsv`
  - `archives/2026-05-12-pre-dynega-432-simulation-results/archive_directories.txt`
- Left report/empirical-validation artifacts in place: `comparison_revision_report_v2/`, `comparison_revision_report_v2_retained100/`, and `empirical_outputs_smoke/`.
- Clean-slate check: no root-level `benchmark_outputs*`, `comparison_signed*`, or `sim_outputs*` directories remain.

### DynEGA 432-cell signed-feedback main benchmark

- Launched the clean 432-cell signed-feedback main-grid DynEGA run as a detached `nohup`/`setsid` background process.
- Output directory: `benchmark_outputs_signed_feedback_main_dynega_432/`.
- Run command used `SIGNED_FEEDBACK_RUN_GRID=main`, `SIGNED_FEEDBACK_MAIN_REPS=1`, `BENCHMARK_METHODS=dynEGA`, `BENCHMARK_WORKERS=8`, `BENCHMARK_RESUME=1`, and `BENCHMARK_CHECKPOINT_EACH=1`; full command is saved in `run.command.txt`.
- Result count: 432 final rows and 432 checkpoint rows, all `dynEGA`, all `ok`.
- Grid coverage: `N = 50,100`; `TimeP = 50,100`; `scenario_nfac = 2,3,4`; `NVAR = 3,4`; `Error = 0.025,0.05`; `Loadings = 0.6`; `Q = 0.75`; `eta = -0.30,-0.15,-0.05`; `zeta = -0.30,0.00,0.08`.
- Feedback-regime balance: 144 damped rows, 144 critical rows, and 144 persistent rows.
- Growth filter check: all rows satisfy `finite_growth <= 5`; observed maximum `finite_growth = 4.874415`.
- Recovery summary: exact dimension recovery `1.000`, assignment accuracy `1.000`, mean common-score correlation `0.954`, and mean common-score NRMSE `0.392`.
- Runtime summary: summed fitted-row runtime `970.403 s`, mean `2.246 s`, maximum `3.584 s`.
- Interpretation: DynEGA now has a complete main-grid signed-feedback baseline covering negative, zero, and positive `zeta` as part of the primary benchmark design. The next comparator step should be planned against this exact 432-cell grid rather than against the earlier feasibility layer.

### Correction: fixed-measurement signed-feedback baseline, not final primary benchmark

- Rechecked the signed-feedback 432-cell grid against the earlier manuscript simulation design after noting that a serious benchmark should vary measurement strength and dynamic shock/forcing strength.
- The completed 432-cell DynEGA run fixed `Loadings = 0.6` and `Q = 0.75`; only `eta`, `zeta`, number of factors, sample size, horizon, variables per factor, and measurement error were varied.
- The older manuscript simulation explicitly varied dynamic factor loadings (`0.4, 0.6, 0.8`) and measurement/error conditions; the current signed-feedback grid lost that layer when it was built from the signed-zeta smoke/feasibility design.
- Correction to interpretation: `benchmark_outputs_signed_feedback_main_dynega_432/` is useful as a fixed-measurement signed-feedback DynEGA baseline and a grid/admissibility check, but it should not be called the final primary simulation benchmark.
- Next primary benchmark design should restore variation in `Loadings` and `Q` while keeping signed-feedback regimes (`zeta < 0`, `zeta = 0`, finite-horizon `zeta > 0`) in the main factorial grid.

### Expanded signed-feedback primary-grid dry run

- Updated `run_signed_feedback_standardized_benchmark.R` so the main signed-feedback grid varies measurement loadings, measurement-error variance, and dynamic forcing strength.
- Main grid now crosses:
  - `eta = -0.05,-0.15,-0.30`
  - `zeta = -0.30,0.00,0.08`
  - `scenario_nfac = 2,3,4`
  - `N = 50,100`
  - `TimeP = 50,100`
  - `NVAR = 3,4`
  - `Error variance = 0.125^2,0.25^2,0.50^2`
  - `Loadings = 0.4,0.6,0.8`
  - `Q = 0.50,0.75,1.00`, with `sigma_q = 0.25 * (Q / 0.75)`.
- Dry-run output directory: `benchmark_outputs_signed_feedback_main_expanded_dryrun/`.
- Dry-run count: 5,832 main candidate cells and 5,832 retained main rows at one replication; all rows satisfy `finite_growth <= 5`, with maximum `finite_growth = 4.874415`.
- Scenario labels were expanded from three feedback regimes to nine dynamic scenarios: weak/moderate/strong restoring crossed with damped/critical/persistent feedback. Each dynamic scenario has 648 rows.
- Regime balance remains preserved: 1,944 damped rows, 1,944 critical rows, and 1,944 persistent rows.
- Interpretation: this dry-run grid is the first manuscript-serious signed-feedback primary-grid candidate. The eta/zeta combinations provide a meaningful dynamic-scenario layer, but they are still homogeneous no-switching scenarios; expression-schedule heterogeneity should be treated as a separate later scenario layer if needed.

### DynEGA 10-rep expanded signed-feedback benchmark launch

- Locked and launched the expanded signed-feedback primary DynEGA benchmark at 10 replications.
- Output directory: `benchmark_outputs_signed_feedback_main_dynega_reps10/`.
- Detached command uses `nohup setsid`, `taskset -c 0-15`, `BENCHMARK_WORKERS=12`, `BENCHMARK_RESUME=1`, and `BENCHMARK_CHECKPOINT_EACH=1`; full command is saved in `run.command.txt`.
- Run PID: `1149729`.
- Intended row count: 5,832 main cells x 10 replications = 58,320 DynEGA rows.
- Metadata check confirms `selected_runnable_rows = 58320`, `selected_reps = 10`, `methods = dynEGA`, and the expanded grid with varied `Error`, `Loadings`, and `Q`.
- Initial checkpoint check: 96 rows written, all `dynEGA`, all `ok`.
- Process check: PID `1149729` is running with CPU affinity `0-15`.

### DynEGA 10-rep expanded signed-feedback benchmark complete

- Completed the expanded signed-feedback primary DynEGA benchmark.
- Output directory: `benchmark_outputs_signed_feedback_main_dynega_reps10/`.
- Final result count: 58,320 rows, all `dynEGA`, all `ok`.
- Grid coverage: 5,832 main cells x 10 replications, with varied `Error`, `Loadings`, and `Q`.
- Feedback-regime balance: 19,440 damped rows, 19,440 critical rows, and 19,440 persistent rows.
- Dynamic-scenario balance: all nine weak/moderate/strong restoring x damped/critical/persistent scenarios have 6,480 rows.
- Growth filter check: all rows satisfy `finite_growth <= 5`; observed maximum `finite_growth = 4.874415`.
- Recovery summary: exact dimension recovery `1.000`, assignment accuracy `1.000`, mean common-score correlation `0.943`, and mean common-score NRMSE `0.476`.
- Runtime summary: summed fitted-row runtime `343,506.5 s`, mean `5.890 s`, maximum `28.142 s`.
- Interpretation: this is now the completed full DynEGA side of the locked primary signed-feedback simulation benchmark. Comparator planning should use this grid as the design reference, but GraphicalVAR/GIMME feasibility must be handled as a compute-design decision rather than by changing the benchmark grid again.

### GraphicalVAR nLambda pilot launch

- Added `BENCHMARK_GRAPHICALVAR_NLAMBDA` support to the GraphicalVAR wrapper.
- The wrapper now passes `nLambda` through to `graphicalVAR::mlGraphicalVAR()` when set, while preserving the default package behavior when unset.
- Added deterministic signed-feedback pilot subsetting with `SIGNED_FEEDBACK_PILOT_ROWS`; the pilot uses 180 fitted cells balanced across feedback regimes.
- Launched a detached sequential GraphicalVAR pilot comparing `nLambda = 10`, `nLambda = 20`, and package default on the same 180 signed-feedback main-grid cells.
- Community detection is tested efficiently as post-fit variants: one `mlGraphicalVAR()` fit per simulated cell, then louvain, walktrap, and leiden on the same `fixedPCC` matrix.
- Output wrapper directory: `benchmark_outputs_graphicalvar_nlambda_pilot/`.
- Per-setting output directories:
  - `benchmark_outputs_graphicalvar_nlambda_pilot_10/`
  - `benchmark_outputs_graphicalvar_nlambda_pilot_20/`
  - `benchmark_outputs_graphicalvar_nlambda_pilot_default/`
- Run PID: `2980109`.
- Initial `nLambda = 10` checkpoint: 20 fitted cells produced 60 rows, all `ok`, with 20 rows each for louvain, walktrap, and leiden.

### Simulation analysis reset

- Stopped the active signed-feedback / GraphicalVAR `nLambda` pilot process group after deciding the terminology and grid were pulling the analysis away from the intended manuscript question.
- Also terminated stale orphaned R workers in `/home/atomasevic/dlo` that had been sleeping for about 15 days.
- Reset the manuscript-facing benchmark aim: extend the old emotion-dynamics simulation from `paper/Paper_emotion_dynamics_HGolino.Rmd` / `paper/Script_Simulation_Dynamics_Emotion_2023-readout.md` to DynEGA, GraphicalVAR, and bounded GIMME.
- The active design should test 2, 3, and 4 measured factors, remove neutrality from the benchmark-generating model, and add a small named set of OpenAI-results-inspired scenarios.
- Marked signed-feedback / signed-`zeta` outputs as exploratory only; they should not define the manuscript grid and should not be relaunched unless explicitly revived as a separate sensitivity analysis.
- Added `simulation-analysis-reset.md` and updated `paper-plan.md`, `benchmark_reporting_policy.md`, and `README.md` to make this reset durable.

### Reset emotion-dynamics DynEGA launch

- Added `run_emotion_dynega_benchmark.R` as the first reset-run entry point.
- The DynEGA phase uses the old Rmd prose grid as the anchor: `N = 50,100`, `NVAR = 4` core plus `NVAR = 6` sensitivity, `NFAC = 2,3,4`, `Error_sd = 0.125,0.25,0.50` with `Error_variance = Error_sd^2` in the generator, and `Loadings = 0.4,0.6,0.8`.
- Neutrality is removed from the benchmark generator: each panel simulates only the measured latent factors and their observed item blocks.
- Scenario layer: four old translated expression scenarios (`old_low_single_stable`, `old_high_single_stable`, `old_high_switch_stable`, `old_high_switch_volatile`) plus two OpenAI-results-inspired scenarios (`oai_ranked_persistence`, `oai_broad_geometry`).
- Dry-run grid count: 1,134 simulated panels and 3,402 DynEGA result rows after louvain, leiden, and walktrap.
- Smoke run completed with 36/36 DynEGA rows `ok`.
- Launched the full detached DynEGA run with `EMOTION_DYNEGA_REPS=3`, `EMOTION_DYNEGA_WORKERS=12`, `BENCHMARK_CHECKPOINT_EACH=1`, and `BENCHMARK_RESUME=1`.
- Output directory: `benchmark_outputs_emotion_dynega/`.
- Run PID: `117727`.

### Reset emotion-dynamics DynEGA complete

- Completed the detached DynEGA reset benchmark in `benchmark_outputs_emotion_dynega/`.
- Final result count: 3,402 rows, all `ok`.
- Coverage: 1,134 rows each for louvain baseline, leiden, and walktrap; 1,134 rows each for true factor counts 2, 3, and 4.
- Grid-scope coverage: 2,916 rows for `core_NVAR4_TimeP100`, 324 rows for `sensitivity_NVAR6`, and 162 rows for `sensitivity_TimeP300`.
- Overall exact dimension recovery: louvain baseline `0.948`, leiden `0.949`, walktrap `0.929`.
- Assignment accuracy was stable across variants: louvain baseline `0.956`, leiden `0.956`, walktrap `0.955`.
- The OpenAI-inspired scenarios recovered exactly across 2, 3, and 4 factors for all three DynEGA community algorithms; most misses were concentrated in `old_low_single_stable`, especially low-loading or high-error cells.
- Mean runtime per row: louvain baseline `2.69 s`, leiden `1.29 s`, walktrap `1.21 s`.

### Reset emotion-dynamics GraphicalVAR launch

- Added `run_emotion_graphicalvar_benchmark.R` as the reset GraphicalVAR entry point, reusing the same measured-factor grid as `run_emotion_dynega_benchmark.R`.
- GraphicalVAR dry-run count: 1,134 structure-fit panels and 3,402 method/community rows after louvain, leiden, and walktrap post hoc community detection on the same `fixedPCC` structure.
- Resource finding: an uncapped one-worker GraphicalVAR smoke spawned 49 threads and used most CPU cores, so full runs must use explicit CPU affinity.
- Patched `run_method_benchmark.R` so GraphicalVAR structure fits use the external hard-timeout helper, preventing compiled `mlGraphicalVAR()` work from bypassing elapsed-time limits.
- Capped smoke run used `taskset -c 0-3`, one worker, and `EMOTION_GRAPHICALVAR_TIMEOUT_SEC=120`; it completed with 36 rows, 24 `ok` and 12 `timeout`, verifying checkpointing and hard-timeout behavior.
- Launched the full detached GraphicalVAR reset run with `taskset -c 0-7`, `EMOTION_GRAPHICALVAR_WORKERS=2`, `EMOTION_GRAPHICALVAR_TIMEOUT_SEC=900`, `BENCHMARK_CHECKPOINT_EACH=1`, and `BENCHMARK_RESUME=1`.
- Output directory: `benchmark_outputs_emotion_graphicalvar/`.
- Run PID: `797314`.

### Reset emotion-dynamics GraphicalVAR stopped at 4D timeout boundary

- Monitored the detached GraphicalVAR reset run through the old benchmark families and the OpenAI-inspired ranked-persistence 2D/3D panels.
- Stopped the process group intentionally at the first systematic 4D timeout boundary to avoid burning additional CPU on repeated 900-second failures.
- Final checkpoint before stop: 2,280 / 3,402 rows recorded in `benchmark_outputs_emotion_graphicalvar/checkpoint_results.csv`.
- Status count: 2,268 `ok`, 12 `timeout`.
- Completed coverage before the stop: all old translated expression scenarios for 2D/3D/4D; all `oai_ranked_persistence_2d`; all `oai_ranked_persistence_3d`.
- The first `oai_ranked_persistence_4d` cells timed out systematically: `N = 50`, `TimeP = 100`, `NVAR = 4`, `Error_sd = 0.125`, `Loadings = 0.4` for reps 1-3, plus `Loadings = 0.6` rep 1, across louvain baseline, leiden, and walktrap rows.
- Resource status during monitoring: CPU was bounded by `taskset -c 0-7`; memory remained unconstrained; sampled `vmstat` readings showed no sustained I/O wait; output directory size was about 1.5 MB.
- Interim interpretation: bounded GraphicalVAR is feasible for the old benchmark grid and for OpenAI-inspired 2D/3D ranked-persistence cases, but 4D ranked-persistence is not computationally feasible under the 900-second per-fit cap. Treat this as a feasibility/result boundary, not a failed run to blindly continue.

### Reset emotion-dynamics method coverage decision

- Manuscript-facing coverage status is now: DynEGA on the full reset grid; GraphicalVAR on the computationally feasible subset.
- DynEGA is the only completed all-cell result so far: 3,402 / 3,402 rows `ok`, covering all simulated panels, all 2/3/4 measured-factor conditions, all old translated expression scenarios, both OpenAI-inspired scenarios, and all three community algorithms (`louvain`, `leiden`, `walktrap`).
- GraphicalVAR should not be described as a failed full-grid run. It should be described as a bounded comparator evaluated until the pre-specified computational feasibility rule was met.
- GraphicalVAR completed the old translated expression benchmark for 2D/3D/4D and completed `oai_ranked_persistence` for 2D/3D. The retained GraphicalVAR analytic subset is therefore the 2,268 `ok` rows in `benchmark_outputs_emotion_graphicalvar/checkpoint_results.csv`.
- The 12 GraphicalVAR `timeout` rows are retained as feasibility evidence for the 4D ranked-persistence stress condition, not as dimension-recovery failures.
- Reporting rule: compare DynEGA and GraphicalVAR directly only on the GraphicalVAR-completed subset; report DynEGA full-grid results separately; report `oai_ranked_persistence_4d` for GraphicalVAR as computationally infeasible under the 900-second per-fit cap.
- Next method should follow the same bounded-comparator principle: GIMME should begin with feasibility gates and should not be allowed to silently redefine the grid or consume compute on repeated timeout regions.

### Reset emotion-dynamics GIMME feasibility gate

- Added `run_emotion_gimme_benchmark.R` as the bounded reset GIMME entry point, reusing the same measured-factor grid as `run_emotion_dynega_benchmark.R`.
- GIMME is evaluated as one `gimmeSEM()` structure fit per simulated panel, with louvain, leiden, and walktrap applied as post-fit community variants to the same recovered structure.
- Dry-run grid count: 1,134 structure-fit panels and 3,402 method/community rows after louvain, leiden, and walktrap.
- The wrapper writes the old-vs-new grid comparison artifacts and tags each panel with an explicit staged feasibility tier so the analysis does not silently drift into a different grid.
- Smoke gate command used one worker, `taskset -c 0-3`, BLAS/OpenMP thread caps, and `EMOTION_GIMME_TIMEOUT_SEC=300`.
- Smoke gate result: 9 rows recorded for 3 structure fits; 6 rows `ok`, 3 rows `timeout`.
- Completed smoke cells: `old_high_switch_volatile_3d` in about 256 seconds and `old_low_single_stable_2d` in about 62 seconds.
- Timed-out smoke cell: `oai_ranked_persistence_2d` at the 300-second cap.
- Resource status during smoke monitoring: memory stayed unconstrained at about 110 GiB available, sampled `vmstat` showed no sustained I/O wait, and CPU use stayed bounded by the single external fit.
- Decision: a full unbounded GIMME grid is not justified, but a bounded staged run with one worker, hard per-fit timeout, checkpointing, and automatic stage-stop rules is justified because old-generator smoke cells completed.

### Reset emotion-dynamics GIMME staged run launched

- Launched the bounded staged GIMME reset run with `taskset -c 0-3`, `EMOTION_GIMME_WORKERS=1`, `EMOTION_GIMME_TIMEOUT_SEC=900`, `EMOTION_GIMME_STAGE_CHUNK_SIZE=2`, `EMOTION_GIMME_STAGE_MIN_FITS_FOR_STOP=2`, and `EMOTION_GIMME_STAGE_MAX_NON_OK_RATE=0.5`.
- Output directory: `benchmark_outputs_emotion_gimme/`.
- Run PID: `1014124`.
- Initial stage: `01_gate_core_N50_2d3d_mid_signal`, 36 structure fits, 108 method/community rows.
- First completed chunk status: 2 structure fits, 6 method/community rows, all `ok`, fit-level non-OK rate `0.000`.
- First checkpointed fits were `old_high_single_stable_2d` rep 1 and rep 2, with runtimes about 106 and 110 seconds.
- Current monitoring status at launch checkpoint: the run is active under its own session, memory remains unconstrained, sampled `vmstat` shows no sustained I/O wait, and output writes remain small.
- Second completed chunk status: 4 structure fits, 12 method/community rows, all `ok`, fit-level non-OK rate `0.000`.
- The run advanced to chunk 3/18 of the first stage at 2026-05-13 15:47:45 CEST; keep treating this as a live bounded comparator run until it either finishes a stage or trips the automatic non-OK stop rule.

### Reset emotion-dynamics GIMME monitoring update

- Monitoring checkpoint at about 2026-05-13 16:31 CEST: `benchmark_outputs_emotion_gimme/checkpoint_results.csv` contained 57 rows, all `ok`.
- Fit-level accounting (`method == "gimme"`): 19 structure fits had checkpoint rows; the completed chunk-level stop summary had 18/18 `ok`, 0 `timeout`, 0 `error`, and non-OK rate `0.000`.
- The run had advanced to chunk 10/18 of `01_gate_core_N50_2d3d_mid_signal`; no stop rule had triggered.
- Longest fit observed in the stage-stop table was about 275 seconds, still below the 900-second hard timeout.
- Resource sampling remained safe: the parent R process stayed near 449 MB RSS, memory availability remained above 100 GiB, sampled `vmstat` showed no sustained I/O wait, and the output directory remained below 1 MB.
- Current interpretation: GIMME remains computationally slow but feasible on the first old/OAI gate stage so far; continue monitoring until either the gate stage completes or the staged non-OK rule trips.
- Later checkpoint at about 2026-05-13 16:34 CEST: 60 method/community rows, 20 fit-level rows, all `ok`; the run advanced to chunk 11/18 of the first gate stage.
- Dimension exactness among the 20 completed baseline GIMME fits was 9/20 so far; this is a recovery-performance signal, not a feasibility failure.

### Reset emotion-dynamics GIMME stopped at OAI stage boundary

- The bounded staged GIMME run ended cleanly by the pre-specified stage-stop rule at 2026-05-14 03:34:49 CEST; the parent process is no longer running.
- Final output directory: `benchmark_outputs_emotion_gimme/`.
- Final checkpoint count: 690 method/community rows, corresponding to 230 structure fits, or 20.28% of the full 1,134-fit GIMME ladder.
- Final fit-level status: 224 `ok`, 6 `error`, 0 `timeout`.
- Stage coverage: `01_gate_core_N50_2d3d_mid_signal` completed with 32 `ok` and 4 `error`; `02_old_core_N50_2d3d` completed fully with 192/192 `ok`; `03_oai_core_N50_2d3d` stopped after its first chunk with 0 `ok` and 2 `error`.
- The stop trigger was `03_oai_core_N50_2d3d` non-OK rate `1.000`, exceeding the configured `EMOTION_GIMME_STAGE_MAX_NON_OK_RATE=0.5`.
- The repeated error class was `GIMME fit did not return any matrices in fit$psi`, with debug directories written under `benchmark_outputs_emotion_gimme/gimme_failure_debug/`.
- Interpretation: GIMME is feasible for the old N=50 2D/3D core subset, but the OpenAI-inspired N=50 2D/3D stage is a computational/estimation feasibility boundary under the current bounded rule. Treat these errors as feasibility evidence, not as dimension-recovery failures.

### Reset emotion-dynamics DynEGA vs GraphicalVAR paper-style figures

- Added `plot_emotion_dynega_graphicalvar_paper_figures.R` to generate a paper-style comparison bundle from the completed reset DynEGA output and the GraphicalVAR checkpoint output.
- Output directory: `comparison_emotion_dynega_graphicalvar_paper/`.
- The bundle mirrors the old `Paper_emotion_dynamics_HGolino.Rmd` result structure: exact hit-rate/accuracy, manipulated-factor sensitivity, latent-score correlation, fit-error proxy, estimated dimensionality distribution, feasibility status, runtime, and partial eta-squared screening.
- GraphicalVAR `ok` rows define the comparable analytic subset; GraphicalVAR timeout rows are retained only in the status-boundary figure, not as dimension-recovery failures.
- Comparable subset size: 756 unique simulated panels, 2,268 method/community rows per method, 4,536 total rows across DynEGA and GraphicalVAR.
- Overall exact n.dim recovery on the comparable subset: DynEGA louvain `0.931`, leiden `0.934`, walktrap `0.914`; GraphicalVAR louvain/leiden/walktrap `0.754`.
- Overall assignment accuracy on the comparable subset: DynEGA about `0.940-0.941`; GraphicalVAR about `0.869`.
- Score recovery was close by construction of the common-score comparison: DynEGA score correlation about `0.631` and NRMSE about `0.836`; GraphicalVAR score correlation about `0.632` and NRMSE about `0.819`.
- Median runtime remained the clear practical separator: DynEGA about `1.1-2.5 s` depending on community variant; GraphicalVAR about `71.3 s`.
- The saved benchmark outputs contain `score_nrmse_common`, not RMSEA, so the figures and README report NRMSE explicitly rather than relabeling it as RMSEA.
- Corrected the first figure after visual inspection: removed confusing "GraphicalVAR-completed subset" wording, added `eta`/`zeta` values to the condition labels, and horizontally dodged method points so DynEGA is visible when it ties GraphicalVAR at 100% in the OpenAI-inspired ranked-persistence panels.
- Added `scenario_parameter_labels.csv` to the figure bundle as the explicit scenario-to-`eta`/`zeta` lookup table.
- Revised the exact-recovery figure layout again so `eta`/`zeta` values appear in facet column titles rather than x-axis tick labels. The bundle now includes a clean overview plus per-scenario exact-recovery panels (`01a`-`01e`) where columns are the factor-specific `eta`/`zeta` conditions.
- Revised the grouped overview after visual review: kept one column per scenario family, made each column title list the 2/3/4-factor `eta` and `zeta` values, kept the x-axis as true measured factors, and retained method dodging so DynEGA remains visible when both methods hit 100%.
- Revised the factor-sensitivity plot after visual review: dropped `TimeP` and `NVAR`, kept `N`, `Error_sd`, and `Loadings`, and changed the display to line plots with shaded 95% CI bands. Added shaded CI bands to the main exact-recovery overview as well.

## 2026-05-15

### OpenAI CLIP single-video dynEGA figure

- Added `plot_oai_clip_video_dynega_figure.R` to generate a four-row single-video figure from existing OpenAI CLIP frame-score outputs: video screenshots, selected-frame score bars, frame-score time series, and a same-clip dynEGA network.
- Selected `09018.mp4` from `/home/atomasevic/FER-evaluation/results/clip-large/mafw-sample-100/frame_scores.csv` because it has 47 complete `oai-large` frames and a stable single-face view.
- Fit the final-row dynEGA model directly to the selected clip's 11-emotion frame-score trajectory with louvain communities and `n.embed = 3`; the fit produced 3 communities.
- Output directory: `comparison_oai_clip_video_dynega/`.
- Primary figure outputs: `comparison_oai_clip_video_dynega/oai_clip_video_dynega_09018.png` and `comparison_oai_clip_video_dynega/oai_clip_video_dynega_09018.pdf`.
- Supporting outputs: `09018_selected_clip_frame_scores.csv`, `09018_dynega_edges.csv`, `09018_dynega_membership.csv`, and `09018_dynega_network.png`.
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`.

### OpenAI CLIP single-video figure revision

- Revised `plot_oai_clip_video_dynega_figure.R` after visual review to default to the fourth contact-sheet row, `02875.mp4`.
- The revised figure excludes `neutral` from the dynEGA fit and network while leaving the full frame-score table available for inspection.
- Time-series display now uses a centered 5-frame smoothing window by default, high-transparency gray lines for low-signal emotions, and a high-contrast palette for the selected clip's top four mean-score emotions.
- For `02875.mp4`, the top traces are `Helplessness`, `Fear`, `Disgust`, and `Sadness`; the neutral-excluded dynEGA model produced 3 communities across 10 nodes.
- Primary revised outputs: `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.png` and `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.pdf`.
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`; membership check confirmed `neutral_in_membership = False`.

### OpenAI CLIP selected-frame refinement

- Revised the selected screenshot row for `02875.mp4` to use explicit frame indices `7, 16, 19, 23` instead of evenly spaced frames.
- The four selected frame profiles are now: `Helplessness + fear`, `Strong disgust`, `Disgust + helplessness`, and `Pure helplessness`.
- Score check for the selected frames: frame 7 `helplessness=0.541`, `fear=0.167`; frame 16 `disgust=0.572`; frame 19 `helplessness=0.308`, `disgust=0.271`; frame 23 `helplessness=0.610`, `fear=0.199`.
- `plot_oai_clip_video_dynega_figure.R` now accepts `OAI_CLIP_FIGURE_FRAMES` and `OAI_CLIP_FIGURE_FRAME_LABELS` overrides for future selected-frame tuning.
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`.

### OpenAI CLIP figure presentation cleanup

- Removed panel gridlines from the selected-frame bar row and smoothed time-series row.
- Removed the time-series frame guide lines so the third row is fully grid-free.
- Changed the fourth-row title to `Dynamic EGA model with 3 discovered communities.`
- Set network edges to straight lines and removed the positive/negative edge legend, leaving only the capitalized community legend (`Community 1`, `Community 2`, `Community 3`).
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`.

### OpenAI CLIP figure title and legend placement

- Changed the first-row title to `Example video clip from MAFW dataset`.
- Moved the final-row community legend to the right side of the Dynamic EGA panel.
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`.

### OpenAI CLIP figure publication polish

- Polished `plot_oai_clip_video_dynega_figure.R` for a journal-ready four-row layout with panel letters, compact section headers, shared selected-frame bar-axis scaling, and a 7-frame smoothed time-series display.
- Rebuilt the Dynamic EGA row as native ggplot/cowplot output with straight signed edges, wrapped long node labels, and a right-side community legend only.
- Primary polished outputs: `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.png` and `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.pdf`; the PNG is 3240 x 2700 at 300 dpi.
- Supporting network output refreshed: `comparison_oai_clip_video_dynega/02875_dynega_network.png`.
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`; membership check confirmed `neutral_in_membership=FALSE`, `communities=3`, and `nodes=10`.

### OpenAI CLIP Dynamic EGA node-label refinement

- Revised the Dynamic EGA node labels so the font is not reduced; instead, node diameter was increased and only the two long labels are wrapped as `Helpless\nness` and `Disappoint\nment`.
- Refreshed `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.png`, `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.pdf`, and `comparison_oai_clip_video_dynega/02875_dynega_network.png`.
- Verification command: `Rscript plot_oai_clip_video_dynega_figure.R`; membership check confirmed `neutral_in_membership=FALSE`, `communities=3`, and `nodes=10`.

## 2026-05-16

### RR Figure 4 DynEGA full-grid replacement

- Added `plot_signed_feedback_dynega_rr_figure4.R` to build the RR replacement for the original hit-rate figure from the latest full signed-feedback DynEGA grid: `benchmark_outputs_signed_feedback_main_dynega_reps10/benchmark_results.csv`.
- Checked the literal hit-rate remake first; exact n.dim recovery is 100% across all 58,320 valid DynEGA main-grid rows, so hit rate was rejected as uninformative.
- Selected latent-score NRMSE as the replacement outcome because it shows the largest meaningful full-grid differences. The strongest main effects are feedback regime, measurement error, eta strength, loading strength, and true dimensionality.
- Primary selected outputs: `comparison_signed_feedback_dynega_rr/figure4_dynega_rr_selected.png` and `comparison_signed_feedback_dynega_rr/figure4_dynega_rr_selected.pdf`.
- Supporting diagnostics: `figure4_candidate_scores.csv`, `figure4_factor_effects.csv`, `figure4_grid_summary.csv`, `figure4_selected_summary.csv`, and `figure4_dynega_rr_caption.txt`.
- Verification command: `Rscript plot_signed_feedback_dynega_rr_figure4.R`.

### RR Figure 4 latest emotion-DynEGA source correction

- Added `plot_dynega_rr_figure4.R` as the report-facing Figure 4 generator after checking that `benchmark_outputs_emotion_dynega/benchmark_results.csv` is the newer emotion-DynEGA reset output and is closer to the original paper Figure 4 target.
- The latest emotion-DynEGA grid has 3,402 valid DynEGA rows across louvain, leiden, and walktrap variants. Exact recovery is not flat in this run, so the final report-facing figure returns to hit rate rather than using score NRMSE as the primary outcome.
- Tested alternative factor layouts. The closest literal old layout (`TimeP` by `NVAR`) was weaker; the selected layout uses measurement error SD by loading strength and facets true dimensionality.
- Revised the plot and report text to avoid manuscript-facing `low signal` / `high signal` scenario-regime labels. Those terms are source-defined scenario-family names in the benchmark wrapper, but they should not be introduced as RR figure labels unless the manuscript explicitly defines them.
- Revised the main DynEGA figure again to use neutral `Scenario A`-`Scenario F` rows, true-dimensionality columns, measurement-error SD on the x-axis, and loading strength as line color. Added `figure4_scenario_definitions.{csv,tex}` with compact generator descriptions and eta/zeta patterns.
- Simplified the main figure to Scenario A and Scenario B only after visual review, because Scenario C-F are near-ceiling and make the figure mostly empty. The A-F definition table remains in the report for transparency.
- Primary report-facing outputs: `comparison_dynega_rr_figure4/figure4_dynega_rr_selected.png` and `comparison_dynega_rr_figure4/figure4_dynega_rr_selected.pdf`.
- Supporting diagnostics: `figure4_candidate_scores.csv`, `figure4_factor_effects.csv`, `figure4_scenario_spread.csv`, `figure4_grid_summary.csv`, `figure4_selected_summary.csv`, and `figure4_dynega_rr_caption.txt`.
- Added the selected figure to `reports/revision_benchmark_report_v2.tex`.
- Verification command: `Rscript plot_dynega_rr_figure4.R`.
- Added a companion Scenario A-only DynEGA versus GraphicalVAR version in the same Figure 4 style. The companion uses matched successful Scenario A GraphicalVAR cells, rows for method, columns for true dimensionality, measurement-error SD on the x-axis, and loading strength as line color.
- Companion outputs: `comparison_dynega_rr_figure4/figure4_dynega_vs_graphicalvar_scenario_a.png`, `.pdf`, `_summary.csv`, `_design.csv`, and `_caption.txt`.
- Revised the companion to include GIMME as the middle method row. GIMME is shown only for its staged Scenario A coverage (2D/3D; N=50, TimeP=100, NVAR=4); the 4D GIMME panel is explicitly marked not run.
- Corrected the Figure 4 summary code so 95% CIs are computed from row-level exact-hit indicators before aggregation rather than from already-averaged cell means.
- Removed visible CI bars from the Scenario A method-comparison companion after visual review; the plot now shows mean lines and points only.
- Added two factor-score correlation companion figures: a DynEGA-only full-grid figure with Scenario A-F rows, and a Scenario A method comparison with DynEGA/GIMME/GraphicalVAR rows.
- Factor-correlation effect screen for DynEGA showed the largest partial eta-squared terms were `scenario_family` (0.897), `Error_sd` (0.565), `Loadings` (0.240), and `scenario_nfac` (0.101), so the full A-F scenario layout is retained for correlation even though it was too flat for hit-rate visualization.
- Added `figure4_scenario_a_method_comparison_table.{csv,tex}` and `figure4_scenario_a_method_comparison_interpretation.md`. The correlation-only comparison does not show a clear DynEGA advantage; DynEGA is clearer on exact dimensional recovery, assignment accuracy, and runtime.
- Added `plot_scenario_a_3d_error05_example.R` to visualize one generated Scenario A 3D condition with measurement-error SD 0.5 and loading 0.6. The output overlays active-factor latent trajectories with noisy observed indicators and shows the observed-item correlation matrix for the full simulated panel.

### Model naming decision: LOMM

- Adopted **Latent Oscillator Measurement Model (LOMM)** as the preferred manuscript-facing name for the generalized latent second-order simulation framework.
- Naming rationale: use `Latent Oscillator Measurement Model`, not `Latent Oscillator with Measurement Model`, because the measurement layer is part of the integrated generative model rather than an attached add-on.
- Updated the active documentation scaffold to reflect LOMM: `README.md`, `paper-plan.md`, `style-guide.md`, `claims-ledger.md`, `analysis-map.md`, `reviewer-issues.md`, `paper/gdlo_working_note.tex`, `paper/Paper_emotion_dynamics_HGolino.Rmd`, `reports/revision_benchmark_report_v2.tex`, and `verification-log.md`.
- Legacy `DLO` terminology remains acceptable only for older script names, archived output names, and historical draft references.
- Rebuilt `reports/revision_benchmark_report_v2.pdf` after the report scaffold title and captions were updated.

### RR Quarto manuscript scaffold

- Created `paper/Paper_emotion_dynamics_HGolino_RR.qmd` from the existing R Markdown manuscript as the RR Quarto scaffold.
- Preserved visible manuscript prose and inserted only non-rendered `RR-TODO` comments marking where R&R changes should be inserted.
- Added Quarto metadata linking the scaffold to the paper-as-code control files: `paper-plan.md`, `claims-ledger.md`, `analysis-map.md`, `reviewer-issues.md`, `style-guide.md`, and `verification-log.md`.
- `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` succeeds. Full render is blocked because `papaja` is not installed, and the original manuscript also references missing local `Examples/` and bibliography files.

### LOMM model-landscape review and map

- Added `paper/lit-review/lomm_model_landscape_review.md` as the manuscript-facing literature-positioning note for LOMM.
- Added `paper/figures/lomm_model_landscape_map.png` and `.svg` as a deterministic map of related model families and where LOMM fits.
- Current framing: LOMM is an interpretable generative bridge between psychological oscillator/LDE models, dynamic factor and state-space measurement models, and dimensional-recovery benchmarks for FER-like time series.
- Originality should be stated narrowly: the contribution is the integrated generator plus explicit measurement layer plus known-truth recovery benchmark, not a claim to have invented latent dynamics, oscillators, measurement models, ctSEM/DSEM, or latent ODEs.

### Kankaanpää et al. measurement-error citation

- Added Kankaanpää, de Ron, Hoekstra, and van Bork (2026), "Comparing Multiple-Indicator Approaches to Account for Measurement Error in Dynamic Networks," as the priority dynamic-network measurement-error citation for the LOMM framing.
- Updated `paper/lit-review/lomm_model_landscape_review.md`, `paper-plan.md`, and `claims-ledger.md`.
- Manuscript use: cite it to motivate the explicit LOMM measurement layer and the interpretation of loading/error conditions in the benchmark.
- Guardrail: the paper supports measurement-error rationale and GraphicalVAR caution, but it is not a direct LOMM generator, FER-score, DynEGA, or latent-dimensional-recovery benchmark.

### Non-Scenario-A Figure 4 main suite

- Moved Scenario A to supplementary hard edge-case framing and re-screened DynEGA effects after excluding it.
- In the non-A DynEGA grid, exact n.dim recovery is driven most by loading strength (partial eta2 = 0.0217), scenario family (0.0179), measurement-error SD (0.0107), and true dimensionality (0.0087). Factor-score correlation is driven most by scenario family (0.864), measurement-error SD (0.514), loading strength (0.216), and true dimensionality (0.104).
- Selected Scenarios B-D for the main DynEGA figure: B and C carry meaningful exact-recovery variation, while D carries major factor-score-correlation variation; E and F are near ceiling and are not visually informative for the main figure.
- Added `plot_non_a_main_and_method_comparison.R` and regenerated the non-A main suite in `comparison_dynega_rr_figure4/`.
- Main DynEGA outputs: `figure4_main_non_a_dynega_hit_rate.{png,pdf}` and `figure4_main_non_a_dynega_factor_cor.{png,pdf}`.
- Method-comparison outputs: broad B-D overlap grids `figure4_method_bcd_overlap_hit_rate.{png,pdf}` and `figure4_method_bcd_overlap_factor_cor.{png,pdf}`, plus compact high-gap figures `figure4_method_gap_highlights_hit_rate.{png,pdf}` and `figure4_method_gap_highlights_factor_cor.{png,pdf}`.
- Added performance assessment table `figure4_method_performance_assessment.{csv,tex}`. In the bounded B-D GIMME-overlap subset, DynEGA has the best structural recovery and fastest runtime; factor-score correlations are comparable across methods rather than uniquely favoring DynEGA.
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### Main scenario relabeling after moving hard case to supplement

- Updated `plot_non_a_main_and_method_comparison.R` so the previous hard Scenario A is excluded from the main sequence and referred to as the supplementary hard scenario.
- Renumbered the main-text scenarios so the previous B-D sequence is now labeled Scenarios A-C in the figures, captions, method-comparison tables, and README output.
- Regenerated the main suite in `comparison_dynega_rr_figure4/`.
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### Reliable method-accuracy comparison figure

- Replaced the selected high-gap bar chart because it hid zero-valued GIMME results and showed only one visible GraphicalVAR bar, making it a poor performance comparison.
- Added cluster-bootstrap accuracy summaries across the full bounded GIMME-overlap subset for main Scenarios A-C, using exact n.dim recovery and assignment accuracy as the accuracy outcomes.
- Added paired DynEGA-minus-comparator differences using matched simulation cells and 95% cluster-bootstrap intervals, clustered by simulation structure cell.
- New outputs: `figure4_method_accuracy_levels.{png,pdf}`, `figure4_method_accuracy_differences.{png,pdf}`, combined `figure4_method_accuracy_reliable.{png,pdf}`, `figure4_method_accuracy_reliable_summary.csv`, and `figure4_method_accuracy_reliable_differences.{csv,tex}`.
- Overall exact n.dim differences: DynEGA - GIMME = 0.580 [0.500, 0.654]; DynEGA - GraphicalVAR = 0.130 [0.080, 0.181]. Overall assignment differences: DynEGA - GIMME = 0.252 [0.211, 0.292]; DynEGA - GraphicalVAR = 0.049 [0.028, 0.072].
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### Exact-dimensionality-only method-accuracy figure

- Simplified the reliable method-accuracy comparison to exact n.dim recovery only, removing assignment accuracy from the figure and companion table.
- Combined exact n.dim method accuracy and paired DynEGA-minus-comparator differences into `figure4_method_accuracy_reliable.{png,pdf}`.
- Kept exact-only standalone versions as `figure4_method_accuracy_levels.{png,pdf}` and `figure4_method_accuracy_differences.{png,pdf}`.
- Updated `figure4_method_accuracy_reliable_summary.csv`, `figure4_method_accuracy_reliable_differences.csv`, and `figure4_method_accuracy_reliable_differences.tex` so they contain only exact n.dim rows.
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### Hit-rate-only labeling for method accuracy

- Relabeled the method-comparison accuracy figure and companion tables to use manuscript-facing `Hit rate` wording only.
- The combined method figure now contains two panels only: method hit rate and paired DynEGA hit-rate advantage.
- Verified that `figure4_method_accuracy_reliable_summary.csv`, `figure4_method_accuracy_reliable_differences.csv`, `figure4_method_accuracy_reliable_differences.tex`, and `figure4_method_accuracy_reliable_caption.txt` no longer contain assignment rows or exact-n.dim labels.
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### Runtime and feasibility method-comparison figure

- Added runtime and feasibility outputs to `plot_non_a_main_and_method_comparison.R`.
- Revised the feasibility panel after review: scenario-by-dimensionality coverage percentages were uniformly repeated because the comparator staging was uniform across scenario labels. The figure now shows design-block coverage instead, which is the meaningful unit for computational feasibility.
- `figure4_method_runtime_successful.{png,pdf}` shows median execution time with IQRs on a log scale for the common base grid (`N=50`, `T=100`, `p=4`) where methods are directly comparable.
- `figure4_method_runtime_coverage.{png,pdf}` shows which design blocks were actually executed: base grid, larger `N`, longer `T`, and more indicators. This explicitly marks GraphicalVAR as not run for longer `T` and more indicators, and GIMME as not run for larger `N`, longer `T`, more indicators, and all 4D blocks.
- `figure4_method_runtime_feasibility.{png,pdf}` combines runtime and coverage into one two-panel figure.
- Added support tables `figure4_method_runtime_summary.csv`, `figure4_method_runtime_common_base_summary.csv`, `figure4_method_runtime_coverage.csv`, and `figure4_method_runtime_speedup.csv`.
- Common-base timing result: median DynEGA runtime is 0.74 s, 0.94 s, and 1.10 s for 2D, 3D, and 4D. GraphicalVAR is about 37x, 57x, and 116x slower by median runtime. GIMME is about 145x and 249x slower for 2D and 3D and was not run for 4D.
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### DynEGA full-grid clean result synthesis

- Added `reports/dynega_full_grid_clean_report.md` as the current source-of-truth synthesis for the noisy DynEGA result trail.
- Reconciled the full signed-feedback DynEGA grid (`benchmark_outputs_signed_feedback_main_dynega_reps10/benchmark_results.csv`): 58,320 rows, 5,832 structural cells x 10 reps, all `ok`, exact n.dim recovery = 1.000, assignment accuracy = 1.000.
- Reconciled the current report-facing emotion/LOMM DynEGA grid (`benchmark_outputs_emotion_dynega/benchmark_results.csv`): 3,402 rows, 378 structural cells x 3 community algorithms x 3 reps, exact n.dim recovery = 0.942 overall and 0.990 after excluding the supplementary hard scenario.
- Community-method conclusion: Louvain and Leiden are effectively equivalent in the current DynEGA grid, Walktrap is slightly weaker, and GraphicalVAR/GIMME post-hoc community variants do not change exact n.dim outcomes in paired successful rows.
- Unidimensionality conclusion: current grids do not include true 1D cells; `uni_method = expand` is supported by older stress-grid behavior but should not be described as direct unidimensionality validation.
- Verification commands: recomputed summaries with pandas from the result CSVs; no benchmark rerun.

### 2026-05-18 Method accuracy plus conditional latent-correlation figure

- Added `figure4_method_accuracy_conditional_correlation.{png,pdf}` as a two-panel method-comparison variant.
- Left panel shows hit rate with 95% cluster-bootstrap intervals across the bounded GIMME-overlap subset for main Scenarios A-C.
- Right panel shows latent-score correlation only among rows where the corresponding method recovered the correct number of dimensions (`n_dim_exact = 1`), also with cluster-bootstrap intervals.
- Added `figure4_method_accuracy_conditional_correlation_summary.csv` and caption text.
- Verification command: `Rscript plot_non_a_main_and_method_comparison.R`.

### 2026-05-18 RR introduction rewrite

- Rewrote the RR manuscript introduction in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` from the old tutorial / political-application frame into a focused methods-paper opening.
- The new introduction presents three linked contributions: LOMM for FER-like video emotion time series, retained-100 empirical-realism checks, and dimensional-recovery benchmarking.
- The zero-shot / CLIP-like scoring step is now framed as empirical input and realism target, not broad FER validation.
- Added guarded citation TODOs for the CCR / Tomasevic workflow, MAFW empirical target, and Kankaanpaa et al. (2026), because the manuscript bibliography files are not present in the current checkout.
- Verification commands: searched the rewritten introduction for unsupported broad-validation language and checked the revised prose against `claims-ledger.md`, `reviewer-issues.md`, `paper-plan.md`, and `style-guide.md`.

### 2026-05-18 RR introduction rewrite reverted

- Reverted the broad RR introduction rewrite in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` after review feedback that the previous version set up the paper better and that the intervention should be more targeted.
- Restored the old introduction prose and the non-rendered `RR-TODO` comments so the next pass can make narrower edits rather than replacing the whole section.
- No render was run; this was a targeted text revert.

### 2026-05-18 RR introduction targeted citation update

- Replaced the placeholder MAFW citation in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` with the official MAFW dataset citation key `@liu_mafw_2022`.
- Source citation: Liu, Dai, Feng, Wang, Yin, Zeng, and Shan (2022), "MAFW: A Large-scale, Multi-modal, Compound Affective Database for Dynamic Facial Expression Recognition in the Wild," ACM MM '22, DOI `10.1145/3503161.3548190`.
- No render was run because the bibliography files are not present in the current checkout.

### 2026-05-18 RR introduction applied behavioral-science paragraph

- Added one targeted concluding introduction paragraph to `paper/Paper_emotion_dynamics_HGolino_RR.qmd` after the existing LOMM bridge paragraph.
- The paragraph motivates LOMM for video-rich behavioral-science domains: psychotherapy and clinical assessment, parent-child and small-group interaction, collaborative learning, public communication, and social-media video.
- Added `paper/lit-review/applied_behavioral_science_entry_points.md` with the small literature-review trail and recommended citation clusters.
- Citation logic: Kuppens & Verduyn (2017), Houben et al. (2015), and Hollenstein (2007) support affect-dynamics relevance; Halamova et al. (2023), Manelis-Baram et al. (2025), Li et al. (2024), and Holiday et al. (2023) give applied video/facial-analysis entry points; McKeown & Allison (2024) and Kankaanpaa et al. (2026) support the interpretation and measurement-error caution.
- Search limitations: `paper-search google scholar` could not run because `SERPER_API_KEY` is not configured; several Semantic Scholar queries returned `RetryError`, so citation details were checked against publisher or primary pages where possible.
- No render was run because the bibliography files are not present in the current checkout.

### 2026-05-18 RR introduction finishing paragraph and bib entries

- Tightened the new concluding introduction paragraph into a three-move structure: applied home, methodological gap, and LOMM contribution.
- Added `paper/references.bib` with the nine new citation keys used in the paragraph: `kuppensEmotionDynamics2017`, `houbenRelationShorttermEmotion2015`, `hollensteinStateSpaceGrids2007`, `halamovaAutomatedFacialExpression2023`, `manelisbaramComparingThreeAlgorithms2025`, `liDecodingGroupEmotional2024`, `holidayMultimodalEmotionPerspective2023`, `mckeownAreFacialExpression2024`, and `kankaanpaa2026multipleindicator`.
- DOI/BibTeX details were checked through DOI content negotiation or publisher pages before entry.
- No full render was run because `general_library_golino.bib` remains absent from this checkout and the older manuscript still references many unresolved legacy keys.

### 2026-05-18 RR introduction paragraph order and roadmap close

- Moved the applied behavioral-science motivation paragraph earlier in the introduction, directly after the opening problem statement about dense, noisy FER-like score series.
- Replaced the previous introduction ending with explicit contributions and a paper-structure roadmap: LOMM specification, empirical realism check, and known-truth dimensional-recovery benchmark.
- Removed tutorial wording from the manuscript title metadata by changing the subtitle from "simulation and tutorial" to "simulation and benchmark".
- Replaced the remaining rendered "tutorial section" sentence in the CLIP illustration section with a methods-paper framing: the scoring workflow is an empirical FER-like time-series source rather than a software tutorial.
- No full render was run because `general_library_golino.bib` remains absent from this checkout and the older manuscript still references many unresolved legacy keys.

### 2026-05-18 RR introduction paragraph 2-4 streamlining

- Reworked the second through fourth introduction paragraphs into a cleaner sequence: substantive behavioral motivation, automated facial-analysis measurement pipeline, and validation/measurement-error gap.
- Removed the earlier political-communication-specific motivation from the introduction so the paper opens as a general methods paper for video-rich behavioral science.
- Kept the new applied references and measurement-error citations, but made the prose less list-like and more directly tied to dynamic structure.
- Verification: `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed; citation parsing of the newly added references passed, with expected unresolved warnings only for older bibliography keys that depend on the missing legacy bibliography files.

### 2026-05-18 RR introduction LOMM realism-check emphasis

- Revised the introduction value/contribution/roadmap paragraphs so the LOMM empirical-realism check is explicitly part of the argument, not a side note.
- Current sequence: specify LOMM, check whether LOMM simulations reproduce selected empirical FER-like signatures, then use the empirically checked known-truth generator to benchmark dimensional recovery.
- Verification: `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed.

### 2026-05-18 RR introduction LOMM name and closing compression

- Removed "Damped" from the first introduction mention of LOMM so the model is introduced as the Latent Oscillator Measurement Model, avoiding conflict with signed-feedback/persistent-regime sign-convention issues.
- Compressed the former four-paragraph method-purpose ending into two paragraphs: one introduces LOMM and its realism-check/benchmark validation sequence, and one states the three contributions plus paper roadmap.
- Verification: `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed.

### 2026-05-18 LOMM section opening locked before math

- Replaced the old political-speech opening paragraphs in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` with a general expressive-affective-dynamics motivation.
- Added `paper/lit-review/affective_dynamics_model_threads.md` summarizing the quick literature-review pass: affective-dynamics models converge on level, variability, instability, inertia, transitions, return after perturbation, and attractor-like regimes; oscillator models add a compact level/velocity/acceleration vocabulary.
- Updated the pre-equation lead-in so the section introduces neutrality, positive affect, and negative affect as latent expressive trajectories rather than political sentiment variables.
- Verification: `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed.

### 2026-05-18 LOMM latent-dimension motivation revision

- Revised the second paragraph of `# Expressive Affective Dynamics as an Oscillator Measurement Model` so latent variables are introduced as theory-guided expressive dimensions rather than as observed FER labels.
- Replaced the positive/negative/neutral motivating example with canonical affect-structure entry points: valence/arousal, PAD dominance/control, Plutchik-style emotion-family opponent tendencies, and facial-expression dimensions such as pleasantness, activation, attention, and rejection.
- Added five supporting references to `paper/references.bib`: Russell (1980), Russell (2003), Russell & Mehrabian (1977), Plutchik (2001), and Schlosberg (1954).
- Clarified that `$Nt$` is the baseline trajectory used in the simplified current implementation and that the same state equation can be written for any chosen latent expressive dimension.
- Verification: `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed; `pandoc --citeproc --bibliography=paper/references.bib` resolved the five new citation keys.

### 2026-05-18 LOMM formal description replacement before legacy figure

- Replaced the old neutrality-centered lines after the section opening in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` with the current LOMM source-of-truth description from `extract.r` and the reset emotion-dynamics benchmark scripts.
- The manuscript now states the implemented latent equation as `x'' = eta_f x + zeta_f x' + q_f`, with factor-specific shocks and no off-diagonal cross-factor dynamic coupling.
- Updated parameter interpretation to use position feedback and velocity feedback, including the implemented sign convention: negative `eta` is restoring position feedback, negative `zeta` attenuates velocity, zero `zeta` removes velocity feedback, and positive `zeta` can sustain/amplify finite-window change.
- Added the benchmark expression-scenario envelope layer: active/background gain, stable versus switching scenarios, and no separate neutrality factor in the source-of-truth generator.
- Added the block measurement equation `y = Lambda x + epsilon`, with one block of observed FER-like indicators per latent factor, loading jitter around the benchmark loading condition, and diagonal measurement error jittered around the benchmark error condition.
- Replaced the old positive/negative/neutral example with a three-factor Plutchik-informed example: affiliative/approach, threat/defensive, and orienting/novelty expression.
- Verification: `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed; `pandoc --citeproc --bibliography=paper/references.bib` resolved the Plutchik and Kankaanpää keys; a minimal `Rscript` call sourced `extract.r` and generated a 5x3 latent matrix plus a 5x12 observed matrix for a three-factor, four-indicator-per-factor test.

### 2026-05-18 Figure 1 replacement with three-factor LOMM example

- Updated `plot_dlo_figure1_model_examples.R` so Figure 1 uses the manuscript's three latent expressive dimensions: affiliative/approach, threat/defensive, and orienting/novelty expression.
- The script now generates a single three-factor example with factor-specific `eta`, `zeta`, and `sigma_q`, using `simulate_latent_multi()` with the `linear_exact` integrator.
- Regenerated `comparison_revision_report_v2/figure1_dlo_model_examples.pdf`, `.png`, and `_data.csv`.
- Replaced the old embedded neutrality/positive/negative Figure 1 code block in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` with a static include of the regenerated Figure 1 artifact and a short interpretation paragraph aligned with the current LOMM description.
- Removed the immediately dependent legacy observable-score chunk in the same manuscript region because it relied on objects created by the deleted Figure 1 block and repeated obsolete positive/negative measurement wording.
- Verification: `Rscript plot_dlo_figure1_model_examples.R` completed; visual inspection of `comparison_revision_report_v2/figure1_dlo_model_examples.png` showed the intended two-panel three-factor figure; `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed.

### 2026-05-18 Figure 1 label and interpretation cleanup

- Revised Figure 1 labels in `plot_dlo_figure1_model_examples.R`: the x-axis now reads `Time`, and the legend uses single labels (`Affiliative`, `Threat`, `Orienting`) instead of slash-form labels.
- Regenerated `comparison_revision_report_v2/figure1_dlo_model_examples.pdf`, `.png`, and `_data.csv`; the data file now uses a `time` column.
- Rewrote the paragraph after Figure 1 in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` to describe the visible dynamics: after approximately time point 100, the blue affiliative trajectory dominates while threat fluctuates below it and orienting remains lower with local excursions.
- Verification: `Rscript plot_dlo_figure1_model_examples.R` completed; visual inspection confirmed the updated axis and legend; `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed.

### 2026-05-18 Figure 1 sigma-q interpretation emphasis

- Revised the paragraph after Figure 1 in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` so the main interpretation is the role of innovation variance, `sigma_q^2`.
- The paragraph now contrasts the smooth reference trajectories with the simulated oscillator panel, where nonzero factor-specific `sigma_q` produces local peaks, reversals, and irregular fluctuations while preserving the affiliative-dominance pattern after approximately time point 100.

### 2026-05-18 MAFW empirical Figure 2 insertion

- Replaced the obsolete Boris Johnson / Brexit zero-shot illustration in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` with the current MAFW single-video empirical illustration generated by `plot_oai_clip_video_dynega_figure.R`.
- The revised text frames OpenAI-large scores as prompt-defined FER-like score channels and empirical realism input, not as direct measurements of latent emotions or broad FER validation.
- The inserted Figure 2 static include uses `comparison_oai_clip_video_dynega/oai_clip_video_dynega_02875.pdf`; the figure shows selected frames, selected-frame score profiles, smoothed frame-level trajectories for the highest mean channels, and a DynEGA network over non-neutral score trajectories.
- Added the official MAFW citation key `@liu_mafw_2022` to `paper/references.bib`.
- Verification: `Rscript plot_oai_clip_video_dynega_figure.R` completed; `quarto inspect paper/Paper_emotion_dynamics_HGolino_RR.qmd` passed; `pandoc --citeproc --bibliography=paper/references.bib` resolved `@liu_mafw_2022`; `rg` confirmed the revised RR file no longer contains the Boris/Brexit block or the old CLIP tutorial citations.

### 2026-05-18 Quarto-native RR PDF compile

- Removed the `papaja` dependency from `paper/Paper_emotion_dynamics_HGolino_RR.qmd` and removed the installed `papaja` R package from the user library.
- Added local APA CSL support through `paper/apa.csl` and changed the RR manuscript bibliography to the existing `paper/references.bib`; the previously declared `general_library_golino.bib` is absent from the repo, lab vault, and adjacent FER workspace.
- Converted the active Figure 1 and Figure 2 references to Quarto-native `@fig-...` cross-references.
- Removed the `orcidlink` LaTeX package dependency and replaced ORCID macros in the author note with plain-text ORCID IDs because the installed TeX Live cannot fetch `orcidlink.sty` from the newer remote repository.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed and wrote `paper/Paper_emotion_dynamics_HGolino_RR.pdf`; citeproc still warns about unresolved legacy keys that were formerly expected from the missing `general_library_golino.bib`.

### 2026-05-18 Bibliography sync from root library

- Found `general_library_golino.bib` at the repository root and copied all cited keys present there but missing from `paper/references.bib`.
- Added 33 entries: `anderson1958`, `boker2018`, `cattell1965`, `chen2008ebic`, `deboeck2009using`, `EGAnet`, `epskamp2012qgraph`, `epskamp2017generalized`, `epskamp2018gaussian`, `epskamp2018tutorial`, `epskampggm`, `epskampNetworkPsychometrics`, `foygel2010extended`, `friedman2008glasso`, `glla2010`, `golino2017ega1`, `golino2019investigating`, `golino2020modeling`, `guttman1953image`, `lauritzen1996graphical`, `liu2016dynamical`, `nesselroade2002`, `nesselroade2008dfm`, `pons2006walktrap`, `rosenstein1993practical`, `schmiedek2020training`, `takens1981detecting`, `tibshirani1996lasso`, `velicer1976map`, `whitney1936differentiable`, `widaman1993common`, `williams2019nonregularized`, and `williams2020back`.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed. Remaining citeproc warnings are for keys cited in the RR QMD but absent from `general_library_golino.bib`: `arriaga2017Realtimea`, `barrett2019Emotionalb`, `boker2002Method`, `boker2015Adaptive`, `ezerceli2022Convolutional`, `foteinopoulou2023EmoCLIP`, `goodfellow2013Challenges`, `hoemann2020Contextaware`, `li2022Deep`, `li2023CLIPER`, `mehu2015Emotion`, `meng2017IdentityAware`, `nesselroade2013dynamic`, `ollero2023Characterizing`, `stewart2009Presidential`, `tomasevic2026TransforEmotion`, `tong2005Use`, and `wang2022Rethinking`.

### 2026-05-18 Bibliography sync from old paper libraries

- Copied the remaining 17 exact citation-key matches from `references.bib-old` into `paper/references.bib`: `arriaga2017Realtimea`, `barrett2019Emotionalb`, `boker2002Method`, `boker2015Adaptive`, `ezerceli2022Convolutional`, `foteinopoulou2023EmoCLIP`, `goodfellow2013Challenges`, `hoemann2020Contextaware`, `li2022Deep`, `li2023CLIPER`, `mehu2015Emotion`, `meng2017IdentityAware`, `nesselroade2013dynamic`, `ollero2023Characterizing`, `stewart2009Presidential`, `tong2005Use`, and `wang2022Rethinking`.
- Added `tomasevic2026TransforEmotion` manually from the CCR citation supplied in the revision thread: Tomašević, Golino, and Christensen (2026), *transforEmotion: An Open-Source R Package for Emotion Analysis Using Transformer-Based Generative AI Models*, DOI `10.5117/CCR2026.2.2.TOMA`.
- Verification: `paper/references.bib` now has 66 unique BibTeX keys, no duplicate keys, and no missing cited keys in `paper/Paper_emotion_dynamics_HGolino_RR.qmd`; `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed without citeproc missing-citation warnings.

### 2026-05-19 Benchmark scenario wording cleanup

- Revised the LOMM benchmark-scenario paragraph in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` to remove internal artifact language such as OpenAI-results-inspired panels and the confusing expression-envelope wording.
- The paragraph now describes benchmark scenarios scientifically in terms of two controlled levels: dynamical regimes set by factor-specific `eta`, `zeta`, and `sigma_q`, and time-varying activity weights that create single-regime, switching, and low-expression conditions.
- Verification: `rg` found no remaining `OpenAI-results`, `results-inspired`, `envelope`, or `internal` phrasing in the RR QMD; `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-19 Measurement equation notation check

- Checked lines 178-190 of `paper/Paper_emotion_dynamics_HGolino_RR.qmd` against `extract.r`, `run_method_benchmark.R`, and the latest emotion benchmark wrappers.
- Revised the measurement equation from a common `Lambda`/`Sigma` notation to per-video `Lambda_i` and `Sigma_i`, matching the current generator where loading jitter and item-level measurement-error variance jitter are drawn inside `generate_observables_multi()` for each simulated video.
- Clarified that the latest emotion benchmark grid reports measurement error as `Error_sd`, but passes `Error_variance = Error_sd^2` into the measurement covariance baseline; the generator then adds small item-level variance jitter.
- Verification: `options(run_extract_pipeline=FALSE); source("extract.r")` plus a 3-factor, 4-indicator smoke call to `generate_observables_multi()` produced a `5x12` observed panel; `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-19 Early pipeline overview figure

- Added `plot_lomm_pipeline_overview.R` to generate a manuscript-ready overview figure for the revised RR paper.
- The figure separates the empirical scoring target from the known-truth simulation benchmark: video frames and FER-like score panels define realism targets, LOMM supplies the source-truth latent structure and observation layer, and DynEGA/GraphicalVAR/GIMME are evaluated against known dimensions and blocks.
- Inserted the generated PDF into `paper/Paper_emotion_dynamics_HGolino_RR.qmd` before the LOMM section so the introduction now visually links empirical scoring, realism checks, simulation, and dimensional-recovery benchmarking.
- Verification: `Rscript plot_lomm_pipeline_overview.R` wrote `paper/figures/lomm_pipeline_overview.pdf` and `.png`; `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-19 Pipeline figure two-row refinement

- Revised `plot_lomm_pipeline_overview.R` from a single-row text-heavy pipeline into a two-row panel: empirical calibration on top and known-truth simulation benchmark on bottom.
- Added graphical elements for video frames, score trajectories, realism targets, latent trajectories, the block measurement matrix, simulated panels, and recovery-network output.
- Made scenario selection more explicit by mapping empirical realism targets into three scenario-design families: dynamical parameters `eta`, `zeta`, and `sigma_q`; activity regimes; and measurement parameters `Lambda` and `Sigma_epsilon`.
- Verification: `Rscript plot_lomm_pipeline_overview.R` regenerated `paper/figures/lomm_pipeline_overview.pdf` and `.png`.

### 2026-05-19 Pipeline figure panel-label refinement

- Revised the two-row pipeline so the bottom row is read from right to left, avoiding the long diagonal calibration line from the scenario-selection panel to the benchmark row.
- Added panel labels A--H to each block.
- Replaced generic realism-target bubbles with the report-facing realism metrics: lag-1 ACF, derivative IQR, run-length entropy, spectral entropy, and effective rank.
- Removed the extra "choose scenarios" label from the scenario-selection block; enlarged the Greek-parameter labels and spelled out dominance, switching, and low expression activity regimes.
- Verification: `Rscript plot_lomm_pipeline_overview.R` regenerated `paper/figures/lomm_pipeline_overview.pdf` and `.png`.

### 2026-05-19 Polished visual-overview figure placement

- Copied the polished image-generated version of the LOMM pipeline into `paper/figures/lomm_pipeline_visual_abstract.png`.
- Kept the figure at the end of the introduction, immediately after the aim/contribution/roadmap paragraph and before the first technical LOMM section, so it functions as the paper's early visual overview rather than a detached graphical abstract.
- Updated the introduction text to refer explicitly to Panels A--H: A--B for video-to-score construction, C for realism targets, D for scenario selection, and E--H for the right-to-left known-truth benchmark.
- Updated the figure include and caption in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` to use the polished PNG.

### 2026-05-19 Basic realism-metric check

- Added retained-100 OAI validation readouts for per-channel IQR (`channel_iqr`) and mean absolute pairwise correlation (`mean_abs_pairwise_cor`), then regenerated `comparison_revision_report_v2/oai_basic_metric_comparison_v2.csv` and `.tex`.
- Result: `channel_iqr` is a stricter amplitude/occupancy check than the entropy metrics. It fails in both raw and louvain latent readouts because DLO synthetic medians are larger than OpenAI-large medians: raw `0.064` vs `0.020`, latent `0.843` vs `0.247`.
- Result: mean absolute pairwise correlation is partial rather than passing. It shows lower synthetic dependence than empirical dependence: raw `0.295` vs `0.369`, latent `0.217` vs `0.386`.
- Interpretation: derivative IQR, lag-1 ACF, and run-length entropy remain the easiest report-facing realism targets; spectral entropy, effective rank, and mean absolute pairwise correlation are partial; channel IQR exposes the clearest remaining amplitude-spread mismatch.
- Verification: `Rscript summarize_oai_validation_sensitivity.R` completed; `Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"` loaded the added metric definitions successfully.

### 2026-05-19 Minimal-rewrite manuscript reorganization

- Reorganized `paper/Paper_emotion_dynamics_HGolino_RR.qmd` around the polished LOMM pipeline figure while preserving the revised introduction and LOMM model prose.
- Moved the visual overview to the bridge between the LOMM model specification and the Method section, keeping it as the first rendered figure; kept the three-factor trajectory figure and its `sigma_q` explanation in the LOMM section.
- Replaced the legacy DynEGA tutorial, EII/DAFS simulation scaffold, and old empirical-example section with compact Method subsections for empirical calibration and realism targets (Panels A--C), scenario selection (Panel D), and the known-truth benchmark (Panels E--H).
- Added Results subsections that mirror the Method order: empirical realism results followed by benchmark recovery results, with narrow Discussion language around controlled dimensional-recovery benchmarking and partial empirical realism.
- Verification: `rg -n "Tutorial|ergodicity information index|Boris|Brexit|BIG FIGURE|DAFS|Eq|NotEq" paper/Paper_emotion_dynamics_HGolino_RR.qmd` returned no matches; `rg -n "validates|proves|ground truth emotion|true emotions|fully reproduces" paper/Paper_emotion_dynamics_HGolino_RR.qmd` returned no matches; `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed and wrote `paper/Paper_emotion_dynamics_HGolino_RR.pdf`.

### 2026-05-19 Early Figure 1 and DynEGA method cleanup

- Moved the polished pipeline overview back to the end of the Introduction so it is the first rendered figure and appears before the formal LOMM section.
- Removed panel labels from Method subsection titles while keeping panel letters parenthetical in the prose.
- Expanded the Dynamic Exploratory Graph Analysis method description into two compact paragraphs: one for the derivative/EGA logic and one for its role as a known-truth dimensional-recovery estimator alongside GraphicalVAR and bounded GIMME.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed; the rendered TeX orders captions as `fig-pipeline`, `fig-dlosim`, then `fig-mafwvideo`; the combined legacy/validation-language search returned no matches.

### 2026-05-20 Realism-target figure update

- Replaced `paper/figures/lomm_pipeline_visual_abstract.png` with an image-generated revision of the pipeline figure where the Panel C realism-target list now reads: lag-1 ACF, IQR, standard deviation, run-length entropy, spectral entropy, and effective rank.
- Updated the manuscript realism-target prose to describe six report-facing metrics, using `IQR` rather than `derivative IQR` and adding standard deviation as a score-dispersion check.
- Updated `plot_lomm_pipeline_overview.R` so future scripted versions use the same realism-target list.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed and wrote `paper/Paper_emotion_dynamics_HGolino_RR.pdf`.

### 2026-05-20 Method detail expansion

- Expanded the Method section in `paper/Paper_emotion_dynamics_HGolino_RR.qmd` without editing Results.
- Clarified that Figure 2 is a single-video illustration, whereas the empirical realism check uses the retained 100-clip OpenAI-large MAFW panel with fixed prompt-defined score channels, clip-wise cleaning, interpolation of short missing runs, complete-row retention, and score renormalization.
- Added implementation detail for raw-channel versus latent-readout realism targets, including channel IQR, derivative IQR, standard deviation, lag-1 ACF, run-length entropy, spectral entropy, and effective rank.
- Added benchmark design details from the R scripts: six scenario families crossed with 2D/3D/4D source structures, the core `N`, `T`, indicator-count, measurement-error, loading, and replication grid, and the `k=6` and `T=300` sensitivity slices.
- Added estimator-setting details for DynEGA, GraphicalVAR, and bounded GIMME, plus the implemented assignment, score-alignment, runtime, and failure/timeout metrics.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed successfully.

### 2026-05-19 Coupled-observation LOMM exploratory smoke

- Added an exploratory, non-paper ablation path for the retained-100 OpenAI-FER realism problem: optional correlated latent innovations, stochastic initialization, observation scaling, correlated residuals, common AR(1) observation factor, weak cross-loadings, score-link temperature, post-hoc robust rescaling, and signed-correlation diagnostics.
- New scripts: `run_lomm_openai_fer_observation_ablation.R` and `test_lomm_openai_fer_observation_ablation.R`. The shared generator hooks were added to `extract.r` with defaults chosen to preserve existing simulation behavior unless the new arguments are supplied.
- Smoke command: `env RUN_LOMM_OPENAI_FER_SMOKE=1 LOMM_OPENAI_FER_MAX_CLIPS=10 LOMM_OPENAI_FER_REPS=1 LOMM_OPENAI_FER_FINAL_REPS=2 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_observation_ablation.R`.
- Smoke output directory: `empirical_outputs_exploratory/lomm_openai_fer_observation_ablation/`.
- Smoke result: the best non-diagnostic candidate was `stage_B_obs_0.50_resid_0.25`, with total weighted loss `1.4262`, raw weighted loss `1.2817`, latent weighted loss `1.5707`, and matched louvain `n_dim = 3`.
- Interpretation: on this deliberately small 10-clip smoke subset, the first useful improvement is still boring observation-layer calibration: amplitude shrinkage plus residual covariance outperformed the tested latent-coupling/common-factor variants. Treat this as a pipeline and direction check, not a retained-100 conclusion.
- Verification: `Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r'); cat('loaded\n')"`, `Rscript test_lomm_openai_fer_observation_ablation.R`, and the smoke command above completed.

### 2026-05-19 Coupled-observation LOMM retained-100 bridge and confirmation

- Patched `run_lomm_openai_fer_observation_ablation.R` so conclusion artifacts (`run_summary.csv`, `best_synthetic_panel.csv`, and `ablation_report.md`) rank confirmed `F_final` rows first when they exist, while `candidate_summary.csv` remains the full ladder trace. Also added environment overrides for the ablation grid and final-parent de-duplication by structural parameter key so `F_final` slots are not wasted on duplicate parameter settings from collapsed later stages.
- Bridge command: `env RUN_LOMM_OPENAI_FER_SMOKE=1 LOMM_OPENAI_FER_MAX_CLIPS=30 LOMM_OPENAI_FER_REPS=2 LOMM_OPENAI_FER_FINAL_REPS=5 LOMM_OPENAI_FER_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_observation_ablation_30clip OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_observation_ablation.R`.
- Bridge result: best confirmed row was `stage_F_parent_018_reps_05`, total weighted loss `2.0134`, raw `1.0875`, latent `2.9393`; the bridge target estimated empirical `n_dim = 2` while synthetic was `n_dim = 3`. It favored `obs_scale = 1.00`, no residual/latent/common coupling, and weak cross-loading `cross_loading_sd = 0.05`; this was treated only as a branch-selection signal because the retained-100 target estimates `n_dim = 3`.
- Retained-100 narrowed command: `env LOMM_OPENAI_FER_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_observation_ablation_retained100 LOMM_OPENAI_FER_REPS=3 LOMM_OPENAI_FER_FINAL_REPS=10 LOMM_OPENAI_FER_OBS_SCALE=0.65,0.80,1.00 LOMM_OPENAI_FER_RESID_RHO=0.00,0.10,0.25 LOMM_OPENAI_FER_RHO_Q=0.00,0.15 LOMM_OPENAI_FER_COMMON_SD=0.00,0.05 LOMM_OPENAI_FER_COMMON_PHI=0.50 LOMM_OPENAI_FER_CROSS_LOADING_SD=0.025,0.05,0.075 LOMM_OPENAI_FER_TEMPERATURE=1.00 LOMM_OPENAI_FER_INIT_DX_SD=NA LOMM_OPENAI_FER_STAGE_B_TOP=6 LOMM_OPENAI_FER_STAGE_C_TOP=6 LOMM_OPENAI_FER_STAGE_D_TOP=4 LOMM_OPENAI_FER_STAGE_E_TOP=3 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_observation_ablation.R`.
- Retained-100 result: best confirmed row was `stage_F_parent_014_reps_10`, total weighted loss `1.4356`, raw `1.1621`, latent `1.7091`, empirical/synthetic `n_dim = 3/3`, with `obs_scale = 1.00`, `resid_rho = 0.10`, `rho_q = 0`, `common_sd = 0`, and `cross_loading_sd = 0`. The full retained run did not support the bridge's weak-cross-loading signal.
- Final unique confirmation command: `env LOMM_OPENAI_FER_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_observation_ablation_retained100_confirm_resid_unique LOMM_OPENAI_FER_REPS=3 LOMM_OPENAI_FER_FINAL_REPS=20 LOMM_OPENAI_FER_SHRINK=1.00 LOMM_OPENAI_FER_OBS_SCALE=1.00 LOMM_OPENAI_FER_RESID_RHO=0.00,0.10 LOMM_OPENAI_FER_RHO_Q=0.00 LOMM_OPENAI_FER_COMMON_SD=0.00 LOMM_OPENAI_FER_COMMON_PHI=0.50 LOMM_OPENAI_FER_CROSS_LOADING_SD=0.000 LOMM_OPENAI_FER_TEMPERATURE=1.00 LOMM_OPENAI_FER_INIT_DX_SD=NA LOMM_OPENAI_FER_STAGE_B_TOP=2 LOMM_OPENAI_FER_STAGE_C_TOP=2 LOMM_OPENAI_FER_STAGE_D_TOP=2 LOMM_OPENAI_FER_STAGE_E_TOP=2 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_observation_ablation.R`.
- Final confirmation result: `stage_F_parent_004_reps_20` (`resid_rho = 0.10`) beat the distinct `resid_rho = 0.00` confirmation row: total weighted loss `1.4282` vs `1.4545`, raw `1.1645` vs `1.2026`, latent `1.6919` vs `1.7064`, both with empirical/synthetic `n_dim = 3/3`.
- Interpretation: for retained-100 OpenAI-FER panels, the best current conclusion is deliberately narrow. Robust rescaling remains a diagnostic amplitude knife, but the confirmed structural improvement is a modest exchangeable residual covariance (`resid_rho = 0.10`) with otherwise baseline observation geometry; latent innovation coupling, common AR(1) observation factor, and weak cross-loadings did not improve the retained-100 confirmation. The remaining mismatch is still amplitude/texture heavy: final synthetic raw `channel_iqr` median is about `0.0655` vs empirical `0.0202`, while matched dimensionality passes.
- Verification: `Rscript test_lomm_openai_fer_observation_ablation.R` passed after the final-selection patch; the unique final-selection helper check passed; all three run commands above completed and wrote their output directories.

### 2026-05-20 Range-geometry forensic diagnostics

- Added `run_lomm_openai_fer_range_geometry_diagnostics.R` and `test_lomm_openai_fer_range_geometry_diagnostics.R` to test the retained-100 range/velocity geometry problem without reopening the broader coupled-observation grid. Added shared helpers for amplitude decomposition, empirical tanh compression, channel-IQR frontier summaries, and burn-in pass-through in `extract.r`.
- The diagnostic fixes the confirmed retained-100 structure at `obs_scale = 1.00`, `resid_rho = 0.10`, `rho_q = 0`, `common_sd = 0`, `cross_loading_sd = 0`, and `link = "softmax"`. It then tests fixed/zero/stochastic initialization, 50/100/200-step burn-in, and post-hoc empirical tanh compression.
- Smoke command: `env RUN_LOMM_RANGE_GEOMETRY_SMOKE=1 LOMM_RANGE_GEOMETRY_MAX_CLIPS=8 LOMM_RANGE_GEOMETRY_REPS=1 LOMM_RANGE_GEOMETRY_FINAL_REPS=2 LOMM_RANGE_GEOMETRY_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_range_geometry_diagnostics_smoke LOMM_RANGE_GEOMETRY_INIT_MODES=fixed_current,zero,stationary_burnin LOMM_RANGE_GEOMETRY_BURN_IN=20,50 LOMM_RANGE_GEOMETRY_RADIUS_MULTIPLIER=1.0,2.0 LOMM_RANGE_GEOMETRY_SLOPE=0.75,1.25 LOMM_RANGE_GEOMETRY_TOP_INIT_FINAL=2 LOMM_RANGE_GEOMETRY_TOP_TANH_SOURCES=1 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_range_geometry_diagnostics.R`.
- Retained-100 command: `env LOMM_RANGE_GEOMETRY_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_range_geometry_diagnostics LOMM_RANGE_GEOMETRY_REPS=3 LOMM_RANGE_GEOMETRY_FINAL_REPS=20 LOMM_RANGE_GEOMETRY_INIT_MODES=fixed_current,zero,stochastic,stationary_burnin LOMM_RANGE_GEOMETRY_INIT_X_SD=0.00,0.05,0.10 LOMM_RANGE_GEOMETRY_INIT_DX_SD=0.00,0.10,0.25,0.50 LOMM_RANGE_GEOMETRY_BURN_IN=50,100,200 LOMM_RANGE_GEOMETRY_RADIUS_MULTIPLIER=1.0,1.5,2.0,3.0 LOMM_RANGE_GEOMETRY_SLOPE=0.75,1.00,1.25,1.50 LOMM_RANGE_GEOMETRY_TOP_INIT_FINAL=3 LOMM_RANGE_GEOMETRY_TOP_TANH_SOURCES=2 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_range_geometry_diagnostics.R`.
- Retained-100 result: the best total-loss diagnostic candidate was `final_init_stationary_burnin_050_reps_20`, total weighted loss `1.3247`, raw `1.5651`, latent `1.0843`, empirical/synthetic `n_dim = 3/3`. This improves latent geometry relative to the confirmed fixed-current candidate but overcompresses raw score range: raw `channel_iqr` becomes `0.0028` vs empirical `0.0202` and remains a Fail in the opposite direction; raw `diff_iqr` drops to near zero and becomes Partial.
- The amplitude decomposition for the confirmed fixed-current structural baseline shows a within-clip support problem more than a between-clip offset problem. Median ratios across channels were pooled IQR `2.22`, mean within-clip IQR `3.04`, and between-clip median IQR `0.25`; channel-specific behavior is heterogeneous, with some low-occupancy empirical channels inflated and some high-occupancy channels under-spread.
- The best tanh diagnostic, `tanh_src_001_radius_3.00_slope_0.75`, shows that bounded score compression can make raw range geometry pass without destroying raw local motion: raw `channel_iqr` Pass (`max_discrepancy = 0.330`) and raw `diff_iqr` Pass (`0.286`). However, it worsens latent geometry: latent `channel_iqr` remains a Fail (`2.697`), latent lag-1 ACF and spectral entropy also fail, and total loss is `1.4458`, worse than the burn-in winner and close to the previous confirmed baseline.
- Frontier interpretation: no tested candidate solves both raw and latent `channel_iqr`. Burn-in is useful as a latent-geometry/stationarity diagnostic but too aggressive for raw FER-like score range. Tanh compression is useful as a raw bounded-score diagnostic but does not preserve the latent validation layer. The open problem is therefore not simple global amplitude scaling, nor just fixed initial velocity; it is score-range geometry under the DynEGA latent readout.
- Verification: `Rscript test_lomm_openai_fer_observation_ablation.R`, `Rscript test_lomm_openai_fer_range_geometry_diagnostics.R`, the smoke command, and the retained-100 command completed. The retained run recorded the `init_stationary_burnin_200` dropped-variable edge case as an infinite-loss candidate rather than aborting.

### 2026-05-20 Post-hoc FER score-normalization diagnostic

- Added `run_lomm_openai_fer_score_normalization_diagnostics.R` and `test_lomm_openai_fer_score_normalization_diagnostics.R` to test whether a simpler FER-score normalization can narrow the synthetic score range after the retained-100 confirmation, without reopening the simulator ladder.
- Added shared post-hoc normalization helpers in `extract.r`: empirical range clamping, affine IQR normalization toward empirical channel medians/IQRs, and empirical quantile normalization with an `alpha` blend back to the original synthetic scores.
- Retained-100 command: `env LOMM_SCORE_NORM_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_score_normalization_diagnostics LOMM_SCORE_NORM_SOURCE_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_observation_ablation_retained100_confirm_resid_unique LOMM_SCORE_NORM_ALPHA=0.25,0.50,0.75,1.00 LOMM_SCORE_NORM_METHODS=affine_iqr,empirical_quantile OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_score_normalization_diagnostics.R`.
- Result: the best total-loss candidate remained the unnormalized retained-100 source row, `source_confirmed`, with total weighted loss `1.4282`, raw `1.1645`, latent `1.6919`, and empirical/synthetic `n_dim = 3/3`.
- Best normalized alternatives were diagnostic rather than conclusions. `affine_iqr_alpha_0.75` improved raw `channel_iqr` from Fail to near-threshold Partial (`max_discrepancy = 0.953`) while keeping raw `diff_iqr` Partial (`0.482`), but worsened latent `channel_iqr` to Fail (`1.837`) and total loss to `1.5357`. `empirical_quantile_alpha_0.25` slightly reduced raw loss (`1.1610` vs `1.1645`) but increased latent loss (`1.9602`) and left raw `channel_iqr` just over the Fail threshold (`1.015`).
- Interpretation: simple FER-score normalization is useful as an upper-bound/diagnostic trick, but not a clean fix for the current retained-100 conclusion. It can soften raw marginal score range, yet it either leaves raw `channel_iqr` near the Fail boundary or damages the latent DynEGA validation geometry. The current paper-facing conclusion should therefore remain narrow: weak residual covariance is the only confirmed structural change, and the remaining unresolved defect is raw-plus-latent score-range geometry rather than a simple output normalization problem.
- Verification: `Rscript test_lomm_openai_fer_score_normalization_diagnostics.R`, `Rscript test_lomm_openai_fer_range_geometry_diagnostics.R`, and the retained-100 score-normalization command completed.

### 2026-05-20 Clamped-logit FER scale diagnostic

- Added `run_lomm_openai_fer_logit_scale_diagnostics.R` and `test_lomm_openai_fer_logit_scale_diagnostics.R` to test whether interpreting bounded FER columns on a clamped logit scale explains the retained-100 range mismatch. Added `logit_transform_score_panel()` in `extract.r` and retained latent score panels in `build_property_target_from_panel()` so transformed DynEGA scores are written for empirical and synthetic panels.
- Retained-100 command: `env LOMM_LOGIT_OUTPUT_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_logit_scale_diagnostics LOMM_LOGIT_SOURCE_DIR=/home/atomasevic/dlo/empirical_outputs_exploratory/lomm_openai_fer_observation_ablation_retained100_confirm_resid_unique LOMM_LOGIT_EPS=1e-4 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript run_lomm_openai_fer_logit_scale_diagnostics.R`.
- Output files include score-scale and logit-scale raw panels, rerun DynEGA latent score panels, `metric_comparison_long.csv`, `metric_call_wide.csv`, `metric_call_summary.csv`, `score_range_summary.csv`, and `logit_scale_report.md`.
- Result: the original score scale remained much better than the clamped-logit scale. Score scale reproduced the retained-100 source result: total weighted loss `1.4282`, raw `1.1645`, latent `1.6919`, empirical/synthetic `n_dim = 3/3`. Logit scale worsened strongly: total weighted loss `4.9511`, raw `5.4936`, latent `4.4085`, empirical/synthetic `n_dim = 3/4`.
- Main diagnostic contrast: on logit scale, raw `channel_iqr` became much worse (`0.6790` empirical vs `5.0997` synthetic, Fail), raw `diff_iqr` also failed (`0.4951` vs `0.8811`), raw `std_value` failed more severely (`0.5625` vs `2.8645`), and latent `channel_iqr` remained a severe Fail (`0.5104` vs `2.7443`). Some dependence/dimensionality diagnostics improved on the logit latent scale (`effective_rank`, `mean_abs_pairwise_cor`, and `run_length_entropy` pass), but the range/persistence/spectral failures dominate.
- Interpretation: reversing bounded FER scores with a simple logit transform does not rescue variability. It amplifies the synthetic tails more than the empirical tails and turns the earlier "wide support but acceptable local motion" story into a broader raw-scale failure. This argues against treating the observed FER columns as hidden logits for the main validation. The main paper should retain the original score-scale retained-100 result and mention logit-scale analysis, if at all, as a negative sensitivity.
- Verification: `Rscript test_lomm_openai_fer_logit_scale_diagnostics.R`, script parse checks, the retained-100 clamped-logit command, and post-run call-table generation completed.

### 2026-05-20 Scientific-writing audit of revised RR manuscript front matter

- Reviewed `paper/Paper_emotion_dynamics_HGolino_RR.qmd` using the scientific-writing guidance for flow, argument discipline, and manuscript-facing vocabulary.
- Removed development-history phrasing from the rendered front and Method sections, including revision-analysis and report-facing wording.
- Rephrased LOMM implementation details, DynEGA settings, GIMME feasibility language, and performance accounting as reproducible scientific prose rather than code-facing notes.
- Copied the LOMM trajectory and MAFW example figures into neutral manuscript asset paths under `paper/figures/` and updated the QMD to avoid internal output-directory labels in figure chunks.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed; rendered TeX no longer contains the audited internal/development labels except for the word "tutorial" inside a cited article title.

### 2026-05-20 Method-comparator citation pass

- Added method citations for GraphicalVAR and GIMME in `paper/references.bib`: Epskamp's `graphicalVAR` package citation, Gates and Molenaar's GIMME group-search paper, Lane et al.'s ambulatory assessment GIMME paper, and the TEFI entropy-fit citation used by the DynEGA embedding-selection paragraph.
- Revised the known-truth benchmark Method text to summarize why DynEGA, GraphicalVAR, and GIMME are compared: DynEGA as dimensional recovery from temporal change, GraphicalVAR as dynamic-network estimation separating lagged and contemporaneous relations, and GIMME as a person-specific time-series modeling framework with group/subgroup/individual paths.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed after adding the citations and comparison paragraph.

### 2026-05-20 Community-detection method clarification

- Revised the known-truth benchmark Method section to explain how community detection is applied across DynEGA, GraphicalVAR, and GIMME.
- Clarified that DynEGA returns a population network that is partitioned into communities, whereas GraphicalVAR and GIMME first produce structural matrices that are then converted into weighted undirected graphs and partitioned with Louvain, Leiden, and Walktrap.
- Added missing community-detection citations for Louvain and Leiden in `paper/references.bib`; Walktrap was already present.
- Clarified that GraphicalVAR and GIMME structure fits are reused once per simulated panel, with community-detection variants applied post hoc to the same fitted matrix rather than treated as separate time-series model fits.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed and the rendered TeX includes the new community-detection citations.

### 2026-05-20 Method prose tightening pass

- Reviewed the Method section for scientific flow, redundancy, internal labels, and code-heavy language.
- Streamlined the opening workflow paragraph, empirical calibration prose, realism-target description, scenario-selection text, and known-truth benchmark description.
- Replaced workflow-style phrases such as "core grid," "full-grid," "retained panel," "expensive model," and "silently dropped" with manuscript-facing language while preserving the design values, threshold rules, and estimator details.
- Removed the model-specific label from the MAFW figure caption and described it as zero-shot FER-like scoring.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed; rendered TeX contains no audited Method-section internal labels or unresolved citations.

### 2026-05-20 Empirical realism Results figure and table

- Implemented the first Results component answering whether LOMM-generated FER-like panels resemble empirical MAFW score panels enough for benchmark construction.
- Added `plot_empirical_realism_results.R`, which reads `comparison_revision_report_v2/oai_validation_sensitivity_metrics_v2.csv` and writes `paper/figures/figure4_empirical_realism.pdf`, `paper/figures/figure4_empirical_realism.png`, and `paper/tables/empirical_realism_table.csv`.
- Added Figure 4 and the empirical realism table to `paper/Paper_emotion_dynamics_HGolino_RR.qmd`, reporting observed-channel and Louvain community-score checks for channel IQR, derivative IQR, standard deviation, lag-1 ACF, run-length entropy, spectral entropy, and effective rank.
- Interpreted the result as a qualified benchmark-construction check: local movement, persistence, run-length complexity, spectral complexity, and dimensional spread are pass/partial matches, whereas amplitude dispersion remains too large, especially for channel IQR and standard deviation.
- Verification: `Rscript plot_empirical_realism_results.R` completed, and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-20 Empirical realism metric correction

- Removed standard deviation from the manuscript-facing empirical realism targets, Figure 4, and the empirical realism table.
- Regenerated `paper/figures/figure4_empirical_realism.pdf`, `paper/figures/figure4_empirical_realism.png`, and `paper/tables/empirical_realism_table.csv` from `plot_empirical_realism_results.R`.
- Updated the pipeline overview figure generated by `plot_lomm_pipeline_overview.R` so Panel C lists six realism targets: lag-1 ACF, channel IQR, derivative IQR, run-length entropy, spectral entropy, and effective rank.
- Verification: `Rscript plot_empirical_realism_results.R`, `Rscript plot_lomm_pipeline_overview.R`, and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-20 Method-comparison Results table and Figure 5

- Implemented the next Benchmark Recovery Results component for the matched bounded method-comparison subset.
- Added `plot_method_recovery_results.R`, which reads `comparison_dynega_rr_figure4/figure4_method_performance_assessment.csv`, `figure4_method_accuracy_reliable_summary.csv`, and `figure4_method_accuracy_reliable_differences.csv`.
- Wrote `paper/tables/table1_method_overall.csv` and `paper/figures/figure5_dimension_recovery.{pdf,png}`.
- Updated `paper/Paper_emotion_dynamics_HGolino_RR.qmd` so the method overview is the first numbered table and Figure 5 reports exact dimensional recovery overall, by scenario, and as paired DynEGA-minus-comparator differences.
- Main reported pattern: in the matched bounded subset, DynEGA had the strongest exact dimensional recovery and assignment accuracy with the shortest median runtime; GraphicalVAR recovered many structures but was slower; bounded GIMME had weaker exact recovery and the longest runtime.
- Verification: `Rscript plot_method_recovery_results.R` and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-20 DynEGA factor-effect Results figure

- Added `plot_factor_effects_results.R`, which reads `comparison_dynega_rr_figure4/figure4_factor_effects.csv` and writes `paper/figures/figure6_factor_effects.{pdf,png}` plus `paper/tables/figure6_factor_effects_table.csv`.
- Added a descriptive partial eta-squared Results figure to show which design factors drive DynEGA exact dimensional recovery, matched-score correlation, and matched-score NRMSE across the full DynEGA grid.
- Wording explicitly treats partial eta-squared values as a descriptive main-effect screen, not confirmatory significance tests.
- Main reported pattern: scenario family dominates exact recovery; measurement error and loading strength are smaller for exact recovery but much stronger for continuous score-recovery metrics.

### 2026-05-20 Measurement-quality companion figure correction

- Replaced the manuscript-facing Figure 6 concept after deciding that an eta-squared-only display was not the right companion to the method-recovery figure.
- The updated `plot_factor_effects_results.R` now uses the matched DynEGA, GraphicalVAR, and GIMME comparison subset to show method performance across measurement-error standard deviation and loading strength.
- The partial eta-squared screen is retained as the rationale for choosing the displayed stratifiers, while the plotted result remains a direct method-performance comparison for exact dimensional recovery, assignment accuracy, and matched-score correlation.

### 2026-05-20 Conditional score-correlation correction

- Revised Figure 6 again to drop assignment accuracy and report dimensional recovery as hit rate.
- Score correlations in Figure 6 are now computed only for rows where the corresponding method recovered the correct number of dimensions.
- Missing score-correlation points in the figure indicate strata with no correctly recovered rows for that method.
- Updated Figure 6 intervals so hit rates use Wilson 95% confidence intervals and conditional score correlations use row-level 95% intervals based only on correctly recovered rows.
- Corrected the score-correlation interval logic to use Fisher-z intervals whose standard error is driven by the number of correctly recovered method rows in each stratum, making the precision consequence of lower hit rates explicit.
- Simplified Figure 6 to hit rates only and moved the conditional score-correlation result into `table2_conditional_score_correlations.csv`, reporting mean conditional r, r range across measurement-quality strata, correct-row denominators, widest CI, and missing strata.

### 2026-05-20 Computational feasibility Results figure

- Added `plot_runtime_feasibility_results.R`, which reads the existing runtime and coverage summaries from `comparison_dynega_rr_figure4/`.
- Generated `paper/figures/figure7_runtime_feasibility.{pdf,png}` to show common-base runtime and design-block coverage in one manuscript-style figure.
- Wrote `paper/tables/table3_runtime_feasibility.csv` as an audit table for the reported runtime medians and method speed ratios.
- Added a computational-feasibility Results subsection before the Discussion, emphasizing that GraphicalVAR and GIMME are bounded comparators while DynEGA covers the broader LOMM design grid.
- Verification: `Rscript plot_runtime_feasibility_results.R` and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-20 Computational feasibility figure redesign

- Replaced the bottom coverage-grid panel in Figure 7 with full-grid status accounting: ok rows, timeout rows, error rows, and not-run rows.
- Added a new manuscript audit table, `paper/tables/table3_runtime_feasibility_accounting.csv`, with planned rows and method-specific feasibility notes.
- Clarified the Results prose: GraphicalVAR produced 12 repeated 900-s timeouts in the 4D ranked-persistence base condition and no estimator errors; GIMME produced 18 post-estimation error rows and no timeouts, corresponding to six failed structure fits because each fit is evaluated with three community-detection variants.
- The figure and text now state why remaining GraphicalVAR and GIMME conditions were bounded rather than implying generic noncoverage.
- Verification: `Rscript plot_runtime_feasibility_results.R` and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-21 Runtime figure simplification

- Removed the feasibility-accounting row from Figure 7 so the figure now shows only common-base runtime by true dimensionality.
- Kept the feasibility accounting in the manuscript prose and in `paper/tables/table3_runtime_feasibility_accounting.csv`.
- Updated the Figure 7 caption so it no longer refers to Panel B or Panel C.
- Verification: `Rscript plot_runtime_feasibility_results.R` and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-21 Results and Discussion prose alignment

- Tightened the Results opening so it previews the four actual result moves: empirical realism, dimensional recovery, measurement-quality effects, and computational feasibility.
- Expanded the Discussion from a placeholder into a six-paragraph synthesis covering LOMM's known-truth role, partial empirical realism, method recovery differences, conditional score recovery, computational feasibility, and limitations.
- Updated manuscript-control documents so they match the current RR manuscript: channel IQR failure is now explicit, older "no metric fails" language was removed, and older benchmark/GIMME accounting was superseded by the current matched-subset and feasibility-accounting results.
- Verification: risky wording searches for broad validation language, old political examples, internal scenario labels, and stale benchmark counts returned no manuscript/control-note contradictions; `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-21 Final front-and-back manuscript pass

- Replaced the placeholder abstract with a 211-word unstructured abstract aligned to the current LOMM benchmark framing.
- Updated the title and short title to center the Latent Oscillator Measurement Model and video emotion-dynamics benchmarking.
- Tightened the Introduction and Discussion around empirical calibration, known-truth dimensional recovery, measurement-quality sensitivity, computational feasibility, and bounded claim discipline.
- Removed several defensive "not X, but Y" prose constructions from the Introduction, Methods, Results, and Discussion while preserving necessary scope limits.
- Standardized the manuscript-facing structural recovery metric to "dimensional hit rate" and updated the method-overall table header accordingly.
- Marked reviewer issue R1 as addressed because the title, abstract, introduction, method organization, results sequence, and discussion now follow the same LOMM benchmark structure.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed; searches for forbidden/internal language and broad validation claims returned no manuscript hits except intentional control-document guardrails and remaining metadata placeholders (`Address Here`, `wordcount: "X"`).

### 2026-05-22 Figure 5 terminology fix

- Updated `plot_method_recovery_results.R` so Figure 5 and Table 1 use "dimensional hit rate" / "Hit rate (%)" instead of the older "exact dimensional recovery" / "Exact m (%)" labels.
- Regenerated `paper/figures/figure5_dimension_recovery.{pdf,png}` and `paper/tables/table1_method_overall.csv`.
- Verification: `Rscript plot_method_recovery_results.R`, terminology search for old exact-recovery labels, and `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` completed.

### 2026-05-26 Lecture-notes research notebook

- Added `notebooks/lomm_research_notebook.tex` as a lecture-notes-style consolidation of the LOMM paper scaffold, implementation notes, equations, benchmark design, current result tables, and interpretation guardrails.
- Generated three blackboard-style image assets with the built-in imagegen tool and copied them into `notebooks/figures/`: `blackboard_lomm_state_space.png`, `blackboard_benchmark_pipeline.png`, and `blackboard_empirical_realism.png`.
- The notebook imports current report-facing table fragments from `comparison_revision_report_v2/` instead of hard-coding all result values, so recompilation reflects regenerated report tables.
- Compiled `notebooks/lomm_research_notebook.pdf` successfully from the repository root; final output is 15 pages.
- Verification: `pdflatex -interaction=nonstopmode -halt-on-error -output-directory=notebooks notebooks/lomm_research_notebook.tex` completed after repeated passes with cross-references resolved; remaining warnings are cosmetic overfull lines from long file paths.

### 2026-05-26 Response letter alignment with decision text

- Rebuilt `paper/response_letter.tex` around the actual decision record: editor guidance first, Action Editor comments next, and Reviewer 1 comments point by point with original quoted comments.
- Added page/section pointers in each response so the revised additions can be checked in the rendered manuscript.
- Added response-letter introduction explaining the removal of "damping" from the model name because damping is an optional velocity-feedback regime, noting that short MAFW clips and brief expressions may not show damping, and stating that tutorial material was removed because the `transforEmotion` package tutorial has been published separately and is cited.
- Updated the manuscript Introduction to define MAFW as a large-scale, multi-modal, compound affective database for dynamic facial-expression recognition in the wild.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` and `pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex` completed.

### 2026-05-26 Submission PDF refresh and Drive-upload check

- Refreshed the revised manuscript PDF from `paper/Paper_emotion_dynamics_HGolino_RR.qmd` and rebuilt `paper/response_letter.pdf` from `paper/response_letter.tex`.
- Verified local outputs: `paper/Paper_emotion_dynamics_HGolino_RR.pdf` is 24 pages and `paper/response_letter.pdf` is 8 pages.
- Drive upload is not yet completed: the Google Drive connector's document and presentation importers reject `application/pdf`, and the local `gog` CLI token decrypts with the stored keyring password but fails Google refresh with `invalid_grant`.
- Verification: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf`, `pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex`, local PDF page-count inspection, and `gog` auth/upload-path diagnostics completed.

### 2026-05-30 Repository manuscript tidy

- Cleaned the repository around the current RR manuscript source and renamed visible active workflow language from benchmark to simulation.
- Active manuscript files remain visible in `paper/`: `Paper_emotion_dynamics_HGolino_RR.qmd`, rendered manuscript PDF, response letter source/PDF, references, APA CSL, figures, and tables.
- Grouped active production scripts under `scripts/simulations/`, `scripts/figures/`, `scripts/empirical/`, and `scripts/reports/`; retained non-active exploratory scripts under `scripts/legacy/`.
- Moved active evidence bundles used by manuscript scripts under `simulation_outputs/`.
- Backed up old manuscript drafts/readouts/reports and archived non-active generated outputs, exploratory diagnostics, local tool state, and older archive material under `archive/2026-05-30-manuscript-tidy/`.
- Archive manifest: `archive/2026-05-30-manuscript-tidy/archive_manifest.tsv`.
- Verification commands for this cleanup are the core extract load, manuscript-facing figure/table scripts in `scripts/README.md`, Quarto manuscript render, response-letter PDF build, terminology search, and `git status --short`.
