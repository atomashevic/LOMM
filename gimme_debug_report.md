# GIMME Debug Report

Date: 2026-05-01

## Scope

Investigate why the current GIMME benchmark path is slow and failure-prone compared
with DynEGA and GraphicalVAR.

## Findings

1. The current slow path is the shared `gimmeSEM()` structure fit, not the
   post-hoc community variants.

   `run_gimme_specs_for_scenario()` fits the GIMME structure once and then reuses
   that matrix for `louvain`, `walktrap`, and `leiden`. In timeout rows, all three
   method variants therefore have the same runtime because they are reporting the
   same failed structure fit.

2. The wrapper-level `node stack overflow` failure has already been addressed in
   the current code.

   Installed `gimme` 10.0 starts `gimmeSEM()` with
   `arguments <- as.list(sys.frame(which = 1))`. Calling that function through the
   benchmark wrapper caused earlier failures such as `node stack overflow` and
   `cannot coerce class '"environment"' to a data.frame`. The current benchmark
   builds a local copy of `gimmeSEM()` and replaces that expression with
   `arguments <- as.list(match.call())[-1L]`. The expression-mode GIMME smoke run
   now completes all 12 scenarios under the patched wrapper.

3. Remaining hard failures are data/model-estimation failures, not parser failures.

   The preserved diagnostic bundle for `emotional_volatile_2d` shows a lavaan
   covariance warning, `fit$psi` containing 10 logical placeholders rather than
   matrices, and unstable observed panels with extremely large values. The current
   extractor now reports this as a post-estimation `fit$psi` failure and preserves
   the raw GIMME output files.

4. The full-size GIMME benchmark is still too slow for some ordinary-looking
   scenarios.

   The resumed full run in
   `benchmark_outputs_full_gimme_comparable_20260427-132102/checkpoint_results.csv`
   has 105 ok rows and 96 timeout rows. The pathological family is no longer only
   `low_single_stable_4d`; after the 1-hour timeout resume, `high_single_stable_2d`
   also timed out repeatedly.

   A fresh bounded repro confirms this:

   ```sh
   env RUN_METHOD_BENCHMARK=1 \
     BENCHMARK_METHODS=gimme \
     BENCHMARK_OUT_DIR=benchmark_outputs_gimme_high_single_2d_timeout_probe \
     BENCHMARK_SCENARIOS=high_single_stable_2d \
     BENCHMARK_FULL_TIMEP=150 \
     BENCHMARK_FULL_BASE_ERR=0.02 \
     BENCHMARK_FULL_REPS=1 \
     BENCHMARK_FULL_P=4 \
     BENCHMARK_FULL_N_VIDEOS=30 \
     BENCHMARK_GIMME_WORKERS=1 \
     BENCHMARK_GIMME_COMMUNITY_ALGORITHMS=louvain,walktrap,leiden \
     BENCHMARK_GIMME_TIMEOUT_SEC=90 \
     BENCHMARK_PROGRESS=1 \
     Rscript run_method_benchmark.R
   ```

   Result: all three GIMME variants timed out after 90.109 seconds before any
   variant-specific community detection could matter.

5. The profile points into lavaan/GIMME model search.

   `debug_gimme_profile.R` profiles a single GIMME structure fit. For
   `high_single_stable_2d`, `TimeP=150`, `base_err=0.02`, `p=4`,
   `N_videos=30`, the profile is saved at:

   - `gimme_profile_high_single_2d/high_single_stable_2d__timep150__err0.02__rep1.Rprof`
   - `gimme_profile_high_single_2d/high_single_stable_2d__timep150__err0.02__rep1.summary.txt`

   The dominant sampled functions are matrix inversions and lavaan information
   matrix work:

   - `solve.default`: 36.92% self time
   - `crossprod`: 10.40% self time
   - `chol2inv`: 7.98% self time
   - `lav_model_information`: 70.84% total time
   - `search.paths.ind` / `indiv.search`: 60.49% total time
   - `search.paths.ms` / `search.paths`: 39.31% total time

6. No tested official full-size GIMME configuration avoided the hard-case
   timeout.

   The probe summary is stored in `gimme_configuration_probe_summary.csv`.
   Results on `high_single_stable_2d`, `TimeP=150`, `base_err=0.02`,
   `p=4`, `N_videos=30`:

   - current default, `stop_crit="model fit"`, `VAR=TRUE`: timeout at 90 s
   - official `stop_crit="standard"`, `VAR=TRUE`: timeout at 180 s
   - `stop_crit="standard"` with looser fit criteria
     (`RMSEA/SRMR=.08`, `CFI/NNFI=.90`, `n_excellent=1`): timeout at 180 s
   - `stop_crit="standard"` with official FDR correction at both group and
     individual stages: timeout at 180 s
   - diagnostic `VAR=FALSE`, `stop_crit="standard"`: timeout at 180 s

   The same method remains tractable in the small smoke design
   (`p=2`, `N_videos=10`), where `high_single_stable_2d` finished in about
   19.3 seconds. The failure boundary is therefore the full-size panel/search
   burden, not only the named scenario family.

## Interpretation

The current bottleneck is not CPU underuse, post-hoc graph clustering, or a simple
benchmark orchestration loop. The benchmark is asking `gimmeSEM()` to run a very
expensive individual path search and repeated lavaan model-information
calculations on full-size simulated panels. Runtime becomes nonlinear and
unstable when moving from the smoke size to `p=4`, `N_videos=30`.

The current implementation is now better at containing this behavior: one GIMME
fit is reused across community variants, hard timeouts are enforced, timeout rows
are marked explicitly, and post-estimation `fit$psi` failures are diagnosable.
But the method remains a poor fit for the full benchmark grid. The official
stopping and fit-threshold controls tested here did not produce a defensible
full-size configuration that avoids timeouts on the representative hard case.

## Recommended next changes

1. Keep `BENCHMARK_GIMME_TIMEOUT_SEC` mandatory for full GIMME runs.
2. Do not compare GIMME wall time by summing the three community variants; count
   one structure fit per scenario. `summarize_full_suite_results.R` now writes
   `full_suite_summary_by_family_fit.csv` for this purpose.
3. Treat `p=4`, `N_videos=30` GIMME as not defensible for routine full sweeps
   unless a deeper method-specific rewrite is introduced. The defensible
   benchmark policy is to report GIMME as a smaller sensitivity comparator
   rather than a peer full-grid method.
4. If GIMME must remain in the full grid, keep timeout rows as first-class
   results and interpret them as method non-completion, not missing data.
5. Use `debug_gimme_profile.R` for future one-case probes instead of launching
   the full benchmark. If the profiling run has to be interrupted, summarize the
   raw profile afterwards with:

   ```sh
   Rscript debug_gimme_profile.R --summarize-rprof=<path-to-profile.Rprof>
   ```
