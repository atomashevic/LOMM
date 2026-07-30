# LOMM Revision V2 Pre-Compute Handoff

## Execution boundary

This preparation package stops before every estimator fit and long-running
revision job. The checked-in manuscript contains revised non-result sections
and non-rendered integration gates. Its Abstract, numerical Results,
quantitative Discussion, result figures and tables, and numerical response
letter remain tied to the checked-in results.

The revision job interface is deliberately read-only by default. A future
launch or resume requires both `--execute` and the exact acknowledgement
`LONG-RUNS-AUTHORIZED`. No command advances the dependency graph
automatically.

## Prepared interfaces

- Job dispatcher: `scripts/simulations/run_revision_job.R`
- Scientific specification: `scripts/simulations/revision_spec.R`
- Result contract: `scripts/simulations/build_revision_results.R`
- Job graph: `scripts/simulations/revision_jobs.json`
- Orchestrator and monitor: `scripts/simulations/orchestrate_revision_jobs.py`
- Output root: `simulation_outputs/revision_v2/<job>/<config-hash>/`

## Safe commands used during preparation

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py plan
python3 scripts/simulations/orchestrate_revision_jobs.py validate
python3 scripts/simulations/orchestrate_revision_jobs.py status
python3 scripts/simulations/orchestrate_revision_jobs.py next
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job t100_dynega
```

`validate` invokes every R job with `--dry-run`. A dry run writes only grid,
count, command, and prospective-manifest artifacts. It does not source or call
an estimator fit.

## Future commands requiring separate authorization

Print a launch command without executing it:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job JOB_ID
```

Launch one dependency-ready job in a guarded tmux session:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py launch \
  --job JOB_ID \
  --execute \
  --ack-long-running LONG-RUNS-AUTHORIZED
```

Resume the same configuration and checkpoint:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py resume \
  --job JOB_ID \
  --execute \
  --ack-long-running LONG-RUNS-AUTHORIZED
```

Read status once or monitor without launching:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py status
python3 scripts/simulations/orchestrate_revision_jobs.py watch --interval 300
```

## Expected scientific units

| Study | Generated panel conditions | Primary future readouts |
|---|---:|---:|
| Empirical five-fold calibration | 13,500 training evaluations and 500 held-out replicates | Clip-level observable metrics plus secondary frozen-summary metrics |
| Matched T = 100, per fitted method | 21,600 | 64,800 Louvain, Leiden, and Walktrap rows |
| Matched T = 25, per fitted method | 21,600 | 64,800 Louvain, Leiden, and Walktrap rows |
| Static EGA plus oracle, each matched grid | 43,200 combined panels across two methods | 129,600 combined readout rows |
| Challenge suite | 4,200 generated conditions | 4,800 representation-panels and 14,400 readout rows per method |

The challenge count includes 600 softmax conditions evaluated on raw and
centered-log-ratio representations. The other six challenge variants each
contribute one representation.

The empirical training count crosses five folds, two calibration regimes, six
position-feedback values, five velocity-feedback values per regime, five
innovation scales, three cadences, and three candidate temperatures. Channel
intercepts are estimated within each training split. The 100 held-out synthetic
replicates per fold are generated only after the fold-specific selection is
frozen.

## Locked job inventory

The following configuration hashes were produced by the final dry-run
validation. Expected units are dispatcher completion units: training and
held-out evaluations for `empirical_cv`, audit panels for the reuse audit,
method-readout rows for estimator jobs, and one validated publication artifact
for each downstream job.

| Job | Config hash | Required dependencies | Expected units | Workers | Job timeout |
|---|---|---|---:|---:|---:|
| `empirical_cv` | `f4cddc6cb402` | None | 14,000 | 12 | 172,800 s |
| `t100_reuse_audit` | `ed29bcc06373` | None | 21,600 | 1 | 3,600 s |
| `t100_dynega` | `966643c4ba53` | `t100_reuse_audit` | 64,800 | 16 | 604,800 s |
| `t100_static_oracle` | `1c9a7b4faffd` | `t100_reuse_audit` | 129,600 | 12 | 604,800 s |
| `t100_graphicalvar_fallback` | `3de29ad19604` | `t100_reuse_audit` | 64,800 | 24 | 604,800 s |
| `t100_gimme_fallback` | `7e169a4b7802` | `t100_reuse_audit` | 64,800 | 24 | 604,800 s |
| `t25_dynega` | `2205c374057a` | `t100_dynega` | 64,800 | 16 | 604,800 s |
| `t25_graphicalvar` | `836985606d75` | `t100_reuse_audit` | 64,800 | 24 | 604,800 s |
| `t25_gimme` | `2f824745bcd4` | `t100_reuse_audit` | 64,800 | 24 | 604,800 s |
| `t25_static_oracle` | `5659b953fbf6` | `t100_static_oracle` | 129,600 | 12 | 604,800 s |
| `challenge_dynega` | `fc6333c2fdab` | `t100_dynega` | 14,400 | 16 | 604,800 s |
| `challenge_graphicalvar` | `d002ffef5bbc` | `t100_reuse_audit` | 14,400 | 24 | 604,800 s |
| `challenge_static_oracle` | `00cc46398811` | `t100_static_oracle` | 28,800 | 12 | 604,800 s |
| `aggregate_results` | `fa09b05befe6` | Required scientific jobs | 1 | 1 | 14,400 s |
| `rebuild_figures_tables` | `b08a52f25415` | `aggregate_results` | 1 | 1 | 7,200 s |
| `integrate_manuscript` | `8c30e364bc25` | `rebuild_figures_tables` | 1 | 1 | 7,200 s |

The reuse audit controls the GraphicalVAR and GIMME alternatives. A verified
reusable artifact satisfies the corresponding T = 100 input; a rejected or
hash-unverified artifact makes its fallback job eligible. The fallback jobs
must not be launched pre-emptively.

## Exact future launch commands

These commands remain examples for a separately authorized phase. Run `next`
before each launch and execute one eligible job at a time. The orchestrator
will refuse an unmet dependency, a second heavy job, an existing lock, or a
missing acknowledgement.

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job empirical_cv --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t100_reuse_audit --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t100_dynega --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t100_static_oracle --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t100_graphicalvar_fallback --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t100_gimme_fallback --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t25_dynega --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t25_graphicalvar --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t25_gimme --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job t25_static_oracle --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job challenge_dynega --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job challenge_graphicalvar --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job challenge_static_oracle --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job aggregate_results --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job rebuild_figures_tables --execute --ack-long-running LONG-RUNS-AUTHORIZED
python3 scripts/simulations/orchestrate_revision_jobs.py launch --job integrate_manuscript --execute --ack-long-running LONG-RUNS-AUTHORIZED
```

For any interrupted eligible job, the exact resume form is:

```bash
python3 scripts/simulations/orchestrate_revision_jobs.py resume --job JOB_ID --execute --ack-long-running LONG-RUNS-AUTHORIZED
```

Before `integrate_manuscript` can become eligible, all pending source markers
must be removed through evidence-backed revision, all five claim-ledger entries
must be supported, and
`simulation_outputs/revision_v2/integrate_manuscript/8c30e364bc25/manuscript_integration_readiness.json`
must record reviewer approval plus the current SHA-256 hashes of both the
`aggregate_results` and `rebuild_figures_tables` `_SUCCESS.json` files.

## Result-dependent manuscript gates

- `empirical_cv`: held-out realism Methods, Figure 4, Appendix B, and related
  Abstract/Discussion claims.
- `t100_reuse_audit`, `t100_dynega`, and `t100_static_oracle`: panel identity,
  common partitioning, static EGA, oracle, and matched T = 100 claims.
- `t25_*`: short-record, same-cadence Results and horizon-qualified Discussion.
- `challenge_*`: initialization, stable feedback, smooth gates, cross-loadings,
  correlated errors and shocks, and raw/CLR bounded-score Results.
- `aggregate_results`: status-inclusive and conditional estimands, paired
  uncertainty, restricted runtime, and denominator accounting.
- `rebuild_figures_tables`: all V2 result displays and terminology changes.
- `integrate_manuscript`: coordinated Abstract, Results, Discussion,
  appendices, response letter, and final render.

The non-rendered source markers in
`paper/Paper_emotion_dynamics_HGolino_RR.qmd` are the integration checklist:

| Source marker | Deferred manuscript surface |
|---|---|
| `REVISION_RESULTS_PENDING[challenge_dynega,challenge_graphicalvar,challenge_static_oracle]` after initialization | Stable and anti-damped initialization comparison |
| `REVISION_RESULTS_PENDING[challenge_dynega,challenge_graphicalvar,challenge_static_oracle]` after activity gates | Five-observation smooth-gate sensitivity |
| `REVISION_RESULTS_PENDING[empirical_cv,challenge_dynega,challenge_graphicalvar,challenge_static_oracle]` | Fold-selected intercept/temperature and raw/CLR reporting |
| `REVISION_FIGURE_PENDING[rebuild_figures_tables]` | Pipeline artwork with the revised study order |
| `REVISION_RESULTS_PENDING[empirical_cv,rebuild_figures_tables]` | Held-out realism display, discrepancy labels, and intervals |
| `REVISION_RESULTS_PENDING[t100_dynega]` | Frozen DynEGA network and paired partition sensitivities |
| `REVISION_RESULTS_PENDING[t100_reuse_audit,t100_dynega]` | Identical-panel claim after key and hash verification |
| `REVISION_RESULTS_PENDING[t100_reuse_audit,t100_dynega,t100_static_oracle,t25_dynega,t25_graphicalvar,t25_gimme,t25_static_oracle]` | Matched T = 100 and short-record method description |
| `REVISION_RESULTS_PENDING[aggregate_results,rebuild_figures_tables]` | Abstract, Results, quantitative Discussion, figures, and tables |
| `REVISION_RESULTS_PENDING[integrate_manuscript]` | Final quantitative interpretation |
| `REVISION_RESULTS_PENDING[empirical_cv]` | Fold-specific unconstrained versus stable negative-damping calibration |

The response-letter source contains a corresponding non-rendered integration
marker for its quantitative responses, discrepancy terminology, figure and
table references, and final locations.

The checked-in realism figure and Appendix B table still contain their legacy
Pass/Partial/Fail labels. Their Methods definition now uses
small/intermediate/large discrepancy bands. The embedded result labels remain
unchanged until `rebuild_figures_tables` is authorized and completed.

## Colleague-comment traceability

| Comment | Implemented source change |
|---|---|
| Title wording | Title changed to “A Latent Oscillator Measurement Model to Simulate Emotional-Expression Score Dynamics in Video” across manuscript and submission surfaces |
| Abrupt Introduction ending | LOMM now leads into a final `Present Research` subsection; the design paragraphs and pipeline figure moved there |
| DynEGA appeared without a rationale | DynEGA is defined in `Present Research` and tied to coordinated-change dimensionality before the Method section |
| Broad DynEGA grid preceded method comparison | The matched comparison now provides the primary ordering, followed by the broad DynEGA extension |
| “dynamic-network methods” and adjacent sentence | Terminology changed to “dynamic methods” and the unnecessary sentence was removed |
| Equation dummy index | The softmax denominator uses `r`, distinct from indicator `j` |
| MAFW abbreviation | The database name is expanded at first Method mention |
| Short time series | A complete `T = 25`, `dt = 0.4` stress-test job family is specified |
| Correlation-Louvain sensitivity | Resolution 0.95 is encoded as a paired secondary DynEGA analysis |
| Pass/Partial/Fail thresholds | Methods now define small, intermediate, and large descriptive discrepancy bands and explain their heuristic scale |

## Validation record

Validation completed on 2026-07-15 under the hard pre-compute boundary:

- All five owned R sources parsed; `scripts/extract.r` loaded with its
  extraction pipeline disabled.
- The revision-specification pure tests passed for exact transitions, roots,
  stability, stationary initialization, horizons, softmax and CLR properties,
  paired seeds, exact grid counts, oracle partitioning, singleton handling,
  GIMME positive-part conversion, component streams, and checkpoint parsing.
- Result-builder fixtures passed for all 23 estimands, six aggregate artifacts,
  conditional reuse/fallback selection, identity gates, distribution-level
  clip bootstrap functionals, deterministic resampling, and resume-safe
  publication. Its CLI dry run reported zero estimator calls and no output
  publication.
- Python byte-compilation and all 11 orchestration fixtures passed. The tests
  covered resource limits, authorization refusal, reuse alternatives, strict
  completion hashes, status/ETA behavior, and the dual-success-hash manuscript
  receipt.
- Full `validate` invoked all 16 dispatcher jobs with `--dry-run`. The current
  manifests validate 21,600 base panels for both matched horizons, 4,200 base
  challenge conditions and 4,800 representation panels, 13,500 empirical
  training evaluations, and 500 held-out evaluation replicates.
- `status`, one-iteration `watch`, `next`, and `print-launch` were exercised.
  The final pre-compute graph had two ready root jobs and 14 blocked dependent
  jobs. Missing or incorrect launch acknowledgement tests returned exit status
  2 and created no session.
- The claim ledger passed with five `needs-evidence` claims and empty evidence
  lists. All 12 non-rendered manuscript and response-letter markers resolve to
  existing job and claim IDs.
- The manuscript rendered successfully to a 48-page PDF, and the response
  letter rendered successfully to a 9-page PDF. The manuscript title extracted
  from the PDF matches the revised title.
- `git diff --check` passed. `aislop scan --changes --json` scored 95 with zero
  errors and zero fixable warnings. Its two nonfixable warnings concern the
  requested single-file orchestrator size and the length of `job_status`.
- At the initial prelaunch checkpoint, revision output directories contained
  prospective and dry-run manifests only, plus the ignored status history.
  The subsequent reuse audit and failed DynEGA attempt are recorded below.
- The closing process check found no active `run_revision_job.R` estimator
  process, no revision launch/resume process, and no tmux session with the
  `lomm-revision-` prefix.

## Post-launch DynEGA correction

The T = 100 reuse audit completed successfully under config hash
`ed29bcc06373`. It found that the legacy GraphicalVAR and GIMME artifacts could
not satisfy the revised output-schema, seed, configuration-hash, and panel-hash
contracts, so their fallback jobs remain required.

The first `t100_dynega` launch used config hash `2212d5486b8f` and passed the
physical cadence `dt = 0.4` into EGAnet's `delta` argument. EGAnet 2.3.0
rejected that value before estimation because its GLLA interface accepts
`delta` values from 1 through 3. The stopped namespace contains 9,792 error
readouts across 3,264 panels. It remains preserved as a failed run and must not
be resumed.

The corrected contract separates the two time scales:

- physical `dt = 0.4` remains in LOMM generation and horizon calculations;
- DynEGA receives `delta = 1` observation interval;
- manifests and result rows record both values and the delta unit;
- the common positive conversion from observation-interval derivatives to
  physical-time derivatives leaves their correlation matrix unchanged.

The correction generated new hashes for `t100_dynega`, its DynEGA descendants,
and the downstream aggregation/publication chain. The current hashes are those
in the locked job inventory above. The failed checkpoint cannot enter any of
the corrected namespaces.

Correction validation passed without fitting an estimator. The regression
suite confirms that EGAnet accepts the observation-unit value and rejects the
physical-cadence value, while the scale-invariance test confirms that the
derivative correlation matrix is unchanged by the common time conversion. The
corrected `t100_dynega` namespace is `966643c4ba53`. Its subsequent run
completed all 64,800 readouts across 21,600 panels without an estimator error.
Fourteen prospective research run manifests, the claim ledger, both manuscript
checks, and both PDF renders also validate.

## T = 100 static-EGA and oracle checkpoint repair

The `t100_static_oracle` computation generated all 129,600 expected rows, but
its first completion check failed because 74 six-row batches were appended in
the error-row column order. The affected batches contained three failed static
EGA readouts and three valid oracle readouts. The original checkpoint is
preserved as `checkpoint_results.failed-d224b3dbd713.csv` with SHA-256
`d224b3dbd713930ee4eec1c9f4523447874bfcd1411fb02b98871262a5b180a6`.

The repair remapped only the 444 affected rows by position, preserved all
129,156 unaffected rows semantically, and produced checkpoint SHA-256
`2a70d2d23adfd35e062661430b09479ddf8f48f80889d40fba067cde0760ada9`.
The runner now aligns every appended batch to the existing checkpoint header
and rejects missing, extra, or duplicate columns. Completion-only resume
validation passed without new estimator fits. The job is `COMPLETE` with
21,600 panels, 129,378 `ok` readouts, and 222 retained static-EGA error
readouts. All 64,800 oracle readouts are valid, and `results.csv` has the same
SHA-256 as the repaired checkpoint.
