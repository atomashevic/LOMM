# Revised First-Simulation Benchmark Design

This benchmark replaces the earlier three-method DLO comparison grid with a revision of the first 2023 emotion-dynamics simulation benchmark.

## Goal

Produce a revision of the original 2023 simulation benchmark that is small enough to finish on the current machine while still preserving the old benchmark's core design factors. The concrete target is a complete DynEGA vs GraphicalVAR main comparison, plus a bounded GIMME feasibility run, with resource monitoring sufficient to stop or resize the run before it harms the host.

## Main Grid

The revised main grid preserves the old benchmark's core factor families but avoids the full factorial explosion. It crosses the current 12 expression-mode scenarios with the factors most needed for the main method comparison:

- scenarios: `low_single_stable`, `high_single_stable`, `high_switch_stable`, `high_switch_volatile`, each at `2D`, `3D`, and `4D`
- `N`: `50`, `100`
- `TimeP`: `100`, `300`
- `NVAR`: `3`, `4`
- `Error`: `0.01`, `0.05`
- `Q`: fixed at `0.75`
- `Loadings`: fixed at `0.6`
- default full-run replications: `10`

`Q` is the latent dynamic-error multiplier: `sigma_q = scenario_base_sigma_q * (Q / 0.75)`.
The time-step rule follows the old script: `dt = 0.4` for `TimeP = 100`, otherwise `dt = 0.25`.

This gives `1,920` scenario-replication rows per main method instead of `97,200`, a `50.6x` reduction.

## Sensitivity Grid

The omitted full-factorial levels are handled as targeted sensitivity runs rather than crossed into the main benchmark.

Recommended sensitivity run:

- scenarios: all 12 revised scenarios
- `N`: `50`, `100`
- `TimeP`: `100`, `300`
- `NVAR`: `3`, `4`
- `Error`: fixed at `0.025`
- `Q`: `0.5`, `0.75`, `1.0`
- `Loadings`: `0.4`, `0.6`, `0.8`
- replications: `5`

This gives `6,480` rows per main method. It should be run only after the main grid completes cleanly.

## Method Policy

DynEGA and GraphicalVAR are the main full-grid methods.

GIMME is a bounded feasibility comparator, not a full-grid method. Its default subset is:

- all 12 revised scenarios
- `N = 50`
- `TimeP = 100`
- `NVAR = 3`
- `Error = 0.025`
- `Q = 0.75`
- `Loadings = 0.6`

The operational GIMME timeout for the bounded feasibility run was tightened
during monitoring. Early bounded rows used a `600` second cap; the final
relaunches used `360` seconds per structure fit. Longer runs are recorded as
timeouts rather than allowed to dominate the benchmark wall time.

GraphicalVAR was also moved to a bounded `600` second hard-timeout relaunch
after monitoring showed chunk tails could otherwise hold the run open.

## CPU Allocation

Recommended allocation on the current 48-thread / 24-core host:

- DynEGA: `BENCHMARK_WORKERS=4`, because each DynEGA worker can use multiple internal cores.
- GraphicalVAR: `BENCHMARK_GRAPHICALVAR_WORKERS=24` with external one-thread BLAS caps and `BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC=600`.
- GIMME: `BENCHMARK_GIMME_WORKERS=8`, with `BENCHMARK_GIMME_TIMEOUT_SEC=360`.

Always use `BENCHMARK_CHECKPOINT_EACH=1` for long runs.

## Resource Monitoring

Monitor every 5 minutes while long runs are active:

- process count and per-process `%CPU`, `%MEM`, RSS, elapsed time, and command
- CPU load and runnable/blocked process pressure
- memory and swap availability
- I/O wait and per-process read/write growth where possible
- checkpoint row growth and recent log lines

Stop or reduce workers if any of these persist for more than two checks:

- load average substantially above useful CPU capacity with low progress
- memory available below `20 GiB`
- non-zero swap usage growing
- sustained I/O wait above `10%`
- GraphicalVAR median row time far above the prior estimate without checkpoint growth
- GIMME timeouts dominating and blocking main-method progress

## Commands

Dry-run the grid without fitting:

```bash
BENCHMARK_DRY_RUN=1 RUN_METHOD_BENCHMARK=1 BENCHMARK_OUT_DIR=benchmark_outputs_revision_grid Rscript run_method_benchmark.R
```

Run main methods separately:

```bash
BENCHMARK_WORKERS=4 BENCHMARK_CHECKPOINT_EACH=1 Rscript run_dynega_full_suite.R
BENCHMARK_GRAPHICALVAR_WORKERS=24 BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC=600 BENCHMARK_CHECKPOINT_EACH=1 Rscript run_graphicalvar_full_suite.R
BENCHMARK_GIMME_WORKERS=8 BENCHMARK_GIMME_TIMEOUT_SEC=360 BENCHMARK_CHECKPOINT_EACH=1 Rscript run_gimme_full_suite.R
```

Run the sensitivity grid:

```bash
BENCHMARK_FULL_REPS=5 \
BENCHMARK_FULL_ERROR=0.025 \
BENCHMARK_FULL_Q=0.5,0.75,1.0 \
BENCHMARK_FULL_LOADINGS=0.4,0.6,0.8 \
BENCHMARK_OUT_DIR=benchmark_outputs_revision_sensitivity_dynega \
BENCHMARK_WORKERS=4 \
BENCHMARK_CHECKPOINT_EACH=1 \
Rscript run_dynega_full_suite.R

BENCHMARK_FULL_REPS=5 \
BENCHMARK_FULL_ERROR=0.025 \
BENCHMARK_FULL_Q=0.5,0.75,1.0 \
BENCHMARK_FULL_LOADINGS=0.4,0.6,0.8 \
BENCHMARK_OUT_DIR=benchmark_outputs_revision_sensitivity_graphicalvar \
BENCHMARK_GRAPHICALVAR_WORKERS=24 \
BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC=600 \
BENCHMARK_CHECKPOINT_EACH=1 \
Rscript run_graphicalvar_full_suite.R
```

Generate revision figures:

```bash
Rscript plot_revision_first_benchmark_figures.R
```
