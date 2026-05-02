# Revised first-simulation benchmark figures

## Sources

- `benchmark_outputs_revision_dynega/`
- `benchmark_outputs_revision_graphicalvar/`
- `benchmark_outputs_revision_gimme/`

## Design

This bundle represents the revision of the 2023 emotion-dynamics simulation benchmark.
It preserves the original factor families (`N`, `TimeP`, `NVAR`, `Error`, `Q`, `Loadings`) and crosses them with the current 12 revised scenarios.

DynEGA and GraphicalVAR are the main full-grid comparison.
GIMME is reported as a bounded feasibility comparator: `N=50`, `TimeP=100`, `NVAR=3`, `Error=0.025`, `Q=0.75`, `Loadings=0.6`.

## Outputs

- `01_hit_rate_by_timep.{png,pdf}`
- `02_old_factor_sensitivity.{png,pdf}`
- `03_score_recovery.{png,pdf}`
- `04_runtime_and_failures.{png,pdf}`
- `05_gimme_bounded_feasibility.{png,pdf}`
