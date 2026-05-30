# Benchmark Reporting Policy

Date: 2026-05-01
Updated: 2026-05-21

## Final Policy

DynEGA and GraphicalVAR are the main dimensional-recovery comparators.
GIMME is reported only as a bounded feasibility comparator.

As of 2026-05-21, the manuscript-facing benchmark is the LOMM known-truth
dimensional-recovery benchmark in the RR manuscript. It uses a matched bounded
subset for direct DynEGA, GraphicalVAR, and GIMME comparison, and a full planned
grid for computational-feasibility accounting.

This policy follows from the current feasibility accounting: DynEGA completed
the full planned grid, GraphicalVAR reached repeated timeout boundaries, and
GIMME produced post-estimation errors before larger extensions could be run.

## Main Full-Grid Outputs

Use the RR manuscript tables as the current manuscript-facing summary:

- `paper/tables/table1_method_overall.csv`: matched bounded method comparison.
- `paper/tables/table2_conditional_score_correlations.csv`: conditional score recovery after correct dimensional recovery.
- `paper/tables/table3_runtime_feasibility.csv`: common-base runtime ratios.
- `paper/tables/table3_runtime_feasibility_accounting.csv`: full planned-grid status accounting.

## GIMME Feasibility Outputs

Use the RR feasibility accounting for manuscript claims:

- DynEGA: 3,402 ok rows, 0 timeouts, 0 errors, 0 not run.
- GraphicalVAR: 2,268 ok rows, 12 timeouts, 0 errors, 1,122 not run.
- GIMME: 672 ok rows, 0 timeouts, 18 errors, 2,712 not run.

For GIMME, error rows are tied to post-estimation extraction failures rather
than numerical timeouts; the manuscript should describe GIMME as bounded rather
than failed as a general method.

## Retired Exploratory Branch

The `signed-feedback` / signed-`zeta` outputs and scripts are exploratory only.
They must not be used as the main manuscript benchmark, must not determine the
parameter grid, and must not be described as the paper's core simulation design.

Do not launch additional `run_signed_feedback_*` or signed-`zeta` jobs unless a
separate, explicit decision revives that branch for a named sensitivity analysis.
