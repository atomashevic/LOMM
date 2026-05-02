# Benchmark Reporting Policy

Date: 2026-05-01

## Final Policy

DynEGA and GraphicalVAR are the main full-grid benchmark comparison.
GIMME is reported only as a bounded feasibility and sensitivity comparator.

This policy follows from the GIMME probes: official GIMME configurations did not
avoid timeout on the representative full-size case
`high_single_stable_2d`, `p=4`, `N_videos=30`, `TimeP=150`,
`base_err=0.02`.

## Main Full-Grid Outputs

Use `benchmark_outputs_revision_summary/` for the final summary tables.

- `main_full_grid_summary_by_method.csv`: DynEGA vs GraphicalVAR only.
- `main_full_grid_summary_by_fit.csv`: fit-level runtime/status for the main methods.
- `main_full_grid_summary_by_mode_n.csv`: main-method results by scenario mode and true dimension count.
- `full_suite_summary.md`: combined markdown report with the policy stated at the top.

Use `comparison_revision_first_benchmark/` as the main comparison figure bundle.
Figures 1-4 show the main DynEGA versus GraphicalVAR full-grid comparison;
Figure 5 reports GIMME as bounded feasibility only.

## GIMME Feasibility Outputs

Use these only for feasibility/sensitivity reporting:

- `benchmark_outputs_reporting_policy_final/gimme_feasibility_by_fit.csv`
- `benchmark_outputs_reporting_policy_final/gimme_feasibility_by_method.csv`
- `benchmark_outputs_reporting_policy_final/gimme_feasibility_by_scenario.csv`
- `benchmark_outputs_reporting_policy_final/gimme_configuration_probe_summary.csv`
- `gimme_debug_report.md`

Current bounded GIMME accounting: 120 structure fits represented in the final
revision run, with 21 completed fits, 96 timeouts, and 3 errors. Because GIMME
variants can share one structure fit, use fit-level GIMME tables for runtime and
completion claims when multiple variants are present.
