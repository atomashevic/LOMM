2026-06-23

- Restored the active final simulation result surface from the June 2026 full-result archive into `simulation_outputs/final_simulation_results/`.
- Active benchmark payloads now contain only the latest full manuscript runs: DynEGA full grid, DynEGA bounded CD readouts, GraphicalVAR bounded NVAR=4, and GIMME bounded NVAR=4.
- Removed archived benchmark/checkpoint result directories and archived simulation-output trees so old smoke, pilot, and duplicate result payloads are no longer present in the repository tree.
- Regenerated `comparison_to_current_manuscript`, Figures 4-7, and Tables 1-3 from the active result files. The Quarto render advanced through all R chunks but stalled in Pandoc and was interrupted; manuscript-facing figures and tables were regenerated successfully.
- Diagnosed the manuscript render stall as a bounded-run/feedback issue rather than a persistent Pandoc failure. A clean `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` now completes and writes fresh PDF, TeX, and log artifacts.
- Cleaned avoidable LaTeX warnings by removing redundant header packages, replacing bold Greek `\mathbf` matrix notation with `\boldsymbol`, and using a single table label for the score-quality longtable.
- Checked results-section float placement from rendered PDF pages. Figures 4-7 now use fixed `[H]` placement; Figure 6 appears before Table 2, Table 2 caption and body stay together, and Figure 7 appears with the computational-feasibility text.
- Revised the Scenario Selection paragraph to describe the six scenario families without stale primary-design, sensitivity, or three-replication wording. The exact final grid sizes and replication counts now remain in the dimensional-recovery simulation paragraph.
- Revised the empirical-realism Discussion boundary to state the directional calibration problem directly: the current observation layer leaves marginal score dispersion too high relative to the empirical panels.
- Clarified the Table 1 caption so readers can distinguish status-inclusive hit-rate denominators from score diagnostics computed only on successful rows with recovered score summaries.
