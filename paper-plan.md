# Paper Plan

## Current Target

- Venue: Behavior Research Methods revise-and-resubmit.
- Working direction: focused methods paper, not a tutorial paper.
- Working title direction: Decoding Emotion Dynamics in Videos with the Latent Oscillator Measurement Model.
- Source-of-truth revision note: `/home/atomasevic/lab/methods/dlo-paper-rr.md`.
- Current repo state checked: 2026-05-11.

## Strategic Frame

The revised manuscript should make three contributions and keep everything else subordinate:

1. Specify the Latent Oscillator Measurement Model (LOMM) for FER-like video emotion time series.
2. Check whether calibrated simulations reproduce measurable properties of empirical OpenAI-large FER-like score series.
3. Benchmark DynEGA against defensible dimensional-recovery alternatives on simulated data.

The current manuscript should stop trying to be all of these at once: package paper, tutorial, political illustration, simulator paper, and DynEGA validation paper.

## Recommended Manuscript Structure

1. Introduction
   - Problem: emotion dynamics in video are multivariate, time dependent, and difficult to validate with real annotated data alone.
   - Contribution 1: Latent Oscillator Measurement Model (LOMM) for FER-like time series.
   - Contribution 2: empirical realism checks against OpenAI-large score series.
   - Contribution 3: dimensional-recovery benchmark for DynEGA and alternatives.

2. Pipeline Overview
   - One early figure showing empirical score extraction, generalized simulator, benchmark methods, and outputs.
   - CLIP/OpenAI-large belongs here as an empirical score source and realism target, not as the conceptual center.

3. Latent Oscillator Measurement Model
   - Define latent state, velocity, position feedback, velocity feedback, noise placement, integration, and block measurement model.
   - Use Kankaanpää et al. (2026) to motivate why the measurement layer is not optional: measurement error in dynamic networks reduces edge recovery and sensitivity, and single-indicator dynamic networks are especially vulnerable.
   - Present the two-factor sentiment setup only as a special case.
   - Keep neutrality out of the core formalism unless it is explicitly implemented.

4. Empirical Realism Check
   - Use the retained-100 OpenAI-large MAFW target derived from the 130-clip scoring run.
   - Report metric-level similarities and mismatches rather than visual trajectory matching.
   - Treat this section as done for the current revision scaffold unless new reviewer constraints require rerunning it.
   - State the claim cautiously: the calibrated LOMM simulation approximates the empirical dynamical regime, not exact trajectory-level or full distributional equivalence.
   - Dynamical-system interpretation: lag-1 ACF checks local persistence, derivative IQR checks typical step-to-step movement, run-length entropy checks directional-regime complexity, spectral entropy checks frequency-domain complexity, and effective rank checks multivariate dimensional spread.
   - Current result: observed-channel derivative IQR, lag-1 ACF, and run-length entropy pass; spectral entropy and effective rank are partial; channel IQR fails. The community-score readout shows the same broad pattern with stronger amplitude mismatch.
   - Limitations: channel IQR shows overly large score dispersion, and effective rank is only partial, so the simulator matches coarse dimensionality better than exact variance concentration or amplitude distribution.

5. Benchmark Simulation
   - Active target: extend the old emotion-dynamics simulation in `paper/Paper_emotion_dynamics_HGolino.Rmd` / `paper/Script_Simulation_Dynamics_Emotion_2023-readout.md`, not the later signed-feedback branch.
   - Methods: DynEGA, GraphicalVAR, and GIMME on the same simulated panels where computationally feasible.
   - Dimensions: explicitly test 2, 3, and 4 measured latent factors.
   - Measurement factors are substantively important, not just nuisance parameters. Interpret loading strength and measurement-error conditions as part of the main benchmark logic, with Kankaanpää et al. (2026) as the closest dynamic-network measurement-error citation.
   - Model scope: remove neutrality from the benchmark-generating model and treat it, if mentioned, only as a historical special case from the old draft.
   - Scenario scope: include the original oscillator-style emotion-dynamics scenarios plus empirically calibrated scenario families; do not use signed-feedback or internal scenario-development terminology as manuscript framing.
   - GIMME remains a bounded feasibility comparator. Current feasibility accounting distinguishes ok rows, timeout rows, post-estimation error rows, and not-run conditions.
   - PCA is excluded for this revision based on the Hudson/Alex meeting decision.

6. Brief Empirical Illustration
   - Neutral, non-political, and short.
   - Do not revive the Boris Johnson / Brexit example.
   - Do not turn the section into a tutorial.

7. Discussion
   - Strongest supported claim: the simulator is useful for controlled dimensional-recovery benchmarking and partially reproduces empirical time-series structure.
   - Use the Kankaanpää et al. (2026) comparison to distinguish this paper's target from edge-recovery studies: LOMM benchmarks known-truth latent dimensional organization in noisy FER-like panels, while Kankaanpää et al. evaluate dynamic-network edge recovery under different measurement approaches.
   - Limitations: zero-shot FER validity, label dependence, amplitude and dispersion mismatch, approximate rather than exact latent geometry, and GIMME feasibility limits.

## Current Writing Implications

- Use `paper/Paper_emotion_dynamics_HGolino_RR.qmd` as the RR Quarto manuscript scaffold. It preserves the old manuscript content and contains non-rendered `RR-TODO` comments marking where changes from the R&R should be inserted.
- Use `reports/revision_benchmark_report_v2.tex` as the immediate results scaffold.
- Use `paper/gdlo_working_note.tex` as the model-notation scaffold, but reconcile code and notation before prose freeze.
- Use `paper/lit-review/lomm_model_landscape_review.md` and `paper/figures/lomm_model_landscape_map.*` for the literature-positioning and originality paragraph: LOMM is an integrated generator plus measurement model plus recovery benchmark, not the first oscillator, latent dynamic model, or general ctSEM/DSEM replacement. The review now treats Kankaanpää et al. (2026) as the priority citation for dynamic-network measurement-error motivation.
- Use the current RR manuscript tables and figures as the manuscript-facing benchmark evidence.
- Use the current RR empirical realism table and Figure 4 as the manuscript-facing realism evidence.
- Do not add broad validation language unless it appears in `claims-ledger.md`.

## Open Decisions

- Whether OpenAI-large should be named as the empirical target in the main manuscript or described generically as zero-shot FER-like scores.
- Which neutral empirical illustration replaces the political example.
- Whether the `Q2 -> m` mapping is included in main text or supplement.
- Whether neutrality is removed, retained as an optional extension, or treated outside the formal model.
- Decide whether any additional sensitivity analyses belong in the supplement; do not add new main-text Results unless a reviewer constraint requires it.
