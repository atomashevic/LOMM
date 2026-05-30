# Style Guide

## Manuscript Voice

- Write as a quantitative methods paper for psychology and behavioral science readers.
- Prefer concrete model-and-evidence language over package/tutorial language.
- Keep examples neutral and methodological.
- Avoid defending the old manuscript structure. The revision should read as a rebuilt paper, not a patched tutorial.

## Required Framing

- Use "Latent Oscillator Measurement Model (LOMM)" on first mention and "LOMM" thereafter for the method.
- Treat the measurement model as part of LOMM, not as an add-on phrased as "latent oscillator with measurement model".
- Frame LOMM's originality as the integrated use of interpretable latent second-order dynamics, explicit psychometric measurement, and known-truth recovery benchmarking for FER-like time series. Do not frame it as the first oscillator model, first latent dynamic model, or a replacement for ctSEM/DSEM/latent ODE frameworks.
- Treat "two-factor positive/negative sentiment" as a special case, not the conceptual starting point.
- Treat OpenAI-large / zero-shot FER-like scores as empirical input and realism target, not as broad external validation.
- Present DynEGA and GraphicalVAR as the main benchmark comparison.
- Present GIMME as bounded feasibility/sensitivity only.

## Claim Discipline

- Use "partially reproduces", "captures the qualitative dynamical regime", "is suitable for controlled benchmarking", and "shows measurable departures" for realism language.
- For the empirical realism analysis, the strongest supported phrasing is: "the calibrated LOMM simulation approximates local persistence, typical movement scale, directional-regime complexity, spectral complexity, and coarse dimensional organization, while remaining imperfect on amplitude dispersion and exact latent geometry."
- Avoid "fully validates", "demonstrates equivalence", "recovers emotion dynamics" without qualification, and "proves FER validity".
- Do not introduce PCA as a current benchmark result. It is excluded for this revision by meeting decision.
- Do not claim cross-factor coupling in the implemented simulator unless code changes support it.

## Terminology

- Use "FER-like score series" when discussing empirical score matrices from zero-shot/image-classification pipelines.
- Use "legacy DLO" only when referring to older scripts, historical draft wording, or archived output names.
- Use "position feedback" and "velocity feedback" for eta and zeta until notation is fully frozen.
- Use "local persistence" for lag-1 ACF, "typical movement scale" for derivative IQR, "directional-regime complexity" for run-length entropy, "spectral complexity" for spectral entropy, and "dimensional spread" for effective rank.
- Use "amplitude dispersion", "channel IQR mismatch", or "larger synthetic excursions" for the amplitude mismatch shown by the current realism checks.
- Use "dimensional recovery" for correct number of latent dimensions; use "score recovery" for common-score correlation and NRMSE.

## Figures And Tables

- Pipeline figure should appear early.
- Benchmark figures must say whether they show raw signals, network scores, parameter recovery, or simulated trajectories.
- Scenario-example figures are simulated examples only; captions must not imply empirical trajectory matching.
- GIMME tables must include timeout accounting.
