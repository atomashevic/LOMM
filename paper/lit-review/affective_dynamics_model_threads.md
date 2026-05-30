# Affective-dynamics model threads for LOMM section opening

Date: 2026-05-18

Scope: quick literature-review pass to support the first paragraphs of `# Expressive Affective Dynamics as an Oscillator Measurement Model` in `paper/Paper_emotion_dynamics_HGolino_RR.qmd`.

Search path:

- Used the `literature-review` skill workflow with `paper-search semanticscholar` queries for affective-dynamics models, latent differential-equation / oscillator models, state-space grids, and dynamic SEM / continuous-time approaches.
- Semantic Scholar returned usable state-space-grid and emotion-dynamics hits, including Hollenstein (2007) and Houben et al. (2015), but several model-family queries returned `RetryError`.
- Reused the existing local LOMM model-landscape review for oscillator / latent differential-equation and dynamic-factor positioning.

Common threads:

- Affective-dynamics work treats emotion as a trajectory rather than a static score.
- The recurring descriptive targets are level/intensity, variability, instability, inertia, transitions between states, dwell time/attractor-like regions, and return after perturbation.
- State-space-grid approaches emphasize movement through regions of a state space and time spent in particular regions.
- Dynamic metric and intensive-longitudinal approaches quantify instability, inertia, and variability as features associated with adjustment and well-being.
- Oscillator and latent differential-equation approaches add a compact mechanistic vocabulary: level, velocity, acceleration, position feedback, velocity feedback, return, persistence, oscillation, and finite-horizon amplification.
- For FER-like video data, the key extension is measurement: observed emotion-label channels should be treated as noisy indicators of latent expressive trajectories, not automatically as independent emotion processes.

Manuscript use:

- Use Kuppens & Verduyn (2017), Houben et al. (2015), and Hollenstein (2007) for the general affective-dynamics motivation.
- Use Boker-style oscillator / latent differential-equation citations plus Ollero et al. for the second-order oscillator grammar.
- Use Nesselroade / Golino DAFS-style citations for the measurement-layer intuition.

## Addendum: latent expressive dimensions for the LOMM opening

Date: 2026-05-18

Scope: targeted follow-up after reviewer concern that positive/negative/neutral was a weak motivating example, especially because neutrality is not implemented as a full measured latent factor in the current benchmark.

Search path:

- `paper-search semanticscholar` queries for circumplex/core-affect, Plutchik emotion-wheel, and PAD emotion dimensions all failed with the current Semantic Scholar retry error.
- Used DOI and publisher lookup for canonical dimension-model entries: Russell (1980), Russell (2003), Russell & Mehrabian (1977), Plutchik (2001), and Schlosberg (1954).

Useful entry points:

- Circumplex/core-affect models motivate valence and arousal as broad latent affect dimensions rather than discrete observed labels.
- PAD models add dominance/control, which is useful when video contexts involve agency, authority, coping, or interpersonal control cues.
- Plutchik-style models are useful as examples of emotion-family or opponent-tendency organization: joy--sadness, trust--disgust, fear--anger, and anticipation--surprise.
- Older facial-expression dimensional work, especially Schlosberg, supports the claim that visible expressions can be organized along dimensions such as pleasantness, activation, attention, and rejection.

Manuscript use:

- Avoid presenting neutrality, positive affect, and negative affect as the conceptual default for LOMM.
- Describe latent variables as theory-guided expressive dimensions or families selected for a target setting.
- Treat observed FER labels as fallible indicators of those latent dimensions.
- If the current notation uses `$Nt$`, frame it as a baseline trajectory from the simplified implementation, not as the defining latent variable of the model.
