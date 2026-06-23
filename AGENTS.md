# AGENTS.md

This repository is the cleaned revise-and-resubmit workspace for the Latent Oscillator Measurement Model (LOMM) paper. Treat it as a manuscript and simulation-reproducibility repo: keep edits scoped to the active paper, response letter, figure/table scripts, and simulation workflow unless the user asks for broader cleanup.

## Active Sources

- `paper/Paper_emotion_dynamics_HGolino_RR.qmd` is the active manuscript source.
- `paper/Paper_emotion_dynamics_HGolino_RR.pdf` is the rendered manuscript.
- `paper/response_letter.tex` and `paper/response_letter.pdf` are the current response-letter sources and artifacts.
- `paper/figures/` and `paper/tables/` contain manuscript-facing generated assets.
- `scripts/figures/` and `scripts/simulations/` contain the current figure/table and simulation runners.
- `archive/` and `simulation_outputs/` are ignored local payload locations. Do not delete or rewrite archived material unless the user explicitly asks.

Before manuscript or response-letter edits, check the current worktree and preserve unrelated user changes. The manuscript source may already contain in-progress edits.

## Current Study Frame

Use this frame when editing scientific prose:

LOMM is a known-truth generative simulation framework for evaluating dynamic dimensional recovery in facial-expression-recognition-like video score series. It generates latent oscillator trajectories, gates their activity over time, maps them through a block measurement layer into classifier-output score channels, and asks whether recovery methods can recover the known latent organization under controlled measurement and dynamical conditions.

The central manuscript chain is:

1. Video scoring systems produce dense, model-derived score channels for visible affect.
2. Those score channels are fallible measurements, not direct observations of latent emotion dynamics.
3. LOMM makes the latent structure known before recovery methods are interpreted.
4. Empirical realism checks ask whether simulated panels reproduce selected temporal and dimensional signatures of MAFW-derived score panels.
5. Known-truth simulations compare DynEGA, GraphicalVAR, and bounded GIMME on dimensional recovery, block alignment, score recovery, and feasibility.
6. The main conclusion is about target alignment and scalability: DynEGA is best aligned with the block-measured dynamic coordination target in this design; GraphicalVAR is informative but more sensitive to measurement quality and partial-correlation attenuation; GIMME is a flexible feasibility-stress comparator with substantial runtime and completion limits.

Keep the paper focused on method validation for video-derived behavioral time series. Avoid broad claims about true emotion, clinical validity, social meaning, or real-world affective mechanisms unless the manuscript evidence supports them directly.

## Terminology

- Prefer `LOMM`, `known-truth simulation`, `latent oscillator trajectories`, `activity-gated block measurement`, `classifier-output score channels`, `FER-like score panels`, `visible affective expression`, `dynamic dimensional recovery`, `block alignment`, and `computational feasibility`.
- Describe FER channels as model outputs, classifier-output score channels, FER-like score channels, or model-derived measurements. Avoid wording that makes the channels sound like direct emotion observations.
- Use `simulation` for the active manuscript workflow. Historical filenames, environment variables, or archived scripts may still contain `benchmark`; do not propagate that wording into visible manuscript prose unless provenance requires it.
- For the motivating example, prefer the current latent blocks: `approach-affiliation`, `threat-defense`, and `orienting-novelty`.
- When explaining comparator methods, state what each method estimates before describing downstream community detection or score summaries.

## Manuscript Prose Quality Gate

Before delivering abstracts, manuscript paragraphs, response-letter prose, scientific notes, or Google Doc handoff text, read the target passage first. Then draft from the protocol below.

Drafting protocol:

1. Start with the scientific judgment in plain language.
2. Name the concrete object under discussion: a video frame sequence, score channel, latent trajectory, measurement block, simulation scenario, recovery method, figure, or table.
3. Put the method after the question or claim it answers.
4. Translate technical metrics immediately. If a term such as dimensional hit rate, NMI, ARI, matched-score correlation, softmax link, partial correlation, timeout, or perturbation margin appears, say what higher, lower, success, failure, or completion means for the claim.
5. State the main limit once, attached to the claim it limits.
6. End by saying what the result changes for the argument or which boundary it identifies.

Style rules:

- Use short declarative openings with the actor before the action.
- Keep video, score channels, latent dimensions, methods, and simulation conditions visible in the sentence.
- Move from concrete measurement to latent structure to recovery evidence. Do not skip directly from a statistic to a broad methodological claim.
- Use synthesis sentences plus selected numerical anchors. Avoid long coefficient lists and repeated intervals in prose.
- Keep caveats local. A realism limitation belongs with the realism claim; a timeout limitation belongs with the feasibility claim.
- Avoid contrast scaffolding that defines the point by rejecting another point first. State the positive scope directly.
- Avoid pipeline prose, defensive caveat chains, unexplained script shorthand, noun piles, comma-list paragraphs, and inanimate abstractions as the main actors.

Practical sentence and paragraph instructions:

- Hooks: open with one concrete problem or measured fact. Examples: "FER scores are dense measurements of visible expression." "A recovery method should find the latent block that generated the score channels." "Timeouts are part of the method comparison in this design."
- Paragraph shape: use four moves when possible: problem, claim, evidence, meaning. If the paragraph has a model, use question, model, answer, limit.
- Result sentences: give the direction first, then the number. Example: "DynEGA recovered the correct dimensionality most often; its hit rate was ...".
- Method sentences: say the comparison in ordinary language before naming the estimator details. Example: "The comparator asks whether each method can recover the known score blocks from the same simulated panel."
- Caveat sentences: make one precise limit. Example: "This supports recovery under the simulated measurement structure; it does not validate the semantic labels of the FER categories."
- Transitions: move by asking what the next scale adds. Example: "The realism check asks whether the simulated panels look like empirical score panels. The known-truth comparison then asks whether methods recover the structure that generated those panels."
- Cut on revision: method-first openings, stacked abstract nouns, repeated hedges, claims about true emotion, and paragraph endings that only restate the table.

## Scientific Explanation Targets

When revising the manuscript, keep these explanation targets stable:

- LOMM generates latent expressive trajectories first, maps them through block loadings into classifier-output score channels, and then asks recovery methods to recover those blocks from the simulated score matrix.
- DynEGA is aligned with the target because the generated score channels form coordinated dynamic blocks.
- GraphicalVAR is a useful network comparator, but its contemporaneous partial-correlation representation can compress shared block variance under a reflective measurement structure.
- GIMME is useful as a flexible idiographic comparator and feasibility stress test in this bounded design.
- Empirical realism is a calibration check against selected statistical signatures of FER-like panels. It does not validate the emotion labels or claim that simulated trajectories reproduce exact empirical affective processes.

## Build And Verification

Load the core implementation without running the expensive extract pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('extract.r')"
```

Regenerate manuscript-facing figures and tables when relevant:

```bash
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

Rebuild the manuscript and response letter when prose or source changes require rendered artifacts:

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

For manuscript-only guidance edits, a markdown syntax/readability check is enough unless the user asks for rendered PDFs.

## Research Log

Maintain `research-log.md` as an append-only project log when manuscript decisions, simulation-status changes, interpretation changes, or verification commands need a durable trace. Add dated entries at the bottom. Do not rewrite older entries; append corrections when an earlier note becomes stale.

## Communication

For reports and collaborator-facing summaries, separate:

- confirmed from manuscript source or generated artifacts
- derived by script or rerun
- interpretation
- limitation or caveat

Keep summaries short, evidence-grounded, and free of private raw data. Draft emails or handoff text unless the user explicitly asks to send.
