# Plain-Language Revision of the LOMM Manuscript

Target file: `paper/Paper_emotion_dynamics_HGolino_RR.qmd`

Goal: replace invented internal vocabulary with the standard technical English used in
*Behavior Research Methods* simulation papers, move heavy math out of the main text, and
restructure the paper so the argument is clear before the reader reaches the Method.
No number, result, or citation changes.

**Governing test:** a graduate student who has read Golino & Epskamp (2017) and nothing
else from this project should be able to follow the paper. Exactly one new term is
allowed: LOMM.

---

## 1. Diagnosis

The manuscript was not vague — it was over-precise in a private language. Three habits
caused the problem:

1. **Invented terms used as if standard.** "score surface", "readout",
   "observation profile", "status-inclusive", "loading-aligned", "evidence lanes" were
   project-internal codenames appearing at very high density (panel 115×, profile 81×,
   readout 26×, status-inclusive 26×).
2. **Stacked noun modifiers.** "selected held-out properties of classifier-output score
   panels" chains five modifiers before the reader learns what is being done.
3. **Agentless abstraction.** "A focused loading-aligned sensitivity examines whether
   clip-level heterogeneity changes recovery on the linear surface" has no actor, no
   method, and three codenames.

---

## 2. Terminology mapping (as implemented)

| Old | New |
|---|---|
| "score surface" / "linear surface" / "compositional surface" | name the data type: **"bounded scores that sum to one"** vs. **"continuous indicators"** |
| "panel" (data sense) | **"simulated dataset"**, "score series", "multivariate time series". Figure panels A–D keep "panel" |
| "observation profile" | **"configuration"** / "observation model"; the eight Study 1 variants are named by component (identical clips, clip baseline, clip scale, activity transition) |
| "readout" | name the object: **"the network each method estimates"**; "derivative-based network (DynEGA)", "lagged-residual network (GraphicalVAR)" |
| "held-out properties" / "fold-held-out discrepancy" | **"summary statistics of the held-out clips"**, "discrepancy on held-out clips" |
| "clip heterogeneity" | **"between-clip differences in baseline level and scale"**; always "video clip", never bare "clip" (CLIP-model collision) |
| "loading-aligned" | defined once ("baselines drawn inside the space spanned by the loadings"); thereafter "the between-clip-variability analysis" |
| "status-inclusive rate" | defined once ("failed or timed-out fits counted as incorrect"); thereafter **"worst-case accuracy"** |
| "recover known blocks" | **"recover the correct number of dimensions and the correct assignment of variables to dimensions"**. "block" survives only in Appendix B, defined there |
| "known-truth recovery" vs. "observable resemblance" | **"recovery of a known structure"** vs. **"similarity between simulated and real data"** |
| "evidence lanes" | "the two studies answer different questions" |
| "record" / "short records" | **"time-series length"**, "shorter time series (T = 25)" |
| "declared" | **"prespecified"** / "data-generating" |

Style rules applied: name the actor in every analysis sentence; define once and reuse
verbatim; unpack 3+-modifier noun stacks at first use; keep every R&R scope hedge,
translating only the vocabulary.

---

## 3. Structural changes

**Section names are now study-based:**

| Old | New |
|---|---|
| Empirical Calibration and Observable-Resemblance Targets | Study 1: Comparing Simulated and Real Score Series |
| Scenario Selection | Choosing Simulation Scenarios |
| Dimensional-Recovery Simulation Study | Study 2: Recovery of Dimensions from Data with Known Structure |
| Five-Fold Profile Comparison | Study 1 Results: Similarity of Simulated and Real Score Series |
| Known-Truth Dimensional Recovery | Study 2 Results: Recovery of the Number of Dimensions |
| Targeted Robustness and Observation-Profile Sensitivity | Robustness and Sensitivity Analyses |
| Model Scope and Identifiability | What LOMM Can and Cannot Identify |

`tbl-study-profiles` became `tbl-study-design`, with the column "Profile and surface"
replaced by "Data type and configuration" and every cell rewritten.

**Math reduced to three numbered equations in the main text:**

1. the latent transition,
2. the observation equation,
3. the softmax map to bounded scores.

Each is followed by a plain-English walkthrough of its parameters. Everything else moved
to a new appendix.

**Appendices relettered.** A new Appendix A holds the complete formal specification;
all previous appendices shifted by one letter:

| New | Contents | Previously |
|---|---|---|
| A | Complete Specification of LOMM (relocated math) | *new* |
| B | What Each Method's Network Looks Like in the Population | A |
| C | Full Results of the Simulated-versus-Real Comparison | B |
| D | Grouping and Trajectory Diagnostics Among Usable Fits | C |
| E | Runtime and Feasibility Accounting | D |
| F | Scenario Parameter Values | E |
| G | Community-Detection Sensitivity | F |

Appendix A collects: the closed forms for the transition and input matrices, stability
conditions and characteristic roots, the finite-horizon amplification diagnostics, the
stationary covariance used for independent starts, the gate product-rule and
finite-difference identities, the three clip-baseline constructions, the log-ratio
definitions and identification argument, the period/damping conversion, and the full
identifiability and aliasing discussion.

---

## 4. Figures, tables, and collateral

Figure and table generators were updated so that embedded text matches the manuscript,
then regenerated from the existing aggregates (no simulations re-run):

- `scripts/figures/plot_lomm_pipeline_overview.R` — node and annotation labels
- `scripts/figures/build_revision_v2_artifacts.R` — figure titles, axis labels, strip labels
- `scripts/figures/build_compositional_profile_artifacts.R` — configuration labels, axis
  label, captions, and CSV headers (`Mean discrepancy`, `Test clips`, `Configuration`,
  `Analysis`, `Representation`)
- `scripts/figures/plot_perturbation_readout_curves.R` — legend and annotation text
- `scripts/figures/plot_lomm_mafw_closest_timeseries.R` — caption and summary label

QMD table-rendering code was updated to match the new CSV headers, and
`scripts/figures/test_compositional_profile_artifacts.R` was updated to assert the new
column names and caption wording.

`paper/response_letter.tex` follows the same vocabulary, and every `\location{}` gives the current
section names, appendix letters, figure numbers, and page numbers, each checked against the
rendered PDF rather than carried over.
`README.md`, `scripts/README.md`, and `REPRODUCIBILITY.md` prose were updated, leaving
machine identifiers such as the `loading_aligned` config key untouched.

---

## 5. What was deliberately not changed

- Any numeric result, interval, or percentage (verified by a token-level diff of the
  prose against the pre-revision manuscript).
- The substantive separation of the two studies and every scope limitation added during
  the R&R.
- Machine identifiers: config keys, snake_case CSV schema columns, artifact filenames,
  and Quarto cross-reference labels.
- Appendix B's derivations, which received a terminology pass only.
