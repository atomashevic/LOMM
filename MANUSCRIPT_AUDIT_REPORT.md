# LOMM Manuscript Audit — Pre-Submission Readiness Report

Scope: `paper/Paper_emotion_dynamics_HGolino_RR.qmd` (revised LOMM manuscript, formerly DLO-MM),
target journal *Behavior Research Methods*. Audit performed 2026-08-13 against the latest
checked-in state (commit `89263b4`, clean tree). All quotations of results below were
cross-checked against `paper/tables/*.csv` and the adoption receipts in
`scripts/lomm/evidence/`. Every issue has a concrete fix instruction for a coding agent.

---

## 0. What is new in this revision (context for the plan)

1. **Renamed model**: DLO-MM → **LOMM** (Latent Oscillator Measurement Model), three
   independently variable layers (latent dynamics / activity / observation), with a
   compositional B+S observation profile as the public default.
2. **Plain-language rewrite** (commit `1a06c25`): internal codenames removed, three
   numbered equations in the main text, study-based section names, new Appendix A
   (complete formal specification), appendices re-lettered A–G.
3. **New Appendix B**: population-level argument that derivative-based (DynEGA) and
   lagged-residual (GraphicalVAR) networks share the same `1/(k-1)` upper bound on
   within-dimension partial correlations, plus a shared perturbation margin.
4. **Redesigned Study 2**: worst-case accuracy (failures/timeouts counted as incorrect),
   a true-structure oracle, static EGA, a T = 25 stress design, a 518,400-dataset broad
   DynEGA grid, and targeted departures including bounded scores.
5. **Redesigned Study 1**: eight-configuration factorial (clip baseline × clip scale ×
   activity transition) with bootstrap diagnostics and a gap-interpolation sensitivity.

### 0.1 What was verified and is CORRECT (do not redo)

- **All headline numbers match the checked-in tables and receipts.** In particular:
  - Study 1: 0.3250 → 0.2567 (−21.0%); score-scale estimate 0.2218, bootstrap mean
    0.2701, basic [0.1433, 0.1992], percentile [0.2444, 0.3002]; baseline main effect
    −0.0418, scale −0.0188; interpolated 0.3187 / 0.2707 / 0.2511; worst statistics
    0.3499 (total variance, scores), 0.4423 → 0.4639 (first-difference IQR, log-ratio).
    All present in `compositional_profile_discrepancy*.csv` and the realism receipt.
  - Study 2 main benchmark (Louvain, T = 100): DynEGA 0.939, static EGA 0.884,
    GraphicalVAR 0.777 (0.941 × 0.826), GIMME 0.176 (0.224 × 0.783), oracle 1.000.
  - T = 25: 0.863 / 0.913 / 0.770 / 0 / 1.000; community-algorithm spread 0.0059
    (T = 100) and 0.0033 (T = 25).
  - Departures (Louvain differences): independent starts −0.155 (DynEGA), −0.387
    (static EGA), −0.430 (GraphicalVAR); gradual gates 0; residual equicorrelation
    +0.148 (GraphicalVAR), +0.075 (static EGA); bounded scores 0.333/0.233 (DynEGA),
    0.333/0.167 (static EGA), 0.087/0.002 (GraphicalVAR), CLR/raw.
  - Between-clip bridge: DynEGA effect −0.025, percentile [−0.100, 0.050] (receipt
    `recovery_bridge_adoption_receipt.json`).
  - Runtime: 3.69 / 85.41 / 518.16 s; status accounting 20,329 + 1,271 and
    4,846 + 14,477 + 2,277 both sum to 21,600.
  - Amplification diagnostics: max |latent| 55.9–105.2 (T = 100), 329.4–1,201.6
    (T = 300); final-tenth energy fraction 0.316–0.517.
- **Appendix B mathematics re-derived independently: correct.** Sherman–Morrison
  precision `θ/(1+(k-1)θ)`; ceiling `sup w^P = 1/(k-1)`; `w^P < w^M` for k > 2;
  VAR(1) residual eigenvalues and `θ_VAR = (r∥ − r⊥)/(k·r⊥)`; Cauchy–Schwarz lower
  bound on `c_VAR`; `ρ₁→1` limit `1/(2k−1)`; perturbation margin
  `δ < (k−1)w_min/(2k−1)`; threshold separation `δ < w_min/2`. All correct.
- **Appendix A mathematics correct.** η = −0.2742, ζ = −0.5236 from P = 12, ξ = 0.5;
  σ_q = 0.5418 numerically reproduces stationary Var(x) = 0.99997 ≈ 1 (verified by
  solving the discrete Lyapunov equation); aliasing bounds (π ≈ 3.14 vs ω ≈ 0.78;
  12.57 / 7.85 vs 1.04) consistent with the grids; stability/root statements correct;
  clr/ilr inversion `ilr_V(p) = u/τ` correct.
- **Citations**: every `@key` resolves in `references.bib`; no dangling citations;
  reference list renders cleanly.
- **Design arithmetic**: 21,600 (6·2·2·3·3·100), 518,400 (1,296·400), 27 candidate
  settings (3³), embedding counts sum to 21,600 for both T values.
- No leftover "DLO" in the manuscript; "readout" survives only in the Quarto label
  `fig-readout-curves` (permitted by the plain-language proposal).

---

## 1. Blocking defects (fix before anything else — P0)

### P0-1. Stray horizontal rule between abstract and Introduction
- **Location**: `paper/Paper_emotion_dynamics_HGolino_RR.qmd` line 109 (a bare `---`
  after the MathJax `<script>` block).
- **Evidence**: rendered `Paper_emotion_dynamics_HGolino_RR.tex` line 255 contains
  `\begin{center}\rule{0.5\linewidth}{0.5pt}\end{center}` directly after the abstract,
  i.e., a visible horizontal line on top of the Introduction page in the PDF.
- **Fix**: delete the stray `---`. Keep the document body starting with the first
  R chunk.

### P0-2. Stale MathJax config numbers every equation "9.x" in HTML
- **Location**: `Paper_emotion_dynamics_HGolino_RR.qmd` lines 97–107
  (`formatNumber: function (n) {return '9.'+n}`).
- **Evidence**: the block is dead config for the PDF pipeline (pandoc drops
  `<script>` tags), but any HTML render (preview, shareable draft, or an HTML
  submission artifact) would label all equations 9.1, 9.2, ….
- **Fix**: remove the whole `<script type="text/x-mathjax-config">` block. Quarto's
  HTML math handles equation numbering natively.

### P0-3. BRM title page include is a placeholder — title page formatting was lost
- **Location**: `paper/includes/brm-title-page.tex` (referenced from qmd line 90).
- **Evidence**: the file's header comment says it is a placeholder; the original was
  lost when a figure rebuild replaced `paper/`. The rendered PDF therefore has no
  journal title page (no author/affiliation block, corresponding-author note, CRediT
  statement, or submission formatting) — the manuscript is not submission-ready as-is.
- **Fix**: rebuild a real BRM title page from `paper/brm_submission_title_page.md`
  (author order, affiliation superscripts, corresponding author with address/phone,
  ORCIDs, CRediT roles already present in the qmd YAML) either as a proper
  `include-in-header`/title-page fragment or as a separate title-page document for
  Editorial Manager. Then re-render and inspect page 1.

### P0-4. Open-practice links are not final
- **Location**: `paper/brm_submission_title_page.md` lines 35–37 and the manuscript
  Declarations / Open Practices Statement (qmd lines 652–658, 664–666).
- **Evidence**: the title-page notes point to `https://github.com/atomashevic/dlo-rr`
  (the pre-rename repository) and state the OSF archive is "to be added before final
  journal upload". The manuscript promises the OSF deposit "before journal
  submission". BRM enforces data/code availability; these promises must be true at
  submission time.
- **Fix**: (a) create the OSF project, freeze the code, result CSVs, frame-score
  summaries, manuscript source, and response letter; (b) replace the placeholder text
  with the actual OSF URL in Declarations, Open Practices Statement, and the title
  page; (c) update the GitHub link to the current LOMM repository (or omit it in
  favor of the OSF archive).

---

## 2. Correctness / consistency issues in the manuscript prose (P1)

### P1-1. Study 1 Results names only 3 of the 4 non-transition configurations
- **Location**: qmd line 434.
- **Evidence**: the text names *identical-clips*, *baseline-only*, and
  *baseline-and-scale*, then says "The remaining four configurations add a brief
  activity transition to each of these" — 3 + 4 = 7, and the *scale-only*
  configuration (present in the 2×2×2 factorial, line 335, and in the figure and
  tables) is never named.
- **Fix**: name all four base configurations explicitly (identical-clips,
  baseline-only, scale-only, baseline-and-scale) and say the other four add the
  activity transition.

### P1-2. Frame-count sentence mixes units and is arithmetically implausible as written
- **Location**: qmd line 327: "usable segment lengths range from 16 to 50 frames,
  with a median of 35".
- **Evidence**: if "lengths" are the 188 clip segments, a median of 35 with a minimum
  of 16 implies ≥ 4,794 frames — impossible against the stated 3,557 frames. As
  per-clip frame counts (100 clips) it is consistent (mean 35.6). The same ambiguity
  recurs for the interpolated analysis (129 segments, median 36.5, mean 28.2).
- **Fix**: verify against the frame-score CSV which unit the medians describe, then
  reword so segment lengths and per-clip frame counts are unambiguously separated,
  e.g. "segments of 16–50 frames; the median number of retained frames per clip was
  35".

### P1-3. Abstract says "four methods" but lists five quantities
- **Location**: qmd line 52.
- **Evidence**: "Study 2 tested how well four methods recover the number of
  dimensions … accuracy was … for DynEGA, static EGA, GraphicalVAR, GIMME, and 1.000
  for an oracle". The oracle is not a method, but the count mismatch invites a
  reviewer query.
- **Fix**: "four methods that estimate structure from data, plus an oracle
  implementation check that receives the true structure".

### P1-4. `wordcount` field is stale
- **Location**: qmd line 55 (`wordcount: "8,585"`).
- **Evidence**: prose-only word count of the main text (Introduction → Discussion,
  excluding math, code, captions) is ≈ 5,960; the rendered tex span is ≈ 11,400
  tokens including math/captions/tables. Neither matches 8,585.
- **Fix**: recompute the count with the rule BRM expects (decide whether abstract,
  references, tables count), then update or delete the field. Do not leave a number
  that cannot be reproduced.

### P1-5. Prose-only numbers are not derivable from any tracked table — add a claims ledger
- **Location**: qmd lines 379, 397, 451–453, 555, 580, 584.
- **Evidence**: these numbers are quoted in prose but absent from `paper/tables/`
  (they live in gitignored `simulation_outputs/`): ideal-condition baselines
  0.993 / 0.912 / 0.683; broad-grid 93.7% and 95.2%; embedding-selection counts
  141 / 242 / 7,206 / 5,509 / 8,502 and 2,327 / 2,735 / 6,068 / 10,470; bridge
  per-method values 0.750 / 0.775, static EGA 0.275 → 1.000, GraphicalVAR 0.0375,
  GIMME 0.400; component-effect bootstrap means −0.0343 / −0.0072 and biases
  0.0074 / 0.0116.
- **Fix**: extend `scripts/simulations/compare_final_results_to_manuscript.R` (or add
  a new `scripts/checks/verify_manuscript_claims.R`) that greps the qmd for every
  numeric claim and asserts it against the corresponding result artifact, printing
  PASS/FAIL per claim. Run it with the results directories present; any number that
  cannot be reproduced must be corrected before submission.

### P1-6. Opaque sentence about the bootstrap
- **Location**: qmd line 451: "The bootstrap reproduced each estimate exactly when
  every clip received unit weight".
- **Evidence**: unclear what this asserts and why it matters; it reads like a
  degenerate-case implementation note.
- **Fix**: rewrite or delete. If it must stay, state the property precisely
  (e.g., "a resample that happens to include every clip exactly once returns the
  original estimate") and why it is reported.

---

## 3. Response letter must be reconciled with the manuscript (P1)

The letter (`paper/response_letter.tex`) is still partly written against the previous
revision. Its own line 22 marks the quantitative responses as pending; the following
must be resolved before submission:

### P1-7. Wrong figure reference for the pull figure
- **Location**: `response_letter.tex` lines 138 and 144 cite "Figure 2, p. 5" for the
  pipeline overview and say Present Research "introduces Figure 2 after the model
  details".
- **Evidence**: in the current PDF the pipeline overview is **Figure 1** and appears
  inside Present Research, *before* the model section.
- **Fix**: correct both references to Figure 1 and fix the ordering description.

### P1-8. Stale description of Study 1's metrics
- **Location**: `response_letter.tex` line 109 describes "six targets — channel IQR,
  derivative IQR, lag-1 autocorrelation, run-length entropy, spectral entropy, and
  effective rank — at observed-channel and recovered community-score-summary network
  results".
- **Evidence**: the current manuscript compares **eight** statistics (adds median
  level and total variance) computed on the scores and on their log-ratio
  coordinates; there is no community-score-summary representation.
- **Fix**: rewrite the passage to match qmd line 329 verbatim in substance.

### P1-9. Finalize quantitative responses and re-verify all `\location` citations
- **Evidence**: the letter's quantitative responses are parked behind the PENDING
  comment (line 22); page numbers in all `\location{}` macros were computed against an
  earlier 48-page render and will shift with any manuscript edit.
- **Fix**: after the manuscript is final and re-rendered, (a) fill in the quantitative
  responses using the final numbers, (b) regenerate every figure/table/page citation,
  (c) align remaining project vocabulary ("network results", "pre-link", "B/S/G
  adoption analysis") with the plain-language terminology of the manuscript.

---

## 4. Repository documentation and hygiene (P2)

### P2-1. `REVISION_V2_HANDOFF.md` no longer describes the repository
- **Evidence**: the handoff (lines 12–15, 190–214) states the checked-in manuscript
  "contains … non-rendered integration gates" and lists 12 `REVISION_*_PENDING`
  markers in the qmd; the qmd currently contains **zero** markers (the letter still
  has one, commented). The handoff's marker table is obsolete.
- **Fix**: add a dated header to the handoff marking it as a historical record of the
  prepared-but-superseded revision-v2 orchestration, and note that the marker-based
  integration checklist has been resolved/removed. Do not delete the file.

### P2-2. Figure filenames do not match document figure numbers
- **Evidence**: `figure5_dimension_recovery.*` is Figure 6 in the document,
  `figure6_factor_effects.*` is Figure 7, `figure7_runtime_feasibility.*` is Figure 8
  (the pipeline figure shifted the numbering).
- **Fix**: optional. Either rename the artifacts (and their generator scripts and
  qmd references) or add a short mapping note in `scripts/figures/README` or
  `REPRODUCIBILITY.md`. Cosmetic only — no manuscript change required.

### P2-3. Front-matter cleanup (low priority)
- **Evidence**: qmd lines 3, 55, 64–65 carry manuscript-template keys (`shorttitle`,
  `wordcount`, `mask`, `draft`, and the top-level `class: "man"`). The PDF renders
  with `documentclass: article` (checked in the tex), so `class` is inert today.
- **Fix**: verify none of these keys changes the article-class render or the HTML
  render, then keep only what BRM's submission instructions need (most likely only
  `linenumbers` and the doublespacing) and delete the rest.

### P2-4. Visual verification of figures (needs a vision-capable agent or human)
The audit could not visually inspect the rendered figures. Before submission confirm:
- Figure 1 (pipeline): panel labels (A–H) and study-path labels match the caption and
  the study order in the text.
- Figure 4 (Study 1): all eight configuration labels shown, including "Clip scale";
  the diamond marks the baseline-and-scale configuration as the caption claims.
- Figures 5–8 and S1–S2: condition codes match `tbl-scenario-families`; interval
  labels ("95% bootstrap") match the captions.
- Figure B (readout curves): the dotted `1/3` ceiling line and the shaded band label
  match the caption's description of `λ²/s_ε²`.

---

## 5. Optional wording polish (P3, not errors)

1. **qmd line 138**: "Which method does better therefore depends on the
   signal-to-noise ratio and the length of the time series, not on whether a network
   is built from derivatives or from lagged residuals." The construction *determines*
   the SNR, so as phrased the sentence can be read as circular. The Discussion
   version (line 617) is careful; consider aligning the Introduction phrasing to it
   (e.g., "…so the derivative-versus-residual choice carries no automatic population
   advantage; observed differences must come from the signal-to-noise ratio each
   construction achieves, the series length, regularization, and feasibility").
2. **qmd line 52 (abstract)**: consider stating GIMME's conditional accuracy
   (78.3% given usable) once in the abstract or leave it to the Results; the current
   0.176 alone understates that GIMME "was right when it worked".
3. **qmd line 238**: fine as is; no change needed.
4. Reviewer quotes in the response letter still contain "DLO-MM" — expected (they
   are quotations) and should not be altered.

---

## 6. Language / readability check (colleague's obfuscation complaint) — resolved in main text, 6 leftovers in appendices

**Verdict: the main text passes.** Both example phrases from the previous version are gone:
- "selected held-out properties of classifier-output score panels" — no longer present;
- "A focused loading-aligned sensitivity examines whether clip-level heterogeneity
  changes recovery on the linear surface" — no longer present.

The terms "surface", "heterogeneity", "loading-aligned", "status-inclusive", "score
panels", "held-out properties", "evidence lane", "declared", and "observation
profile" have **zero** occurrences in the manuscript. "Panel" survives only for
figure panels (allowed), "readout" only in a Quarto label (allowed), "CLR" only
inside the `clr(·)` operator while prose says "log-ratio coordinates", and the
methods/results use active voice with definitions before use ("worst-case
accuracy", "usable fit", "discrepancy", "baseline-and-scale configuration"). The
main text reads at the level of a standard BRM simulation paper and is followable
by a graduate student who has read Golino & Epskamp (2017).

**Six small leftovers to fix (all in appendices/captions, none in the main text):**

1. `qmd` line 1542 (Appendix C): "absolute plug-in **profile** discrepancies" and
   "the empirical-baseline plus clip-scale **profile**" — replace with "the
   absolute plug-in discrepancies" and "the baseline-and-scale configuration".
2. `qmd` line 839 (Appendix A.5): "the **pre-link** quantity" — the main text never
   names a "link"; write "the quantity before the softmax map" or "the continuous
   indicators of Equation 2 before the softmax".
3. `qmd` line 973 (Appendix B.1): "a main-text **pre-link** score channel" — same
   fix ("pre-softmax score channel").
4. `qmd` line 1915 (Appendix F caption): "averaged across clips **within a panel**"
   — data-sense "panel"; write "within a simulated dataset".
5. Appendix B/A terminology drift from the main text: "gated / ungated / activity
   gating / gate transitions" (main text says "activity-scaled"), "jitter" (main
   text says "perturbed"), and "activity envelope" (A.3, B.4). At first use in B.1
   add the main-text gloss (e.g., "gated, that is, multiplied by the activity
   value, so Z_f = a_f X_f") or replace with the main-text terms; replace "jitter"
   with "perturbation" to match qmd line 236.
6. `qmd` line 1452 "worst-case envelope" is standard math usage — leave as is.

## 7. Response-letter assessment and acceptance outlook

**The letter does the right scientific work** — it answers all 24 points (2 editor,
8 action-editor, 14 reviewer), and each response maps to a change that the current
manuscript actually implements (rename, restructure, comparator methods, realism
check, NRMSE, notation, political example removed, tutorial removed). The
manuscript's numbers and math are real and correct, so the claims in the letter are
defensible.

**But the letter is the single largest remaining risk, for four reasons:**

1. It is written in the *old internal vocabulary* the revision just removed —
   "classifier-output panels", "pre-link linear block-measurement panels",
   "historical homogeneous revision-v2 linear benchmark", "method-network result
   rows with recovered score summaries", "CLR panels", "declared indicator blocks".
   The reviewer's round-1 complaint was exactly that the prose was opaque; a
   response letter that re-introduces the jargon undercuts the "we made it clearer"
   message and makes verification hard.
2. It contains a factual mismatch with the manuscript: line 109 describes Study 1
   as "six targets … at observed-channel and recovered community-score-summary
   network results"; the manuscript compares **eight** statistics on the scores and
   their log-ratio coordinates, with no community-summary representation (see
   P1-8). It also calls the pull figure "Figure 2" in four places when it is
   Figure 1 (P1-7).
3. It does not surface the headline results where the reviewers need them. The
   "alternative benchmarks" response (line 116) never reports 0.939 / 0.884 / 0.777
   / 0.176 / 1.000 — the exact evidence that rebuts "the DynEGA authors are just
   validating DynEGA" (static EGA at 0.884 is the key counter-evidence; the oracle
   at 1.000 is the sanity check). The realism response (line 109) never states the
   21.0% improvement or the −0.0418 / −0.0188 component effects.
4. It still carries the PENDING marker (line 22) and the "historical revision-v2"
   framing throughout, which reads as unfinished development archaeology rather
   than a final revision.

**Outlook (calibrated judgment, not a computed probability):**

- Submitted **as-is**: I would put the first-round outcome at roughly 35–45%
  accept/minor-revision and 55–65% another major revision. The manuscript itself is
  strong, but the opaque, internally inconsistent letter will (a) fail to
  demonstrate the improvement the action editor asked for and (b) invite a "please
  restate the revision and the results clearly" request.
- After executing the audit fixes — above all a plain-language letter that states
  the headline numbers (0.939/0.884/0.777/0.176/1.000; 21.0%; the bounded-score
  collapse to 0.23–0.33), fixes Figure 1/2 and the six-vs-eight mismatch, removes
  "historical revision-v2" and the PENDING marker — I would put it at roughly
  65–75% accept or minor revision and 25–35% another round.

The most likely trigger for a further round, even after a clean letter, is the
bounded-score boundary: Study 2 recovers dimensions from *continuous* indicators,
and the paper explicitly does not claim recovery from classifier-style bounded
scores. The bounded-score departure (accuracy falls to 0.23–0.33) is a substantive,
honest answer, but a reviewer could still ask for a truth definition in log-ratio
coordinates and a recovery analysis there. That is the one extension request the
current package does not yet satisfy; expect it as a "minor revision" ask at worst.

## 8. Suggested execution order for the coding agent

1. P0-1, P0-2 (remove stray `---` and MathJax block) — one edit, re-render, inspect.
2. P0-3 (rebuild the BRM title page) and P2-3 (front-matter cleanup) — then re-render
   and confirm a clean 48-page-class PDF with a proper page 1.
3. P1-1 … P1-4, P1-6 (prose fixes in the qmd) — small targeted edits.
4. P1-5 (claims-ledger verification script) — run against the results directories;
   correct any failing claim.
5. P0-4 (OSF deposit + link updates) — do this after all numbers are final.
6. P1-7 … P1-9 (response letter reconciliation) — after the manuscript render is
   final, regenerate all locations.
7. P2-1, P2-2 (documentation hygiene), then P2-4 (visual figure check) and P3
   (optional polish).
8. Final gates: `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf`
   with zero warnings; `git diff --check`; response letter `pdflatex` clean; claims
   ledger all PASS; OSF URL present in Declarations/Open Practices/title page.
