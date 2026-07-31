# LOMM RR_V2 implementation-audit note (Section 8 checks I1–I10)

Revised file audited: `paper/Paper_emotion_dynamics_HGolino_RR_V2.qmd`
Base: `paper/Paper_emotion_dynamics_HGolino_RR.qmd` @ `df905dd809d1da78e4cf936f8add197ff77d13b2`
Numeric audits executed in Python 3 (numpy/scipy 1.16.2) against the transition routines mirrored
from `scripts/simulations/revision_spec.R` and `scripts/extract.r`; full per-case tables and the
reusable script are in `lomm_work/audit_numeric.md` and `lomm_work/lomm_audit.py` (fixed seeds
I3: 20240317; I6: 777/778). Code inspections were performed on the checked-in R sources at the
same commit. This note reports what was run, with numbers; nothing below is a mere statement
that a check "should be run".

## I1 — Exact transition (RUN: PASS, one legacy FAIL outside all grids)

1. Γ(dt) from the augmented 4×4 matrix exponential vs. the implemented closed form
   A⁻¹(Φ−I)b: max |Δ| = 0–1.4e-16 across ordinary (η=−0.55, ζ=−1.00), repeated-root
   (η=−0.5, ζ=−2√0.5), and near-repeated (±1e-8) cases. **PASS**.
2. The legacy `build_exact_transition` fallback (Moore–Penrose pseudo-inverse via
   `MASS::ginv` in `scripts/extract.r`) in the singular case η=0 is **not exact**:
   max |Δ| = 2.90e-01 (reference Γ=[0.0749, 0.3625]; pinv Γ=[−0.1450, 0.0725]) — the
   pseudo-inverse annihilates the null-direction integral component. **FAIL (legacy branch)**.
   Impact assessment: η=0 occurs in no scenario vector (template η∈{−0.30,…,−1.10},
   ranked η=−0.05f), so no checked-in or planned panel touches this branch; the prospective
   `revision_spec.R::revision_integrated_input` uses the closed form only for |η|>1e-10 and a
   40-term series otherwise, which is exact. Manuscript action (M1): the conflating sentence
   was replaced; singular-A validity of the integral and the repeated-root branch requirement
   are now stated separately.
3. Branch formulas (`revision_exact_transition`: sinh/cosh, sin/cos, repeated-root) vs.
   augmented expm: ≤2.2e-16 for Φ and Γ in repeated and ±1e-8 near-repeated cases. **PASS**.
4. One implemented step Φs₀+Γq vs. `solve_ivp` (rtol 1e-12) under a constant shock:
   ≤1.7e-14 in all five cases. **PASS** — confirms one scalar zero-order-hold shock per
   factor and step, matching the new M1 assumption sentence.

## I2 — Growth diagnostic (RUN: completed; g(H) relabeled)

For every scenario factor (codes A–F × 2–4 dimensions; σ_q does not enter the deterministic
object, so A–D and E–F factors coincide pairwise), computed g_f(H)=exp{H·max Re(r)},
‖exp(AH)‖₂, and sup_{0≤u≤H}‖exp(Au)‖₂ on a ≥20,000-point grid for H[100]=39.6 and H[300]=74.75:

- Damped template factors (A–D): sup ‖exp(Au)‖₂ ≤ 1.0300 — no transient growth; endpoint norm
  exceeds g(H) by only 4–46%; large sup/g ratios are trivial (sup includes u=0).
- Anti-damped ranked factors (E–F): sup norm exceeds g(H) by a factor **1.98–3.97**, and
  ‖exp(AH)‖₂ exceeds g(H) by **1.33–3.22** — material understatement of peak state-norm
  amplification by the modal factor.
- Manuscript action (M2): g_f(H) relabeled "modal exponential-rate factor" everywhere;
  G_f^max(H) and G_{x,f}^max(H) defined in the model section; Appendix E caption now states
  the g(H) columns are modal-rate factors, "not total finite-horizon state amplification".
  Computed values are reported here and in `audit_numeric.md` (54 rows); no new numeric
  columns were added to the manuscript (constraint 2 — no generated artifact inside the repo
  render pipeline).

## I3 — Initialization and cross-block moments (RUN: confirms revision)

Oracle panels without measurement error (N=2000 videos, T=100, dt=0.4, gains 1.00/0.08,
rotating dominance; scenario B 3D stable and E 3D anti-damped), raw and filtered cross-factor
moments with batch standard errors, gated and ungated, separating centered covariances from
uncentered second moments:

- Checked-in common start (x=0.25, v=2 for every factor): creates a **systematic cross-block
  term** in raw moments AND centered covariances — |t| = 32.7–302 across scenarios, gated and
  ungated. Centering does not remove it (a common mean transient survives centering); gating
  only attenuates it.
- Revised independent starts (stable: N(0,P_f) from the discrete Lyapunov equation;
  anti-damped: Unif(−0.25,0.25)/Unif(−2,2)): cross-block terms vanish to Monte Carlo
  tolerance, |t| ≤ 2.8 over all 24 estimates.
- Manuscript action: A.1 now states exact block support via zero pooled cross-block moments,
  names the sufficient conditions, warns that "uncoupled" dynamics alone do not imply them,
  and calls the checked-in common start "a structured departure from the ideal target".

## I4 — Measurement positivity and independence (code inspection of the complete grid)

- Grid (all final runners: `run_final_dynega_full.R`, `run_final_gimme_bounded.R`,
  `run_final_graphicalvar_bounded.R`): Loadings ∈ {0.4, 0.6, 0.8}, Error_sd ∈ {0.125, 0.25, 0.50}.
  Minimum baseline loading 0.4 > 0.15; minimum baseline error variance 0.125² = 0.015625 > 0.01.
- Jittered minima: loading 0.4−0.15 = 0.25 > 0; error variance 0.015625−0.01 = 0.005625 > 0.
  Every Σ_i is diagonal with strictly positive entries ⇒ positive definite for every draw.
- `scripts/extract.r::generate_observables_multi`: errors drawn by per-video
  `MASS::mvrnorm(n = num_steps, mu = 0, Sigma = diag(var_vec))` — iid rows over time,
  independent across indicators (diagonal), videos, latent states, initial states, schedules,
  and shocks (independent RNG calls). Loadings/variance jitter via independent `runif` draws.
- Manuscript action (M5): both the independence statement and the positivity sentence
  ("λ>0.15 and s_ε²>0.01 …") were inserted — the grid-verified branch of the instruction.

## I5 — Filter boundaries and cadence (code inspection + archived validation)

- `run_method_simulation.R`: DynEGA call is `EGAnet::dynEGA(..., tau = 1, delta = 1,
  model = "glasso", use.derivatives = 1, ...)` with video identity as the grouping variable,
  so embedding windows cannot cross a clip boundary (the manuscript's prospective-spec
  paragraph states the same constraint: "Time-delay embeddings carry clip identifiers").
  One derivative order (`use.derivatives = 1`) is supplied.
- delta scale-invariance: the repo's pre-compute handoff (`REVISION_V2_HANDOFF.md`) records
  the completed scale-invariance test — the derivative correlation matrix is unchanged by the
  common 1/dt time conversion — and that EGAnet 2.3.0's GLLA accepts integer delta 1–3 only
  (so delta = 0.4 is not directly runnable; the invariance is the analytic statement that a
  common positive rescaling of every channel derivative leaves correlations, the EBICglasso
  network, and partitions unchanged). The retained manuscript claim (physical dt = 0.4 in
  horizon metadata, observation-unit delta = 1) therefore stands on the archived audit
  (acceptance criterion 22). No jitter is involved in this path.

## I6 — GraphicalVAR population check (RUN: PASS with one qualified note)

Oracle panels (N=3000, scenario B 2D stable and E 2D anti-damped, k=4, λ=0.6, ψ=0.0625),
retained within-video lag pairs only (no cross-clip lags by construction), per-video
centering replicating `centerWithin = TRUE`:

- Population lag-1 coefficient B = λ²C/(ψ+kλ²V₋)·J_k vs. pooled OLS: agreement to 1e-6 (E).
- w^P formula vs. empirical partial correlation: 0.2961/0.2966 and 0.3117/0.3119 (E).
- Ω_VAR block formula vs. residual covariance: max in-block diff 1.1e-3 ≈ 2 MC SE (E).
- Scenario B shows small gaps (Ω diff 6.8e-4; one B-matrix entry 0.0114 vs 0.0093) fully
  explained by finite-T `centerWithin` bias ~ψ/T: an uncentered control drops the Ω diff to
  2.4e-4 (MC noise). Cross-block lag-0/lag-1 moments clean in E (max |t| = 1.48); one of 16
  B-scenario cross entries has t = −4.5 at magnitude 3.85e-4 — flagged, negligible, not
  confirmed as latent leakage (consistent with I3's common-start finding).
- Residual serial correlation: latent lag-1 projection residual AC(1) = 0.73/0.80 (B) and
  0.99/0.99 (E); observed block-residual AC strong in E (0.73/0.84) and ≈0 in B only because
  measurement noise dominates (θ_VAR ≤ 0.065). The lag-1 residuals are **not** VAR(1)
  innovations — numerically mandating the "lag-1 projection residual covariance" terminology
  adopted throughout (constraint 7, M9, A.3.2).
- Manuscript action: the A.3.2 derivation (S_{--}, S_{+-}, S_{++}, B_f, Ω_VAR,f, c_VAR,f,
  θ_VAR,f, w^P) is exactly the algebra verified here.

## I7 — GLLA signal-to-noise check (NOT completed → all directional SNR claims removed)

This audit requires the selected-embedding GLLA coefficient vector ℓ per matched condition
(an EGAnet run); no R runtime exists in this environment, so θ_GLLA,f vs. θ_VAR,f paired
distributions were not produced. Per the instruction's explicit fallback, every manuscript
claim about the direction of the SNR ordering was removed or downgraded:

- A.3.1: "the GLLA filter improves on this stand-in" → "scenario and embedding dependent".
- A.3.3: claim-boundary paragraph — "The block algebra does not determine a general ordering
  between θ_GLLA,f and θ_VAR,f … must be tested from oracle or realized filtered covariances";
  the θ_VAR,f > θ_{Δ1,f} counter-comparison and the 1/(2k−1) regime limit are included.
- A.4: "consistent with the derivative filter preserving more within-block signal than the
  innovation filter" → "The observed ordering does not identify which method has the larger
  population signal-to-noise ratio because estimation, regularization, graph conversion, and
  feasibility differ between pipelines."
- A.6: DynEGA cell now requires demonstration "from oracle or realized filtered covariances".
- Discussion (M9): "consistent with, but does not identify, this mechanism" with named
  confounders (lag estimation, regularization, finite-sample variation, graph conversion,
  timeout behavior).

## I8 — GIMME extraction (inspected the actual extraction path)

From `scripts/simulations/run_method_simulation.R::extract_average_gimme_psi` and
`scripts/simulations/revision_spec.R::revision_gimme_positive_structure`:

1. Slot used: `gimme_fit$psi` (filtered to matrix-valued entries).
2. Dimensions: contemporaneous block selected per clip by matching variable names
   (`select_contemporaneous_block`), giving a square p×p block.
3. Scale: standardized — fits run `gimmeSEM(..., standardize = TRUE)`, so psi matrices are
   standardized residual covariances (residual-correlation scale).
4. Symmetry: not guaranteed numerically; the pipeline symmetrizes by transpose-averaging,
   (R+Rᵀ)/2.
5. Object type: residual covariance on the standardized scale — not a partial-correlation
   matrix and not a directed path matrix (matches M10/A.5 wording).
6. Variable ordering: enforced identical across clips by name-matched block selection.
7. Absent/non-finite entries: set to 0 before symmetrization.
8. Clips in average: usable clips only (`Reduce(`+`)/length` over extracted matrices), V_u.
9. No-positive-edge fallback: singleton communities (revision spec and manuscript prose).
10. Every usable structure fit supplies all three community readouts (Louvain, Leiden,
    Walktrap reuse the same fitted structure — consistent with Appendix D accounting).

Discrepancy documented (constraint 11): the legacy shared helper
`community_from_weight_matrix` in `run_method_simulation.R` contains an absolute-value
fallback when a matrix has no positive edges. For the GraphicalVAR path this matches the
checked-in Method prose ("absolute weights used only when no positive edges are available"
and `revision_symmetrize_weights`). For the GIMME path, the manuscript prose and
`revision_gimme_positive_structure` specify the singleton fallback instead; the prose was
kept per M10, and aligning the legacy GIMME call path with the revision-spec helper is
recommended — the manuscript now describes the prospective pipeline exactly and does not
guess slot contents.

## I9 — Population-versus-fitted manifest (from code inspection)

| Field | DynEGA | GraphicalVAR | GIMME |
|---|---|---|---|
| Centering | none beyond package default on derivative panel | `centerWithin = TRUE` (per-clip) | standardized variables (`standardize = TRUE`) |
| Scaling | raw derivative scale (observation-unit delta = 1) | `scale = TRUE` | standardized scale |
| Retained rows | pooled stacked panel, grouped by video | stacked panel | per-clip series |
| Lag rule | embedding windows within video | within-video lag-1 pairs | per-clip VAR |
| Order | GLLA tau = 1, `use.derivatives = 1` | lag-1 projection | VAR search (lagged + contemporaneous) |
| Fitted matrix | EBICglasso partial correlations on derivatives | `fixedPCC` contemporaneous partial correlations | `fit$psi` standardized residual covariances |
| Regularization | glasso (EBIC) | `gamma = 0.5` regularization | path-search selection |
| Diagonal | ignored for CD | ignored | removed (offdiag) |
| Sign conversion | package default (EGA network) | positive edges; absolute weights only if none positive | negative entries zeroed |
| Symmetrization | symmetric by construction | treated as undirected | transpose-average |
| Fallback | — | absolute-weight fallback | singleton communities |
| Community algorithm | Louvain res 1.0 (broad: Louvain/expand); Leiden, Walktrap readouts | same three readouts on one fitted structure | same three readouts on one averaged matrix |
| Usability status | all fits usable | 20,872/21,600 (728 timeouts) | 15,294/21,600 (4,114 timeouts, 2,192 errors) |

## I10 — Render and numerical regression (static validation + precise unresolved-render report)

**Render could not be executed in this environment**: `which R Rscript quarto` returns
nothing (no R, no Quarto CLI, no pandoc; the sandbox has Python only). Constraint 12 and
deliverable 5 therefore take the permitted alternative form — a precise report of the
unresolved render plus the strongest available static validation:

1. YAML block parses cleanly (all keys, authors, affiliations, `format: pdf`, `execute`).
2. All 18 chunk labels identical to base; chunk fences balanced (38 ```; 18 ```{r} + 1
   ```{=latex}); no chunk content altered except string cells of
   `tbl-appendix-perturbation-summary` (same LaTeX-escaping style) and one kable caption
   string in `tbl-scenario-stability-supplement` (plain text). No new R calls or packages.
3. Math balance: 118 `$$` (even; 114 before the post-review R1–R6 fixes — see the addendum), 2 `\begin{equation}`/`\end{equation}` pairs; labels
   `eq:lomm-latent`, `eq:lomm-measurement` and all four `\ref{}` uses intact; all new
   displays use only amsmath constructs (`\xrightarrow`, `\widetilde`, `\mathcal`,
   `\boldsymbol`, `\operatorname`, `\langle`, `\sup`, `bmatrix`) — amsmath is in
   `header-includes`.
4. Cross-references: every `@fig-`/`@tbl-` token resolves to an existing label (18 chunk
   labels + `tbl-lomm-assumptions` pipe-table label; pipe-table pipes balanced after edits).
5. All 11 `REVISION_RESULTS_PENDING`/`REVISION_FIGURE_PENDING` + `CLAIM_LEDGER` comments
   byte-preserved.
6. Numerical regression: automated token check over all checked-in quantitative Results
   (95.3/80.3/25.9, 93.7/95.2, 96.0/90.4/67.2, 2.61/86.1/280.5, 20,872/21,600/728,
   15,294/4,114/2,192, 518,400/64,800/62,616/45,882, 0.832/0.247/1.481, grid values, scenario
   vectors, horizons 39.6/74.75/9.6) — zero reduced counts; the 23-hunk unified diff was
   reviewed hunk-by-hunk and every hunk maps to a numbered instruction (see change log).
7. Residual render risk (to be closed by `quarto render` in the repo environment, R 4.5.1 +
   the package set in "Computational Environment"): none identified beyond the ordinary —
   all edited chunks contain only string-literal changes, and all prose math is amsmath-only.

**Bottom line**: render success could not be demonstrated here (no toolchain); nothing in the
static analysis predicts a render failure. The single command `quarto render
paper/Paper_emotion_dynamics_HGolino_RR_V2.qmd` in the project environment closes I10-1/2.

## Addendum — post-review R1–R6 re-validation

After the main revision, six remaining issues (R1–R6) were fixed in the manuscript. The code
facts used were those already verified above (I5 `dynEGA` arguments; the I9 manifest and
`audit_numeric.md` I6 for `mlGraphicalVAR` + `fixedPCC` + `gamma = 0.5`; the I9 retained-rows
rows confirming that no common row-intersection trim exists). The manuscript edits were
re-validated statically:

- Markers: 10 `REVISION_RESULTS_PENDING` + 1 `REVISION_FIGURE_PENDING` + 11 `CLAIM_LEDGER`
  comments, all byte-preserved.
- Structure: 18 chunk labels intact; 38 ``` fences, 18 ```{r}; 118 `$$` (even, up from 114);
  2 `\begin{equation}`/`\end{equation}` pairs.
- Numeric regression: all checked-in Result/table tokens still present (zero reduced counts).
- Flagged phrases now zero: `common analysis window`, `common-window`, `or path-matrix`.
- The regenerated unified diff round-trips RR → V2 byte-identically (`patch`-verified).

File state after R1–R6: 126,462 bytes, 1,493 lines. The render check remains the single
`quarto render` command in the repo environment (I10).
