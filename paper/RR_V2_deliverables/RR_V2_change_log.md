# LOMM RR → RR_V2 change log

Revised file: `paper/Paper_emotion_dynamics_HGolino_RR_V2.qmd`
Base file: `paper/Paper_emotion_dynamics_HGolino_RR.qmd` @ `df905dd809d1da78e4cf936f8add197ff77d13b2` (`main`)
Unified diff: `RR_to_RR_V2.unified.diff` (22 hunks after regeneration; instruction-driven edits in Sections 4–5, post-review R1–R6 fixes in Section 8)
Line numbers refer to `Paper_emotion_dynamics_HGolino_RR_V2.qmd`. Section 4–7 anchors were recorded against the pre-R1–R6 file (1,467 lines); the R1–R6 fixes shifted later line numbers — current anchors are given in Section 8.

Every hunk in the diff maps to an instruction; no other content was changed. All YAML, citations,
equation labels (`eq:lomm-latent`, `eq:lomm-measurement`), cross-references, all 18 chunk labels,
and all 11 `REVISION_RESULTS_PENDING`/`REVISION_FIGURE_PENDING` + `CLAIM_LEDGER` HTML comments are
preserved verbatim. No checked-in numerical Result, table, figure, or status count was altered.

## Section 4 — Main-text edits

| Instr. | Anchor (V2 lines) | Change |
|---|---|---|
| M1 | L181 | Replaced singular/repeated-root conflation sentence: integral valid when A_f singular incl. η_f=0; separate repeated-root branch required when ζ²+4η≈0. |
| M1 | L183 | Added code-verified assumption sentence: q_{if,t} independent across videos/factors/transitions, independent of initial states/schedules/errors, constant on [t,t+dt); per-step zero-order-hold acceleration inputs, not white-noise diffusion increments; implemented covariance Γσ²Γᵀ stated. Diffusion covariance Q_{d,f} not introduced (generator is ZOH). Verified against `scripts/extract.r` (`generate_q_multi`: iid mvrnorm rows, default ρ_q=0; `integrate_step`: scalar shock × Γ per factor-step) and audit I1. |
| M2 | L198–224 | "anti-damped amplification" sentence replaced (horizon determines represented amplification). Growth paragraph replaced: α(A_f) defined; g_f(H)=exp{Hα(A_f)} relabeled **modal exponential-rate factor**; explicit non-statement about nonnormal/defective-root amplification; new norm-based diagnostics G_f^max(H) and G_{x,f}^max(H) defined; reporting sentence relabels g_f(H) as modal-rate factor, "not … total finite-horizon amplification". Numerical values (audit I2) reported in the implementation-audit note, not claimed in the manuscript. |
| M2 | L1458 | Appendix E `tbl-scenario-stability-supplement` caption: g(H) columns now described as "modal exponential-rate factors … not total finite-horizon state amplification". |
| M3 | L233 | After discrete Lyapunov equation: stationary-covariance statement limited to the stable ungated factor; uniqueness requires ρ(Φ_f)<1; gating does not make a_f(t)X_f(t) stationary. Anti-damped factors keep uniform starts (no stationary implication). |
| M4 | L241–268 | Surrounding explanation replaced: product rule away from discontinuities; impulsive gate derivative at steps (distributional sense); exact identity Δ(ax)=a_{t+dt}Δx+Δa·x_t (post-transition gate, pre-transition state); checked-in finite-difference identity retained verbatim; convention x̃_t=a_t x_t with new gate attached to post-transition observation — matches `run_method_simulation.R` (`latent <- raw_latent * schedule$envelope`, gate applied per observation index after trajectory completion). |
| M5 | L275 | After `eq:lomm-measurement` paragraph: independence statement (over time/videos; of latent states, starts, schedules, shocks; iid N(0,Σ_i) per video) — verified against `generate_observables_multi` (per-video iid mvrnorm, diagonal Σ_i). Positivity sentence inserted: grid satisfies λ>0.15 and s_ε²>0.01 (complete grid: λ∈{0.4,0.6,0.8}, s_ε∈{0.125,0.25,0.50} in `run_final_dynega_full.R`, `run_final_gimme_bounded.R`, `run_final_graphicalvar_bounded.R`); jittered minima 0.25>0 and 0.005625>0 (audit I4). |
| M6 | L290–302 | After softmax-invariance discussion: CLR identity clr(y_it)=(1/τ)(I_p−(1/p)1_p1_pᵀ)(α+z_it); explanation: CLR recovers centered logits up to 1/τ; common-logit component unobservable; uncentered logits and τ not identified without external scale constraint. |
| M7 | L324–330 | New aliasing paragraph at end of "Model Scope and Identifiability": r_{f,±}=ζ_f/2±iω_f; aliasing of ω_f and ω_f+2πq/dt at t_n=n·dt; cadence restriction 0<ω_f<π/dt; closing constraint-stack sentence (sign/scale, gate normalization, loading/innovation anchoring, reference-logit/sum-to-zero, temperature, frequency restriction); simulation does not estimate these. |
| M8 | L357 | RQ2 replaced (partition target despite different estimands/conversions; oracle validates conversion and partition implementation). Oracle sentence added: "deterministic implementation check … not an estimator-performance ceiling." |
| M8 | L359 | "panel keys, simulation seeds, and the generating partition target are shared across pipelines; method-specific graph conversion precedes the common community algorithm." |
| M8 | L457 | Method common-target paragraph replaced with the common-partition-target/different-structural-objects framing. |
| M9 | L621–623 | Discussion mechanism paragraph replaced: conditions on all other retained observed channels; under symmetric moment target the **lag-1 projection residual covariance** preserves block support (diagonal-plus-rank-one); same compression map as DynEGA partial readout; removes the **lag-1 linearly predictable** component; oscillator level need not be VAR(1) → residuals can remain serially correlated; "consistent with, but does not identify" with named confounders. Checked-in feasibility counts (728 timeouts) retained. |
| M10 | L466 | GIMME operational definition rewritten from actual code (`run_method_simulation.R::extract_average_gimme_psi` + `revision_spec.R::revision_gimme_positive_structure`): standardized residual covariances from `fit$psi`; averaged over usable clips only on standardized scale; diagonal removed; transpose-averaged symmetrization; negative entries zeroed; bespoke residual-correlation conversion (not directed-path, not partial-correlation graph); identical variable order across clips via name-matched block selection; no-positive-edge → singleton fallback (audit I8). |

## Section 5 — Appendix A replacement

| Instr. | Anchor (V2 lines) | Change |
|---|---|---|
| A0 | L690–720 | Opening rewritten: separation of population structural matrix / estimation+conversion error conditional on usable fit / community-detection map / fit usability. Pipeline Ŝ^(r) →Π_r→ W̃^(r) →C_r→ B̂^(r) introduced. U_r event; W̃^(r)=W_*^(r)+E^(r) conditional on U_r=1; E contains finite-sample variation, derivative/model estimation, regularization, extraction, dependence-induced variance inflation, systematic deviation from symmetric target — **not** community detection, **not** fit failure. Claims paragraph: exact support conditional on zero pooled cross-block moments; SNR ordering not determined by block algebra. Assumptions table (`tbl-lomm-assumptions`) cells aligned (CD/fit failure outside perturbation; "pipeline readout"). |
| A1 | L728–766 | Notation crosswalk added (Y_j↔z_j, X_f↔x_f, Z_f↔x̃_f; drops video indices/jitter). Common retained rows R; ⟨g⟩_R and Var_R(U) defined; "time-averaged centered second moments over the common analysis window R" everywhere. "zero-mean and mutually independent" replaced by moment-based exact-support statement + sufficient condition; "uncoupled alone does not imply zero pooled covariance". Coordinated-schedules/zero-conditional-mean paragraph; shared deterministic start = structured departure (audit I3); old Ŵ=W_*+E paragraph removed (superseded by A0). |
| A2 | L770–829 | Fixed per-channel filter L with coefficients ℓ_0..ℓ_q, identical boundary handling, no window crossing video boundary. V_{L,f}=Var_R(LZ_f), ψ_L=Var_R(Lε_j); ψ_L=ψΣℓ_r²; lag-covariance ψΣℓ_rℓ_{r−h}. Serial-dependence statement: same-row covariance unchanged; effective sample size/regularization/sampling variability affected (estimation issue, not hidden). C_{L,fg} defined; block-diagonal formulas require C_{L,fg}=0 ∀ f≠g. J_k=\mathbf1_k\mathbf1_kᵀ used consistently. Sherman–Morrison/marginal/partial formulas and 1/(k−1) limit kept verbatim. Pooled partial-correlation readout statement added (not a conditional-independence graph under gating); partial readout conditions on all other retained observed channels. |
| A2 | L888–897 | Every ceiling claim qualified "under the equal-loading, equal-error symmetric population target" (prose + `fig-readout-curves` paragraph and caption). |
| A3.1 | L918–940 | θ_GLLA,f = λ²ℓᵀR_{Z,f}ℓ/(ψℓᵀℓ), conditional on chosen embedding/boundary; embedding selection + regularization belong to fitted pipeline; w_f^P(θ_GLLA,f) notation. Lag-h stand-in θ_f^{Δ_h}=λ²Var_R{Z_f(t+h)−Z_f(t)}/(2ψ) kept. "GLLA filter improves on this stand-in" deleted → "scenario and embedding dependent" (can attenuate latent change components). |
| A3.2 | L946–1026 | GraphicalVAR as population unpenalized lag-1 linear projection P_VAR (never a per-channel filter L_VAR). Centered common-window lag-pair moments V_{−,f}, V_{+,f}, C_f over retained within-video lag pairs (no cross-clip lags, estimator's centering). S_{--,f}, S_{+-,f}, S_{++,f}; B_f=λ²C_f/(ψ+kλ²V_{-,f})·J_k; lag-1 projection residual covariance Ω_VAR,f=ψI_k+c_VAR,f J_k with c_VAR,f=λ²V_{+,f}−kλ⁴C_f²/(ψ+kλ²V_{-,f}); θ_VAR,f=c_VAR,f/ψ; w^P_VAR,f=θ_VAR,f/(1+(k−1)θ_VAR,f). Explicit statement: "innovation covariance" only under the stronger innovation-process interpretation; residuals can remain serially correlated; assumptions and failure modes listed (audit I6 verified the algebra against Monte Carlo). |
| A3.3 | L1034–1084 | Stationary special case: θ_VAR,f=θ_f[1−ρ²_{1,f}kθ_f/(1+kθ_f)]; lag-1 difference θ_{Δ1,f}=θ_f(1−ρ_{1,f}); θ_VAR,f>θ_{Δ1,f} for 0<ρ<1 (first difference cannot justify derivative>residual SNR claim). Regime limits: ρ→1 ⇒ θ_VAR→θ_f/(1+kθ_f); θ_f→∞ in that limit ⇒ w^P→1/(2k−1); fixed ρ<1, θ_f→∞ ⇒ shared 1/(k−1) ceiling (symmetric target). All theorem-like comparisons replaced by the claim-boundary wording (ordering must be tested from oracle/realized filtered covariances). |
| A4 | L1091–1162 | δ_r defined as ‖W̃^(r)−W_*^(r)‖_max after method-specific graph conversion, conditional on U_r=1; w_f^(r)=w_f^P(θ_GLLA,f)/w_f^P(θ_VAR,f). 3/7 (k=4) bound kept, conditioned on usable graph + exact block-support target; consequences limited to nodewise preference, separating threshold, connected components; explicit non-guarantee for Louvain/Leiden/Walktrap kept; optional δ<w_min/2 note added. 1/7 high-signal limit kept with symmetric-target qualification. "Observed recovery ordering … innovation filter" replaced (oracle SNRs unavailable): "The observed ordering does not identify which method has the larger population signal-to-noise ratio …". "w_min set by background blocks" replaced by expectation wording + identify-weakest-block from panel/population calculation. V_{L,f}, w_min, W_* stated conditional on common analysis window (essential for anti-damped). |
| A5 | L1164–1198 | R̄_G=(1/V_u)Σ_{i:U_i=1}R_i^G over usable clips only; W_G=[offdiag{(R̄_G+R̄_Gᵀ)/2}]_+; positive residual-correlation conversion (not path graph, not partial-correlation graph); V_u replaces V. Event notation H*=U∩H, P(H*)=P(U)P(H|U); failures/timeouts are non-hits in P(H*) but excluded from P(H|U); structure-fit vs repeated community-readout denominators explicit. Checked-in decomposition numbers (95.3/80.3/25.9%; 20,872/21,600; 62,616/64,800; 15,294; 45,882) retained verbatim. |
| A6 | L1204–1214 | `tbl-appendix-perturbation-summary` cells: DynEGA population signal w_f^P(θ_GLLA,f); GraphicalVAR w_f^P(θ_VAR,f) "partial readout of lag-1 projection residuals"; "community detection" removed from DynEGA perturbation cell; "innovation-projection attenuation"→"lag-1 projection attenuation". Expected-implication cells replaced per spec (scenario/embedding-dependent SNR; symmetric-moment lag-1 residual statement; usability + documented conversion). |

## Section 6 — Global search-and-replace audit (all 15 items cleared)

1. "innovation filter" — 0 occurrences (was 2; A0 claims + A4 ordering rewritten).
2. "innovation covariance" — only the spec-mandated conditional sentence in A.3.2 (L1026).
3. "removes the predictable part" — 0 (Discussion + A3 rewritten; "lag-1 linearly predictable" used).
4. "derivative filter preserving more" — 0 (A4 replacement).
5. "derivative filter keeps the filtered signal near the shared ceiling" — 0 (A6 cell).
6. "GLLA filter improves on this stand-in" — 0 (A3.1).
7. "same population object" — 0.
8. "shared downstream partition rule" — 0 (M8).
9. "implementation ceiling" — 0 (M8 RQ2 + oracle sentence).
10. "community-detection effects" in E^(r) — 0 (A0/A1; assumptions table aligned).
11. "w_min is set by the background blocks" — 0 (A4 expectation wording).
12. "Indicators assigned to different oscillators are independent." — replaced with the specified zero-pooled-covariance sentence pair (L615).
13. Unqualified 1/(k−1) ceilings — all occurrences qualified (L891, L893, L897, L1077, L1079, L1147).
14. Unqualified block-support-from-uncoupled-dynamics — A.1 moment-based condition + "uncoupled alone does not imply…" (L762); assumptions-table row aligned (L718).
15. g_f(H) as total amplification — 0 (M2 relabel; Appendix E caption L1458).

## Section 7 — Notation consistency

1. J_k=\mathbf1_k\mathbf1_kᵀ everywhere (L814, L831, L847, L959–991; no \mathbf1_k\mathbf1_kᵀ leftovers).
2. V_{L,f} for variance; V_u for usable clips (A.5); no dual use of V.
3. GraphicalVAR never a per-channel filter: "lag-1 projection" / P_VAR (L948); L_VAR eliminated.
4. w_f^P(θ_GLLA,f) / w_f^P(θ_VAR,f) used (L933, L1094, A6 table).
5. Partial correlations condition on all other retained observed channels (L838; Discussion L621).
6. Centering rule defined for every population covariance (A.1 pooled moments; A.3.2 estimator's centering).
7. Lag-pair set defined; no lags across clip boundaries (L953).
8. Method-specific sign/symmetry/diagonal/fallback conversions stated before CD (M10 L466; A.5; GraphicalVAR Method paragraph unchanged and code-verified).
9. "Population graph" used only for fully defined population matrices.
10. "Pooled partial-correlation readout" used under gating (L862).

## Constraint compliance notes

- Constraint 2 (numbers): numeric-token regression over all checked-in results shows zero reduced counts; Results/Discussion figures, status counts, and feasibility numbers untouched.
- Constraint 11 (code over prose): all code-dependent statements (M1, M4, M5, M10, A5) verified against repository sources before editing; the single prose/code discrepancy found (legacy `community_from_weight_matrix` absolute-value fallback vs. singleton fallback in prose/revision spec) is documented in audit I8 — prose follows the revision spec and the current checked-in manuscript wording; code alignment is recommended, not silently assumed.
- Constraint 12 / deliverable 5: no R/Quarto runtime exists in this environment; see the render section of the implementation-audit note for the static validation performed and the precise unresolved-render report.

## Section 8 — Post-review remaining-issue fixes (R1–R6)

These fixes address the six remaining issues raised after the main revision. Code-dependent
statements were grounded in the code-verified audit notes (I5, I6, I9 in
`RR_V2_implementation_audit.md`; `audit_numeric.md`). Anchors refer to the current
`Paper_emotion_dynamics_HGolino_RR_V2.qmd` (126,462 bytes, 1,493 lines).

| Issue | Anchor (V2 lines) | Fix |
|---|---|---|
| R1 — common analysis window | L753 (A.1), L470 (Methods); downstream window phrases in A.2/A.3.1/A.4 | Replaced the imposed common retained window with the common raw horizon $\mathcal{T}$ plus readout-specific retained rows $\mathcal{R}_r\subseteq\mathcal{T}$ (GLLA embedding/boundary losses; lag-1 within-video pairs). All moments are readout-specific ($\mathcal{R}$ suppresses the readout subscript). Methods now states the raw panel and physical horizon are common, retained rows readout-specific, and no common row intersection is imposed (confirmed by the audit I9 retained-rows row). The nonstationarity consequence (different windows can give different $V_{L,f},V_{+,f},V_{-,f},C_f,\theta_{r,f}$ on the same raw panel) is stated explicitly. |
| R2 — Figure A1 shaded band | L906 (prose), L910 (caption) | The band is a checked-in PDF (`perturbation_readout_curves.pdf`) that cannot be regenerated without the R figure pipeline (constraint 2), so the issue's weaker correction was applied: prose and caption now label the band explicitly as the raw loading-to-error-variance ratio $\lambda^2/s_\varepsilon^2$, not a calibrated $\theta_{L,f}$ range, and state that $\theta_{L,f}=(\lambda^2/\psi)V_{L,f}/\sum_r\ell_r^2$ also depends on the filtered latent variance and the filter noise gain. |
| R3 — norm diagnostic scaling | L224 (coordinate paragraph), L237 (reporting sentence) | Added that the Euclidean norm is coordinate-dependent (level vs velocity units), requiring a nondimensionalization or weighted state norm, and added the coordinate-aware definition $G_{f,M}^{\max}(H)=\sup_{0\leq u\leq H}\|\mathbf{M}^{1/2}e^{\mathbf{A}_fu}\mathbf{M}^{-1/2}\|_2$. Replaced the reporting sentence: the roots and $g_f(H)$ characterize the latent modal regime; the activity schedule is reported separately to distinguish latent dynamical amplification from gate-induced nonstationarity. |
| R4 — DynEGA rescaling conditions | L474 (Methods), L924 (A.3) | Stated first-order derivatives only (`use.derivatives = 1`), fixed embedding/retained rows/missing-data treatment, and no zero-variance jitter as the conditions for the $1/dt$ rescaling invariance; added that activated zero-variance jitter could break exact invariance. Grounded in audit I5 (`dynEGA(tau=1, delta=1, model="glasso", use.derivatives=1)`; no jitter; EGAnet accepts integer delta, so the invariance is the analytic statement). |
| R5 — GraphicalVAR mapping | L477 (Methods), L959 (A.3.2 index notation), L1019 (Cauchy–Schwarz) | Methods now names the call: `mlGraphicalVAR` with `centerWithin = TRUE`, `scale = TRUE`, `gamma = 0.5`, within-clip lag-1 pairs only (no cross-clip lag), extracting `fixedPCC` (grounded in the audit I9 manifest and `audit_numeric.md` I6). A.3.2 lag-pair moments switched to observation-index notation ($Z_{f,n-1}$, $Z_{f,n}$, one step $dt$). Added the Cauchy–Schwarz positivity proof $c_{\mathrm{VAR},f}\geq\lambda^2V_{+,f}\psi/(\psi+k\lambda^2V_{-,f})>0$, placing $\theta_{\mathrm{VAR},f}$ on the positive branch of the compression map. |
| R6 — two softened claims | L1171 (A.4 glasso), L1235 (A.6 GIMME cell) | A.4: "glasso shrinkage lowers within-block edges" replaced with "tends to shrink precision off-diagonal entries toward zero and can remove weak edges, although its effect on each normalized partial correlation need not be monotone." A.6: deleted "or path-matrix" from the GIMME graph-conversion cell (the readout is fixed as the standardized `fit$psi` residual covariance). |

Static re-validation after R1–R6 (no R/Quarto runtime here): all 11
`REVISION_RESULTS_PENDING`/`REVISION_FIGURE_PENDING` + `CLAIM_LEDGER` comments byte-preserved;
18 chunk labels intact; 38 ``` fences, 18 ```{r}; 118 `$$` (even); 2 `\begin{equation}`/
`\end{equation}` pairs; numeric-token regression over all checked-in Results/tables shows zero
reduced counts; no flagged phrase remains (`common analysis window`, `common-window`,
`or path-matrix` all 0). The regenerated unified diff round-trips RR → V2 byte-identically
(`patch`-verified). The single command `quarto render
paper/Paper_emotion_dynamics_HGolino_RR_V2.qmd` in the repo environment closes the render
check (I10).
