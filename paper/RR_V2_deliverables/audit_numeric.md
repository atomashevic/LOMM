# LOMM numerical implementation audit (I1, I2, I3, I6)

Environment: numpy 2.2.5, scipy 1.16.2 (Linux, x86-64). All random draws use `numpy.random.default_rng` with fixed seeds (I3: 20240317 per scenario/start; I6: 777 / 778). All simulations are vectorized over videos; time loops only over T=100 steps. Reference quantities use `scipy.linalg.expm` (augmented system) and `scipy.integrate.solve_ivp` at rtol=1e-12/atol=1e-14. Reusable code: `lomm_work/lomm_audit.py`.

Model under audit, per factor f: state s=[x,v]', A_f = [[0,1],[eta,zeta]], exact discrete step s_{t+1} = Phi s_t + Gamma q_t with Phi = expm(A dt), Gamma = ∫_0^dt expm(A u) b du, b=(0,1)', q_t ~ N(0, sigma_q^2) iid scalar zero-order-hold shock per factor per step. Horizons: H[100] = 99×0.4 = 39.6; H[300] = 299×0.25 = 74.75.

---

## I1 — Exact transition and input map

Max-abs discrepancies against the augmented-exponential reference expm([[A,b],[0,0]]·dt) (Gamma = top-right block) and against scipy expm for Phi:

| case | eta | zeta | err closed-form Gamma | err pinv Gamma | err branch Gamma | err branch Phi |
|---|---|---|---|---|---|---|
| ordinary | -0.55 | -1.00000000 | 0.00e+00 | 1.67e-16 | 2.08e-16 | 1.11e-16 |
| singular | 0.00 | -0.50000000 | n/a (A singular) | 2.90e-01 | 5.55e-17 | 2.22e-16 |
| repeated | -0.50 | -1.41421356 | 1.39e-17 | 5.55e-17 | 2.08e-16 | 1.11e-16 |
| near_rep_plus | -0.50 | -1.41421355 | 8.33e-17 | 8.33e-17 | 1.39e-16 | 1.11e-16 |
| near_rep_minus | -0.50 | -1.41421357 | 1.39e-17 | 2.78e-17 | 2.08e-16 | 2.22e-16 |
Singular case detail (eta=0, zeta=-0.5, dt=0.4): Gamma reference = [0.074923, 0.362538]; pinv fallback = [-0.145015, 0.072508]; branch Taylor series = [0.074923, 0.362538]. The pinv fallback does not merely lose precision — it returns a wrong vector (max abs error 2.90e-01, i.e. O(1) relative to ||Gamma|| ≈ 0.37). Reason: for singular A, (Phi − I)·pinv(A)·b annihilates the component of the integral along the null direction of A; the true integral ∫expm(Au)b du has a nonzero linear-in-dt term there.

One discrete step s1 = Phi s0 + Gamma q vs solve_ivp on x'' = eta x + zeta x' + q, s0 = (0.3, −0.2), q = 1.7 constant, dt = 0.4:

| case | max abs |disc − ODE| |
|---|---|
| ordinary | 1.654e-14 |
| singular | 1.099e-14 |
| repeated | 1.343e-14 |
| near_rep_plus | 1.349e-14 |
| near_rep_minus | 1.338e-14 |

### I1 verdicts

| check | verdict | evidence |
|---|---|---|
| (i) closed form (Phi−I)A⁻¹b matches augmented expm in ordinary (nonsingular) cases | **PASS** | errors 0.0–1.4e-16 (ordinary, repeated, near-repeated±1e-8) |
| (ii) pinv fallback exact in the singular case | **FAIL** | error 2.90e-01, O(1) relative error; legacy fallback is not a valid substitute |
| (iii) branch formulas handle repeated roots (incl. near-repeated at ±1e-8) | **PASS** | Gamma ≤2.1e-16, Phi ≤2.2e-16 in repeated and both near-repeated cases; no cancellation instability at the disc≈0 branch (coef=dt, base=1 branch) |
| (iv) one discrete step matches the ODE solution (scalar ZOH shock semantics) | **PASS** | max discrepancy 1.7e-14 across all five cases, consistent with integrator tolerance |

---

## I2 — Growth diagnostics

**Terminology (required caveat):** g(H) = exp(H·max Re r±) is a **modal-rate factor** (the pure exponential envelope rate of the dominant eigenmode), *not* the total amplification of the state-transition map. Because the oscillator state matrix A is non-normal, ||expm(A u)||₂ generally differs from exp(α u); the sup over [0,H] also trivially includes u=0 where ||expm(0)||₂ = 1.

Method: roots r± = (zeta ± sqrt(zeta²+4eta, complex))/2; alpha = max Re(r±); g(H) = exp(H alpha); sup norms on a 40000-point grid over [0,H] (closed-form vectorized expm for the 2×2 oscillator) with a bounded `minimize_scalar` refinement around the grid maximizer (xatol=1e-12); "edge" noted when the sup sits at u=H. Shock magnitude sigma does not enter any of these quantities, so codes A–D (template; sigma scalings 0.5/1.0/1.0/1.25) are numerically identical row-for-row, as are E and F (ranked; sigma 0.25/0.35); rows below are nevertheless enumerated per code.

### Collapsed table (unique factors; codes share rows as indicated)

| codes | factor | eta | zeta | max Re r | regime | g(H100) | g(H300) | sup H100 | sup H300 | sup/g H100 | sup/g H300 | ||e^{AH}||/g H100 | ||e^{AH}||/g H300 | level-only sup H100/H300 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| A,B,C,D | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | 1.414 | 1.414 | 1.030 / 1.030 |
| A,B,C,D | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | 1.340 | 1.107 | 1.018 / 1.018 |
| A,B,C,D | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | 1.158 | 1.457 | 1.001 / 1.001 |
| A,B,C,D | 4 | -1.10 | -0.30 | -0.150 | damped complex | 2.632e-03 | 1.350e-05 | 1.0017 | 1.0017 | 381 | 7.42e+04 | 1.041 | 1.131 | 1.000 / 1.000 |
| E,F | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | 3.151 | 3.223 | 19.335 / 61.982 |
| E,F | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | 1.331 | 3.147 | 18.972 / 131.233 |
| E,F | 3 | -0.15 | 0.12 | 0.060 | anti-damped complex | 1.076e+01 | 8.868e+01 | 24.4891 | 175.7024 | 2.28 | 1.98 | 1.754 | 1.423 | 24.489 / 175.702 |
| E,F | 4 | -0.20 | 0.14 | 0.070 | anti-damped complex | 1.599e+01 | 1.873e+02 | 35.8687 | 426.3959 | 2.24 | 2.28 | 2.243 | 2.277 | 35.868 / 425.229 |

### Reading of the flags

Every row triggers the mechanical flags (ratio sup/g > 1.05 and ||expm(AH)||₂ > g(H)), but for two distinct reasons that must be separated:

1. **Damped template factors (A–D):** the sup norm over [0,H] never exceeds 1.0300 — there is **no transient growth** beyond a 3% initial-velocity overshoot (level-only sup ≤ 1.0300 as well). The astronomically large sup/g ratios (up to 1.7e16) are purely an artifact of comparing a whole-interval sup (which includes u=0, norm = 1) against a decayed endpoint modal rate g(H) ≈ 3.6e-4 … 5.9e-17. The meaningful non-normality statement here is the endpoint comparison: ||expm(AH)||₂ exceeds g(H) by only **4%–46%** (ratio 1.041–1.457). For damped factors, g(H) therefore mildly understates the true endpoint norm but correctly conveys that there is no amplification.
2. **Anti-damped ranked factors (E–F):** the sup norm exceeds g(H) by a **material factor 1.98–3.97** at both horizons, and the endpoint norm ||expm(AH)||₂ itself exceeds g(H) by 1.33–3.22. This is genuine: the growing oscillation rotates energy between level and velocity, and the full-state envelope (velocity included) peaks well above the pure modal rate; for factors 1–2 at H100 and factor 1 at H300 the sup is attained before H ("edge": still growing at H). Level-only sups track the full-state sups (within ~5%), so the understatement is not a velocity artifact alone.

### I2 verdict

| check | verdict |
|---|---|
| sup norm materially above g(H) (ratio > 1.05) in some case | **YES (flagged)** — all anti-damped E/F factors, ratio 1.98–3.97; template factors only trivially (decayed denominator) |
| ||expm(AH)||₂ > g(H) in some case | **YES (flagged)** — all factors; by 4–46% (template) and 33–223% (ranked) |
| g(H) must be labeled a modal-rate factor, not total amplification | **Required and confirmed necessary** |

---

## I3 — Initialization and cross-block moments

Design: oracle panels (no measurement error), exact discrete transition. Scenario B 3D (template, stable; sigma = 0.12/0.30/0.45) and scenario E 3D (ranked anti-damped; sigma = 0.25 each). N=2000 videos, T=100, dt=0.4. Single-dominance gating: active gain 1.00 on factor (i mod 3), background 0.08; Z_f(t) = a_f(t) X_f(t). Starts: legacy common start (x=0.25, v=2 for every video/factor) vs revised independent starts (stable factors ~ N(0,P), P solving P = Phi P Phi' + sigma_q² Gamma Gamma' via `solve_discrete_lyapunov`; anti-damped factors x0~U(−0.25,0.25), v0~U(−2,2), iid per video/factor). MC SEs: 20 batches of 100 videos, batch means, SE of the mean. t = estimate/SE. Retained rows: all (i,t). The ungated process X (a_f ≡ 1) is reported alongside to isolate the gating contribution.

**B3D, legacy start, process Z** (pooled means mu = [0.0791, 0.0387, 0.0252])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | 0.00606 | 0.00002 | 251.0 | 0.00300 | 0.00006 | 53.8 |
| 1,3 | 0.00413 | 0.00004 | 101.8 | 0.00213 | 0.00007 | 32.7 |
| 2,3 | 0.00541 | 0.00003 | 174.0 | 0.00444 | 0.00005 | 89.3 |

**B3D, legacy start, process X** (pooled means mu = [0.204 , 0.1032, 0.065 ])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | 0.10902 | 0.00030 | 366.1 | 0.08797 | 0.00029 | 302.3 |
| 1,3 | 0.07433 | 0.00059 | 126.0 | 0.06107 | 0.00043 | 142.1 |
| 2,3 | 0.09746 | 0.00055 | 177.9 | 0.09076 | 0.00053 | 171.5 |

**B3D, revised start, process Z** (pooled means mu = [ 0.0008, -0.0011,  0.0006])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | -0.00000 | 0.00001 | -0.7 | -0.00000 | 0.00001 | -0.5 |
| 1,3 | 0.00000 | 0.00001 | 0.0 | -0.00000 | 0.00001 | -0.0 |
| 2,3 | 0.00005 | 0.00002 | 2.8 | 0.00005 | 0.00002 | 2.7 |

**B3D, revised start, process X** (pooled means mu = [0.0015, 0.0001, 0.0007])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | 0.00003 | 0.00008 | 0.4 | 0.00003 | 0.00008 | 0.4 |
| 1,3 | 0.00005 | 0.00008 | 0.6 | 0.00005 | 0.00008 | 0.6 |
| 2,3 | 0.00052 | 0.00031 | 1.7 | 0.00052 | 0.00031 | 1.7 |

**E3D, legacy start, process Z** (pooled means mu = [ 2.1182, -1.2407,  1.4724])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | -12.61031 | 0.11808 | -106.8 | -9.98223 | 0.09240 | -108.0 |
| 1,3 | 8.44621 | 0.09794 | 86.2 | 5.32735 | 0.07861 | 67.8 |
| 2,3 | -9.64823 | 0.11475 | -84.1 | -7.82140 | 0.10311 | -75.9 |

**E3D, legacy start, process X** (pooled means mu = [ 5.4663, -3.1761,  3.8354])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | -224.54593 | 1.51345 | -148.4 | -207.18445 | 1.41082 | -146.9 |
| 1,3 | 153.32575 | 1.39758 | 109.7 | 132.36021 | 1.29813 | 102.0 |
| 2,3 | -174.58814 | 1.15079 | -151.7 | -162.40634 | 1.11598 | -145.5 |

**E3D, revised start, process Z** (pooled means mu = [0.0482, 0.0403, 0.0206])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | -0.23117 | 0.15472 | -1.5 | -0.23311 | 0.15675 | -1.5 |
| 1,3 | -0.03616 | 0.10866 | -0.3 | -0.03715 | 0.10835 | -0.3 |
| 2,3 | -0.04551 | 0.11769 | -0.4 | -0.04635 | 0.11723 | -0.4 |

**E3D, revised start, process X** (pooled means mu = [0.1086, 0.1007, 0.0602])
| pair (f,g) | raw <M_f M_g> | SE | t | centered cov | SE | t |
|---|---|---|---|---|---|---|
| 1,2 | -1.70065 | 2.37129 | -0.7 | -1.71158 | 2.36942 | -0.7 |
| 1,3 | -1.57677 | 1.40419 | -1.1 | -1.58331 | 1.40100 | -1.1 |
| 2,3 | -1.54964 | 1.77284 | -0.9 | -1.55570 | 1.77349 | -0.9 |
### I3 verdicts

| check | verdict | evidence |
|---|---|---|
| Legacy common start creates a systematic cross-block term (raw moments) | **YES — confirmed** | t = 32.7–251 (B, gated Z), 67.8–108 (E, gated Z); ungated X even larger in absolute terms (raw up to ±224.5, |t| up to 366) |
| Legacy common start creates a systematic cross-block term (centered covariances) | **YES — confirmed** | t = 32.7–108 (Z); up to ±302 (X). Centering removes only the common mean, not the shared deterministic startup transient, so cross-block structure survives centering |
| Revised independent start removes cross-block terms to MC tolerance | **PASS** | B: |t| ≤ 2.8 (Z), ≤ 1.7 (X); E: |t| ≤ 1.5 (Z), ≤ 1.1 (X). Largest value (B gated pair 2,3: t=2.8, estimate 5e-5) is marginal but two orders of magnitude below the legacy bias (0.0044) and consistent with 1 of 12 tests at the 5% level |
| Gating contribution | The gating attenuates (gains 1.0/0.08) but does not create or remove the cross-block bias: ungated X shows the same pattern at larger scale; with revised starts both Z and X are clean |

---

## I6 — GraphicalVAR population check

Design: scenario B 2D (stable; eta=[−0.30,−0.55], zeta=[−1.70,−1.00], sigma=[0.12,0.30]) and scenario E 2D (anti-damped; eta=[−0.05,−0.10], zeta=[0.08,0.10], sigma=[0.25,0.25]). m=2 factors, k=4 indicators per factor (8 channels), lambda=0.6, psi=s_eps²=0.0625 (s_eps=0.25). N=3000 videos, T=100, dt=0.4; single-dominance gating as in I3; revised independent starts. Measurement Y_j(t) = lambda Z_f(t) + eps_j(t), eps iid N(0,psi).

Conventions: retained within-video lag-1 pairs (t−1,t), t=1..99 (99 pairs/video; no lags across video boundaries); per-video per-channel centering over the T=100 rows (mlGraphicalVAR `centerWithin=TRUE` convention), applied before forming pairs; **unstandardized** values are used for the population algebra — note as a manifest fact that mlGraphicalVAR additionally standardizes (`scale=TRUE` by default in `graphicalVAR`), so fitted package output must be un-standardized before comparing to these numbers. Theoretical blocks: S_-- = lambda²V_- J_k + psi I_k; S_+- = lambda²C J_k; S_++ = lambda²V_+ J_k + psi I_k; B = lambda²C/(psi + k lambda²V_-) J_k; Omega = S_++ − S_+- S_--⁻¹ S_+-'; c_VAR = lambda²V_+ − k lambda⁴C²/(psi + k lambda²V_-); theta_VAR = c_VAR/psi; wP = theta_VAR/(1+(k−1)theta_VAR). Empirical: pooled 8-channel OLS of Y_t on Y_{t−1} (centered pairs, no intercept), residual covariance S_res; MC SEs from 20 batches of 150 videos recomputing the whole pipeline.

**B2D stable** — latent moments (after per-video centering, retained lag-1 pairs, n = 3000×99 = 297000 pairs)
| factor | V_- latent | V_+ latent | C latent | V_- obs(1 ch) | lam^2 V_-+psi | V_+ obs(1 ch) | C obs(1 ch) | lam^2 C |
|---|---|---|---|---|---|---|---|---|
| 1 | 0.002128 | 0.002129 | 0.002079 | 0.062704 | 0.063266 | 0.062699 | 0.000265 | 0.000748 |
| 2 | 0.014745 | 0.014748 | 0.014160 | 0.067384 | 0.067808 | 0.067404 | 0.004415 | 0.005098 |

| factor | c_VAR | theta_VAR | wP formula | wP empirical | B entry theo | B entry emp (block mean) | resid block-avg AC(1) | latent proj-resid AC(1) |
|---|---|---|---|---|---|---|---|---|
| 1 | 0.000732 | 0.0117 | 0.0113 | 0.0113 | 0.011416 | 0.009298 | -0.0012 | 0.7339 |
| 2 | 0.004068 | 0.0651 | 0.0545 | 0.0564 | 0.060879 | 0.059673 | -0.0399 | 0.8007 |

Omega check: max in-block |empirical - theoretical| = **6.76e-04** (approx per-entry MC SE 1.2e-4 for B blocks, 5-9e-4 for E blocks). Cross-block residual-covariance entries: max |emp| = 3.85e-04, max batch SE = 1.67e-04, max |t| = 4.51. Lag-0 cross-block moment = 0.00008 (SE 0.00014); lag-1 cross-block moment = -0.00001 (SE 0.00013).

**Uncentered control (B2D):** repeating the fit without per-video centering (population means are ~0 under revised starts) gives max in-block |emp−theo| = **2.37e-04** (≈2× entry SE, i.e. MC noise) and B entries 0.015151 / 0.065312 vs theoretical 0.015053 / 0.065295 (≤0.7%). Hence the residual centered-case gap (6.76e-04, and the f1 B-entry deviation 0.0114→0.0093) is entirely attributable to the finite-T (T=100) `centerWithin` bias of order psi/T ≈ 6.25e-04 on the noise moments, not to an error in the population algebra.

**E2D anti-damped** — latent moments (after per-video centering, retained lag-1 pairs, n = 3000×99 = 297000 pairs)
| factor | V_- latent | V_+ latent | C latent | V_- obs(1 ch) | lam^2 V_-+psi | V_+ obs(1 ch) | C obs(1 ch) | lam^2 C |
|---|---|---|---|---|---|---|---|---|
| 1 | 65.575054 | 66.913299 | 66.034397 | 23.666658 | 23.669520 | 24.147991 | 23.769044 | 23.772383 |
| 2 | 53.755000 | 54.139472 | 53.550940 | 19.410937 | 19.414300 | 19.549001 | 19.274726 | 19.278339 |

| factor | c_VAR | theta_VAR | wP formula | wP empirical | B entry theo | B entry emp (block mean) | resid block-avg AC(1) | latent proj-resid AC(1) |
|---|---|---|---|---|---|---|---|---|
| 1 | 0.165717 | 2.6515 | 0.2961 | 0.2966 | 0.251585 | 0.251583 | 0.7337 | 0.9942 |
| 2 | 0.300548 | 4.8088 | 0.3117 | 0.3119 | 0.248850 | 0.248851 | 0.8404 | 0.9904 |

Omega check: max in-block |empirical - theoretical| = **1.10e-03** (approx per-entry MC SE 1.2e-4 for B blocks, 5-9e-4 for E blocks). Cross-block residual-covariance entries: max |emp| = 6.73e-04, max batch SE = 5.39e-04, max |t| = 1.48. Lag-0 cross-block moment = -0.04971 (SE 0.04303); lag-1 cross-block moment = -0.05151 (SE 0.04471).

### I6 verdicts

| check | verdict | evidence |
|---|---|---|
| Observed-block moments match lambda²·latent + psi structure | **PASS** | e.g. E f1: V_- obs 23.6667 vs lam²V_-+psi 23.6695 (centering explains the residual ~3e-3); B consistent within the same centering tolerance |
| B = lambda²C/(psi+k lambda²V_-) J matches empirical OLS | **PASS (E); QUALIFIED PASS (B)** | E: agreement to 1e-6 (0.251585 vs 0.251583); B: f2 within 2%, f1 deviates 0.0114 vs 0.0093 — fully explained by the finite-T centerWithin bias (uncentered control: ≤0.7%) |
| Omega block-diagonal formula matches empirical residual covariance | **PASS (QUALIFIED for B)** | max in-block diff 6.76e-04 (B) / 1.10e-03 (E); E within ~2 entry SEs; B gap = psi/T centering artifact, confirmed by uncentered control (2.37e-04 ≈ MC noise) |
| Cross-block residual entries ≈ 0 | **PASS (E); QUALIFIED (B)** | E: max |t| = 1.48; lag-0/lag-1 cross moments −0.0497±0.0430, −0.0515±0.0447 (|t|≤1.2). B: lag-0/lag-1 cross 8e-5±1.4e-4, −1e-5±1.3e-4 (clean), but one of 16 residual cross entries has t=−4.51 (−3.85e-4 ± 0.85e-4); absolute size negligible vs in-block scale (0.0625) and attributable to OLS orthogonality coupling with near-noise regressors (theta_VAR=0.012–0.065) plus 16 multiple comparisons; flagged, not confirmed as systematic latent leakage |
| wP formula vs empirical within-block partial correlation | **PASS** | E: 0.2961 vs 0.2966, 0.3117 vs 0.3119; B: 0.0113 vs 0.0113, 0.0545 vs 0.0564 (3% relative, centering scale) |
| Lag-1 projection residuals are NOT VAR(1) innovations (stay serially correlated) | **PASS (confirmed)** | Latent lag-1 projection residual AC(1): B 0.734/0.801, E 0.994/0.990. Observed block-average residual AC(1): E 0.734/0.840 (strong); B ≈ −0.001/−0.040 — near zero only because measurement noise dominates the B residuals (theta_VAR ≤ 0.065), masking the latent serial correlation |

---

## Manuscript-facing implications

1. **Discrete map is sound; the legacy pinv fallback is not.** The closed-form input map (Phi−I)A⁻¹b and the branch formulas match the augmented-exponential reference to machine precision in all nonsingular, repeated-root, and near-repeated (±1e-8) cases, and one exact step reproduces a 1e-12-tolerance ODE solution — scalar zero-order-hold shock semantics confirmed. The legacy pseudo-inverse fallback for singular A is simply wrong (0.29 abs error, O(1) relative); any singular-A path must use the Taylor-series branch, which is exact here (5.6e-17).
2. **g(H) must be presented as a modal-rate factor, never as total amplification.** For the anti-damped ranked factors (E/F) the actual state-norm amplification exceeds exp(H·alpha) by factors of 2–4 (up to 3.97 at H100, factor 1), because the non-normal oscillator rotates energy between level and velocity. For the damped template factors g(H) is conservative in the opposite trivial sense (sup over [0,H] includes u=0) while the endpoint norm exceeds g(H) by only 4–46%; no damped factor shows transient growth above 1.03.
3. **The legacy common start (x=0.25, v=2 for all videos) creates a strong systematic cross-block term** in both raw second moments and centered cross-covariances (|t| up to ~300, gated and ungated), i.e. centering does not fix it; gating attenuates but does not remove it. **Revised independent starts remove the term to Monte Carlo tolerance** (|t| ≤ 2.8 across all 24 moment estimates, scenarios B and E). This directly supports the initialization revision.
4. **The GraphicalVAR lag-1 residual algebra is confirmed at the population level.** The block formulas for B, Omega, c_VAR, theta_VAR and the partial-correlation weight wP match the N=3000 Monte Carlo fit to 1e-6 in the anti-damped scenario; residual discrepancies in the stable scenario are fully explained by the finite-T `centerWithin` centering bias (~psi/T), as shown by the uncentered control. Cross-block leakage is zero within MC error in E and negligible/marginal in B.
5. **Lag-1 VAR projection residuals are not innovations.** The latent oscillator residuals after the best lag-1 linear projection remain strongly serially correlated (AC(1) ≈ 0.73–0.99 in both scenarios); the near-white observed residuals in the stable scenario are only a measurement-noise artifact (low theta_VAR). Any manuscript claim that the lag-1 GraphicalVAR residual covariance captures the full dynamics is numerically falsified for this model.

*Reproducibility: all numbers regenerated from the seeds above; module `lomm_work/lomm_audit.py` contains the transition/input-map functions, the panel simulator, gating, and moment utilities (smoke-tested).*


---

## Appendix — I2 full per-code table (all 6 codes × 3 dims × factors; A–D and E–F rows identical because sigma does not enter)

| code | dim | f | eta | zeta | max Re r | regime | g(H100) | g(H300) | sup norm H100 | sup norm H300 | sup/g H100 | sup/g H300 | flag |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| A | 2 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 2 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 3 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 3 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 3 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 4 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 4 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 4 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| A | 4 | 4 | -1.10 | -0.30 | -0.150 | damped complex | 2.632e-03 | 1.350e-05 | 1.0017 | 1.0017 | 381 | 7.42e+04 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 2 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 2 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 3 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 3 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 3 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 4 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 4 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 4 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| B | 4 | 4 | -1.10 | -0.30 | -0.150 | damped complex | 2.632e-03 | 1.350e-05 | 1.0017 | 1.0017 | 381 | 7.42e+04 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 2 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 2 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 3 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 3 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 3 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 4 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 4 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 4 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| C | 4 | 4 | -1.10 | -0.30 | -0.150 | damped complex | 2.632e-03 | 1.350e-05 | 1.0017 | 1.0017 | 381 | 7.42e+04 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 2 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 2 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 3 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 3 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 3 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 4 | 1 | -0.30 | -1.70 | -0.200 | damped real | 3.634e-04 | 3.216e-07 | 1.0300 | 1.0300 | 2.83e+03 | 3.2e+06 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 4 | 2 | -0.55 | -1.00 | -0.500 | damped complex | 2.517e-09 | 5.865e-17 | 1.0180 | 1.0180 | 4.04e+08 | 1.74e+16 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 4 | 3 | -0.85 | -0.65 | -0.325 | damped complex | 2.574e-06 | 2.814e-11 | 1.0014 | 1.0014 | 3.89e+05 | 3.56e+10 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| D | 4 | 4 | -1.10 | -0.30 | -0.150 | damped complex | 2.632e-03 | 1.350e-05 | 1.0017 | 1.0017 | 381 | 7.42e+04 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| E | 2 | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| E | 2 | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| E | 3 | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| E | 3 | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| E | 3 | 3 | -0.15 | 0.12 | 0.060 | anti-damped complex | 1.076e+01 | 8.868e+01 | 24.4891 | 175.7024 | 2.28 | 1.98 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| E | 4 | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| E | 4 | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| E | 4 | 3 | -0.15 | 0.12 | 0.060 | anti-damped complex | 1.076e+01 | 8.868e+01 | 24.4891 | 175.7024 | 2.28 | 1.98 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| E | 4 | 4 | -0.20 | 0.14 | 0.070 | anti-damped complex | 1.599e+01 | 1.873e+02 | 35.8687 | 426.3959 | 2.24 | 2.28 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 2 | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 2 | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 3 | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 3 | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 3 | 3 | -0.15 | 0.12 | 0.060 | anti-damped complex | 1.076e+01 | 8.868e+01 | 24.4891 | 175.7024 | 2.28 | 1.98 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| F | 4 | 1 | -0.05 | 0.08 | 0.040 | anti-damped complex | 4.874e+00 | 1.989e+01 | 19.3351 | 64.0875 | 3.97 | 3.22 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 4 | 2 | -0.10 | 0.10 | 0.050 | anti-damped complex | 7.243e+00 | 4.199e+01 | 18.9716 | 132.1556 | 2.62 | 3.15 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
| F | 4 | 3 | -0.15 | 0.12 | 0.060 | anti-damped complex | 1.076e+01 | 8.868e+01 | 24.4891 | 175.7024 | 2.28 | 1.98 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300 |
| F | 4 | 4 | -0.20 | 0.14 | 0.070 | anti-damped complex | 1.599e+01 | 1.873e+02 | 35.8687 | 426.3959 | 2.24 | 2.28 | sup>g(H100); sup>g(H300); ||e^{AH}||>g H100; ||e^{AH}||>g H300; edge |
