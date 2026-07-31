"""LOMM numerical audit code (I1, I2, I3, I6).

Reusable simulation/estimation utilities. Fixed seeds throughout.
Model: per factor f, state s=[x,v]', A_f=[[0,1],[eta,zeta]],
exact discrete step s_{t+1} = Phi s_t + Gamma q_t, Phi=expm(A dt),
Gamma = int_0^dt expm(A u) b du, b=(0,1)', q_t ~ N(0, sigma_q^2) (scalar ZOH shock).
"""
import numpy as np
from scipy.linalg import expm, solve_discrete_lyapunov

B_VEC = np.array([0.0, 1.0])


def A_mat(eta, zeta):
    return np.array([[0.0, 1.0], [eta, zeta]])


def gamma_augmented(eta, zeta, dt):
    """Reference Phi and Gamma from the augmented matrix exponential."""
    M = np.zeros((3, 3))
    M[:2, :2] = A_mat(eta, zeta)
    M[:2, 2] = B_VEC
    E = expm(M * dt)
    return E[:2, :2], E[:2, 2]


def phi_branch(eta, zeta, dt):
    A = A_mat(eta, zeta)
    half = zeta / 2.0
    M = A - half * np.eye(2)
    disc = half ** 2 + eta
    if disc > 1e-12:
        s = np.sqrt(disc); coef = np.sinh(s * dt) / s; base = np.cosh(s * dt)
    elif disc < -1e-12:
        s = np.sqrt(-disc); coef = np.sin(s * dt) / s; base = np.cos(s * dt)
    else:
        coef = dt; base = 1.0
    return np.exp(half * dt) * (base * np.eye(2) + coef * M)


def gamma_branch(eta, zeta, dt, nterms=40):
    A = A_mat(eta, zeta)
    Phi = phi_branch(eta, zeta, dt)
    if abs(eta) > 1e-10:
        return np.array([(Phi[0, 0] - 1.0) / eta, Phi[1, 0] / eta])
    g = np.zeros(2)
    term = B_VEC.copy() * dt          # n=0: A^0 b dt^1 / 1!
    for n in range(nterms):
        g = g + term
        term = A @ term * dt / (n + 2)  # term_{n+1} = A^{n+1} b dt^{n+2}/(n+2)!
    return g


def simulate_panel(etas, zetas, sigmas, N, T, dt, start, rng):
    """Exact discrete simulation. start in {'legacy','revised'}.
    legacy: all videos/factors x=0.25, v=2.
    revised: stable factors ~ N(0, P) with P the stationary covariance;
             anti-damped factors x0~U(-0.25,0.25), v0~U(-2,2).
    Returns X (N, T, d) of levels."""
    d = len(etas)
    s = np.zeros((N, 2, d))
    for f in range(d):
        if start == "legacy":
            s[:, 0, f] = 0.25
            s[:, 1, f] = 2.0
        else:
            roots = np.roots([1.0, -zetas[f], -etas[f]])
            if max(np.abs(np.exp(roots * dt))) < 1.0:   # discrete-stable
                Phi, Gam = gamma_augmented(etas[f], zetas[f], dt)
                Q = sigmas[f] ** 2 * np.outer(Gam, Gam)
                P = solve_discrete_lyapunov(Phi, Q)
                L = np.linalg.cholesky(P + 1e-14 * np.eye(2))
                s[:, :, f] = rng.standard_normal((N, 2)) @ L.T
            else:
                s[:, 0, f] = rng.uniform(-0.25, 0.25, N)
                s[:, 1, f] = rng.uniform(-2.0, 2.0, N)
    Phis, Gams = zip(*[gamma_augmented(etas[f], zetas[f], dt) for f in range(d)])
    X = np.empty((N, T, d))
    for t in range(T):
        X[:, t, :] = s[:, 0, :]
        q = rng.standard_normal((N, d)) * np.asarray(sigmas)
        for f in range(d):
            s[:, :, f] = s[:, :, f] @ Phis[f].T + np.outer(q[:, f], Gams[f])
    return X


def gains(N, d, active_gain=1.00, bg=0.08):
    """Single-dominance activity: video i has active factor i mod d."""
    a = np.full((N, d), bg)
    a[np.arange(N), np.arange(N) % d] = active_gain
    return a


def cross_moments(M, nbatch=20):
    """Pooled raw and centered cross-factor moments with batch-over-videos SEs.
    M: (N, T, d). Returns dict with mu, raw, raw_se, cen, cen_se (keys (f,g), f<g)."""
    N, T, d = M.shape
    pairs = [(f, g) for f in range(d) for g in range(f + 1, d)]
    flat = M.reshape(N * T, d)
    mu = flat.mean(axis=0)
    raw, cen, raw_se, cen_se = {}, {}, {}, {}
    bs = N // nbatch
    for (f, g) in pairs:
        raw[(f, g)] = (flat[:, f] * flat[:, g]).mean()
        cen[(f, g)] = ((flat[:, f] - mu[f]) * (flat[:, g] - mu[g])).mean()
        rb, cb = [], []
        for b in range(nbatch):
            Mb = M[b * bs:(b + 1) * bs].reshape(bs * T, d)
            mub = Mb.mean(axis=0)
            rb.append((Mb[:, f] * Mb[:, g]).mean())
            cb.append(((Mb[:, f] - mub[f]) * (Mb[:, g] - mub[g])).mean())
        raw_se[(f, g)] = np.std(rb, ddof=1) / np.sqrt(nbatch)
        cen_se[(f, g)] = np.std(cb, ddof=1) / np.sqrt(nbatch)
    return dict(mu=mu, raw=raw, raw_se=raw_se, cen=cen, cen_se=cen_se)


# --- scenario parameter vectors (first d entries used) ---
TEMPLATE_ETA = [-0.30, -0.55, -0.85, -1.10]
TEMPLATE_ZETA = [-1.70, -1.00, -0.65, -0.30]
TEMPLATE_SIGMA = [0.12, 0.30, 0.45, 0.65]     # base; A: x0.5, B/C: x1.0, D: x1.25
RANKED_ETA = [-0.05, -0.10, -0.15, -0.20]
RANKED_ZETA = [0.08, 0.10, 0.12, 0.14]        # E: sigma 0.25, F: sigma 0.35

if __name__ == "__main__":
    # minimal smoke test: ordinary-case input map against augmented reference
    Phi_r, G_r = gamma_augmented(-0.55, -1.00, 0.4)
    assert np.max(np.abs(phi_branch(-0.55, -1.00, 0.4) - Phi_r)) < 1e-12
    assert np.max(np.abs(gamma_branch(-0.55, -1.00, 0.4) - G_r)) < 1e-12
    rng = np.random.default_rng(0)
    X = simulate_panel(TEMPLATE_ETA[:3], TEMPLATE_ZETA[:3], [0.12, 0.30, 0.45],
                       50, 100, 0.4, "revised", rng)
    print("smoke test OK", X.shape)
