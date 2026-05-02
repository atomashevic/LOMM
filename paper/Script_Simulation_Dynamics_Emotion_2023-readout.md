# Script_Simulation_Dynamics_Emotion_2023 readout

Source: user-pasted script titled `Script_Simulation_Dynamics_Emotion_2023`.
No matching local script or `Simulation/` output directory was found in the current checkout.

## Benchmark identity

This is a DynEGA-only emotion-dynamics simulation benchmark. It is not a DynEGA vs GraphicalVAR vs GIMME comparison script.

The script installs and uses `EGAnet` and `transforEmotion`, simulates videos from `transforEmotion::simulate_video()`, estimates population DynEGA structures, selects `n.embed` by lowest population TEFI, and stores dimension-recovery and network-score/latent-score correlations.

## Simulation conditions

The manipulated condition grid is:

- `N`: `50`, `100` videos per replication
- `TimeP`: `100`, `300`, `600` frames/time points
- `NVAR`: `3`, `4` observable variables per measured latent factor
- `Error`: `0.01`, `0.025`, `0.05` measurement-error SD for observable variables
- `Q`: `0.5`, `0.75`, `1.0` latent dynamic error (`sigma_q`)
- `Loadings`: `0.4`, `0.6`, `0.8`
- `uni.method`: `louvain`, `expand`
- `cd.algorithm`: `louvain`, `walktrap`
- `replications`: `500`

This is `2 x 3 x 2 x 3 x 3 x 3 x 2 x 2 = 1296` grid conditions.
With `500` replications, the intended run has `648000` condition-replication evaluations, each simulating either `50` or `100` videos.

Within each simulated video:

- `dt = 0.4` when `TimeP == 100`, otherwise `dt = 0.25`
- `window_size = 5`
- `num_observables = NVAR`
- `loadings = Loadings`
- `sd_observable = Error`
- `sigma_q = Q`

Each video randomly samples one of four DLO parameter scenarios:

1. `eta_n = -0.1`, `zeta_n = -2`, `eta = -1`, `zeta = -0.1`
2. `eta_n = -0.5`, `zeta_n = -1.5`, `eta = -0.5`, `zeta = -0.1`
3. `eta_n = -0.5`, `zeta_n = -0.1`, `eta = -0.1`, `zeta = 1`
4. `eta_n = -1`, `zeta_n = 1`, `eta = 0.75`, `zeta = 1`

The true target dimensionality used for hit-rate scoring is `2`, corresponding to positive and negative measured latent emotion factors. The neutrality process is used for scaling/baseline dynamics and is not part of the observable measurement structure.

## DynEGA estimation conditions

For each condition, the script tests candidate embedding dimensions:

- `n.embed.dims = 5, 10, 15, floor(TimeP / 4), floor(TimeP / 2)`

For zero-order DynEGA:

- `use.derivatives = 0`
- `algorithm = cd.algorithm`
- `uni.method = uni.method`
- best model selected by minimum population `TEFI`

For first-order DynEGA:

- `use.derivatives = 1`
- `algorithm = "louvain"` hard-coded
- best model selected by minimum population `TEFI`

The returned fields per run are:

- `ndim.0`: zero-order estimated population number of dimensions
- `ndim.1`: first-order estimated population number of dimensions
- `corPos`: best average correlation between zero-order network scores and latent positive score
- `corNeg`: best average correlation between zero-order network scores and latent negative score
- `N.Embed.0`: selected zero-order embedding dimension
- `N.Embed.1`: selected first-order embedding dimension

## Files saved by the pasted script

The script saves result objects, not image files:

- `Simulation/res.emo.sim.RData`
- `Simulation/res.all.df.RData`
- `Simulation/es.hit0.RData`
- `Simulation/summary.hit0.plot.RData`
- `Simulation/es.hit1.RData`
- `Simulation/summary.hit1.plot.RData`
- `Simulation/es.corpos.RData`
- `Simulation/summary.corPos.plot.RData`
- `Simulation/es.corneg.RData`
- `Simulation/summary.corNeg.plot.RData`

## Comparative plots produced

As pasted, this script produces no comparative plot image files.

It loads `ggplot2` and creates plot-ready summary objects, but it does not call `ggplot(...)`, `ggsave(...)`, `png(...)`, `pdf(...)`, or equivalent plotting-output functions in the shown code.

The plot-ready comparative summaries are:

- `summary.hit0.plot`: zero-order exact two-dimension recovery percentage (`HIT`) grouped by `TimeP`, `NVAR`, and `uni.method`
- `summary.hit1.plot`: first-order exact two-dimension recovery percentage (`HIT`) grouped by `TimeP`, `NVAR`, and `N`
- `summary.corPos.plot`: intended positive-score correlation summary grouped by `TimeP`, `Error`, and `NVAR`
- `summary.corNeg.plot`: negative-score correlation summary grouped by `TimeP`, `Error`, and `NVAR`

The effect-size tables are:

- `es.hit0`: condition effects on zero-order HIT
- `es.hit1`: condition effects on first-order HIT
- `es.corpos`: condition effects on positive-score correlation
- `es.corneg`: condition effects on negative-score correlation

## Code issues visible in the pasted text

- The line `zeta =` is incomplete. As pasted, the script will not parse. It likely intended `zeta = scenario[4]`.
- The `simulate_video()` call passes `zeta_n` without a name. It may work only if positional matching happens to align with the installed `simulate_video()` signature; it should be written as `zeta_n = zeta_n`.
- The first-order DynEGA branch hard-codes `algorithm = "louvain"` and does not pass `uni.method`, even though both variables remain in the condition grid and later effect-size table.
- `summary.hit1` computes `Mean = mean(ndim.0, na.rm = TRUE)` while summarizing first-order DynEGA; this should probably be `mean(ndim.1, na.rm = TRUE)`.
- `summary.corPos.plot` uses `mean(corNeg, na.rm = TRUE)` despite being the positive-correlation plotting summary. As pasted, `summary.corPos.plot` and `summary.corNeg.plot` summarize the same correlation target.
