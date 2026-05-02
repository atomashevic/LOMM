# Paper_emotion_dynamics_HGolino.Rmd plot readout

Source inspected: `../Paper_emotion_dynamics_HGolino.Rmd`.

## Executable plot chunks in the Rmd

The Rmd defines two executable figure-producing chunks before the simulation benchmark section:

- `dlosim` at lines 173-323. This produces a 2 x 2 `ggarrange()` figure of simulated latent trajectories for two DLO parameter scenarios, each shown with `sigma_q = 0` and `sigma_q = 0.5`.
- `dloobs` at lines 345-374. This produces a two-row figure: latent trajectories on top and six generated observable emotion scores below.

The `boris` chunk at lines 391-400 has a figure caption but only reads CSV files and defines colors; it does not draw or return a plot in the current file.

The DynEGA network illustration block at lines 471-705 is inside an HTML comment, ending with ```` ``` --> ````. It is therefore not executed by knitr and does not produce a figure unless uncommented and repaired.

## Simulation benchmark comparison section

The simulation benchmark section starts at line 708. It describes a Monte Carlo benchmark of EII methods, but it does not contain executable plotting code.

The benchmark conditions described in prose are:

- sample size: `N = 50, 100`
- variables per factor in the main group: `NVAR = 4, 6`
- number of factors in the main group: `NFAC = 2, 3`
- secondary-group variables/factors for the `NotEq` condition: `NVAR2 = 2, 3, 4, or 9`; `NFAC2 = 2, 3, or 4`, depending on the main `NVAR`/`NFAC` combination
- measurement error: `0.125`, `0.25`, `0.5`, squared in the measurement error covariance diagonal
- dynamic factor loadings: `LOAD = 0.4, 0.6, 0.8`
- EII method: `weighted`, `unweighted`, `edge list`
- held constant: autoregressive coefficients `0.8`, cross-regressive coefficients `0`, random-shock covariance off-diagonal `0.18`, random-shock variance diagonal `0.36`
- repetitions: prose says `500` data matrices per condition

The results subsection lists intended outputs in prose:

- accuracy / hit rate for the three EII methods
- partial eta-squared effect sizes for manipulated conditions and combinations
- correlations
- mean EII values for `Eq` and `NotEq`
- `NotEq - Eq` mean EII differences, where positive values indicate correct discrimination

The only code in the Results section is an `eval = FALSE` chunk that computes an effect-size table with `latentFactoR:::effect_table(...)` and saves `effect.sizes.all.RData`. Because `eval = FALSE`, it is not run during rendering and does not produce a plot.

## Existing comparison plots in this checkout

The comparative benchmark plots present in the repository are not produced by `Paper_emotion_dynamics_HGolino.Rmd`. They come from separate scripts and output directories:

- `comparison_dynega_graphicalvar/`: older DynEGA vs GraphicalVAR comparison plots, including factor correlation, exact n.dim recovery, NRMSE, small multiples, advantage, and runtime-speedup plots.
- `comparison_dynega_graphicalvar_variants/`: current main figure bundle from `plot_dynega_graphicalvar_variant_figures.R`, with `baseline_dynega_graphicalvar_recovery.{png,pdf}` and `variant_sensitivity_low_single_stable.{png,pdf}`.
- `comparison_dynega_graphicalvar_collab/`: current collaborator-facing suite from `plot_dynega_graphicalvar_collaborator_suite.R`, with five numbered figures:
  - `01_main_low_single_stable_exact_ndim_recovery.{png,pdf}`
  - `02_main_low_single_stable_estimated_ndim_ranges.{png,pdf}`
  - `03_scenario_winner_grid_exact_ndim.{png,pdf}`
  - `04_dynega_advantage_by_scenario.{png,pdf}`
  - `05_graphicalvar_cd_variant_check.{png,pdf}`

The current checkout has no `Examples/` directory, so the Rmd cannot be rendered as-is: the executable chunks load files such as `Examples/scenario_1.RData` and `Examples/boris/boris-1.csv`.
