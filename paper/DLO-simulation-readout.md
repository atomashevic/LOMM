# DLO-simulation.R Readout

Source: `paper/DLO-simulation.R`.

## Global Simulation Mechanics

- Simulates three latent scores: neutral (`Neu`), negative (`N`), positive (`P`), plus derivatives (`dNeu`, `dN`, `dP`).
- Initial state is fixed in `simulate_video()`: `Neu = 0.5`, `N = 0.25`, `P = 0.25`; derivatives start at `dNeu = 4`, `dN = 2`, `dP = 2`.
- The DLO update uses `eta * x + zeta * dxdt + q`, with Gaussian dynamic error `q`.
- Latent scores are bounded below at 0 and rescaled at every time step to sum to 1.
- Observables are generated only for negative and positive factors: `num_observables` per factor, so `2 * num_observables` observed variables.
- `emphasize()` adds spikes to the strongest observed positive/negative emotion when the corresponding latent derivative is above 0.2, then smooths observed variables.

## Exploratory Demonstration Conditions

Single-video demonstration:

| parameter | value |
| --- | --- |
| `window_size` | 5 |
| `eta_n` | -0.1 |
| `zeta_n` | -2 |
| `eta` | -1 |
| `zeta` | -0.1 |
| `sigma_q` | 0.7 |
| `sd_observable` | 0.1 |
| `dt` | 0.15 |
| `num_steps` | 300 |
| `num_observables` | 4 |
| emphasis | `k = 20` |

Large simulation / dynEGA demonstration:

| parameter | value |
| --- | --- |
| `n` videos | 30 |
| `eta_ns` | -0.2, -0.25, -0.3, -0.35 |
| `zeta_ns` | 1.4, 1.5, 1.6, 1.7 |
| `etas` | -0.2, -0.25, -0.3, -0.35 |
| `zetas` | -0.2, -0.3, -0.4, -0.5 |
| `sigma_qs` | 0.3, 0.4, 0.5, 0.6, 0.7 |
| per-video sampling | each video samples one value from each vector |
| dynEGA settings | `n.emdbed = 5`, `tau = 1`, `delta = 1`, `algorithm = "louvain"`, `level = c("individual", "population")` |
| dynEGA variants | `use.derivatives = 0` and `use.derivatives = 1` |

## Named Emotion-Dynamics Scenarios

Shared settings:

| parameter | value |
| --- | --- |
| `window_size` | 5 |
| `sd_observable` | 0.05 initially, changed to 0.1 in Scenario 2 and reused afterward |
| `dt` | 0.25 |
| `num_steps` | 300 |
| `num_observables` | 4 |

| scenario | script label | `eta_n` | `zeta_n` | `eta` | `zeta` | `sigma_q` | extra processing | saved plots |
| --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- |
| 1 | Neutral resistant politician / boring speech | -0.1 | -2 | -1 | -0.1 | 0.7 | smooth latent/derivative columns with moving average window 2 | `Figures/example-neutral-stable.png` |
| 2 | Mostly neutral with brief emotional episodes | -0.5 | -1 | -0.6 | -1 | 1.0 | `sd_observable = 0.1`; `emphasize(k = 100)` assigned to `sim_results_obs` but plots use smoothed `sim_results`; moving average window 5 | `Figures/example-neutral-volatile.png`, `Figures/example-neutral-volatile-pos.png` |
| 3 | Emotional with long, stable emotional episodes | -0.5 | -0.1 | -0.1 | -1 | 0.7 | uses current `sd_observable` value, i.e. 0.1 after Scenario 2; `emphasize(k = 100)` assigned to `sim_results_obs` but plots use smoothed `sim_results`; moving average window 5 | `Figures/example-emotional-stable.png`, `Figures/example-emotional-stable-pos.png`, `Figures/example-emotional-stable-neg.png` |
| 4 | Emotional with short, volatile emotional episodes | -1 | 1 | -0.75 | 1 | 0.7 | uses current `sd_observable` value, i.e. 0.1 after Scenario 2; `emphasize(k = 100)` assigned to `sim_results_obs` but plots use smoothed `sim_results`; moving average window 5 | `Figures/example-emotional-volatile.png`, `Figures/example-emotional-volatile-pos.png`, `Figures/example-emotional-volatile-neg.png` |

## Comparative/Diagnostic Outputs In The Script

- The script does not create saved comparative benchmark plots for DynEGA vs GraphicalVAR vs GIMME.
- It creates unsaved interactive/base-R plots:
  - one-video `plot_emotions(..., "latent")`, `"positive"`, `"negative"`;
  - one random video from `simulation_1`, again latent/positive/negative;
  - `plot(dynega.0)` for the first dynEGA run.
- It computes correlations between dynEGA network scores and latent scores:
  - without derivatives: `D1` vs `N`, `D2` vs `P`;
  - with derivatives: `D1` vs `dN`, `D2` vs `dP`, `D1` vs `dP`, `D2` vs `dN`.
- It computes `boot.ergoInfo()` for `dynega.0` and `dynega.1`, but does not save those outputs to files.
- It saves only the nine scenario-example PNG files listed in the scenario table, under a relative `Figures/` path.
