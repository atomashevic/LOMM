# Public observation example

The public observation example lets a user generate a small bounded-composition probability table from a documented LOMM observation call and confirm the public profile metadata.

## Sub-features

- `example-run` runs the README example from the repository root.
- `example-probabilities` prints a 4-column probability table whose rows sum to 1.
- `example-metadata` prints `public_profile`, `baseline_mode`, and `observation_surface`.

## How to get to it (user POV)

- Run `Rscript scripts/lomm/example_observation_profile.R` from the repository root, as in README.md.

## Driving it with verify-lomm

Preconditions:

- Doctor reports `doctor: ok`.
- The working directory is the repository root.

- **Run example.** Generate the worked example. Run `.cursor/skills/verify-lomm/scripts/verify-lomm drive example-observation-profile`. Exit code `0`. Stdout contains a table with columns `anger`, `fear`, `happiness`, and `neutral`.
- **Confirm surface.** Read the printed metadata. Stdout contains `public_profile` `compositional_bs`, `baseline_mode` `empirical`, and `observation_surface` `bounded_composition`.
- **Proof.** Open `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/example-observation-profile/proof.json`. `ok` is `true` and `missing_tokens` is empty.

## Gotchas

- `source("scripts/lomm/observation_profile.R")` fails unless the working directory is the repository root.
- The example writes no files under `paper/` or `simulation_outputs/`. A dirty git tree from those paths is not this feature.
- Do not treat a later test PASS line as proof of the example. Drive this feature on its own.
