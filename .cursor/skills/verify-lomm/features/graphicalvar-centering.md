# GraphicalVAR centering test

The GraphicalVAR centering test lets a user check the finite-clip centered covariance identities used by the simulation method, without fitting GraphicalVAR.

## Sub-features

- `centering-run` executes `scripts/simulations/test_graphicalvar_centering_covariance.R`.
- `centering-pass` prints `GraphicalVAR finite-clip centering covariance tests: PASS`.

## How to get to it (user POV)

- Run `Rscript scripts/simulations/test_graphicalvar_centering_covariance.R` from the repository root, as in REPRODUCIBILITY.md.

## Driving it with verify-lomm

Preconditions:

- Doctor reports `doctor: ok`.
- The working directory is the repository root.

- **Run test.** Execute the centering test. Run `.cursor/skills/verify-lomm/scripts/verify-lomm drive graphicalvar-centering`. Exit code `0`.
- **Read PASS.** Stdout contains `GraphicalVAR finite-clip centering covariance tests: PASS`.
- **Proof.** Open `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/graphicalvar-centering/proof.json`. `ok` is `true`.

## Gotchas

- This test does not launch `run_revision_job.R` or write under `simulation_outputs/`.
- Doctor requires `MASS`. A missing package fails doctor before this drive starts.
- Do not treat DynEGA or GIMME simulation output as proof of this identity test.
