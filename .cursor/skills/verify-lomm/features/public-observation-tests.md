# Public observation tests

The public observation tests let a user run the checked-in observation-profile test file and see a single PASS line for the public LOMM wrapper.

## Sub-features

- `tests-run` executes `scripts/lomm/test_observation_profile.R`.
- `tests-pass` prints `Public LOMM observation-profile tests: PASS`.
- `tests-hashes` prints whether public adoption-evidence hashes passed or were skipped.

## How to get to it (user POV)

- Run `Rscript scripts/lomm/test_observation_profile.R` from the repository root, as in REPRODUCIBILITY.md.

## Driving it with verify-lomm

Preconditions:

- Doctor reports `doctor: ok`.
- The working directory is the repository root.

- **Run tests.** Execute the public test file. Run `.cursor/skills/verify-lomm/scripts/verify-lomm drive public-observation-tests`. Exit code `0`.
- **Read PASS.** Stdout contains `Public LOMM observation-profile tests: PASS`.
- **Hash line.** Stdout contains either `Public adoption-evidence hashes: PASS` or `Public adoption-evidence hashes: SKIP`.
- **Proof.** Open `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/public-observation-tests/proof.json`. `ok` is `true`.

## Gotchas

- The test locates itself with `--file=`. Invoke it as `Rscript scripts/lomm/test_observation_profile.R`, not by `source()`.
- A SKIP on adoption-evidence hashes is allowed. A missing PASS line is not.
- These tests are not the GraphicalVAR centering test. Drive that feature separately.
