# LOMM verification map

This directory is the maintained source for verifying user-facing LOMM CLI behavior. Read the index before driving, then use the matching feature file as the recipe.

## Baseline preconditions

- Work from the repository root. `scripts/lomm/observation_profile.R` and `REPRODUCIBILITY.md` must exist.
- Export `VERIFY_LOMM_RUN_ID` for the run.
- Run `.cursor/skills/verify-lomm/scripts/verify-lomm doctor` and require `doctor: ok`.
- Do not start `scripts/extract.r` with the empirical pipeline enabled.
- Do not call `orchestrate_revision_jobs.py launch` or `resume`, and do not pass `--execute`.
- Never render into the live `paper/` directory.

## Driving conventions

- Start every recipe from the repository root unless a feature copies `paper/` into scratch.
- Treat every command as literal. Keep quoted names and flags unchanged.
- Run all actions through `.cursor/skills/verify-lomm/scripts/verify-lomm drive FEATURE`.
- Keep proof artifacts. Cleanup removes scratch only.

## Proof and skip reporting

- Capture the user command and the resulting stdout, stderr, and exit code.
- CLI proof includes the command, stdout, stderr, and exit code.
- Mutation proof includes a second view of the written file. Paper render checks the isolated PDF and `git status -- paper`.
- Record the feature ID with every artifact.
- Report an unreachable path with the attempted command and the unmet precondition.
- Do not report a skipped entry point as verified through a different path.

## Feature entry contract

Each feature file starts with an H1 title and one paragraph describing the user-visible behavior. It then uses exactly four H2 sections in this order.

1. `Sub-features` lists short IDs with one line for each behavior.
2. `How to get to it (user POV)` lists every user entry point.
3. `Driving it with verify-lomm` starts with `Preconditions:` and uses labeled bullets that pair each user action with an exact command and observable result.
4. `Gotchas` lists traps that can waste or invalidate a verification run.

Keep implementation details out of the map. Name only user paths, stable handles, required state, commands, and observable proof.

## Features

- [Public observation example](./example-observation-profile.md) runs the README worked example and prints bounded probabilities.
- [Public observation tests](./public-observation-tests.md) runs the public observation-profile test file.
- [GraphicalVAR centering test](./graphicalvar-centering.md) runs the finite-clip centering covariance test.
- [Job graph inspect](./job-graph-inspect.md) prints and structurally validates the revision job graph without launching fits.
- [Paper render](./paper-render.md) renders the manuscript and response letter into an isolated directory.
