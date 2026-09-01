# Job graph inspect

Job graph inspect lets a user print the revision-v2 job graph, structurally validate the manifest, and see the guarded launch command without starting a fit or a tmux session.

## Sub-features

- `graph-plan` prints the immutable job graph, including `empirical_cv`.
- `graph-validate` structurally validates 16 jobs without dispatcher dry runs.
- `graph-print-launch` prints the authorized invocation for one job and does not execute it.

## How to get to it (user POV)

- Run `python3 scripts/simulations/orchestrate_revision_jobs.py plan` from the repository root.
- Run `python3 scripts/simulations/orchestrate_revision_jobs.py validate --structural-only`.
- Run `python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job JOB_ID`.

## Driving it with verify-lomm

Preconditions:

- Doctor reports `doctor: ok`.
- The working directory is the repository root.
- You will not pass `--execute` or call `launch` / `resume`.

- **Plan.** Print the job graph. Run `.cursor/skills/verify-lomm/scripts/verify-lomm drive job-graph-inspect`. The first command is `plan --json`. Stdout contains `empirical_cv`.
- **Validate.** The second command is `validate --structural-only`. Stdout contains `Manifest valid: 16 jobs; max workers 24; max heavy jobs 1.`
- **Print launch.** The third command is `print-launch --job t25_static_oracle`. Stdout contains `authorized invocation:` and `LONG-RUNS-AUTHORIZED`. No tmux session is created. `simulation_outputs/` is not written by this drive.
- **Proof.** Open `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/job-graph-inspect/proof.json`. `ok` is `true`. Confirm the captured `print-launch` command has no `--execute`.

## Gotchas

- `validate` without `--structural-only` runs dispatcher dry runs and can touch job output dirs. Do not use that mode here.
- `status` without `--no-record` appends history. Do not call `status` in this feature.
- `print-launch` showing an authorized invocation is not permission to run it. The helper refuses `launch`, `resume`, and `--execute`.
- Full grids are expensive. Inspecting the graph is not rerunning Study 2.
