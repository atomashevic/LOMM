---
name: verify-lomm
description: Drive the LOMM paper repository from the documented CLI. Run the public observation example and tests, inspect the revision job graph without launching fits, and render the manuscript into an isolated directory. Use when proving a change to scripts/lomm, scripts/simulations, paper/, README.md, or REPRODUCIBILITY.md, or when the user asks to verify LOMM, render the paper, or inspect simulation jobs.
---

# Verify LOMM

LOMM is a short-lived CLI, not a server. A user runs R scripts and a Python orchestrator from the repository root. There is no web UI.

Read [features/README.md](features/README.md) before driving. Drive every listed entry point for the feature you claim. One convenient command is not coverage when the map lists others.

Do not run `scripts/extract.r` as a live pipeline. Do not call orchestrator `launch` or `resume`. Do not pass `--execute`.

## Launch

There is no process to keep alive. Launch means doctor plus a run id.

```bash
cd "$(git rev-parse --show-toplevel)"
export VERIFY_LOMM_RUN_ID="${VERIFY_LOMM_RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)-$$}"
.cursor/skills/verify-lomm/scripts/verify-lomm doctor
```

Ready when stdout ends with `doctor: ok`. Teardown is `verify-lomm cleanup` after the last drive.

Each drive is its own process. Example, tests, and job-graph inspect can run side by side. Paper render copies `paper/` into `/tmp/verify-lomm-$VERIFY_LOMM_RUN_ID/paper`. Never render into the live `paper/` directory.

## Doctor

Run this first, and again after any failed drive.

```bash
.cursor/skills/verify-lomm/scripts/verify-lomm doctor
```

For paper render:

```bash
.cursor/skills/verify-lomm/scripts/verify-lomm doctor --paper-tools
```

Doctor is read-only. It checks the repo root, `Rscript`, `python3`, and the README quickstart R packages. `--paper-tools` also requires `quarto` and `pdflatex`. Refuse to drive when it prints `doctor: fail`.

## Drive

Use the helper. Do not reconstruct README commands by hand.

```bash
.cursor/skills/verify-lomm/scripts/verify-lomm drive FEATURE
.cursor/skills/verify-lomm/scripts/verify-lomm features
```

Feature ids are `example-observation-profile`, `public-observation-tests`, `graphicalvar-centering`, `job-graph-inspect`, and `paper-render`. Recipes live in [features/](features/).

Capture the command, stdout, stderr, and exit code. For paper render, also copy the isolated PDFs. A pass line in stdout is not enough if `proof.json` has `ok: false`.

Job-graph inspect uses `plan --json`, `validate --structural-only`, and `print-launch`. It must not create a tmux session or write under `simulation_outputs/`. If `print-launch` stdout contains `authorized invocation:`, that is the command you must not run.

## Evidence

Write proof under `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/FEATURE/`. That directory is gitignored. Cleanup must leave it in place.

Each drive writes numbered `*.command.txt`, `*.stdout.txt`, `*.stderr.txt`, `*.exit_code.txt`, and `proof.json`. Paper render also copies `Paper_emotion_dynamics_HGolino_RR.pdf` and `response_letter.pdf` from the scratch output dir.

Proof standard:

- Exercise the documented user command, not an internal setter.
- Keep the action transcript and the resulting state.
- For tests, the PASS line and exit code 0.
- For the example, stdout contains `bounded_composition` and a 4-column probability table.
- For job-graph inspect, stdout lists the 16 jobs and an authorized invocation you did not execute. Confirm `simulation_outputs/` was not created or changed by the drive.
- For paper render, the PDFs exist in the evidence dir. `git status -- paper` must not gain render outputs from this run.

## Cleanup

```bash
.cursor/skills/verify-lomm/scripts/verify-lomm cleanup
```

This deletes `/tmp/verify-lomm-$VERIFY_LOMM_RUN_ID` only. It never deletes evidence. There is no daemon to kill. If a drive is still running, wait for that pid or kill that pid from the drive's process, not by process name.

## Helpers

`.cursor/skills/verify-lomm/scripts/verify-lomm` is executable. Run it from the repo root, or pass `--repo PATH`.

```bash
.cursor/skills/verify-lomm/scripts/verify-lomm doctor
.cursor/skills/verify-lomm/scripts/verify-lomm drive example-observation-profile
.cursor/skills/verify-lomm/scripts/verify-lomm cleanup
```

Optional env: `VERIFY_LOMM_RUN_ID`, `VERIFY_LOMM_SCRATCH`.
