# Reproducing The LOMM Paper Materials

Run all commands from the repository root. This document explains what is
checked in, what external derived data are needed, and how to rebuild the
simulation summaries, figures, tables, manuscript PDF, and response letter.

## What Is In This Repository

The repository includes the manuscript source, rendered paper materials, final
simulation runners, and scripts that rebuild the paper-facing figures and
tables.

- `paper/`: manuscript source, response letter, bibliography, figures, tables,
  and rendered PDFs.
- `scripts/lomm/`: the public LOMM observation model, default configuration,
  and focused contract tests.
- `scripts/extract.r`: historical core functions and estimator wrappers used by
  completed simulation packages.
- `scripts/simulations/`: final method-comparison runners and summary builders.
- `scripts/figures/`: figure and table builders for the manuscript.
- `simulation_outputs/`: ignored local output directory used by reruns.

The checked-in manuscript can be rendered from the current files under
`paper/figures/` and `paper/tables/`. A full rerun writes intermediate and final
outputs under the ignored `simulation_outputs/` directory.

Run the small public observation example without launching any simulation grid:

```bash
Rscript scripts/lomm/example_observation_profile.R
```

The script constructs an ILR basis, latent trajectories, a loading matrix, and
an empirical between-clip covariance, then calls `lomm_observe()` and verifies
that the returned score rows lie on the simplex.

## MAFW-Derived Inputs

The final method-comparison simulations can be rerun from the code in this
repository. The empirical realism figure and Figure 3 use derived files from
MAFW, a third-party dataset. The raw videos are not included here.

### 1. Request MAFW access

Use the official MAFW access page: <https://mafw-database.github.io/MAFW/>.
Download the EULA, review the terms, complete the required information, obtain
the required signature, and email the signed form to the contact listed by the
MAFW maintainers. After approval, use the provided Baidu Netdisk or Google Drive
links to download the dataset. The MAFW site states that the dataset is for
non-commercial research use and restricts copying, distribution, and commercial
use of the dataset materials. Check the EULA before sharing derived files.

### 2. Prepare local MAFW files

Store the approved MAFW files outside the Git repository, for example:

```text
/path/to/mafw/clips/
/path/to/mafw/labels/
/path/to/mafw/frames/
```

The MAFW FAQ states that the Google Drive option includes video clips and label
files but not frame data. If you use that download, extract frames from the
clips and run face detection/alignment locally. The FAQ recommends aligned
224 x 224 face frames for consistency with the dataset benchmark format.

### 3. Create the frame-score CSV

Score the selected MAFW frames with `transforEmotion` or another FER scoring
pipeline. The empirical helper in `scripts/extract.r` expects one row per frame
with these columns:

```text
clip,frame_index,truth_label,anger,disgust,fear,happiness,neutral,sadness,
surprise,contempt,anxiety,helplessness,disappointment
```

For Figure 3, `scripts/figures/plot_oai_clip_video_dynega_figure.R` also uses a
`model` column and defaults to `model == "oai-large"`. The manuscript version
used `oai-large`, which maps in the manuscript text to OpenAI CLIP
ViT-Large/14.

### 4. Supply the manuscript realism file

The manuscript figure script reads this derived metric file:

```text
simulation_outputs/empirical_link_options/oai-large-100/metric_comparison_long.csv
```

The Git repository ignores `simulation_outputs/`, so this file must be placed
locally before the figure script is run. If you rebuild the MAFW scoring and
calibration upstream, place the rebuilt file at the same path before running:

```bash
Rscript scripts/figures/plot_empirical_realism_results.R
```

The expected `metric_comparison_long.csv` columns are:

```text
family,metric,empirical_median,synthetic_median,empirical_iqr,
median_diff_scaled,wasserstein_scaled,metric_loss,weight,max_discrepancy,
paper_call,candidate_id,stage,candidate_label,link,temperature,intercept_mode,
candidate_rank,paper_promising
```

### 5. Regenerate the Figure 3 empirical example

Set the frame-score CSV and image directory before running the Figure 3 script:

```bash
export OAI_CLIP_FIGURE_FRAME_SCORES=/path/to/mafw-derived/frame_scores.csv
export OAI_CLIP_FIGURE_FRAME_IMAGE_DIR=/path/to/mafw-derived/selected_frames
export OAI_CLIP_FIGURE_CLIP=02875.mp4
export OAI_CLIP_FIGURE_MODEL=oai-large

Rscript scripts/figures/plot_oai_clip_video_dynega_figure.R
```

The script writes intermediate outputs under
`simulation_outputs/video_dynega_example/` and refreshes
`paper/figures/mafw_video_dynega_example.pdf`.

## Software

The workflow uses R, Quarto, XeLaTeX, Python 3, and tmux for long runs. The main
R packages used by the retained scripts are `EGAnet`, `dplyr`, `tidyr`, `purrr`,
`MASS`, `ggplot2`, `readr`, `stringr`, `scales`, `patchwork`, `igraph`, `clue`,
`graphicalVAR`, `gimme`, `cowplot`, `jpeg`, `digest`, and `jsonlite`.

The final simulation launchers limit BLAS-style thread pools. For direct
foreground runs, set these variables first if the machine uses multithreaded
linear algebra:

```bash
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
```

## Quick Integrity Check

Validate the public bounded-score B+S default, the empirical, loading-aligned,
and none baseline modes (the `loading_aligned` mode draws clip baselines inside
the space spanned by the loadings), exact homogeneous nesting, and the retained
internal hashes:

```bash
Rscript scripts/lomm/test_observation_profile.R
```

The check always validates tracked adoption receipts under
`scripts/lomm/evidence/`. It also validates the full realism and bridge outputs
when their ignored `simulation_outputs/` artifacts are available locally.

Load the historical core implementation without running the expensive
extraction pipeline:

```bash
Rscript -e "options(run_extract_pipeline=FALSE); source('scripts/extract.r')"
```

## Dry-Run Grid Checks

### Revision V2 prospective jobs

The unified revision interface validates all prospective grids, manifests,
dependencies, and commands without fitting DynEGA, GraphicalVAR, GIMME, or
static EGA:

```bash
export REVISION_EMPIRICAL_FRAME_SCORES=/absolute/path/to/frame_scores.csv
python3 scripts/simulations/orchestrate_revision_jobs.py plan
python3 scripts/simulations/orchestrate_revision_jobs.py validate
python3 scripts/simulations/orchestrate_revision_jobs.py status
python3 scripts/simulations/orchestrate_revision_jobs.py next
python3 scripts/simulations/orchestrate_revision_jobs.py print-launch --job t100_dynega
```

The full validation binds the empirical frame-score file before deriving job
hashes. If the file is unavailable, `validate --structural-only` checks the job
graph and static contracts without producing launch-ready dry-run manifests.

The revision grids contain 21,600 canonical panels for each matched
`T = 100` and `T = 25` study and 4,200 generated conditions in the challenge
suite. Dry-run artifacts are stored under
`simulation_outputs/revision_v2/<job-id>/<config-hash>/`. Future launch and
resume commands require `--execute` together with the literal acknowledgement
`LONG-RUNS-AUTHORIZED`. The orchestrator does not progress automatically to a
dependent job. Full details are recorded in `REVISION_V2_HANDOFF.md`.

The structural simulations retain physical `dt = 0.4` for trajectory
generation and horizon metadata. DynEGA uses `delta = 1` observation interval
in EGAnet's GLLA implementation. Prospective manifests and result rows record
these values separately; changing either value changes the corresponding job
configuration or generator hash.

Revision-v2 is a preserved historical evidence surface. Its generators retain
their original versioned observation behavior: the broad and matched recovery
panels remain homogeneous linear, while the softmax-realism and bounded-
challenge surfaces remain their historical variants. The public LOMM default
is `compositional_bs`; applying it to a future full benchmark requires a new
job/configuration version and output namespace.

### Checked-in final-result runners

These commands validate the final grids and write only small grid/manifest files
to a temporary output tree:

```bash
FINAL_DYNEGA_DRY_RUN=1 \
FINAL_DYNEGA_OUT_DIR=simulation_outputs/repro_dry_run/dynega_full \
Rscript scripts/simulations/run_final_dynega_full.R

FINAL_DYNEGA_CD_DRY_RUN=1 \
FINAL_DYNEGA_CD_OUT_DIR=simulation_outputs/repro_dry_run/dynega_bounded_nvar4_cd \
Rscript scripts/simulations/run_final_dynega_bounded_cd.R

FINAL_GRAPHICALVAR_BOUNDED_DRY_RUN=1 \
FINAL_GRAPHICALVAR_BOUNDED_OUT_DIR=simulation_outputs/repro_dry_run/graphicalvar_bounded_nvar4 \
Rscript scripts/simulations/run_final_graphicalvar_bounded.R

FINAL_GIMME_BOUNDED_DRY_RUN=1 \
FINAL_GIMME_BOUNDED_OUT_DIR=simulation_outputs/repro_dry_run/gimme_bounded_nvar4 \
Rscript scripts/simulations/run_final_gimme_bounded.R
```

Expected dry-run grid sizes are:

- DynEGA full grid: 1,296 structural cells, 400 replications, 518,400 primary
  method rows.
- DynEGA bounded CD grid: 21,600 structure fits, Leiden and Walktrap readouts,
  43,200 method rows.
- GraphicalVAR bounded grid: 21,600 structure fits, Louvain/Leiden/Walktrap
  readouts, 64,800 method rows.
- GIMME bounded grid: 21,600 structure fits, Louvain/Leiden/Walktrap readouts,
  64,800 method rows.

## Checked-In Full Simulation Rerun

The tmux launchers are the preferred entrypoints for full reruns because the
final grids are long-running jobs with checkpoint/resume behavior:

```bash
bash scripts/simulations/launch_final_dynega_tmux.sh
bash scripts/simulations/launch_final_dynega_bounded_cd_tmux.sh
bash scripts/simulations/launch_final_graphicalvar_bounded_tmux.sh
bash scripts/simulations/launch_final_gimme_bounded_tmux.sh
```

Default outputs are:

- `simulation_outputs/final_simulation_results/dynega_full/`
- `simulation_outputs/final_simulation_results/dynega_bounded_nvar4_cd/`
- `simulation_outputs/final_simulation_results/graphicalvar_bounded_nvar4/`
- `simulation_outputs/final_simulation_results/gimme_bounded_nvar4/`

The main adjustable environment variables are:

- `FINAL_DYNEGA_WORKERS`, `FINAL_DYNEGA_REPS`, `FINAL_DYNEGA_OUT_DIR`
- `FINAL_DYNEGA_CD_WORKERS`, `FINAL_DYNEGA_CD_REPS`,
  `FINAL_DYNEGA_CD_ALGORITHMS`, `FINAL_DYNEGA_CD_OUT_DIR`
- `FINAL_GRAPHICALVAR_BOUNDED_WORKERS`,
  `FINAL_GRAPHICALVAR_BOUNDED_REPS`,
  `FINAL_GRAPHICALVAR_BOUNDED_TIMEOUT_SEC`,
  `FINAL_GRAPHICALVAR_BOUNDED_OUT_DIR`
- `FINAL_GIMME_BOUNDED_WORKERS`, `FINAL_GIMME_BOUNDED_REPS`,
  `FINAL_GIMME_BOUNDED_TIMEOUT_SEC`, `FINAL_GIMME_BOUNDED_STOP_CRIT`,
  `FINAL_GIMME_BOUNDED_OUT_DIR`

The manuscript values use the default replication counts: 400 for the full
DynEGA grid and 100 for the bounded comparator grids.

## Build Final Summaries, Figures, And Tables

After the four final result directories exist, build the comparison summaries
and paper-facing assets:

```bash
Rscript scripts/simulations/compare_final_results_to_manuscript.R
Rscript scripts/figures/plot_empirical_realism_results.R
Rscript scripts/figures/plot_method_recovery_results.R
Rscript scripts/figures/plot_factor_effects_results.R
Rscript scripts/figures/plot_runtime_feasibility_results.R
```

`plot_empirical_realism_results.R` requires the derived empirical input:

```text
simulation_outputs/empirical_link_options/oai-large-100/metric_comparison_long.csv
```

Researchers with approved MAFW access can rebuild upstream frame scores and
calibration outputs as described in `MAFW-Derived Inputs` above. The retained
empirical helper in `scripts/extract.r` can be called from R as:

```r
options(run_extract_pipeline = FALSE)
source("scripts/extract.r")
run_empirical_dlo_realism_pipeline(
  frame_scores_path = "/path/to/mafw-derived/frame_scores.csv",
  model_name = "oai-large",
  output_dir = "simulation_outputs/empirical_link_options/oai-large-100"
)
```

That helper writes `final_metric_comparison.csv`; the manuscript figure script
uses the paper-facing `metric_comparison_long.csv` file described above.

Figure 3 is checked in as `paper/figures/mafw_video_dynega_example.pdf`. To
regenerate it, supply the derived frame-score CSV and selected video frames used
by `scripts/figures/plot_oai_clip_video_dynega_figure.R`.

## Render The Manuscript Package

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```
