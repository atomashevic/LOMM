# Reproduce the paper

Use Bash. Run every command from the repository root.
The example and PDF builds need no external data. Study 2 generates its own data.
Study 1 requires the original restricted MAFW score export.

## 1. Install

Install R 4.5.1, Python 3.13, Quarto, and a LaTeX distribution with
`xelatex` and `pdflatex`. Then run:

```bash
git clone https://github.com/atomashevic/LOMM.git
cd LOMM
Rscript -e 'install.packages(c("clue", "cowplot", "data.table", "digest", "dplyr", "EGAnet", "ggplot2", "gimme", "graphicalVAR", "igraph", "jpeg", "jsonlite", "knitr", "lavaan", "MASS", "patchwork", "psych", "purrr", "qgraph", "Rcpp", "readr", "scales", "stringr", "tibble", "tidyr"), repos="https://cloud.r-project.org")'
```

Recorded analysis versions: R 4.5.1, EGAnet 2.3.0, graphicalVAR 0.3.4,
gimme 10.0, igraph 2.2.1, clue 0.3-66, and jsonlite 2.0.0.
The installation command above installs current CRAN releases; it does not pin
these versions. Each simulation run records its package versions.

## 2. Run the example

```bash
Rscript scripts/lomm/example_observation_profile.R
Rscript scripts/lomm/test_observation_profile.R
Rscript scripts/simulations/test_common_scores.R
Rscript scripts/simulations/test_graphicalvar_centering_covariance.R
```

The example prints simulated scores. Each test prints a success message.

## 3. Build the PDFs

These commands use the supplied figures and tables. They replace the two PDFs
in `paper/`.

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

## 4. Rerun Study 2

Run the jobs below in order. They run in the foreground, use at most four
workers, and write to `simulation_outputs/revision_v2/`.
The full simulation grids are computationally expensive.

```bash
set -euo pipefail
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export REVISION_JOB_WORKERS=4

for job in t100_reuse_audit t100_dynega t100_static_oracle \
  t100_graphicalvar_fallback t100_gimme_fallback \
  t25_dynega t25_graphicalvar t25_gimme t25_static_oracle \
  challenge_dynega challenge_graphicalvar challenge_static_oracle
do
  Rscript scripts/simulations/run_revision_job.R --job "$job"
done

Rscript scripts/simulations/run_revision_job.R --job aggregate_results --study2-only
aggregate_dir=$(Rscript scripts/simulations/run_revision_job.R --job aggregate_results --study2-only --print-output-dir)
Rscript scripts/figures/build_revision_v2_artifacts.R \
  --aggregate-dir "$aggregate_dir" --output-root paper --study2-only
Rscript scripts/simulations/audit_finite_horizon_amplification.R
```

The reuse audit accepts absent historical files; the two fallback jobs generate
fresh GraphicalVAR and GIMME results. Aggregation verifies the completed inputs.
The final commands replace the Study 2 figures and tables in `paper/`.
Run section 3 again to update the PDFs.

To continue an interrupted job, use its name and add `--resume`:
`Rscript scripts/simulations/run_revision_job.R --job t100_dynega --resume`.
Fresh runs already compute corrected trajectory diagnostics.
Floating-point values and runtimes can vary with numerical libraries and hardware.
In the retained correlated-residual runs, exact panel hashes matched with four
OpenBLAS threads at R startup and failed with one. Fresh runs use one BLAS thread
per worker. To reconstruct those historical panels, use one worker with
`OPENBLAS_NUM_THREADS=4 OMP_NUM_THREADS=4`, set before starting R.

## 5. Rerun Study 1

Set `MAFW_FRAME_SCORES` to the original 100-clip score CSV.
The preprocessing script verifies its SHA-256:
`a6416f69f2090770f9fba5b13359883483ec4c689a34046a1c6c121c5843b67a`.
This file is not distributed in the repository.

```bash
set -euo pipefail
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export MAFW_FRAME_SCORES=/absolute/path/to/frame_scores.csv

MAFW_COMPOSITIONAL_BUNDLE=$(Rscript scripts/preprocessing/build_empirical_compositional_views.R \
  --input "$MAFW_FRAME_SCORES" | tail -n 1)
export MAFW_COMPOSITIONAL_BUNDLE

Rscript scripts/lomm_1_1/build_mafw_plausibility_validation_25v25.R
Rscript scripts/lomm_1_1/build_mafw_plausibility_validation_25v25_extended.R
Rscript scripts/lomm_1_1/build_mafw_clip_level_benchmark.R
Rscript scripts/figures/build_mafw_study1_figures.R
```

Intermediate results go to `simulation_outputs/lomm_1_1/`.
The last command replaces the Study 1 benchmark figures and tables in `paper/`.

MAFW access is governed by the [dataset provider](https://mafw-database.github.io/MAFW/).
Raw videos and frame-level scores are not redistributed. The original scoring
package and model-checkpoint versions were not archived, so newly scored videos
cannot be assumed to reproduce the original export. The supplied aggregate
tables and figures remain usable without that export.
