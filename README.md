# LOMM

Code and materials for *A Latent Oscillator Measurement Model to Simulate
Emotional-Expression Score Dynamics in Video*.

LOMM simulates oscillator-based latent dynamics observed through multiple
fallible indicators. The paper evaluates recovery of prespecified dimensionality
and indicator assignments with DynEGA, static EGA, GraphicalVAR, and GIMME.

## Quick start

Requirements:

- R 4.5.1
- Quarto
- a LaTeX distribution
- Python 3.13 for simulation-job management

Clone the repository and install the packages needed for the example and paper
render:

```bash
git clone https://github.com/atomashevic/LOMM.git
cd LOMM
Rscript -e 'install.packages(c("digest", "dplyr", "jsonlite", "knitr", "readr", "tibble", "tidyr"))'
```

Run the LOMM example:

```bash
Rscript scripts/lomm/example_observation_profile.R
```

Render the manuscript and response letter from the checked-in figures and
tables:

```bash
quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex
```

See [REPRODUCIBILITY.md](REPRODUCIBILITY.md) for validation checks, full package
requirements, and simulation rerun instructions.

## Repository contents

- `paper/`: manuscript, response letter, figures, tables, and references
- `scripts/lomm/`: the public LOMM observation model and tests
- `scripts/simulations/`: simulation specifications and method runners
- `scripts/figures/`: figure and table builders

## Data

The analysis-ready tables and aggregate summaries underlying the reported
results are in `paper/tables/`. Raw MAFW videos and frame-level classifier
scores are not redistributed. Recomputing Study 1 from frame-level data
requires approved access to MAFW and local generation of the classifier scores.
