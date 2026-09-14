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

## Manuscript editions

The existing `paper/Paper_emotion_dynamics_HGolino_RR.qmd`/TEX/PDF files are
preserved as the integral baseline. The submission editions are:

- `paper/manuscript-arxiv.qmd`: complete paper, including Appendices A–C,
  in a single-spaced reading layout.
- `paper/manuscript-brm.qmd`: double-spaced main paper with a title page,
  upper-right page numbers, and references to the separate supplement.
- `paper/supplement-brm.qmd`: Appendices A–C with their own reference list;
  equation numbering continues from the main paper.

Each QMD generates a same-named TEX and PDF. Edit QMD, then render; TEX is a
build output. The editions share `paper/figures/`, `paper/tables/`, and
`paper/references.bib`. Apply future scientific corrections consistently to
the arXiv edition and the corresponding BRM main/supplement text.

```bash
quarto render paper/manuscript-arxiv.qmd --to pdf
quarto render paper/manuscript-brm.qmd --to pdf
quarto render paper/supplement-brm.qmd --to pdf
```

Submission ZIP packages are generated locally and excluded from Git.

`paper/manuscript-arxiv-source.zip` contains the generated TEX and its 11
referenced figures. References and title formatting are embedded in the TEX;
compilation needs neither Quarto nor R. Extract the archive and run
`pdflatex -interaction=nonstopmode -halt-on-error manuscript-arxiv.tex` twice
with a current TeX Live installation (verified with TeX Live 2025). The system
TeX Live 2018 is too old for the generated citation commands. Regenerate the
archive after changes to the arXiv TEX or its figures.

`paper/manuscript-brm-source.zip` contains the generated main/supplement TEX
and their required figure files. The data/code statements retain the existing GitHub URL and
MAFW access restrictions; no repository deposit or submission is implied.

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
