# Paper render

Paper render lets a user build the manuscript PDF and the response letter PDF from checked-in figures and tables without overwriting the live `paper/` outputs.

## Sub-features

- `render-manuscript` runs Quarto on `paper/Paper_emotion_dynamics_HGolino_RR.qmd` into an isolated output directory.
- `render-letter` runs `pdflatex` on `paper/response_letter.tex` into the same isolated directory.
- `render-isolate` leaves `git status -- paper` unchanged by this drive.

## How to get to it (user POV)

- Run `quarto render paper/Paper_emotion_dynamics_HGolino_RR.qmd --to pdf` from the repository root, as in README.md.
- Run `pdflatex -interaction=nonstopmode -halt-on-error -output-directory=paper paper/response_letter.tex`.

Verification does not use those live output paths. It copies `paper/` into scratch and writes PDFs there.

## Driving it with verify-lomm

Preconditions:

- `.cursor/skills/verify-lomm/scripts/verify-lomm doctor --paper-tools` reports `doctor: ok`.
- The working directory is the repository root.
- You will not write PDFs into the live `paper/` directory.

- **Isolated render.** Build both PDFs in scratch. Run `.cursor/skills/verify-lomm/scripts/verify-lomm drive paper-render`. Exit code `0`.
- **Manuscript PDF.** `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/paper-render/Paper_emotion_dynamics_HGolino_RR.pdf` exists and is non-empty.
- **Letter PDF.** `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/paper-render/response_letter.pdf` exists and is non-empty.
- **No live overwrite.** `git status -- paper` does not show new or modified files caused by this drive. The qmd has `keep-tex: true`, which is why the helper copies `paper/` before rendering.
- **Proof.** Open `.cursor/skills/verify-lomm/evidence/$VERIFY_LOMM_RUN_ID/paper-render/proof.json`. `ok` is `true`.

## Gotchas

- `quarto render` without `--output-dir` writes next to the qmd and dirties `paper/`. Never do that in a verification run.
- `keep-tex: true` can write `Paper_emotion_dynamics_HGolino_RR.tex` beside the source. The helper copies `paper/` into `/tmp/verify-lomm-$VERIFY_LOMM_RUN_ID/paper` first.
- Quarto `--output-dir` is input-relative if you pass a relative path. The helper passes an absolute scratch path.
- This feature does not rerun simulations. It only renders checked-in figures and tables.
- A full render can take several minutes. That is still the user path.
