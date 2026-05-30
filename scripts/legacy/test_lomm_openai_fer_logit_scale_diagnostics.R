#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)
source("extract.r")

score_cols <- c("anger", "fear", "happiness")
panel <- data.frame(
  clip = rep("a", 5),
  frame_index = seq_len(5),
  truth_label = NA_character_,
  model = "test",
  anger = c(0, 0.001, 0.5, 0.999, 1),
  fear = seq(0.1, 0.9, length.out = 5),
  happiness = c(0.25, 0.3, 0.35, 0.4, 0.45),
  stringsAsFactors = FALSE
)

z <- logit_transform_score_panel(panel, score_cols = score_cols, eps = 1e-4)

stopifnot(all(dim(z) == dim(panel)))
stopifnot(all(is.finite(as.matrix(z[score_cols]))))
stopifnot(z$anger[[1]] == stats::qlogis(1e-4))
stopifnot(z$anger[[5]] == stats::qlogis(1 - 1e-4))
stopifnot(abs(z$anger[[3]]) < 1e-12)
stopifnot(identical(z$clip, panel$clip))
stopifnot(identical(z$frame_index, panel$frame_index))

message("LOMM OpenAI-FER logit-scale diagnostics tests passed.")
