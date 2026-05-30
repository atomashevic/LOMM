#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)
source("extract.r")

score_cols <- c("anger", "fear", "happiness")
empirical <- data.frame(
  clip = rep(c("a", "b"), each = 5),
  frame_index = rep(seq_len(5), times = 2),
  truth_label = NA_character_,
  model = "empirical",
  anger = seq(0.01, 0.10, length.out = 10),
  fear = seq(0.02, 0.20, length.out = 10),
  happiness = seq(0.70, 0.30, length.out = 10)
)
synthetic <- empirical
synthetic$model <- "synthetic"
synthetic[score_cols] <- synthetic[score_cols] * 2

affine <- affine_iqr_normalize_panel(
  synthetic_panel = synthetic,
  empirical_panel = empirical,
  score_cols = score_cols,
  alpha = 1.0
)
stopifnot(all(dim(affine) == dim(synthetic)))
stopifnot(all(is.finite(as.matrix(affine[score_cols]))))
stopifnot(max(abs(vapply(affine[score_cols], stats::IQR, numeric(1)) -
                  vapply(empirical[score_cols], stats::IQR, numeric(1)))) < 1e-8)

quantile <- empirical_quantile_normalize_panel(
  synthetic_panel = synthetic,
  empirical_panel = empirical,
  score_cols = score_cols,
  alpha = 1.0
)
stopifnot(all(dim(quantile) == dim(synthetic)))
stopifnot(all(is.finite(as.matrix(quantile[score_cols]))))
stopifnot(abs(stats::median(quantile$anger) - stats::median(empirical$anger)) < 0.01)
stopifnot(abs(stats::IQR(quantile$anger) - stats::IQR(empirical$anger)) < 0.01)
stopifnot(min(quantile$anger) >= min(empirical$anger))
stopifnot(max(quantile$anger) <= max(empirical$anger))

blend <- empirical_quantile_normalize_panel(
  synthetic_panel = synthetic,
  empirical_panel = empirical,
  score_cols = score_cols,
  alpha = 0.5
)
stopifnot(all(blend$anger >= pmin(synthetic$anger, quantile$anger)))
stopifnot(all(blend$anger <= pmax(synthetic$anger, quantile$anger)))

message("LOMM OpenAI-FER score-normalization diagnostics tests passed.")
