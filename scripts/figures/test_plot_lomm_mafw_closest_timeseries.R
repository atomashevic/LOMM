#!/usr/bin/env Rscript

lomm_mafw_test_stop <- function(message) stop(message, call. = FALSE)
lomm_mafw_expect <- function(value, message) {
  if (!isTRUE(value)) lomm_mafw_test_stop(message)
}
lomm_mafw_equal <- function(actual, expected, tolerance = 1e-10, message) {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    lomm_mafw_test_stop(message)
  }
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
environment <- new.env(parent = globalenv())
assign(
  "LOMM_MAFW_SCRIPT_PATH",
  file.path(root, "scripts", "figures", "plot_lomm_mafw_closest_timeseries.R"),
  envir = environment
)
sys.source(
  file.path(root, "scripts", "figures", "plot_lomm_mafw_closest_timeseries.R"),
  envir = environment
)

prepared <- environment$lomm_mafw_prepare()
ranked <- environment$lomm_mafw_rank_draws(prepared)
winner <- ranked$ranking[1L, , drop = FALSE]

lomm_mafw_expect(nrow(ranked$ranking) == 100L, "The ranking does not contain 100 draws.")
lomm_mafw_expect(identical(winner$draw, 26L), "The deterministic closest draw changed.")
lomm_mafw_equal(
  winner$trajectory_distance, 0.9257800, tolerance = 1e-6,
  message = "The closest-draw trajectory distance changed."
)
lomm_mafw_equal(
  winner$level_rmse, 0.1051453, tolerance = 1e-6,
  message = "The closest-draw level RMSE changed."
)
lomm_mafw_equal(
  winner$change_rmse, 0.0828666, tolerance = 1e-6,
  message = "The closest-draw change RMSE changed."
)
lomm_mafw_expect(
  all(diff(ranked$ranking$trajectory_distance) >= 0),
  "The ranking is not ordered by trajectory distance."
)
lomm_mafw_expect(
  identical(ranked$selected$panel$frame_key, prepared$empirical$frame_key),
  "The selected draw and empirical clip do not share frame keys."
)

emotions <- prepared$emotions
empirical <- as.matrix(prepared$empirical[, emotions, drop = FALSE])
synthetic <- ranked$selected$probability
lomm_mafw_expect(all(empirical > 0) && all(synthetic > 0), "A composition is nonpositive.")
lomm_mafw_expect(
  max(abs(rowSums(empirical) - 1)) < 1e-8 &&
    max(abs(rowSums(synthetic) - 1)) < 1e-10,
  "A composition does not close to one."
)

display <- environment$lomm_mafw_long_data(prepared, ranked)
lomm_mafw_expect(
  identical(
    display$highlighted,
    c("helplessness", "fear", "disgust", "sadness")
  ),
  "The empirically fixed highlighted emotions changed."
)
lomm_mafw_expect(
  nrow(display$data) == 2L * 49L * 11L,
  "The plot does not contain both complete 11-emotion series."
)

temporary <- tempfile("lomm-mafw-figure-test-")
dir.create(temporary)
on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
paths <- environment$lomm_mafw_paths()
paths$output_pdf <- file.path(temporary, "comparison.pdf")
paths$output_png <- file.path(temporary, "comparison.png")
figure <- environment$lomm_mafw_plot(prepared, ranked)
environment$lomm_mafw_atomic_figure(figure, paths$output_pdf, "pdf")
environment$lomm_mafw_atomic_figure(figure, paths$output_png, "png")
lomm_mafw_expect(file.info(paths$output_pdf)$size > 10000, "The PDF is unexpectedly small.")
lomm_mafw_expect(file.info(paths$output_png)$size > 100000, "The PNG is unexpectedly small.")

cat("LOMM--MAFW closest time-series figure tests: PASS\n")
