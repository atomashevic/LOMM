#!/usr/bin/env Rscript

score_expect <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

score_equal <- function(actual, expected, message, tolerance = 1e-10) {
  score_expect(
    isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE)),
    message
  )
}

score_expect_error <- function(expr, message) {
  result <- tryCatch({ force(expr); NULL }, error = identity)
  score_expect(inherits(result, "error"), message)
}

argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
source_path <- file.path(dirname(script_path), "run_method_simulation.R")

# Load function definitions without sourcing the simulation driver or extraction pipeline.
implementation <- new.env(parent = globalenv())
for (expression in parse(source_path)) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.call(expression[[3L]]) &&
      identical(expression[[3L]][[1L]], as.name("function"))) {
    eval(expression, envir = implementation)
  }
}
build_scores <- implementation$build_common_scores
alignment <- implementation$compute_score_nrmse_alignment
score_expect(requireNamespace("clue", quietly = TRUE), "The clue package is required.")

time <- seq_len(100L)
latent_one <- sin(2 * pi * time / 100)
latent_two <- cos(4 * pi * time / 100)
panel <- data.frame(
  Y1 = 2 + 3 * latent_one,
  Y2 = -5 + 2 * latent_one,
  Y3 = 7 + 4 * latent_two,
  Y4 = -1 + latent_two,
  id = 1L
)
items <- paste0("Y", seq_len(4L))
membership <- setNames(c(1L, 1L, 2L, 2L), items)
reference <- build_scores(panel, items, membership)
score_equal(dim(reference), c(100L, 2L), "A two-group partition must produce two scores.")
score_equal(
  abs(stats::cor(reference[, 1L], latent_one)), 1,
  "The first group must summarize its original indicator levels."
)
score_equal(
  abs(stats::cor(reference[, 2L], latent_two)), 1,
  "The second group must summarize its original indicator levels."
)

for (suffix in c(".Ord0", ".Ord1")) {
  named <- setNames(membership, paste0(items, suffix))
  named <- named[c(4L, 2L, 1L, 3L)]
  actual <- build_scores(panel, items, named)
  score_equal(dim(actual), dim(reference), "Suffix normalization must preserve score count.")
  score_equal(
    abs(diag(stats::cor(actual, reference))), rep(1, 2L),
    "Shuffled derivative names must recover the original groups up to PC sign."
  )
}

singleton <- build_scores(panel, items, setNames(c(1L, 2L, 2L, 2L), items))
score_equal(
  singleton[, 1L], as.numeric(scale(panel$Y1)),
  "A singleton group must retain its standardized original indicator."
)

invalid_memberships <- list(
  empty = integer(),
  unnamed = unname(membership),
  partial = membership[-1L],
  extra = c(membership, Y5 = 3L),
  duplicate_original = setNames(membership, c("Y1", "Y1", "Y3", "Y4")),
  duplicate_canonical = setNames(membership, c("Y1", "Y1.Ord1", "Y3", "Y4")),
  missing_label = setNames(c(1L, NA_integer_, 2L, 2L), items),
  blank_label = setNames(c("1", "", "2", "2"), items),
  missing_name = setNames(membership, c(NA_character_, "Y2", "Y3", "Y4")),
  unsupported_suffix = setNames(membership, paste0(items, ".Ord2"))
)
for (label in names(invalid_memberships)) {
  score_expect_error(
    build_scores(panel, items, invalid_memberships[[label]]),
    paste("Invalid partition must fail instead of changing groups:", label)
  )
}

# Independent series with complete, common support obey the stated sample-SD identity.
x <- latent_one
y <- 3 * latent_one + 4 * latent_two
expected <- sqrt(2 * 99 / 100 * (1 - abs(stats::cor(x, y))))
for (orientation in c(-1, 1)) {
  actual <- alignment(matrix(orientation * y), list(matrix(x)), rep(1L, 100L))
  score_equal(actual, expected, "NRMSE must select the smaller sign-invariant error.")
  score_expect(actual <= sqrt(198 / 100) + 1e-12, "Complete-series NRMSE exceeds its bound.")
}
score_equal(
  alignment(matrix(latent_two), list(matrix(x)), rep(1L, 100L)),
  sqrt(198 / 100), "Orthogonal complete series must attain the NRMSE bound."
)

latents <- list(
  cbind(latent_one, latent_two),
  cbind(5 + 2 * latent_one, -3 + 4 * latent_two)
)
recovered <- rbind(
  cbind(-latent_two, latent_one),
  cbind(20 - 3 * latents[[2L]][, 1L], -9 + 5 * latents[[2L]][, 2L])
)
ids <- rep(seq_len(2L), each = 100L)
score_equal(
  alignment(recovered, latents, ids), 0,
  "Per-clip scaling, sign selection, and one-to-one matching must recover permuted trajectories."
)
score_equal(
  alignment(matrix(rep(1, 100L)), list(matrix(x)), rep(1L, 100L)), 2,
  "An undefined constant-series comparison must retain the penalty of 2."
)
score_equal(
  alignment(matrix(x), list(cbind(x, latent_two)), rep(1L, 100L)), 1,
  "One missing recovered dimension must contribute one penalty of 2 before averaging."
)

cat("Common-score partition and trajectory regression tests passed.\n")
