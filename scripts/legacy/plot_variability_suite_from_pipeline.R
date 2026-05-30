#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("plot_variability_suite_from_pipeline.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "extract.r"))

read_char_env <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  value
}

input_dir <- read_char_env(
  "PIPELINE_INPUT_DIR",
  file.path(repo_root, "empirical_outputs_smoke", "mafw-sample", "oai-large-100")
)
compare_mode <- read_char_env("PIPELINE_COMPARE_MODE", "heterogeneous")
output_prefix <- read_char_env("PIPELINE_OUTPUT_PREFIX", compare_mode)
output_dir <- read_char_env("PIPELINE_OUTPUT_DIR", input_dir)
include_jsd <- tolower(read_char_env("PIPELINE_INCLUDE_JSD", "1")) %in% c("1", "true", "yes")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

empirical_panel_path <- file.path(input_dir, "empirical_panel_clean.csv")
synthetic_panel_path <- file.path(input_dir, sprintf("%s_synthetic_panel.csv", compare_mode))
mode_summary_path <- file.path(input_dir, "mode_summary.csv")
run_summary_path <- file.path(input_dir, "run_summary.csv")

stopifnot(file.exists(empirical_panel_path))
stopifnot(file.exists(synthetic_panel_path))
stopifnot(file.exists(mode_summary_path))
stopifnot(file.exists(run_summary_path))

empirical_panel <- utils::read.csv(empirical_panel_path, stringsAsFactors = FALSE)
synthetic_panel <- utils::read.csv(synthetic_panel_path, stringsAsFactors = FALSE)
mode_summary <- utils::read.csv(mode_summary_path, stringsAsFactors = FALSE)
run_summary <- utils::read.csv(run_summary_path, stringsAsFactors = FALSE)

mode_row <- mode_summary[mode_summary$mode == compare_mode, , drop = FALSE]
if (nrow(mode_row) == 0L) {
  stop(sprintf("Mode '%s' not found in %s", compare_mode, mode_summary_path), call. = FALSE)
}
empirical_n_dim <- run_summary$empirical_n_dim[[1]]

score_cols <- EMPIRICAL_EMOTION_COLUMNS
model_name <- basename(normalizePath(input_dir, mustWork = TRUE))
synthetic_label <- if (compare_mode == "heterogeneous") {
  "heterogeneous DLO"
} else {
  sprintf("%s DLO", compare_mode)
}

permutation_entropy <- function(x, m = 3L) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < m + 1L) {
    return(NA_real_)
  }
  patterns <- vapply(seq_len(n - m + 1L), function(i) {
    paste(order(x[i:(i + m - 1L)]), collapse = ",")
  }, character(1))
  freq <- table(patterns) / length(patterns)
  -sum(freq * log2(freq)) / log2(factorial(m))
}

dfa_exponent <- function(x, min_box = 4L, max_box = NULL) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 16L) {
    return(NA_real_)
  }
  if (is.null(max_box)) {
    max_box <- floor(n / 4)
  }
  y <- cumsum(x - mean(x))
  box_sizes <- unique(as.integer(2^seq(log2(min_box), log2(max_box), length.out = 12)))
  box_sizes <- box_sizes[box_sizes >= min_box & box_sizes <= max_box]
  if (length(box_sizes) < 3L) {
    return(NA_real_)
  }

  fluct <- vapply(box_sizes, function(bs) {
    n_boxes <- floor(n / bs)
    if (n_boxes < 1L) {
      return(NA_real_)
    }
    resid_sq <- numeric(n_boxes)
    for (b in seq_len(n_boxes)) {
      idx <- ((b - 1L) * bs + 1L):(b * bs)
      segment <- y[idx]
      fit <- lm.fit(cbind(1, seq_along(segment)), segment)
      resid_sq[b] <- mean(fit$residuals^2)
    }
    sqrt(mean(resid_sq))
  }, numeric(1))

  valid <- is.finite(fluct) & fluct > 0
  if (sum(valid) < 3L) {
    return(NA_real_)
  }
  coef(lm(log(fluct[valid]) ~ log(box_sizes[valid])))[2]
}

spectral_entropy <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 8L) {
    return(NA_real_)
  }
  spec <- Mod(fft(x - mean(x)))^2
  spec <- spec[2:(floor(n / 2) + 1)]
  spec <- spec / sum(spec)
  spec <- spec[spec > 0]
  -sum(spec * log2(spec)) / log2(length(spec))
}

jsd <- function(p, q) {
  p <- pmax(p, 1e-12)
  q <- pmax(q, 1e-12)
  m <- 0.5 * (p + q)
  0.5 * sum(p * log2(p / m)) + 0.5 * sum(q * log2(q / m))
}

jsd_rate <- function(mat) {
  n <- nrow(mat)
  if (n < 2L) {
    return(NA_real_)
  }
  jsds <- vapply(seq.int(2L, n), function(i) {
    jsd(mat[i - 1L, ], mat[i, ])
  }, numeric(1))
  mean(jsds, na.rm = TRUE)
}

compute_clip_metrics <- function(clip_df, score_cols) {
  mat <- as.matrix(clip_df[, score_cols, drop = FALSE])
  n <- nrow(mat)
  if (n < 8L) {
    return(NULL)
  }

  rows <- list()
  rid <- 1L
  for (col in score_cols) {
    x <- mat[, col]
    se <- compute_sample_entropy(x)
    xc <- x - mean(x, na.rm = TRUE)
    zcr <- sum(diff(sign(xc)) != 0, na.rm = TRUE) / max(1L, length(xc) - 1L)
    ac <- tryCatch(
      acf(x, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2, 1, 1],
      error = function(e) NA_real_
    )
    pe <- permutation_entropy(x)
    dfa <- dfa_exponent(x)
    spe <- spectral_entropy(x)

    rows[[rid]] <- data.frame(
      channel = col,
      sample_entropy = se,
      zero_crossing_rate = zcr,
      lag1_acf = ac,
      permutation_entropy = pe,
      dfa_exponent = dfa,
      spectral_entropy = spe,
      stringsAsFactors = FALSE
    )
    rid <- rid + 1L
  }

  out <- dplyr::bind_rows(rows)
  if (include_jsd) {
    out$jsd_rate <- jsd_rate(mat)
  }
  out
}

compute_panel_metrics <- function(panel, score_cols, label) {
  clips <- split(panel, panel$clip)
  out <- lapply(clips, function(cl) {
    res <- compute_clip_metrics(cl, score_cols)
    if (!is.null(res)) {
      res$clip <- cl$clip[1]
    }
    res
  })
  out <- out[!vapply(out, is.null, logical(1))]
  out <- dplyr::bind_rows(out)
  out$dataset <- label
  out
}

cat("Computing empirical metrics...\n")
emp <- compute_panel_metrics(empirical_panel, score_cols, "empirical")
cat("Computing synthetic metrics...\n")
syn <- compute_panel_metrics(synthetic_panel, score_cols, "synthetic")

all_data <- dplyr::bind_rows(emp, syn)

metric_names <- c(
  "sample_entropy",
  "permutation_entropy",
  "spectral_entropy",
  "zero_crossing_rate",
  "lag1_acf",
  "dfa_exponent"
)
metric_labels <- c(
  sample_entropy = "Sample Entropy",
  permutation_entropy = "Permutation Entropy",
  spectral_entropy = "Spectral Entropy",
  zero_crossing_rate = "Zero-Crossing Rate",
  lag1_acf = "Lag-1 ACF",
  dfa_exponent = "DFA Exponent"
)

if (include_jsd) {
  metric_names <- c(metric_names, "jsd_rate")
  metric_labels <- c(metric_labels, jsd_rate = "JSD Rate")
}

long <- tidyr::pivot_longer(
  all_data,
  cols = dplyr::all_of(metric_names),
  names_to = "metric",
  values_to = "value"
)
long <- long[is.finite(long$value), , drop = FALSE]
long$metric <- factor(long$metric, levels = metric_names, labels = metric_labels[metric_names])
long$dataset <- factor(long$dataset, levels = c("empirical", "synthetic"))

dataset_palette <- c(empirical = "#3b5bdb", synthetic = "#c2610c")
facet_ncol <- if (length(metric_names) > 6L) 4L else 3L

subtitle <- sprintf(
  "eta=%s, zeta=%s, sigma_q=%.2f, dt=%.1f | n_dim: emp=%d, syn=%d",
  mode_row$eta_spec[[1]],
  mode_row$zeta_spec[[1]],
  mode_row$sigma_q[[1]],
  mode_row$dt[[1]],
  empirical_n_dim,
  mode_row$synthetic_n_dim[[1]]
)

gg <- ggplot2::ggplot(long, ggplot2::aes(x = dataset, y = value, fill = dataset)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.35, color = NA) +
  ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.85) +
  ggplot2::facet_wrap(~metric, scales = "free_y", ncol = facet_ncol) +
  ggplot2::scale_fill_manual(values = dataset_palette) +
  ggplot2::labs(
    title = sprintf("Variability suite: empirical %s vs %s", model_name, synthetic_label),
    subtitle = subtitle,
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_rect(fill = "#f3f4f6", color = "#d1d5db"),
    axis.text.x = ggplot2::element_text(size = 10)
  )

plot_path <- file.path(output_dir, sprintf("%s_variability_suite.png", output_prefix))
metrics_path <- file.path(output_dir, sprintf("%s_new_metrics_raw.csv", output_prefix))

ggplot2::ggsave(plot_path, gg, width = if (facet_ncol == 4L) 14 else 12, height = 8, dpi = 170)
utils::write.csv(long[, c("metric", "value", "dataset")], metrics_path, row.names = FALSE)

summary_tbl <- long |>
  dplyr::group_by(metric, dataset) |>
  dplyr::summarise(
    median = round(median(value, na.rm = TRUE), 4),
    q25 = round(quantile(value, 0.25, na.rm = TRUE), 4),
    q75 = round(quantile(value, 0.75, na.rm = TRUE), 4),
    .groups = "drop"
  )

summary_path <- file.path(output_dir, sprintf("%s_variability_suite_summary.csv", output_prefix))
utils::write.csv(summary_tbl, summary_path, row.names = FALSE)

cat("Saved:", plot_path, "\n")
cat("Saved:", metrics_path, "\n")
cat("Saved:", summary_path, "\n")
empirical_n_dim <- run_summary$empirical_n_dim[[1]]
