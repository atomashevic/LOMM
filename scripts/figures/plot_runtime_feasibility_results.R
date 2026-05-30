#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

repo_root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"))) {
  stop("Run this script from the repository root.")
}

source_dir <- file.path(repo_root, "simulation_outputs", "method_comparison")
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

runtime_path <- file.path(source_dir, "figure4_method_runtime_common_base_summary.csv")
speedup_path <- file.path(source_dir, "figure4_method_runtime_speedup.csv")
dynega_results_path <- file.path(repo_root, "simulation_outputs", "emotion_dynega", "benchmark_results.csv")
graphicalvar_results_path <- file.path(repo_root, "simulation_outputs", "emotion_graphicalvar", "checkpoint_results.csv")
gimme_results_path <- file.path(repo_root, "simulation_outputs", "emotion_gimme", "benchmark_results.csv")

missing_inputs <- c(
  runtime_path,
  speedup_path,
  dynega_results_path,
  graphicalvar_results_path,
  gimme_results_path
)[
  !file.exists(c(
    runtime_path,
    speedup_path,
    dynega_results_path,
    graphicalvar_results_path,
    gimme_results_path
  ))
]
if (length(missing_inputs) > 0L) {
  stop("Missing required runtime inputs: ", paste(missing_inputs, collapse = ", "))
}

method_levels <- c("DynEGA", "GraphicalVAR", "GIMME")
method_colors <- c(
  DynEGA = "#0072B2",
  GraphicalVAR = "#E69F00",
  GIMME = "#CC79A7"
)
method_shapes <- c(
  DynEGA = 16,
  GraphicalVAR = 17,
  GIMME = 15
)
dimension_levels <- c("2D", "3D", "4D")

fmt_runtime <- function(x) {
  x <- as.numeric(x)
  ifelse(
    is.na(x),
    "not run",
    ifelse(x < 10, sprintf("%.2f", x), sprintf("%.1f", x))
  )
}

fmt_ratio <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "not run", paste0(sprintf("%.1f", x), "x"))
}

runtime <- read_csv(runtime_path, show_col_types = FALSE) |>
  mutate(
    method_group = factor(method_group, levels = method_levels),
    true_dim_label = factor(paste0(true_dim_label, "D"), levels = dimension_levels)
  )

speedup <- read_csv(speedup_path, show_col_types = FALSE) |>
  mutate(true_dim_label = factor(paste0(true_dim_label, "D"), levels = dimension_levels))

table3 <- speedup |>
  transmute(
    Dimensionality = as.character(true_dim_label),
    `DynEGA median (s)` = fmt_runtime(DynEGA),
    `GraphicalVAR median (s)` = fmt_runtime(GraphicalVAR),
    `GIMME median (s)` = fmt_runtime(GIMME),
    `GraphicalVAR/DynEGA` = fmt_ratio(graphicalvar_vs_dynega),
    `GIMME/DynEGA` = fmt_ratio(gimme_vs_dynega)
  )
write_csv(table3, file.path(table_dir, "table3_runtime_feasibility.csv"))

target_rows <- nrow(read_csv(dynega_results_path, show_col_types = FALSE))
status_counts <- bind_rows(
  read_csv(dynega_results_path, show_col_types = FALSE) |>
    count(status, name = "rows") |>
    mutate(method_group = "DynEGA"),
  read_csv(graphicalvar_results_path, show_col_types = FALSE) |>
    count(status, name = "rows") |>
    mutate(method_group = "GraphicalVAR"),
  read_csv(gimme_results_path, show_col_types = FALSE) |>
    count(status, name = "rows") |>
    mutate(method_group = "GIMME")
) |>
  mutate(status = if_else(status == "ok", "ok", status)) |>
  complete(
    method_group = method_levels,
    status = c("ok", "timeout", "error"),
    fill = list(rows = 0L)
  ) |>
  group_by(method_group) |>
  mutate(`not run` = target_rows - sum(rows)) |>
  ungroup() |>
  select(method_group, status, rows, `not run`) |>
  pivot_wider(names_from = status, values_from = rows) |>
  distinct() |>
  pivot_longer(
    cols = c(ok, timeout, error, `not run`),
    names_to = "status",
    values_to = "rows"
  ) |>
  mutate(
    method_group = factor(method_group, levels = rev(method_levels)),
    status = factor(status, levels = c("ok", "timeout", "error", "not run"))
  )

status_table <- status_counts |>
  mutate(method_group = as.character(method_group), status = as.character(status)) |>
  group_by(method_group, status) |>
  summarise(rows = sum(rows, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = status, values_from = rows, values_fill = 0) |>
  mutate(
    Method = method_group,
    `Planned method rows` = target_rows,
    `Not run` = `not run`,
    `Feasibility note` = case_when(
      Method == "DynEGA" ~ "Full grid completed.",
      Method == "GraphicalVAR" ~ "Stopped after repeated 900-s timeouts in the 4D ranked-persistence base condition; T = 300 and k = 6 sensitivity blocks were not attempted.",
      Method == "GIMME" ~ "Staged ladder stopped after OAI calibration scenarios returned unusable person-level Psi matrices; 4D, N = 100, T = 300, and k = 6 extensions were not attempted.",
      TRUE ~ ""
    )
  ) |>
  select(Method, `Planned method rows`, ok, timeout, error, `Not run`, `Feasibility note`)
write_csv(status_table, file.path(table_dir, "table3_runtime_feasibility_accounting.csv"))

runtime_labels <- c(
  `0.5` = "0.5 s",
  `1` = "1 s",
  `3` = "3 s",
  `10` = "10 s",
  `30` = "30 s",
  `60` = "1 min",
  `180` = "3 min",
  `600` = "10 min"
)

not_run_label <- data.frame(
  true_dim_label = factor("4D", levels = dimension_levels),
  median_sec = 520,
  label = "GIMME\nnot run"
)

figure7 <- ggplot(
  runtime,
  aes(
    x = true_dim_label,
    y = median_sec,
    color = method_group,
    shape = method_group,
    group = method_group
  )
) +
  geom_errorbar(
    aes(ymin = q25_sec, ymax = q75_sec),
    width = 0.08,
    linewidth = 0.45,
    alpha = 0.80,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(size = 2.45, stroke = 0.75, position = position_dodge(width = 0.45)) +
  geom_text(
    data = not_run_label,
    aes(x = true_dim_label, y = median_sec, label = label),
    inherit.aes = FALSE,
    color = "grey35",
    size = 2.7,
    lineheight = 0.88
  ) +
  scale_color_manual(values = method_colors, name = "Method") +
  scale_shape_manual(values = method_shapes, name = "Method") +
  scale_y_log10(
    breaks = as.numeric(names(runtime_labels)),
    labels = unname(runtime_labels),
    limits = c(0.45, 800)
  ) +
  labs(
    x = "True dimensionality",
    y = "Common base-grid time, median [IQR]",
    title = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

pdf_path <- file.path(figure_dir, "figure7_runtime_feasibility.pdf")
png_path <- file.path(figure_dir, "figure7_runtime_feasibility.png")

ggsave(pdf_path, figure7, width = 7.2, height = 3.0, device = cairo_pdf)
ggsave(png_path, figure7, width = 7.2, height = 3.0, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "table3_runtime_feasibility.csv"))
message("Wrote ", file.path(table_dir, "table3_runtime_feasibility_accounting.csv"))
