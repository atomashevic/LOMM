#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

repo_root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "paper", "Paper_emotion_dynamics_HGolino_RR.qmd"))) {
  stop("Run this script from the repository root.")
}

comparison_dir <- file.path(
  repo_root,
  "simulation_outputs/final_simulation_results/comparison_to_current_manuscript"
)
rows_path <- file.path(comparison_dir, "final_cd_rows.rds")
structure_status_path <- file.path(comparison_dir, "final_structure_fit_status_accounting.csv")
summary_path <- file.path(comparison_dir, "final_cd_summary.csv")
figure_dir <- file.path(repo_root, "paper", "figures")
table_dir <- file.path(repo_root, "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(rows_path) || !file.exists(structure_status_path) || !file.exists(summary_path)) {
  stop(
    "Missing final CD comparison outputs. Run ",
    "`Rscript scripts/simulations/compare_final_results_to_manuscript.R` first.",
    call. = FALSE
  )
}

method_levels <- c("DynEGA", "GraphicalVAR", "GIMME")
method_colors <- c(
  DynEGA = "#0072B2",
  GraphicalVAR = "#E69F00",
  GIMME = "#CC79A7"
)
method_shapes <- c(DynEGA = 16, GraphicalVAR = 17, GIMME = 15)

fmt_runtime <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "not run", ifelse(x < 10, sprintf("%.2f", x), sprintf("%.1f", x)))
}

fmt_ratio <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "not run", paste0(sprintf("%.1f", x), "x"))
}

rows <- readRDS(rows_path) %>%
  mutate(Method = factor(as.character(Method), levels = method_levels))

summary_df <- read_csv(summary_path, show_col_types = FALSE) %>%
  mutate(Method = factor(as.character(Method), levels = method_levels))

runtime_summary <- rows %>%
  filter(status == "ok") %>%
  group_by(Method) %>%
  summarise(
    median_sec = median(runtime_sec, na.rm = TRUE),
    q25_sec = quantile(runtime_sec, 0.25, na.rm = TRUE),
    q75_sec = quantile(runtime_sec, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Method)

dynega_louvain_runtime <- runtime_summary %>%
  filter(Method == "DynEGA") %>%
  pull(median_sec)
if (length(dynega_louvain_runtime) != 1L || !is.finite(dynega_louvain_runtime)) {
  dynega_louvain_runtime <- NA_real_
}

runtime_table <- runtime_summary %>%
  mutate(
    `Runtime/DynEGA` = median_sec / dynega_louvain_runtime
  ) %>%
  transmute(
    Method = as.character(Method),
    `Median runtime (s)` = fmt_runtime(median_sec),
    `IQR low (s)` = fmt_runtime(q25_sec),
    `IQR high (s)` = fmt_runtime(q75_sec),
    `Runtime/DynEGA` = fmt_ratio(`Runtime/DynEGA`)
  )
write_csv(runtime_table, file.path(table_dir, "table3_runtime_feasibility.csv"))

status_table <- read_csv(structure_status_path, show_col_types = FALSE) %>%
  mutate(
    Method = as.character(Method),
    `Feasibility note` = case_when(
      Method == "DynEGA" & Scope == "Full DynEGA grid" ~ "Full expanded DynEGA Louvain/expand grid completed.",
      Method == "DynEGA" ~ "Bounded comparator completed across CD readouts.",
      Method == "GraphicalVAR" ~ "Bounded NVAR = 4 structure fits completed except OAI 3D timeouts; each fit was partitioned with Louvain, Leiden, and Walktrap.",
      Method == "GIMME" ~ "Bounded NVAR = 4 structure fits include substantial timeouts and post-estimation errors; completed fits were partitioned with Louvain, Leiden, and Walktrap.",
      TRUE ~ ""
    )
  ) %>%
  transmute(
    Method,
    Scope,
    `Structure fits` = structure_fits,
    ok,
    timeout,
    error,
    `Feasibility note`
  )
write_csv(status_table, file.path(table_dir, "table3_runtime_feasibility_accounting.csv"))

runtime_labels <- c(
  `1` = "1 s",
  `3` = "3 s",
  `10` = "10 s",
  `30` = "30 s",
  `60` = "1 min",
  `180` = "3 min",
  `600` = "10 min",
  `900` = "15 min"
)

figure7 <- ggplot(
  runtime_summary,
  aes(
    x = Method,
    y = median_sec,
    color = Method,
    shape = Method,
    group = Method
  )
) +
  geom_errorbar(
    aes(ymin = q25_sec, ymax = q75_sec),
    width = 0.10,
    linewidth = 0.45,
    alpha = 0.80,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 2.55, stroke = 0.75, position = position_dodge(width = 0.55)) +
  scale_color_manual(values = method_colors, guide = "none") +
  scale_shape_manual(values = method_shapes, guide = "none") +
  scale_y_log10(
    breaks = as.numeric(names(runtime_labels)),
    labels = unname(runtime_labels),
    limits = c(1, 1000)
  ) +
  labs(
    x = NULL,
    y = "Median successful-fit runtime [IQR], log10 scale"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom"
  )

pdf_path <- file.path(figure_dir, "figure7_runtime_feasibility.pdf")
png_path <- file.path(figure_dir, "figure7_runtime_feasibility.png")

ggsave(pdf_path, figure7, width = 7.2, height = 3.2, device = cairo_pdf)
ggsave(png_path, figure7, width = 7.2, height = 3.2, dpi = 450, bg = "white")

message("Wrote ", pdf_path)
message("Wrote ", png_path)
message("Wrote ", file.path(table_dir, "table3_runtime_feasibility.csv"))
message("Wrote ", file.path(table_dir, "table3_runtime_feasibility_accounting.csv"))
