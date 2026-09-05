#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

repo_root <- normalizePath(".", mustWork = TRUE)

source(file.path(repo_root, "scripts", "extract.r"))

library(cowplot)
library(dplyr)
library(ggplot2)
library(grid)
library(igraph)
library(jpeg)
library(tidyr)

read_char <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) default else value
}

read_num <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) return(default)
  as.numeric(value)
}

read_int_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(as.integer(default))
  }
  as.integer(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
}

read_label_vec <- function(name, default) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  trimws(strsplit(value, "|", fixed = TRUE)[[1]])
}

read_bool <- function(name, default) {
  value <- tolower(trimws(Sys.getenv(name, unset = "")))
  if (!nzchar(value)) {
    return(default)
  }
  value %in% c("1", "true", "yes", "y")
}

sha256_file <- function(path) {
  hash_line <- tryCatch(
    system2("sha256sum", path, stdout = TRUE, stderr = FALSE),
    error = function(...) character()
  )
  if (length(hash_line) == 0L || !nzchar(hash_line[[1]])) {
    return(NA_character_)
  }
  strsplit(hash_line[[1]], "[[:space:]]+")[[1]][[1]]
}

clip_id <- read_char("OAI_CLIP_FIGURE_CLIP", "02875.mp4")
model_name <- read_char("OAI_CLIP_FIGURE_MODEL", "oai-large")
model_id <- if (identical(model_name, "oai-large")) {
  "openai/clip-vit-large-patch14"
} else {
  model_name
}
model_description <- if (identical(model_name, "oai-large")) {
  "OpenAI CLIP ViT-Large/14"
} else {
  model_name
}
frame_scores_path <- read_char(
  "OAI_CLIP_FIGURE_FRAME_SCORES",
  ""
)
output_dir <- read_char(
  "OAI_CLIP_FIGURE_OUTPUT_DIR",
  file.path(repo_root, "simulation_outputs", "video_dynega_example")
)
manuscript_pdf_path <- read_char("OAI_CLIP_FIGURE_MANUSCRIPT_PDF", "")
n_embed <- as.integer(read_num("OAI_CLIP_FIGURE_N_EMBED", 3))
edge_threshold <- read_num("OAI_CLIP_FIGURE_EDGE_THRESHOLD", 0.08)
smooth_window <- as.integer(read_num("OAI_CLIP_FIGURE_SMOOTH_WINDOW", 7))
top_emotion_count <- as.integer(read_num("OAI_CLIP_FIGURE_TOP_N", 4))
include_frame_stills <- read_bool("OAI_CLIP_FIGURE_INCLUDE_STILLS", TRUE)
figure_width <- read_num("OAI_CLIP_FIGURE_WIDTH", 10.8)
figure_height <- read_num("OAI_CLIP_FIGURE_HEIGHT", if (include_frame_stills) 7.84 else 5.9)
figure_dpi <- as.integer(read_num("OAI_CLIP_FIGURE_DPI", 300))
selected_frame_indices <- read_int_vec("OAI_CLIP_FIGURE_FRAMES", c(7L, 16L, 19L, 23L))
selected_frame_labels <- read_label_vec(
  "OAI_CLIP_FIGURE_FRAME_LABELS",
  c("Score profile 1", "Score profile 2", "Score profile 3", "Score profile 4")
)
frame_image_dir <- read_char(
  "OAI_CLIP_FIGURE_FRAME_IMAGE_DIR",
  ""
)

emotion_cols <- EMPIRICAL_EMOTION_COLUMNS
dynega_cols <- setdiff(emotion_cols, "neutral")
emotion_labels <- c(
  anger = "Anger",
  disgust = "Disgust",
  fear = "Fear",
  happiness = "Happiness",
  neutral = "Neutral",
  sadness = "Sadness",
  surprise = "Surprise",
  contempt = "Contempt",
  anxiety = "Anxiety",
  helplessness = "Helplessness",
  disappointment = "Disappointment"
)
emotion_colors <- c(
  anger = "#E41A1C",
  disgust = "#8C2D04",
  fear = "#FB6A4A",
  happiness = "#41B6C4",
  neutral = "#9E9E9E",
  sadness = "#9ECAE1",
  surprise = "#756BB1",
  contempt = "#A65628",
  anxiety = "#F1C40F",
  helplessness = "#377EB8",
  disappointment = "#253494"
)
community_colors <- c("#D73027", "#4575B4", "#1A9850", "#984EA3", "#FF7F00")
top_palette <- c("#0072B2", "#D55E00", "#CC79A7", "#009E73")
base_family <- "sans"

smooth_series <- function(x, window) {
  x <- as.numeric(x)
  window <- as.integer(window)
  if (!is.finite(window) || window <= 1L || length(x) <= 2L) {
    return(x)
  }
  if (window %% 2L == 0L) {
    window <- window + 1L
  }
  half_width <- floor(window / 2L)
  vapply(seq_along(x), function(idx) {
    lo <- max(1L, idx - half_width)
    hi <- min(length(x), idx + half_width)
    mean(x[lo:hi], na.rm = TRUE)
  }, numeric(1))
}

if (!nzchar(frame_scores_path) || !file.exists(frame_scores_path)) {
  stop(
    "Set OAI_CLIP_FIGURE_FRAME_SCORES to the derived frame-score CSV.",
    call. = FALSE
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw_scores <- utils::read.csv(frame_scores_path, stringsAsFactors = FALSE)
processing_summary_path <- file.path(dirname(frame_scores_path), "processing_summary.csv")
processing_summary <- if (file.exists(processing_summary_path)) {
  utils::read.csv(processing_summary_path, stringsAsFactors = FALSE)
} else {
  NULL
}
required_cols <- c("clip", "frame_index", "truth_label", "model", emotion_cols)
missing_cols <- setdiff(required_cols, names(raw_scores))
if (length(missing_cols) > 0L) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

clip_scores <- raw_scores %>%
  filter(.data$clip == clip_id, .data$model == model_name) %>%
  arrange(.data$frame_index)

if (nrow(clip_scores) == 0L) {
  stop("No rows found for clip ", clip_id, " and model ", model_name, call. = FALSE)
}

clip_scores <- clip_scores[complete.cases(clip_scores[, emotion_cols, drop = FALSE]), , drop = FALSE]
if (nrow(clip_scores) < 15L) {
  stop("Selected clip has too few complete frames after filtering: ", nrow(clip_scores), call. = FALSE)
}
clip_summary <- if (!is.null(processing_summary)) {
  processing_summary[
    processing_summary$clip == clip_id & processing_summary$model == model_name,
    ,
    drop = FALSE
  ]
} else {
  data.frame()
}
clip_n_frames <- if (nrow(clip_summary) > 0L && "n_frames" %in% names(clip_summary)) {
  clip_summary$n_frames[[1]]
} else {
  nrow(clip_scores)
}

clip_scores[, emotion_cols] <- renormalize_probability_rows(clip_scores, emotion_cols)[, emotion_cols]
truth_label <- unique(clip_scores$truth_label)[[1]]
clip_stem <- tools::file_path_sans_ext(clip_id)
emotion_means <- sort(colMeans(clip_scores[, emotion_cols, drop = FALSE]), decreasing = TRUE)
top_emotions <- names(emotion_means)[seq_len(min(top_emotion_count, length(emotion_means)))]
display_colors <- setNames(rep("#B8B8B8", length(emotion_cols)), emotion_cols)
display_colors[top_emotions] <- rep(top_palette, length.out = length(top_emotions))

if (length(selected_frame_indices) > 0L) {
  selected_positions <- match(selected_frame_indices, clip_scores$frame_index)
  if (any(is.na(selected_positions))) {
    missing_requested <- selected_frame_indices[is.na(selected_positions)]
    stop("Requested selected frames are absent after filtering: ", paste(missing_requested, collapse = ", "), call. = FALSE)
  }
} else {
  selected_positions <- unique(pmax(1L, pmin(
    nrow(clip_scores),
    round(seq(1, nrow(clip_scores), length.out = 4L))
  )))
  while (length(selected_positions) < 4L) {
    selected_positions <- unique(c(selected_positions, nrow(clip_scores)))
  }
}
selected_positions <- selected_positions[seq_len(4L)]
selected_frames <- clip_scores[selected_positions, , drop = FALSE]
selected_frames$profile_label <- selected_frame_labels[
  seq_len(min(length(selected_frame_labels), nrow(selected_frames)))
]
selected_frames$profile_label[is.na(selected_frames$profile_label)] <- ""

bar_y_max <- max(0.65, max(as.matrix(selected_frames[, emotion_cols, drop = FALSE]), na.rm = TRUE) * 1.06)
bar_y_breaks <- pretty(c(0, bar_y_max), n = 4)
bar_y_breaks <- bar_y_breaks[bar_y_breaks >= 0 & bar_y_breaks <= bar_y_max]

selected_frames$still_path <- file.path(
  frame_image_dir,
  sprintf("%s-frame-%d.jpg", clip_stem, selected_frames$frame_index)
)
if (include_frame_stills) {
  if (!nzchar(frame_image_dir) || !dir.exists(frame_image_dir)) {
    stop(
      "Set OAI_CLIP_FIGURE_FRAME_IMAGE_DIR to the selected-frame directory.",
      call. = FALSE
    )
  }
  missing_stills <- selected_frames$still_path[!file.exists(selected_frames$still_path)]
  if (length(missing_stills) > 0L) {
    stop(
      "Missing selected frame stills: ",
      paste(missing_stills, collapse = ", "),
      call. = FALSE
    )
  }
}

dynega_panel <- clip_scores[, c("clip", "frame_index", "truth_label", "model", dynega_cols), drop = FALSE]
dynega_fit <- fit_pooled_ord0_dynega(
  panel = dynega_panel,
  score_cols = dynega_cols,
  n_embed = n_embed,
  tau = 1L,
  algorithm = "louvain"
)

network_mat <- dynega_fit$score_obj$network
network_names <- gsub("\\.Ord0$", "", rownames(network_mat))
rownames(network_mat) <- network_names
colnames(network_mat) <- network_names
network_mat <- network_mat[dynega_cols, dynega_cols, drop = FALSE]

edge_index <- which(upper.tri(network_mat) & abs(network_mat) > 0, arr.ind = TRUE)
edge_table <- data.frame(
  from = rownames(network_mat)[edge_index[, "row"]],
  to = colnames(network_mat)[edge_index[, "col"]],
  weight = network_mat[edge_index],
  stringsAsFactors = FALSE
) %>%
  mutate(
    abs_weight = abs(.data$weight),
    sign = if_else(.data$weight >= 0, "positive", "negative")
  ) %>%
  arrange(desc(.data$abs_weight))

membership_table <- data.frame(
  emotion = names(dynega_fit$community_map),
  label = emotion_labels[names(dynega_fit$community_map)],
  community = as.integer(dynega_fit$community_map),
  stringsAsFactors = FALSE
)

utils::write.csv(
  clip_scores[, c("clip", "frame_index", "truth_label", "model", emotion_cols)],
  file.path(output_dir, paste0(clip_stem, "_selected_clip_frame_scores.csv")),
  row.names = FALSE
)
utils::write.csv(
  edge_table,
  file.path(output_dir, paste0(clip_stem, "_dynega_edges.csv")),
  row.names = FALSE
)
utils::write.csv(
  membership_table,
  file.path(output_dir, paste0(clip_stem, "_dynega_membership.csv")),
  row.names = FALSE
)
transforemotion_version <- tryCatch(
  as.character(utils::packageVersion("transforEmotion")),
  error = function(...) NA_character_
)
utils::write.csv(
  data.frame(
    artifact = "figure3_scoring_provenance",
    clip = clip_id,
    scoring_package = "transforEmotion",
    scoring_package_version = transforemotion_version,
    model_key = model_name,
    model_id = model_id,
    model_description = model_description,
    prompt_channels = paste(emotion_cols, collapse = ";"),
    frame_extraction_settings = paste(
      paste0("MAFW calibration frame-score CSV with ", clip_n_frames, " retained frames for selected clip"),
      "complete-score rows retained",
      "score vectors renormalized after clipping",
      sep = "; "
    ),
    frame_scores_path = frame_scores_path,
    frame_scores_sha256 = sha256_file(frame_scores_path),
    processing_summary_path = if (file.exists(processing_summary_path)) processing_summary_path else NA_character_,
    processing_summary_sha256 = if (file.exists(processing_summary_path)) sha256_file(processing_summary_path) else NA_character_,
    selected_frame_indices = paste(selected_frames$frame_index, collapse = ";"),
    include_frame_stills = include_frame_stills,
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, paste0(clip_stem, "_figure3_scoring_provenance.csv")),
  row.names = FALSE
)

make_bar_panel <- function(row, show_y_axis = FALSE) {
  scores <- data.frame(
    emotion = factor(emotion_cols, levels = emotion_cols),
    label = emotion_labels[emotion_cols],
    score = as.numeric(row[1, emotion_cols]),
    stringsAsFactors = FALSE
  ) %>%
    mutate(is_top = as.character(.data$emotion) %in% top_emotions)

  panel <- ggplot(scores, aes(x = .data$emotion, y = .data$score, fill = .data$emotion, alpha = .data$is_top)) +
    geom_col(width = 0.78) +
    scale_fill_manual(values = display_colors, guide = "none") +
    scale_alpha_manual(values = c(`TRUE` = 0.96, `FALSE` = 0.22), guide = "none") +
    scale_x_discrete(labels = emotion_labels[emotion_cols]) +
    scale_y_continuous(
      limits = c(0, bar_y_max),
      breaks = bar_y_breaks,
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      x = NULL,
      y = "Classifier-output score",
      title = sprintf("Frame %d", row$frame_index[[1]]),
      subtitle = row$profile_label[[1]]
    ) +
    theme_classic(base_size = 7.5, base_family = base_family) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.3),
      axis.text.y = element_text(size = 6.8),
      axis.title.y = element_text(size = 7.6, margin = margin(r = 3)),
      axis.line = element_line(linewidth = 0.25),
      axis.ticks = element_line(linewidth = 0.25),
      panel.grid = element_blank(),
      plot.title = element_text(size = 8.5, face = "bold", margin = margin(b = 1)),
      plot.subtitle = element_text(size = 7.0, margin = margin(b = 3)),
      plot.margin = margin(2, 4, 2, 4)
    )

  if (!show_y_axis) {
    panel <- panel +
      labs(y = NULL) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      )
  }

  panel
}

bar_panels <- lapply(seq_len(nrow(selected_frames)), function(idx) {
  make_bar_panel(selected_frames[idx, , drop = FALSE], show_y_axis = idx == 1L)
})

time_series <- clip_scores %>%
  dplyr::select(frame_index, dplyr::all_of(emotion_cols)) %>%
  pivot_longer(
    cols = all_of(emotion_cols),
    names_to = "emotion",
    values_to = "score"
  ) %>%
  group_by(.data$emotion) %>%
  mutate(smoothed_score = smooth_series(.data$score, smooth_window)) %>%
  ungroup() %>%
  mutate(
    emotion = factor(.data$emotion, levels = emotion_cols),
    selected = .data$frame_index %in% selected_frames$frame_index,
    is_top = as.character(.data$emotion) %in% top_emotions
  )

time_panel <- ggplot() +
  geom_line(
    data = filter(time_series, !.data$is_top),
    aes(x = .data$frame_index, y = .data$smoothed_score, color = .data$emotion, group = .data$emotion),
    linewidth = 0.34,
    alpha = 0.11,
    show.legend = FALSE
  ) +
  geom_line(
    data = filter(time_series, .data$is_top),
    aes(x = .data$frame_index, y = .data$smoothed_score, color = .data$emotion, group = .data$emotion),
    linewidth = 0.95,
    alpha = 0.96
  ) +
  scale_color_manual(
    values = display_colors,
    breaks = top_emotions,
    labels = emotion_labels[top_emotions],
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(0, 50, by = 10), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, max(0.50, max(time_series$smoothed_score) * 1.05)), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Frame", y = sprintf("Classifier-output score (%d-frame smooth)", smooth_window)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.2, alpha = 1))) +
  theme_classic(base_size = 8.5, base_family = base_family) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8.0),
    legend.key.width = unit(12, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    axis.text = element_text(size = 7.5),
    axis.title = element_text(size = 8.8),
    axis.line = element_line(linewidth = 0.28),
    axis.ticks = element_line(linewidth = 0.28),
    panel.grid = element_blank(),
    plot.margin = margin(0, 8, 0, 8)
  )

plot_edges <- edge_table %>% filter(.data$abs_weight >= edge_threshold)
if (nrow(plot_edges) == 0L) {
  plot_edges <- edge_table %>% dplyr::slice_head(n = min(12L, n()))
}

graph <- graph_from_data_frame(
  plot_edges[, c("from", "to", "weight", "abs_weight", "sign"), drop = FALSE],
  directed = FALSE,
  vertices = membership_table[, c("emotion", "label", "community"), drop = FALSE]
)

set.seed(20260515)
layout <- layout_with_fr(graph, weights = pmax(E(graph)$abs_weight, 0.01))
layout <- as.data.frame(layout)
names(layout) <- c("x", "y")
layout$emotion <- V(graph)$name
layout$label <- V(graph)$label
layout$community <- factor(V(graph)$community, levels = sort(unique(membership_table$community)))
layout$plot_label <- dplyr::recode(
  layout$emotion,
  helplessness = "Helpless\nness",
  disappointment = "Disappoint\nment",
  .default = layout$label
)
rescale_centered <- function(x) {
  x <- x - mean(x, na.rm = TRUE)
  max_abs <- max(abs(x), na.rm = TRUE)
  if (is.finite(max_abs) && max_abs > 0) {
    x / max_abs
  } else {
    x
  }
}
layout$x <- rescale_centered(layout$x)
layout$y <- rescale_centered(layout$y)
max_edge <- max(E(graph)$abs_weight, na.rm = TRUE)
edge_plot_table <- igraph::as_data_frame(graph, what = "edges") %>%
  left_join(layout %>% dplyr::select(from = emotion, x_from = x, y_from = y), by = "from") %>%
  left_join(layout %>% dplyr::select(to = emotion, x_to = x, y_to = y), by = "to") %>%
  mutate(
    sign_color = if_else(.data$sign == "positive", "#2E7D32", "#D32F2F"),
    edge_width = 0.30 + 1.65 * .data$abs_weight / max_edge
  )
community_values <- setNames(
  community_colors[seq_along(levels(layout$community))],
  levels(layout$community)
)
community_labels <- setNames(
  paste("Community", levels(layout$community)),
  levels(layout$community)
)

network_panel <- ggplot() +
  geom_segment(
    data = edge_plot_table,
    aes(x = .data$x_from, y = .data$y_from, xend = .data$x_to, yend = .data$y_to),
    linewidth = edge_plot_table$edge_width,
    color = grDevices::adjustcolor(edge_plot_table$sign_color, alpha.f = 0.64),
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_point(
    data = layout,
    aes(x = .data$x, y = .data$y, fill = .data$community),
    shape = 21,
    size = 17.0,
    stroke = 0.35,
    color = grDevices::adjustcolor("black", alpha.f = 0.35)
  ) +
  geom_text(
    data = layout,
    aes(x = .data$x, y = .data$y, label = .data$plot_label),
    size = 2.8,
    family = base_family,
    color = "black",
    lineheight = 0.80
  ) +
  scale_fill_manual(values = community_values, labels = community_labels, name = NULL) +
  coord_equal(
    xlim = c(min(layout$x) - 0.18, max(layout$x) + 0.52),
    ylim = c(min(layout$y) - 0.18, max(layout$y) + 0.18),
    clip = "off"
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5.2), ncol = 1)) +
  theme_void(base_family = base_family) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8.0),
    legend.key.size = unit(10, "pt"),
    legend.margin = margin(0, 6, 0, 0),
    plot.margin = margin(0, 14, 0, 14)
  )

make_section_header <- function(label, title) {
  ggdraw() +
    draw_label(
      label,
      x = 0.006, y = 0.5,
      hjust = 0, vjust = 0.5,
      size = 10.5,
      fontface = "bold",
      fontfamily = base_family
    ) +
    draw_label(
      title,
      x = 0.035, y = 0.5,
      hjust = 0, vjust = 0.5,
      size = 10.5,
      fontface = "bold",
      fontfamily = base_family
    )
}

make_frame_still_panel <- function(row) {
  frame_image <- jpeg::readJPEG(row$still_path[[1]])
  ggdraw() +
    draw_grob(
      rasterGrob(frame_image, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE),
      x = 0, y = 0, width = 1, height = 1
    ) +
    draw_label(
      sprintf("Frame %d", row$frame_index[[1]]),
      x = 0.02, y = 0.95,
      hjust = 0, vjust = 1,
      size = 8.5,
      fontface = "bold",
      fontfamily = base_family,
      color = "white"
    )
}

if (include_frame_stills) {
  still_panels <- lapply(seq_len(nrow(selected_frames)), function(idx) {
    make_frame_still_panel(selected_frames[idx, , drop = FALSE])
  })
  still_row <- plot_grid(plotlist = still_panels, nrow = 1, align = "h", axis = "tb")
}

bar_row <- plot_grid(plotlist = bar_panels, nrow = 1, align = "h", axis = "tb")

if (include_frame_stills) {
  figure_body <- plot_grid(
    make_section_header("A", "Selected MAFW video frames"),
    still_row,
    make_section_header("B", "Selected-frame classifier-output score profiles"),
    bar_row,
    make_section_header("C", "Smoothed classifier-output trajectories"),
    time_panel,
    ncol = 1,
    rel_heights = c(0.11, 0.72, 0.10, 0.94, 0.10, 1.16)
  )
} else {
  figure_body <- plot_grid(
    make_section_header("A", "Selected-frame classifier-output score profiles"),
    bar_row,
    make_section_header("B", "Smoothed classifier-output trajectories"),
    time_panel,
    ncol = 1,
    rel_heights = c(0.12, 1.02, 0.10, 1.34)
  )
}
figure <- ggdraw() +
  draw_plot(figure_body, x = 0.016, y = 0.014, width = 0.968, height = 0.972)

output_stem <- file.path(output_dir, paste0("oai_clip_video_dynega_", clip_stem))
network_path <- file.path(output_dir, paste0(clip_stem, "_dynega_network.png"))
ggsave(paste0(output_stem, ".png"), figure, width = figure_width, height = figure_height, dpi = figure_dpi, bg = "white")
ggsave(paste0(output_stem, ".pdf"), figure, width = figure_width, height = figure_height, bg = "white")
if (nzchar(manuscript_pdf_path)) {
  dir.create(dirname(manuscript_pdf_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(manuscript_pdf_path, figure, width = figure_width, height = figure_height, bg = "white")
}
ggsave(network_path, network_panel, width = 5.4, height = 3.0, dpi = figure_dpi, bg = "white")

cat("Saved figure PNG: ", paste0(output_stem, ".png"), "\n", sep = "")
cat("Saved figure PDF: ", paste0(output_stem, ".pdf"), "\n", sep = "")
if (nzchar(manuscript_pdf_path)) {
  cat("Saved manuscript figure PDF: ", manuscript_pdf_path, "\n", sep = "")
}
cat("Saved dynEGA network PNG: ", network_path, "\n", sep = "")
cat("Saved dynEGA edge table: ", file.path(output_dir, paste0(clip_stem, "_dynega_edges.csv")), "\n", sep = "")
cat("Saved dynEGA membership table: ", file.path(output_dir, paste0(clip_stem, "_dynega_membership.csv")), "\n", sep = "")
