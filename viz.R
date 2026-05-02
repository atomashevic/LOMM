#!/usr/bin/env Rscript
# Visualization script for simulation results
# Filters out catastrophic ss results to show meaningful comparisons

library(ggplot2)
library(dplyr)
library(tidyr)

# Read the simulation summary
data <- read.csv("sim_outputs/sim_summary.csv")

# Define threshold for filtering catastrophic results
# Looking at reasonable results, RMSE should be < 1 for meaningful comparison
RMSE_THRESHOLD <- 1
BIAS_THRESHOLD <- 1

# Filter out catastrophic results
data_filtered <- data %>%
  filter(abs(rmse) < RMSE_THRESHOLD, abs(mean_err) < BIAS_THRESHOLD)

cat("Original rows:", nrow(data), "\n")
cat("Filtered rows:", nrow(data_filtered), "\n")
cat("Removed", nrow(data) - nrow(data_filtered), "catastrophic results\n\n")

# Show which combinations were removed
removed <- data %>%
  filter(abs(rmse) >= RMSE_THRESHOLD | abs(mean_err) >= BIAS_THRESHOLD) %>%
  select(integrator, param, method, Q2, rmse, mean_err)
cat("Removed combinations:\n")
print(removed)
cat("\n")

# Set up theme
theme_set(theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ))

# Color palette
method_colors <- c("raw" = "#E41A1C", "scores" = "#4DAF4A", "ss" = "#377EB8")

# ============================================================================
# Plot 1: RMSE by integrator and method
# ============================================================================
p_rmse <- ggplot(data_filtered, aes(x = integrator, y = rmse, fill = method)) +

geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(param ~ Q2, scales = "free_y", labeller = labeller(
    Q2 = function(x) paste0("Q² = ", x),
    param = function(x) paste0("Parameter: ", x)
  )) +
  scale_fill_manual(values = method_colors, name = "Recovery Method") +
  labs(
    title = "RMSE by Integrator and Recovery Method",
    subtitle = "Catastrophic results (RMSE > 1) filtered out",
    x = "Integrator",
    y = "RMSE"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("sim_outputs/viz_rmse.png", p_rmse, width = 12, height = 8, dpi = 150)
cat("Saved: sim_outputs/viz_rmse.png\n")

# ============================================================================
# Plot 2: Bias (mean error) with confidence intervals
# ============================================================================
p_bias <- ggplot(data_filtered, aes(x = integrator, y = mean_err, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.6),
    size = 0.4
  ) +
  facet_grid(param ~ Q2, scales = "free_y", labeller = labeller(
    Q2 = function(x) paste0("Q² = ", x),
    param = function(x) paste0("Parameter: ", x)
  )) +
  scale_color_manual(values = method_colors, name = "Recovery Method") +
  labs(
    title = "Bias (Mean Error) with 95% Confidence Intervals",
    subtitle = "Catastrophic results filtered out | Dashed line = zero bias",
    x = "Integrator",
    y = "Mean Error (Bias)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("sim_outputs/viz_bias_ci.png", p_bias, width = 12, height = 8, dpi = 150)
cat("Saved: sim_outputs/viz_bias_ci.png\n")

# ============================================================================
# Plot 3: Summary comparison - average RMSE across Q2 values
# ============================================================================
summary_stats <- data_filtered %>%
  group_by(integrator, param, method) %>%
  summarize(
    mean_rmse = mean(rmse),
    sd_rmse = sd(rmse),
    mean_bias = mean(mean_err),
    n_conditions = n(),
    .groups = "drop"
  )

p_summary <- ggplot(summary_stats, aes(x = reorder(integrator, mean_rmse), y = mean_rmse, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~param, scales = "free_y", labeller = labeller(
    param = function(x) paste0("Parameter: ", x)
  )) +
  scale_fill_manual(values = method_colors, name = "Recovery Method") +
  labs(
    title = "Average RMSE by Integrator (Across Q² Values)",
    subtitle = "Integrators ordered by mean RMSE (best to worst)",
    x = "Integrator",
    y = "Mean RMSE"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("sim_outputs/viz_summary.png", p_summary, width = 10, height = 5, dpi = 150)
cat("Saved: sim_outputs/viz_summary.png\n")

# ============================================================================
# Plot 4: Log-scale RMSE for better comparison of small values
# ============================================================================
p_rmse_log <- ggplot(data_filtered, aes(x = integrator, y = rmse, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(param ~ Q2, scales = "free_y", labeller = labeller(
    Q2 = function(x) paste0("Q² = ", x),
    param = function(x) paste0("Parameter: ", x)
  )) +
  scale_y_log10() +
  scale_fill_manual(values = method_colors, name = "Recovery Method") +
  labs(
    title = "RMSE by Integrator and Recovery Method (Log Scale)",
    subtitle = "Log scale reveals differences between best-performing methods",
    x = "Integrator",
    y = "RMSE (log scale)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("sim_outputs/viz_rmse_log.png", p_rmse_log, width = 12, height = 8, dpi = 150)
cat("Saved: sim_outputs/viz_rmse_log.png\n")

# Print summary statistics
cat("\n========================================\n")
cat("Summary Statistics (filtered data)\n")
cat("========================================\n\n")

cat("Best combinations by RMSE:\n")
data_filtered %>%
  arrange(rmse) %>%
  head(10) %>%
  select(integrator, param, method, Q2, rmse, mean_err) %>%
  print()

cat("\nWorst combinations by RMSE (among filtered):\n")
data_filtered %>%
  arrange(desc(rmse)) %>%
  head(10) %>%
  select(integrator, param, method, Q2, rmse, mean_err) %>%
  print()

cat("\nMean RMSE by method:\n")
data_filtered %>%
  group_by(method) %>%
  summarize(mean_rmse = mean(rmse), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  print()

cat("\nMean RMSE by integrator:\n")
data_filtered %>%
  group_by(integrator) %>%
  summarize(mean_rmse = mean(rmse), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  print()

cat("\nDone!\n")


