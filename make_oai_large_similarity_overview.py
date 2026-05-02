#!/usr/bin/env python3
"""
Build a publication-style overview figure for empirical OAI-large time series
versus the heterogeneous DLO simulation fit.

The figure has two components:
1. Clip-normalized trajectory summaries for the most variable observed channels.
2. A forest plot of standardized median differences on key raw time-series metrics.

Uncertainty is shown as 95% bootstrap confidence intervals across base clips.
Synthetic replicates are collapsed back to their source clip before bootstrapping,
so the uncertainty is not artificially narrowed by multiple replicates per clip.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import numpy as np
import pandas as pd
import seaborn as sns


REPO_ROOT = Path(__file__).resolve().parent
SKILL_ROOT = Path("/home/atomasevic/.codex/skills/scientific-visualization")
SKILL_SCRIPTS = SKILL_ROOT / "scripts"

if str(SKILL_SCRIPTS) not in sys.path:
  sys.path.insert(0, str(SKILL_SCRIPTS))

from figure_export import save_publication_figure  # noqa: E402
from style_presets import OKABE_ITO_COLORS, apply_publication_style  # noqa: E402


DEFAULT_INPUT_DIR = REPO_ROOT / "empirical_outputs_smoke" / "mafw-sample" / "oai-large-100"
DEFAULT_METRICS = [
  "lag1_acf",
  "sample_entropy",
  "total_variation",
  "effective_rank",
  "mean_pairwise_cor",
  "std_value",
]

METRIC_LABELS = {
  "lag1_acf": "Lag-1 ACF",
  "sample_entropy": "Sample entropy",
  "total_variation": "Total variation",
  "effective_rank": "Effective rank",
  "mean_pairwise_cor": "Mean pairwise corr.",
  "std_value": "Standard deviation",
}

CHANNEL_LABELS = {
  "anger": "Anger",
  "anxiety": "Anxiety",
  "contempt": "Contempt",
  "disappointment": "Disappointment",
  "disgust": "Disgust",
  "fear": "Fear",
  "happiness": "Happiness",
  "helplessness": "Helplessness",
  "neutral": "Neutral",
  "sadness": "Sadness",
  "surprise": "Surprise",
}

DATASET_LABELS = {
  "empirical": "Observed OAI-large",
  "synthetic": "Heterogeneous DLO",
}

DATASET_COLORS = {
  "empirical": OKABE_ITO_COLORS[4],
  "synthetic": OKABE_ITO_COLORS[5],
}

TRAJECTORY_LABELS = {
  "raw": "Channel value",
  "demean": "Deviation from clip mean",
  "zscore": "Within-clip z score",
}


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Create a similarity overview figure for empirical OAI-large versus heterogeneous DLO outputs."
  )
  parser.add_argument(
    "--input-dir",
    default=str(DEFAULT_INPUT_DIR),
    help="Directory containing empirical/synthetic CSV outputs from run_empirical_mafw_sample_pipeline.R",
  )
  parser.add_argument(
    "--mode",
    default="heterogeneous",
    choices=["homogeneous", "heterogeneous"],
    help="Synthetic mode to compare against the empirical target.",
  )
  parser.add_argument(
    "--n-channels",
    type=int,
    default=4,
    help="Number of observed channels to show in the trajectory panels.",
  )
  parser.add_argument(
    "--channels",
    default="",
    help="Comma-separated channel names to plot instead of automatic selection.",
  )
  parser.add_argument(
    "--n-timepoints",
    type=int,
    default=60,
    help="Number of relative-time grid points for clip-normalized interpolation.",
  )
  parser.add_argument(
    "--bootstrap-reps",
    type=int,
    default=600,
    help="Number of bootstrap draws for confidence intervals.",
  )
  parser.add_argument(
    "--trajectory-scale",
    default="demean",
    choices=["raw", "demean", "zscore"],
    help="How to transform each channel trajectory before aggregation.",
  )
  parser.add_argument(
    "--output-prefix",
    default="heterogeneous_similarity_overview",
    help="Base name for the figure and exported summary tables.",
  )
  parser.add_argument(
    "--seed",
    type=int,
    default=20260421,
    help="Random seed for bootstrap intervals.",
  )
  return parser.parse_args()


def derive_base_clip(clip_value: str) -> str:
  return re.sub(r"^rep\d+_", "", str(clip_value))


def score_columns(panel: pd.DataFrame) -> list[str]:
  excluded = {"clip", "frame_index", "truth_label", "model"}
  return [col for col in panel.columns if col not in excluded]


def choose_channels(panel: pd.DataFrame, requested: str, n_channels: int) -> list[str]:
  available = score_columns(panel)
  if requested.strip():
    channels = [value.strip() for value in requested.split(",") if value.strip()]
    missing = [value for value in channels if value not in available]
    if missing:
      raise ValueError(f"Requested channels not found in panel: {', '.join(missing)}")
    return channels

  channel_sds = panel[available].std(axis=0, skipna=True).sort_values(ascending=False)
  return channel_sds.index.tolist()[:n_channels]


def transform_series(values: np.ndarray, scale: str) -> np.ndarray | None:
  values = values.astype(float)
  if np.all(np.isnan(values)):
    return None

  if scale == "raw":
    return values

  center = np.nanmean(values)
  transformed = values - center
  if scale == "demean":
    return transformed

  spread = np.nanstd(values)
  if not np.isfinite(spread) or spread <= 0:
    return None
  return transformed / spread


def interpolate_clip_trajectories(
  panel: pd.DataFrame,
  channels: list[str],
  dataset: str,
  scale: str,
  n_timepoints: int,
) -> pd.DataFrame:
  grid = np.linspace(0, 1, n_timepoints)
  working = panel.copy()
  working["base_clip"] = working["clip"].map(derive_base_clip)

  rows: list[dict[str, object]] = []
  for clip_id, clip_df in working.groupby("clip", sort=False):
    clip_df = clip_df.sort_values("frame_index")
    base_clip = clip_df["base_clip"].iloc[0]
    raw_time = np.linspace(0, 1, len(clip_df))
    for channel in channels:
      transformed = transform_series(clip_df[channel].to_numpy(), scale)
      if transformed is None or len(transformed) < 3:
        continue
      if np.allclose(np.nanstd(transformed), 0):
        continue
      interp = np.interp(grid, raw_time, transformed)
      for rel_time, value in zip(grid, interp):
        rows.append(
          {
            "dataset": dataset,
            "clip": clip_id,
            "base_clip": base_clip,
            "channel": channel,
            "rel_time": rel_time,
            "value": float(value),
          }
        )

  trajectories = pd.DataFrame(rows)
  if trajectories.empty:
    raise ValueError(f"No usable trajectories were produced for {dataset}.")

  if dataset == "synthetic":
    trajectories = (
      trajectories.groupby(["dataset", "base_clip", "channel", "rel_time"], as_index=False)["value"]
      .mean()
    )
  else:
    trajectories = trajectories.drop(columns=["clip"])

  return trajectories


def bootstrap_curve_summary(
  trajectory_df: pd.DataFrame,
  bootstrap_reps: int,
  seed: int,
) -> pd.DataFrame:
  rng = np.random.default_rng(seed)
  summaries: list[pd.DataFrame] = []

  for (dataset, channel), subset in trajectory_df.groupby(["dataset", "channel"], sort=False):
    pivot = subset.pivot(index="base_clip", columns="rel_time", values="value").sort_index(axis=1)
    values = pivot.to_numpy(dtype=float)
    if values.shape[0] == 0:
      continue
    estimate = np.nanmean(values, axis=0)
    draw_idx = rng.integers(0, values.shape[0], size=(bootstrap_reps, values.shape[0]))
    boot = np.nanmean(values[draw_idx], axis=1)
    ci_low = np.nanpercentile(boot, 2.5, axis=0)
    ci_high = np.nanpercentile(boot, 97.5, axis=0)

    summaries.append(
      pd.DataFrame(
        {
          "dataset": dataset,
          "channel": channel,
          "rel_time": pivot.columns.to_numpy(dtype=float),
          "estimate": estimate,
          "ci_low": ci_low,
          "ci_high": ci_high,
          "n_base_clips": values.shape[0],
        }
      )
    )

  return pd.concat(summaries, ignore_index=True)


def prepare_metric_values(metrics_df: pd.DataFrame, dataset: str, selected_metrics: list[str]) -> pd.DataFrame:
  filtered = metrics_df.copy()
  filtered["base_clip"] = filtered["clip"].map(derive_base_clip)
  filtered = filtered[filtered["metric"].isin(selected_metrics)].copy()
  filtered["dataset"] = dataset
  filtered = filtered.dropna(subset=["value"])

  # Collapse series-level metrics across observed channels so each clip contributes
  # one summary value per metric before synthetic replicates are averaged back to the
  # empirical clip level.
  clip_level = (
    filtered.groupby(["dataset", "clip", "base_clip", "metric"], as_index=False)["value"]
    .median()
  )

  if dataset == "synthetic":
    filtered = (
      clip_level.groupby(["dataset", "base_clip", "metric"], as_index=False)["value"]
      .mean()
    )
  else:
    filtered = clip_level[["dataset", "base_clip", "metric", "value"]]

  return filtered


def bootstrap_metric_differences(
  empirical_metrics: pd.DataFrame,
  synthetic_metrics: pd.DataFrame,
  metric_order: list[str],
  bootstrap_reps: int,
  seed: int,
) -> pd.DataFrame:
  rng = np.random.default_rng(seed)
  rows: list[dict[str, float | str]] = []

  for metric in metric_order:
    empirical_values = empirical_metrics.loc[empirical_metrics["metric"] == metric, "value"].to_numpy(dtype=float)
    synthetic_values = synthetic_metrics.loc[synthetic_metrics["metric"] == metric, "value"].to_numpy(dtype=float)
    empirical_values = empirical_values[np.isfinite(empirical_values)]
    synthetic_values = synthetic_values[np.isfinite(synthetic_values)]
    if len(empirical_values) < 2 or len(synthetic_values) < 2:
      continue

    empirical_median = float(np.median(empirical_values))
    synthetic_median = float(np.median(synthetic_values))
    empirical_iqr = float(np.subtract(*np.percentile(empirical_values, [75, 25])))
    if not np.isfinite(empirical_iqr) or empirical_iqr <= 0:
      empirical_iqr = 1.0

    empirical_draws = empirical_values[
      rng.integers(0, len(empirical_values), size=(bootstrap_reps, len(empirical_values)))
    ]
    synthetic_draws = synthetic_values[
      rng.integers(0, len(synthetic_values), size=(bootstrap_reps, len(synthetic_values)))
    ]

    empirical_boot = np.median(empirical_draws, axis=1)
    synthetic_boot = np.median(synthetic_draws, axis=1)
    diff_boot = (synthetic_boot - empirical_boot) / empirical_iqr
    diff_estimate = (synthetic_median - empirical_median) / empirical_iqr

    rows.append(
      {
        "metric": metric,
        "empirical_median": empirical_median,
        "synthetic_median": synthetic_median,
        "empirical_iqr": empirical_iqr,
        "scaled_diff": diff_estimate,
        "ci_low": float(np.percentile(diff_boot, 2.5)),
        "ci_high": float(np.percentile(diff_boot, 97.5)),
        "n_empirical": len(empirical_values),
        "n_synthetic": len(synthetic_values),
      }
    )

  return pd.DataFrame(rows)


def read_mode_summary(input_dir: Path, mode: str) -> dict[str, str]:
  summary_path = input_dir / "mode_summary.csv"
  if not summary_path.exists():
    return {}

  summary = pd.read_csv(summary_path)
  if summary.empty or "mode" not in summary.columns:
    return {}

  matched = summary.loc[summary["mode"] == mode]
  if matched.empty:
    return {}

  row = matched.iloc[0].to_dict()
  return {str(key): str(value) for key, value in row.items()}


def format_channel_label(channel: str) -> str:
  return CHANNEL_LABELS.get(channel, channel.replace("_", " ").title())


def format_metric_label(metric: str) -> str:
  return METRIC_LABELS.get(metric, metric.replace("_", " ").title())


def plot_similarity_figure(
  trajectory_summary: pd.DataFrame,
  metric_diffs: pd.DataFrame,
  channel_order: list[str],
  metric_order: list[str],
  trajectory_scale: str,
  output_base: Path,
  winner_summary: dict[str, str],
) -> None:
  apply_publication_style("default")
  plt.rcParams["figure.constrained_layout.use"] = False
  sns.set_theme(style="ticks", context="paper")

  fig = plt.figure(figsize=(7.2, 10.0), constrained_layout=False)
  fig.set_layout_engine(None)
  outer = fig.add_gridspec(3, 2, height_ratios=[1, 1, 1.1], hspace=0.34, wspace=0.24)
  fig.subplots_adjust(top=0.76, bottom=0.08, left=0.10, right=0.98)

  trajectory_axes = [
    fig.add_subplot(outer[0, 0]),
    fig.add_subplot(outer[0, 1]),
    fig.add_subplot(outer[1, 0]),
    fig.add_subplot(outer[1, 1]),
  ]
  forest_ax = fig.add_subplot(outer[2, :])

  for ax, channel in zip(trajectory_axes, channel_order):
    channel_df = trajectory_summary[trajectory_summary["channel"] == channel].copy()
    for dataset in ["empirical", "synthetic"]:
      subset = channel_df[channel_df["dataset"] == dataset].sort_values("rel_time")
      if subset.empty:
        continue
      ax.fill_between(
        subset["rel_time"],
        subset["ci_low"],
        subset["ci_high"],
        color=DATASET_COLORS[dataset],
        alpha=0.18,
        linewidth=0,
      )
      ax.plot(
        subset["rel_time"],
        subset["estimate"],
        color=DATASET_COLORS[dataset],
        linewidth=1.8,
        label=DATASET_LABELS[dataset],
      )

    if trajectory_scale != "raw":
      ax.axhline(0, color="#9CA3AF", linewidth=0.8, linestyle="--")

    ax.set_title(format_channel_label(channel))
    ax.set_xlim(0, 1)
    ax.xaxis.set_major_formatter(mtick.PercentFormatter(xmax=1, decimals=0))
    ax.set_xlabel("Clip progress")
    sns.despine(ax=ax)

  for ax in trajectory_axes[len(channel_order):]:
    ax.set_visible(False)

  trajectory_axes[0].set_ylabel(TRAJECTORY_LABELS[trajectory_scale])
  trajectory_axes[2].set_ylabel(TRAJECTORY_LABELS[trajectory_scale])

  metric_diffs = metric_diffs.set_index("metric").reindex(metric_order).reset_index()
  positions = np.arange(len(metric_diffs))[::-1]
  forest_ax.axvspan(-0.25, 0.25, color="#E5E7EB", alpha=0.85, zorder=0)
  forest_ax.axvline(0, color="#111827", linewidth=1.0, linestyle="--", zorder=1)
  forest_ax.errorbar(
    metric_diffs["scaled_diff"],
    positions,
    xerr=[
      metric_diffs["scaled_diff"] - metric_diffs["ci_low"],
      metric_diffs["ci_high"] - metric_diffs["scaled_diff"],
    ],
    fmt="o",
    color=DATASET_COLORS["synthetic"],
    ecolor=DATASET_COLORS["synthetic"],
    capsize=3,
    linewidth=1.4,
    markersize=4.5,
    zorder=3,
  )
  forest_ax.set_yticks(positions)
  forest_ax.set_yticklabels([format_metric_label(metric) for metric in metric_diffs["metric"]])
  forest_ax.set_xlabel("Synthetic minus observed median (scaled by observed IQR)")
  forest_ax.set_title("Clip-level metric differences with 95% bootstrap CI")
  sns.despine(ax=forest_ax)

  title = "Observed OAI-large versus heterogeneous DLO similarity"
  subtitle_parts = []
  if winner_summary:
    subtitle_parts.append(f"eta={winner_summary.get('eta_spec', 'NA')}")
    subtitle_parts.append(f"zeta={winner_summary.get('zeta_spec', 'NA')}")
  empirical_n = int(trajectory_summary.loc[trajectory_summary["dataset"] == "empirical", "n_base_clips"].max())
  synthetic_n = int(trajectory_summary.loc[trajectory_summary["dataset"] == "synthetic", "n_base_clips"].max())
  subtitle_parts.append(f"95% bootstrap CI across base clips (observed n={empirical_n}, synthetic n={synthetic_n})")
  handles, labels = trajectory_axes[0].get_legend_handles_labels()
  fig.suptitle(title, fontsize=9.5, y=0.975)
  fig.text(0.5, 0.952, " | ".join(subtitle_parts), ha="center", va="top", fontsize=7)
  fig.legend(
    handles[:2],
    labels[:2],
    loc="upper center",
    bbox_to_anchor=(0.5, 0.934),
    ncol=2,
    frameon=False,
    fontsize=7,
    handlelength=2.0,
  )

  panel_labels = ["A", "B", "C", "D", "E"]
  for label, ax in zip(panel_labels[:4], trajectory_axes):
    ax.text(-0.16, 1.05, label, transform=ax.transAxes, fontsize=10, fontweight="bold", va="top")
  forest_ax.text(-0.05, 1.05, panel_labels[4], transform=forest_ax.transAxes, fontsize=10, fontweight="bold", va="top")

  save_publication_figure(fig, output_base, formats=["pdf", "png"], dpi=300)
  plt.close(fig)


def main() -> None:
  args = parse_args()
  input_dir = Path(args.input_dir).resolve()
  if not input_dir.exists():
    raise FileNotFoundError(f"Input directory does not exist: {input_dir}")

  empirical_panel = pd.read_csv(input_dir / "empirical_panel_clean.csv")
  synthetic_panel = pd.read_csv(input_dir / f"{args.mode}_synthetic_panel.csv")
  empirical_metrics = pd.read_csv(input_dir / "empirical_raw_metrics.csv")
  synthetic_metrics = pd.read_csv(input_dir / f"{args.mode}_synthetic_raw_metrics.csv")

  channel_order = choose_channels(empirical_panel, args.channels, args.n_channels)
  metric_order = [metric for metric in DEFAULT_METRICS if metric in empirical_metrics["metric"].unique()]

  empirical_traj = interpolate_clip_trajectories(
    panel=empirical_panel,
    channels=channel_order,
    dataset="empirical",
    scale=args.trajectory_scale,
    n_timepoints=args.n_timepoints,
  )
  synthetic_traj = interpolate_clip_trajectories(
    panel=synthetic_panel,
    channels=channel_order,
    dataset="synthetic",
    scale=args.trajectory_scale,
    n_timepoints=args.n_timepoints,
  )
  trajectory_df = pd.concat([empirical_traj, synthetic_traj], ignore_index=True)
  trajectory_summary = bootstrap_curve_summary(
    trajectory_df=trajectory_df,
    bootstrap_reps=args.bootstrap_reps,
    seed=args.seed,
  )

  empirical_metric_values = prepare_metric_values(empirical_metrics, "empirical", metric_order)
  synthetic_metric_values = prepare_metric_values(synthetic_metrics, "synthetic", metric_order)
  metric_diffs = bootstrap_metric_differences(
    empirical_metrics=empirical_metric_values,
    synthetic_metrics=synthetic_metric_values,
    metric_order=metric_order,
    bootstrap_reps=args.bootstrap_reps,
    seed=args.seed + 17,
  )

  output_base = input_dir / args.output_prefix
  trajectory_summary.to_csv(f"{output_base}_trajectory_ci.csv", index=False)
  metric_diffs.to_csv(f"{output_base}_metric_ci.csv", index=False)

  plot_similarity_figure(
    trajectory_summary=trajectory_summary,
    metric_diffs=metric_diffs,
    channel_order=channel_order,
    metric_order=metric_order,
    trajectory_scale=args.trajectory_scale,
    output_base=output_base,
    winner_summary=read_mode_summary(input_dir, args.mode),
  )

  print(f"Wrote similarity overview to {output_base}.pdf/.png")


if __name__ == "__main__":
  main()
