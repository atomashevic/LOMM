#!/usr/bin/env bash
set -euo pipefail

interval_sec="${1:-900}"
log_dir="${2:-benchmark_monitoring}"
mkdir -p "$log_dir"

split_root="benchmark_outputs_revision_gimme_split"
summary_dir="benchmark_outputs_revision_summary"
figure_dir="comparison_revision_first_benchmark"
log_file="$log_dir/revision_completion_watcher_$(date +%Y%m%d-%H%M%S).log"

line_count() {
  local file="$1"
  if [[ -f "$file" ]]; then
    wc -l < "$file"
  else
    echo 0
  fi
}

split_dirs_csv() {
  find "$split_root" -mindepth 1 -maxdepth 1 -type d | sort | paste -sd, -
}

split_complete_count() {
  local done_count=0
  local total_count=0
  local dir lines
  while IFS= read -r dir; do
    [[ -n "$dir" ]] || continue
    total_count=$((total_count + 1))
    lines=$(line_count "$dir/checkpoint_results.csv")
    if [[ "$lines" -ge 11 ]]; then
      done_count=$((done_count + 1))
    fi
  done < <(find "$split_root" -mindepth 1 -maxdepth 1 -type d | sort)
  printf '%s/%s' "$done_count" "$total_count"
}

{
  echo "revision completion watcher started at $(date --iso-8601=seconds)"
  echo "interval_sec=$interval_sec"

  while true; do
    ts=$(date --iso-8601=seconds)
    dy=$(line_count benchmark_outputs_revision_dynega/checkpoint_results.csv)
    gv=$(line_count benchmark_outputs_revision_graphicalvar/checkpoint_results.csv)
    split_status=$(split_complete_count)

    echo "[$ts] lines dynega=$dy graphicalvar=$gv gimme_split_complete=$split_status"
    uptime
    free -h | sed -n '2p'

    if [[ "$dy" -ge 1921 && "$gv" -ge 1921 && "$split_status" == "12/12" ]]; then
      echo "row-count gate passed at $(date --iso-8601=seconds)"
      echo "waiting for GraphicalVAR and split GIMME parent processes to exit"
      while pgrep -af '[r]un_graphicalvar_full_suite.R|[r]un_gimme_full_suite.R|[r]un_method_benchmark.R' >/dev/null; do
        pgrep -af '[r]un_graphicalvar_full_suite.R|[r]un_gimme_full_suite.R|[r]un_method_benchmark.R'
        sleep 60
      done
      break
    fi

    sleep "$interval_sec"
  done

  input_dirs="benchmark_outputs_revision_dynega,benchmark_outputs_revision_graphicalvar,$(split_dirs_csv)"
  echo "postprocessing input_dirs=$input_dirs"

  FULL_SUITE_INPUT_DIRS="$input_dirs" \
    FULL_SUITE_SUMMARY_OUT_DIR="$summary_dir" \
    Rscript summarize_full_suite_results.R

  REVISION_INPUT_DIRS="$input_dirs" \
    REVISION_FIGURE_OUT_DIR="$figure_dir" \
    Rscript plot_revision_first_benchmark_figures.R

  echo "postprocess complete at $(date --iso-8601=seconds)"
  find "$summary_dir" "$figure_dir" -maxdepth 1 -type f | sort
} >> "$log_file" 2>&1
