#!/usr/bin/env bash
set -u

interval_sec="${1:-120}"
log_dir="${2:-benchmark_monitoring}"
mkdir -p "$log_dir"

gvar_dir="benchmark_outputs_revision_graphicalvar"
gimme_dir="benchmark_outputs_revision_gimme"
dynega_dir="benchmark_outputs_revision_dynega"

gvar_target=1921
gimme_target=121
dynega_target=1921

thread_env=(
  OMP_NUM_THREADS=1
  OPENBLAS_NUM_THREADS=1
  MKL_NUM_THREADS=1
  BLIS_NUM_THREADS=1
  VECLIB_MAXIMUM_THREADS=1
  NUMEXPR_NUM_THREADS=1
)

count_lines() {
  local file="$1"
  if [[ -f "$file" ]]; then
    wc -l < "$file"
  else
    echo 0
  fi
}

method_running() {
  local script="$1"
  ps -eo cmd | awk -v script="$script" '
    $0 == "/usr/lib64/R/bin/exec/R --no-echo --no-restore --file=" script { found = 1 }
    END { exit found ? 0 : 1 }
  '
}

launch_graphicalvar() {
  setsid nohup env "${thread_env[@]}" \
    BENCHMARK_GRAPHICALVAR_WORKERS=24 \
    BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC=600 \
    BENCHMARK_CHECKPOINT_EACH=1 \
    BENCHMARK_RESUME=1 \
    Rscript run_graphicalvar_full_suite.R \
    > "$gvar_dir/run.log" 2>&1 < /dev/null &
  echo $! > "$gvar_dir/run.pid"
  echo "launched GraphicalVAR pid $(cat "$gvar_dir/run.pid")"
}

launch_gimme() {
  setsid nohup env "${thread_env[@]}" \
    BENCHMARK_GIMME_WORKERS=8 \
    BENCHMARK_GIMME_TIMEOUT_SEC=360 \
    BENCHMARK_CHECKPOINT_EACH=1 \
    BENCHMARK_RESUME=1 \
    Rscript run_gimme_full_suite.R \
    > "$gimme_dir/run.log" 2>&1 < /dev/null &
  echo $! > "$gimme_dir/run.pid"
  echo "launched GIMME pid $(cat "$gimme_dir/run.pid")"
}

while true; do
  ts="$(date --iso-8601=seconds)"
  dynega_lines="$(count_lines "$dynega_dir/checkpoint_results.csv")"
  gvar_lines="$(count_lines "$gvar_dir/checkpoint_results.csv")"
  gimme_lines="$(count_lines "$gimme_dir/checkpoint_results.csv")"
  echo "[$ts] dynega=$dynega_lines graphicalvar=$gvar_lines gimme=$gimme_lines"
  uptime
  free -h | sed -n '2p'

  if [[ "$gvar_lines" -lt "$gvar_target" ]] && ! method_running "run_graphicalvar_full_suite.R"; then
    launch_graphicalvar
  fi
  if [[ "$gimme_lines" -lt "$gimme_target" ]] && ! method_running "run_gimme_full_suite.R"; then
    launch_gimme
  fi

  if [[ "$dynega_lines" -ge "$dynega_target" && "$gvar_lines" -ge "$gvar_target" && "$gimme_lines" -ge "$gimme_target" ]]; then
    if ! method_running "run_graphicalvar_full_suite.R" && ! method_running "run_gimme_full_suite.R"; then
      break
    fi
  fi

  sleep "$interval_sec"
done

FULL_SUITE_INPUT_DIRS="$dynega_dir,$gvar_dir,$gimme_dir" \
  FULL_SUITE_SUMMARY_OUT_DIR="benchmark_outputs_revision_summary" \
  Rscript summarize_full_suite_results.R

REVISION_INPUT_DIRS="$dynega_dir,$gvar_dir,$gimme_dir" \
  REVISION_FIGURE_OUT_DIR="comparison_revision_first_benchmark" \
  Rscript plot_revision_first_benchmark_figures.R

echo "postprocess complete at $(date --iso-8601=seconds)"
find benchmark_outputs_revision_summary comparison_revision_first_benchmark -maxdepth 1 -type f | sort
