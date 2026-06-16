#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

session="${FINAL_DYNEGA_TMUX_SESSION:-lomm-final-dynega}"
out_dir="${FINAL_DYNEGA_OUT_DIR:-simulation_outputs/final_simulation_results/dynega_full}"
workers="${FINAL_DYNEGA_WORKERS:-16}"
reps="${FINAL_DYNEGA_REPS:-400}"
dry_run="${FINAL_DYNEGA_DRY_RUN:-0}"
log_dir="$out_dir/logs"

mkdir -p "$log_dir"

if tmux has-session -t "$session" 2>/dev/null; then
  echo "tmux session already exists: $session" >&2
  echo "Attach with: tmux attach -t $session" >&2
  exit 1
fi

cmd=$(cat <<EOF
set -euo pipefail
cd "$repo_root"
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export FINAL_DYNEGA_OUT_DIR="$out_dir"
export FINAL_DYNEGA_WORKERS="$workers"
export FINAL_DYNEGA_REPS="$reps"
export FINAL_DYNEGA_DRY_RUN="$dry_run"
log_file="$log_dir/final_dynega_\$(date +%Y%m%d_%H%M%S).log"
echo "Starting final DynEGA run at \$(date)"
echo "Output directory: $out_dir"
echo "Workers: $workers; reps: $reps; dry_run: $dry_run"
echo "Log file: \$log_file"
Rscript scripts/simulations/run_final_dynega_full.R 2>&1 | tee "\$log_file"
echo "Finished final DynEGA run at \$(date)"
EOF
)

tmux new-session -d -s "$session" "bash -lc $(printf '%q' "$cmd")"

echo "Started tmux session: $session"
echo "Attach with: tmux attach -t $session"
echo "Logs: $log_dir/final_dynega_*.log"
