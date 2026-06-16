#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

session="${FINAL_GRAPHICALVAR_BOUNDED_TMUX_SESSION:-lomm-final-graphicalvar-bounded}"
out_dir="${FINAL_GRAPHICALVAR_BOUNDED_OUT_DIR:-simulation_outputs/final_simulation_results/graphicalvar_bounded_nvar4}"
workers="${FINAL_GRAPHICALVAR_BOUNDED_WORKERS:-24}"
reps="${FINAL_GRAPHICALVAR_BOUNDED_REPS:-100}"
timeout_sec="${FINAL_GRAPHICALVAR_BOUNDED_TIMEOUT_SEC:-900}"
dry_run="${FINAL_GRAPHICALVAR_BOUNDED_DRY_RUN:-0}"
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
export FINAL_GRAPHICALVAR_BOUNDED_OUT_DIR="$out_dir"
export FINAL_GRAPHICALVAR_BOUNDED_WORKERS="$workers"
export FINAL_GRAPHICALVAR_BOUNDED_REPS="$reps"
export FINAL_GRAPHICALVAR_BOUNDED_TIMEOUT_SEC="$timeout_sec"
export FINAL_GRAPHICALVAR_BOUNDED_DRY_RUN="$dry_run"
log_file="$log_dir/final_graphicalvar_bounded_\$(date +%Y%m%d_%H%M%S).log"
echo "Starting final GraphicalVAR bounded run at \$(date)"
echo "Output directory: $out_dir"
echo "Workers: $workers; reps: $reps; timeout_sec: $timeout_sec; dry_run: $dry_run"
echo "Log file: \$log_file"
Rscript scripts/simulations/run_final_graphicalvar_bounded.R 2>&1 | tee "\$log_file"
echo "Finished final GraphicalVAR bounded run at \$(date)"
EOF
)

tmux new-session -d -s "$session" "bash -lc $(printf '%q' "$cmd")"

echo "Started tmux session: $session"
echo "Attach with: tmux attach -t $session"
echo "Logs: $log_dir/final_graphicalvar_bounded_*.log"
