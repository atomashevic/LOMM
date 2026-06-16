#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

session="${FINAL_DYNEGA_CD_TMUX_SESSION:-lomm-final-dynega-bounded-cd}"
out_dir="${FINAL_DYNEGA_CD_OUT_DIR:-simulation_outputs/final_simulation_results/dynega_bounded_nvar4_cd}"
workers="${FINAL_DYNEGA_CD_WORKERS:-16}"
reps="${FINAL_DYNEGA_CD_REPS:-100}"
dry_run="${FINAL_DYNEGA_CD_DRY_RUN:-0}"
algorithms="${FINAL_DYNEGA_CD_ALGORITHMS:-leiden,walktrap}"
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
export FINAL_DYNEGA_CD_OUT_DIR="$out_dir"
export FINAL_DYNEGA_CD_WORKERS="$workers"
export FINAL_DYNEGA_CD_REPS="$reps"
export FINAL_DYNEGA_CD_DRY_RUN="$dry_run"
export FINAL_DYNEGA_CD_ALGORITHMS="$algorithms"
log_file="$log_dir/final_dynega_bounded_cd_\$(date +%Y%m%d_%H%M%S).log"
echo "Starting final DynEGA bounded CD run at \$(date)"
echo "Output directory: $out_dir"
echo "Workers: $workers; reps: $reps; dry_run: $dry_run; algorithms: $algorithms"
echo "Log file: \$log_file"
Rscript scripts/simulations/run_final_dynega_bounded_cd.R 2>&1 | tee "\$log_file"
echo "Finished final DynEGA bounded CD run at \$(date)"
EOF
)

tmux new-session -d -s "$session" "bash -lc $(printf '%q' "$cmd")"

echo "Started tmux session: $session"
echo "Attach with: tmux attach -t $session"
echo "Logs: $log_dir/final_dynega_bounded_cd_*.log"
