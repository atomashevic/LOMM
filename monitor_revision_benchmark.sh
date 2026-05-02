#!/usr/bin/env bash
set -euo pipefail

interval="${1:-300}"
out_dir="${2:-benchmark_monitoring}"
mkdir -p "$out_dir"

log="$out_dir/resource_monitor_$(date +%Y%m%d-%H%M%S).log"

while true; do
  {
    echo "===== $(date --iso-8601=seconds) ====="
    echo "--- uptime ---"
    uptime
    echo "--- memory ---"
    free -h
    echo "--- vmstat ---"
    vmstat 1 2 | tail -n 1
    echo "--- benchmark processes ---"
    ps -eo pid,ppid,psr,pcpu,pmem,rss,etimes,stat,cmd --sort=-pcpu |
      awk 'NR==1 || /Rscript|gimme|graphicalVAR|dynEGA|run_method_benchmark/ {print}'
    echo "--- checkpoint rows ---"
    find . -maxdepth 2 -name checkpoint_results.csv -printf '%p ' -exec sh -c 'wc -l < "$1"' _ {} \; 2>/dev/null |
      sort
    echo
  } | tee -a "$log"

  sleep "$interval"
done
