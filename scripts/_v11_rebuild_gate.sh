#!/usr/bin/env bash
# wait for the CalCOFI ctd-noaa candidate's marker, then until NO R process is rendering in a CalCOFI checkout for
# 3 consecutive minutes (their candidates sit at ~26 GB and two such jobs thrash the 48 GB mini; a memory gauge
# missed one that had started 40 s earlier and was still small), then launch the v11 rebuild.
cd "$(dirname "$0")/.."
m=~/_big/calcofi/logs/cand_ctd-noaa.done
g=_output/logs/v11_rebuild_gate.txt
echo "gate: waiting for $m ($(date -u +%H:%MZ))" > "$g"
until [ -f "$m" ]; do sleep 60; done
echo "marker seen $(date -u +%H:%MZ); waiting for memory headroom" >> "$g"
calcofi_r() {   # R processes rendering in a CalCOFI checkout (their candidates sit at ~26 GB whatever ps RSS says)
  for p in $(pgrep -x R); do lsof -p "$p" 2>/dev/null | awk '$4=="cwd" && $NF ~ /\/CalCOFI\//{print}' ; done | wc -l | tr -d ' '
}
quiet=0
while [ "$quiet" -lt 3 ]; do
  n=$(calcofi_r)
  if [ "${n:-0}" -gt 0 ]; then quiet=0; echo "  $(date -u +%H:%MZ) $n CalCOFI R process(es) -- holding" >> "$g"; else quiet=$((quiet+1)); fi
  sleep 60
done
echo "gate opened $(date -u +%H:%MZ)" >> "$g"
env TURTLE_SUIT=ogm TURTLE_RULE=dps OCC_RULE=1 REGION_WEIGHT=1 FOLD_DENSITY=1 VGPM_INTEGRATED=1 COMPONENTS_V11=1 NC_ATL_SOURCE=2023 \
    FOLD_DENSITY_CAL=suit SHARE_BASIS=cell RANGE_FILL=mean BUILD_MEMORY_GB=16 BUILD_THREADS=6 NATIVE_PRIOR_FROM=v10 TMPDIR="$PWD/.tmp" \
    scripts/run_version.sh --from merge_models --to release_s3 --variant v11 > _output/logs/v11_rebuild.log 2>&1
echo "exit $?" > _output/logs/v11_rebuild.done
