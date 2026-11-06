#!/usr/bin/env bash
# launch the v11 rebuild once NO R process has been rendering in a CalCOFI checkout for 3 consecutive minutes
# (their candidates sit at ~26 GB and two such jobs thrash the 48 GB mini; a memory gauge missed one that had
# started 40 s earlier and was still small). The CalCOFI sequencers in turn wait for _output/logs/v11_rebuild.done,
# which this script removes before launching and writes (with the exit code) when run_version.sh returns.
# The merge knobs are the ones scripts/attribution_runs.sh uses on this box: the one-pass mc_parts repartition
# spilled past the 400 GB default cap at 08:55Z (MERGE_PARTS_CHUNKS=16 bounds the spill, identical output).
cd "$(dirname "$0")/.."
g=_output/logs/v11_rebuild_gate.txt
rm -f _output/logs/v11_rebuild.done
echo "gate: waiting for a CalCOFI-free mini ($(date -u +%H:%MZ))" > "$g"
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
    FOLD_DENSITY_CAL=suit SHARE_BASIS=cell RANGE_FILL=mean NATIVE_PRIOR_FROM=v10 \
    MERGE_PARTS_CHUNKS=16 MERGE_MEM_GB=20 MERGE_TEMP_GB=900 BUILD_MEMORY_GB=22 BUILD_THREADS=8 TMPDIR="$PWD/.tmp" \
    scripts/run_version.sh --from merge_models --to release_s3 --variant v11 > _output/logs/v11_rebuild.log 2>&1
echo "exit $?" > _output/logs/v11_rebuild.done
