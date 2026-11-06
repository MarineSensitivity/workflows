#!/usr/bin/env bash
# wait for the CalCOFI ctd-noaa candidate's marker, then for memory headroom (no other R process above 8 GB in
# `top` for 3 consecutive minutes -- CalCOFI candidates sit at ~26 GB and two such jobs thrash the 48 GB mini),
# then launch the v11 rebuild. ps RSS undercounts compressed pages, so `top` is the gauge the CalCOFI sessions use.
cd "$(dirname "$0")/.."
m=~/_big/calcofi/logs/cand_ctd-noaa.done
g=_output/logs/v11_rebuild_gate.txt
echo "gate: waiting for $m ($(date -u +%H:%MZ))" > "$g"
until [ -f "$m" ]; do sleep 60; done
echo "marker seen $(date -u +%H:%MZ); waiting for memory headroom" >> "$g"
big_r() {   # GB of the largest R process that is not ours (none of ours run before the launch)
  top -l 1 -stats mem,command -o mem 2>/dev/null | awk '$2 ~ /^(R|rsession)$/ {print $1}' | head -1 | \
    awk '{v=$1; u=substr(v,length(v)); n=substr(v,1,length(v)-1); if (u=="G") print n; else if (u=="M") print n/1024; else print 0}'
}
quiet=0
while [ "$quiet" -lt 3 ]; do
  b=$(big_r); b=${b:-0}
  if awk -v b="$b" 'BEGIN{exit !(b > 8)}'; then quiet=0; echo "  $(date -u +%H:%MZ) another R at ${b} GB -- holding" >> "$g"; else quiet=$((quiet+1)); fi
  sleep 60
done
echo "gate opened $(date -u +%H:%MZ)" >> "$g"
env TURTLE_SUIT=ogm TURTLE_RULE=dps OCC_RULE=1 REGION_WEIGHT=1 FOLD_DENSITY=1 VGPM_INTEGRATED=1 COMPONENTS_V11=1 NC_ATL_SOURCE=2023 \
    FOLD_DENSITY_CAL=suit SHARE_BASIS=cell RANGE_FILL=mean BUILD_MEMORY_GB=16 BUILD_THREADS=6 NATIVE_PRIOR_FROM=v10 TMPDIR="$PWD/.tmp" \
    scripts/run_version.sh --from merge_models --to release_s3 --variant v11 > _output/logs/v11_rebuild.log 2>&1
echo "exit $?" > _output/logs/v11_rebuild.done
