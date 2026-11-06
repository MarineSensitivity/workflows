#!/usr/bin/env bash
# wait for the CalCOFI ctd-noaa candidate to finish (its .done marker), then launch the v11 rebuild
cd "$(dirname "$0")/.."
m=~/_big/calcofi/logs/cand_ctd-noaa.done
echo "gate: waiting for $m ($(date -u +%H:%MZ))" > _output/logs/v11_rebuild_gate.txt
until [ -f "$m" ]; do sleep 60; done
echo "gate opened $(date -u +%H:%MZ)" >> _output/logs/v11_rebuild_gate.txt
sleep 120   # let its light preview build start before we load the machine
env TURTLE_SUIT=ogm TURTLE_RULE=dps OCC_RULE=1 REGION_WEIGHT=1 FOLD_DENSITY=1 VGPM_INTEGRATED=1 COMPONENTS_V11=1 NC_ATL_SOURCE=2023 \
    FOLD_DENSITY_CAL=suit SHARE_BASIS=cell RANGE_FILL=mean BUILD_MEMORY_GB=16 BUILD_THREADS=6 NATIVE_PRIOR_FROM=v10 TMPDIR="$PWD/.tmp" \
    scripts/run_version.sh --from merge_models --to release_s3 --variant v11 > _output/logs/v11_rebuild.log 2>&1
echo "exit $?" > _output/logs/v11_rebuild.done
