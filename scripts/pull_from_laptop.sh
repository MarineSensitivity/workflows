#!/usr/bin/env bash
# Pull the pipeline inputs the Mac mini lacks from the laptop over Tailscale (`ssh laptop`).
# The mini is the always-on render machine (plan 2026-10-29 §4.9.9, D18); the laptop is offline
# much of the day, so anything a v10/v11 stage will read is copied while it is reachable.
# macOS rsync is openrsync: no --info=* flags. Idempotent (rsync -a --partial); re-run to resume.
#
#   scripts/pull_from_laptop.sh            # everything in PATHS
#   scripts/pull_from_laptop.sh v9/sdm.duckdb raw/oregonstate.edu   # a subset (paths under ~/_big/msens)
set -uo pipefail
BIG=~/_big/msens
PATHS=(
  derived/v9/sdm.duckdb                       # v9 scoring db (acceptance shape for the share family; v11 base checks)
  derived/v9/marine-atlas/merge.duckdb
  derived/v9/marine-atlas/dist_merged_er      # per-cell ER of the dps taxa
  derived/v1                                  # ply_boem-usa.gpkg + v1 zone polygons (libs/paths.R ply_usa_gpkg)
  derived/aquamaps                            # am.duckdb + bilinear weights (a merge re-run in v11)
  derived/preview                             # swfsc_cce regrid (v11 Q6)
  raw/oregonstate.edu                         # VGPM annual means (score_cell_metrics primprod; v11 Q4)
  raw/swot_seamap.env.duke.edu
  raw/obis-maps                               # v11 Q3 turtle ogm/mpaeu checks
  raw/obis-products
  raw/fws.gov                                 # v11 re-ingests
  raw/marinespecies.org
  raw/BOTW_2024_2.gpkg
)
# Sources that live OUTSIDE ~/_big/msens on the laptop (Google Drive mirrors): "<laptop path>|<path under ~/_big/msens>".
# The gm ingest's GM_SHP_DIR and the cce preview's CCE_GDB point at these on the mini (libs/paths.R).
DRIVE_PATHS=(
  "My Drive/projects/msens/data/raw/ncei.noaa.gov - GoMex cetacean & sea turtle SDMs|raw/sdm/gm"            # v11 Q6: SEFSC Gulf cetacean shapefiles + spp_gmx.xlsx (718 MB)
  "Library/CloudStorage/GoogleDrive-ben@ecoquants.com/My Drive/projects/msens/data/raw/swfsc.noaa.gov|raw/sdm/swfsc"   # v11 Q6: Becker 2020b CCE gdb
)
if [ $# -gt 0 ]; then
  if [ "$1" = "--drive" ]; then PATHS=(); else PATHS=("$@"); DRIVE_PATHS=(); fi
fi
rc=0
for m in "${DRIVE_PATHS[@]}"; do
  m="${m%%#*}"; m="$(echo "$m" | xargs)"; [ -z "$m" ] && continue
  rp="${m%%|*}"; lp="${m##*|}"
  mkdir -p "$BIG/$lp"
  echo "==> drive: $rp -> $lp  ($(date +%H:%M))"
  rsync -a --partial "laptop:\"$HOME/$rp/\"" "$BIG/$lp/" || { echo "!! $lp failed (rsync exit $?)"; rc=1; }
done
for p in ${PATHS[@]+"${PATHS[@]}"}; do   # set -u safe when --drive emptied PATHS
  p="${p%%#*}"; p="$(echo "$p" | xargs)"; [ -z "$p" ] && continue
  mkdir -p "$BIG/$(dirname "$p")"
  if ssh -o BatchMode=yes laptop "test -d ~/_big/msens/$p"; then src="laptop:~/_big/msens/$p/"; dst="$BIG/$p/"
  else src="laptop:~/_big/msens/$p"; dst="$BIG/$p"; fi
  echo "==> $p  ($(date +%H:%M))"
  rsync -a --partial "$src" "$dst" || { echo "!! $p failed (rsync exit $?)"; rc=1; }
done
echo "==> done $(date +%H:%M) rc=$rc"
exit $rc
