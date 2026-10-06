#!/usr/bin/env bash
# the seven og species (six sea turtles + north atlantic right whale), fit then bootstrap, on surface layers
# with a target-group background. everything else as OBIS ran it (no pinning: own thinning, own tuning).
#   scripts/mm_run.sh og_seven  scripts/og/run_seven.sh og                                      # background = the species' order (tg_map.csv)
#   scripts/mm_run.sh ogm_seven scripts/og/run_seven.sh ogm ~/_big/sdm/og_inputs/tg_mega.parquet # background = air-breathing megafauna
# env: OG_OUT (default ~/_big/sdm/og), OG_CORES (default 7), OG_IDS (a subset, comma-separated).
# the bootstrap runs even when a species has no model, and the fit's exit code is kept.
# one acronym at a time: thinning holds ~6 GB per large species, the fit ~5 GB per worker.
set -euo pipefail

main() {
  [ $# -ge 1 ] || { echo "usage: $0 <acro> [<target-group file or map csv>]" >&2; exit 2; }
  local here; here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
  local acro=$1
  # loggerhead green hawksbill kemp's leatherback olive-ridley  n-atlantic-right-whale
  local ids=${OG_IDS:-137205,137206,137207,137208,137209,220293,159023}
  export OG_OUT=${OG_OUT:-$HOME/_big/sdm/og} OG_CORES=${OG_CORES:-7}
  export OG_HAB_DEPTH=depthsurf OG_BG_FROM=${2:-$HOME/_big/sdm/og_inputs/tg_map.csv}
  unset OG_HYP_FROM OG_FITOCC_FROM
  [ -f "$OG_BG_FROM" ] || { echo "target-group file not found: $OG_BG_FROM (scripts/og/build_target_group.R)" >&2; exit 1; }

  local rc=0
  echo "== $acro fit $(date '+%F %T') bg $OG_BG_FROM"
  "$here/run_one.sh" "$acro" "$ids" fit || rc=$?
  echo "== $acro fit exit $rc $(date '+%F %T')"
  "$here/run_one.sh" "$acro" "$ids" bootstrap || rc=$?
  echo "== $acro bootstrap done, exit $rc $(date '+%F %T')"
  return $rc
}
main "$@"; exit
