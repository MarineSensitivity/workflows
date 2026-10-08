#!/usr/bin/env bash
# the loggerhead (137205) on the megafauna target-group background, fit then bootstrap, two ways:
#   scripts/mm_run.sh ogm_logger  scripts/og/run_logger_ogm.sh ogm      # own thinning (24,316 pts, ~15 h fit + 5 h bootstrap)
#   scripts/mm_run.sh ogmp_logger scripts/og/run_logger_ogm.sh ogmp pin # pinned to OBIS's published fit points (1,160 pts, ~1 h)
# the og run of 2026-10-07 (order-level turtle background) is not a model: its background is the species' own
# records (held-out ensemble AUC 0.42). the pinned run is the quick card; the own-thinning run makes the seven
# species comparable under ogm.
set -euo pipefail

main() {
  [ $# -ge 1 ] || { echo "usage: $0 <acro> [pin]" >&2; exit 2; }
  local here; here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
  local acro=$1
  export OG_OUT=${OG_OUT:-$HOME/_big/sdm/og} OG_CORES=${OG_CORES:-4}
  export OG_HAB_DEPTH=depthsurf OG_BG_FROM=$HOME/_big/sdm/og_inputs/tg_mega.parquet
  unset OG_HYP_FROM OG_FITOCC_FROM
  [ "${2:-}" = pin ] && export OG_FITOCC_FROM=$HOME/_big/sdm/obis/species
  [ -f "$OG_BG_FROM" ] || { echo "target-group file not found: $OG_BG_FROM" >&2; exit 1; }

  local rc=0
  echo "== $acro fit $(date '+%F %T') bg $OG_BG_FROM fitocc ${OG_FITOCC_FROM:-own}"
  "$here/run_one.sh" "$acro" 137205 fit || rc=$?
  echo "== $acro fit exit $rc $(date '+%F %T')"
  "$here/run_one.sh" "$acro" 137205 bootstrap || rc=$?
  echo "== $acro bootstrap done, exit $rc $(date '+%F %T')"
  return $rc
}
main "$@"; exit
