#!/usr/bin/env bash
# one og run from the mpaeu_sdm clone, for mm_run.sh (which flattens quoting, so env goes in as args):
#   scripts/mm_run.sh ogc_159023 scripts/og/run_one.sh ogc 159023            # fit, OBIS settings
#   scripts/mm_run.sh ogcb_159023 scripts/og/run_one.sh ogc 159023 bootstrap
# optional env passthrough: OG_OUT OG_CORES OG_ALGOS OG_SCENARIOS
# a fit is wrapped by ledger.R: the pipeline skips a species its storr calls finished and still exits 0.
# the body is one function so bash has parsed all of it before a run starts (a script edited mid-run is
# otherwise read on from the old offset); run_og.R has no such guard: do not edit it while a run is alive.
set -euo pipefail

main() {
  export PATH=/usr/local/bin:/opt/homebrew/bin:$HOME/.local/bin:$PATH
  [ $# -ge 2 ] || { echo "usage: $0 <acro> <species[,species]> [fit|bootstrap]" >&2; exit 2; }
  local here; here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
  export OG_ACRO=$1 OG_SPECIES=$2 OG_STEP=${3:-fit}
  cd "${OG_SDM_REPO:-$HOME/Github/iobis/mpaeu_sdm}"
  [ "$(git rev-parse --abbrev-ref HEAD)" = "msens-patches" ] || { echo "mpaeu_sdm not on msens-patches" >&2; exit 1; }
  [ "$OG_STEP" = fit ] && Rscript "$here/ledger.R" pre
  Rscript "$here/run_og.R"
  [ "$OG_STEP" = fit ] && Rscript "$here/ledger.R" post
  return 0
}
main "$@"; exit
