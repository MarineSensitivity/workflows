#!/usr/bin/env bash
# one og run from the mpaeu_sdm clone, for mm_run.sh (which flattens quoting, so env goes in as args):
#   scripts/mm_run.sh ogc_159023 scripts/og/run_one.sh ogc 159023            # fit, OBIS settings
#   scripts/mm_run.sh ogcb_159023 scripts/og/run_one.sh ogc 159023 bootstrap
# optional env passthrough: OG_OUT OG_CORES OG_ALGOS OG_SCENARIOS
set -euo pipefail
export PATH=/usr/local/bin:/opt/homebrew/bin:$HOME/.local/bin:$PATH
[ $# -ge 2 ] || { echo "usage: $0 <acro> <species[,species]> [fit|bootstrap]" >&2; exit 2; }
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
export OG_ACRO=$1 OG_SPECIES=$2 OG_STEP=${3:-fit}
cd "${OG_SDM_REPO:-$HOME/Github/iobis/mpaeu_sdm}"
[ "$(git rev-parse --abbrev-ref HEAD)" = "msens-patches" ] || { echo "mpaeu_sdm not on msens-patches" >&2; exit 1; }
exec Rscript "$HERE/run_og.R"
