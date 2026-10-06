#!/usr/bin/env bash
# install obissdm (OBIS mpaeu_msdm) from the PATCHED local clone, so the code that fits is the code that is committed.
# asserts branch msens-patches, refuses a dirty tree unless OG_ALLOW_DIRTY=1, records the sha next to the data
# (read back into every run's manifest.json).
#   scripts/og/install_obissdm.sh
set -euo pipefail

REPO=${OG_MSDM_REPO:-$HOME/Github/iobis/mpaeu_msdm}
DIR_DATA=${OG_DATA:-$HOME/_big/sdm/mpaeu}
export PATH=/usr/local/bin:/opt/homebrew/bin:$HOME/.local/bin:$PATH

branch=$(git -C "$REPO" rev-parse --abbrev-ref HEAD)
[ "$branch" = "msens-patches" ] || { echo "ERROR: $REPO is on '$branch', expected msens-patches" >&2; exit 1; }
dirty=$(git -C "$REPO" status --porcelain | wc -l | tr -d ' ')
if [ "$dirty" != "0" ] && [ "${OG_ALLOW_DIRTY:-0}" != "1" ]; then
  echo "ERROR: $REPO has $dirty uncommitted change(s); commit them (or OG_ALLOW_DIRTY=1)" >&2; exit 1
fi
sha=$(git -C "$REPO" rev-parse HEAD)

R CMD INSTALL --no-docs --no-multiarch "$REPO"

mkdir -p "$DIR_DATA"
printf '{"repo":"%s","branch":"%s","sha":"%s","dirty":%s,"installed":"%s"}\n' \
  "$REPO" "$branch" "$sha" "$([ "$dirty" = 0 ] && echo false || echo true)" "$(date '+%F %T')" > "$DIR_DATA/obissdm_install.json"
echo "obissdm installed: $branch @ $sha"
Rscript -e 'cat("obissdm", as.character(packageVersion("obissdm")), "\n")'
