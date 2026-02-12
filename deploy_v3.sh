#!/usr/bin/env bash
# deploy_v3.sh — sync v3 derived data + app code to msens server
#
# usage:
#   ./deploy_v3.sh            # sync data + pull apps
#   ./deploy_v3.sh data       # sync data only
#   ./deploy_v3.sh apps       # pull apps only
set -euo pipefail

# ssh config ----
SSH_KEY="$HOME/My Drive/private/msens_key_pair.pem"
SSH_HOST="ubuntu@msens1.marinesensitivity.org"

# local paths ----
DIR_V3="$HOME/My Drive/projects/msens/data/derived/v3/"
DIR_BIG="$HOME/_big/msens/derived/v3/"
DIR_SHARED="$HOME/My Drive/projects/msens/data/derived/r_bio-oracle_planarea.tif"

# remote paths ----
REMOTE_DERIVED="/share/data/derived"

MODE="${1:-all}"

# sync data ----
if [[ "$MODE" == "all" || "$MODE" == "data" ]]; then
  echo "=== syncing derived/v3/ (small files) ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    --exclude='*.aux.xml' \
    "$DIR_V3" \
    "$SSH_HOST:$REMOTE_DERIVED/v3/"

  echo "=== syncing _big/v3/ (sdm.duckdb, parquet) ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    "$DIR_BIG" \
    "$SSH_HOST:$REMOTE_DERIVED/v3/"

  echo "=== syncing shared input raster ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    "$DIR_SHARED" \
    "$SSH_HOST:$REMOTE_DERIVED/"
fi

# pull app code ----
if [[ "$MODE" == "all" || "$MODE" == "apps" ]]; then
  echo "=== pulling v3 apps on server ==="
  ssh -i "$SSH_KEY" "$SSH_HOST" bash -s <<'REMOTE'
    cd /share/github/MarineSensitivity/apps_v3
    echo "--- $(pwd) ---"
    git fetch origin
    git checkout main
    git pull origin main
    echo "=== done ==="
REMOTE
fi
