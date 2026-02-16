#!/usr/bin/env bash
# deploy_to_server.sh — sync versioned derived data + app code to msens server
#
# usage:
#   ./libs/deploy_to_server.sh v3            # sync data + pull apps
#   ./libs/deploy_to_server.sh v3 data       # sync data only
#   ./libs/deploy_to_server.sh v3 apps       # pull apps only
set -euo pipefail

VER="${1:?usage: deploy_to_server.sh <version> [data|apps|all]}"
MODE="${2:-all}"

# ssh config ----
SSH_KEY="$HOME/My Drive/private/msens_key_pair.pem"
SSH_HOST="ubuntu@msens1.marinesensitivity.org"

# local paths ----
DIR_V="$HOME/My Drive/projects/msens/data/derived/${VER}/"
DIR_BIG="$HOME/_big/msens/derived/${VER}/"
DIR_SHARED="$HOME/My Drive/projects/msens/data/derived/r_bio-oracle_planarea.tif"

# remote paths ----
REMOTE_DERIVED="/share/data/derived"
REMOTE_BIG="/share/data/big"

# sync data ----
if [[ "$MODE" == "all" || "$MODE" == "data" ]]; then
  echo "=== syncing derived/${VER}/ (small files) ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    --exclude='*.aux.xml' \
    "$DIR_V" \
    "$SSH_HOST:$REMOTE_DERIVED/${VER}/"

  echo "=== syncing _big/${VER}/ (sdm.duckdb) to /share/data/big/${VER}/ ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    "$DIR_BIG" \
    "$SSH_HOST:$REMOTE_BIG/${VER}/"

  echo "=== syncing shared input raster ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    "$DIR_SHARED" \
    "$SSH_HOST:$REMOTE_DERIVED/"
fi

# pull app code ----
if [[ "$MODE" == "all" || "$MODE" == "apps" ]]; then
  echo "=== pulling apps on server ==="
  ssh -i "$SSH_KEY" "$SSH_HOST" bash -s <<'REMOTE'
    cd /share/github/MarineSensitivity/apps
    echo "--- $(pwd) ---"
    git fetch origin
    git checkout main
    git pull origin main
    echo "=== done ==="
REMOTE
fi
