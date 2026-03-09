#!/usr/bin/env bash
# deploy_to_server.sh — sync versioned derived data + app code to msens server
#
# usage:
#   ./libs/deploy_to_server.sh v4            # sync data + pull apps
#   ./libs/deploy_to_server.sh v4 data       # sync data only
#   ./libs/deploy_to_server.sh v4 apps       # pull apps only
set -euo pipefail

VER="${1:?usage: deploy_to_server.sh <version> [data|apps|all]}"
MODE="${2:-all}"

# ssh config ----
SSH_KEY="$HOME/My Drive/private/msens_key_pair.pem"
SSH_HOST="ubuntu@msens1.marinesensitivity.org"

# local paths ----
DIR_V="$HOME/My Drive/projects/msens/data/derived/${VER}/"
DB_BIG="$HOME/_big/msens/derived/${VER}/sdm.duckdb"
TIF_SHARED="$HOME/My Drive/projects/msens/data/derived/r_bio-oracle_planarea.tif"

# remote paths ----
REMOTE_DERIVED="/share/data/derived"
REMOTE_BIG="/share/data/big"

# sync data ----
if [[ "$MODE" == "all" || "$MODE" == "data" ]]; then
  REMOTE_DB="$REMOTE_BIG/${VER}/sdm.duckdb"
  REMOTE_DB_NEW="${REMOTE_DB}.new"

  # 1. rsync all files to server ----
  echo "=== syncing derived/${VER}/ (small files) ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    --exclude='*.aux.xml' \
    "$DIR_V" \
    "$SSH_HOST:$REMOTE_DERIVED/${VER}/"

  echo "=== ensuring remote dir ${REMOTE_BIG}/${VER} exists ==="
  ssh -i "$SSH_KEY" "$SSH_HOST" "sudo mkdir -p ${REMOTE_BIG}/${VER} && sudo chown ubuntu:staff ${REMOTE_BIG}/${VER} && sudo chmod g+s ${REMOTE_BIG}/${VER}"

  echo "=== syncing sdm.duckdb to staging file ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    --exclude='*.wal' \
    "$DB_BIG" \
    "$SSH_HOST:${REMOTE_DB_NEW}"

  echo "=== syncing shared input raster ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    "$TIF_SHARED" \
    "$SSH_HOST:$REMOTE_DERIVED/"

  echo "=== syncing pmtiles ==="
  rsync -avz --progress \
    -e "ssh -i \"$SSH_KEY\"" \
    "$HOME/_big/msens/derived/${VER}/pmtiles/" \
    "$SSH_HOST:$REMOTE_DERIVED/${VER}/pmtiles/"

  # 2. fix permissions + swap duckdb + restart rstudio ----
  # shiny needs staff group to read data; setgid ensures new files inherit staff;
  # rstudio restart picks up new group membership and clean duckdb
  echo "=== fixing permissions, swapping duckdb, restarting rstudio ==="
  ssh -i "$SSH_KEY" "$SSH_HOST" bash -s <<DEPLOY
    set -euo pipefail

    # add shiny to staff group inside rstudio container (idempotent)
    docker exec rstudio usermod -aG staff shiny 2>/dev/null || true

    # fix group ownership + permissions on synced data
    sudo chgrp -R staff ${REMOTE_DERIVED} ${REMOTE_BIG}
    sudo chmod -R g+rX  ${REMOTE_DERIVED} ${REMOTE_BIG}

    # setgid on directories so new files inherit staff group
    sudo find ${REMOTE_DERIVED} -type d -exec chmod g+s {} +
    sudo find ${REMOTE_BIG}     -type d -exec chmod g+s {} +

    # fix staged duckdb permissions before swap
    sudo chgrp staff ${REMOTE_DB_NEW}
    sudo chmod g+r   ${REMOTE_DB_NEW}

    # stop rstudio, swap duckdb, delete stale WAL, restart
    cd /share/github/MarineSensitivity/server
    docker compose stop rstudio
    rm -f ${REMOTE_DB}.wal
    mv ${REMOTE_DB_NEW} ${REMOTE_DB}
    docker compose start rstudio
    echo "--- rstudio restarted with fixed permissions ---"
DEPLOY
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

  echo "=== setting up v3 legacy apps ==="
  ssh -i "$SSH_KEY" "$SSH_HOST" bash -s <<'VERSIONING'
    set -euo pipefail

    APPS_V3="/share/github/MarineSensitivity/apps_v3"
    APPS="/share/github/MarineSensitivity/apps"

    # clone v3 branch if not present (remove stale non-git dir if needed)
    if [ ! -d "$APPS_V3/.git" ]; then
      rm -rf "$APPS_V3"
      git clone --branch v3 --single-branch \
        https://github.com/MarineSensitivity/apps.git "$APPS_V3"
      echo "--- cloned apps v3 branch ---"
    fi

    # symlinks into shiny-server root (idempotent)
    ln -sfn "$APPS_V3/mapgl" "$APPS/mapgl_v3"
    ln -sfn "$APPS_V3/mapsp" "$APPS/mapsp_v3"
    echo "--- v3 symlinks ready ---"
VERSIONING

  echo "=== pulling server config + reloading caddy ==="
  ssh -i "$SSH_KEY" "$SSH_HOST" bash -s <<'REMOTE'
    cd /share/github/MarineSensitivity/server
    git fetch origin
    git pull origin main
    docker compose exec caddy caddy reload --config /etc/caddy/Caddyfile
    echo "--- caddy reloaded ---"
REMOTE
fi
