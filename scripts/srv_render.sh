#!/usr/bin/env bash
# Render a workflows notebook ON THE SERVER, as the right user.
#
# WHY THIS EXISTS
#
# `docker exec rstudio ...` runs as ROOT. The image's USER is root because
# RStudio Server's init needs it, and `docker exec` inherits that unless told
# otherwise. Every server-side render therefore wrote root-owned files into
# /share, which is bind-mounted from the host and owned by `ubuntu`.
#
# The damage is silent until git touches it: a `git merge` in the workflows
# checkout aborts with "unable to unlink ... Permission denied", because
# unlinking a file needs write permission on its CONTAINING DIRECTORY, and the
# nested `_files/` directories a render creates were root-owned too. A sweep on
# 2026-08-10 found 23,729 root-owned files under /share/data, including pipeline
# inputs like r_cellid.tif that a non-root render could not have overwritten.
#
# The uids are NOT misaligned: the container's `rstudio` user is already
# uid 1000 / gid 1000, exactly matching host `ubuntu`. The fix is simply to ask
# for it. `-u 1000:1000` rather than `-u rstudio` because docker-compose sets
# DEFAULT_USER=admin, so the account NAME changes on the next container
# recreate while the uid does not.
#
# USAGE
#   scripts/srv_render.sh build_zone_sets.qmd
#   scripts/srv_render.sh score_cell_metrics.qmd REDO_SCORES=1
#
# Env assignments after the notebook are passed through to the render.

set -euo pipefail

QMD="${1:?usage: srv_render.sh <notebook.qmd> [VAR=value ...]}"
shift || true

HOST="${MSENS_HOST:-msens}"
REPO="${MSENS_REPO:-/share/github/MarineSensitivity/workflows}"
UIDGID="1000:1000"

envs=""
for kv in "$@"; do envs="$envs -e $kv"; done

echo "==> rendering $QMD on $HOST as uid $UIDGID"

# shellcheck disable=SC2029  # $QMD/$envs are meant to expand locally
ssh "$HOST" "set -e
  cd '$REPO'

  # Render the COMMITTED notebook, never whatever the server happens to hold.
  # Without this the first version of this script silently rendered a checkout
  # two commits behind and reported success -- the exact class of bug that makes
  # a 'reproducible' render worthless.
  git fetch --quiet origin
  if ! git merge --ff-only origin/main >/dev/null 2>&1; then
    echo \"ERROR: cannot fast-forward to origin/main. Local commits or dirty tracked\" >&2
    echo \"       files on the server; resolve there before rendering.\" >&2
    git status --short | head -10 >&2
    exit 1
  fi
  echo \"    at \$(git rev-parse --short HEAD)\"

  docker exec -u $UIDGID -w '$REPO'$envs rstudio quarto render '$QMD'

  # Belt and braces: a render can still shell out to something that escalates,
  # and one root-owned file is enough to wedge the next git operation. Report it
  # loudly rather than leaving it to be discovered as a failed merge days later.
  bad=\$(find . -path ./.git -prune -o -user root -print 2>/dev/null | wc -l)
  if [ \"\$bad\" -gt 0 ]; then
    echo \"WARNING: \$bad root-owned path(s) left in the checkout; fixing\" >&2
    sudo chown -R ubuntu:ubuntu . 2>/dev/null || true
  fi
"

echo "==> done. Commit the rendered _output/ from the server, or fetch it:"
echo "    git remote add msens-server $HOST:$REPO && git fetch msens-server main"
