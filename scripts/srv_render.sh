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

# Split trailing args: VAR=value -> docker env; everything else -> quarto.
# This is what lets a version loop (backfill_all.sh) go through THIS script,
# with its git guard, instead of calling `docker exec` directly and rendering
# whatever the server happens to hold.
envs=""
qargs=""
for a in "$@"; do
  case "$a" in
    [A-Z_]*=*) envs="$envs -e $a" ;;
    *)         qargs="$qargs $a"  ;;
  esac
done

# A notebook whose LOGIC lives in msens is only as current as the installed
# package. MSENS_MIN guards that: the server reported 0.14.0 while running a
# manifest_build() that predated zone PMTiles, so manifests regenerated there
# came out silently missing them. Same version number, different code -- which
# is exactly what the NEWS/Version rule exists to make impossible.
minver="${MSENS_MIN:-}"

# the render is killed if the HOST's MemAvailable falls below this (see the watchdog below)
minavail="${SRV_MIN_AVAIL_MB:-1200}"

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

  if [ -n '$minver' ]; then
    have=\$(docker exec rstudio Rscript -e 'cat(as.character(packageVersion(\"msens\")))' 2>/dev/null)
    if ! docker exec rstudio Rscript -e 'q(status = as.integer(packageVersion(\"msens\") < \"$minver\"))' 2>/dev/null; then
      echo \"ERROR: server msens \$have < required $minver. Reinstall it there first:\" >&2
      echo \"       docker exec rstudio Rscript -e 'devtools::install(\\\"/share/github/MarineSensitivity/msens\\\")'\" >&2
      exit 1
    fi
    echo \"    msens \$have (>= $minver)\"
  fi

  # MEMORY WATCHDOG. msens1 is the PRODUCTION box: 16 GB, no swap, shared with the apps,
  # the API, two titilers and ERDDAP, and the rstudio container has no memory cap. With no
  # swap the kernel does not OOM-kill promptly -- it evicts file pages and thrashes. On
  # 2026-09-21 a render at a hardcoded 12 GB DuckDB limit took MemAvailable to 175 MB and the
  # host to load 100: sshd unreachable, /scores, /species and STAC timing out for ~25 min,
  # until the render was killed by hand. Notebooks now size DuckDB to the machine
  # (libs/duckdb_budget.R); this is the second line of defence for the ones that do not.
  # A render that pushes the HOST below SRV_MIN_AVAIL_MB is killed (exit 137) while the box
  # can still answer. Reproducibility beats uptime -- but not by taking the site down.
  docker exec -u $UIDGID -w '$REPO'$envs rstudio quarto render '$QMD'$qargs &
  rpid=\$!
  flag=/tmp/srv_render_lowmem.\$rpid
  (
    while kill -0 \$rpid 2>/dev/null; do
      avail=\$(awk '/^MemAvailable:/ {print int(\$2/1024)}' /proc/meminfo)
      if [ \"\$avail\" -lt $minavail ]; then
        echo \"ERROR: host MemAvailable \${avail} MB < $minavail MB -- killing the render of $QMD\" >&2
        touch \$flag
        # [.] keeps pkill -f from matching THIS shell, whose command line contains the
        # pattern text (the first version killed itself and left the render running).
        # Loop: at the moment of the first kill R may not have started yet.
        n=0
        while kill -0 \$rpid 2>/dev/null && [ \$n -lt 60 ]; do
          pkill -9 -u 1000 -f 'quarto/share/rmd/rmd[.]R' || true
          pkill -9 -u 1000 -f 'quarto[.]js render'       || true
          n=\$((n + 1)); sleep 1
        done
        kill -9 \$rpid 2>/dev/null || true
        break
      fi
      sleep 5
    done
  ) &
  wpid=\$!
  rc=0; wait \$rpid || rc=\$?
  kill \$wpid 2>/dev/null || true
  if [ -e \$flag ]; then
    rm -f \$flag
    echo \"ERROR: render killed by the memory watchdog (SRV_MIN_AVAIL_MB=$minavail). Give the notebook\" >&2
    echo \"       a machine-sized DuckDB budget (libs/duckdb_budget.R) rather than raising the floor.\" >&2
    exit 137
  fi
  [ \$rc -eq 0 ] || exit \$rc

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
