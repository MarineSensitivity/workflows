#!/usr/bin/env bash
# Render build_app_bundle.qmd ONCE PER VERSION, sequentially, one `quarto render`
# per version (never the whole registry in a single APP_BUNDLE_VERS list, so one
# version's failure can never abort another's evidence). Committed so the loop is
# part of the process (CLAUDE.md: "the orchestration is part of the process --
# commit the loop too").
#
# NEVER run this in the foreground and wait on it: a legacy release takes
# ~90-190s, v8/v9 longer, and this notebook must not be waited on in a chat turn
# (atlas-contract round 4 -- three foreground waits on this exact render already
# got a session cut off). Start it backgrounded and poll the exit-codes file:
#
#   nohup scripts/render_app_bundle.sh v1 v2 v3 v4 v4b v5 v6 v7 v7b v8 v9 \
#     > _output/logs/render_app_bundle_driver.log 2>&1 &
#   cat _output/logs/render_app_bundle_exit_codes.txt   # "<ver> <exit code>", appended as each finishes
#   tail -f _output/logs/render_app_bundle_<ver>.log     # one version's own render log
#
# `quarto render build_app_bundle.qmd` -- with or without an explicit --output --
# always writes its HTML beside the source (./build_app_bundle.html), never into
# _output/, despite this project's _quarto.yml `output-dir: _output` (confirmed
# empirically 2026-09-21: a project-wide render like scripts/run_version.sh
# respects it, an ad hoc single-file render of THIS qmd does not). This script IS
# the fix: it always moves the file itself, so a stray build_app_bundle.html /
# build_app_bundle_files/ never sits at the repo root after it runs.
set -uo pipefail   # deliberately NOT -e: one version's failure must not stop the loop
cd "$(dirname "$0")/.."

mkdir -p _output/logs
# a private, always-writable TMPDIR -- the machine's default (a /var/folders/...
# path under $TMPDIR) has been observed owned by root with mode 700 in this
# sandbox, which fails quarto's own session-tempdir creation before a single
# chunk runs ("PermissionDenied: tmpdir"). Never assume the ambient TMPDIR works.
RENDER_TMPDIR="/tmp/atlas_contract_render_tmp"
mkdir -p "$RENDER_TMPDIR"

# EXIT_FILE is override-able (a publish turn is 2+ separate invocations of this
# script run one after another -- app/-only, then v7's cell_model, then v7b's --
# and the default name is truncated fresh on EVERY start, so a second invocation
# would silently erase the first invocation's result unless each names its own).
EXIT_FILE="${EXIT_FILE:-_output/logs/render_app_bundle_exit_codes.txt}"
: > "$EXIT_FILE"   # fresh file every driver run -- a stale line from a prior run must never be mistaken for this one's result

if [ "$#" -eq 0 ]; then
  echo "usage: $0 <ver> [ver ...]" >&2
  exit 2
fi

for ver in "$@"; do
  log="_output/logs/render_app_bundle_${ver}.log"
  echo "==> $ver  ($(date +%H:%M:%S))"
  APP_BUNDLE_VERS="$ver" R_LIBS=".Rlib" TMPDIR="$RENDER_TMPDIR" \
    quarto render build_app_bundle.qmd > "$log" 2>&1
  code=$?
  if [ "$code" -eq 0 ] && [ -f build_app_bundle.html ]; then
    mv -f build_app_bundle.html "_output/build_app_bundle_${ver}.html"
  fi
  rm -rf build_app_bundle_files   # the shared, unversioned quarto-html support dir -- never left at the root
  echo "$ver $code" >> "$EXIT_FILE"
  echo "==> $ver exit=$code  ($(date +%H:%M:%S))"
done
echo "==> driver done $(date +%H:%M:%S)"
