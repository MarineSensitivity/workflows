#!/usr/bin/env bash
# Drive the multi-version backfill across every MST release, on the server.
#
# WHY THIS EXISTS
#
# The per-version notebooks (backfill_versions.qmd, publish_score_cogs.qmd) are
# each parameterized by ONE version, which is right -- one render, one HTML,
# one auditable artifact per version. But the *loop over versions* was for a
# while a pair of throwaway scripts in /tmp on the server. That is not
# reproducible: nothing recorded which versions ran, in what order, with which
# flags, and the scripts were deleted by the next reboot.
#
# Worse, those /tmp scripts called `docker exec ... quarto render` DIRECTLY,
# bypassing srv_render.sh and therefore its `git merge --ff-only origin/main`
# guard -- the guard whose own comment warns that rendering a stale checkout
# "reports success" while producing wrong output. Two failures followed from
# exactly that class of drift:
#
#   - the v1-v7 manifests were generated against an msens whose manifest_build()
#     predated zone PMTiles, so all seven came out missing their zone tiles
#     while reporting success (hence MSENS_MIN below);
#   - a v3 run rendered against a stale sdm_db_path() and silently produced
#     nothing for that version.
#
# So: the loop is committed, it goes through srv_render.sh, and it pins the
# minimum msens the notebooks' logic requires.
#
# USAGE
#   scripts/backfill_all.sh                     # all stages, all versions
#   scripts/backfill_all.sh --stage manifest    # just regenerate manifests
#   scripts/backfill_all.sh --vers "v7 v6"      # a subset
#   scripts/backfill_all.sh --stage scores --vers v3
#
# STAGES (in dependency order; `all` runs the three in sequence)
#   models    backfill_versions.qmd  BACKFILL_COGS=1  -> model COGs into the shared store
#   scores    publish_score_cogs.qmd                  -> metric x subregion COGs
#   manifest  backfill_versions.qmd                   -> manifest.json, now citing both
#
# `manifest` runs LAST and separately on purpose: a manifest is a projection of
# what exists, so regenerating it before the COGs land publishes a contract the
# data does not yet satisfy.

set -euo pipefail

cd "$(dirname "$0")/.."

VERS="v7 v6 v5 v4b v4 v3 v2 v1"
STAGE="all"

while [ $# -gt 0 ]; do
  case "$1" in
    --vers)  VERS="$2"; shift 2 ;;
    --stage) STAGE="$2"; shift 2 ;;
    -h|--help) sed -n '1,45p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

# The floor is the msens that introduced manifest zone PMTiles + zone_cells().
# Bump this whenever a notebook here starts depending on newer msens logic.
export MSENS_MIN="${MSENS_MIN:-0.16.0}"

run() {  # run <ver> <label> <qmd> <output-stem> [extra args...]
  local ver="$1" label="$2" qmd="$3" stem="$4"; shift 4
  echo "--- $label $ver  $(date +%H:%M) ---"
  # --output keeps one HTML per version, so a version's run stays auditable
  # instead of each render overwriting the last.
  scripts/srv_render.sh "$qmd" -P "ver:$ver" --output "${stem}_${ver}.html" "$@" \
    2>&1 | grep -E "INFO|WARN|ERROR|Error|Output created|at [0-9a-f]{7}|msens" | sed 's/^/    /'
}

store_count() {
  aws s3 ls "s3://oceanmetrics.io-public/marine-atlas/cog/$1/" --recursive 2>/dev/null | wc -l | tr -d ' '
}

echo "==> stage=$STAGE  versions: $VERS  (msens >= $MSENS_MIN)"

for V in $VERS; do
  case "$STAGE" in
    all|models)   run "$V" "model COGs"  backfill_versions.qmd  backfill_versions  BACKFILL_COGS=1 ;;
  esac
  case "$STAGE" in
    all|scores)   run "$V" "score COGs"  publish_score_cogs.qmd publish_score_cogs ;;
  esac
done

# Manifests last, and in their own pass -- see the header note.
for V in $VERS; do
  case "$STAGE" in
    all|manifest) run "$V" "manifest"    backfill_versions.qmd  backfill_versions ;;
  esac
done

echo "==> done $(date +%H:%M).  store: usa05=$(store_count usa05) global05=$(store_count global05) objects"
echo "    verify a manifest actually gained what this run should have added, e.g.:"
echo "    curl -s https://storage.marinesensitivity.org/marine-atlas/v7/manifest.json | jq '.zone_sets, (.metrics|length)'"
