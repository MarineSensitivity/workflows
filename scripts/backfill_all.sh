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
#   models    backfill_versions.qmd     BACKFILL_COGS=1  -> model COGs into the store
#   scores    publish_score_cogs.qmd                     -> metric x subregion COGs
#   cellmodel build_v7_cell_model.qmd   DEPLOY_V7_CELLMODEL=1
#                                        -> the cell-oriented twin of model_cell,
#                                           WITHOUT which a release advertises
#                                           cell_species_list:false and a clicked
#                                           cell cannot list species. ~1.2B rows per
#                                           version: slow, run deliberately.
#   manifest  backfill_versions.qmd                     -> manifest.json + serve.duckdb,
#                                           then (automatic, same render) the STAC
#                                           catalog entry and a reload of BOTH Shiny
#                                           instances, public and preview
#   index     publish_storage_index.qmd (once, not per version)
#                                        -> storage.marinesensitivity.org index pages.
#                                           Runs after `all` and after `manifest` too: a
#                                           release that landed is a bucket that changed.
#                                           A RESTRICTED release gets no pages by design
#                                           (they appear when it becomes public).
#                                           BACKFILL_NO_INDEX=1 skips it.
#
# Extra VAR=value arguments are forwarded to every render of the run, e.g.
#   scripts/backfill_all.sh --vers v7b --stage cellmodel V7_CELLMODEL_REDO=1
#
# `manifest` runs LAST and separately on purpose: a manifest is a projection of
# what exists, so regenerating it before the COGs land publishes a contract the
# data does not yet satisfy.

set -euo pipefail

cd "$(dirname "$0")/.."

VERS="v7 v6 v5 v4b v4 v3 v2 v1"
STAGE="all"
EXTRA=()   # VAR=value arguments, forwarded to every render (srv_render.sh turns them into -e)

while [ $# -gt 0 ]; do
  case "$1" in
    --vers)  VERS="$2"; shift 2 ;;
    --stage) STAGE="$2"; shift 2 ;;
    -h|--help) sed -n '1,58p' "$0"; exit 0 ;;
    [A-Z_]*=*) EXTRA+=("$1"); shift ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

# The floor is the msens whose backfill_versions.qmd records the serving grid
# (cell_grid_write) and verifies the stored tile ids against it (cell_model_tile_check) --
# without which every usa05 release ships a cell_model whose partitions the reader prunes
# away, answering "no species" for a clicked cell instead of failing.
# Bump this whenever a notebook here starts depending on newer msens logic.
export MSENS_MIN="${MSENS_MIN:-0.42.1}"

run() {  # run <ver> <label> <qmd> <output-stem> [extra args...]
  local ver="$1" label="$2" qmd="$3" stem="$4"; shift 4
  echo "--- $label $ver  $(date +%H:%M) ---"
  # --output keeps one HTML per version, so a version's run stays auditable
  # instead of each render overwriting the last.
  # the FULL output is kept: the filter below is for the console only. It used to be the only
  # copy, so when a render died the R error text was discarded with everything else that did
  # not match (2026-09-21: a cell_model stage failed and left nothing to diagnose it with).
  local log="_output/logs/backfill_${stem}_${ver}_$(date +%Y%m%dT%H%M%S).log"
  mkdir -p _output/logs
  scripts/srv_render.sh "$qmd" -P "ver:$ver" --output "${stem}_${ver}.html" "$@" ${EXTRA[@]+"${EXTRA[@]}"} \
    2>&1 | tee "$log" | grep -E "INFO|WARN|ERROR|Error|Output created|at [0-9a-f]{7}|msens" | sed 's/^/    /'
  echo "    full log: $log"
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
  # NOT in `all`: this is the single largest compute item in the pipeline and
  # should be asked for explicitly, one version at a time.
  case "$STAGE" in
    cellmodel)    run "$V" "cell_model"  build_v7_cell_model.qmd build_v7_cell_model \
                      DEPLOY_V7_CELLMODEL=1 ;;
  esac
done

# Manifests last, and in their own pass -- see the header note.
for V in $VERS; do
  case "$STAGE" in
    all|manifest) run "$V" "manifest"    backfill_versions.qmd  backfill_versions ;;
  esac
done

# The browsable index, ONCE for the whole bucket (the notebook takes no `ver`): a release that
# landed is a bucket that changed, and the pages are a projection of it exactly as a manifest is.
case "$STAGE" in
  all|manifest|index)
    if [ -z "${BACKFILL_NO_INDEX:-}" ]; then
      echo "--- storage index  $(date +%H:%M) ---"
      log="_output/logs/backfill_publish_storage_index_$(date +%Y%m%dT%H%M%S).log"
      mkdir -p _output/logs
      scripts/srv_render.sh publish_storage_index.qmd ${EXTRA[@]+"${EXTRA[@]}"} \
        2>&1 | tee "$log" | grep -E "INFO|WARN|ERROR|Error|Output created|at [0-9a-f]{7}|msens" | sed 's/^/    /'
      echo "    full log: $log"
    fi ;;
esac

echo "==> done $(date +%H:%M).  store: usa05=$(store_count usa05) global05=$(store_count global05) objects"
echo "    verify a manifest actually gained what this run should have added, e.g.:"
echo "    curl -s https://storage.marinesensitivity.org/marine-atlas/v7/manifest.json | jq '.zone_sets, (.metrics|length)'"
