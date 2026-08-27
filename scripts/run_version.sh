#!/usr/bin/env bash
# Drive the LAPTOP half of a release build in DAG order, stage by stage, with the flags a
# supersession version (v9 AquaX) needs. Committed so the loop over stages is part of the
# process (CLAUDE.md: "the orchestration is part of the process — commit the loop too").
#
#   scripts/run_version.sh                       # every stage from the top
#   scripts/run_version.sh --from merge_models   # resume at a stage
#   scripts/run_version.sh --to score_zone_metrics
#   scripts/run_version.sh --control             # AX_SUPERSEDE=0: the control merge/score (validate-sdm)
#   scripts/run_version.sh --stage merge_models  # exactly one stage
#
# Each stage is `quarto render <qmd>` (the tracked _output/*.html + content-hash checkpoint), never
# purl+source. Flags per stage are explicit below; a stage that fails stops the run. Logs go to
# _output/logs/<stage>.log (untracked). The server half (DEPLOY_* flags) stays in
# release_marine-atlas.qmd — see the publish-sdm skill for the order.
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p _output/logs

FROM=""; TO=""; ONLY=""; CONTROL=0
while [ $# -gt 0 ]; do
  case "$1" in
    --from)    FROM="$2"; shift 2 ;;
    --to)      TO="$2";   shift 2 ;;
    --stage)   ONLY="$2"; shift 2 ;;
    --control) CONTROL=1; shift ;;
    -h|--help) sed -n '2,16p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

# stage <name> <qmd> [VAR=value ...]
STAGES=(
  "bootstrap_version   bootstrap_version.qmd"
  "build_cell_grid     build_cell_grid.qmd"
  "ingest_aquax        ingest_aquax.qmd            AX_COG=1 AX_COG_S3=1"
  "merge_models_prep   merge_models_prep.qmd"
  "merge_models        merge_models.qmd            REDO_MC_PARTS=1 REDO_MERGE=1"
  "merge_taxon         merge_taxon.qmd"
  "score_zones         score_zones.qmd             REDO_SCORE_ZONES=1"
  "score_cell_metrics  score_cell_metrics.qmd"
  "score_zone_metrics  score_zone_metrics.qmd"
  "build_registry      build_registry.qmd"
  "publish_native      publish_native.qmd          PUBLISH_MERGED_COG=1"
  "publish_score_cogs  publish_score_cogs.qmd"
  "release_stage       release_marine-atlas.qmd    RELEASE_NO_S3=1"
  "release_s3          release_marine-atlas.qmd    RELEASE_S3_TABLES=1"
  "build_version_manifest build_version_manifest.qmd"
  "publish_stac_api    publish_stac_api.qmd"
  "publish_storage_index publish_storage_index.qmd"
)

active=1; [ -n "$FROM" ] && active=0
for entry in "${STAGES[@]}"; do
  read -r name qmd envs <<<"$entry"
  [ -n "$FROM" ] && [ "$name" = "$FROM" ] && active=1
  [ -n "$ONLY" ] && { [ "$name" = "$ONLY" ] && active=1 || active=0; }
  if [ "$active" = 1 ]; then
    extra=""
    if [ "$CONTROL" = 1 ]; then
      # the control run: ax registered, nothing superseded; outputs must reproduce ver_prev's
      # Program-Area scores (cor 1.000). Its rendered HTML is the evidence -- keep it apart.
      extra="AX_SUPERSEDE=0"
    fi
    echo "==> $name  ($(date +%H:%M))  $qmd $envs $extra"
    # shellcheck disable=SC2086
    env $envs $extra quarto render "$qmd" > "_output/logs/$name.log" 2>&1 || {
      echo "!! $name FAILED — see _output/logs/$name.log" >&2
      sed 's/\x1b\[[0-9;]*m//g' "_output/logs/$name.log" | grep -E "ERROR|Error|Quitting|^!" | tail -20 >&2
      exit 1
    }
    sed 's/\x1b\[[0-9;]*m//g' "_output/logs/$name.log" | grep -E "INFO|WARN|Output created" | tail -6 | sed 's/^/    /'
  fi
  [ -n "$TO" ] && [ "$name" = "$TO" ] && break
done
echo "==> done $(date +%H:%M)"
