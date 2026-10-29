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
#   scripts/run_version.sh --spatial --from ingest_dps_nmfs   # spatial-ER taxa only (see below)
#   scripts/run_version.sh --scoring-only                     # v10: clone ver_prev's merge + scoring outputs
#                                                             # (bootstrap) and run only the share stages
#
# Each stage is `quarto render <qmd>` (the tracked _output/*.html + content-hash checkpoint), never
# purl+source. Flags per stage are explicit below; a stage that fails stops the run. Logs go to
# _output/logs/<stage>.log (untracked). The server half (DEPLOY_* flags) stays in
# release_marine-atlas.qmd — see the publish-sdm skill for the order.
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p _output/logs

FROM=""; TO=""; ONLY=""; CONTROL=0; SPATIAL=0; SCORING_ONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --from)    FROM="$2"; shift 2 ;;
    --to)      TO="$2";   shift 2 ;;
    --stage)   ONLY="$2"; shift 2 ;;
    --control) CONTROL=1; shift ;;
    --spatial) SPATIAL=1; shift ;;
    --scoring-only) SCORING_ONLY=1; shift ;;
    -h|--help) sed -n '2,16p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

# stage <name> <qmd> [VAR=value ...]
STAGES=(
  "bootstrap_version   bootstrap_version.qmd"
  "build_cell_grid     build_cell_grid.qmd"
  "ingest_aquax        ingest_aquax.qmd            AX_COG=1"
  "ingest_dps_nmfs     ingest_nmfs-dps.qmd"
  "ingest_listings     ingest_nmfs-fws-listings.qmd"
  "merge_models_prep   merge_models_prep.qmd"
  "merge_models        merge_models.qmd            REDO_MC_PARTS=1 REDO_MERGE=1"
  "merge_taxon         merge_taxon.qmd"
  "build_common_names  build_common_names.qmd"
  "score_zones         score_zones.qmd"
  "score_cell_metrics  score_cell_metrics.qmd"
  "score_zone_metrics  score_zone_metrics.qmd"
  "build_taxon_global_total build_taxon_global_total.qmd"
  "score_share_metrics score_share_metrics.qmd    SHARE_METRICS=1"
  "build_registry      build_registry.qmd"
  "publish_native      publish_native.qmd          PUBLISH_MERGED_COG=1 REDO_MERGED_COG=1 NATIVE_SKIP_PMTILES=1"
  "publish_score_cogs  publish_score_cogs.qmd"
  "release_stage       release_marine-atlas.qmd    RELEASE_NO_S3=1 RELEASE_REDO_SERVE=1 RELEASE_REDO_CELL_MODEL=1"
  "release_s3          release_marine-atlas.qmd    RELEASE_S3_TABLES=1"
  "build_version_manifest build_version_manifest.qmd"
  "publish_stac_api    publish_stac_api.qmd"
  "publish_storage_index publish_storage_index.qmd"
)

# a SCORING-ONLY version (v10 on the v8 base): bootstrap clones ver_prev's merge + scoring outputs and
# verifies them against ver_prev's checkpoints; every ingest / merge / v8-scoring stage is skipped
# (their outputs are the clone); only the share stages and what follows run
SCORING_SKIP="build_cell_grid ingest_aquax ingest_dps_nmfs ingest_listings merge_models_prep merge_models merge_taxon build_common_names score_zones score_cell_metrics score_zone_metrics"
active=1; [ -n "$FROM" ] && active=0
for entry in "${STAGES[@]}"; do
  read -r name qmd envs <<<"$entry"
  [ -n "$FROM" ] && [ "$name" = "$FROM" ] && active=1
  [ -n "$ONLY" ] && { [ "$name" = "$ONLY" ] && active=1 || active=0; }
  if [ "$SCORING_ONLY" = 1 ] && [ "$active" = 1 ]; then
    case " $SCORING_SKIP " in *" $name "*) echo "--  $name  skipped (scoring-only: the clone of ver_prev is this stage's output)"; [ -n "$TO" ] && [ "$name" = "$TO" ] && break; continue ;; esac
    [ "$name" = bootstrap_version ] && envs="BOOTSTRAP_CLONE_MERGE=1 BOOTSTRAP_VERIFY=1"
  fi
  if [ "$active" = 1 ]; then
    extra=""
    out=""
    if [ "$SPATIAL" = 1 ]; then
      # a change confined to the spatial-ER taxa (sea turtles + NMFS DPS species: a re-ingested ER
      # surface or a merge-rule change for them): re-ingest dps_nmfs, refresh only their merge-input
      # partitions + merged surfaces, repaint only their merged COGs. Scoring and release stages run
      # in full (they are cheap and read whatever changed).
      case "$name" in
        ingest_dps_nmfs) envs="REDO_INGEST=1" ;;
        merge_models)    envs="REDO_MC_PARTS_SPATIAL=1 REDO_MERGE_SPATIAL=1" ;;
        publish_native)  envs="PUBLISH_MERGED_COG=1 REDO_MERGED_COG_SPATIAL=1 NATIVE_SKIP_PMTILES=1" ;;
      esac
    fi
    if [ "$CONTROL" = 1 ]; then
      # the control run = EVERY method flag off: AquaX EXCLUDED from the merge (merge_models_prep), so
      # the inputs are ver_prev's and merge_models' content hash must equal ver_prev's checkpoint;
      # the share family not computed (score_share_metrics proves ver_prev's rows untouched). Its
      # rendered HTML is the evidence -- written under a _control suffix so the real run cannot
      # overwrite it.
      extra="AX_SUPERSEDE=0 SHARE_METRICS=0"; out="--output ${name}_control.html"
    fi
    echo "==> $name  ($(date +%H:%M))  $qmd $envs $extra $out"
    # shellcheck disable=SC2086
    env $envs $extra quarto render "$qmd" $out > "_output/logs/$name.log" 2>&1 || {
      echo "!! $name FAILED — see _output/logs/$name.log" >&2
      sed 's/\x1b\[[0-9;]*m//g' "_output/logs/$name.log" | grep -E "ERROR|Error|Quitting|^!" | tail -20 >&2
      exit 1
    }
    sed 's/\x1b\[[0-9;]*m//g' "_output/logs/$name.log" | grep -E "INFO|WARN|Output created" | tail -6 | sed 's/^/    /'
  fi
  [ -n "$TO" ] && [ "$name" = "$TO" ] && break
done
echo "==> done $(date +%H:%M)"
