#!/usr/bin/env bash
# The v11 attribution: one pipeline run per method variant, each snapshotting its zone scores to
# data/{ver}/attribution/{variant}.parquet (snapshot_scores.qmd) for compare_versions.qmd. Committed so the loop
# over variants is part of the process (CLAUDE.md). Run on the mini in a tmux window:
#
#   scripts/attribution_runs.sh full2023 no_occ_rule ...      # named variants, in order
#   scripts/attribution_runs.sh --list                         # what each variant sets and from which stage it runs
#
# The candidate ("full2023") turns every flag on; each "no_<flag>" variant is the candidate with ONE flag at its
# control value, so a variant's delta against full2023 attributes that flag. Variants that change the merge INPUT
# (TURTLE_SUIT, FOLD_DENSITY, NC_ATL_SOURCE) re-run from merge_models_prep (repartition + merge ~2.5 h); OCC_RULE and
# REGION_WEIGHT change only the written surfaces, so they re-run the merge without the repartition
# (REDO_MERGE=1 on the existing mc_parts, ~1 h); TURTLE_RULE re-runs the spatial-ER taxa only; VGPM_INTEGRATED
# and COMPONENTS_V11 are scoring-only. NC_ATL_SOURCE=2023 needs ingest_sdm-nc rendered with that source
# beforehand (dist/nc_atl_source.txt says which set the directory holds; the variant asserts it).
set -euo pipefail
cd "$(dirname "$0")/.."
FULL="TURTLE_SUIT=ogm TURTLE_RULE=dps OCC_RULE=1 REGION_WEIGHT=1 FOLD_DENSITY=1 VGPM_INTEGRATED=1 COMPONENTS_V11=1 NC_ATL_SOURCE=2023"
BUDGET="AX_SUPERSEDE=0 MERGE_PARTS_CHUNKS=16 MERGE_MEM_GB=20 MERGE_TEMP_GB=900 BUILD_MEMORY_GB=22 BUILD_THREADS=8"
# variant -> flags (the candidate's, with the one change), first stage, extra stage envs (bash 3 on macOS: no
# associative arrays, so one function per lookup)
variant_flags() { case "$1" in
  full2023)         echo "$FULL" ;;
  no_turtle_suit)   echo "${FULL/TURTLE_SUIT=ogm/TURTLE_SUIT=am}" ;;
  no_fold_density)  echo "${FULL/FOLD_DENSITY=1/FOLD_DENSITY=0}" ;;
  nc_atl_2018)      echo "${FULL/NC_ATL_SOURCE=2023/NC_ATL_SOURCE=2018}" ;;
  no_occ_rule)      echo "${FULL/OCC_RULE=1/OCC_RULE=0}" ;;
  no_region_weight) echo "${FULL/REGION_WEIGHT=1/REGION_WEIGHT=0}" ;;
  no_turtle_rule)   echo "${FULL/TURTLE_RULE=dps/TURTLE_RULE=turtle}" ;;
  no_vgpm)          echo "${FULL/VGPM_INTEGRATED=1/VGPM_INTEGRATED=0}" ;;
  no_components)    echo "${FULL/COMPONENTS_V11=1/COMPONENTS_V11=0}" ;;
  full2023_cal)     echo "$FULL FOLD_DENSITY_CAL=mass" ;;   # the candidate + the mass-preserving density calibration (2026-10-31)
  full2023_cal_suit) echo "$FULL FOLD_DENSITY_CAL=suit" ;;  # ... calibrated to the suitability prior (er beside), the basis the fold-in's cell-mode merge uses
  full2023_basis)    echo "$FULL SHARE_BASIS=cell RANGE_FILL=presence" ;;  # Call 2 (2026-11-02): single counting for every taxon, a range cell no model covers = presence (100)
  full2023_basis_er) echo "$FULL SHARE_BASIS=cell RANGE_FILL=er" ;;        # ... the same with the dps fitting point (the governing er_score) as the fill
  *) echo "" ;; esac; }
variant_from()  { case "$1" in
  full2023|no_turtle_suit|no_fold_density|nc_atl_2018|full2023_cal|full2023_cal_suit) echo merge_models_prep ;;
  no_occ_rule|no_region_weight|no_turtle_rule|full2023_basis|full2023_basis_er) echo merge_models ;;
  no_vgpm)                                             echo score_cell_metrics ;;
  no_components)                                       echo merge_taxon ;;
  *) echo "" ;; esac; }
# the merge notebook tests nzchar(): an EMPTY value is "off", a "0" would be "on"
variant_extra() { case "$1" in
  no_occ_rule|no_region_weight|full2023_basis|full2023_basis_er) echo "REDO_MC_PARTS= REDO_MERGE=1" ;;
  no_turtle_rule)               echo "REDO_MC_PARTS= REDO_MERGE= REDO_MERGE_SPATIAL=1" ;;
  *) echo "" ;; esac; }
# ORDER MATTERS: a variant resumes on what the previous one left in derived/v11. The scoring-only and the
# spatial-only variants must follow the candidate directly (they read ITS merged surfaces); the merge-rewriting
# ones come next (each rewrites every surface); the two re-prep ones last; nc_atl_2018 needs ingest_sdm-nc
# re-rendered with NC_ATL_SOURCE=2018 in between (`--all` does that render itself).
VARIANTS="full2023 no_vgpm no_components no_turtle_rule no_occ_rule no_region_weight no_turtle_suit no_fold_density nc_atl_2018"
EXTRA_VARIANTS="full2023_cal full2023_cal_suit full2023_basis full2023_basis_er"   # follow-ups, not part of --all
if [ "${1:-}" = "--list" ]; then for v in $VARIANTS $EXTRA_VARIANTS; do echo "$v  from $(variant_from "$v")  $(variant_extra "$v")  $(variant_flags "$v")"; done; exit 0; fi
[ $# -gt 0 ] || { echo "usage: $0 [--keep-parts] <variant> [<variant> ...] | --all | --list" >&2; exit 2; }
mkdir -p _output/logs
# --keep-parts: re-run a merge-INPUT variant from merge_models on the EXISTING mc_parts (REDO_MC_PARTS unset, REDO_MERGE=1)
# -- for a merge-RULE change that leaves the partitioned input as it is (the 2026-10-31 dps_sql er_floor fix); the
# caller asserts the partitions on disk are the variant's own
KEEP_PARTS=; if [ "${1:-}" = "--keep-parts" ]; then KEEP_PARTS=1; shift; fi
if [ "$1" = "--all" ]; then set -- $VARIANTS; fi
for v in "$@"; do
  flags=$(variant_flags "$v"); from=$(variant_from "$v"); extra=$(variant_extra "$v")
  [ -n "$flags" ] || { echo "unknown variant: $v" >&2; exit 2; }
  # dist/dataset=nc holds ONE Atlantic bird set; a variant wanting the other re-renders the ingest first (~20 min)
  src_want=$(echo "$flags" | grep -o "NC_ATL_SOURCE=[0-9]*" | cut -d= -f2)
  src_have=$(cat ~/_big/msens/derived/v11/marine-atlas/dist/nc_atl_source.txt 2>/dev/null || echo 2018)
  if [ "$src_want" != "$src_have" ]; then
    echo "==> ingest_sdm-nc with NC_ATL_SOURCE=$src_want for the $v variant ($(date +%H:%M))"
    env NC_ATL_SOURCE=$src_want REDO_INGEST=1 BUILD_THREADS=4 BUILD_MEMORY_GB=8 quarto render ingest_sdm-nc.qmd > _output/logs/ingest_sdm_nc.log 2>&1 || { echo "!! ingest_sdm-nc ($src_want) FAILED" >&2; exit 1; }
    src_have=$(cat ~/_big/msens/derived/v11/marine-atlas/dist/nc_atl_source.txt 2>/dev/null || echo 2018)
    [ "$src_want" = "$src_have" ] || { echo "!! ingest_sdm-nc did not leave NC_ATL_SOURCE=$src_want in dist/nc_atl_source.txt" >&2; exit 1; }
  fi
  if [ "$KEEP_PARTS" = 1 ] && [ "$from" = merge_models_prep ]; then from=merge_models; extra="REDO_MC_PARTS= REDO_MERGE=1"; fi
  echo "==> variant $v  ($(date +%H:%M))  from $from  $extra${KEEP_PARTS:+  (--keep-parts)}"
  # the runner's own merge stage sets REDO_MC_PARTS=1 REDO_MERGE=1; a variant that keeps mc_parts overrides it
  # by rendering the merge directly, then hands the rest to the runner
  if [ "$from" = merge_models ]; then
    # shellcheck disable=SC2086
    env $flags $BUDGET $extra quarto render merge_models.qmd > "_output/logs/merge_models.log" 2>&1 || { echo "!! $v merge FAILED" >&2; exit 1; }
    next=merge_taxon
  else next="$from"; fi
  # shellcheck disable=SC2086
  env $flags $BUDGET scripts/run_version.sh --from "$next" --to snapshot_scores --variant "$v" || { echo "!! $v FAILED" >&2; exit 1; }
done
echo "==> attribution runs done $(date +%H:%M)"
