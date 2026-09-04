#!/usr/bin/env bash
# Rebuild the "Sensitivity Across Regions" review page (v10 distribution share vs the published
# v9 rule) from the v9 release on this machine. Every step is a numbered script in this directory;
# nothing is done by hand. Intermediates go to tmp/ (gitignored); the page is tracked.
#
#   scripts/v10_review/build.sh                    # full build -> scripts/v10_review/sensitivity-across-regions.html
#   REDO_OBIS=1 scripts/v10_review/build.sh        # refetch the OBIS counts (27 taxa x 2 calls)
#   OBIS_WRITE_DATA=1 scripts/v10_review/build.sh  # also rewrite data/us_share_fallback.csv (curated; diff before committing)
#
# Inputs, resolved through libs/paths.R (laptop layout under ~/_big/msens):
#   {dir_big}/v9/sdm.duckdb                                   zone_metric, zone_taxon, taxon, taxon_model, model_cell, cell, zone_cell
#   {dir_big}/v9/marine-atlas/dist_merged_global/dataset=ms_merge/*/*.parquet   per-taxon global merged surfaces
#   {dir_big}/v9/marine-atlas/dist/dataset=am/*.parquet       AquaMaps surfaces (taxa with no merged global surface)
#   {dir_derived}/v8/ply_programareas_2026_v8.gpkg            Program Area polygons (names, region)
#   data/boem-mst_usa-study-area.geojson, data/us_share_fallback.csv
# Publish: the Artifact tool republishes the built HTML at the same URL (see README.md).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd); repo=$(cd "$here/../.." && pwd)
export S="$here/tmp" MS_REPO="$repo" VER="${VER:-v9}"
mkdir -p "$S"; cd "$repo"
t0=$(date +%s); step() { printf '\n== %s (%ss)\n' "$1" "$(( $(date +%s) - t0 ))"; }
step "01 pull v9 tables";        Rscript "$here/01_pull_v9.R"
step "02 global totals";         Rscript "$here/02_global_totals.R"
step "03 aquamaps totals";       Rscript "$here/03_am_totals.R"
step "04 national-only taxa";    Rscript "$here/04_national_only.R"
step "05 OBIS fallback";         python3 "$here/05_obis_fallback.py"
step "06 geometry";              Rscript "$here/06_geo.R"
step "07 v9_data.json";          Rscript "$here/07_build_v9_json.R"
step "08 denominator check";     Rscript "$here/08_denom_check.R"
step "09 post-process json";     python3 "$here/09_post_json.py"
step "10 build page";            python3 "$here/10_build_page.py"
step "done"
