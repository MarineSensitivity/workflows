#!/usr/bin/env bash
# copy the published OBIS (MPA Europe) models + QC'd points of the og species, keeping the S3 layout.
# anonymous reads; idempotent (aws s3 sync skips what is already there).
#   scripts/mm_run.sh og_fetch scripts/og/fetch_obis_published.sh
#   OG_SPECIES="137209 159023" scripts/og/fetch_obis_published.sh     # a subset
set -euo pipefail

DIR_OBIS=${DIR_OBIS:-$HOME/_big/sdm/obis}
# loggerhead green hawksbill kemp's leatherback olive-ridley  n-atlantic-right-whale
OG_SPECIES=${OG_SPECIES:-137205 137206 137207 137208 137209 220293 159023}
S3=s3://obis-maps/sdm

mkdir -p "$DIR_OBIS"
for id in $OG_SPECIES; do
  echo "== $id models"
  aws s3 sync --no-sign-request --only-show-errors \
    "$S3/species/taxonid=$id/model=mpaeu/" "$DIR_OBIS/species/taxonid=$id/model=mpaeu/"
  echo "== $id points"
  aws s3 cp --no-sign-request --only-show-errors \
    "$S3/source/model=mpaeu/data/species/key=$id.parquet" "$DIR_OBIS/source/model=mpaeu/data/species/key=$id.parquet"
done

# the small run-wide tables the pipeline reads (species list, habitat lookup)
for f in all_splist_20240724.csv species_ecoinfo.csv; do
  aws s3 cp --no-sign-request --only-show-errors "$S3/source/model=mpaeu/data/$f" "$DIR_OBIS/source/model=mpaeu/data/$f"
done

echo "== done"; du -sh "$DIR_OBIS"/species/* | sed "s|$DIR_OBIS/||"
