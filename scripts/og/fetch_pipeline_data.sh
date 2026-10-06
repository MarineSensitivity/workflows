#!/usr/bin/env bash
# stage everything model_species() reads, under ~/_big/sdm/mpaeu/data (the layout the pipeline expects), and symlink
# ~/Github/iobis/mpaeu_sdm/data -> that dir. idempotent; anonymous S3 reads.
#   scripts/mm_run.sh og_data scripts/og/fetch_pipeline_data.sh
#
# what the fit path reads (verified in mpaeu_sdm/functions + codes/model_fit.R + obissdm::get_envofgroup):
#   data/all_splist_*.csv, data/species_ecoinfo.csv          S3 -> via ~/_big/sdm/obis (fetch_obis_published.sh)
#   data/species/key=<AphiaID>.parquet                       QC'd points (same source)
#   data/shapefiles/MarineRealms_BO.*                        S3 (ecoregions of occurrence + adjacent)
#   data/env/terrain/{wavefetch,distcoast}.tif               S3
#   data/env/terrain/{bathymetry_mean,rugosity}.tif          NOT on S3 -> Bio-ORACLE ERDDAP (fetch_env_bio_oracle.R)
#   data/env/current/{thetao,so,o2,sws,siconc}_baseline_{depthsurf,depthmean}_*.tif  NOT on S3 -> ERDDAP (same script)
# not read by the fit path (skipped): data/distances (post-processing), fao_areas.parquet, future/ssp layers.
set -euo pipefail
export PATH=/usr/local/bin:/opt/homebrew/bin:$HOME/.local/bin:$PATH

DIR_DATA=${OG_DATA:-$HOME/_big/sdm/mpaeu}
DIR_OBIS=${DIR_OBIS:-$HOME/_big/sdm/obis/source/model=mpaeu/data}
CLONE=${OG_SDM_REPO:-$HOME/Github/iobis/mpaeu_sdm}
S3=s3://obis-maps/sdm/source/model=mpaeu/data
HERE=$(cd "$(dirname "$0")" && pwd)

D=$DIR_DATA/data
mkdir -p "$D/env/terrain" "$D/env/current" "$D/shapefiles" "$D/log"

echo "== species points + lists (from the published fetch)"
[ -d "$DIR_OBIS/species" ] || { echo "run scripts/og/fetch_obis_published.sh first" >&2; exit 1; }
ln -sfn "$DIR_OBIS/species" "$D/species"
for f in all_splist_20240724.csv species_ecoinfo.csv; do
  [ -f "$DIR_OBIS/$f" ] || aws s3 cp --no-sign-request --only-show-errors "$S3/$f" "$DIR_OBIS/$f"
  ln -sfn "$DIR_OBIS/$f" "$D/$f"
done

echo "== shapefiles (MarineRealms_BO)"
aws s3 sync --no-sign-request --only-show-errors "$S3/shapefiles/" "$D/shapefiles/" --exclude "*" --include "MarineRealms_BO.*"

echo "== terrain layers on S3"
aws s3 sync --no-sign-request --only-show-errors "$S3/env/terrain/" "$D/env/terrain/"

echo "== Bio-ORACLE layers (not on S3; ERDDAP)"
OG_DATA=$DIR_DATA Rscript "$HERE/fetch_env_bio_oracle.R"

echo "== link the clone's data/ -> $D"
if [ -e "$CLONE/data" ] && [ ! -L "$CLONE/data" ]; then echo "ERROR: $CLONE/data exists and is not a symlink" >&2; exit 1; fi
ln -sfn "$D" "$CLONE/data"

echo "== staged"; du -sh "$D/env/terrain" "$D/env/current" "$D/shapefiles"; ls "$D/env/current" | wc -l
