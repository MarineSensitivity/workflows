# v8 plan: reingest GoMex (gm) + NCCOS (nc) SDMs into the DuckDB on the 0.05° cell grid

## Context

PostGIS has been retired. `gm` (GoMex cetacean/turtle, monthly) was ingested only into the old PostGIS
schema (`sdm_datasets`/`sdm_models` + hexagon geometry tables in `ingest_sdm-gm.qmd`) and served via
pg_tileserv — that path is now dead, and `gm` was never in `sdm.duckdb`, so it is absent from the STAC
catalog. `nc` (NCCOS seabirds, seasonal) only ever produced **COGs + `nc_models.csv`** (its DuckDB write is
commented out), so it works in the catalog today as COG-backed seasonal items but is **not** a first-class
DuckDB dataset and is **not scored**.

**Decision (this plan):** port both `gm` and `nc` into `sdm.duckdb` on the existing **0.05° `cell_id` grid**
(the representation every other dataset uses), served by the DuckDB-SQL TiTiler endpoint — no PostGIS, no H3.
Because adding datasets changes the merged taxon set and scores, this is a **version bump to v8** (per the
"integer bump when outputs change" rule), not a v7 surgical fix.

**Blocker:** the `gm` source (NOAA SEFSC hexagon shapefiles) lives at
`~/My Drive/projects/offhab/data/raw/ncei.noaa.gov - GoMex.../NOAA_SEFSC_Cetacean_SeaTurtle_SDM_shapefiles`
and is **not currently synced locally** — re-sync it before running the `gm` ingest. The `nc` source COGs
**are** local (`workflows/data/sdm/raw/nc_atl_birds_dens/*.tif`, 140 tifs).

## Goal

`gm` and `nc` become DuckDB datasets (`dataset`/`model`/`model_cell`/`species`) with per-interval surfaces on
the 0.05° grid; `merge_models` + `calc_scores` include them; `stac_build()` emits them automatically (gm =
monthly datacube, nc = seasonal) served via the titiler SQL endpoint. Catalog/apps work with Postgres gone.

## Design

### 1. Rasterize source → 0.05° cell grid → `model_cell` (the core pattern)

Reuse, don't reinvent — the grid + helpers already exist:
- `msens::cell_id_raster()` → the `cell_id` SpatRaster anchored by `r_bio-oracle_planarea.tif`.
- `terra::rasterize(vect(sf), r_cell_id, field = <value>, fun = "mean")` for polygons (gm hexagons), and
  `terra::resample(rast(cog), r_cell_id, method = "average")` for rasters (nc COGs) → a value-per-cell raster.
- Extract to a tibble `(cell_id, value)` (drop NA / non-positive), attach `mdl_seq`, then
  `dbWriteTable(con_sdm, "model_cell", d_mdl_cell, append = TRUE)` — exactly the AquaMaps pattern
  (`ingest_aquamaps_to_sdm_duckdb.qmd:793`), with indexes added after the mass insert (`:928`).
- `msens::cells_in_polygon(poly, r_cell_id)` (returns `cell_id` + `pct_covered`) is the fallback for
  area-weighted assignment if direct `rasterize(field=)` over the coarse hexagons proves too lossy.

### 2. `gm` ingest → DuckDB (rewrite the write-target half of `ingest_sdm-gm.qmd`)

Keep the source-reading half (reads the 19 hexagon shapefiles, monthly `*_n` abundance #/40 km², the
`get_annual_density()` helper, the taxa crosswalk `spp_gmx.xlsx`). Replace the PostGIS writes
(`st_write`/`dbWriteTable` to `sdm_datasets`, `t_geoms`, `gm_model_hexagons`, …) with DuckDB writes:
- `dataset`: one row `ds_key='gm'`, `response_type='density'`, **`temporal_res='monthly'`**,
  `source_broad='NOAA SEFSC'`, dates/citation already in the file (`:40`–`:51`).
- `species` / `taxon`: from the `spp_gmx.xlsx` crosswalk (scientific name + WoRMS where available).
- `model`: **one row per (taxon × month)**, `time_period = "2019-{MM}/P1M"` (the file already builds this),
  `mdl_type='density'`, `region='Gulf of Mexico'` → `mdl_seq` from the sequence.
- `model_cell`: for each (taxon, month) rasterize that month's hexagon density (#/km² = `n/40`) onto the
  cell grid → `(mdl_seq, cell_id, value)`. GoMex extent only, so only Gulf cells get values.
- **Annual surface for scoring:** also write one `model` per taxon with `time_period="2019"` whose
  `model_cell` is `get_annual_density()` (mean across months) — this is what `merge_models`/`calc_scores`
  consume (cell-based, static scoring), while the 12 monthly surfaces drive the catalog/apps temporal view.
  (Alternatively, derive the annual mean inside `merge_models`; writing it at ingest is simpler.)

### 3. `nc` ingest → DuckDB (extend `ingest_sdm-nc.qmd`)

Keep the COG production + `nc_models.csv` (the catalog's per-season COG assets stay — they're the real
high-res files). Add the currently-commented DuckDB writes (`:330`–`:335`):
- `dataset`: `ds_key` per region (`nc_atl_birds_dens`, `nc_pac_birds_dens`), `response_type='density'`,
  **`temporal_res='seasonal'`**, `source_broad='NOAA NCCOS'`.
- `model`: one per (taxon × season); **season time_period encoding** = a representative ISO interval, e.g.
  winter `2018-12-01/P3M`, spring `2019-03-01/P3M`, summer `2019-06-01/P3M`, fall `2019-09-01/P3M`
  (decision below). `model_cell`: `resample()` each season COG band-1 (`n_per_km2`) onto the cell grid.
- Annual surface for scoring: mean across seasons → `time_period="2019"` model per taxon.

### 4. Merge + score (`merge_models.qmd`, `calc_scores.qmd`) → v8

- `merge_models` picks up the new `gm`/`nc` annual models → larger taxon set (cetaceans, turtles, seabirds
  with density), `is_ok`, `ms_merge`, `taxon_model` all change. Confirm density-response handling in the
  merge (these are densities, not 0–100 suitability) — they enter via the multiplicative/max rules used for
  other non-suitability sources; verify rescaling so density doesn't dominate.
- `calc_scores` re-runs zone/score metrics → **scores change → bump `ver` to `v8`** in `libs/paths.R`
  (`ver`/`ver_prev`), and let the version-compare chunk (`:3945`) report the delta vs v7.
- Temporal stays **annualized for scoring**; per-interval surfaces are preserved in `model_cell` for the
  catalog/apps (no time-aware scoring in v8 — that's a later step).

### 5. STAC catalog (mostly automatic; two small generator updates)

- Once `gm`/`nc` are in the DuckDB with `temporal_res` monthly/seasonal + multiple `time_period`s,
  `stac_model_cell_item()` already emits the **datacube month / season dimension** + titiler-SQL links — gm
  and nc appear with no generator change beyond what already exists.
- Update `msens::stac_build()` so the **seasonal `nc` Item carries BOTH** the per-season COG assets
  (from `stac_season_cog_item()` / `nc_models.csv`) **and** the DuckDB model_cell surface (parquet + SQL) —
  merge the two builders for `nc_*` rather than emitting separate items. Drop the standalone CSV-only path
  once nc is in the DB (keep the COG hrefs as assets).
- Update the **published `gm` example** in `stac-sdm/examples/item-gm-monthly.json`: replace the now-dead
  `tile.marinesensitivity.org/public.sdm_spatial` pg_tileserv vector link with the DuckDB-SQL titiler link
  (and/or a GeoParquet vector asset). Re-run `npm test`, commit, cut `stac-sdm` v1.1.0 (manual gh-pages push,
  per the org-policy note — see [[caddy-single-file-mount-reload]] sibling memory).

## Reusable utilities / patterns

- `msens::cell_id_raster()`, `msens::cells_in_polygon()` (`msens/R/calc.R`).
- `terra::rasterize(field=, fun="mean")`, `terra::resample(method="average"|"bilinear")` — see
  `ingest_aquamaps_res05.qmd:94,199,924` and `ingest_birdlife.org_botw.qmd:293`.
- model_cell mass-insert + post-insert indexes — `ingest_aquamaps_to_sdm_duckdb.qmd:793,928`.
- `get_annual_density()` — already in `ingest_sdm-gm.qmd:85` (monthly → annual mean).
- `msens::stac_build()` + builders — `msens/R/stac.R`.

## Critical files

- `workflows/ingest_sdm-gm.qmd` — swap PostGIS writes for DuckDB `dataset`/`model`/`model_cell` (monthly + annual).
- `workflows/ingest_sdm-nc.qmd` — enable the commented DuckDB writes; resample COGs → cell grid (seasonal + annual).
- `workflows/merge_models.qmd` — verify density-response merge + rescaling with the new datasets.
- `workflows/calc_scores.qmd` — re-run for v8 (the `do_stac` chunk then emits gm/nc automatically).
- `workflows/libs/paths.R` — bump `ver`→`v8`, `ver_prev`→`v7`.
- `msens/R/stac.R` — merge COG + DuckDB representations for `nc_*`; (no change needed for gm monthly).
- `stac-sdm/examples/item-gm-monthly.json` — drop dead pg_tileserv link; bump extension to v1.1.0.

## Open decisions (resolve before building)

1. **Season `time_period` encoding** in `model.time_period`: representative `YYYY-MM-01/P3M` per season
   (recommended — keeps it ISO-8601 and lets `stac_build` detect seasonal), vs a non-ISO `"2019-summer"`.
2. **Annual surface**: write it at ingest (simpler, explicit) vs derive in `merge_models` (less storage).
3. **Keep nc COG path**: yes — retain per-season COG assets on the nc Items (high-res), add the DuckDB
   surface alongside; don't delete `nc_models.csv` until the merged builder is verified.
4. **Density rescaling in the score**: confirm GoMex/NCCOS densities are rescaled comparably to other
   components so they don't dominate `ms_merge`/scores.

## Verification

1. **Ingest unit check**: after `gm` ingest, `SELECT ds_key, count(*) n_models, count(DISTINCT time_period)
   FROM model WHERE ds_key='gm' GROUP BY 1` → 12 monthly (+1 annual) periods per taxon; spot-check a
   `model_cell` surface renders over the Gulf via the titiler SQL endpoint.
2. **Grid alignment**: confirm rasterized `cell_id`s are a subset of the `cell` table and values are finite,
   positive densities; compare a hex-vs-cell total abundance for one taxon/month (mass conservation sanity).
3. **Merge/score delta**: `calc_scores` version-compare chunk reports v8-vs-v7 taxon counts + score changes;
   confirm the new taxa appear in `zone_taxon` and `is_ok`.
4. **Catalog**: regenerate via `do_stac`; `stac-validator` the new `gm` (12-month datacube) + `nc`
   (4-season) items; confirm an interval-parameterized titiler tile renders for one gm month.
5. **No Postgres**: grep the served catalog for `sdm_spatial`/pg_tileserv → 0; all gm/nc delivery is
   titiler-SQL + file-server COGs.
