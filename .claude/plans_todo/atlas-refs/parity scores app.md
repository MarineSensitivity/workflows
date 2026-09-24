# Feature-parity inventory — MarineSensitivity **scores** Shiny app

Source of truth read in full:

- `/Users/bbest/Github/MarineSensitivity/apps/scores/app.R` (3,292 lines, 144 KB)
- `/Users/bbest/Github/MarineSensitivity/apps/scores/notes.md` (dev log, 2025-06 origins)
- `/Users/bbest/Github/MarineSensitivity/apps/scores/CLAUDE.md` (a symlink to the generic
  `bbest/ai_guidance/CLAUDE.md` — **R style guidance only, nothing app-specific**)
- `/Users/bbest/Github/MarineSensitivity/apps/README.md` (deployment: two instances, mobile, cold start)
- `msens` package sources under `/Users/bbest/Github/MarineSensitivity/msens/R/`:
  `version.R`, `ver_token.R`, `version_ui.R`, `grid.R`, `study_area.R`, `viz.R`, `calc.R`,
  `cell_model.R`, `zone_style.R`, `zone_set.R`, `analytics.R`, `atlas.R`
- Live artifacts fetched to confirm the contract: `latest.txt`, `versions.json`,
  `v7/manifest.json`, `v8/manifest.json`, published Parquet schemas + row counts + object sizes
  (S3 `oceanmetrics.io-public/marine-atlas`).

Target for the port: static JS SPA (MapLibre GL + DuckDB-WASM over public Parquet/COG/PMTiles,
GitHub Pages, no R). Everything below is what must be reproduced.

---

## 1. Purpose + user-facing summary

1. Explore **composite marine-sensitivity scores** for US federal waters from any published
   release (v1…v9) of the BOEM Marine Sensitivity Toolkit — one app, version is data (`app.R:1635-1680`).
2. Pick a **study area** camera preset (All US waters / Alaska / Atlantic / Gulf of America /
   Pacific) — it moves the camera only, it never filters or masks data (`app.R:906-932`, apps#13/#14).
3. Pick a **spatial unit**: raster cells (0.05°) or any polygon unit that release actually scores
   and can draw (Program areas, Planning areas, Ecoregions, Subregions) (`app.R:570-647`, `1447-1461`).
4. Pick a **layer** (overall score; per-category extinction-risk, raw or ecoregion-rescaled;
   primary productivity raw/rescaled) and a **color palette** (Spectral / Viridis / Cividis /
   Magma) (`app.R:764-851`, `1469-1483`).
5. **Click a cell** → its cell id, the layer value at that point, a flower plot of the 7 component
   scores for that cell, and the full species list for that cell (`app.R:2366-2427`, `2436-2475`, `2644-2669`).
6. **Click a polygon** → that zone's flower plot and species list, drawn from precomputed zone
   aggregates (`app.R:2507-2551`, `2672-2684`).
7. **Table of Species** — sortable/filterable DT with taxon links (WoRMS/BoTW), a per-model link
   into the sibling *species* app, area, average suitability and % contribution to its category;
   plus a **Composition** treemap of the taxonomy (`app.R:2768-2825`, `2850-2893`).
8. **Download CSV** of the current species table (`app.R:2828-2847`).
9. **Report tab** — build a list of labeled areas (drawn polygons and/or clicked Program Areas),
   pick title / data version / format (HTML, Word, PDF) and POST to a server-side Quarto render;
   the finished file opens in a pre-opened tab (`app.R:1550-1607`, `3149-3269`).
10. Chrome: welcome modal with docs/species links, a 10-step guided tour, a version picker modal,
    dark-mode toggle, sphere/mercator toggle, geocoder search, fullscreen, layer toggles, a
    PREVIEW badge naming the signed-in reviewer on restricted releases (`app.R:1113-1173`, `1743-1873`).

---

## 2. Startup and data loading

### 2.1 Process-level globals (once per R worker, `app.R:13-117`)

| item | value | line |
|---|---|---|
| `verbose` | `interactive()` | 58 |
| `ver_fallback` | `"v8"` — last-resort only if the registry is unreachable | 64-65 |
| `APP_VERSION` | `git -c safe.directory=* -C <here()> rev-parse --short HEAD`, else `ver` | 74-82 |
| `is_server` | `Sys.info()[["sysname"]] == "Linux"` | 84 |
| `dir_private` | `/share/private` (server) or `~/My Drive/private` | 85-89 |
| `dir_data` | `/share/data` (server) or `~/My Drive/projects/msens/data` | 90-94 |
| `UNUSED_MAP_DEPS` | `c("html2canvas","mapbox-gl-globe-minimap")` stripped from the widget's HTML deps (343 KB saved of 3.7 MB/39 files) | 111-117 |
| async plan | `future::plan(multisession, workers = 2)` — background `/report` POSTs | 52 |

### 2.2 Per-version bundle — `build_bundle(ver)` (`app.R:132-1045`), memoised in `.bundles` (`app.R:1050-1054`)

Everything below is a function of the requested version. Both `ui_impl` and `server_impl` get
`environment(f) <- bundle(ver)` so every name resolves to that release (`app.R:3279-3290`).

**Paths and files**

| name | value | required? | line |
|---|---|---|---|
| `dir_v` | `{dir_data}/derived/{ver}` | — | 134 |
| `dir_big` | `/share/data/big/{ver}` (server) or `~/_big/msens/derived/{ver}` | — | 135-139 |
| `is_prod` | `Sys.getenv("MSENS_ENV") == "prod"` | — | 140 |
| `pmtiles_base_url` | `/pmtiles` when prod else `https://file.marinesensitivity.org/pmtiles` (**fallback only**, used when no manifest) | — | 141-144 |
| `mapbox_tkn_txt` | `{dir_private}/mapbox_token_bdbest.txt` → `Sys.setenv(MAPBOX_PUBLIC_TOKEN=…)` | **YES (hard stop)** | 146, 191 |
| `cell_tif` | `{dir_data}/derived/{msens::grid_registry()$cellid_tif[grid_id == grid_for_ver(ver)]}` = `r_cellid.tif` (usa05) or `r_cellid_global.tif` (global05) | **YES (hard stop, twice)** | 151, 744-745 |
| `sdm_db` | `{dir_big}/serve.duckdb` if it exists, else `{dir_big}/sdm.duckdb` | **YES** | 154 |
| `dir_cache` | `here("scores/cache/{ver}")`, created if absent | — | 161 |
| `lyrs_csv` | `{dir_v}/layers_{ver}.csv` (fallback layer picker) | no | 162, 774 |
| `pra_gpkg` | `{dir_v}/ply_programareas_2026_{ver}.gpkg` (fallback geometry) | no | 163, 521 |
| `taxonomy_csv` | `here("scores/data/taxonomic_hierarchy_worms_2025-10-30.csv")` | **YES (hard stop)** | 164, 1031 |
| `tbl_pra_pm` | `"ply_programareas_2026"` (legacy PMTiles filename fallback) | — | 166 |

`v_required = c(mapbox_tkn_txt, cell_tif, sdm_db, taxonomy_csv)`; missing → `stop()` (`app.R:171-182`).

**Database** — `con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = TRUE)` (`app.R:197`).

On the server `serve.duckdb` is a **274 KB view-only DB**; its views (verified from
`~/_big/msens/derived/v8/serve.duckdb`) are:

```sql
CREATE VIEW cell        AS SELECT * FROM read_parquet('/share/data/big/v8/tables/cell.parquet');
CREATE VIEW cell_metric AS SELECT *, val AS "value" FROM read_parquet('…/tables/cell_metric.parquet');
CREATE VIEW metric      AS SELECT * FROM read_parquet('…/tables/metric.parquet');
CREATE VIEW model       AS SELECT * FROM read_parquet('…/tables/model.parquet');
CREATE VIEW taxon       AS SELECT * FROM read_parquet('…/tables/taxon.parquet');
CREATE VIEW taxon_model AS SELECT * FROM read_parquet('…/tables/taxon_model.parquet');
CREATE VIEW dataset     AS SELECT * FROM read_parquet('…/tables/dataset.parquet');
CREATE VIEW listing     AS SELECT * FROM read_parquet('…/tables/listing.parquet');
CREATE VIEW native_asset AS SELECT * FROM read_parquet('…/tables/native_asset.parquet');
CREATE VIEW "zone"      AS SELECT *, val AS "value" FROM read_parquet('…/tables/zone.parquet');
CREATE VIEW zone_cell   AS SELECT * FROM read_parquet('…/tables/zone_cell.parquet');
CREATE VIEW zone_metric AS SELECT *, val AS "value" FROM read_parquet('…/tables/zone_metric.parquet');
CREATE VIEW zone_taxon  AS SELECT * FROM read_parquet('…/tables/zone_taxon.parquet');
CREATE VIEW cell_model  AS SELECT cm.tile, cm.mdl_id, cm.cell_id, cm.val, cm.val AS "value"
  FROM read_parquet('/share/data/big/v8/cell_model/*/*.parquet', hive_partitioning=true) cm;
CREATE VIEW model_cell  AS SELECT m.mdl_key, mc.mdl_id, mc.cell_id, mc.val, mc.val AS "value"
  FROM read_parquet('/share/data/big/v8/model_cell/*/*.parquet', hive_partitioning=true) mc
  JOIN model m USING (mdl_id);
```

`msens::attach_atlas()` / `atlas_views()` build the same view set over **HTTPS path-style S3**
(`msens/R/atlas.R:67-89`) — the JS port's DuckDB-WASM equivalent:
`https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/{ver}/tables/{t}.parquet`,
with `val AS value` aliased for `cell_metric`, `zone_metric`, `zone`. **The `model_cell` glob view
needs S3 LIST credentials** and is created only when `anon = FALSE`.

**Zone table-name + value-column resolution (never guessed)**

- `zone_tbl_for(fld, prefer)` → `SELECT DISTINCT tbl FROM zone WHERE fld = '<fld>' ORDER BY tbl`;
  prefers the version-suffixed name, else the highest-sorting (`app.R:212-226`). Yields
  `tbl_er`, `tbl_sr`, `tbl_pra`, `tbl_pa`.
- `val_zone/val_zm/val_cm <- msens::sdm_val_col(con, tbl)` → `"val"` if present, else `"value"`
  (`msens/R/calc.R:352-359`). Three shapes exist: `value`-only (v1-v7), `val`-only (v8 source),
  **both** (v8 served) (`app.R:228-264`).
- `zone_vals()/zone_metric_vals()/cell_metric_vals()` = `tbl(con, x)` with the redundant `value`
  alias dropped and the resolved column renamed to `value` (`app.R:257-264`).

**Tables used by the app, and the columns it actually reads**

| table | columns used | where |
|---|---|---|
| `metric` | `metric_seq`, `metric_key`, (`description` via manifest) | 406-421, 428-437, 2447, 2525 |
| `cell_metric` | `cell_id`, `metric_seq`, `val`/`value` | 417, 434, 2450 |
| `zone` | `zone_seq`, `tbl`, `fld`, `val`/`value` | 215, 583-587, 941, 2266, 2520 |
| `zone_metric` | `zone_seq`, `metric_seq`, `val`/`value` | 1007, 2269, 2527 |
| `zone_cell` | `zone_seq`, `cell_id` (+ `pct_covered` inside msens) | 456, 976 |
| `zone_taxon` (preferred path) | `zone_fld`, `zone_value`, `sp_cat`, `sp_common`, `sp_scientific`, `taxon_id`, `taxon_authority`, `er_code`, `er_score`, `is_mmpa`, `is_mbta`, `mdl_key`, `area_km2`, `avg_suit`, `pct_cat` | `msens/R/calc.R:606-614` |
| `cell_model` (clicked cell) | `tile`, `mdl_id`, `cell_id`, `val` | `msens/R/calc.R:379-401` |
| `model` | `mdl_id`, `mdl_key` (join back for the stable id) | `calc.R:392` |
| `taxon` | `ms_merge_key`/`mdl_seq`, `sp_cat`, `common_name`, `scientific_name`, `taxon_id`, `taxon_authority`, `extrisk_code`, `er_score`, `is_mmpa`, `is_mbta`, `is_valid_usa`/`is_ok`, `is_marine` | `calc.R:295-436` |
| `cell` | `cell_id`, `area_km2` (+ `lon`,`lat` for polygon selection) | `calc.R:124-150`, `431` |

Published schemas (confirmed against S3 v8):
`cell(cell_id INT, lon, lat, depth_mean, depth_min, depth_max, oxy_b_mean, oxy_mean,
prim_prod_mean, ice_con_ann, salinity_b_mean, salinity_mean, sbt_an_mean, sst_an_mean,
fao_area_m, area_km2, in_usa BOOL, in_pra BOOL)`;
`metric(metric_seq, metric_key, description)`; `cell_metric(cell_id, metric_seq, val)`;
`zone(zone_seq, zone_set_key, tbl, fld, val)`; `zone_cell(zone_seq, cell_id, pct_covered)`;
`zone_metric(zone_seq, metric_seq, val)`;
`model(mdl_key, mdl_id, ds_key, sp_id, sci_name, common_name, er_score, sp_cat)`.

**Published v8 volumes (S3, measured 2026-09-20)**

| object | bytes | rows |
|---|---|---|
| `tables/cell.parquet` | 392,779,989 | 17,072,105 |
| `tables/cell_metric.parquet` | 68,121,353 | 9,549,911 |
| `tables/zone_cell.parquet` | 151,176 | 2,241,876 |
| `tables/zone_taxon.parquet` | 4,978,283 | 115,700 (36 zones) |
| `tables/taxon.parquet` | 1,040,399 | 37,051 |
| `tables/model.parquet` | 1,086,176 | 80,791 |
| `tables/zone.parquet` | 1,368 | 37 |
| `tables/zone_metric.parquet` | 6,454 | 795 |
| `tables/metric.parquet` | 1,300 | 41 |
| `serve/cell_model/tile=*/data_0.parquet` | 1,298,951,659 | 422 partitions (~3 MB each) |
| `serve/model_cell/mdl_id=*/data_0.parquet` | 3,266,450,630 | 40,407 objects |
| `zones/{set}/zones.pmtiles` | 0.96–1.9 MB each | — |
| `zones/{set}/zones.fgb` | 7.2–52.6 MB each | — |
| `grid/global05/cellid.tif` | 73,363,482 | — |
| `grid/usa05/cellid.tif` | 124,534 | — |
| `cog/` (all score COGs, both grids) | 788,415,518 across 23,649 objects (`cog/global05`: 286 objects / 153,599,649 B) | — |

v7 equivalents: `cell` 23.8 MB, `cell_metric` 114.4 MB, `zone_cell` 17.8 MB, `zone_taxon` 6.3 MB,
`taxon` 543 KB, `model` 623 KB.

**Parquet row-group layout (measured — decides what DuckDB-WASM can prune)**

| file | row groups | pruning available |
|---|---|---|
| `v8/tables/cell_metric.parquet` | **3**, each `cell_id` 1,080,221–15,480,268 and `metric_seq` 1–33 | **none** — a single-cell query reads all 3 groups (effectively the whole 68 MB) |
| `v8/tables/cell.parquet` | 30 (624,640 rows each) | `lat` prunes (e.g. rg0 = 38.8–90.0); **`lon` spans -180..180 in every group**, so a bbox query prunes on latitude only |
| `v8/tables/zone_taxon.parquet` | 1 | none needed — 4.98 MB whole-file read answers any zone |
| `v8/serve/cell_model/tile=*/` | 422 Hive partitions | exact partition key computed client-side (no LIST needed) |

**Anonymous S3 LIST is DENIED** on the bucket (verified: `ListObjectsV2` → `AccessDenied`), so a
static client can never glob — every object key must be constructed from the manifest or from an
arithmetic partition rule.

### 2.3 Tile service + manifest (`app.R:268-421`)

- `tile_base_url <- "https://titiler-v8.marinesensitivity.org"` — **hardcoded for every version**
  (`app.R:271`). `msens::cog_tile_url/cell_tile_url/cell_stats/cog_point_value` default to the same.
- `db_mtime <- format(file.info(sdm_db)$mtime, "%Y%m%dT%H%M%SZ", tz="UTC")` — cache-bust tag for the
  SQL-tile fallback only (`app.R:275`).
- `manifest <- msens::atlas_manifest(ver)`, wrapped in `tryCatch` → `NULL` means "fall back to SQL
  tiles" (`app.R:288-291`).
- `cog_tbl` = `manifest$metrics[, c(metric_key, description, subregion_key, cog, rescale_min,
  rescale_max, colormap)]` **only when `capabilities$score_cogs`**, with `manifest$overlays`
  row-bound in as pseudo-metrics keyed by `overlay_key` and NA rescale (`app.R:292-308`).
- `zone_tbl <- manifest$zones` (`app.R:323`).
- `ztile(zone_type, fallback_tbl)`: manifest row where `fld == "{type}_key"` and `pmtiles` not NA →
  `list(url = pmtiles, source_layer = zone_type)`; **NULL when the manifest exists but has no such
  zone** (a positive "this release has none"); only a *missing* manifest falls back to
  `{pmtiles_base_url}/{fallback_tbl}.pmtiles` with `source_layer = fallback_tbl` (`app.R:332-340`).
- `cog_of(metric_key, subregion_key = "FULL")` → `list(url, rescale = c(min,max), colormap)` (`app.R:355-362`).
- `layer_tiles(metric_key, subregion_key, palette)` (`app.R:383-396`) — **always asks for the FULL
  COG first** (the study area is a camera, not a filter); per-subregion COG is a defensive fallback;
  SQL tiles are the last resort:

```r
cg <- cog_of(metric_key, "FULL") %||% cog_of(metric_key, subregion_key)
if (!is.null(cg)) list(rescale = cg$rescale,
     url = msens::cog_tile_url(cg$url, colormap = palette, rescale = cg$rescale, base = tile_base_url))
else { sql <- cell_sql(metric_key); st <- msens::cell_stats(sql, mtime = db_mtime, base = tile_base_url)
       list(rescale = c(st$min, st$max),
            url = msens::cell_tile_url(sql, colormap = palette, rescale = …, mtime = db_mtime, base = …)) }
```

- `cell_sql(metric_key, subregion_key)` — **`subregion_key` accepted and deliberately ignored**
  (`app.R:406-421`); strict allowlist `^[A-Za-z0-9_.-]+$` / `^[A-Za-z0-9_]+$`:

```sql
SELECT cm.cell_id, cm.{val_cm} AS value
FROM cell_metric cm
JOIN metric m ON cm.metric_seq = m.metric_seq
WHERE m.metric_key = '{metric_key}'
```

- `outside_pra_sql` (`app.R:972-979`):

```sql
SELECT c.cell_id, 1.0 AS value
FROM (SELECT DISTINCT cell_id FROM cell_metric) c
WHERE c.cell_id NOT IN (
  SELECT zc.cell_id FROM zone_cell zc
  JOIN zone z ON zc.zone_seq = z.zone_seq
  WHERE z.fld = 'programarea_key')
```

  `outside_pra_tile_url` prefers `cog_of("_outside_pra","FULL")` rendered with `color = "#222222"`
  (`app.R:980-987`).

### 2.4 Geometry, labels and derived caches

- `zone_geom(type)` (`app.R:514-551`): local gpkg first when the release ships one
  (`programarea` → `pra_gpkg`; `planarea` → `{dir_v}/ply_planareas_2025_{ver}.gpkg`;
  `ecoregion` → `{dir_v}/ply_ecoregions_2025.gpkg`), else the vintage's published FlatGeobuf
  `/vsicurl/{msens::atlas_base_url()}/zones/{zone_set_key}/zones.fgb`; selects
  `{type}_key`, `{type}_name`; **NULL when the release has no such unit**. Memoised in
  `.zone_geom_cache` (bundle-level env). `pra_geom()` = `zone_geom("programarea")` (`app.R:554`).
- `zone_pts(type)` (`app.R:685-719`): reads `{dir_cache}/{type}_label_pts.csv` if present, else
  derives `st_point_on_surface(st_shift_longitude(zone_geom(type)))` → columns
  `{type}_key, {type}_name, lng, lat`, writes the CSV, returns an sf point layer (EPSG:4326).
  **Longitudes are 0-360** in the cached CSVs (e.g. ALA `lng = 187.500`).
- `zone_units` (`app.R:570-642`), then re-filtered for drawability (`app.R:621-639`). A unit
  appears iff **(a)** the manifest gives it PMTiles, **(b)** ≥ 2 of its zones carry a composite
  score, **(c)** ≥ 2 of those keys exist in the geometry. Scored-zone query:

```sql
SELECT DISTINCT z.{val_zone} AS zkey, z.fld
FROM zone z JOIN zone_metric zm USING (zone_seq)
JOIN metric m USING (metric_seq)
WHERE m.metric_key LIKE 'score!_%' ESCAPE '!'
```

  Labels: `programarea`→"Program areas", `planarea`→"Planning areas", `ecoregion`→"Ecoregions",
  `subregion`→"Subregions", else Titlecase+"s". Ordering: Program Areas first, then finest (largest
  `n`) first. Columns: `fld, type, label, url, source_layer, zone_set_key, n, keys(list)`.
  Excluding rollups (`USA` on v8, `FULL` on v7) happens by intersecting with geometry keys, not by
  name.
- `primary_unit <- zone_units$type[1]`; `primary_ln <- "{primary_unit}_ln"`;
  `has_er_ln`; `before_er <- primary_ln`; `before_r <- if (has_er_ln) "er_ln" else primary_ln`
  (`app.R:647-664`) — MapLibre rejects an add whose `before_id` doesn't exist and the failure
  cascades (v1 symptom: map with nothing but labels).
- `zone_ctrl_layers()` (`app.R:673-680`): `"{label} outlines" = "{type}_ln"` for every unit plus
  `"{label} labels" = "{type}_lbl"` only for units whose `msens::zone_label_args(type)` is non-NULL.

**Layer picker `d_lyrs`** (`app.R:764-800`): from `cog_tbl` when it has `lyr_order, category, label`
(dedup on `metric_key`, ordered by `lyr_order`) → tibble `order, category, layer, lyr`;
else `layers_{ver}.csv`; else derived from the manifest descriptions with the score first.
`lyr_choices` = named list grouped by `category` → `setNames(lyr, layer)`; `lyr_default = d_lyrs$lyr[1]`
(`app.R:822-832`). Live v8/v7 content (from the manifests) is 17 layers:

| order | category | label | metric_key | FULL rescale (v8) |
|---|---|---|---|---|
| 1 | Overall | score | `score_extriskspcat_primprod_ecoregionrescaled_equalweights` | 0–96 |
| 2 | Species, rescaled by Ecoregion | `{cat}: ext. risk, ecorgn` | `extrisk_{cat}_ecoregion_rescaled` | 0–100 |
| 3 | Primary Productivity, rescaled by Ecoregion | prim prod, ecorgn | `primprod_ecoregion_rescaled` | 0–100 |
| 4 | Species, raw Extinction Risk | `{cat}: ext. risk` | `extrisk_{cat}` | per-metric, e.g. fish 0.01–2648.03 |
| 5 | Primary Productivity, raw Phytoplankton | prim prod, 2014-2023 avg (mg C/m^2/day) | `primprod` | 32.95–13743.47 |

`{cat}` on v8 = bird, coral, fish, invertebrate, mammal, primary_producer, turtle
(v7 uses `other` in place of `primary_producer`).

**Palettes** (`app.R:836-851`): choices
`Spectral (default)=spectral_r`, `Viridis (deuteranopia)=viridis`, `Cividis (protanopia)=cividis`,
`Magma (tritanopia)=magma`; `get_pal_colors(key, n = 11)` = `rev(RColorBrewer::brewer.pal(11,"Spectral"))`
or `viridisLite::viridis/cividis/magma(11)`. The same key string is sent to titiler as the
rio-tiler `colormap_name`.

**Study areas** `d_sa <- msens::study_areas()` (`app.R:924`, `msens/R/study_area.R:193-203`) — baked
constants, identical for every release:

| key | label | lon | lat | zoom | ecoregions |
|---|---|---|---|---|---|
| FULL | All US waters | -101.304 | 46.900 | 2.16 | CAC CBS EBS EGOA GOA HAR NECS PIS PUR SECS WAOR WCGOA |
| AK | Alaska | -164.654 | 63.327 | 2.35 | CBS EBS GOA HAR |
| AT | Atlantic | -67.627 | 29.862 | 2.71 | NECS PUR SECS |
| GA | Gulf of America | -89.089 | 26.251 | 3.74 | EGOA WCGOA |
| PA | Pacific | -171.570 | 28.541 | 1.70 | CAC PIS WAOR |

`sr_view(key)` → `list(center = c(lon,lat), zoom)`, falling back to FULL (`app.R:926-930`).
Derivation (for the record): spherical centroid / minimum-enclosing-circle on unit-sphere
vertices, `zoom = clamp(6.75 - log2(radius*(1+0.10)), 1.7, 5)` (`study_area.R:57-177`).

**`zone_all_key`** (`app.R:940-945`): the zone meaning "everything this release scored" —
first of `FULL`, `USA` present in `zone.fld = 'subregion_key'`, else the first available, else `"USA"`.

**`rpt_ver_choices`** (`app.R:950-956`): `msens::atlas_versions()` → labels `v7` or `v8 (prerelease)`.

**Pre-warmed initial state** (`app.R:962-966`): `initial_sql`, `initial_lyr = layer_tiles(lyr_default,
sr_choices[[1]])`, `initial_rescale`, `initial_tile_url`, `initial_view = sr_view("FULL")`.

**Default flower cache** (`app.R:1001-1028`): `here("scores/cache/flower_default_subregions.csv")`
— **NOT per-version** (a real cross-version bug: the first release to run writes it and every other
release then reads it). Built from:

```r
zone_vals() |> filter(tbl == tbl_sr, fld == "subregion_key") |>
  select(zone_seq, subregion_key = value) |>
  inner_join(zone_metric_vals(), by = "zone_seq") |>
  inner_join(tbl(con,"metric") |> filter(str_detect(metric_key, ".*_ecoregion_rescaled$")),
             by = "metric_seq") |>
  select(subregion_key, metric_key, score = value) |> collect() |>
  mutate(component = metric_key |> str_replace("extrisk_","") |>
           str_replace("_ecoregion_rescaled","") |> str_replace("_"," "), even = 1) |>
  filter(component != "all")
```

  Committed file has 40 data rows: `subregion_key ∈ {AK, GA, PA, USA}` × 10 components.

**Taxonomy** `d_taxonomy <- read_csv(taxonomy_csv, guess_max = Inf)` (`app.R:1031`) —
16,873 rows, columns `species_id, scientific_name, common_name, component, Kingdom, Phylum,
Subphylum, Class, Order, Family, Genus, Species, …` (36 rank columns).

**Dead-but-present code to NOT port**: `get_rast()` (`app.R:424-470`, terra path, unused),
`r_cell_open()` (758, used only by `get_rast`), `pra_pts` (722, unused),
`pra_src_layer`/`er_src_layer` (350-351, unused), `tbl_pa` (224, unused),
`output$click_info` (2579-2593, **no UI element renders it**), commented-out planarea branch
(2151-2236) and `d_sr_pa` block (855-901).

**Environment variables read**: `MSENS_ENV` (140), `MSENS_REPORT_URL` (3168-3170),
`MSENS_PREVIEW_TOKEN` (3182); via msens: `MS_PREVIEW` (`version.R:201`), `MS_PREVIEW_URL`
(`version.R:214`), `MS_TOKEN_SECRET` (`ver_token.R:38`), `MSENS_LOG_URL` (`analytics.R:314`),
`MSENS_ATLAS_CACHE` + `MSENS_ATLAS_TTL` (`version.R:32-51`), `MAPBOX_PUBLIC_TOKEN` (set at 191).

---

## 3. Version handling

### 3.1 The registry (`msens/R/version.R`)

Base URL: `atlas_base_url()` = `https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas`
(path-style, because the bucket name contains dots) (`version.R:97-99`).

- `latest.txt` → the promoted release (live value today: **`v7`**). `atlas_latest()` validates
  `^v[0-9]+[a-z]?$` and errors rather than guessing (`version.R:111-120`).
- `versions.json` → `{"versions":[{ver,status,access,released,title}]}`. `status ∈
  {released, prerelease, retired}` (anything else = hard error); `access ∈ {public, restricted}`,
  **derived fail-closed** when absent: `prerelease → restricted`, else `public`
  (`version.R:134-175`). Live content: v9 prerelease/restricted, v8 prerelease/restricted, v7
  released/public, v6…v1 retired/public.
- `{ver}/manifest.json` → `atlas_manifest()`; `validate_manifest()` requires
  `ver, status, grid_id, id_field, capabilities (non-empty), tables`, and `id_field ∈
  {mdl_seq, mdl_key}` (`version.R:300-333`).
- Caching: in-process memo + **disk cache** at `MSENS_ATLAS_CACHE` (default `tempdir()/msens-atlas`)
  with `MSENS_ATLAS_TTL` seconds (default **300**), write-then-rename (`version.R:38-74`).

### 3.2 What the manifest provides (confirmed from live v7 + v8)

| key | v7 | v8 |
|---|---|---|
| `ver`/`status`/`access` | v7 / released / *absent* → derived `public` | v8 / prerelease / restricted |
| `grid_id` | `usa05` | `global05` |
| `id_field` | `mdl_seq` | `mdl_key` |
| `capabilities` | cell_species_list T, native_representation F, programareas T, planareas T, zone_taxon T, score_cogs T, model_cogs T, taxon_model_reconstructed F | cell_species_list T, native_representation T, programareas T, planareas **F**, zone_taxon T, score_cogs T |
| `tables` | 12 HTTPS parquet hrefs | 13 (adds `native_asset`) |
| `metrics` | 17 metrics × {FULL, USA, AK, GA, PA} | 17 × {FULL, USA, AK, AT, GA, PA} = 102 rows |
| metric row fields | `metric_key, description, subregion_key, cog, rescale_min, rescale_max, colormap, lyr_order, category, label` | same |
| `overlays` | 1 row: `overlay_key="_outside_pra", subregion_key="FULL", cog, colormap="spectral_r"` | same |
| `zones` | 4 rows (ecoregion, planarea, programarea, subregion) with `tbl, fld, n, zone_set_key, pmtiles` | 3 rows (no planarea) |
| `grid` | `null` | `{nc:7200, nr:3600, xmin:-180, ymax:90, resx:0.05, resy:0.05, lon360:false}` |
| `title`/`released` | present | present |

COG hrefs are **content-addressed**:
`…/marine-atlas/cog/{grid_id}/{16-hex}.tif` (e.g. v8 FULL score = `cog/global05/…tif`). The hash
covers the *encoding*, not just the payload, so a re-render gets a new key (avoids the `/vsicurl`
stale-header class of bug).

Zone PMTiles: `…/marine-atlas/zones/{zone_set_key}/zones.pmtiles`; the layer inside is the **zone
type** (`programarea`, `ecoregion`, `subregion`, `planarea`), not the table name. Zone sets in use:
`ecoregion_2025-06`, `planarea_2025-06`, `programarea_2026-01`, `subregion_2025-06`
(+ `subregion_2025-08` published but unused by v7/v8).

### 3.3 v1–v7 vs v8+ differences the app absorbs

| concept | v1-v7 | v8+ | resolver |
|---|---|---|---|
| grid | `usa05` 3103×2006, **0-360 longitude**, xmin 141.10, ymax 82.60, ncell 6,224,618 | `global05` 7200×3600, -180..180, ncell 25,920,000 | `msens::grid_for_ver()`, `grid_spec_for()` (`grid.R:25-101`) |
| public model id | `mdl_seq` (int, renumbers per rebuild) | `mdl_key` (`ms_merge\|WORMS:137209`) | `manifest$id_field`; `fmt_spp_tbl` picks whichever column exists (`app.R:2710-2711`) |
| measurement column | `value` | `val` (served DBs carry **both**) | `msens::sdm_val_col()` |
| taxon validity | `is_ok` | `is_valid_usa` **plus** `is_marine` + `sp_cat NOT IN ('reptile','amphibian')` | `msens::sdm_cols()` (`calc.R:295-318`) |
| zone table names | v1/v2 unsuffixed, v3+ `_v{n}` | `_v8` | `zone_tbl_for()` (`app.R:212-226`) |
| zone_set_key in `zone` | absent (resolved via `data/zone_sets.csv`) | stamped | `msens::zone_set_resolve()` |
| ER columns | v1/v2 have **no** `extrisk_code/er_score/is_mmpa/is_mbta` | present | typed NULL substitution (`calc.R:410-415`) |
| `zone_taxon` er_score scale | v1/v2 `rl_score` fraction; v3-v7 `er_score` 1-100 with `rl_code` | fraction | `.zone_taxon_normalize()` (`calc.R:455-488`) |
| subregion keys | v7: FULL/USA/AK/GA/PA | v8: USA/AK/AT/GA/PA | `zone_all_key` (`app.R:940`) |
| cell_model tile width | 3103 | 7200 | `cell_grid_ncol(con)` from a `cell_grid` table (`cell_model.R:43-50`) |

### 3.4 Access control for restricted versions

- Policy is a property of the **process**, never of a request header: the preview Shiny Server
  block runs a 3-line wrapper that sets `MS_PREVIEW=1`; `msens::atlas_allow_access()` returns
  `c("public","restricted")` there and `"public"` on the public instance (`version.R:193-202`).
  Rationale: Shiny Server OSS opens its own localhost websocket, so no proxy header survives.
- `ver_of(qs)` parses `?ver=` and calls `atlas_resolve_ver(q$ver, allow_access = atlas_allow_access())`;
  failures fall back to the promoted release (`app.R:1070-1077`).
- `ver_of_req(req)` prefers the **`X-MS-Version`** header Caddy sets from the URL path
  (`/v9/scores/`), else `?ver=` (`app.R:1103-1106`).
- `ui(req)` embeds a **signed token** in a hidden input:
  `msens::ver_token_sign(ver)` = `"{ver}.{expiry}.{hmac-sha256(secret, "{ver}.{expiry}")}"`,
  ttl 24 h, secret `MS_TOKEN_SECRET` or per-process `/dev/urandom` (`ver_token.R:73-79`, `app.R:1412-1413`).
- `server` re-derives the session's version from that token only
  (`ver_token_verify` → `atlas_resolve_ver` through the instance policy) — `url_search`/
  `url_pathname` are client-supplied and are **not** trusted (`app.R:1090-1095`, `3285-3290`).
- A restricted `?ver=` on the public instance raises a classed `msens_restricted` condition, and
  the app shows a modal pointing at `msens::preview_app_url("scores", req)` =
  `https://preview.marinesensitivity.org/{ver}/scores/` (`app.R:1652-1663`).
- `preview_badge(req, ver)` shows `PREVIEW · {X-MS-User}` only when the instance is preview **and**
  the version's registry `access == "restricted"` (`app.R:1113-1122`).
- Curl-checkable sentinels in `<head>`: `<meta name="ms-ver" content="{ver}">` and
  `<meta name="ms-preview" content="0|1">` (`app.R:1185-1186`).
- `product_urls(ver, access)` (`ver_token.R:159-182`): public →
  `https://app.marinesensitivity.org/{ver}/scores|species/`, `https://marinesensitivity.org/docs/{ver}/`,
  `https://marinesensitivity.org`; restricted → `https://preview.marinesensitivity.org/{ver}/scores|species/`,
  `…/docs/{ver}/`, same home.

---

## 4. URL / query parameters and deep-link behaviour

| parameter | meaning | default | handling |
|---|---|---|---|
| `?ver=` | release to render (`v1`…`v9`, or `latest`) | `latest.txt` (today `v7`) | `ver_of()`/`ver_of_req()` (`app.R:1070-1106`); unknown → modal "Unknown data version"; restricted-on-public → modal "under review"; resolvable-but-unserved → modal "not served here yet" (`app.R:1647-1680`) |
| `X-MS-Version` (header, not a query param) | version from the URL path `/v{n}/scores/`, set by Caddy | — | preferred over `?ver=` (`app.R:1104-1105`) |
| `?splash=false` | suppress the welcome modal for this load | unset | read in client JS; sets `show_splash_pref = "false"` (`app.R:1292-1300`) |
| *(localStorage)* `msens_mapgl_show_splash` | persisted welcome-modal preference (`"true"`/`"false"`) | `"true"` when null | set by the `saveSplashPref` custom message (`app.R:1301-1303`, `1792-1796`) |
| *(localStorage)* `msens_client_id`, *(sessionStorage)* `msens_session_id` | analytics identity | generated | `msens::ga_js` (`analytics.R:345-356`) |

**URL canonicalisation**: on every session an observer runs
`updateQueryString(sprintf("/%s/scores/", ver), mode = "replace")` (`app.R:1688-1690`) — a
**relative** path (absolute would be cross-origin on the preview host and `replaceState` throws).
So `/scores/?ver=v7` becomes `/v7/scores/`.

**No other app state is bookmarkable** — study area, unit, layer, palette, clicked cell, clicked
zone, report areas are all session state with no URL representation. (An obvious win for the JS
port: make them URL params.)

**Links out** carry the version implicitly by being relative (`../species/?mdl_key=…`) or by
`product_urls(ver)`.

---

## 5. UI layout

`ui_impl(req)` is `bslib::page_sidebar()` (`app.R:1175-1609`), `fillable_mobile = TRUE`.

### 5.1 `<head>` (`app.R:1179-1405`)

- `favicon.ico` link; `<meta name="ms-ver">`, `<meta name="ms-preview">`.
- `msens::ga_head("scores" | "scores-preview", app_version = APP_VERSION, ip = ms_client_ip(req))`.
- ~100 lines of inline CSS: fullscreen girafe sizing (`calc(100vh - 120px)`), `#plot_flower{height:300px}`,
  `.header-right/.header-nav/.nav-here/.nav-sep`, `.map-container{position:relative;flex:1 1 auto;min-height:0}`,
  `.map-container > .html-widget{height:100%!important}`, `.map-loading-overlay` (+ `.hidden`,
  spinner `@keyframes msens-spin`), `.tab-short{display:none}`, the sidebar-hint pill
  (`.sidebar-collapsed > .collapse-toggle::after { content:'Map options' }`, horizontal on phones,
  `writing-mode: vertical-rl` at ≥576px), and a `@media (max-width: 575.98px)` block setting
  `--bslib-spacer`/`--bslib-mb-spacer` to 0.5rem, card body padding 0.5rem, wrapping `.ms-header`,
  swapping `.tab-long`/`.tab-short`, tightening `.nav-tabs .nav-link` padding.
- Inline JS (`app.R:1291-1405`):
  - on `shiny:connected`, read `?splash` then `localStorage` → `Shiny.setInputValue("show_splash_pref", …)`;
  - `saveSplashPref` handler → `localStorage.setItem`;
  - `hideMapOverlay` handler → add `.hidden` to an element id;
  - report tab plumbing: on `#btn_rpt_submit` click **synchronously** `window.open('', '_blank')`
    (user-activation window), push to `window._msens_report_pending`; `setReportReqId` shifts that
    window into `window._msens_report_wins[reqId]`; `openUrl` points it at the finished URL and
    closes it after 2 s, with a hidden-anchor download fallback;
  - `setPraTooltips` handler: stores a `{zone_key: html}` map and binds `mousemove`/`mouseleave`
    on the named layer (default `programarea_fill`, key prop `programarea_key`, name prop
    `programarea_name`), showing a `maplibregl.Popup({closeButton:false, closeOnClick:false})`
    and setting `cursor:pointer`. Handlers are bound once **per layer id**.
- `useConductor()` (conductor tour assets).
- Hidden signed-version input: `<div style="display:none"><input id="ms_ver_token" type="text" value="{token}"></div>`.

### 5.2 Title bar (`app.R:1414-1427`)

`div.ms-header` containing: `span.ms-title` → `"BOEM Marine Sensitivity "` +
`actionLink("show_versions", "({ver})", title="data version - click to switch")` + optional
`preview_badge`; then `product_nav(ver, "scores")` (Scores · Species · Docs · Home [· Sign out on
preview → `/cdn-cgi/access/logout`]); then `div.header-right` with `actionLink("btn_about","About")`
and `bslib::input_dark_mode(id = "tgl_dark", mode = "dark")`.
`window_title = "BOEM Marine Sensitivity ({ver})"` (`app.R:1433`).

### 5.3 Sidebar (`app.R:1434-1489`)

`sidebar(open = list(desktop = "open", mobile = "closed"))`:

| inputId | control | label | choices | default | notes |
|---|---|---|---|---|---|
| `sel_subregion` | `selectInput` (wrapped in `#tour_subregion`) | "Study area" | `setNames(d_sa$key, d_sa$label)` → FULL/AK/AT/GA/PA | FULL | camera only |
| `sel_unit` | `selectInput` (`#tour_unit`) | "Spatial units" | `"cell" = "Raster cells (0.05°)"` + one entry per drawable scored zone unit | `cell` | derived, never hardcoded |
| — | note text | — | — | — | shown only when `primary_unit != "programarea"`: "{ver} predates the BOEM Program Areas — it reports on {label}." |
| `sel_lyr` | `selectInput` (`#tour_lyr`) | "Layer" | `lyr_choices` grouped by category | `lyr_default` (order 1 = overall score) | |
| `sel_palette` | `selectInput` | "Color palette" | Spectral/Viridis/Cividis/Magma | `spectral_r` | |
| `tgl_sphere` | `input_switch` | "Sphere" | — | `TRUE` | globe vs mercator |

### 5.4 Main area — `navset_card_tab(id = "main_tabs", full_screen = TRUE)` (`app.R:1491-1608`)

1. **Map** (`value = "Map"`): `div.map-container` → `div#map-overlay.map-loading-overlay` (spinner +
   "Loading map…") + `map_output("map")`.
2. **Plot of Scores** (`value = "Plot of Scores"`, short label "Plot"): a `card(full_screen = TRUE)`
   with `card_header(textOutput("flower_panel_title"))` and `girafeOutput("plot_flower", height="100%")`.
3. **Table of Species** (`value = "Table of Species"`, short "Table"): card header =
   `textOutput("spp_tbl_hdr")` + `actionButton("btn_tbl_info", icon = circle-info, class="btn-sm")`
   + `downloadButton("download_tbl", "Download CSV", class="btn-sm")`; body = a nested
   `navset_card_tab` with **Table** (`DTOutput("spp_tbl")`) and **Composition**
   (a literal note "NOTE: The 'bird' component has yet to be added to this visualization." +
   `plotlyOutput("spp_comp")`).
4. **Report** (`value = "Report"`): `layout_sidebar` with a 360px sidebar —
   `textInput("rpt_title", "Report title", "BOEM Marine Sensitivity Report")`;
   `selectInput("rpt_ver", "Data version", rpt_ver_choices, selected = ver)`;
   `radioButtons("rpt_format", "Output format", c(HTML="html", "Word (.docx)"="docx", PDF="pdf"),
   selected="html", inline=TRUE)`; `hr()`; `h5("Add area")` + muted instructions;
   `textInput("rpt_area_label","Label for next area","Area 1")`;
   `actionButton("btn_add_drawn","Add drawn polygon")` and `actionButton("btn_add_pra","Add selected
   Program Area")`; `hr()`; `h5("Areas")` + `uiOutput("rpt_areas_ui")`; `hr()`;
   `actionButton("btn_rpt_submit","Generate report", class="btn-primary w-100")`.
   Main pane: 700px `div.map-container` with `#map-rpt-overlay` and `map_output("map_rpt", height="700px")`.

### 5.5 Modals

| trigger | content | line |
|---|---|---|
| `show_versions` (the `(v8)` link) | size `l`; "This app renders one published release…" + `msens::version_picker_html(ver, href = function(v) product_urls(v, access = ver_access(v))[["scores"]])` — a `list-group` of every version newest-first with `pre-release`/`retired`/`🔒 restricted` badges, title and date | 1619-1633 |
| startup (unless suppressed) and `btn_about` | **Welcome**: `<img src="{docs}figures/overview-methods.svg">`, intro paragraph, links to the species app and docs (both `target="_blank"`), footer = `checkboxInput("chk_show_splash", …, value = TRUE)` + `actionButton("btn_tour","Take a Tour")` + `modalButton("Explore")` | 1743-1789 |
| `btn_tbl_info` | size `l`; markdown glossary of every species-table column incl. the er_score rule: `NMFS\|FWS:EN=100, NMFS\|FWS:TN=50, IUCN:CR=50, IUCN:EN=25, IUCN:VU=5, IUCN:NT=2, IUCN:LC\|DD=1`, MMPA=20, MBTA=10 | 2604-2628 |
| `?ver=` unresolved / restricted / unserved | three distinct explanatory modals | 1652-1679 |

### 5.6 Guided tour (conductor, `app.R:1799-1873`)

10 steps, each `title/text/el/position`: `#tour_subregion` (right), `#tour_unit` (right),
`#tour_lyr` (right), `[data-value='Map'].nav-link` (bottom), `.layers-control` (right),
`.maplibregl-ctrl-geocoder` (left), `.maplibregl-ctrl-fullscreen` (left),
`.maplibregl-ctrl-zoom-in` (left), `[data-value='Plot of Scores'].nav-link` (bottom),
`[data-value='Table of Species'].nav-link` (bottom), `[data-value='Report'].nav-link` (bottom).
Start removes the modal then `session$onFlushed(tour$start, once = TRUE)`.

### 5.7 Mobile

bslib's own 575.98px breakpoint: sidebar becomes a toggle row + full-width overlay over the map;
`fillable_mobile = TRUE` is what gives the map non-zero height; tab labels swap to the short forms
(`tab_title(long, short)`, `app.R:1149-1151`) so the strip stays one row; the collapsed-sidebar hint
pill is a pure `::after` on bslib's chevron (hit-tested as part of the button).

---

## 6. Map

### 6.1 Base map and projection

`mapgl::maplibre(style = carto_style("dark-matter"), projection = ifelse(sphere,"globe","mercator"))`
(`app.R:1978-1981`). `carto_style("dark-matter")` =
**`https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json`** (no API key).
No light-theme basemap swap is implemented (TODO at `app.R:6`).
Initial camera: `set_view(center = initial_view$center, zoom = initial_view$zoom)` (`app.R:1984`).

### 6.2 Sources and layers created up front (`build_initial_map`, `app.R:1973-2038`)

Order matters (MapLibre `before_id` must name an existing layer):

1. **One PMTiles source + line layer per scored zone unit** via `msens::add_pmline()`
   (`viz.R:159-178` → `mapgl::add_pmtiles_source(id, url)` + `add_line_layer(...)`):
   - source id `"{type}_src"`, layer id `"{type}_ln"`, `source_layer = "{type}"`, `url` = the
     manifest's `zones[].pmtiles`.
   - style from `msens::zone_line_args(type)` (`zone_style.R:22-55`):
     `programarea`/`planarea` → white, width 1, opacity 1;
     `ecoregion` → black, width 3, opacity 1;
     `subregion` → `#d9d9d9`, width 2, opacity 0.7, `line_dasharray = c(3,3)`;
     anything else → white, width 0.5, opacity 0.45.
2. **Standalone ecoregion outline** `id = "er_ln"`, `source_id = "er_src"`,
   `before_id = before_er` — only when the release has ecoregion tiles **and** ecoregion is not
   itself a scored unit (`app.R:1999-2003`).
3. **Zone label symbol layers** via `msens::add_pmlabel()` (`viz.R:195-237`), one per unit whose
   `zone_label_args(type)` is non-NULL (i.e. NOT subregions): `id = "{type}_lbl"`,
   `source = zone_pts(type)` (an sf point layer), `text_field = get_column("{type}_key")`,
   `text_allow_overlap = TRUE`, colors/sizes/halos from `zone_style()`
   (programarea: white 12px, halo `rgba(0,0,0,0.75)` 1; ecoregion: black 16px, halo
   `rgba(255,255,255,0.85)` 1.5).
4. **Score raster** `msens::add_cell_tiles(initial_tile_url, raster_opacity = 0.6,
   before_id = before_r)` → `add_raster_source(id="r_src", tiles=<url>, tileSize=256)` +
   `add_raster_layer(id="r_lyr", raster_resampling="nearest")` (`viz.R:294-312`).
5. **Outside-Program-Areas mask** `add_cell_tiles(outside_pra_tile_url, id="outside_pra_lyr",
   source_id="outside_pra_lyr", raster_opacity = 0.55, visibility = "none", before_id = before_r)`.
6. `mapgl::add_legend(get_lyr_name(lyr_default), values = signif(initial_rescale, 3),
   colors = get_pal_colors("spectral_r", 11), position = "bottom-right")` — a continuous
   gradient legend with the two endpoints labelled.
7. Controls: `add_fullscreen_control()`, `add_navigation_control()`, `add_scale_control()`,
   `add_layers_control(layers = c(zone_ctrl_layers(), list("Raster cell values" = "r_lyr",
   "Cells outside Program Areas" = "outside_pra_lyr")))`,
   `add_geocoder_control(placeholder = "Go to location")`.
   **The geocoder is OSM Nominatim**, not Mapbox: mapgl's MapLibre path calls
   `https://nominatim.openstreetmap.org/search?q={query}&format=geojson&polygon_geojson=1&addressdetails=1`
   and centres on each feature's bbox midpoint (verified in `mapgl/htmlwidgets/maplibregl.js`).

### 6.3 Exact tile URL templates

**Score / metric COG tiles** — `msens::cog_tile_url()` (`viz.R:400-429`):

```
https://titiler-v8.marinesensitivity.org/cog/tiles/WebMercatorQuad/{z}/{x}/{y}.png
  ?url={URLencode(cog_url, reserved=TRUE)}
  &colormap_name={spectral_r|viridis|cividis|magma}
  &rescale={rescale_min},{rescale_max}
```

with `cog_url` = the manifest's `metrics[metric_key, subregion_key="FULL"].cog`, e.g.
`https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/cog/global05/a3a8eb5ab4bec53b.tif`.

**Outside-PRA binary mask** — same helper with `color = "#222222"` (`viz.R:412-420`), which emits an
explicit value→RGBA colormap instead of a named ramp:

```
https://titiler-v8.marinesensitivity.org/cog/tiles/WebMercatorQuad/{z}/{x}/{y}.png
  ?url={enc}&colormap={URLencode('{"1":[34,34,34,255]}')}
```

**SQL fallback tiles** (only when no manifest/score_cogs) — `msens::cell_tile_url()` (`viz.R:347-379`):

```
https://titiler-v8.marinesensitivity.org/msens/tiles/{z}/{x}/{y}.png
  ?sql={base64url(canonicalized SQL)}&colormap={pal}&rescale={min},{max}&mtime={YYYYMMDDTHHMMSSZ}
```

canonicalisation = `trimws` + collapse runs of whitespace to one space; base64url = standard base64
with `+`→`-`, `/`→`_`, `=` stripped. The stats companion is
`…/msens/statistics?sql={same}&mtime={…}` → `{n,min,max,mean,std,p2,p50,p98}` (`viz.R:447-466`).

**Point value** — `msens::cog_point_value()` (`viz.R:1036-1050`):

```
https://titiler-v8.marinesensitivity.org/cog/point/{lon},{lat}?url={URLencode(cog_url)}
```
→ JSON; the app takes `values[[1]]`, `NA` on any failure.

**Cell-id lookup COG** — `msens::grid_cellid_url(grid_id)` (`grid.R:261-262`):
`https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/grid/{usa05|global05}/cellid.tif`
(INT4U, **no overviews**, since ids are categorical).

### 6.4 Layer-state updates (`app.R:2084-2363`)

One observer on `c(sel_subregion, sel_unit, sel_lyr, sel_palette, main_tabs, rpt_map_loaded())`,
`ignoreInit = FALSE`. It applies the *same* update function to both `maplibre_proxy("map")` and
`maplibre_proxy("map_rpt")`.

**Cell branch** (`unit == "cell"`, `app.R:2095-2150`): clears `pra_lyr`, `outside_pra_lyr`, `r_lyr`,
`r_src`, the legend; re-adds the score raster at `meta$tile_url` (opacity 0.6, `before_r`) and the
mask (opacity 0.55, hidden); re-adds the legend with `values = signif(meta$rescale, 3)` and 11
palette colors; `mapgl::fly_to(center = meta$view$center, zoom = meta$view$zoom)`; then
`clear_controls("layers")` and re-adds a layers control.
**KNOWN BUG to fix in the port**: this control is hardcoded to `pra_ln`, `pra_lbl`, `er_ln`,
`r_lyr`, `outside_pra_lyr` (`app.R:2136-2143`) while the layers actually created are
`programarea_ln`, `programarea_lbl`, `ecoregion_ln`… — so after any sidebar change in cell mode
three of the five switches are dead. Likewise `clear_layer("pra_lyr")` names an id that no longer exists.

**Zone-choropleth branch** (any scored unit, `app.R:2237-2360`):
- `zu <- zone_unit_row(unit)`; `kcol = "{unit}_key"`, `ncol = "{unit}_name"`; `rx$clicked_cell <- NULL`.
- `zone_keys <- zu$keys[[1]]` — **all** of the unit's zones, never filtered by study area;
  `zone_filter <- c("in", kcol, zone_keys)`.
- Values:

```r
d_zone <- zone_vals() |> filter(fld == kcol, value %in% zone_keys) |>
  select(zone_key = value, zone_seq) |>
  inner_join(zone_metric_vals(), by = "zone_seq") |>
  inner_join(tbl(con_sdm,"metric") |> filter(metric_key == lyr), by = "metric_seq") |>
  select(zone_key, value) |> collect()
```

  Empty → a warning `showNotification` and no draw (guards the `InfM/-InfM` legend bug).
- Colours: `rng_zone <- range(value)`; `cols_zone <- colorRampPalette(get_pal_colors(pal, 11),
  space = "Lab")(11)`;
  `val_scaled = (value - min) / max(max-min, 1e-6)`;
  `col_idx = min(max(round(val_scaled * 10) + 1, 1), 11)`; `fill_color = cols_zone[col_idx]`.
- Tooltips: `"{name}: {round(value)}"` (name from `zone_pts(unit)`, else the key), pushed to the
  browser as `setPraTooltips({tips, layer: "{unit}_fill", keyProp, nameProp})`.
- Draw: clear `r_lyr`, `r_src`, `outside_pra_lyr` and **every** `"{t}_fill"`; then
  `add_fill_layer(id = "{unit}_fill", source = "{unit}_src", source_layer = zu$source_layer,
  fill_color = match_expr(column = kcol, values = zone_key, stops = fill_color, default = "lightgrey"),
  fill_opacity = 0.7, fill_outline_color = "white",
  hover_options = list(fill_color = "purple", fill_opacity = 1),
  before_id = "{unit}_ln", filter = zone_filter)`; re-add the hidden mask; legend with
  `values = round(rng_zone, 1)`; `fly_to(sr_view(sr_key))`; rebuild the layers control as
  `c(zone_ctrl_layers(), "{label} values" = "{unit}_fill", "Cells outside Program Areas" = "outside_pra_lyr")`.

### 6.5 Fit-bounds / antimeridian

There is **no `fit_bounds` anywhere** — deliberately. All camera moves are `set_view`/`fly_to` with a
centre+zoom from `msens::study_areas()`, because a bbox inverts across the antimeridian (EBS and PIS
both cross it; PIS's true 67.5° span reads as 360°). The supporting helpers exist in msens for the
species app: `lon_span()`/`lon_span_agg()` keep the narrower of the -180..180 and 0..360 spans and
may return `xmax > 180` (MapLibre accepts it), and `bbox_spans_globe(bb, max_span = 350)` rejects a
useless whole-world camera (`grid.R:129-203`).
Label points are computed after `st_shift_longitude()`, so cached label CSVs hold 0-360 longitudes
(`app.R:705`).

### 6.6 Click behaviour

**Cell click** (`app.R:2366-2413`) — `input$map_click` gives `{lng, lat}`:
1. `cell_id <- msens::cog_point_value(msens::grid_cellid_url(msens::grid_for_ver(ver)), lng, lat)`
   (`/cog/point/{lon},{lat}?url=…cellid.tif`).
2. If NA, arithmetic fallback `msens::cell_from_lonlat(lng, lat, grid_spec_for(grid_id))`
   (`grid.R:219-245`): shift into the grid's own frame when `lon360`;
   `col = floor((lon - xmin)/resx) + 1`; `row = ceiling((ymax - lat)/resy)` (row 0 → 1);
   validity tested on the coordinates, not clamped indices; `cell_id = (row-1)*nc + col`.
3. `cell_val <- msens::cog_point_value(cog_of(sel_lyr, sel_subregion)$url, lng, lat)` — i.e. the
   value comes from the *same* COG the user is looking at.
4. `rx$clicked_cell <- list(lng, lat, cell_id, value, lyr)`; `rx$clicked_pra <- NULL`.
   Three historical bugs this design avoids are documented at `app.R:2379-2391`: band named
   `r_cellid` vs `depth_mean`; 0-360 shifting; **a cached `SpatRaster` segfaults** across sessions.

**Zone click** (`app.R:2414-2426`): `input$map_feature_click` → `rx$clicked_pra <- list(id,
properties, unit = input$sel_unit)`; `rx$clicked_cell <- NULL`. Key/name are derived from the unit
(`zone_key_of`/`zone_label_of`, `app.R:1876-1886`), never assumed to be `programarea_*`.

**Hover**: no Shiny round-trip — the pure-JS `setPraTooltips` handler shows a MapLibre popup with
`"{name}: {round(value)}"`; `hover_options` additionally repaints the hovered polygon purple at
full opacity.

**Highlights** (proxy observers, applied to both maps):
- clicked zone → `add_line_layer(id="pra_highlight_ln", source="{unit}_src",
  source_layer=…, line_color="#ff00aa", line_width=4, filter=list("==","{unit}_key",key))` (`app.R:3015-3033`);
- clicked cell → `add_circle_layer(id="cell_highlight", source = <sf point at the click>,
  circle_color="#ffffff", circle_opacity=0.4, circle_radius=8, circle_stroke_color="#ff00aa",
  circle_stroke_width=4)` (`app.R:3039-3059`).

**Loading overlays**: hidden on the first `input$map_zoom` / `input$map_rpt_zoom` (mapgl emits these
from `map.on("load")`), via the `hideMapOverlay` custom message (`app.R:2062-2078`).

### 6.7 Draw tools (Report tab only)

`add_msens_draw_control(m)` (`app.R:2042-2055`) → `mapgl::add_draw_control(position="top-right",
fill_color="#fbb03b", line_color="#fbb03b", fill_opacity=0.2, controls=list(point=FALSE,
line_string=FALSE, polygon=TRUE, trash=TRUE, combine_features=FALSE, uncombine_features=FALSE))`
— i.e. Mapbox GL Draw with only polygon + trash.

Geometry capture: `input$map_rpt_drawn_features` → `msens::drawn_features_sf()` (`viz.R:996-1011`),
which accepts **both** shapes mapgl has used (a JSON string, or a parsed list → re-serialised with
`jsonlite::toJSON(auto_unbox=TRUE)`), reads it with `sf::read_sf`, and sets CRS 4326 if missing.
`rpt_drawn_sf()` keeps only the **last** feature (`app.R:2914-2918`).

On "Add drawn polygon" (`app.R:3062-3085`): `wkt <- st_as_text(st_geometry(p))`; if
`nchar(wkt) > 8000` it is simplified with `st_simplify(dTolerance = 0.01, preserveTopology = TRUE)`
and the user is warned; the area is appended as `list(label, kind = "wkt", value = wkt)` and the
label input auto-advances to `"Area {n+1}"`.

Added areas are drawn on `map_rpt` (`app.R:2954-2985`) as `rpt_added_fill` (`#ff00aa`, opacity 0.15),
`rpt_added_ln` (`#ff00aa`, width 4) and `rpt_added_lbl` (symbol at `st_point_on_surface`,
`text_field = get_column("label")`, size 14, white with a `#ff00aa` halo width 2, offset `c(0,-1)`).
`rpt_areas_sf()` rebuilds that sf from `rx$rpt_areas`: `kind == "pra"` → `pra_geom() |>
filter(programarea_key == value) |> st_geometry()`; else `st_as_sfc(wkt, crs = 4326)` (`app.R:2924-2949`).

**Note**: the app never turns a drawn polygon into cells or scores itself — that is the report
API's job. `msens::cells_in_polygon()` + `scores_for_cells()` exist for that (§9 and §13).

---

## 7. Every output — exact computation

### 7.1 `flower_panel_title` (text, `app.R:1899-1909`)

`"Cell {cell_id}"` if a cell is clicked; else the clicked zone's name; else
`"Full study area (default)"`.

### 7.2 `plot_flower` (ggiraph girafe, `app.R:2432-2576`)

Three data paths, all producing `component, score, even = 1` and all dropping `component == "all"`:

**(a) clicked cell**
```r
tbl(con,"metric") |> filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
  left_join(cell_metric_vals(), by = "metric_seq") |>
  filter(cell_id == <clicked>) |>
  select(metric_key, score = value) |>
  mutate(component = metric_key |> str_replace("extrisk_","") |>
           str_replace("_ecoregion_rescaled","") |> str_replace("_"," "), even = 1) |>
  filter(component != "all") |> collect()
```
title `"Cell ID: {cell_id} (x: {round(lng,3)}, y: {round(lat,3)})"`.

**(b) clicked zone** — resolve `z_seq` by **field + key** (never table name):
`zone_vals() |> filter(fld == "{unit}_key", value == key) |> pull(zone_seq)`, then the same
pipeline against `zone_metric_vals()` filtered on `zone_seq`; title = the zone name.

**(c) nothing clicked** — read `d_flower_default |> filter(subregion_key == zone_all_key)`;
title `"Full study area"`.

Rendering — `msens::plot_flower()` → `ggplot_flower()` (`viz.R:719-829`):
- centre number = `round(weighted.mean(score, even, na.rm = TRUE))` (with `even = 1` this is the
  plain mean of the components);
- bars: sorted by component, `ymax = cumsum(even)`, `ymin = lag(ymax, 0)`, `xmax = score`, `xmin = 0`,
  drawn as `geom_rect_interactive` with `coord_polar(theta = "y")`, `xlim(c(-10, max(score)))`,
  white borders, `alpha = 0.5`;
- fill palette: `scales::hue_pal()(8)` fixed to the names
  `invertebrate, mammal, other, primprod, turtle, bird, coral, fish` (**note**: v8's
  `primary producer` and `primprod` are separate names, so a v8 component can fall outside this
  named scale);
- tooltip glue string `"{component}: {round(score, 2)}"`;
- theme minimal, legend bottom, 20pt margins; wrapped by `ggiraph::girafe(opts_sizing(rescale=TRUE,
  width=1), opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;"))`.

### 7.3 `spp_tbl_hdr` (text) and the species table source `get_spp_tbl()` (`app.R:2631-2697`)

| state | header | filename stem | computation |
|---|---|---|---|
| cell clicked | `Species for Cell ID: {cell_id}` | `species_cellid-{cell_id}` | `msens::species_for_cells(con, data.frame(cell_id, pct_covered = 100))`, wrapped in `tryCatch` → warning notification + "— unavailable" header |
| zone clicked | `Species for Program Area: {name}` | `species_programarea-{lowercased name with first space → '-'}` | `msens::species_for_zone(con, "{unit}_key", key)` |
| nothing clicked | `Species in Full study area` | `species_{zone_all_key}` | `msens::species_for_zone(con, "subregion_key", zone_all_key)` |

`msens::species_for_zone()` (`calc.R:596-625`) **prefers the precomputed `zone_taxon` table**:

```sql
SELECT * FROM zone_taxon WHERE zone_fld = '<fld>' AND zone_value = '<val>'
```
then `.zone_taxon_normalize()` (`calc.R:455-488`) maps each generation's spelling onto
`sp_cat, sp_common, sp_scientific, taxon_id, taxon_authority, er_code, er_score (0-1),
is_mmpa, is_mbta, mdl_key, area_km2, avg_suit` and **recomputes** the share columns.
Falling back to the live aggregation uses

```sql
SELECT zc.cell_id, zc.pct_covered FROM zone_cell zc JOIN zone zn USING (zone_seq)
WHERE zn.fld = '<fld>' AND zn.val = '<val>'
```

`msens::species_for_cells()` (`calc.R:521-534`) builds
`SELECT * FROM (VALUES (id, pct), …) AS v(cell_id, pct_covered)` and prunes partitions with
`cell_model_tiles(cell_id, ncol = cell_grid_ncol(con))`.

Both feed **the one aggregation**, `.species_sql()` (`calc.R:363-437`) — verbatim:

```sql
WITH z AS ({cells_sql})
SELECT t.sp_cat,
       t.common_name              AS sp_common,
       t.scientific_name          AS sp_scientific,
       t.taxon_id,
       t.taxon_authority,
       t.extrisk_code             AS er_code,
       t.er_score / 100.0         AS er_score,
       t.is_mmpa                  AS is_mmpa,
       t.is_mbta                  AS is_mbta,
       CAST(mc.{mkey} AS VARCHAR) AS mdl_key,
       sum(c.area_km2 * z.pct_covered / 100.0)                       AS area_km2,
       sum(mc.{val} * z.pct_covered) / sum(z.pct_covered) / 100.0    AS avg_suit
FROM {mc_from}
JOIN z      USING (cell_id)
JOIN cell c USING (cell_id)
JOIN taxon t ON t.{tkey} = mc.{mkey}
WHERE t.{valid} AND t.is_marine
  AND t.sp_cat NOT IN ('reptile', 'amphibian')
GROUP BY 1,2,3,4,5,6,7,8,9,10
```
where `{mc_from}` is, when `cell_model` exists (the preferred, cell-oriented surface):
```sql
(SELECT cm.cell_id, cm.val, mo.mdl_key FROM cell_model cm JOIN model mo USING (mdl_id)
 WHERE tile IN (…)) mc
```
(v7's `cell_model` stores `mdl_seq`/`mdl_key` directly and skips the `model` join), otherwise
plain `model_cell mc`. On v1/v2 the four ER expressions become `CAST(NULL AS VARCHAR|DOUBLE|BOOLEAN)`.

Then `.species_shares()` (`calc.R:491-501`):
```
suit_er          = avg_suit * er_score
suit_er_area     = avg_suit * er_score * area_km2
cat_suit_er_area = sum(suit_er_area) over sp_cat
pct_cat          = suit_er_area / cat_suit_er_area
order by sp_cat, sp_scientific
```

### 7.4 `fmt_spp_tbl()` (`app.R:2701-2765`) — display frame

`id_col` = `mdl_key` if present else `mdl_seq`; then
- `model_id = as.character(<id_col>)`;
- `model_url = "../species/?mdl_key={URLencode(model_id, reserved = TRUE)}"` (relative, so it stays
  under `/v{n}/` on either host; URL-encoded because a v8 key contains `|` and `:`);
- `taxon_str = "{taxon_authority}:{taxon_id}"`;
- `taxon_url = if (taxon_authority == "botw") "https://birdsoftheworld.org" else
  "https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}"`;
- select/rename → `component(=sp_cat), taxon_authority, taxon_id, taxon_str, taxon_url,
  scientific(=sp_scientific), common(=sp_common), er_code, er_score, is_mmpa, is_mbta,
  model_id, model_url, area_km2, avg_suit, pct_component(=pct_cat)`; `arrange(component, scientific)`.

### 7.5 `spp_tbl` (DT, `app.R:2768-2825`)

Stores the unformatted frame in `rx$spp_tbl` (for the CSV), then renders:
`taxon = <a href="{taxon_url}" target="_blank">{taxon_str}</a>`,
`model = <a href="{model_url}" target="_blank">{model_id}</a>`, relocates them, drops the helper
columns, renames `component→cat`, `pct_component→pct_cat`.
**Final column order**: `cat, taxon, scientific, common, er_code, er_score, model, is_mmpa,
is_mbta, area_km2, avg_suit, pct_cat`.
`datatable(escape = FALSE, rownames = FALSE, fillContainer = TRUE, filter = "top",
class = "display compact", extensions = c("ColReorder","KeyTable","Responsive"),
options = list(colReorder = TRUE, keys = TRUE, pageLength = 5, lengthMenu = c(5,50,100),
scrollX = TRUE, dom = "lfrtip"))`, `server = TRUE`.
Formats: `formatPercentage("er_score", 0)`, `formatPercentage(c("avg_suit","pct_cat"), 2)`,
`formatSignif("area_km2", 4)`.

### 7.6 `spp_comp` — Composition treemap (plotly, `app.R:2850-2893`)

```r
fmt_spp_tbl() |> filter(tolower(taxon_authority) == "worms") |>
  mutate(taxon_id_chr = as.character(taxon_id)) |>
  inner_join(d_taxonomy |> select(-component) |> mutate(species_id_chr = as.character(species_id)) |>
               select(-species_id),
             by = join_by(taxon_id_chr == species_id_chr)) |>
  mutate(name = "{scientific} ({common}; worms:{taxon_id})", n = 1) |>
  select(component, Kingdom, Phylum, Class, Order, Family, Genus, name, n)
```
→ `plotme::count_to_treemap(d)` = `plotly::plot_ly(type = "treemap", branchvalues = "total",
hoverinfo = "text")` over that hierarchy. Theme colours come from
`bslib::bs_get_variables(bs_current_theme(), "body-bg"/"body-color")` (or the `-dark` variants when
`input$tgl_dark == "dark"`), applied via `layout(font$color, plot_bgcolor, paper_bgcolor)`.
Birds are excluded by construction (BoTW ids are not in the WoRMS hierarchy CSV) — hence the
literal note in the UI.

### 7.7 `rpt_areas_ui` (`app.R:3108-3126`)

`"{i}. {label} ({kind_lbl})"` where `kind_lbl` maps `pra`→"Program Area", `wkt`→"drawn", plus a
per-row `actionButton("rpt_del_{i}", icon = trash, class = "btn-sm btn-outline-danger")`. Delete
observers are created dynamically, `once = TRUE` per index (`app.R:3129-3144`).

### 7.8 `click_info` (`app.R:2579-2593`) — defined but **never placed in the UI** (dead).

---

## 8. Downloads / exports

- **`download_tbl`** (`app.R:2828-2847`): filename `"{rx$spp_tbl_filename}_{Sys.Date()}.csv"`
  (e.g. `species_USA_2026-09-20.csv`); content = `write_csv(rx$spp_tbl, file)` — the **unformatted**
  `fmt_spp_tbl()` frame including `taxon_authority, taxon_id, taxon_str, taxon_url, model_id,
  model_url`. `rx$spp_tbl` is only populated by the DT render, so the Table tab must have rendered.
  Emits a `download_species_csv` tracking event with `n_rows, area, subregion, unit, layer`.
- **No PNG/plot export** and no raster/GeoPackage download. The TODO at `app.R:11` ("Add Download
  button for cell (tif), pa (gpkg), er (gpkg) … as zip") is unimplemented.
- Reports (HTML/DOCX/PDF) are downloads in effect — see §9.
- The API also exposes `GET /species.csv?ver=&kind=pra|wkt&value=&label=` (`api/plumber.R:446-490`),
  used by the report's per-area download links, not by this app.

---

## 9. Report generation

1. User builds `rx$rpt_areas` — an unnamed list of `list(label, kind ∈ {"wkt","pra"}, value)`
   (`app.R:3062-3105`).
2. `btn_rpt_submit` (`app.R:3149-3269`):
   - empty list → `report_submit status="no_areas"` + error notification;
   - `req_id <- "rpt_{unixtime}_{random}"`, pushed to the browser (`setReportReqId`) so the
     pre-opened placeholder tab can be matched to this response;
   - body `list(title = input$rpt_title, ver = input$rpt_ver, format = input$rpt_format,
     areas = areas)`;
   - endpoint `Sys.getenv("MSENS_REPORT_URL", "https://api.marinesensitivity.org/report")`;
   - on the preview instance only, header `X-MS-Preview-Token: {MSENS_PREVIEW_TOKEN}`;
   - sticky spinner notification ("Generating report in new tab — this may take a couple of
     minutes…", `duration = NULL`, no close button);
   - the POST runs in a `promises::future_promise` (2-worker multisession) with
     `httr2::req_timeout(600)`;
   - on fulfil: `resp$url` → `session$sendCustomMessage("openUrl", list(url, reqId))`, which points
     the stashed tab at the file and closes it 2 s later; else an error notification. Both branches
     log `report_result` with `status`, `ms`, `format`, `n_areas`, `report_url`/`error`.
3. Server side (`api/plumber.R:327-444`): validates format ∈ {html,pdf,docx}, non-empty areas, and
   gates a restricted `ver` on the preview token (403 otherwise);
   `key <- substr(digest(list(title, areas_json, ver, format)), 1, 8)`;
   output file `/share/public/reports/MarineSensitivity.org_{key}.{ext}`, served as
   **`https://file.marinesensitivity.org/reports/MarineSensitivity.org_{key}.{ext}`**;
   cache hit just bumps mtime; otherwise `quarto::quarto_render("report.qmd", output_format = format,
   execute_params = list(title, areas_json, ver, format, api_base, mapsp_base, access))`;
   LRU-prunes the cache at `MSENS_REPORTS_MAX_MB` (default 500 MiB). Response: `{"url": …}`.
4. The report itself (api/`report.qmd` + `report_area_child.qmd`) resolves each area to cells
   (`msens::cells_in_polygon()` / `cells_in_pra()`), scores them (`scores_for_cells()` /
   `scores_for_pra()`), and renders a flower plot + species table + a static ggplot map
   (`msens::ggmap_areas()`), with per-area CSV links back to `GET /species.csv`.

---

## 10. Analytics / logging / feedback

Two legs, both driven from the browser, wired by `msens::ga_head()`/`ga_js()`
(`analytics.R:309-469`) injected into `<head>` (`app.R:1192-1194`):

- **GA4** `G-9HW6L751XG` (one id across every product), `gtag("config", GA_ID,
  {content_group, app_name, app_version})` where `content_group`/`app` = `"scores"` or
  `"scores-preview"`, and `app_version` = the deployed git short SHA.
- **Sheet beacon**: queue → `navigator.sendBeacon(MSENS_LOG_URL, Blob([...], {type:
  "text/plain;charset=UTF-8"}))` (CORS-simple so Apps Script's `/exec` accepts it), flushing at 10
  queued events, every 15,000 ms, and on `visibilitychange→hidden` / `pagehide`;
  `fetch(..., keepalive, mode:"no-cors")` fallback. Silent no-op when `MSENS_LOG_URL` is unset.
  Row schema (`ms_log_header()`): `timestamp, ip, session, event, params, n_rows, ms, status,
  error, app_version, app, client_id, session_id, page, referrer, user_agent`.
  `n_rows/ms/status/error` are hoisted out of `params` and kept numeric.
- `msens::ms_track_session(session)` pushes `{ip, session token}` once at session start; the page's
  `ip` (from `ms_client_ip(req)` — `CF-Connecting-IP` → first `X-Forwarded-For` → `REMOTE_ADDR`)
  wins, because a shiny-server websocket always reports `127.0.0.1` (`app.R:1705`).
- `trk(event, ...)` = `msens::ms_track()` — a websocket message only, never HTTP, so instrumenting
  a hot control adds no latency (`app.R:1707`).

**Events emitted** (all `ignoreInit = TRUE`): `select_tab{tab}`, `select_subregion{subregion}`,
`select_unit{unit}`, `select_layer{layer,subregion,unit}`, `select_palette{palette}`,
`open_about`, `start_tour`, `open_table_info`, `report_add_area{area_type}`,
`download_species_csv{n_rows,area,subregion,unit,layer}`,
`report_submit{status,rpt_ver,format,n_areas,area_kinds,areas,title}`,
`report_result{status,ms,rpt_ver,format,n_areas,report_url|error}` (`app.R:1710-1740`, `2840-2845`,
`3152`, `3192-3199`, `3239-3261`).
Event names are normalised to GA4 rules (lowercase, `[a-z0-9_]`, ≤40 chars); param values are
truncated to 100 chars for GA4 only (`analytics.R:94-131`, `401`).

**No feedback widget** (no form, no issue link) beyond the Docs/Home nav links.

---

## 11. Performance tricks, caches, and recorded gotchas

**Caches**
- `.bundles` env memoises `build_bundle(ver)` per process (`app.R:1050-1054`); measured cost
  3.2 s for v8, 1.0 s for a second version (apps/README.md).
- `msens` registry files: in-process memo + disk cache (`MSENS_ATLAS_CACHE`, TTL
  `MSENS_ATLAS_TTL` = 300 s) — without it every shiny-server session re-fetched
  latest.txt + versions.json + manifest.json (~0.58 s of TTFB) (`version.R:38-74`).
- `dir_cache = scores/cache/{ver}/` holds `{type}_label_pts.csv` (written on first use);
  `scores/cache/flower_default_subregions.csv` is **unversioned** and tracked;
  `.gitignore` excludes `cache/*/` because "a cache is recomputed ONLY when the file is absent, so a
  bad one never heals".
- `.zone_geom_cache` (per bundle) memoises zone geometry; the FlatGeobuf is ~7–53 MB so it is loaded
  **lazily**, only when the Report tab needs it (`app.R:497-551`).
- `initial_*` pre-warm avoids a `/msens/statistics` round-trip on first paint (`app.R:958-966`).
- Varnish/`titilecache` keys on the full tile URL; `mtime` in the SQL-tile URL is the cache-bust tag.
- `app_idle_timeout 3600` on the server keeps workers warm (packages alone cost 8.6 s to attach).
- Bundle caching to disk was evaluated and **deliberately rejected** (apps/README.md).

**Known gotchas recorded in comments**
1. A cached `terra::SpatRaster` is an external pointer; reusing it across sessions **segfaults** the
   process — "clicking the map disconnects the app" (`app.R:747-758`).
2. `value` is a DuckDB reserved word; a bare `value` with no such column resolves to a *function*,
   giving `cannot coerce type 'closure'` far from the real cause (`app.R:228-246`).
3. v1/v2 zone tables are unsuffixed → `glue("ply_subregions_2026_{ver}")` matched nothing, the
   subregion cache was written empty and never healed; symptom was an unfilled choropleth with an
   `InfM/-InfM` legend from `range(numeric(0))` (`app.R:202-220`, `2276-2287`).
4. MapLibre rejects an add whose `before_id` doesn't exist, **and the failure cascades** — v1 came up
   with nothing but labels (`app.R:649-664`).
5. Pointing a new PMTiles URL at an old `source_layer` silently yields an empty overlay
   (`app.R:311-322`).
6. `/species_v8` is retired; Caddy 301s it to `/species/?ver=v8`, whose own query string replaces the
   one in flight — so the model id was dropped and every row opened the leatherback turtle
   (`app.R:2715-2720`).
7. `st_coordinates()` on a named geometry column breaks when the source changes gpkg(`geom`) ↔
   fgb(`geometry`) — it took the whole app down, not just labels (`app.R:700-703`).
8. The study area must not filter the surface: the v4–v7 `USA` subregion COG is exactly the
   Program-Area union and hid 47 % of scored cells (apps#13) (`app.R:366-382`).
9. `meta$view` was once `bbox`, so `fly_to(center = NULL)` — latent until the bbox path was removed
   (`app.R:1936-1943`).
10. `taxon_id` arrives integer or character and the authority is `worms`/`WORMS` — an exact-type
    join took the Composition tab down (`app.R:2854-2860`).
11. `window.open` after an async response is popup-blocked; hence the pre-opened placeholder tab
    (`app.R:1311-1326`).
12. mapgl's drawn-features input changed shape between versions; requiring `is.character()` silently
    ignored every polygon (`app.R:2906-2913`, `viz.R:974-995`).
13. `future_promise` globals: `rpt_tok` is a plain scalar, not `!!!`-spliced, or report generation
    breaks for everyone (`app.R:3178-3182`).
14. Program-Area tooltip JS was hardcoded to `pra_lyr`/`programarea_*`; any other unit hovered to
    nothing (`app.R:1369-1392`).
15. `cell_model` tile ids depend on the grid width; a wrong width returns **zero rows silently**
    (`cell_model.R:101-156`).
16. mapgl ships every optional JS bundle whether used or not (3.7 MB/39 files) (`app.R:95-117`).

---

## 12. External dependencies → what the JS app must replace

| R package | capability used | JS replacement |
|---|---|---|
| `shiny` + `bslib` | reactive server, `page_sidebar`, cards, tabs, sidebar, modals, dark mode, notifications, mobile behaviour | any SPA framework + a CSS framework; Bootstrap 5 classes are already assumed by the markup |
| `mapgl` (MapLibre GL JS wrapper) | map widget, PMTiles source, raster source/layer, fill/line/symbol/circle layers, `match_expr`, legends, layers control, geocoder, draw control, fullscreen/nav/scale controls, globe projection | **maplibre-gl** directly + `pmtiles` protocol + `@mapbox/mapbox-gl-draw` + a geocoder (Nominatim) + hand-rolled legend & layers control |
| `duckdb` + `DBI` + `dbplyr`/`dplyr` | all tabular queries | **duckdb-wasm** (`httpfs`, `read_parquet` over HTTPS ranges) |
| `sf` | zone geometry read (gpkg/FGB), `st_point_on_surface`, `st_shift_longitude`, WKT in/out, `st_simplify`, `st_as_sfc` | flatgeobuf JS / GeoJSON, `turf.js` (pointOnFeature, simplify), `wkt` writer |
| `terra` | only the dead `get_rast()`/`r_cell_open()` path | — (drop) |
| `DT` | species datatable: paging, top filters, column reorder, keyboard nav, responsive, %/signif formatting | TanStack Table / AG Grid / DataTables.js |
| `ggplot2` + `ggiraph` | the polar "flower" plot with per-petal tooltips | D3 / Vega-Lite arc chart (formula in §7.2) |
| `plotly` + `plotme` | taxonomy treemap (`branchvalues:"total"`) | plotly.js directly (same trace type) |
| `RColorBrewer`, `viridisLite`, `scales` | 11-step Spectral/viridis/cividis/magma ramps, `hue_pal()(8)` for flower fills, `colorRampPalette(space="Lab")` | `d3-scale-chromatic` (`schemeSpectral[11]`, `interpolateViridis/Cividis/Magma`) — but the **tile** colors come from titiler's `colormap_name`, so legends must match those ramps |
| `httr2` + `promises`/`future` | async POST to the report API | `fetch()` |
| `glue`, `stringr`, `tidyr`, `tibble`, `purrr`, `readr`, `fs`, `digest` | string building, CSV IO, hashing | template literals, `papaparse`, Web Crypto |
| `conductor` | guided tour | Shepherd.js / driver.js |
| `bsicons` | icons | any icon set |
| `msens` | **the domain logic** — version registry, manifest, grid math, study areas, tile URL builders, species SQL, zone styles, analytics snippet, signed tokens | port function-by-function (§2, §3, §6, §7) |

---

## 13. Hard parts to port (candid)

1. **Species list for a clicked cell.** Needs `cell_model` (`serve/cell_model/tile={t}/data_0.parquet`,
   **422 partitions, 1.30 GB total, ~3 MB each**) joined to `model` (1.09 MB) and `taxon` (1.04 MB).
   Feasible in duckdb-wasm *if* the exact partition key is computed client-side — the tile formula is
   `tile = ((cell_id-1)//ncol)//50 * (ncol//50) + ((cell_id-1)%ncol)//50` with `ncol` = 7200
   (global05) or 3103 (usa05) — because **anonymous S3 LIST is denied** (verified: `AccessDenied`),
   so globbing is impossible and every object key must be constructed. Getting `ncol` wrong returns
   zero rows silently.
2. **Species list for a zone.** Easy: `zone_taxon.parquet` is 4.98 MB / 115,700 rows covering all 36
   zones, and the whole-study-area default is just `zone_fld='subregion_key' AND zone_value='USA'`.
   But the schema differs by generation (v1/v2 `rl_code`/`rl_score`; v3-v7 raw 1-100 `er_score`) and
   `.zone_taxon_normalize()` must be ported exactly, including the `0 ≤ er_score ≤ 1` assertion.
3. **Flower plot for a clicked cell** needs `cell_metric` filtered to one `cell_id`
   (**68.1 MB / 9.55 M rows on v8; 114.4 MB on v7**) — and I measured it: the file has only **3 row
   groups, each spanning the entire cell_id range**, so there is **nothing to prune** and one click
   would pull the whole file. The port needs either (a) a one-off 68 MB download cached in the
   browser (OPFS/IndexedDB), or (b) a new per-cell artifact — the obvious one is to re-sort/partition
   `cell_metric` on the same 2.5° tile key `cell_model` already uses (17 metric values/cell × ~600k
   scored cells is tiny once tiled). Alternatively read the 7 rescaled-component COGs with
   `/cog/point` — 7 extra HTTP calls per click, no bulk download, and it reuses the tile service.
4. **Polygon → cells (the Report path).** `msens::cells_in_polygon()` does a `cell` bbox select
   (`cell.parquet` is **392.8 MB / 17.07 M rows / 30 row groups** on v8) then exact `st_intersection`
   coverage fractions per 0.05° box. Row-group stats prune on **latitude only** (`lon` is
   -180..180 in every group), so even a small polygon costs tens of MB. Client-side the right answer
   is almost certainly to skip the table entirely and rasterise the polygon against the grid
   definition arithmetically (`cell_from_lonlat` + scanline fill + turf coverage fractions) —
   `area_km2` per cell can then be computed from the grid rather than read.
5. **Report generation.** Server-side Quarto render (HTML/DOCX/PDF) behind an authenticated API,
   with its own version gating and a shared-secret header. Nothing about it is portable to a static
   site — it must stay a remote API call (CORS is already handled by the plumber `cors` filter), or
   the feature degrades to a client-rendered HTML/print view.
6. **Access control for restricted releases.** The whole scheme — `MS_PREVIEW` per process,
   HMAC-signed version tokens, Cloudflare Access per version path, `X-MS-Version` — assumes a
   server. A GitHub Pages SPA can only ever render **public** versions (today: v7 and the retired
   v1–v6); v8/v9 are `restricted` and `latest.txt` is `v7`. Decide explicitly whether the port is
   public-only, and keep the fail-closed derivation (`prerelease → restricted`).
7. **`MS_TOKEN_SECRET` / `MSENS_PREVIEW_TOKEN` / the Mapbox token file** are server secrets. The
   Mapbox one turns out to be *vestigial* for rendering (basemap is CARTO, geocoder is Nominatim) but
   is still a hard startup requirement — drop it, don't ship it.
8. **Legend ↔ tile colour agreement.** Tiles are coloured by titiler (`colormap_name=spectral_r|…`)
   while the legend swatches are generated in R. In JS they must be generated from the *same* ramp
   definitions or the legend will lie; and the FULL-COG rescale (published per metric per subregion
   in the manifest) must be used verbatim, never recomputed.
9. **titiler dependency.** Every raster pixel, the clicked value and the clicked cell id come from
   `titiler-v8.marinesensitivity.org`. A "no server" port still depends on that service unless COGs
   are read client-side (`geotiff.js` + a custom tile renderer — possible but a real project; note
   `grid/global05/cellid.tif` is 73 MB and has **no overviews** by design).
10. **Per-version drift.** Four resolvers must be ported or the app breaks on older releases:
    `sdm_val_col` (val/value/both), `sdm_cols` (is_ok vs is_valid_usa + is_marine),
    `zone_tbl_for` (table-name suffixes), `manifest$id_field` (mdl_seq vs mdl_key) — plus two grids
    where the *same* `cell_id` means different places, and v1/v2 releases with no extinction-risk
    columns at all.
11. **Aggregation fidelity.** `avg_suit`, `area_km2`, `pct_cat`, the zone colour binning, the flower
    centre and the `_ecoregion_rescaled` component extraction (`str_replace("extrisk_","")` →
    `str_replace("_ecoregion_rescaled","")` → `str_replace("_"," ")`, dropping `all`) must match
    byte-for-byte or published numbers will disagree with the app.
12. **Two known bugs to fix rather than faithfully reproduce**: the hardcoded `pra_ln/pra_lbl/er_ln`
    layers control on the cell path (`app.R:2136-2143`) and the unversioned
    `flower_default_subregions.csv` shared across releases (`app.R:1001`).
