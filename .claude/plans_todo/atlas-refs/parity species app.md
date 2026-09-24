# Feature-parity inventory — MarineSensitivity **species** Shiny app

Source of truth read in full:
- `/Users/bbest/Github/MarineSensitivity/apps/species/app.R` (2,237 lines; ~109 KB)
- `/Users/bbest/Github/MarineSensitivity/apps/README.md`
- `/Users/bbest/Github/MarineSensitivity/apps/scores/CLAUDE.md` (generic R/package conventions only — *not*
  app-specific; the app-specific shared context actually lives in `apps/README.md` and
  `workflows/CLAUDE.md`)
- `msens` package: `/Users/bbest/Github/MarineSensitivity/msens/R/{version,ver_token,version_ui,grid,viz,zone_style,mdl_key,analytics,atlas,calc,cog_store}.R`
- Producers of the data the app reads: `workflows/{publish_native,build_registry,build_version_manifest,schema,release_marine-atlas}.qmd`,
  `server/titiler/factory.py`, `server/docker-compose.yml`

Target of the port: static JS SPA (MapLibre GL + DuckDB-WASM over public Parquet/COG/PMTiles, GitHub Pages, no R).

---

## 1. Purpose + user-facing summary

- Browse the **per-species distribution surface** for any of ~17.8k–22.2k marine taxa in one published
  release of the BOEM Marine Sensitivity Toolkit (v1 … v9), picked by scientific or common name
  (`app.R:1026-1030`, `app.R:426-480`).
- See the **merged model** (the max-merge across all inputs, range-masked) as a colored raster
  (Spectral-reversed, 1–100) on a dark MapLibre basemap (`app.R:1925-1926`, `app.R:1996-2021`).
- Switch to **any single input model** that fed that taxon's merge (AquaMaps, AquaX, IUCN/BirdLife/FWS
  expert ranges, NMFS critical habitat, SWOT turtle ranges, NMFS DPS extinction-risk surface) via
  clickable "pills" in a colored layer bar (`app.R:1497-1602`). Inputs that fed the merge but have **no
  published surface** render as struck-through, disabled pills (`app.R:1522-1527`, CSS `app.R:766-773`).
- For inputs published in two representations, toggle **Original ↔ Interpolated** — relabelled
  **Delivered ↔ As ingested** for datasets delivered already on the analysis grid (`dataset.on_grid`,
  AquaX from v9) (`app.R:1556-1578`).
- **Click any map cell** to get a colored popup with cell id, lon/lat and the value of the layer on
  screen — sampled from local DuckDB SQL (merged) or titiler `/cog/point` (COG) (`app.R:2124-2210`).
- Read a **species info card** in the sidebar: common name, category, ESA listing (code + source), IUCN
  Red List code, WoRMS link, MMPA/MBTA protection flags, and the input/mask composition tree
  (`app.R:1605-1706`).
- Overlay **BOEM Program Area** or **Ecoregion** outlines (+ Program-Area labels & hover tooltips), toggle
  a **globe vs mercator** projection, and optionally overlay **OBIS occurrence hexagons** for the species
  (`app.R:1006-1021`, `app.R:1043-1046`, `app.R:2079-2121`).
- **Deep-link** to any model: `?mdl_key=` (v8+) or `?mdl_seq=` (v1–v7); the app resolves it to the taxon
  *and* the input layer, and rewrites the URL as you browse so the address bar is always shareable
  (`app.R:1351-1449`, `app.R:1946-1947`).
- **Switch published release** from the header (`(v8)` link → modal listing every version, pre-release and
  retired ones labelled; restricted ones link to the signed-in preview host) (`app.R:1142-1156`).
- Guided **welcome modal + 5-step tour**, copy-to-clipboard buttons for the scientific/common name, and
  cross-product nav to Scores / Docs / Home (`app.R:1229-1328`, `app.R:1470-1494`, `app.R:676-696`).

---

## 2. Startup and data loading

### 2.1 Process-level globals (evaluated once per R worker)

| thing | value / source | line |
|---|---|---|
| `VER_FALLBACK` | `"v8"` — used only if the version registry is unreachable | `app.R:42` |
| `APP_VERSION` | `git -c safe.directory=* -C <here()> rev-parse --short HEAD`, else `VER_FALLBACK`; stamped on every logged event | `app.R:52-60` |
| `is_server` | `Sys.info()[["sysname"]] == "Linux"` | `app.R:62` |
| `dir_private` | server `/share/private`, laptop `~/My Drive/private` | `app.R:63-67` |
| `dir_data` | server `/share/data`, laptop `~/My Drive/projects/msens/data` | `app.R:68-72` |
| `mapbox_tkn_txt` | `{dir_private}/mapbox_token_bdbest.txt` → `Sys.setenv(MAPBOX_PUBLIC_TOKEN=…)` (read **before** `library(mapgl)`) | `app.R:73-78` |
| `verbose` | `TRUE` — server-side `message()` logging only | `app.R:37` |
| `.bundles` | memo env: one bundle per version, keyed by `"v8"` etc. | `app.R:167`, `app.R:585-589` |

**Dead code to skip in the port:** `add_fixed_range_raster()` (`app.R:93-119`) is defined and never called
(only a commented usage example at `app.R:120-122`). It is the only use of `terra::` in the file.

### 2.2 OBIS (h3t) constants — optional overlay

```
H3T_HOST            = "https://h3t.marinesensitivity.org"                      app.R:131
H3T_TILES_BASE      = "h3tiles://h3t.marinesensitivity.org/h3t/{z}/{x}/{y}.h3t" app.R:132
H3T_RELEASE         = format(Sys.Date(), "v%Y%m%d")                             app.R:133
H3T_VIRIDIS5        = c("#440154","#3b528b","#21918c","#5ec962","#fde725")      app.R:134
H3T_STATS_TIMEOUT_S = 12                                                        app.R:135
```
- `h3t_src` = first existing of `/share/github/marinebon/obisindicators/R/h3t.R`,
  `../../marinebon/obisindicators/R/h3t.R`, `~/Github/marinebon/obisindicators/R/h3t.R`; `has_obis` gates the
  whole feature (`app.R:137-145`).
- `h3t_stats(sql, res_h3 = 4)` (`app.R:148-156`): GET
  `https://h3t.marinesensitivity.org/h3t/stats?q={base64(sql) with newlines stripped, URLencoded reserved=TRUE}&res_h3=4`,
  12 s timeout, returns JSON with (used keys) `n`, `p02`, `p98`, `min`, `max`, `error`.

### 2.3 `build_bundle(ver)` — everything version-dependent (`app.R:169-581`)

Paths & endpoints:

| name | value | line |
|---|---|---|
| `dir_v` | `{dir_data}/derived/{ver}` | 171 |
| `dir_big` | server `/share/data/big/{ver}`, laptop `~/_big/msens/derived/{ver}` | 172-175 |
| `is_prod` | `Sys.getenv("MSENS_ENV") == "prod"` | 176 |
| `pmtiles_base_url` | prod `"/pmtiles"` (same-origin via Caddy), else `https://file.marinesensitivity.org/pmtiles` | 177-180 |
| `tbl_er` | `"ply_ecoregions_2025"` (fallback PMTiles basename + source-layer) | 181 |
| `tbl_pra` | `"ply_programareas_2026_{ver}"` (gpkg basename) | 182 |
| `tbl_pra_pm` | `"ply_programareas_2026"` (fallback PMTiles basename + source-layer) | 183 |
| `grid_id` | `msens::grid_for_ver(ver)` → `usa05` (v1–v7) / `global05` (v8, v9) | 217 |
| `grid` | `msens::grid_spec_for(grid_id)` | 218 |
| `cell_tif` | `{dir_data}/derived/{grid_registry()$cellid_tif}` = `r_cellid.tif` or `r_cellid_global.tif` (**assigned but never used** in this app) | 219 |
| `pra_gpkg` | `{dir_v}/ply_programareas_2026_{ver}.gpkg` | 220 |
| `sdm_db` | `{dir_big}/serve.duckdb` if it exists, else `{dir_big}/sdm.duckdb` | 221 |
| `tile_base_url` | `"https://titiler-v8.marinesensitivity.org"` — **the stock `/cog` tiler for every release**; only the `/msens/tiles` `model_cell` path is v8-bound | 224 |
| `db_mtime` | `format(file.info(sdm_db)$mtime, "%Y%m%dT%H%M%SZ", tz="UTC")` — cache-bust tag in `/msens/tiles` URLs | 225 |
| `con_sdm` | `dbConnect(duckdb(), dbdir = sdm_db, read_only = TRUE)` | 229 |
| `has_model_cell` | `"model_cell" %in% dbListTables(con_sdm)` (presence-derived) | 234 |

**`serve.duckdb` is a view-only DB** (KB) whose views point at LOCAL Parquet under `/share/data/big/{ver}/`
(`tables/*.parquet`, `model_cell/`), *not* S3 — see `workflows/CLAUDE.md` ("Serving reads LOCAL Parquet").

#### Remote registry files read at bundle build (all via `msens::.atlas_fetch`, `version.R:38-74`)

Base: `msens::atlas_base_url()` = **`https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas`**
(path-style, because the bucket name has dots; `version.R:97-99`).

| URL | parsed by | notes |
|---|---|---|
| `{base}/latest.txt` | `atlas_latest()` `version.R:111-117` | one line, must match `^v[0-9]+[a-z]?$`; currently **`v7`** |
| `{base}/versions.json` | `atlas_versions()` `version.R:134-158` | array of `{ver,status,access,released,title}`; `status ∈ released|prerelease|retired`, `access ∈ public|restricted`; missing `access` derived fail-closed (`prerelease → restricted`) |
| `{base}/{ver}/manifest.json` | `atlas_manifest()` `version.R:300-305` + `validate_manifest()` `317-333` | required keys `ver,status,grid_id,id_field,capabilities,tables`; extras used here: `zones` |

Caching: in-process env memo **plus** a disk cache at `MSENS_ATLAS_CACHE` (default
`{tempdir()}/msens-atlas/{hash16}.txt`) with TTL `MSENS_ATLAS_TTL` seconds (default **300**);
write-then-rename (`version.R:32-74`).

#### Manifest fields the species app uses

- `zone_manifest$zones` — data frame with `zone_set_key`, `tbl`, `fld`, `n`, `pmtiles`
  (`app.R:196-200`). `ztile(zone_type, fallback_tbl)` (`app.R:202-210`) returns
  `list(url = zones$pmtiles[fld == "{zone_type}_key"], source_layer = zone_type)`, or
  `NULL` when the manifest exists and names no such type (draw nothing — v1 has no Program Areas), or the
  unversioned fallback `{pmtiles_base_url}/{fallback_tbl}.pmtiles` with `source_layer = fallback_tbl`
  when the manifest is **missing** entirely.
  Manifest zone PMTiles URL shape (`build_version_manifest.qmd:190-192`):
  **`{atlas_base_url()}/zones/{zone_set_key}/zones.pmtiles`**, `zone_set_key ∈
  {ecoregion_2025-06, planarea_2025-06, programarea_2026-01, subregion_2025-06, subregion_2025-08}`
  (`workflows/data/zone_sets.csv`).
- `zone_manifest$id_field` — `"mdl_seq"` or `"mdl_key"`; **the whole v1–v7 vs v8+ adapter switch**
  (`app.R:363-364`).

#### Cached CSVs written into the app directory

1. `species/cache/pra_label_pts_{ver}.csv` (`app.R:247-279`) — Program-Area label points.
   Built from `read_sf({dir_v}/ply_programareas_2026_{ver}.gpkg)` when present, else from
   **`/vsicurl/{atlas_base_url()}/zones/{zone_set_key}/zones.fgb`**, then `st_point_on_surface()`.
   Columns: `programarea_key`, `programarea_name`, `lng`, `lat` → `st_as_sf(crs = 4326)`.
   Empty (0-row `sf` with geometry column) when neither source exists — correct for v1.
   *Files present on disk:* `pra_label_pts.csv`, `pra_label_pts_v7.csv`, `pra_label_pts_v8.csv`.
2. `species/cache/ecoregions_bbox_{ver}.csv` (`app.R:288-305`) — default map extent.
   SQL (dbplyr): `zone` filtered to `fld == 'ecoregion_key'`, inner-joined to `zone_cell` on `zone_seq`,
   pull `cell_id`; then `msens::cell_lonlat(cells, grid, wrap = FALSE)` and grow by half a cell:
   `c(min(lon)-resx/2, min(lat)-resy/2, max(lon)+resx/2, max(lat)+resy/2)`. Falls back to
   `c(-180,-90,180,90)`. Columns `xmin,ymin,xmax,ymax`.
   *Files present:* `ecoregions_bbox.csv`, `_v7.csv`, `_v8.csv`.

#### DuckDB tables read from `sdm_db` / `serve.duckdb`, and the columns actually used

| table | columns used | where |
|---|---|---|
| `dataset` | `ds_key`, `name_short`, `name_display`, `value_info`, `is_mask`, `on_grid`, `sort_order` — every one except `ds_key` is **optional** and back-filled (`name_display` ← first comma-clause of `name_short` truncated to 30 chars at a word boundary + `…`, else `ds_key`; `is_mask` ← FALSE; `sort_order` ← row order; `on_grid` ← FALSE) | `app.R:315-336` |
| `taxon` (v8+) | `taxon_id`, `taxon_authority`, `scientific_name`, `common_name`, `sp_cat`, `ms_merge_key`, `iucn_code`, `extrisk_code`, `er_score`, `rarity`, `is_mmpa`, `is_mbta`, `is_valid_usa`, `is_valid_global`, `is_marine` | `app.R:426-439` |
| `taxon` (v1–v7) | `taxon_id`, `taxon_authority`, `scientific_name`, `common_name`, `sp_cat`, `mdl_seq`, `is_ok`, `redlist_code`, `extrisk_code`, `esa_source`, `er_score`, `is_mmpa`, `is_mbta` (all guarded by `colnames()` presence) | `app.R:367-385` |
| `taxon_model` | v8: `ms_merge_key`, `mdl_key`; v1–v7: `taxon_id`, `ds_key`, `mdl_seq` | `app.R:388-396`, `440-445` |
| `native_asset` (v8+) | `ms_merge_key`, `mdl_key`, `ds_key`, `asset_type`, `representation`, `asset_url`, `rescale_min`, `rescale_max`, `colormap`, `source_layer`, `xmin`, `ymin`, `xmax`, `ymax` | `app.R:446-452` |
| `model_asset` (v1–v7) | `mdl_seq`, `ds_key`, `cog_url` | `app.R:398-413` |
| `zone`, `zone_cell` | `fld`, `zone_seq`, `cell_id` | `app.R:291-293` |
| `model_cell` + `cell` + `model` (v8+) | `mdl_id`, `cell_id`, `val`; `cell.lon`, `cell.lat`; `model.mdl_key`, `model.mdl_id` | `app.R:1753-1758`, `2144-2147` |
| `cell_model` (v1–v7) | `mdl_seq`, `cell_id` | `app.R:1741-1746` |

`taxon` schema reference (`workflows/schema.qmd:56-65`): `taxon_authority, taxon_id, ms_merge_key,
scientific_name, iucn_code, n_models, n_datasets, worms_is_marine, worms_is_extinct, extrisk_code,
er_score, is_mmpa, is_mbta, is_bcc, common_name, n_cells, n_ocean, n_usa, n_pra, range_km2,
range_usa_km2, n_global, is_valid_global, is_valid_usa, is_valid_pra, pct_marine, us_endemism, rarity,
sp_cat, is_marine, pct_marine_bl, in_v7, is_er_spatial`.

### 2.4 Tile / raster endpoints — **exact URL templates**

**A. Merged & COG inputs → stock titiler `/cog`** (`msens::cog_tile_url()`, `viz.R:400-429`):

```
https://titiler-v8.marinesensitivity.org/cog/tiles/WebMercatorQuad/{z}/{x}/{y}.png
  ?url=<URLencode(cog_url, reserved=TRUE)>
  &colormap_name=spectral_r
  &rescale=1,100
```
- `tms` default `WebMercatorQuad`; `base` = `tile_base_url`.
- For an input asset the params come from the `native_asset` row:
  `colormap = coalesce(asset$colormap, "spectral_r")`,
  `rescale = c(coalesce(asset$rescale_min, 1L), coalesce(asset$rescale_max, 100L))` (`app.R:2008-2011`).
  AquaX **native/delivered** rows carry `rescale_min = 0, rescale_max = 1000`
  (`publish_native.qmd:740-743`); everything else is 1..100.
- A `color=` variant exists in msens (`{"1":[r,g,b,255]}` explicit colormap) but the species app never uses it.

**B. Merged fallback for am-only taxa → the custom `/msens` factory** (`msens::cell_tile_url()`, `viz.R:347-379`):

```
https://titiler-v8.marinesensitivity.org/msens/tiles/{z}/{x}/{y}.png
  ?mdl_key=<URLencode(mdl_key, reserved=TRUE)>      e.g. ms_merge%7CWORMS%3A137209
  &colormap=spectral_r
  &rescale=1,100
  &mtime=<sdm_db mtime, %Y%m%dT%H%M%SZ UTC>
```
Server side (`server/titiler/factory.py:142-156, 474-540`): resolves `mdl_key → mdl_id` from the `model`
registry, reads exactly one partition
`{MSENS_SERVE_MODEL_CELL}/mdl_id={id}/data_0.parquet` (default
`https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v8/serve/model_cell`),
builds a dense `cell_id → value` array, samples the cell-id lookup COG
(`MSENS_CELLID_COG = /share/data/derived/r_cellid_global.tif` for `titiler-v8`), applies the colormap, and
returns PNG with `Cache-Control: public, max-age=604800` (7 days). `MSENS_MAX_ROWS = 1,000,000`.
**Note:** the `/msens` factory is off by default on the v8 container (`MSENS_FACTORY=1` to mount it) —
`server/docker-compose.yml:325-335`.

**C. Point sampling on click** (`app.R:2151-2153`):
```
GET https://titiler-v8.marinesensitivity.org/cog/point/{lng},{lat}?url=<cog_url>
→ JSON { "values": [v, ...] }   → values[[1]]
```

**D. Vector-range inputs → PMTiles** (`app.R:2022-2033`): `asset$asset_url` from `native_asset`, shape
(`publish_native.qmd:328-332`):
```
https://file.marinesensitivity.org/pmtiles/{ver}/{ds}/{safe_key(mdl_key)}.pmtiles?v={file mtime epoch}
```
with `source_layer = ds` (= `ds_key`), and a client-side filter `["==", ["get","mdl_key"], layer_mdl_key]`.

**E. Published COG object keys** (all under `{atlas_base_url()}/{ver}/`, `publish_native.qmd`; `safe_key(k) =
gsub("[^A-Za-z0-9._-]","_",k)` so `am|Fis-29291` → `am_Fis-29291`):

| class | key | representation | rescale |
|---|---|---|---|
| AquaMaps 0.05° resampled | `native/am/{safe_key}.tif` | `model` | 1–100 |
| AquaMaps 0.5° original | `native/am_native/{safe_key}.tif` | `native` | 1–100 |
| AquaX as ingested | from `dist/model_ax.csv` `cog_url` (→ `native/ax/…`) | `model` | 1–100 |
| AquaX as delivered | from `dist/model_ax.csv` `cog_native_url` (→ `native/ax_native/…`) | `native` | **0–1000** |
| vector-range 0.05° grid | `native/vec_grid/{safe_key}.tif` | `model` | 1–100 |
| NMFS DPS per-cell ER | `native/dps_nmfs/{safe_key}.tif` | `model` | 1–100 |
| merged whole-range | `native/merged/{safe_key(ms_merge_key)}.tif` | `model` | 1–100 |
| v1–v7 per-model (content-addressed) | `cog/{grid_id}/{content_hash}.tif` via `msens::content_url()` (`cog_store.R:117-131`) | `native` (synthesised) | 1–100 |

`native/*` keys are **stable**, so republishing changes bytes under a URL `/vsicurl` may have cached —
hence `DEPLOY_TITILER` after any repaint (`workflows/CLAUDE.md`).

### 2.5 Basemap / map controls

- Basemap style: `mapgl::carto_style("dark-matter")` → **`https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json`** (`app.R:1781`).
- Projection: `"globe"` or `"mercator"` from `input$tgl_sphere` (`app.R:1782`).
- `MAPBOX_PUBLIC_TOKEN` is set from disk (`app.R:73-74`) for mapgl; `add_geocoder_control()` uses it
  (or a MapTiler key) for place search.

### 2.6 Environment variables consulted

`MSENS_ENV` (`prod` → same-origin `/pmtiles`), `MS_PREVIEW` (preview instance policy),
`MS_PREVIEW_URL` (default `https://preview.marinesensitivity.org`), `MS_TOKEN_SECRET` (HMAC key),
`MSENS_ATLAS_CACHE`, `MSENS_ATLAS_TTL`, `MSENS_LOG_URL` (Sheet beacon; unset ⇒ silent no-op).

---

## 3. Version handling

### 3.1 Resolution chain

| function | line | behaviour |
|---|---|---|
| `ver_of(qs)` | `app.R:602-609` | `parseQueryString(qs)$ver` → `msens::atlas_resolve_ver(q$ver, allow_access = msens::atlas_allow_access())`; on any error re-resolve with `NULL` (= `latest.txt`); on error again `VER_FALLBACK` |
| `ver_of_req(req)` | `app.R:635-638` | prefers `req[["HTTP_X_MS_VERSION"]]` (a header **Caddy sets from the URL path**, unforgeable) → `ver_of("ver={v}")`; else `ver_of(req$QUERY_STRING)` |
| `ver_of_session(input, session)` | `app.R:622-627` | `msens::ver_token_verify(isolate(input$ms_ver_token))`, then re-resolve through the instance policy. **Never** trusts `session$clientData$url_search`/`url_pathname` (client-supplied) |
| `ui(req)` / `server(...)` | `app.R:2224-2234` | both re-enclose `ui_impl` / `server_impl` in `bundle(ver)` — `environment(f) <- b` |

`msens::atlas_resolve_ver()` (`version.R:244-268`): `NULL`/`""`/`"latest"` → `atlas_latest()`; must match
`^v[0-9]+[a-z]?$`; must exist in `versions.json`; `status` must be in `allow` (default all three);
`access` must be in `allow_access` else a **classed condition `msens_restricted`** carrying `ver` + `access`.

`msens::atlas_allow_access()` (`version.R:193-194`): `MS_PREVIEW ∈ {1,true,yes}` ⇒
`c("public","restricted")`, else `"public"`. Policy is a property of the **process**, never a request
header — Shiny Server OSS opens its own websocket so no proxy header survives.

### 3.2 Signed session token

`msens::ver_token_sign(ver)` (`ver_token.R:73-79`) → `"{ver}.{expiry}.{hmac_sha256(secret, \"{ver}.{expiry}\")}"`,
TTL 86,400 s. Embedded as a hidden bound text input `#ms_ver_token` (`app.R:978-979`).
`ver_token_verify()` (`ver_token.R:94-104`) splits on `.`, checks the version label, constant-time-compares the
HMAC, checks expiry; returns the *candidate* version, which the caller must still re-resolve through the
instance policy.
Secret: `MS_TOKEN_SECRET`, else a per-process random 32 bytes from `/dev/urandom` (`ver_token.R:37-50`).

### 3.3 The v1–v7 ↔ v8+ adapter (`app.R:347-455`) — the single most important porting artifact

Switch: `is_mdl_seq <- identical(manifest$id_field, "mdl_seq")`.

| concept | v1–v7 (`usa05`) | v8+ (`global05`) |
|---|---|---|
| public model id | integer `mdl_seq` (renumbers per rebuild) | string `mdl_key` = `{ds_key}\|{sp_id}[\|{interval}]`, merged = `ms_merge\|{AUTHORITY}:{taxon_id}` (`mdl_key.R:53-85`) |
| taxon → merged id | `taxon.mdl_seq`, cast to character | `taxon.ms_merge_key` |
| validity | single `is_ok` (bakes in the Program-Area gate) | `is_valid_global` **or** `is_valid_usa` (neither is a superset: 4,473 global-only, 46 US-only on v8) |
| asset registry | `model_asset(mdl_seq, ds_key, cog_url)` → synthesised as `asset_type="cog"`, `representation="native"`, `rescale 1..100`, `colormap "spectral_r"`, `source_layer/xmin/ymin/xmax/ymax = NA` | `native_asset` (14 columns) |
| input edges | `taxon_model(taxon_id, ds_key, mdl_seq)` joined through `taxon`; **includes an `ms_merge` self-edge that must be dropped** | `taxon_model(ms_merge_key, mdl_key)`, `ds_key = str_extract(mdl_key, "^[^\|]+")`; no self-edge |
| extra taxon fields | `redlist_code`, `extrisk_code`, `esa_source`; `rarity = NA`; `is_valid_usa = TRUE` by construction | `iucn_code` → `redlist_code`, `extrisk_code` → `esa_code`, `esa_source = NA`, real `rarity` |
| value column | `value` | `val` (`value` is a DuckDB reserved word). `msens::sdm_val_col(con, tbl)` (`calc.R:352-360`) returns `"val"` if present else `"value"`; **the served v8 DB has BOTH** |
| cell grid | 3103 × 2006, `xmin 141.10`, `ymax 82.60`, res 0.05, **lon 0–360** (runs 141.10°E eastward across the antimeridian), ncell 6,224,618, lookup `r_cellid.tif` | 7200 × 3600, `xmin -180`, `ymax 90`, res 0.05, lon −180..180, ncell 25,920,000, lookup `r_cellid_global.tif` (`grid.R:25-42`) |
| cell_id ↔ lon/lat | `cell_lonlat` (`grid.R:116-127`): `row = (id-1) %/% nc + 1`, `col = (id-1) %% nc + 1`, `lon = xmin + (col-0.5)*resx`, `lat = ymax - (row-0.5)*resy`; `wrap=TRUE` maps `lon>=180 → lon-360` | same arithmetic, `lon360 = FALSE` |
| merged-model bbox SQL | row/col arithmetic on `cell_model WHERE mdl_seq = {int}` (no lon/lat columns exist) | `min/max(c.lon)` + a 0–360 recomputation, joined `model_cell ⋈ cell` on `cell_id` |

Version→grid map (`grid.R:40-42`): `v1..v7 → usa05`; `v8, v9 → global05`. **`grid_for_ver()` errors on an
unknown version** rather than assuming.

### 3.4 Restricted-release access in the UI

- `ver_access(ver)` = `msens::atlas_ver_access(ver)` with `"public"` fallback (`app.R:671`).
- `preview_badge(req, ver)` (`app.R:643-652`) renders a `badge bg-warning` **PREVIEW · {email}** only when
  `atlas_is_preview()` **and** the version's `access == "restricted"`; email from `req[["HTTP_X_MS_USER"]]`
  (Caddy sets it from the verified Cloudflare Access JWT).
- `product_urls(ver, access)` (`ver_token.R:159-182`):
  - public → `https://app.marinesensitivity.org/{ver}/scores/`, `…/{ver}/species/`,
    `https://marinesensitivity.org/docs/{ver}/`, `https://marinesensitivity.org`
  - restricted → `https://preview.marinesensitivity.org/{ver}/{scores|species}/`,
    `https://preview.marinesensitivity.org/docs/{ver}/`, home unchanged.
- `preview_app_url(app, ver)` (`ver_token.R:119-123`) = `{base}/{ver}/{app}/`.
- On the preview instance the nav adds a **Sign out** link to `/cdn-cgi/access/logout` (`app.R:692-695`).

Current registry (`workflows/data/versions.csv`): v1–v6 retired/public, **v7 released/public (= `latest.txt`)**,
**v8 and v9 prerelease/restricted**.

---

## 4. URL / query parameters and deep links

| param | meaning | default | handled at |
|---|---|---|---|
| `ver` | requested release. Resolved through the instance policy. Unknown → modal "Unknown data version" linking `versions.json`; restricted+public-instance → modal "Version {v} is under review" linking `preview_app_url("species", v)` | promoted release from `latest.txt` | `app.R:1106-1130`, `app.R:602-609` |
| `mdl_key` | v8+ model id — **either** a merged `ms_merge\|AUTH:id` **or** any raw input key (`am\|Fis-29291`, `rng_iucn\|12345`, …). Resolved via `mdl_key_lookup` to `(merged_mdl_key, ds_layer)`; a non-US target auto-unchecks `us_only` | none → default species | `app.R:1360-1429`, lookup built `app.R:574-577` |
| `mdl_seq` | v1–v7 alias of `mdl_key` (published in the BOEM final report + deliverables) | — | `app.R:1360` |
| `er_clr` | *undocumented debug overlay*: `"CAC:red;EGOA:green;NECS:blue"` → an ecoregion fill layer `er_ply` colored by `match_expr(column="ecoregion_key", values, stops, default="#cccccc")`, `fill_opacity 0.5`, `before_id "pra_ln"` | none | `app.R:1441-1445`, `1819-1868`, `2068-2072` |
| `splash` | `splash=false` suppresses the welcome modal for this load (otherwise `localStorage["msens_mapsp_show_splash"]`, default `"true"`) | — | `app.R:871-879` |

**URL rewriting.** Two observers own the address bar:
- `app.R:1133-1140`: when **neither** `mdl_key` nor `mdl_seq` is present, replace the URL with
  `/{ver}/species/`.
- `app.R:1946-1947`: on every species/layer render, replace with
  **`/{ver}/species/?mdl_key={layer_mdl_key}`** (`mode = "replace"`), i.e. the id of the *layer on
  screen*, not just the taxon. The version is the path; `?ver=` is never re-added.
- "Copy link" is **not** implemented; copy buttons copy the scientific/common **name** only
  (`app.R:1479-1493`, JS `app.R:884-925`).
- Note `mdl_key` is **not** URL-encoded when written by `updateQueryString` (`|` and `:` go in raw) but
  *is* encoded in the links the scores app emits (`scores/app.R:2725-2726`).

---

## 5. UI layout and every input control

Page: `bslib::page_sidebar(fillable_mobile = TRUE, …)` (`app.R:705-1090`).

### 5.1 Header (`title` argument, `app.R:980-994`)

- `.ms-header` flex row: `span.ms-title` = `"BOEM Marine Sensitivity "` + `actionLink("show_versions", "({ver})", title="data version - click to switch")` + `span.ms-title-sub " species distribution"` + `preview_badge(...)`.
- `product_nav(ver, "species")` (`app.R:676-696`): Scores · Species (current → `span.nav-here`) · Docs (target `_blank`) · Home [· Sign out on preview].
- `.header-right`: `actionLink("btn_about", "About")` + `bslib::input_dark_mode(id = "tgl_dark", mode = "dark")`.
- `window_title = "BOEM Marine Sensitivity ({ver}) species distribution"` (`app.R:1004`), replaced at runtime
  by the `updateTitle` custom message.
- `<head>`: `favicon.ico`; `<meta name="ms-ver" content="{ver}">`; `<meta name="ms-preview" content="0|1">`
  (curl-checkable sentinels); `msens::ga_head(...)` (`app.R:709-722`).

### 5.2 Sidebar — `sidebar(open = FALSE, …)` (`app.R:1006-1021`)

| inputId | control | label | default | conditional |
|---|---|---|---|---|
| `tgl_sphere` | `input_switch` | "Sphere" | `TRUE` | always |
| `tgl_obis` | `input_switch` | "OBIS occurrences" | `FALSE` | only if `has_obis` |
| — | `uiOutput("species_info")` | — | — | requires `sel_sp` + `ds_layer` |

Collapsed-sidebar hint pill: CSS `::after` on `.collapse-toggle` reading **"Species info"** — beside the
chevron on phones, vertical (`writing-mode: vertical-rl`) in the 48 px desktop gutter (`app.R:813-830`).

### 5.3 Main column

`fluidRow` (`app.R:1022-1048`):
- `column(9)` wrapped in `div#tour_sp`:
  - `selectizeInput("sel_sp", "Species:", choices = NULL, width = "100%")` — **server-side**
    (`updateSelectizeInput(..., server = TRUE)`), choices supplied later.
  - `checkboxInput("us_only", "Only species in US waters", value = TRUE)` (margin-top −8 px).
- `column(3)` wrapped in `div#tour_mask`:
  - `selectInput("sel_mask", "Outlines:", choices = c("Program Areas (white)"="programarea_key",
    "Ecoregions (black)"="ecoregion_key", "None"="none"), selected = "ecoregion_key", width="100%")`.

Then:
- `uiOutput("sp_title")` — selectable taxon names + copy buttons (`app.R:1052`, rendered `1470-1494`).
- `uiOutput("layer_bar")` — the colored layer bar (`app.R:1053`, rendered `1497-1602`).
- **Hidden** `div#ds_layer_container` (CSS `display:none`) holding
  `radioButtons("ds_layer", "Display Layer", choices = c("Merged Model"="mdl_key"), selected="mdl_key", inline=TRUE)`
  — re-populated per species (`app.R:1055-1064`, `1871-1892`). The pills drive it by synthesising a
  `.click()` on the matching hidden radio.
- **Hidden** `div#representation_container` holding
  `radioButtons("representation", "Representation", choices = c("Original (native)"="native",
  "Interpolated (model)"="model"), selected="native", inline=TRUE)` (`app.R:1067-1076`).
- `card(style="position:relative", maplibreOutput("map"), div.zoom-extent-wrap >
  actionButton("btn_zoom_extent", "Zoom to layer", icon=icon("expand"), class="btn-sm",
  style="background:#fff;color:#333;border:1px solid #bbb;box-shadow:0 1px 4px rgba(0,0,0,.3)"))`
  (`app.R:1077-1089`). Positioned `top:27px; left:75px` (desktop) / `top:76px; left:18px` (≤575.98 px).

Other inputs: `chk_show_splash` (in the welcome modal footer, default TRUE), `btn_tour`,
`show_splash_pref` (set from JS), `ms_ver_token` (hidden), `map_click`, `map_marker_click_marker`.

### 5.4 Species picker — how the choices list is built

`app.R:457-480`:
```r
d_spp$lbl_cmn  <- if common_name non-empty then " ({common_name})" else ""
d_spp$label    <- "{sp_cat}: {scientific_name}{lbl_cmn}"
.make_choices  <- arrange(sp_cat, label) |> group_by(sp_cat) |>
                  summarise(layer = list(setNames(mdl_key, label))) |> deframe()
spp_choices_all <- .make_choices(d_spp)
spp_choices_us  <- .make_choices(filter(d_spp, coalesce(is_valid_usa, FALSE)))
spp_choices     <- spp_choices_us                 # default (us_only = TRUE)
```
So the select is **optgroup-grouped by `sp_cat`** (`bird, mammal, turtle, fish, coral, invertebrate,
primary_producer`; reptile/amphibian excluded), option label `"mammal: Odobenus rosmarus (Walrus)"`,
value = merged `mdl_key`.

Default selection: `Dermochelys coriacea` (leatherback) restricted to `is_valid_usa`, else the first
US-valid taxon (`app.R:477-480`).

**Scale**: v9 checkpoints (`workflows/data/manifests/`) — `merge_taxon`: 37,067 taxa total,
`valid_usa = 17,781`, `valid_global = 29,186`; `build_registry`: `n_models = 91,362`,
`n_taxon_model = 51,562`, 13 datasets; `publish_native`: **86,857 `native_asset` rows**.
The app's own comments cite ~16k taxa for analytics cardinality, 47,034 distinct `mdl_key` in
`native_asset` (v8), and a 21.7 MB `native_asset` table (`app.R:544-548`, `1173`).

### 5.5 Modals

1. **Welcome / About** (`show_welcome()`, `app.R:1229-1267`): image
   `{product_url(ver,"docs")}figures/overview-methods.svg`, blurb, links to the Scores app + Docs
   (`target="_blank"`), footer = `checkboxInput("chk_show_splash", …, TRUE)` +
   `actionButton("btn_tour","Take a Tour", icon("route"))` + `modalButton("Explore")`.
2. **Data version** (`app.R:1142-1156`): `msens::version_picker_html(ver, href = function(v)
   product_urls(v, access = ver_access(v))[["species"]])` → a `ul.list-group` with badges
   `pre-release` / `retired` / `🔒 restricted` and the current row marked `active … showing`
   (`version_ui.R:30-71`).
3. **Version under review** / **Unknown data version** (`app.R:1113-1129`).
4. **Model not found** (`app.R:1410-1428`) — explains a retired `mdl_key` and the IUCN-outside-EEZ case.

### 5.6 Mobile (`@media (max-width: 575.98px)`, `app.R:832-855`)

`--bslib-spacer` and `--bslib-mb-spacer` → 0.5rem; card bodies 0.5rem; header wraps with nav on row 2;
`.ms-title-sub` hidden; picker **labels hidden**; `.layer-bar` wraps and its `.layer-links` collapse behind a
`"{N} layers ▾"` toggle (`.layer-bar.expanded`, pure client-side `classList.toggle`); `.layer-note` hidden;
zoom button moves under the geocoder.

### 5.7 Tour (`conductor`, `app.R:1286-1328`)

5 steps: `#tour_sp` (bottom), `#tour_mask` (bottom), `.layer-bar` (bottom), `#map` (top),
`.collapse-toggle` (right).

---

## 6. Map — sources, layers, styling, interaction

### 6.1 Initial map (`output$map`, `app.R:1777-1816`)

```
maplibre(style = carto_style("dark-matter"), projection = ifelse(tgl_sphere,"globe","mercator"))
  |> fit_bounds(er_bbox)
  |> msens::add_pmline(list(
        c(ztile("programarea", tbl_pra_pm), id="pra_ln", source_id="pra_src", zone_line_args("programarea")),
        c(ztile("ecoregion",  tbl_er),      id="er_ln",  source_id="er_src", before_id="pra_ln",
          zone_line_args("ecoregion"))))            # NULL entries dropped
  |> add_fill_layer(id="pra_hover", source="pra_src", source_layer=pra_src_layer,
                    fill_opacity=0.01, fill_color="white",
                    tooltip=get_column("programarea_name"), before_id="pra_ln")
  |> msens::add_pmlabel(list(c(source=pra_pts, text_field="programarea_key", id="pra_lbl",
                               zone_label_args("programarea"))))
  |> add_fullscreen_control() |> add_navigation_control() |> add_scale_control()
  |> add_geocoder_control() |> add_globe_minimap(position="bottom-left")
  |> add_layers_control(layers = list("Program Area outlines"="pra_ln",
                                      "Program Area labels"="pra_lbl",
                                      "Ecoregions outlines"="er_ln"))
```

Zone styling (`msens::zone_style`, `zone_style.R:22-70`) — the one table both apps read:

| zone_type | line | label |
|---|---|---|
| `programarea` / `planarea` | white, width 1, opacity 1 | white, size 12, halo `rgba(0,0,0,0.75)` width 1 |
| `ecoregion` | black, width 3, opacity 1 | black, size 16, halo `rgba(255,255,255,0.85)` width 1.5 |
| `subregion` | `#d9d9d9`, width 2, opacity 0.7, dash `[3,3]` | **none** |
| anything else | white, width 0.5, opacity 0.45 | white, size 11, halo `rgba(0,0,0,0.75)` width 1 |

`add_pmline` (`viz.R:159-178`) = `add_pmtiles_source(id, url)` + `add_line_layer(...)`; `add_pmlabel`
(`viz.R:195-237`) = `add_symbol_layer` with `text_field = get_column(field)`, `text_allow_overlap = TRUE`,
optional per-feature `text_anchor`/`text_justify`/`text_offset_{right,down}` columns.

The species app **does not** draw ecoregion labels (only the Program-Area ones).

### 6.2 The species surface (`app.R:1895-2073`) — the core render observer

Triggers: `list(input$sel_sp, input$ds_layer, input$sel_mask, input$representation)`.
Deep-link "jigger" guard: while `rx_ds_layer()` holds a pending URL layer, skip any render whose
`ds_layer` has not caught up (`app.R:1901-1905`).

Sequence:
1. `clear_markers()`.
2. If the layer is unavailable → `clear_layer("r_lyr"|"r_src")`, `clear_legend()`,
   `showNotification("No data to display — this species lacks this layer", "warning")`.
3. Color ramp: `cols_r <- rev(RColorBrewer::brewer.pal(11, "Spectral"))`, `rng_r <- c(1, 100)`
   (`app.R:1925-1928`, and again at `2170-2173`).
4. Browser title → `session$sendCustomMessage("updateTitle",
   "{sci} distribution ({sp_cat}[: {common}]; mdl_key: {layer_mdl_key}) from {layer_name} | BOEM Marine Sensitivity")`.
5. URL → `/{ver}/species/?mdl_key={layer_mdl_key}`.
6. `clear_layer` on **all four** of `r_lyr`, `r_src`, `r_pm`, `pm_src` + `clear_legend()` (clearing a layer
   also drops its source, so re-adding cannot collide) (`app.R:1980-1982`).
7. Pick the asset: merged → `rows_for(layer_mdl_key)[1, ]$asset_url`; input →
   `pick_asset(layer_mdl_key, input$representation %||% "native")` with a native-first fallback
   (`app.R:557-570`, `1957`, `1993-1994`).
8. Branch:
   - **COG branch** (`app.R:1996-2021`): merged-with-COG → `cog_tile_url(merged_cog_url, "spectral_r", c(1,100))`;
     merged-without-COG **and** `has_model_cell` → `cell_tile_url(mdl_key=…, mtime=db_mtime)`;
     merged with neither → `NULL` ⇒ `showNotification("No surface published for this taxon in {ver}")`;
     input → `cog_tile_url(asset$asset_url, asset$colormap, c(asset$rescale_min, asset$rescale_max))`.
     Then `msens::add_cell_tiles(tile_url, id="r_lyr", raster_opacity = 0.8, before_id = "er_ln")`
     (`viz.R:294-312`: `add_raster_source(id="r_src", tiles=url, tileSize=256)` +
     `add_raster_layer(id="r_lyr", raster_resampling="nearest", raster_opacity=0.8)`) and
     `add_legend(title_str, values = c(1,100), colors = cols_r, position = "bottom-right")`.
     Layers-control entry: `"Raster cell values" = "r_lyr"`.
   - **PMTiles branch** (`app.R:2022-2033`): `add_pmtiles_source(id="pm_src", url=asset$asset_url)` +
     `add_fill_layer(id="r_pm", source="pm_src", source_layer=asset$source_layer,
     filter=list("==", list("get","mdl_key"), layer_mdl_key), fill_color="#3388ff",
     fill_opacity=0.5, before_id="er_ln")` + `add_categorical_legend(title_str, values="range (presence)",
     colors="#3388ff", position="bottom-right")`. Layers-control entry `"Range (presence)" = "r_pm"`.
   - else `showNotification("No native surface available for this input", "warning")`.
9. `rx_shown(list(mdl_key, name, type ∈ {"cog","merged","pmtiles"}, url))` — what a click will sample.
10. Outline visibility: `set_layout_property("pra_ln"|"pra_lbl", "visibility", pa_vis)` and
    `set_layout_property("er_ln", "visibility", er_vis)` from `input$sel_mask`.
11. `clear_controls("layers")` + `add_layers_control(list("Program Area outlines"="pra_ln",
    "Program Area labels"="pra_lbl", "Ecoregion outlines"="er_ln", <active layer>))`.
12. `er_clr` fill, if the param was given.

### 6.3 Fit-bounds & antimeridian (`app.R:1733-1767`, `1959-1975`)

`mdl_bbox(mdl_key)`:
- **v1–v7** — pure arithmetic, no lon/lat column exists:
  ```sql
  SELECT min(((cell_id - 1) %  {nc}) + 1) c0, max(((cell_id - 1) %  {nc}) + 1) c1,
         min(((cell_id - 1) // {nc}) + 1) r0, max(((cell_id - 1) // {nc}) + 1) r1
    FROM cell_model WHERE mdl_seq = {as.integer(mdl_key)}
  ```
  then cell **edges**: `x0 = xmin + (c0-1)*resx`, `x1 = xmin + c1*resx`,
  `y0 = ymax - r1*resy`, `y1 = ymax - (r0-1)*resy`.
- **v8+**:
  ```sql
  SELECT min(c.lon) x0, min(c.lat) y0, max(c.lon) x1, max(c.lat) y1,
         min(CASE WHEN c.lon < 0 THEN c.lon + 360 ELSE c.lon END) w0,
         max(CASE WHEN c.lon < 0 THEN c.lon + 360 ELSE c.lon END) w1
    FROM model_cell mc JOIN cell c USING (cell_id)
   WHERE mc.mdl_id = (SELECT mdl_id FROM model WHERE mdl_key = '{mdl_key}')
  ```
- Both feed `msens::lon_span_agg(x0, x1, w0, w1)` (`grid.R:175-185`): **keep the 0–360 frame iff
  `(w1-w0) < (x1-x0)` AND `(w1-w0) < 350`**, else keep −180..180. The returned `xmax` may exceed 180 and
  **must be passed to `fitBounds` unwrapped** — MapLibre reads `[160,48,210,66]` as crossing the dateline.
- Fit target (`app.R:1968-1975`): merged → `mdl_bbox(merged key)`; input → `native_asset` bbox **unless**
  `msens::bbox_spans_globe(bb)` (`grid.R:200-203`: NULL / non-finite / `xmax-xmin >= 350` ⇒ TRUE), in which
  case fall back to `mdl_bbox(sp_row$mdl_key)`, then `er_bbox`.
- `rx_fit_bbox()` remembers it for the "Zoom to layer" button (`app.R:1461-1464`).
- **Re-fit only when the SPECIES changes** (`rx_fitted_sp`, `app.R:2053-2056`) — switching layers or
  representations keeps the user's current camera.

### 6.4 OBIS occurrence overlay (`app.R:2079-2121`)

Fires on `list(input$tgl_obis, input$sel_sp)`. Tears down `c("obis_occ_fill","obis_occ")` first.
Requires `taxon_authority == "worms"` and an integer `taxon_id` (AphiaID), else
`showNotification("OBIS occurrences need a WoRMS AphiaID …", "warning")`.
```
sql   <- obis_h3t_sql(indicator = "n", aphiaid = <AphiaID>, res_max = 7L)
stats <- h3t_stats(sql)                 # /h3t/stats?q=<b64>&res_h3=4
lo/hi <- stats$p02 %||% min %||% 0 ; stats$p98 %||% max %||% 1   (guarded, lo<hi)
brks  <- seq(lo, hi, length.out = 5)
tiles <- obis_h3t_url(base_url = "h3tiles://h3t.marinesensitivity.org/h3t/{z}/{x}/{y}.h3t",
                      sql = sql, release = format(Sys.Date(), "v%Y%m%d"))
        → "{base}?q={URLencode(base64(sql), reserved=TRUE)}&release={release}"
add_h3t_source(id="obis_occ", tiles=tiles)      # registers maplibregl.addProtocol("h3tiles")
add_fill_layer(id="obis_occ_fill", source="obis_occ", source_layer="obis_occ",
               fill_color = interpolate(column="value", values=brks, stops=H3T_VIRIDIS5),
               fill_opacity = 0.6, tooltip = get_column("value"))
add_legend("OBIS occurrences: {sci}", values = round(c(lo,hi),0),
           colors = H3T_VIRIDIS5, position = "bottom-left")
```
`obis_h3t_sql(aphiaid=…)` (`obisindicators/R/h3t.R:427-530`) builds a `WITH RECURSIVE taxon_tree …`
CTE resolving the AphiaID subtree, then rolls `occ_h3` up to `LEAST({{res}}, 7)`, returning
`(cell_id, value, n)`. Zero occurrences → `showNotification("No OBIS occurrences found for {sci}.")`.

### 6.5 Click behaviour (`app.R:2124-2210`)

1. If `rx_marker_clicked()` is set (a click landed on the existing marker), consume the flag and return
   (`app.R:2128-2131`, `2214-2217` sets it from `input$map_marker_click_marker`).
2. **Cell id is computed with the global05 constants hard-coded** (a real bug for v1–v7):
   ```r
   col <- floor((lng + 180) / 0.05); row <- floor((90 - lat) / 0.05)
   if (col < 0 || col >= 7200 || row < 0 || row >= 3600) return()
   cell_id <- row * 7200L + col + 1L
   ```
   (`app.R:2137-2140`). Contrast `msens::cell_from_lonlat(lon, lat, grid)` (`grid.R:219-245`), which is the
   grid-aware version (col `floor`, row `ceiling`, 0–360 shift) — **use that in the port**.
3. Value sample by `rx_shown()$type`:
   - `"merged"` →
     ```sql
     SELECT val FROM model_cell
      WHERE mdl_id = (SELECT mdl_id FROM model WHERE mdl_key = '{mdl_key}') AND cell_id = {cell_id}
     ```
   - `"cog"` → `GET {tile_base_url}/cog/point/{lng},{lat}?url={shown$url}` → `values[[1]]`.
   - `"pmtiles"` → `NA` (presence only).
4. `NA` → grey pin + popup `"Cell {id}<br>Lon {lon,3}, Lat {lat,3}<br><i>no value here</i>"`.
5. Otherwise: `val_scaled = clamp((val-1)/99, 0, 1)`; `col_idx = round(val_scaled*10)+1`;
   `bg_color = cols_r[col_idx]`; `luminance = (0.299R + 0.587G + 0.114B)/255`;
   `txt_color = luminance > 0.5 ? "black" : "white"`; popup HTML:
   `<b>{sci}</b><br>Cell ID: {id}<br>Lon: {x}<br>Lat: {y}<br>Value: {round(val,3)}`.
6. `clear_markers()` + `add_markers(c(lng,lat), marker_id="click_marker", color=bg_color)` and a
   `clickPopup` custom message that opens a `maplibregl.Popup({closeButton:true, closeOnClick:true,
   maxWidth:'260px'})` immediately (mapgl's own marker popup needs a *second* click) (`app.R:862-870`).

Hover: only the `pra_hover` fill layer (`fill_opacity 0.01`, tooltip = `programarea_name`) — there is no
hover readout on the species raster.

---

## 7. Outputs

### 7.1 `output$sp_title` (`app.R:1470-1494`)

`div.sp-title` with `span.sci` (italic) + a `button.sp-copy` (`data-copy`, `title="copy scientific name"`,
Font-Awesome copy icon), then — only if a common name exists — `span.sep "·"` + `span.cmn` + a second copy
button. Selectable text (`user-select: text`) because selectize's own item swallows clicks.
Clipboard JS (`app.R:884-925`): delegated from `document`; `navigator.clipboard.writeText` when
`window.isSecureContext`, **always** with an `execCommand('copy')` textarea fallback (writeText rejects
`NotAllowedError` whenever the document is unfocused); flashes `✓`/`✗` for 1,200 ms with classes
`copied`/`copy-failed`.

### 7.2 `output$layer_bar` (`app.R:1497-1602`)

- `available` = `c("Merged Model" = "mdl_key")` plus one entry per `ds_key` in `ds_keys` for which
  `!is.na(sp_row[[ds_key]])`, labelled `mdl_names[ds_key]` (= `dataset.name_display`).
- Bar class: `.layer-bar.is-merged` (green `#198754`) when `ds_layer == "mdl_key"`, else
  `.layer-bar.is-input` (orange `#fd7e14`).
- Merged left content: `✓` + `"Merged Model"` + `" (maximum of {n_inputs} inputs)"` when `n_inputs > 1`
  (`.layer-note`, hidden on phones). **`n_inputs` is counted from the normalised edges
  (`n_inputs_of`, `app.R:511-515`), never from `taxon.n_datasets`/`n_ds`** — v1–v7 count the `ms_merge`
  self-edge and v8 does not.
- Input left content: `▶` + `"Viewing input: {layer_name}"` + the representation toggle + `" — "` +
  `a.merged-link "show Merged Model"`.
- Representation pills, only when `length(reps_for(in_key)) > 1` (`app.R:1567-1578`):

  | `dataset.on_grid` | native pill | model pill | tooltips |
  |---|---|---|---|
  | FALSE | **Original** | **Interpolated** | "the source SDM at its native resolution" / "resampled to the 0.05° scoring grid" |
  | TRUE (AquaX, v9) | **Delivered** | **As ingested** | "the band exactly as delivered (already on the 0.05° grid)" / "as ingested: rescaled to 1–100 with the ingest threshold applied — what the merge uses" |

- Pills (`app.R:1519-1536`): `a.layer-pill[.active]` whose `onclick` clicks the hidden radio
  `#ds_layer_container input[value="{ds}"]`. An input with **no published asset**
  (`!has_asset(sp_row[[ds]])`, `app.R:500-501`) becomes
  `span.layer-pill.unavailable` — `cursor:not-allowed; opacity:.5; text-decoration:line-through;
  border-style:dashed; background:transparent` — with title
  `"{label} feeds the merged model, but {ver} publishes no surface for it — nothing to draw"`.
- Mobile toggle: `a.layer-pill.layer-toggle` reading `"{N} layers"` with a `▾`/`▴` `::after`,
  `onclick="this.closest('.layer-bar').classList.toggle('expanded')"`.

### 7.3 `output$species_info` — the sidebar card (`app.R:1605-1706`)

Rendered as:
```
h5({scientific_name})
ul
  li  Common name: {common_name}
  li  Category: {sp_cat}
  li  ESA Listing: {esa_code} ({toupper(sub('ch_','',esa_source))})      # "NA" if esa_code is NA
  li  IUCN RedList: {redlist_code}
  li  WoRMS: <a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}"
              target="_blank">{taxon_id}</a>                             # only when taxon_authority=="worms"
  li  MMPA: Protected (20)          # only when is_mmpa is TRUE
  li  MBTA: Protected (10)          # only when is_mbta is TRUE
<b>Values</b>:
ul                                   # single model & no IUCN range → one flat <li>
  li  <a href="?mdl_key={ms_merge_key}" onclick="…">[<b>]Merged Model[ (IUCN masked)][</b>]</a>
      <br><em>(maximum of):</em>
      ul
        li <a href="?mdl_key={input_mdl_key}" onclick="…">[<b>]{name_display}[</b>]</a>
           <br><em>({value_info})</em>
        …
[<b>Mask</b><br><em>(to constrain extent)</em>:
ul
  li <a …>{name_display}</a> [<em> (required)</em> when ds_key == "rng_iucn"]]
```
Details:
- `has_iucn` = the taxon has an `rng_iucn` input (`app.R:1613`). It drives both the "(IUCN masked)"
  suffix and whether the Mask section appears at all.
- `value_models` = every `ds_key` in `ds_keys` present & non-NA for this taxon; `mask_models` = the same
  intersected with `ds_keys_mask` (`dataset.is_mask`).
- `make_link()` (`app.R:1620-1642`): the currently displayed layer is **bold**; an input with no published
  asset renders as plain text + `<em class='text-muted'>(no published surface)</em>` (no link);
  `str_info` (the `value_info` italic line) is emitted only for `type == "value"`.
- `er_score`, `rarity` and `is_valid_usa` are **carried in `d_spp` but never displayed**.
- Outbound links: **WoRMS only**. No IUCN, FishBase, BirdLife or GBIF links (the scores app builds an
  IUCN/BirdLife link; the species app does not).
- ESA score semantics (for the port's tooltips/docs): `NMFS:EN`/`FWS:EN` = 100, `:TN` = 50, IUCN
  CR=50 EN=25 VU=5 NT=2 LC/DD=1, MMPA floor 20, MBTA floor 10 (`msens::compute_er_score`; see the
  ingest notebooks and `workflows/CLAUDE.md`).

### 7.4 Notifications

`showNotification` in four places: no layer for this species (`warning`, `app.R:1921`), no surface
published in this release (`warning`, `2014`), no native surface for this input (`warning`, `2035`),
OBIS: missing AphiaID (`warning`, `2091`) / no occurrences (`message`, `2100`).

### 7.5 Plots / tables

**None.** The species app has no `plotOutput`, no `DT`/`reactable`, no chart.

---

## 8. Downloads / exports

**None.** There is no `downloadHandler`, no CSV/GeoTIFF export, and no "copy link" button. The only
clipboard action is copying the scientific or common **name** (§7.1). Every underlying artifact is a
public S3 URL, so a port could add downloads cheaply (COG / PMTiles / the taxon's Parquet partition).

---

## 9. Links to/from the scores app and docs

Outbound, all from `msens::product_urls(ver, access = ver_access(ver))` (`ver_token.R:159-182`):

| link | destination | where |
|---|---|---|
| nav "Scores" | `{app_base}/{ver}/scores/` (public) or `{preview}/{ver}/scores/` (restricted) | `app.R:683` |
| nav "Species" | current page → `span.nav-here` | `app.R:684` |
| nav "Docs" | `https://marinesensitivity.org/docs/{ver}/` or `{preview}/docs/{ver}/`, `target="_blank"` | `app.R:685` |
| nav "Home" | `https://marinesensitivity.org` | `app.R:686` |
| nav "Sign out" | `/cdn-cgi/access/logout` (preview instance only) | `app.R:692-695` |
| welcome modal figure | `{docs}figures/overview-methods.svg` | `app.R:1240` |
| welcome modal links | Scores app, Documentation | `app.R:1249-1255` |
| version modal rows | `product_urls(v, access = ver_access(v))[["species"]]` per row | `app.R:1152-1153` |
| unknown-version modal | `{atlas_base_url()}/versions.json` | `app.R:1127-1128` |
| restricted-version modal | `msens::preview_app_url("species", req)` | `app.R:1116` |

Inbound, from the scores app's Table of Species (`scores/app.R:2722-2726`):
```
../species/?mdl_key={URLencode(model_id, reserved = TRUE)}
```
— relative, so it stays inside `/v{n}/` on either host, and URL-encoded because a v8 `mdl_key` contains
`|` and `:`. On v1–v7 `model_id` is a `mdl_seq`, still passed as `mdl_key=` (the app accepts either name).
Historic links published in the BOEM final report use `?mdl_seq=` and `/mapsp/` (Caddy 301s, `&{query}`
preserved).

---

## 10. Analytics / logging / feedback

Two legs, both driven from the **browser** so no reactive ever blocks on network I/O
(`msens/R/analytics.R:1-53`).

- **GA4**: measurement id `G-9HW6L751XG`, one property for every product; `content_group` separates them.
  The species app sends `content_group = "species"` or `"species-preview"` on the preview instance, plus
  `app_name`, `app_version` (git SHA) (`app.R:720-722`).
- **Sheet beacon**: `window.msTrack(event, params, metrics)` queues a row and flushes at **10 events** or
  every **15,000 ms**, plus on `visibilitychange→hidden` and `pagehide`, via `navigator.sendBeacon` with a
  `text/plain;charset=UTF-8` blob (CORS-simple; Apps Script `/exec` answers no `OPTIONS`), falling back to
  `fetch(..., mode:"no-cors", keepalive:true)` (`analytics.R:364-427`). Endpoint = `MSENS_LOG_URL`;
  unset ⇒ silent no-op.
- Row schema (`ms_log_header()`, `analytics.R:66-69`): `timestamp, ip, session, event, params, n_rows, ms,
  status, error, app_version, app, client_id, session_id, page, referrer, user_agent`.
  `client_id` ← `localStorage["msens_client_id"]`, `session_id` ← `sessionStorage["msens_session_id"]`.
- Server→browser: `msens::ms_track(session, event, ...)` pushes a `"msTrack"` custom message;
  `ms_track_session(session)` pushes `"msTrackSession"` with `{ip, session}` — the IP is a **fallback only**
  (behind shiny-server a session always sees 127.0.0.1; the real address is baked in from the page GET by
  `msens::ms_client_ip(req)`, which prefers `CF-Connecting-IP`, then the first `X-Forwarded-For` hop, then
  `REMOTE_ADDR`) (`analytics.R:193-238`).
- Event names are normalised GA4-style (lowercase, `[a-z0-9_]`, ≤40 chars); GA params truncated to 100
  chars, the Sheet gets the full value (`ms_event()`, `analytics.R:94-131`).

Events emitted by this app:

| event | params | line |
|---|---|---|
| `select_species` | `mdl_key, scientific_name, common_name, sp_cat, taxon_id, n_datasets, redlist_code, us_only` — de-duplicated and **seeded with the default species** so the opening taxon is not logged as a user choice | `app.R:1186-1201` |
| `select_layer` | `layer, mdl_key` | `1206-1209` |
| `select_representation` | `representation, mdl_key` | `1210-1213` |
| `select_outlines` | `outlines` | `1216` |
| `toggle_us_only` | `enabled` | `1218-1220` |
| `zoom_to_layer` | — | `1221` |
| `open_about` | — | `1222` |
| `toggle_obis` | `enabled` | `1223-1226` |
| `deeplink_mdl_key` | `mdl_key, resolution ∈ {input_model, merged_model, not_found}` (**`resolution`, not `status` — `status` is a reserved hoisted column**) | `1388-1392` |
| `search_species` | `query` — client-side, hooked onto selectize's `type` event, **900 ms debounce, ≥3 chars, no repeats** | `951-970` |

A long comment at `app.R:939-950` records that a **result count is deliberately not logged**: with
`server = TRUE`, selectize's `currentResults.total` cannot distinguish a hit from a miss. Recover it at
analysis time by joining the query against the taxon list, or by treating a `search_species` with no
following `select_species` as unsuccessful.

No feedback form, no error reporting beyond `ms_track_query`'s `status`/`error` columns (unused here).

---

## 11. Performance tricks, caches, and gotchas recorded in comments

1. **`native_asset` sorted-table + hashed index instead of `split()`** (`app.R:544-562`): `split()` created
   **47,034 one-row tibbles, 156 MB**, to hold a 21.7 MB table — 70 % of the app's bundle, and most of why a
   species worker cost **222 MB** against the scores app's 9.6 MB. Replaced with `order()` + `rle()` +
   an `rle`-index environment: ~5 MB, O(1) lookup.
2. **Per-version memoised bundles** (`app.R:167, 585-589`) — one DuckDB connection + one manifest fetch +
   one table normalisation per version per worker.
3. **Registry disk cache** with a 300 s TTL (`version.R:38-74`) — without it every shiny-server session
   re-fetched `latest.txt` + `versions.json` + `manifest.json` over HTTPS (~0.58 s of TTFB).
4. **Cold start** (`apps/README.md:62-91`): ~15 s TTFB idle. Attaching packages 8.3 s; `build_bundle(v8)`
   8.8 s for species. `library(msens)` is 8.1 s first / 0.38 s last — its weight is the shared
   sf/terra/mapgl/duckdb stack. Mitigation in place is `app_idle_timeout 3600`. Caching a serialised bundle
   was evaluated (8.80 s → 3.03 s, 11 MB gzip) and **deferred** as a stale-metadata risk.
5. **`match()` not `[[`** in `n_inputs_of` (`app.R:508-515`) — `[[` on a `table` *errors* ("subscript out of
   bounds") for an absent key, so the `is.null()` guard never ran and the layer bar died on every v1 taxon
   whose only model is its merged model.
6. **Cached `SpatRaster` SEGFAULTS across sessions** — recorded in `workflows` memory
   (`project_multiversion_apps`); this app therefore samples clicks via titiler `/cog/point` rather than
   holding a raster.
7. **Clipboard rejects silently when the document is unfocused** (`app.R:901-907`): `writeText` throws
   `NotAllowedError`; an empty rejection handler produced a button that did nothing and said nothing.
   Always keep the `execCommand` fallback.
8. **`clear_layer` also removes the source of that id** (`app.R:1977-1982`) — re-adding a source with the
   same id would otherwise collide client-side, which is the bug that left a stale merged surface on screen
   when switching to an input.
9. **`fit_bounds` only on species change** (`app.R:2051-2056`) so layer/representation switches let you
   compare models at one camera.
10. **Whole-world asset bbox is treated as missing** (`app.R:1962-1974`) — a wraparound range's COG honestly
    *is* −180..180; obeying it framed every Bering Sea species off Iceland (apps#9).
11. **URL race** (`app.R:1136-1139`): the "echo the version into the URL" observer must also treat
    `mdl_seq` as owned, or it rewrote the URL to a bare `?ver=` before the deep-link observer read it.
12. **Deep-link "jigger"** (`app.R:1885-1905`): `rx_ds_layer` suppresses the stale merged-model render so a
    `?mdl_key=<input>` link does not flash the merged surface first.
13. **`shiny.autoreload` is a development option** and was left on in production (`app.R:29-33`): a legacy
    file watcher per process and a failed `wss://…/autoreload/` socket + red console line per page load.
14. **`window_title` must be set explicitly** (`app.R:995-1004`) — bslib otherwise flattens the `title` div
    (a whole nav bar) into the tab/bookmark title.
15. **Two `#sp_title` ids** — `uiOutput("sp_title")` already puts the id on its wrapper, so the rendered
    `div` must not repeat it (`app.R:1485-1487`).
16. **Local dev trap** (`apps/README.md:58-61`): the app prefers
    `~/_big/msens/derived/{ver}/serve.duckdb`, which is the *server's* view DB (views over `/share/...`) —
    a laptop run fails on it; run under a HOME without that file so it falls back to `sdm.duckdb`.
17. **Stale widget JS at the same package version** — swapping an htmlwidget package at an unchanged
    version leaves browsers on the cached JS (`reference_widget_js_stale_cache` memory).
18. **`native_asset` coverage is logged at startup** (`app.R:527-533`) so a registry that loses an asset
    class shows up in the app log, not only in the UI.
19. **Empty `sf` warns 4× about min/max of nothing** (`app.R:274-279`) — constructed explicitly instead.
20. **`docker exec` runs as ROOT on the server** — always `scripts/srv_render.sh` / `-u 1000:1000`
    (`workflows/CLAUDE.md`). Irrelevant to the JS port but relevant to regenerating the data.
21. **Repainting a COG under a stable key needs `DEPLOY_TITILER`** — GDAL `/vsicurl` caches the header
    in-process; a shrunken COG then reads past EOF and z2–z4 return HTTP 500 while z5+ look fine.

---

## 12. External dependencies → what the JS app must replace

| R package | what it provides here | JS replacement |
|---|---|---|
| `shiny` + `bslib` | page_sidebar layout, sidebar with mobile overlay, cards, modals, `input_switch`, `input_dark_mode`, notifications, reactive graph | any SPA framework + a CSS layer; Bootstrap 5 classes are already used verbatim |
| `mapgl` | R bindings over **MapLibre GL JS**: `maplibre`, `carto_style`, `fit_bounds`, `add_raster_source`/`add_raster_layer`, `add_pmtiles_source` (registers the `pmtiles://` protocol), `add_fill_layer`, `add_line_layer`, `add_symbol_layer`, `add_image_source`, `add_markers`, `add_legend`/`add_categorical_legend`, `add_layers_control`, `add_navigation_control`, `add_fullscreen_control`, `add_scale_control`, `add_geocoder_control`, `add_globe_minimap`, `clear_layer`/`clear_legend`/`clear_markers`/`clear_controls`, `set_layout_property`, `get_column`, `match_expr`, `interpolate`, `add_h3t_source` | **maplibre-gl** directly + `pmtiles` JS (`PMTiles` protocol) + hand-rolled legend/layer-control/minimap widgets (mapgl's controls are bespoke) + `maplibre-gl-geocoder`/MapTiler |
| `duckdb` + `DBI` + `dplyr`/`dbplyr` | the local release DB: `dataset`, `taxon`, `taxon_model`, `native_asset`/`model_asset`, `zone`, `zone_cell`, `model`, `model_cell`/`cell_model`, `cell` | **DuckDB-WASM** over the published Parquet (`{base}/{ver}/tables/*.parquet` + `serve/model_cell/mdl_id=*/`); or precompute the small tables to JSON/Arrow at build time |
| `msens` | version registry, grid registry, antimeridian math, tile-URL builders, zone styles, analytics snippet, ver tokens | a small JS module — every function is pure and documented in §2–§6 |
| `sf` (`read_sf`, `st_point_on_surface`, `st_as_sf`, `st_coordinates`, `st_sf`, `st_sfc`) | reading the Program-Area gpkg/FlatGeobuf and deriving label points | precompute label points to GeoJSON at build time (they are already cached CSVs), or `flatgeobuf` JS + `turf.pointOnFeature` |
| `terra` | only inside the dead `add_fixed_range_raster()` | — |
| `htmltools`/`glue`/`stringr`/`tibble`/`tidyr`/`readr`/`scales` | HTML building, string interpolation, `pivot_wider` of the taxon→input matrix, CSV caches | plain JS / template literals; `pivot_wider` → build a `Map<ms_merge_key, {ds_key: mdl_key}>` |
| `RColorBrewer` | `rev(brewer.pal(11,"Spectral"))` for the legend + popup color | hard-code the 11 hexes (see below) |
| `httr2` | `/cog/point` GET | `fetch` |
| `curl` + `jsonlite` + `base64enc` | h3t `/stats` GET, base64 of SQL | `fetch` + `btoa` |
| `conductor` (etiennebacher) | the 5-step guided tour | Shepherd.js (conductor wraps it) or Driver.js |
| `obisindicators` (optional) | `obis_h3t_sql()` / `obis_h3t_url()` SQL + tile-URL builders | port the two builders (they are pure string builders) |
| `librarian` | package install/attach | — |

Spectral-reversed 11-class ramp (RColorBrewer `Spectral`, reversed):
`#5E4FA2 #3288BD #66C2A5 #ABDDA4 #E6F598 #FFFFBF #FEE08B #FDAE61 #F46D43 #D53E4F #9E0142`.
The **raster itself** is colored server-side by titiler's `spectral_r` colormap, so the ramp above only has
to match the legend and click popup.

---

## 13. Hard parts to port (candid)

1. **The picker's option list — ~22k taxa with grouping and server-side search.**
   v9 `valid_usa = 17,781`, `valid_global = 29,186`; the app's set is
   `is_marine ∧ ms_merge_key ∉ NULL ∧ sp_cat ∉ {reptile, amphibian} ∧ (is_valid_global ∨ is_valid_usa)` —
   on v8 ≈ 22.2k (17,763 US + 4,473 global-only). Shiny does this with `server = TRUE` (the browser holds
   only the visible page). Client-side you need either a prebuilt compact index
   (mdl_key + sci + common + sp_cat + is_valid_usa ≈ 22k × ~80 B ≈ **2 MB raw, ~600 KB gzip**) loaded once,
   or a DuckDB-WASM `ILIKE` query per keystroke. Both are feasible; the trap is the **two** lists
   (US-only vs all) that swap on a checkbox while preserving the selection.

2. **`native_asset` is 86,857 rows (v9) / ~21.7 MB.** The app needs, per taxon: which inputs exist, which
   have a drawable surface, and per (mdl_key, representation) the url + bbox + rescale + colormap +
   source_layer. Shipping the whole table to the browser is too much; the natural shapes are
   (a) DuckDB-WASM over `tables/native_asset.parquet` with an HTTP-range point query per taxon
   (needs the file to be row-group-sorted on `ms_merge_key` to be cheap), or
   (b) a build-time shard per taxon / per `ms_merge_key` prefix. **The bbox columns are load-bearing**
   (fit-bounds) and cannot be dropped.

3. **`model_cell` click-sampling for the merged surface.** The `"merged"` branch runs
   `SELECT val FROM model_cell WHERE mdl_id = (…) AND cell_id = …` against a **local** DB. Over HTTPS this
   is one partition read of `serve/model_cell/mdl_id={id}/data_0.parquet` — the same file titiler reads,
   so DuckDB-WASM *can* do it with a range request. Total serving surface is **~580 M rows (v8) /
   784,076,697 rows (v9)** across 17,781 partitions, so never glob it; always resolve `mdl_key → mdl_id`
   from `tables/model.parquet` (91,362 rows) first. **But**: the vast majority of taxa now have a merged
   COG, so `/cog/point` covers them — the `"merged"` branch is the ~192-taxon residual.

4. **`mdl_bbox()` — an aggregate over a whole model's cells.** v8+ joins `model_cell ⋈ cell` and takes
   4 min/max plus a 0–360 recomputation. `cell` is 25,920,000 rows (`tables/cell.parquet`, 18 columns).
   Doing this client-side per species is the single most expensive query in the app. **Recommendation:
   precompute a `(mdl_key, xmin, ymin, xmax, ymax)` table at publish time** — `native_asset` already carries
   the per-asset bbox, and `publish_native.qmd:760-800` computes exactly this by cell-id arithmetic.
   Note v1–v7 needs `cell_model` (a 2.5° spatially-tiled surface) instead, which may not be published for
   every historical release.

5. **The v1–v7 adapter.** Two grids (`usa05` 0–360 vs `global05`), two id fields, two validity schemas, two
   asset registries, two value column names, and a `taxon_model` self-edge that exists on one side only.
   Every one of these has a recorded production bug behind it. `sdm_val_col` matters because the **served**
   v8 DB has *both* `val` and `value`, so a bare rename dies exactly in production.

6. **Antimeridian fit-bounds.** `lon_span_agg` must be ported exactly — including the fact that the returned
   `xmax` may be **> 180** and must be handed to `map.fitBounds` unwrapped, and the `bbox_spans_globe(350°)`
   rejection. Getting this wrong silently frames the wrong ocean (2,744 of v8's 17,763 models framed the
   whole globe before the fix).

7. **Access control for restricted releases.** Today it is a *process* property (`MS_PREVIEW`) behind
   Cloudflare Access with per-version policies scoped by URL path, plus an HMAC-signed session token so a
   v9 reviewer cannot steer a shared worker to v10. On GitHub Pages **none of this exists** — a static SPA
   cannot enforce it, and the underlying S3 objects for a restricted release are the same public bucket.
   This needs an explicit decision: publish only `public` releases from Pages, or keep a gated host.
   `atlas_versions()` derives `access` **fail-closed** (`prerelease → restricted`), so at minimum the SPA
   must hide restricted versions from its picker.

8. **The `/msens/tiles` custom factory.** It is a DuckDB-in-FastAPI service, currently **disabled by
   default** on `titiler-v8` (`MSENS_FACTORY=1` to mount it). Any port that keeps the merged-model fallback
   must either keep that service, or ensure every taxon has a merged COG (`PUBLISH_MERGED_COG=1`,
   `publish_native.qmd:838-935` builds the am-only aliases + the suitability-only painted COGs). The COG
   path is anonymous, cacheable and CDN-friendly; the SQL path is not.

9. **PMTiles per-model, not per-dataset.** Vector ranges are **one PMTiles file per model**
   (`{pmt_base}/{ver}/{ds}/{safe_key}.pmtiles?v={mtime}`, z0–z10) and the layer additionally filters on
   `["==", ["get","mdl_key"], key]`. That is straightforward in MapLibre, but the files live on
   `file.marinesensitivity.org` (Caddy), **not** GitHub Pages — cross-origin and outside the Pages budget.
   v9's last `publish_native` run reports `n_pmtiles_native: 0` (carried forward from a prior run), so the
   count must be re-checked before relying on it; v8 had **2,234** vector-range PMTiles rows.

10. **Zone outlines are per-vintage, not per-release.** `{base}/zones/{zone_set_key}/zones.pmtiles` with
    `source_layer = zone_type`, resolved from the manifest; `NULL` (draw nothing) is a meaningful answer for
    a release that has no such unit (v1 has no Program Areas). The FlatGeobuf sibling
    (`zones/{zone_set_key}/zones.fgb`) is read via GDAL `/vsicurl` for label points — replace with
    build-time GeoJSON.

11. **Server-only facts baked into the page.** The real client IP survives only on the page GET
    (shiny-server rebuilds the websocket as a localhost connection), and `APP_VERSION` is a `git rev-parse`
    at startup. In a static SPA the IP is simply unavailable (drop the column or let the log endpoint
    record it) and `APP_VERSION` becomes a build-time constant.

12. **DuckDB reserved words and grid constants that are hard-coded in one place.** `value` is reserved;
    `class`/`order` need quoting. And `app.R:2137-2140` hard-codes `7200/3600/0.05/±180` for the click →
    `cell_id` conversion, which is **wrong for v1–v7** (`usa05` is 3103 × 2006 from 141.10°E in a 0–360
    frame). Port `msens::cell_from_lonlat()` instead, and read the grid from `manifest.grid`
    (`build_version_manifest.qmd:212-214` publishes `nc, nr, xmin, ymax, resx, resy, lon360`).

13. **Cold-start parity is actually an opportunity.** ~15 s TTFB today is almost all R process startup.
    A static SPA removes it entirely — but only if the per-taxon lookups above are precomputed rather than
    re-derived from multi-GB Parquet on every selection.
