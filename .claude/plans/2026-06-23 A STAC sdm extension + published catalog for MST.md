# Plan: A STAC `sdm:` extension + published catalog for MarineSensitivity

## Context

**Why this question now.** The msens toolkit ingests many heterogeneous species-distribution sources
(AquaMaps, IUCN, BirdLife, NMFS/FWS critical habitat & ranges, SWOT/USGS turtles, GoMex & NCCOS & GoA
seasonal models, derived merge), scores them, and serves them — but all of that metadata is **embedded in
DuckDB tables, PostGIS tables, ad-hoc CSVs, and file-naming conventions**. There is no machine-readable
catalog, so the products are not *Findable* or directly *ingestable* by outside tools (QGIS, `rstac`/`pystac`,
ODC). The project lead's own *ocean-indicators* manuscript (which names msens as a case study) argues for
FAIR, cloud-native, persistent-identifier data publishing — so aligning msens with a catalog standard both
serves real users and walks the manuscript's talk.

**The temporal/phenology requirement is central, not incidental.** Many MST SDMs are **monthly or seasonal**
to capture phenology (migration, breeding, residency). The encoding is fragmented today:
- `gm` (GoMex cetacean/turtle): **monthly**, each `(taxon×month)` a separate `mdl_seq` with
  `model.time_period = "2019-01/P1M" … "2019-12/P1M"`; also lives as PostGIS `sdm_spatial` served by
  pg_tileserv (`?model_time=2019-01/P1M`).
- `nc_atl_birds_dens`/`nc_pac_birds_dens` (NCCOS seabirds): **seasonal** (`fall/spring/summer/winter`), stored
  as **season-named COG files** (`ARTE_summer.tif`) + `nc_models.csv`; DuckDB ingest still a TODO.
- `goa_marmam`: **seasonal** (forthcoming). AquaMaps/IUCN/BirdLife/critical-habitat: **static**.
- These surfaces are **climatological** — a *representative recurring* month/season (e.g. obs 2003–2019, env
  2015–2019 monthly means for `gm`), not a literal calendar instant. The apps disagree on mechanism: `sdm`
  → pg_tileserv MVT `?model_time=`; `sdm-cog` → public `titiler.xyz` reading season-named COGs by filename;
  `vmap` → local season TIFs summed in-memory. **That heterogeneity is itself the strongest argument for a
  unifying catalog standard.**

**What we already have (don't rebuild it).** The "DuckDB analytics with arbitrary-SQL titiler endpoint" in the
prompt **already exists**: `server/titiler/factory.py` base64url-decodes a `SELECT cell_id, value` against
`sdm.duckdb`, joins to the `cell_id` anchor COG (`r_bio-oracle_planarea.tif`), returns PNG XYZ tiles +
`/tilejson.json` + `/statistics`. We also emit per-version **COG** (`r_metrics_{ver}.tif`), **GeoPackages**,
**PMTiles**, **Parquet** exports, season-named **COGs** on the file server, and a PostGIS **`sdm_spatial`**
MVT layer. STAC's job is **not** to enable any of that — it is the *discovery/provenance layer over the
artifacts we already write + the live endpoints*, with the temporal axis made explicit.

**Verdict (answers to the three questions):**
1. **Beneficial? Yes, scoped as a publishing/discovery layer + a minimal `sdm:` extension.** Avoid the
   anti-pattern of one Item-per-`mdl_seq` (~37k near-identical bboxes × intervals). Models stay as rows /
   files; they are *advertised with an explicit temporal dimension*, not enumerated.
2. **Raster / vector / hexagon? Yes — at the catalog layer.** SDM semantics live in `properties.sdm:*`; the
   representation lives in the Asset (media type + band/column schema). `gm` even demonstrates this: the same
   monthly model exists as a **vector** MVT (PostGIS `sdm_spatial`) *and* a **raster** cell surface (DuckDB
   `model_cell`) — STAC carries both as assets on one Item. H3 is forward-looking (one `sdm:h3_resolution` +
   a `table` column; no ratified STAC DGGS extension exists).
3. **Eases sharing/ingestion + DuckDB/titiler? Yes outbound; STAC *indexes*, not powers, the endpoints.** A
   static catalog makes products machine-ingestable; each Item advertises a static cloud-native asset *and*
   the live tile endpoint (titiler-SQL and/or pg_tileserv), **parameterized by interval**, via
   `web-map-links` + `alternate-assets`, so a client picks delivery mechanism *and* month/season.

**Chosen scope:** full build **and** publicly publish the `sdm:` extension. **H3:** forward-looking only.

---

## Design

### 1. Catalog topology — Collection-per-dataset; Item per file/dataset surface; interval as a *dimension*, not an Item

```
Catalog: marinesensitivity                              (root)
└─ Collection: msens-v7                                 (per VERSION; sci:doi, version, summaries)
   │  assets: sdm.duckdb, sdm_parquet/, cell_id anchor COG (grid-anchor), titiler + pg_tileserv service roots
   ├─ Collection: msens-v7-dataset-am_0.05              (STATIC dataset)
   │  ├─ Item: …-am_0.05-model_cell                     (one Item; no temporal blocks)
   │  └─ Item: …-am_0.05-species
   ├─ Collection: msens-v7-dataset-gm                   (MONTHLY dataset)
   │  └─ Item: …-gm-model_cell                          (ONE Item w/ datacube time dim = 12 months)
   │        timeseries: ts:period "R12/2019-01-01/P1M"
   │        assets: data (model_cell.parquet) + vector (sdm_spatial MVT) + tiles (titiler SQL, {time_period})
   ├─ Collection: msens-v7-dataset-nc_atl_birds_dens    (SEASONAL dataset, file-per-season COGs)
   │  └─ Item per species: …-nc_atl-ARTE                (datacube season dim = 4 seasons)
   │        assets: summer→ARTE_summer.tif, winter→ARTE_winter.tif, … (one COG asset per season)
   └─ Collection: msens-v7-zones                        (vector zones: gpkg + pmtiles + zone_taxon csv)
```

Rules: **one Collection per `dataset` row**; **one Item per physical surface** (a DB-backed `model_cell`
table, or a per-species season-COG set). The **interval (month/season) is a `datacube` dimension on the Item**
(enumerated values), **not** its own Item. A per-`mdl_seq` surface is an **asset/link parameterized by
`{time_period}`/`{season}`/`{mdl_seq}`**. Result: tens of Collections + ~30–50 Items per version, each
carrying a small enumerable temporal axis — no explosion. (Optional: a handful of flagship per-species Items.)

### 2. Temporal / phenology encoding (the crux)

Three complementary, lightweight pieces — **all driven from `model.time_period` + `dataset.temporal_res`,
already in the schema**:

- **`datacube` dimension (primary, machine-actionable).** On each multi-temporal Item, add `cube:dimensions`:
  - monthly → a `temporal` dim with `extent`, `values` (`["2019-01",…,"2019-12"]`), `step:"P1M"`;
  - seasonal → an **additional categorical dimension** `season` with `values:["winter","spring","summer","fall"]`.
  - `cube:variables` maps the value (`suit`/`density`) to that dimension. This is what a client iterates to
    enumerate valid intervals and fill the asset template.
- **`timeseries` summary (compact, search-friendly).** At Collection/Item level: `ts:period` (ISO-8601
  repeating, e.g. `R12/2019-01-01/P1M` for monthly; `R4/…` or `ts:dates` for seasonal) + `ts:parameters`.
  Optional but cheap; good for discovery ("this is a 12-step monthly series").
- **`sdm:` phenology fields (semantics).** `sdm:temporal_interval` (`static|monthly|seasonal`, from
  `dataset.temporal_res`), `sdm:climatological` (boolean — **representative recurring** interval vs literal
  date; true for `gm`/`nc`), and per-asset/per-step `sdm:month` (1–12) or `sdm:season`
  (`winter|spring|summer|fall`) labels.

**Why `sdm:climatological` matters (second reason the extension must exist):** STAC `datetime` implies an
instant; a climatological "February" surface is a recurring representative interval. `ts:period` (R-repeating)
+ `sdm:climatological=true` says so; bare `start/end_datetime` would mislead. Core `start/end_datetime` still
carries the **observation window envelope** (and `sdm:env_datetime` the environmental vintage — the *other*
time axis), while the cube/timeseries carry the **per-step phenology**.

**Asset-vs-dimension split by storage reality:**
- DB-backed surfaces (`gm`, merged): one Item, dynamic asset href templated by `{time_period}` (titiler SQL
  `WHERE time_period='{time_period}'`, or pg_tileserv `?model_time={time_period}`); the cube enumerates the
  12 steps. No file explosion.
- File-per-season COGs (`nc_*`): one Item per species with **one Asset per season** (the real `*_summer.tif`
  files already on the file server), each tagged `sdm:season`; the cube's season dim lists the four.

### 3. Reuse aggressively — existing extensions → msens columns

| Extension | Fields | msens source |
|---|---|---|
| core | id, bbox, geometry, **start/end_datetime = observation window**, title, description, license, providers, keywords | `dataset.name_*`, `description`, `date_obs_beg/end` |
| **datacube** | `cube:dimensions` (temporal month / categorical season), `cube:variables` | `model.time_period`, season from filenames/`nc_models.csv` |
| **timeseries** | `ts:period`, `ts:dates`, `ts:parameters` | derived from the set of `time_period` per `(ds_key,taxon)` |
| **proj** | epsg=4326, shape, transform, bbox | `r_bio-oracle_planarea.tif` |
| **raster** | `raster:bands[]` nodata/unit/scale/statistics | `r_metrics_v7.tif`, season COGs (bidx per stat) |
| **table** | `table:columns[]`, primary_geometry, row_count | Parquet / GeoPackage / `sdm_spatial` |
| **scientific** | `sci:doi`, `sci:citation`, `sci:publications` | `dataset.citation`, RMMS + ocean-indicators DOIs |
| **version** | version="v7", deprecated, latest/predecessor/successor | `paths.R` ver/ver_prev |
| **classification** | `classification:classes` (value→label) | status→value maps in `merge_models.qmd` |
| **processing** | lineage, level, software | `model.description`, `{duckdb,terra,msens}` |
| **alternate-assets** | second URL for same data | Parquet file ⇄ live DuckDB SQL |
| **web-map-links** | `wml:xyz`, `wml:tilejson`, `wml:pmtiles` | titiler SQL tiles, pg_tileserv MVT, `.pmtiles` |
| **item-assets** | declare common asset dict on Collections | the repeating asset shape |

### 4. The new `sdm:` extension — minimal field set (cross-walked to RMMS)

Carry **only** SDM semantics no other extension covers. Cross-walk to **RMMS** (Merow et al. 2019,
*Glob. Ecol. Biogeogr.* 28(12):1912–1924, doi:10.1111/geb.12993); full RMMS records can live in a linked doc
(`rel="describedby"`), optionally generated via the CRAN `rangeModelMetadata` package.

| `sdm:` field | type / enum | req | msens column | RMMS |
|---|---|---|---|---|
| `sdm:response_type` | suitability\|probability\|range\|density\|biomass\|occurrence\|mixed | **req** | `dataset.response_type` | output type |
| `sdm:value_unit` / `sdm:value_range` | string / [min,max] | opt | convention | prediction units |
| `sdm:method` | env_envelope\|expert_range_polygon\|critical_habitat\|core_area\|derived_merge (open vocab) | **req** | `model.mdl_type` + desc | algorithm |
| `sdm:source_authority` | AquaMaps\|IUCN\|BirdLife\|NMFS\|FWS\|SWOT\|GoMex\|NCCOS\|BOEM\|MarineSensitivity | **req** | `dataset.source_broad/detail` | data source |
| `sdm:derived_from` | [ds_key] | req on merge | `model.description`/`taxon_model` | provenance |
| `sdm:predictors` | [string] | opt | env cols in `cell` / source inputs | predictor variables |
| `sdm:env_datetime` | [start,end] | opt | `dataset.date_env_beg/end` | env temporal extent (2nd axis) |
| **`sdm:temporal_interval`** | static\|monthly\|seasonal | **req** | `dataset.temporal_res` | temporal applicability |
| **`sdm:climatological`** | boolean | rec | implied by source (gm/nc=true) | (representative period) |
| **`sdm:month`** / **`sdm:season`** | int 1–12 / winter\|spring\|summer\|fall | per-step | `model.time_period` / season | seasonality |
| `sdm:taxon` | object {scientific_name(req at taxon nodes), common_name, authorities{worms,gbif,itis,iucn,botw}, group, redlist_code} | req at taxon nodes | `species`/`taxon` | taxon block |
| `sdm:h3_resolution` | int 0–15 | opt | (future H3 assets) | — (forward-looking) |

Everything else stays in `proj`/`raster`/`table`/`classification`/`scientific`/`processing`/`datacube`/
`timeseries` or the linked RMMS record. The extension is ~9 top-level keys + the nested taxon object.

### 5. Coexistence with the live endpoints — static AND dynamic, AND interval-parameterized

STAC holds ready-to-fetch URLs beside static assets; it **indexes**, doesn't power, the endpoints
(titiler's native STAC reader only mosaics per-Item COGs — it does not do arbitrary SQL, confirming the SQL
factory stays custom). One Item exposes multiple delivery mechanisms, each interval-aware:
- `assets.data` — `model_cell.parquet` (`table:columns`) opened directly by DuckDB/QGIS; `alternate.duckdb_sql`
  with `sdm:sql_template: "SELECT cell_id, value FROM model_cell WHERE mdl_seq = {mdl_seq}"`.
- `assets.tiles_raster` — titiler `tilejson`/`xyz` via `wml:*`, href embedding **urlsafe-base64** SQL exactly
  as `factory.py::_decode_sql` expects, with `{time_period}` substituted per cube step (+ colormap/rescale).
- `assets.tiles_vector` — pg_tileserv MVT (`public.sdm_spatial/{z}/{x}/{y}.pbf?...&model_time={time_period}`)
  via `wml:xyz`, for the vector representation.
- `assets.statistics` — titiler `/statistics?sql={sql_b64}` so a client sets `rescale` first.

`alternate-assets` = same bytes by another route (file ⇄ live SQL); `web-map-links` = rendered representation
(PNG/MVT tiles). The `{time_period}`/`{season}` placeholder is filled from the `datacube` dimension values.

### 6. Representation-agnosticism (raster / vector / hexagon)

The Item-level `sdm:` block is **byte-identical** across representations; only the asset's `type` + structural
extension changes: raster COG → `raster:bands`; vector GeoParquet/GeoPackage/MVT → `table:columns`; future H3
Parquet → `table:columns` naming the `h3` column + one `sdm:h3_resolution`. 0.05° grid is the present case
(join key `cell_id`, anchored by the cell-id COG advertised as a Collection asset). No H3 materialized now;
when added later it attaches as a *second asset on the same Item* — the catalog does not restructure.

---

## Implementation phases

**Phase A — Publish the `sdm:` extension.** Scaffold from `stac-extensions/template` into the project's own
org first (e.g. `MarineSensitivity/stac-sdm`; official `stac-extensions/*` adoption is a later community
proposal). Author `json-schema/schema.json` (the §4 fields incl. the phenology block + nested `sdm:taxon`),
`README.md` with the RMMS crosswalk, `examples/` (the worked Items below). Enable template CI
(schema-publish + example validation); cut a versioned release; host the schema at a **stable**
GitHub-Pages URL referenced in every Item's `stac_extensions`.

**Phase B — Generate the catalog from the stores (R).** New `msens/R/stac.R` (sibling to `db.R`/`calc.R`),
reading via `msens::sdm_db_con(version)` and templating static JSON (`jsonlite::write_json(auto_unbox=TRUE)`):
- `stac_dataset_collection(con, ds_key, ver)` — `dataset` row → Collection (`sci:*`, `version`, `sdm:` block,
  `ts:period` summary, `item_assets`).
- `stac_model_cell_item(con, ds_key, ver)` — builds the **`datacube` dimension from the distinct
  `time_period`** of that dataset; emits static + dynamic (titiler-SQL, pg_tileserv) assets templated by
  `{time_period}`; urlsafe-base64 via `openssl::base64_encode(sql,FALSE)` → `chartr("+/","-_",…)` strip `=`.
- `stac_season_cog_item(ver, sp_code)` — for `nc_*`: one Item per species, one COG Asset per season from
  `nc_models.csv` (`season`, `cog_url`, `bidx`), `raster:bands`, `sdm:season` per asset, season cube dim.
- `stac_metrics_cog_item`, `stac_zone_item`, `stac_version_collection`, `stac_root_catalog` — as before.
Reuse the path/URL constants in `workflows/libs/paths.R` so hrefs can't drift.

**Phase C — Wire into the pipeline + serve.** A `do_stac <- T`-gated emit chunk in `workflows/calc_scores.qmd`
(after the Parquet export, ~L4814) writes the tree to `glue("{dir_v}/stac/")`; `deploy_to_server.sh` rsyncs
to `/share/public/stac/`; add a `handle_path /stac/*` block to `server/prod/Caddyfile` (mirror `/pmtiles/*`,
+CORS). Static JSON — no new container.

**Phase D — Validate + announce.** `stac-validator --recursive` (or `rstac`/`pystac` read-back) in CI;
validate the published schema against examples; link the catalog from the docs site and reference it in the
ocean-indicators manuscript.

**Temporal-source harmonization (recommendation, flagged).** Temporal SDMs live in three places (DuckDB
`gm`; file-server season COGs `nc_*`; PostGIS `sdm_spatial`), and the merge currently collapses to
`temporal_res="static"` (`merge_models.qmd`). **Recommendation: catalog each where it physically lives in the
first pass** (don't block STAC on completing the `nc` DB ingest TODO at `ingest_sdm-nc.qmd`), while flagging
the clean end-state — fold all temporal SDMs into `sdm.duckdb` with `time_period` populated and
season-preserving scoring — as follow-on. (If you'd rather harmonize-first, that becomes Phase B0.)

**Deferred:** H3 materialization; a dynamic search tier via **stac-geoparquet** (`COPY … TO 'items.parquet'`)
and/or **pgstac** behind the existing PostGIS — only if cross-version/full-text search is later needed.

---

## Worked example — `gm` monthly Item (raster + vector + 12-month phenology), trimmed

```jsonc
{
  "type": "Feature", "stac_version": "1.0.0",
  "stac_extensions": [".../datacube/...", ".../timeseries/...", ".../projection/...", ".../table/...",
    ".../web-map-links/...", ".../alternate-assets/...", "https://stac-extensions.github.io/sdm/v1.0.0/schema.json"],
  "id": "msens-v7-gm-model_cell", "collection": "msens-v7-dataset-gm",
  "bbox": [-98,18,-80,31], "geometry": { "type":"Polygon","coordinates":[[...]] },
  "properties": {
    "start_datetime":"2003-01-01T00:00:00Z", "end_datetime":"2019-12-31T23:59:59Z",  // observation envelope
    "sdm:response_type":"density", "sdm:value_unit":"animals_km2",
    "sdm:method":"env_envelope", "sdm:source_authority":"GoMex",
    "sdm:temporal_interval":"monthly", "sdm:climatological": true,                    // recurring, not literal
    "sdm:env_datetime":["2015-01-01","2019-12-31"],
    "cube:dimensions": {
      "month": { "type":"temporal", "extent":["2019-01-01","2019-12-01"],
        "values":["2019-01","2019-02","2019-03","2019-04","2019-05","2019-06",
                  "2019-07","2019-08","2019-09","2019-10","2019-11","2019-12"], "step":"P1M" } },
    "ts:period": "R12/2019-01-01/P1M",
    "sdm:taxon": { "scientific_name":"Stenella frontalis", "group":"mammal", "authorities":{ "worms":137116 } }
  },
  "assets": {
    "data": { "href":".../sdm_parquet/model_cell.parquet", "type":"application/vnd.apache.parquet",
      "roles":["data"], "table:columns":[{"name":"mdl_seq","type":"int32"},{"name":"cell_id","type":"int32"},
        {"name":"value","type":"double"}],
      "alternate": { "duckdb_sql": { "href":"https://tile.marinesensitivity.org/msens", "roles":["data","virtual"],
        "sdm:sql_template":"SELECT cell_id, value FROM model_cell m JOIN model USING(mdl_seq) WHERE ds_key='gm' AND taxa='Stenella frontalis' AND time_period='{time_period}'" } } },
    "tiles_raster": { "href":".../msens/tilejson.json?sql={sql_b64}&colormap=spectral_r",
      "type":"application/json", "roles":["tiles"],
      "wml:xyz":".../msens/tiles/{z}/{x}/{y}.png?sql={sql_b64}&colormap=spectral_r" },
    "tiles_vector": { "href":"https://tile.marinesensitivity.org/public.sdm_spatial/{z}/{x}/{y}.pbf?dataset_key=gm&species_key=Stenella_frontalis&model_time={time_period}",
      "type":"application/vnd.mapbox-vector-tile", "roles":["tiles"],
      "wml:xyz":"https://tile.marinesensitivity.org/public.sdm_spatial/{z}/{x}/{y}.pbf?dataset_key=gm&species_key=Stenella_frontalis&model_time={time_period}" }
  }
}
```
(`{time_period}` is filled from a `cube:dimensions.month` value; `{sql_b64}` = urlsafe-base64 of the templated
SQL with that interval, baked to match `factory.py::_decode_sql`. The **seasonal `nc_*` Item** is the same
shape with a categorical `season` cube dim + one COG Asset per season tagged `sdm:season`. **Static** datasets
(AquaMaps/IUCN) simply omit the datacube/timeseries/`sdm:month|season` blocks.)

---

## Critical files

- `workflows/libs/paths.R` — source of truth for `ver`, `dir_v`, `metrics_tif`, `dir_pq`, PMTiles URLs.
- `workflows/calc_scores.qmd` — host for the `do_stac` emit chunk (after Parquet export ~L4814).
- `workflows/create_sdm-tables.qmd` (+ `_zoom-erd`) — schema/ERD; `model.time_period`, `dataset.temporal_res`.
- `workflows/ingest_sdm-gm.qmd` — monthly `time_period` construction (`2019-{MM}/P1M`, 12 rows/sp).
- `workflows/ingest_sdm-nc.qmd` — seasonal season-named COGs (`*_summer.tif`); **DB ingest TODO** at ~L324.
- `workflows/explore_goa-marmam.qmd` — seasonal GoA marine mammals (forthcoming).
- `msens/R/db.R` — `sdm_db_con(version)`; new **`msens/R/stac.R`** connects through it.
- `server/titiler/factory.py` — the base64url-SQL contract the dynamic raster assets encode against.
- `server/prod/Caddyfile` — add `handle_path /stac/*` (mirror `/pmtiles/*`).
- `apps/sdm` (pg_tileserv `?model_time=`), `apps/sdm-cog` (titiler COG by season filename + `nc_models.csv`),
  `apps/vmap` (local season TIFs) — the consumers whose temporal access the catalog must reproduce; their
  `month.abb`/`winter|spring|summer|fall` vocab seeds the `sdm:month`/`sdm:season` enums.
- **New repo** (own org): `stac-sdm` scaffolded from `stac-extensions/template`.

## Verification

1. **Extension schema:** `stac-validator` validates `examples/*.json` (static, monthly, seasonal) against the
   published `sdm` schema in the extension repo's CI.
2. **Catalog build:** run `do_stac` against v7 `sdm.duckdb`; `stac-validator --recursive catalog.json`; assert
   every asset `href` resolves to a real file or a live endpoint URL.
3. **Temporal round-trip:** read the `gm` Item with `rstac`/`pystac`; confirm `cube:dimensions.month.values`
   has 12 steps, `ts:period == "R12/2019-01-01/P1M"`, `sdm:temporal_interval=="monthly"`,
   `sdm:climatological==true`; for an `nc_*` Item confirm 4 season assets + categorical season dim.
4. **Interval-parameterized rendering:** take the `gm` Item, substitute one `month` value into
   `tiles_raster.wml:xyz` (and `tiles_vector` `model_time`), GET against the live titiler and pg_tileserv —
   confirm a rendered tile for that month (proves the baked base64 SQL matches `_decode_sql` and the temporal
   filter works end-to-end).
5. **Direct ingestion:** `duckdb -c "SELECT count(*) FROM read_parquet('…/model_cell.parquet')"` and open a
   season COG / `.gpkg` in QGIS via the STAC plugin — proves outbound ingestion without bespoke glue.
