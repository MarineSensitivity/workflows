# Build out obisindicators via the H3T hex tiling service on the MST server

## Context

The manuscript *"Operationalizing Place-Based Ocean Indicators…"* (§4.4, Supplement S2)
describes a "store-plus-hex" pattern: an authoritative cloud-native DuckDB store is queried
by read-only SQL embedded (base64) in a map-tile URL, and a thin **H3T tile factory** renders
per-hexagon **H3J JSON** tiles on demand for MapLibre. It is deployed for CalCOFI environment
data (`CalCOFI/api-h3t`). The paper names the **"Future — biodiversity by hex (OBIS)"** leg as
the near-term next step: *"arbitrary taxonomic and spatial queries summarized to H3 on the fly…
the biodiversity analogue of the CalCOFI environment work"* (aligned with `iobis/speedy`, which
summarizes OBIS to H3 res 7).

This plan realizes that leg: serve `obisindicators` biodiversity metrics (ES50, Shannon, species
richness, # records) as zoomable H3 hexagon tiles from the **MST server**, backed by a DuckDB store
built from **OBIS open-data parquet**, with **live taxonomic + temporal filtering**. Outcome: a
deployed `h3t.marinesensitivity.org` service + a `mapgl` demo map, plus build code & docs added to
`obisindicators`.

**Decisions (confirmed with user):**
- **Full hybrid** store now: precomputed all-taxa layers *and* a species-level store for on-the-fly
  taxon/year filtering.
- **Demo region first, then global** OBIS extent.
- **Vendor** the FastAPI H3T engine into `server/h3t/` (like `server/titiler/`).

## How it works (the contract — already verified in code)

Reuse the data-agnostic FastAPI engine `CalCOFI/api-h3t-py` unchanged. Per
`app/sql_validate.py` + `app/h3t_query.py`, a served query must:
- be a single root `SELECT`/`WITH … SELECT`, projecting **exactly** `cell_id` (BIGINT H3 index),
  `value` (numeric), and optional `n` — no `SELECT *`, no extra columns;
- use no denylisted function. **`lgamma`, `ln`, `exp`, `case`, `least`, `sum`, `count`,
  `any_value`, `h3_cell_to_parent`** are all allowed → **ES50's Hurlbert math is expressible in
  pure DuckDB SQL.** (`read_parquet`/`httpfs_`/`attach`/`copy`/`pg_*` are blocked → all parquet/S3
  reads happen at **build** time, never in the served query.)
- The engine substitutes the `{{res}}` placeholder with the tile's H3 resolution (zoom→res ∈ [1,10],
  store tops out at **res 7** so clamp with `LEAST({{res}}, 7)`), wraps the query with a
  centroid bbox filter + `H3T_MAX_ROWS=50000` cap under a `H3T_STMT_TIMEOUT_MS=3000` timeout,
  and emits `{cells:[{h3id,value,n},…]}`. R client = `mapgl::add_h3t_source()`.

## Architecture

```
OBIS open-data parquet                  MST server (docker-compose)
s3://obis-open-data/occurrence/*.parquet
        │ build (DuckDB httpfs + h3 ext, offline)
        ▼
/share/data/obis/obis_h3.duckdb   ──►  h3t (server/h3t, FastAPI :8889)
  ├ idx_h3   (precomputed, all taxa)        │  base64 SELECT → H3J
  └ occ_h3_r7/_r5/_r3 (species-level)       ▼
                                       h3tcache (Varnish :6083) ──► Caddy
                                                  h3t.marinesensitivity.org
                                                          ▼
                                       mapgl::add_h3t_source()  (demo map)
```

## 1. Authoritative DuckDB store — schema + build pipeline

Build code lives in `obisindicators` (the "build out obisindicators" ask). Heavy lifting is DuckDB
SQL streamed over httpfs; an R driver (`DBI`+`duckdb`) orchestrates so it runs from RStudio on the
server. Output → `/share/data/obis/obis_h3.duckdb` (data-root convention matches titiler's
`/share/data/big/latest/sdm.duckdb`).

**Schema (one file, hybrid):**
- `occ_h3_r7(cell_id BIGINT, aphiaid BIGINT, phylum, class, "order", family, genus, species VARCHAR, date_year SMALLINT, records BIGINT)` — species-level, finest res. `_r5`, `_r3` = `h3_cell_to_parent` roll-ups (so low-zoom **filtered** tiles scan a small table, staying < 3 s).
- `idx_h3(res UTINYINT, cell_id BIGINT, n BIGINT, sp BIGINT, shannon DOUBLE, simpson DOUBLE, es DOUBLE)` — all-taxa, all-year, res 1–7 (fast default; ES50 precomputed so no per-request `lgamma`).

**Build steps** (`data-raw/build_obis_h3_duckdb.R` → `inst/sql/build_*.sql`):
```sql
INSTALL httpfs; LOAD httpfs; INSTALL h3 FROM community; LOAD h3;
SET s3_region='us-east-1'; SET s3_access_key_id=''; SET s3_secret_access_key='';  -- anon
-- step 1: parquet → res-7 species table  (Phase 1 adds a region predicate; Phase 2 drops it)
CREATE TABLE occ_h3_r7 AS
SELECT CAST(h3_latlng_to_cell(decimalLatitude, decimalLongitude, 7) AS BIGINT) AS cell_id,
       CAST(aphiaid AS BIGINT) AS aphiaid, phylum, class, "order", family, genus, species,
       CAST(date_year AS SMALLINT) AS date_year, COUNT(*) AS records
FROM read_parquet('s3://obis-open-data/occurrence/*.parquet')
WHERE dropped IS NOT TRUE AND absence IS NOT TRUE AND species IS NOT NULL
  AND decimalLatitude IS NOT NULL AND decimalLongitude IS NOT NULL
  -- PHASE 1 demo region (parameterized bbox), e.g.:
  -- AND decimalLatitude BETWEEN {lat_min} AND {lat_max}
  -- AND decimalLongitude BETWEEN {lon_min} AND {lon_max}
GROUP BY ALL;
-- step 2: coarse species tiers
CREATE TABLE occ_h3_r5 AS SELECT CAST(h3_cell_to_parent(cell_id,5) AS BIGINT) AS cell_id,
  aphiaid,phylum,class,"order",family,genus,species,date_year,SUM(records) records FROM occ_h3_r7 GROUP BY ALL;
CREATE TABLE occ_h3_r3 AS SELECT CAST(h3_cell_to_parent(cell_id,3) AS BIGINT) AS cell_id,
  aphiaid,phylum,class,"order",family,genus,species,date_year,SUM(records) records FROM occ_h3_r5 GROUP BY ALL;
-- step 3: precomputed all-taxa indicators, res 1..7 (R driver loops res, INSERTs the §2 ES50 CTE)
-- step 4: CHECKPOINT; then atomically symlink versioned file → obis_h3.duckdb
```
Est. size: `occ_h3_r7` ~50–120M rows global (~3–8 GB); `idx_h3` ~0.3–1M rows (tens of MB); file ~5–10 GB. Demo-region build is a tiny fraction.

## 2. Served SQL query templates (client base64-encodes; `{{res}}` substituted by engine)

**Default, all taxa — from `idx_h3` (fast, no per-request lgamma):**
```sql
SELECT cell_id, es      AS value, n FROM idx_h3 WHERE res = LEAST({{res}}, 7)  -- ES50
SELECT cell_id, sp      AS value, n FROM idx_h3 WHERE res = LEAST({{res}}, 7)  -- richness
SELECT cell_id, shannon AS value, n FROM idx_h3 WHERE res = LEAST({{res}}, 7)  -- Shannon
SELECT cell_id, n       AS value, n FROM idx_h3 WHERE res = LEAST({{res}}, 7)  -- # records
```

**Filtered (taxon/year) ES50 — from species store, live Hurlbert math** (line-for-line port of
`obisindicators::calc_indicators(esn=50)` in `R/analyze.R`; helper picks `occ_h3_r3|r5|r7` by res):
```sql
WITH src AS (
  SELECT CAST(h3_cell_to_parent(cell_id, {{res}}) AS BIGINT) AS cell_id, aphiaid, SUM(records) AS ni
  FROM occ_h3_r7                                  -- _r5 for res 4–5, _r3 for res ≤3
  WHERE class = 'Aves' AND date_year BETWEEN 2000 AND 2020   -- any taxon/year predicate
  GROUP BY 1, 2),
tot AS (SELECT cell_id, SUM(ni) AS n FROM src GROUP BY 1),
es AS (
  SELECT s.cell_id,
    CASE WHEN t.n - s.ni >= 50 THEN 1 - exp(lgamma(t.n-s.ni+1)+lgamma(t.n-50+1)
                                            -lgamma(t.n-s.ni-50+1)-lgamma(t.n+1))
         WHEN t.n >= 50 THEN 1 ELSE NULL END AS esi
  FROM src s JOIN tot t USING (cell_id))
SELECT e.cell_id AS cell_id, SUM(e.esi) AS value, ANY_VALUE(t.n) AS n
FROM es e JOIN tot t USING (cell_id) GROUP BY e.cell_id;
```
Filtered richness/`#records`/Shannon = same `src` CTE, simpler final SELECT. All pass the validator
(root WITH→SELECT, projection exactly `cell_id,value,n`, no denylisted funcs/schemas).

## 3. Service deployment — vendor into `server/h3t/`

Copy `CalCOFI/api-h3t-py` → `MarineSensitivity/server/h3t/` (`Dockerfile`, `app/`, `requirements.txt`).
Bake the h3 extension into the image (add `RUN python -c "import duckdb; duckdb.connect().execute('INSTALL httpfs; INSTALL h3 FROM community')"` to the Dockerfile) so the read-only container needs no community-extension network fetch at boot.

**`server/docker-compose.yml`** — add after the `titiler`/`titilecache` pair (ports 8889/6083 are free):
```yaml
  h3t:
    container_name: h3t
    build: ./h3t
    environment:
      H3T_DBS: "obis:/share/data/obis/obis_h3.duckdb"
      H3T_DEFAULT_DB: obis
      H3T_MAX_ROWS: "50000"
      H3T_STMT_TIMEOUT_MS: "3000"
    volumes: [ /share:/share ]
    ports: [ "8889:8889" ]
    restart: always
  h3tcache:
    container_name: h3tcache
    image: varnish:latest
    volumes: [ /share:/share, "./varnish/h3t.vcl:/etc/varnish/default.vcl:ro" ]
    ports: [ 6083:6083 ]
    environment: { VARNISH_BACKEND_HOST: h3t, VARNISH_BACKEND_PORT: 8889, VARNISH_HTTP_PORT: 6083 }
    command: "-p default_keep=604800"   # 7d, matches titilecache
    restart: always
    depends_on: [ "h3t" ]
```
**`server/varnish/h3t.vcl`** — clone `server/varnish/titiler.vcl`, change backend port → 8889
(cache key is the full URL incl. `?q=…&release=…`; service already sets `ETag`/`Cache-Control`/`Vary`).

**`server/caddy/Caddyfile`** — add (mirror titiler block):
```
h3t.marinesensitivity.org      { reverse_proxy h3t:8889 }
h3tcache.marinesensitivity.org { reverse_proxy h3tcache:6083 }
```
**Release invalidation:** build to `obis_h3_vYYYYMMDD.duckdb`, symlink → `obis_h3.duckdb`,
`docker compose restart h3t`, ban `^/h3t/` in Varnish; clients pass `&release=vYYYYMMDD`.

## 4. Build-out in `obisindicators` (reuses `calc_indicators()`)

- `data-raw/build_obis_h3_duckdb.R` — R driver (DBI+duckdb) running the §1 SQL; params: output path,
  resolutions, optional region bbox, `include_year`. Mirrors existing `data-raw/occ.R` style.
- `inst/sql/` — canonical SQL: `build_occ_h3.sql`, `build_idx_h3.sql`, and served templates
  `tile_{es50,richness,shannon,records}.sql` + `tile_es50_filtered.sql` (single source of truth for R + JS clients).
- `R/h3t.R` (new, `@concept h3t`) — exported helpers:
  `obis_h3t_sql(indicator=c("es","sp","shannon","n"), taxon=NULL, years=NULL)` → SQL string
  (chooses default vs filtered + correct `occ_h3_r*` tier); `obis_h3t_url(base, …)` → base64 `q=` URL.
- `tests/testthat/test-h3t-parity.R` — build tiny DuckDB from shipped `occ_SAtlantic`/`occ_1M`, assert
  SQL ES50/richness/Shannon per cell ≈ `calc_indicators()` (pins SQL math to the R reference).
- `vignettes/h3t.Rmd` — full workflow (build → register → serve → `mapgl::add_h3t_source()`); cross-link manuscript §4.4. Add `duckdb`,`DBI` to `Suggests`; `NEWS.md`/`_pkgdown.yml` entries.

## 5. Phased rollout

- **Phase 0 — local verification (laptop, no S3/server):** build a tiny `obis_h3.duckdb` from shipped
  `obisindicators::occ_SAtlantic` (has `decimalLongitude/Latitude, species, date_year, records`; species-level
  filter by `species`, full phylum/class filtering deferred to the S3 build), run `server/h3t` via
  `docker build`+`docker run` locally, hit `/h3t/health|stats|{z}/{x}/{y}.h3t`, render with `mapgl`.
  Validates the entire contract before touching the server.
- **Phase 1 — server demo region:** run `build_obis_h3_duckdb.R` against `s3://obis-open-data/occurrence/*.parquet`
  with a region bbox on the server → `/share/data/obis/`; deploy `h3t`+`h3tcache`+Caddy; publish the `mapgl` demo.
- **Phase 2 — global:** rerun the build without the region predicate; rebuild, restart, flush cache.

## Critical files
- `MarineSensitivity/server/docker-compose.yml`, `caddy/Caddyfile`, `varnish/titiler.vcl` (clone→`h3t.vcl`), new `server/h3t/` (vendored from `CalCOFI/api-h3t-py`: `app/`, `Dockerfile`, `requirements.txt`).
- `CalCOFI/api-h3t-py/app/sql_validate.py` + `app/h3t_query.py` — the served-SQL contract.
- `marinebon/obisindicators/R/analyze.R` (`calc_indicators` = ES50 spec), new `data-raw/build_obis_h3_duckdb.R`, `inst/sql/`, `R/h3t.R`, `vignettes/h3t.Rmd`, `tests/testthat/test-h3t-parity.R`.

## Verification (end-to-end)
1. **SQL parity:** `devtools::test()` in obisindicators — SQL indicators ≈ `calc_indicators(occ_SAtlantic)`.
2. **Validator:** each template passes `app/sql_validate.py::validate()` (add a pytest fixture, or assert in `obis_h3t_sql` round-trip).
3. **Service smoke (local then server):**
   ```bash
   curl -s .../h3t/health | jq .                      # {"ok":true,"default_db":"obis",...}
   SQL='SELECT cell_id, es AS value, n FROM idx_h3 WHERE res = LEAST({{res}},7)'
   Q=$(printf '%s' "$SQL" | base64 | tr -d '\n')
   curl -s ".../h3t/stats?q=$Q&res_h3=4" | jq .        # min/max/p02/p98/n for legend
   curl -sI ".../h3t/4/8/6.h3t?q=$Q&release=v1"        # ETag, Cache-Control; 2nd hit X-Cache: HIT
   curl -s ".../h3t/4/8/6.h3t?q=$Q" | jq '.cells[0:3]' # [{h3id,value,n},…]
   ```
4. **Filtered path:** base64 the §2 filtered ES50 (e.g. `class='Aves'`, 2000–2020); confirm non-empty cells and < 3 s at low zoom.
5. **Map:** `mapgl::maplibre() |> add_h3t_source(url=".../h3t/{z}/{x}/{y}.h3t", query=…)` renders ES50/richness; taxon dropdown swaps the SQL.

## Notes on execution
- Build + deploy run **on the MST server** (`/share` + S3 bandwidth); I provide code + commands and verify locally on `occ_SAtlantic` first. `server` repo edits may be auto-committed/pushed per standing preference; `obisindicators` edits will be confirmed before commit.
- Open: confirm `obis-open-data` S3 region/anon access at build time; pick the Phase-1 demo-region bbox (suggest a coverage-rich area, e.g. South Atlantic matching `occ_SAtlantic`, or a sanctuary/CalCOFI-adjacent box).
