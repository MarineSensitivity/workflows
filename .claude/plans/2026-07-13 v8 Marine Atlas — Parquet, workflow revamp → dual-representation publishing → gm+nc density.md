# v8 Marine Atlas — Parquet/workflow revamp → dual-representation publishing → gm+nc density

## Context

v8 is built, released, and live, but three problems motivate this revamp:

1. **Parquet format is stale and inconsistent.** Writes are split across two engines with
   ad-hoc options: DuckDB `COPY` (mostly zstd, no V2, row groups sized in *rows*) and
   `arrow::write_parquet` with pure defaults (snappy, Parquet 2.4). No `PARQUET_VERSION V2`
   anywhere; `ROW_GROUP_SIZE` set once and in rows not bytes. DuckDB 1.5.2 + arrow 24 are
   installed, so V2 + zstd + ~80 MB byte-sized row groups + native `GEOMETRY` are all available.

2. **The pipeline can't tell when data actually changed.** Every notebook's `output:` is
   `data/manifests/{target}.json`, tracked via `targets` `format="file"`. But each manifest
   embeds `built = Sys.time()`, so its bytes change every run → the file hash changes →
   **all downstream targets re-run every time**, even when the parquet content is byte-for-byte
   equivalent. There is no content hash anywhere. And nothing renders the notebooks' HTML as a
   tracked, reportable artifact (HTML is produced by `quarto_render` but is an untracked side
   effect).

3. **We publish the resampled surface as if it were native.** A core motivation for going global
   in v8 was to show BOTH the original native SDM and the resampled/rescaled surface used in
   scoring. Today the AquaMaps STAC publishes the **0.05° resampled** COG labeled as native, when
   AquaMaps is natively **0.5°**; and for vector datasets we publish original ranges as PMTiles but
   never publish their **0.05° gridded** raster representation. We must faithfully publish both
   representations for raster AND vector.

**Intended outcome:** a standardized, content-addressed, self-documenting pipeline (goal 1+2),
then faithful dual-representation native publishing (goal 3), then resume the deferred gm+nc
density ingest — all reproducibly, gated by env flags, with the score-equivalence guardrail intact.

## Decisions locked (from user)

- **Parquet**: V2, zstd, ~80 MB row groups, routed through reusable `msens` helpers.
- **DuckDB ≥ 1.5 minimum**; native `GEOMETRY` **enabled but not yet persisted** (design leaves room
  to add a GeoParquet cell-geometry column later without rework).
- **Change detection**: **order-independent DuckDB content fingerprint** (`count(*)` +
  `bit_xor(hash(row))` + `sum(hash(row))` over on-disk parquet). Deterministic manifest, drop
  wall-clock `built`. Idempotent skip-if-unchanged + force flags.
- **Serving surface**: **switch `serve/model_cell.parquet` to `PARTITION_BY(mdl_id)`** now — where
  `mdl_id` is a new stable **integer** id replacing the `mdl_key` string in `model_cell` (avoid the
  ~500 GB global `ORDER BY` spill; partition-pruned titiler reads), with an A/B benchmark + rollback.
- **Reporting**: every QMD renders to HTML with summary tables + a mermaid design diagram rendered
  as **PNG with lightbox**.
- **Native vs resampled**: publish BOTH (a) original-native and (b) resampled-model surfaces for
  raster and vector, correctly labeled in `native_asset` + STAC. (Design in Phase C below.)

---

## Phase A — Parquet standardization + content-hash change detection + reporting

The core. Converge the *options* (one place), not the *engines* (arrow stays for fork-parallel
per-species writes; DuckDB `COPY` stays for engine-resident bulk writes).

### A1. New `msens` code

- **`msens/R/parquet.R`** (new) — one internal option object `.atlas_pq` (arrow `version="2.6"`,
  duckdb `PARQUET_VERSION V2`, `COMPRESSION zstd`, `ROW_GROUP_SIZE_BYTES '80MB'` with
  `ROW_GROUP_SIZE` raised high so the byte limit binds), plus:
  - `write_atlas_parquet(x, path, chunk_size=)` — arrow path (in-memory tibble, fork-safe).
  - `copy_atlas_parquet(con, sql, path, order_by=NULL, per_thread=FALSE, partition_by=NULL)` —
    DuckDB `COPY` path (extend the Plan-agent signature with `partition_by` for Phase B).
  - Both call `require_duckdb()` so the version floor is enforced on every write.
  - **Gotcha to encode in a comment**: for the narrow `(mdl_key,cell_id,val)` schema, 122 880 rows
    compress to ~1–2 MB, so `ROW_GROUP_SIZE_BYTES '80MB'` only binds if `ROW_GROUP_SIZE` is raised
    (e.g. 1e8). Otherwise you silently keep ~1 MB groups.

- **`msens/R/manifest.R`** (new) — the change-detection core:
  - `hash_parquet(glob, con=NULL, cols=NULL)` — order-independent fingerprint over on-disk parquet
    (`count(*)` + `bit_xor(hash(cols))` + `sum(hash(cols))`, folded via `digest::xxhash64`).
    Reads finished files only — never re-runs the ingest.
  - `hash_query(con, sql, cols=NULL)` — same three aggregates over a DuckDB **table** (for
    table-only targets like `merge_taxon`, `score_*` whose real output is a DB table, not a file).
  - `write_manifest(path, target, content_hash, stats=list(), force=FALSE)` — deterministic JSON
    (fixed key order, **no timestamp**, machine paths dropped so it's host-independent); leaves the
    file untouched (mtime preserved) when unchanged unless `force`.
  - `force_target(target)` — reads `MSENS_FORCE_ALL` (global) + `MSENS_FORCE` (comma-separated
    target names). Notebooks pass it to `write_manifest(force=)` and gate their own rebuild.

- **`require_duckdb(min="1.5.0", con=NULL, spatial=FALSE)`** in `msens/R/db.R` — version guard;
  `spatial=TRUE` (future) asserts `LOAD spatial` for the eventual GEOMETRY cell column.

- **`report_parquet_summary(glob, con=NULL)`** + `report_table(x, caption=)` in `msens/R/viz.R`
  (or new `report.R`) — standardize the `## Outputs` kable across notebooks.

### A2. `msens/DESCRIPTION`

- `duckdb` → `duckdb (>= 1.5.0)`; add `digest` to Imports; add `arrow` to Suggests (guarded by
  `requireNamespace` in `write_atlas_parquet`). Bump `Version` (new exported API).
  `devtools::document()` after.

### A3. Per-notebook rollout (~22 notebooks, one repeated pattern)

Each parquet-writing notebook gets three mechanical edits:
1. **Write** → route through `write_atlas_parquet` (arrow ingests) or `copy_atlas_parquet` (DuckDB
   notebooks). Arrow ingests: `ingest_birdlife.org_botw`, `ingest_fisheries.noaa.gov_critical-habitat`,
   `ingest_fws.gov_critical-habitat`, `ingest_fws.gov_range-maps`, `ingest_iucnredlist.org_ranges`,
   `ingest_nmfs_core-areas`, `ingest_sdm-nc`, `ingest_turtles-swot-dps`. DuckDB-COPY:
   `build_registry`, `ingest_aquamaps`, `merge_models`, `publish_native`, `release_marine-atlas`.
2. **Manifest chunk** → replace `write_json(..., built=Sys.time())` with:
   compute content-derived `smry`, `h <- msens::hash_parquet(glob, con)` (or `hash_query` for
   table targets), `msens::report_table(smry)`, then
   `msens::write_manifest(manifest, target=..., content_hash=h, stats=list(...deterministic...),
   force=msens::force_target(...))`. **Do this in ALL msens targets** — including DB-table-only ones
   (`build_cell_grid`, `ingest_taxon`, `ingest_worms`, `merge_models_prep`, `merge_taxon`,
   `build_common_names`, `build_app_support`, `score_zones`, `score_cell_metrics`,
   `score_zone_metrics`) via `hash_query`.
3. **Reporting** → add a `## Design` mermaid block (renders to PNG + lightbox) + a `## Outputs`
   summary section.

`build_targets_list()` and the `msens:` front-matter contract are **unchanged** — `output:` stays
the manifest JSON; only what's written into it changes. `[auto]` release caboose unchanged.

### A4. `_quarto.yml` + index

- Add `format: html: { mermaid-format: png }` (lightbox already on). Ensure headless Chromium is
  available on render hosts (`quarto install chromium` if missing); fall back to SVG where absent.
- Do **NOT** enable `freeze` — `targets` already controls execution; `freeze` keys on the .qmd text
  hash not the upstream data, so it would serve stale results when only data changed.
- `scripts/build_workflows_index.R`: `last_ran` now comes from the manifest **file mtime** (with
  skip-preserved mtime this is "last time output changed"), not `$built`.

---

## Phase B — Serving surface: `PARTITION_BY(mdl_id)` + titiler partition-pruning

Introduce **`mdl_id`** — a stable **integer** model id that replaces the `mdl_key` **string** in
`model_cell` for serving. Replace the ~3 GB global `ORDER BY mdl_key` serve file with a dataset
partitioned on `mdl_id` (the proven AquaMaps-COG trick — avoids the ~500 GB `ORDER BY` spill and
lets titiler prune by partition instead of row-group; the narrow integer key also shrinks the
serving surface vs. the 40-byte `mdl_key` string).

- **`mdl_id` assignment**: a stable integer per `mdl_key`, added as a column in the **`model`
  registry** (`build_registry.qmd`) so it survives releases (assign densely over sorted `mdl_key`;
  never renumber existing keys). (Precedent: `publish_native` already partitions am on an integer
  key — `_am_parts/mid=*` — the same trick, now generalized and named `mdl_id`.)
- **Serve write**: `release_marine-atlas.qmd` joins `mdl_id` onto `model_cell` and writes
  `serve/model_cell/mdl_id=*/…parquet` via `copy_atlas_parquet(con, sql, dir, partition_by="mdl_id")`
  (each partition itself V2/zstd/80 MB; keep `mdl_key` as a data column too for readability). Guard
  behind `RELEASE_REDO_SERVE`; set `partitioned_write_max_open_files` as in the am path.
- **Serving path**: the `serve.duckdb` `model_cell` view exposes `mdl_id` + `mdl_key`; the titiler
  client SQL becomes `SELECT cell_id, val AS value FROM model_cell WHERE mdl_id={mdl_id}`
  (partition-prune on the integer). The app/URL builder maps `mdl_key→mdl_id` from the `model`
  registry it already loads. Touches `../server/titiler/factory.py` and the tile-URL builder
  (`msens::cell_tile_url` / app).
- **A/B + rollback**: keep the current sorted single-file serve as a fallback path; benchmark a
  titiler point-read (`/statistics`) under both before flipping titiler-v8; one-command revert.
- **Guardrail**: no score math changes → `pra_score_delta` unaffected; verify `scores_v8`/`species_v8`
  still render after the flip.

---

## Phase C — Faithful native vs resampled dual-representation publishing

Publish **two labeled representations** of every source model instead of one mislabeled surface:
- **(a) native original** — AquaMaps at its true 0.5°; vector datasets as original polygons; density
  at native #/km².
- **(b) resampled model** — the global 0.05° grid surface actually used in scoring (density rescaled
  to [0,100]).

**The bug**: `native/am/{key}.tif` today is painted from `dist/dataset=am/*.parquet` (the 0.05°
resampled `val`) yet labeled "native" — it's actually (b). And vector inputs publish only (a) as
PMTiles; their (b) 0.05° gridded raster is never published.

### C1. Add a `representation` dimension
Add `representation ∈ {native, model}` to the `native_asset` registry — set it in both
`am_reg` (`publish_native.qmd:301`) and `pmt_reg` (`:221`), carry it through the `bind_rows`/joins
into `native_asset` (`:353-369`), and add the column to the app's fallback empty tibble
(`apps/species/app.R:223-227`). Row grain becomes one row per `(ms_merge_key, mdl_key, representation)`.

### C2. Publish the missing surfaces (reuse existing helpers — no new machinery)
- **AquaMaps native 0.5° COG** — build from `am.duckdb` `spp_cells(sp_key, cell_id, probability)`
  painted onto the native 720×360 `loiczid` grid (revive the v7 pattern in
  `old/ingest_aquamaps_res05.qmd:199-228`, or `libs/am_functions.R` `get_hcaf_raster`/
  `get_species_raster`). Publish to `native/am/native/{key}.tif`, `representation=native`,
  rescale 0–100 (probability×100). Relabel the existing `native/am/{key}.tif` as
  `representation=model`.
- **Vector inputs 0.05° gridded COG** — for each vector model, `SELECT cell_id, val FROM
  read_parquet('dist/dataset={ds}/…')` → `msens::publish_cog(cell_id, val, out_tif, grid)`
  (`msens/R/publish.R:55-84`; the same call already used for am, `publish_native.qmd:287`;
  `grid = grid_spec(rast(cellid_tif))`). `representation=model`; keep the PMTiles as
  `representation=native`. Use `INT1U` for 0–100 er/coverage vals, `FLT4S` for fractional cover.
- **Density (gm/nc, overlaps Phase D)** — native #/km² COGs are `representation=native` (nc already
  publishes them; `stac_season_cog_item` references them); the resampled [0,100] surface via
  `publish_cog` is `representation=model`. gm needs its native density COGs built.

### C3. STAC — two representations per model, correctly labeled
Extend `stac.R` so `native_stac` (`stac.R:260-271`) emits **both** a native and a model asset
(e.g. `cog_native` + `cog_model` / `pmtiles_native` + `cog_model`) pointing at the actual per-model
files (not directories) with correct `sdm:` units + rescale via `.sdm_value` (`:95-104`): native
AquaMaps = probability, model = suitability [0,100]; native density = n_per_km2, model = [0,100].
Keep the `model_cell` GeoParquet `data` asset as the canonical (b).

### C4. Reproducibility loose-ends (from the dev plan §0.5)
- Fold the **tar-based file-host sync** into `publish_native.qmd`'s `sync` chunk (tar the
  `native/pmtiles/{ds}/` tree → one transfer → untar on `msens`; the per-file `rsync` stalls).
- Make the **`native_asset` → S3 `tables/native_asset.parquet`** push reproducible in
  `release_marine-atlas.qmd` (it's already in `rel_tables`; confirm it releases when present).
- App native/model **toggle**: `native_by_key[[mdl_key]]` returns native+model rows + a UI selector;
  no new render path (`cog_tile_url` takes any url+rescale). App UI work itself is out of scope here.

---

## Phase D — gm + nc density ingest (resume the deferred backlog)

Order: **nc first (in-repo, safe), then gm (needs Drive), then DEFER the merge/scoring fold-in.**

- **nc** (`ingest_sdm-nc.qmd`, already drafted): finish + validate; uses the new
  `write_atlas_parquet` + manifest helpers; two-tier keys `nc|{sp}|{season}` + annual `nc|{sp}`;
  p99.5 density→[0,100] cap stored as `val_cap`. Fix the missing Atlantic `mw_spp-multiple.csv` soft
  gap. Verify N models / val range / one species renders; commit.
- **gm** (`ingest_sdm-gm.qmd`): rewrite the v7 PostGIS notebook to the dist-Parquet pattern; hydrate
  19 Drive shapefiles + `spp_gmx.xlsx`; density = `_n`/40; sum Oceanic/Shelf split for the two
  dolphins; guilds via `GUILD:`; monthly `gm|{sp}|01..12` + annual `gm|{sp}`.
- Publish both density representations natively (Phase C machinery).

**Deferred / flagged (NOT in this plan):** the merge/scoring fold-in of gm/nc graded density
(`suit_ds <- "am"` at `merge_models.qmd:42` flattens non-am to ER floor; `!is.na(worms_id)` drops
guilds) — a composite-score change gated behind `pra_score_delta`. And **Phase 11 cutover** (make
v8 the default), unchanged from the dev plan.

---

## Verification

Prove downstream does NOT re-run when an upstream re-executes with identical data:

```r
library(targets); tar_make()                 # up to date once
tar_invalidate(ingest_rng_iucn); tar_make(ingest_rng_iucn)   # re-render, identical data
tar_outdated()                               # EXPECT character(0) — no merge/score/release
tar_visnetwork(targets_only = TRUE)          # EXPECT all downstream green
```
Positive control: change one input val → `tar_outdated()` now lists `merge_models`, `score_*`,
`release_*`. Byte-determinism: render a notebook twice, `md5` of its manifest must match (differs
today due to `built=Sys.time()`).

Format + serving: after Phase A/B, `release_marine-atlas` re-run, confirm `msens::attach_atlas()`
round-trips the V2/zstd parquet, the titiler-v8 `/statistics` smoke test passes under the new
partitioned serve, and `scores_v8`/`species_v8` still render.

## Critical files

- `msens/R/parquet.R` (new), `msens/R/manifest.R` (new), `msens/R/db.R` (`require_duckdb`),
  `msens/R/viz.R` (report helpers), `msens/R/workflow.R` (confirm NO change), `msens/DESCRIPTION`.
- `msens/R/publish.R`, `msens/R/stac.R` (Phase C representation dimension).
- `workflows/_quarto.yml`, `workflows/scripts/build_workflows_index.R`.
- Representative notebook edits: `ingest_iucnredlist.org_ranges.qmd` (arrow),
  `build_registry.qmd` + `release_marine-atlas.qmd` (DuckDB-COPY + serve partition),
  `publish_native.qmd` (Phase C), `ingest_sdm-nc.qmd` + `ingest_sdm-gm.qmd` (Phase D).
- `../server/titiler/factory.py` (Phase B client SQL).
