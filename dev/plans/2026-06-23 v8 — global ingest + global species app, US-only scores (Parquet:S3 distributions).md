# Plan: v8 — global ingest + global species app, US-only scores (Parquet/S3 distributions)

## Context
v7 (shipped) decoupled `is_ok` from the BOEM Program-Area gate and exposed the full **US** study area
(~16,153 valid species) with Program Areas (~9,230) as a spatial scope. But every distribution is still
**clipped to the US study-area grid**, so the toolkit cannot show a species' worldwide range, cannot
compute global range-size / endemism / rarity, and conflates "valid species" with "valid in the US."

**v8 goes global on ingest and on the species app, while keeping all scoring inside the US BOEM study
area.** Concretely:
- **Global, fine (0.05°) species distributions** for every dataset, displayed worldwide in the species
  app (mapsp). This unlocks **range-size / endemism / rarity for all species**.
- **Scores app (mapgl) and all scoring stay limited to the US BOEM study area** — US EEZ + offshoots,
  extended to the coast exactly as `workflows/show_study-area.qmd` defines (`ply_boem-usa.gpkg`).
- Storage that makes global 0.05° feasible: **per-dataset distributions written to partitioned Parquet
  on S3** (a monolithic global 0.05° `model_cell` in DuckDB would be ~80 GB — impractical). DuckDB keeps
  only the lighter aggregate/derived tables and queries Parquet on demand.
- Validity is **relabeled into three explicit, precomputed flags** so each scope is a direct count:
  `is_valid_global` ⊇ `is_valid_usa` ⊇ `is_valid_pra`.

Region scope remains **spatial, never a hidden filter** (the v7 principle), now expressed as the three
named validity flags + zone queries.

## Status: READY FOR REVIEW

## Decisions (resolved with the user)
1. **Resolution / storage** — single **global 0.05° cell index** (one cell-id COG, longitude standardized
   to **[-180,180]**). Per-species distributions live in **dataset-partitioned Parquet on S3**, queried
   lazily by DuckDB/titiler; the **same cell index** is reused by the (US-only) score/aggregate layers.
2. **Costello-2017 realms** — **dropped/deferred** (only needed for global *scoring*, which v8 does not do).
   US-ecoregion rescaling stays exactly as v7. Keep the reference in `dev/msens_notes.md` for a future
   global-scoring version.
3. **Validity flags** — replace the single `is_ok` with three booleans on `taxon`:
   - **`is_valid_global`** = taxonomic/data validity (not extinct, marine, accepted/alt-rep taxonomy,
     redlist≠EX, + TBD criteria) **and has a merged distribution anywhere globally**.
   - **`is_valid_usa`** = `is_valid_global` **and** has ≥1 merged cell inside the US study area
     (`ply_boem-usa.gpkg`). ≈ today's 16,153.
   - **`is_valid_pra`** = `is_valid_usa` **and** has ≥1 merged cell inside the 20 BOEM Program Areas.
     ≈ 9,230.
4. **Range size / endemism** — computed for every `is_valid_global` species from its global 0.05°
   distribution (occupied-cell area sum); exposed as a taxon attribute + in the species app/report. Not
   wired into scoring in v8 (rarity-weighted scoring is a future option).

## Architecture at a glance
```
                        GLOBAL 0.05° cell index  (r_cellid_global.tif COG, [-180,180], ~17M ocean cells)
                                     │  cell_id is shared by everything below
        ┌────────────────────────────┴───────────────────────────────────────────┐
   GLOBAL species layer (Parquet/S3)                         US score layer (DuckDB, US-extent cells only)
   s3://…/dist/dataset=aquamaps/…parquet                     cell_metric → zone_metric → zone_taxon
   s3://…/dist/dataset=iucn/…       (raw, per dataset)       (per-ecoregion rescale, unchanged from v7)
   s3://…/dist/dataset=botw/…                                served by titiler factory #1 (cell_metric)
   s3://…/dist_merged/…             (merged per mdl_seq)
   range_size_km2 per species (from merged global)           mapgl (scores) — default = US study area
   served by titiler factory #2 (model_cell by mdl_seq)      filters is_valid_usa / is_valid_pra
   mapsp (species) — default = global, filters is_valid_global
```
DuckDB `sdm.duckdb` holds the small/derived tables: `taxon` (+3 validity flags +range_size), `model`,
`dataset`, `cell` (global grid + area_km2 + in_usa/in_pra membership), `cell_metric` (US only),
`zone`/`zone_cell`/`zone_metric`/`zone_taxon`, `metric`. It reads the Parquet distributions via httpfs.

---

## Phase 0 — Scaffolding & conventions
**Files:** `workflows/libs/paths.R` (ver `:4`), `apps/mapsp/app.R:37`, `apps/mapgl/app.R:60`, new S3 config.
- `ver <- "v8"`, `ver_prev <- "v7"`. Add path constants: global COG `r_cellid_global.tif`; S3 base
  (e.g. `s3://marinesensitivity/derived/v8/dist/`), with a local serving mirror under `/share/data`.
- Decide Parquet partitioning: **raw = `dataset=<name>/`**; **merged = by `sp_cat` (or hash bucket of
  `mdl_seq`), sorted by `mdl_seq`** so DuckDB predicate-pushdown skips row groups per species.
- Set up DuckDB `httpfs` + S3 credentials for ingest/merge/serve; confirm titiler can read the same.

**Verify:** `paths.R` sources; a trivial `read_parquet('s3://…')` round-trips from R and from the titiler container.

## Phase 1 — Global 0.05° cell grid + COG
**Files:** `workflows/ingest_aquamaps_to_sdm_duckdb.qmd` (grid build `~509-577`), `workflows/libs/am_functions.R`.
- New `create_grid_global` chunk: build a global 0.05° grid (7200×3600), ocean-masked (globe − Natural
  Earth hi-res land, any cell ≥1% ocean), assign global `cell_id`, write **COG in [-180,180]** to
  `r_cellid_global.tif`. Record `area_km2` per cell via `terra::cellSize` (varies by latitude) and
  boolean `in_usa` (within `ply_boem-usa.gpkg`) / `in_pra` (within `ply_programareas_2026`) on `cell`.
- This **replaces** the US-only `r_bio-oracle_planarea.tif` as the canonical index; US cells become the
  `in_usa` subset of the global grid (cell_ids are renumbered — expected for a new version).

**Verify:** `cell` rowcount ≈ global ocean cells; `sum(in_usa)` ≈ prior 661k order; COG `/debug/cog` reports
[-180,-90,180,90]; a known lon/lat maps to the right `cell_id`.

## Phase 2 — Global ingest → per-dataset Parquet
**Files:** `ingest_aquamaps_res05.qmd`, `ingest_birdlife.org_botw.qmd` (rasterize `~291-307`, `r_cell`
`:190`, "outside USA" `:412`), `merge_models_prep.qmd` (IUCN mask `:2981`, no-EEZ backfill `~3050-3100`),
`ingest_aquamaps_to_sdm_duckdb.qmd` (model_cell write `~585-593`), NMFS/FWS ingests.
- Each ingest writes its distributions to **partitioned Parquet** (`dataset=<name>/…`, columns
  `mdl_seq, cell_id, value`) against the **global** COG — no US clip for the global datasets (AquaMaps,
  IUCN/worms-marine, BirdLife/botw). **Remove** the "drop species outside US EEZ" backfill for the global
  output.
- US-only datasets (NMFS CH, FWS CH, FWS range) stay US-extent but write to the same Parquet schema
  (their cells are simply all `in_usa`); tag each in the `dataset` table with a native `extent` field.

**Verify:** per-dataset Parquet species/row counts; 3 extra-US species (e.g. an Indo-Pacific reef fish)
have global cells and zero `in_usa` cells.

## Phase 3 — Global merge + 3 validity flags + range size
**Files:** `workflows/merge_models.qmd` (merge + is_ok `~734-818`), `workflows/schema.qmd` (is_ok doc `~450`),
new `workflows/dev/build_v8.R` (clone `build_v7.R`; current is_ok UPDATE `~36-50`, chunk list `81-88`).
- Merge per-dataset Parquet → **merged distributions Parquet** (one merged model per `mdl_seq`,
  AquaMaps/IUCN-mask logic as today but on the global grid). `taxon.mdl_seq` references the merged model.
- Compute the **three flags** on `taxon`:
  `is_valid_global` = taxonomic gates (`merge_models.qmd:766-817`) ∧ has merged global cells;
  `is_valid_usa` = `is_valid_global` ∧ has merged cells where `cell.in_usa`;
  `is_valid_pra` = `is_valid_usa` ∧ has merged cells where `cell.in_pra`.
  Retire `is_ok` (migrate all readers; optionally keep as a deprecated view = `is_valid_usa`).
- Compute `range_size_km2` per species = Σ `area_km2` over merged cells above the suitability threshold;
  add a rarity class. Store on `taxon` (or a `taxon_range` table).
- Update `schema.qmd` to document the global grid, Parquet distributions, the 3 flags, and range size.

**Verify:** `count(*) WHERE is_valid_global` ≫ `is_valid_usa` (~16k) > `is_valid_pra` (~9,230); range size
finite and sane (a cosmopolitan species ≫ an endemic).

## Phase 4 — calc_scores (US-only, structure unchanged from v7)
**Files:** `workflows/calc_scores.qmd` (ecoregion min/max rescale, `cell_metrics_to_zone_metrics`,
`zone_taxon`, plus the v7 `backfill_cell` / `add_full_studyarea_zone` chunks), `workflows/dev/build_v8.R`.
- Scoring runs **only over `cell.in_usa`** cells, reading the merged Parquet filtered to US cells (or
  materialize a US-subset `model_cell` table in DuckDB for join speed). Rescaling = the 12 BOEM US
  ecoregions, **identical algorithm to v7** (no Costello).
- "FULL" study-area zone = the `ply_boem-usa.gpkg` to-shore polygon via `msens::cells_in_polygon()`
  (`msens/R/calc.R:35`); `zone_taxon` extended to ecoregions as in `build_v7.R:87-88`.

**Verify:** v8-vs-v7 PRA composite-score delta (mirror `build_v7.R:90-105`) ≈ 0 except where the global
re-ingest changed US cell membership; investigate any non-trivial drift.

## Phase 5 — titiler: one global COG, two SQL backends
**Files:** `server/titiler/factory.py` (`COG_PATH:38`, `DUCKDB_PATH:39`, `MsensCellsFactory:297`,
bounds `:153`, `/tiles:422`, caps `40-41`), `server/titiler/app.py`.
- Point the existing cell_metric factory at the **global** COG (US scores still paint only `in_usa`
  cells → empty elsewhere). Parameterize `COG_PATH`/source per factory.
- Add a **species factory** querying the **merged distributions Parquet** by `mdl_seq`
  (`SELECT cell_id, value FROM read_parquet('s3://…/dist_merged/…') WHERE mdl_seq = ?`), reusing the
  base64url-SQL + `/statistics` machinery; mount at a distinct prefix (e.g. `/msens-species`).

**Verify:** `/msens-species/statistics` returns finite min/max for a global species; a tile over the
Indian Ocean renders for that species and is empty from the scores factory.

## Phase 6 — mapsp (species app) → global, tiled, with range/rarity
**Files:** `apps/mapsp/app.R` (`get_rast:725-762`, dropdown `:184`, US bbox `:779`, cell COG `64-65`,
0-360 notes `149-150`, copy `529-538`).
- Replace in-R terra rendering with **`msens::add_cell_tiles()` + a species tile URL** (`msens/R/viz.R:293,337`),
  rescale via `msens::cell_stats()`. Default extent **global**; dropdown filters **`is_valid_global`**.
- Show **range size / rarity** per selected species; US Program-Area / ecoregion polygons become optional
  overlays, not clips. Update splash/"not found" copy for global coverage. Use the [-180,180] global COG.

**Verify:** an extra-US species shows its worldwide 0.05° range as XYZ tiles (viewport-only fetches); a US
species still renders; click readout + range-size display work.

## Phase 7 — mapgl (scores app) → US study-area extent (mostly copy/flag fixes)
**Files:** `apps/mapgl/app.R` (`ver:60`, `sr_choices:270-276`, FULL bbox `482-489`, `cell_sql:147-172`,
copy `965/975`).
- Default extent from `ply_boem-usa.gpkg`; scoring SQL/zone logic unchanged (US `cell_metric`/`zone*`).
- Dropdowns/filters use **`is_valid_usa`** (and `is_valid_pra` for PRA views). Reword the misleading
  "outside Program Areas" copy to the v8 study-area framing. Costello not introduced.

**Verify:** mapgl defaults to the US study area; FULL/USA/AK/GA/PA selectors + flower plots match v7
(within Phase 4 tolerance).

## Phase 8 — API / report / docs / cross-version
**Files:** `api/plumber.R` (`/stats.json:403-464`, report `91-99`, `?ver=` `:409`), `docs/stats.R`
(fallback `11-29`), `docs/*.qmd`, `MarineSensitivity.github.io/`, `msens/R/calc.R` (`species_for_cells:171-209`).
- `species_for_cells()` filters **`is_valid_global`** then inner-joins the supplied cells — a US AOI
  yields only US species via the cell join, so global validity can't distort US reports. **Keep it on the
  US cell tables** (assert/comment so it's never pointed at the global Parquet by accident).
- `/stats.json`: return `valid_species_global`, `valid_species_usa`, `valid_species_pra`, `n_cells`,
  `n_cells_global`, plus range/endemism summary; extend the funnel with a "global distribution" step.
- Report: A-vs-B for an AOI = `is_valid_usa` vs `is_valid_pra` within the AOI, plus rarity.
- Cross-version v1..v8 via the `?ver=` loop (global fields `NA` before v8). Update docs prose to
  distinguish **global valid / US study area / Program Areas**.

**Verify:** `/stats.json?ver=v8` returns all three counts; v7 endpoint unchanged; docs render live numbers.

## Phase 9 — Safe build & rollout
**Files:** new `workflows/dev/build_v8.R`, `workflows/dev/msens_notes.md`.
1. **Prototype end-to-end on ONE region / a handful of species** (global ingest → Parquet → merge →
   range size → species tile → US score) to validate the Parquet/S3 path, the [-180,180] COG, and the
   two titiler factories **before** the multi-hour full global build.
2. Full global ingest → merged Parquet → flags + range size → US scoring → `zone_taxon`.
3. v8-vs-v7 PRA delta gate (Phase 4). Smoke-test `/stats.json`, mapgl, mapsp against v8.
4. Snapshot prior apps (`apps_v7`) before cutting `latest`/apps to v8 (as done for v6→v7).

---

## Risks / watch-items
- **New S3 + httpfs infra**: credentials in R, DuckDB, and the titiler container; a **local serving
  mirror** likely needed so per-tile reads aren't network-bound. Confirm Parquet partitioning gives good
  per-`mdl_seq` predicate pushdown (avoid 16k tiny partitions).
- **Global 0.05° volume**: even as Parquet, merged global distributions are large; measure before the full
  build. Consider a separate `sdm_global` store so the US scoring DB stays lean.
- **Longitude [-180,180]**: a convention change from the current 0-360 COG (`msens/R/calc.R:25,38`,
  `st_shift_longitude`, `apps/mapsp/app.R:149-150`). Apply consistently on the new global path; do not
  retrofit US-internal code unnecessarily.
- **titiler cache budget**: global species tiles = far larger key space; size Varnish + factory
  `LRU_SIZE`/`MSENS_MAX_ROWS`; keep `mtime` cache-busting (`viz.R:357`).
- **Scoring drift**: global re-ingest can change which species/values land in US cells → ecoregion
  min/max shifts. The Phase 4 PRA delta is the guardrail; explain any drift, don't silently accept.
- **cell_id renumbering**: the global grid renumbers cells vs v7; ensure every consumer reads v8 ids.

## End-to-end verification
- Counts: `is_valid_global` ≫ `is_valid_usa` (~16k) > `is_valid_pra` (~9,230); range size sane.
- Species app renders any global species worldwide at 0.05° via tiles; US species unaffected.
- Scores app + report reproduce v7 US numbers within drift tolerance; A-vs-B side by side.
- A drawn AOI outside any PRA returns a non-empty species list (incl. extra-US context in the species app).
- `/stats.json?ver=v8` serves the three counts + range/endemism; docs render them live.
- Prototype-region run is green before the full global build.
