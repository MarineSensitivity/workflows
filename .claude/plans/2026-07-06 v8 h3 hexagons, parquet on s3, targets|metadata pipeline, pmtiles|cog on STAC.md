# Plan: v8 "Marine Atlas" — H3 res-7 sampling unit, targets/metadata pipeline, partitioned Parquet on S3, native-format model publishing + OBIS overlay

## Context

Two earlier v8 plans exist and are **complementary**:
- **Plan A** (`2026-06-23 v8 — global ingest…Parquet:S3`): go global on ingest + species app so worldwide
  range/endemism/rarity become computable, keep scoring inside the US BOEM study area, and move
  distributions to **partitioned Parquet on S3**. Introduces `is_valid_global ⊇ is_valid_usa ⊇ is_valid_pra`
  + `range_size_km2`.
- **Plan B** (`2026-06-24 v8 gm-nc reingest`): port GoMex `gm` (cetacean/turtle, monthly, ex-PostGIS) and
  NCCOS `nc` (seabird, seasonal, COG-only) onto the grid so `merge_models`/`calc_scores`/STAC pick them up.

Both assumed the **0.05° raster `cell_id` grid**. This plan **supersedes and merges** them under the new
direction, with the user's chosen (most ambitious) fork on every decision:

1. **H3 hexagon resolution 7 (~5.16 km²) *replaces* the 0.05° raster cell** as the core storage/scoring/serving
   unit, end-to-end (`cell`→`hex`, `model_cell`→`model_hex`, `cell_metric`→`hex_metric`). SDMs are interpolated
   onto hexes; scoring runs on hexes; hex surfaces serve through the **existing `h3t` tile service** (already
   res 1–7), so OBIS observations overlay on the identical `hex_id`.
2. **One comprehensive v8** including Plan A's global ambition, Plan B (gm/nc), plus the cross-cutting work
   (targets+metadata, S3 marine-atlas release, native-format publishing, OBIS+land overlay).
3. **Native-format publishing of the original source models is first-class**: per model emit **PMTiles**
   (vector), **COG** (raster), **hex-parquet + `sdm:h3_resolution`** (H3), described by `stac-sdm` items, so the
   mapper overlays the *original* model against the *interpolated* hex version. **PMTiles is the vector standard**;
   cut **stac-sdm v1.1.0**.

Cross-cutting mechanics:
- **Orchestration** → `targets` driven by **custom metadata tags in each pipeline `.qmd`** (CalCOFI convention),
  replacing today's stale `_targets.R` scaffold + bespoke `dev/build_v*.R` chunk-extraction scripts.
- **Release** → **partitioned Parquet published to `s3://oceanmetrics.io-public/marine-atlas/`** (versioned tree),
  replacing the single-`sdm.duckdb` release. DuckDB stays the working engine (attaches Parquet via httpfs).

Additional requirements the user set when reviewing this plan:
- **v7↔v8 Program-Area score equivalence is a central, test-driven concern** (see the Validation-first section).
- **Grow the `msens` library**: every reusable step becomes a documented (roxygen2 + `@concept`) function in
  `../msens`; notebooks orchestrate, the package does the work (CalCOFI's `calcofi4db` model).
- **Split the oversized notebooks** (`calc_scores.qmd` ~130 KB, `merge_models.qmd`) into smaller, logically
  scoped `.qmd`s wired by declared dependencies — which is exactly what the targets+metadata DAG enables.
- **Prefer the DuckDB `h3` extension** for all H3 work (`h3jsr` only as fallback).
- **Rewrite `workflows/CLAUDE.md`** (currently a symlink to shared personal guidance) into a real repo-specific
  file mirroring `../../CalCOFI/workflows/CLAUDE.md`, referencing the parent `../CLAUDE.md` and sibling repos.
- **Add `.claude/skills/`** with SDM characterize/ingest skillsets, mirroring `CalCOFI/workflows/.claude/skills`.

### A happy consequence: the longitude headache disappears
H3 is lat/lng-native — no 0-360 vs [-180,180] cell-raster convention to reconcile and no cell-id COG to
regenerate. This removes a whole class of Plan-A risk.

## Decisions locked
- H3 res-7 **replaces** the cell grid; `hex_id` = BIGINT H3 index at res 7; H3 ops via the DuckDB `h3` extension.
- v8 = everything at once (global + gm/nc + targets + S3 + native publish + OBIS/land).
- Native-format publishing is core; PMTiles is the vector standard; cut stac-sdm v1.1.0.
- Validation of v7↔v8 PRA equivalence is developed *first* and run continuously.
- Reusable logic lives in `msens`; big notebooks are split; `workflows/CLAUDE.md` + `.claude/skills/` authored.

## Current state (from exploration)
- **targets scaffolded but dead**: `_targets.R` + `R/run_*.R` reference non-existent qmd filenames; real v7 build
  is `dev/build_v7.R` (copy DuckDB → `UPDATE` SQL → regex-extract named chunks from `calc_scores.qmd` + `eval()`).
- **CalCOFI pattern to replicate**: per-qmd `calcofi:` YAML block (`target_name`, `workflow_type`, `dependency`,
  `output`, dataset metadata); `calcofi4db::parse_qmd_frontmatter()` + `build_targets_list()` emit
  `tar_target_raw(name,{<dep symbols>; quarto_render(qmd); <output>}, format="file")`; no `tarchetypes`;
  `_targets.R` = `library(targets); library(pkg); build_targets_list()`. (`CalCOFI/calcofi4db/R/workflow.R:655-848`.)
- **Grid today**: `cell` (661,372 US ocean cells, 0.05°, lon 0-360) built by `create_grid` in
  `ingest_aquamaps_to_sdm_duckdb.qmd:503-577` → COG `r_bio-oracle_planarea.tif`; `msens::cell_id_raster()`,
  `cells_in_polygon()`/`cells_in_pra()` (`msens/R/calc.R`).
- **Schema** (`schema.qmd`): `dataset`, `model`(mdl_seq), `model_cell`(mdl_seq,cell_id,value), `species`,
  `taxon`(is_ok…), `taxon_model`, `cell`(env + area_km2), `metric`, `cell_metric`, `zone`/`zone_cell`/
  `zone_metric`/`zone_taxon`, `listing`. Composite key `score_extriskspcat_primprod_ecoregionrescaled_equalweights`.
- **STAC**: `calc_scores.qmd:4962` `stac_catalog` chunk → `msens::stac_build()` (`msens/R/stac.R`) → static
  sdm-extension catalog at `file.marinesensitivity.org/stac/`; emits GeoParquet + DuckDB-SQL titiler link + datacube.
- **Serving**: `server/titiler` colors a cell-id COG by a base64 `SELECT cell_id,value`; `server/h3t` FastAPI
  serves OBIS as res-1..7 H3 tiles (h3j) from a DuckDB store, SQL contract `SELECT cell_id,value[,n]` with `{{res}}`,
  multi-DB via `H3T_DBS=obis:/path,msens:/path`, uses DuckDB `h3` community ext (`h3_latlng_to_cell`,
  `h3_cell_area`, `h3_cell_to_lng/lat`); `mapgl::add_h3t_source()` + `apps/h3-db/app.R` consume it (no land overlay).
- **stac-sdm** v1.0.0 (unreleased): COG + GeoParquet/MVT mature; **H3 path aspirational** (`sdm:h3_resolution`
  field, no example). PMTiles infra exists at server layer, uncited by STAC; pg_tileserv MVT is dead.
- **R/DuckDB H3**: `h3jsr` installed; DuckDB `h3` community ext proven in `h3t` → primary path.
- **CLAUDE.md**: `workflows/CLAUDE.md` is a symlink to `../../bbest/ai_guidance/CLAUDE.md` (generic). The real
  model is `CalCOFI/workflows/CLAUDE.md` (155 lines: repo purpose, sibling pkgs, targets-first commands,
  data-flow, conventions). Parent `MarineSensitivity/CLAUDE.md` holds general R/Quarto style.
- **Skills model**: `CalCOFI/workflows/.claude/skills/` = `RUNBOOK.md` + `explore-dataset.md`,
  `generate-metadata.md`, `ingest-new.md`, `validate-ingest.md`, `publish-template.md` + `templates/*.qmd`.

## Architecture at a glance
```
                         H3 res-7 hex grid  (hex_id = BIGINT, lat/lng-native — no COG, no 0-360/-180 issue)
                                     │  hex_id shared by everything below AND by the OBIS h3t store
        ┌────────────────────────────┴───────────────────────────────────────────────┐
   GLOBAL species layer (Parquet → S3 marine-atlas)          US score layer (derived, US hexes only)
   dist/dataset=<key>/*.parquet  (raw per-model hex)         hex_metric → zone_metric → zone_taxon
   dist_merged/*.parquet          (merged per mdl_seq)       (per-ecoregion rescale, v7 algorithm)
   range_size_km2 / rarity (is_valid_global)                 served by h3t (SELECT hex_id AS cell_id, value)
   served by h3t (msens DB)  ──►  mapgl::add_h3t_source()    mapgl (scores): default US, is_valid_usa/pra
   mapsp (species): default global, is_valid_global
                                     │
   ORIGINAL models (native → marine-atlas + stac-sdm v1.1.0): vector→PMTiles | raster→COG | H3→parquet(hex_id)
   OBIS overlay: obisindicators h3t store, SAME hex_id  ·  LAND overlay: Natural Earth PMTiles above hex fill
```
Working store holds only light/derived tables (`hex` +env +area_km2 +in_usa/in_pra, `taxon` +3 flags +range_size,
`model`, `dataset`, `hex_metric` US-only, `zone*`, `metric`) and attaches the Parquet via httpfs. **Release** = the
versioned Parquet tree on S3.

---

## Validation-first (cross-cutting — built in Phase 1, run every phase)

The grid change (raster cell → hexagon) will shift interpolated values somewhat; the guardrail is that the
**Program-Area composite scores stay nearly equivalent between v7 and v8 on a common input subset**. This test
harness is authored **before** the migration and is a continuous gate, not an end-of-project check.

- **New `msens/R/validate.R`** with documented functions: `pra_score_delta(con_v7, con_v8)` (join PRA composite
  scores, return per-PRA delta + summary), `assert_within_tolerance(delta, mean_tol, max_tol)`, `hex_grid_checks()`
  (hex ids ⊂ `hex`, finite/positive values, membership sanity), `mass_conservation(model, interval)` (source-vs-hex
  integral for a taxon/interval).
- **New `workflows/validate_v7_v8.qmd`** (a `workflow_type: test` targets node depending on the score nodes):
  runs the common-input comparison — restrict both versions to the **same datasets** (e.g. AquaMaps-only, the
  dominant source) and the same taxa, compute PRA composite scores each way, and assert
  `mean|Δ|` and `max|Δ|` under agreed tolerances (start e.g. mean ≤ 0.02, max ≤ 0.05 on the 0–1 score; tune on
  first run). Emits `v8_vs_v7_pra_score_delta.csv` + a plotted report; a breach fails the target.
- **`tests/testthat/` in `msens`** for the pure helpers (delta math, tolerance assertions, hex indexing parity vs
  the OBIS store) so regressions surface in `devtools::test()` independent of the multi-hour build.
- Wired so `targets::tar_make("validate_v7_v8")` is the standard "is the migration still faithful?" command, and
  the `/validate-sdm` skill (Phase 10) invokes it.

---

## Phase 0 — targets + qmd-metadata convention; msens/paths/S3 scaffolding

**Files:** new `msens/R/workflow.R` (+ `tests/`); rewrite `workflows/_targets.R`; `msens:` YAML block in every
pipeline qmd; `workflows/libs/paths.R` (v8 + S3); new `msens/R/s3.R`; retire `workflows/R/run_*.R` + stale `_targets/`.
- Port the CalCOFI generator into **`msens`** as documented functions: `parse_qmd_frontmatter(dir)` +
  `build_targets_list(exclude=)` reading a **`msens:`** frontmatter key; emit
  `tar_target_raw(target_name,{<dep symbols>; quarto::quarto_render(qmd); <output>}, format="file")`.
- Add to each pipeline qmd a block (leave `explore_*`/`investigate_*` without one so they're excluded):
  ```yaml
  msens:
    target_name: ingest_birdlife_botw
    workflow_type: ingest        # grid | ingest | merge | score | publish | release | test
    dependency: [build_hex_grid, ingest_taxon]
    output: data/manifests/ingest_birdlife_botw.json
    dataset: {ds_key: bl, response_type: range, source_authority: BirdLife,
              temporal_interval: static, native_format: vector}   # single source for STAC + report
  ```
- `_targets.R` → `library(targets); library(msens); tar_option_set(...); build_targets_list()`.
- `paths.R`: `ver<-"v8"`, `ver_prev<-"v7"`; `s3_atlas<-"s3://oceanmetrics.io-public/marine-atlas"`,
  `dir_atlas_v<-glue("{s3_atlas}/{ver}")`; `msens/R/s3.R::attach_atlas(con)` = `INSTALL/LOAD httpfs`,
  S3 creds, and CREATE VIEWs over the Parquet; keep a local mirror for fast tile reads.

**Verify:** `tar_manifest()` lists every pipeline qmd; `tar_visnetwork()` shows grid→ingest→merge→score→publish→
release→test; `read_parquet('s3://oceanmetrics.io-public/marine-atlas/…')` round-trips via DuckDB httpfs.

## Phase 1 — H3 res-7 hex grid + membership + env covariates + validation harness

**Files:** new `workflows/build_hex_grid.qmd`; `msens/R/calc.R` (new `hexes_in_polygon()`, `hex_id_from_lonlat()`,
`hex_grid_ocean()`; deprecate `cell_id_raster`/`cells_in_polygon`); `msens/R/validate.R` + `workflows/validate_v7_v8.qmd`
(the harness above); `msens/tests/`.
- Build the **global ocean hex set** via the DuckDB `h3` extension: `h3_latlng_to_cell(lat,lng,7)` over Bio-Oracle
  ocean-cell centroids, `DISTINCT` → ocean hexes. Table `hex(hex_id, lon, lat, area_km2 = h3_cell_area(hex_id,'km^2'),
  in_usa, in_pra, <env covariates resampled onto hex centroids>)`.
- `hexes_in_polygon(poly)` → `(hex_id, pct_covered)` (hex analog of `cells_in_polygon`), for vector interpolation +
  zone membership. All H3 ops via the DuckDB extension; `h3jsr` only where a boundary polygon is needed in R.

**Verify:** global ocean hex count sane; `sum(in_usa)` a few hundred k; a known lon/lat → expected `hex_id` == the
OBIS store's id for that point; the validation harness runs (even if trivially, pre-migration).

## Phase 2 — publish ORIGINAL models natively (PMTiles / COG / hex-parquet) + stac-sdm v1.1.0

**Files:** each `ingest_*.qmd` (native-publish step); new `msens/R/publish.R`
(`publish_pmtiles()`, `publish_cog()`, `publish_hexparquet()`); `msens/R/stac.R` (per-representation assets +
`sdm:h3_resolution`); `stac-sdm/{examples,json-schema,CHANGELOG.md,package.json}`.
- Per model emit native rep to `marine-atlas/{ver}/native/…` + serving mirror: vector ranges/CH → **PMTiles**
  (`tippecanoe`); rasters → **COG**; native-H3 → **parquet(hex_id)**. Functions land in `msens` (roxygen + `@concept`).
- Extend `stac_build()` so each dataset Item carries the native asset (PMTiles `application/vnd.pmtiles` / COG /
  hex-parquet with `sdm:h3_resolution:7`) + the interpolated hex asset + h3t link; replace dead pg_tileserv `.pbf`
  links. Add stac-sdm example items for all three geometry types (fills the H3 gap); bump to **v1.1.0**; `npm test`;
  tagged gh-pages release.

**Verify:** `stac-validator` passes for one PMTiles, one COG, one hex-parquet item; the mapper toggles the original
layer against the interpolated hex layer for a species.

## Phase 3 — global ingest → interpolate onto hexes → per-dataset Parquet on S3

**Files:** `ingest_aquamaps_res05.qmd`, `ingest_birdlife.org_botw.qmd`,
`ingest_fisheries.noaa.gov_critical-habitat.qmd`, `ingest_fws.gov_*`, `ingest_nmfs_core-areas.qmd`,
`ingest_turtles-swot-dps.qmd`, `merge_models_prep.qmd`; new `msens/R/interp.R`.
- Each ingest interpolates onto res-7 hexes → `(mdl_seq, hex_id, value)` → **partitioned Parquet** `dist/dataset=<key>/…`
  on S3 (no US clip for global datasets; **remove** the "drop species outside US EEZ" backfill). Raster sources →
  zonal mean per hex; vector/range sources → `hexes_in_polygon()` area-weighted coverage. Interpolation helpers →
  `msens/R/interp.R`. US-only datasets stay US-extent, same schema, `extent` tagged on `dataset`.

**Verify:** per-dataset Parquet counts; 3 extra-US species have global hexes and zero `in_usa`; `mass_conservation()`
passes for a raster source over a test region.

## Phase 4 — gm + nc onto hexes (Plan B, hex edition)

**Files:** `workflows/ingest_sdm-gm.qmd` (swap PostGIS writes for hex writes), `workflows/ingest_sdm-nc.qmd`
(enable DuckDB writes, resample seasonal COGs onto hexes).
- **gm**: re-sync NOAA SEFSC hexagon shapefiles (blocker); keep source-read + `get_annual_density()`; write
  `dataset`(monthly), `model` per (taxon×month) `2019-{MM}/P1M` + annual `2019`; `model_hex` = each month's density
  (#/km²) onto Gulf hexes. **nc**: `dataset`(seasonal), `model` per (taxon×season) `YYYY-MM-01/P3M` + annual;
  `model_hex` = each season COG resampled onto hexes; keep per-season COG assets on STAC + add interpolated hex; drop
  CSV-only path once verified.

**Verify:** gm 12 monthly (+annual)/taxon renders over the Gulf via h3t; nc 4 seasons (+annual); `mass_conservation()`
sanity per taxon/interval.

## Phase 5 — global merge + 3 validity flags + range/endemism (split from the monolith)

**Files:** split `workflows/merge_models.qmd` into `merge_models.qmd` (the merge), new `merge_flags.qmd`
(3 validity flags), new `merge_range.qmd` (range/endemism) — each a targets node with declared deps; `schema.qmd` doc;
merge logic → `msens/R/merge.R` where reusable.
- Merge per-dataset Parquet → merged distributions Parquet (one merged model per `mdl_seq`; AquaMaps/IUCN-mask on
  hexes; confirm density-response handling for gm/nc so densities don't dominate `ms_merge`).
- `is_valid_global` (gates ∧ merged hexes anywhere) ⊇ `is_valid_usa` (∧ `hex.in_usa`) ⊇ `is_valid_pra` (∧ `hex.in_pra`);
  migrate readers; deprecated `is_ok` view = `is_valid_usa`. `range_size_km2` = Σ `hex.area_km2` over occupied hexes +
  rarity class, on `taxon`.

**Verify:** `is_valid_global ≫ is_valid_usa (~16k) > is_valid_pra (~9,230)`; range size sane.

## Phase 6 — calc_scores on hexes (US-only), split into logical notebooks

**Files:** split `workflows/calc_scores.qmd` (~130 KB) into targets-wired notebooks, e.g.
`score_hex_metrics.qmd` (redlist + extrisk-per-sp_cat base metrics), `score_ecoregion_rescale.qmd`
(ecoregion min/max rescale), `score_zone_metrics.qmd` (hex→zone: PRA/subregion/ecoregion/FULL aggregation +
`backfill`/`add_full_studyarea_zone`), `score_composite.qmd` (composite score + weights + metric_labels),
`score_zone_taxon.qmd` (per-species contributions). Scoring helpers → `msens/R/score.R`.
- Scoring runs over `hex.in_usa` reading merged Parquet filtered to US hexes; **ecoregion rescale = v7 algorithm
  unchanged** on hex env covariates; FULL zone via `hexes_in_polygon(ply_boem-usa.gpkg)`; `zone_taxon` extended to
  ecoregions.

**Verify:** `validate_v7_v8.qmd` PRA composite delta within tolerance (grid change → bounded, not zero); outliers
explained. Each split notebook is an independent green targets node.

## Phase 7 — release: partitioned Parquet → S3 marine-atlas + STAC catalog

**Files:** new `workflows/release_marine-atlas.qmd` (`workflow_type: release`, `dependency:[auto]`);
`msens/R/stac.R` (`stac_cfg` hosts); STAC/deploy chunks.
- Write versioned Parquet tree to `s3://oceanmetrics.io-public/marine-atlas/v8/`: `dist/`, `dist_merged/`, derived
  tables (`hex`, `taxon`, `model`, `dataset`, `hex_metric`, `zone*`, `metric`) as partitioned Parquet + manifest —
  **replaces** the single-DuckDB release (optionally ship a small derived `.duckdb` that only attaches the Parquet).
- STAC assets point at marine-atlas Parquet + native PMTiles/COG/hex-parquet + h3t links.

**Verify:** fresh DuckDB + titiler/h3t containers read the S3 Parquet; STAC hrefs resolve; catalog validates.

## Phase 8 — serving: h3t hex tiles (scores + species + OBIS) + land overlay

**Files:** `server/h3t/` config (add `msens:` DB to `H3T_DBS`); `server/docker-compose.yml`/`Caddyfile`; new land
PMTiles; `msens/R/viz.R` (`add_h3t_source` helpers + `add_land_layer()`).
- Point h3t at the msens hex store (or a DuckDB attaching the marine-atlas Parquet); serve score/species surfaces as
  `SELECT hex_id AS cell_id, value …` (h3t contract); reuse res-1..7 tiling + Varnish `release=` cache-bust. OBIS
  overlay = existing `obis` DB, same `hex_id`. Retire the titiler cell-id COG scores path (keep titiler for native COG
  assets).
- **Land layer**: build a Natural Earth land **PMTiles** and draw it as a fill **above** the hex layer in both apps.

**Verify:** a US score hex tile + a global species hex tile render via h3t; OBIS + SDM hexes align on shared `hex_id`;
land layer delineates coast.

## Phase 9 — apps (mapsp global, mapgl US) + API / report / docs

**Files:** `apps/mapsp/app.R` (global default, `add_h3t_source()`, filter `is_valid_global`, range/rarity,
original-vs-interpolated toggle), `apps/mapgl/app.R` (US default, hex_metric via h3t, `is_valid_usa`/`_pra`),
`api/plumber.R` `/stats.json`, `docs/stats.R` + `docs/*.qmd`, `msens/R/calc.R::species_for_cells`→`species_for_hexes`.
- Reuse `apps/h3-db/app.R`'s zoom→res / stats / proxy-update patterns. `species_for_hexes()` filters `is_valid_global`
  then inner-joins supplied hexes (US AOI → only US species). `/stats.json`: `valid_species_{global,usa,pra}`,
  `n_hexes`, range/endemism; cross-version `?ver=` (global fields NA pre-v8).

**Verify:** mapsp shows an extra-US species worldwide at res 7; mapgl defaults US, selectors + flower plots match v7
within tolerance; `/stats.json?ver=v8` serves the counts.

## Phase 10 — CLAUDE.md rewrite + `.claude/skills/` for SDM datasets

**Files:** replace symlink `workflows/CLAUDE.md` with a real file; new `workflows/.claude/skills/`.
- **`workflows/CLAUDE.md`** — mirror `../../CalCOFI/workflows/CLAUDE.md`: reference the parent `../CLAUDE.md` for
  general R/Quarto style; describe the v8 pipeline (H3 hex grid, targets+`msens:` metadata, S3 marine-atlas release,
  h3t serving, native publishing, validation-first); list sibling repos up one folder (`../msens`, `../server`,
  `../apps`, `../stac-sdm`, `../api`, `../docs`, `../MarineSensitivity.github.io`); targets-first commands
  (`tar_make`, `tar_visnetwork`, `tar_outdated`, `tar_make("validate_v7_v8")`); conventions + gotchas.
- **`workflows/.claude/skills/`** — mirror the CalCOFI set for SDMs (RUNBOOK + user-invocable skills + templates):
  - `RUNBOOK.md` — the loop: `/characterize-sdm → /generate-sdm-metadata → /ingest-sdm → run → /validate-sdm → release_marine-atlas.qmd`.
  - `characterize-sdm.md` — profile a new SDM source: geometry (vector/raster/H3), `sdm:response_type`, taxa/authority,
    temporal interval, coverage, native format → recommend PMTiles/COG/hex-parquet publish + interpolation approach;
    seed a questions file.
  - `generate-sdm-metadata.md` — dataset registry row + `sdm:` STAC props + taxa crosswalk (WoRMS).
  - `ingest-sdm.md` — scaffold `ingest_{source}.qmd` from `templates/ingest_sdm_template.qmd`: native publish +
    hex interpolation + `dataset`/`model`/`model_hex` writes + `msens:` metadata block (auto-wires targets).
  - `validate-sdm.md` — PK/FK/coverage + hex/grid alignment + `mass_conservation()` + the **v7↔v8 PRA equivalence**
    gate (`tar_make("validate_v7_v8")`).
  - `publish-sdm.md` — native-format publish + stac-sdm item.
  - `templates/ingest_sdm_template.qmd` — the production SDM ingest template.

**Verify:** `workflows/CLAUDE.md` renders the pipeline accurately and links resolve; invoking `/characterize-sdm` on a
sample source produces a correct profile; the skills loop scaffolds a runnable ingest node.

## Phase 11 — safe build & rollout

**Files:** `workflows/dev/build_v8.R` (thin `tar_make()` + validation wrapper), `dev/msens_notes.md`, snapshot `apps_v7`.
1. **Prototype end-to-end on ONE region / a handful of species** (hex grid → interpolate → Parquet/S3 → merge →
   range → h3t tile → US score → `validate_v7_v8`) before the multi-hour global build.
2. Full global build via `targets::tar_make()`; validation gate; smoke-test `/stats.json`, mapgl, mapsp, h3t.
3. Snapshot `apps_v7` before cutting `latest`/apps to v8.

---

## Critical files (summary)
- **New in `msens`** (documented, `@concept`, with `tests/`): `R/workflow.R` (targets generator), `R/s3.R`,
  `R/validate.R`, `R/interp.R`, `R/publish.R`, `R/merge.R`, `R/score.R`; extend `R/calc.R` (hex helpers),
  `R/stac.R` (native assets + `sdm:h3_resolution`), `R/viz.R` (h3t + land layer).
- **New in `workflows`**: `build_hex_grid.qmd`, `validate_v7_v8.qmd`, `release_marine-atlas.qmd`, split score
  notebooks (`score_*.qmd`), split merge notebooks (`merge_flags.qmd`, `merge_range.qmd`); rewrite `_targets.R`,
  `libs/paths.R`, `CLAUDE.md`; new `.claude/skills/**`.
- **Pattern edits across files**: every pipeline `.qmd` gets a `msens:` block + (ingests) native-publish + hex
  interpolation — representative paths `ingest_aquamaps_res05.qmd`, `ingest_birdlife.org_botw.qmd`,
  `ingest_sdm-gm.qmd`, `ingest_sdm-nc.qmd`.
- **Infra**: `server/h3t/*`, `server/docker-compose.yml`, `server/caddy/Caddyfile`;
  `stac-sdm/{examples,json-schema,CHANGELOG.md,package.json}` → v1.1.0.
- **Apps/API**: `apps/mapsp/app.R`, `apps/mapgl/app.R`, `api/plumber.R`.

## Reuse (don't reinvent)
- targets+metadata generator → `CalCOFI/calcofi4db/R/workflow.R:655-848`; `_targets.R` shape from `CalCOFI/workflows/_targets.R`.
- H3 (primary) → DuckDB `h3` community ext (`h3_latlng_to_cell`, `h3_cell_area`, `h3_cell_to_lng/lat`) as in
  `server/h3t/app/{db,h3t_query}.py`; `h3jsr` fallback for R-side boundaries.
- hex tile serving → `server/h3t` + `mapgl::add_h3t_source()`; port `apps/h3-db/app.R` zoom→res/stats/proxy patterns.
- OBIS store → `obisindicators::build_obis_h3_duckdb()` / `obis_h3t_sql()` (`marinebon/obisindicators`).
- merge/density/masking → existing `merge_models*.qmd` chunks; `get_annual_density()` (`ingest_sdm-gm.qmd:85`).
- STAC → `msens::stac_build()`, `stac_model_cell_item()`, `stac_dataset_collection()`, `stac_season_cog_item()`,
  `sdm_sql_b64()` (`msens/R/stac.R`).
- CLAUDE.md + skills → `CalCOFI/workflows/CLAUDE.md` and `CalCOFI/workflows/.claude/skills/*` as templates.

## Risks / watch-items
- **Scope**: effectively a full pipeline rewrite — the Phase-11 prototype-first gate + the continuous
  `validate_v7_v8` gate are the guardrails against late failure.
- **Global res-7 volume**: tens of millions of ocean hexes; merged global distributions are large even as Parquet —
  measure on the prototype; keep US-scoring tables lean; partition for per-`mdl_seq` pushdown (bucket by sp_cat/hash,
  avoid ~16k tiny partitions).
- **Grid change vs v7**: cell→hex → PRA delta is bounded, not zero; the validation harness sets the tolerance and
  must pass; explain outliers.
- **New S3/httpfs credentials** in R, DuckDB, the h3t + titiler containers; a local Parquet mirror likely needed so
  per-tile hex reads aren't network-bound.
- **h3t multi-DB serving** for the msens store (SQL contract, res clamp ≤7, Varnish `release=`) must be validated.
- **PMTiles tooling** (tippecanoe) + land layer is new to stand up.
- **stac-sdm H3 asset** is aspirational — Phase 2 ratifies it (example + v1.1.0).
- **CLAUDE.md symlink**: replacing it changes shared personal guidance for this repo only — confirm the parent
  `../CLAUDE.md` still carries the general conventions it references.

## End-to-end verification
- `tar_visnetwork()` DAG matches grid→ingest→merge→score→publish→release→test; `tar_make()` builds; `msens` `tests/` pass.
- `validate_v7_v8` PRA composite delta within tolerance on the common-input subset.
- Parquet marine-atlas tree round-trips from S3 via DuckDB httpfs, h3t, titiler.
- `is_valid_global ≫ is_valid_usa (~16k) > is_valid_pra (~9,230)`; range size sane.
- mapsp renders any global species worldwide at res 7; original PMTiles/COG/hex overlays toggle vs interpolated hex;
  mapgl reproduces v7 US numbers within tolerance; A-vs-B side by side.
- OBIS hexes overlay exactly on shared `hex_id`; land layer delineates coast.
- stac-sdm v1.1.0 validates for all three geometry types; catalog serves marine-atlas assets.
- `workflows/CLAUDE.md` + `.claude/skills/` present and accurate; the SDM ingest loop scaffolds a runnable node.
- Prototype-region run is green before the full global build.
```
