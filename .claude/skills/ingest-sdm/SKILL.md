---
name: ingest-sdm
description: Add or update a species-distribution-model (SDM) ingest in the MarineSensitivity v8 pipeline — turn a source distribution into partitioned model_cell Parquet on the global 0.05° cell grid. Use when wiring a new dataset (vector ranges, raster SDMs, AquaMaps-style) into workflows/ingest_*.qmd.
---

# Ingest an SDM into MarineSensitivity v8

Each `ingest_*.qmd` turns one dataset's per-species models into `model_cell`-shaped
`(mdl_key, cell_id, value)` rows on the **global 0.05° cell grid**, written as one
Parquet per model to `dist/dataset=<ds_key>/`. The merge (`merge_models_prep` →
`merge_models` → `merge_taxon`) combines them per taxon.

## Non-negotiable conventions (do not regress)

1. **Whole range, no land mask.** Capture each species' entire global home range —
   land AND ocean (birds are largely terrestrial). The cell grid (`cellid_tif`) is
   GLOBAL: `cell_id = 1:ncell` for every cell. "How marine" is the derived metric
   `msens::cells_pct_marine()` computed at merge, never a filter. Marine-only datasets
   stay marine via their SOURCE (AquaMaps `w05` is built from ocean cells), not a grid mask.
   See memory `feedback_whole_range_no_mask`.

2. **Value = `msens::compute_er_score("<authority>:<code>")`, NEVER hard-coded**, for
   *range* datasets — the species' extinction-risk score from its status:
   - `NMFS:`/`FWS:` EN=100, TN=50, LC=1 (US ESA; +MMPA 20 / +MBTA 10 floors)
   - `IUCN:` CR=50, EN=25, VU=5, NT=2, LC/DD=1
   Map the source status to that vocabulary (ESA "Endangered"→`NMFS:EN`, IUCN category
   →`IUCN:{cat}`). AquaMaps keeps suitability 1–100; density (gm/nc) keeps its own scale
   (rescaling TBD). See `feedback_whole_range_no_mask`, `project_extinction_risk_coding`.

3. **`mdl_key = "{ds_key}|{sp_id}[|{interval}]"`** via `msens::mdl_key_raw()` — pipe
   separator, dataset-native `sp_id`. Monthly/seasonal datasets add `|{interval}`.

4. **Clean runnable notebook** (all chunks `eval:true`, no awk-extraction), **resumable**
   (skip models whose Parquet exists), gated by a `libs/vars.R` flag (`REDO_INGEST`, etc.).
   `msens:` YAML block drives the `targets` DAG — and for a NEW dataset the `dataset:` block must
   also carry `name_short`, `name_display`, `description`, `citation`, `link_info`, `value_info`,
   `regions`, `is_mask` (there is no `ver_prev` row to inherit them from; `build_registry.qmd` reads
   them). A smoke-test flag (`AX_TEST_N`) must write nothing to `data/` and no manifest.

## Per-format recipe

- **Vector ranges** (IUCN/BOTW/FWS/NMFS polygons): `msens::cells_from_ranges(sf, cellid_tif,
  value = compute_er_score(er_code))` — exactextractr coverage, whole range, robust to
  s2 geometry "funk". Filter to extant/marine per the source (IUCN `marine=true`,
  BOTW `presence ∈ {1,2,3}`). Union a species' polygons; `cover=TRUE` for edge weighting.
- **Raster SDMs** on another grid (NCCOS COGs): `msens::cells_from_raster(rast, cellid_tif)`
  — zero-fill + bilinear resample; source values define coverage.
- **AquaMaps 0.5°**: a precomputed **bilinear-weight JOIN in DuckDB** (`ingest_aquamaps.qmd`),
  ocean-only `w05` weight table — NOT terra (I/O-bound). ~50× faster.
- **Raster ALREADY on the cell grid** (AquaX, Bio-Oracle): `msens::cells_from_aligned_raster(tif,
  cell_ids, scale=)` — reads the cell ids at the source's non-NA pixels (on `global05` the id IS the
  pixel index; assert it once per run, never assume it), scales (`0.1` for AquaX's 0–1000), applies
  the same `≥ 1` threshold as AquaMaps, drops land. Read the 100 MB id raster ONCE per worker and pass
  the vector. Constant per-model bands (AUC/TSS/cutoff) come from one modeled pixel (`r[[2]][px][[1]]`),
  not three more full reads. Pattern: `ingest_aquax.qmd`.

## A dataset that REPLACES part of another (AquaX ⊃ AquaMaps, v9)

When a newer model of the *same quantity* arrives for a subset of taxa and an extent:

1. **Its extent is its own mask, persisted** — `dist/{ds}_mask.parquet` = the union of every
   *modeled* pixel (non-NA in the source), **not** the thresholded Parquet cells (half of AquaX's
   pixels are near-zero suitability and are dropped from the Parquet, yet "modeled absent" must
   supersede too) and **not** `in_usa` (AquaX's ocean mask is its own) and **not any one model**
   (a model's NA area is its biogeographic-range crop: 53,818 `in_usa` cells for the most-covered
   model, ~1k for the union). Accumulate it in the worker loop and OR it across resumed runs.
2. **Which taxa supersede is a committed registry**, `data/{new}_supersedes_{old}.csv`, built from
   `ver_prev`'s published `taxon_model` (never by name — by the native id), with a `supersedes`
   column that encodes the policy flags (`AX_ABSENT_SUPERSEDES`). Assert the headline counts.
3. **Supersession is a filter on the merge INPUT**, `msens::supersede_sql()` where `mc_parts` is
   written (`merge_models.qmd`), never a per-cell coalesce inside `merge_sql()` — a coalesce keeps the
   old model wherever the new one says absent. Then `merge_sql(suit_ds = c("am","ax"))`.
4. **Measure what changed, in the ingest** — per species × subregion on the shared cells (`delta`,
   footprint Jaccard, correlation), the 20 least/most different with preview deep links, and the
   "modeled but absent" list — `data/ax_vs_am_summary.csv`. Reviewers inspect it in the species app.
5. **COGs built in the ingest when native == model grid** (`AX_COG=1`, `AX_COG_S3=1`): native =
   `msens::cog_from_tif()` (bit-exact, cropped, metadata), model = `publish_cog()` from the Parquet
   (`round()` first — INT1U truncates); urls + bbox into `model_{ds}.csv`; `publish_native.qmd`
   *registers* them (both representations), never repaints. **For an on-grid dataset the two
   representations are "as delivered" vs "as ingested"** (scale, integer, the ≥1 threshold —
   what the merge consumes), not original vs interpolated: declare `on_grid: true` in the
   `dataset:` block so the app labels the toggle *Delivered / As ingested*. Keep both — the
   threshold drops ~half of AquaX's pixels and reviewers should see that on the input. A round-trip check MUST assert its sample is non-empty — the first smoke test
   "verified 0 models" while every native COG was missing (GDAL does not expand `~`).

## Crosswalk by native id

A dataset keyed by WoRMS AphiaID declares `worms_id` in `model_{ds}.csv`; `merge_models_prep`
short-circuits name matching for those rows (`is.na(worms_id)` only goes to `match_taxa`). `sp_cat`
for by-component tables *before* a merge exists: `msens::sp_cat_from_taxonomy()`.

## Notebook skeleton

```r
librarian::shelf(arrow, DBI, dplyr, duckdb, fs, glue, here, jsonlite, logger, readr,
                 sf, terra, tibble, MarineSensitivity/msens, quiet = T)
source(here("libs/paths.R")); source(here("libs/vars.R"))
ds_key <- "<key>"; dir_dist <- glue("{dir_big_v}/marine-atlas/dist/dataset={ds_key}")
stopifnot(file_exists(cellid_tif))
# species crosswalk -> model_{ds}.csv (mdl_key, sp_id, scientific_name, er_code, er_score)
# resumable loop: cells_from_ranges/raster -> arrow::write_parquet(tibble(mdl_key,cell_id,value))
# verify (count/min/max) + write data/manifests/ingest_<ds>.json
```

## Terrestrial-heavy sources (FWS, BOTW)

Match v7's species set for comparability (`data/v7_<ds>_spcodes.csv` from the v7
`sdm.duckdb`); marine-relevance is **spatial** (`cell.in_usa` overlap at merge), NOT
`worms_is_marine` — which fails for birds (use BOTW) — and NOT `pct_marine` (sub-pixel
islands confound it). See `feedback_marine_relevance`.

## Verify

- `mdl_key`/`cell_id`(INTEGER)/`value` schema; `cell_id ∈ [1, 25,920,000]`.
- Whole range captured (a terrestrial bird has land cells; `pct_marine` reflects it).
- Range value == `compute_er_score(...)` (not a magic number).
- Runs on the **laptop** (has all sources; the server has only `am.duckdb` — see
  `reference_server_msens1`). Reinstall msens on the server after changes.

## Add a unit test for how the new model MERGES

Ingesting is only half the job — the new dataset changes how a taxon's cells merge (does it constrain
AquaMaps like a range? multiply like turtles? add a new suitability surface?). **Add a fixture to
`../msens/tests/testthat/test-merge.R`** with a synthetic taxon exercising the new dataset's rule and
assert its exact merged output (US + global), so the behavior can't silently break as more models are
added. If the merge needs new logic, put it in `msens::merge_sql()`/`turtle_sql()` (the notebook calls
them) rather than inline SQL. Run `devtools::test("../msens")` before rendering. See the `validate-sdm`
skill for why rule-level tests catch what the aggregate `pra_score_delta` gate hides.

## Spatial extinction risk (sea turtles, NMFS DPS species) — two rules, pick the right one

- **Sea turtles** (`rng_turtle_swot_dps`): `msens::turtle_sql()` — ER × suitability IS the merged
  value; scoring passes 100 through (`taxon.er_mode = 'premultiplied'`).
- **NMFS DPS-listed species** (`dps_nmfs`, `ingest_nmfs-dps.qmd`): `msens::dps_sql()` — the merged
  value is the **suitability** masked to the ER footprint (the distribution), and the per-cell ER is
  written beside it (`dist_merged_er/`) for `score_cell_metrics` to multiply in (`er_mode = 'cell'`).
  Putting such a taxon through the turtle rule collapses its merged surface to ~1 wherever its ER is
  the baseline (the humpback: 99.8 % of its range) — the app then draws the weight, not the species.
- The baseline outside listed entities follows `merge_models_prep`'s convention: a marine mammal is
  `NMFS:LC` + MMPA = 20 (`compute_er_score()` ignores `is_mmpa` for IUCN codes — `IUCN:LC` + MMPA is
  silently 1), anything else its IUCN category.
- A gridded-from-the-start ER dataset needs its COGs painted in `publish_native` (`native/dps_nmfs/`)
  or the species app lists the input struck through (no published surface).
- Re-running only these taxa: `scripts/run_version.sh --spatial --from ingest_dps_nmfs`.

