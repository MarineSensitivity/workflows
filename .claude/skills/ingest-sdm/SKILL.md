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
   `msens:` YAML block drives the `targets` DAG.

## Per-format recipe

- **Vector ranges** (IUCN/BOTW/FWS/NMFS polygons): `msens::cells_from_ranges(sf, cellid_tif,
  value = compute_er_score(er_code))` — exactextractr coverage, whole range, robust to
  s2 geometry "funk". Filter to extant/marine per the source (IUCN `marine=true`,
  BOTW `presence ∈ {1,2,3}`). Union a species' polygons; `cover=TRUE` for edge weighting.
- **Raster SDMs** on another grid (NCCOS COGs): `msens::cells_from_raster(rast, cellid_tif)`
  — zero-fill + bilinear resample; source values define coverage.
- **AquaMaps 0.5°**: a precomputed **bilinear-weight JOIN in DuckDB** (`ingest_aquamaps.qmd`),
  ocean-only `w05` weight table — NOT terra (I/O-bound). ~50× faster.

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
