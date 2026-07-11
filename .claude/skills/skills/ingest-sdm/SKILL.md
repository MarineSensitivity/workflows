---
name: ingest-sdm
description: Add a new SDM source to the v8 Marine Atlas pipeline — write an ingest_{provider}_{dataset}.qmd that rasterizes onto the global 0.05° cell grid and emits partitioned Parquet keyed by mdl_key. Use when adding/updating a species-distribution data source (ranges, suitability, density) in MarineSensitivity/workflows.
---

# Ingest a new SDM source

Create `workflows/ingest_{provider}_{dataset}.qmd`. Mirror an existing one:
`ingest_birdlife.org_botw.qmd` (vector ranges → cells) or `ingest_aquamaps.qmd` (raster
suitability → cells). Steps:

## 1. `msens:` front-matter (the single source of truth)

```yaml
msens:
  target_name: ingest_{ds}
  workflow_type: ingest
  dependency: [build_cell_grid]
  output: data/manifests/ingest_{ds}.json
  dataset: {ds_key: {ds}, response_type: range|suitability|density, source_authority: "...",
            temporal_interval: static|monthly|seasonal, native_format: vector|raster}
```
The `dataset:` block is consumed by `build_registry` → the `dataset` table. **Do NOT write a
`dataset`/`model` table inline** (v7 did; v8 consolidates once — that per-ingest write is the
gap that silently disappeared).

## 2. Rasterize onto the global grid → `(cell_id, val)`

- Load `libs/paths.R` (`cellid_tif`, `dir_big_v`) + `libs/vars.R` (`redo_ingest`).
- **Vector** ranges → `msens::cells_from_ranges(polys, cellid_tif, value = <er>)` (exactextractr,
  tolerant of messy geometry; set `Sys.setenv(OGR_STROKE_CURVE="TRUE")` for curved MULTISURFACE).
- **Raster** → `msens::cells_from_raster(rast, cellid_tif, method="bilinear", ...)`.
- **Whole range, NO land mask** (esp. birds — capture the entire global range; `pct_marine` is
  derived at merge, never a mask here).
- **`val` = the source ER** via `compute_er_score("IUCN:{code}")` etc. — the ingest carries the
  dataset's OWN ER; the governing/floored ER is applied later at merge. Never hard-code values.
- `value` is a DuckDB reserved word → the output column is **`val`**.

## 3. Write outputs

- One Parquet per model → `{dir_big_v}/marine-atlas/dist/dataset={ds}/{sp_id}.parquet` with
  `(mdl_key, cell_id, val)`; `mdl_key = msens::mdl_key_raw(ds_key, sp_id)`.
- `model_{ds}.csv` next to `dist/` with per-model metadata incl. `mdl_key` + a scientific-name
  column (`build_registry` unions these into the `model` table).
- Resumable: skip species whose Parquet exists; `REDO_INGEST=1` rebuilds.
- Parallelize with `parallel::mclapply` (fork) or `furrr` (per-worker read-only DuckDB con).

## 4. Verify + manifest

Count models/cells; write `data/manifests/ingest_{ds}.json`. Run headless:
`Rscript -e 'library(knitr); purl("ingest_{ds}.qmd","/tmp/x.R",quiet=T,documentation=0); source("/tmp/x.R")'`

Then the crosswalk (`merge_models_prep`) picks it up via `model_{ds}.csv` + the Parquet; no other
wiring needed. For density sources (gm/nc), convert #/km² → suitability [0,100] before writing.
