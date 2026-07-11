# CLAUDE.md

This file guides Claude Code when working in `MarineSensitivity/workflows`.

> General R/Quarto/plumber conventions live in the parent `../CLAUDE.md` (2-space indent,
> snake_case, `|>`, roxygen2, `librarian::shelf()` outside packages, etc.). This file covers
> what is specific to the `workflows` repo and the **v8 "Marine Atlas"** pipeline.

## What this repo does

Ingests marine species distribution models (SDMs) from many sources onto a **global 0.05°
raster cell grid**, merges them per taxon, scores marine sensitivity over the **US study
area**, and publishes the result as **partitioned Parquet on S3** (the "marine-atlas") served
via **titiler** + a **STAC** catalog. The reusable logic lives in the sibling R package
**`msens`** (`../msens`); the notebooks here orchestrate it.

Each source is one `ingest_{provider}_{dataset}.qmd`; the pipeline then runs
`merge_models_prep → merge_models → merge_taxon → score_zones → score_cell_metrics →
score_zone_metrics → build_registry → release_marine-atlas`.

## Commands

```r
# from workflows/ — prefer targets (renders the .qmd + tracks deps) over hand-rendering
Rscript -e 'targets::tar_make()'                    # full pipeline
Rscript -e 'targets::tar_visnetwork()'              # dependency DAG
Rscript -e 'targets::tar_make("merge_taxon")'       # one target

# run a single notebook headless (bypasses targets) — the common dev loop:
Rscript -e 'suppressMessages(library(knitr)); purl("merge_taxon.qmd","/tmp/x.R",quiet=TRUE,documentation=0); source("/tmp/x.R")'
quarto render score_cell_metrics.qmd                # or via quarto

# after editing the msens package, reinstall so library(msens) isn't stale
Rscript -e 'devtools::install("../msens")'          # or source the file directly in a pinch
```

**Env flags** (gate expensive/side-effecting steps): `REDO_INGEST=1` (rebuild an ingest),
`REDO_WORMS=1` (rebuild the worms table), `SCORE_V7COMMON=1` (score only v7's species, for
apples-to-apples), `SCORE_ALLBIRDS=1` (disable the marine-bird cull), `RELEASE_NO_S3=1` /
`RELEASE_RAW=1` / `RELEASE_DEPLOY=1` (release + serving).

No test suite; correctness is enforced by `msens::pra_score_delta` (v7↔v8 gate) and the
`stopifnot`/validation chunks in the notebooks.

## Architecture

### Data flow

```
sources (AquaMaps am, BirdLife bl, IUCN/FWS/NMFS ranges, SWOT turtles, [gm/nc density])
   │  ingest_*.qmd  → cells_from_ranges/raster (exactextractr / bilinear)
   ▼
dist/dataset={ds_key}/*.parquet  (mdl_key, cell_id, val)  +  model_{ds}.csv   [~/_big or /share]
   │  merge_models_prep (crosswalk → taxon, taxon_model, listing, governing er_score)
   │  merge_models      (US-scoped max-merge + turtle ×; range cells carry governing er_score)
   ▼
dist_merged/dataset=ms_merge/*.parquet  +  merge.duckdb (taxon, model_cell)
   │  merge_taxon   (validity flags, range/rarity, is_marine, sp_cat by taxonomy)
   │  score_zones → score_cell_metrics → score_zone_metrics  (v7-faithful; US cells)
   ▼
sdm.duckdb (cell, taxon, model_cell, cell_metric, zone*, metric) + build_registry (dataset, model)
   │  release_marine-atlas.qmd
   ▼
s3://oceanmetrics.io-public/marine-atlas/v8/ {tables/, dist_merged/, serve/, registry/}
   │  + STAC catalog (file.marinesensitivity.org/stac/v8) + titiler-v8 serving
```

### `msens:` YAML-driven targets (don't hand-edit `_targets.R`)

`_targets.R` calls `msens::build_targets_list()`, which parses the `msens:` front-matter of
every `*.qmd` (`target_name`, `workflow_type`, `dependency`, `output`, and for ingests a
`dataset:` block). **Add a dataset by adding the notebook with a `msens:` block** — the
`dataset:` block is the single source of truth the `build_registry` target consolidates into
the `dataset` table (v7 wrote `dataset`/`model` inline per ingest; v8 must NOT — see
`build_registry.qmd`). `dependency: [auto]` depends on all upstream ingests.

### `mdl_key` — the stable model identifier (replaces v7 `mdl_seq`)

`{ds_key}|{sp_id}[|{interval}]`, **pipe separator**. Raw: `am|Fis-29291`, `bl|{sisid}`,
`gm|{id}|01`. Merged: `ms_merge|WORMS:137209` / `ms_merge|BOTW:22694927`. All URLs/serving key
on `mdl_key` (`msens::mdl_key_raw()` / `mdl_key_merged()`). Values live in a `val` column —
**`value` is a DuckDB reserved word**, never use it as a column name.

### Grid, scoring model, key conventions

- **Grid**: global 0.05° `[-180,180]` ocean cells, `cell_id = 1:ncell`, COG `r_cellid_global.tif`;
  `cell` table carries env + `in_usa`/`in_pra`. Scoring runs over `in_usa` (~634k cells).
- **Whole range, no land mask** (esp. birds) — capture the entire global range; `pct_marine`
  is derived, not a mask.
- **`er_score`** (extinction-risk): governing ER computed in `merge_models_prep` (most-protective
  across a taxon's datasets, US-national overrides IUCN); **MMPA floor by taxonomy** (all WoRMS
  Mammalia), **MBTA floor by the FWS CFR 50 §10.13 list** (not all Aves). Applied to range cells
  at merge (the "fitting point"). `compute_er_score()` errors on bad codes.
- **`sp_cat` by taxonomy** (`merge_taxon`): WoRMS class/phylum + BirdLife→bird → bird, mammal,
  turtle, fish, coral, invertebrate, primary_producer; reptile/amphibian EXCLUDED from scoring.
  No `other` bucket. AlgaeBase taxa missing from the WoRMS download are fetched via
  `worrms::wm_record` + cached in `data/worms_taxonomy_supplement.csv`.
- **`is_marine`** cull: birds scored only if in a marine/coastal family AND whole-range
  `pct_marine ≥ 5%`, or curated-in (`data/marine_bird_families.csv` + `marine_birds_curation.csv`).
- **Version equivalence**: on a bump, `msens::pra_score_delta` must show Program-Area scores
  stay ~equivalent on common inputs.

### Release + serving

- **`release_marine-atlas.qmd`** publishes Parquet to S3 (`msens::attach_atlas()` reads it back;
  the dotted bucket `oceanmetrics.io-public` needs **path-style** addressing + the `aws` extension
  for globs). The **serving surface** `serve/model_cell.parquet` is ONE file **sorted by mdl_key**
  (row-group pruning) so a titiler tile is an HTTP-range point read.
- **Serving = a tiny view-only DuckDB** (`serve.duckdb`, KB) whose `model_cell` is a VIEW over the
  S3 Parquet — never a multi-GB DB. `titiler-v8` (parallel to v7) reads it; the factory
  (`../server/titiler/factory.py`) is env-driven and its SQL validator blocks `read_parquet` in
  *client* SQL, so the client sends `SELECT cell_id, val AS value FROM model_cell WHERE mdl_key='…'`.
- **STAC** via `msens::stac_build(version="v8")`. Deploy is gated behind `RELEASE_DEPLOY=1`.

## Where things live

- **`../msens`** (sibling package): `ingest.R` (cells_from_ranges/raster), `calc.R`
  (species_for_cells), `stac.R`, `atlas.R` (attach_atlas), `viz.R` (cell_tile_url/add_cell_tiles),
  `workflow.R` (targets generator), `validate.R` (pra_score_delta). Edit here → reinstall.
- **`libs/paths.R`** — all paths (`ver`, `sdm_db`, `spp_db`, `s3_atlas`, `cellid_tif`, `dir_big`).
  `~/_big/msens/derived` (laptop) ↔ `/share/data/big` (server).
- **`data/`** — curation CSVs (marine birds, worms supplement), manifests, listings.
- **`../server`** — docker-compose (titiler, titiler-v8, caddy), `titiler/factory.py`; deploy via
  `ssh msens; cd /share/github/MarineSensitivity/server; git pull; docker compose up -d …`.
- **`old/`** — archived v7 notebooks (e.g. `calc_scores_v7.qmd`), kept for reference.

## Gotchas

- `value` is reserved → use `val`. `class`/`order` are reserved in DuckDB → quote (`"class"`).
- `library(msens)` can be **stale** — source `../msens/R/*.R` or reinstall after edits.
- Idempotency: notebooks re-run cleanly (e.g. `merge_taxon` drops derived cols first; ingests skip
  done files). Don't rely on per-ingest table writes that can be silently dropped (the v8 registry
  gap) — consolidate in one target.
- **gm/nc density models are NOT yet ingested** (need density #/km² → suitability [0,100]).
