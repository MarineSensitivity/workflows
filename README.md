# workflows

Data-ingest, merge, scoring and release workflows for **MarineSensitivity v8
("Marine Atlas")**. Notebooks are `targets`-orchestrated (see `_targets.R`, built
from each notebook's `msens:` YAML block) and write a partitioned-Parquet release
to `s3://oceanmetrics.io-public/marine-atlas/`.

## Spatial unit

The **global 0.05° raster cell** in `[-180,180]` (Bio-Oracle v3 / AquaX topology,
3600×7200). `cell_id = 1:ncell` for **every** cell (land + ocean), from
`build_cell_grid.qmd`'s cell-id COG (`cellid_tif`). See `../msens/R/` and
`libs/paths.R`.

## Conventions (do not regress)

- **Whole range, no land mask.** v8 captures each species' *entire* global home
  range — land AND ocean (birds are largely terrestrial). Never mask to the ocean.
  "How marine" is the derived metric `msens::cells_pct_marine()` computed at merge,
  not a filter. Marine-only datasets (AquaMaps) stay marine via their *source*
  (e.g. the `w05` weight table is built from ocean cells), not a grid mask.

- **Range value = extinction-risk score, never hard-coded.** A range/critical-habitat
  cell's `value` is `msens::compute_er_score("<authority>:<code>")` from the species'
  status, NOT a magic number:
  - `NMFS:` / `FWS:` — `EN` = 100, `TN` = 50, `LC` = 1 (US ESA; MMPA +20 / MBTA +10 floors)
  - `IUCN:` — `CR` = 50, `EN` = 25, `VU` = 5, `NT` = 2, `LC`/`DD` = 1
  Each ingest maps its source status to that vocabulary (ch_nmfs ESA status → `NMFS:*`,
  BOTW Red List category → `IUCN:*`, turtles EN-DPS/TN → `NMFS:EN`/`NMFS:TN`).
  Suitability (AquaMaps 1–100) and density (gm/nc, rescaling TBD) keep their own scales.

- **`mdl_key` = `{dataset_key}|{sp_id}[|{interval}]`** — the stable public model id
  (`msens::mdl_key_raw()` / `mdl_key_merged()`), pipe-separated. Raw = dataset-native
  id (`am|Fis-29291`, `bl|22687170`, `gm|<id>|01`); merged = taxadb-prefixed
  (`ms_merge|WORMS:137209`). Replaces the old auto-increment `mdl_seq`.

- **Re-run flags** in `libs/vars.R` (env-overridable, e.g. `REDO_INGEST=1`,
  `REDO_AM_INGEST=1`, `REDO_CELL_GRID=1`) gate expensive cached steps so
  `quarto render` / `tar_make` is cheap once the heavy artifacts exist.

- **Every process is baked into a committed QMD and executed by RENDERING to HTML.**
  Ingest, merge, score, publish, **S3 sync, and server + Shiny-app deploy** are all chunks
  in the owning notebook, gated behind env flags (`RELEASE_DEPLOY`, `DEPLOY_APPS`,
  `RELEASE_S3_TABLES`, …) — never ad-hoc `ssh`/`aws`/scratch scripts. Run real work via
  `targets::tar_make()` or `quarto render` so the tracked `_output/*.html` (Design mermaid +
  summary tables, the workflows-page links) is produced; `purl + source` is diagnostics-only
  (it skips the HTML + content-hash checkpoint).

- **Model logic is `msens` functions, guarded by unit tests.** The scientific rules — how
  per-dataset cells merge per taxon (`msens::merge_sql()` / `msens::turtle_sql()`), how ranges
  constrain AquaMaps, how ER is scored — live in `../msens` as the single source of truth; the
  notebooks *call* them. Every rule has a testthat fixture in `../msens/tests/testthat/` asserting
  its exact output (one synthetic taxon per category: range-only, both-masked,
  `iucn_range_outside_us_eez`-excluded, am-only single/multi-model, turtle). **Add or update the
  test in the same change as the logic**, run `devtools::test("../msens")`, reinstall msens, and
  never let a red test through. This catches rule-level breakage the aggregate `pra_score_delta`
  gate can hide (the 750 no_eez over-predictions had near-zero score impact yet were wrong).

## Ingest → `model_cell` (cell_id, value) by native format

- vector ranges → `msens::cells_from_ranges()` (exactextractr; whole range)
- raster SDMs → `msens::cells_from_raster()` (zero-fill + resample)
- AquaMaps 0.5° → a bilinear-weight DuckDB join (`ingest_aquamaps.qmd`, ocean-only `w05`)

Each writes one Parquet per model to `dist/dataset=<ds_key>/`, resumable, keyed by
`mdl_key`.
