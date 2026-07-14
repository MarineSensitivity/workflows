---
name: publish-sdm
description: Release the MarineSensitivity marine-atlas to S3 and stand up its serving (view-DB over S3 Parquet + parallel titiler + STAC). Use when publishing a v8 data release or deploying/updating the tile serving.
---

# Publish + serve the marine-atlas

**Every process is baked into `release_marine-atlas.qmd` and run by RENDERING to HTML** — never
ad-hoc `ssh`/`aws`/scratch scripts, and never `purl + source` (which executes chunks but SKIPS the
tracked `_output/*.html` + content-hash checkpoint; diagnostics only). See
feedback_reproducible_by_default. The notebook stages tables + the serving surface, syncs to S3,
builds the view DB, emits STAC, and (gated) deploys the server + the v8 Shiny apps.

## Release (data → S3)

```r
quarto render release_marine-atlas.qmd            # or: Rscript -e 'targets::tar_make(release_marine_atlas)'
```
Produces `s3://oceanmetrics.io-public/marine-atlas/{ver}/`: `tables/` (cell, taxon, dataset, model,
cell_metric, zone*, metric, native_asset), `dist_merged/`, `registry/`, and **`serve/model_cell/`** —
Hive-**partitioned by the integer `mdl_id`** (dense_rank over mdl_key; stored rows are cell_id,val) so a
titiler tile is an exact-partition point read. The STABLE public key stays **`mdl_key`** (the factory
resolves mdl_key→mdl_id). Flags: `RELEASE_NO_S3=1` (stage only), `RELEASE_S3_TABLES=1` (push `tables/`
incl `native_asset` WITHOUT the serve cutover), `RELEASE_REDO_SERVE=1` (re-partition when the model set
changed — e.g. after a merge/crosswalk change renumbers mdl_id), `RELEASE_RAW=1` (also push raw `dist/`).

## Read it back (any consumer)

`con <- msens::attach_atlas(version="v8")` then `msens::atlas_tbl(con, "taxon")`. **Gotcha:** the
dotted bucket `oceanmetrics.io-public` breaks virtual-hosted TLS → path-style required; globbing
partitioned Parquet needs the `aws` extension + credential chain (single files read anonymously).
`attach_atlas` encodes this.

## Serving = tiny view-DB over S3 (never a multi-GB DB)

`release_marine-atlas.qmd` builds `serve.duckdb` (KB): `CREATE VIEW model_cell/cell/taxon/… AS
SELECT * FROM read_parquet('https://s3.us-east-1.amazonaws.com/…')` — explicit **path-style HTTPS**
single-file URLs (never `s3://`, which trips the vhost-TLS bug). Adding a dataset / re-scoring never
rsyncs the big data — only the S3 Parquet changes. Intra-region (EC2+bucket us-east-1): ~0.13s warm
SQL, ~0.07s tile; if cold reads ever exceed 1–2s, fall back to a local `/share` Parquet copy.

## Deploy (gated — all in-notebook chunks, no ad-hoc ssh)

- **`RELEASE_DEPLOY=1`** — the notebook rsyncs the KB view DB + STAC subtree to `msens1`, `git pull`s
  the server repo and (re)builds the **parallel** `titiler-{ver}` service (picks up the mdl_key→mdl_id
  factory), restarts caddy, smoke-tests.
- **`DEPLOY_APPS=1`** (also implied by `RELEASE_DEPLOY`) — the `deploy-apps` chunk pulls the `apps_v8`
  checkout (`MarineSensitivity/apps@main`, symlinked `/share/shiny_apps/{species,scores}_v8`) and reloads
  ONLY the v8 apps via Shiny Server `restart.txt`. The v7 apps are a separate checkout in the same
  container and are **not** restarted — no container bounce, v7 uninterrupted.

titiler-v8 (`titiler-v8.marinesensitivity.org`, port 8001) runs parallel to v7 (A/B). The factory
(`../server/titiler/factory.py`) is env-driven (`MSENS_DUCKDB`=view DB, `MSENS_CELLID_COG`=global COG)
and its SQL validator **blocks `read_parquet` in client SQL** — the view expands server-side, so
clients send `SELECT cell_id, val AS value FROM model_cell WHERE mdl_key='…'`.

## STAC

`msens::stac_build(version="v8", cfg=…)` (override `cfg$titiler_base` → titiler-v8). Deploy the
`{ver}/` subtree to `/share/data/derived/stac/{ver}/` and add a child link to the root
`catalog.json` (keep other versions). Source `../msens/R/stac.R` — the installed pkg may be stale.
