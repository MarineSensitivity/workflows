---
name: publish-sdm
description: Release the MarineSensitivity marine-atlas to S3 and stand up its serving (view-DB over S3 Parquet + parallel titiler + STAC). Use when publishing a v8 data release or deploying/updating the tile serving.
---

# Publish + serve the marine-atlas

Everything is reproducible via **`release_marine-atlas.qmd`** — do NOT hand-run ad-hoc scripts
(see feedback_reproducible_by_default). It stages tables + the serving surface, syncs to S3,
builds the view DB, emits STAC, and (gated) deploys.

## Release (data → S3)

```r
Rscript -e 'library(knitr); purl("release_marine-atlas.qmd","/tmp/x.R",quiet=T,documentation=0); source("/tmp/x.R")'
```
Produces `s3://oceanmetrics.io-public/marine-atlas/{ver}/`: `tables/` (cell, taxon, dataset,
model, cell_metric, zone*, metric), `dist_merged/`, `registry/`, and **`serve/model_cell.parquet`**
— ONE file **sorted by mdl_key** (`COPY (… ORDER BY mdl_key)`, ROW_GROUP_SIZE 100000) so a titiler
tile is an HTTP-range point read; single file ⇒ anonymous GET (no LIST/creds). Flags: `RELEASE_NO_S3=1`
(stage only), `RELEASE_RAW=1` (also push the ~74G raw `dist/`).

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

## Deploy (gated: `RELEASE_DEPLOY=1`)

The notebook rsyncs the KB view DB + STAC subtree to `msens1`, (re)builds the **parallel** `titiler-{ver}`
service, restarts caddy, smoke-tests. Manual equivalent:
```
ssh msens; cd /share/github/MarineSensitivity/server; git pull
docker compose up -d --build titiler-v8; docker compose restart caddy   # caddy restart needed for new routes
```
titiler-v8 (`titiler-v8.marinesensitivity.org`, port 8001) runs parallel to v7 (A/B). The factory
(`../server/titiler/factory.py`) is env-driven (`MSENS_DUCKDB`=view DB, `MSENS_CELLID_COG`=global COG)
and its SQL validator **blocks `read_parquet` in client SQL** — the view expands server-side, so
clients send `SELECT cell_id, val AS value FROM model_cell WHERE mdl_key='…'`.

## STAC

`msens::stac_build(version="v8", cfg=…)` (override `cfg$titiler_base` → titiler-v8). Deploy the
`{ver}/` subtree to `/share/data/derived/stac/{ver}/` and add a child link to the root
`catalog.json` (keep other versions). Source `../msens/R/stac.R` — the installed pkg may be stale.
