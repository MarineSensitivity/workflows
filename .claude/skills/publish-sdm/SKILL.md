---
name: publish-sdm
description: Release the MarineSensitivity marine-atlas to S3 and stand up its serving (view-DB over S3 Parquet + stock-titiler COGs + STAC + the version registry). Use when publishing a data release or deploying/updating the tile serving.
---

# Publish + serve the marine-atlas

**Every process is baked into `release_marine-atlas.qmd` and run by RENDERING to HTML** — never
ad-hoc `ssh`/`aws`/scratch scripts, and never `purl + source` (which executes chunks but SKIPS the
tracked `_output/*.html` + content-hash checkpoint; diagnostics only). See
feedback_reproducible_by_default. The notebook stages tables + the serving surface, syncs to S3,
builds the view DB, emits STAC, and (gated) deploys the server + the v8 Shiny apps.

## A release is not finished until the registry says so (2026-08)

One app now renders any version, driven by files on S3. A data push that skips these is invisible
to the apps and the docs:

| notebook | publishes |
|---|---|
| `build_zone_sets.qmd` | `data/zone_sets.csv` — spatial units by vintage |
| `build_zone_cells.qmd` | `zones/{zone_set_key}/{grid_id}/zone_cell.parquet` (shared across releases) |
| `publish_score_cogs.qmd` | one COG per (metric × subregion) + `tables/score_cog.parquet` |
| `build_version_manifest.qmd` | `versions.json`, `{ver}/manifest.json`, and (gated) `latest.txt` |
| `publish_storage_index.qmd` | the browsable index behind storage.marinesensitivity.org |

- **`PROMOTE_LATEST=1` is the only thing that changes what users see by default.** Publishing data
  does not. `latest.txt` names the newest `released` version — today **v7**, with v8 a `prerelease`.
- **A suitability-only taxon gets a merged COG only if `publish_native` paints one.** The
  `native/am` alias is sound for exactly one AquaMaps model and nothing else; multi-model taxa and,
  from v9, any taxon with an AquaX model (AquaMaps outside the mask ∪ AquaX inside) are painted
  from the post-supersession `mc_parts` partition. v9's first registry silently had 14,799 → 5,957
  aliases and ~8,800 taxa with no merged surface — compare `native_asset` class counts against
  `ver_prev`'s before releasing.
- **`PUBLISH_MERGED_COG=1` is NOT optional when rebuilding the registry.** Without it `merged_reg`
  is an empty tibble and the bind is guarded on `nrow()`, so a re-render silently DROPS every real
  merged-COG row and leaves only the am-only aliases.
- **Render on the server with `scripts/srv_render.sh`**, never a bare `docker exec` — that runs as
  root and leaves files git cannot touch.
- **Measure coverage against `is_valid_global`**, the filter the species app actually uses; using
  `is_marine` alone overstates gaps badly.

## Starting a new version (v9 was the first same-grid bump) — see `bootstrap-release`

`ver` in `libs/paths.R`, a `data/versions.csv` row (`prerelease`,`restricted`), `grid_for_ver()` in
msens, then `bootstrap_version.qmd` clones the unchanged ingests copy-on-write from `ver_prev`. **No
`titiler-v{n}`**: `titiler-v8` is the stock `/cog` tiler for every release (the apps hardcode it);
`TITILER_SERVICE` names what the release notebook rebuilds. `mdl_id` is assigned fresh for a version
that never shipped. A restricted version mints two Cloudflare Access applications
(`DEPLOY_ACCESS=1` → paste the AUDs into the server `.env` → `DEPLOY_CADDY=1`) and needs
`PREVIEW_REVIEWERS_V{N}`. `latest.txt` is untouched. The docs CI picks the version up from
`versions.json`; the dataset note + release entry are yours to write (see `generate-sdm-metadata`).

## Release (data → S3)

```r
quarto render release_marine-atlas.qmd            # or: Rscript -e 'targets::tar_make(release_marine_atlas)'
```
Produces `s3://oceanmetrics.io-public/marine-atlas/{ver}/`: `tables/` (cell, taxon, dataset, model,
cell_metric, zone*, metric, native_asset, **taxon_model, listing**), `dist_merged/`, `registry/`, and
**`serve/model_cell/`** — Hive-**partitioned by the integer `mdl_id`** (stored rows are cell_id,val) so a
titiler tile is an exact-partition point read. The STABLE public key stays **`mdl_key`**.

**`mdl_id` must never renumber a published model.** It is the serve PARTITION key, so
`msens::assign_mdl_id(mdl_key, published)` reuses the published registry's ids and appends new keys
above the max; `build_registry.qmd` fetches that registry and asserts nothing moved. It was
`dense_rank(mdl_key)`, which is a function of the model SET — ingesting `gm`+`nc` into v8's `dist/`
moved **45,499 of 80,261** ids, and nothing would have failed: the registry and the partitions would
simply disagree, and titiler would serve the wrong species past `ch_nmfs`.

**`dataset.is_scored` separates registered from used.** `gm`/`nc` are ingested but excluded from the
merge, so they contribute to no score; the flag is introspected from `taxon_model`
(`msens::dataset_is_scored()`), never declared, and the docs count datasets by it. Flags: `RELEASE_NO_S3=1` (stage only), `RELEASE_S3_TABLES=1` (push `tables/`
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
- **`DEPLOY_APPS=1`** (also implied by `RELEASE_DEPLOY`) — pulls the `apps_v8` checkout
  (`MarineSensitivity/apps@main`) and reloads it via Shiny Server `restart.txt`. **Since the
  2026-08-12 cutover this IS the live `/scores` + `/species`**, one app rendering every release from
  `?ver=` — so it restarts what everyone sees, not a parallel deployment. The 18 former per-version
  instances are in `/share/shiny_apps_retired/` (moved aside, not deleted) with Caddy 301ing every old
  URL to `/scores/?ver=v{n}`.

titiler-v8 (`titiler-v8.marinesensitivity.org`, port 8001) runs parallel to v7 (A/B).

**The custom `/msens` cells factory is RETIRED but kept** (`MSENS_FACTORY=1`, off by default, and
deliberately NOT behind Varnish). Every app layer now reads a **pre-rendered COG** through stock
`/cog` routes, with the href + build-time rescale coming from the release manifest — nothing reaches
the factory, because every metric on v1–v8 has a COG. Verified equivalent before retiring: 100/105
tiles byte-identical, the rest 3–6 px of 262,144 from float32 storage. Score COGs carry **no
overviews** (the renderer decimates per request, so a pyramid disagrees at low zoom) and their object
key includes the **encoding** (`content_hash_encoded()`) — rewriting bytes at a stable URL left
`/vsicurl` serving a cached header for bytes that no longer existed: fine at z5+, HTTP 500 at z2–z4.

## STAC

`msens::stac_build(version="v8", cfg=…)` (override `cfg$titiler_base` → titiler-v8). Deploy the
`{ver}/` subtree to `/share/data/derived/stac/{ver}/` and add a child link to the root
`catalog.json` (keep other versions). Source `../msens/R/stac.R` — the installed pkg may be stale.
