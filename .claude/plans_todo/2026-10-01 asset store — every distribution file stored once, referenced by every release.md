# Asset store: every distribution file stored once, referenced by every release

Owner decision (Ben, 2026-10-01): the v7 native backfill as planned "is not properly handling non-duplicative use of
cloud-optimized spatial distribution files (raster COGs, vector PMTiles) from source datasets for availability across
different MST release versions, which always have version-specific score results and optionally species merged models
(but even those we might not update between versions so need way of handling non-redundant use)." Steering moves to
the atlas orchestrator session (`atlas-bf`); the release session `migrate-native-assets-s3` executes. The Shiny apps
(`apps/`) are being retired: the Atlas is the only app that must keep working.

Supersedes the "Store" and "Runbook" sections of `atlas-refs/round4-session/c-release-side.md` (its finding and
coverage numbers stand).

## 1. What is true today (measured 2026-10-01, read-only listing)

| prefix | objects | size | what |
| --- | ---: | ---: | --- |
| `cog/usa05/` | 23,382 | 0.64 GB | v1–v7 species + score COGs, content-addressed: the only place "stored once" holds |
| `cog/global05/` | 286 | 0.15 GB | v8+ score COGs only |
| `v8/native/am/` · `v9/native/am/` | 18,710 · 18,715 | 5.85 GB each | AquaMaps gridded to 0.05° ("model") |
| `v8/native/am_native/` · `v9/…` | 18,703 · 18,708 | 0.15 GB each | AquaMaps 0.5° originals |
| `v8/native/pmtiles/` · `v9/…` | 6,776 · 6,776 | 7.09 GB each, identical byte totals | vector-range originals (S3 mirror of the file host) |
| `v8/native/vec_grid/` · `v9/…` | 6,753 · 6,753 | 0.15 GB each | vector ranges gridded ("model") |
| `v8/native/merged/` · `v9/…` | 6,785 · 15,674 | 0.54 · 3.77 GB | merged models |
| `v9/native/ax/` · `ax_native/` · `dps_nmfs/` | 10,527 · 10,536 · 19 | 0.50 · 1.79 GB · 1 MB | AquaX, DPS |
| file host `pmtiles/{v8,v9}/` | 13,552 | (third copy) | what `native_asset` URLs point at; 6,187 `rng_iucn` files are unregistered |

- v8 and v9 each carry a full per-release tree (~20 GB each, mostly identical). The bucket README's promise ("`cog/`
  is shared by every release … a surface that did not change between releases is stored once") is false for v8+.
- v7's registry has one row per model (the gridded COG, mislabelled `native`), no originals.
- The plan in flight would add a THIRD home (`native/{ds}/{md5}`) holding copies of the 17,815 originals v7 needs,
  keyed by the MD5 of the bytes. A future `publish_native.qmd` computes a source-content hash instead, so the next
  release would not find them and would upload them again. It also leaves the v8/v9 trees and the gridded and merged
  surfaces untouched.
- S3 already serves PMTiles to a browser (range + CORS verified on `zones/…/zones.pmtiles`), so the file host copy is
  only needed by the Shiny apps.

## 2. The rule

**A release never owns a distribution file. It owns pointers.** Every cloud-optimized distribution file — per-input
gridded surface, merged model, source-resolution original (raster or vector), score COG — lives exactly once in an
unversioned, content-addressed store. A release publishes tables whose rows point into the store. Nothing ending in
`.tif` or `.pmtiles` is ever written under `{ver}/`.

```
marine-atlas/
  cog/{grid_id}/{hash}.tif              rasters ON an analysis grid: per-input "model", merged, score COGs   (exists)
  native/{ds_key}/{hash}.{tif|pmtiles}  source-resolution originals ("native"): raster or vector              (new)
  zones/{zone_set_key}/zones.pmtiles    zone geometry per vintage                                             (exists)
  assets.parquet                        the store catalog: one row per stored object                          (new)
  {ver}/manifest.json · {ver}/tables/*.parquet · {ver}/app/** · {ver}/serve/**                                pointers + scores
```

### Identity = source content + encoding, never bytes, never release

The key is known BEFORE the file is built, so an unchanged surface costs neither a build nor an upload:

| asset class | hashed content (per `mdl_key`) | encoding tag (examples; one per file family, recorded in the catalog) |
| --- | --- | --- |
| gridded per-input / merged / score (`cog/{grid}`) | `(cell_id, val)` rows — `msens::content_hashes()` as v1–v7 already do | `int1u-nd0-noovr` (v1–v7); the tag that describes the existing v8/v9 files |
| native raster (`native/{ds}`) | the source grid's `(cell_id, val)` rows (e.g. AquaMaps HCAF) | e.g. `flt4-0.5deg-cog` |
| native vector (`native/{ds}`) | normalized WKB + the attributes that reach the tile, ordered | e.g. `mvt-z0-10-simp10` |

`hash = content_hash_encoded(content_hash, enc)` (16 hex), the function that exists. The object's MD5/ETag is stored in
the catalog for integrity checks; it is never the key. A different encoding of the same content is a different
object, deliberately.

### The catalog and the pointers

- `assets.parquet` (bucket root, public): `store` (`cog`|`native`), `key`, `content_hash`, `enc`, `asset_type`,
  `grid_id` or `ds_key`, `bytes`, `md5`, `created`, `first_ver`. "Is it already stored?" is an anti-join against this
  file, not an S3 listing (anonymous listing is denied; `cog_store_index()`/`native_store_index()` become readers of
  it, with the listing kept as an audit).
- Every release, v1 onward, has ONE pointer table in the v8 shape, `tables/native_asset.parquet`
  (`mdl_key, ds_key, representation, asset_type, content_hash, asset_url, rescale_*, colormap, source_layer, bbox`):
  `representation = model` rows point into `cog/{grid}/`, `native` rows into `native/{ds}/`, merged models are
  `ds_key = ms_merge` rows. v1–v7 keep `model_asset.parquet` beside it for old links. `msens:::.app_assets()` already
  prefers `native_asset`, so shards, STAC and the Atlas follow without a schema change.
- Garbage = catalog rows no release points at: `store_unreferenced()` is a join over every release's pointer table.
  Nothing is deleted except through it.

### What "non-redundant" then means in practice

- A source dataset unchanged between releases (AquaMaps, BirdLife, IUCN…): hashed, found in the catalog, zero bytes
  uploaded, one row written.
- A merged model unchanged between releases, or equal to its only input: same `(cell_id, val)` rows, same key, same
  object. No special case.
- A dataset re-ingested with changes: only the models whose content changed get new objects; the old ones stay for the
  releases that still point at them.
- Scores are always new: they are tables under `{ver}/` (and score COGs, which dedupe only when identical).

## 3. Migration (one pass, gated; nothing destructive before G5)

- **M1 · Map (read-only).** For v8 and v9 (and v7b if it differs from v7): compute the store key of every existing
  per-model object from the release DB / sources (not from bytes), list every existing object with size + ETag, and
  write `store_migration.parquet`: `old_url → store, key, hash, enc, bytes, md5, n_releases`. Report the collapse
  (expect ≈ 39 GB across the v8+v9 trees → ≈ 20 GB unique) and three checks: same hash ⇒ same ETag between v8 and v9
  for a sample; a decoded sample COG equals its DB rows; no two different contents share a key. Include the 6,187
  unregistered `rng_iucn` PMTiles. **G1: Ben sees the numbers.**
- **M2 · Copy.** Server-side `aws s3 cp` old key → store key, only for keys not in the catalog, PMTiles from the S3
  mirror (file host untouched). Write `assets.parquet`. Verify: catalog row count = distinct keys; HEAD a sample.
  **G2.**
- **M3 · Re-point.** v8, v9: rewrite `native_asset` URLs to the store (add `content_hash`), restore the `rng_iucn`
  native rows. v7 (and v7b): build `native_asset` — `model` rows = the `cog/usa05/` objects it already has, `native`
  rows = the store objects whose source key matches (crosswalk as in the dry run: 17,810 of 19,811; plus `rng_iucn` by
  exact, unambiguous scientific-name match only, logged). Rebuild manifests, app bundles and STAC as DRY RUNS; diff
  against published: only asset URLs/rows may change. **G3.**
- **M4 · Publish** the re-pointed tables, manifests and bundles under their named flags (v9, v8, then v7; never
  `PROMOTE_LATEST`). Verify in the Atlas: v7 public (incognito) and v9 on the preview host show Original |
  Interpolated with the original drawn from `native/…` on S3. **G4.**
- **M5 · Guard.** `publish_native.qmd` and the merge/score publishers write only to the store + catalog; a publish
  gate fails if any `.tif`/`.pmtiles` key exists under `{ver}/` or any pointer URL is not in the catalog.
  `native_url()` stops defaulting PMTiles to the file host. Bucket READMEs (`README.md`, `cog/`, new `native/`).
- **M6 · Prune (destructive, explicit go, after a soak).** Delete `v8/native/`, `v9/native/` and file-host
  `pmtiles/{v8,v9}/` only when `store_unreferenced()` and a full pointer-URL HEAD sweep are clean. **G5: Ben's
  explicit go; bucket versioning is off, so this is not undoable.**

## 4. Atlas, docs, apps

- **Atlas:** shards carry absolute asset URLs and the app has no host allow-list, so the move needs no code change.
  Two small items after M4: an input with a single representation is captioned for what it is ("on the 0.05° scoring
  grid", not "as delivered"); one hermetic spec with store-shaped URLs (`native/{ds}/{hash}.pmtiles` on the S3 origin).
- **docs:** one "Asset store" section (layout, the rule, how to find a file, how a release references it) in
  `db.qmd`/`workflows.qmd`; `data-sources.qmd` and the per-release pages say which datasets carry originals;
  `apps.qmd`/`apps-guide.qmd` make the Atlas the current app and move Shiny `scores`/`species` to Legacy.
- **apps:** no further work; the per-version Shiny adapters in `msens` stay until the apps are switched off.

## 5. gm / nc (density datasets), without scoring

Fold in as registered, unscored datasets (`dataset.is_scored = FALSE`), through the store from the start:
1. msens: land the density work once — the newer copy in the `msens-density` worktree (`density_to_suit()`,
   `density_annual()`, `cells_from_raster(digits=)`, the "ud" tie fix + its regression test) as the next version on
   `main`; back up and drop the stale duplicate in the main checkout; main checkout back on `main`.
2. `ingest_sdm-nc.qmd` (drafted) then `ingest_sdm-gm.qmd` (rewrite to the dist-Parquet pattern): two-tier keys
   (`nc|{sp}|{season}` + annual `nc|{sp}`; `gm|{sp}|01..12` + annual `gm|{sp}`), originals to `native/{nc,gm}/`
   (nc rasters; gm hexes as PMTiles), gridded surfaces to `cog/global05/`, rows in `native_asset`.
3. No change to `merge_models.qmd` or any score. The Atlas shows them as species inputs with both representations.
Target release: the next one built (v10 bootstrap), not a re-issue of v8/v9.

## 6. Open decisions for Ben

1. `rng_iucn` on v7: accept exact, unambiguous name matches (≈ 2,114) or leave all 1,518+ single-layer?
2. PMTiles: S3 only from now on (file host copies pruned in M6) — yes?
3. Prune timing (M6): how long a soak after M4?
4. gm/nc target: v10 bootstrap (recommended) or a v9 re-issue?
