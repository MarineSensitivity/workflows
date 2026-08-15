# One app, every version: retire the per-version app forks

## Context

Today each MST version ships by **forking the apps repo**: freeze `apps` at a commit, clone it to
`/share/github/MarineSensitivity/apps_v{n}`, symlink `/share/shiny_apps/{scores,species}_v{n}` at it.
Nine such forks exist (`apps_v1`…`apps_v6`, `apps` on branch `v7`, `apps_v8` on `main`). Every app
improvement is stranded in the newest fork — v6's app will never get the v8 deep-link handling, the
analytics beacon, or a bug fix, because it is a different checkout of a different commit.

The version is currently a **code** fact (`ver <- "v8"` at `scores/app.R:60`, `species/app.R:36`)
that propagates into data paths, table names, a per-version titiler service with its own hostname,
and the symlink name. Nothing derives one from another.

**The goal: make the version a *data* parameter.** One app, from `main`, served at `/scores` and
`/species`, rendering any released version selected by `?ver=`, defaulting to `latest.txt`. Each
version becomes a self-describing bundle of COGs, PMTiles and Parquet on S3 plus a `manifest.json`
declaring what exists and what it can do.

Getting there means removing the two things that force the fork — the **titiler custom DuckDB→tile
factory** (replaced by precomputed COGs on stock titiler) and the **unstable model id** (`mdl_seq`
renumbers every rebuild, so a v6 deep link resolves to a different species in v7) — and normalizing
the two things that silently differ per version: the **spatial units** and the **grid**.

Decisions taken: stay on **AWS S3** (`oceanmetrics.io-public`; the "GCS" in the brief is loose usage
— nothing here touches Google); support **all of v1–v8**; **retire the custom factory**; **full
fidelity** for species and score layers on every version; **build `cell_model` for all versions**;
the new app **takes `/scores` + `/species`** directly.

### Facts established during scoping (measured, not assumed)

| | |
|---|---|
| Per-version model counts | v1 18,383 · v2 21,382 · v3 30,430 · v4/v4b/v5 30,436 · v6/v7 32,315 · v8 17,763 merged |
| `model_cell` rows | ~635M (v1) → ~1.20B (v4–v5) → 1,184,895,607 (v6/v7) |
| Every version's DB is on the server | `/share/data/big/v{1,2,3,4,4b,5,6,7}/sdm.duckdb`, 2.0–3.6 GB each |
| v8 COG economics | 17,587 AquaMaps COGs = **5.4 GiB** (~320 KB each); 3,009 PMTiles = 501 MiB |
| Content hash is cheap | `bit_xor(hash(…))` over 325M rows = **7.8 s** → a full 1.18B-row version hashes in <1 min |
| **Dedup works** | 3,000 raw AquaMaps models are payload-**identical** across v3, v6 and v7 |

That last row makes "full fidelity, all versions" affordable. Naive full fidelity is ~226,000 COGs /
~70 GB, but raw source surfaces don't change between versions — only merged (`ms_merge`) surfaces
do, and only where the merge rule changed. Content addressing should land nearer 60–90k unique
objects / ~25 GB.

### Divergences the design must absorb

- **`value` (v1–v7) vs `val` (v8)**; **`mdl_seq` (v1–v7) vs `mdl_id`/`mdl_key` (v8)**.
  `msens::.sdm_cols()` / `.species_sql()` (`msens/R/calc.R:266-360`) already resolve these per
  connection — extend that, don't reinvent it.
- **Three ids named alike**: v7 `mdl_seq` (autoincrement) ≠ v8 `mdl_id` (`dense_rank(mdl_key)`, a
  serving partition key that renumbers per release) ≠ the dead PostGIS-era `mdl_id`.
- **Two grids**: v1–v7 are US-extent `r_cellid.tif` (3103 cols); v8 is global `r_cellid_global.tif`
  (7200 cols). `cell_id` is **not comparable across grids**.
- **Spatial units drift**: v1/v2 have `planarea_key` (36); `programarea_key` (20) arrives in v3 —
  and Program Areas themselves have changed between versions. Subregion counts differ (v1 4, v3 8,
  v6 4, v7 5).
- **v1/v2 lack `taxon_model` and `listing`**; v1 lacks `er_score`/`extrisk_code`.

---

## Target architecture

```
s3://oceanmetrics.io-public/marine-atlas/
  latest.txt                          "v8"   <- newest RELEASED version (not pre-release)
  versions.json                       [{ver, status, released, n_taxa, n_models, docs_url}, ...]
  cog/{grid_id}/{hash}.tif            content-addressed, SHARED BY ALL VERSIONS
  zones/{zone_set_key}/
      zones.pmtiles                   vintage-versioned, SHARED BY ALL MST VERSIONS
      {grid_id}/zone_cell.parquet     geometry x grid only -> computed once, reused forever
  {ver}/
    manifest.json                     the contract between a version and the app
    tables/*.parquet                  taxon, model_asset, metric, zone_metric, zone_taxon, ...
    cog/metric/{metric_key}__{subregion_key}.tif
```

Three registries, each independent of MST version, replace what is today per-version implicit state:

1. **Grid registry** (`msens::grid_registry()`) — `grid_id` → nrow/ncol/extent/res/CRS + cell-id COG.
   `usa05` (v1–v7) and `global05` (v8). Every `cell_id`, content hash and COG carries its `grid_id`.
2. **Zone-set registry** — every spatial-unit layer ever used, labelled by **vintage**:
   `planarea_2025-06`, `programarea_2026-01`, `ecoregion_2025-06`, `subregion_usa_2026-08`. Carries
   `zone_type`, `title`, source gpkg, PMTiles href, `n_zones`.
3. **Version registry** — `versions.json` + `latest.txt` + per-version `manifest.json`.

### `manifest.json` — the keystone

```jsonc
{ "ver": "v6", "status": "released", "grid_id": "usa05",
  "id_field": "mdl_seq",                        // "mdl_key" for v8+
  "capabilities": { "cell_species_list": true, "native_representation": false,
                    "er_spatial": true, "reconstructed": ["taxon_model", "listing"] },
  "zone_sets": {                                 // what THIS version scored over
     "primary": "programarea_2026-01",
     "available": ["planarea_2025-06", "programarea_2026-01", "ecoregion_2025-06"] },
  "subregions": "subregion_usa_2026-08",         // canonical, same for every version
  "tables": { "taxon": ".../v6/tables/taxon.parquet", ... },
  "metrics": [ { "metric_key": "...", "title": "...", "subregion_key": "FULL",
                 "cog": ".../cog/usa05/9f3c….tif", "rescale": [0, 96],
                 "colormap": "spectral_r" }, ... ],
  "models": ".../v6/tables/model_asset.parquet", // mdl_key, mdl_seq, ds_key, hash, cog, bbox
  "stac": "https://stac-api.marinesensitivity.org/collections?..." }
```

Adding v9 means publishing a manifest — no app edit.

---

## Workstreams

### 1. Spatial units: a vintage-labelled zone registry, scored across all vintages

Per your note, this is more than a compatibility shim. Spatial units will keep morphing as BOEM's
planning process does, and the interesting question — *how did this Program Area's score change from
v3 to v8?* — is only answerable if every version is scored over the **same** unit.

- **`zone_set_key = {zone_type}_{YYYY-MM}`** identifies a layer *vintage*, decoupled from MST
  version. Today's `ply_programareas_2026_v6.gpkg` / `ply_programareas_2026_v7.gpkg` get compared;
  where geometry is identical they collapse to one `zone_set_key` (same content-hash trick as COGs,
  over the geometry), where it differs they become distinct vintages.
- **`zone_cell` moves out of the version tree.** It depends only on (geometry × grid), so it is
  computed once per `(zone_set_key, grid_id)` and reused by every MST version on that grid. Today
  it is recomputed per version by `score_zones.qmd:120-125` (`exactextractr`) — that work collapses.
- **`score_zone_metrics.qmd` iterates zone sets, not "the" zone set.** Each version produces
  `zone_metric` and `zone_taxon` for *every* registered zone set its grid supports, not only its
  contemporaneous one. Cost is modest — v8's `zone_cell` is 1.68M rows for 36 zones, so ~6 vintages
  is single-digit millions — and it is what makes a cross-version chart of one Program Area possible.
- **Subregions become canonical and version-independent**: one `subregion_usa_{vintage}` set
  spanning all US waters, no longer derived from whichever Planning/Program Areas that version had.
  Every version then shares one subregion list, so the score-COG matrix (metrics × subregions) is
  the same shape everywhere and dedups across versions where the metric didn't change.
- `msens::pra_score_delta` (`msens/R/validate.R`) gains a `zone_set_key` argument so version
  equivalence is asserted on a *fixed* spatial unit — today it silently compares whatever each
  version called "programarea".

### 2. Version registry — `msens/R/version.R` (new)

Port the CalCOFI pattern (`CalCOFI/workflows/libs/gcs_index.R`, `calcofi4r/R/match.R:20-34`) to S3:

- `atlas_latest()` — read `latest.txt`, trimmed; error (never silently fall back) if unreachable.
- `atlas_versions()` — `versions.json`; `status ∈ {released, prerelease, retired}`.
- `atlas_resolve_ver(x)` — `NULL`/`"latest"` → `atlas_latest()`; validate against `versions.json`;
  a pre-release is reachable only by naming it explicitly.
- `atlas_manifest(ver)` — fetch + memoise.

Promotion is **gated**, per `CalCOFI/workflows/test_release.qmd:445-493`: `latest.txt` is written
only after the release's validation passes, never by the build step itself.

### 3. Content-addressed COG store, catalogued in STAC

**Hash the payload, not the file.** A GeoTIFF is not byte-reproducible — GDAL stamps
`TIFFTAG_DATETIME`/`TIFFTAG_SOFTWARE` and the COG driver's IFD layout varies, so a file digest
changes on every rebuild and defeats dedup entirely.

```sql
-- one pass, no sort, no spill; order-independent so partition order can't perturb it
SELECT mdl_seq,
       count(*)                                               AS n,
       bit_xor(hash(concat_ws(':', cell_id, value)))::VARCHAR AS h   -- ::VARCHAR IS LOAD-BEARING
  FROM model_cell GROUP BY 1
```

> **Gotcha, verified:** without `::VARCHAR` the R driver returns UBIGINT as a double
> (`1.315468e+19`) and silently truncates to ~15 significant digits, aliasing distinct models onto
> one COG. Cast in SQL, carry as a string, never as numeric.

- `content_key(grid_id, n, h)` → object `cog/{grid_id}/{key}.tif`; `n` is a cheap collision guard.
- `cog_store_index()` — one recursive `aws s3 ls` into a local index; existence is a set lookup,
  **not** a HEAD per model.
- `publish_cog_store()` — hash → skip if present → else `publish_cog()` (reuse
  `msens/R/publish.R:55-84` unchanged, `COMPRESS=DEFLATE OVERVIEW_RESAMPLING=NEAREST BLOCKSIZE=256`)
  → upload → return manifest rows. Runs **on the server**; the DBs are there.

**STAC is the catalog of the store.** `publish_stac_api.qmd` already emits one Item per model as
stac-geoparquet into collections `msens-{ver}-{ds_key}`, served by stac-fastapi-duckdb at
`stac-api.marinesensitivity.org`. Extend it so:

- every MST version's models become Items whose COG asset href points into the shared
  `cog/{grid_id}/` store — dedup means Items in `msens-v3-am` and `msens-v7-am` legitimately share
  one href, which is exactly what content addressing is for;
- Items carry `msens:ver`, `msens:grid_id`, `msens:content_hash`, `mdl_key`, `mdl_seq` as
  properties, making them CQL2-queryable (`rstac` + `ext_filter()`);
- `msens/R/stac.R` gains `stac_search_models()` wrapping `rstac::stac()` / `items_fetch()` so R
  users get "which models cover this bbox in v6" without touching Parquet.

**Manifest vs STAC:** the manifest is a build-time *projection* of the same registry tables, not a
competing source of truth. The app reads the manifest (one fetch, no dependency on a backend whose
own notebook calls it "Experimental… not load-bearing infrastructure yet"); STAC serves discovery,
interop and programmatic access. Both are generated in the same chunk so they cannot drift.

### 4. Score surfaces → COGs, and the factory dies

- New chunk in `score_cell_metrics.qmd` (and its backfill twin) writing one COG per
  **(metric_key × subregion_key)** including a `FULL` pseudo-subregion — ~36 metrics × ~9 canonical
  subregions per version, a few hundred MB, content-addressed so unchanged metrics dedup across
  versions. This encodes today's `cell_sql(metric_key, subregion_key)` masking
  (`scores/app.R:168-193`) as pixels, so the app stops sending SQL. Per-COG `rescale` is computed at
  build time, replacing the runtime `msens::cell_stats()` call.
- `apps/scores/app.R` drops `cell_sql()`, `cell_stats()`, `cell_tile_url()`; every raster becomes
  `msens::cog_tile_url()` (`msens/R/viz.R:399-413`).
- `msens::cell_tile_url()`/`cell_stats()` deprecated (kept one cycle so the v7 fork survives cutover).
- `server/titiler/factory.py` and the `MsensCellsFactory` mount are deleted; `docker-compose.yml`
  collapses `titiler` + `titiler-v8` into **one stock titiler** for all versions, behind the existing
  Varnish (`titilecache`) — v8 currently has no cache at all. Keep `titiler-v8.marinesensitivity.org`
  as a Caddy alias during cutover.

`r_metrics_{ver}.tif` (`build_app_support.qmd:97-134`) already stacks every `cell_metric` for v7/v8
and is never uploaded — the natural source for the FULL-subregion COGs.

### 5. PMTiles: vintage-versioned, shared, and actually full-resolution

`msens::publish_pmtiles()` (`msens/R/publish.R:107-136`) runs `-Z0 -z6 --simplification 20` (callers
pass 10). The dominant defect is **maxzoom 6** — every view above z6 overzooms a z6 tile —
compounded by `--simplification` applying at every zoom including the deepest. Adopt the CalCOFI fix
(`CalCOFI/workflows/ingest_spatial.qmd:544-573`):

```
-Z0 -z10 --simplification=10 --simplify-only-low-zooms --no-tiny-polygon-reduction \
--no-tile-size-limit --no-feature-limit -y mdl_key -y ds_key --force
```

Vector tiles are **versioned by their own vintage and shared across MST versions**, per §1 —
`zones/{zone_set_key}/zones.pmtiles`, referenced by manifest, not duplicated per MST release.

Note **nothing in v8 builds the zone PMTiles at all**: `ply_programareas_2026.pmtiles` and
`ply_ecoregions_2025.pmtiles` are inherited artifacts from the archived
`old/calc_scores_v7.qmd:3172-3203`, served unversioned. Closing that reproducibility hole is part of
this workstream.

### 6. `mdl_seq` ↔ `mdl_key`, and reconstructing v1/v2

Verified: v1–v7 `model` is `(mdl_seq, ds_key, taxa, …)` where **`taxa` is the source species id**
(`am_0.05` / `ITS-Mam-180528`). So `msens::mdl_key_raw(ds_key, taxa)` synthesizes a stable
v8-grammar key for every historical model mechanically — no hand-built mapping. Needs a small
`ds_key` normalization (`am_0.05` → `am`).

- Per version, `tables/model_asset.parquet`: `mdl_key, mdl_seq, ds_key, sp_id, hash, cog_url, bbox,
  ms_merge_key`. This *is* the crosswalk and doubles as the manifest's model index.
- **URLs are always `?ver=…&mdl_key=…`.** Legacy `?mdl_seq=N` is accepted and resolved through the
  requested version's crosswalk — `mdl_seq` is meaningless without a version, so the Caddy redirect
  (§8) must inject `ver` from the old path.
- Cross-version species navigation keys on `taxon_authority` + `taxon_id`, stable across all versions.
- **v1/v2 `taxon_model` is reconstructable, not a placeholder**: v1's `taxon` carries per-dataset
  columns (`am_0.05, ch_nmfs, ch_fws, rng_fws, bl`) plus `mdl_seq` — unpivot them and the real
  taxon→model edges fall out. `listing` genuinely has no v1/v2 source, so it is emitted as a
  schema-conformant stub of NAs. Both are flagged in `capabilities.reconstructed` so the app can
  label them and no one mistakes a stub for data.

### 7. Per-version serving DB + `cell_model` everywhere

- One `serve.duckdb` with **a DuckDB schema per version** (`v1`…`v8`), each holding views over that
  version's local Parquet; the app holds one connection and qualifies (`FROM v6.taxon`). Keeps the
  CLAUDE.md rule that serving reads **local** Parquet — over HTTPS the per-query footer re-read
  costs 18–24× on small interactive queries.
- Backfill exports each version's app-facing tables from `sdm.duckdb` → Parquet → S3 + rsync to
  `/share/data/big/{ver}/tables/`.
- Run the `build_v7_cell_model.qmd` transpose for v1–v6 as well, so the per-cell species list works
  everywhere. The single largest compute item: 6 × ~1.2B rows. **Parameterize that notebook by
  version** rather than copying it.

### 8. The apps

`apps` `main`, `scores/app.R` + `species/app.R`. `ver` stops being a constant and becomes reactive,
so every startup-time resource keyed on it moves behind a memoised per-version bundle:

- `msens::atlas_version(ver)` → memoised `{manifest, schema, tables, urls, capabilities}`.
- Startup loads **only** `versions.json`; the first read of `ver()` materializes the bundle.
- `?ver=` parsed alongside the existing `?mdl_key=` handling (`species/app.R:741-836` already has a
  clean one-shot deep-link observer with `trk()` resolution tracking — extend it, and add the same
  to `scores/app.R`, which today has no R-side query handling at all).
- `updateQueryString()` writes `?ver=…&mdl_key=…`.
- Version picker sourced from `versions.json`, pre-releases badged and non-default. The Report tab's
  hardcoded `selectInput("rpt_ver", …)` list (`scores/app.R:905-911`) reads the same source.
- **Spatial unit becomes a picker**, fed by `manifest.zone_sets.available` — labelled with its
  vintage ("BOEM Program Areas, 2026-01"), so v1/v2 offering Planning Areas is a normal case rather
  than a missing feature.
- `capabilities` drives degradation (e.g. the Original/Interpolated toggle only where
  `native_representation` is true).
- `tile_base_url` stops being a literal in both apps and in `msens/R/viz.R:350,402,435`.

### 9. Caddy — redirects and `storage.marinesensitivity.org`

**Correction to the brief:** redirects cannot live in `MarineSensitivity.github.io` — that repo is
GitHub Pages for `marinesensitivity.org`, while the apps are on `app.marinesensitivity.org`, a Caddy
vhost (`server/caddy/Caddyfile:86-88` → `rstudio:3838`). Redirects belong there:

```
app.marinesensitivity.org {
  @old path_regexp oldver ^/(scores|species)_(v[0-9]+[a-z]?)(/.*)?$
  redir @old /{re.oldver.1}/?ver={re.oldver.2}{uri.query_append} 301
  reverse_proxy rstudio:3838
}
```
plus an `mdl_seq`-preserving variant; then retire the `/share/shiny_apps/{mapgl,mapsp}_v*` symlinks.
The github.io change is smaller but real: `index.qmd:124-126` links `/scores_v8/` → `/scores/?ver=v8`.

`storage.marinesensitivity.org` ports `CalCOFI/server/caddy/Caddyfile:25-107` to S3 — same shape
(explicit bucket allow-list, folder→`index.html` rewrite, inline `robots.txt` before the bucket
handler, JSON access log), proxying `https://s3.us-east-1.amazonaws.com` with a rewrite hiding the
bucket segment so URLs read `storage.marinesensitivity.org/marine-atlas/v8/`. The generator
(`CalCOFI/workflows/scripts/build_storage_index.R` + `libs/gcs_index.R`) ports to
`msens/R/storage.R`; S3's XML `ListBucketResult` is shape-compatible with the GCS XML API those
helpers already scrape, so `gcs_list_all()` becomes `s3_list_all()` with `continuation-token`. Keep
their two hard-won lessons: never index generated `index.html` objects, and **verify the deepest
page after upload** — a zero exit status has already proved not to mean the objects landed.

### 10. Versioned documentation

One docs set for eight model versions is already confusing and gets worse. `docs/` is a Quarto
**book** published to `gh-pages` by `quarto-actions/publish@v2` on push to `main` — an action that
**wipes** the branch, which is why versioned output needs a different publish step, not a new
static-site generator.

Recommended (the `jolars/moloch` workaround from quarto-cli#474, adapted):

- Render `_book` as today, then publish with `peaceiris/actions-gh-pages` using
  `destination_dir: {ver}` and `keep_files: true`, so `/docs/v7/`, `/docs/v8/` accumulate as
  siblings and `/docs/` redirects to the promoted version.
- A JS version switcher injected via `include-in-header`, fetching **the same `versions.json`** that
  drives the apps and the storage index — one registry for all three. Mount it in the existing
  `book.page-footer` (books don't expose `sidebar.footer` the way moloch's website does).
- Trigger on an MST release tag rather than every push to `main`, so docs versions track model
  releases, not commits.

Rejected: **pkgdown multi-version** versions the *package*, and `msens`'s version deliberately isn't
the MST version — you already spotted this. **Docsy / Read the Docs / Sphinx** all get native
versioning by replacing the entire Quarto book, discarding the R execution the docs depend on; the
versioning is not worth that.

### 11. Pipeline, docs-of-record, and skills

The `msens:` YAML front-matter drives `_targets.R` via `msens::build_targets_list()`, so new
notebooks join the DAG by existing — but the following must be updated in the same change as the
code, not after:

- `workflows/CLAUDE.md` — the new registries (grid/zone-set/version), the content-addressed store,
  "serving is stock titiler over COGs", the manifest contract, the corrected tippecanoe flags, and
  the stale line at `:175-176` claiming `serve/model_cell.parquet` is one sorted file (it has been
  `mdl_id`-partitioned since `release_marine-atlas.qmd:97-106`).
- `.claude/skills/{ingest-sdm,publish-sdm,generate-sdm-metadata,validate-sdm}` — each currently
  describes the v8-only, factory-based path.
- `workflows/schema.qmd` — new/changed tables (`zone_set`, relocated `zone_cell`, `model_asset`).
- `scripts/build_workflows_index.R` + `tar_visnetwork` DAG — verify the new targets land correctly.

### 12. Tests (`msens/tests/testthat/`)

Per CLAUDE.md, logic lives in `msens` and is asserted there, not inline in a QMD:

- `test-content-hash.R` — same payload in two cell orders hashes identically; a one-cell difference
  does not; **a hash carried as double is rejected** (regression for the UBIGINT precision trap).
- `test-mdl-key-xwalk.R` — `mdl_key_raw()` round-trips on real v1/v3/v7 `model` rows; `am_0.05`
  normalizes to `am`; a v6 `mdl_seq` resolves to the same species as the v7 `mdl_key` for that taxon.
- `test-version.R` — `atlas_resolve_ver()`: `NULL`→latest, `"latest"`→latest, a pre-release is never
  returned as latest, an unknown version errors.
- `test-manifest.R` — a manifest fixture validates; a **missing `capabilities` key fails loudly**
  rather than defaulting to "supported".
- `test-zone-set.R` — identical geometry across two versions collapses to one `zone_set_key`;
  differing geometry does not; `zone_cell` for a given `(zone_set_key, grid_id)` is grid-correct.
- `test-taxon-model-reconstruct.R` — unpivoting v1's wide `taxon` reproduces the v3 `taxon_model`
  shape for taxa present in both.
- Existing `test-merge.R` and `pra_score_delta` gates stay green throughout.

---

## Execution order

Each phase is a committed QMD executed by **rendering to HTML** (`tar_make()` / `quarto render`) per
the repo's reproducibility rule — no ad-hoc scripts; deploys gated behind env flags.

1. **Registries** — `msens/R/version.R`, grid registry, zone-set registry; `latest.txt` /
   `versions.json` / v8 `manifest.json`. Nothing consumes them yet; cheap, and it fixes the contract.
2. **Zone sets + canonical subregions**, `zone_cell` relocated out of the version tree; zone PMTiles
   rebuilt at the corrected zoom/simplification. Re-render v8 scoring and assert scores unchanged.
3. **Score COGs for v8**, wired into `score_cell_metrics.qmd`. Verify a COG tile is pixel-identical
   to the factory's.
4. **v8 app reads the manifest** — `?ver=` accepted, only `v8` valid. Ship to `/scores_v8` first;
   this is the deepest app refactor and wants real use before it inherits `/scores`.
5. **COG store + backfill driver** (`backfill_versions.qmd`, parameterized by version) + STAC Items
   over the store. Run **v7 first** — the version that must be perfect at cutover, and the dedup
   seed for the rest.
6. **Backfill v6 → v1** descending, one render each; log store hit-rate per run.
7. **`cell_model` transpose for v1–v6** — the long pole; run detached, one version at a time.
8. **Cutover**: `/scores` + `/species` → the multi-version app; Caddy redirects; retire the `apps_v*`
   symlinks and the `v7` fork; update github.io links.
9. **Retire the factory** — collapse to one stock titiler behind Varnish; delete `factory.py`.
10. **Versioned docs**, then **`storage.marinesensitivity.org`** — both independent of the above.

Phases 1–5 are reversible and touch nothing live. Phase 8 is the only user-visible cutover.

---

## Verification

- **No score changes.** `pra_score_delta` on v7↔v7 and v8↔v8 across the refactor must be exactly
  zero, now pinned to an explicit `zone_set_key`. `validate_versions.qmd` renders clean.
- **Tile equivalence.** For a sample of metrics × subregions, fetch the same `{z}/{x}/{y}.png` from
  the old factory URL and the new COG URL; assert pixel-identical output *before* deleting the factory.
- **Dedup honesty.** The backfill logs per version: models processed, unique hashes, store hits,
  bytes uploaded. A near-zero hit rate on v6-after-v7 means the hash is wrong or a grid changed —
  investigate rather than proceed.
- **Cross-version spatial unit.** Score one fixed `programarea_2026-01` zone across v3…v8 and plot
  it — the feature this normalization exists to enable, and a direct check that `zone_cell` reuse is
  correct.
- **Deep links** (Chrome via `claude-in-chrome`): `/scores/` → `latest.txt` version;
  `/scores/?ver=v1` → Planning Areas offered, not a crash; `/species/?ver=v6&mdl_seq=1234` → resolves
  via the crosswalk to the right species; `/scores_v6/` → 301 to `/scores/?ver=v6`.
- **Every version renders.** A smoke chunk that, for each `versions.json` entry, fetches the
  manifest, picks its default metric COG, and requests one tile — asserting 200 + non-empty PNG.
- **STAC.** `rstac::stac("https://stac-api.marinesensitivity.org")` + a CQL2 filter on
  `msens:ver = "v6"` returns Items whose COG hrefs 200.
- **Storage browse.** `storage.marinesensitivity.org/marine-atlas/` lists; a nested folder lists; a
  file downloads; a non-allow-listed path 404s with the helpful message.
- `devtools::test("../msens")` green; a red test is a hard stop.

## To settle during execution

- Whether v1/v2 warrant full app support or publish as data + manifest with `status: "retired"`.
  Decide after v3 works — they are ~40% of the schema-divergence work for the two least-used versions.
- Whether historical Program Area vintages are geometrically distinct enough to warrant separate
  `zone_set_key`s, or collapse to fewer than the per-version gpkg count suggests. The geometry hash
  in phase 2 answers this empirically and should be reported before the scoring fan-out is sized.
