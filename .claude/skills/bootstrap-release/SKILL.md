---
name: bootstrap-release
description: Start a new MarineSensitivity release version (e.g. v9 → v10) on the same cell grid — bump `ver`, register it, clone the unchanged ingests copy-on-write from the previous version, and avoid the readers that silently assume the previous version's schema. Use when beginning any new version, before touching a dataset.
---

# Bootstrap a new release version

A version bump on the **same grid** reuses every ingest nothing changed. The first such bump
(v8 → v9, 2026-08-27) found the pipeline only ever moved `ver` with a full rewrite, and that four
readers of `ver_prev` assumed v7's schema or a pre-built version directory. This is the checklist.

## 1. Register the version (in this order)

1. `libs/paths.R`: `ver <- "v10"`, `ver_prev <- "v9"`.
2. `data/versions.csv`: add `v10,prerelease,restricted,<date>,<title>` — `bootstrap_version.qmd` and
   `build_version_manifest.qmd` both assert the row exists. Restricted until reviewed
   (`project_prerelease_review_gate`).
3. `../msens/R/grid.R`: `.GRID_VER["v10"] <- "global05"` (+ `test-grid.R`), bump `Version:` + `NEWS.md`,
   reinstall. `grid_for_ver()` errors on an unknown version on purpose.
4. `../server/.env`: `PREVIEW_REVIEWERS_V10=…` (later: the two `CF_ACCESS_AUD`s `access.sh` mints).

## 2. Clone, don't re-ingest — `bootstrap_version.qmd`

`quarto render bootstrap_version.qmd` (the `bootstrap_version` target; `build_cell_grid` depends on
it). It clones `{dir_big}/{ver_prev}/marine-atlas/dist/dataset=*` + `model_*.csv` into this
version's `dist/` — `cp -c` (APFS clonefile: instant, zero bytes) on macOS, `cp -al` on Linux —
and checks count + bytes per dataset. `BOOTSTRAP_VERIFY=1` re-hashes every clone with
`msens::hash_parquet()` and asserts equality with `data/manifests/ingest_*.json` (proof it is the
checkpointed surface). `BOOTSTRAP_SKIP_DS=am` for a dataset this version re-ingests. The ingests
then *resume* on the clone in seconds, manifests unchanged.

Then `quarto render build_cell_grid.qmd`: with the shared cell-id COG present it **copies the
`cell` table from `ver_prev`'s `sdm.duckdb`** and asserts its hash against the checkpoint. Before
this fix a fresh `sdm.duckdb` had no `cell` table and every downstream
`stopifnot("run build_cell_grid first" = file_exists(sdm_db))` passed on an empty database.

## 2b. Tables that live in `merge.duckdb`, which a new version starts EMPTY

`merge.duckdb` is created by `merge_models_prep`; the clone in step 2 covers `dist/` only. Anything
another target writes into it must be re-rendered for the new version **before** the merge:
`ingest_listings` (`ingest_nmfs-fws-listings.qmd` → `listing`: US ESA / MBTA / BCC — without it the
governing `er_score` silently lost every federal floor on v9's first merge; it is now a hard stop
in `merge_models_prep`), and `build_common_names` (`taxon.common_name`, after `merge_taxon` —
without it `score_zone_metrics`' `zone_taxon` fails on the missing column). `scripts/run_version.sh`
runs both as stages. Check the DAG for any new writer (`grep -l merge.duckdb *.qmd`) when a target
is added.

## 3. Readers that assume `ver_prev`'s schema — now introspective, keep them so

| reader | what it assumed | what it does now |
|---|---|---|
| `score_zones.qmd` `v7_cat` | `v7.taxon.is_ok` | `is_ok` if present, else `is_valid_usa AND is_marine`; column still named `in_v7` (schema.qmd, `SCORE_V7COMMON`) |
| `merge_models_prep.qmd` | `taxon.worms_id`; re-resolved EVERY model by name | a model `ver_prev` already crosswalked keeps its taxon by exact `mdl_key` (its `taxon_model`); name / native-id resolution only for new models. Re-resolving by name against v8's one-name-per-taxon table re-keyed 12 models and failed the plumbing check by 3 taxa |
| `build_zone_cells.qmd` gate | this version's released `zone_cell` | gates an unreleased version against `ver_prev`'s on the same grid instead of "skipped" |
| `build_registry.qmd` | `ver_prev` dataset metadata for every ds_key | new datasets declare theirs in front-matter (`generate-sdm-metadata`) |
| `release_marine-atlas.qmd` | a `titiler-{ver}` compose service | `TITILER_SERVICE` (default `titiler-v8` — the stock `/cog` tiler for every release) |

Treat any `column not found` on a `v7`-attached table as this class of bug: resolve by column
introspection (`msens::sdm_cols()` style), never by version string.

## 4. Order of the first full run

`bootstrap_version → build_cell_grid → ingest_* (resume) → the new ingest → merge_models_prep →
merge_models (REDO_MC_PARTS is implicit: fresh dir) → merge_taxon → score_* → build_registry →
publish_native (PUBLISH_MERGED_COG=1 — mandatory) → publish_score_cogs → release_marine-atlas
(RELEASE_NO_S3=1 first) → build_version_manifest → publish_stac_api → publish_storage_index`, then
the server flags in the order `publish-sdm` gives. Run the **control** render first when a dataset
supersedes another (`validate-sdm`).

## 5. What "bootstrapped" looks like

`du` of the new version dir reports the logical size (≈ ver_prev's) while `df` did not move; the
`cell` table has `n_usa = 634,208`; `tar_make("ingest_aquamaps")` finishes in seconds with the same
`content_hash`; `bootstrap_version.json` lists every dataset with `method: clonefile`.
