# R4-F — backfill v7's species bundle with the original (native) surfaces (release-side; DRY RUN ONLY)

Ben (2026-09-30): the atlas must again show, per species input, the ORIGINAL layer (vector range from PMTiles, or
the native-resolution raster) next to the INTERPOLATED 0.05° surface. Decision: "backfill v7".
Background and the evidence table: `round4-session/c-release-side.md` (read it first).

The gap: v7 (the public default, `id_field = mdl_seq`) has no `native_asset` table. `msens:::.app_assets()`
(`msens/R/app_bundle.R` ~l.1157–1215) therefore synthesises ONE row per model from `model_asset`, labelled
`representation = 'native'`, which is really the gridded 0.05° COG. v8/v9 publish `native_asset` with both
representations per input (`publish_native.qmd`: PMTiles polygons or the 0.5° AquaMaps COG = `native`; the gridded
COG = `model`). The atlas shows its Original | Interpolated toggle whenever an input has both.

## Hard limits
- **No bucket writes, no server writes.** Never set `APP_BUNDLE_S3`, `PROMOTE_LATEST`, or any publish flag; never
  write `latest.txt`, `versions.json`, a manifest, or anything under `s3://`. Read-only HTTPS GETs are fine.
- Both repos have another session's uncommitted work. Do NOT touch the main checkouts. Use your own worktrees:
  `git -C /Users/bbest/Github/MarineSensitivity/workflows worktree add .claude/worktrees/r4-f -b r4-f-v7-native main`
  and `git -C /Users/bbest/Github/MarineSensitivity/msens worktree add .claude/worktrees/r4-f -b r4-f-v7-native
  atlas-contract-fixes`. Load msens with `devtools::load_all("<msens worktree>")`; never `devtools::install()`.
  (The msens main checkout has an uncommitted 0.45.0; take the next free version and say so in the report.)
- Read `/Users/bbest/Github/CLAUDE.md` (R style, NEWS.md + testthat are not optional) and
  `workflows/.claude/skills/` `publish-sdm` / `validate-sdm` if present.

## Step 1 — coverage (report before building anything big)
v7's DB: `msens::sdm_db_path("v7")` (exists locally). For every v7 INPUT model (not the merged `ms_merge` ones) get
its stable raw key via the existing `mdl_seq ↔ mdl_key` crosswalk (`backfill_versions.qmd` "The mdl_seq ↔ mdl_key
crosswalk", `msens::normalize_ds_key()`), then join to the published v8 and v9 `native_asset` tables
(`https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/{v8,v9}/` — find the Parquet path in that
release's `manifest.json`) on `mdl_key` where `representation = 'native'`. Report, per `ds_key`: v7 inputs, matched
in v8, matched in v9, unmatched (with 3 example keys). Also report whether each dataset's SOURCE is the same vintage
in v7 and v8 (the `dataset` tables' version/date/citation columns): a reused original is only honest if it is the same
source the v7 model was gridded from. HEAD-check three sample native URLs anonymously.

## Step 2 — the v7 `native_asset`
Produce a v8-shaped `native_asset` for v7 (same columns as v8's), keyed the way v7's `taxon_model`/shards key models
(`mdl_seq` as a string — check `.app_id_cast()`):
- one `representation = 'model'`, `asset_type = 'cog'` row per model from `model_asset` (the gridded COG, rescale
  1–100, its existing colormap/bbox);
- one `representation = 'native'` row per INPUT with a matched original (URL, `asset_type`, `source_layer`, rescale,
  colormap, bbox copied from the v8 row; prefer v8, fall back to v9 only if the source vintage check allows);
- merged models: follow what v8 does for `ms_merge` rows.
Put the builder in a documented, exported (or internal + tested) msens function, e.g.
`native_asset_backfill(con, native_ref, crosswalk)`, with testthat fixtures: matched input → two rows; unmatched
input → one `model` row; merged model; duplicate `(mdl_key, representation)` is an error; key spelling matches
`taxon_model`. Then make `.app_assets()` use it for a `model_asset`-only release when the table is present (it
already prefers `native_asset` — confirm the v1–v7 key cast still joins) and add a regression test that a v7-shaped
fixture yields shards whose inputs carry both `rep`s. Bump `DESCRIPTION` + `NEWS.md`; `devtools::document()`;
`devtools::test()` green.
Wire it into `backfill_versions.qmd` (export `native_asset.parquet` for a `mdl_seq` release, gated like its
neighbours) — code only; do not render the publishing chunks.

## Step 3 — dry run
Build v7's `app/taxon/*.json` and `app/alias/*.json` locally with `build_app_bundle.qmd`'s dry-run path
(`APP_BUNDLE_S3` unset), schema-validate them, and report: shards built; inputs with both reps vs the published v7
(0 today); (rep, type) counts; bytes of the largest shard (contract: ≤ 25 KB gzip); a diff summary proving only
`assets` changed versus the published shards; one full example input. Write the dry-run output under your workflows
worktree (git-ignored dir) and give the path.

## Report (≤ 45 lines; first line your model id)
Coverage table; vintage findings; what you built (files, functions, tests, versions); test results; dry-run numbers
and path; the exact commands the release session would run to publish (flag names only — you do not run them);
anything that makes reuse of a v8 original for v7 questionable. Commit on your two branches; do not merge or push.
