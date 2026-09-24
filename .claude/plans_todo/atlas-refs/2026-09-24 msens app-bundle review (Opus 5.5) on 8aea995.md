claude-opus-5-5[1m]

# msens `atlas-contract-fixes` review: 8aea995 + c250a0a (0.44.0)

Reviewer: Opus 5.5, 2026-09-24. Scope: `git diff atlas-contract...atlas-contract-fixes` (8aea995, the
M1 / zone-name / short-label change, plus c250a0a, the `fs` Imports and NEWS follow-up; c250a0a touches
only DESCRIPTION + NEWS.md, so every code finding below applies to both commits).

How I checked it: I exported both branches with `git archive` into the session scratchpad and loaded
them with `load_all()` there. Nothing in `../msens` was edited: `git status` there shows only the other
session's density WIP, which I left alone. I then ran the branch code on the real published v2/v7/v8/v9
`tables/*.parquet`, fetched anonymously from S3.

## Verdict

**MERGEABLE after edits.** The M1 fix is correct. On real v7 and v2 data it reproduces the species app's
input resolution row for row. None of the problems below is in the M1 logic. The blocking edits are the
NEWS changelog and one missing guard. Most of the work that remains is on the RELEASE side: nothing
changes in production until the bundle is rebuilt with 0.44.0, and for most releases the metric labels
will not change at all (see 4).

## 1. Asset resolution vs the species app: matches exactly

- **Evidence on real data.** I wrote a faithful copy of `apps/species/app.R`'s v1-v7 branch (lines
  ~367-413 plus `rows_for()`/`pick_asset()`). I ran it and `app_taxon_shards()` on the published v7
  and v2 `taxon`/`taxon_model`/`model_asset`/`dataset` tables.

  | release | taxa | input edges | inputs with an asset (fix) | same, species app | (key, mdl_key) diffs | URL diffs | merged-URL diffs | atlas-contract (old) |
  |---|---|---|---|---|---|---|---|---|
  | v7 | 16,153 | 12,120 | **10,247** | 10,247 | 0 | 0 | 0 of 16,153 | **0** inputs with an asset |
  | v2 | 17,353 | 3,173 | **3,172** | 3,172 | 0 | 0 | 0 of 17,348 | **0** |

  The v7 inputs that still have no asset are 1,873 `rng_iucn` models. They are in `model` but have
  no `model_asset` row, so they show as struck through in BOTH apps: a gap in the data, not in the
  join. The real walrus card built from v7's tables has am `790` -> `cog/usa05/3e1d4309c691974f.tif`
  and rng_iucn `23495` -> `9f3a81fdac2583ed.tif` (is_mask TRUE). The live published `v7/app/taxon/6f.json`
  today has `[am_0.05: 0 assets, rng_iucn: 0 assets]`.
- **The join key is `mdl_key` alone** (`card()`: `a$mdl_key == ei$mdl_key[j]`). Asset `ds_key` is
  never compared. The spelling mismatch is real: v7 `taxon_model`/`dataset` say `am_0.05`, and
  `model_asset` says `am` for the same `mdl_seq` (16,929 rows). The input `ds_key` stays raw, so the
  `is_mask` lookup against `dataset.ds_key` still works.
- **mdl_key collisions.** Within a release `model_asset` is unique on `mdl_seq`: v7 has 30,061 rows
  and 30,061 distinct values, v2 has 21,376 and 21,376, and each has one `ver`. v8/v9 `native_asset`
  is unique on `(mdl_key, representation)`, and their `taxon_model`/`native_asset` `ds_key` agree on
  every row. So dropping the `ds_key` condition changes nothing on v8/v9 by construction. No v2 or v7
  taxon has two inputs from the same dataset. A clash between releases can only happen if `con`
  carries a registry that spans several releases. `build_app_bundle.qmd` opens one release's
  database, so this does not happen today. Nothing asserts it either, since `mdl_key` uniqueness is
  now the invariant everything rests on (edit E4).
- **HUGEINT casts.** `.app_edges()` now casts `key` with the same `.app_id_cast()` as
  `.app_taxon_sql()` and `.app_assets()`, so a DOUBLE `mdl_seq`/`taxon_id` gives `"790"` on every
  side. On v1-v7 `mdl_seq` is INTEGER, so the result is identical there.
- **Rows that are rightly left out** (the species app does the same):
  - v2 single-source taxa whose `taxon.mdl_seq` IS the raw am model are dropped by
    `mdl_key != key`.
  - Edges of unlisted taxa (`mdl_seq` NULL or not `is_ok`) are dropped.

## 2. Effect of switching to a LEFT JOIN: safe

- `.app_assets()` now returns about 30k rows on v7. For about 19.8k of them `key` is NA: every input
  model, plus merged models of unlisted taxa. Two things read `.app_assets()`:
  - `.app_merged()` filters `!is.na(a$key)`, so merged URLs are unchanged (0 of 16,153 differ).
  - `card()` reads assets only through `mdl_key`.
- Asset rows are never published on their own. `app_model()` returns NULL for v1-v7, so a row with
  no taxon cannot leak into the bundle.
- `taxon.mdl_seq` is unique on real v7 (16,388 of 16,388), so the join creates no duplicate rows.
- Cost: `card()` stays O(inputs × |a|). v7 shards took 1.6 min against 59 s before, on the laptop.
  That is acceptable.

## 3. `app_zone_names()`: correct; three robustness gaps

- **Real GeoPackages.** All five `data/zone_sets.csv` sources, plus `v7/ply_programareas_2026_v7.gpkg`,
  have exactly one layer and a `{type}_key`/`{type}_name` pair, with no duplicate keys.
- **Key case.** Keys in the published v1/v7/v9 `zone` tables match the gpkg keys exactly, and none
  differs only by case. The only unmatched keys are the unnamed rollups `USA`, `FULL`, `L48` and
  `AKL48`, which publish `name: null`.
- **Where the name lands in the Atlas.** `app_zones()` names each list by `sub("_key$","",fld)`, so
  the name lands at `zones.programarea[i].name`. The boot schema already allows `name: string|null`.
  `zoneStatFromBoot()` reads `raw.name` when it is a non-empty string and falls back to the key
  otherwise, and `paLabel()` renders "Aleutian Arc (ALA)". So the name lands where the Atlas reads it.
- **Gap 1: a mistake produces no names, silently.** `app_zones()` never checks `zone_names`. Pass it
  wrong column names, or `fld = "programarea"` instead of `"programarea_key"`, and every zone gets
  `name: null` with no error. That is a check that cannot fail (edit E3).
- **Gap 2: the wrong file gives partial names.** `app_zone_names(programarea_gpkg, "planarea")`
  succeeds, because program-area files also carry `planarea_key`/`planarea_name`, and returns a
  partial list. E3 covers this too.
- **Gap 3: extra layers.** A gpkg with several layers would have its first layer read, with no
  warning under `quiet = TRUE`. No real file has more than one layer, so this is a nit.
- **No input checks.** `path` and `type` get no `stopifnot()`. `ncol` shadows `base::ncol`, and the
  two assignments share one line with `;`. Style nits.
- **Plumbing untested.** Only `app_zones()` is tested. Nothing checks that `app_boot()` and
  `app_bundle_build()` pass `zone_names` through (edit E5).

## 4. Short labels: correct, but they will not change what most releases display

- **Curated labels are safe.** The backfill only touches blank labels (`is.na | !nzchar(trimws())`).
  Tested with a curated row, a missing column and no `metrics`.
- **The composite is "Overall score".** The category names match `docs/receptors.qmd` exactly:
  Corals, Invertebrates, Fish, Marine Mammals, Seabirds, Sea Turtles.
- **What the Atlas will show.** `metricLabelsFromManifest()` takes the first `label` for each key
  from the PUBLISHED `{ver}/manifest.json`, which I fetched:
  - **v3, v4, v4b, v5, v6, v7, v7b, v8 already carry curated labels:** `score`, `bird: ext. risk`,
    `prim prod, ecorgn`, and so on. Because the backfill never overrides a label, **these releases
    keep showing "score"**, including v7, the public default. NEWS/roxygen quote v7's "score" as the
    motivating case, which is misleading (edit E2).
  - **Only v1, v2 and v9 have no `label` column.** Once their manifests are republished they will
    show "Overall score", "Seabirds: extinction risk" and so on. Until then the Atlas falls back to
    the long `boot.layers[].label`.
  - Across versions the result is inconsistent: title-case canonical labels on v1/v2/v9, terse
    lower-case curated ones on v3-v8. Making them uniform is a curation decision for Ben: blank or
    rewrite `layer` in `layers_{ver}.csv`. The code rule itself is fine.
- **v1/v2 keys the lookup table does not cover.** `extrisk_all` becomes **"All: extinction risk"** and
  `extrisk_reptile` (v1's turtle bucket) becomes "Reptile: extinction risk". Both are coined by the
  fallback and are neither tested nor docs-styled (edit E6).
- **Future collision.** Any `score_*` becomes "Overall score", so a release with two composites
  (for example a v10 variant) would get two identical picker entries. Not a problem today; every
  release has one.
- **Production will not see the backfill as things stand.** `build_app_bundle.qmd` (atlas-contract)
  deliberately passes the PUBLISHED manifest and has removed `manifest_build()` from its render path.
  Its manifest patch is `pub_manifest + app`, with no label backfill. Labels only reach production
  through a manifest republish (R4).

## 5. Tests

- **Results.** Both changed files pass on the branch export:
  - `test-app_bundle.R`: all pass, with 1 skip offline. Under `NOT_CRAN=true` the walrus test passes
    8/8 and all 3 S3 objects HEAD 200.
  - `test-version.R`: all pass, including the backfill test (3/3).
  - Full suite: my own `devtools::test()` run on the export (`NOT_CRAN=true`) covered 37 files and
    469 tests: **2,793 expectations, 0 failed, 0 errors, 2 skipped**.
- **The tests catch the bug.** With the NEW test file loaded against the OLD `atlas-contract` code,
  all three M1 tests fail: 5 of 9, 1 of 2 and 3 of 6 expectations (plus errors).
- **Exact rows are asserted.** `.app_assets()` must return 3 rows with `mdl_key` = {101, 201, 301}.
  Edges must be 2, with raw `am_0.05`/`bl`. The card must have 2 inputs, each with exactly 1 asset,
  and URLs set-equal to the expected ones. The two changes are tested separately:
  - The turtle test would still fail if only the LEFT JOIN were applied.
  - The tern test would still fail if only the card join were changed.
- **Not asserted.** Nothing checks that input asset rows have `key` = NA, or the tern's `merged$url`
  (edit E5).
- **The "real walrus" test** uses rows copied by hand from the real tables, not a live read. I checked
  them against the published v7 parquet and they are identical. `skip_if_offline()` sits AFTER the
  offline assertions, which is correct: only the HEAD check skips. `req_perform()` has no timeout or
  retry, so a network hiccup after the probe errors the test instead of skipping it. Nit.
- **No mocking.** Everything runs through synthetic DuckDB files or a temporary gpkg.
- **Coverage gaps.** `extrisk_all`/`extrisk_reptile`, a whitespace-only curated label, and the
  `zone_names` pass-through into `boot.json` are untested.

## 6. Package hygiene

- **Roxygen.** Rerunning `devtools::document()` on the export produced no diff.
- **Imports.** `@importFrom sf st_read st_drop_geometry` is already covered by the multi-line
  `importFrom(sf, ...)`. `export(app_zone_names)` was added. c250a0a adds `fs` to Imports.
- **NEWS.md: BLOCKING.** The `# msens 0.43.0` heading was overwritten by `# msens 0.44.0` (diff
  `-# msens 0.43.0` / `+# msens 0.44.0`). The whole 0.43.0 section (the `{ver}/app/` contract) now
  sits under 0.44.0, at both 8aea995 and c250a0a. c250a0a DID fix the false line claiming ".app_assets
  /.app_edges also normalise ds_key".
- **Style.** 2-space indentation and snake_case throughout. The `nm <- if … else` block in
  `app_zones()` has a dangling `else` that is hard to read. `app_zone_names()` uses `stop()` with a
  clear message but has no `stopifnot()` on its inputs. Nits.
- **Merging to `main` conflicts.** `main` carries 0.42.1 (stac_build legacy, 0faf3b8). A dry
  `git merge-tree` in a scratch clone conflicts in DESCRIPTION + NEWS.md only. Fix it by keeping
  0.44.0 and ordering the headings 0.44.0 > 0.43.0 > 0.42.1 > 0.42.0. The fix branch fast-forwards
  onto `atlas-contract`.

## 7. Edits before merge (ordered)

- **E1 (blocking):** restore `# msens 0.43.0` above "**The `{ver}/app/` data contract…**" in NEWS.md.
- **E2 (blocking, NEWS accuracy):**
  - Say that curated labels (v3-v8, including v7's "score") are unchanged, and that only unlabelled
    manifests (v1/v2/v9 today) gain canonical labels once republished.
  - Replace "lost all 19,811 input COGs" with the measured figure: 10,247 of 12,120 v7 input edges
    now resolve. 19,811 is the count of non-merged `model_asset` rows, not of inputs.
- **E3 (should):** validate `zone_names` in `app_zones()` or `app_bundle_build()`:
  - `stopifnot(is.data.frame(zone_names), all(c("fld","key","name") %in% names(zone_names)))`
  - stop, or at least warn, when a non-NULL `zone_names` names zero zones of a field it lists.
- **E4 (should):** at the top of `app_taxon_shards()`, stop on duplicated `(mdl_key, representation)`
  in `.app_assets()`. For v1-v7, when `model_asset` has a `ver` column, assert it is single-valued.
  This makes the new mdl_key-only invariant loud.
- **E5 (nice):** extend the tests:
  - assert `a$key` is `c("101", NA, NA)` and the tern's `merged$url`;
  - one `app_boot(…, zone_names=)` assertion that the name reaches `boot$zones$programarea` and
    passes schema validation;
  - a whitespace-only curated-label case.
- **E6 (nice):** add `all = "All Species"` and `reptile = "Sea Turtles"` (or "Reptiles"; Ben's call)
  to `.SP_CAT_LABEL`, with tests. Otherwise v1/v2 publish "All: extinction risk".
- **E7 (optional):** add `export`/`@export` to `.metric_short_label()`, or add an exported
  `manifest_labels_backfill(m)` that `manifest_build()` calls. Then the release can patch published
  manifests without `:::` (see R4).

## Release side: what production needs

- **R1: rebuild the bundles with 0.44.0 and push.** Run
  `APP_BUNDLE_S3=1 quarto render build_app_bundle.qmd` for v1-v7b. Before that, raise the notebook's
  msens guard from ">= 0.43.0" to 0.44.0, or check `"zone_names" %in% names(formals(app_bundle_build))`.
  A stale 0.43.0 install republishes the broken M1 bundle with every gate green.
- **R2: the build must reach a `model_asset`.**
  - Public v1-v7: the notebook's tier 3 (anonymous S3 `tables/` cache) works on the laptop.
  - **v7b (restricted):** tier 3 skips it. The laptop has only `sdm.duckdb`, which has no
    `model_asset`, and no `serve.duckdb`. The live v7b bundle has `merged: null` and no input
    assets. Build v7b ON THE SERVER, where `serve.duckdb` carries the `model_asset` view, or allow
    tier 3 for restricted releases whose `tables/` are anonymously readable (`v7b/tables/model_asset.parquet`
    returns HTTP 200).
- **R3: zone names.** In `build_one()`, pass
  `zone_names = msens::app_zone_names(<same gpkg path app_bundle_geom_keys() resolves>, d17_type)`.
  Optionally rbind ecoregion/subregion names too, for `boot.zones.ecoregion|subregion`. Without this
  the Atlas keeps falling back to the bare key.
- **R4: labels for releases already published.**
  - Only v1, v2 and v9 lack labels.
  - Rebuilding them through `manifest_build()` (backfill_versions for v1/v2, build_version_manifest
    for v9) should work: v2's subregion tie resolves via `zone_sets`. But a rebuild republishes the
    WHOLE manifest and can trip the round-10 identity gate.
  - Lower risk: a label-only patch. Read the published manifest, fill blank `metrics$label` from the
    same function (needs E7), run `validate_manifest()`, then put. Fold it into the same patch that
    adds the `app{}` block.
  - Whether v3-v8 should get canonical labels is Ben's curation call (edit `layers_{ver}.csv`).
- **R5: add a gate to `build_app_bundle.qmd` that the old code would fail.** For a `mdl_seq`
  release, require that the number of input edges with at least one asset equals the number with a
  `model_asset` row (v7: 10,247/12,120). This is the M1 regression check at release level.
- **R6: merge order and cache.** Merge fixes -> `atlas-contract` -> `main` (resolve DESCRIPTION/NEWS
  against 0.42.1). The rstudio container then reinstalls msens `main` at its next start.
  CloudFront/browser caches of `app/taxon/*.json` and `boot.json` follow the bundle's own
  `boot.tables` digests; the JSON shards have no digest, so check the Atlas's cache policy after the
  push.
