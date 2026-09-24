# build_app_bundle.qmd release-side reviews (Opus 5.5), 2026-09-24

## Review 1 of ecbce896 (R1–R5): PUBLISH WITH THESE FIXES FIRST

claude-opus-5-5[1m]

**Verdict: PUBLISH WITH THESE FIXES FIRST.** The R1–R5 logic is mostly sound, and every gate runs before any write. But run on the server as planned, the notebook stops at R5 before pushing anything. And once that is fixed, the push order can permanently serve stale data to Atlas visitors who load during the upload.

## Fixes, ranked (all in `build_app_bundle.qmd`)

1. **Blocker on the server: R5 writes into a read-only database (line 1105).** `dbWriteTable(con2, "_r5_published_keys", …)` has no `temporary = TRUE`. On the server, v1–v7b get their assets from `serve.duckdb` (tier 2), which is opened read-only (line 337). I reproduced the result locally with DuckDB 1.5.5: `Cannot execute statement of type "CREATE" … read-only mode`; with `temporary = TRUE` it works. The laptop dry runs passed only because tier 3 is a writable in-memory database (line 313). Expect the pending server v7 dry run to fail at `validate-asset-resolution`. Nothing gets pushed, but nothing can be published either.
2. **Must fix, for live visitors: upload `boot.json` last (lines 1580–1601).** The upload walks files in `list.files()` order, so `boot.json` goes right after the 256 `alias/` shards, before roughly 690 other objects (cell tiles, `taxa.json`, `taxon/`, the Parquet tables). Each object is a separate `aws s3 cp`, so that window lasts minutes per version.
   - The Atlas caches each table in the browser under `boot.tables[name].digest` (`atlas/src/lib/analysis/sources.ts:63`) and never checks the bytes it fetched.
   - A visitor during the window stores the old bytes under the new digest. That never refreshes, because the digest only changes when the content does.
   - If the upload fails partway, that mixed state stays live until someone reruns it.
3. **Should fix: don't publish when no asset table was found (lines 346–350).** When no source has `model_asset`/`native_asset`, the last fallback returns `ok = TRUE` with empty assets. For v8/v9, R5 skips (line 1063), the COG spot-check skips "with a reason" (1337–1341), and push checks only `b$ok` (1571). So v9 would be rebuilt with no assets and pushed with every gate green. It can't happen today on the laptop: v9's `sdm.duckdb` has a real `native_asset` table (86,857 rows). I did not check the server's copy. For v1–v7b this path already stops, because R5's query hits the missing `model_asset`. Return `ok = FALSE` (or stop when `APP_BUNDLE_S3=1`).
4. **Your call: the manifest patch is never pushed.** `APP_BUNDLE_S3=1` changes no `manifest.json` at all. The `app{}` block and the label backfill are written only to `_output/app_bundle/patches/{ver}_manifest_with_app.json` (lines 506–510). The push uploads only the bundle directory, and its key guard refuses anything outside `{ver}/app/`. So v1, v2 and v9 keep no labels live after this run. I diffed the staged files against the published manifests: v2 differs by 133 labels plus `app{}`, v7 and v7b by `app{}` only. They are faithful, so uploading them as a separate, explicit step is safe whenever you want.

Hardening, not blocking:
- At line 1099, R5 returns an empty table after already recording the gate as having run, so it passes without checking anything. It should stop instead, and assert `opened2$db == b$con_db`.
- R5's ground truth counts `DISTINCT mdl_seq` (line 1108), while the bundle counts one input per taxon edge. That can only cause a false stop, never hide a loss. The local v3–v6 databases have 0 raw models feeding more than one taxon; v1 and v7b are unmeasured.
- Assert `model_asset.ver == ver`. The msens duplicate check (E4) only requires a single `ver` value, not the right one.
- The tier-3 cache has two silent paths (laptop runs only; the server uses tier 2):
  - It reuses any existing file (line 306), and a truncated download leaves a partial file behind, which I reproduced.
  - It proceeds with any subset of tables as long as an asset table attached (line 321). A missing `zone_taxon`, for example, would publish an empty `zone_taxon.parquet` with only warnings.
- Add `duckdb_tune()` on `con2` (line 1100).

## Your seven questions

1. **R1 guard: yes, it stops before any write.** Lines 145–184 check the version (≥ 0.44.0), the `zone_names` argument, and the `app_zone_names()`, `manifest_labels_backfill()` and `app_manifest_block()` exports. A 0.43.0 install fails the version check. The earlier 0.44.0 commits (`8aea995`, `c250a0a`) fail the `manifest_labels_backfill` export check. `_quarto.yml` has no `error: true`, so the render halts; the push chunk is at line 1558. R5 also catches the old broken join directly on v1–v7b.
2. **R2 fallback: no wrong-version attach is possible today.**
   - The tier-3 cache folder and URL are both per version, and the server's `serve.duckdb` views read that version's own `tables/`.
   - I checked v7b's published `model_asset`: `ver = v7b`, 30,061 rows, and 6 turtle COG URLs that differ from v7's.
   - v9 never uses tier 3: its table list has no `native_asset`, and a 403 leaves no file behind (tested). v9 resolves from `sdm.duckdb`.
   - The only way to an empty v9 bundle is fix 3.
3. **R3 zone names: same GeoPackage, correct names landed.**
   - It resolves the same `dir_derived/zone_sets$source` for the same `zone_set_key` as `libs/app_bundle.R:716–721`, and the D17 check stops first if that key is missing.
   - A wrong `d17_type` can't pass today: the type and field are hard-coded together, and the geometry check confirms the column exists.
   - But the gate counts GeoPackage rows, not names that landed. msens's "matched zero zones" warning is also hidden by `warning: false`.
   - The dry-run `boot.json` files have 20 of 20 Program Areas named on v7, v2 and v7b.
4. **R4 label patch.**
   - The byte-identical assertion is real: backfill preserves row order and compares non-blank labels before and after.
   - `validate_manifest()` does run on the patched object (line 498), but only when `metrics` is non-empty, and it doesn't look at labels.
   - There's no second push path and no partial write because there is no push at all (fix 4).
5. **R5 gate: independent, correctly scoped, and it would catch the old bug.**
   - It uses raw SQL, and only the published key set comes from the bundle.
   - Scoping to published taxa matches what `card()` actually emits; unscoped would give a false 12,147/10,271.
   - The old bug gives 0 against 10,247 and fails.
   - Its one real defect is fix 1.
6. **Write ordering.**
   - All builds and gates for all versions finish before the first write, so any failed gate means zero writes.
   - A version whose gates were skipped with a reason is still pushed (only `b$ok` is checked).
   - Pushes are not atomic per version. If v3 fails partway, v1 and v2 are fully overwritten (and were validated), v3 is left partial with its new `boot.json` already live (fix 2), and the verify chunk never runs.
7. **Memory.** The only new database connection is R5's `con2`, opened without `duckdb_tune()`. Its query is tiny (about 30k asset rows against about 15k edges), so there's no practical risk on the shared server; add it for consistency.

## Review 2 of 869f450e (round 2, push paths): FIX FIRST

claude-opus-5-5[1m]

**Verdict: FIX FIRST.** Don't run the flagged publish for all versions yet. The bundle push itself is sound. The manifest-patch step would fail on 3 of 11 versions, it checks against a possibly stale copy of the live manifest, and it strips a header from all 11 manifests. The v7/v7b `cell_model` sync has no working safety checks.

I read `ff91ccff`; the two files are unchanged at the current main head `c6a19406`. Nothing was written and nothing touched S3 or the server. I ran three read-only checks: the diff guard on the real staged patches and cached manifests, a type-aware JSON diff, and a test that DuckDB allows a temporary table on a read-only connection.

## 1. Push ordering
- **`app/` push is correct.** `boot.json` goes last (`build_app_bundle.qmd:1769-1771`). Every `aws s3 cp` checks its exit status and calls `stop()`; there is no `try` and no `|| true` (`1782-1787`). The gzip round-trip is asserted before each upload (`1779`). Nothing in `_quarto.yml` or the front matter sets `error: true`, so a `stop()` halts the render. The manifest chunk (`1840-1865`) runs after all bundle objects.
- **Exception, for the v7/v7b `cell_model` runs.** The `aws s3 sync` runs *after* `boot.json`, and its exit status is thrown away (`1797-1799`). It logs "synced" even when the sync failed, and verify-anonymous only checks `app/`.
- "Never a mix" is overstated. While a push is running, the old `boot.json` sits over new bytes at the same keys. That mix clears itself once the new `built_at` lands. The harmful direction (new `boot.json` over old bytes) is closed.

## 2. `app_bundle_assert_manifest_patch_diff()`
- **It is not compared against the live manifest at push time.** `b$pub_manifest` comes from `app_bundle_fetch_published_manifest()`, which reuses `_output/app_bundle/_s3cache/{ver}/manifest.json` indefinitely (`libs/app_bundle.R:847-858`). The patch is built from that same object (`qmd:502, 533`), so the guard compares the patch against its own starting point. It cannot detect a manifest republished since then, and a stale cache would silently revert it. Stale caches exist in both worktrees (the atlas-contract one from 09-22).
- **It wrongly refuses v1, v2 and v9.** Their live `metrics` has no `label` column, and the backfill appends one (msens `.metrics_backfill_labels`). The shape check at `374-377` then stops before the absent-label branch at `385-386` is reached. I reproduced this: the real v2 staged patch fails, and simulated patches fail for v1/v2/v9 while the other 8 pass. These are exactly the versions the label backfill exists for. Because the manifest push comes after the bundle push, those three renders would publish their bundles and then fail.
- **What it does reject:**
  - a changed curated label (`387-395`)
  - a changed non-label metrics column (`379-383`)
  - a removed or added metric (dimension check at `374`)
  - any change to another top-level key (`365-370`)
- `app{}` is excluded as a whole, so a patch that changes `app{}` and also another key is still refused on the other key.
- **Selftest (`403-445`)** covers the curated label, a non-label column, `id_field`, and a removed row. It does not cover an added row, an added or removed top-level key, or the missing-label-column case; its fixture (`405-409`) always has a `label` column.
- **JSON round-trip is safe today.** A type-aware diff shows 0 differences outside `app` and labels for all 11 cached manifests and the 3 real patches: no array turned into a scalar, no rounding.

## 3. `allow_manifest = TRUE`
Safe. The version label is shape-validated first (`libs/app_bundle.R:130-134`), then only the exact string `{ver}/manifest.json` is admitted (`147-149`). Everything else must pass the `{ver}/app/` whitelist. The selftests (`302-326`) cover exact key, prefix, sibling path, other version and off-by-default. The call site passes one key built from `ver` (`qmd:1854-1855`). Nothing else can get through.

## 4. Gates that can skip
Any skip whose reason is not on `gate_push_allowlist` blocks the push (`1711-1725`).

| Gate | Lines | Skips when | Blocks the push? |
|---|---|---|---|
| `one_file_per_tile` | `929-934` | build incomplete / no cell table | Yes |
| `tile_width` | `960-965` | build incomplete / no cell table | Yes |
| `digest` | `993-999` | build incomplete / no cell table | Yes |
| `zone_metric` | `1042-1057` | no `boot.json` / no `zone_metric` table | Yes |
| `sets` | `1111-1116` | build incomplete / no `boot.json` | Yes |
| `asset_resolution` (R5) | `1174-1180` | "not a mdl_seq-shaped release" | **No — the one allow-listed reason** |
| `cog_species_spotcheck` | `1470-1480` | no taxon has a merged COG | Yes |

- **Gates that print but can never fail:** the schema re-validation table (`843-844`) and the sets table (`1152-1153`) are never asserted. Both are TRUE on v2/v7/v7b today.
- The R3 zone-name check passes trivially if `boot$zones[[d17_type]]` is missing: expected 0 = landed 0 (`680-683`, `728-729`).
- **The allow-listed v8/v9 skip describes R5's SQL scope correctly, but nothing replaces it.** The COG spot-check only looks at merged COGs, and it blocks only when there are zero merged COGs. A v8/v9 bundle with merged COGs but zero input assets would pass.
- The code path worked in the 09-22 bundles built with msens 0.43: v8 had 25,450 of 25,450 inputs with an asset, v9 had 35,999 of 36,000. v8/v9 have not been rebuilt with msens 0.44.0's mdl_key-only matching.

## 5. What a server run does that the laptop dry run did not
- **The documented path is the laptop.** `PUBLISH_PLAN.md` says "publish from the laptop". `scripts/render_app_bundle.sh` runs `quarto render` directly on the host, not through `srv_render.sh`, and the msens1 host has no R or quarto. On the server it must be `scripts/srv_render.sh build_app_bundle.qmd APP_BUNDLE_VERS=<ver> APP_BUNDLE_S3=1`, one version per render, in a committed loop. Don't do one render of all 11: that puts them in a single R process inside the 9 GB rstudio container, which the live Shiny workers share.
- **`aws` CLI:** the CLAUDE.md note is stale. `server/rstudio/Dockerfile:25-38` installs AWS CLI v2. Credentials come from the `/home/ubuntu/.aws:/home/rstudio/.aws:ro` mount (`docker-compose.yml:128`). That only works if HOME resolves to `/home/rstudio` under `-u 1000:1000`, the same assumption the other release notebooks rely on.
- **No credential check up front.** `backfill_versions.qmd:446` runs `aws sts get-caller-identity` first; this notebook doesn't. A missing credential fails at the first `cp`, which is safe (nothing written, `boot.json` untouched) but only shows up after the whole build.
- **Different asset source.** On the laptop, v1–v7b were built from the S3 `tables/` cache (tier 3). On the server they resolve to `serve.duckdb` opened read-only (tier 2), which the dry runs never exercised. I confirmed locally on DuckDB 1.5.5 that the `temporary = TRUE` table works on a read-only connection and a non-temporary one fails, so the R5 fix holds.
- **Paths.** `dir_derived` is `/share/data/derived` on Linux (`qmd:213`), consistent with `libs/paths.R:15-16`.
- **Stale caches.** Any server `_output/app_bundle/_s3cache` can feed stale manifests to the item-2 problem. The tables cache is also reused indefinitely (`qmd:315`).
- **Policy question for Ben.** v9 `tables/` answers 403 anonymously (`qmd:271-274`), but v9 `app/` (and v7b `cell_model`) would be anonymously readable. Confirm that is intended for restricted releases.

## Fix first, ranked
1. **Diff guard:** accept a live manifest with no `label` column when the only change is an added `label` column whose values are all non-blank. Add a selftest fixture with no `label` column.
2. **Live manifest:** in `push_manifest_one`, fetch the manifest fresh (no cache) and assert it is `identical()` to `b$pub_manifest` before diffing. Clear `_output/app_bundle/_s3cache/` at build start.
3. **Cache header:** add `--cache-control no-cache` to the manifest `cp` at `1857`. The existing convention sets it (`backfill_versions.qmd:467`, `build_version_manifest.qmd:322`, which calls manifest.json a mutable pointer), and a plain `cp` replaces it on all 11 manifests. Add the same to the `boot.json` upload.
4. **`cell_model` sync:** check its exit status and stop on failure. Move it before the `app/` loop so `boot.json` stays last. The "already on S3" check probes `tile=0` (`libs/app_bundle.R:1101-1105`), but v7/v7b tiles start at `tile=19`, so it always says "not on S3" and its all-or-nothing protection can never trigger. Probe `plan$keys[1]` instead, and HEAD the uploaded keys afterwards.
5. **Wrong `app{}` block:** it is probed before the push (`qmd:529`), and msens `app_capabilities()` checks `tile=0`, which no release has (usa05 grids start at 19, global05 at 436). The staged patches therefore say `cell: false, cell_model: false` (403) and would keep saying so after a successful push. Nothing reads `manifest.app` today (the Atlas reads `boot.capabilities`), but it publishes a false contract. Fix the probe in msens and recompute the block after the push.
6. **Unasserted checks:** assert the schema re-validation and sets tables. Add `n_zone_names_expected > 0` to R3. Assert `gzip_ok` in verify-anonymous.
7. **v8/v9 input assets:** add a native_asset version of R5, counting input edges by `mdl_key` independently.
8. **Full dry run first:** round-2 code has only been rendered on v2, v7 and v7b. Before publishing, do a full dry run with `APP_BUNDLE_S3` unset for all 11 versions on the machine that will publish.
