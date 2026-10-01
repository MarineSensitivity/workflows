# Asset store · M1 · G1 report (2026-10-01)

Source of truth: `map_asset_store.qmd` rendered to `_output/map_asset_store.html` (39/39 chunks, 0 errors).
Outputs (laptop, `~/_big/msens/derived/asset_store/`): `store_migration.parquet` (145,452 rows = every
object under `v8/native/` + `v9/native/`), `assets_proposed.parquet` (86,421 catalog rows, passes
`asset_catalog_check()`), `verify_raster.parquet` (per-object pixel verdict), `v7_rng_iucn_match.parquet`.
Nothing was written to S3, the file host or any release. Not pushed.

## Today vs after (resolved objects; keys from DB/source content, never bytes)

| family | objects today | GB | distinct keys after | GB after |
| --- | ---: | ---: | ---: | ---: |
| gridded `cog/global05` (am, ax, vec_grid, dps, merged) | 83,936 | 16.81 | 50,403 | 10.60 |
| `native/am` (0.5° originals) | 37,411 | 0.31 | 18,708 | 0.15 |
| `native/ax` (as delivered) | 10,536 | 1.79 | 10,534 | 1.79 |
| `native/*` PMTiles | 13,552 | 14.18 | 6,776 | 7.09 |
| **total** (+17 unmapped `gm`) | **145,452** | **33.08** | **86,421** | **19.63** |

Merged objects whose key equals an input's object: 5,662 of 22,459 (0.04 GB). The savings are the v8/v9
duplication (am, am_native, vec_grid, PMTiles are byte-identical between releases); only `merged`
(1,634 of 6,785 same-name pairs differ in content, v9 supersession) and v9-only `ax`/`dps` are new.

## The three checks
- **(a) same name v8/v9 ⇒ same key and same ETag:** am 18,710/18,710, am_native 18,703/18,703, vec_grid 6,753/6,753,
  all PMTiles 6,776/6,776 (0 differ). merged: 5,151 same key (5,103 same ETag, 48 same key but different bytes),
  1,634 different key (legitimate re-merge). Of the 48, a decode shows v8's COG is the stale one (below).
- **(b) decoded COG == the rows its key hashes** (EVERY object except `am`, which is sampled):
  am_native 18,703/18,703, ax 10,527/10,527, dps_nmfs 19/19, v9 merged 15,674/15,674 consistent.
  Not consistent: **v8 merged 192**, **vec_grid 6 (both releases)**, **am 898 of 6,240 decoded (14%)**;
  the other 12,475 `am` are NOT decoded (opt-in `MAP_VERIFY_AM=1`; M2 will gate each copy on it). Causes:
  1. `am` (898): `dist/dataset=am` files were rewritten 2026-07-13, after the COGs were painted (07-11); v9
     inherited both. Median |Δ| is 0.001% of pixels, but 5 of the worst sampled are material: e.g.
     `am|Fis-31618` COG = 392,627 px vs 3,311 rows in the current dist (118×). The map shows a surface the
     scoring did not use. These must be REPAINTED from current rows, never copied under the new key.
  2. v8 merged (192): exactly the v8 suitability-only models; painted `trunc(max)`, v9 uses `round(max)`.
     Same pixels, values differ by ≤ 1 level. Needs repaint or a distinct encoding tag.
  3. vec_grid (6): the 6 `rng_turtle_swot_dps` models; dist has 2 rows per cell and (CC) 542 cells disagree,
     so the painted value is "last row wins". Needs a dedup rule.
- **(c) no key names two contents:** 0 collisions across 145,452 objects; catalog check passes.
- Local copies == S3 objects: sizes equal for every object that exists locally; MD5 == ETag on all 20-per-folder
  samples. 42 PMTiles are multipart uploads (ETag ≠ MD5, bytes verified equal on the 19 sampled earlier).
- Vector: 19 of 20 `rng_iucn` PMTiles header bounds agree with the source (1 differs, not yet inspected);
  ch_fws / rng_fws orphans are S3-only (no local file). `ax_native` == delivered TIF band 1 on 5/5.

## Not mapped / orphans
- `gm` (17 objects, v8 only): not in scope (§5 of the plan).
- Orphans (on S3, in no release's pointer table): am 13 (v8) / 15 (v9), am_native 6 / 8, ax_native 9, ch_fws 3,
  rng_fws 20 per release, and **all 6,753 v9 `vec_grid`**: v9's `native_asset` has no model rows for any vector
  range, so v9 shows no Interpolated representation for them (v8 does).
- All source content was at hand (laptop). The server has no `dist/`, `sdm.duckdb` or `merge.duckdb`
  (only `model_cell`, `tables`, `serve.duckdb`): hashing can only run on the laptop.

## For M3: v7 `rng_iucn` (exact, unambiguous name match + bbox agreement)
v7 models 3,766; with a published COG (the inputs the app shows) 1,518; exact name hit in v9 2,325;
unambiguous 2,179; with a COG, unambiguous, original on disk **1,460**; **bbox agrees 1,426**, disagrees 34
(Pacific ranges where the v9 tile bounds stop short of the dateline; list in `v7_rng_iucn_match.parquet`).

## Timing (measured)
- All content hashing for BOTH releases (dist, merged, suitability-only merged from mc_parts, am_native, ax_native,
  every vector source): 15:18 → 16:06 = **≈ 49 min** on the laptop (8 cores, 24 GB). Slice rate for `content_hashes()`
  on dist: 90 M rows in 6 s (~14 M rows/s); `rng_iucn` vectors 5.5 min, `rng_fws` 3.2 min.
- Expected-side quantised hashes (for the verification): ≈ 40 min (17:00 → 17:28 under swap pressure).
- Decode of ~58,000 non-am objects: ≈ 38 min once the machine stopped swapping (it ran ~1.5 h earlier at < 2 batch/min
  while the laptop held 20 GB of swap and was killed twice). Batches are resumable (`verify_obs/`).
- The `am` decode (12,475 objects, ≈ 7 B pixels) is the expensive one and was deferred to M2 (`MAP_VERIFY_AM=1`).

## The six facts atlas-bf asked for
1. **State:** msens `main` = `6be51da` (0.47.0; merge `5054fef` of r4-f 0.46.0 on 0.44.2, then `943f00e`,
   `6be51da`); workflows `main` = `19632ac2` + my uncommitted-then-committed M1 files (see git log). Nothing pushed.
2. **DBs:** v9 `sdm.duckdb` (29 GB), `merge.duckdb`, `dist/`, `mc_parts/`, `dist_merged_global/` on the laptop; v8 has
   `dist/`, `merge.duckdb` etc. but no `sdm.duckdb`. Server: none of those. Timing above.
3. **Encodings** (all COG: DEFLATE, 256 blocks): am / vec_grid / merged / dps_nmfs / ax: Byte, nodata 0,
   NEAREST overviews (none on tiny rasters), values TRUNCATED into INT1U by terra; am_native: Byte nodata 0 on the
   720×360 grid; ax_native: Float32 nodata -9999 as delivered. Tags in `msens::asset_enc()`:
   `int1u-trunc-nd0-ovr`, `int1u-trunc-nd0-ovr-0.5deg`, `flt4s-nd9999-ovr-delivered`, `mvt-z0-10-simp10`.
4. **Byte identity v8/v9:** full population, in (a) above. Not a sample.
5. **`rng_iucn` PMTiles:** all 6,187 ARE registered in both published `native_asset` tables (file-host URLs).
   My earlier "6,187 unregistered" was wrong; the 316 in my mapping is only the subset v7's crosswalk matched.
   S3 serves PMTiles with `206` + `Access-Control-Allow-Origin: *`, so the atlas reads them from S3 directly;
   the file host copy is only needed by the Shiny apps.
6. **Merged pointers:** v8: 14,799 → `native/am` (aliases), 6,785 → `native/merged`; v9: 5,957 aliases,
   15,674 merged. `cog/global05/` holds only score COGs (286 ≈ v8 94 + v9 191); no species object.
