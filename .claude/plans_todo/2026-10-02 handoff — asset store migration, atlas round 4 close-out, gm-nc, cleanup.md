# Handoff (2026-10-02): asset store migration, atlas round-4 close-out, gm/nc, cleanup

Written by the atlas orchestrator session (`atlas-bf`, Fable 5.1) for whichever agent picks this up. Everything here
was verified on 2026-10-01/02 unless marked "not verified". Read this file, then the three it points at:

1. `plans_todo/2026-10-01 asset store — every distribution file stored once, referenced by every release.md`
   — the design, the measured state, Ben's decisions (section 6).
2. `plans_todo/atlas-refs/round4-session/m2-m4-runbook.md` — the ordered commands for the migration (written by the
   release session, reviewed by the orchestrator).
3. `plans_todo/atlas-refs/round4-session/m1-g1-report.md` — what the mapping pass measured.

## Status update, 2026-10-02 evening (egress session, with Ben's go)

**§5 A is DONE: the runbook ran end to end; v9, v8, v7b and v7 are published from the store and each passes
`scripts/check_release_pointers.R`** (run log: `atlas-refs/round4-session/m2-m4-runbook.md` §7). §5 B: v7 verified in a
browser on the public Atlas (`atlas/scripts/prepublish-check.mjs --live`); Ben looked at v9 on the review host; v8 and
v7b were not looked at by a person. §6 items 1–4 shipped as atlas 0.10.87. What remains, in order:

1. **STAC** — msens 0.52.0 (local `main`, not pushed) replaces the dataset Items' four directory hrefs with one link
   to `{ver}/tables/native_asset.parquet`. Still to run: the STAC rebuild + deploy per release and
   `publish_stac_api.qmd` (per-model Item hrefs still name the old `{ver}/native/` files). Needs msens 0.52.0 on the
   server (push + pull + `03_msens_from_share`), so it waits on Ben's go to push msens. Two other directory hrefs
   remain by design for now (`dist_merged/`, legacy `tables/`).
2. **M5** (§5 D) — not started. `publish_native.qmd` must not be run until then.
3. **Docs** (§5 C) — branch `asset-store` unchanged; three of its sentences wait on 1 and 2.
4. **M6 prune** — earliest 2026-10-16, Ben's explicit go.
5. Cleanup (§7), gm/nc (§7) — not started.

## 0. One-paragraph state

The Atlas (static app, `atlas/`) finished round 4 and is live at **0.10.86** (`main` = `83b4839`, clean, one branch,
one worktree). The species "Original vs Interpolated" layers are missing on the public release (v7) because v7 never
published originals; fixing that turned into a redesign of where every distribution file lives (one content-addressed
store, releases hold pointers). That redesign is **fully staged locally and has NOT been written to S3**: the copy
plan, 2,934 repainted rasters, the re-pointed v9/v8/v7b/v7 tables, manifests and app bundles, and a runbook. **The
next action is Ben's: he runs the runbook's writing steps himself.** After that: verify in the Atlas, merge the docs
draft, do two small Atlas follow-ups, prune after a two-week soak, and fold gm/nc into the v10 bootstrap.

## 1. Hard constraints (learned the hard way)

- **No agent may write to the production bucket unattended.** The orchestrator's attempt to arrange overnight S3
  writes was refused by Claude Code's auto-mode classifier as `[Production Deploy]`; the release session is gated on
  Ben's approval for any S3/file-host write, push or delete (his rule). The runbook's writing steps are run by Ben
  with the `!` prefix, or under a permission rule he grants. Do not look for a way around this.
- **Pushing the atlas repo is allowed and routine** (CI → GitHub Pages). `workflows` and `msens` are NOT pushed:
  workflows `main` is 33+ commits ahead of origin, msens `main` 12 ahead. Push only when Ben says.
- **The bucket `oceanmetrics.io-public` is shared** (`backups/`, `gazetteer/`, `issues/`, `calcofi.duckdb` at the
  root). Versioning is off, no lifecycle rule exists. Versioning is bucket-wide and can be suspended, never removed.
- **Never `PROMOTE_LATEST`; never write `latest.txt` or `versions.json`.** v7 stays the public default.
- **Token discipline (Ben, 2026-09-25/30):** lean gates locally, CI does three-engine; no reviewer agents; the
  orchestrator looks at contact sheets itself; builders shoot but do not view images. See the atlas `CLAUDE.md`
  ("Eyes-on before push, once per wave") and the memory note `lean-gating`.
- **Sonnet 5.5 builders** (`model: "sonnet"` resolves to `claude-sonnet-5-5`; every builder prints its model id on
  line 1 of its report). Haiku for docs sweeps. The orchestrator does not write feature code.

## 2. Where everything is

| thing | where | state |
| --- | --- | --- |
| atlas | `atlas/` `main` @ `83b4839`, 0.10.86 | pushed, deployed (gh-pages `deploy: f6dc008` + a script-only commit) |
| workflows | `workflows/` `main` @ `0a538ded` | 33 ahead of origin, NOT pushed; 36 dirty files are Ben's own notes/plans — leave them |
| msens | `main` is checked out in `msens/.claude/worktrees/main-merge` @ `406cebb`, **0.50.0** | 12 ahead, NOT pushed; installed on the laptop |
| msens main checkout | `msens/` on dead branch `atlas-contract-fixes`, 11 dirty files | a STALE duplicate of the density work; nothing in it is missing from `main` (verified by diff). Retire it last (§7) |
| msens density | `msens-density/` (worktree, branch `density-transforms` @ `234a543`) | merged into msens `main` as 0.48.0; worktree can go (§7) |
| docs | `docs/.claude/worktrees/asset-store`, branch `asset-store` @ `5209354` | draft, unpushed, HOLD until the store is live (§5) |
| site | `MarineSensitivity.github.io` `main` @ `9b8a65f` | pushed; brand page live at marinesensitivity.org/branding/ |
| staged migration | `~/_big/msens/derived/asset_store/` (`stage/`, `publish_stage/`, `store_migration.parquet`, hash tables) | local only |
| release session | Claude Code session named `migrate-native-assets-s3` (Sonnet 5.5) | idle, holding for Ben; has the full working context of the migration |
| plan page (round 4 UI) | https://claude.ai/artifact/46i61atguf5HzoHb6hnbF8 | marked shipped |

Bucket layout today (measured): `cog/usa05/` 23,382 objects (v1–v7, content-addressed); `cog/global05/` 286 (v8+
score COGs only); `v8/native/` ≈ 57,700 objects ≈ 14 GB; `v9/native/` ≈ 87,700 ≈ 19 GB; file host
`file.marinesensitivity.org/pmtiles/{v8,v9}/` a third copy of the PMTiles. `native/` does not exist yet.

## 3. The asset store: what was decided and why

**Rule:** a release never owns a distribution file, only pointers. Nothing ending in `.tif`/`.pmtiles` under `{ver}/`.
`cog/{grid_id}/{hash}.tif` for anything on an analysis grid (per-input gridded, merged, score COGs);
`native/{ds_key}/{hash}.{tif|pmtiles}` for source-resolution originals; `assets.parquet` at the bucket root is the
catalog; every release has a `tables/native_asset.parquet` in the v8 shape (+ `content_hash`, + `source_key`).
**Key = hash of the QUANTISED pixels / source content + an encoding tag** (`msens::pixel_hashes`, `asset_enc()`), never
the bytes: the key is known before a file is built, identical pixels share a key, and a merged model equal to its only
input is the same object. **Verify-before-store:** an object is admitted under key K only if its decoded pixels equal
the rows K was computed from; anything else is repainted from the current rows, never copied.

Ben's decisions (all recorded in the plan's section 6): IUCN ranges on v7 by exact unambiguous name + a cell-level
agreement test; PMTiles served from S3 only; bucket versioning + 30-day rule BEFORE the first store write, two-week
soak, then prune; gm/nc into the v10 bootstrap as raw density, unscored; repaint mismatched files; restore v9's lost
vector `model` rows; dedupe turtle DPS rows by the value the merge consumed (max).

## 4. What the migration found (facts a successor must not rediscover)

- **v8 and v9 are byte-identical** for `am`, `am_native`, `vec_grid` and all PMTiles; only `merged` differs (1,634 of
  6,785 same-name pairs) plus v9-only `ax`/`dps_nmfs`. 145,452 objects / 33.08 GB → 86,351 keys / 19.64 GB.
- **2,784 of 18,715 AquaMaps gridded COGs (14.9%) do not match the rows scoring used** (the `dist/dataset=am` files
  were rewritten two days after the COGs were painted). 2,772 differ only at cells exactly at the value-1 threshold;
  **9 hold a NEIGHBOURING model's surface** (the positional-`mid` bug described in `publish_native.qmd`; e.g.
  `am_Fis-31618.tif` holds `Fis-31621`'s 392,627 cells); **3 shrink massively when repainted** and nobody has checked
  whether the smaller current rows are right: `am|ITS-Mam-180451` 9,008,300 → 10,269 cells, `am|SLB-190137`
  1,111,001 → 134,164, `am|W-Pyc-134687` 2,028,655 → 129,951. **Open question for Ben / the AquaMaps ingest.**
  It is NOT US clipping: `dist` rows are whole-range; the US restriction happens in the merge.
- v8 `merged`: 192 suitability-only models painted `trunc(max)` where rows are `round(max)` (≤ 1 level, 47% of pixels).
- `rng_turtle_swot_dps`: 14 files for 6 models, every cell in two files, disagreeing on up to 1,458 cells (by up to
  50). The painter read one file. Repaint takes max across files (what `turtle_sql()` consumed). **v10 ingest defect
  to fix at the source.**
- v9's `native_asset` had lost every `model` row for vector ranges (6,753): v9 shows no Interpolated for them. Staged
  fix restores them.
- All 6,187 `rng_iucn` PMTiles ARE registered in v8/v9 (an earlier note said otherwise; it was wrong).
- v7 `rng_iucn`: 1,518 inputs with a COG; 1,460 exact unambiguous name matches; **1,455 pass** the cell test (≥ 95% of
  v7 cells within 25 km of the source polygon). Rejected: *Oncorhynchus nerka* 0.08, *Carcharhinus obscurus* 0.10,
  *Pristis pristis* 0.00 (clear); *Carcharodon carcharias* 0.85, *Trichechus manatus* 0.86 (borderline — Ben decides;
  `PS_ACCEPT_MDL_SEQ=19621,51921` includes them).
- **The staged v7 bundle passed every table/shard diff and still drew nothing for an original range.** The Atlas
  filters tile features on the input's `mdl_key`; v7 keys inputs by `mdl_seq` (`17626`) while the shared tile's
  features carry `bl|22694870`. Fixed on both sides: msens 0.50.0 emits `source_key` on PMTiles assets; atlas 0.10.86
  uses `asset.source_key ?? input key` (`rangeFeatureKey()` in `src/lens/species/mapInputs.ts`). Verified in a
  browser against the restaged files: BirdLife range and FWS critical habitat for Marbled Murrelet both draw.
- A PMTiles asset whose table bbox spans the globe (a dateline-crossing range) gets `bbox: null` by the bundle
  builder's rule, in every release: the range draws but "Zoom to layer on change" cannot frame it (§6 item 1).
- The server has no `dist/`, `sdm.duckdb` or `merge.duckdb`: hashing and painting run only on the laptop
  (8 cores, 24 GB; the decode runs hit swap — keep to ≤ 3 workers).
- Staged results: inputs with both representations v7/v7b 0 → 10,120; v9 29,227 → 35,980; v8 25,450 (unchanged).
  In every release nothing changes outside `inputs[].assets` and `merged.url`; alias shards identical; the builder
  reproduces every published bundle from its published tables (so the diffs mean something); largest shard 14,351 B
  gzip (contract 25,600).

## 5. What happens next, in order

**A. Ben runs the runbook** (`m2-m4-runbook.md`; ≈ 4–5.5 h): P0 preconditions → Step 1 bucket versioning (his D1
choice A/B/C) → Step 2 backup → Step 3 83,417 server-side copies → Step 4 2,934 uploads → Step 5 verify → Step 6
catalog → Step 7 per release v9, v8, v7b, v7 (`PS_PUSH=1` then `build_app_bundle.qmd` with `APP_BUNDLE_S3=1`).
Before Step 7 he decides D2 (the two borderline IUCN pairs; a re-stage if yes). The release session
`migrate-native-assets-s3` can drive it with him; if that session is gone, the runbook is self-contained.

**B. After each release's publish, rerun the browser check** (read-only):
```bash
cd /Users/bbest/Github/MarineSensitivity/atlas && export TMPDIR=$PWD/.tmp/tmp
# BEFORE a release is published: staged files, store objects routed to what the plan copies
duckdb -csv -c "COPY (SELECT key, src_key FROM read_parquet('$HOME/_big/msens/derived/asset_store/stage/store_plan.parquet') WHERE action='copy') TO '.tmp/store_copy_map.csv' (HEADER);"
node scripts/prepublish-check.mjs --stage ~/_big/msens/derived/asset_store/publish_stage --ver v7 \
  --q "lens=species&sp=54272&in=bl" --map .tmp/store_copy_map.csv --name bl
# AFTER the store is populated: drop --map. AFTER v7 is published: just open the public Atlas in an
# incognito window (the OPFS cache is keyed by built_at) and look.
```
Pass = `representation` reads "Show … as Original Interpolated", `pressed` is "Original", a non-zone `.pmtiles`
returned 206, and the screenshot shows the range drawn. Check at least: Marbled Murrelet (`sp=54272`) `in=bl` and
`in=ch_fws`; one AquaMaps input (0.5° original vs gridded); on the preview host one v9 vector input (its restored
Interpolated). **v8/v9 are restricted: they render only on `preview.marinesensitivity.org/{ver}/atlas/` behind
Cloudflare Access — not verified by anyone yet.**

**C. Docs.** Merge `docs` branch `asset-store` (`5209354`) once the store is live. Before merging: change the tense to
present; re-check the eleven "depends on something not yet true" sentences the writer listed (its report is in the
orchestrator's transcript; the claims are the callout, the `.tif`/`.pmtiles` rule, the `native_asset` shape, the
`assets.parquet` columns, "STAC links the same URLs" — unverified, STAC must be rebuilt first — the publish gate,
the server chapter's S3 claim, the retirement wording); and **fix one wrong sentence**: it says an input delivered on
the analysis grid has a single representation, but AquaX has two (Delivered | As ingested). The docs repo's own
`CLAUDE.md` binds: no local book render, numbers only from chunks via `libs/versioned.R`, hold a merge until live.

**D. M5 guard (code, not yet done).** `publish_native.qmd` still contains the 0.46 MD5 fallback — **do not run it**
until it writes only to the store + catalog via `pixel_hashes`. Add the publish gate (fail if any `.tif`/`.pmtiles`
exists under `{ver}/` or a pointer URL is missing from the catalog). `native_url()` already defaults PMTiles to S3.
Rebuild STAC (`publish_stac_api.qmd`) so Item hrefs point at the store — required before the prune.

**E. M6 prune (destructive; Ben's explicit go; two weeks after M4).** Delete `v8/native/`, `v9/native/` and file-host
`pmtiles/{v8,v9}/` only when `store_unreferenced()` and a full pointer-URL sweep are clean and STAC is rebuilt.

## 6. Atlas follow-ups (small; one Sonnet builder each, lean gate)

1. **Frame an original whose bbox is null.** When the selected asset has no bbox (dateline-spanning range), fall back
   to the camera the input's gridded surface would get (the `/cog/info` chain in `src/lens/species/data/camera.ts`).
   Today the range draws but sits partly under the left-docked panel.
2. **Caption a single-representation input for what it is** ("on the 0.05° scoring grid"), not "as delivered"
   (`src/lens/species/mapInputs.ts`, the legend subtitle). After the backfill ~2,000 v7 inputs stay single-layer.
3. **One hermetic spec with store-shaped URLs** (`native/{ds}/{hash}.pmtiles` on the S3 origin).
4. **The `parity` CI job is red on the last three runs**: the comparison prints `PASS (max|delta| < 1e-9 on every
   quantity)` and then a later `fetch` dies with `SocketError: other side closed`. Not a parity failure; find which
   fetch follows the PASS in `scripts/parity/` and add a retry or drop it. Three-engine and gallery are green on
   0.10.86 (run 36944227000).
5. **Eyes harness:** no `eyes-shots.mjs` state shows an input with both representations; add one once v7 has them.
6. Flaky under load, never diagnosed: `shell.firstview` honeycomb, `shell.phone-search`, two `feedback` tests,
   `scores.search` (Firefox), `shell.firstview.phone` (Firefox).
7. R4-D design call still open with Ben: with only a last-clicked cell (not added), "Open report" is disabled.

What round 4 shipped, for reference: 0.10.80 legend histogram (whole-layer, marker) · 0.10.81 the spine (rail attached
to the panel, Layers · Details · Table · Report, docked left, `ui=` token v4) · 0.10.82 Table at full stage + `Tabs` ·
0.10.83 Report as one flow · 0.10.84 globe `queryRenderedFeatures` fallback (`queryLayerFeatures()`) · 0.10.85
representation switch ("Show <input> as" + gold switch) · 0.10.86 `source_key`. Control grammar (switch / tabs /
spine): `MarineSensitivity.github.io/branding/control-grammar.md` and the brand page. Round-4 lessons are in the atlas
`CLAUDE.md`.

## 7. gm / nc and the remaining cleanup

- **gm/nc (v10 bootstrap, raw density, unscored).** Step 1 done: msens `main` has `density_to_suit()`,
  `density_annual()`, `cells_from_raster(digits=)` (0.48.0). Still to do, in `workflows`: finish
  `ingest_sdm-nc.qmd` (drafted, untested), rewrite `ingest_sdm-gm.qmd` to the dist-Parquet pattern, two-tier keys
  (`nc|{sp}|{season}` + `nc|{sp}`; `gm|{sp}|01..12` + `gm|{sp}`), originals to `native/{nc,gm}/`, gridded to
  `cog/global05/`, rows in `native_asset`, `dataset.is_scored = FALSE`, unit/legend metadata ("animals km⁻²") reaching
  the registry and the app bundle. No change to `merge_models.qmd` or any score. See `plans_todo/v10-0 bootstrap +
  housekeeping.md` and `plans/2026-07-13 v8 next steps — …gm+nc density….md` §1. Not started. Not assigned.
- **Repo cleanup still open:** (a) msens: after the migration, remove worktrees `main-merge`, `r4-f`, and
  `../msens-density`; back up the stale diff in `msens/` (`git -C msens diff > …patch` + the untracked files), then
  check out `main` there; delete branches `atlas-contract-fixes`, `density-transforms`, `r4-f-v7-native` (all merged).
  (b) workflows: remove worktree `.claude/worktrees/r4-f` (its `tmp/` holds the first dry run; superseded by
  `~/_big/msens/derived/asset_store/`), delete `r4-f-v7-native`. (c) docs: remove the `asset-store` worktree after
  the merge. (d) `apps/` is being retired: its per-version branches (`v2`–`v7`, …) were left alone on purpose.
- **Plans:** `plans_todo/` now holds v10 (5 files), `2026-09-20 v7.1 plan.md` (v7b is still a restricted
  prerelease), `atlas-8`, `atlas-9` (progress logs inside; not re-assessed this session), the asset-store plan, this
  handoff, and `atlas-refs/` (parity pages, report spec, calcofi review, `round4-session/`). When the migration is
  done, move the asset-store plan, this file and `atlas-refs/round4-session/` to `plans_done/`.
- **Site repo:** the bureau's guide PDF is now served publicly at `/branding/` because the brand page links it (Ben
  approved making the page public; he was told the PDF comes with it).

## 8. How to resume as orchestrator

Start in `atlas/` with `--add-dir ../workflows ../msens ../docs`. Ask Ben where the runbook stands. If the store is
live: do §5 B, C, D and §6 items 1–3 (one builder each; briefs in the style of
`atlas-refs/round4-session/common.md`), then §7. If it is not: nothing in §5 B–E or §6 item 5 can proceed; §6 items
1–4 and the gm/nc notebooks are independent and can start. To coordinate with the release session use `SendMessage`
to `migrate-native-assets-s3`; it treats messages as information and confirms writes with Ben.
