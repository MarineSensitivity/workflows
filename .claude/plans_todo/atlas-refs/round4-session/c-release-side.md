# Round 4 — release-side: native species layers on the public release (finding, 2026-09-30)

> **SUPERSEDED IN PART (2026-10-01).** The "Store", "Existing objects to copy", "Runbook" and "Kickoff" sections below
> describe a `native/{ds}/{md5}` copy for v7 only. Do NOT run that copy batch. The store design and the migration are
> now `plans_todo/2026-10-01 asset store — every distribution file stored once, referenced by every release.md`
> (one content-addressed store for every release, keys from source content, registries as pointers, then prune).
> The finding and the coverage numbers in this file still stand.


Ben (2026-09-30): "Somehow the native Species distribution layers got dropped from the original Shiny
`apps/species/` app and its porting over into this atlas app. At some point … the Shiny app showed the original
raster or vector layer (from PMTiles) and the interpolated values (onto 0.05 deg raster cells). We need to restore
that into this atlas app."

## What was checked
One published taxon shard per release (`…/marine-atlas/{ver}/app/taxon/00.json`), counting assets by
(representation, type):

| release | access     | inputs | with both reps | native pmtiles | native cog | model cog |
| ------- | ---------- | ------ | -------------- | -------------- | ---------- | --------- |
| v7      | public     | 47     | 0              | 0              | 41         | 0         |
| v7b     | restricted | 47     | 0              | 0              | 41         | 0         |
| v8      | restricted | 99     | 99             | 26             | 73         | 99        |
| v9      | restricted | 135    | 109            | 26             | 109        | 109       |

## What it means
- The atlas app has the feature: `src/lens/species/data/layerBar.ts` offers the Original | Interpolated
  (or Delivered | As ingested) toggle when an input publishes both representations (`reps.size > 1`), and
  `mapInputs.ts` draws a PMTiles range or a COG accordingly. Same logic as `apps/species/app.R`'s `pick_asset()`.
- The PUBLIC release (v7, `latest.txt`) publishes exactly one asset per input: the per-model COG on the scoring grid,
  labelled `rep: "native"`. That is the v1–v7 adapter (`app.R` ~l.397: "model_asset IS the v1-v7 native_asset").
  There is no vector-range PMTiles row and no second representation, so neither app can show an original next to an
  interpolated surface on v7. The label is also misleading: that COG is the gridded surface, not the source.
- v8 and v9 carry both. Not verified in a browser on the preview host this session.

## Options (owner + release session decide; the atlas session does not write to the bucket)
1. **Make a release that carries both the public default** (promote v9 when it is ready). No app work.
2. **Backfill v7's app bundle** (`app/taxon/*.json`) with the original surfaces, if v7-era originals exist as
   public PMTiles/COGs. Needs `msens` app-bundle work + `APP_BUNDLE_S3` by the release session.
3. **App-side, small, optional**: on a release whose inputs have one representation, say so in the species Layers
   tab ("This release publishes the gridded surface only") and caption the v1–v7 asset "on the 0.05° scoring grid"
   rather than "as delivered".

## Decision + dry run (2026-09-30, Ben: "backfill v7"; "originals sourced outside the versioned outputs")

Built (dry run only, nothing published): msens branch `r4-f-v7-native` @ f3b2f26 (0.46.0: `native_key()`,
`native_url()`, `native_store_index()`, `native_hash_file()`, `native_store_rewrite()`, `native_asset_backfill()`;
`devtools::test()` green) and workflows branch `r4-f-v7-native` @ f7173b69 (`publish_native.qmd` store design +
chunks, `backfill_versions.qmd` `BACKFILL_NATIVE=<ref> BACKFILL_NATIVE_MAP=<parquet>`, `scripts/native_store_map.R`).
Both worktrees under `.claude/worktrees/r4-f/`; dry-run output in `.claude/worktrees/r4-f/tmp/`.

**Store** (unversioned, content-addressed, like `cog/{grid}/{hash}.tif`): S3 `marine-atlas/native/{ds_key}/{hash}.tif|.pmtiles`;
file host `pmtiles/native/{ds_key}/{hash}.pmtiles` (`/share/data/derived/pmtiles/native/` on msens). Hash = source
content (16 hex + encoding tag) for new publishes; MD5 of the object bytes (32 hex) for the existing v8/v9 objects
and for rng_iucn. `native_asset.asset_url` holds the store URL, no `?v=`.

**Coverage:** v7 inputs with a gridded COG 19,811; matched to a v8/v9 original 17,810 (89.9%). Unmatched: rng_iucn
1,518 (v7 keys by name, v8 by IUCN id — no name join), bl 358, rng_fws 60, ch_fws 14, am 45. After the backfill:
12,120 inputs → 9,436 with both representations (today 0), 811 model-only, 1,873 no asset. Only `inputs[].assets`
changes in the shards; alias shards identical; largest shard 6.3 KB gzip (contract 25.6 KB).

**Existing objects to copy:** 17,499 native COGs (149.3 MB, all single-part, ETag == MD5 verified on 10 downloads)
+ 316 PMTiles on the file host (Caddy ETag is not an MD5: the server must `md5sum` them first — the mapping's
PMTiles hashes are PROVISIONAL until then). Mapping table + generated commands:
`.claude/worktrees/r4-f/tmp/native_store_map.parquet` and `…parquet.commands.sh` (18,139 lines, not run).

**Runbook (release session; flags only, none run here):**
1. `ssh msens 'find /share/data/derived/pmtiles/v8 /share/data/derived/pmtiles/v9 -name "*.pmtiles" -print0 | xargs -0 md5sum' > pmt_md5.txt`,
   then `Rscript scripts/native_store_map.R out.parquet heads_cog_all.tsv --pmt-md5 pmt_md5.txt`.
2. Run `…commands.sh`: 17,499 server-side `aws s3 cp` (v8/v9 native → `native/{ds}/{md5}.tif`), the PMTiles S3
   mirror copies, and on msens `cp -n pmtiles/{ver}/{ds}/{id}.pmtiles pmtiles/native/{ds}/{hash}.pmtiles`.
   Copy, never move (v8/v9 keep working until re-pointed).
3. `BACKFILL_NATIVE=v8 BACKFILL_NATIVE_MAP=<parquet>` on `backfill_versions.qmd -P ver=v7` → `native_asset.parquet` + view.
4. Rebuild the v7 manifest (`tables.native_asset`, `capabilities.native_representation`).
5. `APP_BUNDLE_VERS=v7 APP_BUNDLE_S3=1` on `build_app_bundle.qmd`; upload `tables/native_asset.parquet`.
6. Recommended in the same operation: re-point v8 and v9's `native_asset` via `native_store_rewrite()`, republish
   their `app/` shards and manifests (shards hard-code URLs; OPFS cache key is `built_at`), so all releases agree and
   the versioned originals can later be pruned.
7. Future releases: `publish_native.qmd` writes to the store directly.

## Kickoff (a dedicated release session)

Start a NEW Claude Code session in the workflows checkout, model Sonnet 5.5 (Opus 5.5 if you want more judgement
on the merge sequencing), with the two other repos attached:

```
cd /Users/bbest/Github/MarineSensitivity/workflows && claude --add-dir ../msens --add-dir ../atlas
```

Paste this as the first message:

> You are the release session for round 4's v7 native backfill. Read, in order: (1)
> `workflows/.claude/plans_todo/atlas-refs/round4-session/c-release-side.md` (this file: finding, design, coverage,
> runbook), (2) `workflows/.claude/skills/publish-sdm/SKILL.md`, (3) `workflows/CLAUDE.md`'s app-bundle section and
> `/Users/bbest/Github/CLAUDE.md`. The code is on two branches, dry-run tested to the shard build but never rendered
> for publishing: msens `r4-f-v7-native` @ f3b2f26 (worktree `msens/.claude/worktrees/r4-f`, version 0.46.0) and
> workflows `r4-f-v7-native` @ f7173b69 (worktree `workflows/.claude/worktrees/r4-f`; dry-run output + mapping table in
> its `tmp/`). Work in this order and STOP for my go at each "GATE":
> 1. Preconditions: `aws sts get-caller-identity` works; `ssh msens true` works; the msens main checkout's uncommitted
>    0.45.0 — tell me what it is and propose how to sequence it against 0.46.0 (rebase r4-f onto it, or the reverse);
>    GATE.
> 2. Merge msens `r4-f-v7-native` (after sequencing), `devtools::document(); devtools::test()` green, install it
>    wherever the render will run (laptop and/or server — say which); merge workflows `r4-f-v7-native` into main.
> 3. Hash the PMTiles on the server (runbook step 1) and rebuild the mapping table with real hashes; report counts and
>    the number of provisional rows that changed; GATE.
> 4. Run the copies (runbook step 2): the S3 server-side `aws s3 cp` batch and the file-host `cp -n` batch (batch the
>    316 cp calls per dataset into one ssh each). Verify by sampling 20 store URLs (anonymous HEAD) and
>    `native_store_index()`; GATE.
> 5. `BACKFILL_NATIVE=v8 BACKFILL_NATIVE_MAP=<parquet>` on `backfill_versions.qmd -P ver=v7`; rebuild the v7 manifest;
>    a DRY RUN of `build_app_bundle.qmd` for v7 (flag unset) and diff the shard counts against the numbers in this file
>    (9,436 inputs with both reps; 0 taxa differ outside `inputs[].assets`); GATE.
> 6. Publish: `APP_BUNDLE_VERS=v7 APP_BUNDLE_S3=1` (the way it was last run:
>    `TMPDIR=$PWD/.tmp APP_BUNDLE_S3=1 nohup scripts/render_app_bundle.sh v7 > _output/logs/… &`), upload
>    `tables/native_asset.parquet`, then re-point v8 and v9 (runbook step 6) and republish their `app/` + manifests.
>    Never `PROMOTE_LATEST` (v7 is already latest); never touch `latest.txt` or `versions.json`.
> 7. Verify on the public atlas (`https://marinesensitivity.org/atlas/` — an incognito window: the OPFS cache is keyed
>    by `built_at`): pick a species input such as Marbled Murrelet → BirdLife; the "Original | Interpolated" toggle
>    appears and Original draws a PMTiles range. Report what you saw. Commit the merged branches; push only after I say.
>
> Rules: one thing at a time, each GATE is a stop; no `git add -A`; every publishing step under its named flag only;
> log every render to `_output/logs/`; if any count disagrees with this file, stop and show me the diff.
