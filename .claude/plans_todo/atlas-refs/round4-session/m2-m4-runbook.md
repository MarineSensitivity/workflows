# M2–M4 runbook: put the species files in one store and re-point every release at it

Prepared overnight 2026-10-01/02 by the release session (`migrate-native-assets-s3`). **Nothing in this file has been run.**
Every command below is read-only until step 1; nothing is pushed to git; `latest.txt`, `versions.json` and every
`{ver}/native/` file are never touched. When Ben says go, run the steps in order. Each step has its own check; do not start
the next until the check passes.

## 0 · What this does, in five lines

- 86,351 files go into one store, `cog/global05/` and `native/{dataset}/`, named by what they contain: **83,417 server-side
  copies** of files already in the bucket (17.69 GB, no download) and **2,934 repainted rasters** uploaded from this laptop
  (1.96 GB), because the old ones do not match the data that was scored.
- A catalog, `marine-atlas/assets.parquet` (110,017 rows), lists every file in the store, including the 23,666 already there.
- Each release's pointer table is rewritten to name store files: v9 (restoring 6,753 lost vector rows), v8, and v7/v7b (which gain
  the original next to the interpolated surface for 10,120 inputs).
- The app's shards for each release are rebuilt from those tables; the proof that only pointers change is in §7.
- Afterwards the bucket has the same files stored three times (store, `v8/native/`, `v9/native/`). The old copies are deleted only
  at M6, after a two-week soak, with a separate go.

## 1 · Decisions only Ben can make (read before step 1)

**D1. Bucket-wide versioning.** The bucket `oceanmetrics.io-public` is **shared**: besides `marine-atlas/` it holds `backups/`,
`gazetteer/`, `issues/` and root files (`calcofi.duckdb`, `indicators.pmtiles`, …). Versioning cannot be scoped to a prefix:
turning it on versions every project's prefix, and it can later be *suspended* but never removed. Today versioning is off and
there is **no lifecycle configuration** (checked 2026-10-02). The rule in `commands/lifecycle.json` has an empty prefix, so it
expires noncurrent versions and delete markers after 30 days **for every project's prefix** (and aborts incomplete multipart
uploads after 7 days). Nothing existing is deleted by it; only versions created *after* it is enabled can expire.
- **A (recommended, and what the plan decided):** bucket-wide versioning + the 30-day rule. Cost stays bounded for every project.
- **B:** versioning + a rule scoped to `marine-atlas/` only. Other prefixes then keep noncurrent versions forever (cost grows).
- **C:** no versioning. The only safety nets are the local backups in step 2 and the fact that nothing old is deleted before M6.
  Choose C only if you do not want to touch the shared bucket; the store itself is append-only either way.

**D2. Five v7 `rng_iucn` originals the cell test rejects** (they stay OUT of the staged v7 table; those inputs keep only
their gridded surface). Score = share of the v7 surface's cells within 25 km of the IUCN range polygon (accept ≥ 0.95):

| v7 model | species | IUCN id | score | verdict |
|---|---|---:|---:|---|
| 19443 | Oncorhynchus nerka | 135301 | 0.08 | clear rejection |
| 19616 | Carcharhinus obscurus | 3852 | 0.10 | clear rejection |
| 19724 | Pristis pristis | 18584848 | 0.00 | clear rejection |
| 19621 | Carcharodon carcharias | 3855 | 0.85 | borderline: likely the same range, Ben decides |
| 51921 | Trichechus manatus | 22103 | 0.86 | borderline: likely the same range, Ben decides |

To accept the two borderline ones, re-stage before step 7 with `PS_ACCEPT_MDL_SEQ=19621,51921` (§7a). Default: none.

**D3. Three `am` surfaces that shrink when repainted.** Scoring used the current rows, so the repaint is right, but the maps
Ben has seen for these will change a lot (cells in the old COG → cells in the current rows):

| model | old COG | current rows |
|---|---:|---:|
| `am|ITS-Mam-180451` | 9,008,300 | 10,269 |
| `am|SLB-190137` | 1,111,001 | 134,164 |
| `am|W-Pyc-134687` | 2,028,655 | 129,951 |

They are an older, larger surface of the same species (the current rows lie inside the old footprint and differ in value);
not a shifted neighbour. Nine more gross failures *are* shifted neighbours (the positional-`mid` bug: a COG holding another
model's current surface): Fis-31618→Fis-31621, Fis-25651→Fis-25652, Fis-34084→Fis-34101, Fis-26145→Fis-26146,
Fis-59151→Fis-59819, SLB-70139→SLB-70176, W-Msc-216452→W-Msc-216447, W-Ase-368790→W-Ase-368952, W-Pyc-150542→W-Pyc-134750.

**D4. Already decided (Ben, 2026-10-01):** repaint every object whose pixels do not match its rows; restore v9's vector `model`
rows; dedupe the turtle rows by the value the merge consumed (max); `rng_iucn` on v7 by exact unambiguous name + a check that
the original holds the cells v7 draws; PMTiles from S3 only; versioning + 30-day rule before the first store write; two-week
soak then prune; gm/nc go into the v10 bootstrap.

## 2 · Preconditions (P0, five minutes, read-only)

```bash
cd /Users/bbest/Github/MarineSensitivity/workflows
aws sts get-caller-identity                                  # account 814665782451, user ben
df -h ~ | tail -1                                            # >= 20 GB free (staging is 1.8 GB; backups 0.3 GB)
git log --oneline -1                                         # workflows main: the M3 commits (local, unpushed)
Rscript -e 'cat(as.character(packageVersion("msens")), "\n")'   # 0.50.0
ls ~/_big/msens/derived/asset_store/stage/staging | wc -l   # 2934
Rscript -e 'cat(nrow(arrow::read_parquet(path.expand("~/_big/msens/derived/asset_store/stage/store_plan.parquet"))), "\n")'  # 86351
uptime; sysctl vm.swapusage                                  # load < 4, swap < 3 GB (close big browser tabs first)
```
If msens is not 0.50.0: `Rscript -e 'devtools::install("../msens/.claude/worktrees/main-merge")'`. Everything below runs from this
directory with `TMPDIR=$PWD/.tmp`. The auto-mode classifier refuses production publishes from the orchestrator: **Ben runs the
steps that write with the `!` prefix** (or grants a narrow permission rule).

## 3 · The steps

### Step 1 — bucket protection (D1)  ·  <1 min  ·  needs Ben's go
```bash
CONFIRM_BUCKET_WIDE_VERSIONING=yes sh ~/_big/msens/derived/asset_store/stage/commands/00_bucket_versioning.sh
```
Expected: `--- before:` prints nothing (versioning off); after: `{"Status": "Enabled"}` and one rule `expire-noncurrent-and-delete-markers-30d`.
The script refuses without the variable and **aborts if any lifecycle configuration exists** (a `put` replaces the whole thing:
merge the rule in by hand instead). *If it fails:* `AccessDenied` → check the IAM policy for `s3:PutBucketVersioning` /
`s3:PutLifecycleConfiguration`; do not continue with D1=A without it (take D1=C). *Undo:* `aws s3api put-bucket-versioning
--bucket oceanmetrics.io-public --versioning-configuration Status=Suspended` and `aws s3api delete-bucket-lifecycle --bucket
oceanmetrics.io-public` (existing versions stay).

### Step 2 — back up what will be overwritten (read-only from S3)  ·  ≈5 min  ·  ≈0.3 GB
```bash
PS_BACKUP=1 PS_VERS=v9,v8,v7b,v7 TMPDIR=$PWD/.tmp quarto render stage_publish.qmd
```
Copies each release's `manifest.json`, `tables/native_asset.parquet` (v7/v7b have none) and the whole `app/` tree into
`~/_big/msens/derived/asset_store/publish_stage/_backup/{ver}/`. Expected app objects: v9 940, v8 940, v7b 945, v7 945 (52–57 MB each).
Check: `for v in v9 v8 v7b v7; do find ~/_big/msens/derived/asset_store/publish_stage/_backup/$v/app -type f | wc -l; done`.

### Step 3 — the 83,417 server-side copies  ·  estimated 45–90 min  ·  17.69 GB inside S3, nothing downloaded
```bash
sh ~/_big/msens/derived/asset_store/stage/commands/10_copy.sh        # log: _output/logs/store_copy.log
```
16 parallel `aws s3 cp s3://… s3://…`; each new key is a copy of an existing consistent object (newest release first). The time is an
**estimate** (request latency 0.4 s from here; not measured, because measuring means writing). The step is additive: nothing is
overwritten (keys are new), so it can be interrupted and re-run. *Check:* the script exits non-zero if the log has a `FAILED` line.
*If some fail:* `Rscript scripts/verify_asset_store.R ~/_big/msens/derived/asset_store/stage/assets.parquet --write-missing /tmp/m.txt`
then `grep -F -f /tmp/m.txt commands/copy.tsv > commands/copy_retry.tsv` and run the xargs line of `10_copy.sh` on that file.

### Step 4 — the 2,934 repainted rasters  ·  estimated 10–20 min  ·  1.96 GB uploaded
```bash
sh ~/_big/msens/derived/asset_store/stage/commands/20_upload.sh      # log: _output/logs/store_upload.log
```
8 parallel uploads with `Content-Type: image/tiff`. Every file was decoded after painting and equals the quantised rows its key
promises (2,934 of 2,934 verified; 6 turtle ranges take `max` across their two source files). Uplink speed is unknown (assumed
10–20 MB/s). *If some fail:* same retry recipe with `upload.tsv`.

### Step 5 — verify the store against the catalog  ·  ≈4 min  ·  read-only
```bash
Rscript scripts/verify_asset_store.R ~/_big/msens/derived/asset_store/stage/assets.parquet --sample 200
```
Expected last line: `VERIFIED: the store prefixes match the catalog` (exit 0): 110,017 catalogued objects listed under
`cog/usa05`, `cog/global05`, `native`; 0 missing, 0 unexpected, 0 size differences, single-part ETag = md5 everywhere, 200 sampled
HEADs all 200 with the right content type. The two generated `index.html` pages in `cog/usa05/` and `cog/global05/` are listed as
*ignored*, not flagged. *If it fails:* it prints the first offenders; missing → retry (steps 3/4); **unexpected** or **size differs**
→ stop and tell the release session (an object exists under a key the plan did not write).

### Step 6 — publish the catalog  ·  1 min
```bash
sh ~/_big/msens/derived/asset_store/stage/commands/30_catalog.sh
```
Uploads `marine-atlas/assets.parquet` (public, additive). Then the three READMEs staged in `publish_stage/` (`README.md`,
`cog/README.md`, `native/README.md`). The first two **overwrite** existing files, so back them up first:
```bash
S=~/_big/msens/derived/asset_store/publish_stage; B=s3://oceanmetrics.io-public/marine-atlas
aws s3 cp $B/README.md $S/_backup/README.md.old --only-show-errors && aws s3 cp $B/cog/README.md $S/_backup/cog_README.md.old --only-show-errors
aws s3 cp $S/README.md $B/README.md --content-type text/markdown --only-show-errors
aws s3 cp $S/cog/README.md $B/cog/README.md --content-type text/markdown --only-show-errors
aws s3 cp $S/native/README.md $B/native/README.md --content-type text/markdown --only-show-errors   # new file
```
(`native/README.md` is new; `marine-atlas/native/` currently answers 403 for it. Versioning, if enabled in step 1, also keeps the old text.)

### Step 7 — per release, in this order: **v9, v8, v7b, v7** (public default last)  ·  ≈35 min each ≈ 2.5 h total

For each `V`:

**7a. (optional, only for D2 or after any change) re-stage** — read-only, ≈3 min:
`PS_VERS=$V PS_ACCEPT_MDL_SEQ=19621,51921 TMPDIR=$PWD/.tmp quarto render stage_publish.qmd` (omit the variable to accept none).

**7b. publish the pointer table and the manifest patch** — 4 min (it first re-runs the step-5 verifier and stops if it fails):
```bash
PS_VERS=$V PS_PUSH=1 TMPDIR=$PWD/.tmp quarto render stage_publish.qmd
```
Uploads exactly two keys: `{V}/tables/native_asset.parquet` and `{V}/manifest.json` (v9/v8: manifest unchanged; v7/v7b: `tables.native_asset`
added and `capabilities.native_representation` false→true). Allow-listed; anything else is refused. At this moment the release's
shards still hold the OLD asset URLs, which still resolve (nothing old is deleted), so the app keeps working.

**7c. build and publish the app shards** — ≈25–30 min (945 single `aws s3 cp` calls, `boot.json` last, `built_at` bumped):
```bash
APP_BUNDLE_NATIVE_ASSET_DIR=$HOME/_big/msens/derived/asset_store/publish_stage APP_BUNDLE_S3=1 \
  TMPDIR=$PWD/.tmp nohup scripts/render_app_bundle.sh $V > _output/logs/render_app_bundle_$V.driver.log 2>&1 &
cat _output/logs/render_app_bundle_exit_codes.txt            # "<V> 0" when done; tail -f _output/logs/render_app_bundle_$V.log
```
The notebook reads the staged pointer table (its log says `native_asset is the STAGED pointer table …`), re-fetches the *live* manifest
(which now has step 7b's keys) and patches only `app{}` on top, pushes `{V}/app/**` then the manifest, and runs its own anonymous
verification (200, gzip, size budget). Its gates HEAD the store objects, which is why this must come after steps 3–6.
**First real run of the override:** it was dry-run-tested on v7 (identical shards to the staged ones) but never against S3.
*If it fails before pushing:* nothing changed except step 7b's two keys; fix and re-run. *If it fails mid-push:* re-run (idempotent;
`boot.json` is uploaded last so visitors keep the old contract until it lands).

**7d. check the published release** — ≈3 min:
```bash
Rscript scripts/check_release_pointers.R $V --n 200 --shards 20
```
Expected `PASS: $V points only at catalogued store objects that answer`: 0 pointers off the store, 0 missing from the catalog, 200/200 HEADs,
20/20 shards valid, 0 URLs off the store; inputs with both representations in the sample > 0 (v7/v7b/v9). Also `curl -s
https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/$V/app/boot.json --compressed | jq .built_at` is newer than before.

**7e. look at it** (Ben): v9 and v7b on the review host (Cloudflare Access), v8 likewise, **v7 on the public Atlas in a private window**
(the browser caches by `built_at`): `https://marinesensitivity.org/atlas/`, a species input such as *Marbled Murrelet* → BirdLife: the
**Original | Interpolated** toggle appears and Original draws a PMTiles range from `native/bl/…pmtiles` on S3. Pause after v9: if it looks
wrong, stop and roll back (below) before v8/v7b/v7.

**What each release should show after 7b–7d** (from the dry run, all against the published bundle):

| release | pointer rows | inputs with both representations | taxa with changed inputs | largest taxon shard (gzip, contract 25,600) |
|---|---:|---|---:|---:|
| v9 | 86,857 → 93,610 (+6,753 restored) | 29,227 → 35,980 | 21,599 of 21,600 | 14,351 B |
| v8 | 72,484 → 72,484 | 25,450 → 25,450 | 21,581 of 21,581 | 11,193 B |
| v7b | 10,247 asset rows → 20,367 (49,331-row table) | 0 → 10,120 | 9,424 of 16,153 | 6,055 B |
| v7 | same as v7b | 0 → 10,120 | 9,424 of 16,153 | 6,054 B |

Every taxon changes **only** in `inputs[].assets` and its own `merged.url` (0 taxa differ elsewhere, all four releases); alias shards are
identical in content; every shard validates against the schema.

**New field `source_key` (msens 0.50.0), on every shard asset beside `source_layer`.** The Atlas draws a range by filtering the PMTiles
tile's features on the key they carry. v8/v9 stamp the input's own `mdl_key` (`bl|22694870`); v1–v7 key the same input by `mdl_seq`
(`17626`), so without it the v7 Original drew nothing (found by the orchestrator running the live Atlas against the staged v7 bundle).
`source_key` is the feature key for a PMTiles asset (6,753 of 6,753 in v9 and v8; 911 of 911 in v7 and v7b) and `null` for a COG. For
v8/v9 it equals the input's `mdl_key`, so the old and new Atlas builds both work with old and new shards. The comparison "builder vs the
published bundle" is therefore *identical except for this one field*.

**bbox.** A PMTiles asset's `bbox` is `null` for a range that spans the globe (the builder publishes no bbox for a whole-world box; Marbled
Murrelet is one, in v8 too). Checked across the board: all 10,120 v7 (and v7b) native assets carry exactly the same bbox as the same store
file in the v8/v9 shards, so there is no v7-specific gap.

### Step 8 — leave the old files alone
`v8/native/`, `v9/native/` and the file host's `pmtiles/{v8,v9}/` stay exactly as they are (≈33 GB). They are deleted only at **M6** after a
two-week soak, when `store_unreferenced()` and a HEAD sweep of every pointer are clean, with a separate explicit go.

## 4 · If something goes wrong: rollback by step

| step | what changed | undo |
|---|---|---|
| 1 | versioning + lifecycle | `Status=Suspended`; `delete-bucket-lifecycle` (versions stay) |
| 3, 4 | new, unreferenced keys under `cog/global05/` and `native/` | none needed (they harm nothing); prune later with `store_unreferenced()` |
| 6 | `assets.parquet`, READMEs | `aws s3 rm` the catalog (nothing reads it yet) / restore the READMEs from `_backup/` or the previous version |
| 7b | `{V}/tables/native_asset.parquet`, `{V}/manifest.json` | `aws s3 cp publish_stage/_backup/$V/manifest.json s3://…/$V/manifest.json --content-type application/json --cache-control no-cache`; same for `tables/native_asset.parquet` (v7/v7b had none: `aws s3 rm`) |
| 7c | `{V}/app/**` | `aws s3 sync publish_stage/_backup/$V/app/ s3://…/$V/app/` then re-upload `boot.json` last; bump nothing else. Versioning also keeps the previous object versions for 30 days |

Because the old `{ver}/native/` files and file-host PMTiles are never removed before M6, a restored pointer table or shard resolves immediately.

## 5 · What is NOT done, and not part of this

- `publish_native.qmd`'s store chunks still carry the 0.46 bytes-MD5 fallback: **do not run `publish_native.qmd`** until M5 rewrites it to write
  only to the store (M5 also adds the publish gate and the bucket-README text).
- Nothing is pushed to git: workflows `main` is 30 commits ahead of `origin/main` (some are the atlas orchestrator's), msens `main` 11
  ahead (0.46.0 → 0.50.0, includes the density merge as 0.48.0). Pushing is Ben's call.
- Atlas captions ("on the 0.05° scoring grid" for single-representation inputs) and the docs "Asset store" section come after step 7.
- The turtle DPS duplicated rows (every cell of the 6 ranges is in two files; up to 50 levels apart) are a **v10 ingest defect** to fix
  in `ingest_turtles-swot-dps.qmd`; the store takes the value the merge consumed.
- The 17 `gm` objects under `v8/native/gm/` are out of scope (gm/nc are folded in at the v10 bootstrap).

## 6 · Files

| what | where |
|---|---|
| plan (one row per store key: copy / upload) | `~/_big/msens/derived/asset_store/stage/store_plan.parquet` |
| catalog it yields (110,017 rows) | `…/stage/assets.parquet` |
| object → store key (every v8/v9 object) | `…/stage/object_keys.parquet`, `…/asset_store/store_migration.parquet` |
| verdict per raster object | `…/asset_store/verify_raster.parquet`, failure diagnosis `…/stage/diagnose.parquet` |
| repaints (staged, verified) | `…/stage/staging/` (2,934 files) and `…/stage/repainted.parquet` |
| commands (not run) | `…/stage/commands/` (`00_…`, `10_copy.sh`, `20_upload.sh`, `30_catalog.sh`, `copy.tsv`, `upload.tsv`, `lifecycle.json`) |
| staged releases | `…/asset_store/publish_stage/{v9,v8,v7b,v7}/{tables,app,manifest.json}`, root `assets.parquet`, READMEs |
| notebooks (rendered records in `_output/`) | `map_asset_store.qmd` (M1), `stage_asset_store.qmd` (M2-prep), `stage_publish.qmd` (M3 dry run + flagged backup/push) |
| scripts | `scripts/verify_asset_store.R`, `scripts/check_release_pointers.R` |
| G1 report | `.claude/plans_todo/atlas-refs/round4-session/m1-g1-report.md` |

## 7 · Run log (2026-10-02, oversight session, with Ben's go; D1 = A, D2 = none)

| step | result |
|---|---|
| P0 | account, disk (189 GB), 2,934 staged, 86,351 plan rows ok; msens 0.51.0 (superset of 0.50.0); laptop load 12–19 and swap 10.6 GB at the start (another session's R jobs), ~3.6 by step 7 |
| 1 | versioning `Enabled`; rule `expire-noncurrent-and-delete-markers-30d` |
| 2 | backups: app objects v9 940, v8 940, v7b 945, v7 945; manifests 4; `native_asset` v9, v8 |
| 3 | **deviation in method, not content:** `10_copy.sh` (one `aws s3 cp` process per object) made 365 copies/min = 3.8 h; stopped after 1,062. `scripts/store_copy.py` made the same CopyObject calls from one process: 82,313 in 374 s, 0 failed; the 42 multipart sources (> 8 MiB) went through `aws s3 cp` as the runbook had them. A second pass: 83,417 present at the planned size, 0 conflicts |
| 4 | 2,934 uploads, 0 failed |
| 5 | `VERIFIED`: 110,017 listed = 110,017 catalogued; 0 missing / unexpected / size / md5; 200 HEADs ok |
| 6 | `assets.parquet` public (110,017 rows); three READMEs; old ones in `_backup/*.old` |
| 7 v9 | 7b pushed `native_asset` + manifest (gate: 93,610 store, 0 vm_bulk); 7c 21 min, shards gate 84,780 store / 0 vm_bulk / 11 external, `built_at` 2026-10-02T13:58:32Z; 7d `PASS` (200/200 HEADs, 20/20 shards, 2,691 of 2,694 sampled inputs with both representations). **Paused for Ben's look (7e) before v8, v7b, v7.** |

Also on the bucket today: root `calcofi.duckdb` deleted at Ben's request after step 1 (recoverable for 30 days as a
noncurrent version); S3 access logging and request metrics enabled by `server/aws/guardrails.sh`.
