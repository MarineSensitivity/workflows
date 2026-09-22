# `{ver}/app/` publish plan — all eleven registered releases

Generated 2026-09-22 from a real dry run of `build_app_bundle.qmd` @ `b95b34e1` (`APP_BUNDLE_S3`
unset throughout — nothing has been pushed). Scope per Ben's decision (2026-09-21, verbatim):
"yes run again until all bugs are fixed; publish from the laptop; go after A is fixed and make all
versions with all uploads available." Every number below is measured from this notebook's own
written `_output/app_bundle/{ver}/` directories and `boot.json`/`taxa.json`, not estimated.

## D17 — why only ONE unit type is drawn per release, never subregion/ecoregion

Ben's decision D17 (master plan, 2026-09-22, supersedes D16 in full): "drawable units are Program
Areas only (v2-v9), Planning Areas on v1 — no subregion or ecoregion unit on any release." So this
notebook resolves and hands `app_bundle_build()` exactly ONE geometry per release (`geom_keys`),
and `app_units()` (msens 0.43.0 @ `7d8c6c0`, `APP_UNIT_TYPES <- c("programarea","planarea")`,
first match wins) publishes at most one unit. Subregion and ecoregion zones are still SCORED (they
carry real `zone_taxon` rows on most releases — see the manifest-identity gate note below) but are
never drawable choropleths; the app's picker offers only the one D17 unit.

## Per-release: objects, bytes, tables, unit

| ver | objects | bytes | MB | tiles | tables (`boot$tables`) | unit (type=n_keys) | n_taxa |
|---|---|---|---|---|---|---|---|
| v1  | 945 | 64,446,786 | 61.5  | 428 | taxon,zone_taxon,taxonomy,cell       | planarea=36    | 17,348 |
| v2  | 945 | 66,895,892 | 63.8  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 | 17,353 |
| v3  | 945 | 54,510,423 | 52.0  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 |  9,795 |
| v4  | 945 | 53,694,372 | 51.2  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 |  9,795 |
| v4b | 945 | 53,618,964 | 51.1  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 |  9,795 |
| v5  | 945 | 53,626,208 | 51.1  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 |  9,795 |
| v6  | 945 | 53,242,775 | 50.8  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 |  9,424 |
| v7  | 945 | 61,315,194 | 58.5  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 | 16,153 |
| v7b | 945 | 58,275,405 | 55.6  | 428 | taxon,zone_taxon,taxonomy,cell       | programarea=20 | 16,153 |
| v8  | 940 | 79,164,363 | 75.5  | 422 | taxon,zone_taxon,taxonomy,model,cell | programarea=20 | 21,581 |
| v9  | 940 | 86,998,989 | 83.0  | 422 | taxon,zone_taxon,taxonomy,model,cell | programarea=20 | 21,600 |

**Grand total (app/ only, all 11 releases): 10,385 objects, 685,789,371 bytes (654.0 MB).**

`model.parquet` exists only for v8/v9 (the `mdl_id -> mdl_key` mapping app_model() needs; v1-v7b
have no such mapping to give — a capability, not a failure). Every release passed every gate this
notebook runs; where a release cannot supply a capability (e.g. no `cell_model`), the relevant gate
SKIPS with a logged reason (`app_bundle_gate_note()`/`app_bundle_gate_coverage()`) rather than
failing — see each release's own `_output/build_app_bundle_{ver}.html` for its gate-coverage table
and `n_gates_skipped` line. The one skip pattern common to every release in a normal single-version
render is the cell_model plan gate (`validate-cell-model-plan`), which only runs non-trivially for a
version named in `APP_BUNDLE_CELLMODEL_VERS` — see the v7/v7b plan below, computed with that flag set.

## v7 + v7b `serve/cell_model/` upload plan

v7 and v7b advertise `cell_species_list: true` in their published `manifest.json` from a
server-only `cell_model` table that has never reached S3 (per-cell species lists, 428 tiles each).
Computed with `APP_BUNDLE_CELLMODEL_VERS` set to each version in turn (dry run, `APP_BUNDLE_S3`
still unset — no upload happened):

| ver | n_tiles | n_keys | bytes | keys_ok | one_file_per_tile_ok | tile_width_ok (nc=3103) | already_on_s3 | clear_to_upload |
|---|---|---|---|---|---|---|---|---|
| v7  | 428 | 428 | 792,641,115 (755.9 MB) | TRUE | TRUE | TRUE | FALSE | **TRUE** |
| v7b | 428 | 428 | 788,655,748 (752.1 MB) | TRUE | TRUE | TRUE | FALSE | **TRUE** |

Both are clear to upload: every planned key matches the exact shape
`^{ver}/serve/cell_model/tile=[0-9]+/data_0\.parquet$`, one file per tile locally, the real tile
width (3103) checks out, and neither prefix exists on S3 yet (refuses to overwrite otherwise —
upload is all-or-nothing, never partial). v7b's prefix is `restricted` (matches its `versions.csv`
access), which Ben's "all uploads" instruction covers.

## Exact commands a publish turn runs, per release

Three invocations of the committed driver (`scripts/render_app_bundle.sh`), run ONE AFTER ANOTHER
— never concurrently: they share one `.qmd` and its intermediates (`.knit.md`,
`build_app_bundle_files/`), and two `quarto render`s of the same notebook at once is a proven
collision hazard, not merely a style preference. Never a raw `quarto render` and never a hand `aws`
command — the push chunk (`label: push`) IS the publish, gated by `APP_BUNDLE_S3`.

The driver truncates its exit-codes file (`_output/logs/render_app_bundle_exit_codes.txt`) at the
START of every invocation, so a publish turn of three invocations would silently erase invocation
1's result when invocation 2 starts unless each names its own. Fixed the script itself
(`scripts/render_app_bundle.sh`, committed) to take an `EXIT_FILE` env override, defaulting to the
old unnamed path when unset — chose this over a plan-only `cp` step because the hazard is in the
script, not the plan, and a `cp` after the fact does not stop invocation 2 from clobbering
invocation 1's file before anyone runs it.

```sh
# app/ only (v1, v2, v3, v4, v4b, v5, v6, v8, v9) -- 9 releases, no cell_model:
APP_BUNDLE_S3=1 nohup scripts/render_app_bundle.sh v1 v2 v3 v4 v4b v5 v6 v8 v9 \
  > _output/logs/render_app_bundle_publish.log 2>&1 &
# wait for this to finish (poll its exit-codes file) before starting the next line

# v7 ALSO uploads serve/cell_model/ (same push chunk, cm_vers-gated sync):
APP_BUNDLE_S3=1 APP_BUNDLE_CELLMODEL_VERS=v7 EXIT_FILE=_output/logs/render_app_bundle_exit_codes_publish_v7.txt \
  nohup scripts/render_app_bundle.sh v7 > _output/logs/render_app_bundle_publish_v7.log 2>&1 &
# wait for this to finish before starting the next line

# v7b, same as v7:
APP_BUNDLE_S3=1 APP_BUNDLE_CELLMODEL_VERS=v7b EXIT_FILE=_output/logs/render_app_bundle_exit_codes_publish_v7b.txt \
  nohup scripts/render_app_bundle.sh v7b > _output/logs/render_app_bundle_publish_v7b.log 2>&1 &
```

Each of the three invocations is judged by its OWN exit-codes file (named above) plus that
render's own `verify-anonymous` chunk (`label: verify-anonymous`, `eval: !expr do_s3`), which HEADs
every object it just wrote anonymously — no credentials, exactly as the browser will fetch it —
asserting 200, `content-encoding: gzip` on every JSON object, and a path-style URL. A publish is
not "done" until all three exit-codes files read 0 and all three verify-anonymous tables are clean.

## Total upload, one publish turn

```
app/ (11 releases, all versions)          685,789,371 B
serve/cell_model/ v7                      792,641,115 B
serve/cell_model/ v7b                     788,655,748 B
--------------------------------------------------------
TOTAL                                    2,267,086,234 B  (2.27 GB)
```

**Rough wall-clock** (back-of-envelope, not a promise): one v7 render, push-free, measured
end-to-end at 105 s (`_output/logs/render_app_bundle_driver_round2fix2.log`, 18:49:56 → 18:51:41).
Treating that as representative of "one render" and the turn as 11 renders total (9 in the app/
batch + v7 solo + v7b solo, sequential, per the never-concurrently rule above):

- compute: 11 × ~105 s ≈ 1,155 s ≈ **~19 min**
- upload: 2,267,086,234 B ÷ 10 MB/s (assumed upstream) ≈ 227 s ≈ **~4 min**
- **total ≈ 23 min**, plausibly 20–30 min given legacy releases actually render faster once their
  S3 table cache is warm (~75 s measured for v4-v6) and v8/v9 slower (~170–190 s measured in the
  prior `final11` run) — the 105 s figure is a single mid-range data point, not an average.

## Never touched by this notebook, at any time

`latest.txt`, `versions.json`, `{ver}/manifest.json`, `{ver}/tables/**` — `app_bundle_assert_prefix()`
refuses any S3 key outside `{ver}/app/` (or, for a version in `APP_BUNDLE_CELLMODEL_VERS`,
`{ver}/serve/cell_model/`) before it ever reaches `aws s3 cp`/`sync`. Promoting a version, changing
its `access`, or fixing a manifest is `build_version_manifest.qmd`'s and the release session's job,
never this notebook's.

## Cache-invalidation rule (must be restated, not just known)

Handed over by the atlas-2 review (2026-09-21): the browser caches under OPFS every object that
carries a digest in `boot.tables`, keyed by `boot.built_at` + path — but it ALSO caches objects
`app_bundle_build()` never digests (`tables/model.parquet` on v1-v7, every `serve/cell_model` tile)
under that same `built_at`. So **re-publishing ANY object the app reads — even one outside `app/`,
like `tables/model.parquet` or a `serve/cell_model` tile — requires rebuilding and re-publishing
`app/` too** (a fresh `built_at`), or browsers keep serving the old copy from OPFS indefinitely.
This is why the v7/v7b `serve/cell_model/` upload above happens IN THE SAME PUSH as `app/`, not as
a separate follow-up.

## The app{} manifest patch (this round's addition)

`build_app_bundle.qmd` writes one file per built release to
`_output/app_bundle/patches/{ver}_manifest_with_app.json` = the PUBLISHED `{ver}/manifest.json`
(fetched anonymously, never rebuilt) with `msens::app_manifest_block(ver, base)`'s output attached
as `$app`. This lets whoever runs `build_version_manifest.qmd` for real apply the `app{}` block
verbatim (overwrite `{ver}/manifest.json` with this file's content) without ever calling
`manifest_build()` — which, for v3, v4, v4b, v5, v6, v7, v7b, hits a genuine zone-table tie
(two subregion tables tied at n=4) that is msens round 10's own hard stop. Details and rationale:
`_output/app_bundle/patches/PROPOSAL.md`.

## Fixes landed this round (see commit messages for full detail + seeded-fault proof)

1. **`manifest_build()` removed from the render path entirely** (was kept "for the app{} patch and
   id_field cross-check"; both now come from the published manifest + `app_manifest_block()`
   directly). `grep -c "manifest_build(" build_app_bundle.qmd libs/app_bundle.R` → 9, 4, all
   comments/prose, zero live calls.
2. **`validate-manifest-identity`'s content check** no longer requires every manifest-named zone
   field to have non-empty `zone_taxon` content — a field with no drawable unit (ecoregion_key,
   planarea_key on v3-v7b) can legitimately have ZERO rows on both the db and written sides under
   D17; `identical(written_keys, db_keys_for_tbl)` alone still catches a real mismatch.

Both fixes verified against real data (no simulation), each with the old logic reproducing the
exact real failure and the fix turning it green, before any commit.

## Exit codes, all eleven, both rounds

```
final11 (prior round): v1 0  v2 0  v3 1  v4 1  v4b 1  v5 1  v6 1  v7 1  v7b 1  v8 0  v9 0
this round:             v3 0 (solo verify)   v4 0  v4b 0  v5 0  v6 0  v7 0  v7b 0
```

All eleven registered releases build clean (exit 0) on `atlas-contract` @ `b95b34e1`. Nothing has
been pushed to S3 (`APP_BUNDLE_S3` unset throughout this plan's generation) — the publish turn
above is what would run next, on Ben's/the orchestrator's go-ahead.
