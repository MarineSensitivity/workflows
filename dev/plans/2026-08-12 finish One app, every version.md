# Finish "One app, every version" — the unfinished tail

Wraps up what `2026-08-10 One app, every version; retire the per-version app forks.md` left
undone. The docs-of-record items from its §11 (`schema.qmd`, the two stale skills, the workflows
index) move to `2026-08-12 documentation per version.md`, where they belong. What remains here is
code and data.

## Context

That plan is ~90% delivered: one app renders v1–v8 from `?ver=`, `/scores` + `/species` are the
live consolidated apps with 301s from all 18 former instances, the titiler factory is retired
(kept, `MSENS_FACTORY=1`), merged COG coverage is 17,108/17,108, and 576 msens tests pass.

An audit against the plan's own §7 and §12 found four gaps. Two of them (A1, A4) are real
capability losses that also constrain what the versioned docs can say, which is why they should
land before the docs work starts.

---

## A1. `cell_model` for v1–v6 — the largest gap

§7 said: *"Run the `build_v7_cell_model.qmd` transpose for v1–v6 as well, so the per-cell species
list works everywhere. The single largest compute item: 6 × ~1.2B rows."* Never run.

Measured on the server: `cell_model` exists only for **v7 and v8**. v1–v6 have none, so their
manifests declare `cell_species_list: false` and clicking a cell on those versions cannot list
species.

- `build_v7_cell_model.qmd` is already parameterized by version.
- Add a `cell_model` stage to `scripts/backfill_all.sh` rather than writing a new driver — the
  loop over versions belongs in a committed script, and it must go through `scripts/srv_render.sh`
  for the `git merge --ff-only origin/main` guard (bypassing it is how a v3 run once rendered a
  stale `sdm_db_path()` and reported success while producing nothing).
- ~1.2B rows × 6 versions: run **one version at a time, detached** on the server. Do not run it
  concurrently with anything that reinstalls `msens` — that pulls `msens.rdb` out from under the
  running render (hit twice this session).
- Re-run `--stage manifest` afterwards so `cell_species_list` flips to `true`.

**Verify:** each of v1–v6 reports `cell_species_list: true`; clicking a cell in `/scores/?ver=v3`
returns a species list.

## A2. `am_0.05` → `am` normalization belongs in `msens`

`backfill_versions.qmd:84-85` does it as an inline `sub()`. That breaks the repo's own rule —
*"logic lives in msens, not in QMD strings"* — and is untested.

- Add an exported, documented normalizer in `msens` (either `normalize_ds_key()` or folded into
  `mdl_key_raw()`), and call it from `backfill_versions.qmd`.
- Test in `test-mdl_key.R`: `am_0.05` → `am`; every other key passes through untouched; vectorised.

## A3. Complete the crosswalk tests (§12)

`test-mdl_key.R` covers composers/parsers but not the plan's actual asks:

- `mdl_key_raw()` round-trips on **real** v1/v3/v7 `model` rows — read the published
  `{ver}/tables/model_asset.parquet` over HTTPS (no server needed); `skip_if_offline()`.
- A v6 `mdl_seq` and the v7 `mdl_key` for the same taxon resolve to the same species.

## A4. Reconstruct `taxon_model` for v1/v2

§6 said v1/v2's wide `taxon` table carries per-dataset columns (`am_0.05, ch_nmfs, ch_fws,
rng_fws, bl`) plus `mdl_seq`, so unpivoting them yields the real taxon→model edges. It was instead
treated as genuinely absent — which is why v1/v2 `serve.duckdb` has **11 views, not 13**, and why
their documentation cannot say which models fed a taxon.

- Implement in `backfill_versions.qmd`, guarded: only when `taxon_model` is absent AND the wide
  columns exist. Write `tables/taxon_model.parquet`.
- `listing` genuinely has no v1/v2 source — emit nothing rather than a stub, and record the
  derivation in `capabilities.reconstructed` so nobody mistakes it for a published table.
- Test: unpivoting a synthetic v1-shaped `taxon` reproduces the v3 `taxon_model` shape.

**Verify:** v1/v2 view DBs gain the view; `msens::species_for_zone()` returns rows for v1.

## A5. Re-publish and re-verify

- `scripts/backfill_all.sh --stage manifest` for v1–v6; all nine manifests validate.
- All nine versions still return 200 through `/scores/?ver=`.
- `devtools::test("../msens")` green, and `pra_score_delta` still reports score equivalence.

---

## Notes for whoever runs this

- **Verify by watching the thing work, not by absence of evidence.** This session twice concluded
  "broken" from a screenshot taken before the map had painted (~60 s under browser automation) and
  from a log grep against a container that does not log tile requests — and reverted a working
  optimisation on that basis. Server-side tile requests are the reliable signal.
- **Any run whose output will be deployed must skip nothing.** `native_asset` is rebuilt wholesale,
  so `NATIVE_SKIP_AM=1` silently dropped ~35.9k rows, and syncing that table then overwrote the
  server's good copy.
