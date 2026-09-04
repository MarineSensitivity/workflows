# v10-0 · Bootstrap v10 on the v9 grid, and the housekeeping the explorers found

**Phase:** P0 of `2026-09-04 v10 plan.md`. **Model:** Sonnet (mechanical, every step has a template);
no review agent needed beyond the gates below. **Touches:** `libs/paths.R`, `data/versions.csv`,
`data/zone_sets.csv`, `../msens/R/grid.R` (+ `test-grid.R`, `DESCRIPTION`, `NEWS.md`),
`../docs/data/release_notes.yml`, `schema.qmd`, `bootstrap_version.qmd` (run, not edited).

Read first: `.claude/skills/bootstrap-release/SKILL.md`, `bootstrap_version.qmd:47-63,85-106,186-199`,
`CLAUDE.md` § "Bumping the version on the same grid".

## Steps

1. `../msens/R/grid.R:40-42`: add `v10 = "global05"` to `.GRID_VER` (`grid_for_ver()` errors on an
   unknown version by design, which blocks `bootstrap_version`'s same-grid assertion and
   `manifest_build()`); extend `tests/testthat/test-grid.R:19`; bump `Version:` (0.42.0) + `NEWS.md`;
   `devtools::test()`; `devtools::install("../msens")`.
2. `data/versions.csv`: append `v10,prerelease,restricted,<today>,Distribution share + integrated
   productivity + derived ranges` (status and access are orthogonal; the newest `released` stays v7).
3. `libs/paths.R:4-5`: `ver <- "v10"`, `ver_prev <- "v9"`. Check the two `ver_prev`-pinned inputs exist:
   `{dir_derived}/v9/ply_programareas_2026_v9.gpkg` carrying the column
   `score_extriskspcat_primprod_ecoregionrescaled_equalweights` (the equivalence gate in
   `score_zone_metrics.qmd:152-164` and `score_zones.qmd:47-48` read it). If v9 never wrote it, render
   the v9 step that does (find it: `grep -l "ply_programareas_2026_{ver}" *.qmd`) BEFORE flipping `ver`.
4. `data/zone_sets.csv`: the `versions` column stops at v8; append `v9 v10` to the sets v9 used
   (`ecoregion_2025-06`, `programarea_2026-01`, `subregion_2025-06`). Cosmetic for v8+ (which stamp
   `zone_set_key` into `zone`), but `msens::zone_set_resolve()` reads it for v1–v7 comparisons.
5. `schema.qmd:82,171,532`: `taxon_flags` declares `has_am`; the merge writes `has_suit`
   (`merge_models.qmd:237-242`). Fix the declaration; add `er_mode` to the expected `taxon` columns
   (`score_zones.qmd:132` adds it; `schema.qmd:56-60` omits it).
6. Render `bootstrap_version.qmd` with `BOOTSTRAP_VERIFY=1` (clones v9's `dist/dataset=*` +
   `model_*.csv` + `native/{am,am_native,pmtiles,vec_grid,src,gm,…}` copy-on-write; re-hashes against
   `data/manifests/ingest_*.json`). Then `build_cell_grid.qmd` (copies `cell` from v9), `ingest_worms`,
   `ingest_listings`, `build_common_names` (the fresh `merge.duckdb` needs them — v9 gotcha).
7. `../docs/data/release_notes.yml`: a `v10:` entry with `methods:` (three bullets: distribution
   share, integrated productivity, derived ranges — each phase appends its sentence) and
   `zones:` ("unchanged"). Counts are computed by the docs, never typed.
8. Commit: "v10 bootstrap on the global05 grid (ver_prev v9)".

## Gates

- `BOOTSTRAP_VERIFY=1` passes for every cloned dataset (hashes equal the v9 manifests).
- `Rscript -e 'msens::grid_for_ver("v10")'` prints `global05`; msens tests green.
- `targets::tar_visnetwork()` builds (no new target errors).
- Nothing published: no S3, no server. `latest.txt` untouched (v7).
