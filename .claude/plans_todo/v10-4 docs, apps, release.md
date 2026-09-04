# v10-4 · Docs, apps, restricted release of v10

**Phase:** P5 of `2026-09-04 v10 plan.md`. **Model:** Sonnet for the docs edits, `layers_v10.csv`, the
release notebook run and the deploy flags (every step has a written procedure); Opus for the scores-app
mode switch (Shiny reactivity across three code paths and a legend-domain change) and for the final
cross-phase review. **Touches:** `../docs/scoring.qmd`, `../docs/data-sources.qmd`,
`../docs/model-merging.qmd`, `../docs/data/release_notes.yml`, `../apps/scores/app.R`,
`{dir_derived}/v10/layers_v10.csv`, `release_marine-atlas.qmd` + `build_version_manifest.qmd` (run),
`server/.env` + `server/cloudflare/access.sh` (run), memo + this plan (close-out).

## Docs (Sonnet)

Drop-in text is already written: memo `.claude/plans/2026-09-04 distribution-share review +
literature.md` and page section 11 (`scripts/v10_review/sensitivity-across-regions.tpl.html`, search
`Drop-in text`). Apply it:
- `scoring.qmd`: new `## Distribution share (v10)` after `## Ecoregional rescaling` with `@eq-share-cell`,
  `@eq-share-zone`, `@eq-v10`, the reference-point paragraph (location quotient, OHI typology, fixed
  reference), the richness identity, the reporting floor subsection `{#sec-reporting-floor}`, the
  composite paragraph; keep `## Scores are relative within an ecoregion` and add one sentence pointing
  to the v10 family as the cross-region comparison. Fix `@eq-zone-agg` (line 163): the implemented
  weight is `pct_covered`, not cell area; say so (v9) and that v10 weights by area explicitly.
  `tbl-metrics` fills itself from the manifest once the cell-level metrics exist (P3).
- `data-sources.qmd`: primary productivity paragraph (P1's integration) and the derived-range
  dataset entry (P2). `model-merging.qmd`: the derived-range subsection (P2). `references.bib`:
  Isserman 1977; O'Hara 2017; Halpern 2017; Jones & Cheung 2015; OECD/JRC 2008; Samhouri 2012;
  Williams 1996 if not present.
- `release_notes.yml` `v10:` — three `methods:` sentences and the retained-ecoregional statement.
- Docs render locally for `DOCS_VER=v10` with `-M mermaid-format:js` (never png locally).

## Apps (Opus) — `../apps/scores/app.R`

Design in page section 12 ("Showing v9 and v10 without showing six numbers"): ONE `Compare` switch,
*Within ecoregion* (v9, default) | *Across the nation* (v10), not a metric picker.
- Sidebar: `radioButtons("sel_mode")` beside `sel_lyr` (app.R:1469-1477); hidden when the release's
  manifest carries no `_v10` metric (capability derived from presence: `any(grepl("_v10$", metrics))`).
- The three hard-coded `".*_ecoregion_rescaled$"` filters (clicked cell 2447-2462, clicked zone
  2526-2539, default flower cache 995-1028) become a mode-derived pattern (`_ecoregion_rescaled$` |
  `_v10$`) with matching label `str_replace`s; the flower cache file gets a mode + version key (it is
  neither today: `scores/cache/flower_default_subregions.csv`).
- Flower plot in v10 mode: petal length = v10 component (dashed ring at 100 = national average),
  absent = not reportable; centre = `score_v10_equalweights`.
- Choropleth legend: v9 mode keeps `range(d_zone$value)`; v10 mode uses a fixed domain anchored at
  100 (e.g. 0–400 with 100 marked) so the reference is visible; tooltip sentence per the page
  ("a fifth of the national average concentration … ; 0.2 % of the world's turtle importance at stake").
- Layer picker: author `{dir_derived}/v10/layers_v10.csv` (`metric_key, lyr_order, category, label`)
  so the v10 family groups as "Species, across the nation (v10)"; otherwise the regex at 786-795 files
  it under "Species".
- Table of species: `share_global` / `share_v10_contrib` columns from `zone_taxon` (P3 deliverable 7).
- Never average a national column with ecoregional ones (one composite per mode).
- Verify in Chrome, running the scores app locally (the laptop has no `serve.duckdb`; run with a scratch
  `HOME` so the app resolves the local `~/_big` copies, see `../apps/README.md`): both modes on v10, v9 unchanged
  (v9 has no `_v10` metrics → switch hidden), deep links keep working.

## Release (Sonnet; follow `release_marine-atlas.qmd` + `CLAUDE.md` flags exactly)

1. Pipeline through `build_registry`, `publish_native` (`PUBLISH_MERGED_COG=1`; P2 added taxa to the
   global surface), `publish_score_cogs`, then `release_marine-atlas` with `RELEASE_NO_S3=1` first;
   `build_version_manifest` (v10 `prerelease, restricted`; `latest.txt` stays v7, never `PROMOTE_LATEST`).
2. Server `.env`: `PREVIEW_REVIEWERS_V10=ben@oceanmetrics.io,timothy.white@boem.gov`,
   `PREVIEW_RESTRICTED_VERSIONS=v8|v9|v10`; `DEPLOY_ACCESS=1` mints the Access application → paste
   `CF_ACCESS_AUD` → `DEPLOY_APPS=1` FIRST, then `DEPLOY_CADDY=1`, then `DEPLOY_TITILER=1` (COGs were
   repainted), `CHECK_PREVIEW=1`. Publish `versions.json` BEFORE deploying an app/msens that reads it.
3. Docs CI publishes `/docs/v10/` to the preview branch on its own once `versions.json` carries v10.
4. Close-out: the memo gets an "implemented" addendum with the attribution table (P1, P2, P3 each
   toggled) and the Spearman; this plan's phases marked done; email to Tim with the preview links.

## Gates

- `compare_versions` v9 vs v10 renders with the v10 section; the attribution table shows each phase's
  Δ separately (control flags `VGPM_INTEGRATED=0`, `AM_RANGE=0`, `SHARE_METRICS=0`).
- `CHECK_PREVIEW=1` passes; public host 302s `/v10/scores/`; `latest.txt` = v7.
- Review by Opus of the whole v10 diff against the four subplans' checklists; anything unmet is listed,
  not waved through.
