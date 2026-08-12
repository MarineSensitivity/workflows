# Documentation per version — one parameterized source, rendered per release

Depends on `2026-08-12 finish One app, every version.md` (A1 and A4 change what v1–v6 docs can
claim). Also absorbs the docs-of-record items from §11 of the 2026-08-10 plan.

## Context

The apps now render nine releases from one codebase; the docs do not. The book is a single
unversioned snapshot whose content has drifted from the code, and **one build currently mixes four
different versions**:

| source | says |
|---|---|
| `docs/VERSION` | `v8` — but drives only the publish path; no `.qmd` reads it |
| `libs/functions.R:4` | `ver <- "v4"` — the ecoregion/programarea PMTiles the maps actually draw |
| `db.qmd:11,34` | `v6` — paths and the "Rows (v6)" table header |
| `stats.R:31` | an **unversioned** `api.marinesensitivity.org/stats.json`, hardcoded **v7** fallback |

`stats.R` is sourced by `index`, `intro`, `science`, `data-sources`, `taxonomy`, `model-merging`
and `summary`. Rebuilding a v4 doc set today would print **v8 numbers under a v4 label** — worse
than a stale figure, because it looks authoritative. Killing that is the point of this plan.

The machinery is already correct and live: CI reads `VERSION` and publishes to `gh-pages/{ver}/`
via `peaceiris/actions-gh-pages` with `keep_files: true`, and `_version-switcher.html` reads **the
same `versions.json` the apps read**, HEAD-probing each version and disabling the unbuilt ones.
Only `v8/` has ever been built (242 files).

**Everything a per-version chapter needs is machine-derivable and public** — no server access:

- `versions.csv` → `versions.json` — `ver, status, released, title` for all nine (incl. `v4b`)
- `{ver}/manifest.json` — `grid_id`, `id_field`, `capabilities`, `tables`, `metrics`, `zones`
- `{ver}/tables/dataset.parquet` — datasets **with real citations**, `is_mask`, vintages
- `{ver}/tables/taxon.parquet` — `sp_cat`, `er_score`, `is_mmpa/mbta/bcc`, validity flags
- `_output/backfill_versions_v*.html` — per-version row counts, already rendered

Verified by query: v6 → **9,424** valid species and v7 → **16,153** reproduce the existing prose
exactly. **Generate the numbers; never transcribe them.**

---

## B1. One parameterized source (the core change)

New `docs/libs/versioned.R`, sourced from `.Rprofile` alongside `functions.R`:

```r
doc_ver()               # VERSION file, overridable by env DOCS_VER
doc_manifest()          # msens::atlas_manifest(doc_ver()), memoised
doc_can(cap)            # msens::manifest_can() — capability gate
doc_tbl("dataset")      # DuckDB read_parquet("{base}/{ver}/tables/dataset.parquet")
doc_stat("n_taxa")      # computed per version, never from an unversioned API
```

Then remove every competing hardcode:

- **`stats.R`** — replace the unversioned API call with `doc_tbl()` queries. Keep a `tryCatch`, but
  fall back to a **clearly-labelled placeholder**, never to another version's numbers.
- **`libs/functions.R:4,13,73,74`** — take zone PMTiles from `doc_manifest()$zones[].pmtiles`
  (published per vintage since the zone-set work) instead of `ver <- "v4"`. Drop the `<<-`
  `.ecoregions_caption` side channel that makes `caption_ecoregions()` order-dependent.
- **`db.qmd`** — table list from `manifest$tables`, columns from `DESCRIBE read_parquet(...)`, row
  counts from `doc_tbl()`. Kills the "Rows (v6)" header.

## B2. Rewrite the version-specific chapters against the code

Code is authoritative. Concrete corrections found — sweep each chapter, this is not exhaustive:

| chapter | fix |
|---|---|
| `data-sources.qmd` | generate the table from `dataset.parquet`; resolves the **7 vs 8 vs 9** dataset-count contradiction between `index`/`intro`/`taxonomy`/`summary` and `data-sources`/`model-merging` |
| `taxonomy.qmd` | v8 `sp_cat` has **no `other`**, adds `primary_producer`, uses `turtle`, excludes reptile/amphibian from scoring; document the **`is_marine` bird cull** (marine/coastal family AND `pct_marine ≥ 5%` + curated CSVs) — currently undocumented anywhere |
| `extinction-risk.qmd` | MBTA floor is the **FWS CFR 50 §10.13 list, not all Aves**; MMPA floor by taxonomy (all WoRMS Mammalia) |
| `model-merging.qmd` | governing ER is applied **at merge** ("the fitting point"), not at scoring; keep the v5/v6/v7 callouts but gate them by version |
| `scoring.qmd` | `[Appendix: mapgl](apps/mapgl.qmd)` and `mapsp.qmd` — **files do not exist**; `@fig-mapgl-flower` — **no such label**; "See `calc_scores.qmd`" — **file does not exist** |
| `db.qmd`, `workflows.qmd`, `model-merging.qmd` | `mdl_seq` → `mdl_key`, `value` → `val` for v8 (`value` is a DuckDB reserved word); `layers_v6.csv`; `r_cellid.tif` → global |
| `server.qmd`, `apis.qmd`, `software.qmd` | drop retired **PostGIS/pg_tileserv** as current; the SQL-per-tile factory is **retired** (kept, `MSENS_FACTORY=1`, not behind Varnish); scores are pre-rendered COGs |
| `apps.qmd`, `apps/*.qmd`, `libs/apps-guide-regenerate.R` | post-cutover URLs `/scores`, `/species` with `?ver=`; sources are `apps_v8/{scores,species}`, not `apps/mapgl`/`mapsp`; fix `app.` vs `shiny.` label/href mismatches |
| `summary.qmd` | **H3/COG/STAC/targets "future work" already shipped** — move to delivered; drop H3 (v8 went global 0.05°, not hexes) |
| `_quarto.yml` | `apps/scores.qmd` is listed **twice** (lines 80 and 87) |

## B3. A Releases part (in every build)

- `releases.qmd` — timeline generated from `versions.json` joined to computed per-version stats
  (datasets, metrics, taxa, valid species, grid, spatial unit). The prose already exists at
  `MarineSensitivity.github.io/index.qmd:105-190`; lift it, and **write the missing `v4b` entry** —
  it is a real release in `versions.csv` with no narrative anywhere but a dev journal.
- `releases/v1.qmd` — the richest page, from the 2025 final report DOCX
  (`.../2025 final report/BOEM-MarineSensitivityToolkit_2025-final-report_v6_BB.docx`; the repo's
  `final-report_2025.qmd` is **behind** it — treat the DOCX as authoritative, and lift clean
  markdown from `final-report_2025_review-responses_v5-MKR.md`).

  Carry forward verbatim: the `V = f(E,S,A)` framing; the `v_c` equation with its plain-language
  gloss; the equal-steps/STAR justification of the 0.2–1.0 ladder; **"What ecoregional rescaling
  does and does not preserve"**; "Grid and projection"; the worked example; and the Marine Relevance
  paragraph, which states the v2+ rule *and* why WoRMS `isMarine` fails for seabirds.

  Do **not** carry: any count (all v1 snapshots), Table 2, or the Hope/Norton NPP figures the report
  itself flags as wrong and retains "as a matter of record".

  Resolve first: the report's **AquaMaps vintage contradicts itself** — "Accessed October 2025"
  (Methods) vs "AquaMaps 2019" (Conclusions §6.2).

## B4. Capability-gate the historical versions

v1/v2 have no `listing` and no `er_score`; v1 has Planning Areas and no Program Areas. Gate every
such section on `doc_can()` / `manifest$tables`, exactly as the app does, so a v1 build **omits**
sections rather than printing v8 behaviour under a v1 label.

## B5. CI: render every version, and clean the root

- `.github/workflows/quarto-publish.yaml`: matrix over the versions in `versions.json`, setting
  `DOCS_VER`; publish each to `gh-pages/{ver}/`. `keep_files: true` stays — it is what preserves
  siblings, and the comment explaining why `quarto-actions/publish` cannot be used is correct.
- **Delete the stale flat root**: 486 files frozen at the 2026-07-25 build, still crawlable
  (`robots.txt: Allow: /`). Keep only the meta-refresh `index.html` → `/{latest}/`. Two competing
  copies in search results is worse than a 404.
- Beware `.gitignore`'s `*.html` — its own comment records that this is exactly how the version
  switcher "shipped as a no-op on first attempt". Confirm every needed HTML is negated.

## B6. Docs-of-record folded in from the 08-10 plan §11

- **`workflows/schema.qmd`** — v8-only today, with hardcoded paths and per-DB column lists.
  Parameterize off `manifest$tables` + `DESCRIBE read_parquet(...)` so it renders any version; add
  `zone_set`, relocated `zone_cell`, `model_asset`.
- **`.claude/skills/`** — `publish-sdm` (4 refs) and `validate-sdm` (3) still describe the retired
  factory; `generate-sdm-metadata` has no multi-version content. Update to manifest/COG/`?ver=`.
- **Workflows index** — `_output/_data/workflows.yml` has **0 entries**; rebuild via
  `scripts/build_workflows_index.R` so the six new targets appear (`build_zone_sets`,
  `build_zone_cells`, `build_version_manifest`, `publish_score_cogs`, `backfill_versions`,
  `publish_storage_index`). They already carry correct `msens:` front-matter, so they join the DAG
  on rebuild.

---

## Verification

1. **No cross-version bleed.** Build `DOCS_VER=v3`, grep the output for v8-only strings
   (`mdl_key`, `global05`, `primary_producer`, `native_asset`) — assert none. Build `v8`, assert no
   `mdl_seq` or `planarea`. This is the specific failure the plan exists to prevent.
2. **Numbers match the release.** The v6 page reports 9,424 valid species and v7 reports 16,153,
   matching `taxon.parquet`; no chapter prints a number its manifest contradicts.
3. **Every version builds and publishes.** `/docs/v1/` … `/docs/v8/` all 200. The switcher
   HEAD-probes, so a missing build silently disables — check the rendered `<select>`, not just
   the HTTP codes.
4. **Root is clean.** `/docs/` redirects to `latest.txt`'s version; the 2026-07-25 flat pages are
   gone; `robots.txt` no longer invites indexing of them.
5. **Links resolve.** Link-check each built version; zero 404s, and the three known-broken refs
   (`apps/mapgl.qmd`, `apps/mapsp.qmd`, `@fig-mapgl-flower`) are gone.
6. **Docs match the live apps.** Every app URL resolves post-cutover (`/scores`, `/species`,
   `?ver=`), and no doc claims the factory or PostGIS is in use.
