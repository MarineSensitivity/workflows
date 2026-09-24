# Atlas app plan: one static JavaScript app for scores, species, places and reports

*Written 2026-09-20 from four same-day surveys (kept as the specification under `atlas-refs/`: the two
Shiny apps read in full, the report pipeline, the CalCOFI Explorer precedent), live probes of the bucket,
titiler and the preview host, the MMA Branding Guide (July 2026) and DuckDB's OPFS post (2026-09-18).
Every file:line and every number below was verified that day; re-verify before editing, the code moves.*

*Review page with live mockups of the MMA look and feel: https://claude.ai/artifact/6mFmLygX1VsAFWnJn1iX9E
(private; share it from the page's Share menu). Its source is `atlas-refs/atlas-plan-page.html`: pure ASCII
on purpose, republish with the Artifact tool and that `url`.*

## What this is

One browser-only app, **Atlas**, replaces `apps/scores` + `apps/species` + the Quarto report API. It is
static files on GitHub Pages that read the published marine-atlas release straight from cloud storage:
a full-screen map with floating panels, two lenses (Scores, Species) over one map, places you pick,
draw or upload, and a report that is just a URL. Restricted pre-releases (v8, v9 today) are served by
the same build from the Cloudflare-Access preview host.

| today | Atlas |
|---|---|
| 2 Shiny apps, ~15 s cold TTFB (R process start), 222 MB per species worker | static shell paints from HTML; default view and any species deep link draw **before DuckDB-WASM is requested** |
| state is session-only (only `?ver=` survives a reload) | the URL is the whole view; places travel in the hash |
| drawn polygon → WKT → POST → Quarto → LaTeX/Pandoc, "a couple of minutes" | `report.html?…#pl=…` recomputed in the browser in seconds; print → PDF, HTML, ZIP |
| polygon geometry sent to the API and into a CSV link's query string | geometry never leaves the browser (not even to analytics) |
| per-version adapters in each app | one version-independent `app/` contract written at publish time by msens |
| drawn-polygon scores ≠ Program-Area scores (different formula) | one method; a polygon tracing a Program Area reproduces its published score |

## Decisions (⚑ = wants Ben's word; work proceeds on the recommendation meanwhile unless a subplan says stop)

- **D1 Name and address (decided 2026-09-20: `atlas`).** Repo `MarineSensitivity/atlas` (the name is free; the data release is
  already "marine-atlas"). Public `https://marinesensitivity.org/atlas/` (project Pages under the org
  site's existing CNAME: no DNS work). Preview `https://preview.marinesensitivity.org/{ver}/atlas/`.
- **D2 Framework: Svelte 5 + Vite + TypeScript, no SvelteKit, no client router.** Cloudflare Access
  scopes a reviewer policy by path, so the same build must run under `/atlas/` and under `/v9/atlas/`:
  assets are base-relative and all view state is query + hash. Kit's router binds to one `paths.base`
  (its hash-router escape hatch would take the hash we want for places), and nothing here needs SSR.
  Two HTML entries (`index.html`, `report.html`) cover the "routes".
- **D3 Three data tiers.** Tier 0: `manifest.json` + a prebuilt `app/boot.json` and per-taxon JSON
  shards → first paint with zero WASM. Tier 1: DuckDB-WASM, lazy, in a worker, **materialize then
  query** (whole small objects fetched and registered; no object the app needs is both large and
  prunable, so no httpfs range reads in v1). Tier 2: an OPFS-persistent DuckDB per release for the small
  tables (~10–25 MB) plus an LRU of cell tiles; one tab holds it (Web Locks), everyone else and every
  failure mode runs in memory. The app never *requires* OPFS.
- **D4 Rasters are displayed through the existing stock titiler; numbers never come from it.** Score
  values, cell ids, zonal statistics come from Parquet; only species click values keep `/cog/point`
  (behind an interface). Client-side COG rendering is measured in spike S3 and deferred.
- **D5 Normalize at publish, not in the browser.** `msens::app_bundle_build()` writes `{ver}/app/` (one
  schema for v1…v9) and the legacy quirks stay in msens, where their tests already live.
- **D6 The review gate (decided 2026-09-20: presentation gate now, protect the data later).** Public host
  renders public releases only; the preview host serves the same build per version path behind Access +
  `jwtauth`, and a same-origin `session.json` (which exists only there) is the sole way into preview
  mode; screens and reports from a restricted release carry a PREVIEW watermark. The files of restricted
  releases stay on the public bucket for now, as they are today. **Follow-up Ben has committed to:**
  protect preview data from direct reading, either by publishing a restricted release under an
  unguessable prefix that only the signed-in `session.json` reveals (anonymous LIST is already denied) or
  by serving it only through the preview host (atlas-9, "Later"). Both are configuration for the app:
  `dataBase(ver)` is the one place a data origin is formed, and it prefers `session.data` when present.
- **D7 One scoring method (decided 2026-09-20: harmonize, including `pct_covered`).** Custom places use
  the published zone method `Σ(coalesce(val,0)·pct)/Σ(pct)`; `scores_for_cells()` gains the coverage
  blend and `cells_in_pra()` stops discarding `pct_covered`. Fixed in msens with regression tests, not
  ported. Reproduced on v9 2026-09-20 (the blend equals every published `zone_metric` exactly, max |Δ| =
  0.0): the old formula reads high wherever a component covers only part of an area: turtle in St. George
  Basin 49.8 vs 0.7 published (297 of 16,850 cells, 1.4 %), primary producer in SOC +20.3, coral in BFT
  +9.6; composite +6.3 for GEO, median +0.5 over the 20 Program Areas. Ben confirmed both points
  2026-09-20: for custom places "absent means zero, inside the study area", and it is fine that
  drawn-polygon numbers change from old reports (listed as an intentional difference on the parity page).
  **D7b (decided 2026-09-20: clip custom places to US waters).** ONE cell set per custom place drives
  scores, species, area and N cells: the touched cells that are inside the study area (present in the
  release's cell table with `coalesce(in_usa, TRUE)`). Land and foreign waters never enter as zeros.
  Measured: inside Program Areas only 0–0.29 % of the weight lies outside study-area cells, so a traced
  Program Area still reproduces its published composite within 0.08 points (median 0.03). The panel and
  the report state the share of the place inside the study area, and each component's coverage and mean
  where present beside its score.
- **D8 Places live in the hash** with a versioned binary codec (`g1`: delta + zigzag varints, base64url,
  ~4–5 characters per vertex at 0.001°), and every analysis runs on the *decoded* geometry, so a link
  reproduces its numbers. msens gets the R twin.
  **D8 addendum (orchestrator ruling, 2026-09-21, from a measured R-vs-TypeScript disagreement).** The two
  coverage twins (`msens::cells_in_polygon_grid()`, `src/lib/geo/coverage.ts`) read coordinates
  **literally**: planar, **unwrapped** longitudes (a Bering box is `179.9 … 180.1`), columns wrapped modulo
  `nc` on `global05` and shifted into the 141.10° frame on `usa05`. **Neither twin guesses at the
  antimeridian.** A ring written wrapped (`179.9 → -179.9`) means, read literally, the 359.8° complement
  (TypeScript: 7,196 cells; R's old heuristic: 4), so it must be unwrapped BEFORE coverage by ONE explicit,
  shared rule with its own shared fixtures: `unwrap_ring()` (R) / `unwrapRing()` (TS): walking a ring, when
  consecutive vertices differ by more than 180° of longitude, carry ∓360° onward (shortest-path convention;
  a ring that truly spans more than 180° cannot be expressed wrapped, and that limit is documented). It runs
  at every input boundary (uploads and drawn places in atlas-6's normalizer, `place_encode()`, the sf-facing
  R wrappers and the report API), never inside coverage, and the `g1` codec only ever sees unwrapped rings.
  Coverage fixtures are written unwrapped; the wrapped cases live in `normalize-*` fixtures (wrapped in →
  unwrapped ring out → cells). **The codec and wrapped input (ruling 2, same day, after the two sides were
  found to differ because the first ruling's wording invited both readings):** the BYTE-LEVEL encoder
  refuses a ring that still has a step of more than 180° in both languages (reject code `wrapped`, a shared
  vector), and the HIGH-LEVEL entry normalizes first (R `place_encode()` = `unwrap_polygon()` then the strict
  encoder; TypeScript callers go `normalizeForAnalysis()` then `encodeGeometry()`), so the same high-level
  call yields the same token and the same low-level call yields the same error. On `lon360` grids coverage
  shifts each vertex into the grid's own frame (`xmin + ((lon - xmin) %% 360)`, R's floor-division modulus)
  in both languages; on `global05` coordinates are left alone and columns fold modulo `nc`.
- **D9 Report formats (decided 2026-09-20: build Word output).** Print-first HTML (browser "Save as PDF":
  real text, no LaTeX), a self-contained HTML download, a ZIP data package **and Word (.docx)**: Ben
  answered "yes, build Word output", so atlas-7 export 4 is in scope without the `log/reports.csv` count.
  ESP template: Ben, 2026-09-20: "no ESP template for docx yet", so the .docx is styled from the app's
  own brand tokens with no template to match; matching an ESP template is a later change, when one exists.
- **D10 Brand (decided 2026-09-20: seal and agency name approved for use).** Ben: "ok to use seal and
  agency name; use 'Minerals'; skip offshore_hub.png wordmark (that's only being used internally for
  now)". So `VITE_SEAL` and `VITE_AGENCY` may be turned on, the agency string is spelled with
  "Minerals" everywhere, and nothing links to or shows the Offshore Hub wordmark. The guide's seal rules
  (size, clear space, unmodified) still apply. Original note, kept for context:
  MMA palette, type and the hexagon + wave motifs now; the seal and the agency string
  are build flags (`VITE_SEAL`, `VITE_AGENCY`, mirroring `server/branding/make_branding.py`), off until
  the Office of Communication (through the COR) approves. The guide spells the agency two ways
  ("Minerals" on the seal, "Mineral" in text): ask. `branding/offshore_hub.png` is an unexplained
  "Offshore Hub" wordmark: is that a parent portal to link?
- **D11 Releases covered.** v7 (public, legacy shape: `usa05`, `mdl_seq`) and v9 (preview, `global05`)
  to full parity; v1–v6 and v8 through the same contract at smoke level; a release that cannot supply
  a capability simply does not advertise it.
- **D12 Cutover** is beside → default → retire, each reversible (atlas-9), gated on a signed parity page.
- **D13 Size budget after MapLibre 6 (decided 2026-09-21; RELAXED the same day: Ben, "We don't need to be so
  tight on the 350 KB budget").** NEW NUMBERS (orchestrator, under Ben's delegation): static critical path
  **≤ 450 KB gzip**, runtime workers **≤ 150 KB gzip**, both gated (≈ 600 KB before first interaction). Why 450:
  measured MapLibre 6 + pmtiles 288 KB + the shell 108 KB (of which 80.5 KB is the brand's self-hosted fonts:
  Carlito 61.5, Jost 19.1) = ~396 KB, leaving ~50 KB for the lenses. The fonts STAY (the brand look matters more
  than 80 KB). The budget's job is to catch an accidental multi-megabyte static import (DuckDB, d3, docx), not
  to squeeze fonts; the forbidden-lazy-marker and source-scan gates are unchanged. Earlier text, superseded:
  Ben: "Do what makes sense. Obviously MapLibre needs to get loaded and is part of the default Scores view
  ... defer to you." DECISION: static critical path ≤ 350 KB gzip AND runtime workers ≤ 150 KB gzip, both
  gated (together that IS a 500 KB cap on what loads before first interaction, but each line names what grew);
  atlas-2/3 must also try to stop shipping MapLibre's shared chunk twice. Original note: The plan's 350 KB gzip critical
  path assumed MapLibre 5, whose worker ships inside the main bundle. S2 forced `maplibre-gl ^6.10.0` (no 5.x
  escapes the critical advisory GHSA-jrc7-96c5-q579), and 6.x loads a separate runtime worker before first
  interaction: measured static path **288 KB** (MapLibre + pmtiles, no app code yet) + worker **144 KB** =
  432 KB. Working rule until Ben answers (implemented in `scripts/size-budget.mjs`): static path ≤ 350 KB
  **and** runtime workers ≤ 150 KB, both gated, the 500 KB sum printed but not separately gated. Ben to
  confirm or replace: (a) keep 350 + 150; (b) one combined gate (what number?); (c) hold the line at a
  lower total and make atlas-2 earn it by not shipping MapLibre's shared chunk twice (main bundle + inlined
  in the worker). Only ~62 KB of the static 350 is left for Svelte, the app, CSS and fonts either way.
- **D14 GeoPackage uploads (decided 2026-09-21: keep it, lazy, on demand).** Ben: "I like the lazy approach
  -- keep functionality, but only load on demand." So: consented, lazy, best-effort through DuckDB `spatial`,
  with the "convert to GeoJSON" fallback; not mirrored. Original note: Works through DuckDB `spatial` on the
  pinned version, but only by downloading ~22.4 MB from `extensions.duckdb.org` the first time a `.gpkg` is
  dropped (fails when that host is blocked; geometry itself never leaves the browser). Verdict and working
  rule: consented, lazy, best-effort, with a "convert to GeoJSON" fallback; not mirrored (≈ 45 MB per engine
  version against Pages' limits). Ben to confirm, drop GeoPackage, or ask for the mirror.
- **D15 Access control for the atlas = the same paths as the apps it replaces (Ben, 2026-09-21).** Ben: "each
  pre-release preview version should be able to use cloudflare to manage different user level access", and,
  clarifying: "I meant for you to inspect the paths of existing apps (scores and species) that are being
  replaced with this new atlas app to enable similar paths for user access control from cloudflare".
  Inspected (`server/cloudflare/access.sh`, `server/caddy/{app_version_routes,preview_routes}.caddy`,
  `apps/README.md`): per restricted release `access.sh` keeps two Cloudflare Access applications, "preview
  {ver} apps" on `preview.marinesensitivity.org/{ver}` and "preview {ver} docs" on `…/docs/{ver}`, each with
  that release's reviewer policy (`PREVIEW_REVIEWERS_<VER>` in the server `.env`) plus a per-version service
  probe token; a catch-all application (admins only) covers everything else. Scoping is by path PREFIX, so the
  Shiny apps live at `/{ver}/scores/` and `/{ver}/species/` and the public app host 302s a restricted
  `/{ver}/(scores|species)` to the preview host (`Caddyfile:230`, `PREVIEW_RESTRICTED_VERSIONS`).
  **Rule, applied:** the atlas takes the SAME shape, `preview.marinesensitivity.org/{ver}/atlas/`, a sibling
  of `/{ver}/scores/` under the existing "preview {ver} apps" application, so every version's reviewer list
  covers it with NO change to Access (`access.sh` untouched; already built on the server repo's
  `atlas-preview` branch, atlas-9 part a). Two things follow from the inspection and are atlas-9's to do:
  (1) the public app host must 302 a restricted `/{ver}/atlas/…` to the preview host exactly as it does for
  `/{ver}/(scores|species)` today, and the public Pages host's own "under review" page must link the same
  `/{ver}/atlas/` URL (never `?ver=` on Pages); (2) `CHECK_PREVIEW` gains the per-version probe for
  `/{ver}/atlas/` and its `session.json` (own token → 200 and `ver` = that version; another version's token →
  refused), the same proof it runs for `/{ver}/scores/`. NOT built: any new Access application, any change to
  the origin's `audience_whitelist`, any per-version AUD at the origin.
- **D16 A drawable unit publishes only keys that are BOTH scored and drawn (orchestrator ruling 2026-09-22,
  corrected the same day; Ben may overrule).** Measured on every release's `zone ⋈ zone_metric ⋈ metric`:
  the subregion zones carry a `score_%` metric only on v8 and v9 (5 of 5); on v6 0 of 4, on v7 / v7b only the
  `FULL` rollup (1 of 5), on v1–v5 no subregion rows at all. So the drawable-unit rule inherited from
  `apps/scores/app.R:570-647` (PMTiles present, ≥ 2 zones with a `score_%` metric, ≥ 2 keys in the geometry)
  already gives the right answer without any per-release geometry cutting: **subregion is a unit on v8 and
  v9 only**; ecoregion is a unit on v1, v8, v9; planarea on v1, v2; programarea on v2–v9. My first version of
  this ruling (cut the geometry to {AK, GA, PA} for v4–v7b) was wrong: those keys have zone rows but no
  scores, so there is nothing to draw a choropleth from, and the app's expectation gate rightly refused the
  unit msens rightly did not publish. Rule as it stands: `units[].keys` = keys scored with a `score_%` metric
  ∩ keys the geometry draws; the notebook passes each release the published 2025-06 geometry uncut; the
  msens subset check (geometry keys ⊆ the release's zone table) still applies where a unit IS published;
  `boot$zones[unit]` keeps every scored key. Every unit a release lacks is a named row in `PUBLISH_PLAN.md`
  ("v7: no subregion unit: its subregion zones carry no score_ metric"). The nine manifests' `subregion_2025-06`
  label is reported to the release session, not fixed here.
- **D17 Drawable units are Program Areas only, or Planning Areas on v1 (Ben, 2026-09-22).** Ben: "let's not
  display subregion or ecoregion scores, just program areas (or planning areas in v1). let's move things
  forward. feels like we keep getting stuck in minutia that is not necessary". So `boot$units[]` carries
  exactly ONE unit per release: `programarea` where the release scores it (v2–v9), `planarea` on v1. No
  subregion or ecoregion unit is published on any release, whatever the data would support; their scores
  stay in `boot$zones` (numbers, used by the report and the flower) and are not drawn as a choropleth. D16's
  derivation, the per-release subregion geometry question, the v8 `AT` case and the two-table ambiguity
  on v2/v3 therefore stop mattering to the app: the geometry check runs for the one unit only, the notebook
  passes one GeoPackage per release (programareas, or planareas for v1), and everything else already built
  stays as tested but unexercised. Priority from here is publishing and the lenses, not further data
  archaeology.

## Ground truth (verified 2026-09-20; details and line refs in `atlas-refs/`)

- Registry: `latest.txt` = v7; v8, v9 `prerelease`/`restricted`; v1–v6 retired/public. `manifest.json`
  already carries `grid_id, id_field, capabilities, tables{name→URL}, metrics[] (COG + rescale +
  colormap per subregion), zones[] (PMTiles per vintage), overlays, grid{}`.
- Bucket: `us-east-1`, path-style URLs only, CORS answers `Origin`+`Range` (206, `ETag` exposed),
  anonymous LIST denied (every key must be constructible), restricted releases anonymously readable.
- Sizes: v9 `cell` 374 MB, `cell_metric` 72 MB (**3 row groups spanning everything: unprunable**),
  `zone_taxon` 5.7 MB, the other tables ≤ 1.2 MB. `serve/cell_model/` = 422 tiles at 2.5°, p50 2.1 MB,
  on S3 for v8/v9 and **missing for v7** (757 MB local only) although v7 advertises the capability.
  Score COGs Float32 0.05° p50 294 KB (no overviews); merged species COGs Byte p50 125 KB (15,674).
- titiler-v8 serves every release's `/cog` tiles and `/cog/point`, answers CORS for our origins; the
  SQL tile factory is off by default and ~192 taxa have no surface today.
- The Shiny apps: study area is a camera not a filter; units are derived from the release; 4 known
  bugs to fix rather than port (dead layers control, unversioned flower default, grey `primary
  producer` petal, hard-coded global grid constants in the species click); the species lens needs no
  SQL at all once bboxes and assets are precomputed.
- The report: 7 sections, no branding or provenance, DOCX without a template, PDF through LaTeX;
  three internal inconsistencies (D7). `query.html` already proves DuckDB-WASM over this bucket.
- CalCOFI Explorer lessons adopted: early-fetch inline script, self-hosted non-threaded DuckDB bundles,
  one promise chain, one composed MapLibre style with `setStyle(diff)`, URL-as-state with
  `replaceState`, one ramp function and one citation function, headed-Chrome pixel probes, parity
  fixtures shared across languages, zero-backend feedback. It pins `maplibre-gl < 6` (Vite worker issue).
- DuckDB OPFS: `opfs://` paths, explicit `CHECKPOINT`, one handle per file, same-origin worker;
  npm `latest` (1.33.1-dev57) creates OPFS files and never writes them: pin `1.32.0` or `@next ≥ dev64`.
- MMA guide: Gold `#E8C24A`, Steel `#173D6D`, Navy `#001A57`, Crimson `#6E110F`; Century Gothic
  headlines, Calibri body; hexagon and wave at 10 % opacity as secondary elements; strict seal rules
  (≥ 0.75 in, clear space, unmodified); Section 508 compliance required.

### Addendum 2026-09-21 (from the v7.1 release session; registry facts re-verified by the orchestrator)

- **`v7b` exists**: `versions.json` lists it as `prerelease` / `restricted`, `prev: v7`, released
  2026-09-20, `grid_id = usa05`, `id_field = mdl_seq`: a PATCH of v7 dated AFTER v9. Never infer lineage
  or "newest" from `released` order: use `latest.txt` (still `v7`; promotion is not authorized) and `prev`.
  Restricted releases are now v7b, v8, v9; none may ever render on the public host. Keys live under the
  `marine-atlas/` prefix of the bucket (`…/oceanmetrics.io-public/marine-atlas/{ver}/…`).
- `v7b/manifest.json` carries a new **optional** top-level `methods` array (4 rows from `release_method`:
  `method_key, value, description`); no other release has it, so types and the `app/` contract treat it as
  optional. Reported, not verified here: v7b zone metrics include `{component}_coverage`, and a
  not-reportable component has NO `zone_metric` row (not a zero), which the scores lens and D7's coverage
  display must respect.
- **msens is 0.42.0 on origin/main** (`coverage_sql()`, `coverage_floor()` in `R/coverage.R`; `.GRID_VER`
  gains `v7b = "usa05"`; `turtle_sql()` gains `suit_min, fill, half_even, ch_outer`). atlas-1 cuts
  `atlas-contract` from a fresh pull of msens AND workflows, bumps msens to **0.43.0** under a NEW
  `NEWS.md` heading, and reuses `coverage_sql()` rather than writing a second coverage rule.
- workflows main changed the same day: `build_v7b.qmd`, `libs/duckdb_budget.R`,
  `build_version_manifest.qmd` (`MANIFEST_REGISTRY_ONLY=1`), `backfill_versions.qmd` (`release_method` →
  `manifest$methods`), `build_v7_cell_model.qmd`, `scripts/srv_render.sh` (host-memory watchdog, exit 137),
  `scripts/backfill_all.sh` (`MSENS_MIN=0.42.0`), `data/versions.csv` (+ `prev`, + `v7b`), `data/zone_sets.csv`.
- **Server safety**: msens1 is 16 GB / 4 cores / no swap, shared with the live apps; a literal DuckDB
  `memory_limit='12GB'` took the public apps down for ~22 min. Any server render goes through
  `scripts/srv_render.sh` and sizes DuckDB with `source(here("libs/duckdb_budget.R")); duckdb_tune(con, tmp_dir)`,
  never a literal limit. Writes to a v1–v7 `sdm.duckdb` need `INSTALL icu; LOAD icu;`.
- The release session owns server `.env`, Cloudflare Access, Caddy and the docs deploy while it is in
  flight: atlas-9's Caddy route, Access app and `.env` keys are handed to it to sequence, not applied in
  parallel. It also asked to be told before any `APP_BUNDLE_S3` publish or the v7 `cell_model` upload.
- **`_coverage` zone metrics are v7b ONLY** (reported by the release session, not verified here): written
  by `build_v7b.qmd` when `COVERAGE_FLOOR > 0`; v7, v8, v9 are not and will not be backfilled (v10 gets
  them natively via `msens::coverage_sql(weight = "area")`), so they are **optional by presence**, like
  `methods`. Shape: metric key = the FULL component key + `_coverage`, i.e.
  `extrisk_{sp_cat}_ecoregion_rescaled_coverage` (bird, coral, fish, invertebrate, mammal, other, turtle)
  and `primprod_ecoregion_rescaled_coverage` (8 keys); rows for **Program-Area zones only**
  (`zone.tbl = 'ply_programareas_2026_v7b'`); value in **percent 0–100**, DOUBLE. A Program Area where the
  component has no scored cell has NO coverage row (absent, not 0): e.g. the Alaska areas have no
  `extrisk_turtle_…_coverage` row. **Reportability is read from the ABSENCE of the
  `{component}_ecoregion_rescaled` row**, never from `…_prepctareaweighting` (those rows stay even for a
  not-reportable pair). In released v7b nothing further was dropped
  (`tbl_component_not_reportable_programarea_v7b.csv` is empty), so fixtures for the rule must be synthetic.
- **A manifest capability is not "fetchable from the bucket"**: `v7/` and `v7b/` hold only `tables/` +
  `manifest.json` on S3 (no `serve/`); only v8 and v9 have `serve/{cell_model,model_cell}/`. v7b's
  `cell_model` (570,090,776 rows, 428 tiles, 0.79 GB, usa05 tile key via
  `msens::cell_model_tile_sql(ncol = 3103)`) lives on the server only, and
  `manifest$capabilities$cell_species_list` is TRUE because the server-side apps can read it. The `app/`
  contract (atlas-1) must therefore advertise what a static app can actually fetch, decided at bundle
  build time from what exists on S3, not copied from `manifest$capabilities`. Uploading v7b's tiles is the
  same notebook as v7's (`-P ver:v7b`) but it publishes under a restricted prefix: **that one goes
  through Ben**.
- **From the release session after its v7b backfill (2026-09-21, not verified here):** msens1 is free again;
  shared repos: msens `0faf3b8` (0.42.1), workflows `90ad6026`, api `8db76cb`, server `5ae8869`, docs `12007fa`.
  (1) The static STAC catalog lists `v7 v7b v8 v9`; a LEGACY release's Item (v7b today; v1–v7 when
  re-backfilled) points at `{ver}/tables/model_asset.parquet` (`mdl_seq` → content-addressed `cog_url`) with
  titiler `/cog` links, while v8+ Items keep `dist_merged/` + the SQL template: **a client branches on the
  manifest's `id_field`, never on one assumed Item shape**; the live v7 Item is still the OLD shape until its
  manifest stage is re-run. (2) `storage.marinesensitivity.org` index pages are regenerated after every
  backfill; restricted releases get NO pages by design (`/marine-atlas/v7b/` → 403): a missing index page is
  not a missing release, `versions.json` is the registry. (3) Every publishing backfill reloads BOTH Shiny
  instances (a few seconds of restart). (4) In the server `.env`, values containing `|` stay double-quoted
  (atlas-9). workflows `CLAUDE.md` has a new section "A release that lands registers itself" + the msens1
  memory rules.
- v7b `model_asset` re-uses v7's content-addressed COGs for every model except the six merged turtles
  (`mdl_seq` 54238–54243, new hashes); `mdl_seq` ids are unchanged from v7, so v7 deep links map 1:1.

## Architecture

```
GitHub Pages  marinesensitivity.org/atlas/          preview.marinesensitivity.org/{ver}/atlas/
  (public releases)                                    (same commit; Cloudflare Access → Caddy jwtauth;
        │                                               session.json ⇒ preview mode, version = the path)
        ▼
index.html  static shell + inline early fetch ──► latest.txt · versions.json · session.json
        │                                          {ver}/manifest.json · {ver}/app/boot.json
        ├─ Tier 0 (no WASM)  map: basemap ⊕ zones PMTiles (S3) ⊕ raster tiles (titiler ← COG on S3)
        │                    zones choropleth · zones table · flowers · legend · release picker
        │                    species: app/taxon/{xx}.json → card, inputs, assets, camera; taxa.json → picker
        ├─ Tier 1 (lazy)     DuckDB-WASM worker: app/zone_taxon · app/taxon · app/taxonomy
        │                    app/cell/tile=* (wide metrics) · serve/cell_model/tile=* → sql/*.sql (msens twins)
        └─ Tier 2 (optional) opfs://atlas/{ver}…duckdb : those tables + LRU tiles, digest-invalidated
places:  pick zones · draw (terra-draw) · upload (GeoJSON, SHP.zip, KML, GPX, FGB, WKT[, GPKG])
         → g1 codec in #pl → coverage.ts (planar, half-even pct) → blended zone method
report.html?ver&…#t&pl  → same modules → print CSS / HTML / ZIP [/ DOCX]
```

Repo layout: `index.html`, `report.html`, `gallery.html`; `src/lib/{release,state,grid,geo,engine,raster,
map,report,ui,brand,analytics}`; `src/lens/{scores,species}`; `src/places`; `sql/`; `tests/` (+ fixtures
from msens); `e2e/`; `scripts/{verify,smoke_release,size-budget,parity/}`; `docs/{design,spikes}`.

## Phases, dependencies, gates

```
A0 scaffold + 4 spikes ─┬─ A1 app/ contract (msens + workflows) ──┐
                        ├─ A2 core runtime ───────────────────────┼─┬─ A4 scores lens ──┐
                        └─ A3 design system (★ mockup checkpoint) ┘ ├─ A5 species lens ─┼─ A7 report ─ A8 verify · 508 · perf ─ A9b cutover
                                                                     └─ A6 places ───────┘
A9a Pages + preview host: any time after A0 produces a build
```

| # | subplan | model (steps / review) |
|---|---|---|
| A0 | `atlas-0 scaffold + spikes.md` | Sonnet; Opus for the spike verdicts / Opus |
| A1 | `atlas-1 data contract + app bundles (msens, workflows).md` | Opus functions + fixtures, Sonnet notebook + backfill / Opus |
| A2 | `atlas-2 core runtime (release, state, grid, engine, OPFS, places).md` | Opus codec, coverage, OPFS, SQL; Sonnet the rest / Opus |
| A3 | `atlas-3 design system + shell (MMA brand).md` | Opus spec + mockups + a11y; Sonnet components; Haiku icon/token maps / Opus |
| A4 | `atlas-4 scores lens.md` | Sonnet / Opus |
| A5 | `atlas-5 species lens.md` | Sonnet; Opus deep links + antimeridian camera / Opus |
| A6 | `atlas-6 places (select, draw, upload, share).md` | Sonnet; Opus parsing + 180° / Opus |
| A7 | `atlas-7 report.md` | Sonnet; Opus data model + print / Opus |
| A8 | `atlas-8 verification, accessibility, performance.md` | Sonnet; Opus audit |
| A9 | `atlas-9 deploy, preview gate, cutover.md` | Sonnet / Opus |

- A2 develops against fixture JSON from the A1 schemas; its parity step waits for A1's v7 + v9 push.
- **Gates that stop the line:** any first-paint spec that passes with DuckDB statically imported; the
  tile-width assertion; parity `max|Δ| ≥ 1e-9`; the Program-Area-tracing gate; the hash seen in any
  request; the public host requesting anything under a restricted release; axe serious findings; a
  budget regression; a red msens test. Every gate ships with the seeded fault that turns it red.
- Size: about 40 agent runs (≈ 70 % Sonnet). Long poles: A1's backfill (v7 `cell_model` 757 MB up, nine
  `app/` builds) and A8's cross-browser matrix. Budgets are in atlas-8.

## How to execute with agents (sufficient, not excessive)

1. **One agent per subplan step group, given the subplan and the files it names.** The surveys are done:
   `atlas-refs/` is the specification, and an agent reads the sections a subplan cites, not the files.
2. **Model by task**: Sonnet for building against a checklist, CI, notebooks, docs; Opus for rules with
   an R twin (codec, coverage, scoring SQL, the contract builders), for anything security- or
   gate-shaped, for design judgment, and for every Review checklist; Haiku only for mechanical,
   verifiable transforms (atlas-3). **Fable never writes or reviews here: it orchestrates at high effort.**
3. **Worktrees** for A2 ∥ A3 and for A4 ∥ A5 ∥ A6; A1 on a branch `atlas-contract` in msens and workflows.
4. **Reports are short**: files changed, gate output verbatim, what is unmet. The orchestrator re-runs
   the gate commands itself.
5. **Nothing publishes or deploys without its flag, in a separate turn from the code it runs.**

### Orchestration prompt

Start Claude Code in the app's own repo so worktrees apply to it (A0 fills it):
`mkdir -p ~/Github/MarineSensitivity/atlas && cd $_ && git init -b main && claude --add-dir ../workflows ../msens ../server ../apps ../api ../docs ../MarineSensitivity.github.io`,
then `/model` → Fable 5.1, effort **high**, and paste:

```
You are the orchestrator for "../workflows/.claude/plans_todo/2026-09-20 atlas app plan.md". Read it
fully; read a subplan only when its phase starts. You do not write feature code: you delegate with the
Agent tool, verify gates, merge and commit.

Per phase, in the order and parallelism of the plan's "Phases" section:
1. Launch one Agent per step group with the model the subplan names: model "sonnet" for Sonnet steps,
   "opus" for Opus steps and every Review checklist, "haiku" only where a subplan says so. Never "fable".
   Agent prompt: "Read <subplan path>, this repo's CLAUDE.md, and only the files and atlas-refs sections
   the subplan cites. Do Steps <n–m>. Run the Gates that apply, including each gate's seeded fault, and
   paste their output verbatim. Report under 300 words: files changed, gate results, anything unmet or
   moved. No narration."
2. isolation "worktree" for atlas-2 ∥ atlas-3 and for atlas-4 ∥ atlas-5 ∥ atlas-6. atlas-1 runs in
   ../msens and ../workflows on a branch "atlas-contract"; atlas-9 in ../server on "atlas-preview".
3. Re-run every gate command yourself before accepting a step; a gate with no fault that turns it red
   goes back. Then an Opus agent runs the subplan's Review checklist; at most two fix rounds, then stop.
4. Green → merge in dependency order, commit with the gate numbers in the body, append a dated "done"
   line to the subplan.
5. Stop and report (continuing everything not blocked) at: a gate still red after two rounds; the
   atlas-3 mockup checkpoint (give the screenshot paths); a step that depends on a ⚑ decision Ben has
   not answered (atlas-7 export 4 needs D9; VITE_SEAL needs D10; D1, D6, D7 and D7b are decided).
6. Never publish or deploy in the same turn as the code that does it, and only under the named flag:
   APP_BUNDLE_S3, DEPLOY_ATLAS, DEPLOY_CADDY, DEPLOY_ACCESS, CHECK_PREVIEW. Pushing the atlas repo's main
   (→ Pages) is fine once atlas-0's CI is green. Never PROMOTE_LATEST; never write latest.txt or
   versions.json; server renders only through scripts/srv_render.sh.
7. Long runs (renders, backfills, the Playwright matrix) go in the background with logs under
   _output/logs/ or test-results/; judge them by exit code and the gate's numbers, never by log prose.
```

### Per-phase prompts (if run one at a time)

- **A0**: `Do "../workflows/.claude/plans_todo/atlas-0 scaffold + spikes.md"; sonnet for the scaffold and harnesses, opus for the four verdicts; report the pins and the measured first-frame time.`
- **A1**: `Do "atlas-1 data contract + app bundles (msens, workflows).md" on branch atlas-contract in ../msens and ../workflows; opus for msens functions, fixtures and the tile writer, sonnet for the notebook and backfill; report the budgets table, the tile-width gate on both grids and the GAA tracing numbers.`
- **A2**: `Do "atlas-2 core runtime …md" in a worktree; opus for codec, coverage, OPFS store and SQL twins, sonnet for the rest; report parity max|Δ| for v7 and v9 and the OPFS matrix.`
- **A3**: `Do "atlas-3 design system + shell (MMA brand).md" in a worktree; opus writes the spec and three mockups and stops for Ben's checkpoint; then sonnet builds the gallery; opus a11y review; report the contrast script and axe results.`
- **A4 / A5 / A6**: `Do "atlas-4 scores lens.md" (or -5, -6) in a worktree; sonnet builds against the parity checklist, opus reviews it line by line against atlas-refs; report the checklist with assertion ids and the gate numbers.`
- **A7**: `Do "atlas-7 report.md"; opus for lib/report/model.ts against the R fixtures, sonnet for the document and exporters; DOCX only if D9 says so; report the numbers gate and the PDF text-extraction check.`
- **A8**: `Do "atlas-8 verification, accessibility, performance.md"; sonnet fills the matrix and budgets, opus audits every gate for a seeded fault and does the keyboard walk; produce docs/parity.html and the intentional-differences list.`
- **A9**: `Do "atlas-9 deploy, preview gate, cutover.md" part a (Pages, sidecar, Caddy route, session.json, access, CHECK_PREVIEW) now; part b (B1→B2→B3) only after Ben signs docs/parity.html; sonnet, opus review; report CHECK_PREVIEW and the redirect matrix.`

(Deterministic fan-out is available too: say "use a workflow" and A4–A6 plus their reviews can run as
one Workflow script instead of hand-launched agents.)

## Decisions still open (decide before the phase that needs them)

1. ~~D7 / D7b~~ decided 2026-09-20: harmonize with the zone method incl. `pct_covered`; clip custom places
   to US waters.
2. ~~D6~~ decided 2026-09-20: presentation gate now; protecting restricted data from direct reading is a
   committed follow-up (atlas-9, "Later").
3. ~~D9~~ decided 2026-09-20: yes, build Word output; there is no ESP template for docx yet, so none to
   match (style the .docx from the brand tokens).
4. ~~D10~~ decided 2026-09-20: seal and agency name are ok to use; spell it "Minerals"; skip the
   Offshore Hub wordmark (internal only for now).
5. ~~D1~~ decided 2026-09-20: `atlas`.
5a. ~~D13~~ decided 2026-09-21, relaxed the same day by Ben: **450 KB static** + 150 KB runtime workers, both gated; the self-hosted brand fonts stay.
5b. ~~D14~~ decided 2026-09-21: keep GeoPackage, lazy and on demand (consented 22 MB download, GeoJSON fallback).
5c. ~~D11 and v7b~~ decided 2026-09-21: v7b at smoke level through the same `app/` contract, like v8.
5d. Also confirmed by Ben 2026-09-21: the **D8 addendum** (coverage reads rings literally; one shared unwrap
   rule) "sounds fine"; **the seal: yes** (approval in hand; `VITE_SEAL=1`, agency spelled "Minerals"); Ben
   turned OFF GA4 Enhanced Measurement "page changes based on browser history events" for `G-9HW6L751XG`
   (the one analytics leak code cannot close; see the atlas repo's `docs/analytics.md`).
6. **Tile egress**: custom places read 2–24 MB tiles straight from S3 (a 40-tile place ≈ 100 MB). Fine at
   today's traffic; a CDN in front of the bucket is the later fix, and would also add compression.
7. **OBIS occurrences** depend on `h3t.marinesensitivity.org`; keep as an optional layer that hides
   itself when the service is down (recommended), or drop it from the port.

## Risks

- **DuckDB-WASM OPFS is young** (dev-tagged builds, a write bug on `latest`): pinned by S1, feature
  detected, never required; the memory store is the default path and the tested one.
- **titiler stays a dependency** for pixels: if msens1 is down the rasters go blank while zones, scores,
  tables, places and reports keep working from S3. S3 spike measures the way out.
- **Phones and big custom places**: batched aggregation, a fetch plan the user confirms, tile LRU.
- **Legacy releases**: handled once, in msens, at publish; a retired release that cannot be backfilled
  loses a capability, not the app.
- **GitHub Pages** cannot send headers: no COOP/COEP (single-threaded DuckDB is ample for these working
  sets), 10-minute cache on everything (hashed assets + ETag revalidation make that harmless).
- **A scoring number will change** for drawn polygons (D7): it is the fix, it is documented as an
  intentional difference, and the Program-Area-tracing gate is what makes it defensible.
- **Two URL conventions** (query on Pages, path on preview): one resolver, unit-tested, plus the
  redirect matrix in atlas-9.

## Close-out

When A9-B3 is done: move this plan, its subplans and `atlas-refs/` from `plans_todo/` to `plans/`; update
`workflows/CLAUDE.md` (the "ONE app" section becomes the Atlas; `build_app_bundle`, `APP_BUNDLE_S3`,
`DEPLOY_ATLAS`; the `app/` contract as a release requirement) and `apps/README.md` (retired); add the
`app/` contract to `bootstrap-release` and `publish-sdm` skills so v10 ships it from day one; email Tim
the parity page and the preview links.

## Fiddly bits (for later) — Ben, 2026-09-22: "Flag any fiddly bits for fixing later"
Deferred deliberately so the app moves; none blocks a lens. Each has a home (subplan) and a measured fact.
- **Nine manifests mislabel their subregion zone set** (`subregion_2025-06` on every release; only v8/v9 scored
  that geometry). Owner: `build_version_manifest.qmd` / the next release session. Under D17 the app never draws
  subregions, so it is cosmetic for the atlas. (atlas-1 log, 2026-09-22.)
- **v8 `zone_taxon` has no rows for subregion `AT`** (scored, 52,674 cells, 0 taxa; v9 has 7,562). Bundle carries
  `n_taxa = 0`; not drawn under D17. Rebuild v8's table or leave (restricted, superseded by v9). (atlas-1.)
- **`manifest.json` `app{}` block**: patches in `workflows/_output/app_bundle/patches/` are NOT applied by the
  publish; the release session applies them (`build_version_manifest.qmd`), then the atlas reads capabilities.
- **`atlas-contract` → main merges** (msens 0.43.0 eleven rounds; workflows): NAMESPACE hygiene (rebuild as
  0.42.1's file + added lines), drop `.Rd` churn, re-run suites; needs Ben + a release session. Until then the
  server's installed msens is 0.42.1 and cannot build bundles (the laptop can).
- **Guard/flag regressions live only in the notebook's render-time selftest** (workflows has no test runner).
- **Preview host** (`server` branch `atlas-preview` @ `8461a24`): deploy is Ben's call (`DEPLOY_CADDY=1`, after
  `chown` of `/share/atlas_preview`); the bare `redir`s in the existing Shiny routes answer before jwtauth
  (harmless 30x); per-version AUD at the origin is optional hardening. `CHECK_PREVIEW` probe for
  `/{ver}/atlas/` and the public host's 302 for restricted `/{ver}/atlas/` are atlas-9 Deliverable 5.
- **atlas-2 timing ratio** (`tests/geo/coverage.test.ts:~283`) too weak to catch O(n²) alone; the absolute
  budgets do. atlas-8.
- **atlas-3 leftovers** in atlas-8: nested landmarks in `Panel`, no status when the bundle fails to load, skip
  link bypasses the rail, 27×28 filter inputs on coarse pointers, `?theme=navy|paper` vs `dark|light`,
  Categories demo table at 320 px.
- **OPFS store not wired** into the app yet; whoever calls `openTableStoreBackend()` passes
  `restrictedVersions` (atlas-4 note).
- **S2 first-data-frame timing** on the CI runner and a real-Safari OPFS check: atlas-8.
- **Data protection for restricted releases** (D6 "Later"): unguessable prefix or origin-served data; not
  started.
- **Shard bboxes are not minimal-span, and v7 has none** (found 2026-09-22 by the species data layer, measured
  over every published shard): v9 has 48,378 bboxes, 5,053 spanning > 300° (Pacific taxa written wrapped, e.g.
  `[-173.7,-16.05,163.7,20.2]`), only 16 with `xmax > 180`, 38,448 null; v7's 16,153 are ALL null (the
  `model_asset` bbox path yields nothing for that generation). Contract says "precomputed in the
  `lon_span_agg` frame". The lens compensates client-side (D8 complementary frame + study-area fallback);
  the msens fix (`.app_bbox` / `lon_span_agg` on v9; the v1–v7 asset bbox) is atlas-1's, re-publish
  `app/` after (new `built_at` invalidates caches).
- **Basemap is CARTO's raster tiles** (`basemaps.cartocdn.com`, chosen as the smallest working option for the
  first paint): a third-party dependency for every page load; a self-hosted PMTiles basemap on S3 would
  remove it. atlas-8/9.
- **Playwright Firefox teardown race**: with the map now loading a worker in every shell spec, a test ending
  while a `page.route` handler is mid-fetch reports "route.fetch: Test ended" outside any test and skips the
  next one (seen once at load 27, 2 clean runs after). Route handlers must tolerate a test ending. atlas-8.
- **Shard bbox contract** (above) also means a v7 species always frames the study area, never its own range.
- **Species lens chrome not built** (2026-09-23): outlines control, globe minimap, fullscreen/nav/scale/geocoder,
  OBIS overlay + `toggle_obis`/`select_outlines`/`open_about` events. Shared map chrome → one place, atlas-8.
- **Map chrome placement** (2026-09-23): MapLibre's nav/fullscreen/scale controls and a geocoder are real
  buttons that would nest inside `#map[role="img"]` and fail axe; neither lens built them. Decide once
  (a toolbar outside the map element, or drop `role="img"` for a labelled region) in atlas-8.
- **driver.js tour** not added by either lens (new dependency, lazy-chunk risk at 412 KB static). atlas-8.
- **Treemap is one level** (species category), not the 6 WoRMS ranks the checklist describes. atlas-8.
- **CI runs no Playwright** (2026-09-23): `pages.yml` runs vitest and the build gates only; the browser
  suites (three engines, the timing project, the gallery) run on the laptop. Add them to CI in atlas-8 with
  the timing project as its own serial step.
- **Zone rows carry no `name` / `label_pt`** (2026-09-23, every published boot, all eleven): the atlas-1
  contract promised both (`zones{unit:[{key,name,label_pt,…}]}`); msens's `app_zones()` writes neither. The
  app shows keys ("GAA") where names belong and draws no zone labels. msens fix + re-publish `app/` (new
  `built_at`). atlas-1.
- **v8/v9 publish two primary-producer component keys** (`extrisk_primary_producer_*` and `primprod_*`)
  and `flower_default` lists both as petals; the lens de-duplicates (0.9.12). Whether the bundle should
  publish one is atlas-1's question. 
- **Upload parsers run on the main thread** (2026-09-23; lazy, but not off-thread: 290 ms sync at 49 k
  vertices). Worker or a documented reading of the plan's line. atlas-8.
- **The refusal-copy test can pass a useless message** (a "mysteryRule" with vague what/why/fix passes every
  assertion). Per-rule assertions in atlas-8.
- **Places list shows "—" for coverage/composite on geometry places** (`rowFigures` null). atlas-8.
- **`noSecondMapCopy` guards only the map** (2026-09-23): the atlas-7 checklist line covers ramps,
  categories, cite and SQL too; widen the scan. atlas-8.
- **`VITE_SEAL=1` breaks the offline HTML's self-containment** (the lockup `<img>` is a live URL; no test
  builds with the flag): inline or embed the seal on export. atlas-8.
- **Playwright `reuseExistingServer` is a false-result trap on a shared machine** (2026-09-23, seen twice:
  a fault run that passed falsely and a fix run that failed falsely, both because another process held
  4331 with a different build). Gate: the webServer's health URL should answer a build hash the spec
  compares with `dist/`'s, or each run takes a free port. atlas-8.
- **Basemap (Ben, 2026-09-23, with a screenshot): CARTO's RASTER tiles now watermark "API KEY REQUIRED".**
  The map module had chosen `basemaps.cartocdn.com/dark_all|light_all/{z}/{x}/{y}.png`. Ben: "use the ones in
  ../../CalCOFI/explore that do not for dark/light themes" = CARTO's VECTOR styles
  (`gl/dark-matter-gl-style/style.json`, `gl/positron-gl-style/style.json`), verified key-free today
  (200, 70 KB, `carto.streets` vector tiles + sprite + glyphs). Swap in progress (0.10.11): the style is
  fetched and composed into the ONE style object as CalCOFI's `basemap.ts` does; the report's map follows
  through the shared builder. The earlier "self-hosted basemap" fiddly bit stays as the long-term answer.
- 2026-09-23 · **Public-host `session.json` probe shows as a console 404.** Plan D6's inline early fetch kicks
  off the same-origin `session.json` request eagerly (never awaited on the public path); on the public host it
  404s, which Chrome logs as an error and Ben noticed. Correct by design, noisy: only issue the probe when a
  candidate release is restricted (`requiresSession()`), in BOTH copies (the inline script and `session.ts`),
  with a case added to the shared table. Also: `share-modal.js` errors in Ben's console are a browser extension,
  not `dist/`.
- 2026-09-23 · **`test:faults` is the longest CI job (~11 min)** because three faults build inside a throwaway
  worktree; split into a fast vitest-fault job and a browser-fault job when it starts to hurt.
- 2026-09-23 · **`scripts/verify.mjs` runs chromium only in CI** — it launches browsers itself and cannot take
  the headed-Firefox recipe (`FIREFOX_HEADED` + xvfb); teach it the recipe (all three engines measured at 315 s
  locally) so the matrix covers WebKit/Firefox on the runner.
- 2026-09-23 · **Regenerate `docs/parity.html` after each merged fix** (`npm run parity:shots` is a live-network
  step): G-23/24/25 are fixed on main but the page still lists them open; the picker (combobox) and treemap copy
  changed the screenshots.
