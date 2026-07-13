# v8 next steps — parquet/workflow improvement → gm+nc density ingest → Phase 11 cutover

_2026-07-13 · resumable after a context clear. Sequence: **(0) a parquet/workflow improvement first**
(user-chosen), then **(1) gm+nc density ingest**, then **(2) Phase 11 cutover to make v8 default**._

Related: [[project_v8_gm_nc_density]] · [[project_v8_apps]] · [[project_v8_serving]] ·
[[project_v8_per_species_native]] · [[feedback_version_score_equivalence]] · [[feedback_stage_surgical_then_rewrite]].
Supersedes the pre-pivot `2026-06-24 v8 gm-nc reingest to DuckDB cell grid.md` (that predates the
0.05°-raster + Parquet-on-S3 pivot).

---

## 0 · Where things stand (2026-07-13)

The v8 **Marine Atlas** pipeline is built, released, served, and live:

- **Pipeline** — `targets` + `msens:` YAML metadata; global 0.05° grid; stable `mdl_key`; merge →
  taxon → validity/range/rarity → v7-faithful US scoring; `build_registry`; `release_marine-atlas`
  → partitioned Parquet on `s3://oceanmetrics.io-public/marine-atlas/v8/`.
- **Serving** — `serve.duckdb` KB view-DB over S3 Parquet; `titiler-v8`; STAC v8 (`file.marinesensitivity.org/stac/v8`), now **`native_format`-driven** (no hardcoded ds_key lists).
- **Native surfaces (done this session)** — 17,575 per-model AquaMaps COGs + **per-species PMTiles,
  one file per `mdl_key`** (replaced the dense per-dataset archive that failed to render at global
  zoom). `native_asset` registry pushed to S3 `tables/`; verified in-browser (IUCN/BirdLife/NMFS).
  Helpers `publish_pmtiles_models` / `publish_pmtiles_from_gpkg` in `msens/R/publish.R`.
  → This closes the "Phase 4b native original layers" item that was still open in [[project_v8_apps]].
- **Apps** — `app.marinesensitivity.org/scores_v8` + `/species_v8` LIVE (Chrome-verified). v7
  `/scores`+`/species` untouched. Branch: `v7` = repo default (frozen v7 apps); `main` = v8 apps;
  a git worktree `apps_v8` on the server holds v8; update via `cd apps_v8 && git pull && touch {app}/restart.txt`.
- **Commits** — msens `9bbcd2d`, workflows `cf42505c` (per-species PMTiles + registry-driven STAC).

**Remaining v8 backlog:** `gm`+`nc` density ingests (§1) and the Phase 11 cutover (§2). Minor loose
ends: Wikipedia common-name gap-fill (gated `COMMON_WIKI=1`), and notebook→HTML render so the
workflows-index cards get notebook links.

---

## 0.5 · PRECURSOR — parquet / workflow improvement (do first)

_User will do a parquet/workflow improvement before continuing. Define the exact scope, but these are
the strongest candidates surfaced this session — capture them here so nothing is lost on a context clear:_

- [ ] **Tar-based file-host sync (highest value, directly from this session).** The per-file `rsync`
  of ~2,900 native PMTiles over ssh **stalls badly** (per-file round-trip). The working fix was a
  single `tar.gz` + unpack on the server (485 MB in ~7 min vs. rsync crawling). **Fold this into
  `publish_native.qmd`'s `sync` chunk** (tar the `native/pmtiles/{ds}/` tree → one transfer → untar
  on `msens`), and **fold the `native_asset` → S3 `tables/native_asset.parquet` push into
  `release_marine-atlas.qmd`** (this session did it via a scratch script — make it reproducible).
- [ ] **`serve/model_cell.parquet` re-sort cost.** It's ONE ~3 GB file sorted by `mdl_key` for
  row-group pruning (the `RELEASE_REDO_SERVE` flag guards the expensive global re-sort). Consider:
  (a) `PARTITION_BY` an integer `mid` (the am-COG trick — avoids the ~500 GB ORDER BY spill and lets
  titiler do partition-pruning instead of row-group pruning), or (b) incremental/append re-serve.
  Measure a titiler point-read under each before committing.
- [ ] **Notebook → HTML render** (freeze/`quarto render`) so the workflows-index cards carry notebook
  links (the pipeline currently runs via `purl`+source, so no HTML is emitted).
- [ ] **Registry consolidation check** — confirm `build_registry` fully owns `dataset` + `model` +
  `native_asset` (the v7 "inline per-ingest table" gap); nothing should depend on per-ingest writes.

**Guardrail for any of these:** they touch the release/serve surface → after the change, re-run
`release_marine-atlas` and confirm `scores_v8`/`species_v8` still render (view-DB picks up the new
Parquet on app restart). No score math changes, so `pra_score_delta` should be unaffected.

---

## 1 · gm + nc density ingest

Two remaining datasets, both **density (#/km²)**, never scored in v7. Scoped in [[project_v8_gm_nc_density]].
Order: **nc first (in-repo, safe), then gm (needs Drive), then DEFER the merge/scoring fold-in.**

### 1a · nc — NOAA NCCOS seabird density (seasonal) — the clean one, all data in-repo
`ingest_sdm-nc.qmd` is **already drafted** to the dist-Parquet pattern (parses, untested; currently
uncommitted). Finish + validate:

- [ ] Source COGs in-repo: `data/sdm/raw/nc_{atl,pac}_birds_dens/*.tif` (275 total; omerc, Float64,
  `{sp_code}_{season}.tif`). `data/nc_models.csv` — **band 1 = `n_per_km2`** (density to ingest).
- [ ] Crosswalk `data/sdm/derived/nc_*/…_spp.csv` (`sp_id = GBIF:…`); guilds in `m_spp-multiple.csv`
  (slashed names, e.g. `COTE-ARTE`) → give guilds a **`GUILD:{sp_code}`** taxon authority.
- [ ] Resolve the **ds_key tension**: partition `dataset=nc` (singular), carry region + season in
  `mdl_key` (`nc|{sp_code}|{season}`) — do NOT split into `nc_atl_birds_dens`/`nc_pac_birds_dens`
  partitions (front-matter says `nc`; keep body/CSV/STAC consistent).
- [ ] `cells_from_raster` (bilinear) onto the global 0.05° grid; write `dist/dataset=nc/*.parquet`
  (`mdl_key, cell_id, val`) + `model_nc.csv`.
- [ ] **Two-tier keys**: seasonal `nc|{sp}|{season}` (serving) + a collapsed annual `nc|{sp}` (mean
  across seasons) that `taxon_model`/merge/scoring consume.
- [ ] Verify: N models, val range, a species renders; commit.

### 1b · gm — NOAA SEFSC GoMex cetacean+turtle density (monthly) — needs Drive hydration
`ingest_sdm-gm.qmd` (584 lines) is the transitional PostGIS-era notebook — **rewrite to the dist-Parquet
pattern** (template: `ingest_iucnredlist.org_ranges.qmd`).

- [ ] Hydrate 19 hexagon shapefiles from Google Drive: `~/My Drive/projects/_archive/offhab/.../
  NOAA_SEFSC_Cetacean_SeaTurtle_SDM_shapefiles/` (HEXID, LCC→4326) + `spp_gmx.xlsx`.
- [ ] Monthly cols `{Jan..Dec}_n` = animals per **40 km²** hex; nodata **-9999**; **density = `_n`/40**.
- [ ] Sum the Oceanic/Shelf split for *Stenella frontalis* + *Tursiops truncatus*; handle guilds via
  `GUILD:`; rasterize hexes → cells; two-tier keys `gm|{sp}|01..12` + annual `gm|{sp}` (mean).

### 1c · Density → [0,100] transform — a DECISION (no faithful v7 recipe)
- [ ] Only dead code exists (`scales::rescale` linear min-max, which compresses heavy-tailed density
  near 0). **Choose** a percentile-cap (e.g. p99.5) or log rescale; **store the transform +
  `val_min`/`val_max` per species in `model_{ds}.csv`** for reversibility. (The nc draft already uses
  a p99.5 cap — confirm and apply the same to gm.)

### 1d · Merge/scoring fold-in — DEFER, flag as a scoring change
`merge_models.qmd` hard-wires `suit_ds <- "am"` (~:42); non-am inputs are treated as binary "range"
whose `val` is **overwritten by `er_score`** (~:115–153). So ingesting gm/nc alone → merge **discards
their graded density**. Folding them in as suitability contributors is a **composite-score change**:

- [ ] Broaden `suit_ds` to include `gm`/`nc` and adjust `has_range`; give guilds `GUILD:{sp_code}` in
  `merge_models_prep` (~:80–119) so they aren't dropped for lacking a worms_id.
- [ ] **Gate behind `pra_score_delta`** — this moves Program-Area composites; validate the delta and
  get sign-off before it lands (see [[feedback_version_score_equivalence]]). Ingest + serve the native
  density surfaces first (safe); the scoring fold-in is a separate, flagged step.

Registry/STAC already anticipate gm/nc (`build_registry` ds_order; `stac.R` density methods +
`stac_season_cog_item`).

---

## 2 · Phase 11 — cutover to make v8 the default

Prototype-first, then promote. Keep v7 recoverable at every step.

- [ ] **Validate first** — `msens::pra_score_delta` v7↔v8 near-zero on common inputs (AquaMaps-subset
  and full). Investigate any real drift beyond the US→global grid + 0-360→[-180,180] renumbering.
- [ ] **Snapshot v7** — tag/branch the frozen v7 apps + data (`derived/v7/`, `apps` `v7` branch) so
  cutover is reversible.
- [ ] **Promote v8 apps** — flip the server so `/scores` + `/species` serve v8 (repoint symlinks from
  the v7 checkout to `apps_v8`, or merge `main`→`v7`-default per the branch convention in
  [[project_v8_apps]]); keep `/scores_v8`+`/species_v8` as aliases during transition.
- [ ] **Serving** — confirm `titiler-v8` + `serve.duckdb` are the production path; retire or alias v7
  titiler once v8 is default.
- [ ] **Docs/site** — publish a v7→v8 version-history entry (marinesensitivity.org), update the
  workflows index "last ran", refresh the methods figure if the global grid changes any framing.
- [ ] **Report** — a short v8 addendum/changelog for BOEM (global ingest, US scoring unchanged in
  method, native surfaces); confirm the score-equivalence result for the record.
- [ ] **Rollback plan** — one command to revert symlinks/branch to v7 if anything regresses.

---

## 3 · Standing guardrails

- **Version equivalence** — every integer bump validated with `pra_score_delta`; Program-Area scores
  stay ~equivalent on common inputs, or the drift is explained.
- **Reproducible by default** — changes land in committed notebooks/msens, not scratch scripts; gate
  side-effecting/expensive steps behind env flags (`REDO_NATIVE`, `RELEASE_REDO_SERVE`, `COMMON_WIKI`, …).
- **Stage the change** — surgical/safe first (ingest+serve density natively), then the flagged scoring
  fold-in, then cutover.

## 4 · Open decisions for Tim (carried from the May–Jul progress deck)
1. v7→v8 Program-Area score deltas acceptable (`pra_score_delta` near-zero)?
2. Density → suitability [0,100] rescale approach for `gm` (SEFSC) + `nc` (NCCOS)?
3. Adopt AquaX (Reygondeau 2026) as the primary 0.05° raster provider for v8+?
4. Marine-relevance cull (family list + `pct_marine ≥ 5%`) — review in/out lists?
5. Confirm reptile/amphibian exclusion + `primary_producer` inclusion?
6. Phase II reports: full global vs. US-masked models side-by-side?
