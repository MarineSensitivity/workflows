# Parity-page audit (Opus 5.5, `claude-opus-5-5[1m]`) — atlas `main` @ 0.10.28, 2026-09-24 ~01:50 CEST

Read-only. Trigger: Ben found S-13/S-14 (flower petals missing) and S-01 (study area Alaska never moves the camera)
marked "done" with evidence that could not fail. The auditor read every cited evidence test (247), all 26 shot images,
`apps/{scores,species}/app.R`, msens `atlas-contract` and v7's live `manifest.json` + `tables/model_asset.parquet`.

## Ranked defects (S-01, S-13/S-14 excluded: in flight)
### Blocker
- **B1 The report map draws no place for any Program-Area report on any published release (R-16, R-27).** Shots
  `report-figures-atlas`, `report-gaa-atlas`: bare basemap, Gulf of Mexico cut off; text still says "Map: 1 place…".
  Cause: `src/report/reportMap.ts#zonePointFromBoot` needs `label_pt`; no published boot has it (G-01) → the place is
  dropped, `combinedBbox` null, `Report.svelte:212-240` never moves the camera. The gate passes because
  `e2e/report-hermetic.ts:25-41` ADDS `label_pt` to its fixture. Fix: draw zone places from the unit's PMTiles polygon
  filtered by key and frame on it (or msens publishes `label_pt` + bbox and re-publishes `app/`). Gate: production-shaped
  fixture (no `label_pt`), rendered-feature count on the places layer, camera bounds contain the place.
### Major
- **M1 Every v1–v7 input is struck through with a false reason (P-05, P-15).** v7's `model_asset.parquet` lists 19,811
  input COGs (am 17,544; rng_iucn 1,518; bl 573; …; walrus am mdl_seq 790 → `cog/usa05/3e1d4309c691974f.tif`) and the
  manifest says `capabilities.model_cogs: true`; Shiny draws them via its `model_asset` adapter (`apps/species/app.R:397-413`).
  Root cause in msens `.app_assets()` (`atlas-contract` `R/app_bundle.R:611-622`): INNER JOIN of `model_asset` to `taxon`
  on the taxon's MERGED key keeps only merged-model assets; plus a ds_key mismatch (`am_0.05` vs `am`). Fix: join through
  the edges on `mdl_seq`, normalise ds_key, re-publish v1–v7 `app/` (`APP_BUNDLE_S3`, a release-session turn). Gate: msens
  test that v7 walrus `am` carries its COG; atlas e2e that the v7 AquaMaps pill is a button and paints.
- **M2 The Layers control is a static list; S-08's evidence tests dead code** (`layersControlItems()` `style.ts:362` has no
  caller; `LayersPanel.svelte:170-182` hardcodes bullets, no toggles, the drawn Program-Area outline not listed). → U4.
- **M3 The standalone ecoregion outline is never drawn (S-04).** Shiny draws black 3 px ecoregion lines in every scores
  pair; v7 publishes `zones/ecoregion_2025-06/zones.pmtiles`; the atlas composes only the one unit → Atlantic, Hawaii,
  Puerto Rico have no outline with Program Areas selected. Gate: `ecoregion_ln` rendered-feature count on v7. → U4
  (boundaries row; D17 still holds — not a scored unit).
- **M4 No map attribution on screen** (`map.ts:121-123` `attributionControl: false`; only inside the collapsed About,
  and CARTO is missing). Shiny shows "MapLibre | © CARTO, © OpenStreetMap contributors". → U1.
- **M5 S-16's claims are tested on a different component**: gallery DataTable specs, but `SpeciesTable.svelte` is a
  custom grid whose header says keyboard nav + virtualization are NOT implemented; it renders every row (16,153 on v7).
  Caption "adds … keyboard cell navigation" is false. → species-table round.
- **M6 The composition treemap measures something different (S-19)**: Shiny sizes by species COUNT; the atlas sums
  `suit_er_area` (Mammal 36 % largest vs small in Shiny; Invertebrate 12 % vs ~half). Only a header comment says so.
  Fix: size by count, or an intentional-difference entry + label the measure. → Ben (R8).
- **M7 The report flower has no legend and the wrong opacity (R-17)**: inline SVG `Report.svelte:548-575` has no
  category legend (spec: "legend beneath"; tooltips do not print); petal opacity 0.92 vs spec 0.5; R-17's test covers
  `flowerStandaloneSvg`, not the page SVG. → flower round.
- **M8 The Shiny species screenshots were captured before anything painted** (`scripts/shots.mjs:83-90` waits for the
  canvas + a bare 4.5 s sleep); P-08's "same COG, same titiler" comparison was never made. → parity-tooling round.
### Minor
- m1 `report-gaa` titled "Gulf of Alaska Program Area" for `z.pa.GAA` (= Gulf of America) — `scripts/shots.mjs:431,443`.
- m2 Stale text: S-03 note + G-22 say Viridis/Cividis/Magma show "unavailable" (0.10.26 fell back to built-in ramps);
  S-03's cited test title says "unavailable when…" while its body asserts `unavailable === false`; species-default caption
  says "Program Areas in white on the atlas" but the shot has no outline.
- m3 Layer labels: the atlas uses long descriptions (select truncates, legend title 3 lines) where Shiny uses "score",
  "fish: ext. risk, ecorgn"; group names differ; palette labels drop "(default)"/"(deuteranopia)"; hermetic fixtures use
  short labels so no test sees it (the G-24 fixture gap again). `boot.ts`, `LayersPanel.svelte`, `ScoresLegend.svelte`.
- m4 Unrounded flower values (`Flower.svelte:125`, `Report.svelte:563`, `flowerGeometry.ts:233`); the summary says "See
  the component table below" but the only table is a "Show table" toggle ABOVE. → flower round.
- m5 Species table: headers renamed vs §7.5 and vs the glossary terms; no visible row count (Shiny: "Showing 1 to 5 of
  16,153 entries"); CSV columns differ (atlas 13 vs Shiny 16 from `fmt_spp_tbl`) — G-04 says "not compared", it differs.
- m6 Species layer bar gold/grey vs Shiny green/orange (deliberate per `LayerBarView.svelte:123-129`; no ID entry).
- m7 Species picker opens at the first row, not the current species; the box shows the placeholder; the list is ~140 px
  wide and wraps (`SpeciesPicker.svelte:122-128, 226`).
- m8 Raw key "primprod" in the zones-table header and the report narrative.
- m9 Light theme swaps to a light basemap (Shiny keeps the dark map); white 1 px outlines faint on it.
- m10 Evidence titles overclaim: R-21 (only "no preview banner"), P-02 (gallery DataTable, not the picker), P-09 (hand-built
  style, not the lens).

## The in-flight fixes — what would satisfy the auditor
- **S-13/S-14**: THE ACTUAL BUG: petal radius = score/100 × 100 and the hub circle is r = 24 (`Flower.svelte:128`,
  `Report.svelte:566`, `flowerSvg.ts:49`) — any component under 24 is hidden entirely; on v7 that is 5 of 8 (10.4–15.96).
  Needs: (1) real data — v7's real 8 components (the hermetic `flower_default` has 3); (2) every renderer — the lens flower
  (default, zone, cell → also S-11), the report inline SVG (R-17), `flowerStandaloneSvg` (DOCX/HTML); (3) probe between the
  hub edge and the tip inside each slot, assert the category colour; (4) a seeded fault re-hiding one petal → red.
- **S-01**: the old test spied on `flyTo` and `flyToStudyArea` has NO caller in `src/`. The new test must go through the
  real app: `?area=AK` on load AND a select change FULL → AK; wait for `moveend`; assert centre + zoom against the AK
  preset (normalising 0–360 longitudes on v7); a seeded fault; keep `tileUrlLeaksStudyArea` alongside.

## Visual disagreements not on the ID list (per pair)
scores-default: no ecoregion outlines; no attribution; long truncated layer label; Layers panel covers the Atlantic/PR.
scores-programareas: Atlantic/Hawaii/PR study-area outlines missing (Shiny black). scores-component-layer: long label.
scores-study-area-ak: camera unmoved (in flight); Shiny's ecoregion divider through the Aleutians. scores-flower: ~4/8
petals; unrounded; "table below" is a toggle above. scores-species-table: renamed headers; no row count; no expander.
scores-composition: measure + palette differ; atlas adds Bird + Turtle boxes. scores-zones-table: "primprod"; keys not
names. species-default/deeplink: Shiny side never painted; six inputs struck through falsely; gold bar; no outline;
no attribution. species-picker: current species not highlighted/scrolled; placeholder; narrow. report-gaa: "Gulf of
Alaska". report-figures: no place on the map; ~5/8 petals; no flower legend; "primprod". theme-paper: light basemap
swap; faint outlines.

## Classification (id → strongest assertion → verdict) — cannot-fail or visibly wrong
S-01 NEG+dead spy · S-04 real but ecoregion missing · S-06 STATE (tile URL) · S-08 STATE on dead code · S-11 unit only
(same hub bug) · S-12 STATE · S-13 ELEM · S-14 COUNT · S-15 STATE (CSV differs) · S-16 ELEM on another component ·
S-19 measure differs · P-04 STATE (colour differs) · P-05 ELEM (false reason) · P-08 STATE (Shiny never compared) ·
P-15 STATE (wrong on v7) · R-16/R-27 pixel on a fixture production lacks · R-17 COUNT (hub bug, no legend). Unverified
(NONE/STATE): P-03, P-10, P-17, P-18, P-21, P-22, R-15, R-28. Holds (REAL): S-02, S-03, S-05, S-07, S-09, S-10, S-17,
S-18, S-21, S-22, P-01, P-02 (search), P-06, P-09 (caveat), P-11, P-12, P-13, P-14, P-16, P-19, R-01…R-14, R-20…R-26.
