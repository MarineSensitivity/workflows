# U4 "Layers model" review — Opus 5.5 (`claude-opus-5-5[1m]`), branch `worktree-r2-u4` @ `5a7c731` (0.10.31), 2026-09-24

Read-only; live CARTO styles + v1/v3/v7/v9 manifests fetched; `git apply --check` (patch applies); `git merge-tree` dry run.
Verdicts: 1 one style PASS w/ findings (byte-identical claim FALSE against real CARTO) · 2 order gate PASS w/ findings
(data groups invertible) · 3 URL grammar PASS w/ findings (missing groups appended at the TOP, not at their default
position) · 4 ecoregion PASS w/ findings (`out=` contradiction unresolved; verify.mjs:436-446 + style.test.ts:442 now
false) · 5 short labels FAIL (code right, test missing) · 6 a11y PASS w/ findings (22 px switch; no `aria-valuetext`) ·
7 fault PASS w/ findings (gate asserts `getStyle()` only) · 8 merge notes below.

## Blocker
- **B1 opacity REPLACES instead of scaling** (`layerStack.ts:222-231`): every zone unit carries an invisible query fill
  `#000000` at `fill-opacity: 0` (0.10.26 B3) → dimming "Program Areas" to k paints every Program Area black at k;
  raster 0.6 at 100 % but 0.95 at 95 % (non-monotonic); selection 0.15 → k and the per-cell `["get","opacity"]`
  expression overwritten; overlay 0.55 / range 0.5 / subregion 0.7 / CARTO's own opacities replaced; tests LOCK IT IN
  (`style.test.ts:627,636`, `layerStack.test.ts:152-158`). Fix: `scaleOpacity(existing, k)` (undefined → k; number →
  n·k; `{stops}` → each ×k; zoom `interpolate`/`step` → outputs ×k; else `["*", existing, k]`; k=1 no-op); rewrite the
  two tests to 0.15 / 0.28; fixtures: `data-zones` at 0.5 keeps the query fill at 0; `data-places` at 0.5 over
  `cellOpacity` → `["*",["get","opacity"],0.5]`; raster at 0.95 → 0.57; e2e pixel probe with
  `layers=…data-zones:o50…` in cell mode inside a Program Area still reads the plain raster blend.
## Major
- **M1 default basemap order changed for real CARTO** (sub-role sort `style.ts:322-331`, `LAYER_ORDER :117-121`):
  dark-matter draws `water`/`water_shadow` under `boundary_county`/`boundary_state` (state lines across lakes) and
  country boundaries under roads/buildings; positron 55 layers out of order (e.g. `waterway_label` over tunnels).
  Fix: one shared rank per run of ADJACENT basemap groups so the stable sort keeps CARTO order within a run; fixture
  `[background, boundary_state, water, road, boundary_country, place_label]` unchanged with/without the default stack;
  decide/document that basemap rows move only across data rows.
- **M2 a partial `layers=` token reorders** (`layerStack.ts:284-286` appends missing ids at the TOP; test :246-252 locks
  it): `?layers=data-raster:o50` puts the raster under the opaque land fill → invisible. Fix: insert each missing id after
  the nearest earlier `DEFAULT_LAYER_STACK` id present; update the test and `layers.spec.ts:67-70`.
- **M3 only the Data row's eye is pixel-proven**; verify's `data-zones:h` state checks only that the raster paints.
  Fix: panel-click e2e — `programarea_ln` features > 0 → 0 (layer still exists); selection ring pixel gone with
  `sel=cell:…`; hiding `basemap-land` shows the background; verify state asserts `zoneFeatureCount === 0`.
- **M4 the fault gate checks `getStyle()` only** (`-g` → `layers.spec.ts:148-161`, vacuous `>= 0` at :159): retarget to
  "promoted basemap layer painting OVER the raster" (:163); delete :159.
- **M5 in zone mode the Data row controls nothing** (raster null with `unit=programarea`; the choropleth is a `zone-fill`
  in `data-zones`). Fix: a `choropleth` role after `overlay` in `LAYER_ORDER`, mapped to `data-raster`; tag
  `zoneFillLayer(u)` with it when stops exist; rename the zones row (e.g. "Zone outlines").
- **M6 short-label test missing**: v7's real `primprod` long label + `manifest.metrics` rows in the hermetic fixtures;
  unit-test the title precedence in `scoresMapInputs`; e2e asserts the option text "prim prod, 2014-2023 avg (mg C/m^2/
  day)" and the description shown once; hide the description when equal to the option label.
- **M7 data groups can be inverted** (Selection under Land & water breaks "selection on top", `style.ts:99-103`). Fix:
  keep `data-raster < data-zones < data-places` fixed in `moveLayerStackEntry`/the panel, pin `data-places` on top,
  unit test.
- **M8 after merging main the e2e pixel expectations assume paper**: `layers.spec.ts` navigates to a bare
  `/?proj=mercator` and expects `BASEMAP_RGB`/paper blends; main's default theme is dark (navy `[10,20,40]`) → :163,
  :178, :190, :219, :269 would fail. Fix: `&theme=light` in `gotoLayersScores`. (Also check `scores.firstpaint`'s
  `gotoScoresMap` on main for the same assumption.)
## Minor
m1 Switch hit area ≥ 44 px (`Switch.svelte:28-35`, every use) · m2 `aria-valuetext` (%) on the slider · m3 slider
`oninput` → `selStore.set` per step = `history.replaceState` per step (Safari `SecurityError` > 100/30 s): write on
`onchange` · m4 refocus the moved row's button after `tick()`; keyboard-reorder e2e (URL, live region,
`activeElement`) · m5 Bathymetry "coming soon" row: switch/▲▼ still write `basemap-bathymetry:h` — disable or drop ·
m6 `aria-controls` on the Data expander · m7 delete `layersControlItems()` + helpers + describe block; parity doc row
693 / ID-10 should cite the eye e2e · m8 correct `verify.mjs:436-446` + `style.test.ts:442`; settle `out=` in a pure
tested helper (scores: ecoregion unless `out=none`; species: only `out=ecoregion` per ID-11); adjust
`layers.spec.ts:392` · m9 `Shell.svelte:518` skip the manifest ecoregion if `zonesForStyle` already has an ecoregion
unit (duplicate ids break the style) · m10 `composeStyle` throws if `layerStack` lacks a group → normalise by
appending missing groups · m11 `layers.spec.ts:7-15` header claims a `basemap-labels` test that does not exist.

## Merge notes (dry run vs `main` 0.10.35)
Conflicts: `Shell.svelte` (only the import on line 37 → `import { DEFAULT_SEL, defaultOut, resolveTheme, type
LayerStackEntry } from "../lib/state/types";`; keep from main: camera imports, `studyAreaFromBoot(boot, sel.area)` in
onMount, the study-area `$effect`, Report/Help/tour code, `docsHref`, `ReportToolComp`, the `activeTool === "report"`
branch before the species-layers branch; keep from U4: `layerStack` imports, `ecoregionUnit` (:363), `layerStack` +
`onLayerStackChange` (:507-510), `zones: ecoregionUnit ? […] : zonesForStyle` and `layerStack` inside the single
`composeStyleInput` `$derived`, the `{layerStack} {onLayerStackChange}` props on both lens mounts), `types.ts`
(`DEFAULT_SEL`: keep `layers: undefined` AND main's `theme: "dark"`; keep U4's `layers?` field, import, `QUERY_KEYS`
entry), `test-faults.mjs` (all four newer entries + U4's, retargeted per M4), CHANGELOG/package (main's version, U4's
own number), `GATES.md` (main's prettier table + U4's row). `ScoresLens.svelte`, `state.svelte.ts`, `mapInputs.ts`:
U4's apply as is. Scores `LayersPanel.svelte` auto-merges (keep main's `onAreaChange` without `flyTo`; unused
`studyAreas`/`mapHandle` may lint). `verify.mjs` auto-merges (bare `scoresRasterProbe()` now navy). After merge: M8.

## Re-check of the fix round — Opus 5.5, branch `9d2d47b` (0.10.36), 2026-09-24 ~06:05
Throwaway plant of the OLD replace semantics → 6 unit tests red (0.27/0.15/0.28 scaling, query fill 0, per-cell wrap);
pre-M1 / pre-M2 / pre-M7 / pre-M5 plants → 3/3/3/2 red. CLOSED: B1 (logic + unit; the in-zone `data-zones:o50` e2e probe is
MISSING — every OCEAN_PROBES point lies outside zones20), M1, M2 (residue: `layers.spec.ts:92-95` comment; `normalizeLayerStack`
still appends at the TOP, `docs/map.md:127` says default position), M3, M4, m4, M5 (new observation: hiding "Zone outlines"
hides the invisible query fill and hiding "Data" in zone mode hides the choropleth — both are the hit-test targets, so
zone click/pick stops working while hidden — decision needed), M8, m8 (deferral acceptable; behaviour pinned: scores always
draws the ecoregion line, even `out=none`; species never — but ID-11 on the parity page still says the opposite).
PARTIAL: M6 (fixture label is v8's `score_cell_metrics.qmd:141` string, not v7's live paragraph "Primary productivity: Oregon
State Vertically Generalized Production Model (VGPM) … 2014 to 2023" — swap in `tests/lens/scores/fixtures.ts:40,:324`,
`mapInputs.test.ts:91`, `layers.spec.ts:479`), M7 (model fixed + tested; the PANEL never disables a rejected move — four
enabled no-op buttons announce "moved to position N"; `parseLayerStack` does not enforce the data order — a crafted URL
puts Selection under the raster and only Reset repairs it). Merge vs main correct (Shell keeps U5/U3/area/U6 + the
`$derived`; style.ts keeps the theme-aware outline + choropleth; 25 faults all apply; `map-hermetic-same-theme-color.patch`
byte-identical to main's). Verdict: ONE more small round — M7 panel + parse order, M6 label, ID-11 + regenerate, the B1
in-zone e2e probe, ride-alongs (`layerStack.ts:197-202` "never multiplies" comment, `:57` deleted-helper reference,
`docs/map.md:74/:96/:127`, `test-faults.mjs:526` comment, CHANGELOG M4/m4 labels).
