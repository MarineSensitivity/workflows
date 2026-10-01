# atlas-8 phase review (Opus 5.5, `claude-opus-5-5[1m]`) — atlas `main` @ `89895b1` (0.10.21), 2026-09-23

Read-only review (no vitest/Playwright/build run; `git apply --check` shows all 11 `tests/faults/*.patch` apply).
**No blocker for pushing 0.10.21.** Two things block Ben's signature on the parity page: M7 and the page being stale.

## Verdicts
1. Every gate has a committed fault that turns it red: **PASS WITH FINDINGS** (M2, m1, m2, m9).
2. No "element exists" assertion stands in for pixels/numbers: **FAIL** (M4, M5, M6, m10).
3. Intentional-differences list complete: **FAIL** (M7).
4. 508 note claims nothing the matrix does not test: **FAIL on the letter, nothing blocking** (M8).
5. 0.10.21 spot review: **PASS WITH FINDINGS** (M1, m3, m4, m5, m6).

## Major
- **M1 The scores map click is still panel-bound** (same class 0.10.21 fixed for map inputs). Click handler, selection
  write, popup and Esc listener live in `ScoresLens.svelte:133-182`, mounted only inside the panel body
  (`Shell.svelte:819-835`; `Panel.svelte:87` renders nothing while collapsed). Desktop with the panel collapsed or the
  Places tool open (`Shell.svelte:805-811`): a scores click does nothing (no `sel=cell:`, no popup, no announcement);
  species' click is wired at Shell level (`:358`). Fix: lens-level `handleMapClick` (store or Shell-level owner like
  species'), flower fetch stays in the panel; new `scores.collapsed-panel.spec.ts` case (seed collapsed, click an ocean
  cell, assert `location.search` matches `sel=cell:` and `.atlas-popup` visible; variant with the Places tool open);
  `test:faults` patch moving the handler back into the panel; a parity-page line if not fixed.
- **M2 `test:faults` cannot tell "red for the right reason" from "red for any reason"** (`scripts/test-faults.mjs:237-284`
  never runs the gate unpatched, checks only `status !== 0`; only `node_modules` symlinked (`:250`), so a build error, port
  clash or missing `public/duckdb-ext/` reads as RED). `scores-state-panel-bound` reddens at the precondition
  (`collapsed-panel.spec.ts:73` `getLayer` wait), not the pixel poll. Fix: `expectFailure: RegExp` per `FAULTS` entry
  (e.g. `/no score raster pixel painted/`, `/button-name/`, `/G-25/`, `/focus must return to the version chip/`) matched
  against the gate output; in the collapsed-panel spec drop the `getLayer` wait or make it a non-throwing poll so the pixel
  poll fails; optional `--baseline` mode running each gate unpatched first.
- **M3 CI does not run gates GATES.md says it does.** `checks` job (`pages.yml:26-97`) has `tsc` but no `npm run check`,
  `lint` or `format:check`; absent too: `e2e:analytics-privacy`, `scripts/parity/faults.mjs`, a v7 parity run (`:395` v9
  only; plan says v7 and v9). `tests/GATES.md:137-144` claims step 3 added these; rows `:97,:100,:101,:103` ("e2e not yet
  in CI") are wrong the other way (`pages.yml:183` runs them). Fix: add the three steps, an analytics-privacy job,
  `parity/faults.mjs` + v7 to the parity job; correct the GATES.md CI column.
- **M4 The report map's data layers are never pixel-proven** (`report.spec.ts:507-512`, `:218`, `:247` check only that
  `.map-print img` is visible; `captureRejectionReason` `reportMap.ts:264-272` passes on the basemap alone and fails open
  without a 2D context `:281`). Fix: in-page `drawImage` the img onto a canvas + `getImageData`; assert the pixel at the
  drawn place's centroid equals the ramp colour for overall 40 (fixture `report.spec.ts:425`) or ramp-coloured pixel count
  > 0; keep the guard.
- **M5 The theme-switch test proves neither the swap nor the paint** (`map.spec.ts:219-244`: swap proven by
  `JSON.stringify(composeStyle(inputs()))` containing "positron", not `map.getStyle()`; paint = `readPixel(0,0) != null`;
  both fixture themes share one colour `map-hermetic.ts:176-192`). Fix: different water colours per fixture theme; probe an
  ocean pixel before/after; assert `map.getStyle().sprite` contains the theme name.
- **M6 `verify.mjs` has 22 layout-only states** (17 species `:395-418`, 3 shell `:333-337`, `scores sel=cell:*`
  `:385-386`); a 404'd species raster passes; `sel=zone:*` asserts base `programarea_ln`, not the highlight; scores
  selection ring/highlight, Places pick, drawn outline and "show analysis cells" layers have no rendered-feature assertion
  anywhere (`selection-line` only in the deep-link spec). Fix: `speciesRasterProbe` (blended fixture colour as
  `species.timing.spec.ts:40`) on `rep=model` states + `species-range` count > 0 on `rep=native`; `selection-line` count
  > 0 on `sel=cell:*`/`sel=zone:*`; one e2e each for pick highlight, drawn outline, `cellOpacity` cells.
- **M7 The parity page content is wrong and stale** (stamped 0.10.17 @ `609982b84c`, `docs/parity.html:87`; shots
  08:08Z). G-23 and G-25 fixed in 0.10.19 but listed open (`scripts/parity-page/content.mjs:492`, `:475`; S-19 note
  `status.mjs:410` cites G-23). **G-24 is mislabelled and NOT fixed**: 0.10.19 called the stale bird note "G-24"
  (`CHANGELOG.md:117`, `tests/GATES.md:131`, `composition-note.test.ts:24`) but on the page the bird note is the second
  half of G-23; the page's G-24 (`content.mjs:483`) is the zones table's full-label column header, still
  `ZonesTable.svelte:68` `<th scope="col">{metricLabel}</th>`. ID-11 (`content.mjs:207-210`) and the P-07 note
  (`status.mjs:599`) say "Program Areas on BOTH lenses; `out=` … nothing reads it" — since 0.10.19 species defaults to
  `out=none` (`types.ts:133-136`); species screenshot pairs show outlines the build no longer draws. **Differences with
  no line**: scores click/popup dead while the desktop panel is collapsed or Places is open (M1); zones have no hover
  (Shiny repaints the hovered zone purple + popup; S-07 note `status.mjs:166-191` says "the whole rule is implemented");
  Report tool + top-bar Report button are a placeholder (`src/shell/tools.ts:44`); no legend on a phone
  (`ScoresLegend.svelte:75-78`, `SpeciesLegend.svelte:64-68`); theme defaults to `auto` (`types.ts:175`) vs Shiny dark;
  scores-lens search input has no handler (`Shell.svelte:703-708`); no Docs/Home nav, no preview sign-out; ID-17 omits
  `showOutsidePra` (`state.svelte.ts:27-30`, `:75`). Covered: D7, D7b, D17, printed report, D6 gate, watermark, CARTO
  vector; 0.10.21 raster `bounds` clips nothing drawable; the Places deep-link restore can be cited under ID-17.
  Evidence spot-check: 46 citations across 11 "done" rows + 48 ID `where` citations all exist. Fix: edit `content.mjs` +
  `status.mjs` (remove/mark G-23, G-25 fixed; relabel the bird note "G-23 (2)" in CHANGELOG/GATES/composition-note test;
  keep G-24 open or fix it: short header + full label in `title` at `ZonesTable.svelte:68` + header-text test; rewrite
  ID-11 + P-07; add the eight missing lines), THEN once Pages carries 0.10.21: `npm run parity:shots -- --side atlas`,
  `npm run parity:page`, `npm run parity:page:check`.
- **M8 The 508 note over-claims** (`docs/accessibility.md`): the screen-reader limitation IS still stated (`:189`,
  `:388`); the three-engine claim is false (`:36` "174 runs × 3 engines", `:180` "measured on chromium, webkit and
  firefox" — CI `verify` is chromium only `pages.yml:314`; `matrix.a11y.spec.ts` excluded on webkit/firefox
  `playwright.config.ts:100-114`; gallery + both popup specs chromium only); the axe sweep never sees Table, Flower, Places
  list, Report tool, popups or the version picker (active tool is not URL state, `gotoState` loads a URL only) — the zones
  table, the map's declared equivalent, is never axe-audited in the app. NO TEST: 2.4.7 "throughout"; 3.3.1 refusals as
  `role="alert"`; combobox arrow keys (2.1.1); 1.4.4 (cites the 1.4.12 text-spacing test `:107`; nothing tests 200 %
  resize); 1.4.5; 2.4.2 title text; the "Skip to the details panel" link; `th scope` in 1.3.1 (manual tree dump only).
  2.1.2 cites the modal focus-trap tests; the real no-trap evidence is `shell.a11y:179`, `keyboard-walk:583`. Fixes
  #8-14 have markup-only tests with no revert proof; only #1 has a committed fault. Non-VPAT terms "Supports (by
  removal)" `:90`, "Supports (exceeds AA 2.1)" `:116`. Stale version 0.10.18 `:5`; `:180-182` mangled by prettier.
  §3.1 says the map is "not keyboard-operable" (`:131`) but `map.ts:114-125` keeps MapLibre's keyboard handler. Fix:
  reword `:36`, `:109`, `:180` + an engine column; add the missing tests or downgrade the rows (`role=alert` refusal in
  `places.spec.ts`, combobox ArrowDown → `aria-activedescendant`, `toHaveTitle` for the three entry points, skip-to-panel
  test, 200 % font-size + `assertLayout`, focus-indicator check in `assertFocusUsable`); `matrix.a11y` cases opening each
  rail tool; VPAT 2.x terms; bump the version line; fix the mangled text.

## Minor
- **m1** collapsed-panel spec proves neither `mapExtra.zones` nor a ramp legend (`[data-testid="scores-legend"]` is also
  rendered by the "unavailable"/"empty" notes, `ScoresLegend.svelte:35-54`): add a `?unit=programarea` collapsed variant
  asserting `programarea_fill` features > 0; legend via `.scores-legend:not(.scores-legend--note)` + exactly two endpoints.
- **m2** Places deep-link gate has no fault and no GATES.md row: `tests/faults/places-outline-panel-bound.patch` deleting
  the `$effect` at `placesMap.svelte.ts:55-58`, registered on chromium; optionally assert the feature's bbox = rectangle.
- **m3** `placesMap.outline` has two effect writers (`placesMap.svelte.ts:55-58`, `Places.svelte:398-403`) — the shape
  GATES.md:49 fixed for `document.title`: split into a store-derived baseline + an interaction override set by pick/draw,
  compose in Shell as `interaction ?? baseline`, unit-test the precedence.
- **m4** `showCells` (`Places.svelte:320-385`) is panel-local while `mapStore.cells` and the outline persist: after a
  collapse/tool switch + remount the toggle reads "off" with cells still painted; neither in the URL. Move `showCells` into
  the store (or URL); clear the pick highlight in `onDestroy`.
- **m5** the lazy lens import has no error path (`Shell.svelte:598-604`, no `.catch`): a chunk-load failure = `scoresLens`
  null forever = the 0.10.17 symptom; same for the other lazy imports. Catch, announce, reset the guard to retry.
- **m6** the `composeStyle` input object is built twice (seam `Shell.svelte:368-384`, effect `:469-484`; the basemap fault
  edits only one). Build once as a `$derived` and use at both sites.
- **m7** G-23's "species" wording is unpinned (`Composition.svelte:61` `valueLabel` could revert): source-scan assertion.
- **m8** two scans narrower than their rules: `no-fitbounds` scans `src/lib/map` + `src/lens` only (not `src/places`,
  `src/report`); `no-readpixels` scans `src/lens/scores` only (D4 is app-wide). Widen with allow-lists.
- **m9** two "seeded fault" labels are not faults: `coverage.test.ts:126-131` asserts `Math.round`'s own behaviour (the
  corner-sliver fixture is the real discrimination); `resolveVer.test.ts:149,:160` are discriminating inputs; the plan's D6
  fault ("`access` missing → treated as public") has no patch. Relabel; add `access-missing-public` patches for
  `src/lib/release/access.ts` and `index.html`'s inline twin.
- **m10** `species.smoke:36-81` reads the raster source URL once right after it appears (would miss the 0.10.10 class):
  re-read after `DEFAULT_STYLE_FALLBACK_MS` + 1 s. `verify.faults.spec.ts:98`: call the exported `scoresRasterProbe`
  instead of `≠ RASTER_RGB`.
- **m11** `LayersPanel.svelte:84` calls `setProjection` imperatively beside `selStore.set({proj})` (pre-existing,
  documented "imperative twin"; a second path outside the one composed style) — consider removing.
- **m12** `showOutsidePra` is map-visible state not in the URL (pre-existing, documented `state.svelte.ts:27-30`): name it
  in ID-17 or add it to the codec.

## What holds in 0.10.21
Scores-only inputs gated at both `composeStyle` sites (`Shell.svelte:374-383`, `:476-482`, `zonesForStyle` `:332-337`);
the lens store is a dynamic import keyed on the lens (`:597-605`) policed by `tests/shell/lazy-lens-imports.test.ts:67`,
`:94-116`; `composeStyle` synchronous at both sites, one `setStyle` in `src/` (`style.ts:379`); no new `any`/`ts-ignore`;
`bind:` in Shell is only `bind:this={mapEl}` (`:756`); the places `$effect` is owned by Shell (`createPlacesMapStore`
called synchronously at `:274`); `createScoresLens` has no `$effect`, so building it inside `.then` is safe.
`basemap-not-reactive.patch` regenerated in 0.10.21 is the same edit (removes `basemapStyle: basemapStyles[resolvedTheme]`
from the reactive effect, `Shell.svelte:474`), caught by its pixel assertion (`scores.firstpaint.spec.ts:184-203`).

## Sampled gates (36; all REAL unless noted)
size budget (fixture builds inverted in CI) · dist no session.json · no absolute /assets/ (only `/assets/` covered) ·
duckdb-ext mirror · inlined tokens · pins · contrast · rmod (+patch) · coverage half-even (gate real via corner-sliver;
labelled fault tautology-adjacent, m9) · coverage vertex ratio (wall-clock) · layer order · G-25 `out=` · no-fitbounds
(scope m8) · no-readpixels (scope m8) · document.title one writer · lazy lens chunks · report model vs R (6) · D7b clip ·
batching · G-23 rounding (caller label unpinned m7) · "G-24" bird note (mislabelled M7) · D6 missing access (fault not
materialized m9) · inline early-fetch twin (no drift fault) · feedback no-hash · OPFS eviction · axe matrix · modal focus
restore · modal Esc · verify job · basemap late style.json · collapsed panel (red lands on the `getLayer` precondition,
M2) · Places deep-link outline (NO FAULT, m2) · verify raster 404 (indirect, m10) · verify setStyle drops zones · verify
panel off-screen · report map not blank (NOT A PIXEL GATE, M4).
