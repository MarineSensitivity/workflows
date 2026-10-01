Model: claude-opus-5-5 (Opus 5.5), orchestrator session 2026-09-25.

**HOLD.** Ben's ask 5, the wide-range species toggle, FAILS again on the live site. The fix merged for D1 (`65878fe`) does not take effect in production. Everything else in asks 1–5 passes, and D2–D7 from the 7e88b94 review are fixed. There are three new small defects: the export title reads "score · score", the Selection row says "nothing selected" while a place is loaded, and the Release-notes link is a 404. All four go in fix round `r3-rr-fixes` (0.10.75). The flower-table a11y fix is branch `r3-w3b-flower-scroll` (0.10.74).

# Eyes-on re-review, round 3 wave 1 (atlas `main` a7ba24a = 0.10.73, LIVE Pages build)

**Build under review:** `https://marinesensitivity.org/atlas/`, serving bundle `index-CTFsGNmL.js`, which contains `0.10.73`. It was deployed by CI run 36158947685's "publish dist/ to gh-pages" job at 16:11Z. Both harnesses were pointed at the live URL (`ATLAS_URL=https://marinesensitivity.org/atlas`), not at a local preview.

**Shots** are in `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-atlas/02211312-0388-470a-b56e-cb591464882b/scratchpad/review/`:
- `shots/` holds `scripts/eyes-shots.mjs` in the dark theme (41 PNGs). `shots-light/` is the same harness with `theme=light`.
- `extra/` and `extra-light/` hold `harness/extra2.mjs`, the previous review's extras harness: opacity and palette popovers, Outlines expanded, reorder facts, Selection before and after a tap, the flower table scrolled, the Download menu and phone Download modal, the downloaded PNG and SVG, the leatherback toggle, and the theme icon. DOM facts are in `extra/facts.json`.
- `probe-pl-places.png` comes from a `#pl=` link probe. `crops/` holds contact sheets.
- The comparison baseline is the previous reviews' shots under `…workflows/dc8aa535…/scratchpad/reports/{review,review2}/`.

## CI on a7ba24a (run 36158947685)
Green: build & checks, the verify state matrix (58×3), parity (v9 vs msens), OPFS, the DuckDB-WASM engine matrix, analytics privacy, and publish. Red: **e2e (gallery)**, 12 failures, both expected (see the hand-off):
- 6 × **axe `scrollable-region-focusable`** on `.flower-table-scroll` (W3 B10). The fix is on `r3-w3b-flower-scroll`.
- 6 × **screenshots**. There are no linux per-section baselines yet (`e2e/gallery.spec.ts-snapshots/` has 132 darwin files and 0 linux). This needs `npm run gallery:baselines-from-ci -- 36158947685`.
- `test:faults` and the three-engine e2e were still running at the time of writing. See the dated addendum at the bottom.

## Ben's asks

1. **No hexagon pip beside the active rail tool: PASS.** The accent fill alone marks the active tool in both themes (`desktop-03`, `desktop-06`, `desktop-11`, and `shots-light/desktop-06`).

2. **Layers pane: PASS.**
   - **Compact toggle.** "Raster cells | Program areas" is content-sized, with no "(0.05°)" (`desktop-03`, `phone-03`).
   - **Zoom to region.** The select is gone from the pane on purpose: W6 moved regions into the Search bar, as Ben asked. That supersedes this sub-item, and the Search placeholder now reads "Regions, Program Areas or lon, lat".
   - **Checkboxes.** Muted (`accent-color rgb(132,148,189)`, facts) and left of the name.
   - **Opacity.** The "◐ 100%" pill opens its popover (`extra/*-x01`, `extra-light/*-x01`).
   - **Glyphs.** The caret rotates and the reorder buttons use ↑↓.
   - **Outlines.** Now **one line each**, with correct casing: "BOEM's 2026 planning units the scores are reported for." / "Always shown — the regions each score rescales within (0–100)." (`extra/phone-x03`). **D7 fixed.**
   - **Palette picker.** Wide, with strip and name. On the phone the popover now sits fully above the nav, with Magma visible (`extra/phone-x02`). **D6 fixed.**
   - **Sphere and hidden rows.** Sphere is at the bottom, and there are no basemap rows.
   - **Selection.** Dimmed with "— nothing selected" on a bare load; undimmed after a tap (`extra/desktop-x06`; the row loses `--dim`, facts `selrow`). **But see D10:** it is wrongly dimmed when a place comes from a link.
   - **Reorder over visible rows.** Place labels ↓ is now disabled and Selection's ↑ is disabled. Facts `*-stack`: `data-places up/down disabled`, `basemap-labels down disabled`. **D2 fixed.**
   - **"Reset layers".** Fully visible at the docked default (`desktop-03`), so the earlier nit is fixed.

3. **Download menu and theme icon: PASS, with one defect in the export (D9).**
   - **Menu.** The desktop icon sits left of Help. It lists PNG / SVG / GeoTIFF, plus GeoJSON when a place exists (`extra/desktop-x08`, `extra-light/desktop-x08`). The "Download" tooltip no longer draws over the open menu, so that nit is fixed.
   - **Phone.** ⋯ → "Download…" opens a modal with the same four items (`extra/phone-x08a`, `phone-x08`).
   - **Downloaded PNG** (`extra/desktop-x09-download.png`, 1280×814, plus the light twin). It shows the map, the legend bottom-left and a three-line footer. The filename is short: `marine-atlas_scores_score_v7_20260925.png`. The description line is present, and the URL is **absolute and theme-free**: `https://marinesensitivity.org/atlas/?ver=v7#pl=z.pa.GAA&t=…`. **D3 fixed.** However, the title line reads **"score · score"** (D9).
   - **Theme toggle.** A sun or moon everywhere: desktop top bar, and on the phone "Switch to light theme ☀" / "Switch to dark theme ☾" (`phone-16`, `shots-light/phone-16`).

4. **Nits: PASS.**
   - **Popup and flower agree.** Both read "Cell 3353806 · lon -90.625, lat 28.575 · score: 46" on one line, and the flower heading is "(x: -90.625, y: 28.575)" (`desktop-06`). The phone shows the same agreement for cell 1587006.
   - **Welcome modal.** No focus ring on the × (`desktop-01`, `phone-01`).
   - **Phone legend modal.** Snug (`phone-05`).
   - **Places buttons.** They match the panel text (`desktop-11`).
   - **Report heading.** The place name appears once (`desktop-13`, `phone-13`).
   - **Table filters.** Placeholders are untruncated and the table fills the panel (`desktop-10`, `phone-09/10`).
   - **Flower table.** "Mean" is reachable on both viewports (facts `mean-visible: true`).
   - **Report map.** No clipped labels (`desktop-13b`).

5. **Phone default view, species framing and palette: FAIL.**
   - **Phone default view: PASS, with a residual.**
     - **Framing:** lower-48 waters in frame with minimal sky (`phone-02`, `phone-03`).
     - **Legend chip:** it no longer covers the Louisiana/Mississippi Program Areas. It still sits on the Texas coast and western Gulf, though.
     - **Tap miss:** the harness's first tap candidate, the northern Gulf at (-90.55, 28.6), **missed again** on the phone in both themes and fell through to the Gulf of Alaska (`dark.log`). The point projects at the chip's right edge. **D5 is improved, but not clear.** See N1.
   - **Wide-range species: FAIL, D1 is not fixed on the live data.**
     - **What the screen shows:** `extra/desktop-x10-species-wide-us.png` and `shots/desktop-17-species.png` frame the whole Pacific. `extra/phone-x10…` and `shots/phone-17` frame Oceania. `radiogroup "Zoom to"` counts 0 on both viewports (facts `*-zoomto: 0`).
     - **Root cause, probed on the live site:** the leatherback's merged COG `/cog/info` (titiler-v8, `cog/usa05/9fe6f75498affae1.tif`) returns `bounds: [-180, -17.7, 180, 60.45]`. The model reaches American Samoa and Guam across the antimeridian. `minimalFrame()` cannot narrow a -180..180 box because its complement is zero-width. `refineCameraFromCogBounds` then hits `if (bboxSpansGlobe(frame, 350)) return;` **before** the new `wideRangeAware()` call.
     - **Why the tests missed it:** the D1 e2e mocked `/cog/info` with "a real, wide, non-degenerate span", a shape the live data never returns. This is the CLAUDE.md "hermetic fixture hides the live shape" lesson a second time, on the same item.
   - **Eight petal colours: PASS.**
     - **Dark theme:** eight distinct hues.
     - **Paper theme:** Mammal is now olive-gold `#82773d` in its own hue family rather than near-black (`shots-light/desktop-06`). **D4 fixed.**
     - **Closest pair:** Mammal next to Turtle `#495625` reads as the nearest pair but is distinguishable by lightness.

## General checklist and regressions vs 0.10.67
No new layout breakage, overlap or clipping was found in the 41 × 2 harness states or the extras. The same holds for the Program-Area popup, flower and table (`desktop-19..21`, `phone-19..21`), the report (`desktop-13..15`), the walrus (`*-18`) and the phone more-menu.

Everything that changed since 0.10.67 matches or improves on it. The exceptions are D1 (still open) and D10 (a new inconsistency from W1's Selection dimming meeting `pl=` links).

## Defects

| # | What / where | Shot / evidence | Fix | Size |
|---|---|---|---|---|
| D1′ | **The "Zoom to" toggle never appears and the leatherback is not framed to US waters.** The COG bounds are a -180..180 box, so `refineCameraFromCogBounds` exits at `bboxSpansGlobe` before `wideRangeAware`. **Blocks ask 5.** | `extra/*-x10`, `*-17-species`, facts `zoomto: 0`, live `/cog/info` body above | Treat a globe-spanning COG extent as WIDE: narrow it to the study-area US box and keep "Whole range". Implement this as an exported pure function with a unit test on the **exact live bounds**. Add an e2e whose `/cog/info` mock is the live -180..180 shape, and a seeded fault that restores the early return. The walrus must not change. | S |
| D9 | **The export footer title reads "score · score".** The label and unit are both "score". | `extra/desktop-x09-download.png`, `extra-light/…` | Drop the unit when it equals the label, via an exported helper with a unit test. | S |
| D10 | **The Selection row says "— nothing selected" while a place is loaded from a link.** With `#pl=z.pa.GAA`, Places lists "1 / 20 places" and Download offers GeoJSON. `isPlacesSelectionEmpty(sel)` looks only at `sel.sel`. | `probe-pl-places.png` (Places panel) + probe `selrow` text; `extra/desktop-x08` (dimmed row with a place present) | `isPlacesSelectionEmpty(sel, placeCount)`, with both lens callers and a unit test. | S |
| D11 | **The About-modal "Release notes" link is a 404**: `docs/v7/release_notes.html`. The docs book's chapter is `releases.qmd`, and `https://marinesensitivity.org/docs/v7/releases.html` returns 200. | `curl` 404 vs 200 | Fix both copies: `Shell.svelte` `releaseNotesHref` and `docsUrl.ts#releaseNotesUrl`, plus its test. | S |
| D12 | **CI gallery red**: axe `scrollable-region-focusable` on `.flower-table-scroll`. | run 36158947685 | Branch `r3-w3b-flower-scroll`: `tabindex="0"`, `role="region"`, `aria-label`, `:focus-visible` ring, and regenerated darwin baselines. | S |
| D13 | **CI gallery red**: linux per-section baselines are missing. | 0 linux files | `npm run gallery:baselines-from-ci -- 36158947685`, then look at the diff stat before committing. | S |

## Nits (not blocking)
- **N1, phone half detent.** The Legend chip still overlaps the Texas coast and western Gulf. The harness's northern-Gulf tap point sits on the chip's edge and misses. Options: move the default bounds' south edge a little further, or pad for the chip's full width rather than its height only.
- **N2, Pacific coast.** California and Oregon waters touch the phone's left edge at the default view (`phone-03`). This carries over from the 7e88b94 review.
- **N3, exported map.** The export does not highlight the selected place, even when `pl=` is present. The exported legend has no title of its own (carried over).
- **N4, report naming.** The report title "Gulf of America Program Area" (from `t=`) does not match the body's "GOA Program Area A (GAA)". This predates round 3.
- **N5, report species table.** The report's species-count table is cut at the right page edge on desktop ("IUCN:NT(…", `desktop-14`). The same happens on 0.10.67, so it is not a regression. It needs a visible horizontal-scroll affordance or narrower headers.
- **N6, basemap label.** "Golfo de México" is a Spanish basemap label. W7's `name_en` item covers it.
- **N7, harness hygiene.** The previous review's `extra2.mjs` still clicks a non-existent "Explore" button and a "Whole range" *button*. Both are harmless misses, but the harness should say so. Also, a sandboxed session needs `TMPDIR` set, or Playwright fails to `mkdtemp`.

## Addendum, 2026-09-25: CI run 36158947685 completed, and what the orchestrator found after the review

### Three jobs red
**1. e2e (chromium, webkit, firefox): 7 failed, 4 flaky, 1336 passed.**
- **Gallery axe (2):** `matrix.a11y` × 2 hit the same `scrollable-region-focusable` issue; fixed by W3b.
- **`.gpkg` upload (3 engines):** `places.upload-geopackage.spec.ts:143` fails because `getByRole("button",{name:"Dismiss"})` now matches W6's two toast × buttons as well as the refusal's own Dismiss. The spec was never run after W6. This is D14.
- **Search fit:** `scores.search.spec.ts:159` "Enter flies the camera into the Aleutian Arc's own polygon bbox" fails on webkit (red on all 3 attempts) and is flaky on firefox. The camera never lands in ALA's box. D15: it may be a real bug in W6's bounds-cache path, and is under investigation.
- **Camera test (webkit):** `species.camera.spec.ts:610` (R3-A1) reads the camera **mid-flight**, because its "settled" poll only checks `zoom > 0`. Flakes in the same file: V4 469, D8 121, R3-A1 686. This is D16, a test-timing issue.

**2. e2e (gallery):** D12 and D13 as above.

**3. test:faults: 105/107 turned red.** `select-width-removed` and `seg-flex-dropped` **stay green**. The W1/W6 relayout made their gates stop depending on the rule. This is D17, gate rot.

### Tooling bug found while landing D13
`gallery:baselines-from-ci` wrote 132 **suffix-less** files: `…-about.png`, where the spec expects `…-about-chromium-linux.png`. `baselineNameFor()` only dropped `-actual`, but CI actuals carry no platform suffix. Its unit test used an invented `…-chromium-linux-actual.png` input. This is the same fixture-shape lesson, a third time this round.
- **Baselines:** renamed by hand (`6aa87aa`).
- **Script:** fixed on `r3-gallery-installer-fix` (`ed8921e`). It was validated end to end against the real artifact (replaced 132, added 0) and has seeded fault `gallery-baseline-suffix`.

### Landing status
- **Merged to local main, not pushed:**
  - `6303390`: W3b flower a11y, 0.10.74.
  - `6aa87aa`: linux gallery baselines.
  - `9da1123`: installer fix.
- **In flight:**
  - `r3-rr-fixes` (0.10.75): D1′, D9, D10, D11, plus the D16 camera-test timing.
  - `r3-ci-reds`: D14, D15 and D17 (0.10.76 if D15 turns out to be a real bug).
- **Push plan:** push once all are merged and the full gate, including webkit on the touched specs, is green locally.

## Addendum 2, 2026-09-25: fix outcomes (orchestrator eyes-on of the merged builds)

### D1′: three compounding bugs, not one
The fix round found that the brief's early-return was only one of three bugs:
1. `bboxSpansGlobe` returned early before `wideRangeAware()` could run.
2. `lib/raster/bounds.ts#narrowLongitude` handed back a single-hit 40°×78° window. It now probes every candidate, and a dateline-aware `hitLonArc()` frames all confirmed-data regions: −165, −157, −66 and 145.
3. A boot-vs-taxon race: `prevCameraKey` was latched before any camera existed, so a fresh load never refit.

**Round 2**, after the orchestrator's eyes-on of `e1fcfc8`:
- **Phone "US waters"** now uses the phone default lower-48 frame whenever the narrowed box cannot fit 390 px (`phoneAwareWideRangeBounds`). Before, it settled at lat −48.8, z 0.78, a small globe behind the sheet. It now settles at −98.5 / 10.35 / z 2.80.
- **"Whole range"** fits the arc with symmetric padding. On desktop it now shows Hawaii and Japan: −140.5 / 28.0 / z 2.03, where it used to be almost the same as US waters.
- **Probe speed.** `/cog/point` probes run with bounded concurrency of 4, and the toggle appears in about 3.5 s instead of 11–14 s.

**Residual (a product question for Ben):** on the phone, **"Whole range"** is positioned correctly but the disk is small, and most of it is behind the half-detent sheet. A proposal: selecting "Whole range" on the phone drops the sheet to its peek detent. Needs sign-off.

### Other fixes
- **D9:** the export title now reads "score".
- **D10:** the Selection row is undimmed when a `pl=` link supplies a place.
- **D11:** the release notes link now points to `releases.html`.

All three were verified on the merged build.

### Local-environment note
`[webkit] shell.panel.spec.ts:211`, "focus is trapped inside the maximized panel", fails on darwin WebKit on **a7ba24a too**, while CI's linux webkit passes it. It is the known macOS WebKit Tab-to-text-fields preference, not a regression.
