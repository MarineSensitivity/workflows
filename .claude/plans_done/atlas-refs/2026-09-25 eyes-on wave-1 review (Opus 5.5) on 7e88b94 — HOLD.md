Model: claude-opus-5-5[1m] (Opus 5.5)

**HOLD**: one ask fails and one partly fails. (a) **R3-A1, the wide-range species toggle, never appears on v7.** The leatherback is the Species lens' default landing species. It is still framed across the whole Pacific (desktop) and on Oceania (phone), and there is no "Zoom to" toggle. (b) **R3-A2, the phone default view:** the minimal-sky part passes. At the default "half" detent, though, the Legend chip and the sheet top cover the northern Gulf of Mexico.

# Eyes-on review, round 3 wave 1 (atlas `main` 7e88b94 = 0.10.72, 2026-09-25)

**Build:** a detached worktree at `atlas/.claude/worktrees/r3-review2`, built with `npm ci`, `duckdb:fetch-ext` and `build`, and served by `vite preview` on 4511 (now killed). Paths below are relative to `review2/`.
- `shots/`: `scripts/eyes-shots.mjs` in the dark theme, 43 PNGs.
- `shots-light/`: the same harness with `theme=light` (`harness/eyes-shots-light.mjs`), 43 PNGs.
  - Neither run logged a WARN or MISSED. On the **phone**, however, the first candidate (the northern Gulf of Mexico) missed in **both** themes, and the tap fell through to the Gulf of Alaska. That miss is evidence for (b).
- `extra/` and `extra-light/`: `harness/extra2.mjs`. It covers the opacity popover, the palette popover, Outlines expanded, the reorder probe, Selection after a tap, the flower table scrolled, the Download menu and the phone Download modal, the downloaded PNG and SVG, the leatherback species, and the theme icon. `extra/facts.json` holds the DOM facts.
- `crops/`: contact sheets and zooms.
- Previous build for regression comparison: `../review/shots/` (0.10.67).

## Ben's asks

1. **No hexagon pip on the active rail tool: PASS.** See `shots/desktop-03-layers-half.png`, `desktop-06-flower-half.png` and `desktop-11-places.png`. The accent fill alone marks the active tool, in both themes.

2. **Layers pane: mostly PASS, with two partial items.**
   - **PASS, the compact toggle.** "Raster cells | Program areas" is content-sized, with no "(0.05°)" (`desktop-03`, `phone-03`).
   - **PASS, Layer and Zoom-to-region placement.** They sit side by side on desktop and stacked on the phone.
   - **PASS, the checkboxes.** They are muted and sit left of the name. The computed `accent-color` is `rgb(132,148,189)` (facts.json).
   - **PASS, the opacity control.** It is a small inline "◐ 100%" pill that opens a gold-slider popover (`extra/desktop-x01-opacity-popover.png`, `extra/phone-x01…`, and the light pair in `extra-light/`).
   - **PASS, the glyphs.** The caret rotates (› / ⌄) and the reorder buttons use ↑↓ arrows.
   - **PASS, the palette picker.** It is wide, with a gradient strip and the name, and opens a four-ramp list (`extra/desktop-x02-palette-popover.png`).
   - **PASS, Sphere and removed rows.** Sphere is at the bottom. There are no rows for roads, boundaries, land/water or bathymetry.
   - **PASS, Selection dimming.** Selection is dimmed with "— nothing selected" on a bare load (`desktop-03`). After a cell tap it is undimmed (`extra/*-x06-selection-after-tap.png`; the row class loses `--dim`).
   - **PARTIAL, the "Outlines" expander.** It does offer Program Areas | Ecoregions (`extra/desktop-x03-outlines-expanded.png`, `crops/phone-popovers.png`). The explanations are not one line: Program Areas takes 2 lines and Ecoregions takes 4. The Ecoregions text starts lowercase. It also reads as self-contradicting ("…draws the ecoregion boundary itself … independent of this choice").

3. **Download menu and theme icon: PASS.**
   - The desktop download icon sits left of Help, and the menu lists PNG / SVG / GeoTIFF / GeoJSON. GeoJSON appears with a place selected (`extra/desktop-x08-download-menu.png`, `extra-light/…`).
   - On the phone, ⋯ → "Download…" opens a modal with the same four items (`extra/phone-x08a-more-menu.png`, `phone-x08-download-modal.png`).
   - The downloaded PNG (`extra/desktop-x09-download.png`, 1280×798) shows the map, the legend at bottom left, and a two-line footer. The SVG has real `<text>` for the footer and legend.
   - The theme toggle is a sun or a moon, never a gear: desktop `desktop-03` / light `desktop-03`, and phone `shots/phone-16-more-menu.png`, "Switch to light theme ☀". Defects in the export itself are listed below as D3.

4. **Nits: PASS.**
   - **Popup and flower agree.** Both print the cell centre: "Cell 3353806 · lon -90.625, lat 28.575 · score: 46" on one line, and the flower reads "(x: -90.625, y: 28.575)" (`desktop-06-flower-half.png`). The phone agrees as well (`phone-06`).
   - **Welcome modal.** There is no focus ring on the × (`desktop-01`, `phone-01`).
   - **Phone legend modal.** No empty space; the description fills it (`phone-05-legend-modal.png`, both themes).
   - **Places buttons.** Share, Download places and Report match the panel text (`desktop-11-places.png`).
   - **Report heading.** The place name appears once, with no tab pill (`desktop-13-report-top.png`).
   - **Table filters.** The placeholders "Area", "Suit." and "% cat" fit, and the table fills the panel (`desktop-10-table-full.png`, `desktop-09-table-half.png`).
   - **Flower table.** It scrolls to "Mean" on both viewports (`extra/*-x07-flower-table-scrolled.png`; facts `mean-visible: true`).
   - **Report map.** No clipped labels; basemap labels are gone (`desktop-13b-report-map.png`).

5. **Phone default view, species framing and palette: FAIL.**
   - **Phone default view: PARTIAL.**
     - **Sky:** it is minimal, and the globe fills the top (`phone-02-map.png`). This passes.
     - **Default "half" detent** (`phone-03-layers-half.png`, `crops/phone-03-south.png`): the Legend chip sits over the Louisiana/Texas Program Area, and the sheet top cuts at the Florida Keys. That is the state every first-time phone visitor sees.
     - **Northern-Gulf tap:** it missed in both themes because the Gulf is under chrome.
     - **Pacific coast:** the Washington/Oregon waters sit hard against the left edge.
   - **Wide-range species, "Zoom to: US waters | Whole range": FAIL.** See `extra/desktop-x10-species-wide-us.png`, `extra/phone-x10-species-wide-us.png`, `shots/desktop-17-species.png` (the default species *is* the leatherback) and `phone-17`.
     - **What the screen shows:** v7 `mdl_seq=54241` is framed on the whole Pacific on desktop, and on the Oceania nesting patches on the phone, with no toggle. `getByRole("radiogroup",{name:"Zoom to"})` counts 0 on both viewports.
     - **Root cause** (read, not guessed): `wideRangeAware()` (`src/lens/species/data/camera.ts:186`) runs only inside `cameraFor()`'s bundle-bbox steps. v7 publishes **no bbox on any asset**, so `cameraFor()` falls through to `kind:"center"`. `state.svelte.ts:260 refineCameraFromCogBounds()` then fits the raw `/cog/bounds` frame, and it never calls `wideRangeAware` or `recordWideRangeCamera`.
     - **Why the tests missed it:** the unit and e2e fixtures carry a bbox. It is the same "hermetic fixture hides the live shape" lesson recorded in CLAUDE.md. The CHANGELOG names the v7 leatherback as the motivating case.
   - **Eight distinguishable petal colours: PASS, with a caveat.**
     - **Dark theme:** the eight categories are clearly distinct (`desktop-06-flower-half.png`).
     - **Light theme:** they are distinct too, but Mammal `#372506` (L\*≈16) reads as near-black (`shots-light/desktop-06`, `crops/light-phone.png`).
     - **Closest light pairs (CIE76):** Bird/Other ΔE 24.1 and Mammal/Turtle 26.6. Both are acceptable. See D4.

## General checklist

These are the only new regressions against 0.10.67. They are D1 and D2 below, both from the new pane. Everything else I checked matched or improved: Places, the report, Program-Area popup/flower/table and the more-menu.

## Defects

| # | What / where | Shot | Fix | Size |
|---|---|---|---|---|
| D1 | **The wide-range toggle and US framing never apply when the camera comes from COG bounds**, which covers every v7 species and is the Species-lens default. | `extra/*-x10-*`, `shots/*-17-species.png` | In `refineCameraFromCogBounds`, pass `minimalFrame(bbox)` through `wideRangeAware(…, "merged", studyAreaView(boot, FULL))` (export it), then `applyCamera` + `recordWideRangeCamera`. Add an e2e with a **bbox-less** leatherback fixture that asserts the radiogroup and the US fit, and a seeded fault. | S |
| D2 | **Place labels' ↓ is enabled but does nothing visible.** It swaps with a hidden basemap row: the order stays `[places, zones, raster, labels]` after the click (facts `*-after-labels-down`). Outlines has both arrows disabled, and Data has ↑ disabled, so the arrow semantics are opaque. | `crops/desktop-reorder.png`, `extra/desktop-x04-after-lastrow-down.png` | Compute `canMove` over the *visible* rows only, and skip hidden entries when swapping. | S |
| D3 | **Map-view export: the filename and footer title use the layer's long description**, not its label. The filename is ~190 characters (`marine-atlas_scores_combined-score-of-extinction-risk-per-species-…_v7_20260925.png`), and the footer title line is the full sentence plus " · score". The footer "share URL" is **relative** (`/?ver=v7&theme=dark#pl=…`), so it is useless off-site, and it bakes `theme=dark` into the link. | `extra/desktop-x09-download.png`, `.svg` | Use the metric label for the title and slug. Emit an absolute canonical URL (`location.origin+pathname`) and drop `theme`. | S |
| D4 | **Mammal changes hue family between themes**: bright yellow `#e6c700` on navy, near-black brown `#372506` on paper. Every other category keeps its hue across themes. | `shots-light/desktop-06-flower-half.png` vs `shots/desktop-06…` | Pick a mid-dark gold or ochre for paper (for example around L\*40), and re-run `npm run contrast` and the CVD check. | S |
| D5 | **The phone default camera does not account for the legend chip at the half detent.** The Gulf of Mexico Program Areas sit under the chip and the sheet edge. | `phone-03-layers-half.png`, `crops/phone-03-south.png` | Include the chip band in `chromePadding` for the default fit at the default detent, or shift the bounds' south edge. Re-measure sky afterwards. | S–M |
| D6 | **On the phone, the palette popover runs under the bottom nav** at full height, so "Magma" is hidden. | `extra/phone-x02-palette-popover.png` | Flip the popover up, or cap its height to the space above the nav. | S |
| D7 | **The Outlines explanations run 2–4 lines**, the Ecoregions one starts lowercase, and "independent of this choice" contradicts the radio. | `extra/desktop-x03-outlines-expanded.png` | Use one-line copy, for example "Program Areas: BOEM 2026 planning units" and "Ecoregions: the regions each score is rescaled within". Put the detail in a tooltip. | S |

## Nits

- **Desktop "Reset layers" clipped.** At the docked-panel default, the button's bottom border is cut off at the panel's lower edge (`shots/desktop-03-layers-half.png`, bottom right).
- **Download tooltip overlaps the menu.** The "Download" tooltip draws on top of the open menu's top-right corner (`extra/desktop-x08-download-menu.png`).
- **Layer description missing on the phone.** It shows on desktop under Layer/Zoom-to-region but not in the phone pane (`phone-04` vs `desktop-04`). This may be intended, but it is inconsistent between viewports.
- **Palette popover covers controls.** When open it covers "Cells outside Program Areas" and Place labels with no scrim. Acceptable, but consider flipping it up when there is room.
- **Exported legend is bare.** It is 160 px with no title (the title is only in the footer). A small "score" label above the ramp would make a cropped PNG self-describing.
- **Report naming mismatch.** The title reads "Gulf of America Program Area" (from `t=`), while the figure and flower read "GOA Program Area A (GAA)". Two names for one place on one page predates this round.
- **Off-screen popup.** The phone popup for a cell whose anchor is off-screen (Gulf of Alaska) draws with its arrow pointing off the canvas (`phone-06-flower-half.png`).
- **Console 404s are benign.** `session.json` 404 is the public-host contract, and the titiler `/cog/tiles` 404s are empty tiles outside the COG. Neither is a defect, but the harness's error counter will always be non-zero.
