# Eyes-on visual review 5 (Opus 5.5) of atlas 0.10.66 (21778e5; W1–W4), 2026-09-25

Orchestrator decision: PUSHED. Next nits round: desktop panel inset (+gutter) in the zone fit, a phone side gutter, a harness state that scrolls the report map into view, the capped-flower header wrap.

claude-opus-5-5[1m]

# Eyes-on review 5 (Opus 5.5): atlas 0.10.66 (21778e5), 40 shots in `/Users/bbest/Github/MarineSensitivity/atlas/.tmp/eyes13/`

**Verdict: PUSH.**
- Review 4's blocker is fixed on both form factors. A Scores-search pick now frames GAA in the free map area, and its popup is visible.
- Every item from W3, the W2 follow-up and W4 that is in frame checks out.
- What is left is two framing residuals (a gutter of about 12 px) and a blind spot in the shot harness. All three are should-fix items for the next round, not blockers.

**Provenance:** the shots are of the current code.
- The dist hashes match `.tmp/gm13-build.log`, which was built at 08:45:47 from 1a2938c (ScoresLens-BlsX7gYD, report-Br5Wh1HK, reportMap-CFHSq3H4, index-ChowEDAu).
- HEAD 21778e5 differs from 1a2938c only in `tests/faults/report-map-duplicated.patch`.

I edited nothing. The only files I wrote are crops in the scratchpad.

## Per screenshot
- **phone-01 welcome:** OK. Modal centred, Take a Tour and Explore reachable. The gold focus ring on the × is still there.
- **phone-02 map:** OK. Top bar, "Legend · score" chip, peek sheet and tab bar are all clear.
- **phone-03 layers-half:** OK. The segments fill the pill (Raster cells x 33–390, Program areas 390–746). The switches are quiet.
- **phone-04 layers-full:** OK. The chip has its own band, the segments fill the pill, and the Layer select is still native with a raw "score".
- **phone-05 legend modal:** OK. The ramp reads 0 to 93. There is still about 160 px of empty card below it.
- **phone-06 flower-half:** OK. The flower is capped: the ring spans x 220–560 and is centred. "max 100" and hub 44 are shown.
  - Two rows show: Bird 3.2 in full, and Coral 21.3 readable with its bottom border at the sheet edge.
  - New nit: the "Cell ID…(x: -90.575, / y: 28.625)" header now wraps onto two lines.
- **phone-07 flower-petal:** OK. The "Coral: 21.3" chip covers the hub with **no white stubs** (checked at 3× zoom). The highlight follows the petal shape; there is no rectangle.
- **phone-08 flower-full:** OK. Big ring, table rows from Bird to Other (the rest scroll).
- **phone-09 table-half:** OK. 4 columns, a Columns control, and Species | Zones | Composition filling its pill.
- **phone-10 table-full:** OK. Names are ellipsized, not reduced to one letter. Red Knot still shows "IUCN:TN" (a data issue).
- **phone-11 places:** OK. The struck "Show analysis cells" pill shows its reason.
- **phone-12 places-full:** OK. Share / Download places / Report still use a larger font.
- **phone-13 report-top:** OK. The frame ends at the "Map" heading, so **the report map is not in frame**.
- **phone-14 report-scrolled:** The caption reads **"Map: 1 place colored by mean score; GOA Program Area A (GAA) scored 33."**
  - Its first line is cut at the top of the viewport because of the harness scroll.
  - The duplicated pill-then-same-text line is still there.
- **phone-15 report-scrolled2:** OK. The counts table fades at the right, with "Scroll right for more columns →" below it.
- **phone-16 more-menu:** OK. Share, Report, Feedback, About this release, Take a tour, Docs, Switch to light theme.
- **phone-17 species:** OK. The toggle is greyed with its reason. The leatherback still frames as a cap of the globe (Ben's call).
- **phone-18 species-model:** OK. The walrus is framed above the chip.
- **phone-19 programarea-popup:** **Fixed.** GAA fills the free area: outline x 6–779, y 235–627, below the top bar and above the chip at y 650.
  - The popup "GOA Program Area A (GAA): 33" is fully visible.
  - Nit: there is no side gutter. The east outline touches the right edge (pink at x 779 on rows 544–626), so its outermost pixel is cut.
- **phone-20 programarea-flower:** OK. Same map. The title "GOA Program Area A (GAA)", "max 100", and rows Bird 59.1 and Coral 12.0 are visible.
- **phone-21 programarea-table:** OK. Same map. The header reads "Species for Program Area: GOA Program Area A (GAA)".
- **desktop-01 welcome:** OK. The × focus ring is still there. The Sphere switch behind the modal is quiet.
- **desktop-02 map:** OK. Alaska → Gulf → Atlantic plus Puerto Rico framed. The theme icon still reads as a gear.
- **desktop-03 layers-half:** OK. The segments fill the pill (905–1078 | 1078–1250). The **Sphere track matches Selection's quiet grey-blue** (132,148,189), and "Cells outside Program Areas" is off.
- **desktop-04 layers-full:** OK. The segments fill the pill (17–640 | 640–1262), and Sphere is quiet.
- **desktop-06 flower-half:** OK. The ring and "max 100" are there. The "Mean" row is still cut at the bottom edge of the panel, and the popup and panel coordinates still differ.
- **desktop-07 flower-petal:** OK. "Coral: 21.3" sits over the hub with **no stubs** (checked at 4× zoom).
- **desktop-08 flower-full:** OK. Large flower; the table continues below the fold.
- **desktop-09 table-half:** OK. The segments fill the pill. About 165 px of blank space below the table.
- **desktop-10 table-full:** OK. ER score is shown as a percent (by design, not flagged). The cut placeholders ("Area (kn", "Avg. suit", "% of cat") and about 185 px of blank space are still there.
- **desktop-11 places:** OK. The button-font nit is still there.
- **desktop-12 places-full:** OK. Same nit.
- **desktop-13 report-top:** OK, and the W4 fix shows here. Only the top 190 px of the map is in frame.
  - **GAA is filled with (238,240,203).** That is exactly the Spectral midpoint #ffffbf at 0.6 over the CARTO water (212,218,220). The no-data colour #b6bfd0 would give about (194,202,213), so this is not grey.
  - "LOUISIANA" is still clipped at the top of the map.
- **desktop-14 report-scrolled:** OK. The frame starts at the flower, so the map, its legend and its caption are not in frame. The scores table has no footnote markers.
- **desktop-15 report-scrolled2:** OK. The Sources paragraph says "**modeled** habitat suitability", and the citation now reads "3.0 **Unported License**". "in this R package" is still there.
- **desktop-17 species:** OK. The toggle is greyed with its reason; legend 1–100.
- **desktop-18 species-model:** OK. The walrus is clear of the legend and the panel.
- **desktop-19 programarea-popup:** **Mostly fixed.** GAA spans x 324 to about 900, and the popup is fully visible.
  - Residual: GAA's east outline and about 12 px of its east lobe run under the panel's left edge. The fill meets the panel border at x 888, and the NE and SE pink corners disappear into it.
- **desktop-20 programarea-flower:** OK. Same map. The title, "max 100" and all 8 components are there (Primary production 10.8). The Mean row is cut at the bottom.
- **desktop-21 programarea-table:** OK. Same map, header with the full name, blank space below the table.

## Item status
**W3**
- **Zone fits use the chrome padding: VERIFIED** on phone-19/20/21 (GAA fills the free area, popup visible).
  - On desktop-19/20/21 the area and popup are clear, but the east outline is about 12 px under the panel edge.
  - The cause is that `desktopPanelPadding` reserves `right = geometry.size` (380). The panel actually covers x 888–1280 (392 px), because its outer inset is not counted (`src/lib/map/chromePadding.ts:63`).
  - The phone has the opposite problem: left and right padding are 0 (`NO_PADDING`), so there is no gutter at all.
- **Hub number hidden under the petal label: VERIFIED** (phone-07, desktop-07).
- **Segments fill their pill: VERIFIED** on phone-03/04/09/10/21 and desktop-03/04/09/10/21, plus the greyed Species toggle on phone-17/18 and desktop-17/18.
- **Phone half-detent flower cap, two rows: VERIFIED** (phone-06; also phone-20).
- **"modeled": VERIFIED** (desktop-15).
- **ER score as a percent:** present by design, not flagged.

**W2 follow-up**
- **Ring value: VERIFIED visible.** "max 100" appears on phone-06/07/08/20 and desktop-06/07/08/20. A screenshot cannot show which source it came from.
- **Sphere and "Cells outside Program Areas" quiet: VERIFIED** (desktop-01/03/04/19, by pixel sample).

**W4**
- **Place painted mid-ramp, not grey: VERIFIED on desktop-13** (pixel-exact). **NOT VISIBLE on the phone**: phone-13 ends at the Map heading and phone-14 starts at the caption.
- **One-place caption: VERIFIED on phone-14.** NOT VISIBLE on desktop.
- **Single-swatch legend: NOT VISIBLE** on either form factor.
  - It is very likely there. The legend comes from `model.map.single`, which uses the same palette stops as the painted fill (`src/lib/report/model.ts:665`, `Report.svelte:615`).
  - Why the shots miss it: `scripts/eyes-shots.mjs:379-385` scrolls 1400 px blind, which jumps over the map figure.

## Review 4's nits
**Fixed:**
- The Scores-search zoom under the sheet/panel (blocker). On desktop a 12 px residual remains.
- The unshot HEAD.
- The hub stubs.
- The segment tail.
- The phone half-detent flower.
- The "33–34" one-place ramp (the caption is now the one-place form).
- "modelled".
- "UnportedLicense".

**Still present:**
- Raw "score" in the Layer select, chip and legend.
- Popup vs panel coordinates differ, and the popup wraps "44".
- The × focus ring on the welcome modal.
- The native Layer select.
- Empty space in the legend modal.
- The Places action-button font size.
- The duplicated pill line in the report.
- "in this R package".
- The desktop table's cut placeholders and blank space below it.
- The Mean row cut in the desktop flower panel.
- Bird/Fish and Mammal/Turtle report colours still close.
- "GOA" meaning two things.
- "LOUISIANA" clipped at the top of the report map.
- The theme icon reads as a gear.
- The phone default frame and the leatherback cap (Ben's calls).

## New defects, ranked
1. **Should-fix:** the desktop zone fit ignores the panel's outer inset of about 12 px. GAA's east outline is under the panel edge (desktop-19/20/21).
   - Fix: add the panel's inset plus a small gutter to `right`.
   - Related: the fit also reserves a full 320 px left column for the bottom-left legend card, which is only about 75 px tall. That pushes the area right, into the panel.
2. **Should-fix (harness):** the report map figure is never in frame (not at all on the phone, only the top 190 px on desktop). The phone fill and the single-swatch legend are unverified. Add a shot that scrolls the map figure into view.
3. **Nit:** the phone zone fit has no side gutter. GAA spans x 6–779 of 780, and its east outline touches the right edge (phone-19/20/21). A gutter of 16–24 CSS px on every side of the fit would fix it.
4. **Nit:** the capped phone flower narrows its header column. "Cell ID: 3350704 (x: -90.575, y: 28.625)" wraps onto two lines and starts at x 220 instead of the sheet's gutter (phone-06/07).

**Verdict: PUSH.** No checklist state fails, and the review-4 blocker is fixed on both form factors. Items 1–3 (the framing gutters and the report-map shot) belong in the next round.

