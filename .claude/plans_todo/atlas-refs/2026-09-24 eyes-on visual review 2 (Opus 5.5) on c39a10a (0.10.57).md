# Eyes-on visual review 2 (Opus 5.5) of atlas 0.10.57 (c39a10a), 2026-09-24

Orchestrator decision: HOLD honoured — V4 (phone species camera on the globe projection, report caption/word-break, harness desktop taps, desktop legend padding) dispatched before pushing 0.10.58; the regression is already live in 0.10.56.

claude-opus-5-5[1m]

**Verdict: HOLD.** V1 and V2 fixed defects 1, 2, 3, 5 and 6, and fixed most of 7. But the phone Species lens now frames the default model (leatherback) worse than 0.10.55 did. The range is pushed to the edge of the globe and mostly under the sheet, with the top half of the map empty. That is the first thing a phone user sees in Species, so this build fails even the "no worse than live" bar. The fix is narrow: phone species camera only. Everything else can push once it is fixed.

**How I checked.** I read all 34 PNGs in `atlas/.tmp/eyes3/`. I also served the existing `dist` (built 21:52, after c39a10a) with `vite preview` on port 4391 and took extra shots, now in `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/1811e35d-6030-49df-bcff-ec4168b75d1f/scratchpad/verify3/`. The extra checks were:
- the Program Area picker's options;
- the phone species views at 10–12 s, identical to the harness shots, so the camera had settled;
- the report page width on both screens.

The atlas repo is untouched (`git status` clean) and the server is stopped.

## Per screenshot
- **phone-01-welcome:** OK. Nit: a thick gold focus ring on the × at load.
- **phone-02-map:** OK. The sheet is collapsed to a peek, so the map is clean. Top bar, chip and tab bar are all clear. The view is the northern Gulf.
- **phone-03-layers-half:** OK. Distinct from 02. Collapse, dock and full are unobstructed; the chip sits above the sheet.
- **phone-04-layers-full:** OK. The chip has its own band under the header. Nit: the native "score" select looks different from the other dropdowns.
- **phone-05-legend-modal:** OK. The ramp and its 0–93 labels are visible. Nit: empty space under the ramp.
- **phone-06-flower-half:** OK. The flower is centred with hub 44, and the Component | Score table is there with one Cell ID line. Nits:
  - The popup coordinates (-90.550, 28.601) differ from the panel's (-90.575, 28.625) for the same cell.
  - The popup wraps "44" onto its own line.
- **phone-07-flower-petal:** OK. The label "Coral: 21.3" shows, with a gold outline that follows the petal and no rectangle.
- **phone-08-flower-full:** OK. All 8 components plus Mean, one decimal, with "Primary producer" shown as a label.
- **phone-09-table-half:** OK. The table has loaded: 4 columns, a Columns control, full header labels.
- **phone-10-table-full:** OK. Readable rows with sensible ellipses.
- **phone-11-places:** OK. The struck pill now has a visible reason: "Select a drawn or uploaded place first." The picker is closed in this shot.
- **phone-12-places-full:** OK. Nit: Share, Download places and Report still use a larger font than the rest of the panel.
- **phone-13-report-top:** OK. The URL wraps, and the page is no longer wider than the screen: `scrollWidth` 390 = `innerWidth` 390.
- **phone-14-report-scrolled:** OK. The flower is centred and the petals exactly match the legend swatches (pixel-sampled). The labels are proper category names. Nits:
  - The "GOA Program Area A (GAA)" pill is followed by the same text again on its own line.
  - The one-place ramp reads 33–34.
- **phone-15-report-scrolled2:** Issue (new). The Top-20 caption is cut off at the table's scroll edge ("…habitat-weighted exti"). Common names also break mid-word ("Leatherb|ack", "Humpba|ck Whale").
- **phone-16-more-menu:** OK. All six items plus the theme switch.
- **phone-17-species:** FAIL. The leatherback shows as a cap of the globe with a few red blobs by "OCEANIA". Most of the range is under the chip and the sheet, and the top half of the map is empty. It was the same after 10 s. The 0.10.55 re-shot showed the Aleutian and Gulf of Alaska band well framed.
- **phone-18-species-model:** Issue. The walrus is now visible above the chip, but it is small (about 1/4 of the free height). It sits low with its bottom edge touching the chip, and the top ~40% of the map is empty.
- **desktop-01-welcome:** OK. Nit: the same gold ring on the ×.
- **desktop-02-map:** OK. The panel is collapsed to its pill. Alaska → Gulf → Atlantic (plus Puerto Rico) is framed with padding.
- **desktop-03-layers-half:** OK. Distinct from 02, and the header controls are clear.
- **desktop-04-layers-full:** OK. Full screen now spans the whole 1280 px width.
- **desktop-06-flower-half:** OK. Centred, 8 petals. The harness tap still ends on land (Maryland), so this shows "Full study area".
- **desktop-07-flower-petal:** OK. "Bird: 45.7" with the petal outline.
- **desktop-08-flower-full:** OK. Full width and centred. Nit: the flower stays about 130 px across on a 1280 px stage.
- **desktop-09-table-half:** OK. Loaded, and it scrolls sideways inside the panel.
- **desktop-10-table-full:** OK. All 12 columns are readable. Nits:
  - Filter placeholders are cut ("Area (kn", "% of cat").
  - The table does not grow to fill the full-screen height; about 180 px is blank below it.
- **desktop-11-places:** OK. The pill reason is visible and not clipped.
- **desktop-12-places-full:** OK. Full width.
- **desktop-13-report-top:** OK. The GAA outline is drawn and zoomed to fill the frame. Nits (unchanged): the place is painted in a grey "low" colour because the ramp is 33–34, and the "LOUISIANA" label is clipped at the top.
- **desktop-14-report-scrolled:** OK. No footnote markers; the "Primary producer" header replaces "primprod". The species-count table is now clipped at the content column, which means it scrolls. Nit: there is no visible sign that it scrolls.
- **desktop-15-report-scrolled2:** OK. Nit: the AquaMaps citation still says "as provided in this R package".
- **desktop-17-species:** OK. The leatherback is framed on the North Pacific, with the West Coast and Hawaii clear of the panel.
- **desktop-18-species-model:** Issue (low). The panel side is fixed. But the floating legend covers the southwest corner of the walrus range (cropped and confirmed), and the range still runs off the bottom edge.

## Previous defects 1–7
1. **Program Area full names: FIXED.** The Places picker options read "Aleutian Arc (ALA)" through "St. Matthew-Hall (MAT)", 20 of them, sorted by name (DOM check). Shown selected in `verify3/phone-picker-MAT.png`; the report uses "GOA Program Area A (GAA)" (phone-14, desktop-14). The shots never open the native list.
2. **Desktop full screen capped at 720 px: FIXED.** desktop-04, 08, 10 and 12 all span 1280 px.
3. **Phone report sideways overflow: FIXED.** phone-13 (URL wraps), plus the measured `scrollWidth` above. The tables now scroll inside their own wrappers, which caused the new caption and word-break defect below.
4. **Species camera padding for the sheet, chip and panel: PARTLY.**
   - Desktop: fixed (desktop-17, desktop-18), apart from the legend overlap.
   - Phone walrus: better but small and low (phone-18).
   - Phone leatherback: regressed (phone-17).
5. **Harness false results: FIXED.**
   - 02 and 03 now differ in bytes on both screens.
   - 07 shows a petal label on both (phone-07 "Coral: 21.3", desktop-07 "Bird: 45.7").
   - 09 and 10 are loaded tables on both.
   - The log has no WARN lines.
6. **Struck "Show analysis cells" reason: FIXED.** The reason is visible under the pill on phone-11/12 and desktop-11/12, and is not clipped.
7. **Report petal colours: PARTLY.** Full opacity, petals equal to the swatches, centring and category labels are all fixed (phone-14, desktop-14). Two pairs of colours are still near-identical:
   - Bird #166a99 vs Fish #0060a0;
   - Mammal #8a5e00 vs Turtle #726b00.

## New defects, ranked
1. **Should-fix (high), a regression, and the HOLD reason: phone species camera** (phone-17; also phone-18). V1's live chrome padding is right for a flat map, but the phone draws a globe at these zooms. My hypothesis, not proven: `boundsToCameraView`'s Mercator bounds math turns the large bottom padding into a latitude shift, which rotates the globe instead of moving the view. The leatherback is pushed to the globe's edge, and the walrus is placed small and low. The e2e check (the bbox centre projected above the sheet's top edge) passes in both cases, so it cannot catch this.
2. **Should-fix (low): phone report Top-20 table** (phone-15).
   - Common names break mid-word ("Leatherb|ack", "Humpba|ck"). The cause is V1's `#report-root a { overflow-wrap: anywhere; }` in `src/report/report.css:145`, which also hits the table's name links. It should be limited to the URL.
   - The caption sits inside the scroll wrapper, so it is cut off ("…exti").
3. **Should-fix (low): desktop walrus.** The floating legend covers part of the range, and the fit ignores the legend and the bottom edge (desktop-18).
4. **Nit: harness desktop taps.** `tapScoredCell` clicks all three points in turn, so the last click, on land, replaces a valid cell. desktop-06, 07, 09 and 10 therefore never exercise a per-cell flower or table on desktop.
5. **Nit: "GOA" means two things.** The canonical names use "GOA Program Area A/B" (Gulf of America) while "Gulf of Alaska (GOA)" is another key in the same list.
6. **Carried-over nits:**
   - Raw "score" in the Layer select, legend and chip; lowercase "turtle" and "mammal" in the species panel.
   - The popup and panel coordinates differ, and the popup wraps its score.
   - The × focus ring on the welcome modal.
   - The native select styling.
   - Empty space in the legend modal.
   - The Places action-button font size.
   - The 33–34 one-place ramp.
   - The "in this R package" citation.

**Verdict: HOLD.** Only phone-17 fails outright, and it is a regression from 0.10.55. Fix the phone species camera (and ideally the one-line `overflow-wrap` scope), then re-shoot `ONLY=species,report`.
