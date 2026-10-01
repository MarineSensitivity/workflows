# Eyes-on visual review 4 (Opus 5.5) of atlas 0.10.64 (51b7b06; W1 + W2), 2026-09-25

Orchestrator decision: HOLD honoured — W3 (0.10.65) fixes the Scores-search zoom padding (same class as V4), the hub-number stubs, segment fill, phone half-detent flower cap, ER-score formatting and copy; full reshoot + re-check before the push.

claude-opus-5-5[1m]

# Eyes-on review 4 (Opus 5.5): atlas 0.10.64, 40 shots in `/Users/bbest/Github/MarineSensitivity/atlas/.tmp/eyes10/`

**Verdict: HOLD.** There are two reasons.

1. **Ben's item 1 is wrong on the phone.** When search picks a Program Area, the map moves so the area sits under the sheet. The full-name popup lands under the sheet too.
2. **The shots are not of the current HEAD.** The eyes10 dist was built at 07:04:43. Its asset hashes (ScoresLens-qfgzjq7G, preload-helper-B9OhFxN5) match `gm10-build.log`, which was built from 51b7b06. HEAD is now adf7156: the W2 follow-up (every Layers switch quiet, and the ring value read from the manifest), merged at 07:05:59. The current dist (07:11:52) has different hashes and was never shot. You can see this in the shots: the Sphere switch is still gold.

I edited nothing. The only files I wrote were crops in the scratchpad.

## Per screenshot
- **phone-01 welcome:** OK. Modal centred, Take a Tour and Explore reachable. The gold focus ring on the × is still there.
- **phone-02 map:** OK. Top bar, legend chip, peek sheet and tab bar are clear. The frame is still the northern Gulf only (Ben's call).
- **phone-03 layers-half:** OK. The segmented "Raster cells (0.05°) | Program areas" toggle is at the top, and the three layer switches are quiet (grey-blue, not gold). Nit: the two segments fill only about 60% of the pill.
- **phone-04 layers-full:** OK. The chip has its own band. The Layer select is still the native one and still says raw "score".
- **phone-05 legend modal:** OK. The ramp shows 0 and 93. Still empty space below it.
- **phone-06 flower-half:** OK. Cell 3350704, a big centred flower, dashed ring with a "max 100" chip, hub 44.
  - Because the flower is bigger, the Component | Score table is now below the fold at this detent.
  - The popup and the panel still give different coordinates, and the popup still wraps "44" onto its own line.
- **phone-07 flower-petal:** Label "Coral: 21.3" with an outline that follows the petal, no rectangle. **New artefact:** the feet of the hub's "44" poke out as two white stubs under the label chip.
- **phone-08 flower-full:** OK. Ring, "max 100", every petal inside the ring, table rows Bird 3.2 … Other 57.8 (the rest scroll below).
- **phone-09 table-half:** OK. 4 columns, a Columns control, full header labels.
- **phone-10 table-full:** OK. Rows are readable. The v7 data shows "IUCN:TN" for Red Knot (a data issue, not the app).
- **phone-11 places:** OK. The struck "Show analysis cells" pill shows its reason.
- **phone-12 places-full:** OK. Share / Download places / Report still use a larger font.
- **phone-13 report-top:** OK.
- **phone-14 report-scrolled:** OK. The caption now says "colored" and the report legend says "Primary production".
  - The pill is still followed by the same "GOA Program Area A (GAA)" text on its own line.
  - The one-place ramp still reads 33–34.
- **phone-15 report-scrolled2:** The previous defect is fixed: "Scroll right for more columns →" now sits under the counts table. Names break cleanly.
- **phone-16 more-menu:** OK. Share, Report, Feedback, About this release, Take a tour, Docs, Switch to light theme.
- **phone-17 species:** OK. The toggle is greyed with "Species surfaces are rasters only." The leatherback still frames as a cap of the globe (Ben's call).
- **phone-18 species-model:** OK. The walrus is framed above the chip, and the toggle is greyed with its reason.
- **phone-19 programarea-popup:** **FAIL.** After the search pick, the map shows Arkansas and Mississippi. GAA is a sliver of pink outline at the bottom (y 725–750 of 1688), and the rest of it is under the sheet. No popup is visible.
- **phone-20 programarea-flower:** The flower title is "GOA Program Area A (GAA)" (full name), with the ring and "max 100". The map still hides the area under the sheet.
- **phone-21 programarea-table:** The header reads "Species for Program Area: GOA Program Area A (GAA)", so the full-name part is right. The map is the same as phone-19.
- **desktop-01 welcome:** OK. Same focus-ring nit on the ×. The Sphere switch behind the modal is gold.
- **desktop-02 map:** OK. Alaska → Gulf → Atlantic plus Puerto Rico framed.
  - The top-right icons are icon-only: Share, Help, Feedback, Info, then the theme icon (it reads as a gear). There is no Report.
- **desktop-03 layers-half:** OK. Segmented toggle and quiet layer switches. The **Sphere switch is still gold** (this build predates the follow-up). The segments fill only about 215 of 345 px of the pill.
- **desktop-04 layers-full:** OK, with the same gold Sphere switch. The empty pill is most visible here: the segments take about 283 of 1245 px.
- **desktop-06 flower-half:** The harness now lands a real cell (3350704).
  - The ring is about 345 px across; the Turtle petal (94.9) sits just inside it.
  - The table says "Primary production 36.2" and has no "No data" row.
  - The "Mean" row is cut at the bottom edge of the panel. The coordinates differ between popup and panel (-90.584, 28.609 vs -90.575, 28.625).
- **desktop-07 flower-petal:** The "Coral: 21.3" label is right, with the same "44" stubs under the chip.
- **desktop-08 flower-full:** OK. The ring is now about 480 px across, so the old "small full-screen flower" nit is fixed.
- **desktop-09 table-half:** OK. "Species for Cell ID: 3350704". About 155 px of blank space below the table.
- **desktop-10 table-full:** OK. 12 columns.
  - The cut filter placeholders ("Area (kn", "Avg. suit", "% of cat") and about 180 px of blank space are still there.
  - New nit: "ER score" is shown as "1%", "2%", "10%".
- **desktop-11 places:** OK. The button-font nit is still there.
- **desktop-12 places-full:** OK. Same nit.
- **desktop-13 report-top:** OK. "colored". "LOUISIANA" is still clipped at the top of the map.
- **desktop-14 report-scrolled:** OK. "Primary production" appears. The counts table is cut at "IUCN:NT(" (the hint is not in frame).
- **desktop-15 report-scrolled2:** OK. The common names are now sentence case.
  - Still "in this R package" and "UnportedLicense" in the AquaMaps citation.
  - "modelled" (British spelling) in Sources.
- **desktop-17 species:** OK. "Search species" has the same 240 px width and single outline as Scores. The toggle is greyed with its reason. "Category: Turtle" is now capitalized.
- **desktop-18 species-model:** OK. The walrus is clear of the legend. "Category: Mammal".
- **desktop-19 programarea-popup:** The search pick zoomed to GAA, and the popup reads "GOA Program Area A (GAA): 33".
  - But the fit ignores the docked panel: GAA's eastern third runs under the Layers panel, and its tan fill shows again at x 1265–1280.
  - The Sphere switch is gold.
- **desktop-20 programarea-flower:** The title is "GOA Program Area A (GAA)", with the ring, "max 100", all 8 components including "Primary production 10.8", and no "No data" row.
- **desktop-21 programarea-table:** The header reads "Species for Program Area: GOA Program Area A (GAA)". Blank space below the table.

## Ben's items 1–8
1. **Search pick zooms to the Program Area: WRONG on phone** (phone-19/20/21); VERIFIED on desktop (desktop-19), except that part of the area is under the panel.
   - Cause: `src/lens/scores/state.svelte.ts:419` calls `handle.flyToBounds(bounds, { padding: 40 })` with a flat padding, which knows nothing about the sheet or the docked panel.
   - `Shell.svelte:542` passes `chromePadding` only to the species lens. The species lens already uses it (`src/lens/species/state.svelte.ts:217`, the fix for the walrus under the sheet).
   - This is the same bug class. Passing the Shell's `chromePadding()` into the Scores lens deps and using it here should fix both the phone and the desktop framing.
2. **Utility menu icon-only, Report removed: VERIFIED** (desktop-02/17/19). The hover tooltips are **NOT VISIBLE**, because no state hovers. In source they are `data-tooltip` attributes in `TopBarActions.svelte`.
3. **About credits: NOT VISIBLE**, because no state opens About. The source wording is exact: `TopBarActions.svelte:322-324`, "Prepared by Ben Best of Ocean Metrics LLC and Timothy White of MMA."
4. **Equal search width, no inset outline: VERIFIED.** Both fields are x 443–683 with one 1 px outline (desktop-02 vs desktop-17, pixel crop).
5. **Segmented toggle and quiet switches: VERIFIED** for the toggle at the top (phone-03/04, desktop-03/04), greyed with its reason in Species (phone-17/18, desktop-17/18), and quiet per-layer switches.
   - The one gold switch left in these shots is Sphere (desktop-01/03/04). It becomes quiet only in adf7156, which was not shot.
6. **Species Table shows the model's inputs: NOT VISIBLE.** No state opens Table in the Species lens.
7. **Flower: VERIFIED** (desktop-06/07/08/20, phone-06/07/08/20).
   - It is bigger, with a dashed low-contrast ring and a small grey "max 100" chip on the ring.
   - Every petal is inside the ring, the label reads "Primary production", and there is no "No data" row.
   - Side finding: this pre-follow-up build already showed "max 100", not the fallback text. v7's real `app/boot.json` does publish `by_subregion.FULL.rescale = [0,100]` on its component rows. So the follow-up's reason ("no real release publishes it … structurally unable to return a number") is false for v7, and that claim now sits in the `boot.ts`/`flowerGeometry.ts` comments. The value is harmless: both paths give 100 on v7.
8. **Full Program Area name: VERIFIED on desktop** for the popup, flower title and table header (desktop-19/20/21). On the phone the flower title (phone-20) and table header (phone-21) are verified; the **map popup is NOT VISIBLE**, because it sits under the sheet (item 1's bug).

## New defects, ranked
1. **Blocker:** the Scores search's Program-Area fit uses a flat 40 px padding (item 1 above). On the phone the area and its popup land under the sheet. On desktop about a third of it is under the docked panel.
2. **Should-fix (process):** HEAD adf7156 is unshot. Reshoot all 40 states after fixing item 1, so that item 1's fix and the follow-up's quiet Sphere and "Cells outside Program Areas" switches (and the ring value from the manifest) are all seen.
3. **Nit:** the petal-hover label does not hide the hub number. Two white stubs of "44" show under the chip (phone-07, desktop-07). This is new with the bigger flower.
4. **Nit:** the segmented controls' segments do not fill their pill, which leaves a large empty tail. This shows most on desktop-04, and also on Species | Zones | Composition.
5. **Nit:** the flower now fills the phone half detent, so the component table and the bottom of the ring are below the fold (phone-06/20).
6. **Nit:** the "ER score" column is formatted as a percent ("1%", "10%") in desktop-10.
7. **Nit:** "modelled" in the report's Sources, and "UnportedLicense" run together (desktop-15).
8. **Nit:** the desktop theme icon reads as a settings gear at this size.

## The previous review's nits
Still present:
- Raw "score" in the Layer select, legend and chip.
- Popup and panel coordinates differ, and the popup wraps the score.
- The × focus ring on the welcome modal.
- The native Layer select.
- Empty space in the legend modal.
- The Places action-button font size.
- The 33–34 one-place ramp.
- The duplicated place pill line in the report.
- The "in this R package" citation.
- The desktop table's cut placeholders and blank space below it.
- Bird/Fish and Mammal/Turtle report colours still close.
- "GOA" meaning two things ("GOA Program Area A (GAA)" is the Gulf of America).
- "LOUISIANA" clipped at the top of the report map.
- The phone default frame and the leatherback cap (Ben's calls).

Fixed:
- Lowercase "turtle"/"mammal" in the species panel.
- The small full-screen flower on desktop.
- The harness desktop taps.
- The phone counts-table scroll cue.
- "coloured" vs "colored".
- Lowercase common names, now sentence case (mixed with title case such as "Kemp's Ridley Turtle").
