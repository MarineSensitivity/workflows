# Eyes-on visual review 3 (Opus 5.5) of atlas 0.10.59 (1565dfd), 2026-09-24

Orchestrator decision: FOLLOW-UP honoured (no rollback); the harness desktop tap points, the phone counts-table scroll cue and the spelling/lowercase nits go to the next nits round; the leatherback wide-range framing, the palette collisions, the health banner covering the top bar and the phone default frame are Ben's calls.

claude-opus-5-5[1m]

**Verdict: FOLLOW-UP.** Nothing needs a rollback. The phone species camera (the HOLD reason), the report captions and word breaks, and the desktop legend overlap are fixed. The desktop harness tap is still not fixed in effect: the three desktop tap points all land on land, so no per-cell view is shot on desktop. The build is current: dist was built at 23:40, 2 minutes after merge 1565dfd, and all 34 shots were taken after that. I edited nothing; my only writes were two crops in the scratchpad.

## Per screenshot
- **phone-01-welcome:** OK. Modal centred, Take a Tour and Explore reachable. Nit: gold focus ring on the ×.
- **phone-02-map:** OK. Top bar, chip, peek sheet and tab bar are clear. The frame is the northern Gulf only (the phone default frame is still your call).
- **phone-03-layers-half:** OK. Differs from 02; collapse, dock and full are unobstructed; chip above the sheet.
- **phone-04-layers-full:** OK. Chip has its own band. Nits: the Layer select is still the native one and still says raw "score".
- **phone-05-legend-modal:** OK. Ramp with 0 and 93 visible. Nit: empty space below it.
- **phone-06-flower-half:** OK. Cell 3350704, centred flower, hub 44, Component | Score table, one Cell ID line. Nits: the popup says -90.550, 28.601 but the panel says -90.575, 28.625; the popup wraps "44" onto its own line.
- **phone-07-flower-petal:** OK. "Coral: 21.3" label with a petal-shaped outline, no rectangle.
- **phone-08-flower-full:** OK. 8 components plus Mean, one decimal each.
- **phone-09-table-half:** OK. 4 columns, a Columns control, full header labels, first row loaded.
- **phone-10-table-full:** OK. Readable rows with sensible ellipses.
- **phone-11-places:** OK. The struck pill shows its reason.
- **phone-12-places-full:** OK. Nit: Share / Download places / Report still use a larger font.
- **phone-13-report-top:** OK. The URL wraps mid-token, which is intended.
- **phone-14-report-scrolled:** OK. Centred flower, legend, category names. Nits:
  - The pill is followed by the same "GOA Program Area A (GAA)" text on its own line.
  - The one-place ramp reads 33–34.
  - This caption spells "coloured"; the desktop-13 map intro spells "colored".
- **phone-15-report-scrolled2:** Defect 2 fixed. The Top-20 caption is fully visible above the table, and names break only at spaces or hyphens ("Band-/rumped", "Leatherback Turtle", "Humpback Whale"). New issue in the counts table, see new defect 2.
- **phone-16-more-menu:** OK. Share, Report, Feedback, About this release, Take a tour, Docs, plus the theme switch.
- **phone-17-species:** Accepted, not the HOLD reason. By pixel scan, the range sits at y 151–361 of 1688 and the chip starts at 648, so nothing is under the chip or the sheet. It is still a cap of the globe with the blobs at the top limb and empty ocean below; "ANTARCTICA" shows behind the tab bar.
- **phone-18-species-model:** Fixed. The walrus fills about 64% of the free map height, centred, with 101 px free above and 96 px above the chip.
- **desktop-01-welcome:** OK. Same × ring nit.
- **desktop-02-map:** OK. Alaska → Gulf → Atlantic plus Puerto Rico framed; the panel is collapsed to its pill.
- **desktop-03-layers-half:** OK. Header controls clear.
- **desktop-04-layers-full:** OK. Spans the full 1280 px.
- **desktop-06-flower-half:** Harness miss. "Full study area", with the popup "No scored cell here · lon -76.757, lat 39.307".
- **desktop-07-flower-petal:** Harness miss. "Bird: 45.7" label on the full-study-area flower.
- **desktop-08-flower-full:** OK. Nit: the flower is only about 105 px across on a 1280 px stage.
- **desktop-09-table-half:** Harness miss. "Species in Full study area"; the table scrolls sideways inside the panel.
- **desktop-10-table-full:** Harness miss, same full study area; 12 readable columns. Nits: filter placeholders are cut ("Area (kn", "Avg. suit", "% of cat"), and the table leaves about 180 px blank below it.
- **desktop-11-places:** OK.
- **desktop-12-places-full:** OK. Same button-font nit.
- **desktop-13-report-top:** OK. The heading "Gulf of America Program Area" is the harness's own `t=` title, not a name-table miss. Nits: grey 33–34 fill and the "LOUISIANA" label clipped at the map's top.
- **desktop-14-report-scrolled:** OK. Captions sit above the tables. Nit: the counts table cuts off at "IUCN:NT(2" with no scroll cue.
- **desktop-15-report-scrolled2:** OK. Nits: the AquaMaps citation still says "in this R package"; lowercase "great hammerhead shark" and "sicklefin devil ray".
- **desktop-17-species:** OK. Leatherback framed on the North Pacific; the West Coast is clear of the panel. Nit: "Category: turtle" is lowercase.
- **desktop-18-species-model:** Fixed. The walrus sits between the rail and the panel, clear of the legend card (range bottom at 657, legend top at 687) and no longer runs off the bottom. Nit: "Category: mammal" is lowercase.

## The previous review's four new defects
1. **Phone species framing: PARTLY (accepted).** The walrus is fixed (phone-18). The leatherback is no longer under the sheet or chip, but still frames badly (phone-17); that is the pending wide-range decision.
2. **Report caption clipped and names breaking mid-word: FIXED.** See phone-15 and desktop-14.
3. **Desktop legend over the walrus: FIXED.** See desktop-18.
4. **Harness desktop taps: NOT FIXED in effect.** The stop-at-first-real-cell loop is in `scripts/eyes-shots.mjs:113-124`. But all three desktop points, (335,400), (490,585) and (600,520), sample RGB 14,14,14 in desktop-02, which is land. So desktop-06, 07, 09 and 10 still show the full study area, not a per-cell flower or table. Nearby points that are scored cells: (320,420), (470,600), (640,520).

The routing of Program Area names through the name table (map tooltip, flower title, table header) cannot be checked from these shots, because no state selects a Program Area in the Scores lens. Only the report shows names, and they are correct there.

## New defects, ranked
1. **Harness, low:** the desktop tap points are stale for the globe camera. When every point misses, the loop also exits without a WARN, so the log reads clean while four desktop states are never tested. Fix: pick the tap point by projecting a known scored lon/lat for the current camera, and WARN or fail when nothing lands.
2. **Low should-fix, phone-15:** the report's counts table hides 6 of its 8 extinction-risk columns with no scroll cue. On the phone the cut falls exactly on a column edge, so the table looks complete: Total 25 / 11 under a caption saying "5,861 species". It needs a scroll fade or hint, or a transposed layout on narrow screens. Desktop-14 at least shows a cut header.
3. **Nit:** "coloured" (phone-14) and "colored" (desktop-13) in the same report.
4. **Nit:** lowercase source common names in the Top-20 table (desktop-15).

Carried-over nits, all still present:
- Raw "score" in the Layer select, legend and chip.
- Lowercase "turtle" and "mammal" in the species panel.
- Popup and panel coordinates differ, and the popup wraps the score.
- The × focus ring on the welcome modal.
- The native Layer select.
- Empty space in the legend modal.
- The Places action-button font size.
- The 33–34 one-place ramp.
- The duplicated place pill line in the report.
- The "in this R package" citation.
- The desktop table's cut placeholders and blank space below it.
- The small full-screen flower on desktop.
- Bird/Fish and Mammal/Turtle report colours still close.
- "GOA" meaning two things.

Screenshots are in `/Users/bbest/Github/MarineSensitivity/atlas/.tmp/eyes5/`, and the shot log is `/Users/bbest/Github/MarineSensitivity/atlas/.tmp/eyes5-shots.log`.
