# Eyes-on visual review (Opus 5.5) of atlas 0.10.55 (001bf18 / 12d730c), 2026-09-24

Orchestrator decision: PUSHED anyway — the three HOLD defects are all present in the live 0.10.50 build (reviewer's own finding), 0.10.55 makes nothing worse, and they go to fix rounds V1/V2.

claude-opus-5-5[1m]

**Verdict: HOLD.** The Program Area picker still shows acronyms only, although Ben reported it and it was marked fixed. On the phone, the walrus model view puts the range behind the legend chip and the sheet. On the phone, the report page scrolls sideways. None of these came in with Q2 or Q7: the live 0.10.50 build has all three. If your bar is "no worse than live", this build would pass. The checklist's rule ("any FAIL blocks") gives HOLD.

**How I checked.** I looked at all 34 PNGs in `/Users/bbest/Github/MarineSensitivity/atlas/.tmp/eyes/`. Some states were missed or unclear in that set, so I built atlas HEAD 12d730c into the scratchpad and took extra shots. 12d730c differs from 001bf18 only by one fault-patch file. The extra shots are in `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/1811e35d-6030-49df-bcff-ec4168b75d1f/scratchpad/verify/`. The atlas repo is untouched (`git status` is clean) and the preview server is stopped.

## Per screenshot
- **phone-01-welcome:** OK. Modal centred; Explore and Take a Tour reachable. Nit: the × is focused on load with a thick gold ring.
- **phone-02-map:** OK. The first view is scored northern Gulf of Mexico cells, not Canada. The chip sits above the sheet, clear of all controls.
- **phone-03-layers-half:** OK. Byte-identical to 02, because Layers is already the open sheet. Collapse, dock and full buttons are unobstructed.
- **phone-04-layers-full:** OK. The chip sits in its own band under the header, not over content or buttons. Nit: the "score" Layer dropdown is a native select with a different chevron and inset from the others.
- **phone-05-legend-modal:** OK. The ramp and its 0–93 labels are visible. Nit: about 80 px of empty card below the ramp.
- **phone-06-flower-half:** OK. Flower centred, 8 petals, hub 46, two-column Component | Score table, one "Cell ID" line. Nit: the popup gives the click point (-91.283, 28.594) and the panel the cell centre (-91.275, 28.575) for the same cell.
- **phone-07-flower-petal:** Not exercised; byte-identical to 06. My re-shot: tapping a petal shows "Bird: 65.6" with a gold outline that follows the petal shape, and no rectangle. OK.
- **phone-08-flower-full:** OK. All 8 components plus Mean are visible, one decimal.
- **phone-09-table-half:** FAIL as captured (only "Loading species…"). My re-shot: the per-cell list loads after about 7 s, with 4 columns and a Columns control, all readable. The app is fine; the harness waited only 3–5 s.
- **phone-10-table-full:** Same as 09.
- **phone-11-places:** Issue. The picker is closed in the shot; the page's option list shows only `ALA … SOC`. "Show analysis cells" is struck through with no hint. The drop zone lists GeoPackage with no "(not yet)" — OK.
- **phone-12-places-full:** Issue. Same picker. Nit: Share, Download places and Report use a larger font than the rest of the panel.
- **phone-13-report-top:** Issue. The report URL is cut off at the right edge of the screen, because the page overflows sideways.
- **phone-14-report-scrolled:** Issue (low). One map. The flower is left-aligned and its petals are pale while the legend swatches are dark; Bird and Fish are near-identical blues. There is a "GAA" pill plus a duplicate "GAA" line, and the raw key "primprod" appears.
- **phone-15-report-scrolled2:** Issue. The caption "…habitat-weighted extinc" and the ER column are cut off at the screen edge.
- **phone-16-more-menu:** OK. Share, Report, Feedback, About this release, Take a tour, Docs, plus Switch to light theme.
- **phone-17-species:** Issue. The leatherback loads, but the view shows only Alaska; the West Coast is at the right edge and Hawaii and the Pacific islands are off-screen or under the sheet.
- **phone-18-species-model:** FAIL. The walrus range sits at the bottom of the visible map under the chip and the top of the sheet, with empty Arctic above. It is the same after 15 s.
- **desktop-01-welcome:** OK. Modal centred, buttons reachable. Nit: the same gold ring on the × at load.
- **desktop-02-map:** OK. Alaska → Gulf → Atlantic (plus Puerto Rico) framed with padding. Tool rail, top bar and floating legend are clear of each other.
- **desktop-03-layers-half:** OK. Byte-identical to 02; the panel header controls are clear.
- **desktop-04-layers-full:** Issue. The "Full screen" panel stops at 720 px. The rest of the screen shows Europe and West Africa, and the tool rail is covered.
- **desktop-06-flower-half:** OK. The harness tap landed on land, so the flower shows the full study area: centred, 8 petals, hub 24, two-column table.
- **desktop-07-flower-petal:** Not exercised; identical to 06. My re-shot shows the label "Bird: 64.9" and a gold outline, no rectangle. OK.
- **desktop-08-flower-full:** Issue. Same 720 px cap.
- **desktop-09-table-half:** OK. Headers are shown in full, cells are distinguishable, and the table scrolls sideways inside the panel.
- **desktop-10-table-full:** Issue. Because of the 720 px cap, 9 columns are still squeezed: "Acrocephalus …" appears twice, and the filter placeholders read "MI" and "M".
- **desktop-11-places:** Issue. The picker is acronym-only. When you hover the struck pill, its reason is clipped at the panel edge: "ect a drawn or uploaded place first."
- **desktop-12-places-full:** Issue. Same 720 px cap.
- **desktop-13-report-top:** OK. One map; the GAA outline is drawn and zoomed to fill the frame. Nit: with one place the ramp is "33 to 34", so the only place is painted at the "low" blue end and looks grey. The "LOUISIANA" label is clipped at the top.
- **desktop-14-report-scrolled:** OK. The scores table has no footnote markers. Nits: swatch and petal colours don't match, Bird ≈ Fish, "primprod" header, and the species-count table runs past the content column.
- **desktop-15-report-scrolled2:** OK. Nit: the AquaMaps citation says "as provided in this R package".
- **desktop-17-species:** OK-ish. The leatherback is framed on the North Pacific with its legend. The West Coast runs under the right panel. All 6 input pills are struck through, and the reason appears only further down, under Values.
- **desktop-18-species-model:** OK-ish. The walrus fills the frame, but the range runs under the right panel at top right and off the bottom edge.

## Ben's seven phone findings
- **Legend chip covering the sheet's buttons:** fixed.
- **Blank legend modal:** fixed.
- **Off-centre flower with a focus rectangle:** fixed.
- **12-column table of 2-character ellipses:** fixed (4 columns plus a Columns control on the phone).
- **Duplicated empty report map:** fixed (one map in full-page shots on both screen sizes).
- **Per-cell footnotes restating the cell:** fixed.
- **Acronym-only Program Area picker:** NOT fixed for users (defect 1).

**The struck-through "Show analysis cells" pill:** it does not read as intentional. At rest, a dashed border with struck-through text looks like a removed feature. The only explanation is a tooltip, which is clipped on desktop and did not appear when I tapped it on the phone.

## Defects, ranked
1. **Blocker — Program Area picker is acronym-only** (phone-11/12, desktop-11/12). The options are `ALA, ALB, … SOC`. No published release bundle (v6, v7, v7b, v8, v9 `app/boot.json`) has a `name` on its `zones.programarea` rows, so `paLabel()` in `src/places/zoneStats.ts` falls back to the bare key. The report also labels the place "GAA" in its flower and tables. The fix is on the data side: msens `app_bundle_build()` needs to publish names and the bundles need republishing, or the app needs its own name table.
2. **Should-fix (high) — desktop "Full screen" is capped at 720 px** (desktop-04/08/10/12). `.panel { max-width: 720px }` in `src/lib/ui/Panel.svelte` limits the panel even though `.panel-region[data-maximized]` in `shell.css` spans the whole stage. The rest of the screen shows Europe and Africa, and wide tables stay truncated.
3. **Should-fix — the report page overflows sideways on a phone** (phone-13, phone-15). The page lays out 1,082 px wide on a 390 px screen. The species-count table and the scores table are not in their own scroll containers, so the URL, the caption and the ER column are cut off at the screen edge.
4. **Should-fix — species map framing on the phone ignores the sheet** (phone-18, phone-17; milder on desktop-17/18). The walrus range is hidden under the chip and sheet; the leatherback view shows Alaska only.
5. **Should-fix (process) — the eyes harness produced 3 false results** (`scripts/eyes-shots.mjs`). The petal selector (`svg path` nth(3)) never hits a petal, which are `path.petal`. The table step shoots before "Loading species…" clears, and the per-cell list takes about 7 s cold. The 02/03 shots are identical because Layers is already open.
6. **Should-fix (low) — struck-through "Show analysis cells" has no visible reason.** The desktop tooltip is clipped and nothing showed on a phone tap.
7. **Should-fix (low) — report flower colours** (phone-14, desktop-14). Legend swatches don't match the pale petals, Bird and Fish are near-identical blues, and the flower is left-aligned on the phone.
8. **Nits:**
   - A one-place report map uses a 33–34 ramp, so the place reads as "low".
   - When the tapped cell is low on the map, the phone popup is partly covered by the legend chip (my re-shot).
   - The popup's coordinates differ from the panel's for the same cell, and the popup wraps "score:" onto its own line.
   - Raw keys show in the UI: "score", "primprod", lowercase categories.
   - The gold focus ring on the welcome modal's ×.
   - The native Layer dropdown is styled differently from the other dropdowns.
   - Font size of the Places action buttons.
   - The legend modal has empty space under the ramp.
   - The report's species-count table spills past the content column.
   - "In this R package" in the AquaMaps citation.
   - v7's six struck-through input pills are explained only under Values.
