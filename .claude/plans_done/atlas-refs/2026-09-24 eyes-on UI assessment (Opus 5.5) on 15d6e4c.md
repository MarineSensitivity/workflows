claude-opus-5-5[1m]

# Eyes-on UI assessment: atlas 0.10.37 @ 15d6e4c, desktop + phone, dark, v7

- **Date:** 2026-09-24
- **Reviewer:** Opus 5.5 (UI reviewer subagent)
- **Inputs:** 34 PNGs in `scratchpad/eyes/before-15d6e4c/` (desktop 1280×800 @1x, phone 390×844 @2x), the
  driver `scratchpad/eyes/shoot.mjs`, the checklist `scratchpad/briefs/eyes-review.md`, and the atlas source *at 15d6e4c*
  (`git show 15d6e4c:…`; main has since moved to 876ed4c / 0.10.38).
- **Coordinates:** desktop positions are CSS px; phone positions are **device px of the 780×1688 PNG** (÷2 for CSS px).
- **Enlarged crops used as evidence:** `scratchpad/eyes/crops/`.

---

## 0. Coverage: what the screenshots could and could not show

The 34 files contain only **17 distinct frames**. Byte-identical groups (md5):

- desktop 01 = 02 = 03 = 04
- desktop 06 = 07 = 08
- desktop 09 = 10
- desktop 11 = 12
- desktop 13 = 14 = 15
- phone 01 = 02 = 03
- phone 06 = 07 = 08
- phone 09 = 10
- phone 11 = 12
- phone 13 = 14 = 15

Why these states were not reached (harness causes, not app verdicts):

| state | cause | fix for the next pass |
|---|---|---|
| 01 welcome (both) | `?ver=v7&theme=dark` counts as a deep link: `hasViewState()` (codec.ts) returns true, and WelcomeModal.svelte suppresses itself on any deep link. The "Explore" wait timed out silently. | Load `/atlas/` with no query; take dark from `colorScheme: "dark"`. |
| desktop 04/08/10/12 | The desktop panel's button is `aria-label="Full screen"` (Panel.svelte). "Full height" exists only on the phone Sheet. | Use "Full screen" on desktop. |
| desktop 06–10 | The three clicks landed on land in western Utah. The popup reads "Cell 2711027 · lon -113.526, lat 38.932 … no value". | The new points in shoot.mjs, (335,400), (490,585) and (600,520), do sit on scored cells in this frame. Better: a `?sel=` deep link. |
| phone 06–10 | The sheet detent **persists across page loads**. Phone 04 set "full", and phones 05–18 all open full-height, so every tap hit the sheet. | Use a fresh browser context per state. Also: new point (150,470) is under the half sheet, and (120,330) is Nevada. |
| 13–15 report (both) | The top-bar Report opens the report in a **new tab** (Shell.svelte `onReport` → `window.open`). On phone the script only reached the Report *tool* panel. | Capture with `context.waitForEvent("page")` after clicking Report / "Open report". |
| phone 07 | The petal click caused no visible change. This is consistent with the known "no petal values on tap". | none |

**Net effect:** desktop flower, desktop table, and the report at both widths are **unverified**. The verdict below does not
depend on them, because other states fail already.

---

## 1. State-by-state PASS / FAIL

### Desktop (1280×800)

| state | result | evidence |
|---|---|---|
| 01 welcome | **N/R** | No modal: the URL is treated as a deep link (see §0). The frame shows the default map with the Layers panel docked right. |
| 02 map | **FAIL** | Top bar and rail are fully visible, and the study area is framed with Alaska at top-left (x 370–580, y 100–250), the Atlantic to x≈690 and PR at (680,660). But **no legend anywhere**: it sits under the right-docked panel, visible only as a blurred rectangle with a faint red tint at x≈965–1245, y≈680–775 (see D1). |
| 03 layers | **FAIL** | Header controls are all visible at y≈85 (dock left/bottom/right, full screen, collapse). Stack rows are readable. But the Study area, Spatial units and Color palette boxes end at x≈1017/1031/983 while their chevrons float alone at x≈1235. The Layer select is clipped at "…category and primar" with no ellipsis (see D2). |
| 04 layers full | **N/R** | Identical to 03 ("Full height" does not exist on desktop). |
| 05 legend modal | n/a | Phone-only state. The desktop equivalent fails as D1. |
| 06 flower | **N/R** | The click landed on land. The panel shows only "No flower data is published for this selection in this release." That wording is itself a defect (D3). |
| 07 petal | **N/R** | Identical to 06. |
| 08 flower full | **N/R** | Identical to 06. |
| 09 table | **N/R / FAIL** | No scored cell was selected. For the land cell the panel reads "Species for Cell ID: 2711027" and then "**The species table could not be loaded.**" (D3). |
| 10 table full | **N/R** | Identical to 09. |
| 11 places | **FAIL** | **There is no Program Area list in Places at all**: only Pick mode, draw tools, the drop zone, "No places yet" and 0 / 20 (D5). "Show analysis cells" is struck through (D9). Share / Download places / Report use a different, larger typeface (D10). |
| 12 places full | **N/R** | Identical to 11. |
| 13–15 report | **N/R** | The report opened in a new tab. The captured page is unchanged: Layers panel, with the top-bar Report button left in its hover state. |
| 16 ⋯ menu | n/a | Phone-only state. |
| 17 species | **FAIL** | The lens loads: leatherback card, the Merged Model pill, taxonomy, values. But **no legend** (under the panel again, smudge at x≈990–1250, y≈700–770). The search box is drawn as a bordered input *inside* the bordered pill (D7). 6 of the 7 layer pills are struck through (D9). |
| 18 species model | **FAIL** | The walrus card loads (mdl_seq 54383, WoRMS 137077). But the range is a ~150×100 px sliver on the globe's top-left limb (x 380–535, y 145–250) and the map is **not framed to the model** (D8). No legend (D1). |

### Phone (390×844 @2x)

| state | result | evidence |
|---|---|---|
| 01 welcome | **N/R** | Same reason as desktop. The frame shows the map with the Layers sheet at half. |
| 02 map | **FAIL** | The top bar and tab bar are visible, but **the study area is not framed**: the Bering Sea and Aleutians run off the left edge at x=0, the Gulf of Mexico is a sliver at the sheet edge (y≈730–750), and Florida and PR are under the half sheet. There is ~200 device px of empty navy above the globe limb (y 97–300) (P2). The chip covers the Color palette select (known). |
| 03 layers half | **FAIL** | Header controls are visible (collapse, half, full at y≈845). The chip over the sheet content at y 1425–1510 is known. The same select defects: boxes end at x≈257/285, chevrons alone at x≈715, and the Layer select is clipped at "…and primary p" with no ellipsis (P3). |
| 04 layers full | **FAIL** | Header controls are visible (y≈190). Same select defects. The chip is over the sheet (known). Stack rows are readable. |
| 05 legend modal | **FAIL (known)** | A 338 px-wide "Legend" card with an empty body. |
| 06 flower | **FAIL** | No cell is selected (N/R for the cell case); the "Full study area" flower is shown. It is off-centre at x 135–340 of 780 (known). The 8 petals match the 8 listed components and the hub "24" is readable. But the **petals have no labels and there is no colour key**, the values are a **prose paragraph** rather than a component \| score table, and "Full study area" is said twice (P4). |
| 07 petal | **N/R** | Identical to 06: no label and no visible change after the tap (consistent with known). |
| 08 flower full | **FAIL** | Identical to 06 (the sheet was already full). |
| 09 table | **FAIL (known)** | "Cat…", "Taxo", "Scie.", "Co…"; cells reduced to "b…", "A…", "1…". |
| 10 table full | **FAIL (known)** | Identical to 09. |
| 11 places | **FAIL** | No Program Area list (P-side of D5). "Show analysis cells" is struck through. The Share / Download places / Report buttons use a larger Arial-like face (P8). |
| 12 places full | **FAIL** | Identical to 11. |
| 13–15 report | **N/R / FAIL** | Only the Report tool panel is shown; the document was not opened. Visible copy bugs: "**Pick a Program areas…**", "**Report on this Program areas**", and "Report on the current place list." without naming the place (P9). |
| 16 ⋯ menu | **PASS** | Share, Report, Feedback, About this release, Take a tour and Docs are all present, in ~88 device px rows. Minor: Take a tour and Docs share the same (?) icon (P7). |
| 17 species | **FAIL** | The leatherback card loads ("7 layers ▾"). The full-height sheet (persisted) hides the map entirely. The chip overlaps the Mask list (known). **There is no way to change species on the phone** (P1). |
| 18 species model | **FAIL** | The walrus card loads ("3 layers ▾", Share, Download this layer). The map is entirely behind the full sheet, so neither range nor legend can be seen. The chip opens the blank modal (known). |

---

## 2. Ranked defects NOT in the known list

**Known list, not re-reported:** phone chip over the sheet; blank legend modal; flower off-centre / focus box / no values on tap;
phone species-table ellipsis columns; PA picker acronym-only; report map; per-cell footnotes.

### Desktop (Ben has not reviewed this yet)

| # | sev | state | what is wrong | what right looks like | owner |
|---|---|---|---|---|---|
| **D1** | **blocker** | 02, 03, 17, 18 (every desktop map) | **No legend is visible on desktop.** ScoresLegend/SpeciesLegend are `position:absolute; right/bottom: var(--space-3); z-index:5`. That is the same bottom-right corner the default right-docked `.panel-region` fills top-to-bottom at z-index 16. The legend renders *under* the translucent glass and shows only as a blurred smudge with a faint red (Spectral end) tint at x≈965–1245, y≈680–775. Every desktop map is colour without a key. This is the desktop counterpart of the known phone legend bugs, with a different cause. | The legend floats bottom-right of the **visible** map, offset by the panel's live geometry: `right: panel width + gutter` when docked right, above the panel when docked bottom, normal when collapsed. Or it lives at the top of the Layers panel. It must be readable at every dock. | shell (`ScoresLegend.svelte`, `SpeciesLegend.svelte` positioning vs `.panel-region` in shell.css) |
| **D2** | major | 03 (and every Layers view) | **Two broken select styles in the first panel everyone sees.** (a) Study area, Spatial units and Color palette draw their border round the text only ("All US waters" box ends x≈1017), and their chevron sits alone at x≈1235, outside the box. `lib/ui/Select.svelte`'s `.select` has no `width:100%` inside a stretched `.select-wrap`. (b) The Layer select is a separate native `<select>`, full width with a heavy white chevron. Its text is clipped mid-word at "…species category and primar" with no ellipsis and no end padding. | All four selects are full-width boxes with the same chevron inside the right edge. A long layer name gets `text-overflow: ellipsis` plus the full name as `title`, or a two-line custom listbox. | panel (`lib/ui/Select.svelte`, `lens/scores/LayersPanel.svelte`) |
| **D3** | major | 06, 09 | **A click outside the scored area gives misleading output in three places.** (1) The popup gives a cell id and coordinates for **Utah**, then the full 30-word layer title, ending ": no value" (6 lines). (2) The Flower panel says "No flower data is published for this selection **in this release**", which blames the data. (3) The Table panel says "The species table **could not be loaded**", which reads as a failure. It may even be a real per-cell query error: check the console on a scored ocean cell. | A land or off-grid click makes no selection, or a one-line popup "No score here: outside the study area". The panels keep their previous (or study-area) content with the hint "Click a coloured ocean cell". The table's empty state is never phrased as an error. | map (popup) + lens (FlowerPanel / TablePanel empty states) |
| **D8** | major | 18 | **Selecting a model does not frame it.** `?mdl_seq=54383` (walrus) keeps the default globe, so the range is a sliver on the limb (x 380–535, y 145–250) and ~95 % of the frame is empty continent. The v8 species app framed each range (`msens::lon_span`). | On model load, `fitBounds(model bbox, padding incl. panel)`, keeping the narrower antimeridian frame for Pacific ranges. | map / species lens |
| **D5** | major | 11/12 | **Places has no Program Area list at all.** The only route is Pick mode, then clicking a polygon on the map. The only PA chooser is in the Report tool ("Pick a Program areas…"). The checklist expects "Aleutian Arc (ALA)"-style entries here. This overlaps the known "picker acronym-only" item: confirm that round covers *Places*, not just the Report chooser. | A searchable PA list (full name plus key) at the top of Places, where each row adds the PA to the place list. | places |
| **D9** | major (for v7) | 11, 17, 18 | **Disabled is drawn as strikethrough.** `Pill.svelte .pill--disabled` sets a dashed border and `text-decoration: line-through`. In v7 **6 of 7** species input pills (AquaMaps SDM, NMFS/FWS Critical Habitat, FWS/IUCN Range, SWOT+DPS) and Places' "Show analysis cells" look deleted. "(no published surface)" is also repeated ~10 times on the leatherback card. The convention is deliberate, but at v7 the whole input stack reads as broken. | Disabled means dimmed text with a dashed or no border, and the reason in a tooltip. Say "Input layers are not published for v7" **once** above the pills. | panel (`lib/ui/Pill.svelte`) + species lens |
| D7 | minor | 17, 18 | In the species lens the top-bar search is a bordered "Search species" input **inside** the bordered search pill (double outline). The inner box ends at x≈618, leaving the right third of the pill empty. The Scores lens shows one clean box. | One field, one border, full pill width, the same as the Scores lens. | shell / species (`SpeciesPicker .picker-input` inside `.search-field`) |
| D6 | minor | 02 (all) | The globe disc is centred on the full 1280 px width, so its right quarter runs under the 380 px panel: the labels "BE", "KIN", "IR" and "NORWAY" are chopped at the panel edge while x 84–258 is empty. The top limb is hidden under the top bar and "BOLIVIA" is cut at the bottom edge. Alaska, the largest part of the EEZ, is foreshortened on the limb, as are the Pacific parts of the species ranges (the leatherback strip at x≈260–310 in 17). | Camera padding includes the rail and panel. Consider a centre further west and north, or a flat default, so Alaska is not on the limb. | map / shell |
| D10 | minor | 11 | The **Share / Download places / Report** buttons are in a different, larger face (Arial/Helvetica-like, ~15 px) than every other control (Lato/Carlito, 13 px). The button does not inherit the font. They are also enabled with 0 places. | `font: inherit` at the token size. Disable or hide Share and Report until a place exists. | places |
| D12 | minor | 06–10 | The map popup is navy but its **tip is white** (the default `.maplibregl-popup-tip` border colour), visible at (380,412). | Tip coloured `--surface-raised` to match the popup. | map |
| D4 | minor | 06–10 | The popup restates the full layer description (6 lines) for any click. | Line 1: short value ("Score 42.3"). Line 2, muted: "Cell 2711027 · 38.93 °N 113.53 °W". | map |
| D13 | minor | all | Top-bar inconsistencies: "Feedback" is underlined (link style) while Share and Report are not; "?" and "ⓘ" are icon-only between labelled buttons; the "v7" chip is an 84 px pill with its chevron **left** of the text. | One button style for all top-bar actions; the chevron after the text. | shell |
| D14 | minor | all | The rail's active item is gold-filled **and** has a gold nub poking out of the rail's left edge at (12, 89). The nub looks like a stray artifact. The same nub sits on top of the phone's active tab. | One indicator: the filled button. | shell (Rail) |
| D15 | minor | 13 | There are two Report entry points that behave differently. The top-bar Report opens a new tab immediately, with no in-app feedback. The rail Report opens the Report panel. | One behaviour: open the panel, with an explicit "Open report" button. | shell |
| D16 | minor | Report tool | Copy: "Pick a **Program areas**…" and "Report on this **Program areas**" (`ReportTool.svelte`: ``Pick a ${unitLabel}``, where unitLabel is the plural set name). "Report on the current place list." does not say what is in it. | "Pick a Program Area…"; "Report on this Program Area"; "Report on Gulf of Alaska Program Area (1 place)". | shell (ReportTool.svelte) |
| D17 | minor | all panels | The desktop type reads small. Rail labels and panel body are 12–13 px Carlito, whose x-height is small. The grey italic notes on the species card ("(EN:100, TN:50)") are the hardest to read on navy. | 14 px body and 12–13 px rail labels at ≥900 px. Notes in regular, not italic, at `--text-secondary`. | shell / brand tokens |
| D18 | minor | 03 | "Layers on the map" is a static bullet list ("Raster cell values", "Cells outside Program Areas (off)") and looks unfinished. Probably superseded by U4 (0.10.38, "one Layers panel that IS the stack"), so re-check on the new tree. | A real stack with visibility, opacity and order controls. | panel |
| D19 | minor | 06, 09, 11 | The fixed full-height panel frame (R1) leaves a mostly empty glass slab over 30 % of the map when content is short: one line in the Flower empty state, and the lower 55 % of Places. It is also what hides the legend (D1). | A content-height panel up to a max, or a smaller default. | shell |

### Phone

| # | sev | state | what is wrong | what right looks like | owner |
|---|---|---|---|---|---|
| **P1** | **blocker** | 17, 18 (and all) | **There is no search anywhere on the phone.** The top-bar `.search-field`, which also hosts the SpeciesPicker in the species lens, is `topbar-desktop-only`, and the ⋯ menu has no Search item. A phone user cannot search places, and in the Species lens **cannot change species**: they are stuck on the default leatherback or whatever the link named. | A search icon in the phone top bar (or a ⋯ item) opening a full-width search sheet, plus a "Change species" row at the top of the species sheet. | shell |
| **P2** | major | 02 | **The first view does not frame the study area.** The Bering Sea and Aleutians are cut at x=0, the Gulf is a sliver at the sheet edge, Florida and PR are under the half sheet, and ~100 CSS px of empty sky sits above the globe. The fit ignores the sheet (and probably the width). | Fit the study area into the map area **above** the half sheet (bottom padding = sheet height), or open at "peek" so the map is the first thing seen. | map / shell |
| **P3** | major | 03, 04, 16 | The same select defects as D2: box borders end at x≈257/285 with the chevrons alone at x≈715, and the Layer select is clipped at "…and primary p". | As D2. | panel |
| **P4** | major | 06, 08 | **The flower cannot be read at rest.** There are 8 coloured petals and the hub "24" but no petal labels and no colour key. The only name-to-number mapping is a prose sentence ("Composite mean 24 across 8 components: Bird 45.7, Coral 10.4 …") that gives no colours, and "Full study area" is repeated. Overlaps the known flower round (values on tap) but is a separate requirement: confirm it is in scope. | A two-column component \| score table (one decimal) under the flower, each row with its petal's swatch, and short labels on or next to the petals. | lens (FlowerPanel) |
| P5 | minor | 05–18 | **The sheet detent persists across page loads.** One tap on "full" and every later visit, lens switch or deep link opens with the map completely covered (phone 17/18 show a species model with no map visible). | Persist at most "half", and always reset on a deep link or new load. | shell (Sheet) |
| P6 | minor | 01–18 | Map labels ghost through the sheet's right margin, the last ~15 CSS px: "MBI", "BOL", "PAR" and "URUG" are near-sharp at x≈750–780, y≈1030–1380 (a backdrop-filter edge). The globe and "PER" also show around the floating tab bar in 17/18. | A fully opaque edge (inset the blur, or a solid 1 px border band), and a solid band behind the tab bar. | shell |
| P7 | minor | 16 | The ⋯ menu's "Take a tour" and "Docs" both use the `help` (?) icon (TopBarActions.svelte). There is no Search item (see P1). | A distinct icon for Docs (book / external link). | shell |
| P8 | minor | 11 | As D10: Share / Download places / Report are larger and in a different typeface (visibly larger, ~16 CSS px, and Arial-like; "Polygon"/"Rectangle" beside them are 13 px Carlito). | As D10. | places |
| P9 | minor | 13 | As D16: "Pick a Program areas…", "Report on this Program areas". | As D16. | shell |
| P10 | minor | 17 | "Dermochelys coriacea" appears three times in one screen (title row, card heading, legend chip). The chip adds nothing when the sheet already names the species. | The chip shows the legend's *measure* ("Suitability 0–100") rather than the name. | shell / species |
| P11 | unverified | all | The selects and inputs use 13 px text. iOS Safari auto-zooms any focused input or select under 16 px. | Verify on a real iPhone. If it zooms, use 16 px for form controls under `(pointer: coarse)`. | shell / brand tokens |

---

## 3. The known items: what the desktop shows

- **Legend:** worse on desktop. The phone at least has a chip; the desktop legend is fully hidden under the default panel (D1).
  The fix round for the phone legend should also own D1, so "a legend with its ramp is visible" holds at both widths.
- **Flower (off-centre, focus box, values on tap):** not reachable on desktop in these shots (land click). Re-shoot with a
  scored cell before calling the flower round done at 1280.
- **Species table ellipsis columns:** not reachable on desktop. At 380 px the desktop panel is *narrower* than the phone
  sheet's CSS width once the rail is counted, so the same compression is likely. Re-shoot.
- **PA picker acronym-only:** the list is *absent* from Places at both widths (D5). The only chooser is the Report tool's
  select, which also has the grammar bug D16.
- **Report map / per-cell footnotes:** not reached at either width (the report opens in a new tab).

---

## 4. Cross-cutting observations

**Composition.**
- Desktop: acceptable but not good. The study area is inside the view with ~50 px top padding, but the globe disc
  ignores the panel, Alaska sits on the limb, and the left 170 px of map is empty.
- Phone: the first view fails. Half the study area is off-screen or under the sheet, with 100 px of empty sky on top.
- Species models are never framed (D8), so small ranges read as nothing on a globe.

**Hierarchy.**
- The single most important control in Layers, the layer name, is the one that is clipped.
- The flower's key information (which petal is which) is not on screen.
- Empty and error states outrank content: a land click replaces useful content with "could not be loaded".

**Spacing.**
- The fixed full-height desktop panel leaves large empty glass areas (D19).
- The phone flower has a ~70 CSS px dead gap between chart and text, and the chart uses ~25 % of the sheet width.
- The Places action row buttons are visibly larger than every other button.

**Contrast.**
- Gold on navy is strong.
- The weakest text is the grey italic notes on the species card, plus the deliberately low-contrast disabled buttons
  ("Add to places", "Report on this Program areas").
- The basemap's ocean (mid-grey) is lighter than its land (near-black), and the space around the globe is navy, so there
  are three background tones competing behind the score ramp.

**Brand.**
- Navy and gold are consistent in the chrome, the active states and the gold primary "Open report".
- Off-brand details: the white popup tip (D12), the Arial action buttons (D10), two different select styles (D2), and
  the double-bordered species search (D7).

**Looks unfinished.**
- Struck-through pills (D9).
- The bullet-list "Layers on the map" (D18).
- Share and Report enabled with 0 places.
- Map labels ghosting through the sheet edge (P6).
- The gold nub on the rail and tab bar (D14).

---

## 5. Verdict

**NOT PUSHABLE** as a public preview.

Three things to fix first, beyond the known list already in flight:

1. **D1: the desktop legend is hidden under the default docked panel.** Every desktop map has no key. Pair it with the
   known phone legend fixes, so a visible ramp is asserted at both widths.
2. **P1: no search or species picker on the phone.** The Species lens cannot change species on a phone, and places
   cannot be searched.
3. **D2/P3: the Layers selects.** The chevrons float outside the boxes and the layer name is clipped with no ellipsis,
   in the first panel every visitor sees, at both widths.

Next in line are D3 (land-click empty and error states), D8 (frame the selected model), P2 (phone first-view framing)
and P4 (flower labels and table).

**Before the next eyes pass, fix the harness (§0)** so that welcome, desktop flower/table/full and the report
document are actually captured. In this run, 11 of the 16 desktop states that apply at 1280 were not reached, and 5 of 18 on the phone (plus the selected-cell case of 06/08).
