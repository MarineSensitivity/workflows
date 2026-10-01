# Atlas docs chapter re-check (Opus 5.5) on docs `49e5afe`

**Reviewed:** `MarineSensitivity/docs` branch `atlas-guide` @ `49e5afe`: `apps/atlas.qmd` (361 lines), `apps.qmd`,
`_quarto.yml`, `libs/versioned.R` (`doc_atlas_url()`), and the 14 PNGs in `images/atlas/`. `Lnnn` = a line of
`apps/atlas.qmd` at `49e5afe`.

**Checked against:** atlas `main` (local `15a34e7` = `94e9c93` + a status.md commit; live = `95ee595`). The live
bundle at <https://marinesensitivity.org/atlas/> reports **0.10.48**. CHANGELOG 0.10.39–0.10.48 read in full (U1c is
filed under 0.10.39/0.10.40). server `origin/main` is still `5ae8869` (no Atlas route). msens `origin/main` is still
`0faf3b8` (`place_decode()` exists only on the local `atlas-contract` branch).

**Live checks** (headless Playwright against the live host, 0.10.48, script kept in the session scratchpad):
- phone 390×844 first view;
- a desktop click on land;
- draw a rectangle, then drag one of its corners.

---

## Verdict

**MERGEABLE to docs `main` after the listed edits.** No rework is needed.

The rewrite fixed all but five of the 53 findings (the five are #11, #24, #25, #42 and #43). The structure, the Share and link section, Feedback, the Report and
the retaken screenshots are now accurate. What is left is text:
- **Three sentences went stale when P8 (0.10.47) shipped** (Places "Export CSV", per-row "Open in report", Program Area
  pointer).
- **One earlier error came back** (the GeoPackage consent prompt).
- **One claim is contradicted by the live app** ("Corners stay draggable afterward": dragging a corner adds a *second*
  place).
- **One finding comes from my own first review.** Its "Uploaded place N" naming was wrong.
- **P5 and P6 need a clause each** (the desktop legend, and "No scored cell here").

All 14 screenshots are current for 0.10.48 and free of test artefacts. One caption (Layers) describes rows the image
does not show.

---

## 1. The 53 findings

**Fixed** = the chapter is now accurate at 0.10.48. **Stale** = it was fixed, then the app moved. **Open** = still
wrong.

| # | Topic | Status | Where now | Note |
|---|---|---|---|---|
| 1 | Budget checked only in Places ▸ Share | Fixed | L208–209 | Nit: "full-precision geometry". It is the *unsimplified, quantized* geometry. |
| 2 | Simplification never silent | Fixed | L213–216 | |
| 3 | Ladder aims at 2,000 characters | Fixed | L210–214 | Nit: say the ladder only starts **above 8,000**. See the optional edits. |
| 4 | `u.` fallback; recipient never prompted | Fixed | L216–220 | Nit: only the *copied* link ("Copy link anyway") carries `u.` tokens. The sender's address bar keeps the shapes. |
| 5 | Real fixture token | Fixed | L185–187 | Re-verified: `tests/fixtures/place_codec.json:62`. |
| 6 | Precision 4 "in both directions" | Fixed | L200–201 | |
| 7 | msens footnote | Fixed | L204–205 | A neutral sentence inside the collapsed dev note. |
| 8 | "always" softened | Fixed | L220–221 | |
| 9 | Two Share buttons | Fixed | L173–175 | `Shell.svelte:260` still copies `location.href` verbatim. |
| 10 | Invented dialog quote | Fixed | — | The figure carries it. |
| 10b | Switch on retired rows | Fixed | L324–325 | |
| 11 | Phone top bar | **Partly fixed: new error** | L73–76 | The ⋯ collapse and the search button happen **below 900 px** (`.topbar-desktop-only` / `.topbar-phone-only`, `max-width: 899px`). 381 px only hides the mark (`shell.css:781`). See N5. |
| 12 | Search in the Scores lens | Fixed | L331–332 | |
| 13 | Top-bar order | Fixed | L70–73 | Matches `Shell.svelte:1187–1380`. |
| 14 | Attribution lower-left | Fixed | L37 | |
| 15 | Preview host not live | Fixed | L28–33, L318–321, L342–343 | server main is still without an Atlas route. |
| 16 | Denied `?ver=` behaviour | Fixed | L321–324 | |
| 17 | "No application server of its own" | Fixed | L21–26; `apps.qmd` | |
| 18 | `@sec-app-scores` reference | Fixed | — | Removed. |
| 19 | `apps.qmd`: version is the path | Fixed | `apps.qmd` callout | The **pre-existing** `apps/scores.qmd` L11 and its Source Code line still say `?ver=`. That is on main, not in this branch; a follow-up. |
| 20 | "this book's own release picker" | Fixed | — | |
| 21 | Exact empty-state text | Fixed | L127–129 | Byte-matches `Places.svelte:812`. |
| 22 | Pick only highlights; "Add to places (n)" | Fixed | L131–134 | |
| 23 | GEO; bare keys | Fixed | L134–135, L337 | |
| 24 | Click-move-click, **Done** | **Partly fixed: new error** | L136–138 | "Corners stay draggable afterward" is contradicted live (N4). The polygon close gesture is not stated. |
| 25 | GeoPackage: no consent prompt | **Open (regressed)** | L149–151 | "a GeoPackage additionally prompts consent for a one-time ~22 MB download before it fails". `parseGeoPackage` throws before `consent()` whenever `runtime` is null (`geopackage.ts`, first line of the function; `UploadPanel.svelte:70` passes `runtime: null`). **No prompt ever appears.** Since 0.10.47 the refusal reads "GeoPackage is not supported yet." (N1) |
| 26 | `MAX_FILE_MB` aside | Fixed | — | |
| 27 | Row figures | Fixed (by P7/P8) | L157–160 | Now true: drawn places auto-analyse, and a Program Area row reads `metrics[composite]`. Live: "ALA … 28.3 composite". |
| 28 | Row actions incl. Open in report | Fixed | L160–162 | |
| 29 | Coordinate-dialog caption | Fixed | L155 | Retaken with a refusal. |
| 30 | Sphere / Cells-outside switches | Fixed | L93–95 | |
| 31 | Real stack rows and constraints | Fixed | L95–101 | |
| 32 | Species card sits in the Data row | Fixed | L112–113 | |
| 33 | One representation toggle | Fixed | L116–118 | |
| 34 | Legend position (species) | Fixed | L118–120 | |
| 35 | Six struck; stated as a limitation | Fixed | L122, L335–336 | |
| 36 | Flower/Table scope | Fixed | L233–235 | |
| 37 | Plain-mean hub; values table | Fixed | L237–239 | Nit: "per species category". v7's "Primary producer" petal is primary *productivity* (N16). |
| 38 | Gulf of America | Fixed | L241 | Cell 3372565 at −83.575, 28.275. |
| 39 | Species columns | Fixed | L243–245 | Matches `SpeciesTable.svelte:60–108`. |
| 40 | Zones table by current layer; treemap | Fixed | L245–247 | |
| 41 | Columns control on species only | Fixed | L247–249 | |
| 42 | CSV exports | **Stale since 0.10.47** | L249–252 | "the Places results tables' own 'Export CSV' buttons do not export anything yet". P8 item 4 wired them: `ResultsPanel.svelte:198–256` → `downloadCsv`. (N2) |
| 43 | Report scope | **Partly stale since 0.10.47** | L258–260 | The first sentence is right. The second, "A row's own 'Open in report' link opens the same report — every place, not just that row", is now wrong: P8 item 3, `Places.svelte#reportHref` encodes only `[places[i]]`. (N3) |
| 44 | Interactive map on screen | Fixed | L268 | |
| 45 | 99 % / 5 % footnotes | Fixed | L270–272 | |
| 46 | Parameters, Plot of Scores, PREVIEW | Fixed | L263–269 | |
| 47 | Parity twins; no R promise | Fixed | L274–278 | Nit: "Reproduce in R" sits inside the collapsible **Provenance** section (`Report.svelte:877`), not in Sources and Method (N12). |
| 48 | HTML flowers are SVG | Fixed | L287–288 | |
| 49 | ZIP `query/*.sql`; `places.geojson` | Fixed | L288–290 | Matches `exportFiles.ts:107–118`. |
| 50 | "Partially supports"; no screen reader | Fixed | L297–299 | |
| 51 | Two skip links | Fixed | L299–300 | `index.html:389–390`. |
| 52 | Budgets dated | Fixed | L301–304 | |
| 53 | Feedback | Fixed | L308–316 | Re-verified: `pages.yml` sets only `VITE_SEAL`/`VITE_AGENCY`. Every quoted string matches `FeedbackDialog.svelte:550–592`. |

**Section-level items from the first review:**
- Done: every one of the 14 figures has a `fig-alt`; the Generating Screenshots section; "More (⋯)"; "(beta)"; `doc_atlas_url()`, with the callout removed; the cross-references; the Quick start; Known limitations; the upload rules; the Places details.
- Not done: length (361 lines against the ~250 target, which the caller has made optional); a sentence on the Welcome dialog; the Column glossary.

### New inaccuracies (from the rewrite, or because the app moved)

**A = would mislead an analyst. B = a wrong detail. C = a nit.**

| # | Sev | Where | Chapter says | Code or live says |
|---|---|---|---|---|
| N1 | **A** | L149–151 | GeoPackage "prompts consent for a one-time ~22 MB download before it fails at the 'no runtime' refusal" | No prompt. The refusal is immediate and reads *"GeoPackage is not supported yet."* with the fix "Export the layer as GeoJSON or a zipped shapefile in your GIS and drop that instead." (`UploadPanel.svelte#honestRefusal`). |
| N2 | **A** | L249–252 | The Places results "Export CSV" buttons "do not export anything yet" | They download CSVs of the visible Components/Species rows (0.10.47). The zones table and the treemap still have no export. |
| N3 | **A** | L259–260 | A row's "Open in report" reports "every place, not just that row" | It reports **only that row's place** (0.10.47). |
| N4 | **A** | L138 | "Corners stay draggable afterward." | Live-verified on 0.10.48: draw a rectangle, click it, drag a corner, and the list goes from 1 row to 2 ("Drawn place 1", "Drawn place 2"). The original is not changed. The cause is terra-draw 1.35, which fires `finish` on select-mode `dragCoordinate`/`dragFeature`/`dragCoordinateResize`. `draw.ts#createDrawSession` forwards every `finish` to `Places.svelte#onDrawFinish`, which **appends** a place. The chapter must not promise editing (see app finding A1). |
| N5 | B | L73–76 | "Below 381 px the mark hides and everything past the lens switch collapses into one More (⋯) button …" | Below **900 px** (the phone layout) the title collapses to the mark, and Share, Report, Help, Feedback, About and the theme toggle fold into ⋯ beside a search button. Only the **mark** hides at ≤ 380 px. |
| N6 | B | L86–87 | "A floating legend chip tracks the current layer's color ramp …" | Since P5 (0.10.44) the **desktop** shows a full legend in a map corner. It moves clear of the panel on whichever side it is docked, and hides while the panel is maximized (the figure shows it bottom-left). The *chip* ("Legend · score") is phone-only. It opens the full legend and sits above the sheet, or inside the sheet's header at full height. |
| N7 | B | L105–107, L118 | Clicking selects it / "returns that cell's modelled value" | P6 (0.10.45). A click on land or outside the scored area shows **"No scored cell here · lon …, lat …"** and does **not** change the selection (live: `sel` is not written). In the Species lens a no-value click shows "No scored cell here". Selecting a species model flies the map to that model's extent. |
| N8 | B | L113–114 | Pills are "green for the merged model and orange for each input" | There is no green or orange. The merged model has a ✓ mark and a gold accent border; an input has ▶ and a grey border; the active pill is gold-filled (`LayerBarView.svelte:118–144`, and the figure). I missed this in the first review. |
| N9 | B | L149 | Several features become places "named 'Uploaded place N'" | They are named after the **file**: `<file name> 1`, `<file name> 2`, …, or just the file name for a single feature (`normalize.ts#nameOf`/`baseName`). The panel passes no `fallbackName` and no `nameProperty`. **I introduced this error in the first review's §2 item 2**, copying `docs/upload.md`'s example call, which is not what the panel does. |
| N10 | B | L163–165, L338–340 | A Program Area place's scores "live in the Flower/Table tools instead" | Selecting the row writes `sel=place:i`, and `parseScoresSelection` ignores it, so Flower and Table show the release's **default whole-area** flower. To see the Program Area, the user has to click it **on the map** (with Program Areas as the spatial unit) or choose it in **Table ▸ Zones**. |
| N11 | B | L56–59 (Quick start 2) | "Select the place in the list to see its coverage note, flower and component table" | That is true only of a drawn, typed or uploaded place. A Program Area picked in step 1 opens **no** results panel (its row shows the composite). |
| N12 | C | L273–278 | "Reproduce in R" is under Sources and Method | It is in the collapsible Provenance section. |
| N13 | C | L208–209 | "full-precision geometry" | The unsimplified geometry, already quantized to 3–4 decimals. |
| N14 | C | L209–210 | Up to 8,000 characters with a "long link" note | Between 2,000 and 8,000 the dialog says "This link is over 2,000 characters — some chat apps and older browsers truncate very long links." and offers **Copy anyway**. The ladder runs only above 8,000. |
| N15 | C | L216–219 | Every drawn place "becomes a `u.` token" | Only in the link that **Copy link anyway** copies. **Download GeoJSON** is the primary button. The sender's own `#pl=` is untouched. |
| N16 | C | L237 | "one … petal per species category" | Per score **component**. v7's "Primary producer" petal is primary productivity (app finding A4). |
| N17 | C | L227–229 | The query-key list reads as exhaustive | `QUERY_KEYS` also has `proj`, `in`, `rep`, `us`, `out`, `obis`, `show` and `hide` (`state/types.ts:89–110`). Say "among others". |
| N18 | C | L140–141 | Coordinate entry | Since 0.10.47 it also refuses a shape entirely outside the U.S. study area, as uploads do. |

---

## 2. "How places are stored in the link" against `placeCodec.ts` and `share.ts` at atlas main

Re-verified line by line. **Accurate**, apart from the C-level nits N13–N15.

- **Grammar** (L184–195): the `~` join, the `z.<set>.<key>[,…]` form with the sets `pa|pl|er|sr`, the `g1.<name>.<b64url>` form and `u.<name>.<8 hex>` all match `placeCodec.ts:3–6, 340–375, 386–397`.
  - The `u.` digest is the first 4 bytes of SHA-256 over `JSON.stringify(geometry)`, printed as 8 hex digits (`digest8`, `:422–428`).
- **Fragment**, not query (`HASH_KEYS = ["pl","t"]`). The claim "never sent to a server or a referrer" is correct.
- **The example** `g1.Gulf%20box.EAMBAQSf_grYrQPQDwAA0A_PDwA` is the fixture's own token (`place_codec.json:62`). The note "the address bar percent-encodes `~`" is correct: `URLSearchParams` gives `%7E`, and a name's `%20` becomes `%2520` (the live probe's URL shows `Drawn%2520place%25201`).
- **Dev note** (L200–205):
  - precision is 3, or 4 when **both** bbox sides are < 0.5° (`choosePrecision`, `:202–205`);
  - zigzag-varint deltas run across rings and polygons (`encodeGeometry`, `:257–282`);
  - the closing vertex is omitted;
  - base64url has no padding and no compression;
  - longitudes are unwrapped, and a > 180° step is **refused** at encode;
  - the fixture sentence is correct, and msens `place_decode()` is still only on `atlas-contract`.
- **Budgets and ladder** (L208–216):
  - `URL_SILENT_MAX` 2000 and `URL_LONG_MAX` 8000;
  - tolerances `[0.001 … 0.016]`;
  - `MAX_AREA_CHANGE` 0.01, with every ring simple;
  - every rung simplifies the **original**; a refused rung is skipped, and the one after it tried (`continue`);
  - it stops at the first rung ≤ 2,000, otherwise takes the coarsest accepted rung if that is ≤ 8,000, otherwise falls back to `u.` for every `geom` place with zone places kept (`fitPlacesToUrl`, `:435–507`);
  - **"Simplify and update the numbers"** writes `#pl=` first and then re-fits (`ShareDialog.svelte:83–97`).
  - All of this matches the chapter.
- **P8's `shareUrl` change** (`share.ts:69–85`): the copied link is now built by **parsing** `location.href` with `URL`/`URLSearchParams` and setting `pl` to `fit.hash`, with `pl` first and the same `%2C`/`%3A` un-escaping as `formatSel`. It no longer string-matches `pl=<old>`, so "Copy link anyway" copies the `u.` tokens even with several places or names that contain spaces. The chapter's claim (L215–216) that "the copied link and the displayed numbers can never disagree" is therefore true now; before P8 it was false in exactly those cases. No edit is needed beyond N15.
- **The recipient's view of a `u.` place** (L218–219) is right: a named row, "— km²", and the chip **"not analysed yet"** (`rowFigures` returns no status for `kind: "upload"`, and the chip falls through to that). There is no prompt. Zoom announces "This place's geometry isn't in this session — ask for the GeoJSON to zoom to it."

---

## 3. Screenshots (14)

All 14 were compared against live 0.10.48. The CI round's Layer-select `title` tooltip and the gallery-axe round change
nothing visible in these states.
- No `localhost`: the report permalink is `https://marinesensitivity.org/atlas/report.html?ver=v7#pl=z.pa.GAA&t=…`, and the QR is generated from that same `model.header.permalink.href` (`Report.svelte:183–188`).
- No test artefacts, and no error toasts.
- All are legible: the panel crops are 380 px, and the phone pair sits in `layout-ncol=2`.

| File | Caption true? | Current? | Action |
|---|---|---|---|
| `desktop-map.png` | Yes. The legend is visible bottom-left, the attribution is bottom-left, and the chevron sits after "v7". | Yes (P5) | Keep |
| `phone-map.png` | Yes | Yes. Live 0.10.48 renders the **identical frame**. | Keep. This is P6's first view, and it shows **no scored cell at all** (Canada and the Great Lakes; app finding A2). Retake once the app frames U.S. waters (optional). |
| `phone-more-menu.png` | Yes: 7 items, compass for the tour, "Switch to light theme" | Yes | Keep |
| `desktop-layers.png` | **No.** The caption says "scrolled to show the basemap sub-layer rows below Data", and the `fig-alt` names the Sphere and Cells-outside controls and the basemap rows. The image shows Selection, Zone outlines, **Place labels (moved above Data)** and Data expanded to its palette. There are no basemap rows and no switches. | Yes | **Fix the caption and `fig-alt`** to what it shows (it illustrates the reorder, which is the point of the feature), or retake it scrolled. |
| `desktop-species.png` | Yes (six struck) | Yes | `fig-alt`: the ESA/IUCN/WoRMS block is **below** the card, not "beside" it (C) |
| `desktop-places.png` | Yes (ALA 28.3 composite; bounding box 100 %, 42.7; "GeoPackage (not yet)") | Yes (P8) | Keep |
| `desktop-coordinate-dialog.png` | Yes (a refusal is shown) | Yes | Keep |
| `desktop-share-dialog.png` | Yes | Yes | Keep (266 px wide, but legible) |
| `desktop-share-ladder.png` | Yes (1,564 characters; 3501 → 411 vertices; −0.03 %) | Yes | Keep |
| `desktop-flower.png` | Yes (Gulf cell, Mean 51 = the plain mean of 8) | Yes | Keep. Its "Primary producer" petal is v7 primprod (A4). |
| `desktop-table.png` | Yes | Yes | Keep |
| `desktop-report.png` | Yes | Yes | Drop "(never `localhost`)" from the caption. It is reviewer voice (C). |
| `desktop-report-scores.png` | Yes | Yes | Keep. Cosmetic: the flower is clipped at the top; the species cross-tab overflows the text column. |
| `desktop-version-picker.png` | Yes | Yes | Keep |

---

## 4. Book fit and render risks

- **Inline `r` without a chunk:** safe.
  - L9–14 is a `{r}` chunk (comments only, `include: false`), which is enough for knitr to evaluate L18's `` [`r doc_atlas_url()`](`r doc_atlas_url()`) ``.
  - This is the **first chapter in a subdirectory** to call a `doc_*` helper, so I tested that the project-root `.Rprofile` is sourced for `apps/*.qmd`. It is, on Quarto 1.8.25: a scratch project with a root `.Rprofile` function called inline from `apps/t.qmd` rendered the URL.
  - `doc_atlas_url()` needs the same `msens::atlas_versions()` call that `apps.qmd` already makes.
- **`!expr` captions:** none. **Mermaid:** none.
- **`@` in prose:** only cross-references, and all 10 resolve: `fig-atlas-{map,phone-map,phone-more,layers,share-ladder}`, `sec-atlas-{scores,places,flower-table}` and `sec-releases`; `sec-app-atlas` is used from `apps.qmd`. Nine figures are never referenced from the text, which is harmless.
- **Figure layout:** the two phone figures sit in `::: {layout-ncol=2}` with their own ids, which renders side by side in HTML, PDF and DOCX.
- **`fig-alt` strings:** none contains an unescaped `"`.
- **The PDF (TinyTeX in CI):**
  - `▸` (5×), `⋯` (3×), `≤` and `Δ` are probably missing from the default font. That gives "Missing character" warnings, not errors. No other chapter uses `▸`, `⋯`, `≤` or `Δ`, although `→` is already in four.
  - "More (⋯)" keeps the word. "Places ▸ Share" would print as "Places Share". Optional: write "Places › Share", or "Places > Share".
- **Versioned render:**
  - `doc_atlas_url()` returns `?ver=` on public releases. For restricted books (v7b, v8, v9) it returns the not-yet-served `…/{ver}/atlas/`. The chapter says so, in Known limitations and in the helper's comment. That is acceptable.
  - The static v7 examples ("For v7, …") read fine in any version's book.
- **Front matter and sidebar:** match `scores.qmd` and `species.qmd`. The Atlas leads "Applications (Current)"; that is Ben's call.
- **`apps.qmd`:** "sharable" should be "shareable" (the chapter uses shareable). Everything else is accurate.

---

## 5. Ordered edit list

### Required before merge

1. **L148–151, Upload.**
   - Replace "(up to 20, named 'Uploaded place N')" with "(up to 20, named after the file and numbered)" (N9).
   - Delete the clause "; a GeoPackage additionally prompts consent for a one-time ~22 MB download before it fails at the 'no runtime' refusal above" (N1).
   - Optionally quote the refusal at L143–145: *"GeoPackage is not supported yet."*
2. **L249–252, CSV.** Replace it with: "**Download CSV** exports the species table; the zones table and the composition treemap have no export yet. A drawn place's Components and Species tables in Places each have their own working **Export CSV**." (N2)
3. **L259–260, Report.** Replace it with: "A row's own **Open in report** reports just that row's place." (N3)
4. **L136–138, Draw.**
   - Replace "Corners stay draggable afterward." with the close gesture: "(a polygon closes when you click its first vertex again, or press Enter)".
   - Add a Known-limitations bullet: "Dragging a drawn shape's corner **adds a new place** rather than changing the original: delete the extra row, or redraw" (N4).
   - Drop the bullet once the app fix (A1) ships.
5. **L73–76, Phone top bar.** Replace it with: "On a phone (below 900 px) the title collapses to the mark, and Share, Report, Help, Feedback, About and the theme toggle fold into one **More (⋯)** button, beside a **search** button; below 381 px the mark hides too." (N5)
6. **L86–87, Legend.** Replace it with: "On a desktop the map's legend floats in a corner clear of the panel, whichever side it is docked (hidden while the panel is maximized); on a phone it is a **Legend** chip above the sheet that opens the full legend, moving into the sheet's header at full height." (N6)
7. **L105–107 and L118, the P6 click.**
   - Add after L107: "A click on land or outside the scored area shows *No scored cell here* with its coordinates and leaves the selection unchanged."
   - At L118, after "modelled value", add: "(or *No scored cell here*); choosing a model flies the map to its extent." (N7)
8. **L113–114, Species card.** Replace "green for the merged model and orange for each input" with "the merged model marked ✓ with a gold edge, each input marked ▶" (or just delete the colour clause). (N8)
9. **The Program Area pointer** (L56–59, L163–165, L338–340):
   - Quick start step 2: "Select a drawn place in the list to see its coverage note, flower and component table … (a Program Area's row already shows its composite)."
   - L163–165 and L338–340: replace "its scores live in the Flower/Table tools instead" with "to see its flower and species, click it on the map (spatial unit: Program Areas) or choose it in **Table ▸ Zones**". (N10, N11)
10. **L103, the Layers figure.** The caption and `fig-alt` must describe what the image shows. For example, caption: "The Layers panel docked right, with **Place labels** moved above **Data** and the Data row expanded to its study-area, unit, layer and palette controls." Or retake it scrolled to the basemap rows.

### Optional (in this order; prose trimming last)

11. **L273–278.** Move the "Reproduce in R … unreleased branch" clause into the Provenance bullet (N12).
12. **Share nits** (N13–N15):
    - "unsimplified" for "full-precision";
    - "Between 2,000 and 8,000 characters the dialog warns that some chat apps truncate long links and offers **Copy anyway**; only above 8,000 does the ladder run";
    - "…in the link **Copy link anyway** copies (your own address bar keeps the shapes)".
13. **L237.** "one … petal per score component" (N16).
14. **L227–229.** "…`theme` and `tour`, among others" (N17). **L140–141:** "typed coordinates must also touch the U.S. study area" (N18).
15. **Captions.**
    - Drop "(never `localhost`)" from L282.
    - `desktop-species` `fig-alt`: "beside" becomes "below".
    - Generating Screenshots (L349–351): "the standard states; the dialog and panel figures here were captured by hand from the same live build". Six of the 14 figures are not among the script's 18 states.
16. **`apps.qmd`.** "sharable" becomes "shareable". Optionally, one sentence on the Welcome dialog ("Explore" / "Take a Tour"; `?tour=off` suppresses it) and on the Table's **(i) Column glossary**.
17. **PDF glyphs.** Optionally, "›" or ">" for "▸".
18. **Retake `phone-map.png`** once the app frames U.S. waters on a phone (A2).
19. **Trim the prose** toward about 250 lines. The best candidates are the Places paragraph at L157–171 and the report section.

**Follow-ups outside this branch:**
- `apps/scores.qmd` L11 and its Source Code line still describe `?ver=` (this is on main).
- Point the Atlas's Help ▸ Docs at `…/docs/{ver}/apps/atlas.html` after the merge (`Shell.svelte:448` still points at the book root).

---

## Appendix: app findings (atlas repo, new in this pass)

- **A1. Editing a drawn shape duplicates it.**
  - *Evidence:* live-verified on 0.10.48 (desktop 1280×800; draw a Rectangle; click inside it; drag a corner 30 px). The rows go from 1 to 2: "Drawn place 1" (the original, unchanged) and "Drawn place 2" (the edited shape). The terra-draw 1.35 bundle fires `onFinish(id, {mode, action: "dragCoordinate" | "dragFeature" | "dragCoordinateResize"})` from select mode.
  - *Cause:* `src/places/draw.ts` → `draw.on("finish", onFinish)` ignores the context, so `Places.svelte#onDrawFinish` appends a new place on every drag end. No unit test or e2e test drags a handle.
  - *Fix, either:* route non-`draw` actions to an `onEdit(featureId, geometry)` that replaces that place's geometry (this needs a feature-id → place-index map, and the place must round-trip through the codec again); or drop the edit `flags` and the "Drag its corners to adjust" announcement.
- **A2. The phone first view shows no data.**
  - *Evidence:* live 0.10.48 at 390×844@2x: Canada and the Great Lakes, with no U.S. water in frame (the same frame as `phone-map.png`).
  - *Cause:* P6's zoom boost plus the sheet shift is centred on the whole study-area bbox (Alaska through the Caribbean), whose centre is over land.
  - *Fix:* frame the lower-48 Program Areas, or pick a default camera per release.
- **A3. GeoPackage: dead consent code and a stale catalogue.**
  - `UploadPanel.svelte`'s comment says consent is asked "even when accepted", but `parseGeoPackage` throws before it calls `consent()` whenever `runtime` is null. The `window.confirm` prompt is dead code today.
  - `messages.ts:221–227` still carries the "wait and drop again" text. It is overridden only at the render site.
- **A4. "Primary producer" still labels v7's `primprod`** (primary productivity) in the flower, the flower legend and the report legend (`categories.ts:63`). The report's Table of Scores header says "primprod" for the same column.
- **A5. Upload naming differs from `docs/upload.md`.** The documented `fallbackName: "Uploaded place"` and the person-chosen `nameProperty` are not wired (`UploadPanel.svelte:117, 137`). Places are named from the file. Wire them, or fix `upload.md`.
- **A6. Scores-lens search is still a dead input** labelled "Search species and places", on the desktop field and in the phone search modal (`Shell.svelte:1238–1242`, phone modal `:1728`).
- **A7. Cosmetic.**
  - The report's species cross-tab overflows the text column (`desktop-report-scores.png`).
  - The ladder's vertex and cell counts are unformatted ("3501", "12059") and the area change uses a hyphen-minus ("-0.03%").
