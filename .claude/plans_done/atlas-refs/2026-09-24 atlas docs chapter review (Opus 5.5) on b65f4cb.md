# Atlas docs chapter review (Opus 5.5) on docs `b65f4cb`

**Reviewed:** `MarineSensitivity/docs` branch `atlas-guide` @ `b65f4cb`: `apps/atlas.qmd` (359 lines), the new
Atlas subsection and callout edit in `apps.qmd`, the `_quarto.yml` chapter entry, and the 12 PNGs in `images/atlas/`.
`Lnnn` below means a line of `apps/atlas.qmd`.

**Checked against:** `MarineSensitivity/atlas` main @ `25968fb` (0.10.43), as asked. **Atlas main has moved since:**
it is now at `add14e0` (0.10.44, P5 merged 08:59 CEST), and **0.10.44 is what is live**: the bundle at
<https://marinesensitivity.org/atlas/> reports 0.10.44. The only files that changed between the two commits are the
shell and top bar, the legends, `Select.svelte` and the Layers panel's announcements. Every other file cited below is
identical at both commits. Where 0.10.44 changed behaviour, the finding says so.

**Also read:** the live v7 `app/boot.json` and the live `FeedbackDialog-*.js` chunk; server main `5ae8869` (the Caddy
routes); msens main `0faf3b8`, plus the local msens branch `atlas-contract`; the atlas `docs/status.md`, `docs/upload.md`,
`docs/accessibility.md` and `docs/performance.md`; and the `atlas-9` plan.

---

## Verdict

**Not mergeable as is. It can be merged after the edits below.** This is a substantial revision pass, not a rewrite.

What already works:
- The structure.
- Most of the Overview, the rail and panel material, and the Scores lens.
- The `#pl` grammar and codec parameters.
- The report export list.

What has to change:
- About 30 claims are wrong or stale. The worst of them are in the three flows Ben asked to have documented:
  - sharing a place: the length budget, the simplification step, the two different Share buttons;
  - uploading a file: GeoPackage;
  - sending feedback.
- The preview host is described as serving the Atlas. It does not yet.
- 8 of the 12 screenshots need retaking:
  - one has a `localhost` permalink and QR code;
  - one shows a maximized, dimmed panel pointed at Europe;
  - the rest predate the 0.10.44 release that is live.

---

## 1. Accuracy against the code

Severity: **A** = wrong in a way that would mislead an analyst using the app. **B** = a wrong or stale detail.
**C** = unverifiable, or developer-only.

### 1a. "How places are stored in the link" (the g1 codec, budgets, ladder, `u.` fallback)

**Verified correct:**
- Places live in the fragment, not the query (`state/types.ts` `HASH_KEYS` and its rationale).
- Places are joined with `~`.
- `z.<set>.<keys>` with the sets `pa|pl|er|sr`.
- The `g1.<name>.<b64url>` grammar.
- Quantization: 3 decimal digits by default, 4 for a small place.
- Zigzag varint deltas, with the delta cursor running across rings and polygons.
- base64url without padding, and no compression, for the stated reason.
- Longitudes are stored unwrapped (the encoder refuses a ring that steps more than 180°).
- The `u.` digest is the first 8 hex characters of the SHA-256 of the geometry's JSON.
- The 2,000 and 8,000 budgets (`URL_SILENT_MAX`, `URL_LONG_MAX`).
- The tolerances 0.001, 0.002, 0.004, 0.008 and 0.016°.
- The rung rule: area change ≤ 1 % and every ring simple (`MAX_AREA_CHANGE`, `simplifyPlace`).
- Analysis runs on the decoded geometry: `geomPlace.ts` → `analysisGeometry()` → `roundTrip()`.

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 1 | **A** | L209–210 | The budget is "applied automatically whenever a place is added, shared or reported." | `fitPlacesToUrl` has exactly two call sites: `ShareDialog.svelte:51` and `:92`, the Places footer's **Share** dialog. Adding a place writes the full quantized geometry (`Places.svelte` `writePlaces` → `model.ts` `hashFromPlaces` → `encodePlaces`). The top-bar Share copies `location.href` verbatim (`Shell.svelte:255`). **Report** opens `report.html` with the full `#pl`. | "The link length is checked when you open **Share** in the Places panel. Until then the address bar carries every place at full precision." |
| 2 | **A** | L219 | A detailed upload "may be **silently** simplified". | Whenever `fit.simplified` is set, the dialog shows "Simplifying to X° would change this place from N to M vertices (±p % area, a → b cells)" and one button, **"Simplify and update the numbers"**. The copy button appears only after the user accepts it and the numbers recompute (`ShareDialog.svelte:83–97`, `:144–159`). | It is never silent. You accept the simplification, and it changes the area by at most 1 %. |
| 3 | **B** | L210–211, 215 | Up to 8,000 characters "with a note, and beyond that a … ladder". | `placeCodec.ts:468–506`. The ladder runs only when the unsimplified link exceeds 8,000 characters. Each rung simplifies the original geometry. It stops at the first accepted rung that brings the link down to **≤ 2,000**. If no rung does, it uses the coarsest accepted rung, provided that fits ≤ 8,000 ("long link" and **Copy anyway**). A rung refused for any one place is refused for all of them. | Say that the ladder aims for the 2,000-character link, not the 8,000 one. |
| 4 | **A** | L200–202, L215–216 | "the place falls back to the `u.` digest form … the recipient is prompted to re-upload or re-draw it." | The fallback turns **every** drawn, typed or uploaded place in the link into a `u.` token; zone places keep their `z.` tokens (`placeCodec.ts:500–505`). The recipient sees a named row with an upload icon, "— km²", and a "not analysed yet" chip, with no results (`Places.svelte:484–495`, `:795`). The app never prompts. Only the report says "…only its name was saved — re-add it from the Places panel to include it in a report" (`report/data.ts:115–122`). Nothing checks a re-added file against the digest. | Describe what the recipient actually sees, and tell the **sender** to use **Download GeoJSON** (the dialog's own advice). |
| 5 | **B** | L185 | Example: `#pl=z.pa.GAA~g1.Monterey%20box.EAECDMSNAdcC...` | That payload decodes to magic `0x10`, **precision 1**, npoly 2, nring 12. The app never produces precision 1 (it uses 3 or 4). The app also percent-encodes the fragment value (`formatSel` through `URLSearchParams`), so the address bar shows `~` as `%7E` and `%20` as `%2520`. | Use the shared fixture's anchor vector, `g1.Gulf%20box.EAMBAQSf_grYrQPQDwAA0A_PDwA` (`tests/fixtures/place_codec.json` "rect"; I re-derived it independently). Label it as the decoded value. |
| 6 | B | L193 | Precision 4 "for a place under half a degree across". | `choosePrecision`: precision 4 when **both** the bbox width and height are < 0.5° (`placeCodec.ts:202–205`). | Add "in both directions", or leave as is. |
| 7 | C | L195–197, footnote L204–207 | "Not independently verified … `msens` … does not yet contain a decoder." | This is true of msens **main** on GitHub (`0faf3b8`: no `inst/`, no decoder). But `place_encode()` and `place_decode()` exist on the **local, unpushed** msens branch `atlas-contract` (0.43.0, 35 commits ahead of main). Its `inst/fixtures/place_codec.json` is byte-identical to the atlas fixture (sha1 `6e41e17e…`). | Drop the footnote and write one neutral sentence ("an R decoder in msens is in preparation"). A user guide should not carry "not independently verified in this book". |
| 8 | C | L218 | "a simple lease-block rectangle … **always** survives" | This holds for one place: a 4-vertex rectangle is a ~41-character token. It is not guaranteed for many places plus a long base URL. | Soften "always". |

### 1b. Share, top bar, phone chrome

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 9 | **A** | L67–68, L222 | Top-bar **Share** "copies the current view as a link and announces what it carries"; the Share dialog opens "from Places' footer **or the top bar**". | The top-bar Share (`Shell.svelte:255–262`) writes `location.href` to the clipboard and announces "Link copied to your clipboard." It shows no dialog, no summary and no length check. The dialog opens **only** from the Places footer (`Places.svelte` footer → `<ShareDialog>`). | Describe the two Shares separately, and recommend Places ▸ Share whenever places are in the link. |
| 10 | B | L223–224 | Quotes "This link carries release v7, the scores lens, the current map view, 3 places. Link length: 1,842 characters." | This sentence is invented. The summary and the length are two separate lines (`ShareDialog.svelte:123–133`). The summary also names `layer "…"` when one is set, and says "the current release" when the URL has no `?ver=` (`share.ts:33–39`). | Quote what the figure shows: "This link carries release v7, the scores lens, 1 place." and "Link length: 91 characters". |
| 10b | B | L65–66 | In the release picker, "retired rows offering 'Switch'". | Every **public** release other than the current one offers **Switch** (`VersionPickerModal.svelte:46–55`). Today those all happen to be retired. | "other public releases offer Switch" |
| 11 | **A** | L54–56, caption L60 | On the phone the top bar keeps "the mark, release chip, lens switch and theme toggle, with search, Share, Report, Help, About and Feedback folded into a ⋯ (more) menu". | At `25968fb`, search could not be reached on the phone at all: it was desktop-only and not in ⋯ (the "P1" finding). At **0.10.44 (live)** the bar holds the mark (hidden below 381 px), the chip, the lens switch, a **search button** and **⋯**. The theme toggle moved into ⋯, which now reads Share, Report, Feedback, About this release, Take a tour, Docs, **Switch to light theme** (`TopBarActions.svelte:134–158`). The keyboard-shortcut list is desktop-only. | Rewrite both. Retake both phone shots. |
| 12 | **A** | L66–67 | "a search field (species **and places** …)" | In the Scores lens the field is a bare `<input type="search">` with no handler (`Shell.svelte:1196`, and the phone modal at `:1686`). Only the Species lens mounts the SpeciesPicker ("Search species", "Only species in US waters"). No place search exists. | "Search finds a species by common or scientific name (Species lens). In the Scores lens the box does nothing yet." Put this under Known limitations. |
| 13 | B | L48 caption, L70 | The top bar lists "…Share, Report, Help and a theme toggle", and "the About / Feedback / ⋯ control group". | The desktop order is Share, Report, (?) Help, **Feedback**, **(i) About**, theme. ⋯ appears only on the phone. | Fix the caption and L70. |
| 14 | B | L46 | "the release's own attribution in the lower-right corner" | This is the basemap's attribution ("MapLibre \| © OpenStreetMap contributors © CARTO"), in the **bottom-left** corner (`Shell.svelte:1408`; see the screenshot). | Fix. |

### 1c. Overview, versions, preview host

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 15 | **A** | L17–21, L331–337 | "The same static build runs unmodified at … `https://preview.marinesensitivity.org/{ver}/atlas/`" | On server main `5ae8869`, `caddy/app_version_routes.caddy` routes only `(scores\|species)`, and `preview_routes.caddy` has no Atlas route and no `atlas-preview` sidecar. The `atlas-9` plan that adds both is still in `plans_todo`. `/v9/atlas/` answers 302 to Cloudflare Access, and after sign-in it falls through to the landing `file_server`. The app's own "Continue on the preview host" link points there (`release/previewLink.ts:22–23`). | Present this as planned ("…will be served at …"). Until then, reviewers use the preview Scores and Species apps. Alternatively, Ben deploys atlas-9 before this merges. |
| 16 | B | L31–33 | "an unrecognized `?ver=` … opens the release picker with a link to 'Continue on the preview host' rather than silently falling back to another version's data" | `index.html` `decide()`: a denied version **falls through to `latest.txt`'s release**, and the denial is recorded. The picker opens automatically. Only the reason `restricted` gets the **Continue** link. An unknown version reads "is not a version this host recognizes." (`VersionPickerModal.svelte:22–33`). | "The promoted release loads underneath, and the picker opens to say why." |
| 17 | B | L14–17, and `apps.qmd` | "no server component at all — every score, click, flower plot and report is computed client-side" | Score and species rasters are tiles from the project's **titiler** (`lib/map/layers/titiler.ts`). A species click reads titiler's `/cog/point` (`lib/raster/point.ts`). Zone outlines are PMTiles on S3, and the basemap comes from CARTO. | "No application server of its own: scores, flowers, tables and reports are computed in your browser from the release's published files." Make the same change in `apps.qmd`. |
| 18 | B | L20 | "@sec-app-scores's 'one application, every release' idea" | That callout is in `apps.qmd` (`@sec-apps`), not in the Scores chapter. | `@sec-apps` |
| 19 | B | `apps.qmd` callout | "Scores and Species do this from a `?ver=` parameter — `?ver=v3` shows v3" | Since 2026-08-27 the version is the **path** on both app hosts (`/v3/scores/`), and `?ver=` 301-redirects there (see the `libs/versioned.R` `doc_app_url` comment). The sentence predates this commit, but the edit restates it. | "from the URL (`/v3/scores/`; older `?ver=v3` links redirect there)" |
| 20 | B | L337 | "Opening **this book's own** release picker inside the Atlas" | This is the Atlas's picker. | Fix the wording. |

### 1d. Places

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 21 | B | L139–140 | The empty state reads "Turn on pick mode and click a Program Area, draw a shape, …" | It actually reads "No places yet. Choose a Program Area above, turn on pick mode and click one on the map, draw a shape, enter coordinates, or drop a file on the map." (`Places.svelte:733`; the same at `25968fb`). | Quote it exactly. |
| 22 | **A** | L142–143 | Pick mode: click, or Ctrl/Cmd-click / long-press "to add more than one". | Picking only highlights. The **"Add to places (n)"** button is what adds. Several picks become **one** place (`z.pa.A,B`, via `addPicked` → `addZonePlace`), and the report splits that place into one row per key (`lib/report/model.ts` `expandPlaces`). Shift also works as a modifier. | Add the "Add to places" step and the one-place behaviour. |
| 23 | **A** | L144 | "a full-name-sorted picker ("St. George Basin (**GEB**)", not the bare key)" | The key is **GEO**. "Name (KEY)" appears only where the app bundle publishes names (`atlas/docs/status.md`, Known gaps). The v7 `boot.json` zone rows carry no `name`, so today the chooser and the rows show **bare keys** (GAA, GEO, …), as the share-dialog screenshot shows. | "St. George Basin (GEO) where the release publishes names; bare keys in current releases." |
| 24 | **A** | L147 | "a rectangle or circle is a drag" | terra-draw 1.35.0 defaults rectangle and circle to `drawInteraction = "click-move"` (checked in `node_modules/terra-draw`), and `draw.ts` does not override it. So you **click, move, click**. A circle is stored as a 64-gon. A **Done** button ends drawing. | Fix the gesture and mention **Done**. |
| 25 | **A** | L159–160 | "a GeoPackage additionally asks consent before its one-time fetch, naming the size and the third-party host" | `UploadPanel.svelte:50` passes `runtime: null`. `parseGeoPackage` then throws `geopackageNoRuntime` **before** it asks for consent (`parsers/geopackage.ts:91`), so no prompt ever appears. The refusal reads "…needs the data engine, which is not running in this tab yet … Wait for the map's numbers to appear and drop the file again, or convert the layer to GeoJSON" (`messages.ts:221–227`). The CHANGELOG says: "every `.gpkg` ends at the 'convert to GeoJSON' fallback refusal today". | "GeoPackage is listed but cannot be read yet. Export the layer to GeoJSON, FlatGeobuf or a zipped shapefile." |
| 26 | B | L157 | "(`MAX_FILE_MB = 10` in `src/places/UploadPanel.svelte`.)" | This is accurate (it restates `normalize.ts` `MAX_FILE_BYTES`), but it is a source-code aside in a user guide. | Drop it. |
| 27 | **A** | L166–167 | Each row shows "its area, once analyzed its data coverage percentage and composite score" | `rowFigures` (`Places.svelte:484–495`): a **drawn** place never gets coverage or composite. It always shows "—" and a **"not analysed yet"** chip; its numbers appear in the results below. A **Program Area** row reads `composite`/`score`/`pct_covered`/`coverage` from the top level of `boot.zones` rows (`zoneStats.ts:45–48`). v7's `boot.json` does not publish those there: the composite is under `metrics.score_extriskspcat_primprod_ecoregionrescaled_equalweights`, and `coverage` is null. So **every** row reads "not analysed yet" today, as the chapter's own figure shows ("GAA 382,003 km² — not analysed yet"). | Describe what the user sees. Point to the results panel for the numbers. List as a known limitation (and see app finding 3). |
| 28 | B | L167 | "per-row actions (rename, zoom to, duplicate, delete)" | There is also **Open in report**. Rename is the inline name field (≤ 60 characters), and both Rename and Duplicate apply only to drawn or uploaded places. A Program Area place offers zoom, delete and report. | Fix. |
| 29 | B | caption L164 | The coordinate dialog is shown "with the same verbatim what/why/fix refusal format" | The screenshot shows the empty placeholder, with no refusal. | Retake with a refusal, or trim the caption. |

### 1e. Scores lens, Species lens, Flower, Table

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 30 | B | L100–102 | The Data row has "study area, spatial unit, the score layer, color palette". | It also has a **Sphere (globe projection)** switch, and a **Cells outside Program Areas** switch (raster units, when the release publishes that overlay). See `lens/scores/LayersPanel.svelte`. | Add both. |
| 31 | B | L102–106, caption L108 | The stack is "outlines/labels, a Program-Area/ecoregion boundary group, and the basemap's own sub-layers …; **every** row … can be … moved up or down" | The rows are **Selection, Zone outlines, Data, Place labels, Roads & buildings, Boundaries, Bathymetry** (disabled until a relief layer exists) and **Land & water** (`layerStack.ts` `LAYER_GROUP_LABEL`, `LAYER_GROUP_ENABLED`). **Selection** is pinned on top, and **Data** cannot move above **Zone outlines** (`moveLayerStackEntry`, `enforceDataOrder`). A **Reset layers** button restores the default. | Use the real row names and state the constraints. |
| 32 | B | L119–120 | "the panel becomes the species card" | The species card replaces the **Data** row's contents inside **Layers** (see the screenshot). | Minor: say where it appears. |
| 33 | B | L126 | "**Two** toggles sit beside the pills" | There is **one**: the representation toggle. It appears only when the selected **input** publishes both representations (`data/layerBar.ts` `RepresentationToggle.available`; `LayerBarView.svelte`). A "show Merged Model" link appears when an input is selected. | Fix. |
| 34 | B | L130–131 | "the legend **under** the map" | The legend floats in a corner of the map on the desktop, and is the legend chip on the phone. | Fix. |
| 35 | **A** | caption L133 | The merged model "with **five** inputs struck through — published … but not, in this release, as their own drawable surface" | The screenshot shows **six of six** inputs struck through. For v7 this is a **known release-side gap**, not the design: "struck-through pills for unpublished v7 surfaces (release-side msens fix, not the app)" (`atlas/docs/status.md`, Known gaps). | Fix the count. State the limitation: in v7 the Atlas cannot yet draw the individual inputs, and the Species app can. |
| 36 | **A** | L242; section overall | The Flower plot works "once a cell, Program Area **or Place** is selected" | `FlowerPanel.svelte` handles cell and zone selections, and shows the release's default whole-area flower when nothing is selected. A drawn place's flower is in the **Places results** instead. The Table tool's species list works the same way. In the **Species lens**, **Table** shows the placeholder "The species and zone tables arrive in a later phase." (`Shell.svelte` panel body → `tools.ts:39–45`). | State plainly that Flower and Table are Scores-lens tools for a cell or Program Area. |
| 37 | B | L244–247 | "a shared inner hub that carries the **weighted** center value … values … list below the plot as **plain text**" | The hub shows the **plain mean** of the components present. Every petal has equal angular width (`flowerGeometry.ts` header, `even = 1`), which is unlike the Scores app, where petal width is the weight. Since 0.10.41 the values are listed in a **table** (swatch, component, score, and a **Mean** row). | Fix. |
| 38 | B | caption L249 | "…a clicked cell in the **Gulf of Mexico** … the map popup … below it" | The book uses **Gulf of America** everywhere else (`scoring.qmd`, `science.qmd`, `releases/v1.qmd`), and the app calls it GAA. The popup is on the map, not below the plot. | Fix. |
| 39 | B | L252 | Species columns include "ESA/IUCN/MMPA/MBTA flags" | The columns are Category, Taxon, Scientific name, Common name, **ER code**, ER score, Model, MMPA, MBTA, Area (km²), Avg. suitability and % of category (`SpeciesTable.svelte:60–108`). ESA and IUCN status are folded into **ER code**. There is also a **Column glossary** (i), and the **Model** column opens that species. | Fix. |
| 40 | B | L253–255 | The zones table is "ranked by its composite score"; the composition treemap shows "species contribution" | The zones table is ranked by **the current layer**. The treemap has **one box per species category**, sized by suitability × ER × area (`composition.ts` header). | Fix. |
| 41 | B | L255–256 | "the species **and zones** tables open with a compact default column set and a **Columns** control" | Only the **species** table has this (`speciesTableColumns.ts`; `ZonesTable.svelte` has no Columns control). Both tables get minimum column widths, a sticky header and first column, and horizontal scrolling. | Fix. |
| 42 | **A** | L258 | "Every table has a CSV download." | Only the Table tool's **species** list has a working **Download CSV** (`TablePanel.svelte` `onExportCsv`). The zones table and the composition treemap have none. The Places results' Components and Species tables show an **Export CSV** button that does nothing (`DataTable.svelte:153–162` always renders it, and `ResultsPanel` passes no `onExport`). The report offers a per-place CSV of the full species list, and the ZIP contains CSVs. | Fix, and see app finding 5. |

### 1f. Report

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 43 | B | L264–266 | Report covers "whatever places are currently selected" | `reportAction` (`shell/report.ts`) reports **every place in the list**. With no places, it reports the selected Program Area. With neither, the rail tool shows a chooser. A clicked **cell** cannot be reported. A row's **Open in report** link carries `sel=place:i`, but `report.html` ignores it and reports every place (`Report.svelte:79–81`). | Fix, and see app finding 4. |
| 44 | B | L271 | "a static map snapshot" | On screen the map is **interactive**. The static PNG is used only in print and in the HTML export (0.10.43; `report.css` `.map-print`). | Fix. |
| 45 | B | L273–274 | Footnotes appear "for any component whose data coverage falls under **the release's coverage floor**" | A footnote appears when a component covers **< 99 %** of the area (the app's threshold), or when the release's own 5 % floor dropped it (CHANGELOG 0.10.43). | Fix. |
| 46 | B | L268–280 | (list of sections) | The list leaves out the collapsible **Parameters** section (kind, keys or vertices, area, N cells, study-area share, token). The flowers sit in tabs under "Plot of Scores". A restricted release adds a PREVIEW banner and print watermark. | Add these. |
| 47 | B | L278–279 | "the SQL that actually ran (**the same query templates `sql/` shares with the `msens` R package**)" | `sql/*.sql` are atlas-side **twins** of msens functions, parity-tested (`npm run parity`, max\|Δ\| < 1e-9). msens does not contain them. The report's **Reproduce in R** snippets (`lib/report/provenance.ts:88–110`) call `msens::place_decode()`, `cells_in_polygon_grid()` and `scores_for_cells(blend =, denominator =)`. These exist only on the unpushed `atlas-contract` branch. | "…each query a parity-tested twin of an msens function." Do not promise R reproduction for drawn places until that msens is released. |
| 48 | B | L289 | The HTML export embeds "the map and flower as embedded PNG data URIs" | The flowers stay **inline SVG**; only the map is a PNG data URI (`exportHtml.ts` header). | Fix. |
| 49 | B | L290–291 | The ZIP contents | It also holds `query/*.sql` (`exportFiles.ts`). `places.geojson` holds drawn and uploaded places only; zone places are left out. | Add. |

### 1g. Accessibility, performance, feedback

| # | Sev | Where | Chapter says | Code says | Fix |
|---|---|---|---|---|---|
| 50 | B | L302–304 | a "WCAG 2.1 Level AA accessibility conformance note" | The note's claim is **"partially supports"**. It is dated 0.10.24 (2026-09-23) and says "**no screen reader was actually driven**". The layer stack (0.10.38) postdates it. | Give the claim as the note gives it. For a Section 508 audience this matters. |
| 51 | B | L309 | "skip links jump past the map to the tool rail" | There are **two** skip links, "Skip to the tools" and "Skip to the details panel", at the very top of the page (`index.html:389–390`). | Fix. |
| 52 | C | L315–320 | the size and timing budgets | They match `performance.md`, but they were measured at **0.10.20** on 2026-09-23. | Date them, or cut them (they are developer material). |
| 53 | **A** | L324–329 | Feedback "posts to the project's feedback endpoint, or falls back to a prefilled GitHub issue if that post fails" | The production build sets **no `VITE_FEEDBACK_URL`**. The `pages.yml` `vite build` env has only `VITE_SEAL` and `VITE_AGENCY`, and the live `FeedbackDialog` chunk carries only the `localStorage` override. So **Send is disabled**, and the dialog says "This build has no feedback endpoint configured yet — use 'Open as GitHub issue' below." **Open as GitHub issue** is a separate button, never a fallback. It opens a **public** issue in `MarineSensitivity/atlas` labelled by kind, and copies the screenshot to the clipboard for you to paste. On a restricted release there is no GitHub route, and "feedback cannot be sent from here right now". Once the endpoint is configured, a public-release submission sent without an email is **also filed as a public GitHub issue** (the dialog's "What is sent" hint). The dialog also has an optional **Title**. | Rewrite. Tell analysts plainly that the text becomes public. |

---

## 2. Completeness for the stated purpose

Ben asked to "document the app, like with uploads". Here is whether a first-time user learns how to:

- **Draw:** partly. The gesture is wrong (#24). **Done**, editing and naming are missing.
- **Enter coordinates:** yes.
- **Upload:** the format list, yes. The rules that actually cause refusals, no. GeoPackage is misdescribed (#25).
- **Share:** confused. The two Share buttons behave differently (#9), and the budget description is wrong (#1–#4).
- **Report:** mostly. The sections need #43–#49.

**Missing, and the app does it** (ordered by value to a first-time analyst):

1. **A procedural Quick start**, for desktop and for phone. The book's other app guide (`apps-guide.qmd`) is step by step. For example:
   1. Places ▸ choose a Program Area, or draw a Rectangle (click, move, click).
   2. Select the place to see its coverage note, flower, components and species.
   3. Rename it inline.
   4. Places ▸ Share ▸ Copy link.
   5. Places ▸ Report ▸ Print, HTML, ZIP or Word.

   On a phone: use the Program Area chooser or Enter coordinates, and the file picker (dropping a file on the map is a desktop gesture).
2. **Upload rules** (`atlas/docs/upload.md` rules table):
   - polygons only: points and lines are refused, never buffered;
   - a CRS is required: a shapefile needs its `.prj`, and projected coordinates are refused;
   - ≤ 50,000 vertices;
   - a zipped shapefile ≤ 50 MB uncompressed and ≤ 200 entries;
   - the shape **must touch the U.S. study area**;
   - one place per feature (up to 20), or one merged place; new places are named "Uploaded place N".
3. **Places details:**
   - "Add to places";
   - the **20-place cap** ("0 / 20 places");
   - **Recent** (Add back / Clear; this browser only);
   - **Show analysis cells** (paints the analysed 0.05° cells, up to 20,000);
   - the "outside US waters here: no scores can be computed" message;
   - the species prompt ("Loading species needs N tiles, about X MB. Continue?").
4. **What the recipient of a link sees:** the same places and the same numbers, because the decoded geometry is the analysed geometry. A `u.` place appears as a name-only row.
5. **The Welcome dialog and the guided tour.** The Scores tour has 8 steps: map, release, lenses, layers, click, table, places, report & share. The Species tour has 5: search, card, map, legend, back. `?tour=on` starts the tour and `?tour=off` suppresses the welcome.
6. **The About (i) dialog** (release, status, date, app version; Documentation, What changed, GitHub). **Help ▸ Keyboard shortcuts** (Tab/Shift+Tab, Esc, ↓/↑/Home/End in the rail, `=` to zoom in).
7. **Table extras:** the Column glossary; the Model column opens that species; **Report on selected** in Zones.
8. **Scope by lens:** Flower is Scores-only. Table is a placeholder in the Species lens. Places and Report work in both lenses.
9. **Phone-specific paths:**
   - the sheet's drag handle and its collapse, half and full buttons;
   - the search button (0.10.44);
   - the contents of the ⋯ menu, including Theme;
   - Program Area multi-pick by long-press.
10. **A short "Known limitations (as of 0.10.44)" box.** It is easier to maintain than scattering caveats through the prose:
    - search does nothing in the Scores lens;
    - GeoPackage cannot be read;
    - v7 species inputs cannot be drawn;
    - Program Areas are shown by key;
    - list chips say "not analysed yet";
    - feedback **Send** is not configured;
    - the preview-host Atlas is not yet served;
    - Reproduce in R for drawn places needs an unreleased msens.
11. **Status.** The atlas-9 plan's step B1 labels it **"Atlas (beta)"**, beside the Shiny apps until cutover. Neither the chapter nor `apps.qmd` says it is in beta.

The **layout** is covered well on both desktop and phone: the rail and tab bar, the dockable panel and the three-detent sheet, and the legend chip. The phone **top bar** is wrong (#11), and the **task paths** on the phone are absent.

---

## 3. Fit with the book

- **Front matter and heading ids** follow `apps/scores.qmd`: a `title:` of the form "Name — tagline", `# Atlas {#sec-app-atlas}`, and `categories: [current]`. `_quarto.yml` places it first under "Applications (Current)". Ben should decide whether it leads before cutover, and whether to label it "(beta)".
- **Cross-references.** Every id resolves: `sec-app-atlas`, `sec-atlas-scores`, `sec-atlas-places`, `sec-atlas-place-links`, `sec-atlas-flower-table`, `sec-app-scores` and `sec-apps`. Three point to the wrong place:
  - L20 `@sec-app-scores's` should be `@sec-apps`;
  - L298 `(@sec-apps)`, cited for pipeline reproducibility, should be `@sec-workflows` or `@sec-releases`;
  - L332 `[Releases](../releases.qmd)` should be `@sec-releases`.
- **Figures.** The 12 ids are unique. Six are referenced in the text (map, phone-map, phone-more, layers, places, coords) and six are not. None has a `fig-alt`, which `apps-guide.qmd` uses and a Section 508 audience expects.
- **Versioned render and CI.** The chapter has no inline `` `r` ``, no chunks, no `!expr` captions and no mermaid, so there is no render or CI risk. The book-wide `mermaid-format: png` does not touch it. A grep for the versioned-render bleed tokens (`mdl_key`, `global05`, `primary_producer`, `is_valid_usa`, `mdl_seq`, `planarea`, `native_asset`) finds none in `atlas.qmd` or `apps.qmd`.
- **Hard-coded URLs** (L11, L18–19, L333–335). **Every** version's book, including a restricted book on the preview host, will link to the Atlas's **promoted** release, whereas the Scores and Species links follow their book's version through `doc_app_url()`. The L342–351 callout is commentary addressed to maintainers. Its claim that fixing this is "more than a one-line addition" overstates it: it takes about six lines in `libs/versioned.R`, plus the `#| include: false` chunk that `apps.qmd` already uses (use the preview branch only once #15 is deployed):

  ```r
  #' URL of the Atlas for a version (public: ?ver=; restricted: the preview path).
  doc_atlas_url <- function(ver = doc_ver()) {
    if (identical(doc_access(ver), "restricted"))
      sprintf("%s/%s/atlas/", doc_preview_url(), ver)
    else sprintf("https://marinesensitivity.org/atlas/?ver=%s", ver)
  }
  ```

  Then delete the callout.
- **Length and tone.** The chapter is 359 lines, against 80 for Scores and 69 for Species. Places and Report justify some of the extra length. About 80 lines, though, are developer material:
  - the codec byte layout;
  - the msens footnote;
  - the `MAX_FILE_MB` aside;
  - the `doc_app_url` callout;
  - the KB budgets.

  Cut these, or move them into a `::: {.callout-note collapse="true"}` titled "For developers". Remove the reviewer's voice: "Not independently verified in this book", "per the codec's own source comments", "A few figures worth carrying into this book".
- **"Generating Screenshots".** `scores.qmd` and `species.qmd` have this section; this chapter does not. Add one that points to the atlas `scripts/eyes-shots.mjs` (18 states at 390×844 and 1280×800), run against the live host.
- **The PDF download** (`downloads: [pdf, docx]`, `scrreprt`). "⋯" (U+22EF) is probably missing from the default LaTeX font, so the glyph drops. Write "**More (⋯)**" so the word survives.
- **Image sizing.** The book's column is about 750–800 px wide, so the 1280-px shots render at about 60 %. Panel text then renders at about 7–8 px, which is legible only through the lightbox (`lightbox: true` is set). The phone PNGs are 780×1688 at 2×, so each renders at full column width and about 1,650 px tall. Set `{width=40%}`, or pair the two shots in a `::: {layout-ncol=2}`. For the panel figures (Places, Share, Coordinates, Flower, Table), crop to the panel.

---

## 4. Screenshots

Most of the shots show UI that 0.10.44 changed ("stale chrome"):
- the chevron sits to the left of "v7";
- **Feedback** is underlined;
- the Select boxes shrink to their label, with the chevron floating at the far right of the field.

| File | Shows | Matches caption? | Current UI? | Problems | Action |
|---|---|---|---|---|---|
| `desktop-map.png` | Scores, v7, globe, Layers panel | Partly | No (stale chrome) | **No legend visible**: it is hidden under the right-docked panel, the defect 0.10.44 fixed. The attribution is bottom-left, but the text says lower-right. | **Retake** |
| `phone-map.png` | Phone, sheet at half | Yes (for 0.10.43) | No | The top bar has the theme button and no search button (0.10.44 swapped them). Shrunk Select. | **Retake** |
| `phone-more-menu.png` | The ⋯ menu, 6 items | Yes (for 0.10.43) | No | "Take a tour" and "Docs" share the (?) icon. Live 0.10.44 has 7 items (+Theme) and a compass icon for the tour. | **Retake**, and fix the caption |
| `desktop-layers.png` | The Layers panel, **maximized** | No | No | A backdrop dims the whole image. The map is turned to Europe and Africa. Only Selection, Zone outlines and Data are visible, not the basemap rows the caption names. | **Retake** with the panel docked and scrolled to the basemap rows, ideally with Place labels moved above Data to show the point of the feature |
| `desktop-species.png` | Leatherback, v7 | No (six struck, not five) | No | It illustrates the known v7 release-side gap as if it were the design. No legend. | **Retake** after the msens fix, or re-caption as a limitation |
| `desktop-places.png` | Empty Places panel (0 / 20) | Partly (there is no "current place list") | No | This is the most important figure for the stated purpose, and it shows no place, results or flower. | **Retake** with 1–2 places, one selected, results expanded |
| `desktop-coordinate-dialog.png` | The dialog with its placeholder | No (no refusal shown) | No | — | Retake showing a refusal, or trim the caption |
| `desktop-share-dialog.png` | Summary, "91 characters", Copy link | Yes | No | The row behind it ("GAA … not analysed yet") contradicts L166–167. It does not show the ladder, which is the section's subject. | Keep or retake. Add a shot of the ladder ("Simplify and update the numbers"). |
| `desktop-flower.png` | Cell 3496545, 8 components, Mean 17 | Yes, apart from "Gulf of Mexico" | No | The popup covers part of the Gulf data. Its "Primary producer" petal is v7's **primary productivity** (see app finding 8). | Fix the caption; optional retake |
| `desktop-table.png` | Species for cell 3496545 | Yes | No | Truncated names; acceptable | Optional retake |
| `desktop-report.png` | Report header, intro, top of map | No | No | **Test artefact:** the permalink **and the QR code** encode `http://localhost:4398/report.html?ver=v7#pl=z.pa.GAA&t=…`. The production MMA seal lockup is absent (the live build sets `VITE_SEAL=1` and `VITE_AGENCY=MMA`). It shows none of the flower, table, species or provenance that the caption lists. | **Retake on the live host** (a full page, or two shots) |
| `desktop-version-picker.png` | Data release list (v7b, v9, v8 restricted; v7 current; v6–v1 retired) | Yes | Chrome only | — | Keep, or retake with the rest |

The app shots are all in the dark theme, and the report's light theme is by design. No error toasts appear. For consistency, retake **all 12 in one pass** from `https://marinesensitivity.org/atlas/` at 0.10.44 or later.

---

## 5. Verdict and ordered edit list

**It can be merged into docs `main` after these edits, in this order:**

1. **Rewrite "How places are stored in the link"**, using #1–#8:
   - the budget is checked only in Places ▸ Share;
   - the ladder aims at 2,000 characters;
   - the user accepts the simplification ("Simplify and update the numbers") and it is never silent;
   - the fallback turns every drawn place into a `u.` token;
   - describe what the recipient actually sees;
   - use the real fixture token;
   - move the byte layout and msens material into a collapsed "For developers" note, or cut it.
2. **Separate the two Share buttons** (#9, #10). The top bar copies the current address; the Places Share opens the dialog.
3. **Fix Upload** (#25, #26). GeoPackage cannot be read today. Add the refusal rules (§2 item 2) and drop the source-code aside.
4. **Rewrite Feedback** (#53). **Send** is not configured; **Open as GitHub issue** makes the text public; a restricted release has no route.
5. **Fix the preview-host claims** (#15, #16) in the Overview callout and in Versions, unless atlas-9 is deployed first.
6. **Fix the top bar and phone description** for 0.10.44 (#11–#14), including that search works only in the Species lens.
7. **Fix Places** (#21–#24, #27–#29):
   - the exact empty-state text;
   - "Add to places", and that several picks make one place;
   - the click-move-click gesture and **Done**;
   - GEO, and bare keys in current releases;
   - list chips read "not analysed yet";
   - **Open in report**.

   Add the cap, Recent and Show analysis cells.
8. **Fix Scores, Species, Flower and Table** (#30–#42):
   - the Data-row switches;
   - the real stack row names and their constraints;
   - one representation toggle;
   - the v7 struck-through inputs stated as a limitation;
   - Flower and Table are for a cell or Program Area in the Scores lens;
   - the hub shows the plain mean;
   - ER code;
   - the Columns control is on the species table only;
   - CSV exists only for the species list;
   - "Gulf of America".
9. **Fix Report** (#43–#49):
   - it reports every listed place;
   - the map is interactive on screen;
   - add the Parameters section;
   - the 99 % footnote rule;
   - the SQL templates are parity-tested twins;
   - no promise of Reproduce in R yet;
   - the HTML flowers are SVG;
   - the ZIP holds `query/*.sql`.
10. **Overview and `apps.qmd` wording** (#17–#20): say "no application server of its own", and use the path form for Scores and Species.
11. **Cross-references**: L20 → `@sec-apps`, L298 → `@sec-workflows`, L332 → `@sec-releases`.
12. **Add `doc_atlas_url()`** to `libs/versioned.R`, use it in both files, and delete the L342–351 callout.
13. **Add sections**: a Quick start for desktop and phone (§2 item 1), Known limitations as of 0.10.44 (§2 item 10), and a sentence each for the Welcome dialog and tour, About, and Keyboard shortcuts.
14. **Accessibility and performance**: give the conformance claim as "partially supports" with no screen reader driven, correct the skip links, and date or cut the KB budgets.
15. **Retake the screenshots**:
    - all 12 in one pass from the live host at 0.10.44 or later;
    - none with `localhost`, none maximized, the legend visible;
    - Places with a place and its results;
    - a shot of the Share ladder;
    - crops for the panel figures;
    - phone shots at `width=40%`, or side by side;
    - `fig-alt` on every figure;
    - a "Generating Screenshots" section pointing to `scripts/eyes-shots.mjs`.
16. **Polish**:
    - "More (⋯)" for the PDF;
    - "(beta)" in the title and in `apps.qmd`, per atlas-9 B1;
    - trim the reviewer and developer voice;
    - bring the chapter back to about 200–250 lines.

---

## Appendix: app findings (atlas repo, outside this chapter)

These came up while verifying the chapter. They belong to the app, and some explain why the docs have to hedge.

1. **Search in the Scores lens does nothing.** The box is labelled "Search species and places" but is a bare input with no handler (`Shell.svelte:1196`, phone `:1686`). This is a trap for a first-time user in the default lens. Either wire place search, or switch to the Species lens when someone types.
2. **The GeoPackage refusal misleads.** With `runtime: null` (`UploadPanel.svelte:50`), every `.gpkg` gets `geopackageNoRuntime`, whose fix line says "Wait for the map's numbers to appear and drop the file again". That can never succeed. Either drop GeoPackage from the accepted-format line, or change the message to "not supported yet — convert to GeoJSON".
3. **The Places list chips never show numbers.** `zoneStatFromBoot` reads `composite`/`score` and `pct_covered`/`coverage` from the top level of `boot.zones` rows. v7 publishes the composite under `metrics.score_extriskspcat_primprod_ecoregionrescaled_equalweights`, and its `coverage` is null. Drawn places never get row figures at all (`Places.svelte:484–495`). Every row therefore says "not analysed yet", even after the results panel has computed its numbers.
4. **A row's "Open in report" ignores its row.** It adds `sel=place:i` (`Places.svelte:559–563`), but `report.html` decodes and reports every place (`Report.svelte:79–81`).
5. **The Places results "Export CSV" button is dead.** `DataTable.svelte:153–162` always renders it, and `ResultsPanel.svelte` passes no `onExport`.
6. **`shareUrl` and fragment encoding.** `formatSel` percent-encodes the `#pl` value (`~` → `%7E`, `%` → `%25`), but `shareUrl()` substring-replaces the **decoded** `pl=<old>` (`share.ts:57–58`). With several places, or a name containing a space, the replace silently misses. **"Copy link anyway"** in the `upload` case then copies the full original link instead of the `u.` tokens, and the displayed length is off by 2 characters per escaped character. The unit tests use only single places with plain names (`tests/places/share.test.ts`).
7. **The feedback endpoint is unset in production** (`pages.yml` has no `VITE_FEEDBACK_URL`). **Send** is disabled on the live app, and a restricted release has no working feedback route. This is known in `status.md` ("Ben … sets `VITE_FEEDBACK_URL`"), but the live UI shows it.
8. **v7's primary productivity is labelled "Primary producer".** `categories.ts:63`, `:91–92` fold `primprod` (an environmental metric) into the "Primary producer" slot, so v7's flower and legend call primary **productivity** a species category. This is a scientific label error on the default public release.
9. **The preview host does not serve the Atlas yet**, but the app links there. "Continue on the preview host" (`previewLink.ts:22–23`) leads to Cloudflare Access and then to the landing page's `file_server`. No `/{ver}/atlas/` route exists on server main `5ae8869`.
10. **"Reproduce in R" depends on unreleased msens.** The report prints calls to `msens::place_decode()`, `cells_in_polygon_grid()` and `scores_for_cells(blend=, denominator=)` (`provenance.ts:88–110`). These exist only on the local, unpushed msens branch `atlas-contract` (0.43.0).
11. **The coordinate dialog skips the study-area check.** `<CoordinateDialog>` is mounted without the `studyArea` hook (`Places.svelte:840`), whereas the upload path enforces "must touch the study area" (`UploadPanel.svelte` `finalize`).
12. **The Help ▸ Docs link could point at the Atlas chapter**, `…/docs/{ver}/apps/atlas.html`, once this chapter merges.
