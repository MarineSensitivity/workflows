Model: claude-opus-5-5[1m] (Opus 5.5)

# Atlas UI review, round 3: consistency and functionality (atlas `main` 0.10.67, 2026-09-25)

**Build under review:** a detached worktree at `main` 66e5678 (`atlas/.claude/worktrees/r3-review`). It was built with
`npm ci && npm run duckdb:fetch-ext && npm run build` and served by `vite preview` on port 4461. All shot paths below are
relative to `scratchpad/reports/review/`.

- **`shots/`**: `scripts/eyes-shots.mjs`, dark theme, 42 PNGs, with no WARN and no MISSED.
- **`shots-light/`**: the same harness with `theme=light`, 42 PNGs, driven by `harness/eyes-shots-light.mjs`.
- **`extra/`, `extra-light/`**: states the harness never shoots, driven by `harness/extra-shots.mjs` and
  `harness/probe*.mjs`. They cover the version picker, About, Help menu, Share, Feedback, search results, the Zones and
  Composition tabs, the glossary, the Columns menu, the coordinate dialog, Places with a Program Area and while drawing,
  the collapsed and docked-bottom panel, keyboard Tab order, species not-found, and the species-lens Table and Report.
- **`crops/`**: one brightened crop.

**Reference:** CalCOFI explore. I did not rebuild it: `vite preview` of its existing `dist/` came up on 4462, but its
data is remote. I read `src/style.css` and its committed `shots/prod/v2_*.png` instead.

**Already being built this round, so not re-listed:** A1–A3, B1–B17, the Layers-pane redesign and the Download menu. Where
a finding touches one of them, it is marked as a hand-off in §0.

---

## 0. Hand-offs to builders already in flight (not numbered findings)

- **A3, the palette re-hue.** "Fish → teal" collides with **Primary production**, which is already teal-green:
  `--cat-primprod` is `#4cc9a0` on navy and `#00765a` on paper (`src/lib/brand/tokens.css:206,271`). The phone flower
  shows the two side by side (`shots/phone-08-flower-full.png`). A3 also names only the light-theme pairs. In the dark
  theme, **Coral `#f08a3c` against Mammal `#f0b33c`** and **Bird `#7cc7f0` against Fish `#5aa9e6`** are the close pairs
  (`shots/desktop-06-flower-half.png`). Run a ΔE/CVD check across all eight categories in **both** themes. A
  blue-violet for Fish would sit clear of both Bird and Primary production.
- **The Layers redesign.** Four things to pick up:
  - The opacity `<input type=range>` has no `accent-color`, so it is browser-default bright blue on paper
    (`shots-light/desktop-03-layers-half.png`; see UI-10).
  - "Bathymetry — coming" ships a stub row to reviewers (`extra/desktop-y-species-notfound.png`).
  - In the Species lens, the Raster cells | Program areas control is shown disabled only to say "Species surfaces are
    rasters only" (`shots/desktop-17-species.png`). Hide it instead.
  - The row "Zone outlines" should say "Program Area outlines" (UI-5).
- **The Download menu.** The report's export bar and its CSV button use their own button styles (UI-6). If the Download
  menu is app-wide, the report should adopt it too.

---

## Do this round (S/M, clear wins)

### UI-1: Most user feedback is invisible. About 60 `announce()` messages go only to a screen-reader live region.
- **What.** Share copies the link and says "Link copied to your clipboard." Only a screen reader hears it: a sighted user
  sees nothing happen. The same holds for:
  - "Pick mode on. Click a Program Area to select it; Ctrl-click…"
  - "Place drawn. Drag its corners to adjust…"
  - every error: "The map isn't ready yet.", "Couldn't load the drawing tools…", "Feedback was not sent."

  `lib/ui` has a visible toast component, but the app never mounts it.
- **Where.**
  - `src/lib/ui/announcer.ts` feeds `src/lib/ui/Announcer.svelte`, which is visually hidden (`clip: rect(0,0,0,0)`).
  - `src/lib/ui/Toast.svelte` and `toastQueue.ts` are only used by the gallery.
  - The callers are `src/shell/Shell.svelte:281-288` (`onShare`) and the pick and draw messages in
    `src/places/Places.svelte:189-338`.
- **Evidence.**
  - `extra/desktop-x-share.png` and `extra/phone-x-share.png`: Share was clicked and nothing is visible.
  - `extra/desktop-y-pickmode.png`: Pick mode is on, and there is no instruction on screen.
- **Fix.** Mount one `<Toast>` in `Shell.svelte` and `report.html`. Add a `notify(text, {tone})` to `announcer.ts` that
  announces **and** enqueues a toast. Switch the user-initiated calls to it: share and copy, add, remove, download, pick
  on and off, draw hints, and every "couldn't…" error. Keep the plain `announce()` for chatty status such as row counts.
- **Effort:** M. **Touches:** `src/shell/` and `src/lib/ui/`, so run the shell e2e.

### UI-2: On the phone, the map attribution (© OpenStreetMap, © CARTO) is never visible
- **What.** On the phone the attribution sits at y=745. The sheet's top edge is at y=668 at the peek detent and y=376 at
  half, and `elementFromPoint` at the attribution returns the sheet at both detents (`harness/probe7.mjs`). The credit
  shows faintly *through* the translucent sheet. OSM's licence (ODbL) requires the credit to be visible.
- **Where.** `src/shell/shell.css:195-205`. It is positioned above the rail row, but the sheet sits on top of it.
- **Evidence.** `crops/phone-02-peek-bright.png` and `shots/phone-02-map.png`.
- **Fix.** Anchor it above the sheet with `--legend-chip-sheet-height`, the same variable the legend chip already uses
  (`shell.css:780`). Alternatively, on phone use a compact "ⓘ" button at the map's top-left that expands to the credits.
- **Effort:** S. **Touches:** `src/shell/`.

### UI-3: No shared Button. One panel shows four control shapes and five heights.
- **What.**
  - **Places alone.** Places has:
    - a 44 px `.pill` (Pick mode);
    - 26 px pill-shaped *actions* ("Add to places", "Add this Program Area");
    - 28 px 8 px-radius draw tools (Polygon, Rectangle, Circle, Enter coordinates), where only Polygon has an icon;
    - a dashed, struck-through `Pill` for a *transient* disabled state ("Show analysis cells" / "Select a drawn or
      uploaded place first");
    - 44 px footer buttons at 1 rem (B6 fixes only their font size).

    The coarse-pointer 44 px rule in `touch-targets.css` lists only the `lib/ui` classes, so the Places buttons stay
    26–28 px on touch.
  - **The report** adds three more styles: cream export buttons, a native "Parameters ▸" button and a native CSV
    button.
  - **Others:** Welcome, Table ("Columns", "Show table", "Report on selected") and Feedback each define their own
    `.btn`.
- **Where.**
  - `src/places/Places.svelte:1095-1145` (`.add-picked`, `.draw-tool`) and `:1255-1280` (`.places-footer`).
  - `src/lib/ui/touch-targets.css:18-44`.
  - `src/lens/scores/WelcomeModal.svelte:118-133`, `src/lib/feedback/FeedbackDialog.svelte:695-740`,
    `src/report/Report.svelte:989-1001`.
- **Evidence.** `shots/phone-11-places.png`, `shots/desktop-11-places.png`, `shots/phone-13-report-top.png`,
  `shots/phone-15-report-scrolled2.png`, `shots/phone-10-table-full.png`.
- **Fix.**
  1. Add `src/lib/ui/Button.svelte`:
     - `variant`: primary (gold), secondary (outlined), ghost or icon;
     - `size`: sm = 28 px, md = 36 px, 44 px under `pointer: coarse`;
     - an `icon` prop;
     - disabled = opacity 0.5 with `cursor: not-allowed`.
  2. State the rule in `docs/design`: **rectangles are actions; pills are toggles, filters, chips and segmented
     choices.** Keep the struck-through dashed pill **only** for "not available in this release".
  3. Migrate Places first, then Welcome, Table, Feedback and Report.
- **Effort:** M. **Touches:** `src/lib/ui/`, so run the shell e2e and update the gallery baselines.

### UI-4: The same cell or zone is named five ways
- **What.**
  - Map popup: "Cell 3350704 · lon -90.550, lat 28.601 · score: 44". It wraps, and prints the click point (B3).
  - Flower: "Cell ID: 3350704 (x: -90.575, y: 28.625)".
  - Table: "Species for Cell ID: 3350704".
  - Species popup: "Cell ID: … / Lon: … / Lat: …".
  - Zone popup: "GOA Program Area A (GAA): 33".

  On the **Zones** tab, the table header still reads "Species in Full study area", which is the Species tab's subject.
  On **Composition** the same line is printed twice.
- **Where.** `src/lens/scores/popup.ts:78,107`, `src/lens/scores/flower.ts:181-183`,
  `src/lens/scores/species.ts:33-38`, `src/lens/species/popup.ts:192`.
- **Evidence.**
  - `shots/phone-06-flower-half.png`, `shots/phone-09-table-half.png`, `shots/phone-19-programarea-popup.png`.
  - `extra/phone-x-table-zones.png` and `extra/desktop-y-composition.png`.
- **Fix.**
  1. One `formatSubject(selection)` in `src/lib/format.ts`, returning for example "Cell 3350704 · 28.625° N,
     90.575° W" or "GOA Program Area A (GAA)", with its own unit test. Use it in the popup, flower, table and species
     popup, with score shown as "Score 44" everywhere.
  2. Give each tab its own caption: Species "Species in …", Zones "Program Areas ranked by score", Composition with
     no second copy.
  3. Coordinate with the B3 builder, who owns the coordinates.
- **Effort:** S/M. **Touches:** neither.

### UI-5: One concept, several names: study area, Program Area, zone
- **What.**
  - The Layers select calls the whole area "All US waters". The Flower, Table and Composition call the same thing
    "Full study area", capitalised mid-sentence as "Species in Full study area".
  - The spatial unit appears as "Program Areas" in most copy and "Program areas" in the segmented control, which is the
    bundle's unit label.
  - Internal "zone" leaks into the UI as the "Zones" tab, the "Zone" column and the "Zone outlines" layer. A BOEM
    reviewer knows these as Program Areas.
- **Where.**
  - `src/lens/scores/flower.ts:181` and `src/lens/scores/species.ts:38` against `src/lib/map/interaction.ts:40`.
  - `src/lib/map/layerStack.ts:74` and the ZonesTable column header.
  - The segmented label comes from `boot` (`lens/scores/boot.ts:61`).
- **Evidence.** `shots/phone-03-layers-half.png`, `extra/phone-x-flower-nosel.png`, `extra/phone-x-table-zones.png`.
- **Fix.**
  - Use the Study-area select's current label as the no-selection subject ("All US waters"). This also stays right when
    a sub-area is chosen.
  - Use the published unit label in title form, "Program Areas", for the tab, column and layer.
  - Title-case the segmented label with the same helper that `categoryLabel()` uses.
- **Effort:** S. **Touches:** neither. Grep `e2e/` for the old strings.

### UI-6: The report page's controls are unstyled or off-brand
- **What.**
  - **Disclosures.** "Parameters ▸" and "Provenance ▸" render as grey native buttons. `report.css:281` targets
    `.disclosure > button`, but the button sits inside the heading (`section.disclosure > h2 > button`), so the rule
    never matches.
  - **CSV button.** "Download full species list (CSV — 5,861 species)" is also a native button.
  - **Links.** Links are browser-default `#0000EE` blue instead of `--text-link`.
  - **Export bar.** The export bar is a fourth, cream style, and it sits *above* the brand mark and title, so a phone
    reader opens onto four buttons and "Done — 1 place.".
- **Where.** `src/report/report.css:281-290` and `src/report/Report.svelte:477-481,545-553,895`. The screen `a` rule is
  missing; `report.css:492` covers print only.
- **Evidence.** `shots/desktop-13-report-top.png`, `shots/desktop-15-report-scrolled2.png`,
  `shots/phone-13-report-top.png`.
- **Fix.**
  - Change the selector to `.disclosure h2 > button { font: inherit; background: none; border: 0 }`, so it reads as the
    h2 it is.
  - Add a screen rule `a { color: var(--text-link) }`.
  - Style every report button with the UI-3 Button tokens.
  - Move the export bar under the header, or make it a compact sticky bar.
  - Change the status line to "Report ready — 1 place".
- **Effort:** S. **Touches:** neither.

### UI-7: "Open this release in the Atlas" drops the report's places
- **What.** The report knows its places, since the `#pl=` hash is in its own permalink. The link back uses
  `appHref(ver)` with no patch, so the reader lands on an empty Atlas. The empty state ("No places in this link —
  nothing to report on.") has no way back at all.
- **Where.** `src/lib/report/model.ts:456,820` and `src/report/Report.svelte:976-978`.
- **Evidence.** `shots/desktop-13-report-top.png`.
- **Fix.**
  - Pass the same place token: `appHref(ver, { pl: … })`. The app then opens with those places loaded in Places, and
    the tool is Places.
  - Add an "Open the Atlas" link to the empty state.
- **Effort:** S. **Touches:** neither.

### UI-8: Numbers are formatted inconsistently, and the Composition caption has a typo
- **What.**
  - **Zones table.** It prints "29" beside "29.7" and "5" beside "9.9", because `maximumFractionDigits: 1` has no
    minimum.
  - **Flower.** The Mean row reads "44" while the rows average 44.3. The "Score" header is not right-aligned over the
    right-aligned numbers.
  - **Legends.** Cell legends are integers (0 … 93); the Program Area legend has 1 decimal place (7.5 … 52.7).
  - **Composition.** The caption says "**16,153 n species** across 7 categories" (a raw value label).
  - **ER score in the report.** The top-20 table prints "100%". Its counts header prints `USA:EN(100)`, and its prose
    says "the extinction-risk score (1–100)". Three forms of one number on one page.
- **Where.**
  - `src/lens/scores/ZonesTable.svelte:53-55`.
  - `src/lib/ui/Flower.svelte`, for the mean row and the `th` alignment.
  - `src/lens/scores/Composition.svelte:67` (`valueLabel="n species"`).
  - The report top-20 formatter in `src/lib/report/model.ts`.
- **Evidence.** `extra/desktop-y-zones.png`, `shots/phone-08-flower-full.png`, `shots/desktop-19-programarea-popup.png`,
  `extra/desktop-y-composition.png`, `shots/desktop-14-report-scrolled.png`.
- **Fix.**
  - Add `minimumFractionDigits: 1` to the Zones table.
  - Show the Mean to 1 decimal place, the same as its rows.
  - Set `th.num { text-align: right }`.
  - Change `valueLabel` to `"species"`.
  - In the report, print the ER score as the same "(100)" form its own prose defines. The app table's "%" is a
    documented Shiny-parity reversal, so leave it (see UI-L7).
- **Effort:** S. **Touches:** `src/lib/ui/` (Flower), so update the gallery baselines.

### UI-9: Species-lens copy that a NOAA reviewer will stop at
- **What.**
  - **Species card.**
    - It says "**ESA Listing: FWS:LC**", but LC is not an ESA status; the species is simply not listed.
    - "ESA Listing: NMFS:EN (NMFS)" names the source twice.
    - "IUCN RedList: VU" should read "IUCN Red List: Vulnerable (VU)".
  - **Species-lens Table.** It shows raw dataset keys ("ms_merge", "am_0.05", "rng_iucn") and a "Representation" of
    "native". It has no subject line naming the species.
  - **Not-found state.** It prints the raw code ("Couldn't load this species (not-found).") and offers no next step.
- **Where.** `src/lens/species/data/card.ts:64-120`, `src/lens/species/SpeciesInputsTable.svelte` and
  `src/lens/species/data/inputsTable.ts`, and the not-found text in `SpeciesCardView.svelte` / `NotFoundModal.svelte`.
- **Evidence.** `shots/desktop-18-species-model.png`, `extra/desktop-x-species-table2.png`,
  `extra/desktop-y-species-notfound.png`.
- **Fix.**
  - Map the codes for display: EN → Endangered, TN → Threatened, LC → "Not listed"; keep the code in parentheses.
  - Label it "Listed under the ESA" with the source once.
  - Change the IUCN label to "IUCN Red List" and show the category name.
  - Show dataset names from `datasets.json`, not keys.
  - Title the table "Inputs for *Odobenus rosmarus* (Walrus)".
  - Not-found: "This species isn't in release v7. Search for another above."
- **Effort:** S. **Touches:** neither. Grep `e2e/species.*` for the old strings.

### UI-10: Controls that only look right in one theme
- **What.**
  - **No `accent-color` anywhere in `src/`.** Every native range and checkbox therefore uses the browser default:
    - on paper, the Layers opacity sliders are bright system blue;
    - in both themes, the Columns menu checkboxes, the "Only species in US waters" box and the welcome "Don't show this
      again" box are system blue.
  - **Gold check mark on paper.** The merged-model "✓" in the species card uses `--fill-accent` (gold) as a **text**
    colour. Gold on paper is about 1.5:1, which tokens.css itself forbids.
  - **Switch ON colour.** The quiet Switch reads ON as lavender on navy and olive-brown on paper. Neither is the gold
    "selected" colour that every other on-state uses.
- **Where.** `src/lib/brand/tokens.css` (there is no `:root { accent-color }`), `src/lens/species/LayerBarView.svelte:143`,
  `src/lib/ui/Switch.svelte:73-84`.
- **Evidence.** `shots-light/desktop-03-layers-half.png`, `shots-light/desktop-17-species.png`,
  `extra/phone-x-columns-menu.png`, `extra/desktop-x-search-species.png`.
- **Fix.**
  - Add `:root { accent-color: var(--fill-accent) }`, with a steel value on paper, the same pair as `--focus-ring`.
  - Change `.layer-bar.is-merged .layer-mark` to `color: var(--text-accent)`.
  - Decide whether the quiet switch keeps a neutral ON. If it does, give its knob the accent colour so ON reads the same
    in both themes.
- **Effort:** S. **Touches:** `src/lib/ui/` (Switch). Run the contrast gate `scripts/contrast.mjs`.

### UI-11: Icon-only buttons with no tooltip, and three tooltip mechanisms
- **What.**
  - **Only the top bar has hover tooltips** (`.tool[data-tooltip]`, a CSS-only mechanism). The following have none:
    - the panel's Dock left / Dock bottom / Dock right / Full screen / Collapse buttons (five icons);
    - the phone sheet's three detent buttons;
    - the Table's ⓘ and ⬇;
    - the popup ×.
  - **The species copy buttons** use native `title` in lowercase ("copy scientific name").
  - **About.** Its tooltip reads "Info" while its label is "About this release".
  - **Help.** The tooltip paints over the open Help menu.
- **Where.**
  - `src/lib/ui/Panel.svelte:334-380`, `src/lib/ui/Sheet.svelte:98-124`, `src/lens/scores/TablePanel.svelte:229-237`.
  - `src/shell/shell.css:434-460`, `src/shell/TopBarActions.svelte:233`, `src/lens/species/SpeciesTitle.svelte:59,75`.
- **Evidence.** `shots/desktop-03-layers-half.png`, `extra/desktop-x-probe-datacontrolhelp.png`.
- **Fix.**
  1. Move the `data-tooltip` rule into `src/lib/ui/` as a global utility, and give every icon-only button
     `data-tooltip` equal to its `aria-label`, in Sentence case.
  2. Hide the tooltip while `[aria-expanded="true"]`.
  3. Change the About tooltip to "About this release".
- **Effort:** S. **Touches:** `src/shell/` and `src/lib/ui/`.

### UI-12: The glossary shows column keys, and a header breaks mid-word
- **What.**
  - The "Species table columns" glossary lists `cat`, `er_code`, `is_mmpa`, `avg_suit`, `pct_cat`. The table itself
    shows "Category", "ER code", "MMPA", "Avg. suitability", "% of category".
  - On the Zones table, the header "Primary production" breaks as "Primary producti / on" on both viewports.
- **Where.** `src/lens/scores/glossary.ts:15-40` with `GlossaryModal.svelte`, and `src/lens/scores/ZonesTable.svelte:230-231`
  (`overflow-wrap: break-word` on `th`).
- **Evidence.** `extra/phone-x-glossary.png`, `extra/phone-x-table-zones.png`, `extra/desktop-y-zones.png`.
- **Fix.**
  - Make the displayed header the glossary term, with the key small and in monospace after it.
  - Use `overflow-wrap: normal; hyphens: auto` on `th`, or a short header ("Prim. prod.") with a `title`.
- **Effort:** S. **Touches:** neither.

### UI-13: Scientific names are italic in some places and not others
- **What.**
  - **Italic:** the species card, the species popup and the report tables.
  - **Roman (upright):**
    - the Scores Species table (Scientific name column);
    - the species search results;
    - the legend card and legend chip titles ("Dermochelys coriacea");
    - the report's summary lines.
- **Where.** `src/lens/scores/SpeciesTable.svelte` / `speciesTableColumns.ts`, `src/lens/species/SpeciesPicker.svelte`,
  `src/lens/species/SpeciesLegend.svelte:32,51`, and the LegendChip title.
- **Evidence.** `shots/desktop-10-table-full.png`, `extra/desktop-x-search-species.png`, `shots/phone-17-species.png`.
- **Fix.** Add one `<SciName>` component or `.sci { font-style: italic }` class and use it wherever a binomial is
  printed.
- **Effort:** S. **Touches:** `src/lib/ui/` if LegendChip is changed.

### UI-14: Species search still buries the obvious match (round-2 usability m5, still open)
- **What.** "humpback" still puts the humpback whale 6th of 18. All 18 tie on the common-name prefix and fall back to
  alphabetical order by scientific name.
  - **Desktop:** the dropdown is 190 px, narrower than the 240 px input, so every result wraps to two lines.
  - **Options:** each reads "fish: Centropomus unionensis (humpback snook)", with a lowercase category and no italics.
- **Where.** `src/lens/species/data/picker.ts:317-321` and `src/lens/species/SpeciesPicker.svelte`.
- **Evidence.** `extra/desktop-x-search-species.png`, `extra/phone-x-search-species.png`.
- **Fix.**
  - Give the dropdown `min-width: max(100%, 360px)`.
  - Make each option two lines: the **common name** on top, and *scientific name* · category chip below, as in CalCOFI's
    picker.
  - Tie-break within a tier by an exact whole-word common-name match, then by a "listed" flag. The flag needs a bit in
    `taxa.json`, which is msens work (a "later" item). Until it exists, fall back to category priority
    (mammal, turtle, bird, then the rest).
- **Effort:** S/M. **Touches:** neither.

### UI-15: The welcome modal's links, and one name for the product
- **What.**
  - **"Species lens" link.** It is a relative `?lens=species` link with `target=_blank`. It opens a **new tab**, drops
    the current `?ver=` (a reader on a retired version is sent to latest), and does not switch lens in place.
  - **"The project documentation"** is plain text, not a link.
  - **"Take a Tour"** is title case in the modal; the Help and ⋯ menus say "Take a tour".
  - **Three names for the product** in reader-facing copy:
    - "the published marine-atlas release" (welcome);
    - "one published release of the marine-atlas" (version picker);
    - "immutable release v7 of the MarineSensitivity marine atlas" (report).
- **Where.** `src/lens/scores/WelcomeModal.svelte:84-104`, `src/lens/scores/VersionPickerModal.svelte:74`,
  `src/lib/report/model.ts:537`.
- **Evidence.** `shots/desktop-01-welcome.png`, `extra/desktop-x-probe-datacontrolversionchip.png`,
  `shots/desktop-13-report-top.png`.
- **Fix.**
  - Make "Species lens" a button that calls the lens switch and closes the modal.
  - Link the docs to this version's docs URL, the one the report already builds.
  - Change "Take a Tour" to "Take a tour".
  - Standardize on "Marine Sensitivity Atlas" for the app and "data release v7" for the data. Drop "immutable" and
    "marine-atlas" from reader copy.
- **Effort:** S. **Touches:** neither. The modal is mounted by the shell, so grep `e2e/` for "Take a Tour".

### UI-16: The About modal, and the two top-bar menus
- **What.**
  - **Release line.** It renders "**v7· 2026-06-12**" with no space before the dot, because Svelte drops the leading
    whitespace inside `{#if}`.
  - **Links.** "What changed" links the app's CHANGELOG, not the data release notes. "Documentation" is the generic
    docs root, while the report links the versioned docs.
  - **Restricted note.** It uses "--" where an em dash is meant.
  - **Menus.**
    - The phone ⋯ menu has icon + label rows. The desktop Help menu has full-width outlined buttons with no icons.
    - The phone menu keeps "Report" although desktop dropped it as a duplicate of the rail (owner review item 3).
    - The Help menu lists "= — zoom the map in" but no zoom-out key.
- **Where.** `src/shell/TopBarActions.svelte:294-320` and `src/shell/Shell.svelte` (the `help-menu` block).
- **Evidence.** `extra/phone-x-about.png`, `extra/desktop-x-probe-datacontrolhelp.png`, `shots/phone-16-more-menu.png`.
- **Fix.**
  - Write the separator as `{" · "}`.
  - Point "What changed" at the release notes (the docs `release_notes` page for `{ver}`), and link "Documentation" to
    `/docs/{ver}/`.
  - Replace "--" with an em dash.
  - Render both menus with one `MenuItem` (icon, label, optional small caption, as in CalCOFI's `.menu-item`).
  - Either drop Report from ⋯ or restore it on desktop, so the two match.
- **Effort:** S. **Touches:** `src/shell/`.

### UI-17: The search hint stays open after focus leaves
- **What.** Tabbing through the Scores search box leaves its hint listbox ("Type a Program Area name or key, or
  coordinates like '-140, 57'") open over the map while focus moves on to Share and beyond.
- **Where.** `src/lens/scores/ScoresSearch.svelte`.
- **Evidence.** `extra/desktop-y-kbd7.png` and `extra/desktop-y-kbd12.png`.
- **Fix.** Close the list on `focusout` when the next focus target is outside the combobox. Keep it open on Escape-less
  pointer use.
- **Effort:** S. **Touches:** neither. The component is mounted in the shell, so run the shell e2e anyway.

### UI-18: The version picker on the phone
- **What.**
  - Dates wrap mid-value ("2026-" on one line, "09-20" on the next), and "on preview" wraps into two lines.
  - The prerelease rows are ordered v7b, v9, v8 (by date), which reads as unsorted.
  - The status pills are all lowercase (prerelease, restricted, current, retired).
- **Where.** `src/lens/scores/VersionPickerModal.svelte`.
- **Evidence.** `extra/phone-x-version-picker.png`.
- **Fix.**
  - Set `white-space: nowrap` on dates, and stack the row on the phone: version and status on one line, links and date
    on the next.
  - Group the rows under "Under review", "Current" and "Earlier releases".
  - Sort by version number within each group.
- **Effort:** S. **Touches:** neither.

### UI-19: At the peek detent, the sheet's collapse button stays and does nothing
- **What.** At peek, "Collapse to a peek" (⌄⌄) is still shown and is a no-op. None of the three detent buttons is shown
  as pressed there, although half and full each frame the current detent.
- **Where.** `src/lib/ui/Sheet.svelte:98-124`.
- **Evidence.** `shots/phone-02-map.png` against `shots/phone-03-layers-half.png`.
- **Fix.** At peek, swap that button to "Expand to half" (⌃⌃). Alternatively, render the three as one segmented control
  with `aria-pressed` on the current detent.
- **Effort:** S. **Touches:** `src/lib/ui/`.

### UI-20: Places drawing: the controls jump, and there is no visible instruction
- **What.**
  - Choosing Polygon inserts "Done" in the middle of the draw row, which pushes "Enter coordinates" onto a new line.
  - Nothing on screen says how to draw. The instruction exists only as an announcement (see UI-1).
  - Two "add" paths sit one above the other: "Add to places" for pick mode and "Add this Program Area" for the select.
- **Where.** `src/places/Places.svelte` (the `.draw-bar` block, around lines 860-900).
- **Evidence.** `extra/phone-x-places-drawing.png` against `shots/phone-11-places.png`.
- **Fix.**
  - While drawing, replace the draw row with a single status line: "Drawing a polygon — tap corners, then Done" with
    [Done] [Cancel].
  - Move "Add to places" into the pick-mode status, for example "2 selected · Add to places".
- **Effort:** S/M. **Touches:** neither.

### UI-21: The basemap labels water in the local language ("Golfo de México")
- **What.** The Program Area is "GOA Program Area A (GAA)", in the Gulf of America (A4: official wording kept), but the
  CARTO basemap labels the water "Golfo de México". A BOEM reviewer will notice.
- **Where.** `src/lib/map/style.ts` (`composeStyle()`) with `src/lib/map/layers/basemap.ts`. CARTO's `water_name` and
  `place` layers use the local `name`.
- **Evidence.** `shots/desktop-19-programarea-popup.png` and `shots/desktop-21-programarea-table.png`.
- **Fix.** In `composeStyle()`, rewrite the CARTO symbol layers' `text-field` to `["coalesce", ["get","name_en"],
  ["get","name"]]`. Then either hide the `water_name` source-layer or override the one Gulf label; that choice is Ben's.
- **Effort:** S. **Touches:** neither. The map-style e2e asserts rendered features, so re-run it.

### UI-22: Process: the eyes-on harness never shoots light theme, modals or menus
- **What.** `scripts/eyes-shots.mjs` hard-codes `theme=dark` and `colorScheme: "dark"`, and none of its 21 states opens
  a modal or menu. That is how UI-1, UI-10, UI-12, UI-16 and UI-18 went unseen through five eyes-on reviews.
- **Fix.**
  - Add `THEME=dark|light|both` to the harness.
  - Fold in the states from `harness/extra-shots.mjs`: version picker, About, Help menu, Feedback, search results, the
    Zones and Composition tabs, glossary, Columns menu, coordinate dialog, Places while drawing, species not-found, and
    the species Table and Report.
  - Wait for the rail to become actionable before clicking. In this environment a fresh load kept the main thread busy
    for about 20 s (`harness/probe5.mjs`), and plain 8 s click timeouts miss.
- **Effort:** S. **Touches:** neither.

---

## Later (L, or a product decision)

### UI-L1: No map controls on desktop: zoom, "fit US waters" or a scale bar
MapLibre's controls were tried and reverted: they sit inside `#map`, which has `role="img"`, and that trips axe's
`nested-interactive` rule (`src/lens/scores/ScoresLens.svelte:167-176`). The only ways to zoom are the wheel, a pinch or
the `=` key, and after panning away the only way home is a reload.
- **Suggestion.** Render a small Svelte control group as a **sibling** of `#map`, top-right of the stage as CalCOFI does:
  zoom +/−, "Fit US waters" (A1(c)'s affordance, generalised to both lenses) and a scale bar. Every fit goes through
  `chromePadding.ts`.
- **Evidence.** `shots/desktop-02-map.png` against CalCOFI `shots/prod/v2_default_light.png`.
- **Effort:** M. **Touches:** `src/shell/`.

### UI-L2: A title-sentence legend card
The legend card now says only "score" (B1) or a bare scientific name, over a "0 … 93" ramp.
- **CalCOFI's pattern** is one sentence that says exactly what the map shows, above the ramp.
- **Atlas equivalents:**
  - "Overall score · rescaled 0–100 within each ecoregion · All US waters · v7";
  - "Walrus (*Odobenus rosmarus*) · habitat suitability 1–100 · merged model".
- **Why it helps.** It removes the question of why the ramp ends at 93 when scores run 0–100, and names what the
  species ramp measures.
- **Evidence.** `shots/desktop-02-map.png`, `shots/desktop-17-species.png`, CalCOFI `shots/prod/v2_dark.png`.
- **Effort:** M. **Touches:** `src/lib/ui/` (Legend).

### UI-L3: The report's typography does not match the app
`report.css:1-8` deliberately skips the self-hosted Jost and Carlito fonts to stay within budget. As a result, the
document BOEM will print and forward uses Helvetica/Arial headings and Title-Case section names ("Summary of Species",
"Plot of Scores", "Sources and Method"), while the app is Jost in Sentence case.
- **Suggestion.** Self-host one Jost weight for h1 and h2 (about 20 KB), and use Sentence-case headings. Rename "Plot of
  Scores" to "Flower plot", the app's name for it.
- **Cost.** This is a budget call for Ben.

### UI-L4: What the Species lens's Table and Report mean
In the Species lens:
- **Report** opens the scores place chooser ("Pick a Program Area…"), not a species report.
- **Table** is the model-inputs table, with no download and no glossary.
- **Flower** is disabled.

A user in the Species lens pressing Report will expect a species report: its range in US waters, its listing, and its
top Program Areas. Decide the scope, or relabel the tools for that lens.
- **Evidence.** `extra/desktop-x-species-report2.png` and `extra/desktop-x-species-table2.png`.

### UI-L5: Layouts for the panel's other docks and full screen
- **Dock bottom.** The flower does not adapt: it is clipped and its table falls out of view. The legend card jumps to
  the middle of the right edge (`extra/desktop-y-dock-bottom-flower.png`).
- **Full screen.** Flower and Table keep the narrow single column, so the component table is below the fold on a
  1280×800 screen (`shots/desktop-08-flower-full.png`).
- **Suggestion.** When the panel is wider than about 700 px and shorter than about 420 px, put the flower and its table
  side by side.
- **Effort:** M. **Touches:** `src/lib/ui/`.

### UI-L6: Write down and enforce the casing and naming rules
Adopt **Sentence case** for every label, heading, button and status pill, with proper nouns keeping their capitals
(Program Area, AquaMaps, Species lens). Today all three casings coexist:
- **Title Case:** "Take a Tour", "Merged Model", the report headings.
- **Sentence case:** "Take a tour", "Flower plot", "Add to places".
- **lowercase:** "score", "fish:", and the status pills "prerelease", "restricted", "retired".

Add a short `docs/design/copy.md` with the glossary of preferred terms (UI-5, UI-15), plus a lint test that flags
Title-Case strings in `.svelte` text nodes.

### UI-L7: Report species section: two meanings of "Other", and an unexplained score
- **"Other" twice.** "Other" is a species **category** (a row) and also an extinction-risk **bucket** ('The "other"
  category includes IUCN:DD…').
- **"Category" twice.** "5,861 species across 7 categories and 8 extinction-risk categories" uses the one word for both.
- **The score.** The top-20 "Score" column prints 389,982 with no unit.
- **ER score as a percentage in the app table.** It is a documented Shiny-parity reversal (`lens/scores/species.ts:88-93`),
  but it disagrees with the report's own "(1–100)" wording.

These wordings belong to the method, so settle them with Tim. Suggested wording: "Not listed / data deficient" for the
bucket, "risk classes" for the ER categories, and a score header of "Weighted score (suitability × ER × km²)".
- **Evidence.** `shots/desktop-14-report-scrolled.png`, `shots/phone-14-report-scrolled.png`,
  `shots/desktop-15-report-scrolled2.png`.

### UI-L8: Keyboard order and the map's focus ring
The Tab order runs: top bar → **map canvas → MapLibre attribution link** → rail → panel. The rail is visually first,
but it comes after the map. When the canvas has focus, its 2 px gold outline is clipped by its container, so focus seems
to disappear (`harness/probe6.mjs`, `extra/desktop-y-kbd12.png`). Order the DOM (or `tabindex`) as rail → panel → map,
and draw the map's focus ring as an inset `box-shadow` on its wrapper.
- **Effort:** S/M. **Touches:** `src/shell/`.

### UI-L9: The data-side label and name gaps behind the UI
These are all msens or bundle work:
- **Common names** mix Title Case and sentence case ("Great hammerhead shark" beside "Scalloped Hammerhead Shark").
- **`IUCN:TN`** still appears on v7 rows. The code is not an IUCN category (see the memory note on extrisk coding).
- **`taxa.json`** needs a "listed" bit for the UI-14 ranking.

---

## Overall consistency state (five lines)
1. The **token layer is healthy**. About 90 % of font sizes and about 88 % of radii come from tokens (166 of 185 and 118 of 134), and a contrast gate exists.
   The inconsistency lives one level up, in **components that each re-implement buttons, tooltips, subjects and number
   formats** (UI-3, UI-4, UI-8, UI-11).
2. **Functionally, the largest gap is silent feedback.** About 60 messages reach only screen readers, and the phone
   attribution is always hidden (UI-1, UI-2). Both are S/M, and both are in the shell.
3. **The copy has three names for the product, three for the study area and two for Program Areas**, and in the species
   card it shows raw status codes that a NOAA reviewer will misread (UI-5, UI-9, UI-15).
4. **Dark and light are close to parity.** The gaps are native form controls without an `accent-color` and one gold
   glyph on paper (UI-10). The report is its own island, with system fonts, native buttons and default links (UI-6,
   UI-L3).
5. **The eyes-on harness is the reason these survived.** It has never shot light theme, a modal or a menu, so extend it
   (UI-22) before round 4.
