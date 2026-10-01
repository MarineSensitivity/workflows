# W7 — Consistency slice B: copy, lenses, report (reserved version 0.10.74)
Worktree `r3-w7`, branch `r3-w7-consistency-copy`, ports 4481–4489. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w7`.
Cut your worktree from CURRENT `main` (Layers pane, Download, nits and cameras slices are merged — read `CHANGELOG.md`'s
top entries first; B3 already made the popup and panel print the cell centre through one helper — build UI-4 on it).

Ben asked (2026-09-25) for a UI review and "any other improvements for consistency sake and functionality". The Opus
5.5 review is `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/review.md` (read it in full, screenshots beside it). Implement these findings — each
has What/Where/Evidence/Fix there:

- **UI-4** one `formatSubject(selection)` in `src/lib/format.ts` (unit-tested) used by the map popup, flower, table
  and species popup: "Cell 3350704 · 28.625° N, 90.575° W" / "GOA Program Area A (GAA)", score as "Score 44"
  everywhere; per-tab captions (Species "Species in …", Zones "Program Areas ranked by score", Composition no
  duplicate line).
- **UI-5** naming: the Zoom-to-region select's current label is the no-selection subject ("All US waters"); the
  published unit label in title form "Program Areas" for the tab, column, layer row (the Layers slice already renamed
  the row "Outlines" — leave that); title-case the segmented label with the same helper `categoryLabel()` uses. Grep
  `e2e/` and `tests/` for the old strings.
- **UI-6** report controls: `.disclosure h2 > button` styled as the h2 it is; screen `a { color: var(--text-link) }`;
  the export bar under the header (compact); status line "Report ready — 1 place". (Buttons themselves are migrated to
  `Button.svelte` by slice A if it merged before you — check `src/lib/ui/Button.svelte` exists; if not, leave button
  styling alone and only fix the selector/links/order.)
- **UI-7** "Open this release in the Atlas" carries the report's place token (`appHref(ver, {pl})`), and the empty
  state gets an "Open the Atlas" link.
- **UI-8** numbers: Zones table `minimumFractionDigits: 1`; flower Mean to 1 decimal like its rows; `th.num` right
  aligned; Composition `valueLabel` "species"; report ER score printed in the "(100)" form its prose defines.
- **UI-9** species copy: ESA codes mapped for display (EN → Endangered, TN → Threatened, LC → "Not listed", code in
  parentheses), "Listed under the ESA" with the source once, "IUCN Red List: Vulnerable (VU)", dataset NAMES not keys in
  the species-lens table with a subject line naming the species, not-found → "This species isn't in release v7. Search
  for another above." Grep `e2e/species.*`.
- **UI-12** glossary shows the displayed header term with the key small/monospace after it; Zones table `th`
  `overflow-wrap: normal; hyphens: auto` (or a short header + title).
- **UI-14** species search: dropdown `min-width: max(100%, 360px)`; two-line options (common name; *scientific* ·
  category chip); tie-break within a tier by exact whole-word common-name match, then category priority (mammal,
  turtle, bird, then the rest). Unit test: "humpback" ranks the humpback whale first in the fixture.
- **UI-15** welcome modal: "Species lens" switches lens in place and closes; the docs link is this version's docs URL;
  "Take a tour" casing; product naming "Marine Sensitivity Atlas" / "data release v7" (drop "immutable" and
  "marine-atlas" from reader copy: welcome, version picker, report).
- **UI-18** version picker on the phone: dates `nowrap`, stacked rows, groups "Under review" / "Current" / "Earlier
  releases", sorted by version number within each.
- **UI-20** Places drawing: while drawing, the draw row becomes one status line "Drawing a polygon — tap corners, then
  Done" with [Done] [Cancel]; "Add to places" moves into the pick-mode status ("2 selected · Add to places").
- **UI-21** basemap labels in English: rewrite the CARTO symbol layers' `text-field` in `composeStyle()` to
  `["coalesce", ["get","name_en"], ["get","name"]]` (do not hide `water_name`); the map-style e2e re-run.

Every copy change: grep `e2e/` + `tests/` for the old wording. Eyes-on: `flower`, `table` (all three tabs), `species`,
`species-notfound`, `welcome`, `version-picker`, `places-drawing`, `report` scrolled, `programarea` — both viewports.
Seeded fault: e.g. `formatSubject()` printing the click point, or the search tie-break dropped.
- ALSO (W3 hand-off): ZonesTable.svelte and Composition.svelte still carry the old `max-height: 50vh` blank-space defect that B9 fixed for SpeciesTable — apply the same fill-the-panel fix.

## Ben's ask (2026-09-25): one consistent map-click popup across layers, colour-coded, with a distribution sparkline
Verbatim: "make the map click popup more consistent across layers (scores raster or program area, species raster/vector).
i like the color coding in the species popup, which could be added to scores layers. if its not too much trouble, it
would be really awesome to have a sparkline style histogram showing the range of values and a vertical line where that
given clicked element exists wrt to the full distribution actually in the popup". Do it as part of UI-4:
1. **One popup component** (`src/lib/map/ClickPopup.svelte` or a pure builder in `src/lib/map/popup.ts` + one template)
   used by the scores cell popup, the Program-Area popup and the species popup: subject line from `formatSubject()`,
   then a value row "Score 44" / "Suitability 71" with a **colour swatch** of the ramp colour at that value (the species
   popup's existing colour-coding — `src/lens/species/popup.ts` — generalised; the swatch comes from
   `src/lib/raster/ramps.ts`, never a second ramp), the layer/unit name in small text, and the × close. Same paddings,
   same min-width, same typography on every lens.
2. **Distribution sparkline — a smooth DENSITY curve, not individual bars** (Ben's clarification): a ~120×28 px inline SVG
   area path (a kernel-smoothed density over ~40 bins, drawn as one closed `<path>` FILLED WITH THE LEGEND'S OWN COLOUR RAMP — an SVG `<linearGradient>` built from the
   same `boot.palettes` stops the legend draws (`src/lib/raster/ramps.ts`, x = value), so the curve reads as the legend
   with height = density — plus a thin 1 px stroke in `--text-secondary`; no axes, the min and max as tiny end labels) of the CURRENT layer's values over the study area with a 1.5 px accent vertical
   line at the clicked value. Sources, by lens/unit — each behind ONE small interface (`distributionFor(layerContext)`)
   with an in-memory cache keyed by (ver, lens, layer/unit/model):
   - scores, Raster cells: a DuckDB-WASM histogram query over the release's cell tiles (`src/lib/engine/`; the same
     tables the zonal statistics read — Numbers-from-Parquet rule, never tile pixels);
   - scores, Program areas: the unit's `zone_metric` values already in the bundle (`boot`/`zone_taxon`), one bin per
     value range, the clicked zone's value marked;
   - species (raster surfaces): titiler's `GET /cog/statistics?url=…&histogram_bins=20` — the ONLY sanctioned tile-server
     number path is the species click value via `/cog/point` behind an interface (CLAUDE.md); put this behind the SAME
     interface (`src/lib/raster/point.ts`'s neighbour), document it as the second sanctioned display-only use, and
     fall back to no sparkline (never an error) when the endpoint is unavailable; a species vector input (range
     polygon, no values) shows no sparkline.
   The sparkline must not delay the popup: render the popup immediately, then fill the sparkline when its promise
   resolves (skeleton line meanwhile). Unit tests: the binning (edge values, NaN, all-equal), the marker position, the
   builder's output for each lens; chromium e2e: a scored-cell tap shows the swatch + the sparkline `<svg>` with a
   marker inside the plotted range. Seeded fault: the marker placed at the wrong bin.

## Ben's ask (2026-09-25): the Legend names the layer being displayed (= review UI-L2, now in scope)
"Given the different toolbar selections and data layer options, we should probably update the Legend to mention which
layer is being displayed." One pure builder `legendTitle(context)` (`src/lib/map/legendTitle.ts`, unit-tested) used by
the desktop legend card (`Legend.svelte`/`ScoresLegend`/`SpeciesLegend`), the phone legend chip ("Legend · …" — the
short form) and the phone Legend modal, and reused by the Download menu's PNG/SVG footer title so the exported figure
says the same thing as the screen. Forms:
- Scores, raster cells: **"Overall score"** (the layer's display label via `metricKeyLabel()`) as the title, then a
  subtitle line "Raster cells · All US waters · v7" (unit label · Zoom-to-region label · release); a component layer:
  "Bird: extinction risk · rescaled 0–100 by ecoregion" when the manifest says it is rescaled (`boot.ts`'s
  `component`/`Rescaled by ecoregion` group wording).
- Scores, Program areas: "Overall score · Program Areas · v7" (the choropleth).
- Species: "Leatherback turtle (*Dermochelys coriacea*)" as the title (common name first, binomial italic via the
  `.sci` class), subtitle "FWS Range · presence" or "Merged model · habitat suitability 1–100" or "AquaMaps · as
  delivered" — the active input pill's label + the representation + the value semantics (`layerBar.ts` knows them).
- The chip's short form is the title only; the modal shows title + subtitle + the layer description (B5).
Grep `e2e/` for the current legend text assertions and update them; eyes-on the legend in both lenses, both viewports.
- ALSO (B1 follow-up): the v7 manifest's curated composite label is the literal lowercase word "score" (not equal to its metric key), so it still renders lowercase in the Layer select, legend and chip. Extend `metricKeyLabel()` (src/lens/scores/boot.ts) to sentence-case the first character of ANY label ("score" → "Score"); unit test + update the existing fault patch's gate if wording changes.
