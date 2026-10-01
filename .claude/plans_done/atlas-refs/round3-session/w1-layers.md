# W1 — Layers pane redesign + hexagon pip (reserved version 0.10.68)
Worktree `r3-w1`, branch `r3-w1-layers`, ports 4411–4419. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w1`.

Ben's words (2026-09-25), verbatim: "drop the tiny hexagon icon next to selected tool - excessive and distracting" and
"clean up the Layers pane to something more minimalist and compact, eg checkbox instead of toggle, transparency slider
in dropdown, actual color ramps visualized for given options (see CalCOFI explore for ideas). Move Layer selector to
top, just below "Raster cells (0.05°)" (drop clunky "(0.05°)") vs "Program areas" (and prettify this pill so doesn't
look like 3rd missing option on right that is not colored when "Program areas" is selected, similar to existing Scores
vs Species toggle). Move Sphere outside individual layer selection, like to bottom of Layers pane. Rename "Study area"
to "Zoom to region" and to right (desktop) or below (mobile) Layer selector. Drop all of the following from the Layers
pane that are fine to leave on as default basemap without worrying about layer ordering: "Roads & buildings",
"Boundaries", "Land & water". Expand Zone outlines to explain Ecoregion vs Planning Area boundaries."

Also from the round-3 plan: **R3-B1** (raw metric key "score" shown in the Layer select, legend and chip — title-case a
bare key the way `categoryLabel()` does when the manifest publishes no label: `score` → "Score"; do it in ONE exported
helper used by the select, the legend and the legend chip) and **R3-B2** (the Layer picker is a native `<select>`
while every other control is `Select.svelte` — use the app component; extend `Select.svelte` with optional
`groups: {label, options}[]` rendered as `<optgroup>`s so the grouped layer list survives).

Files: `src/lib/ui/LayersPanel.svelte` (the shared stack panel), `src/lens/scores/LayersPanel.svelte` (the Data row's
controls), `src/lib/map/layerStack.ts` (the pure model), `src/lib/ui/RailButton.svelte` (the pip), `src/lens/scores/
boot.ts#unitOptions`, `src/lib/ui/Select.svelte`, `src/lib/ui/Segmented.svelte`, the species lens's wiring
(`src/lens/species/SpeciesLens.svelte`, `src/lens/scores/ScoresLens.svelte`), `Shell.svelte` only if the projection
(Sphere) needs to be lifted — prefer passing a `projection: {value, onChange}` prop through the lens that already owns
`sel.proj`. Read the current screenshots first: `/Users/bbest/Github/MarineSensitivity/workflows/.claude/plans_todo/
atlas-refs/round3-shots/desktop-04-layers-full.png` and `phone-04-layers-full.png`.

## Deliverables (in the pane, top to bottom)
1. **Hexagon pip gone**: delete `.railitem.is-on::before` and its two orientation rules in `RailButton.svelte` (and the
   comment that describes it — update the header comment: the active state is the accent fill + ring only). The logo's
   hexagon stays. Gallery baseline (darwin) regenerated; check `e2e/shell.rail.spec.ts`/`shell.phone-rail.spec.ts`
   for any pip assertion.
2. **Unit toggle** "Raster cells" | "Program areas" (labels from `unitOptions()`; drop the "(0.05°)" — put the
   resolution in a `title`/tooltip or the legend chip if you must keep it somewhere). Make it look like the top bar's
   Scores|Species switch: content-sized, NOT stretched to the row (a `fit`/`compact` boolean prop on `Segmented` that
   sets `flex: 0 0 auto` on the buttons and `align-self: flex-start`, or a wrapper class — keep the top bar and the
   Table sub-tab unchanged). Disabled in the species lens as today (reason text stays).
3. **Layer select** (the metric) directly below the toggle, using `Select.svelte` with groups; **"Zoom to region"**
   (the renamed Study-area select, same behaviour: `selStore.set({area, map: undefined})`) to its RIGHT on desktop and
   BELOW it on the phone (a flex row that wraps; give each a visible small label above, "Layer" and "Zoom to region").
   The one-line layer description stays under the Layer select (as today, `data-testid="layer-description"`).
4. **Stack rows, compact**: one line each — a native checkbox (`<input type="checkbox">` styled with the app's tokens,
   accessible name = "<Layer> visible on the map") replaces the `Switch`; the opacity slider moves into a small
   per-row "opacity" button (an icon button, `aria-label="<Layer> opacity"`, `aria-expanded`) that opens a
   `Popover.svelte` holding the range input + the current % — the row itself shows no slider; the ▲▼ move buttons
   stay but at 32px and quiet. Rows shown: **Selection, Zone outlines, Data, Place labels** (+ Bathymetry only if you
   keep it — it is disabled "coming soon"; Ben did not list it, keep it but make it visibly minor). HIDE
   `basemap-land`, `basemap-boundaries`, `basemap-roads` from the pane: add `LAYER_GROUP_IN_PANEL` (or similar) to
   `layerStack.ts` (unit-tested) — they stay in the model, in `DEFAULT_LAYER_STACK`, in the `layers=` URL codec and in
   Reset; the pane simply does not list them.
5. **Data row body** (expanded by default as today) now holds only: **Color palette** as a visual ramp picker — a
   button showing the current palette's gradient strip + name, opening a Popover `role="listbox"` with one
   `role="option"` per palette (strip + name, `aria-selected`), arrow keys + Enter/Esc; the stops come from
   `boot.palettes` via `src/lib/raster/ramps.ts#paletteStopsFromBoot` (NEVER a second ramp array — `tests/raster/
   ramps.wiring.test.ts` scans for that) — and the existing "Cells outside Program Areas" as a checkbox row.
6. **Zone outlines row** gets an expander like Data's whose body offers the outline choice bound to the existing
   `sel.out` (`Outline = "programarea" | "ecoregion" | "none"`, `src/lib/state/types.ts`; find who writes `out` today
   and reuse it) as a compact radio group with a one-line explanation under each: Program Areas = "BOEM's 2026 Program
   Areas — the planning units the scores are reported for (thick outline)"; Ecoregions = "the marine ecoregions each
   component is rescaled within (0–100 by ecoregion min/max)" — check `src/lens/scores/boot.ts`'s ecoregion comments and
   the docs for exact truthful wording, and confirm which outline the map actually draws for each value before you
   write the text. The row's checkbox = "visible" as for every row; "none" is not a radio option (unchecking is none).
7. **Sphere** at the BOTTOM of the pane (below the stack, above/beside "Reset layers"): a checkbox row "Sphere (globe
   projection)" bound to `sel.proj` exactly as the old switch was (`selStore.set({proj}); mapHandle?.setProjection`).
   Both lenses mount the shared panel — wire it for both (species too, if the species lens has `proj`; if not, say so).
8. **Reset layers** stays, as a quiet text button bottom-right.

## Tests + specs
- Unit: `layerStack.ts` (panel-visible groups; hidden groups still parse/format/reset), the label helper (B1), the
  ramp-picker's option list from boot palettes.
- e2e: update `e2e/layers.spec.ts`, `e2e/layers.select-style.spec.ts`, `e2e/scores.*.spec.ts`, `e2e/species.*`,
  `e2e/shell.*` and `tests/` wherever "Raster cells (0.05°)", "Study area", "Layers on the map", `getByRole("switch"…)`
  for stack rows, the native select for "Layer", or the Sphere switch were used. Add a chromium spec for: the opacity
  popover changes the layer's opacity (readPixel-based like `e2e/layers.spec.ts` — first prove the layer painted at a
  control point, CLAUDE.md's pixel rule); the ramp picker changes `?pal=`; the outline radio writes `?out=`;
  hidden basemap rows are absent from the pane but a `?layers=` token naming them still parses.
- `scripts/verify.mjs` state matrix may name the old labels — grep it.

## Eyes-on states: `layers`, `map`, `species` (both viewports), plus a shot of the opacity popover and the ramp
popover open (drive them with a small Playwright snippet if eyes-shots has no such state; save as
`desktop-04b-opacity-popover.png`, `desktop-04c-ramp-popover.png`, `phone-04b…`). The pane must fit the phone sheet at
the "half" detent with the toggle, Layer, Zoom to region and the first rows visible without scrolling.
