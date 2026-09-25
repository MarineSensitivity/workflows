# W8 — Species Layers pane, zoom-to-layer, and Share reproduces the UI arrangement (reserved version 0.10.75)
Worktree `r3-w8`, branch `r3-w8-species-share`, ports 4491–4499. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w8`.
Cut from CURRENT `main` (the Layers-pane redesign W1, cameras W4 and the other round-3 slices are merged — read the
top CHANGELOG entries and `src/lib/ui/LayersPanel.svelte` + `src/lens/species/*` first).

Ben's words (2026-09-25): "For the species Layer pane, we need to promote the main data selection up and enable zoom to
selected layer. I just chose FWS Range from default leatherback turtle layer, but see nothing on map because out of
view, so would be good to default to zoom to selected layer and have a tickbox to stop doing that in case you want to
toggle between layers. also, when clicking Share, the link should include all the same UI elements in their
arrangement, eg on Layers pane".

1. **Promote the species data selection to the top of the Layers pane** (species lens): the input picker that today
   sits inside the Data row's body (`LayerBarView.svelte`'s pills: Merged model + each input, e.g. "FWS Range",
   "AquaMaps"; the Delivered/As-ingested representation toggle) moves to the top of the pane, in the SAME slot the
   scores lens uses for its Layer select (the shared panel's `layerField` slot or a new `speciesField` snippet) —
   visually the species equivalent of "Layer": a small label "Model input", the pills (or a `Select` if the pill row
   overflows on the phone — decide by shooting at 390 px, say what you chose), then the representation toggle when
   available. The Data row's body then holds only what remains (palette picker if species has one, etc.).
2. **Zoom to the selected layer on change, with a tickbox to stop it**: picking an input (or Merged) refits the camera
   to THAT surface's extent through the existing camera chain (`src/lens/species/data/camera.ts#cameraFor`, the
   `/cog/info` bounds refinement, W4's wide-range rule and the "US waters | Whole range" target all apply) — the same
   fit the species title's zoom already does. A checkbox row "Zoom to layer on change" (checked by default) directly
   under the input picker turns it off so a user can flip between inputs at a fixed camera. The preference is
   URL-state (`zl=0` when off; absent when on — `src/lib/state/types.ts` + `codec.ts` + round-trip tests), so a shared
   link reproduces it. A fit never runs when `sel.map` was set by the user's own pan since the last pick — read
   `camera.ts`'s precedence header and keep it consistent.
3. **Share reproduces the UI arrangement.** Read `src/lib/state/`'s URL rules and the U1 decision behind the seeded
   fault `panel-geometry-in-url` (tests/faults + its GATES.md row + `docs/`): the live URL must NOT churn on every
   panel drag. Reconcile by adding ONE compact `ui=` token that is written **when Share builds its link** (and parsed
   on load), carrying: active tool, desktop dock side + size preset (or the phone sheet detent), the Layers pane's
   expanded rows (Data/Outlines), the selected species input + representation (already in the URL — reuse), the
   zoom-to-layer preference (item 2), and the scores lens' unit/layer/palette (already URL state — verify they are
   in the share link). Versioned like `g1` (plan D8): `ui=1.<fields>`; unknown/malformed → ignored, never an error.
   Loading a link with `ui=` restores the arrangement before first paint where possible (tool + dock are static
   skeleton concerns — check `index.html`'s skeleton and CLS gate `e2e/shell.cls.spec.ts`). Keep the U1 rule for
   ordinary interaction (no `replaceState` per drag) and make the fault patch + its test still red/green correctly;
   add a round-trip unit test for `ui=` and a chromium e2e: arrange (dock bottom, expand Outlines, pick FWS Range,
   untick zoom-to-layer) → Share → open the copied link in a fresh context → same arrangement.

Gate as in common.md (this touches `src/shell/` and `src/lib/ui/` → `npm run e2e:shell`), plus `e2e/species.*`,
`e2e/layers*`, `e2e/shell.url-state.spec.ts`. Eyes-on: species pane both viewports (before/after picking FWS Range —
the map must show the range), the phone pane at the half detent, and the restored-from-link state. Seeded fault: the
`ui=` token dropping the dock side, or the zoom-on-change fit skipped when the preference is on.

4. **The Layers pane becomes a two-tab panel; the Flower plot moves into it; the rail drops to four tools.** Ben
   (2026-09-25): "differentiating the extra information about the species in another tabset from the interactive
   control of the layers in its own default tab. Perhaps extra info could be populated for scores for symmetry sake —
   that would be a good spot to put the flower plot (so update the tab name when showing Scores) and then allows us to
   drop the Flower plot from the toolbar (which only applies to the Scores)."
   - Tab 1 (default) **"Layers"**: the interactive controls exactly as redesigned (unit toggle, Layer/Zoom-to-region or
     the species input picker, stack rows, Sphere, Reset).
   - Tab 2: **"Flower plot"** in the Scores lens = today's `FlowerPanel.svelte` content (title/subject line, flower,
     component table — same component, same behaviour, same fits); **"Species info"** in the Species lens = the species
     card's descriptive content (`SpeciesCardView.svelte`: names, listing, categories, inputs table…), i.e. everything
     that is information rather than a control. Use the same tab look as the Table's Species|Zones|Composition
     `Segmented` sub-tab; the active tab is URL state (`tab=` inside the `ui=` token from item 3, so a share link opens
     the flower tab if that is what was showing). Tapping a scored cell while on tab 1 keeps tab 1 (the popup shows
     the value); the flower tab is where the user goes to see the breakdown — but when the FLOWER tab is already
     showing, a new tap updates it in place. On the phone, the sheet's title shows the active tab's name.
   - **Remove "Flower plot" from the tool rail** (`src/shell/tools.ts`: the order becomes Layers, Places, Table,
     Report — update its "FIVE controls" comments/tests, `index.html`'s static skeleton, `docs/design/spec.md` §5,
     `tests/shell/tools.test.ts`, `e2e/shell.rail.spec.ts`, `shell.phone-rail.spec.ts`, `scores.flower.spec.ts`,
     `scripts/eyes-shots.mjs`'s `flower` states (now: open Layers → Flower plot tab), `scripts/verify.mjs`, the tour
     (`src/shell/tour.ts` step that pointed at the rail's Flower button), the parity page rows, `docs/status.md`).
     The Flower tool's "inactive in Species lens" rule and its seeded fault (`railbutton-…`/inactive-tool faults —
     check `scripts/test-faults.mjs`) become moot: retire or retarget each affected fault (GATES.md row says why) and
     prove the survivors red.
   - Deep links: an old `?tool=flower` (or whatever key the tool used) must still open the Layers pane on the Flower
     tab — a legacy mapping in the codec (`src/lib/state/legacy.ts`), unit-tested.
   Eyes-on both viewports: the pane on each tab in both lenses, the four-tool rail (desktop + phone tab bar at 320/390),
   a tapped cell with the Flower tab open. This item is large: take it after items 1–3 are green, two fix rounds max,
   then report.

5. **Places folds into the Report tool as its first tab (Ben, 2026-09-25, proposed by him and not objected to).** The
   rail becomes THREE tools: Layers, Table, Report. The Report pane is a two-tab panel like Layers: **Places** (default:
   pick a Program Area / draw / upload, the per-place results list, Share and Download places — today's
   `src/places/Places.svelte` content, unchanged in behaviour) and **Report** (today's `ReportTool.svelte`: options,
   generate, exports). Mitigations for discoverability (the Table also uses the selection): the Table's empty state
   says "Select places under Report → Places" with a button that opens that tab; the pane title reads "Report · Places"
   while the Places tab is active; the tour step for Places points at the tab. Same mechanics as item 4: `tools.ts`
   order + comments/tests, skeleton, spec.md §5, rail specs, `scripts/eyes-shots.mjs` + `verify.mjs` states
   (`places` → open Report → Places tab), parity rows, `docs/status.md`, legacy `tool=places` mapping in the codec,
   affected seeded faults retired/retargeted with GATES.md rows. Take it LAST, after item 4 is green; two fix rounds.
   **Selection model for item 5 (Ben, 2026-09-25, verbatim):** "still allow clickable selection (highlighted in pink
   as now) of either Cell or Program Area depending on Scores layer chosen, such that the last clicked element defaults
   to the current Report Place and therefore also the one applied to the Table tool. So we get the same implied
   functionality. I like the extra verbiage share to explicitly tell the user how it works, and some care should be given
   to not wiping out existing selections that have been explicitly added to Places, but then a most recently selected
   slot that can be updated with subsequent selection (and explicit addition to Places for reporting)." Implement as:
   - a map click on a cell (Raster cells unit) or a Program Area (Program areas unit) sets the **"Last clicked"** slot
     (pink highlight as today, `sel.sel` semantics unchanged); the slot is what the Table and the Flower tab show and
     what the Report tab uses when the Places list is empty — it is shown at the TOP of the Places tab as its own row
     ("Last clicked: Cell 3350704 · 28.625° N, 90.575° W" / "GOA Program Area A (GAA)") with an **"Add to places"**
     button beside it; a new click REPLACES the slot only, never anything in the explicit Places list;
   - explicit Places (picked/drawn/uploaded/added) persist across clicks; the Report reports on the explicit list when
     it has entries and on the Last-clicked slot otherwise — the Report tab says which in one sentence ("Reporting on
     the last clicked place — add it to Places to keep it, or add more places below.");
   - the Table's subject line uses the same rule and says so ("Species for the last clicked cell", "Species for 2
     places"); the empty state explains how selection works, with the "Report → Places" button;
   - the pure rule (`reportSubjects(sel, places)` → `{kind: "last-clicked" | "places", items}`) lives in `src/lib/`
     with unit tests (last-clicked replaced on click; explicit list untouched by a click; empty list → slot; both → list);
     a chromium e2e proves a click after adding a place does not remove that place; seeded fault: the click wiping the
     explicit list.
