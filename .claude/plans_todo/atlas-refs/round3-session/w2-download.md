# W2 — Download menu + theme icon (reserved version 0.10.69)
Worktree `r3-w2`, branch `r3-w2-download`, ports 4421–4429. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w2`.

Ben's words (2026-09-25): "add download button for PNG/SVG for given map view and TIF/geojson for raster/vector source
data layer. See CalCOFI explore for nice dropdown example." Plus **R3-B12**: "The theme toggle reads as a settings
gear. Use sun/moon (the phone ⋯ menu already says 'Switch to light theme')" — check `Shell.svelte` ~line 1550 and the
icon map: `themeSun`/`themeMoon` exist; make sure the DESKTOP button renders them (not a gear) and that the icon shown is
the destination theme; if the map already resolves to a gear glyph (`scripts/icon-map.json`), fix the mapping.

## Deliverables
1. `src/lib/ui/Menu.svelte` — a reusable dropdown: trigger `<button aria-haspopup="menu" aria-expanded>`, a
   `role="menu"` list of `role="menuitem"` buttons/links (icon + label + optional small hint), Esc / outside click /
   item click close, ArrowUp/Down + Home/End roving, focus returns to the trigger, `align` left/right, opens inside
   `Popover.svelte` if that component already does the anchoring (read it first). Gallery section + darwin baseline.
2. A **Download** control: desktop = an icon button in the top bar next to Share (tooltip "Download", same
   `data-tooltip` pattern), phone = a "Download…" entry in the ⋯ More menu (`TopBarActions.svelte`) that opens the same
   menu (or a Sheet listing the same items — your call, say why). Items (`src/lib/download/` — pure helpers + tests):
   - **Map view · PNG** — the MapLibre canvas (`preserveDrawingBuffer` is already set; `map.triggerRepaint()` then two
     rAFs, then `canvas.toDataURL`) composited on a 2D canvas with a footer band (title = layer or species name · unit
     · release `ver` · the app's own share URL minus the origin, like CalCOFI's `drawFooter`) and the legend gradient +
     its endpoint labels drawn in the bottom-left; `saveBlob` with the name
     `marine-atlas_{lens}_{layer-or-species-key}_{ver}_{yyyymmdd}.png`. Theme background, never transparent.
   - **Map view · SVG** — an SVG document the same size: the PNG embedded as `<image href="data:…">` plus the footer
     text and the legend as real vector elements; the menu hint says "raster map in an SVG wrapper" so nobody expects
     vector coastlines.
   - **Data layer · GeoTIFF** — the COG the current view draws: scores lens → the score COG href for the current
     metric × subregion (`src/lens/scores/raster.ts` / the manifest's `cogs`), species lens → the currently drawn
     surface's COG (`src/lens/species/state.svelte.ts`; the layer bar's active pill). Fetch → blob → `saveBlob`
     (S3 sends no content-disposition, so a plain `<a download>` cross-origin would just navigate); show a toast while
     fetching and on failure; name `…_{metric_or_mdl_key}.tif`. Disabled (with the reason as the hint) when no COG is
     published for the view.
   - **Vector data · GeoJSON** — investigate what the app can honestly export: the zone outlines come from PMTiles
     (partial by viewport — `src/places/zoneOutline.ts` says so); the report/places already write GeoJSON
     (`src/places/download.ts`). Offer "Selected places · GeoJSON" (the current places/selection, reusing that
     module) when there is a selection, and "Program areas · GeoJSON" ONLY if a complete geometry source exists (a
     published GeoJSON/FlatGeobuf in the bundle or manifest — check `boot.json`'s zones and the manifest's assets; if
     none, do NOT fake it from loaded tiles: leave the item out and write in the report what would have to be
     published, e.g. `app/zones/programarea.geojson`).
3. Analytics: one `track` event per download kind, through the existing `src/lib/analytics/` API (never the raw URL).
4. Tests: vitest for the filename builder, the SVG wrapper (size, footer text, escaped labels), the item list per lens
   state (enabled/disabled with reasons); chromium e2e: the menu opens/closes by keyboard; PNG download produces a
   file (`page.waitForEvent("download")`) with a non-blank image (`scripts/verify.mjs`/CalCOFI `luminanceStats` idea);
   GeoTIFF item fetches the routed COG fixture (`e2e/map-hermetic.ts`'s `SCORE_COG_URL`).
5. Seeded fault: e.g. the filename builder dropping the release version, or the SVG wrapper losing the footer.

## Eyes-on: `map` desktop with the Download menu OPEN (drive it), phone More menu open, plus open one downloaded PNG and
Read it — the map, legend and footer must all be there. Shell/ui changed → run `e2e/shell.*.spec.ts` + `feedback.spec.ts`.
