# R4-A — the histogram moves into the legend (reserved version 0.10.80)
Worktree `/Users/bbest/Github/MarineSensitivity/atlas/.claude/worktrees/r4-a`, branch `r4-a-legend-histogram`,
ports 4511–4519. Read `round4-session/common.md` first.

Ben (2026-09-30): "the histogram should represent the density of values across the whole layer (raster or vector)
and so not vary across clicks of the same layer, just the vertical line indicating the clicked element's value …
better suited in the Legend above the color ramp so it stays the same throughout."

What is true on `main` today: Scores/Raster cells bins `sql/cell_histogram.sql` over "whatever tiles are currently
mounted" (so the shape follows the click); Scores/Program areas bins every zone value from the boot bundle
(`valueListDistribution`, correct); Species never shows one (`speciesRasterDistribution()` in
`src/lib/map/distribution.ts` has no caller).

1. **`src/lib/ui/Legend.svelte`**: an optional histogram above the ramp, sharing the ramp's x-axis and width; bars
   coloured by the ramp at their x; plus an optional marker (a vertical line through histogram + ramp with the value
   label) for the last clicked element. No histogram prop → today's legend exactly. `aria-label` on the chart;
   marker value also in text for screen readers. Works in the phone legend modal (`LegendChip`).
2. **Scores / Raster cells**: whole-layer bins from titiler `/cog/statistics` on the layer's own COG
   (`boot.layers[].by_subregion.FULL.cog`), display-only (owner decision D4; `CLAUDE.md` "Numbers never come from the
   tile server" now sanctions exactly this). Extend `src/lib/raster/histogram.ts`'s domain guard to allow a `scores`
   domain; pass `histogram_range=<rescale min>,<rescale max>` so the bins line up with the legend's ramp. Cache via
   `distributionFor()` keyed by (ver, lens, layer) — never by click. Failure or tiler down → `null` → ramp alone.
   The marker's VALUE still comes from Parquet (the click's existing `cell_value` path).
3. **Retire the per-click source**: delete `sql/cell_histogram.sql`, `cellHistogramValues`, `rasterCellDistribution`
   and their rows in `tests/analysis/{sqlTwins,queries}.test.ts` and `templates.ts`.
4. **Scores / Program areas**: keep `valueListDistribution()`; move its render to the legend; marker = the clicked
   area's value.
5. **Species**: call `speciesRasterDistribution()` for a COG input (same `histogram_range` rule with the asset's own
   `rescale`); a PMTiles range (presence only) has no histogram. Marker = the `/cog/point` click value.
6. **Popups** (`src/lens/scores/popup.ts`, `src/lens/species/popup.ts`, `src/lib/map/popup.ts`): remove the sparkline
   slot and its "render now, fill in later" plumbing (D5). Keep title, swatch, value. (The "Details" link is R4-B's.)
7. The histogram and marker travel inside the existing legend objects (`scoresLens.mapExtra.legend`,
   `speciesLens.mapInputs.legend`) so `Shell.svelte` needs no change. If it truly must, make the smallest edit.

Pure functions + tests: `markerX(value, histogram)` (clamped, null-safe); bins-to-bars scaling; a null source gives
no histogram. Regression named `legend-histogram-stable-across-clicks`: two clicks on different cells of one layer
yield the identical histogram object and only the marker differs. Seeded fault `legend-histogram-keyed-by-click`:
the cache key includes the clicked cell → that regression goes red.

Owns: `src/lib/ui/Legend.svelte`, `src/lens/scores/ScoresLegend.svelte`, `src/lens/species/SpeciesLegend.svelte`,
`src/lib/map/{distribution,density,popup}.ts`, `src/lib/raster/histogram.ts`, `src/lib/analysis/{queries,templates}.ts`
(histogram parts), `sql/cell_histogram.sql`, the legend/histogram parts of `src/lens/scores/{state.svelte,mapInputs,
popup}.ts` and `src/lens/species/{state.svelte,mapInputs,popup}.ts`, `src/gallery/sections/Legend.svelte`, their tests.
Not `src/shell/*`, not `scripts/eyes-shots.mjs`.

Chromium specs: `scores.popup`, `scores.popup.novalue`, `species-popup`, `shell.legend-position`, `shell.legend-chip`
(+ `e2e:shell` once). Hermetic specs must `page.route` the new `/cog/statistics` call (a fixture response; and one
case where it 500s → legend still renders the ramp). Gallery: Legend section, darwin baselines.

Shots (`ONLY=`; check the script for exact names): map with a clicked cell, Program-Area popup, species model, phone
legend modal.
