# Atlas round 3 — re-review hand-off (2026-09-25, end of the orchestrator session)

**Pushed:** atlas `main` `a7ba24a` (0.10.73) → GitHub Pages + CI (`66e5678..a7ba24a`, 52 commits). Pushed BEFORE an Opus
re-review, on Ben's call, to save credits. Local-only: workflows `main` `9110efe6` (release-side round-3 commit),
msens `main` = 0.44.2 (`app_zone_bbox()`; not installed, not pushed).

**What 0.10.73 contains (all merged with green gates: 12 fast gates + contrast, 107 seeded faults apply,
chromium e2e 237 + 65 + 26):**
- W1 Layers-pane redesign + 3 fix rounds (Ben's asks: muted checkboxes left of the name, caret expander vs arrow reorder
  over VISIBLE rows, "Outlines" = Program Areas checkbox + always-on Ecoregions, wide ramp picker, opacity popovers,
  Sphere at the bottom, basemap rows hidden, hexagon pip gone, Selection dimmed when empty).
- W2 Download menu (map PNG/SVG with legend + footer, data-layer GeoTIFF, selected-places GeoJSON) + fix round (short
  label in title/filename, absolute share URL); sun/moon theme icon (B12).
- W3 nits B3–B16. W4 phone default `[[-119,25],[-78,51]]` counting the legend chip; wide-range species framed to US
  waters first with "Zoom to: US waters | Whole range" (also on v7's bbox-less COG-bounds path); palette Fish
  `#9f81e4/#4822a0`, Turtle `#a1c76b/#495625`, Mammal `#e6c700/#82773d` (CVD ΔE ≥ 15). W5 tooling (per-section gallery
  baselines, `faults:check`, `e2e:shell`, search prefers `boot.zones[].bbox`, collapsed programarea harness shot).
- W6 shell consistency: 2nd Program-Area pick zooms (bounds cache), search opens on focus with REGIONS then Program
  Areas (the Zoom-to-region select is GONE from the pane — regions are a pure camera move, verified), visible toasts
  (`notify()`), phone attribution above the sheet, sheet peek → "Expand to half", one tooltip utility, `accent-color`,
  `SciName`, About-modal links (release notes path `release_notes.html` is a GUESS — check `docs/_quarto.yml`).

**Known reds / gaps to expect in CI:** (1) gallery axe `scrollable-region-focusable` on `.flower-table-scroll`
(W3's B10) — fix = `tabindex="0"` + `role` + `aria-label` + `:focus-visible` on the scroll container, regenerate the
Flower darwin baselines; (2) linux gallery baselines: per-section set + the Menu section need `npm run
gallery:baselines-from-ci -- <run>` once; (3) the three-engine suite has not run on this tree locally (chromium only).

**In flight, checkpointed as WIP on their branches (NOT gated, not merged):**
- `r3-w7-consistency-copy` @ `cb114cf` — W7 lens/copy slice: unified colour-coded click popup with a ramp-filled
  DENSITY sparkline + marker (`src/lib/map/density.ts`, `distribution.ts`, `raster/histogram.ts`,
  `sql/cell_histogram.sql`), `legendTitle.ts`, `formatSubject()`, species copy, report controls/links, version
  picker, species search ranking, `name_en` basemap labels, sentence-case "score". Contains an in-progress merge of
  main — verify no conflict markers, then gate.
- `r3-w8-species-share` @ `ea289de` — W8: barely started (main merged in). Brief: species input picker promoted to the
  top of the pane + zoom-to-layer on change with a `zl=` tickbox; Share writes a versioned `ui=` arrangement token;
  Layers pane = tabs (Layers | Flower plot / Species info) and the rail drops Flower; Places folds into Report as its
  first tab with a "Last clicked" slot that never wipes explicit places (Ben's selection model, in the brief).
- `r3-w3b-flower-scroll` — empty (the a11y fix above, not started).
- Deferred from W6: UI-3 shared `Button.svelte` + migration; UI-22 harness `THEME=` + modal/menu states.

**Release side still needing Ben:** R3-C2 — edit row 1 `layer` → `"Overall score"` in the untracked
`~/_big/msens/derived/{v7,v7b,v8}/layers_*.csv`, then `quarto render build_version_manifest.qmd` + `scripts/
render_app_bundle.sh v7` (and v7b, v8); R3-C4 — the fixed AquaMaps citation needs an `UPDATE dataset` on each
legacy `sdm.duckdb` + bundle republish; R3-C1 = a `publish_native` rewrite for the usa05/`mdl_seq` schema (assessed,
not run); install msens 0.44.2 and run `build_app_bundle.qmd` to publish zone bboxes.

**Briefs and reviews:** `atlas-refs/round3-session/` (every brief, incl. `review-ui.md`, `eyes-review-wave1b.md`),
`atlas-refs/2026-09-25 UI review (Opus 5.5) on 0.10.67.md` (22 do-now findings), `atlas-refs/2026-09-25 eyes-on
wave-1 review (Opus 5.5) on 7e88b94 — HOLD.md` (its HOLD items were all fixed in the W1/W2/W4 fix rounds before the
push, verified by the orchestrator from the fix agents' shots, not by a second Opus pass).

## Re-review plan (for an Opus 5.5 session)
1. Wait for CI on `a7ba24a`; read the three-engine, gallery and faults jobs first — expect the two known reds above.
2. Shoot the LIVE Pages build (`https://marinesensitivity.org/atlas/`) with `scripts/eyes-shots.mjs` (both viewports,
   dark) and a light pass; add the popovers, the Download menu, the species toggle and a downloaded PNG by hand.
3. Judge against the checklist in `atlas-refs/round3-session/eyes-review-wave1b.md` (Ben's asks 1–5) and the HOLD
   items of the wave-1 review; then the general checklist; then regressions vs 0.10.67.
4. Write `plans_todo/atlas-refs/2026-09-25 eyes-on wave-1 re-review (Opus 5.5) on a7ba24a.md`: verdict, PASS/FAIL per
   ask, defects with fixes. Land the flower a11y fix and any FAIL fixes as small commits with gates, push, then resume
   W7 → W8 per their briefs (two fix rounds each, eyes-on + review before each push).
