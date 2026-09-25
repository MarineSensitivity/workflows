# Eyes-on visual review of atlas main after round-3 wave 1 (Opus 5.5)

Print your model id on line 1. You judge screenshots of a REAL build; you do not edit source.

Build: `/Users/bbest/Github/MarineSensitivity/atlas` at current `main` (a7ba24a, 0.10.73 — merges W1 Layers-pane redesign + its 3 fix rounds,
W2 Download menu + sun/moon icon, W3 nits B3–B16, W4 phone default view + wide-range species framing + palette,
W5 tooling, W6 shell consistency: toasts, tooltips, regions in the search bar (the Zoom-to-region select is GONE from the pane by design), accent colours; and the W2/W4 fix rounds from your previous review — verify each of your HOLD items is now fixed). Read `CHANGELOG.md`'s top five entries to know what changed. Work in a fresh detached worktree:
`git worktree add .claude/worktrees/r3-review3 main` (`npm ci && npm run duckdb:fetch-ext && npm run build`,
`npx vite preview --port 4561 --strictPort`), then shoot every state with
`ATLAS_URL=http://localhost:4561 OUT=/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review3/shots node scripts/eyes-shots.mjs` (dark) AND a light-theme pass
(reuse `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/harness/eyes-shots-light.mjs` from the previous review if it still runs against this build,
else add `?theme=paper`), plus: the Layers pane's opacity popover and palette popover open (both viewports), the
Download menu open (desktop) and the phone ⋯ → Download modal, the species lens with the "Zoom to: US waters | Whole
range" toggle visible, and a downloaded Map-view PNG opened and Read.

Judge against Ben's asks for this round (each must be visibly true, say PASS/FAIL with the shot name):
1. No hexagon pip beside the active rail tool.
2. Layers pane: "Raster cells | Program areas" compact toggle at the top (no "(0.05°)"); Layer select and "Zoom to
   region" beside it (desktop) / stacked (phone); compact rows with MUTED checkboxes LEFT of the name; opacity as a
   small inline control opening a popover; a rotating caret for the Data/Outlines expander vs arrow glyphs for
   reorder; "Outlines" expander offering Program Areas | Ecoregions with one-line explanations; palette picker wide
   with the gradient strip + name; Sphere at the bottom; NO rows for Roads & buildings / Boundaries / Land & water /
   Bathymetry; "Selection" dimmed with "— nothing selected" on a bare load and undimmed after a cell tap.
3. Download menu present (desktop icon left of Help; phone via ⋯), items PNG/SVG/GeoTIFF (+ GeoJSON when places
   exist); the downloaded PNG shows map + legend + footer; the theme toggle is a sun/moon, not a gear.
4. Nits: popup and flower print the same cell centre and the popup does not wrap "score: 44"; welcome modal has no
   focus ring on the × at load; phone legend modal has no empty space; Places buttons match panel text size; the report
   shows the place name once; table filter placeholders are not truncated and the table fills the panel; the flower's
   component table reaches "Mean" (scroll); report map labels not clipped.
5. Phone default view: lower-48 waters in frame with minimal sky above the globe; species wide-range model framed to
   US waters with the toggle; eight distinguishable petal colours in both themes.
Then the general checklist: anything broken, overlapping, clipped, unreadable, inconsistent between viewports/themes,
or a regression from 0.10.67 (compare with `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/shots/` from the previous review).

Output `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review3/review.md`: verdict line first (`SHIP` or `HOLD` + the blocking items), then the numbered PASS/FAIL
list with evidence, then defects found (what/where/shot/fix, S/M/L), then nits. Kill your preview server; leave the
worktree.
