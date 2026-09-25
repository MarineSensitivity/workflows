# W4 — Cameras and the palette (reserved version 0.10.71)
Worktree `r3-w4`, branch `r3-w4-camera`, ports 4441–4449. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w4`.

Three product decisions from the round-3 plan, decided by Ben on 2026-09-25:

## R3-A2 — the phone's default first view (Ben's own words)
"find an extent that includes at least the waters of the lower 48 and ideally a sliver of Alaska (hinting at extent
coverage there)". Today `src/lib/map/camera.ts#PHONE_DEFAULT_BOUNDS` = the northern Gulf of Mexico `[[-92,24],[-84,31]]`
(read its long header: the whole study-area bbox computed to zoom ~1.27 and showed sky; every fit goes through
`chromePadding.ts` + MapLibre's `cameraForBounds`, and the phone shoots at 390×844@2x with the Layers sheet at its
default half detent). Find, BY LOOKING, an extent that frames the Pacific coast, the Gulf and the Atlantic/Florida
Program Areas of the lower 48 with the south-east Alaska / Gulf of Alaska Program-Area edge visible at the top-left as a
hint. Start from `[[-135, 20], [-60, 52]]`, shoot `phone` state `02-map` (and `03-layers` at the half detent), Read the
PNG, adjust, repeat (bounded: ≤ 6 iterations); if the globe projection makes a sliver of Alaska impossible without
losing Florida, prefer the lower 48 complete and say so with the shot. Update the header comment to the new rationale
and the unit test in `tests/map/camera.test.ts`; check `e2e/shell.firstview.phone.spec.ts` and `verify.mjs`.

## R3-A1 — wide-range models on the phone (the leatherback): option (a) + (c)
(a) when a species model's fitted bbox spans > 120° of longitude (`src/lens/species/data/camera.ts`, the pure chain
`cameraFor()`), frame the IN-US portion first: intersect the model bbox with the study area's bbox (the "All US waters"
area from `boot` — `src/lib/map/interaction.ts#studyAreasFromBoot`; if it carries no bbox, use the release's cell-grid
study-area extent already used for the desktop default camera — find it in `Shell.svelte`/`camera.ts`; never a
hardcoded number without a comment naming its source). Dateline-aware (Alaska crosses ±180 — `src/lib/geo/` has the
helpers; the CLAUDE.md pin notes say why). Applies to BOTH viewports (a whole-Pacific fit is also poor on desktop), but
only when the model actually exceeds the threshold — compact models keep the whole-range fit. (c) a small
"Zoom to: US waters | Whole range" `Segmented` (fit prop from W1 is NOT available to you — use your own compact style)
in the species title/card (`src/lens/species/SpeciesTitle.svelte` or `SpeciesCardView.svelte`) that refits; the
choice is ephemeral (not URL state) unless a `Sel` key already exists for it. Unit tests: the threshold, the
intersection incl. dateline, the fallback when the intersection is empty. e2e: `species.camera.spec.ts` gains a wide
fixture (bbox 130°E→120°W) asserting the camera centre lands inside the US intersection.

## R3-A3 — component palette pairs
`src/lib/brand/tokens.css` `--cat-*`: Fish and Bird are near-identical, Mammal and Turtle too, in both themes. Ben's suggestion was Fish → teal and Turtle → moss, BUT the Opus UI review found teal collides with Primary production (`--cat-primprod` is teal-green `#4cc9a0` navy / `#00765a` paper) and that on the DARK theme the near pairs are Coral/Mammal and Bird/Fish. So: re-hue **Fish → a blue-violet** (clear of Bird's light blue and of Primary production), **Turtle → moss/olive** (clear of Mammal's gold and of Primary production), and nudge Coral vs Mammal apart on dark if their ΔE is under ~20, both themes, keeping every pair distinguishable AND the contrast
gate green (`npm run contrast`, `tests/contrast.test.ts`, `scripts/check-hex-literals.mjs`), AND distinct from
`--cat-primprod` (already a green) — compute a pairwise ΔE (CIE76 is fine, a tiny script) across all eight categories in BOTH themes plus a deuteranopia/protanopia simulation check, and list the final hex values, the minimum pairwise ΔE per theme and the contrast ratios in the report. Regenerate the darwin gallery baselines (Flower is in the
gallery) and shoot `flower` + `report` on desktop and phone; Read them and confirm the eight petals are
distinguishable. Grep `docs/` and `e2e/` for the old hex values.

Seeded fault: the longitude-span threshold comparison inverted (or the intersection ignoring the dateline).
