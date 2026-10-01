# R4-0 — contact sheet for eyes-on (tooling, no version bump)
Worktree `/Users/bbest/Github/MarineSensitivity/atlas/.claude/worktrees/r4-0`, branch `r4-0-contact-sheet`,
ports 4501–4509. Read `round4-session/common.md` first; the version/CHANGELOG/seeded-fault/size parts do not apply
to you (nothing user-visible changes).

Why: the orchestrator reviews each wave by looking at shots. Two images per wave instead of forty.

1. `scripts/eyes-shots.mjs` gains `SHEET=1`. After the normal shooting loop, for each viewport that produced shots,
   compose them into ONE labelled grid and write `<OUT>/contact-desktop.png` and `<OUT>/contact-phone.png`.
   - Use the Playwright the script already imports: build an HTML string of `<figure><img><figcaption>` tiles
     (images as `file://` URLs or data URIs), `page.setContent()`, screenshot the grid element. No new dependency.
   - Each tile: the shot scaled to a fixed width (desktop tiles ~480 px wide, phone tiles ~260 px wide), its state
     name under it, and a visible red border + "MISSED" label when the file name carries `-MISSED`.
   - Cap a sheet at ~2400 px wide; wrap rows. With more than 24 tiles write `contact-<vp>-2.png` etc.
   - `SHEET=1` with `ONLY=` sheets only the states shot in this run (not stale PNGs already in `OUT`).
2. Update the usage comment at the top of the script (the `ATLAS_URL=… OUT=… [ONLY=…]` line) to include `SHEET=1`.
3. Keep the compose step a small exported pure function where practical (tile list → HTML string) and add one vitest
   test for it (labels present, MISSED marked, row wrap). Do not restructure the rest of the script.

Owns: `scripts/eyes-shots.mjs`, one new test under `tests/scripts/` (or wherever script tests live — check `tests/`).

Gate: `npx vitest run <your test>`, `npm run lint`, `npm run format:check`, `npx tsc --noEmit`, then one real run:
`npm run build`, `vite preview --port 4501 --strictPort`, `ATLAS_URL=http://localhost:4501 OUT=.tmp/eyes
ONLY=map,layers SHEET=1 node scripts/eyes-shots.mjs` (check the script for the exact state names). Report the two
contact-sheet paths and their pixel sizes. No e2e, no faults run (`npm run faults:check` only).
