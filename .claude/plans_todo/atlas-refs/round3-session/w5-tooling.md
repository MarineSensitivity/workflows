# W5 — Process, tooling and harness debt (reserved version 0.10.72)
Worktree `r3-w5`, branch `r3-w5-tooling`, ports 4451–4459. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w5`.

From the round-3 plan, sections B/D (read them):

- **R3-D1** gallery screenshots per SECTION, not full page: `e2e/gallery.spec.ts` currently does one
  `toHaveScreenshot(fullPage)` per theme × viewport and the desktop one jitters by 1 px in height between CI runs
  (Playwright refuses a size mismatch before comparing). Screenshot each gallery `<section>` element (stable heights;
  name `gallery-{theme}-{viewport}-{sectionId}.png`), keep `maxDiffPixelRatio`, delete the old full-page baselines,
  regenerate the darwin set (`PW_PORT=<port> npm run e2e:gallery -- --update-snapshots`) and Read a few. Also write
  `scripts/gallery-baselines-from-ci.mjs <run-id>` (uses `gh run download <run> -n gallery-test-results`, copies each
  final-attempt `*-actual.png` over the matching `*-chromium-linux.png`, prints what it replaced) and document both in
  `CLAUDE.md`'s gallery lesson + `tests/GATES.md`.
- **R3-D2** faults hygiene: `scripts/check-faults-apply.mjs` = `git apply --check` over every patch referenced in
  `scripts/test-faults.mjs` (exit 1 listing the stale ones); `npm run faults:check`; mention in CLAUDE.md's registry
  lesson.
- **R3-D3** `npm run e2e:shell` = chromium, `--workers=1`, `e2e/shell.*.spec.ts e2e/feedback.spec.ts`; CLAUDE.md's
  "any change under src/shell/ or src/lib/ui/" rule names it.
- **R3-D4** pixel gates prove the layer painted first: find the gate the `layerstack-order-ignored` fault relies on
  (`scripts/test-faults.mjs` ~line 589 → its spec in `e2e/layers.spec.ts`) and make it assert the UN-promoted colour at a
  control point before the promoted one (or gate on the whole spec); prove with `--only layerstack-order-ignored`.
- **R3-D6** `docs/status.md` "Decisions" table: rows R1–R7 still say "building" for shipped work (U1/U3/U4/U5 shipped in
  0.10.30–0.10.67 — check `CHANGELOG.md` for the version each landed in) — refresh statuses/versions.
- **R3-B17** harness: `scripts/eyes-shots.mjs` gets a desktop `programarea` state with the panel COLLAPSED so the map
  tooltip's full Program Area name is visible; keep WARN + `-MISSED` behaviour.
- **R3-B14** (app half of C3): `src/lens/scores/search.ts` / `ScoresSearch.svelte`'s zone zoom prefers a published
  `boot.zones[].bbox` (`[w, s, e, n]`, WGS84, dateline-aware — a zone crossing ±180 publishes `w > e`) when present, and
  falls back to today's `zoneBoundsFromMap()` otherwise. The bundle does not publish it yet (msens will); a unit test
  with a fixture boot that has bbox, and one without.

Seeded fault: the bbox preference dropped (search falls back even when bbox is present).
Eyes-on: run the whole `scripts/eyes-shots.mjs` once (both viewports) and Read the new `programarea` desktop shot.
