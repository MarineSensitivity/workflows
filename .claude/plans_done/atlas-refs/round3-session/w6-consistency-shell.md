# W6 — Consistency slice A: shell + shared UI (reserved version 0.10.73)
Worktree `r3-w6`, branch `r3-w6-consistency-shell`, ports 4471–4479. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w6`.
Cut your worktree from CURRENT `main` (the orchestrator has merged the Layers pane, Download menu and nits slices — read
`CHANGELOG.md`'s top entries so you build on them, not around them).

Ben asked (2026-09-25): "also do a UI review and make any other improvements for consistency sake and functionality as
you see fit." An Opus 5.5 review of 0.10.67 produced `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/review.md` (read it in full; its screenshots
are beside it). You implement these findings — read each one's What/Where/Evidence/Fix there and do the Fix:

- **UI-1** visible feedback: mount ONE `<Toast>` (`src/lib/ui/Toast.svelte` + `toastQueue.ts`) in `Shell.svelte` and in
  `report.html`'s app; add `notify(text, {tone})` to `src/lib/ui/announcer.ts` that announces AND enqueues a toast;
  switch the user-initiated calls (share/copy, add/remove/download places, pick on/off, draw hints, every "couldn't…"
  error, the new Download menu's fetch/failure messages) to it; keep `announce()` for chatty status. The toast must
  never cover the phone sheet's buttons or the legend chip (position above the chip band, check both viewports).
- **UI-2** phone map attribution visible above the sheet at every detent (use the same variable the legend chip uses).
- **UI-3** `src/lib/ui/Button.svelte` (variant primary | secondary | ghost | icon; size sm 28 / md 36, 44 under
  `pointer: coarse` via `touch-targets.css`; `icon` prop; disabled look) + the rule in `docs/design/` ("rectangles are
  actions; pills are toggles, filters, chips and segmented choices; the struck-through dashed pill only for 'not
  available in this release'"). Migrate Places, Welcome, Table ("Columns", "Show table", "Report on selected"),
  Feedback and the report's export bar / CSV button to it. Gallery section for Button.
- **UI-10** `:root { accent-color: var(--fill-accent) }` with the paper-theme steel pair; the species card's gold ✓ on
  paper → `--text-accent`; decide the quiet Switch's ON colour so ON reads the same in both themes (say what you chose).
  `npm run contrast` + `tests/contrast.test.ts` green.
- **UI-11** one tooltip mechanism: move the `data-tooltip` rule into `src/lib/ui/` as a global utility; every icon-only
  button (panel dock/full-screen/collapse, sheet detents, Table ⓘ/⬇, popup ×, species copy buttons) gets
  `data-tooltip` = its `aria-label` in Sentence case; hide while `[aria-expanded="true"]`; About's tooltip = "About this
  release".
- **UI-13** one `.sci`/`<SciName>` for every printed binomial (Scores species table, species search results, legend card
  + chip titles, report summary lines).
- **UI-16** About modal: the "v7 · date" separator, "What changed" → the release notes for `{ver}`, "Documentation" →
  `/docs/{ver}/`, `--` → em dash; render the phone ⋯ menu and the desktop Help menu with ONE `MenuItem` look (the
  Download slice added `src/lib/ui/Menu.svelte` — reuse it); Report in ⋯ vs desktop: make them match (drop it from ⋯,
  the rail has it); Help menu lists the zoom-out key too.
- **UI-17** Scores search hint closes on `focusout` to a target outside the combobox.
- **UI-19** phone sheet at peek: the collapse button becomes "Expand to half" (or the three detents become one segmented
  control with `aria-pressed`); no no-op button.
- **UI-22** harness: `scripts/eyes-shots.mjs` gains `THEME=dark|light|both` and the extra states from
  `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/harness/extra-shots.mjs` (version picker, About, Help menu, Feedback, search results, Zones +
  Composition tabs, glossary, Columns menu, coordinate dialog, Places while drawing, species not-found, species Table +
  Report), waiting for the rail to be actionable before the first click. Document the states in the script header.

Rules: everything under `src/shell/` and `src/lib/ui/` → run `npm run e2e:shell` (added by the tooling slice; if it is
not on main yet, `npx playwright test --project=chromium --workers=1 e2e/shell.*.spec.ts e2e/feedback.spec.ts`).
Regenerate darwin gallery baselines for the components you touched and Read them. Eyes-on: `map`, `layers`, `places`,
`table`, `report` on both viewports in BOTH themes, plus the About modal and the phone ⋯ menu. Seeded fault: e.g.
`notify()` enqueuing no toast (the toast e2e goes red) or the tooltip utility not hiding while expanded.

## Ben's live-site findings (2026-09-25, Scores search — do these first, they are bugs)
- **Second Program-Area pick does not zoom.** On the live site, typing and selecting a Program Area from the Search bar
  zooms to it, but selecting a SECOND one afterwards does not. Reproduce in a chromium e2e (`e2e/scores.search.spec.ts`:
  pick "GAA", assert the camera moved, then pick "ALA"/another and assert the camera moved AGAIN); find the cause in
  `src/lens/scores/state.svelte.ts#selectZone` / `search.ts` / the camera guard (`shouldFlyToArea`'s "same key already
  flown" logic, or `zoneBoundsFromMap()` seeing no tile for the second zone, or an unchanged `sel.map` short-circuit) and
  fix it at the rule, with the e2e as the permanent regression case and a seeded fault.
- **Show the options on focus.** Clicking into the Search box should open the dropdown (the Program Areas list, ranked
  as now) before any typing — like the phone search already does or a normal combobox; Escape/`focusout` close it
  (this pairs with UI-17). Keep `aria-expanded` truthful.
- **Regions move into the Search bar (Ben, 2026-09-25):** "Since the Search bar is really a zoom to this place,
  currently Program Area or lon,lat, we could also move Region (currently called 'Study area' under Layers for Scores)
  there too." The Scores search's dropdown (open on focus, per the item above) gets a first group **Regions** — the
  entries of `studyAreasFromBoot(boot)` ("All US waters", Alaska, Atlantic, Gulf, Pacific… whatever the release
  publishes), then **Program Areas**, then the coordinates hint; picking a region does exactly what the Layers pane's
  "Zoom to region" select did (`selStore.set({area, map: undefined})`, the fly handled by `Shell.svelte`'s effect).
  FIRST verify what `sel.area` changes besides the camera: the score COG is published per metric × subregion
  (`fullSubregion(layer)` etc. in `src/lens/scores/raster.ts`/`boot.ts`) — if the subregion COGs are pure crops of the
  FULL surface (same values), the move is a pure zoom; if their VALUES differ (rescaled per subregion), keep `area`
  as data state anyway, but say so in the report and make sure the legend names the region (W7's legend title). Then
  REMOVE the "Zoom to region" select from the Layers pane (`src/lib/ui/LayersPanel.svelte`'s `zoomField` slot +
  `ScoresLens.svelte`), leaving the Layer select full width; update `e2e/scores.studyarea.spec.ts` and every spec that
  used the select; the placeholder text becomes "Regions, Program Areas or lon, lat". The species lens search (if it
  is the same box) is unaffected.
