# R4-D — Report as one flow (reserved version 0.10.83)
Worktree `/Users/bbest/Github/MarineSensitivity/atlas/.claude/worktrees/r4-d`, branch `r4-d-report`, ports 4541–4549.
Read `round4-session/common.md` first. Cut from `main` AFTER R4-B is merged. Runs in parallel with R4-C: do not touch
`TablePanel.svelte`, `src/lib/ui/{Tabs,Segmented}.svelte` or the table components.

Ben (2026-09-30): "Report: Places vs Report. The content of the Report tab seems redundant and unnecessary. Perhaps
consolidate Places into simply Report." Checked on `main`: `ReportTool.svelte`'s chooser (a Program Area select plus a
"Draw, enter coordinates or upload a file" link) duplicates what `Places.svelte` already offers.

The Report tool becomes ONE scrolling flow, top to bottom, with no sub-tabs:
1. **Places** heading with the count ("Places 2 / 20"). First the **Last clicked** row (pink-outlined, label +
   "Add") when there is a last-clicked cell/area not already in the list; then the explicit list (each with zoom /
   remove as today); an empty list with no last click shows one sentence ("No places yet. Click the map, or add one
   below.").
2. **Add a place**: the Program Area select + add, Pick on map, Polygon / Rectangle / Circle, Enter coordinates, the
   upload drop zone, "Show analysis cells" — today's `Places.svelte` controls, regrouped under this one heading.
3. **A footer pinned to the bottom of the pane** (it does not scroll away): the subject sentence from
   `reportSubjectSentence()` ("Reporting on 2 places." / "Reporting on the last clicked place. Add it to keep it." /
   "Add a place to open a report."), then the primary gold **Open report** button (disabled with the third
   sentence), then Share and Download places.
4. "Reports opened this session" and "Recent": one collapsed disclosure under Add a place.

Code:
- `src/shell/ReportPane.svelte`: remove the `Segmented` and `activeReportTab`; render the order above.
- `src/shell/ReportTool.svelte`: delete the chooser section (select, "Report on this …", the draw link, `onOpenPlaces`);
  keep the open action and the recents list.
- `src/shell/uiState.ts`: retire `UiReportTab`; a v3/v4 token carrying a report-tab field parses and ignores it.
  (R4-B owns the rest of `uiState.ts`; touch only the report-tab lines.) `Shell.svelte`: only the `ReportPane` props
  and snippets that change.
- The W8 selection rules do not change: a map click replaces only the Last-clicked slot; explicit places persist.
  `reportSubjects()` is untouched.
- Tour: the Places step points at the Report tool's Places heading.

Tests: regenerate `places-list-wiped-by-click` and prove it red. Remove the `openReportTab()` helper that 0.10.79
added to `e2e/report.flow.spec.ts` and the matching click in `e2e/places.cold-link.spec.ts` (there is no Report tab
to click any more). New seeded fault `report-open-enabled-with-nothing`: Open report is enabled with no place and no
last click → the "nothing selected" case in `report.flow.spec.ts` goes red.

Owns: `src/shell/{ReportPane,ReportTool}.svelte`, `src/places/Places.svelte`, the report-tab lines of `uiState.ts`,
the `ReportPane` wiring in `Shell.svelte`, the Places tour step, their tests/specs.

Chromium specs: `report.flow`, `places`, `places.pick`, `places.last-clicked`, `places.cold-link`,
`places.concurrency`, `keyboard-walk` (+ `e2e:shell` once).

Shots: report empty, report with a last-clicked place, report with two places, phone report (full height).
