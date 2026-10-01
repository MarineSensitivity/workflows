# R4-B — the spine: rail attached to the panel, four tabs, left by default (reserved version 0.10.81)
Worktree `/Users/bbest/Github/MarineSensitivity/atlas/.claude/worktrees/r4-b`, branch `r4-b-spine`, ports 4521–4529.
Read `round4-session/common.md` first. Cut from `main` AFTER wave 1 (R4-0, R4-A) is merged.

Ben (2026-09-30): "Does the separation of the toolbar on left with details pane on the right make sense? … The use of
Layers for both seems a bit duplicative … Table should probably always go full width." and "Let's default the tool
bar + content pane onto the left side." Decisions D1–D3 are locked (see `common.md`).

1. **Four tools.** `src/shell/tools.ts`: `TOOL_ORDER = ["layers", "details", "table", "report"]`, labels
   Layers / Details / Table / Report, one icon for Details in both lenses (pick from `icon-paths.ts`; add one via
   `scripts/icon-map.json` only if none fits). Update the "THREE controls" comments and `tests/shell/tools.test.ts`.
2. **The rail is attached to the panel** (desktop): it sits on the panel's OUTER edge (the screen edge), the panel
   body opens beside it, and both move together when the side is swapped. The old free-floating left rail card is
   gone. Clicking the active tool collapses the panel (the rail stays); clicking any tool opens it. Phone: the
   bottom bar stays as it is, now with four entries (check 320 and 390 px widths: no overflow, 44 px targets).
3. **Left by default.** `panelGeometry.ts`'s default `dock` becomes `"left"`; the legend's mirrored position
   (`data-panel-dock` on `.stage`) and every camera fit (`chromePadding.ts`) must follow the side. A stored
   per-browser preference still wins; a `ui=` token's side still wins over both.
4. **Details** is `FlowerPanel.svelte` in the Scores lens and `SpeciesCardView.svelte`'s descriptive content in the
   Species lens, moved out of `LayersPanel`'s `infoTab` (delete that prop, its `Segmented`, and `UiTab`). Top of
   Details in Scores: the last-clicked label, "Add to report" (the existing `onAddLastClicked`) and "Open table".
   A map click on a cell/area while Details is open updates it in place; a click never switches tools.
5. **Table takes the full stage** by reusing the existing maximized geometry whenever `tool === "table"`; leaving
   Table restores the side dock. Remove the dock-bottom and maximize buttons and the Esc-restores-maximize path;
   keep drag-resize, add one "move to the other side" button, keep collapse. (Table's inside is R4-C's.)
6. **Header shows context**, not the tool name: Layers → "<layer> · <unit>"; Details → the clicked subject or the
   species; Table → its subject line; Report → "N places" / "Last clicked place". Phone sheet title: same.
7. **Popup**: add a "Details" link (opens the Details tool) to the Scores and Species popups.
8. **URL/`ui=` token** (`src/shell/uiState.ts`): bump to version 4 (tool is one of the four; side is left|right; no
   bottom, no maximized, no `tab`). Version 3 tokens still parse: `tab=info` → tool `details`; dock bottom or
   maximized → side dock; everything else as before. `src/lib/state/legacy.ts`: `tool=flower` → details,
   `tool=places` → report. Unit-test every mapping and the v4 round trip.
9. `index.html`'s static skeleton (rail on the left edge attached to the panel slot) and `e2e/shell.cls.spec.ts`;
   `src/shell/tour.ts` steps that point at the rail or the Flower tab; `scripts/eyes-shots.mjs` and
   `scripts/verify.mjs` states (flower states → open Details; add `details` and `collapsed` states).

Seeded faults: regenerate `ui-token-tab-dropped` against the v4 tool field (or retire it with a GATES.md row saying
why); new `table-tool-not-full-stage` (Table opens at side-dock width → a spec asserting the table's width goes red).

Owns: `src/shell/{Shell.svelte,shell.css,tools.ts,uiState.ts,tour.ts}`, `src/lib/ui/{Rail,RailButton,Panel,
LayersPanel}.svelte`, `src/lib/ui/panelGeometry.ts`, `src/lib/map/chromePadding.ts`, `src/lib/state/legacy.ts`,
`index.html`, `scripts/{eyes-shots,verify}.mjs` (state lists only), the popups' link line, their tests/specs.
Not `TablePanel.svelte`, `ReportPane.svelte`, `ReportTool.svelte`, `Places.svelte` (R4-C / R4-D).

Chromium specs: `scores.flower`, `scores.collapsed-panel`, `layers`, `keyboard-walk`, `scores.table`,
`species.camera`, then `e2e:shell` once (rail, phone-rail, panel, cls, share-arrangement, url-state, legend-position).
Gallery: Rail and Panel sections, darwin baselines.

Shots: layers, details (Scores, with a clicked cell), species + its details, table, collapsed, phone bar at 390.
