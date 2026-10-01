# R4-C — Table at full stage, and the Tabs primitive (reserved version 0.10.82)
Worktree `/Users/bbest/Github/MarineSensitivity/atlas/.claude/worktrees/r4-c`, branch `r4-c-table`, ports 4531–4539.
Read `round4-session/common.md` first. Cut from `main` AFTER R4-B is merged (the Table tool already fills the stage;
you build what is inside it). Runs in parallel with R4-D: do not touch `Shell.svelte`, `ReportPane.svelte`,
`ReportTool.svelte`, `Places.svelte` or `uiState.ts`.

Ben (2026-09-30): "Table should probably always go full width … It seems silly to try fitting the Table into anything
less." and "There seems to be a difference in functionality between major slider toggles … in contrast to others that
are more suited as tabsets since the content below then changes."

1. **New `src/lib/ui/Tabs.svelte`**: an underline tablist (`role="tablist"`/`tab`, `aria-selected`, arrow keys + Home/
   End through the existing `roving.ts`), props shaped like `Segmented`'s (`options`, `value`, `ariaLabel`,
   `onchange`). Look per `branding/control-grammar.md`: plain labels on a 1 px `--divider` hairline, the active one
   bold with a 3 px underline in `--border-accent`; 44 px targets; focus ring. Add `src/gallery/sections/Tabs.svelte`.
2. **`src/lens/scores/TablePanel.svelte`**: one header row: a "← Map" button (collapses the panel, returning to the
   map), the subject line, `Tabs` for Species · Zones · Composition (replacing `Segmented`), the info and download
   buttons. Below it the table uses the whole width: remove narrow-pane truncation/ellipsis rules and any
   horizontal-scroll wrapper that only existed for 380 px; keep horizontal scroll only when the viewport is truly
   narrower than the columns (phone). Column filters stay.
3. **Empty state**: one sentence on how selection works (click a cell or Program Area, or add places) and one button,
   "Add places in Report", that opens the Report tool.
4. **Species lens table** (`SpeciesInputsTable.svelte` wherever the Table tool renders it): same header row pattern
   (← Map, subject, download); no tabs.
5. **`Segmented.svelte`**: state the switch-only rule in its header comment. Check its two other callers,
   `src/lib/feedback/FeedbackDialog.svelte` and `src/lens/species/SpeciesTitle.svelte`: if one is really tabs (the
   content below swaps, the data does not change), move it to `Tabs`; otherwise leave it and say which and why.

Pure + tested: keyboard roving in Tabs (unit or component test, whichever the repo already does for `Segmented`).
Seeded fault `tabs-arrow-keys-dead`: arrow keys no longer move the selection → that test goes red.

Owns: `src/lib/ui/{Tabs,Segmented}.svelte`, `src/gallery/sections/{Tabs,Segmented}.svelte`,
`src/lens/scores/{TablePanel,SpeciesTable,ZonesTable,Composition}.svelte`, `src/lens/species/SpeciesInputsTable.svelte`,
`src/lib/ui/DataTable.svelte` (width rules only), their tests/specs.

Chromium specs: `scores.table`, `scores.zonesTableHeader`, `species.inputs-table`, `keyboard-walk`, `shell.a11y`
(+ `e2e:shell` once). Grep `e2e/` for the table sub-tab selectors (`radiogroup`/`radio` roles become `tablist`/`tab`).
Gallery: Tabs (new) and Segmented sections, darwin baselines.

Shots: table on Species, Zones and Composition; Program-Area table; phone table (full height).
