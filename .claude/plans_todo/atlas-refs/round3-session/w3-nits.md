# W3 — App nits with a known fix (reserved version 0.10.70)
Worktree `r3-w3`, branch `r3-w3-nits`, ports 4431–4439. REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/w3`.

Items from `/Users/bbest/Github/MarineSensitivity/workflows/.claude/plans_todo/2026-09-25 atlas app plan, round 3.md`
(read section B in full; the referenced screenshots are in `…/plans_todo/atlas-refs/round3-shots/`). Do EVERY one:

- **R3-B3** popup vs panel coordinates: both print the CELL CENTRE (the cell is the unit — `src/lens/scores/popup.ts`
  and the panel's coordinate line share one helper), and the popup gets a `min-width` so "score: 44" no longer wraps.
- **R3-B4** welcome modal (`src/lens/scores/WelcomeModal.svelte` / `Modal.svelte`): on open, focus the dialog container
  (or the primary "Explore" action) — not the ×; keep `:focus-visible` rings for keyboard users; check
  `e2e/scores.welcome*.spec.ts`, `shell.a11y`, `keyboard-walk` and the modal focus-restore seeded fault still pass.
- **R3-B5** legend modal (phone): size the modal to its content — no ~160 px empty card under the ramp; if there is a
  layer description available, show it there.
- **R3-B6** Places action buttons (Share / Download places / Report) at the panel's 0.9 rem, one class.
- **R3-B7** Report: the place pill is followed by the same text as a heading line — keep the heading (accessible name),
  drop the duplicate pill (or vice-versa, but ONE). `src/report/Report.svelte`.
- **R3-B8** (display-time patch only; the source fix is a separate msens task): extend
  `src/lib/release/cite.ts#fixKnownCitationTypos` so "as provided in this R package" reads "as provided in the msens R
  package" — with a unit test and a comment naming the retirement condition (msens `datasets.json` fixed + bundles
  republished).
- **R3-B9** desktop species table: shorter column-filter placeholders that fit ("Area", "Suit.", "% cat" or similar)
  with the full text as `title`/`aria-label`; the table body fills the panel height (`height: 100%`) instead of
  leaving ~180 px blank. `src/lens/scores/SpeciesTable.svelte` / `TablePanel.svelte`.
- **R3-B10** desktop flower panel at half width: the component table's "Mean" row is cut at the panel's bottom edge —
  let the table scroll inside the panel or reserve the row. `src/lens/scores/FlowerPanel.svelte`.
- **R3-B11** report map: basemap labels ("LOUISIANA") clipped at the top edge — pad the fit by a label height or hide
  the basemap label layer in the report map (`src/report/reportMap.ts`); say which you chose and why.
- **R3-B15** `createAnalytics({preview: false})` hardcoded in `Shell.svelte` and `Report.svelte`: add
  `updatePreview(preview: boolean)` to the `Analytics` interface (`src/lib/analytics/analytics.ts`), call it once the
  session resolves (`resolveSession()` / `window.__early.session`), so `content_group` is `atlas-preview` (or whatever
  the existing preview naming is — read the module header) on the review host. Unit test with a fake gtag.
- **R3-B16** `eslint.config.js` ignores `docs/*_files/` (and `docs/status_files`).

Each item: a test (vitest where pure, chromium e2e where visual/DOM), the CHANGELOG line, and an eyes-on shot of the
fixed state at the viewport where the defect was reported (states: `flower` half, `legend` modal phone, `places`
phone, `report` scrolled phone + desktop `13b-report-map`, `table` desktop, `welcome` desktop). Seeded fault: pick one
pure rule (e.g. the cell-centre helper returning the click point).
