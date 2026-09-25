# UI review of the Atlas app (Opus 5.5) — consistency + functionality, 2026-09-25

Ben's ask (verbatim): "Please also do a UI review and make any other improvements for consistency sake and
functionality as you see fit." You REVIEW and write findings; a build agent implements what the orchestrator picks.
Print your model id on line 1 of the report; the orchestrator must know if you are not Opus 5.5.

## What to look at
- The app: `/Users/bbest/Github/MarineSensitivity/atlas` at `main` (0.10.67). Build it yourself in a throwaway
  worktree (`git worktree add .claude/worktrees/r3-review main` — detached is fine; `npm ci && npm run duckdb:fetch-ext
  && npm run build`; `npx vite preview --port 4461 --strictPort`), then shoot EVERY state with
  `ATLAS_URL=http://localhost:4461 OUT=/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/shots node scripts/eyes-shots.mjs` (both viewports, dark theme) and
  ALSO the light ("paper") theme for the main states (drive `?theme=paper` or the toggle with a small Playwright script
  — read `scripts/eyes-shots.mjs` for how states are driven) and the `report.html` page. Read every PNG. Also read
  `docs/usability.md` (the round-2 assessment) so you do not repeat what is already decided, and the round-3 plan
  `/Users/bbest/Github/MarineSensitivity/workflows/.claude/plans_todo/2026-09-25 atlas app plan, round 3.md` — its items
  A1–A3, B1–B17 and the Layers-pane redesign + a Download menu are ALREADY being built this round; do not re-list them.
- The reference Ben likes: CalCOFI explore, `/Users/bbest/Github/CalCOFI/explore` (build + preview it on port 4462 the
  same way if it runs — `npm ci && npm run build && npx vite preview --port 4462`; if it needs data it cannot reach,
  read `src/style.css`, `src/ui.tsx`, `src/panels.tsx` instead) — for the shape of controls, density, dropdowns, pills,
  export menus, tooltips, empty states.

## What to judge (consistency first, then functionality)
1. Consistency across the shell, the Scores lens, the Species lens, Places, Table, Flower, Report and the modals:
   control shapes (pill vs rect vs segmented), font sizes per surface, spacing scale, icon sizes, label casing
   (Title Case vs Sentence case vs lowercase), tooltip presence, focus rings, panel headers, the phone sheet vs the
   desktop panel showing the SAME controls the same way, dark vs light theme parity (anything that only looks right in
   one), hover/pressed states, disabled looks.
2. Functionality: anything that does not work, misleads, or dead-ends — a control with no visible effect, a state
   that cannot be reached back from, a phone gesture conflict, an unreadable value, a truncated label with no title,
   an empty state without a message, a loading state that lies, a link that loses view state, keyboard traps.
3. Copy: wording that a BOEM/NOAA reviewer would trip on; inconsistent names for the same thing (e.g. "Program
   Areas" vs "Program areas" vs "planning areas", "Study area" vs "region", "score" vs "Overall score").

## Output — `/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/review/review.md`
A numbered list `UI-1 … UI-n`, most valuable first, each: what (one line), where (component/file if you can find it
with grep — `src/…`), evidence (the shot filename), fix (one or two lines, concrete), effort (S/M/L), and whether it
touches `src/shell/` or `src/lib/ui/` (which forces the shell e2e run). Group into "do this round" (S/M, clear win) vs
"later" (L or a product decision). Aim for the 15–30 findings that matter, not a hundred nits. End with a five-line
summary of the app's overall consistency state. Do not edit any source file; this is a review.
