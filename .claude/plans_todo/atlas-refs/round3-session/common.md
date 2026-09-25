# Round 3 — common brief for every atlas build agent (2026-09-25)

You are building ONE slice of round 3 of the MarineSensitivity Atlas app (`/Users/bbest/Github/MarineSensitivity/atlas`,
Svelte 5 runes + Vite + TS, no SvelteKit). Read `atlas/CLAUDE.md` FIRST (all of it — the rules and the round-2 lessons are
binding), then your own brief. The orchestrator merges; you never touch `main`.

## Worktree + ports (mandatory)
- `cd /Users/bbest/Github/MarineSensitivity/atlas && git worktree add .claude/worktrees/<NAME> -b <BRANCH> main`
  (the shell cwd resets between Bash calls — use absolute paths or `cd` inside every command).
- In the worktree: `npm ci && npm run duckdb:fetch-ext` (a fresh worktree has no node_modules and no self-hosted DuckDB
  extension mirror; `vite build` needs both).
- NEVER use port 4331, 4380, 4386 or 4401 (the shared defaults). Use YOUR assigned range only:
  `PW_PORT=<port>` for `npx playwright test`, `--port <port>` for `vite preview`. Kill only the PIDs you started
  (never `pkill -f "vite preview"` — other agents run their own servers).
- Long commands: run them in the background writing to a log + an exit-code file, then poll the file
  (`(cmd > log 2>&1; echo $? > exit.txt) &`), never a text-only wait.

## What every slice ships
- Code + tests. Every non-trivial rule = an exported function under `src/lib/` (or the lens's own `*.ts`) with a vitest
  test; a `.svelte` component only calls it.
- `CHANGELOG.md`: your entry under a heading for YOUR reserved version (`## 0.10.NN`) at the top; `package.json`
  `version` + BOTH `package-lock.json` version fields (top-level and `packages[""]`) to the same number.
- ONE seeded fault of your own: a small `tests/faults/<id>.patch` (cut with `git diff HEAD -- <one file>` from an
  otherwise clean tree, then reverted) + its entry in `scripts/test-faults.mjs` FAULTS + a row in `tests/GATES.md`;
  prove it with `node scripts/test-faults.mjs --only <id>` (ONE id per run, `TMPDIR` exported) — the gate must go red
  with the patch and green without. Confirm the diff applied before claiming red.
- Copy changes: `grep -rn "<old wording>" e2e tests src` and update every spec/test that used it.
- Any change under `src/shell/` or `src/lib/ui/`: run `npx playwright test --project=chromium --workers=1
  e2e/shell.*.spec.ts e2e/feedback.spec.ts` locally (PW_PORT set) before you report.
- Gallery-rendered components (anything under `src/lib/ui/` the gallery shows — RailButton, Segmented, Switch, Flower,
  Legend, Panel…): regenerate the darwin baselines `PW_PORT=<port> npm run e2e:gallery -- --update-snapshots`, then
  LOOK at the PNGs (Read them) and say what changed. Linux baselines come from CI later (orchestrator).

## Gate (run it ALL before reporting; paste the numbers)
```
npx vitest run            # unit
npm run check             # svelte-check
npx tsc --noEmit
npm run lint && npm run format:check   # run `npm run format` first, then format:check
npm run build && node scripts/size-budget.mjs
node scripts/check-dist-session.mjs dist && node scripts/check-relative-assets.mjs dist \
  && node scripts/check-duckdb-ext.mjs dist && node scripts/check-inlined-tokens.mjs dist
node scripts/test-faults.mjs --only <your-fault-id>
PW_PORT=<port> npx playwright test --project=chromium --workers=2 <the specs you touched + shell/feedback if applicable>
```
CI will run the three-engine suite, verify matrix, gallery and faults on push; you run chromium locally.

## Eyes-on (mandatory — a green gate is not evidence the screen is right)
```
npm run build && (npx vite preview --port <port> --strictPort > preview.log 2>&1 &) ; sleep 3
ATLAS_URL=http://localhost:<port> OUT=.tmp/eyes ONLY=<comma list of states> node scripts/eyes-shots.mjs
```
Read `scripts/eyes-shots.mjs` for the state names (map, layers, species, table, flower, programarea, report…). Read
(view) every PNG your change affects at BOTH viewports, and describe what you see in your report. Fix what is wrong,
then reshoot. Copy the final PNGs to `<REPORT_DIR>/shots/` (named `phone-…png` / `desktop-…png`).

## Commit + report
- Commit on your branch with a clear message (lowercase summary line, body = what/why, plan item ids `R3-…`). Do not
  merge, rebase or push. Leave the worktree in place.
- Write `<REPORT_DIR>/report.md`: model id on line 1 (`claude-…`), branch + worktree path + HEAD sha, what you changed
  (file list), decisions you made where the brief left room, the gate numbers (each command → pass/fail + counts),
  the seeded-fault proof (red/green output lines), the eyes-on findings per shot, anything you could NOT do and why.
  The orchestrator reads only this file and the shots — put everything there.
- Two fix rounds per problem, then stop and report the blocker. Never expand scope beyond your brief; if you notice an
  adjacent defect, list it under "noticed, not fixed".

## Reference: CalCOFI explore (the look Ben likes)
`/Users/bbest/Github/CalCOFI/explore/src/` — `ui.tsx` (`Menu`: a pill button with `aria-haspopup="menu"`, a `role=menu`
list, Esc/outside-click close, `align` left/right), `layers.tsx` (data layer: on/off, opacity slider, ramp `<select>`
with a `.ramp-strip` gradient preview; one compact row per layer), `ramps.ts` (`rampCss()` = a `linear-gradient` of the
stops), `export.ts` (`saveBlob`, footer stamp with title · release · URL, PNG at 2× on a canvas, SVG with a text
footer), `capture.ts` (whole-view figure). Borrow the SHAPE, not the code; this app has its own tokens (`src/lib/brand/
tokens.css`), `Popover.svelte`, `Icon.svelte` (`scripts/icon-map.json` + `npm run` `scripts/build-icon-paths.mjs` to add
an mdi icon), `Select.svelte`, `Segmented.svelte`, `Switch.svelte`.
