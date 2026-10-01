# Round 4 — common brief for every atlas build agent (2026-09-30)

You build ONE package of round 4 of the MarineSensitivity Atlas (`/Users/bbest/Github/MarineSensitivity/atlas`,
Svelte 5 runes + Vite + TS). Read `atlas/CLAUDE.md` (its Rules and Round-2 lessons bind you), then your own brief.
The orchestrator merges and reviews; you never touch `main`, never push, and never spawn agents.

Round 4 in one paragraph (owner decisions, locked 2026-09-30): the tool rail attaches to the panel's outer edge and
becomes four tabs, **Layers · Details · Table · Report**; the panel docks **left** by default; the header keeps only
swap-side and collapse (dock-bottom and maximize retire); Table always takes the full stage; Report is one flow with
no sub-tabs; the histogram moves from the click popup into the legend. Control grammar (binding for every package):
a gold pill **switch** (`Segmented.svelte`) changes the DATA; underline **tabs** (`Tabs.svelte`, new in R4-C) change
the VIEW inside one surface; the **spine** (the rail) changes the SURFACE. Full text:
`/Users/bbest/Github/MarineSensitivity/MarineSensitivity.github.io/branding/control-grammar.md`.

## Worktree + ports
- Your worktree and branch already exist (your brief names them). In it: `npm ci && npm run duckdb:fetch-ext`.
- `export TMPDIR=<worktree>/.tmp/tmp` (mkdir it) in every command that runs Playwright or `test-faults.mjs`: the
  system TMPDIR is not writable here.
- Use ONLY your port range: `PW_PORT=<port>` for Playwright, `--port <port> --strictPort` for `vite preview`. Never
  4331/4380/4386/4401. Kill only PIDs you started.
- Long commands run in the background writing a log + exit-code file (`(cmd > log 2>&1; echo $? > exit.txt) &`);
  poll the exit file, then read `tail -30` or grep the failures. Never read a whole log.

## What every package ships
- Code + tests. A non-trivial rule is an exported function under `src/lib/` (or the lens's own `*.ts`) with a vitest
  test; a component only calls it.
- Your reserved version: a `# atlas 0.10.NN` entry at the top of `CHANGELOG.md` (user-facing, short),
  `package.json` `version`, and BOTH `package-lock.json` version fields. Do not edit other docs (R4-E does that).
- ONE new seeded fault (your brief names it): `tests/faults/<id>.patch` + its `scripts/test-faults.mjs` FAULTS entry
  + a `tests/GATES.md` row; prove it red with `node scripts/test-faults.mjs --only <id>` (one id per run). After all
  edits run `npm run faults:check` and regenerate any patch that no longer applies.
- Copy or label changes: `grep -rn "<old wording>" e2e tests src scripts` and update every user.
- Size: the static path has 4.7 KB of headroom (445.3 of 450 KB gzip). Report the static / worker / combined numbers
  from `size-budget.mjs`. Anything not needed for first paint is a dynamic `import()`. Over 448 KB: stop and report.
- Stay inside your brief's "Owns" list. A file outside it that must change: make the smallest edit and list it.

## Gate (lean; run once per fix round, nothing wider)
```
npx vitest run && npm run check && npx tsc --noEmit
npm run format && npm run lint && npm run format:check
npm run build && node scripts/size-budget.mjs
node scripts/check-dist-session.mjs dist && node scripts/check-relative-assets.mjs dist
npm run faults:check && node scripts/test-faults.mjs --only <your-fault-id>
PW_PORT=<port> npx playwright test --project=chromium --workers=2 <the specs your brief names + any you touched>
PW_PORT=<port> npm run e2e:shell        # once, at the end, if you touched src/shell/ or src/lib/ui/
```
No webkit/firefox, no full `npm run e2e`, no full `npm run test:faults`, no `--repeat-each`. CI runs those.
Gallery-rendered components: regenerate darwin baselines for the touched sections only
(`PW_PORT=<port> npm run e2e:gallery -- --update-snapshots`, then `git checkout` every PNG of a section you did not
touch). Linux baselines come from CI later.

## Eyes-on: shoot, do not look
`npm run build`, start `vite preview` on your port, then
`ATLAS_URL=http://localhost:<port> OUT=.tmp/eyes ONLY=<states your brief names> SHEET=1 node scripts/eyes-shots.mjs`
(`SHEET=1` exists once R4-0 is merged; without it just shoot). Do NOT open the PNGs: report the paths and any
`WARN`/`-MISSED` lines. The orchestrator looks.

## Commit + report
Commit on your branch (lowercase summary, body = what/why, id `R4-x`), explicit paths staged. Your final message IS
the report, at most 40 lines, in this order: line 1 your model id (`claude-…`); branch + HEAD sha; per brief item:
done / not done + file; decisions you made where the brief left room; gate results (counts, the three size numbers);
fault proof (the red line); shot paths; files touched outside "Owns"; "noticed, not fixed". Two fix rounds per
problem, then stop and report the blocker.
