# R4-E — docs sweep (Haiku; no version bump)
Worktree `/Users/bbest/Github/MarineSensitivity/atlas/.claude/worktrees/r4-e`, branch `r4-e-docs`. Cut from `main`
after R4-A…D are merged. No code, no tests, no e2e.

Source of truth: the four `# atlas 0.10.80` … `0.10.83` entries at the top of `CHANGELOG.md`, and
`/Users/bbest/Github/MarineSensitivity/MarineSensitivity.github.io/branding/control-grammar.md`.

Bring these into line with them, changing only what the round changed:
1. `docs/design/spec.md` §5 (tool rail → the spine: four tools, attached to the panel, left by default, Table at
   full stage, Report one flow) and a short "Control grammar" subsection that links the branding page.
2. `docs/status.md`: the round-4 rows.
3. The parity page rows that mention the rail, Flower plot, Places or the popup histogram.
4. `tests/GATES.md`: the fault tally line.
5. `CLAUDE.md`: every "rail is three tools / Layers, Table, Report" statement; nothing else.

Gate: `npm run format` then `npm run format:check` and `npm run lint`. Commit on your branch. Report: files changed,
one line each.
