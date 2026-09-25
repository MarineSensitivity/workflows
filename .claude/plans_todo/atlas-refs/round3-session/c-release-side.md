# C — release-side items (workflows + msens), no browser
REPORT_DIR=`/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/dc8aa535-3c38-4bac-8b85-7db3d8f00672/scratchpad/reports/c`. Repos: `/Users/bbest/Github/MarineSensitivity/workflows` (dirty tree with v10 work —
touch ONLY the files named here, commit ONLY those paths explicitly, never `git add -A`), `/Users/bbest/Github/
MarineSensitivity/msens` (checkout is on branch `atlas-contract-fixes` with UNCOMMITTED density work — do NOT touch
that checkout: `cd msens && git worktree add .claude/worktrees/r3-c -b r3-c-app-zones-bbox main` and work there; never
`devtools::install()` it — use `devtools::load_all()`/`devtools::test()` in the worktree; the orchestrator decides when
it is installed). Read `workflows/CLAUDE.md` (the app-bundle section) and `../CLAUDE.md` (NEWS.md + tests rules) first.

Items from `workflows/.claude/plans_todo/2026-09-25 atlas app plan, round 3.md` §C:

- **R3-C2** v7's composite layer is labelled by the bare key `score`. Find where curated layer labels enter
  `build_version_manifest.qmd` (chunk `manifest`, ~line 191: "layer presentation metadata (order/category/short label)"
  — trace the CSV it reads; the plan calls it `data/layers_v7.csv` but no such file exists — find the real one, e.g. by
  grepping the notebook for `read_csv`). Set v7's composite short label to "Overall score" (and check v1–v7b use the
  same key). Render nothing that publishes; note the exact command to republish v7's manifest
  (`MANIFEST_…` flags per CLAUDE.md) in the report.
- **R3-C3** `msens::app_zones()` (R/app_bundle.R ~line 532) gains a `bbox` per zone row (`[w, s, e, n]` WGS84 from the
  zone-set geometry; dateline-aware — a zone crossing ±180 publishes `w > e`, i.e. the narrower frame, cf.
  `msens::lon_span`); `inst/schema/app_boot.schema.json` allows it (optional, 4 numbers); a testthat test with a tiny
  synthetic sf fixture incl. one dateline-crossing polygon; `build_app_bundle.qmd`'s R3 gate counts zones with a bbox
  (find the gate chunk); NEWS.md entry + `Version:` bump to 0.44.2 in the worktree. `devtools::document()` + tests green.
- **R3-C4** the AquaMaps citation text "as provided in this R package" — the atlas reads it from the bundle's dataset
  citations, which msens writes from … (find it: grep msens R/ inst/ data-raw/ and workflows for the phrase; if it
  originates in the `dataset` table of `sdm.duckdb`/`serve.duckdb` built by `build_registry.qmd`, fix THAT source and
  say which releases' tables would need a rebuild). Fix the source wording to "as provided in the msens R package".
- **R3-C5** publish hygiene: find `PUBLISH_PLAN.md` (grep both repos; if it does not exist, the note goes in
  `workflows/CLAUDE.md`'s app-bundle section): after every `render_app_bundle.sh` run commit
  `data/manifests/build_app_bundle.json` + the per-version HTML BEFORE merging a notebook branch.
- **R3-C1** (assess only — do not run): registering the legacy releases' `rng_iucn` assets. Read `publish_native.qmd`
  and say whether it can run for v2–v7b as is (what it keys on, what it would write to `native_asset`/`model_asset`,
  the runtime and machine, the exact flags), or what would have to change. Write the plan in the report.

Also record Ben's 2026-09-25 decisions in the round-3 plan file (edit in place, a "Decisions 2026-09-25" line under
each): A1 = (a)+(c); A2 = lower-48 waters + a sliver of Alaska; A3 = re-hue Fish teal, Turtle moss; A4 = keep the
official BOEM wording (no change); A5 = accept as is; A6 = no action.

Commit workflows changes on a branch `r3-c-release-side` (explicit paths only) and msens in the worktree branch; do not
push. Report: model id line 1, per item what changed / commands to run later / what you could not do.
