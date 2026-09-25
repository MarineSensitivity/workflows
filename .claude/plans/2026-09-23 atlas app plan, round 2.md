# Atlas app plan, round 2 — usability first, then finish (2026-09-23)

**Supersedes for execution:** `plans/2026-09-20 atlas app plan.md` (round 1; moved from `plans_todo/` at this
hand-off, kept verbatim as the record of decisions D1–D17, the ground truth, the architecture and the
fiddly-bits list). Its finished subplans (`atlas-0` … `atlas-7`) are in `plans_done/`; the two unfinished ones
(`atlas-8`, `atlas-9`) stay in `plans_todo/` with their progress logs and are re-scoped by this plan.
The spec folder `plans_todo/atlas-refs/` stays where it is (both unfinished subplans cite it).

**These are Ben's questions for the NEXT orchestrator, not answered here** (Ben, 2026-09-23: "I don't want
you to tackle these questions … pass them off to the next orchestrator"). Section 5 states them as goals,
section 6 as the decisions they need, section 10 is the file-anchored survey of CalCOFI explore they refer to.

**Why a new plan.** Round 1 built the app: eight phases closed, the app is live on Pages, every gate has a
seeded fault. The orchestration session that ran it grew long (a ~4 h model-side stall on 2026-09-23, an
orchestrator context near its limit) and Ben wants a restart on **Opus 5.5** with a fresh, shorter plan whose
first move is a **comprehensive usability assessment** of the live app — the round-1 checklists proved parity
with the Shiny apps, not that the result is pleasant to use. Ben's own first pass (2026-09-23): "Report" and
"Take a Tour" are unfinished; panels do not expand to full screen, are not resizable or movable; "About this
release" and "Report a problem" sit oddly bottom-left; feedback should be CalCOFI-explore's screenshot +
annotation form → Google Sheet + email + GitHub issue (and the same in `docs/`); dark theme by default with a
sun/moon toggle; CalCOFI's stackable background layers and colour ramps are worth borrowing (which changes what
the "Layers" icon means); the hexagon tool rail and the top-left logo do not feel on-brand; the light theme's
slate grey should be an on-brand yellow.

## 1. State at hand-off (2026-09-23 16:30 CEST)

| what | where | value |
|---|---|---|
| atlas `main`, pushed | GitHub `MarineSensitivity/atlas` | `a577618` (0.10.17): CI green, all 8 jobs incl. the new `verify` job; **Pages serves 0.10.17** with `docs/parity.html` |
| atlas `main`, PUSHED at close-out | GitHub | `033bf7c` = 0.10.19 + the `verify` matrix fix (`2177f57`) + a prettier fix + the CLAUDE.md plan path; gate script green (vitest 2716 — the GAA coverage test is a 5 s wall-clock test that reds at load > 30 and passes alone 79/79; check; tsc; lint; format; size; dist invariants; test:faults 9/9; verify 174/174; outlines ×3 engines 9/9). CI running at hand-off. |
| atlas `main`, PUSHED last (final state) | GitHub | **`9c614db` (0.10.20)** = `033bf7c` + the Firefox/basemap fix `6f62499`; my fault (drop the reactive basemap input) reproduced the exact pixel `247,171,122` → red; full gate script green: vitest 2723 · check · tsc · lint · format · size PASS · dist invariants · test:faults 10/10 · verify 174/174 · Playwright three engines **549 passed / 0 failed** · gallery 58/58. CI on `033bf7c` was green (Pages = 0.10.19); CI on `9c614db` running at hand-off → Pages 0.10.20. (previous local-only row kept for history:) `e178f52` (0.10.19) = `a577618` + a11y fix round (`f806884`, 0.10.18) + G-23/24/25 (`e178f52`, 0.10.19). Not pushed because `node scripts/verify.mjs --engines=chromium` is 166/174: the 8 `out=none`/`out=ecoregion` scores states assert a rendered zone LINE, which the G-25 fix (Sel.out finally read) made correctly absent. Fix in flight (below). Everything else on the merged tree is green: vitest 2716, check, tsc, lint, format, size 417 KB static / 140.5 KB worker, dist invariants, `test:faults` 9/9 red, gallery 58/58, Playwright 542/1 (the 1 = a Firefox timing test at load 50). |
| ~~in flight~~ LANDED, branch `worktree-agent-a90f8d560c7da6fab` @ `2177f57` | merged into `033bf7c` | `verify.mjs` scores `proj/out/area` family asserts the raster painted + line features > 0 only for `out=programarea`, 0 for `none`/`ecoregion` — not a map bug: every release publishes exactly ONE `boot.units` row (D17; parity page ID-03), so `out=ecoregion` has nothing to outline. 174/174. |
| in flight, branch `worktree-agent-a40c94d9e5ceb5f72` @ `98c64b7` (WIP on e178f52) | atlas worktree | **Ben's desktop bug** (0.10.21) — **NOT fixed yet at hand-off**: fix 2 (raster-source `bounds` per grid) is DONE and unit-tested (`src/lib/map/layers/raster.ts#rasterBoundsForGrid`: usa05 → `[-180, -17.7, 180, 82.6]` — longitude kept full because MapLibre's `TileBounds.contains()` has no wraparound, a literal 141.1→-63.75 box would blank the raster; global05 → the globe; `tests/map/raster.test.ts`); fix 1 is STARTED: `src/lens/scores/state.svelte.ts` (`createScoresLens`, mirrors species) exists but `Shell.svelte` still mounts the panel-bound `ScoresLens.svelte` (untouched). Remaining, in the agent's words: dynamic-import the state keyed on `sel.lens === "scores"`; route both `composeStyle` call sites through it and make the "scores-only zones/overlays/selection" gating explicit (today incidental on the lens not being mounted — species would otherwise leak scores' zones); `ScoresLens.svelte` reads a `lens` prop instead of `bind:mapExtra`; the collapsed-panel e2e (localStorage key `atlas.panel.shell.desktop` = `{"collapsed":true,"detent":"half"}`, red first); seeded fault; then the bump. **Same-class candidate bug found**: `src/places/Places.svelte` ~line 403 restores a selected place's outline in an `$effect` that runs only while the Places tool is mounted → a deep-linked `sel=place:n` outline may not render unless that panel is open (unconfirmed by test). Original cause: no score raster on desktop because `Panel.svelte:87` renders children only when not collapsed and `Shell.svelte` mounts `<ScoresLens bind:mapExtra>` inside the panel body → collapsed desktop panel = lens never mounted = `raster: null`; the phone `Sheet` always renders its body. Fix: scores map inputs move to lens-level state (like `src/lens/species/state.svelte.ts`), e2e with a pre-seeded collapsed panel asserting painted pixels (red first), seeded fault; plus raster-source `bounds` per grid so MapLibre stops requesting the z≤3 tiles titiler 404s. |
| ~~in flight~~ LANDED, branch `worktree-agent-a55f993ca819b9669` @ `6f62499` | merged into `9c614db` | **Firefox raster-probe race — ROOT-CAUSED, FIXED, merged** (0.10.20, Opus 5): it was (a) a real app bug, and not in the raster — **the CARTO basemap could silently never paint**: since 0.10.11 `composeStyle()` reads the CARTO style from a module cache synchronously and nothing invalidated that read when the fetch resolved after the last reactive change (a slow connection = a permanently basemap-less map; the failing pixel `247,171,122` = the raster at 0.6 over the flat `--surface-map`, not over the basemap). Fix: `warmBasemapStyles()` reports each theme's load as a reactive input to `composeStyle` (`basemap.ts:171`, `Shell.svelte:435/462`), and `styleQueue.ts` rewritten around a settle cycle (at most one `setStyle` in flight — the old queue only engaged while `!isStyleLoaded()`, trivially true after the blank first style, so two applies could be issued back to back: the "MapLibre mis-ordering" 0.10.11 worked around by NOT recomposing). Measured: 40 repeats firefox before 38/40 (no contention) and 37/40 (2 busy cores); after 40/40 and 40/40. New deterministic e2e gate (`scores.firstpaint.spec.ts:452`, `routeBasemapStyle(page, {styleJsonDelayMs: 3000})`), 3 styleQueue regressions (2 red without the fix), seeded fault `basemap-not-reactive` (test:faults 10/10). Gates: tsc, check, vitest 2721 (+1 load flake in `tests/geo/coverage.test.ts`), lint, format, size 417.4 KB, the three map specs ×3 engines 81/81. NOT run: the full three-engine suite (`map.spec.ts` theme switch, `report.spec.ts`) — do that before merging. Original symptom: `e2e/scores.firstpaint.spec.ts:319` "paints the score raster at two ocean probe points" red on Firefox only, under contention (3 of the last 5 local three-engine runs; 1-in-3 at repeat-each 3 at load 10–34; 45/45 isolated; green on every CI run). Measuring ×20 with/without contention, instrumenting `setStyle`, classifying app bug / render lag / test probe; 0.10.10 already fixed one race of this class in `src/lib/map/styleQueue.ts`. |
| msens / workflows | branch `atlas-contract` in `atlas/.claude/worktrees/contract/{msens,workflows}` | msens 0.43.0 `7d8c6c0` (eleven rounds), workflows `9cc3a12c`; NOT merged to their mains (NAMESPACE hygiene; needs Ben + a release session). Eleven `app/` bundles + v7/v7b `serve/cell_model/` published to S3 2026-09-23 (`APP_BUNDLE_S3=1`, verified anonymously); `latest.txt`, `versions.json`, `manifest.json`, `tables/` never touched. |
| server | branch `atlas-preview` @ `8461a24` (worktree `atlas/.claude/worktrees/preview/server`) | preview host: sidecar + Caddy routes + tests built; **not deployed** (Ben's call: `DEPLOY_CADDY=1` after `chown /share/atlas_preview`). |
| heartbeat | CronCreate job `a5a1e23f` (session-only) | dies with the session; the new session sets its own if wanted. |

**Standing rules carried over unchanged** (from round 1's orchestration prompt): the orchestrator never writes
feature code; Sonnet builds, Opus (now Opus 5.5) does twin-rule code and every Review checklist; verify gates
yourself with your own seeded fault (confirm the diff applied); merge `--no-ff` with gate numbers in the body;
at most two fix rounds per step then stop; **never publish/deploy in the same turn as the code that does it,
only under the named flag** (`APP_BUNDLE_S3`, `DEPLOY_ATLAS`, `DEPLOY_CADDY`, `DEPLOY_ACCESS`, `CHECK_PREVIEW`);
**never `PROMOTE_LATEST`, never write `latest.txt`/`versions.json`; the server renders only through
`scripts/srv_render.sh`**; pushing atlas `main` (→ Pages) is fine once the local gate script is green.

## 2. Closed in round 1 (what the app does today)

| phase | closed | result |
|---|---|---|
| A0 scaffold + spikes | 2026-09-21 | Vite/Svelte 5/TS, pins by spike verdict (S1–S4), size budget + dist invariants, CI on push |
| A1 data contract + `app/` bundles | 2026-09-23 (published) | `msens::app_bundle_build()`; `boot.json`, taxa shards, `cell/tile=*` Parquet for v1–v9, v4b, v7b |
| A2 core runtime | 2026-09-22 | release gate (D6, inline twin + shared case table), URL-is-the-view codec (`g1`), grid twins, DuckDB-WASM engine, OPFS store (logic built, **not wired**), coverage math at 1e-9 |
| A3 design system + shell | 2026-09-21 | tokens, gallery, brand fonts, shell (rail, panel/sheet, topbar, theme pre-paint), axe + CLS gates |
| A4 scores lens | 2026-09-23 | rasters via titiler, zone choropleth (D17: program areas / planning areas only), zones table, flower, composition treemap (one level), legend, cell/zone click popup |
| A5 species lens | 2026-09-23 | search (combobox as of 0.10.18), taxon shards, ranges, US-only switch, click value via `/cog/point` |
| A6 places | 2026-09-23 | select / draw / coordinates / upload (shp, kml, gpx, geojson, fgb; gpkg lazy), share-link ladder, D7/D7b scoring |
| A7 report | 2026-09-23 | `report.html` print-first, `buildReport()` = R fixtures at 1e-9, exports print/HTML/ZIP/DOCX, running footer, watermark on restricted |
| A8 verification | steps 1, 2, 3 (+ fix round), 4, 5 + D4 done | gate inventory + `test:faults` (9 faults), 174-state matrix (`verify.mjs`, now in CI), axe on 179 states, keyboard walk 18/18 ×3 engines, `docs/accessibility.md` (partially supports; no VoiceOver/NVDA driven), `docs/parity.html` (72 lines: 45 done / 21 partial / 3 intentional / 3 deferred; 14 screenshot states; 18 intentional differences; 25 known gaps), "Report a problem" GitHub-issue link, CI browser suites on linux (Firefox headed under xvfb, gallery linux baselines, CI cold-load budget 4000 ms) |
| A9 part a | built 2026-09-22 | preview host (server branch `atlas-preview`), not deployed |
| Ben's three screenshot rounds | 2026-09-23 | CARTO vector basemaps (no watermark), legend ticks + floating Scores legend + search out of the pill, themed popup + Scores click "Cell {id} · lon, lat · {layer}: {value}" from Parquet |

## 3. Open from round 1

### 3a. atlas-8 (in `plans_todo/`, re-scoped here)
- The three in-flight branches above (verify matrix for `out=`; desktop raster/panel; Firefox race).
- **Deliverable 5 — performance notes**: `docs/performance.md` has the CI runner table (three medians: 3281 /
  1906 / 3445 ms, 1.8× spread; `CI_BUDGET_MS` 4000 with 16 % headroom) but not the "three slowest states with
  their cause" section. One Sonnet round.
- **Phase Opus review** (checklist in the subplan) never ran; run it against `main` once the three branches land.
- `docs/parity.html` lists G-23/24/25 as open though fixed; regenerate (`npm run parity:shots` is a live step)
  after each merged fix; the page still needs **Ben's signature** before A9 part b.
- Accessibility follow-ups stated in `docs/accessibility.md`: nothing points a screen-reader user at the zones
  table before the map (#9 is done: `aria-describedby`); real VoiceOver/NVDA session never driven.

### 3b. atlas-9 (in `plans_todo/`)
- Deliverable 5: `release_marine-atlas.qmd` `DEPLOY_ATLAS` / `CHECK_PREVIEW` chunks incl. the `/{ver}/atlas/`
  probe and the public host's 302 for restricted `/{ver}/atlas/`. Deliverable 6: `msens::product_urls()` atlas
  entries. Both are patches for the release session / Ben (flags).
- Deploy of the preview host (Ben). Part b cutover (`/scores`, `/species` → the atlas) **only after Ben signs the
  parity page.**

### 3c. Data defects for msens (atlas-1 hand-offs; each needs a msens fix + `APP_BUNDLE_S3` re-publish)
- Zone rows carry no `name` / `label_pt` (app shows "GAA" where a name belongs; no zone labels).
- Shard bboxes not minimal-span, v7 has none (lens compensates client-side; a v7 species frames the study area).
- v8/v9 publish two primary-producer keys; lens de-duplicates.
- Nine manifests mislabel the subregion zone set; v8 `zone_taxon` has no `AT` rows (cosmetic under D17).
- `manifest.json` `app{}` patches in `workflows/_output/app_bundle/patches/` not applied (release session).
- `atlas-contract` → msens/workflows main merges (NAMESPACE hygiene).

### 3d. Fiddly bits (the round-1 list, grouped; verbatim entries remain in the round-1 plan)
- **Map chrome not built**: nav/fullscreen/scale/geocoder, outlines control, globe minimap, OBIS overlay,
  driver.js tour — and the a11y question of buttons inside `#map[role="img"]` (decide once: toolbar outside
  the map, or a labelled region).
- **OPFS store not wired**; upload parsers on the main thread; treemap one level; places list "—" for geometry
  places; `noSecondMapCopy` scan too narrow; `VITE_SEAL=1` breaks offline-HTML self-containment; refusal-copy
  test can pass a useless message; `?theme=navy|paper` vs `dark|light`.
- **CI/tooling**: `test:faults` ~11 min (split); `verify.mjs` chromium-only in CI (teach it the headed-Firefox
  recipe); Playwright stale-server trap (build-hash health URL or free port per run); Firefox teardown race.
- **Public-host `session.json` console 404** (D6 by design; probe only when a candidate is restricted, both
  copies). `share-modal.js` errors are a browser extension, not `dist/`.
- **Self-hosted basemap** (PMTiles on S3) as the long-term answer to the CARTO dependency; a CDN in front of the
  bucket for tile egress; D6 "Later" data protection for restricted releases.

## 4. Findings and troubles worth carrying (so round 2 does not repeat them)

Orchestration
- **The gate script is the gate.** `scratchpad/gates/*/run.sh` (vitest · check · tsc · lint · format · build ·
  size · dist invariants · test:faults · verify · three-engine Playwright on a private port · gallery). A
  hand-picked subset for a "docs-only" merge shipped `tsc` errors to CI (0.10.17). Every agent brief lists
  `npm run check` + `npx tsc --noEmit` because vitest does not type-check.
- **Agents get cut off by text-only waits** (stream watchdog). Every brief: WIP-commit first; long runs in a
  background script writing an exit-codes file; poll with short Bash calls. Resume a stalled agent with
  SendMessage (context intact) rather than respawning.
- **Worktrees are cut from the repo of the CURRENT cwd and from `origin/main`**: `cd` to the atlas checkout
  before spawning; tell the agent `git merge --ff-only main` when local main is ahead.
- **A fresh worktree lacks `public/duckdb-ext/`** (gitignored): `npm run duckdb:fetch-ext` first, or every
  engine-backed spec fails with "no value" and looks like load flake.
- **Never trust a browser red at load > 25, never refute a flake by re-running a few times**: measure
  (`--repeat-each`), read the assertion. Playwright on a shared port reuses another process's server (false
  green AND false red seen): always a private port (`PW_PORT` / a sed'd config).
- **Version bumps in parallel rounds**: reserve numbers per round in the brief; on merge keep every CHANGELOG
  entry with the highest on top and align `package.json` + both top `package-lock.json` fields.
- **Model note**: the Agent tool's `opus` alias ran as `claude-opus-5[1m]`; `claude-opus-5-5` is rejected by
  its enum. Ben wants Opus 5.5 for Opus-assigned work: set it on the session side (default subagent model),
  and have every Opus agent print its model id on the first line of its report.

Technical
- **MapLibre style races are real and load-widened**: 0.10.10 (stale queued style clobbering a direct apply),
  0.10.11 (an extra `setStyle` at network-timed moments; `composeStyle()` must stay synchronous), and the open
  Firefox probe. Keep the one-composed-style rule; instrument `setStyle` before guessing.
- **Headless Firefox on linux has no WebGL2** (run headed under xvfb with an explicit `FIREFOX_HEADED=1` that
  throws without `$DISPLAY`); `--project=timing` re-runs its `dependencies` unless `--no-deps`; gallery
  snapshots are platform-suffixed (linux baselines come from CI artifacts).
- **Gate rot happens silently**: `verify.mjs` imported a deleted export for hours because it was not in CI; a
  test asserting "zone line rendered" on every scores state was wrong the moment `out=` meant something.
- **WebKit tabs only to text fields** (Option+Tab = Playwright "Alt+Tab"); `document.fonts.ready` can hang on
  linux WebKit (await named faces); `fonts.check()` cannot fail.
- **Lens state must not live in panel-only UI** (Ben's desktop-raster bug): map inputs belong to a lens-level
  store the shell instantiates whenever the lens is selected; the panel renders UI only.

## 5. Round-2 goals (Ben, 2026-09-23) — in order

**U0 · Comprehensive usability assessment (first, before any feature work).** An Opus 5.5 walk of the LIVE app
(Pages, public v7; desktop 1280×800 and 1440×900, phone 390×844, tablet 1024×768; both themes; keyboard and
pointer), producing `docs/usability.md`: every screen/state with a screenshot, what a first-time user tries to
do and where it breaks or puzzles, ranked findings (blocker / major / minor / polish), each with the file that
owns it, and a proposed layout for the shell (panel model, chrome placement, tool rail, top bar). Must cover
Ben's items explicitly: Report flow end-to-end (select a place → "Report" → exports), "Take a tour" (absent:
no driver.js yet), panels (full-screen / resize / move / dock), "About this release" and "Report a problem"
placement, theme default and toggle, layers model, hexagon rail, logo, light-theme palette. Compare against
`../../CalCOFI/explore` side by side and against the Shiny apps for anything a returning user expects. Output
also feeds ⚑ decisions R1–R6 below. No code in this step.

**U1 · Shell layout: panels and chrome.** Panels expand to full screen, resize (drag edge) and move/dock
(left/right/bottom), remembered per viewport, keyboard-operable (a11y gates stay green: `keyboard-walk`,
`matrix.a11y`, `shell.cls`). "About this release" and "Report a problem" leave the bottom-left — Ben's
suggestion: an (i) button top-right and a "Send feedback" control (U3) in the top bar; U0 assesses, Ben decides (R2). Every layout state is chrome (localStorage), never
URL state; URL-is-the-view still holds for view state.

**U2 · Theme: dark by default; sun ⇄ moon toggle** (CalCOFI's), `?theme=` and the pre-paint script kept; the
light theme's slate grey replaced by an on-brand yellow so light and dark coordinate (Ben's ask; tokens only —
`src/lib/brand/tokens.css`; the hex-literal ban and contrast gates decide what is allowed). Resolve the
`navy|paper` vs `dark|light` naming once.

**U3 · Send feedback** (replaces "Report a problem", 0.10.16): CalCOFI-explore's screenshot + annotation modal
→ Google Apps Script endpoint → new Google Sheet for this repo + email notification + GitHub issue in
`MarineSensitivity/atlas` with the screenshot; the same feature on `../docs/` (the Quarto site). Keep the
zero-backend fallback (the prefilled-issue link) and the privacy rule (the URL fragment / drawn geometry never
leaves the device unless the user includes it deliberately — the screenshot is the user's choice). Apps Script
source checked in; a runbook for the Sheet/secrets; `VITE_FEEDBACK_URL` already exists.

**U4 · Layers model (borrow from CalCOFI):** stackable background layers (bathymetry, boundaries, labels,
OBIS later) with order/opacity, and colour-ramp choice per data layer (`ramps.ts` stays the single ramp site;
titiler `colormap_name`/`rescale` are already URL state). This changes what the "Layers" icon means: today's
panel is "the current layer's information + controls" — decide the split (R3).

**U5 · Tool rail and branding.** Rethink the hexagon rail (interlocking/stacked hexagons, or a different
arrangement — mockups in the gallery first, Opus judgment, Ben picks); new logo: a wave in a hexagon in the new
palette (SVG, both themes, favicon + report header + docs); the seal stays (D10).

**U6 · Finish Report and Tour.** Report: the end-to-end flow from the top bar (what "Report" does with no
place selected; place → report; exports; the offline-HTML seal issue), and the parity page's 4 partial R-lines.
Tour: driver.js (lazy chunk; the 450 KB static budget holds), steps for both lenses, "Take a tour" in the help
menu, `open_about`/tour analytics events.

**U7 · Remaining round-1 work**: 3a–3c above (verify/desktop-raster/Firefox branches; perf notes; phase review;
parity page regenerate + Ben's signature; atlas-9 D5/D6 patches; msens data defects + re-publish; OPFS wiring;
map chrome). **U8 · Cutover** (atlas-9 part b) after the signature.

## 6. Decisions for Ben (⚑) — ALL DECIDED by Ben, 2026-09-24 00:55 CEST (his words, from `docs/status.md` + `docs/usability.md`)
- **R1 DECIDED**: one dockable panel (left/right/bottom + maximize, drag-resize), not floating windows.
- **R2 DECIDED**: (i) About top-right popover; **"Feedback"** (not "Send feedback") in the top bar; both under ⋯ on the phone.
- **R3 DECIDED**: one Layers panel that IS the stack, the data row expanding into today's controls; **plus the ability to
  change the stacking of data layers (e.g. Program Areas, score) relative to map layers (e.g. place names, bathymetry)** —
  some belong on top of, some underneath, a semi-transparent data layer.
- **R4 DECIDED**: vertical labelled stack; the hexagon moves to the logo + active marker.
- **R5 DECIDED**: wave-in-hexagon mark; light palette candidate y1 (brand gold on paper + navy ring), y3 `#876700` as the
  drop-in fallback.
- **R6 DECIDED**: Sheet owner `ben@oceanmetrics.io`; notification emails `ben@oceanmetrics.io` + `timothy.white@boem.gov`;
  issue labels `bug` / `idea` / `question` / `data`; **skip the public GitHub issue (Sheet + email only) when
  `release.access === "restricted"`**.
- **R7 DECIDED**: cutover after the parity page signature.
- **R8 DECIDED (2026-09-24 02:05, from the parity audit M6)**: the composition treemap sizes boxes by SPECIES COUNT (Shiny parity);
  the suitability × risk × area measure stays available later as a labelled option.
- **R9 DECIDED**: the light theme keeps its LIGHT basemap (Shiny keeps a dark map) — recorded as an intentional difference;
  outlines get a darker stroke token on paper.
- **R10 DECIDED**: the species layer bar keeps the brand colours (gold merged / grey input vs Shiny green/orange) —
  recorded as an intentional difference.
Original questions kept below for the record.
- **R1** Panel model: one dockable panel (left/right/bottom, full-screen) vs. a floating window set? (U0 proposes.)
- **R2** Where "About this release" and "Send feedback" live (Ben's suggestion: top-right (i) + a top-bar button).
- **R3** The Layers split: a "Layers" stack (backgrounds + data layers, order/opacity/ramp) vs. the current
  per-lens "Layer info" panel — two icons, or one panel with two tabs?
- **R4** Tool rail: interlocking hexagons / vertical stack / horizontal top bar — pick from mockups.
- **R5** Logo + palette: approve the wave-in-hexagon and the yellow light theme from gallery mockups before
  tokens change (the contrast gate may force a different yellow than the brand's).
- **R6** Send feedback destinations: the new Sheet's owner account, notification email, issue labels.
- **R7** Cutover date, gated on the parity-page signature.

## 7. Phases, models, order

| # | scope | model (build / review) | depends on |
|---|---|---|---|
| R2-0 | U0 usability assessment → `docs/usability.md`, mockups for R1–R5 | Opus 5.5 / Ben | live Pages at ≥ 0.10.21 |
| R2-1 | land the three in-flight branches; perf notes; phase review; push | Sonnet; Opus 5.5 review | — |
| R2-2 | U1 panels + chrome placement | Sonnet; Opus 5.5 review (a11y) | R1, R2 |
| R2-3 | U2 theme + tokens; U5 logo + rail (gallery first) | Sonnet + Haiku tokens; Opus 5.5 judgment | R4, R5 |
| R2-4 | U3 Send feedback (atlas + docs) | Sonnet; Opus 5.5 review (privacy) | R6 |
| R2-5 | U4 layers model | Sonnet; Opus 5.5 review (one-style rule) | R3 |
| R2-6 | U6 Report + Tour | Sonnet | — |
| R2-7 | msens data defects + re-publish (`APP_BUNDLE_S3`, its own turn) | Opus 5.5 (twin rules) | Ben + release session |
| R2-8 | atlas-9 D5/D6, preview deploy (Ben), parity signature, cutover | Sonnet / Ben | R7 |

Gates unchanged: every change ships with a seeded fault; the gate script before every push; CI green before
Pages; the size budgets (450 KB static / 150 KB worker) — driver.js, html2canvas-class screenshot code and
any annotation library are lazy `import()`s, never on the static path.

## 8. Orchestration prompt (paste into the new session, started in `atlas/` with `--add-dir` on workflows, msens, server, apps, api, docs, MarineSensitivity.github.io)

> You are the orchestrator of `../workflows/.claude/plans/2026-09-23 atlas app plan, round 2.md`. Read it, then
> `plans/2026-09-20 atlas app plan.md` §Decisions and §Fiddly bits, then `plans_todo/atlas-8 …` and
> `atlas-9 …` progress logs, then `atlas/CLAUDE.md`. You never write feature code: Sonnet builds; **Opus 5.5**
> does twin-rule code, design judgment and every Review checklist (every Opus agent prints its model id on the
> first line of its report — tell Ben if it is not 5.5); Haiku only for mechanical transforms. `cd` to the
> atlas checkout before spawning an atlas agent (worktrees cut from the current cwd's repo and from
> `origin/main`; say `git merge --ff-only main` when local main is ahead). Every brief: `npm run
> duckdb:fetch-ext` in a fresh worktree; never port 4331 (a sed'd config or `PW_PORT`); background scripts +
> exit-codes files, never a text-only wait; `npm run check` and `npx tsc --noEmit` explicitly; reserved version
> number; CHANGELOG + `package.json` + both lock fields together; commit on the worktree branch, never merge to
> main. You verify with your own seeded fault (confirm the diff applied) and the FULL gate script
> (`scratchpad/gates/*/run.sh` shape — recreate it: vitest, check, tsc, lint, format, build, size, dist
> invariants, test:faults, verify chromium, three-engine Playwright on a private port, gallery), merge `--no-ff`
> with gate numbers in the body, push `main` when green (CI publishes Pages), append a dated line to the
> subplan's progress log. Two fix rounds per step, then stop and report. Never publish/deploy in the turn that
> wrote the code, only under the named flag; never `PROMOTE_LATEST`; never write `latest.txt`/`versions.json`;
> the server renders only through `scripts/srv_render.sh`. First action: R2-1 — only ONE branch is still in flight, `worktree-agent-a40c94d9e5ceb5f72` @ `98c64b7`
> (Ben's desktop-raster bug, half-built; see §1 for the exact remaining steps); land it, then R2-0.

## 9. Close-out of round 1 (done at this hand-off)
- `plans_todo/2026-09-20 atlas app plan.md` → `plans/`; `atlas-0` … `atlas-7` subplans → `plans_done/`;
  `atlas-8`, `atlas-9`, `atlas-refs/` stay in `plans_todo/`.
- Memory for the orchestrator updated (`atlas-plan-orchestration`): resume from THIS plan.
- Not done from round 1's own close-out list (it was gated on A9-B3, the cutover): `workflows/CLAUDE.md`
  "ONE app" section, `apps/README.md` retired note, the `app/` contract in the `bootstrap-release` /
  `publish-sdm` skills, the email to Tim. They move to R2-8.

## 10. What `../../CalCOFI/explore` does (survey 2026-09-23, read-only; file paths for the next orchestrator)

Stack: Vite + React 18 + TS, MapLibre GL + deck.gl `MapboxOverlay`, DuckDB-WASM worker, driver.js, html-to-image;
GitHub Pages via `.github/workflows/pages.yml` (env `VITE_FEEDBACK_URL`, `VITE_BASE`, …); docs at calcofi.io/docs/explore.html.

- **Send feedback** (U3): `src/feedback.tsx` (`FeedbackDialog`; endpoint = `VITE_FEEDBACK_URL`, local override
  `localStorage["explore.feedback_url"]`), `src/capture.ts` (`captureView()` via **html-to-image** `toCanvas` — html2canvas
  rejected for `color-mix()`; needs `preserveDrawingBuffer` on the map canvases, which the atlas already sets; fonts
  inlined by `src/brand.ts#fontEmbedCss()`; `fitBytes()` keeps the PNG under ~3 MB), `src/annotate.tsx` (hand-rolled
  canvas annotator: arrow/circle/rect/pen/text, three colours — no third-party lib). POST as `text/plain` JSON
  `{app, kind, label, title, link, datasets, url, release, viewport, theme, text, email, image, website(honeypot), user_agent}`.
  **The Apps Script is generated by R**: `/Users/bbest/Github/CalCOFI/calcofi4r/R/feedback.R` — `cc_feedback_header()`
  (Sheet columns) + `cc_feedback_script()` (Code.gs): PNG → Drive folder, row → Sheet tab `feedback`, mail to every address
  in tab `recipients` (+ the submitter), **public GitHub issue** with the screenshot committed to `feedback/<id>.png` on
  `main` via the Contents API (labels `feedback`; the email is never passed to the issue); script property
  `GITHUB_TOKEN` (fine-grained: contents + issues), optional `DRIVE_FOLDER_ID`; honeypot + `MAX_PER_HOUR` cache cap.
  Runbook: `README.md` "### The feedback endpoint (once)". Zero-backend fallback: "Open as GitHub issue myself"
  (prefilled link + screenshot copied to the clipboard). For the atlas: the same generator with a NEW Sheet and
  `repos = c(atlas = "MarineSensitivity/atlas")` (the msens package is the natural home for the generator twin), the
  privacy rule kept (fragment/geometry only if the user includes it), lazy `import()` for html-to-image and the annotator
  (size budget), and the same control on `../docs/` (Quarto: an include + the endpoint variable).
- **Theme toggle** (U2): `src/App.tsx` ~line 1214 renders the sun/moon button (inline MDI `brightness-7`/`brightness-4`
  paths, Apache-2.0); the click/persist logic lives in the external `calcofi.io/brand/v2/theme.js` (custom event
  `cc:theme`); pre-paint IIFE in `brand/v2.head.html` (URL `?theme=` → cookie if `cc_theme_src=user` → localStorage →
  default); `src/brand.ts` `DEFAULT_THEME`; CSS swap of the two icons on `:root[data-theme]` in `src/style.css`. The atlas
  has its own pre-paint script + `resolveTheme()` twin test; only the control and the default change.
- **Layers** (U4): `src/layers.tsx` (`LayersCard`: Data row with opacity + ramp picker; Sea floor (GEBCO relief/depth/
  contours); basemap labels on/off; "On the map" = draggable, reorderable list with ▲▼ keyboard buttons, per-row
  symbology; "Add a layer" grouped checklist from a registry). Registry shape `SpatialLayerDef`/`SpatialLayers` in
  `src/basemap.ts` (PMTiles sidecar, `VITE_SPATIAL_URL`). Order/opacity/colour = `LayerStyle[]` in the URL `layers=`
  (`src/state.ts`, defaults diffed so the URL carries deviations only). Ramps: `src/ramps.ts` — 22 hand-ported cmocean /
  viridis / GEBCO ramps as 11-stop hex arrays, `defaultRamp()` by variable-name regex, one shared selection rule for every
  lens; URL keys `ramp= data= datao= layers= bathy= bathyo= basemapo= basemap=nolabels land=off`. The atlas's `ramps.ts`
  is already the single ramp site; titiler `colormap_name`/`rescale` are already URL state.
- **Basemap** (already borrowed): `src/basemap.ts` also has island labels (`addIslandLabels()`), label toning/hiding
  (`toneBaseLabels()`), GEBCO 2025 raster-DEM PMTiles bathymetry (`color-relief` + hillshade + contours), an OSM land mask
  (`landLayers()`/`sinkOcean()`) so the ocean stack sits under CARTO's land/roads/labels, a gazetteer label layer and an
  Esri ocean-reference raster — all merged into ONE style, `setStyle(diff:true)`.
- **Panels** (U1): `src/panels.tsx` — custom, no library: floating cards with move (drag bar; double-click re-docks
  left/right/bottom), collapse (edge pill), maximize (`MaxPanel`, backdrop, `Esc`, focus trap), resize (corner/edges);
  geometry per viewport in localStorage (`vpKey()` = `${innerWidth}x${innerHeight}`); fold/max state in the URL
  (`hide= show= max=`); < 900 px → bottom sheet with three detents. (The atlas's `Panel`/`Sheet` already persist per
  viewport; the URL-vs-chrome split for fold/max is a decision — round 1 kept layout out of the URL.)
- **Tour** (U6): `src/tour.ts` — driver.js over `data-tour="…"` anchors (the atlas already stamps `data-tour` on
  controls), `TOUR_STEPS` with `before()`/`after()` hooks and `wait`, `startTour()` snapshots + restores app state on
  destroy; `?tour=off|on`; `?` replays (`src/help.tsx`).
- **Branding** (U5): no local tokens or logo — everything from `calcofi.io/brand/<v>/` (`brand/v1.head.html`,
  `brand/v2.head.html`, `src/brand.ts` `LOGO` URLs). Not a model for the atlas (which owns `tokens.css`, the seal, the
  fonts); the sun/moon and palette-validation ideas are.
- **Also**: `src/state.ts` (`fromUrl`/`toUrl`, `replaceState`), `src/export.ts` (3-line footer stamp on every PNG/SVG/CSV),
  `src/bundle.ts` (zip of data + SQL + citations + `reproduce.R/.py`), `src/cite.ts`, `src/track.ts` (gtag wrapper),
  `scripts/verify.mjs` (headed-Chrome state screenshots), keyboard shortcuts documented in `README.md` "Using it".

## 11. Round-2 progress (orchestrator, Fable 5.1) — state at 2026-09-24 05:05 CEST
Dated lines live in `plans_todo/atlas-8 … .md` "Progress log". Landed and LIVE on Pages (0.10.35 tree, `aab5745`):
R2-1 (desktop raster, perf notes, phase review), R2-0 (`docs/usability.md`), the atlas-8 review fixes F2/F3/F1a, the
usability blockers B1 (places race, Opus 5.5) and B2–B5/M2, the timing regression root cause (0.10.22, Opus 5.5), U6
(Report + tour + Help) + U2a (dark default, sun/moon), the flower fix (annular petals, one-decimal values, treemap by
count = R8), the study-area camera, U5 (labelled rail, wave-in-hexagon mark, y1 light palette + navy ring, paper
outline stroke = R9), U3 (Feedback dialog + Apps Script + runbook; Ben's endpoint step pending). In flight: U1
(dockable panel, About, Feedback control, attribution, padded first view — merged with main, final gates), U4 (layer
stack after the Opus 5.5 review's blocker B1 + M1–M8; fix round 1). Decisions R1–R10 all DECIDED (§6). Process
changes made on Ben's instruction (2026-09-24 00:05): Pages publishes after the fast `checks` job; the local full
gate is the gate; CI's slow jobs are the arbiter for browser suites while the laptop is saturated; `docs/status.md`
is the one-page board. Reviews on file: `atlas-refs/2026-09-23 atlas-8 phase review …`, `2026-09-24 parity-page
audit …`, `2026-09-24 U4 layer-stack review …`. Still queued: report map draws no place (audit B1) + shots tooling,
species table (virtualization, keyboard, row count, CSV columns), msens `.app_assets()` join + `APP_BUNDLE_S3`
re-publish (audit M1), parity signature, atlas-9.

### §11 state as of 2026-09-24 12:00 CEST (orchestrator)

- **Live on Pages: atlas main `dd124b1` = 0.10.48** — U1b, U4 layer stack (R3), P1 phone legend chip + legend modal, P2 flower, P3 readable tables + Program Area chooser, P4 one report map + grouped footnotes, P5 desktop legend / full-width selects / phone search / theme in ⋯, U1c (the nine CI reds + maximized-panel controls), P6 + P6c (honest out-of-area clicks, models frame their extent, collapsed-panel click, species camera ignores masks), P7 (drawn places stay + auto-analyse), CI round (Layer select tooltip), gallery baselines both platforms, P8 (missing tiles = empty, Program Area rows show numbers, per-row report, CSV, share encoding, coordinate check, GeoPackage honest), gallery axe round. CI on it: every job green incl. gallery; three-engine 1116 passed / 1 failed (places.spec:536 timing, fix in P9); seeded-fault suite running for the first time uncancelled.
- **In flight:** P9 (0.10.49, worktree r2-p9): duplicate place on corner drag (terra-draw finish on edit), phone first view with no scored cells, places.spec:536 determinism.
- **Docs:** `apps/atlas.qmd` on docs branch `atlas-guide` @ `b3e3208`, two Opus 5.5 reviews applied, NOT pushed — Ben's go.
- **Ben's calls:** docs push/PR; feedback endpoint (`VITE_FEEDBACK_URL`, runbook `atlas/docs/feedback.md`); msens bundle gaps (zone names, short labels "score"/"Primary producer", g1 codec R twin); Scores-lens search (Program Area search / geocoder / hide); atlas-9 preview deploy (the preview-link gate waits on it).
- **Still open (app):** preview-link gate, GeoPackage catalogue wording (`lib/geo/upload/messages.ts`), uploads ignore `upload.md` naming options, `apps/scores.qmd` on docs main says `?ver=`, Help ▸ Docs → the Atlas chapter, parity re-shoot + R7 signature, atlas-9 D5/D6.
- **Process (kept):** every merge = own seeded fault (diff non-empty AND the intended test red) + full gate + eyes-on shoot of the real build (`scripts/eyes-shots.mjs`) + Opus visual review where design changed; registries merged with `scratchpad/briefs/merge_faults.py` / `resolve_registry.py`; one push per coherent set, held while a three-engine run is in flight.

## 12. Round-2 progress — state at 2026-09-25 01:40 CEST (orchestrator, Fable 5.1)

- **Live:** Pages + preview host serve atlas 0.10.59 (`1565dfd`); 0.10.60 (`d1c0e49`, V5) pushed, CI run 36070452831 running. Docs main `c24ece4` = Atlas chapter as of 0.10.59 (two Opus passes), docs CI running.
- **Landed since §11:** Q1 Scores search, Q2 GeoPackage via DuckDB-WASM + upload naming, Q3 Program Area results panel, Q4 feedback/preview env flags, Q7 `VITE_LOG_URL` wiring, V1 (Program Area full names via an app-side table, full-screen panel width, phone report overflow, pill reason), V2 (harness false results, report petals/centring, category labels), V3 (service-health probes + banner), V4 (species camera via MapLibre `cameraForBounds` after V1's Mercator math regressed the phone globe view in 0.10.56; report captions/URL wrap; CI reds; gallery baselines both platforms; copy fixes), V5 (projection-based harness taps + Program Area states, phone counts-table scroll cue, spelling/case). Feedback endpoint LIVE (issue #1). Preview-host Atlas route DEPLOYED and proven (`VITE_PREVIEW_ATLAS_ROUTE=1`). msens 0.44.1 on main + server container. build_app_bundle.qmd rounds 1–3 merged, 11-version dry run green — the flagged publish awaits Ben's `!` (classifier refuses it from the orchestrator). msens1 outage (Shiny worker OOM) fixed reproducibly (rstudio 9 GB ceiling + 4 GiB swap).
- **In flight:** V6 (0.10.61: `search.ts#matchZones` must match the fallback Program Area names).
- **Ben's calls:** leatherback/wide-range phone framing (frame the in-US portion vs min-zoom on the densest part); Bird≈Fish + Mammal≈Turtle palette; health banner covering the top bar; phone default frame; "GOA Program Area A" naming; v9 `app/` anonymously readable; `preview: false` in `createAnalytics()`; eslint ignore `docs/*_files/`; recapture docs `desktop-places.png` on 0.10.59+.
- **Carried nits (review 3):** raw "score" in the Layer select/legend/chip; popup vs panel coordinates + score wrap; × focus ring; native Layer select; legend-modal space; Places button font; 33–34 one-place ramp; duplicated pill line in the report; "in this R package" citation; desktop table placeholders/blank space; small full-screen flower.
- **Process additions:** Opus fact-check of docs prose against the app source (caught two false privacy claims); fault patches regenerated on a clean tree and proved red before the commit message; stage explicit paths; grep e2e for old wording when copy changes.

## 13. Round-2 progress — state at 2026-09-25 11:10 CEST (orchestrator, Fable 5.1)

- **Live:** atlas 0.10.67 (`9d6be26` incl. the linux gallery baselines + a fixed spelling fault; CI run 36114961882 running — the three-engine job was green on 0.10.66 and 0.10.67 already). Docs main `0b3c827` (chapter as of 0.10.59 + the recaptured Places screenshot). **All 11 releases' app bundles republished** with msens 0.44.1 (v7 first, then v1–v6/v8/v9, then v7 + v7b again with round 4 so `cell_model: true`); live checks: 20/20 Program Area names, v7 inputs with COGs, v2 133 / v9 102 labels, manifests `Cache-Control: no-cache`. Workflows main `c26472be` carries the publish renders + notebook rounds 1–4.
- **Landed since §12:** W1 (search zoom, GeoJSON names + polygons, icon-only utility menu without Report, About credits, equal search width, species inputs Table, "Primary production", v1 reptile label), W2 + follow-up (Layers "Raster cells | Program areas" toggle greyed in Species, quiet switches, bigger flower with a manifest-driven reference ring), W3 (chrome-aware zone fits, hub number under the petal label, segments fill the pill, phone half-detent cap, copy), W4 (the Report map paints places by score — its one-shot style build fired before any place had data; single-swatch legend for one place), W5 (panel-inset + side gutters, full-width flower title, harness report-map state). Five Opus eyes-on reviews (3 HOLD → fixed, 2 PUSH); msens1 outage fixed reproducibly (rstudio 9 GB ceiling + 4 GiB swap); feedback endpoint live (issue #1 commented).
- **Ben's calls (unchanged):** leatherback/wide-range phone framing; Bird≈Fish + Mammal≈Turtle palette; phone default frame; "GOA Program Area A" naming; v9 `app/` anonymously readable; `preview: false` in `createAnalytics()`; eslint ignore `docs/*_files/`.
- **Carried nits (review 5):** raw "score" in the Layer select/chip/legend; popup vs panel coordinates + score wrap; × focus ring on the welcome modal; native Layer select; legend-modal space; Places button font; duplicated pill line in the report; "in this R package"; desktop table placeholders/blank space; Mean row cut in the desktop flower panel; "LOUISIANA" clipped in the report map; theme icon reads as a gear.
- **Process additions this round:** cut a fault patch with `git diff HEAD -- <one file>` on an otherwise clean tree and prove it with `--only` before the commit message; never chain a merge with anything in `&&`; grep e2e for old wording when copy changes; a shell-wide change must run `e2e/shell.*` + `feedback` locally; gates that are single pixel probes can pass under CI's software GL — widen to the whole spec; the publish renders' tracked outputs must be committed before merging a notebook round.
