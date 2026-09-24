# CalCOFI Explorer (`/Users/bbest/Github/CalCOFI/explore`) — architecture review for the MarineSensitivity static app

Scope: read for framework-independent architecture and hard-won lessons, to inform a new MarineSensitivity
static SPA (MapLibre GL + DuckDB-WASM over public Parquet on S3, GitHub Pages, Svelte 5 + Vite, not React).
97 commits total (`git log --oneline | wc -l`), all on `main`, going back to `287c020` "Phase-0 spike"
(2026-08-25ish) through `b1dbcf0` (2026-09-16 "ws-r6" ramp fix). No CHANGELOG.md history beyond an
"Unreleased" section — this repo's real changelog is its commit log, which is written in the same dense,
narrated style as MarineSensitivity's own commits.

---

## 0. TL;DR architecture

```
GitHub Pages (static dist/, Vite base=/explore/)
   │
   ├─ index.html: inline script fires latest.txt → catalog.json + coverage.json + grid.geojson
   │  BEFORE the JS bundle parses (window.__early promises)
   │
   ├─ React shell (App.tsx) paints IMMEDIATELY from those sidecars + a plain CARTO basemap style URL
   │  — a MapLibre map with grid-cell dots colored by the pre-aggregated coverage cube, ZERO WASM queries yet
   │
   └─ in parallel: DuckDB-WASM boots in a Web Worker (self-hosted bundle files, no CDN, no threads,
      no COOP/COEP) → whole Parquet OBJECTS for the current taxon/variable are fetched with plain
      fetch() (not httpfs/range-requests) and registered as in-memory buffers → one SQL template per
      "lens" (sql/*.sql) runs against them, serialized on a single connection's promise chain.
```

The one big idea worth carrying over: **materialize-then-query, not query-the-network**. The app never
attaches remote Parquet via `httpfs`/range requests for its own interactive queries (that's only used in
the *generated* R/Python reproduction scripts that run outside the browser — `src/bundle.ts:43,61`). It
fetches the handful of whole objects the current selection needs (one taxon's `obs_bio`, or one
variable's `obs_env_<type>` partitions) with a plain `fetch()`, registers them as DuckDB-WASM file
buffers (`src/engine.ts:112-133`), and then runs dozens of cheap, index-free SQL queries against that
fully in-memory working set for every lens/filter change. This sidesteps COOP/COEP entirely, gets normal
HTTP/CDN caching for free, and makes every subsequent lens switch a sub-100ms local query instead of a
network round trip.

---

## 1. DuckDB-WASM

- **Version**: `@duckdb/duckdb-wasm@^1.29.0` (`package.json:14`).
- **Bundle selection**: `duckdb.selectBundle({ mvp: {...}, eh: {...} })` (`src/engine.ts:97-100`) — only
  the two non-threaded bundles are offered (`mvp` = baseline WASM, `eh` = exception-handling WASM); there
  is **no `coi`/threaded bundle** entry, so DuckDB-WASM never even tries the pthreads build. Confirmed by
  grep: no `COOP`/`COEP`/`crossOriginIsolated`/`SharedArrayBuffer` anywhere in the repo. This is a direct
  consequence of shipping on GitHub Pages, which cannot set cross-origin-isolation response headers — the
  app designs around that constraint rather than fighting it.
- **Self-hosted, not CDN**: the wasm/worker files are imported with Vite's `?url` suffix
  (`src/engine.ts:4-7`: `duckdb-eh.wasm?url`, `duckdb-browser-eh.worker.js?url`, etc.) so Vite copies them
  into `dist/` beside the app; `vite.config.ts:20` explicitly excludes `@duckdb/duckdb-wasm` from
  `optimizeDeps` with the comment *"ships its own worker + wasm; the optimizer breaks it"*. No jsDelivr/CDN
  load in the critical path (README.md:301, vite.config.ts:15-16 comment).
- **Worker instantiation**: `new Worker(bundle.mainWorker!)` then `new duckdb.AsyncDuckDB(new
  duckdb.VoidLogger(), worker)` then `db.instantiate(bundle.mainModule, bundle.pthreadWorker)` then
  `db.connect()` (`src/engine.ts:101-104`). `VoidLogger` — DuckDB-WASM's own console logging is silenced.
- **Cross-origin isolation**: **not used at all.** No COOP/COEP meta tags or headers anywhere (verified by
  grep across `.ts/.tsx/.html/.mjs/.yml`). This is consistent with plain GitHub Pages hosting, which cannot
  serve custom response headers.
- **Threads**: none — single-threaded WASM only (see above).
- **Remote Parquet attachment — the key finding**: **not `httpfs`, not `ATTACH`, not views over remote
  URLs.** `Engine.load(name, url)` (`src/engine.ts:112-133`) does a plain `fetch(url)`, reads the whole
  response into a `Uint8Array`, and calls `this.db.registerFileBuffer(name, buf)` — i.e. the Parquet object
  is fully downloaded and handed to DuckDB-WASM as an in-memory virtual file. SQL then reads it as
  `'obs_bio.parquet'` etc., a local (in-worker-memory) file, not a URL. `httpfs` is `INSTALL`ed/`LOAD`ed
  only inside the **generated reproduce.R / reproduce.py** scripts (`src/bundle.ts:43,61`) that a user runs
  on their own machine against the release's real object URLs — there, streaming range reads matter because
  R/Python aren't pre-fetching the whole object. There is no `ATTACH 'x.duckdb'` anywhere in the app; the
  release ships **Parquet objects + JSON sidecars**, never a `.duckdb` file, to the browser.
- **Bucket/host + CORS**: Google Cloud Storage, `https://storage.googleapis.com/calcofi-db/` by default
  (`src/release.ts:4`, overridable via `VITE_DATA_URL`); GEBCO bathymetry PMTiles come from
  `storage.googleapis.com/calcofi-db/bathymetry/` (`src/basemap.ts:13`) and boundary-layer PMTiles from a
  similar GCS prefix. Because the app fetches **whole objects** rather than doing HTTP Range requests, the
  CORS requirement is only a plain `Access-Control-Allow-Origin: *` GET — **no `Range`/`Accept-Ranges`
  allow-list is needed for the interactive app itself** (`src/brand.ts:15` and `src/capture.ts:8,28`
  explicitly note "GitHub Pages / calcofi.io answers every asset with CORS `*`"). PMTiles (used for
  bathymetry + boundary vector tiles, `maplibregl.addProtocol("pmtiles", ...)`, `src/basemap.ts:11`) *do*
  use HTTP Range internally (that's how the pmtiles protocol works), so GCS's default CORS + Range support
  is relied on there — but that's the `pmtiles` library's business, not the DuckDB-WASM path. This is a
  **different pattern** than the sibling MarineSensitivity `query.html`/DuckDB-WASM page, which per the
  user's own memory notes needs "bucket CORS Range-header allowance (`RELEASE_CORS`)" because it queries
  remote Parquet directly with `httpfs`; the CalCOFI Explorer deliberately avoided that path.
- **Cold start**: not pinned to a specific millisecond figure anywhere in the reviewed files (the
  `shots/timing_v1/results.json` capture from the dev-catalog era has an empty `timing` object — a stale,
  non-representative run). What *is* baked in is a live, self-instrumenting timing panel: `timing.add(name,
  ms, note)` (`src/engine.ts:10-24`) records `wasm_init` (with bundle size read via a `HEAD` request since
  the worker fetches the bundle itself and the main thread has no `PerformanceResourceTiming` entry for it
  — `src/engine.ts:105-108`), `fetch:<name>` / `register:<name>` per object, and `query:<label>` per SQL
  call, all exposed on `window.__marks` for `scripts/verify.mjs --timing` to harvest
  (`scripts/verify.mjs:793-823`). The one concrete numbers in the repo are for the **contour interpolation**
  (a CPU-bound Worker job, not DuckDB): "≈ 1 s for 12,000 sites" at the site grain, "≈ 0.3 s" at the station
  grid, "+≈ 3 s" for the kriging error surface (README.md:55-56), and a comment recording a real measured
  regression — "32 [nearest neighbors] on 0.06° [cells] took 12.8 s for 44,946 casts" — which is why the
  shipped defaults are 24 nearest on 0.1° cells (`src/App.tsx:474`).
- **Persistence — none.** No IndexedDB, no OPFS, no Cache API usage anywhere in `src/` (grep confirmed).
  Every session re-fetches every object fresh (subject only to ordinary browser HTTP caching, which
  `Engine.load()` actively detects and reports via a `cached` heuristic comparing `transferSize` to
  `decodedBodySize`, `src/engine.ts:118-121` — a cross-origin resource without `Timing-Allow-Origin` reports
  0 for both, treated as "unknown" not "cached"). `localStorage` is used only for tiny UI-preference keys
  (panel geometry per viewport size `src/panels.tsx:22-29,67-68`; "seen the welcome card"/"agreed to cite"
  flags `src/help.tsx:15-25`; the picker's sort/group choice `src/picker.tsx:33-36`; the "More options"
  disclosure state `src/App.tsx:168-169`) — never for caching data rows or Parquet bytes.
- **Concurrency control**: `Engine` serializes every `load()` (buffer registration) and `exec()` (query) on
  one `Promise` chain (`this.q = this.q.then(...)`, `src/engine.ts:91,124-133,136-146`) against the single
  `AsyncDuckDBConnection`. The commit history shows this was a **fix for a real race**: `73c76df "engine:
  register buffers on the query chain (cruise-lens race on the lazy sample_root)"` — before the fix, a
  query could reach the worker before the buffer it depended on had finished registering.

## 2. First paint / precomputed sidecars

**Yes — the map paints before any DuckDB-WASM query answers, and before the JS bundle even finishes
parsing.** `index.html:159-163` runs an inline script that starts `fetch()`s for `latest.txt` →
`catalog.json` + `coverage.json` + `grid.geojson` immediately, stashing the promises on
`window.__early`; `src/release.ts:27-29,43-48` and `src/App.tsx:222-230` pick these up instead of
re-fetching if they exist. `App.tsx`'s boot effect (`src/App.tsx:210-275`) awaits only
`resolveVersion` + `fetchCatalog` + the `grid.geojson`/`coverage.json` pair, builds a `GridCell[]` (218
CalCOFI station centroids) and paints the **coverage cube** — precomputed root-sample counts per station,
across *all* datasets — as the station dots' color/size before the engine has even loaded a Parquet byte
(`covStation` memo, `src/App.tsx:489-495`; `preSlice` flag drives the legend title to say "root samples ·
all datasets (coverage.json, before the engine is warm)", `src/App.tsx:758`). The MapLibre map itself
boots from CARTO's plain hosted style URL first (no DEM/sea-floor layer), with the composed
(basemap ⊕ bathymetry ⊕ boundaries) style applied as a `setStyle(diff)` right after `load` — "D22: the map
BOOTS from CARTO's plain style URL (`first_paint` owes the DEM nothing)" (`src/map.tsx:278-281`,
`src/basemap.ts:1-4`). A `first_paint` timing mark fires on the map's first rendered frame
(`src/App.tsx:1224`: `onFirstFrame={() => timing.add("first_paint", ...)}`).

These are **not** what `src/bundle.ts` produces — that file is the user-facing **download ZIP** (data +
citations + reproduce scripts), a completely different "bundle." The first-paint precomputed JSON is a set
of release **sidecars**:
- `catalog.json` — object manifest (paths, byte counts, `sha256`/`content_hash`, partition info) — see §
  Release below.
- `coverage.json` — the pre-aggregated "coverage cube": per-dataset, per-station, per-year, and (since
  `calcofi4db` ≥ 3.25.0) per-taxon observation counts + year spans + life stages, so the **organism/variable
  picker can list every taxon with its count and category before the engine is warm**
  (`src/App.tsx:88-89`, verified live by `scripts/verify.mjs`'s `u2_prewarm` state, lines ~717-719: "n
  organisms listed · taxa.sql answered: {warm}" must show ≥1000 organisms with the SQL mark still false).
- `grid.geojson` — the 218 station centroids (`line`, `station`, `lon_ctr`/`lat_ctr`).
- `spatial.geojson` / `spatial_layers.json` — boundary-layer polygons + their registry (only fetched when
  the Regions lens or Layers card actually needs them — `src/App.tsx:284-298`).
- `coverage_stations.json` — the heavier per-station detail sidecar, fetched lazily only on first station
  click (`src/App.tsx:280-283`).

Sizes aren't stated in the reviewed source, but the README frames `tables/`-equivalent sidecars as small
("KB"-scale for `catalog.json`/`coverage.json`) versus the multi-MB Parquet objects fetched afterward
(`src/engine.ts:122` logs each fetch's MB). **How generated**: at real-release time, `calcofi4db::build_*`
R functions (`calcofi4db::build_coverage()`, etc. — README.md "How it is built" § Data) cut these sidecars
alongside the browser-shaped Parquet tables; this repo's own `scripts/dev_coverage.R`,
`scripts/dev_climatology.R`, `scripts/dev_spatial_layers.R` are **dev-only stand-ins** that call the same
`calcofi4db` functions against a local/dev cut and push the JSON to a GCS `explore-dev` prefix
(`scripts/dev_coverage.R:1-23`) — i.e. the sidecar-generation logic lives in the sibling `calcofi4db`
package, not in this repo, matching MarineSensitivity's own "logic lives in the package, not the notebook"
discipline. **Caution**: `scripts/dev_coverage.R:6` hardcodes an absolute local path that (at time of
review) pointed into an unrelated project's scratch directory — a sign these dev scripts are personal,
non-portable conveniences, not committed pipeline steps; don't copy that pattern for the new app's
equivalent dev tooling.

**Versioning/pinning to a release**: `src/release.ts` is a straight TypeScript port of
`calcofi4r::cc_release_sources()` (comment, `src/release.ts:1-3`). `resolveVersion()` reads `?release=` or
`{prefix}/latest.txt`; `fetchCatalog(version)` reads `{prefix}/{version}/catalog.json`; every object URL is
built from the catalog's own `objects[]` entries (`path`, optional `sha256`/`content_hash`), never a
hand-built `releases/{v}/parquet/...` path — the catalog is the **single source of truth** for what exists
and where, with a "legacy" fallback (`src/release.ts:67-69`) for a catalog predating the `objects[]` field.
`fetchVersions()` reads a `versions.json` for the release picker, filtering out `retired` ones
(`src/release.ts:37-42`, consumed at `src/App.tsx:221`). This mirrors MarineSensitivity's own
`versions.json`/`latest.txt`/manifest pattern almost exactly (see `msens/R/version.R` per the user's own
memory notes) — strong precedent to reuse the shape as-is.

## 3. SQL organization

Nineteen `.sql` files in `sql/`, each a **plain string template** with `{{named}}` placeholders, loaded at
build time via `import.meta.glob("../sql/*.sql", { query: "?raw", import: "default", eager: true })`
(`src/engine.ts:27-32`) — Vite inlines every file's raw text into the bundle; `template(name)` looks one up
by filename suffix match.

- **Parameter binding** (`src/engine.ts:56-68`, `render()`): a regex `\{\{(\w+)\}\}` substitutes each
  placeholder. Two substitution modes, chosen by a fixed `RAW` allow-list
  (`src/engine.ts:35`: `val, hex, where, where_nodepth, where_noyear, src, taxon_src, root_src,
  spatial_src, clim_src, dataset_filter, quarter_filter, bin`):
  - Names **in** `RAW` are spliced in verbatim — these are always **app-generated SQL fragments** (a
    `WHERE` clause built from `_filters.sql`, a `read_parquet(...)` expression, an `IN (...)` list the app
    itself already quoted, a column/H3-bit-arithmetic expression), never raw end-user text.
  - Every other name goes through `lit(v)` (`src/engine.ts:45-50`): numbers pass through only if
    `Number.isFinite`, booleans become `TRUE`/`FALSE`, everything else is wrapped in `'...'` with every
    embedded `'` doubled (`replace(/'/g, "''")`) — i.e. standard SQL string-literal escaping.
- **Injection safety**: layered by construction. A scalar that reaches the SQL from the URL — `taxon`
  (`?taxon=worms:217452`), a `cruise` key, a `line` number, a dataset key — is **never** in the `RAW` set,
  so it is always `lit()`-escaped even though it originates from user-editable query-string state
  (`state.ts:254-274` parses these straight from `URLSearchParams` with no allow-list beyond type
  coercion). The one exception that *looks* dangerous but isn't: `dataset_filter` is RAW, but its only
  producer, `datasetFilterSql(ds)` (`src/engine.ts:36`), builds `dataset_key IN (${ds.map(lit).join(", ")})`
  — i.e. it's RAW at the "whole fragment" level while every individual value inside it is still
  `lit()`-escaped. Similarly `hex` (RAW) is produced by `hexExpr(res)` (`src/engine.ts:44`) where `res` is
  clamped to `[3,7]` in `state.ts:247` (`Math.min(7, Math.max(3, num(...)))`) before it can reach the SQL
  generator — defense in depth (type + range validation upstream, then only-ever-app-authored fragments
  downstream) rather than relying on escaping alone.
- **The shared filter fragment** (`sql/_filters.sql`) is not literally substringed into every template —
  `render()` computes it three ways (`src/engine.ts:62-66`): the full filter (`{{where}}`), one with the
  `depth_bin` line dropped (`{{where_nodepth}}`, for the depth-strip panel which must not filter on the
  thing it's plotting), and one with the year clause dropped (`{{where_noyear}}`, for the year-strip panel).
  This "one fragment, computed in 2-3 shapes for panels that must not self-filter" pattern is worth copying
  directly — it's exactly the trap a MarineSensitivity time-series or histogram panel would fall into
  otherwise.
- **Layering of templates**: `slice_bio.sql` / `slice_env.sql` **materialize a `CREATE OR REPLACE TABLE
  slice AS ...`** once per taxon/variable change (`src/engine.ts:314`, `sql/slice_bio.sql:10`) — a
  deliberate one-time cost (includes a real domain rule: synthesizing zero-catch rows for positive-only
  datasets, `sql/slice_bio.sql:1-9,19-46`) so every subsequent lens query (`station.sql`, `hex.sql`,
  `section.sql`, ...) is a cheap `SELECT ... FROM slice WHERE {{where}} GROUP BY ...` against an in-memory
  table, not a re-scan of the raw Parquet buffer with the taxon filter repeated every time.
- **Reserved words / column naming**: not DuckDB-reserved-word issues here, but `sql/density.sql` is a
  **shared fixture** — the exact `CASE WHEN` expressions for `density_per_10m2` / `density_per_1000m3` are
  pinned byte-for-byte against `calcofi4r`'s and `calcofi4py`'s equivalents (README.md "One algorithm,
  three runtimes" pattern), the same discipline MarineSensitivity applies to `msens::merge_sql()`.

## 4. State management + URL state

`src/state.ts` (373 lines) is the **entire** application state model — a single `Sel` interface
(`state.ts:11-61`) covering lens, organism/variable, stage/denominator, years/months/quarter, depth band,
region/line/cruise, statistic, contour method/surface/grain, map extent, 3-D camera, ramp, layer visibility
+ styling, panel folds/maximize, theme, and modal. There is **no separate "app state" store** distinct from
the URL — `fromUrl()` (`state.ts:238-293`) parses a `Sel` from `location.search` with per-field validation
and clamping (unknown lens → default; `res` clamped 3-7; malformed `years=`/`depth=`/`map=`/`cam=` silently
fall back to defaults rather than throwing), and `toUrl(sel)` (`state.ts:295-344`) serializes it back with
`history.replaceState` — **never `pushState`**, so there is no back/forward stack of every filter tweak
(confirmed: no `popstate` listener anywhere in `src/`, no router). Back/forward in the browser will reload
the page at whatever URL was last replaced; within a session, "undo" is not supported by browser history at
all — a deliberate simplicity trade (README.md's "**The URL is the whole view**" section frames this as the
whole point: "Share → Copy link, a bookmark and a feedback report all reopen at exactly the same place").

- **Query string, not hash** — plain `?lens=hex&taxon=worms:217452&years=1990-2000` etc. No compression;
  every field only appears when it differs from a `DEFAULTS` constant (`state.ts:203-208`), so a
  default-everything view is `?tour=off` and nothing else, and complex views stay human-readable.
- **Deliberately un-encoded characters**: `toUrl()` un-escapes `%2C`→`,` and `%3A`→`:` after
  `URLSearchParams` serializes them (`state.ts:340-343`, comment: *"a link people read and paste keeps them
  raw ... (Ben, 2026-09-10: 'doesn't get so ugly')"*) — a small but deliberate readability call worth
  copying: `cam=-121.2,32.6,7.2,40,-60` instead of `cam=-121.2%2C32.6...`.
  ​
- **Compound encodings worth stealing directly**:
  - `years=2015-04:2016-10` for month-resolved ranges vs. `years=1990-2000` for whole years, parsed by one
    regex (`parseYears`, `state.ts:218-228`).
  - `map=lon,lat,zoom` / `cam=lon,lat,zoom,pitch,bearing` — comma-joined numeric tuples, each with its own
    "malformed → null (=default view)" guard (`parseMap` state.ts:230-235, `parseCam` state.ts:110-116) and
    a canonical rounding function (`roundMap`/`roundCam`, state.ts:103,108) so that comparing "did the view
    actually change" never suffers float noise.
  - `layers=slug[:colour][:fillOpacity][:lineWidth],...` — one boundary layer per URL segment, an unknown
    slug (e.g. after a later rename) is **kept in state but drawn as nothing** rather than erroring
    (`state.ts:70-90` `parseLayerStyles`/`fmtLayerStyles`; `basemap.ts:316` comment: "an older link
    survives a rename"). This forward-compatibility-by-ignoring-unknown-ids pattern is directly reusable
    for a species/model picker in the new app.
  - `hide=`/`show=` deltas against a `DEFAULT_HIDE` array (`state.ts:121-130`) rather than an absolute list
    of open panels — so the URL only grows when a user deviates from the shipped default, and changing the
    shipped default later doesn't invalidate old links' *intent* (only their literal panel set).
- **Share links**: "Copy link" is just `navigator.clipboard.writeText(location.href)`
  (`src/App.tsx:642`) — no shortening/server round-trip. The **figure footer** (`stampLines()`,
  `src/export.ts:26-36`) bakes the same `location.href` into every exported PNG/SVG/CSV, and the feedback
  dialog attaches it too (`src/feedback.tsx:47,54`) — one canonical "this view" string reused everywhere
  attribution or reproducibility matters.
- **What is explicitly *not* in the URL**: card drag position/size (`localStorage`, keyed by viewport size,
  `panels.tsx:67-68`), the "More options" disclosure (`localStorage`, `App.tsx:168-169`) — the rule stated
  plainly in the README: "nothing re-lays out on a selection change — only a fold, maximize, drag, lens
  change or breakpoint moves panels. Card positions ... live in `localStorage`; folds and maximize live in
  the URL." That's a clean, worth-copying split between "state that defines the view" (URL) and "state that
  is just remembered UI chrome" (localStorage).

## 5. Map stack

**MapLibre GL (`^5.24.0`) + deck.gl (`^9.3.10`) via `@deck.gl/mapbox`'s `MapboxOverlay`, interleaved.**
`maplibre-gl` is pinned below v6 specifically because *"maplibre-gl 6's module worker breaks [Vite's
optimizer]"* (`vite.config.ts:20`) — a concrete version-pin lesson to carry into the new app's dependency
choices.

- **Basemap**: keyless CARTO styles (`dark-matter-gl-style` / `positron-gl-style` JSON URLs,
  `src/map.tsx:15-18`, `src/basemap.ts:79-82`), fetched once per theme and cached in-module
  (`src/basemap.ts:83-88`). The composed style — CARTO ⊕ a GEBCO bathymetry raster-DEM (via `pmtiles://`
  URLs, `src/basemap.ts:129-156`) ⊕ boundary/reference vector layers from a release-independent registry
  sidecar ⊕ an OSM land-mask — is built as **one plain JS object** (`composeStyle()`,
  `src/basemap.ts:114-159`) and applied with `map.setStyle(composed, { diff: true })`
  (`src/map.tsx:291`), never `addLayer()` after the fact. The stated reason (`src/map.tsx:278-281,
  src/basemap.ts:1-4`) is that `addLayer`-after-load layers can silently vanish across a style swap; a
  single diffed style object cannot lose layers that way, and a theme toggle or bathymetry-opacity change
  is just a re-diff. This "compose one style object, `setStyle({diff:true})`, never `addLayer` piecemeal"
  pattern is the single most reusable MapLibre lesson here.
- **Interleaved deck.gl** (`new MapboxOverlay({ interleaved: true, layers }); map.addControl(o)`,
  `src/map.tsx:259-260`): lets a deck.gl data layer be given a MapLibre `beforeId` so it draws **under** a
  specific basemap/boundary layer (e.g. under a sanctuary polygon's fill, or under an OSM land-mask layer so
  data never spills onto land) — impossible with deck.gl's simpler non-interleaved overlay mode. Cost: deck
  must be told to re-resolve (`applyLayers()`, `src/map.tsx:294-301`) every time the style changes, and a
  `beforeId` naming a layer that doesn't exist *yet* (composed style still loading) must be stripped or the
  whole layer group throws and is dropped (`src/map.tsx:299` — `l.clone({ beforeId: undefined })` as a
  guard). Also: **interleaved deck.gl forwards no native mouse events**, so hover/click picking is done by
  hand via `overlay.pickObject({x,y,radius})` inside the map's own `mousemove`/`click` handlers
  (`src/map.tsx:264-273`), falling back to MapLibre's own `queryRenderedFeatures` for boundary-layer hover
  when nothing deck-side is under the pointer (`src/App.tsx:797-820` `getTooltip`).
- **`preserveDrawingBuffer: true`** on the MapLibre canvas (`src/map.tsx:249`) is required so the
  whole-view PNG capture (`html-to-image`, § 6) can read back the WebGL canvas via `toDataURL`; deck.gl 9
  preserves its own buffer by default per the same comment.
- **Layer types used**: `ScatterplotLayer` (station dots — the one layer that *travels* between lens
  positions via deck's built-in `getPosition` transition, used as "the morph carrier" so switching
  Stations↔Hexagons visibly pools the same 218 dots into hex centroids and fades them out, `src/map.tsx:70-
  122,212-225`), `H3HexagonLayer` (hex lens), `GeoJsonLayer` (region polygons), `PathLayer` (cruise track,
  section line, contour isolines), `TripsLayer` (animated ship-track playback, driven by a `requestAnimationFrame`
  loop that calls `overlay.setProps()` directly from a ref — **no React re-render per animation frame**,
  `src/App.tsx:447-461`), `BitmapLayer` (the contour surface, rendered client-side to an `HTMLCanvasElement`
  and handed to deck as an image, `src/map.tsx:158-161`), `TextLayer` (contour iso-labels, SDF font,
  `src/map.tsx:170-175`). No drawing/annotation tools on the *map* itself — the only "drawing tool" in the
  app is the feedback-screenshot annotator (§ 6), which draws on a flat raster copy of the view, not on the
  live map.
- **Popups**: no MapLibre `Popup` widget — a single hand-rolled tooltip `<div>` is created once, positioned
  with `left/top` px on `mousemove`, and its content resolved by a per-view `getTooltip(info)` callback
  (`src/map.tsx:264-269`, `src/App.tsx:797-820`).
- **PMTiles** (`^4.5.0`): used for bathymetry (raster-DEM terrain tiles, `basemap.ts:129-134`), the OSM
  land mask (`basemap.ts:301`), and every boundary/reference vector layer (`basemap.ts:280`) — registered
  once at module load via `maplibregl.addProtocol("pmtiles", new Protocol().tile)` (`basemap.ts:11`). All
  three PMTiles archive families are static objects on GCS, read directly by the browser via HTTP Range
  (the `pmtiles` library's own mechanism) — this is the one place in the app that *does* rely on
  Range-request CORS.
- **Draw order as first-class state**: the "Layers card" (`src/layers.tsx`) treats the visible boundary
  layers **and** the data layer itself as one reorderable list (`layers=` in the URL, top item drawn last
  = on top), including a synthetic `"data"` pseudo-entry that lets a user drag the actual
  species/organism/variable layer *underneath* a specific boundary layer — implemented as splitting the
  style-insertion point into an "above the data" and "below the data" block around the land-mask layer
  (`composeBoundaries()`, `src/basemap.ts:314-336`). Reorder UI is a **hand-rolled pointer-based
  drag-and-drop** (`src/layers.tsx:40-54`), no external DnD library.

## 6. Export / report / capture

Three independent but footer-consistent export paths, all client-side, no server:

1. **Per-panel exports** (`src/export.ts`): every floating panel/card has a small "⬇" menu offering **PNG
   (2×, via Plotly's own `toImage`)**, **SVG (vector, also via Plotly, post-processed to inject the
   footer as real `<text>`)**, and **CSV** of that panel's own table (`plotPng`/`plotSvg`/`csv`,
   `export.ts:53-96`). Every exported image gets the same 3-line footer baked in — selection, `CalCOFI
   Explorer · release <v> · <url>`, and a `Data: <dataset keys> · cite: ...` line
   (`stampLines()`/`drawFooter()`, `export.ts:26-46`) — drawn identically whether the destination is a
   `<canvas>` (PNG) or hand-written SVG `<text>` elements (SVG), so the two formats can never visually
   disagree. CSVs get a `dataset_key` column added post-hoc for any row that lacks one — i.e. a **pooled**
   statistic gets a semicolon-joined list of every dataset it averaged over (`csvWithDatasets()`,
   `export.ts:90-95`) — so a CSV can never leave the app without saying what it's built from.
2. **Whole-view capture** (`src/capture.ts`): `html-to-image`'s `toCanvas()` over the whole `.app` root,
   with an explicit hide-list of transient chrome (`.status`, `.pill-row`, popovers, the map's own zoom/⬇
   buttons, drag handles, tooltips — `capture.ts:14`). Notable engineering details worth copying:
   - Both WebGL canvases (MapLibre, then deck's interleaved layer) are force-redrawn immediately before the
     capture (`__map.triggerRepaint()`, `__overlay._deck.redraw()`) and the code waits **two**
     `requestAnimationFrame`s before reading, because `preserveDrawingBuffer` only guarantees the *last*
     drawn frame is readable (`capture.ts:25-26`).
   - Self-hosted webfonts are inlined as `@font-face` rules with the `woff2` bytes converted to `data:` URLs
     (`fontEmbedCss()` in `brand.ts:17-31`) and handed to `html-to-image` as `fontEmbedCSS`, because the
     clone `html-to-image` produces is detached from the page's real stylesheet cascade and would otherwise
     fall back to system fonts — a real, previously-shipped bug class this fixes proactively.
   - An absolutely/fixed-positioned capture root (a single floating card, as opposed to the whole map box)
     is re-pinned to `position: static; left/top/right/bottom: auto` in the clone
     (`capture.ts:34-36`) — otherwise its own `left/top` coordinates (meant to be relative to the *live*
     page) draw it off-canvas in the isolated clone.
   - A **luminance-based "is this blank" self-check** (`luminanceStats()`, `capture.ts:49-57`) — mean +
     standard deviation + fraction-of-pixels-far-from-background over a coarse grid — is used by the
     Puppeteer verification harness to catch a capture that silently rendered nothing (a real failure mode:
     a `<picture>` wrapper once dropped the header logo from captures with no error, caught by exactly
     this check, `scripts/verify.mjs` `v2_capture_fonts` state).
3. **Full data-package download** (`src/bundle.ts`, "D10" in the codebase's plan numbering) — the
   `buildBundle()` ZIP contains: the exact SQL the browser ran, rewritten so every registered buffer name
   is replaced by the release's real content-addressed object URL (`resolvedSql()`, `bundle.ts:23-38`);
   `query/objects.json` recording the byte count/sha256/content_hash of every object the query touched;
   both `data/observations/*.parquet` and (if ≤300k rows) `*.csv` of the underlying rows, plus the
   on-screen summary table (+ GeoJSON for map-shaped lenses); reference rows (dataset/measurement_type/taxon
   actually used); a `CITATION.md` built from the **same** per-dataset citation builder the UI uses
   (`citationMd()` in `src/cite.ts:86-88` — one function, reused by the ZIP, the Sources modal, and "Cite
   this data", so they cannot drift); and **`reproduce.R` / `reproduce.py` / `reproduce.qmd` /
   `reproduce.ipynb`** that re-run the identical SQL directly against the release's public URLs via
   `INSTALL httpfs` (`bundle.ts:39-75`). A companion "Copy code" menu (`copyAs()`, `bundle.ts:76-81`) gives
   the same SQL/R/Python as clipboard text without downloading anything. `scripts/parity/parity.R` /
   `parity.py` (§ 8) close the loop by actually running a downloaded bundle's SQL and diffing it against the
   CSV the browser wrote.

## 7. UI system

- **Icons**: Material Design Icons (`@mdi/js`, tree-shaken — only imported names reach the bundle) plus a
  handful of bespoke marine glyphs, all defined as raw SVG path strings in `src/icon-paths.ts` and rendered
  **inline** by a single `<Icon>` component that draws `<svg viewBox="0 0 24 24"><path d={ICON[name]}
  fill="currentColor"/></svg>` (`src/icons.tsx:13-22`) — no `<use href>`/sprite at runtime inside the app
  itself (that would need a same-origin or CORS-friendly sprite fetch). `scripts/build_icons.mjs` *does*
  generate a `<symbol>` sprite + a CSS custom-property mask file from that same source map, but that output
  is for **other, non-React CalCOFI properties** (Quarto docs, pkgdown, plain HTML cards) to consume via
  `<use href>` or `.cc-i-<name>` classes — the Explorer app itself never uses its own generated sprite.
  Lesson: keep one canonical path/name map, and let each consuming surface pick its own best embedding
  strategy (inline path vs. sprite `<use>` vs. CSS mask) rather than forcing one mechanism everywhere.
- **Panels** (`src/panels.tsx`): a from-scratch floating-window system — no library. Every panel supports
  drag (pointer-capture, not native HTML5 DnD), resize from any edge/corner, collapse-to-a-labelled-pill on
  the nearest map edge, and maximize-to-fill-the-map (`Panel`/`MaxPanel`/`EdgePills`,
  `panels.tsx:34-51,60-126,132-145`). Geometry is remembered **per viewport size** in `localStorage`
  (`vpKey()`/`store()`, `panels.tsx:22-30,67-70`) so a resize back to a previous window size restores the
  old layout instead of clamping stale coordinates into an unrelated viewport. Under 900px width, the same
  panels reflow into a single bottom **Sheet** with three velocity-aware drag detents (peek/half/full,
  `Sheet`, `panels.tsx:162-191` — a flick's velocity is tracked frame-to-frame and can carry the sheet past
  its nearest detent, snapping to whichever of the three detents ends up closest to the flick's
  projected stop point).
- **Help / tour**: `driver.js@^1.8.0` drives an 11-step guided tour over stable `data-tour="..."` DOM
  anchors (never CSS class selectors, so a styling refactor can't silently break the tour —
  `src/tour.ts:24-27,30-62`). Each step can carry a `before()` hook that puts the app into the exact state
  the step needs (opens a folded panel, switches the lens, opens the phone sheet) and the driver's
  `onDestroyed` calls a `restore()` that snapshots-and-reverts whatever the tour changed
  (`tour.ts:65-92`), so running the tour never leaves the app in a different state than the user started in
  (verified live by `scripts/verify.mjs`'s `walkTour()`, which asserts every step's anchor is on-screen and
  non-zero-sized — a real regression class this catches: a step whose anchor element got hidden or removed).
  A first-visit **welcome modal** (`src/help.tsx:60-92`) offers two "doors" (organism / variable) and four
  **pre-built real URLs** as "start from a question" shortcuts (`QUESTIONS`, `help.tsx:49-58`) — i.e. the
  onboarding examples are literally URLs the app already understands, not a separate scripted demo.
- **Responsive/mobile**: a single `matchMedia("(max-width: 899px)")` listener flips a `phone` boolean
  (`App.tsx:64,129,213`) that reroutes panel rendering to the Sheet component; there is no separate
  mobile route/bundle. `scripts/verify.mjs` runs its entire state matrix at both a 1280×800 desktop and a
  390×844 phone viewport (`DESKTOP`/`PHONE` constants, `verify.mjs:20-21`) and asserts no horizontal
  overflow and every interactive control's bounding box is fully on-screen (or legitimately inside a
  scrollable ancestor) at both sizes (`assertLayout()`, `verify.mjs:51-69`) — this is the app's actual
  responsive-design regression test, not a visual snapshot diff.
- **Theming / brand tokens**: theming is **not owned by this repo at all**. `brand/v1.head.html` /
  `v2.head.html` are pasted verbatim from a separate `calcofi.io/brand/<v>/head.html` asset
  (`vite.config.ts:5-13` injects the matching file into `index.html` at build time via `VITE_BRAND`); that
  head block loads a **cross-origin** `theme.css` + `theme.js` + `fonts.css` from `calcofi.io` and runs an
  inline pre-paint script that sets `<html data-theme>` from `?theme=`, a cookie, or `localStorage` *before*
  first paint, defaulting to light for brand v2 (`brand/v2.head.html:15`) — this is how the app avoids a
  flash-of-wrong-theme without owning any theme CSS of its own. `src/style.css` then only adds
  **app-specific** layout rules, always reading color/spacing through the brand's CSS custom properties
  (`var(--bg)`, `var(--panel)`, `var(--accent)`, `var(--fs-sm)`, etc., `style.css:1-4`) with hard-coded v1
  fallback values inline as a safety net, and using `color-mix(in srgb, var(--fg) ...)` for shadows instead
  of literal black (`style.css:1-4` comment: "a black shadow on v2's white page was v1's dark heritage").
  **Implication for a from-scratch Svelte app with no shared brand asset host**: this pattern (external,
  versioned brand CSS + a pre-paint inline theme script + local component CSS that only ever reads custom
  properties) is worth the *shape* of even without the shared `calcofi.io` host — MarineSensitivity would
  own its own small `theme.css`/inline snippet instead, but the separation (brand tokens vs. app layout)
  is sound.

## 8. Testing + verification

Four distinct layers, of very different character — worth naming precisely because none of them alone
would be "enough," and each catches a failure class the others structurally cannot:

1. **`vitest` unit tests** (`tests/ramps.default.test.ts`, 93 lines) — the **only** unit-test file in the
   repo (`npm test` → `vitest run`, added in the very last commit series, `b1dbcf0`/`beec8cf`). It exists
   because a real bug shipped: four different call sites (map, contour surface, section heatmap, section's
   3-D curtain, year-strip calendar) each carried their **own** copy of "which color ramp does this
   variable get," and they silently disagreed (Plotly's built-in `"Viridis"` string vs. the shared
   `ramps.ts` rule vs. a hand-rolled diverging scale) — the test file is explicitly "the regression guard
   for the *wiring*, not just the per-variable regex table" (`tests/ramps.default.test.ts:1-8`). This is a
   narrow but well-targeted unit-test surface: pure functions (`defaultRamp`, `lensRamp`, `seriesRamp`,
   `rampPlotly`) with no DOM/network/WASM dependency.
2. **Puppeteer state-matrix verification** (`scripts/verify.mjs`, 825 lines) — drives a real, **headed**
   installed Chrome (not the bundled Chromium — a fresh `userDataDir` profile per run for a cold cache) through
   ~120 named `STATES`, each a URL + a sequence of real clicks/drags/keyboard events, asserting: (a) no
   horizontal overflow and every control on-screen at both viewport sizes (`assertLayout`, § 7), (b) an
   optional per-state `assert()` closure checking specific DOM/URL invariants (e.g. "the depth pill pulses
   exactly once," "the URL contains `show=depth`," "the tooltip text matches this regex"), and (c) a
   screenshot. It also does **pixel-level basemap probing** — reading raw WebGL canvas pixels at known
   lon/lat points via `gl.readPixels()` to assert the sea floor is actually painted and land is actually
   tinted the expected color (`expectSeaFloor()`/`probeMap()`, `verify.mjs:71-83`) — a check that a purely
   DOM-based assertion could never make, since the map's content lives entirely in a WebGL canvas. A
   `--timing` mode separately drives cold/warm/phone runs through every lens and dumps every `timing.add()`
   mark for later comparison. The comment at the top is blunt about *why* this exists: "the Claude-in-Chrome
   tab never paints, so this script is the only verification path" (`verify.mjs:1-4`) — i.e. this was built
   specifically because an AI coding agent's own browser tooling couldn't be trusted to see a WebGL canvas
   render, so a real headed browser + real pixel reads became the ground truth.
3. **Smoke / bundle / shot scripts** — `scripts/smoke_release.mjs` (headless, checks a deployed build
   actually reads the *promoted* release version, dumps every failed request + console error + which GCS
   objects were fetched with what status — written specifically for a version-flip cutover and meant to be
   re-run after every release and every `pages.yml` change); `scripts/bundle_check.mjs` (drives a real
   download, unzips nothing itself but reports the byte size and item count — literally downloads two real
   bundles through the UI and reports their sizes, catching regressions in bundle size/shape);
   `scripts/shot.mjs` (single ad-hoc screenshot after optional click, for quick manual checks). None of
   these are asserting per se — they're **operator tools** that print facts a human reads.
4. **R/Python "parity" fixtures** — two distinct kinds:
   - `scripts/parity/parity.R` / `parity.py` — take a **downloaded bundle** (the real ZIP a user would get),
     re-run its `query/*.sql` files verbatim through `duckdb`+`httpfs` in R or Python, and diff the
     resulting table against the CSV the browser actually wrote, asserting row-count equality and
     `max(|numeric diff|) < 1e-9` (`parity.R:1-16`, `parity.py:1-19`). This is an end-to-end proof that "the
     SQL we hand you really does reproduce what you saw," not just a unit test of the SQL text.
   - `scripts/parity/contour_fixture.mjs` — bundles the app's **own** `contour.worker.ts` with `esbuild`
     (shimming `self.postMessage`/`onmessage` so a browser Worker module runs under plain Node), feeds it a
     seeded-RNG synthetic point set, and writes every method's (IDW/kriging/thin-plate-spline, both the
     "every point" and "24-nearest-per-cell" grains) grid values + fit statistics to a JSON fixture
     (`contour_fixture.mjs:1-43`). That fixture is copied **byte-for-byte** into both `calcofi4r`'s and
     `calcofi4py`'s own test suites, so "the map's contour, R's `cc_interpolate()`, and Python's
     `interpolate()` produce the same numbers" is enforced as a cross-language regression test with **one
     source of truth being the browser's own algorithm**, not a hand-re-derived reference implementation —
     directly analogous to MarineSensitivity's `msens::merge_sql()`/`turtle_sql()` "logic lives in one place,
     tests assert it" discipline, just crossing a language boundary (JS↔R↔Python) instead of staying inside
     one package.
- **Bundle-size guard**: `vite.config.ts:22` sets `chunkSizeWarningLimit: 6000` (6 MB) — a soft guard, not a
  hard CI failure gate; there is **no dedicated bundle-size-diff script** in this repo (unlike, say, a
  `size-limit` CI step) — `scripts/bundle_check.mjs` measures the **data download** size, not the JS bundle
  size. A real perf win that *is* committed: `05f98b1 "lazy-load Plotly (3.5 MB off the first paint)"` —
  Plotly is dynamically `import()`ed only when a chart panel first needs it (`src/export.ts:9-10`
  `plotly()` helper: `PlotlyMod ? Promise.resolve(...) : import("plotly.js-dist-min")...`), rather than
  bundled into the main chunk.
- **Type checking as a gate**: `npm run build` = `tsc --noEmit && vite build` (`package.json:9`), and
  `pages.yml` runs `npx tsc --noEmit` as its own separate CI step *before* `vite build` — i.e. a type error
  fails the Pages deploy outright, even though Vite/esbuild would happily emit JS with type errors present.

## 9. Deploy

`.github/workflows/pages.yml` (32 lines): on every push to `main` (or manual dispatch), `actions/checkout`
→ Node 22 + npm cache → `npm ci` → `npx tsc --noEmit` → `npx vite build` (env: `VITE_DATA_URL`,
`VITE_RELEASE_PREFIX`, `VITE_BASE=/explore/`, `VITE_FEEDBACK_URL` from a repo *variable*, not a secret,
since it's just a public Apps Script URL) → `actions/upload-pages-artifact` → a second `deploy` job using
`actions/deploy-pages@v4`, gated by the standard `github-pages` environment. `concurrency: { group: pages,
cancel-in-progress: true }` ensures a rapid double-push doesn't race two deploys.

- **Vite `base`**: `VITE_BASE=/explore/` (a project-path Pages site, `calcofi.io/explore/` is a custom
  domain + path via a CNAME/proxy, not `username.github.io/reponame/`), consumed by `vite.config.ts:19`
  (`base: process.env.VITE_BASE ?? "./"` — the `"./"` fallback makes a **relative-path build** the local
  default, which is how `npx vite preview` works without any base-path juggling).
- **SPA routing / 404 handling**: **none needed.** There is no client-side router (`react-router` etc. is
  not a dependency) and no path-based navigation at all — the entire app is one route (`/explore/`) with
  **all** view state in the query string (§ 4). There is no `404.html` SPA-redirect trick in `public/`
  because there is nothing to redirect *to* — every "different view" is the same document with different
  `?params`. This is a meaningful simplification a new Svelte app should keep if it can: avoid a client
  router entirely and keep "which view" as query-string state, not path state, unless there's a strong
  reason (e.g., MarineSensitivity's own species-detail deep-links might want a path).
- **Caching/hashing**: ordinary Vite production output — hashed asset filenames
  (`assets/index-<hash>.js`), so GitHub Pages' default caching is safe; no explicit cache-control
  tuning is visible in this repo (Pages sets its own headers).
- **Data-release rollout**: **decoupled from app deploys entirely.** The app always resolves
  `{prefix}/latest.txt` at runtime (`src/release.ts:30-36`), so publishing a new data release (a `calcofi4db`
  pipeline run that writes new objects + a new `catalog.json` + updates `latest.txt` on GCS) makes every
  already-deployed copy of the app pick it up on next load — **no rebuild/redeploy of the static site is
  needed for a routine data release.** A real cutover is documented as a one-time env-var flip instead:
  commit `f71006d` changed `VITE_DATA_URL`/`VITE_RELEASE_PREFIX` in `pages.yml` itself, from a `explore-dev`
  GCS prefix to the real `ducklake/releases` prefix, the day the production release pipeline first shipped
  the browser-shaped objects — i.e. *dev vs. prod data source* is a build-time env choice, but *which
  version within prod* is a runtime lookup.
- **Feedback endpoint** provisioning is a one-time manual setup (a Google Sheet + Apps Script deployed as a
  web app, README.md "The feedback endpoint (once)") — not part of the CI pipeline, and the app degrades
  gracefully (offers a prefilled GitHub-issue link instead) when `VITE_FEEDBACK_URL` is unset
  (`src/feedback.tsx:18-23`, `120`).

## 10. Analytics / feedback

- **Analytics** (`src/track.ts`, 6 lines): a single `track(event, params)` function that no-ops safely if
  `window.gtag` isn't present, forwarding to GA4 with a fixed `content_group: "explore"`
  (`track.ts:4-6`). GA4 itself is wired up by a **generated, do-not-hand-edit** script block pasted into
  `index.html` (`calcofi4r::cc_ga_html()`, `index.html:10-157`) shared verbatim across the whole CalCOFI
  "fleet" of apps — notable pieces: it explicitly **excludes any browser with `navigator.webdriver` set**
  from both GA4 and a secondary logging path (`index.html:35`, comment explains this is because
  Playwright/Puppeteer/Selenium and the project's own screenshot scripts would otherwise register as
  phantom "users"); it maintains a `client_id`/`session_id` pair independent of GA (localStorage/
  sessionStorage) so a secondary Sheet-based log can stitch sessions even when `gtag` is blocked by an
  ad-blocker; and it batches events into a queue flushed via `navigator.sendBeacon` (falling back to a
  `keepalive` `fetch`) every 15s or on `visibilitychange`→hidden / `pagehide` (`index.html:69-93`) — the
  `LOG_URL` for that secondary path is empty in this app's `index.html` (`index.html:22` `var LOG_URL =
  ""`), so in practice only the GA4 leg is live here; the Sheet-beacon code is inherited fleet-wide
  boilerplate, not something this repo turns on.
- **Feedback** (`src/feedback.tsx`, `src/annotate.tsx`): one dialog, two "kinds" sharing a payload shape —
  ordinary bug/comment feedback, and a second "Register a product" flow ("I used CalCOFI data in ...", with
  title/link/DOI fields) that reuses the exact same capture+send pipeline but a different GitHub label
  (`KIND_LABEL`, `feedback.tsx:28`). Mechanism: `captureView()` (§ 6) grabs the current view as a canvas the
  moment the dialog opens; the user can mark it up with a hand-rolled annotator (arrow/circle/
  rectangle/pen/text, three fixed high-contrast colors, `src/annotate.tsx`) or retake it; on send, the
  image (downscaled to fit a byte budget via `fitBytes()`, `capture.ts:63-73`) is base64-encoded and POSTed
  as `text/plain` (deliberately, to stay a CORS "simple request" and avoid an OPTIONS preflight Apps Script
  can't answer — `feedback.tsx` `send()`, comment inline) to a Google Apps Script endpoint
  (`VITE_FEEDBACK_URL`, generated by `calcofi4r::cc_feedback_script()`), which fans out to: the screenshot +
  row into a Google Sheet, an email to a `recipients` tab, and a **public GitHub issue** in
  `CalCOFI/explore` labelled `feedback` or `derived-product` — filed **without** the submitter's email
  (README.md § "Feedback"). A honeypot input (`name="website"`, visually hidden, `tabIndex={-1}`,
  `feedback.tsx:108`) is the only anti-spam measure. Without `VITE_FEEDBACK_URL` configured, the dialog
  degrades to a prefilled `github.com/.../issues/new?labels=...&title=...&body=...` link plus a
  clipboard-copy of the screenshot for the user to paste in manually (`feedback.tsx:55,86`) — i.e. the
  feature has a fully-functional **zero-backend fallback**, not just a "coming soon" message.

## 11. Lessons (gotchas + regressions mined from code comments and the commit log)

Ranked roughly by how directly they transfer to a MapLibre + DuckDB-WASM + Svelte + S3 + GitHub Pages
stack (which is nearly identical infrastructure to this app's):

1. **DuckDB-WASM over the network: fetch-whole-then-register-buffer beat httpfs/range-requests for this
   access pattern.** The app never attaches remote Parquet directly; it fetches the (few, per-selection)
   objects it needs whole and registers them as in-memory buffers, then runs many cheap queries locally
   (`engine.ts:112-133`). This avoids COOP/COEP entirely (impossible on plain GitHub Pages) and turns every
   lens switch into a local query instead of a network round trip. `httpfs` is reserved for the *generated,
   run-outside-the-browser* R/Python reproduction scripts. **For MarineSensitivity's per-species/per-cell
   model_cell Parquet (already Hive-partitioned by `mdl_id` per the workflows CLAUDE.md), the same pattern
   applies almost directly**: fetch the one partition for the selected species/model, register it, query
   locally — do not try to run cross-partition SQL against S3 via httpfs from the browser.
2. **Serialize all DuckDB-WASM work on one promise chain against one connection.** A real shipped bug
   (`73c76df`, "cruise-lens race on the lazy sample_root") came from issuing a query before an
   asynchronously-registered buffer it depended on had finished. `engine.ts`'s `this.q = this.q.then(...)`
   pattern for both `load()` and `exec()` is the fix, and is worth adopting verbatim regardless of
   framework.
3. **Compose the whole MapLibre style as one object and `setStyle({diff:true})`; never `addLayer()` a
   basemap enhancement piecemeal after `load`.** Stated motivation: layers added imperatively after load can
   vanish across an unrelated style/theme swap; a single diffed style object cannot lose them
   (`basemap.ts:1-4`, `map.tsx:278-293`). Directly reusable for MarineSensitivity's own basemap + bathymetry/
   boundary composition.
4. **Interleaved deck.gl (`MapboxOverlay({interleaved:true})`) buys draw-order control (data under a
   specific boundary layer) at the cost of native picking** — you must hand-drive hover/click via
   `overlay.pickObject()` inside the map's own mouse handlers, and any deck layer's `beforeId` must be
   defensively stripped when the named MapLibre layer doesn't exist yet (composed style still loading),
   or the whole layer group is silently dropped (`map.tsx:264-273,294-301`).
5. **One ramp/color-scale selection function, called from every drawing surface — not reimplemented per
   chart.** Four separate call sites drifted (map/contour vs. Plotly's own `"Viridis"` vs. a hand-rolled
   diverging scale) before `lensRamp()`/`seriesRamp()` unified them, and this shipped with a **regression
   test asserting the wiring**, not just the per-variable rule (`ramps.ts`, `tests/ramps.default.test.ts`,
   commit `b1dbcf0`). Directly analogous to MarineSensitivity's own "one rule, multiple lenses" failure
   mode described in its CLAUDE.md for `merge_sql()`.
6. **A regex-ordering bug in a "pick the default X for this variable name" function is an easy, silent
   correctness bug**: `sigma_theta` matched `/temp|theta/` before `/sigma|dens/` and drew the wrong color
   ramp; `wind_dir_deg` matched `/wind/` and drew a *speed* ramp for a *direction* (CHANGELOG.md:18-21,
   `ramps.ts:61-73`). The fix was reordering + tightening regexes and adding named test cases for exactly
   these two variables — a reminder that any "infer X from a string name" heuristic needs its edge cases
   turned into permanent named tests the moment they're found (matches MarineSensitivity's own "regression
   cases are permanent" rule verbatim).
7. **Global-surface / merge-style bugs are invisible to aggregate checks and need dedicated fixtures** —
   not literally present in this repo, but the *shape* of the lesson (per-lens/lever fixtures, not just one
   end-to-end smoke check) is exactly what `contour_fixture.mjs` + `parity.R`/`parity.py` implement: every
   interpolation method × grain combination gets its own named fixture entry, and the parity scripts assert
   exact numeric equality against R/Python re-implementations, not just "looks reasonable."
8. **`html-to-image` capture correctness needs real engineering, not just "call toCanvas"**: force a
   WebGL redraw and wait two RAFs before capture (a `preserveDrawingBuffer` canvas only guarantees the
   *last* frame), inline self-hosted webfonts as `data:` URIs (the clone has no access to the live
   stylesheet cascade), and re-pin absolutely-positioned capture roots to `static` (their `left/top` are
   relative to the *live* page and draw off-canvas in an isolated clone) (`capture.ts:25-26,28-36`). Ship a
   luminance/pixel-spread self-check so a silently-blank capture (a `<picture>` wrapper once dropped the
   logo with zero errors) fails a test instead of shipping unnoticed.
9. **Puppeteer against a real, installed, *headed* Chrome — not the bundled headless Chromium — was
   deliberately chosen because an AI coding agent's own browser tooling could not be trusted to render a
   WebGL canvas** (`verify.mjs:1-4`: "the Claude-in-Chrome tab never paints, so this script is the only
   verification path"). For a MapLibre/deck.gl-heavy app, pixel-level `gl.readPixels()` probes at known
   lon/lat (`verify.mjs:71-83`) are sometimes the *only* way to assert a layer actually rendered, since
   the content lives entirely inside an opaque canvas to the DOM.
10. **Version-pin dependencies the moment a specific newer version breaks the dev toolchain, and say why
    inline**: `maplibre-gl` held below v6 because v6's module worker breaks Vite's dependency optimizer
    (`vite.config.ts:20`); `@duckdb/duckdb-wasm` explicitly excluded from `optimizeDeps` for the same class
    of reason. Small, cheap, and otherwise a very confusing "why does dev mode silently break" bug for
    whoever hits it next without the comment.
11. *(bonus, process-level)* **Dev/local-data-generation scripts are allowed to rot with hardcoded personal
    absolute paths** (`scripts/dev_coverage.R:6-8` — one path observed to reference an entirely unrelated
    project's scratch directory, evidently stale/copy-pasted). These scripts are explicitly "the dev copy
    ... until a release ships it" (comment in the file) and are not part of the committed, reproducible
    pipeline the same way the production `calcofi4db::build_*` functions are — a reminder (very much in
    the spirit of the MarineSensitivity CLAUDE.md's own reproducibility rules) to keep even *dev-only*
    tooling either parameterized/portable or clearly marked as disposable, not silently drifting.
12. *(bonus)* **A citation/attribution feature is only trustworthy if it has exactly one code path.**
    `src/cite.ts` is deliberately the *only* place that turns a `dataset` row into citation text, reused
    identically by the Sources line, the Sources modal, "Cite this data," every exported figure's footer,
    and the download bundle's `CITATION.md` (`cite.ts:1-11`, `bundle.ts:126`, `export.ts:34`) — and it is
    explicitly designed to **degrade field-by-field** (documented in a table, README.md § "What degrades,
    and how") rather than ever concatenating `undefined` into user-facing text. Directly applicable to
    MarineSensitivity's own per-dataset/provider citation needs if the new app ever surfaces attribution.

## 12. Recommendations for the new MarineSensitivity app (Svelte 5 + Vite)

**Reuse as-is (the ideas, not the React code):**
- The **materialize-then-query DuckDB-WASM pattern**: self-hosted MVP+EH bundles via `?url` imports (works
  identically in Vite regardless of framework), no COOP/COEP, whole-object `fetch()` + `registerFileBuffer`,
  a single serialized promise-chain wrapper around the one `AsyncDuckDBConnection` for every load/query.
  `src/engine.ts` is close to framework-agnostic already (a plain class + module-level singleton) — it could
  be ported to the new app almost verbatim, swapping only how components subscribe to the `timing` event
  emitter (trivial in Svelte via a store).
- **Catalog/release model**: `release.ts`'s shape (`catalog.json` with `objects[]` carrying `path`/`bytes`/
  `sha256`/`content_hash`/`partition_by`/`partition_value`, a `latest.txt`, a `versions.json`) maps almost
  1:1 onto MarineSensitivity's own `manifest.json`/`latest.txt`/`versions.json` registries described in its
  CLAUDE.md — reuse the *shape*, and note the "legacy compat path as fallback" idea for future schema
  evolution without breaking old deep links.
- **URL-as-state, `history.replaceState` only, `localStorage` only for chrome (panel geometry, disclosure
  state), never for view-defining state.** `state.ts`'s parse/serialize pair (clamp on parse, only emit
  non-default fields, delta-encode fold state against a default set, un-escape `,`/`:` for readability) is a
  clean, portable model for Svelte's own reactive stores + a `fromUrl`/`toUrl` pair.
- **One color-ramp/style-selection function per concern, called everywhere, with a wiring test.** Cheap
  insurance against the exact class of bug (`ramps.ts` history) that's easy to introduce the moment a
  second chart library (e.g. a non-MapLibre chart panel) enters the picture.
- **The SQL-template + `{{name}}` substitution + fixed RAW-vs-lit()-escaped allow-list** (`engine.ts:35,
  45-68`) is small, dependency-free, and safe by construction as long as the RAW set is disciplined about
  only ever holding app-generated fragments. Reuse directly.
- **Composed-basemap-as-one-object + `setStyle({diff:true})`**, and **interleaved deck.gl only if
  draw-order-relative-to-basemap-layers is actually needed** (it's real engineering cost — hand-rolled
  picking, `beforeId` existence guards).
- **Footer-stamped exports from one shared `stampLines()`/`drawFooter()`-equivalent**, reused by PNG, SVG,
  and the whole-view capture, so attribution can never appear on one export type and not another.
- **The zero-backend-fallback pattern for feedback** (prefilled GitHub issue + clipboard-copied screenshot)
  — cheap to build, and means the feature "works" even before any backend endpoint exists, exactly the
  bootstrapping order a new app will go through.

**Adapt:**
- **Panels/Sheet system**: the *behavior* (drag/resize/collapse-to-pill/maximize, phone bottom-sheet with
  velocity-aware detents, per-viewport `localStorage` geometry) is worth reproducing, but the
  implementation is React-hooks-shaped (`useState`/`useRef`-heavy pointer handlers); Svelte 5 runes make
  the same pointer-capture logic considerably shorter (no re-render-on-every-pointermove concern to route
  around) — rewrite, don't port line-for-line.
- **Tour**: keep `driver.js` (framework-agnostic already) and the `data-tour="..."` anchor convention
  (stable regardless of markup refactors), but the `TourActions` before/after-hook wiring can be simplified
  once panel state is Svelte stores rather than React state threaded through closures.
- **Capture/annotate**: keep the engineering lessons (double-RAF, font-embedding, absolute-position
  re-pinning, luminance self-check) but re-evaluate `html-to-image` vs. alternatives given Svelte's DOM
  shape is different (no React portal/clone quirks to work around, though `html-to-image`'s core
  serialize-to-SVG-foreignObject approach is still framework-agnostic and should still work).
- **Icon system**: MarineSensitivity likely doesn't have (or need) a shared brand-asset host the way CalCOFI
  does; still worth keeping "one path map, `<Icon name>` inline SVG in the app" and adding a sprite/CSS-mask
  export script only if/when other MarineSensitivity surfaces (docs, Quarto pages) need to consume the same
  icon set.
- **Theming**: without a shared `calcofi.io`-style brand host, inline the pre-paint theme script directly
  in `index.html` (same technique, own tokens) rather than fetching it cross-origin; keep the discipline of
  "app CSS only ever reads custom properties, never hardcodes a color."
- **Testing pyramid**: keep the four-layer shape (a handful of true unit tests for pure "pick X for Y"
  functions; a Puppeteer/Playwright state-matrix walking real URLs with pixel-level canvas probes where
  DOM assertions can't see into WebGL; smoke/bundle-inspection operator scripts; and — if MarineSensitivity
  ever ships a client-side computation analogous to contour interpolation — a bundled-worker parity fixture
  shared with the R package's tests) but consider Playwright over raw `puppeteer-core` for a *new* project
  (better multi-browser story, first-class trace viewer), unless there's a reason to match this repo's
  existing tool choice for consistency across the CalCOFI/MarineSensitivity toolchains.

**Avoid:**
- Don't reach for `httpfs`/range-request Parquet attachment for the browser's own interactive queries
  unless the access pattern genuinely requires querying *across* many partitions without downloading them —
  for a per-species/per-model working set, whole-object fetch + in-memory buffer will almost always be
  faster and simpler, and avoids the COOP/COEP question entirely.
- Don't add a client-side router / path-based navigation unless there's a concrete deep-linking need beyond
  what query-string state already covers — it adds a GitHub Pages 404-redirect complication this app
  sidesteps entirely by not having one.
- Don't let "how the local dev catalog is built" scripts (`scripts/dev_*.R` here) drift into
  personal-machine-only, hardcoded-path tools; either parameterize them (env vars, like the app itself) or
  keep them explicitly out of the reproducibility story the way this repo (mostly) does.
- Don't let more than one place compute "which color/ramp/style for this variable" or "the citation string
  for this dataset" — both bugs in this repo's own history came from exactly that duplication.
- Don't skip the `tsc --noEmit` CI gate even though Vite would build past type errors — it's one line in
  `pages.yml` and caught real issues before deploy here.

---

*Reviewed files: README.md, CHANGELOG.md, package.json, vite.config.ts, tsconfig.json, index.html,
.github/workflows/pages.yml; src/engine.ts, bundle.ts, release.ts, state.ts, main.tsx, App.tsx, map.tsx,
layers.tsx, basemap.ts, export.ts, capture.ts, cite.ts, track.ts, feedback.tsx, tour.ts, brand.ts, ui.tsx,
panels.tsx, icons.tsx, help.tsx, sources.tsx, annotate.tsx, contour.ts, ramps.ts, picker.tsx (partial),
curtain.tsx (partial); sql/_filters.sql, station.sql, slice_bio.sql, hex.sql, picker.sql, section.sql,
density.sql; scripts/verify.mjs, smoke_release.mjs, bundle_check.mjs, shot.mjs, build_icons.mjs,
dev_coverage.R (partial), parity/parity.R, parity/parity.py, parity/contour_fixture.mjs;
tests/ramps.default.test.ts; brand/v2.head.html; full `git log --oneline` (97 commits). Skipped:
node_modules, dist (neither present/relevant), most of the large chart/curtain/picker files beyond a skim
(charts.tsx, curtain.tsx, picker.tsx, sentence.tsx, categories.ts, variables.ts) per the task's "skim the
large UI files" instruction.*
