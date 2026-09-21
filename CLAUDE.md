# CLAUDE.md

This file guides Claude Code when working in `MarineSensitivity/workflows`.

> General R/Quarto/plumber conventions live in the parent `../CLAUDE.md` (2-space indent,
> snake_case, `|>`, roxygen2, `librarian::shelf()` outside packages, etc.). This file covers
> what is specific to the `workflows` repo and the **v8 "Marine Atlas"** pipeline.

## What this repo does

Ingests marine species distribution models (SDMs) from many sources onto a **global 0.05°
raster cell grid**, merges them per taxon, scores marine sensitivity over the **US study
area**, and publishes the result as **partitioned Parquet on S3** (the "marine-atlas") served
via **titiler** + a **STAC** catalog. The reusable logic lives in the sibling R package
**`msens`** (`../msens`); the notebooks here orchestrate it.

Each source is one `ingest_{provider}_{dataset}.qmd`; the pipeline then runs
`bootstrap_version → build_cell_grid → ingest_* → merge_models_prep → merge_models → merge_taxon →
score_zones → score_cell_metrics → score_zone_metrics → build_registry → release_marine-atlas`.

**v9 (2026-08, `prerelease`/`restricted`) = AquaX (`ax`) supersedes AquaMaps in US waters.**
`ingest_aquax.qmd` ingests 10,536 AquaX rasters that are *already on the `global05` grid* (cell_id =
pixel index, asserted) and *masked to US waters* (AquaX's own mask = the UNION over models of their
non-NA pixels — one model's NA area is its range crop, i.e. AquaX saying absent — measured against
`in_usa` in the ingest: 641,651 cells covering all but 27 `in_usa` cells, plus 103 coastline pixels the
cell table calls land — tolerated, reported). Values = `CUR_NR/10` on AquaMaps'
[0,100] scale; TSS cutoff recorded, not applied. **Supersession is a filter on the merge input**
(`msens::supersede_sql()`, applied where `mc_parts` is written): for a taxon in
`data/ax_supersedes_am.csv` (`supersedes` = TRUE), its `am` cells inside `dist/ax_mask.parquet` are
dropped — on BOTH surfaces — and `am` carries on everywhere else. `merge_sql(suit_ds = c("am","ax"))`
then treats both as suitability. `AX_SUPERSEDE=0` is the control run — AquaX EXCLUDED from the merge, so `merge_models`' content
hash must equal v8's checkpoint (`bd8b2931f17bb2b3`); leaving ax in but unsuperseded would still `max()` it; `AX_ABSENT_SUPERSEDES=1` lets the 2,742 "modeled, absent in US" species supersede too
(default off — a review question, listed in the ingest HTML). The ingest also builds + uploads both
COG representations — for an on-grid dataset these are **as delivered** (`native/ax_native`, Float32
0–1000, bit-exact) vs **as ingested** (`native/ax`, INT1U 1–100, the ≥1 threshold applied = what the
merge consumes), NOT original vs interpolated; `dataset.on_grid = TRUE` (front-matter `on_grid: true`)
makes the species app label the toggle *Delivered / As ingested* — and records the
per-species AquaX-vs-AquaMaps comparison (`data/ax_vs_am_summary.csv`; 20 least/most different
with preview deep links). `publish_native.qmd` only *registers* the ax COGs from `model_ax.csv`.

**v9: extinction risk is SPATIAL for NMFS DPS-listed species (`dps_nmfs`, `ingest_nmfs-dps.qmd`) — and
their merged model is the DISTRIBUTION, not the risk.** `listing` flattens NOAA's species page to its highest
domestic ESA status, so the humpback carried `NMFS:EN` species-wide from DPSs that breed outside US waters and
the plain rule `max(er, suit)` painted it a flat 100 over 14.7 M cells. Every species with a listed entity
below species level (19: humpback + Southern Resident orca, Cook Inlet beluga, MHI false killer whale,
Western Steller sea lion, Beringia bearded / Arctic ringed seal, salmonid + steelhead ESUs, both sturgeons,
sawfish, bocaccio, yelloweye, eulachon) gets a per-cell ER surface from the **authoritative NMFS
critical-habitat service** (`maps.fisheries.noaa.gov/…/All_NMFS_Critical_Habitat/MapServer`, per Jennifer
Schultz, NMFS, 2026-04-16 — DPS-level *range* services follow end of CY2026 and become the source then): each
listed entity's polygons at its status, the IUCN range at the species' BASELINE elsewhere — `NMFS:LC` + MMPA
= 20 for a marine mammal (the same convention `merge_models_prep` uses; `compute_er_score()` IGNORES
`is_mmpa` for IUCN codes, so `IUCN:LC + MMPA` was silently 1), the IUCN category otherwise. These taxa merge
through **`msens::dps_sql()`** (NOT the turtle rule): `val` = suitability (ax/am) masked to the ER footprint,
valued at the ER where nothing models the cell, and the per-cell ER is written BESIDE it —
`dist_merged_er/dataset=ms_merge/` → `sdm.duckdb` `model_cell_er` → `score_cell_metrics` multiplies it in
where `taxon.er_mode = 'cell'` (`'premultiplied'` = sea turtles, whose `val` is already ER × suit via
`turtle_sql()`; `'taxon'` = the scalar `er_score`). The turtle rule on the humpback (ER 1 over 99.8 % of its
range) had collapsed the merged surface to 1 outside critical habitat: the app drew the weight, not the
whale. `ch_*` is NOT an input for dps taxa (it carries the species-level ER). `publish_native` paints
`native/dps_nmfs/*.tif` so the species app can draw the ER layer (an input with no published surface renders
as a struck-through pill). Cached snapshot in `raw/fisheries.noaa.gov/All_NMFS_Critical_Habitat/`;
`DPS_FETCH=1` refreshes. **Spatial-only reruns:** `scripts/run_version.sh --spatial --from ingest_dps_nmfs`
= `REDO_INGEST` on the dps ingest, `REDO_MC_PARTS_SPATIAL=1 REDO_MERGE_SPATIAL=1` (refresh only the ~25
spatial-ER taxa's merge-input partitions + surfaces; the batched surfaces of the other ~17,750 are kept),
`REDO_MERGED_COG_SPATIAL=1` (repaint only their merged COGs; `REDO_MERGED_COG_KEYS=a,b` names taxa by hand)
— minutes instead of the ~3 h a full re-merge + repaint costs. Both keyed refreshes assert the positional
maps (`mkey_id`, `mmid`) have not moved before touching a partition. There is no "v9.1": the DPS work is
part of v9.

**v7.1 (release id `v7b`, 2026-09, `prerelease`/`restricted`) = a surgical PATCH of public v7**, not a
new build: `build_v7b.qmd` clones v7's `sdm.duckdb` copy-on-write, rewrites the six merged sea-turtle
models under their EXISTING `mdl_seq` 54238–54243, and re-evaluates v7's own scoring chunks by label from
`old/calc_scores_v7.qmd`. Two rules, each a flag whose control value is OFF: **turtle core habitat**
(`TURTLE_SUIT_MIN=50`, `TURTLE_FILL=none` → `msens::turtle_sql(suit_min, fill = NA, half_even = TRUE,
ch_outer = TRUE)`: AquaMaps suitability enters only where P ≥ 0.5 and a range cell with none is DROPPED,
not filled at 1; critical habitat joins the ER footprint so no designated cell is ever trimmed) and the
**coverage floor** (`COVERAGE_FLOOR=0.05` → `msens::coverage_sql()` / `coverage_floor()`: a Program-Area
component whose scored cells cover < 5 % of the area has its `_ecoregion_rescaled` row deleted — absent,
never zero — and `{component}_coverage` zone metrics are written). With all flags off the notebook
reproduces v7 **exactly** (2,022,909 turtle cells, 0 diffs; `cell_metric` bit-exact); each single-flag
control moves only what it should and no `primprod*` row moves in any run (the productivity fix is v10's).
`data/manifests/build_v7b.json` holds **reference digests** a same-flag build on any machine must
reproduce (`BUILD_REFERENCE=1` is the only way to move them) — DOUBLE surfaces are digested as
`CAST(.. AS FLOAT)` because parallel `SUM()` reassociates and raw-double digests differ between two correct
builds. The release states its rules as data: a `release_method` table → `manifest$methods` (optional; the
docs gate the method prose on it), and `versions.csv` has an optional `prev` (`v7b → v7`) because a patch
dated after its successors must not take its lineage from `released` order. Dotted ids (`v7.1`) are
rejected in 13 places by design; the id grammar is `^v[0-9]+[a-z]?$`.

**Bumping the version on the same grid (v8 → v9) is `bootstrap_version.qmd`**, not a re-ingest:
it clones the unchanged `dist/dataset=*` from `ver_prev` copy-on-write (APFS `cp -c`; hardlinks on
Linux) so the ingests *resume*, and `BOOTSTRAP_VERIFY=1` re-hashes them against their checkpoints.
Four readers used to assume `ver_prev` had v7's schema or a pre-built version dir — all now
introspect: `build_cell_grid` copies the `cell` table from `ver_prev` when only the shared COG exists
(a fresh `sdm.duckdb` otherwise passes every "run build_cell_grid first" check while empty),
`score_zones` resolves `is_ok`/`is_valid_usa`, `merge_models_prep` resolves `worms_id`/`taxon_id`
(and honours a native `worms_id` column in `model_*.csv`), `build_zone_cells` gates an unreleased
version against `ver_prev`. `titiler-v8` is the stock `/cog` tiler for EVERY release (the apps
hardcode it) — a new version gets no `titiler-v{n}`; `TITILER_SERVICE` names the compose service the
release notebook rebuilds. A new dataset also needs the docs: `data/release_notes.yml` entry, a
hand-written note in `data-sources.qmd` (guarded by `"ax" %in% ds$ds_key`), and `references.bib`.

## Commands

```r
# from workflows/ — prefer targets (renders the .qmd + tracks deps) over hand-rendering
Rscript -e 'targets::tar_make()'                    # full pipeline
Rscript -e 'targets::tar_visnetwork()'              # dependency DAG
Rscript -e 'targets::tar_make("merge_taxon")'       # one target

# render a single notebook to HTML (real runs — produces the tracked _output/*.html):
quarto render score_cell_metrics.qmd
# purl+source is ONLY for quick diagnostics — it executes chunks but SKIPS the HTML artifact:
Rscript -e 'suppressMessages(library(knitr)); purl("merge_taxon.qmd","/tmp/x.R",quiet=TRUE,documentation=0); source("/tmp/x.R")'

# after editing the msens package, reinstall so library(msens) isn't stale
Rscript -e 'devtools::install("../msens")'          # or source the file directly in a pinch
```

### Reproducibility (non-negotiable — do not regress)

**Every process must be baked into a committed QMD and executed by RENDERING to HTML.** Two
requirements, both mandatory:

1. **Baked into the QMD** — every step (ingest, merge, score, publish, **S3 sync, server + Shiny-app
   deploy**) is a chunk in the owning notebook, gated behind an env flag (default off), **never** an
   ad-hoc `ssh`/`aws`/scratch script. Deploys live in `release_marine-atlas.qmd`: `RELEASE_DEPLOY`
   (serve.duckdb + titiler-v8 + STAC), `DEPLOY_APPS` (pull `apps_v8` + reload it via `restart.txt`
   — since the 2026-08-12 cutover this IS the live `/scores` + `/species`), `RELEASE_S3_TABLES` (push `tables/` incl `native_asset` without the
   full serve cutover), `DEPLOY_TABLES` (rsync `tables/` to the server + repoint the views at that
   LOCAL copy — no S3 push, no titiler/caddy restart).
2. **Rendered to HTML** — run via `targets::tar_make()` or `quarto render`, which produce the tracked
   `_output/*.html` (Design mermaid + summary tables) that the workflows landing page links.
   `purl(...) + source()` is for quick diagnostics ONLY — it skips the HTML and the content-hash
   checkpoint, so it is not a substitute for a real run.

3. **The orchestration is part of the process — commit the loop too.** A notebook parameterized by
   version is only half reproducible if the *loop over versions* lives in a throwaway shell script.
   Multi-version / multi-dataset drivers are committed under `scripts/` (`backfill_all.sh`,
   `render_validate.R`) and **must go through `scripts/srv_render.sh`**, never `docker exec quarto
   render` directly — that bypasses its `git merge --ff-only origin/main` guard, which is how a v3
   run once rendered a stale `sdm_db_path()` and reported success while producing nothing.
   Likewise, forcing a rebuild is an **env flag** (`REDO_*`), never moving output directories aside
   by hand: a hand-`mv` leaves no record of what was rebuilt or why.

   Corollary — **the msens the container runs is now converged, not pinned by hand (2026-08-27).**
   `server/rstudio/Dockerfile` tracks msens `main` (`rstudio/build.sh` resolves it to a SHA so the
   build is recorded *and* cache-correct), and `rstudio/cont-init/03_msens_from_share` reinstalls
   msens from the `/share` checkout at every container START when the two disagree. The old
   hand-maintained `MSENS_MIN` floor rotted exactly as you would expect: it sat at 0.31.0 while
   msens reached 0.36.0, and a recreate for unrelated packages took **both public apps down** with
   `'study_areas' is not an exported object`. The exports assertion in the Dockerfile stays — it is
   the check written in terms of what the apps actually call.

   Historical corollary — **`MSENS_MIN` guarded the package, because the version number is not proof.** A
   notebook whose logic lives in `msens` is only as current as the *installed* package. The server
   once reported `msens 0.14.0` while running a `manifest_build()` that predated zone PMTiles, so
   all seven v1–v7 manifests regenerated there came out silently missing their zone tiles. Every
   `msens` behavior change gets a `Version:` bump **and** a `NEWS.md` entry (see `../CLAUDE.md`) so
   "same version" actually means "same code", and the Dockerfile's `MSENS_REF`/`MSENS_MIN` pins
   move with it — otherwise the next container recreate restores the stale build.

4. **Reproducibility beats uptime — always.** When a reproducible fix and a fast in-place
   workaround are both available, take the reproducible one **even if it means brief downtime**.
   Never `docker exec` a change into a running container, hand-edit config inside one, or write
   to `Renviron.site` to dodge a rebuild. Missing packages or env → fix `server/rstudio/Dockerfile`
   or `server/docker-compose.yml`, rebuild the image, recreate the container. Drift between a
   running container and its image is a **bug to fix now** (bake it in), not a reason to avoid
   recreating. A brief outage is cheaper than state nobody can reproduce.

**ONE app, every version (cutover 2026-08-12).** There is no longer a per-generation fork to
keep in sync. `apps` branch `main` → `apps_v8/{scores,species}` is served as **`/scores` and
`/species`**, and renders any published release from `?ver=` (default = `latest.txt`, currently
v7). `DEPLOY_APPS=1` reloads it — and since the cutover that means **restarting what everyone
sees**, not a parallel deployment.

The 18 former instances (`mapgl`, `mapsp`, `mapgl_v1-v6`, `mapsp_v1-v6`, `scores_v6/v8`,
`species_v6/v8`) are in `/share/shiny_apps_retired/` — moved aside, not deleted, so the cutover
is reversible — with Caddy 301ing every old URL to `/v{n}/scores/` (and `/mapgl`, `/mapsp` to
`/v7/…`, which is what they were), query intact.

**Granular vs publishing runs.** A run that asks ONLY for `DEPLOY_*`/`CHECK_PREVIEW` targets
converges the *server* on what is already published: it skips staging, the S3 push, the view DB,
STAC and the manifest (`do_granular`/`do_stage` in `release_marine-atlas.qmd`), and it does not open
`sdm_db`. This matters because `do_s3` defaults **on** — `DEPLOY_APPS=1` alone used to re-stage every
table, re-hash the ~580M-row serving surface and then **push to S3** as a side effect of restarting
two Shiny apps, so an app reload was only safe if you remembered `RELEASE_NO_S3=1`. Any publishing
intent (`RELEASE_DEPLOY`, `RELEASE_RAW`, `RELEASE_S3_TABLES`, `RELEASE_CORS`, `PROMOTE_LATEST`) opts
back into the full path. A granular deploy now takes ~45 s instead of minutes.

**Env flags** (gate expensive/side-effecting steps): `REDO_INGEST=1` (rebuild an ingest),
`REDO_WORMS=1` (rebuild the worms table), `SCORE_V7COMMON=1` (score only v7's species, for
apples-to-apples), `SCORE_ALLBIRDS=1` (disable the marine-bird cull), `RELEASE_NO_S3=1` /
`RELEASE_RAW=1` / `RELEASE_DEPLOY=1` (release + serving), `DEPLOY_APPS=1`
(reload the Shiny apps), `DEPLOY_TABLES=1` (sync `tables/` + `model_cell/` local + repoint
the views), `DEPLOY_API=1` (pull the api repo + **rebuild** the plumber image — msens lives in that
image, which is separate from `rstudio`, so `DEPLOY_APPS` never updates it),
`DEPLOY_CADDY=1` (pull the server repo, `docker compose build caddy` (no-op unless its
Dockerfile changed), **`caddy validate` the Caddyfile with that image**, then `up -d` + `restart
caddy` — the URL surface on its own, so a routing fix need not rebuild titiler; the retirement
301s live there, and they must carry `&{query}` or every published `?mdl_seq=` deep link loses
its model id in flight), `DEPLOY_DOCS=1` (force the `docs-preview` sidecar to pull the docs
repo's `gh-pages-preview` branch — the rendered books of *restricted* releases the preview host
serves; it polls every 5 min anyway), `DEPLOY_ACCESS=1` (converge the Cloudflare Access applications on `versions.json` —
one application + reviewer policy per restricted version, `server/cloudflare/access.sh`; setup
runbook in `server/cloudflare/README.md`), `CHECK_PREVIEW=1` (curl-prove the review gate: public
host never renders a restricted version, preview host is closed without a Cloudflare Access
token and open with one, origin-direct is 401, restricted docs are off GitHub Pages),
`DEPLOY_TITILER=1` (restart `titiler-v8` alone — **required after any `REDO_MERGED_COG` /
`publish_native` run that repaints COGs**, because `native/*` keys are STABLE, so new bytes land
under a URL whose header GDAL's `/vsicurl` has cached in-process; a shrunken COG then reads past
EOF and z2–z4 return HTTP 500 while z5+ look fine),
`REDO_MERGED_COG=1` (repaint just the merged whole-range COGs — what a re-merge invalidates,
without `REDO_NATIVE`'s 7 GB IUCN gpkg rebuild and am re-sort), `REDO_MERGED_COG_SPATIAL=1` /
`REDO_MERGED_COG_KEYS=k1,k2` (repaint only the spatial-ER taxa / named taxa), `REDO_MC_PARTS_SPATIAL=1` /
`REDO_MERGE_SPATIAL=1` (`merge_models`: refresh only the spatial-ER taxa), `REDO_DPS_COG=1`, `AX_COG=1`/`AX_COG_S3=1` (build/upload
the AquaX COGs in `ingest_aquax`), `AX_TEST_N=<n>` (ingest smoke test: nothing written to `data/`),
`AX_SUPERSEDE=0` (control merge), `AX_ABSENT_SUPERSEDES=1`, `AX_APPLY_CUTOFF=1`,
`BOOTSTRAP_VERIFY=1` / `BOOTSTRAP_SKIP_DS=a,b` (`bootstrap_version`), `TITILER_SERVICE=titiler-v8`,
`MANIFEST_REGISTRY_ONLY=1` (`build_version_manifest`: publish `versions.json` ALONE — plus `latest.txt`
only under `PROMOTE_LATEST=1` — without rebuilding or re-pushing any manifest; how a legacy release is
registered), `TURTLE_SUIT_MIN` / `TURTLE_FILL` / `COVERAGE_FLOOR` / `REDO_BUILD=1` / `BUILD_REFERENCE=1`
(`build_v7b`), `BACKFILL_NO_STAC=1` / `BACKFILL_NO_RELOAD=1` / `BACKFILL_NO_INDEX=1` (backfill opt-outs),
`BUILD_MEMORY_GB` / `BUILD_THREADS` (`libs/duckdb_budget.R`), `SRV_MIN_AVAIL_MB` (`srv_render.sh` watchdog).

**The pre-release review gate (`preview.marinesensitivity.org`, 2026-08-15).** A release has
`access` (`public` | `restricted`) beside `status` in `data/versions.csv` → `versions.json`; a
`restricted` release (a pre-release under review by SDM providers / BOEM-NOAA colleagues) is
served ONLY through the signed-in preview host (Cloudflare Access in front, Caddy `jwtauth`
verifying the Access JWT at the origin). Enforcement in the apps is by **process, not header**:
Shiny Server OSS opens its own websocket to the R worker, so no proxy header reaches
`session$request` — instead a second Shiny Server block (`:3839`, `server/rstudio/shiny-server.conf`)
serves 3-line wrapper apps that set `MS_PREVIEW=1`, and `msens::atlas_allow_access()` lets that
process resolve restricted versions while the public `:3838` process cannot.

**The version is the URL PATH on BOTH app hosts** — `/v7/scores/` publicly and `/v8/scores/` on the
preview host (`msens::product_urls()`; `server/caddy/app_version_routes.caddy` is imported by both
vhosts so they cannot drift). On the preview host it is also what makes per-version access possible
at all, since Cloudflare Access scopes a policy by path and never by query. `?ver=` still resolves:
Caddy 301s it to the canonical path, so every published deep link and report keeps working.
Caddy strips the prefix and passes the version to the app as **`X-MS-Version`** (a header it SETS
from the path, like `X-MS-User`); `ui(req)` prefers it and falls back to `?ver=` on the public host.
Never force `?ver=` onto requests there: shiny-server's own client bundle
(`<app>/__assets__/…`) 404s if the request carries ANY query string, and the symptom is an app that
draws its sidebar and hangs on "Loading map…".

**Who may see a restricted release** is `PREVIEW_REVIEWERS_<VER>` in the server `.env`
(comma-separated emails; `PREVIEW_ADMINS` is the catch-all for the landing page and anything not
separately gated). Defaults today: admins = `ben@oceanmetrics.io`; `PREVIEW_REVIEWERS_V8` =
`ben@oceanmetrics.io,timothy.white@boem.gov`. Edit `.env`, then
`ssh msens 'cd /share/github/MarineSensitivity/server && set -a && . ./.env && set +a && cloudflare/access.sh'`
(or `DEPLOY_ACCESS=1`). Changing an existing version's list is a policy-only change — no new AUDs,
so no `DEPLOY_CADDY`. A NEW restricted version mints applications, so it needs the new
`CF_ACCESS_AUD` line pasted into `.env` and then `DEPLOY_CADDY=1`. **`PREVIEW_RESTRICTED_VERSIONS`
(same `.env`, a regex alternation `v8|v9`) is the third per-version entry:** the PUBLIC app host 302s
`/v{n}/scores|species` for those versions to the review host, query intact (2026-08-28) — before that
the public app could not resolve a restricted version and silently rendered v7, so the homepage's v8
links "oddly ended up on v7". `DEPLOY_CADDY` refuses to restart Caddy if the list disagrees with
`versions.json`, and `CHECK_PREVIEW` asserts the 302; a version flipping to `public` must be removed.

**Deploy order for a coordinated routes+apps change: `DEPLOY_APPS=1` FIRST, then `DEPLOY_CADDY=1`.**
`DEPLOY_CADDY` runs `server/caddy/test/run.sh`, which asserts end-to-end that `/v8/scores/` renders
v8 — so if the routing half lands while the apps still expect the old contract, the test fails and
the notebook stops before it reaches the app deploy. **On the preview host
the version is the URL PATH** (`/v9/scores/`, `/v9/species/`, `/docs/v9/` —
`msens::preview_app_url()`), never `?ver=`, because Cloudflare Access holds one reviewer policy per
version and scopes it by path; `server/caddy/preview_routes.caddy` strips the prefix and hands the
version to the app as `X-MS-Version` (`server/caddy/test/run.sh` proves the routes; `DEPLOY_CADDY` runs it). And the
session renders the version its PAGE was served for: `ui(req)` embeds `msens::ver_token_sign(ver)`
and the server trusts only that token (`ver_of_session()`), because `url_search`/`url_pathname`
are client-supplied — a v9 reviewer must not steer the shared preview process to v10. Readers derive
`access` fail-closed when a registry predates the field (`prerelease → restricted`), so **publish
`versions.json` (`build_version_manifest`) BEFORE deploying an app/msens that reads it** or a
public pre-release goes dark. Restricted docs are published by the docs CI to `gh-pages-preview`
(GitHub Pages cannot be gated) and served from `/share/docs_preview`. Plan:
`.claude/plans/2026-08-15 pre-release review gate — preview host + Cloudflare Access.md`.

**Serving reads LOCAL Parquet, not S3.** S3 is the published artifact; the server keeps a versioned
copy under `/share/data/big/{ver}/` and `serve.duckdb`'s views point there. Over HTTPS every query
re-reads each Parquet footer — a ~140 ms floor that costs **18–24×** on the small interactive
queries (Program-areas panel: 0.147 s S3 vs 0.008 s local warm; 0.364 s vs 0.015 s cold). `tables/`
is only 445 MB and syncs in ~14 s. `cell_model` follows the same rule for a different reason (its
per-cell/per-polygon queries touch many partitions, which fails outright over HTTPS).

`model_cell` is served **both ways, by different consumers** — the one case where "stays on S3" is
only half true, so read this before deciding a `DEPLOY_TABLES` sync is redundant:

- **titiler reads it from S3**, one partition per tile as a point read, via its own `read_parquet`
  path built from the `SERVE_MODEL_CELL` env var (`server/titiler/factory.py`). It uses
  `serve.duckdb` only for the `model` registry and never touches that view.
- **the apps read a LOCAL copy** (~3.3 GB, 40k small files, so budget ~1 h for the rsync).
  `DEPLOY_TABLES=1` syncs it. Without it the S3 glob needs LIST credentials the app container does
  not have, and the failure is **silent**: the `tryCatch` around `mdl_bbox()` and the clicked-cell
  lookup returns NULL/NA, so the species app quietly falls back to the study-area extent and a
  blank value rather than erroring.

### Unit tests (non-negotiable — the model logic must stay guarded)

The scientific rules of the model (how per-dataset cells merge into one surface per taxon, how
extinction risk is scored, how ranges constrain AquaMaps) are **fragile**: a wrong join, a dropped
`WHERE`, an added `GROUP BY`, or a US-vs-global scope slip silently corrupts results. Guard every
such rule with a **testthat unit test** in `../msens/tests/testthat/`.

- **Logic lives in `msens`, not in QMD strings.** Merge rules are `msens::merge_sql()` /
  `msens::turtle_sql()` (single source of truth); the notebook *calls* them and
  `test-merge.R` *asserts* them, so the notebook and the tests can never drift. Put new model logic
  (a new dataset's constraint, a new scoring term) behind an `msens` function the same way.
- **Every rule gets a fixture that asserts its exact output.** `test-merge.R` has one synthetic
  taxon per category (range-only, both-masked, `iucn_range_outside_us_eez`-excluded, am-only single,
  am-only multi-model no-dedup, turtle multiplicative) with the expected merged cells hard-asserted.
  A category that isn't tested is a category that will silently break.
- **Add/update tests in the SAME change as the logic.** When you add a distribution model or tweak a
  merge/score rule, add or update its fixture + assertion *before* rendering. Treat a red test as a
  hard stop. Run `Rscript -e 'devtools::test("../msens")'` (or `testthat::test_file(...)`); reinstall
  msens after edits so the notebook picks up the new rule.
- **Regression cases are permanent.** When a bug is found (e.g. the v8 rewrite dropping the
  `iucn_range_outside_us_eez` exclusion, or deduping multi-AquaMaps-model am cells), encode the
  fixed behavior as a named assertion so it can't regress.
- **The merge has TWO output surfaces and BOTH are masked.** `$us` (scoring, `dist_merged`) and
  `$global` (what the species app DRAWS, `dist_merged_global` → `native/merged/*.tif`) apply the
  same range mask; they differ only by the `in_usa` trim and the am-only branch `$us` alone
  carries. `$global` was briefly a FULL OUTER union with the whole am footprint, which painted raw
  AquaMaps over-prediction into all 3,461 both-taxa COGs — half the walrus surface, down to 9.75°N
  — for a month (MarineSensitivity/apps#8). **The merge manifest hash fingerprints `$us` only**, so
  it structurally could not see it, and the existing global-surface tests described the union
  instead of questioning it. Every assertion about `$global` must be one the union FAILS; the
  general one (`no global cell outside the range footprint`) now lives in `test-merge.R` and is
  re-asserted on the written parquet by `merge_models.qmd`'s masking-check chunk.

Whole-pipeline correctness is ALSO enforced by `msens::pra_score_delta` (version-equivalence gate)
and the `stopifnot`/content-hash validation chunks — but those catch *aggregate* drift; the unit
tests catch *rule-level* breakage the aggregates can hide (the 750 no_eez species had near-zero
aggregate impact yet were scientifically wrong).

## Architecture

### Data flow

```
sources (AquaMaps am, BirdLife bl, IUCN/FWS/NMFS ranges, SWOT turtles, [gm/nc density])
   │  ingest_*.qmd  → cells_from_ranges/raster (exactextractr / bilinear)
   ▼
dist/dataset={ds_key}/*.parquet  (mdl_key, cell_id, val)  +  model_{ds}.csv   [~/_big or /share]
   │  merge_models_prep (crosswalk → taxon, taxon_model, listing, governing er_score)
   │  merge_models      (US-scoped max-merge + turtle ×; range cells carry governing er_score)
   ▼
dist_merged/dataset=ms_merge/*.parquet  +  merge.duckdb (taxon, model_cell)
   │  merge_taxon   (validity flags, range/rarity, is_marine, sp_cat by taxonomy)
   │  score_zones → score_cell_metrics → score_zone_metrics  (v7-faithful; US cells)
   ▼
sdm.duckdb (cell, taxon, model_cell, cell_metric, zone*, metric) + build_registry (dataset, model)
   │  release_marine-atlas.qmd
   ▼
s3://oceanmetrics.io-public/marine-atlas/v8/ {tables/, dist_merged/, serve/, registry/}
   │  + STAC catalog (file.marinesensitivity.org/stac/v8) + titiler-v8 serving
```

### `msens:` YAML-driven targets (don't hand-edit `_targets.R`)

`_targets.R` calls `msens::build_targets_list()`, which parses the `msens:` front-matter of
every `*.qmd` (`target_name`, `workflow_type`, `dependency`, `output`, and for ingests a
`dataset:` block). **Add a dataset by adding the notebook with a `msens:` block** — the
`dataset:` block is the single source of truth the `build_registry` target consolidates into
the `dataset` table (v7 wrote `dataset`/`model` inline per ingest; v8 must NOT — see
`build_registry.qmd`). `dependency: [auto]` depends on all upstream ingests.

### `mdl_key` — the stable model identifier (replaces v7 `mdl_seq`)

`{ds_key}|{sp_id}[|{interval}]`, **pipe separator**. Raw: `am|Fis-29291`, `bl|{sisid}`,
`gm|{id}|01`. Merged: `ms_merge|WORMS:137209` / `ms_merge|BOTW:22694927`. All URLs/serving key
on `mdl_key` (`msens::mdl_key_raw()` / `mdl_key_merged()`). Values live in a `val` column —
**`value` is a DuckDB reserved word**, never use it as a column name.

### Grid, scoring model, key conventions

- **Grid**: global 0.05° `[-180,180]` ocean cells, `cell_id = 1:ncell`, COG `r_cellid_global.tif`;
  `cell` table carries env + `in_usa`/`in_pra`. Scoring runs over `in_usa` (~634k cells).
- **Whole range, no land mask** (esp. birds) — capture the entire global range; `pct_marine`
  is derived, not a mask.
- **`er_score`** (extinction-risk): governing ER computed in `merge_models_prep` (most-protective
  across a taxon's datasets, US-national overrides IUCN); **MMPA floor by taxonomy** (all WoRMS
  Mammalia), **MBTA floor by the FWS CFR 50 §10.13 list** (not all Aves). Applied to range cells
  at merge (the "fitting point"). `compute_er_score()` errors on bad codes.
- **`sp_cat` by taxonomy** (`merge_taxon`): WoRMS class/phylum + BirdLife→bird → bird, mammal,
  turtle, fish, coral, invertebrate, primary_producer; reptile/amphibian EXCLUDED from scoring.
  No `other` bucket. AlgaeBase taxa missing from the WoRMS download are fetched via
  `worrms::wm_record` + cached in `data/worms_taxonomy_supplement.csv`.
- **`is_marine`** cull: birds scored only if in a marine/coastal family AND whole-range
  `pct_marine ≥ 5%`, or curated-in (`data/marine_bird_families.csv` + `marine_birds_curation.csv`).
- **Version equivalence**: on a bump, `msens::pra_score_delta` must show Program-Area scores
  stay ~equivalent on common inputs.

### Release + serving

- **`release_marine-atlas.qmd`** publishes Parquet to S3 (`msens::attach_atlas()` reads it back;
  the dotted bucket `oceanmetrics.io-public` needs **path-style** addressing + the `aws` extension
  for globs). The **serving surface** is `serve/model_cell/mdl_id=*/data_0.parquet` — Hive-**partitioned
  by the dense `mdl_id`**, so a titiler tile reads exactly one partition as a point read. (It was
  once a single file sorted by `mdl_key`; the release now syncs with `--delete` to prune that.)
- **Serving = a tiny view-only DuckDB** (`serve.duckdb`, KB) whose `model_cell` is a VIEW over the
  S3 Parquet — never a multi-GB DB. `titiler-v8` (parallel to v7) reads it; the factory
  (`../server/titiler/factory.py`) is env-driven and its SQL validator blocks `read_parquet` in
  *client* SQL, so the client sends `SELECT cell_id, val AS value FROM model_cell WHERE mdl_key='…'`.
- **STAC** via `msens::stac_build(version="v8")`. Deploy is gated behind `RELEASE_DEPLOY=1`.

### One app, every version (2026-08)

The version is **data, not a code fork**. Releases used to ship as frozen copies of the apps repo
symlinked at `/scores_v{n}`, stranding every improvement in the newest fork. Three
version-independent registries replace that:

| registry | what it settles | where |
|---|---|---|
| **version** | which releases exist, which is promoted, how to draw each | `latest.txt`, `versions.json`, `{ver}/manifest.json` on S3; `msens/R/version.R` |
| **grid** | which grid a release's `cell_id` indexes | `msens/R/grid.R` |
| **zone-set** | spatial units by *vintage*, not by release | `data/zone_sets.csv`; `msens/R/zone_set.R` |

- **`manifest.json` is the contract.** `msens::manifest_build()` *introspects* a release (public
  model id, tables present, cell-level metrics, spatial units) so it cannot drift from the data.
  Capabilities derive from **presence and default to FALSE** — a release without `cell_model` must
  not advertise a per-cell species list. Publishing v9 means publishing a manifest, not editing an app.
- **Promotion is gated**: `latest.txt` is written only under `PROMOTE_LATEST=1` and only names a
  `released` version. `data/versions.csv` is the registry (status + access per version);
  `latest.txt` on S3 is what the apps default to, and a `prerelease` is reachable only at its
  own path (`/v{n}/scores/`, restricted ones on the preview host).
- **Two grids, and `cell_id` means a different place on each.** `usa05` (v1–v7) is 3103×2006 in
  **0–360 longitude**, running 141.10°E east *across the antimeridian*; `global05` (v8) is
  7200×3600 in −180..180. The cell-id COGs are **lookup images** (pixel value = cell id), so never
  infer a grid's geometry from them — use `msens::grid_spec_for()`.
- **`zone_cell` is shared, not per-release.** It depends only on (geometry × grid), so it lives at
  `zones/{zone_set_key}/{grid_id}/zone_cell.parquet` and one extraction serves every release on that
  grid. Measured: one program-area geometry covers v2–v8, one ecoregion geometry covers v1–v8.
  Subregions are the canonical `AK/AT/GA/PA` dissolved from the ecoregion `region_key` rollup —
  Program-Area-derived subregions could never represent the **Atlantic**, since the 2026 program has
  no Atlantic areas.
- **Scores are COGs, not SQL-per-tile.** `publish_score_cogs.qmd` writes one raster per
  (metric × subregion) into the content-addressed store; the app reads the href and a build-time
  rescale from the manifest. Verified byte-identical to the old factory on `FULL` surfaces.
  **No overviews** — the factory decimates from full resolution per request, so a pyramid disagrees
  at low zoom.
- **The object key includes the ENCODING, not just the payload** (`content_hash_encoded()`).
  Rewriting objects at a stable URL left GDAL's `/vsicurl` serving a cached header for bytes that no
  longer existed: z5+ fine, z2–z4 HTTP 500.

### A release that lands registers itself (2026-09-21)

Publishing tables + a manifest is not the end of a release, and what followed used to be left to
memory. `backfill_versions.qmd` (the legacy v1–v7b path; every `manifest`/`models` stage) now ends by
(1) building the release's **STAC** tree and registering it in the DEPLOYED root catalog
(`msens::stac_build()` introspects `mdl_key` vs `mdl_seq`; `msens::stac_catalog_register()` is
idempotent and keeps children in release order, `v7 v7b v8 v9`) and reading it back through
`file.marinesensitivity.org/stac`, then (2) **reloading the apps on BOTH Shiny instances**
(`libs/app_reload.R` — public `:3838` AND preview `:3839`; three notebooks touched only the public
`restart.txt`, so v7b's review-host app answered 500 until `DEPLOY_APPS=1` happened to run).
`scripts/backfill_all.sh` then refreshes the **storage index** once per run (`index` stage; also after
`all` and `manifest`). A RESTRICTED release gets no index pages by design — they appear when it becomes
public. Opt-outs: `BACKFILL_NO_STAC=1`, `BACKFILL_NO_RELOAD=1`, `BACKFILL_NO_INDEX=1`. `VAR=value`
arguments to `backfill_all.sh` are forwarded to every render (`V7_CELLMODEL_REDO=1`).

Program-Area **geometry belongs to the zone-set vintage, not to a release**: the API reads
`derived/{ver}/ply_programareas_2026_{ver}.gpkg` when it exists and otherwise the canonical
`derived/v2/ply_programareas_2026.gpkg` (identical v2–v8; every v9 Program-Area report was a 500 for
want of a file that would have been a copy).

### Server gotchas (both cost real time)

- **`docker exec` runs as ROOT.** Use `scripts/srv_render.sh` (which passes `-u 1000:1000` and
  re-checks afterwards), never a bare `docker exec rstudio quarto render`. Uids are *not* misaligned —
  the container's `rstudio` user is already uid 1000 — you just have to ask. A sweep found **23,729**
  root-owned files under `/share/data`; the damage is silent until `git merge` aborts with
  `unable to unlink … Permission denied`.
- **msens1 is 16 GB / 4 cores / NO swap, shared with production** (apps, API, two titilers, ERDDAP).
  Never hardcode `PRAGMA memory_limit='12GB'`/`threads=6` in a notebook that renders there:
  `source(here("libs/duckdb_budget.R")); duckdb_tune(con, tmp_dir)` sizes DuckDB to 40 % of
  MemAvailable (≤ 12 GB, threads ≤ cores; `BUILD_MEMORY_GB`/`BUILD_THREADS` override; a 2.3 GB limit
  peaks ~3.2 GB resident). On 2026-09-21 a hardcoded 12 GB limit took R to 7.4 GB and the host to load
  100 — sshd, `/scores`, `/species`, STAC unreachable ~22 min — ended by `pkill -9` of the render, no
  restart. `scripts/srv_render.sh` now carries a **MemAvailable watchdog** (`SRV_MIN_AVAIL_MB`, default
  1200 → kills the render, exit 137); fix the notebook's budget rather than raising the floor.
- **Writes to a v1–v7 `sdm.duckdb` need DuckDB's `icu` extension**: `zone`/`metric`/`model`/`dataset`
  carry `DEFAULT CURRENT_DATE`. A laptop autoloads it; the container has none for uid 1000 →
  `dbExecute(con, "INSTALL icu; LOAD icu;")` on the write connection.
- **The server `.env` has two readers.** docker compose strips quotes; bash (`. ./.env`, `DEPLOY_ACCESS=1`)
  reads an unquoted `v8|v9` as a PIPELINE. Keep `PREVIEW_RESTRICTED_VERSIONS="v7b|v8|v9"` double-quoted.
- **All four repos push over SSH.** `msens`, `workflows` and `api` had `https://` remotes and could
  not push from the server at all.

## Where things live

- **`../msens`** (sibling package): `ingest.R` (cells_from_ranges/raster), `calc.R`
  (species_for_cells), `stac.R`, `atlas.R` (attach_atlas), `viz.R` (cell_tile_url/add_cell_tiles),
  `workflow.R` (targets generator), `validate.R` (pra_score_delta). Edit here → reinstall.
- **`libs/paths.R`** — all paths (`ver`, `sdm_db`, `spp_db`, `s3_atlas`, `cellid_tif`, `dir_big`).
  `~/_big/msens/derived` (laptop) ↔ `/share/data/big` (server).
- **`data/`** — curation CSVs (marine birds, worms supplement), manifests, listings.
- **`../server`** — docker-compose (titiler, titiler-v8, caddy), `titiler/factory.py`; deploy via
  `ssh msens; cd /share/github/MarineSensitivity/server; git pull; docker compose up -d …`.
- **`old/`** — archived v7 notebooks (e.g. `calc_scores_v7.qmd`), kept for reference.

## Gotchas

- `value` is reserved → use `val`. `class`/`order` are reserved in DuckDB → quote (`"class"`).
- `library(msens)` can be **stale** — source `../msens/R/*.R` or reinstall after edits.
- Idempotency: notebooks re-run cleanly (e.g. `merge_taxon` drops derived cols first; ingests skip
  done files). Don't rely on per-ingest table writes that can be silently dropped (the v8 registry
  gap) — consolidate in one target.
- **gm/nc density models are NOT yet ingested** (need density #/km² → suitability [0,100]).
- **Rendering on the server: always `docker exec -u 1000:1000`** — use
  `scripts/srv_render.sh <notebook.qmd>`, never a bare `docker exec rstudio quarto render`.
  `docker exec` runs as **root** (the image's `USER`, which RStudio Server's init needs), so a
  bare render writes root-owned files into `/share`. The uids are *not* misaligned — the
  container's `rstudio` user is already uid 1000/gid 1000, matching host `ubuntu` — you just
  have to ask for it. Use the numeric uid, not `-u rstudio`: docker-compose sets
  `DEFAULT_USER: admin`, so the account *name* changes on the next container recreate while
  the uid does not.

  The damage is silent until git touches it: `git merge` aborts with `unable to unlink …
  Permission denied`, because unlinking needs write permission on the **containing directory**
  and a render's nested `_files/` dirs are root-owned too. A sweep on 2026-08-10 found
  **23,729** root-owned files under `/share/data`, including pipeline inputs like
  `r_cellid.tif` that a non-root render could not have overwritten. If a merge ever aborts
  this way: `sudo chown -R ubuntu:ubuntu` the checkout, `git restore --source=HEAD --staged
  --worktree <path>`, then merge. `git stash push -- <path>` first if the changes matter —
  a partially-applied stash is recoverable from `git stash list`.

  Deploy chunks that `docker exec rstudio Rscript` into `/share` (`release_marine-atlas.qmd`
  repoint/serve steps, `build_v7_cell_model.qmd` register) carry the same latent risk and
  should get `-u 1000:1000` when next touched. `R CMD INSTALL` is the exception — it writes
  the container's own R library and genuinely wants root.
- **The server pushes over SSH, not HTTPS.** `msens`, `workflows` and `api` had
  `https://github.com/…` remotes, so a server-side commit could not be pushed
  (`could not read Username for 'https://github.com'`) and had to be fetched to a laptop over
  ssh and pushed from there. All four repos now use `git@github.com:…`, which the server's
  existing key already authenticates as `bbest`.
- **`mermaid-format: png` is DISABLED — leave it that way.** It is commented out in
  `_quarto.yml`, so Design `{mermaid}` blocks render client-side via mermaid.js and **no
  browser is involved**. PNG bought zoomable lightbox diagrams and cost far more than it was
  worth: it routed every diagram through headless Chrome, which hangs *indefinitely and
  unpredictably*. Diagnosed in the sibling `CalCOFI/workflows` (2026-07-30), where one
  **60 KB** diagram wedged **3h15m** at 0.2% CPU after rendering in ~2 min on the previous
  run, ignored `SIGTERM`, and silently took down two `tar_make()` runs — first misdiagnosed
  as external kills, because the only symptom is a run that stops progressing. This bites
  hardest under `tar_make()`, where a stalled notebook looks like a stalled pipeline. Do not
  re-enable it to get the lightbox back without asking.

  **The sibling `docs` repo still sets `mermaid-format: png`** and its CI installs chromium
  deliberately, so it works there — but it is the same hang risk if run locally.

  `Sys.setenv(QUARTO_CHROMIUM_HEADLESS_MODE = "new")` in `_targets.R` addresses a *different*
  Chrome failure (≥132 dropped legacy `--headless`) and does **not** prevent this hang. It is
  harmless to keep.

  **If you meet a hung render anyway** (a notebook that sets `mermaid-format: png` itself):
  - `pgrep -f "headless=new"` gives Quarto's Chrome, parented to its `deno`. **Check
    parentage before killing** — the user's real Chrome is a separate tree under PID 1.
    Then `kill -9`; SIGTERM is ignored. Quarto exits.
  - R chunks all run *before* the mermaid step, so data outputs survive; only the HTML is
    lost. On the long scoring/merge notebooks, do not assume a hang discarded the work —
    check the outputs before re-running tens of minutes of compute.
  - Every kill leaves a **stale targets lock and an orphaned `rmd.R`**. Clear both
    (`targets::tar_unblock_process()`, `pgrep -f rmd.R`) before re-running.
  - Presence of figures is *not* a reliable "render finished" signal, and absence is *not*
    reliably "graph too large": this hang produced no figures on a tiny graph. Still bound
    what a diagram enumerates rather than letting it walk every dataset/taxon/table — it is
    slow and unreadable even without Chrome in the path.
