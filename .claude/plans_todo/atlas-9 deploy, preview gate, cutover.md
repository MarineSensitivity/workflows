# atlas-9 · Two hosts, one build: GitHub Pages for public releases, the Cloudflare-Access host for restricted ones; then the cutover

**Phase:** A9 of `2026-09-20 atlas app plan.md` (the preview-host half can start as soon as atlas-0 has
a build; the cutover half waits for atlas-8's signed `docs/parity.html`). **Model:** Sonnet for the CI,
Caddy, compose and notebook chunks; Opus reviews the gate logic and the redirect matrix.
**Touches:** `../server/{docker-compose.yml, caddy/preview_routes.caddy, caddy/Caddyfile,
caddy/test/run.sh, cloudflare/access.sh}`, `../msens/R/ver_token.R` (`product_urls`), here
`release_marine-atlas.qmd` + `CLAUDE.md`, `../docs`, `../MarineSensitivity.github.io`, `../apps/README.md`.
**Rules that apply verbatim:** every deploy step is a chunk in `release_marine-atlas.qmd` behind an env
flag; no ad-hoc `ssh`; server renders via `scripts/srv_render.sh`; `DEPLOY_CADDY` validates before it
restarts; reproducibility beats uptime.

## The gate, restated for a static app (master plan D6, decided 2026-09-20: this gate now, data protection later)
- **Public**: `https://marinesensitivity.org/atlas/` (GitHub Pages of `MarineSensitivity/atlas`, branch
  `gh-pages`; the org site's CNAME puts project pages under the apex). It renders `access: public`
  releases only. `?ver=v9` there draws nothing from v9: it opens "Version v9 is under review" with a
  link to the preview URL carrying the same query and hash.
- **Preview**: `https://preview.marinesensitivity.org/{ver}/atlas/` — the **same commit** of the same
  build, served as static files by Caddy behind Cloudflare Access (one reviewer policy per version,
  scoped by **path**, which is why the version is the path here and why the build is base-relative and
  router-free) and `jwtauth` at the origin. Caddy also answers `/{ver}/atlas/session.json`
  (`{"preview":true,"ver":"v9","user":"<email from the verified JWT>"}`, `Cache-Control: no-store`); that
  same-origin file existing is the only thing that puts the app in preview mode, and the version it
  names is the only one that page renders. Switching release = navigating to the other version's path =
  that version's own policy. This is today's semantics ("the session renders the version its page was
  served for") without a Shiny process to hold it.
- **What this does not do**, stated so nobody assumes otherwise: the Parquet, COGs and manifests of v8
  and v9 are on a public bucket and always have been (`query.html` reads v8 anonymously). The gate
  controls who is *shown* a pre-release as a product, with the agency's name on it, and who can produce a
  titled report from it. Reports and screens from a restricted release carry a PREVIEW watermark
  (atlas-7). Making the data itself private is a hosting change (`dataBase(ver)` → a same-origin path
  behind Access + a non-public prefix + titiler reading with credentials); the app is built so that is
  configuration, not code. Ben has committed to it as a follow-up: see "Later" below.

## Later (committed by Ben 2026-09-20, not part of this build): protect restricted data from direct reading
Two ways, both configuration for the app because `dataBase(ver)` prefers `session.data`:
- **Unguessable prefix.** `release_marine-atlas.qmd` publishes a restricted release under
  `marine-atlas/{ver}-{secret}/` (secret in the server `.env`), `versions.json` carries no href for it, and
  only the Access-gated `session.json` returns `"data": ".../{ver}-{secret}"`. Anonymous LIST is already
  denied, so the prefix cannot be discovered; titiler and PMTiles keep working unchanged; promotion copies
  it to `{ver}/`. A reviewer could still pass an address along.
- **Private data.** Restricted objects leave the public bucket; Caddy serves `/{ver}/data/*` from
  `/share/data/big/{ver}` (already synced there) behind Access, and a gated tile route fronts titiler for
  restricted COGs. Real confidentiality; costs bandwidth on msens1 and a promotion step.
Either way STAC entries and docs for a restricted release must stop exposing raw hrefs. Pick when the
first provider or agency requirement makes it necessary; the 2026-07-15 access-control plan is the
sibling question for dataset-level (viz-only) terms. Until then `session.json` simply omits `data`.

## Deliverables
1. **Pages**: CI publishes `dist/` to `gh-pages` (atlas-0). `<meta name="ms-app-sha">` in both entries.
2. **`atlas-preview` sidecar** in `docker-compose.yml`, a copy of `docs-preview`: polls the atlas repo's
   `gh-pages` into `/share/atlas_preview` every 5 min as uid 1000; tolerates a missing branch.
3. **Caddy** (`preview_routes.caddy`, before the catch-all `handle`):
   ```
   @vatlas_noslash path_regexp van ^/(v[0-9]+[a-z]?)/atlas$
   redir @vatlas_noslash {path}/?{query} 308
   @vatlas path_regexp vatlas ^/(v[0-9]+[a-z]?)/atlas(/.*)$
   handle @vatlas {
     @session path_regexp ^/v[0-9]+[a-z]?/atlas/session\.json$
     handle @session {
       header Cache-Control "no-store"
       header Content-Type "application/json"
       respond `{"preview":true,"ver":"{re.vatlas.1}","user":"{http.auth.user.id}"}` 200
     }
     rewrite * {re.vatlas.2}
     root * /share/atlas_preview
     header /index.html Cache-Control "no-cache"
     header /report.html Cache-Control "no-cache"
     file_server
   }
   ```
   (the agent adapts the syntax to the Caddy version in `server/caddy/Dockerfile` and proves it with
   `caddy validate`). `server/caddy/test/run.sh` gains: no token → 401/302; service token entitled to
   v9 → 200 and `session.json` says `v9` + the user; the same token on `/v8/atlas/` → refused;
   `/v9/atlas/../` traversal → refused; `/v9/atlas` → 308 with the query intact.
4. **Cloudflare Access**: confirm the per-version applications cover `/{ver}/atlas/*` (they are scoped
   to the version path; if they enumerate products instead, add `atlas` in `access.sh`, run
   `DEPLOY_ACCESS=1`, paste any new AUD into `.env`, then `DEPLOY_CADDY=1`).
5. **`release_marine-atlas.qmd`**: `DEPLOY_ATLAS=1` (granular: make the sidecar pull now and assert the
   served `ms-app-sha` equals the Pages one) and `CHECK_PREVIEW=1` extended with the atlas proofs:
   public host never renders a restricted release **and requests no object under it**; preview host is
   closed without a token and open with one; origin-direct is 401; `session.json` is `no-store`.
6. **`msens::product_urls()`** gains `atlas` (public `…/atlas/?ver={ver}`, restricted
   `…/{ver}/atlas/`), tests, `NEWS.md`, version bump; docs and homepage links move in the same change.
   Publish before deploying any reader (the `versions.json` ordering lesson applies to URLs too).
7. **Cutover, in three reversible steps, each its own commit and flag run:**
   - **B1 beside** — homepage + docs link "Atlas (beta)"; a dismissible banner in both Shiny apps.
   - **B2 default** (needs the signed parity page) — 301s, query always carried:
     `app…/{v}/scores/` → `…/atlas/?ver={v}`; `app…/{v}/species/?Q` → `…/atlas/?ver={v}&lens=species&Q`
     (the app reads legacy `mdl_key` / `mdl_seq` itself); preview: `/{v}/scores/` → `/{v}/atlas/`,
     `/{v}/species/?Q` → `/{v}/atlas/?lens=species&Q`; the older retired paths keep resolving in ≤ 2
     hops. Shiny apps move to `/share/shiny_apps_retired/` (not deleted). `PREVIEW_RESTRICTED_VERSIONS`
     keeps 302-ing public `/v8|v9/` paths to the preview host.
   - **B3 retire** (after 60 quiet days) — remove the two shiny-server blocks and the API's `/report` and
     `/species.csv` (the rest of the API stays); archive `api/report*.qmd`; update `CLAUDE.md`'s "ONE
     app" section, `DEPLOY_APPS` notes, and the memory index.
8. **Rollback card** in `atlas/README.md`: revert `gh-pages`; revert the Caddy commit + `DEPLOY_CADDY=1`;
   move the apps back from `shiny_apps_retired/`.

## Gates
- Same `ms-app-sha` on both hosts within 10 min of a push; `scripts/smoke_release.mjs` green on both.
- The redirect matrix: every URL shape published in the BOEM final report and the docs
  (`/mapsp/?mdl_seq=…`, `/species/?ver=v7&mdl_key=…`, `/v7/scores/`, …) lands on the right taxon or view
  in ≤ 2 hops; a test that strips `{query}` from one rule must fail.
- titiler answers CORS for both origins (`marinesensitivity.org`, `preview.marinesensitivity.org`) on
  tiles and `/cog/point`.
- On the preview host an expired session shows the re-sign-in banner, and Sign out purges OPFS.
- The public release picker links restricted rows to the preview host, never to `?ver=` on Pages.

## Review checklist (Opus)
- Preview mode is reachable only through a 200 on same-origin `session.json`; grep the app for any
  other path into it.
- No deploy step exists outside a flagged notebook chunk; `caddy validate` precedes every restart.
- B2 changes no URL the final report printed without a working redirect.

## Progress log (orchestrator)
- 2026-09-21 · **Part a started (Sonnet)** on branch `atlas-preview` in an ISOLATED worktree of the server repo
  (`atlas/.claude/worktrees/preview/server`, cut from `origin/main` `5ae8869`; the shared `../server` checkout
  is never touched). Scope now: Deliverables 2 (sidecar), 3 (Caddy routes in their own snippet + tests) and the
  read-only half of 4 (is Access scoped by path?). **What this laptop can and cannot gate:** homebrew caddy
  2.11.2 has no `jwtauth` plugin, there is no Docker daemon and no Go, so the ROUTING half is proven locally
  (`caddy/test/atlas_routes_local.sh` + four seeded faults) and the AUTH half (`caddy/test/run.sh`: 401 without
  a token, identity in `session.json`) stays UNRUN until `DEPLOY_CADDY=1` runs it on the server, which is the
  release session's or Ben's to do, in a turn of its own. Deferred to keep branches apart: Deliverable 5
  (`release_marine-atlas.qmd` chunks; the atlas-1 notebook agent is active in that worktree) and Deliverable 6
  (`msens::product_urls()`; waits for `atlas-contract` to merge into msens main). Part b waits for atlas-8's
  signed `docs/parity.html`.
- 2026-09-21 · **Part a built: `atlas-preview` @ `e6fdef5`** (11 files, unpushed, isolated worktree; the shared
  `../server` checkout verified untouched). `atlas-preview` sidecar (copy of `docs-preview`, `gh-pages` →
  `/share/atlas_preview`, uid 1000); routes in their own snippet `caddy/atlas_preview_routes.caddy`, imported
  before the catch-all; `caddy/test/atlas_routes_local.sh` (11 assertions + `caddy adapt`/`validate`);
  `caddy/test/run.sh` atlas assertions (UNRUN: needs the server's image). Two real bugs the local test caught
  before commit: Caddy's automatic directive sort ran `rewrite` before the nested `session.json` handler, so
  the FIXTURE `session.json` file was served (fixed with an explicit `route {}`); `header /index.html` never
  matches a request for `/` (fixed with a `path / /index.html` matcher). **Access finding (read-only):**
  applications are scoped by hostname + PATH PREFIX `/{ver}`, so `/{ver}/atlas/*` is already covered;
  `access.sh` unchanged and not run.
  Orchestrator re-run: local test exit 0 (`ATLAS_ROUTES_LOCAL_OK`), `run.sh` `bash -n` clean, compose parses
  with the sidecar and both caddy mounts. My faults, diff confirmed, all red: version label loosened to accept
  `v9x9`; an SPA `try_files` fallback; `index.html` without `no-cache`; `ver` taken from a header. The agent's
  four (query dropped from the 308; `ver` from `?ver=`; no `no-store`; fixture file served) were red too.
  **Opus review of the gate logic launched** (is every path behind jwtauth; is a `/v8` token refused for
  `/v9/atlas/` at the origin; raw `{http.auth.user.id}` in JSON; traversal variants; Cloudflare caching; torn
  reads during a pull; vacuous assertions in `run.sh`). Hand-off to the release session follows the review.
- 2026-09-21 · **Opus gate review of `e6fdef5`: "not as is"; fix round 1 of 2 dispatched.** I reproduced the two
  findings that matter before acting. (1) **The 308 answers before authentication:** `caddy adapt` with an auth
  directive in jwtauth's sort slot gives `0 static_response (redir)`, `1 authentication`, `2 subroute`; the
  same is true of the pre-existing redirects in `app_version_routes.caddy` and `preview_routes.caddy`
  (another session's files: reported, not edited). Content routes (`session.json`, `file_server`) ARE behind
  auth. Fix: `handle @vatlas_noslash { redir … }`, proven locally from the adapted JSON. (2) **Case variants:**
  on this case-insensitive filesystem `/v9/atlas/SESSION.JSON` served the on-disk fixture file; unreachable on
  Linux and the atlas `dist/` never holds a `session.json`, but it is the fault class the block exists to
  prevent: every spelling but exact lowercase is now a 404. (3) **RULED DEVIATION from the contract above:
  `session.json` no longer carries `user`.** `{http.auth.user.id}` was interpolated raw; a claim such as
  `x","data":"https://evil.example/` yields valid JSON with a `data` key, and the app honours `session.data` as
  its data origin on a preview session. The app never reads `user`; if identity is ever needed, Cloudflare's
  same-origin `/cdn-cgi/access/get-identity` is the source. Body is exactly `{"preview":true,"ver":"<path>"}`.
  (4) **Per-version entitlement is enforced at the Cloudflare edge, not the origin:** `audience_whitelist` is
  one flat list of every application's AUD, exactly as for the Shiny routes, so Deliverable 3's "same token on
  `/v8/atlas/` → refused" cannot be an origin assertion; it moves to `CHECK_PREVIEW` (Deliverable 5) with real
  service tokens, and per-version AUDs are offered to the server's owner as hardening. (5) `run.sh` traversal
  checks could pass on `CODE=000` and never read the body: tightened. OK as built: caching (zone rule
  `cache:false` for the preview host, if `CF_ZONE_ID` is set), the sidecar (same as `docs-preview`; peaceiris
  keeps `gh-pages` linear so `--ff-only` holds), the single door into preview mode. The hand-off must say this
  is `docker compose up -d caddy atlas-preview`, not a reload, with `/share/atlas_preview` chowned to uid 1000
  FIRST.
- 2026-09-21 · **Fix round 1 (`659e0a3`) verified; round 2 of 2 dispatched for one gate hole.** Local test exit 0
  (24 assertions); `run.sh` `bash -n` clean; body is exactly `{"preview":true,"ver":…}`; the redirect sits in a
  `handle`; any-case `session.json` → 404. My faults: a `data` key in the body → red; the any-case block
  removed → red (`fixture-leaked=1`); **a NEW top-level `redir /v9/atlas/old …` outside any `handle` → GREEN**:
  the sort-order proof only inspected routes with the known matchers. Round 2: assert that NOTHING in the
  snippet can sort ahead of `authentication` (first route of the adapted stand-in config), and recommend the
  same check on the server against the real Caddyfile.
- 2026-09-21 · **Part a, laptop-provable half DONE: `atlas-preview` @ `8461a24`** (three commits on `origin/main`
  `5ae8869`, unpushed, isolated worktree; shared `../server` checkout verified untouched). Round 2 replaced the
  matcher-name check with a pre-order walk of the adapted route tree: the FIRST handler reached must be
  `authentication`, whatever the matcher; it also established that a bare `header` sorts ahead of auth, like
  `redir`. Orchestrator re-run: local test exit 0; my previously-GREEN fault (a new top-level
  `redir /v9/atlas/old …`) is now red, and so is a top-level `rewrite`. Two fix rounds of two used.
  **Handed to the release session (workflows-75) as information, no deploy requested:** what the branch adds,
  what is UNRUN and theirs (`caddy/test/run.sh` atlas assertions; `docker compose up -d caddy atlas-preview`
  under `DEPLOY_CADDY=1` after chowning `/share/atlas_preview`), and two findings about its own files (bare
  redirects answer before jwtauth; one flat AUD list means per-version entitlement is edge-only). No hurry: the
  published app is still a shell. **Left in part a:** Deliverable 5 (`release_marine-atlas.qmd` `DEPLOY_ATLAS` /
  `CHECK_PREVIEW` chunks, after the atlas-1 notebook agent leaves that worktree) and Deliverable 6
  (`msens::product_urls()`, after `atlas-contract` merges). Part b waits for atlas-8's signed parity page.
- 2026-09-21 · **Release session (workflows-75) acknowledged the hand-off**: it sees `atlas-preview`, has not
  touched it, confirmed both findings against the files (bare `redir`s at `app_version_routes.caddy:21,40,45`
  and `preview_routes.caddy:49` sort ahead of `jwtauth`; `audience_whitelist` at `Caddyfile:293` is one flat
  list) and passed them to Ben as his call. Its context on the second: v7b and v8 reviewer lists are identical
  today (Ben + Tim), so nothing differs in practice until two versions have different reviewers. It has
  nothing in flight on Caddy, Access or `.env`, will not run `DEPLOY_CADDY` or touch the `server` repo without
  telling this session first. **When to deploy the preview host is Ben's decision.**
- 2026-09-21 · **D15 (master plan, clarified by Ben): the atlas uses the SAME per-version paths as the apps it
  replaces, so the existing Cloudflare Access applications cover it.** Inspected: "preview {ver} apps" is
  scoped to `preview.…/{ver}` (prefix), the Shiny apps sit at `/{ver}/scores/` and `/{ver}/species/`, and
  the atlas sits beside them at `/{ver}/atlas/` (already so on `atlas-preview`). No new Access application,
  no `access.sh` change, no origin AUD change. The draft "Deliverable 9" is WITHDRAWN. **Two additions to
  Deliverables 3 and 5 from the inspection:** (a) `app_version_routes.caddy` / the public app host: extend
  the `PREVIEW_RESTRICTED_VERSIONS` 302 (`Caddyfile:230`, `(?:scores|species)`) to `atlas`, so
  `app.marinesensitivity.org/v9/atlas/` sends a reviewer to `preview.…/v9/atlas/` the way `/v9/scores/` does
  today; and the Pages host's "Version v9 is under review" card links that same preview URL with the query
  and hash carried (atlas-4's version picker, never `?ver=` on Pages). (b) `CHECK_PREVIEW`: the per-version
  probe token for `/{ver}/atlas/` and `/{ver}/atlas/session.json` (own version 200 + `ver`; other version
  refused; no token 401/302), beside the existing `/{ver}/scores/` probe. Both are patches to the release
  session's files (`Caddyfile`, `release_marine-atlas.qmd`), delivered as patches and deployed by it or Ben
  under `DEPLOY_CADDY=1` / `CHECK_PREVIEW=1`.
