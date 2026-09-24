# Fact-check (Opus 5.5) of docs apps/atlas.qmd at ea4f243 + 571dfe6, 2026-09-24

Orchestrator decision: FIX FIRST honoured — docs round 2 dispatched with edits 1–10; merge held until 0.10.59 is live; app copy fixes (pill tooltip "v7 publishes no surface", feedback dialog wording) and paLabel at the tooltip/flower/table sites folded into V4.

claude-opus-5-5[1m]

# Fact-check: docs `apps/atlas.qmd` at ea4f243 + 571dfe6 (read-only, nothing edited)

**Verdict: FIX FIRST.** The Quarto syntax is clean and the chapter will render. But it has two false privacy statements about Feedback, and three parts describe behavior that is not live yet. The live site is 0.10.56: `origin/main` = 4c45643, deployed as `gh-pages` a7b7b7c. 0.10.58 (b2df042) is 15 commits ahead locally and has not been pushed.

## Live status (checked)
- **Preview-route flag is off in the live build.** `VITE_PREVIEW_ATLAS_ROUTE` was set at 20:02Z. The live Pages build started at 19:38Z. The live chunk `assets/VersionPickerModal-BicMjPFl.js` contains `U=w(``)`, which is the flag off.
  - So on the live site a restricted release shows *"The preview host does not serve the Atlas yet; open the Scores/Species apps there instead."*
- `VITE_FEEDBACK_URL` (14:39Z) and `VITE_LOG_URL` (14:46Z) were both set before that build, so Send and the usage beacon are live, as you said.
- **App bundles are not republished.** `v7/app/boot.json` and `taxa.json` are still dated 2026-09-23. Live `taxon/6f.json` shards have 0 inputs with assets for v2, v6, v7 and v7b. For v8 and v9 every input has an asset (111/111 and 153/153).

## False, overstated or not yet true

**1. Feedback privacy — L364–368 and L372–373. False.** Per `scripts/feedback/Code.gs`, a public-release Send always files the public issue once `GITHUB_TOKEN` is set, whatever the email field says. The only test is `restricted`. The email goes into the Sheet's `email` column ("the ONE place it is ever written down"), the team mail, and a copy mailed to the sender.
- "for a **public** release, only when no email was given — also files a public GitHub issue" → "for a public release, also files a public GitHub issue (your email, if given, is never included in it)".
- "No email is stored anywhere beyond the notification it triggers" → "An email, if given, is kept only in the team's Sheet row and mail (plus a copy to you), never in the issue".
- L372: "either route can end up as a public GitHub issue" → "on a public release, both routes end in a public GitHub issue".
- The dialog's own copy ("…and, without your email, as a public GitHub issue") is ambiguous and is where this misreading came from. It is worth an atlas copy fix.

**2. Not live until the next Pages build.**
- **L336–358, "When a service is down":** this is 0.10.58, which is not pushed.
- **L383–384, "which now goes straight to that route":** false on the live 0.10.56 (flag off, above). The fig-alt at L389 is also ahead of the live site.
- **L391, "Known limitations (as of 0.10.58)":** names a version that is not deployed.
- Recommendation: hold the merge until 0.10.59 is live, then relabel the heading "as of 0.10.59". Otherwise, remove those passages or mark them "from 0.10.59".

**3. GeoPackage, L166–168. False, and contradicts the chapter's own L398–402.** "a GeoPackage with no vector layer … is refused by name" is wrong. `geopackage.ts` says that check "currently never actually fires against an uploaded file", so the file falls through to `ST_Read`'s generic `parseFailed`.
- Fix: delete the clause, or say "is refused when `ST_Read` fails to read it (see Known limitations)".
- **L166, "the first `.gpkg` upload in a session asks permission":** not so. `askGeoPackageConsent` calls `window.confirm` on every upload and remembers nothing → "each `.gpkg` upload asks first".
- **"~23 MB":** the prompt itself says "22.4 MB". Match it.

**4. Reproduce in R — L232–233, L305–308, L410–411. Out of date.** msens `origin/main` at 0.44.0 (bd5c824, merged 17:36 today and installed on the server) exports every function the snippets call: `place_decode`, `place_encode`, `cells_in_polygon_grid`, `scores_for_cells(blend, denominator)`, `species_for_cells`, `scores_for_pra`, `species_for_zone`, `sdm_db_con`, `grid_spec_for`. `R/place.R` has been on main since 2026-09-21.
- "An R decoder in `msens` is in preparation" → "`msens::place_decode()` is the R decoder; the shared fixture matches it byte for byte".
- Drop "not yet usable … unreleased branch" and the L410 limitation. At most say "needs msens ≥ 0.44.0 and a local copy of the release's `sdm.duckdb`".

**5. Species struck-through inputs (I checked the numbers against v7's published `tables/*.parquet`).**
- 12,120 input edges, 10,247 with a `model_asset` row, 1,873 without: all correct.
- All 1,873 missing ones are `rng_iucn`, but they are 1,873 of 2,619 `rng_iucn` inputs; 746 do resolve (the walrus's `rng_iucn` has a COG).
- **L396–397:** "only the remaining 1,873 (its `rng_iucn` range models, which carry no raster of their own)" → "1,873 of its 2,619 `rng_iucn` range inputs, which have no model-asset registry row (struck through in the Species Shiny app too)". "Carry no raster of their own" is not supported.
- **L137 caption:** "for most of them" is wrong. All six leatherback inputs have registry rows: am_0.05 25, ch_fws 18297, ch_nmfs 18245, rng_fws 18350, rng_iucn 19859, rng_turtle_swot_dps 37571 → "for all six".
- **L393, "For v7":** too narrow → "For every release before v8 (v2–v7b)".
- **L394–395, "are still being republished":** → "have not yet been republished".
- Separate from the chapter: the struck pill's own tooltip still says "…but v7 publishes no surface for it" (`src/lens/species/data/layerBar.ts:145`). The chapter's explanation is the correct one; the app copy needs fixing.

**6. Program Area results panel, L186–190. Wrong for Program Areas.** `ResultsPanel.svelte` treats a zone place differently from a drawn one:
- Its note reads "*N* cells, *A* km²; published composite *C*." — not the "*N*% … inside the US study area" note.
- Species load from a plain **Load species** button, with no "Loading species needs *N* tiles…" prompt.
- Say that Program Areas get their own coverage note and no tile prompt.

**7. Program Area names.**
- **L149, "shown everywhere as 'Full Name (KEY)'":** overstated. `paLabel` is used in the Places panel, Scores search, Zones table and report. The Scores-lens map tooltip, Flower title and Table header for a clicked Program Area still use the bundle's `name ?? key`, so they show a bare "ALA".
- **L183:** the example "ALA … 28.3 composite" is stale; the row now reads "Aleutian Arc (ALA)".

**8. Analysis cells, L406–407.** "a release publishes … but not the underlying cells themselves" is false: v7 publishes `tables/zone_cell.parquet` (17.8 MB). → "the app bundle the Atlas reads carries each zone's count, area and scores but not its cells".

**9. Minor.**
- **Health section L339–341:** a failed tile re-checks only the tile server; the data origin is checked at boot and on Retry. **L347:** the legend's "tiles unavailable" state appears only when the tile server is down.
- **L376, analytics:** "may also log" → "also logs". GA4 receives interaction events too, not only page views.
- **L171–173, upload naming:** the name property is detected from the file's first feature and used for all features. A merged upload is named after the file.
- **L195–197:** the UI's actual text is "Show analysis cells: not available for Program Areas yet".
- **L57–58, Quick start:** step 2's parenthetical still implies a Program Area gets no results panel.
- **L416, "18 states":** these are 18 numbered shots per viewport, from about 11 states.
- **Other file:** `apps.qmd:24` still says "once its preview route ships".
- **571dfe6 (`libs/versioned.R` L112–115):** the commit message says it drops the Known Limitations cross-reference, but it replaced the wrong line. The comment now reads: "…The preview branch / # The preview host serves the Atlas… / #' `server` main has no Atlas route as of this writing (see apps/atlas.qmd's Known Limitations)…". That is a dangling sentence plus a contradiction. It still parses, so it won't fail a render, but rewrite the comment.

## Phone species framing
L132–133 says only "Selecting a species flies the map to that model's extent". It claims nothing about the phone, fixed or broken, which is what you wanted. Keep it.

## Render check
It will render.
- Every `@sec-`/`@fig-` reference resolves (`@sec-releases` is in `releases.qmd`).
- The only inline `r` is L18, and a chunk comes before it.
- Code fences (6) and `:::` divs are balanced.
- All 14 images exist.

## The stale screenshot (`desktop-places.png`, one image with both problems)
The caption and fig-alt describe what the picture shows accurately: "ALA", "GeoPackage (not yet)", 28.3, and 42.7. But nothing tells the reader the image is out of date. It contradicts the prose around it: the drop-zone text quoted at L161–162 and "Full Name (KEY)" at L149. So it is accurate but not honest.
- Best fix: recapture it from 0.10.59 (the `11-places` state in `eyes-shots.mjs`).
- Or add to the caption: "captured on 0.10.48, before GeoPackage support (0.10.52) and Program Area names (0.10.56)".
- The fig-alt also calls the typed bounding box a "drawn place".

## Ranked edits
1. Feedback privacy wording (item 1).
2. Hold the merge until 0.10.59 is live, or reword the health section, the preview-link line and the heading (item 2).
3. GeoPackage refusal claim (item 3).
4. Reproduce in R and the R decoder (item 4).
5. Species struck-through: "all six", v2–v7b, "have not yet been republished", 1,873 of 2,619 (item 5).
6. Program Area results panel (item 6).
7. Program Area names (item 7).
8. `zone_cell` limitation (item 8).
9. Recapture or caption `desktop-places.png`.
10. The minor items, the `libs/versioned.R` comment and `apps.qmd:24`.

Files:
- `/Users/bbest/Github/MarineSensitivity/docs/.claude/worktrees/atlas-r2/apps/atlas.qmd`
- `/Users/bbest/Github/MarineSensitivity/docs/.claude/worktrees/atlas-r2/libs/versioned.R`
- `/Users/bbest/Github/MarineSensitivity/docs/.claude/worktrees/atlas-r2/apps.qmd`
- `/Users/bbest/Github/MarineSensitivity/atlas/scripts/feedback/Code.gs`
- `/Users/bbest/Github/MarineSensitivity/atlas/src/lib/geo/upload/parsers/geopackage.ts`
- `/Users/bbest/Github/MarineSensitivity/atlas/src/places/ResultsPanel.svelte`
- `/Users/bbest/Github/MarineSensitivity/atlas/src/lens/scores/FlowerPanel.svelte`
- `/Users/bbest/Github/MarineSensitivity/atlas/src/lib/report/provenance.ts`
