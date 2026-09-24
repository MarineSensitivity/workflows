# MarineSensitivity place report — full pipeline specification

> **Correction (orchestrator, 2026-09-20, verified against the live bucket).** This survey says
> `cell_model` is "deliberately NOT on S3" (§6.1 row 5, fact 2 of the closing list), following the comment
> at `release_marine-atlas.qmd:303-315`. That comment describes the *serving view*, which points at a
> local path. The tiles themselves **are published and anonymously readable for v8 and v9**
> (`{ver}/serve/cell_model/tile={t}/data_0.parquet`, 422 objects, 1.30 GB and 1.48 GB; anonymous GET of
> `tile=1012` returns 200). They are **absent for v7** (757 MB / 428 tiles exist only under
> `~/_big/msens/derived/v7/cell_model`). So the unlock for drawn-polygon species lists is publishing
> v7's tiles, not inventing the artifact. Likewise a slim `cell` table is superseded by the wide
> `app/cell/tile=*` tiles of `atlas-1`. Everything else below stands as surveyed.

Investigated under `/Users/bbest/Github/MarineSensitivity/`: `api/`, `apps/scores/`, `msens/R/`, `workflows/`, `server/`, `docs/`, `MarineSensitivity.github.io/`, `analytics/`.

Formulas marked **CONFIRMED** were verified empirically against the staged v9 Parquet (`~/_big/msens/derived/v9/marine-atlas/tables/`) and the v9 `sdm.duckdb` (29 GB) — not just read off the code.

Context: `latest.txt` = **v7** (promoted/public); **v8** and **v9** are `prerelease`/`restricted`.

---

## 0. TL;DR

| | |
|---|---|
| Entry point | `POST https://api.marinesensitivity.org/report` (JSON body) |
| Body | `{title, ver, format, areas:[{label, kind:"pra"\|"wkt", value}]}` |
| Response | `{"url": "https://file.marinesensitivity.org/reports/MarineSensitivity.org_<8hex>.<ext>"}` |
| Formats | `html` (default) · `pdf` (LuaLaTeX/TinyTeX) · `docx` |
| Renderer | `quarto::quarto_render()` on `api/report.qmd` + `api/report_area_child.qmd` |
| Latency | "usually a couple of minutes"; client timeout 600 s; observed 32 s success / 90 s failures |
| Cache | 8-hex digest of `(title, areas_json, ver, format)`; LRU-pruned to 500 MiB |
| Sections | Intro · Parameters · Map · Plot of Scores · Table of Scores · Summary of Species · Software |
| Tables read | `zone, zone_metric, metric, zone_cell, cell, cell_metric, taxon, model, cell_model` — **all but `cell_model` published as Parquet on S3** |
| Hard blockers for a browser | PDF/DOCX binaries · static ggplot map · `session_info()` · `cell_model` is **server-local only** · `cell.parquet` is 392 MB · the Program-Area GeoPackage is server-only |

---

## 1. End-to-end sequence today

### 1.1 Click path

```
User (Scores app "Report" tab)
 │  rx$rpt_areas = [{label, kind, value}]
 │    drawn  → kind="wkt", value = sf::st_as_text(geometry)   (WKT, EPSG:4326)
 │    PRA    → kind="pra", value = programarea_key            (e.g. "GAA")
 ▼ click #btn_rpt_submit
[JS] window.open('', '_blank')                 ← placeholder tab, SYNCHRONOUS (popup blocker)
 ▼
[Shiny] promises::future_promise({ httr2 POST })   ← background R worker, keeps session live
 ▼ POST api.marinesensitivity.org/report  (Caddy → plumber:8888)
[plumber] gate version → cache key → quarto_render(report.qmd) → /share/public/reports/<f>
 ▼ {"url": …}
[Shiny] sendCustomMessage("openUrl", {url, reqId})
 ▼ [JS] stashed window.location.href = url; close after 2 s
Caddy `/reports/*` sets `Content-Disposition: attachment` → download
```

### 1.2 App side (file:line)

- Report tab UI `apps/scores/app.R:1546-1607`: `rpt_title` (default `"BOEM Marine Sensitivity Report"`, `:1557`), `rpt_ver` (choices from `msens::atlas_versions()`, built `:950-956`; label `ver` or `"ver (status)"`), `rpt_format` radio html/docx/pdf default html (`:1571-1578`), `rpt_area_label`, `btn_add_drawn`/`btn_add_pra`/deletes/`btn_rpt_submit`, embedded map `map_rpt` with `add_msens_draw_control()` (`:2901-2904`).
- Drawn capture `:2914-2918` → `msens::drawn_features_sf()`; **last feature only**.
- Add drawn `:3062-3085`: `wkt <- sf::st_as_text(...)`; **if `nchar(wkt) > 8000`** → `st_simplify(dTolerance = 0.01, preserveTopology = TRUE)` + notify "Polygon simplified to fit request." (`:3071-3077`). Only geometry limit anywhere.
- Add PRA `:3088-3105`: key = `props$programarea_key %||% props$planarea_key`.
- Submit `:3149-3269`: body `:3163-3167`; endpoint `Sys.getenv("MSENS_REPORT_URL", "https://api.marinesensitivity.org/report")` `:3168-3170`; preview token `:3182, :3226-3227`; `req_timeout(600)` `:3225`; analytics `:3192-3199, :3239-3261`; spinner notification `:3203-3214`.
- Popup/download JS `:1311-1367` (pre-opened tab keyed by `reqId`, hidden-anchor fallback).

### 1.3 API side — `api/plumber.R`

**`POST /report` — `:326-444`**

| Field | Type | Default | Example |
|---|---|---|---|
| `title` | string | `"BOEM Marine Sensitivity Report"` | `"Gulf of America draft"` |
| `ver` | string | `"v6"` | `"v8"`, `"latest"` |
| `format` | `html\|pdf\|docx` | `"html"` | `"pdf"` |
| `areas` | array `{label,kind,value}` | required, non-empty | `[{"label":"GAA","kind":"pra","value":"GAA"}]` |

Optional header `X-MS-Preview-Token` — shared secret proving the caller is the preview Shiny instance (`:54-58`).

1. Parse body → `400 {"error":"invalid JSON body"}` (`:341-347`); `400` bad format (`:354-357`); `400` empty areas (`:358-361`).
2. **Version gate** `ms_gate_ver()` `:90-106` — `""`/`"latest"` → `latest.txt`; `400` unknown; `403` if `versions.json` says `restricted` and no token. Registry read **directly over HTTPS** with a 300 s memo (`ms_versions()` `:66-82`), deliberately *not* via `msens::atlas_*()` because this image pins an old msens (`:60-65`). **Fails open** on registry fetch error.
3. Cache key `substr(digest::digest(list(title, areas_json, ver, format)), 1, 8)` → `MarineSensitivity.org_<key>.<ext>` (`:371-374`). Hit → `Sys.setFileTime()` LRU touch, no render (`:382-385`).
4. Miss → copy both qmds to a tempdir (`:386-397`), `quarto::quarto_render(input, output_format=format, execute_params=list(title, areas_json, ver, format, api_base, mapsp_base, access))` (`:408-427`), copy out (`:429-431`).
5. LRU prune `prune_cache_dir(out_dir, MSENS_REPORTS_MAX_MB×1MiB, keep=out_file)` — default **500 MiB**, oldest `mtime` first (`:301-324, :434-441`).
6. Return `{url}`. Output dir `/share/public/reports` (`:376`). CORS `*` (`:108-112`).

**`GET /species.csv` — `:446-516`** — params `ver, kind(pra|wkt), value, label`. `kind=pra` reads `<data>/derived/<ver>/ply_programareas_2026_<ver>.gpkg` (`:470-481`); `kind=wkt` → `st_as_sfc(value, crs=4326)` (`:482-484`). `400` missing/bad kind, `404` area not found. `cells_in_polygon()` + `species_for_cells()` (`:501-502`). `Content-Disposition: attachment; filename="species_<slug>_<ver>.csv"`.

Other endpoints (non-report): `/species_by_feature` (PostGIS, retired/disabled), `/tilejson`, `/echo`, `/plot`, `/sum`, `/stats.json` (`:518-579`), `/h3`, `/` (303 → swagger).

### 1.4 Infrastructure

- `server/docker-compose.yml:169-183` — `plumber` service, `build ./plumber`, `8888:8888`, mounts `/share`, `MSENS_PREVIEW_TOKEN`, `restart: unless-stopped`. **No CPU/memory/concurrency limits.**
- `server/caddy/Caddyfile:17-19` — `api.marinesensitivity.org { reverse_proxy plumber:8888 }` (no timeout/body-size directives → Caddy defaults).
- `server/caddy/Caddyfile:62-67` —
  ```
  handle_path /reports/* { root * /share/public/reports
                           header Content-Disposition "attachment"
                           file_server }
  ```
  Forces download even for HTML. Host also imports the `cors` snippet (`:22`).
- `server/plumber/Dockerfile` — `rocker/geospatial:4.4.1`; Quarto CLI (`:13-19`); CRAN set (`:24-29`); duckdb ≥1.5 (`:33-35`); **TinyTeX for PDF** (`:46-47`); msens **pinned by commit** `ARG MSENS_REF=742eb68…` / `ARG MSENS_MIN=0.13.1` (`:61-68`). That ref is msens **0.13.1** (2026-07-29); msens `main` is **0.41.0** — the deployed report runs older msens. *Verified*: pinned `.species_sql()` is identical to current except a v1/v2 typed-NULL ER guard, so v3+ behaviour matches.
- `DEPLOY_API=1` (in `workflows/release_marine-atlas.qmd`) pulls the api repo and **rebuilds** the plumber image.

### 1.5 Failure modes

| Mode | Where | Symptom |
|---|---|---|
| Invalid JSON | `plumber.R:344-347` | `400 "invalid JSON body"` |
| Bad format / no areas | `:354-361` | `400` |
| Unknown version | `:100-101` | `400 "unknown data version: {ver}"` |
| Restricted + public caller | `:102-104, :364-368` | `403` + preview-host explanation |
| api repo not pulled on server | `:390-395` | `500 "missing report source: …"` |
| Quarto render error | `:425-427` | `500 "quarto_render failed: …"` |
| `app_link` not vectorised | hist., `report.qmd:69` | every render failed |
| `mdl_seq` vs `mdl_key` drift | hist., `report_area_child.qmd:36-40` | every report 500'd |
| Wrong grid for drawn polygon on v8 | hist., `calc.R:1-13` | **silent** empty species list |
| Stale pinned msens | `Dockerfile:50-60` | `/report?ver=v8` died on missing `sdm.duckdb` |
| Popup blocked | mitigated `app.R:1311-1342` | first click opened nothing |
| Client timeout | `app.R:3225` | 600 s → `report_result status=error` |

Instrumentation: `report_submit`/`report_result` with `rpt_ver, format, n_areas, ms, status, error, report_url` → `analytics/scripts/sheet.py:255-263` → `log/reports.csv` (`event, rpt_ver, format, status, n, p50_ms, p95_ms`), rendered `analytics/layouts/products/single.html:184-208`. The report file is served from a JS-free host, so **the app is the only place a report can be counted** (`apps/analytics/README.md:98-104`). Documented examples: two 90 s HTTP 500s (2026-07-28), a 32 s success after the `mdl_key` fix (`sheet.py:25-28`). **No real volume data in the checkouts** — `analytics/static/data/` is empty locally.

---

## 2. Report content, section by section

Template: `api/report.qmd` (395 lines) + `api/report_area_child.qmd` (145 lines).

### 2.0 Front matter — `report.qmd:1-35`
- Title = `` `r params$title` ``. Date = `now`, `"YYYY-MM-DD HH:mm"` — **render** time, not a data date.
- html: `toc, smooth-scroll, embed-resources: true, format-links: false`
- pdf: `toc, number-sections, colorlinks, lof, lot`
- docx: `toc, toc-depth: 2, lof, lot`. A `reference-doc` pointing at `"../workflows/libs/ESP Report Template 2025_1 tech.docx"` is **commented out** (`:26`) → DOCX uses Quarto defaults.
- **No logo, no branding, no cover page, no disclaimer, no bibliography, no footnotes.**
- `execute: echo:false, warning:false, message:false`

### 2.1 Setup (invisible) — `report.qmd:37-185`
- `is_html <- params$format == "html"` — the single branch for interactive vs static.
- `app_link(app, query)` `:70-75` → `https://{preview|app}.marinesensitivity.org/{ver}/{app}/?{q}`; **preview host when `params$access == "restricted"`**. Must be vectorised (`ifelse`).
- `areas <- fromJSON(params$areas_json)`; `stop()` if empty.
- `pra_gpkg <- <data>/derived/<ver>/ply_programareas_2026_<ver>.gpkg`, read only if any `kind=="pra"` (`:89-98`).
- `area_to_sf()` `:100-110`.
- `con <- msens::sdm_db_con(version = params$ver, read_only = TRUE)` `:112`.
- **`areas_data`** `:125-142` — the whole computation:
  ```r
  if (kind=="pra") { cells <- msens::cells_in_pra(con, value);   scores <- msens::scores_for_pra(con, value) }
  else             { cells <- msens::cells_in_polygon(ply, con); scores <- msens::scores_for_cells(con, cells) }
  spp <- msens::species_for_cells(con, cells)
  ```
- `all_areas` `:144-153` — sf of `label`, `score = round(msens::mean_score(scores), 1)`, geometry.
- `score_cols <- rev(RColorBrewer::brewer.pal(11,"Spectral"))` → **red = high** (`:158`). `score_rng <- range(all_areas$score)`, widened ±0.5 if all equal (`:159-160`). **The ramp is relative to the areas in *this* report, not a fixed 0–100.**
- `emit_table()` `:163-184` — HTML: `kable(format="html", escape=FALSE, table.attr='class="table table-sm"')` inside `<div style="overflow-x:auto; margin-bottom:1em;">`; PDF/DOCX: `kable(booktabs=TRUE, longtable=TRUE)`.

### 2.2 Intro (static boilerplate + 1 computed link) — `report.qmd:187`
> This report was generated via input parameters submitted by the [Scores app](…). For more information about this project please visit [MarineSensitivity.org](https://MarineSensitivity.org), where you can find the latest documentation, apps and log of changes.

### 2.3 Callout "Parameters" (collapsed) — `:189-210`
`callout-note collapse="true"` with `yaml::as.yaml()` of `title, ver, format`, and per area `{label, kind, wkt|programarea_key}`. Computed echo of input.

### 2.4 `## Map`
**Narrative (static)** `:214`:
> Areas of interest colored by mean sensitivity score using a Spectral color ramp (red = high, blue = low). Scores are ecoregionally rescaled to a 0–100 range within each BOEM Ecoregion, so they reflect relative sensitivity within a region rather than absolute values across regions.

**HTML** `:216-251` (`eval: is_html`) — `mapgl::maplibre(bounds = st_bbox(all_areas))` +
`add_fill_layer("areas", fill_color = interpolate(column="score", values=seq(rng, length.out=11), stops=score_cols), fill_opacity = 0.6, tooltip = "<label> — mean score: <score>")` +
`add_symbol_layer("area_labels", source = st_point_on_surface(all_areas), text_size 14, color #000000, halo #ffffff width 2)` +
`add_legend("Mean score", values = round(score_rng,1), colors = score_cols, position = "bottom-right")` +
`add_fullscreen_control("top-left")`. `mapboxgl()` does **not** render inside Quarto (walkerke/mapgl#3) — hence maplibre (`:218-219`). **No basemap tiles configured.**

**PDF/DOCX** `:253-300` (`eval: !is_html`), fig-cap *"Areas of interest, colored by mean sensitivity score."* — ggplot2: `ne_countries(scale="medium")` fill `#e8e4dc`/colour `#b9b3a6`/lw 0.2; `ne_states("united states of america")` fill NA/`#9e9689`/0.15; areas `aes(fill=score)` alpha 0.75 colour `#333333` lw 0.4; `geom_sf_text(point_on_surface, size=3, fontface="bold")`; `scale_fill_gradientn(colors=score_cols, limits=score_rng, name="Mean score")`; `coord_sf(bbox ±10%, expand=FALSE)`; `theme_minimal()`, no grid, legend bottom. (Packaged equivalent `msens::ggmap_areas()` at `msens/R/viz.R:933-972`, unused here.)

### 2.5 `## Plot of Scores`
**Narrative (static — and STALE)** `:304`:
> Flower plots where each petal represents a species category (bird, coral, fish, invertebrate, mammal, reptile, other, primary productivity). Petal length reflects the component sensitivity score (0–100) and petal width reflects the component weight. The center value is the weighted mean across all categories.

⚠ On v8/v9 the actual petals are **bird, coral, fish, invertebrate, mammal, primary producer, turtle, primprod** (8). `reptile`/`other` no longer exist; `turtle` is missing from the sentence. All weights are 1 (`even=1`), so "weighted mean" = plain mean.

HTML wraps children in `::: {.panel-tabset}` (`:310-319`); PDF/DOCX sequential.

Per area — `report_area_child.qmd:1-21`: heading `### <label>`; HTML `msens::plot_flower(scores, component, score, even, interactive=TRUE, title=label)` → `ggiraph::girafe` widget (`msens/R/viz.R:809-829`, `opts_sizing(rescale=TRUE, width=1)`, white tooltip); PDF/DOCX `msens::ggplot_flower(...)`.

Flower geometry (`viz.R:742-793`): sort by component; `ymax=cumsum(even)`, `ymin=lag(ymax)`, `xmax=score`, `xmin=0`; `geom_rect_interactive(color="white", alpha=0.5)`; `coord_polar(theta="y")`; `xlim(c(-10, max(score)))`; centre annotation `round(weighted.mean(score, even))` size 8 bold; `theme_minimal()`, legend bottom, 20 pt margins.

Palette (`viz.R:757-762`): `scales::hue_pal()(8)` named `c("invertebrate","mammal","other","primprod","turtle","bird","coral","fish")`.
⚠ **`"primary producer"` is NOT in that vector** → that petal renders **grey/NA** on v8/v9. Fix in the port; note it when diffing screenshots.

### 2.6 `## Table of Scores`
**Narrative (static)** `:324`:
> Mean component and overall sensitivity scores per area of interest, with the count of raster cells (N cells) included in the analysis. Component scores are ecoregionally rescaled (0–100) averages across all cells in each area.

Table `:326-349`, caption *"Mean component and overall scores per area."*
- One row per area, **submission order. No sorting, no row limit.**
- Columns: `Area` (l) · `N cells` (r, `scales::comma`) · one per component (wide pivot of `ad$scores`) · `Overall` (r).
- `Overall = msens::mean_score(scores)` = `weighted.mean(score, even)` = **plain mean of the 8** (`msens/R/calc.R:636-638`).
- All numerics except `N cells` rounded to **0 dp**. Align `c("l","r", rep("r", n-2))`.

### 2.7 `## Summary of Species`
**Narrative (static)** `:354` and `:356`:
> Species counts by category and extinction risk, the top 20 highest-scoring species (ranked by habitat-weighted extinction risk), and a link to download the full species list as CSV.
>
> Note: Extinction risk categories are consolidated: 'FWS' and 'NMFS' codes combined to 'USA'; parenthetical numbers indicate the extinction risk score (1–100) used in sensitivity weighting. The "other" category includes "IUCN:DD" (Data Deficient), IUCN:LC (Least Concern), and species with no assigned risk code.

HTML panel-tabset `:362-377`. Empty case → `_No species found for this area._`

**(a) counts by category × ER category** — `report_area_child.qmd:29-95`
`er_consolidate()` `:45-57`:

| rule | label |
|---|---|
| `FWS`/`NMFS` + `EN` | `USA:EN(100)` |
| `FWS`/`NMFS` + `TN` | `USA:TN(50)` |
| `FWS`/`NMFS` + `LC` | `USA:LC(1)` |
| `IUCN:CR` | `IUCN:CR(50)` |
| `IUCN:EN` | `IUCN:EN(25)` |
| `IUCN:VU` | `IUCN:VU(5)` |
| `IUCN:NT` | `IUCN:NT(2)` |
| else (incl. `NA`, `IUCN:DD`, `IUCN:LC`) | `other(1)` |

`NA` er_code first normalised to literal `"NA"` (`:33-34`). Fixed column order `:62-65` ∩ present `:66`. Body: `distinct(mdl_key, sp_cat, er_cat) |> count(sp_cat, er_cat) |> pivot_wider(values_fill=0L)`, rename `Category`, `arrange(Category)`, `relocate` (`:68-77`). Adds `Total` **column** (rowSums) and `Total` **row** (colSums) (`:78-83`). `scales::comma(accuracy=1)` (`:86-88`). Caption *"Species counts by category and extinction-risk category (N species)."*, `N = n_distinct(d$mdl_key)` (`:41`). Align `c("l", rep("r", n-1))`.

**(b) Top 20** — `:97-131`
`distinct(mdl_key, sp_cat, sp_common, sp_scientific, er_code, er_score, suit_er_area) |> arrange(desc(suit_er_area)) |> head(20)`.
Columns `Category · Common · Scientific · ER code · ER score · Score`.
`Common` = `<a target="_blank">` to the species app (HTML only); id field `mdl_key` when **`ver >= v8`** else `mdl_seq`; `URLencode(reserved=TRUE)` (escapes `|` and `:`); base = `mapsp_base` override else `app_link("species", …)` (`:113-122`).
`ER score` = `scales::percent(er_score, accuracy=1)` — `er_score` is a **0–1 fraction** here. `Score` = `comma(round(suit_er_area, 0))`.
Caption *"Top 20 highest-scoring species (by habitat-weighted extinction risk)."* Align `c("l","l","l","l","r","r")`.

**(c) CSV link** — `:133-142`
`[Download full species list (CSV — {N} species)]({api_base}/species.csv?ver=&kind=&value=&label=)`, all values `URLencode(reserved=TRUE)`. For a drawn area `value` is the **full WKT in a query string** — the real reason for the 8,000-char cap.

### 2.8 Callout "Software" (collapsed) — `:385-394`
`callout-caution collapse="true"` with `devtools::session_info()`. The only provenance block. **No data-version stamp outside the Parameters callout; no disclaimer, citations, acknowledgements or logo.** `dbDisconnect()` at `:380-383`.

---

## 3. Every computed number — exact queries

### 3.1 What the report actually connects to

`msens::sdm_db_con()` → `msens::sdm_db_path()` (`msens/R/db.R:19-44`): `/share/data/big/{ver}/sdm.duckdb`, **falling back to `serve.duckdb`**. On the server the v8/v9 `sdm.duckdb` does *not* exist, so `/report` reads **`serve.duckdb`** — a KB-sized view DB over the released Parquet (`workflows/release_marine-atlas.qmd:263-326`).

Two consequences:
1. `serve.duckdb` exposes `val AS value` on `cell_metric`, `zone_metric`, `zone` (`release…qmd:290-294`). `cells_in_pra()`/`scores_for_pra()` reference `value`/`zone.value` (`calc.R:168, 198`), so they work only against that view layer (or v1–v7). **The published Parquet has `val`; `zone` has `val`.** Use `msens::sdm_val_col()`.
2. `serve.duckdb` has a **`cell_model`** view (2.5° spatial tiles, LOCAL copy on the server) that `sdm.duckdb` does not. `.species_sql()` prefers it.

Published tables (`release…qmd:144-149`):
```r
rel_tables <- c("cell","taxon","dataset","model","metric","zone_taxon",
                "cell_metric","zone","zone_cell","zone_metric","native_asset",
                "taxon_model","listing")
```
→ `https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/{ver}/tables/{t}.parquet`
**Path-style is mandatory** — the dotted bucket breaks virtual-hosted TLS (`release…qmd:276-278`, `msens/R/atlas.R:3-5`). One file per table, **not partitioned**; Parquet V2 + zstd, `ROW_GROUP_SIZE_BYTES '80MB'` (`msens/R/parquet.R:14-28`). Anonymous **LIST is denied**, which is exactly why tables are single files.

**v9 sizes / rows (measured; live S3 Content-Length matches local):**

| table | rows | bytes | columns |
|---|---:|---:|---|
| `cell` | 17,072,105 | **392,465,203** | `cell_id, lon, lat, depth_mean/min/max, oxy_b_mean, oxy_mean, prim_prod_mean, ice_con_ann, salinity_b_mean, salinity_mean, sbt_an_mean, sst_an_mean, fao_area_m, area_km2, in_usa, in_pra` |
| `cell_metric` | 9,664,461 | **75,035,533** | `cell_id, metric_seq, val` |
| `zone_taxon` | 140,717 | **5,999,979** | 18 cols (below) |
| `native_asset` | 86,857 | 1,208,712 | |
| `model` | 91,362 | 1,176,002 | `mdl_key, mdl_id, ds_key, sp_id, sci_name, common_name, er_score, sp_cat` |
| `taxon` | 37,067 | 1,033,168 | 34 cols |
| `taxon_model` | 51,562 | 404,872 | |
| `zone_cell` | 2,241,876 | **151,872** | `zone_seq, cell_id, pct_covered` |
| `listing` | 11,532 | 97,315 | |
| `dataset` | 13 | 12,935 | 33 cols |
| `zone_metric` | 795 | **6,429** | `zone_seq, metric_seq, val` |
| `zone` | 37 | 1,368 | `zone_seq, zone_set_key, tbl, fld, val` |
| `metric` | 41 | 1,300 | `metric_seq, metric_key, description` |
| **total** | | **~477 MB** (`cell` = 82 %) | |

**Not in `tables/`**: `model_cell` (serve surface, Hive by `mdl_id`) and **`cell_model`** (Hive by 2.5° tile, ~1.4 GB / 422 partitions, **server-local, deliberately never pushed to S3** — `release…qmd:167-178, 303-315`). v9 `model_cell` = **784,076,697 rows** / 17,781 taxa.

Zones v9: `programarea_key` **20** (ALA ALB BFT BOW CEC CHU COK GAA GAB GEO GOA HAR HOP KOD MAT NAV NOC NOR SHU SOC), `ecoregion_key` **12**, `subregion_key` **5**. Cells per PRA: HAR 67,835 · ALA 45,685 · GAA 14,238 · COK 1,505. `zone_cell.pct_covered` is 1–100 with **74,938 of 2,241,876 rows partial**.

`manifest.json` (`msens::manifest_build()`, `msens/R/version.R:377-464`) is the machine contract: `ver, status, access, grid_id, id_field, capabilities{cell_species_list, native_representation, programareas, planareas, zone_taxon, score_cogs}, tables{<name>: <https URL>}, metrics[] (102 = 17 keys × 6 subregion views, each with COG url + rescale + colormap "spectral_r"), zones[{zone_set_key, tbl, fld, n, pmtiles}], grid{nc:7200, nr:3600, xmin:-180, ymax:90, resx:0.05, resy:0.05, lon360:false}`. v9 capabilities: all true except `planareas`.

### 3.2 `metric` — the components (v9, 41 rows; identical key list in v8)

```
 1-7   extrisk_{bird,coral,fish,invertebrate,mammal,primary_producer,turtle}     [cell_metric]
 8-28  extrisk_{cat}_ecoregion_{min,max,rescaled}   min/max → zone_metric ONLY; rescaled → both
29     primprod            Primary productivity VGPM/VIIRS npp_avg (mg C/m2/day)  [cell_metric]
30-32  primprod_ecoregion_{min,max,rescaled}
33     score_extriskspcat_primprod_ecoregionrescaled_equalweights   Equal-weight composite [both]
34-41  {component}_ecoregion_rescaled_prepctareaweighting            [zone_metric ONLY]
```

`scores_for_*()` filters on the **anchored** regex `"_ecoregion_rescaled$"` → **8 components**: the 7 `extrisk_*` + `primprod`. The `$` is what excludes the `_prepctareaweighting` rows.
Component label = `metric_key |> str_replace("extrisk_","") |> str_replace("_ecoregion_rescaled","") |> str_replace("_"," ")` → `bird, coral, fish, invertebrate, mammal, primary producer, turtle, primprod`. `filter(component != "all")` removes nothing on v8/v9.

⚠ Naming collision (`merge_taxon.qmd:143-145`): `primary_producer` = a **species category**; `primprod` = the **NPP raster metric**. Both are in the composite.

⚠ `metric_seq` is **regenerated every run** (`score_cell_metrics.qmd:54` drops the table) — **join on `metric_key`**, never on the integer.

`cell_metric` carries 17 metric_seqs over 634,208 cells. Coverage is **uneven** and load-bearing:
```
extrisk_bird 634,208 · coral 577,811 · fish 634,208 · invertebrate 634,208 · mammal 634,206
extrisk_primary_producer 408,089 · turtle 414,461 · primprod 623,616
*_ecoregion_rescaled: bird/fish/invert/mammal 623,212 · coral 566,820 · prim_prod 397,093 · turtle 406,103 · primprod 617,578
composite (33) 623,212, integer-valued, min 0 max 90 mean 23.20
```
**623,212 = 634,208 `in_usa` cells − 10,996 that fall in no ecoregion polygon** (so no rescale, no composite). Per-cell component count varies: 8 comps at 189,432 cells; 7 at 372,304; 6 at 52,994; 5 at 6,966; 4 at 1,516.

### 3.3 `cell_metric.extrisk_{cat}` — raw per-cell component — **CONFIRMED**

`workflows/score_cell_metrics.qmd:96-105`, verbatim:
```r
er_sql <- "(CASE t.er_mode WHEN 'premultiplied' THEN 100 WHEN 'cell' THEN e.er ELSE t.er_score END)"
for (cat in sp_cats) {
  s <- mseq(glue("extrisk_{cat}"), glue("Extinction-risk-weighted suitability, {cat}"))
  dbExecute(con, glue("INSERT INTO cell_metric (cell_id, metric_seq, val)
    SELECT mc.cell_id, {s}, ROUND(SUM({er_sql} * mc.val) / 100.0, 2) AS val
    FROM model_cell mc JOIN taxon t ON mc.mdl_key = t.ms_merge_key
    LEFT JOIN model_cell_er e ON e.mdl_key = mc.mdl_key AND e.cell_id = mc.cell_id
    WHERE {taxa_where} AND t.sp_cat = '{cat}'
    GROUP BY mc.cell_id"))
}
```
with `taxa_where` = `t.is_valid_usa AND t.is_marine` by default (`:78-80`; `SCORE_V7COMMON` / `SCORE_ALLBIRDS` alter it).

- It is a **SUM over species, not a mean** → richer cells score strictly higher (v9 ecoregion maxima: invertebrate 3,451 · coral 2,813 · fish 2,068 · mammal 1,192 · bird 627 · turtle 454 · primary_producer 167).
- ER weight chosen by `taxon.er_mode` (assigned `score_zones.qmd:125-134`): `'taxon'` (37,042 taxa) → scalar `taxon.er_score`; `'premultiplied'` (6 sea turtles) → literal **100**, because `model_cell.val` is already `round(er × suit / 100)` (`msens::turtle_sql`, `merge.R:170-185`); `'cell'` (19 NMFS DPS taxa) → per-cell `model_cell_er.er` (2,915,281 rows, v9-only).
- ER enters **twice** for ordinary taxa: once as a floor inside `model_cell.val` (`greatest(er, suit)`, `msens::merge_sql`, `merge.R:110-118`) and once as the multiplier here. v7-faithful, not a bug.
- `er_score` scale (`msens/R/listings.R:38-79`): US-listed → `pmax(ESA, MMPA, MBTA)` with `EN=100, TN=50, LC=1, MMPA=20, MBTA=10`; else IUCN `CR=50, EN=25, VU=5, NT=2, else 1`; NA → 1.
- `sp_cat` is purely taxonomic (`msens/R/taxa.R:156-181`). v9 scored species = **17,126** (invertebrate 9,424 · fish 6,290 · coral 783 · primary_producer 319 · bird **229** after the marine cull from 880 · mammal 75 · turtle 6). `reptile`(3)/`amphibian`(1) excluded.

**Verification** — v9 `sdm.duckdb`, `cell_id = 1080221`, stored `extrisk_mammal` = **30.54**:

| taxon | er_mode | er_score | val | cell_er | contribution |
|---|---|---|---|---|---|
| *Ursus maritimus* | taxon | 50 | 50.0 | — | 50×50/100 = 25.00 |
| *Pusa hispida* | **cell** | 50 | 7.7 | **20.0** | 20×7.7/100 = **1.54** |
| *Histriophoca fasciata* | taxon | 20 | 20.0 | — | 20×20/100 = 4.00 |
| | | | | **Σ** | **30.54** ✔ |

The naive `Σ val × taxon.er_score / 100` gives **32.85 — wrong**, because it ignores the per-cell DPS surface.

### 3.4 `*_ecoregion_rescaled` — the 0–100 rescale — **CONFIRMED**

`score_cell_metrics.qmd:115-137`, verbatim core:
```sql
-- min/max per ecoregion zone, into zone_metric
INSERT INTO zone_metric (zone_seq, metric_seq, val)
SELECT z.zone_seq, {min|max seq}, {min|max}(cm.val)
FROM zone z JOIN zone_cell zc USING(zone_seq) JOIN cell_metric cm ON zc.cell_id=cm.cell_id
WHERE z.fld='ecoregion_key' AND cm.metric_seq={base} GROUP BY z.zone_seq;

-- coverage-weighted blend of the per-ecoregion rescale
INSERT INTO cell_metric (cell_id, metric_seq, val)
WITH cell_ecoregion AS (
  SELECT zc.cell_id, zc.zone_seq,
         zc.pct_covered * 100.0 / SUM(zc.pct_covered) OVER (PARTITION BY zc.cell_id) AS norm_pct
  FROM zone_cell zc JOIN zone z USING(zone_seq) WHERE z.fld='ecoregion_key'),
mm AS (SELECT mn.zone_seq, mn.val min_v, mx.val max_v FROM … USING(zone_seq))
SELECT cm.cell_id, {rescaled seq},
  SUM((cm.val - mm.min_v)/(mm.max_v - mm.min_v) * (ce.norm_pct/100.0)) * 100 AS val
FROM cell_metric cm JOIN cell_ecoregion ce ON cm.cell_id=ce.cell_id JOIN mm ON ce.zone_seq=mm.zone_seq
WHERE cm.metric_seq={base} AND mm.min_v < mm.max_v
GROUP BY cm.cell_id
```

**MIN–MAX, not percentile.** Reference population = the cells of **one BOEM Ecoregion** (12 `ply_ecoregions_2025` polygons — *not* program areas, *not* the whole US). A cell in >1 ecoregion (951 cells) gets the **coverage-weighted blend**, `norm_pct` renormalising its `pct_covered` across ecoregions to 100.

```
rescaled(cell) = 100 × Σ_ecoregions [ (raw − min_eco)/(max_eco − min_eco) × norm_pct/100 ]
```
Edge behaviours: a degenerate ecoregion (`min == max`) is skipped but **the denominator is not renormalised**, depressing the value; a cell in no ecoregion yields **no row** (10,996 cells).

*Independent check*: the simple single-ecoregion form `100×(raw−min)/(max−min)` matched **99.70 %** of 624,160 joined rows to <0.001; the 1,894 misses are exactly the 951 dual-ecoregion cells × 2.

`primprod` (`:141-148`): `cells_from_raster(rast(vgpm_tif)["npp_avg"], cellid_tif, method="bilinear", min_value=0, zero_fill=FALSE)` from `{dir_data}/raw/oregonstate.edu/vgpm.r2022.v.chl.v.sst.2160x4320_2014-2023.avg.sd.tif`, restricted to `in_usa`, then **the same ecoregion rescale**.

Per-cell composite (`:157-163`): `ROUND(AVG(cm.val))` over whichever of the 8 `*_ecoregion_rescaled` are present, equal weights.

### 3.5 `scores_for_pra()` — Program Area path (PRECOMPUTED)

`msens/R/calc.R:195-217` → on published Parquet (**`val`, not `value`**):
```sql
SELECT m.metric_key, zm.val AS score
FROM zone_metric zm JOIN zone z USING (zone_seq) JOIN metric m USING (metric_seq)
WHERE z.fld = 'programarea_key' AND z.val = :pra_key
  AND m.metric_key LIKE '%\_ecoregion\_rescaled' ESCAPE '\'
```
→ 8 rows. ALA: bird 55.12 · coral 17.66 · fish 20.85 · invertebrate 20.30 · mammal 28.18 · primary producer 10.40 · turtle 49.98 · primprod 1.43.

**How `zone_metric` was built** — `score_zone_metrics.qmd:72-92`, three steps:
```sql
-- (a) coverage-weighted mean over cells WITH a value
INSERT INTO zone_metric SELECT zc.zone_seq, {seq}, SUM(cm.val*zc.pct_covered)/SUM(zc.pct_covered)
FROM zone_cell zc JOIN zone z USING(zone_seq) JOIN cell_metric cm ON zc.cell_id=cm.cell_id
WHERE z.fld IN (…) AND cm.metric_seq={seq} AND cm.val IS NOT NULL GROUP BY zc.zone_seq;
-- (b) back up as {key}_prepctareaweighting
-- (c) pct-area down-weight
UPDATE zone_metric zm SET val = val * (
  SELECT SUM(zc.pct_covered) FILTER (WHERE cm.val IS NOT NULL) * 1.0 / SUM(zc.pct_covered)
  FROM zone_cell zc LEFT JOIN cell_metric cm ON zc.cell_id=cm.cell_id AND cm.metric_seq={seq}
  WHERE zc.zone_seq = zm.zone_seq) WHERE …;
```
then the zone composite (`:101-108`): `SUM(val)/COUNT(val)` over the 8 **post-weight** components (plain mean, non-NULL only).

**Algebraically (a)×(c) collapses to a zero-fill weighted mean** — I verified this independently:
```
zone_metric.val = SUM(COALESCE(cell_metric.val, 0) × zone_cell.pct_covered) / SUM(zone_cell.pct_covered)
```
over **every** `zone_cell` row of the zone. **Verified across ALA, GOA, SOC, MAT × all 8 components with zero discrepancies > 0.01.** Competing formulas fail:

| ALA, `extrisk_primary_producer_ecoregion_rescaled` | value |
|---|---|
| **zero-fill pct-weighted (= pipeline's a×c)** | **10.41** ✔ (published 10.40) |
| `Σ(val·pct)/Σ(pct)` over cells that *have* it (= `_prepctareaweighting`) | 15.60 ✘ |
| plain `avg(val)` over cells that have it | 16.10 ✘ |
| area-weighted by `area_km2` | 10.13 ✘ |

Same for turtle: 49.98 vs 56.65.

**`Overall` == the published composite for a Program Area** — verified on all 20 PRAs: mean of the 8 `zone_metric` components equals `score_…_equalweights` to 3 dp (ALA 25.489 · ALB 13.658 · BFT 13.146 · BOW 17.059 · CEC 21.889 · CHU 30.588 · COK 51.665 · GAA 40.448 · …). **Nothing is area-weighted by `cell.area_km2`** — a real high-latitude bias on a lat-lon grid.

### 3.6 `scores_for_cells()` — drawn-polygon path (ON THE FLY)

`msens/R/calc.R:236-256`:
```sql
SELECT m.metric_key, SUM(cm.val * z.pct_covered) / SUM(z.pct_covered) AS score
FROM metric m JOIN cell_metric cm USING (metric_seq) JOIN (<polygon cells>) z USING (cell_id)
WHERE m.metric_key LIKE '%\_ecoregion\_rescaled' ESCAPE '\'
GROUP BY 1
```

> ### ⚠ Inconsistency #1 — the two paths disagree
> This is an **INNER JOIN with no pct-area down-weight**: it is exactly the `_prepctareaweighting` value, whereas `zone_metric` publishes the post-weight value. Verified: the naive mean reproduces `_prepctareaweighting` at **max |diff| = 0.0** across 8 components × 37 zones, and differs from the **published** component by up to **49.1** (turtle), 20.3 (primary_producer), 9.6 (coral), 3.7 (mammal), 2.0 (bird), 1.7 (fish), 1.1 (primprod), 0.8 (invertebrate). On the composite the gap is **up to 3.92 points** and **systematically high** on all 20 PRAs (GEO 26.66 published vs 30.58 naive). **A drawn polygon exactly tracing a Program Area reports different scores today.**

> ### ⚠ Inconsistency #2 — `cells_in_pra()` discards `pct_covered`
> `calc.R:167-177` hardcodes `pct_covered = 100L`, but 74,938 of 2,241,876 `zone_cell` rows are partial. So in one report the PRA *scores* are coverage-weighted (via `zone_metric`) while its *species table* is not.

> ### ⚠ Inconsistency #3 — `/species.csv` uses a third cell set
> In-report PRA species come from `cells_in_pra()` (`zone_cell` lookup); the download link hits `/species.csv?kind=pra`, which re-reads the **GeoPackage** and runs `cells_in_polygon()`. The CSV can differ from the table above it.

### 3.7 `species_for_cells()` — species table (ON THE FLY, both area kinds)

`msens/R/calc.R:521-534` ships the cell set as a literal `VALUES` list:
```r
vals <- paste(sprintf("(%d, %s)", cells$cell_id, cells$pct_covered), collapse=", ")
cells_sql <- glue("SELECT * FROM (VALUES {vals}) AS v(cell_id, pct_covered)")
tiles <- cell_model_tiles(cells$cell_id, ncol = cell_grid_ncol(con))
```
⚠ For HAR that is a **67,835-element `VALUES` list in one SQL string** — a principal reason the report takes minutes.

`.species_sql()` (`calc.R:363-437`) as generated for a v8/v9 `serve.duckdb`:
```sql
WITH z AS (<cells_sql>)
SELECT t.sp_cat,
       t.common_name               AS sp_common,
       t.scientific_name           AS sp_scientific,
       t.taxon_id, t.taxon_authority,
       t.extrisk_code              AS er_code,
       t.er_score / 100.0          AS er_score,      -- 0-1 FRACTION
       t.is_mmpa, t.is_mbta,
       CAST(mc.mdl_key AS VARCHAR) AS mdl_key,
       sum(c.area_km2 * z.pct_covered / 100.0)                   AS area_km2,
       sum(mc.val * z.pct_covered) / sum(z.pct_covered) / 100.0  AS avg_suit
FROM (SELECT cm.cell_id, cm.val, mo.mdl_key FROM cell_model cm JOIN model mo USING (mdl_id)
       WHERE tile IN (<pruned tile ids>)) mc
JOIN z USING (cell_id) JOIN cell c USING (cell_id)
JOIN taxon t ON t.ms_merge_key = mc.mdl_key
WHERE t.is_valid_usa AND t.is_marine AND t.sp_cat NOT IN ('reptile','amphibian')
GROUP BY 1,2,3,4,5,6,7,8,9,10
```
Column spellings resolved per connection by `msens::sdm_cols()` (`calc.R:295-318`): `is_ok`↔`is_valid_usa`, `mdl_seq`↔`ms_merge_key`/`mdl_key`, `value`↔`val`.
Tile key (`msens/R/cell_model.R:70-99`): `tile = ((id-1)//ncol //50)*(ncol//50) + ((id-1)%ncol //50)`, `ncol` = 7200 (`global05`) or 3103 (`usa05`) from the `cell_grid` table. **A wrong `ncol` returns zero rows silently.**

Then `.species_shares()` (`calc.R:491-501`):
```r
suit_er = avg_suit * er_score
suit_er_area = avg_suit * er_score * area_km2
cat_suit_er_area = sum(suit_er_area) by sp_cat
pct_cat = suit_er_area / cat_suit_er_area
arrange(sp_cat, sp_scientific)
```
`suit_er_area` is the "Score" column and the top-20 sort key.

> **The single biggest simplification available to a browser:** for a Program Area this is *already published* as **`zone_taxon.parquet`** (140,717 rows / 6.0 MB; 40,843 for the 20 PRAs — GAA 6,346 · GAB 5,841 · SOC 3,547 · ALA 2,157). `msens::build_zone_taxon()` (`calc.R:555-573`) runs the *same* `.species_sql()` over the *same* `zone_cell` cells — **with the real `pct_covered`**. Columns: `zone_fld, zone_value, sp_cat, sp_common, sp_scientific, taxon_id, taxon_authority, er_code, er_score, is_mmpa, is_mbta, mdl_key, area_km2, avg_suit, suit_er, suit_er_area, cat_suit_er_area, pct_cat`. So a PRA report needs **no per-model scan at all**: `SELECT * FROM zone_taxon WHERE zone_fld='programarea_key' AND zone_value=:key`. (In v8+ `er_score` and `avg_suit` are 0–1 fractions; v3–v7 used `rl_code`/`mdl_seq`/`suit_rl*` and a 1–100 `er_score` — `.zone_taxon_normalize()` at `calc.R:455-489` handles all three vintages.)

### 3.8 Precomputed vs on-the-fly

| Element | Program Area (`kind="pra"`) | Drawn polygon (`kind="wkt"`) |
|---|---|---|
| Cell set | `zone` ⋈ `zone_cell` (**precomputed**; `pct_covered` discarded → 100) | `cells_in_polygon()` (**on the fly**, exact coverage) |
| `N cells` | `COUNT(*)` of `zone_cell` for the zone | count of intersecting cells |
| Component scores | `zone_metric` (**precomputed**, post-pct-area) | `cell_metric` live (**inner join, pre-pct-area**) |
| `Overall` / map fill | mean of the 8 (= published composite) | mean of the 8 |
| Species tables | `species_for_cells` over `cell_model` (**on the fly** — though `zone_taxon` has it) | same |
| CSV link | `/species.csv` re-derives cells from the **GeoPackage** | `/species.csv` re-parses the WKT |

---

## 4. How a drawn polygon is handled

1. **Capture** — mapgl draw control → `input$map_rpt_drawn_features` → `msens::drawn_features_sf()` (`msens/R/viz.R:975+`), accepting both a GeoJSON **string** (old mapgl) and a parsed **list** (mapgl ≥ 0.5.0). **Last feature only** (`app.R:2917`).
2. **Serialization** — `sf::st_as_text()` → **WKT, EPSG:4326**. **Limit: 8,000 characters**, else `st_simplify(dTolerance = 0.01)` (~1 km at the equator) with a notification (`app.R:3071-3077`). No server-side limit, no vertex cap. The cap exists because the **CSV link puts the WKT in a query string**.
3. **Transport** — inside the JSON POST body.
4. **Cell selection** — `msens::cells_in_polygon(poly, con)` (`calc.R:64-150`), **dispatching on the connection's schema**:
   - **v8/v9** `.cells_in_polygon_db()` (`:124-150`), when `cell` has `lon`/`lat`:
     a. `st_union(st_transform(poly, 4326))`, `st_wrap_dateline()`;
     b. bbox ± half a cell (`h = 0.025`), longitude split into 1–2 antimeridian-safe ranges (`.lon_ranges()` `:114-119`);
     c. `SELECT cell_id, lon, lat FROM cell WHERE (lon BETWEEN …) AND lat BETWEEN …` (candidates only);
     d. build a 0.05° square per candidate centre, `st_intersects`, then `st_intersection` + `st_area`;
     e. **`pct_covered = round(area_of_intersection / (0.05×0.05) × 100)`**, keep `pct > 0`. Computed **planar in degrees** (matching terra's `cover=TRUE` in the raster CRS), *not* geodesically. ~0.02 s for a 2°×1.5° area.
   - **v1–v7** `.cells_in_polygon_raster()` (`:86-103`): `terra::extract(r_cell_id, vect(poly), exact=TRUE)` on the 0-360 `derived/r_bio-oracle_planarea.tif` after `st_shift_longitude()`; per-feature fractions summed, capped at 1. (0.12 s vs 35.16 s for the old rasterize-everything version.)
   - Passing a `SpatRaster` still works but nothing verifies grid agreement — the historical bug where a Santa Barbara polygon resolved to **Arctic** cells on v8 and returned zero species **silently** (`calc.R:1-13`, `plumber.R:496-500`, `report.qmd:113-119`).

   **So: neither centroid-in-polygon nor exactextractr — an exact `sf` polygon ∩ cell-square intersection, area-weighted, planar in degrees.** (Note `zone_cell` was built differently — `exactextractr::exact_extract()` coverage fraction × 100, rounded, zeros dropped, `msens/R/zone_set.R:222-244` — so the two conventions are near-identical but not bit-identical.)
5. **Score aggregation** — §3.6: `Σ(val·pct)/Σ(pct)`, **inner join, no pct-area weight**.
6. **Species list** — §3.7, coverage-weighted, tile-pruned.
7. **Map geometry** — the WKT itself (`report.qmd:107-108`).

To reproduce a *published* zone score from published cell values a browser needs, per component c:
```
pre_c = Σ(cell_metric[c].val × pct) / Σ(pct)          over polygon cells WITH a value
wgt_c = Σ(pct | cell has c) / Σ(pct | all polygon cells)
val_c = pre_c × wgt_c
score = Σ val_c / count(val_c)                        # plain mean, non-NULL only
```
The denominator cell set matters: `zone_cell` includes cells with `in_usa = FALSE` (19,238 rows) and even 3,173 cells absent from `cell`.

---

## 5. Version handling

- **Chosen** in the app (`rpt_ver`), default = version on screen; choices from `msens::atlas_versions()` → `versions.json` (`app.R:950-956`).
- **Passed** as `ver`; **resolved + gated** by `ms_gate_ver()` (`plumber.R:90-106`): `""`/`"latest"` → `latest.txt` (**currently `v7`**); must exist in `versions.json`; `restricted` requires `X-MS-Preview-Token`. 300 s memo; a registry outage **passes the version through**.
- **Used** for: the DuckDB (`sdm_db_con(version)`), the Program-Area GeoPackage (`derived/{ver}/ply_programareas_2026_{ver}.gpkg`), and every outbound link.
- **Stamped** only inside the collapsed *Parameters* callout. Title is user text; date is **render** time. No "data as of" line.
- **Link host** depends on `params$access`: `restricted` → `preview.marinesensitivity.org`, else `app.marinesensitivity.org`; the version is the **URL path** (`/{ver}/scores/`, `/{ver}/species/?mdl_key=…`) — `report.qmd:70-75`.
- **Species deep-link parameter is version-dependent**: `mdl_key` for `ver >= v8`, `mdl_seq` for v1–v7 (`report_area_child.qmd:113-121`).
- Registry URLs: `…/marine-atlas/versions.json`, `…/latest.txt`, `…/{ver}/manifest.json`. **A browser report should drive everything off `manifest.json`** — it already lists each table's URL and declares capabilities.

---

## 6. Hard in a browser — and what to do

### 6.1 Genuinely hard

| # | Thing | Why | Suggestion |
|---|---|---|---|
| 1 | **PDF** | Quarto + LuaLaTeX/TinyTeX | **Drop LaTeX.** Print-CSS HTML + browser "Save as PDF" (`@page {size: Letter; margin: .75in}`, `break-inside: avoid`, `@media print` expanding callouts and flattening tab-sets). `html2pdf.js`/`jsPDF` works but rasterises text; a headless-Chrome print microservice is the honest fallback. |
| 2 | **DOCX** | Pandoc | Feasible via the `docx` npm package or `html-docx-js`. But the current DOCX has **no reference doc** (ESP template commented out at `report.qmd:26`), so parity = "Quarto default styling". **Check `format` usage in `log/reports.csv` before building it.** |
| 3 | **Static ggplot basemap** (PDF/DOCX map) | `rnaturalearth` + ggplot2 | Render the MapLibre canvas with `map.getCanvas().toDataURL()` (needs `preserveDrawingBuffer: true`) and inline the PNG. |
| 4 | **`session_info()`** | R-specific | Replace with a provenance block: `ver`, manifest `status`/`access`, table URLs + ETags, app git SHA, DuckDB-WASM version, timestamp. Strictly better. |
| 5 | **`cell_model` is server-local** | 1.4 GB / 422 partitions, deliberately never pushed to S3 (`release…qmd:303-315`); anonymous S3 LIST is denied | **The** blocker for an arbitrary drawn polygon's species list. Options in order: (a) **publish `serve/cell_model/tile=*/` to S3** — same bytes, and tile-pruned range reads are exactly what S3 is good at (needs predictable paths since LIST is denied); (b) precompute more zones; (c) v1 ships species lists only for published zones. |
| 6 | **`cell.parquet` = 392 MB** | needed for `area_km2` in the species aggregation | Publish a **slim `cell_slim.parquet`** (`cell_id, lon, lat, area_km2, in_usa`) — should land well under 100 MB — or fold `area_km2` into the `cell_model` partitions. |
| 7 | **The restricted-release gate** | `/report` refuses a restricted version without the preview token (`plumber.R:100-106`), because "a titled, citable PDF is exactly the artifact that must not circulate before review", while the *data* stays public by design (`:37-51`) | A static GitHub-Pages app **cannot hold a secret.** Either serve the browser app from the Cloudflare-Access preview host for restricted versions, or accept the gate becomes presentation-only. Defensible (the data is already public) — but it must be a **decision**, not an oversight. |

### 6.2 Already solved / easy

- **DuckDB-WASM over the published Parquet is a proven pattern in this repo**: `workflows/_output/query.html` (added 2026-07-15, commit `c6763c4e`) does `@duckdb/duckdb-wasm@1.29.0` from jsDelivr, `INSTALL httpfs; LOAD httpfs;`, `read_parquet('https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v8/tables/<t>.parquet')`, Arrow → HTML table → client-side CSV via `Blob`. Cold start ~5 s. Modelled on `CalCOFI/db-query`.
- **CORS is already configured for exactly this**: `workflows/data/s3/cors_public.json` (`GET`/`HEAD`, `AllowedHeaders:["*"]` so the `Range` preflight passes, `ExposeHeaders` incl. `Content-Range`/`Accept-Ranges`), applied by `release_marine-atlas.qmd:252-260` under `RELEASE_CORS=1`. A bare `AllowedMethods:[GET]` rule **403s** the preflight.
- **Program-Area geometry is published**: `{atlas_base}/zones/{zone_set_key}/zones.fgb` (FlatGeobuf; v9 key `programarea_2026-01`) — `apps/scores/app.R:531-540`; PMTiles at `file.marinesensitivity.org/pmtiles/`. **Do not depend on the server-only GeoPackage.**
- **Coverage in the browser**: bbox prefilter in SQL + exact 0.05° square ∩ polygon in JS (turf.js) — a ~25-line port of `.cells_in_polygon_db()`; or DuckDB-WASM's `spatial` extension.
- **Flower plot**: a polar stacked bar — trivial in D3 / Observable Plot / Vega-Lite. Fix the `"primary producer"` palette gap while porting.
- **Map**: the app already uses MapLibre GL; the report's HTML map *is* `mapgl::maplibre()`.
- **Caching**: the server cache exists only because rendering is expensive. Use the **URL as the cache key** — matching the stated goal (`workflows/.claude/msens_notes.md:58`: *"bypass the whole Quarto R API by passing the parameters via URL, including the database version … place names and geospatial vertices … compacted into the URL for sharing"*). Consider OPFS persistence (`msens_notes.md:56` cites the DuckDB-WASM+OPFS post).

### 6.3 Candid recommendations

1. **Drive everything off `manifest.json`** — it names each table's URL and declares `capabilities.zone_taxon` / `cell_species_list`, so the report cannot promise a section the release can't supply.
2. **Use `zone_taxon.parquet` for Program Areas** — a PRA report becomes a 6 MB read plus two GROUP BYs; no `VALUES` list, no `cell_model`. Expect small differences from today because it honours `pct_covered` (inconsistency #2) — which is the *correct* behaviour.
3. **Fix the three inconsistencies rather than port them.** In particular apply the pct-area down-weight to drawn polygons so a polygon tracing a Program Area returns that Program Area's published numbers. That is a *verifiable* parity test (§7).
4. **Publish `serve/cell_model/` and a slim `cell` table** — these two pipeline-side changes are what make an arbitrary drawn polygon possible client-side at all.
5. **HTML-with-print-CSS as the only format for v1**; measure `format` usage before building DOCX.
6. **Retire `/species.csv`** — it is reachable only from the report, and the browser already holds the rows (`query.html` shows the Blob pattern).
7. **Join metrics on `metric_key`, never `metric_seq`** — the registry is dropped and recreated each run.
8. **A UI that iterates `metric` will show 16 empty cell layers** — `_ecoregion_min`/`_max`/`_prepctareaweighting` have zero `cell_metric` rows.

---

## 7. Parity checklist — "report parity = done when…"

### Structure
- [ ] Order: title + timestamp · intro · **Parameters** (collapsed) · **Map** · **Plot of Scores** · **Table of Scores** · **Summary of Species** · **Software/Provenance**.
- [ ] Multiple areas → tab-set (HTML), one `### <label>` per area under *Plot of Scores* and *Summary of Species*, in submission order.
- [ ] All static narrative from §2 present, with the **stale category sentence at `report.qmd:304` corrected** to the 8 real components.
- [ ] Empty-area case prints `_No species found for this area._`

### Numbers
- [ ] For each of the 20 v9 Program Areas the 8 component scores equal `zone_metric.val` to 2 dp (ALA: 55.12 / 17.66 / 20.85 / 20.30 / 28.18 / 10.40 / 49.98 / 1.43).
- [ ] `Overall` = unweighted mean of those 8 = the published composite `score_…_equalweights` (ALA 25.489 · COK 51.665 · GAA 40.448 · BFT 13.146).
- [ ] `N cells` for a PRA = `COUNT(*) FROM zone_cell` (HAR 67,835 · ALA 45,685 · GAA 14,238 · COK 1,505).
- [ ] **A drawn polygon exactly tracing a Program Area reproduces that Area's 8 published scores** — requires the pct-area down-weight of §3.5. *This is the test that catches inconsistency #1.*
- [ ] Species counts per PRA match `zone_taxon` (GAA 6,346 · GAB 5,841 · SOC 3,547 · ALA 2,157).
- [ ] `er_consolidate()` reproduced exactly incl. `NA`→`other(1)` and the fixed order `USA:EN(100), USA:TN(50), USA:LC(1), IUCN:CR(50), IUCN:EN(25), IUCN:VU(5), IUCN:NT(2), other(1)`; Total row **and** Total column; comma-formatted.
- [ ] Top-20 sorted by `suit_er_area = avg_suit × er_score × area_km2` desc, `head(20)`; `ER score` an integer percent of a 0–1 fraction; `Score` = `comma(round(suit_er_area))`.
- [ ] `avg_suit = Σ(val·pct)/Σ(pct)/100`, `area_km2 = Σ(cell.area_km2 · pct/100)` — both coverage-weighted.
- [ ] Eligibility: `is_valid_usa AND is_marine AND sp_cat NOT IN ('reptile','amphibian')`.
- [ ] If any raw metric is recomputed, per-cell ER honoured for `er_mode='cell'` and `100` used for `'premultiplied'` (spot-check: cell 1080221 `extrisk_mammal` = **30.54**, not 32.85).
- [ ] Ecoregion rescale blends across multi-ecoregion cells via `norm_pct` (951 cells affected); cells in no ecoregion produce no value (10,996 cells).

### Figures
- [ ] Map: Spectral **reversed** (red = high), opacity 0.6, label at `point_on_surface`, legend "Mean score" with the **report-relative** range (±0.5 when all areas equal), tooltip `"<label> — mean score: <score>"`.
- [ ] Flower: one petal per component, equal widths, radius = score, centre = rounded mean, polar, white borders, alpha 0.5, legend beneath — **and a real colour for `primary producer`**.

### Links & downloads
- [ ] Intro links to `{host}/{ver}/scores/`; host = preview for `restricted`.
- [ ] Top-20 `Common` links to `{host}/{ver}/species/?mdl_key=<urlencoded>` (v8+) / `?mdl_seq=` (v1–v7).
- [ ] CSV control yields the `species_for_cells()` + `.species_shares()` columns: `sp_cat, sp_common, sp_scientific, taxon_id, taxon_authority, er_code, er_score, is_mmpa, is_mbta, mdl_key, area_km2, avg_suit, suit_er, suit_er_area, cat_suit_er_area, pct_cat`; filename `species_<slug>_<ver>.csv`.

### Versioning & robustness
- [ ] Version from `versions.json`; `latest` → `latest.txt` (**v7** today); unknown version errors clearly; `prerelease`/`restricted` handled per the §6.1-7 decision.
- [ ] v1–v7 releases work: `is_ok`/`mdl_seq`/`value` resolved by introspection; `usa05` grid (`ncol = 3103`, 0-360 longitudes).
- [ ] Published Parquet read with **`val`**, not `value`.
- [ ] The whole report regenerates from the URL alone.
- [ ] An antimeridian polygon (Aleutians) returns the same cells as the R path.
- [ ] Zero intersecting cells degrades gracefully (no silent empty report).

### Output
- [ ] HTML is self-contained and prints cleanly via "Save as PDF" (callouts expanded, tab-sets flattened, no clipped tables).
- [ ] Provenance names `ver`, release `status`/`access`, table URLs, generation time, app version.

---

## Appendix A — file index

| Path | What |
|---|---|
| `api/plumber.R:326-444` | `POST /report` |
| `api/plumber.R:446-516` | `GET /species.csv` |
| `api/plumber.R:301-324` | LRU cache pruning |
| `api/plumber.R:52-106` | preview-token gate + version resolution |
| `api/report.qmd` | report template (395 lines) |
| `api/report_area_child.qmd` | per-area child (145 lines) |
| `apps/scores/app.R:1546-1607` | Report tab UI |
| `apps/scores/app.R:2895-3269` | Report tab server |
| `apps/scores/app.R:1311-1367` | popup/download JS |
| `apps/scores/app.R:509-551` | `zone_geom()` / `pra_geom()` — published `zones.fgb` |
| `msens/R/calc.R:64-150` | `cells_in_polygon()` (db + raster paths) |
| `msens/R/calc.R:167-177` | `cells_in_pra()` |
| `msens/R/calc.R:195-217` | `scores_for_pra()` |
| `msens/R/calc.R:236-256` | `scores_for_cells()` |
| `msens/R/calc.R:295-359` | `sdm_cols()` / `sdm_val_col()` |
| `msens/R/calc.R:363-437` | `.species_sql()` |
| `msens/R/calc.R:455-501` | `.zone_taxon_normalize()` / `.species_shares()` |
| `msens/R/calc.R:521-625` | `species_for_cells()` / `build_zone_taxon()` / `species_for_zone()` |
| `msens/R/calc.R:636-638` | `mean_score()` |
| `msens/R/cell_model.R:44-99` | tile key |
| `msens/R/db.R:19-61` | `sdm_db_path()` / `sdm_db_con()` (serve.duckdb fallback) |
| `msens/R/viz.R:719-829` | `ggplot_flower()` / `plot_flower()` |
| `msens/R/viz.R:933-972` | `ggmap_areas()` |
| `msens/R/version.R:97-99, 377-464` | `atlas_base_url()`, `manifest_build()` |
| `msens/R/zone_set.R:222-244` | `zone_cell` coverage extraction |
| `msens/R/listings.R:38-79` | `er_score` scale |
| `msens/R/merge.R:67-230` | `merge_sql()` / `turtle_sql()` / `dps_sql()` |
| `workflows/score_cell_metrics.qmd:54-163` | `metric` / `cell_metric` construction |
| `workflows/score_zone_metrics.qmd:53-137` | `zone_metric` + `zone_taxon` construction |
| `workflows/score_zones.qmd:78-213` | `model_cell`, `model_cell_er`, `zone`, `zone_cell`, `er_mode` |
| `workflows/release_marine-atlas.qmd:144-149` | published table list |
| `workflows/release_marine-atlas.qmd:242-260` | S3 CORS for DuckDB-WASM |
| `workflows/release_marine-atlas.qmd:263-326` | `serve.duckdb` views |
| `workflows/data/s3/cors_public.json` | the CORS policy |
| `workflows/_output/query.html` | DuckDB-WASM-over-S3 precedent |
| `workflows/.claude/msens_notes.md:50-64` | the stated intent for the new JS app |
| `server/plumber/Dockerfile` | API image (Quarto, TinyTeX, pinned msens 0.13.1) |
| `server/docker-compose.yml:169-183` | `plumber` service |
| `server/caddy/Caddyfile:17-19, 62-67` | api vhost, `/reports/*` attachment |
| `docs/figures/apps-guide/steps.json` (Scores step 11) | the only user-facing description |
| `docs/figures/apps-guide/scores-11.png` | the only screenshot |

## Appendix B — user-facing documentation (there is almost none)

- **The only substantive description** is Apps User Guide step 11, generated from the in-app tour at `apps/scores/app.R:1851-1853`:
  > "Generate a sensitivity report for one or more custom areas. Build up a list of labeled areas by drawing a polygon on the map (using the polygon tool) and/or by selecting a Program Area, then clicking 'Add' for each. Set a title, data version, and output format (HTML, Word, or PDF), then click 'Generate report'."
- One screenshot: `docs/figures/apps-guide/scores-11.png`.
- Passing mentions: `docs/apis.qmd:74` ("on-demand PDF/DOCX report generation"), `docs/server.qmd:82` ("report downloads"), `docs/libraries.qmd:27`.
- **`docs/apps.qmd:34-40` lists the Scores app's key features and omits the Report tab entirely.**
- No README anywhere mentions it; no release-note entry outside `msens/NEWS.md` (0.3.0 at `NEWS.md:885-890` added `ggplot_flower()`/`ggmap_areas()` "for multi-format (html / pdf / docx) reports"; `:600-605`, `:624`, `:668-672` record the silent-empty-report, drawn-polygon and `serve.duckdb` bugs).
- **No stated limitations anywhere.** The only duration hint is the app's JS string, "usually a couple of minutes".
- **There is no other per-place report template in any repo.** `grep -ril "params:" --include="*.qmd"` returns 6 files; the only other genuine parameterized report is `workflows/compare_versions.qmd` (per version-pair, not per place); the rest are pipeline notebooks (`backfill_versions`, `build_v7_cell_model`, `publish_score_cogs`). `msens/NEWS.md:801` references a `workflows/validate_versions.qmd` that no longer exists (renamed to `compare_versions.qmd`).
- **Plan status**: the intent to move reports into a browser app is written at `workflows/.claude/msens_notes.md:50-64` (dated 2026-09-20) with an empty `app/` repo scaffolded that same day (single commit, LICENSE + README only). No formal plan file exists yet in `.claude/plans/` or `.claude/plans_todo/`.

---

# Summary (≈480 words)

The Scores Shiny app's Report tab collects a list of labelled areas — a Program Area key or a drawn polygon serialised to WKT — plus a title, data version and output format, and POSTs them as JSON to `https://api.marinesensitivity.org/report`. The plumber endpoint gates the version against `versions.json` (403 for a restricted release without the preview token), hashes the inputs into an 8-hex cache key, and, on a miss, runs `quarto::quarto_render()` on `api/report.qmd` + `api/report_area_child.qmd` inside a tempdir. The output lands in `/share/public/reports/` under a 500 MiB LRU cache and is returned as a URL on `file.marinesensitivity.org`, which Caddy serves with `Content-Disposition: attachment`. The app pre-opens a placeholder browser tab synchronously to dodge popup blocking and does the POST in a background R worker with a 600 s timeout. Rendering takes "a couple of minutes"; analytics record 32 s successes and 90 s failures.

The report has seven parts: a boilerplate intro, a collapsed Parameters echo, a Map (interactive MapLibre for HTML, a static ggplot with Natural Earth for PDF/DOCX), a flower plot per area, a Table of Scores, a per-area Summary of Species (a category × extinction-risk cross-tab, a top-20 table, and a CSV link), and a collapsed `session_info()`. There is no logo, disclaimer, bibliography or data-vintage stamp beyond the Parameters callout, and the DOCX reference template is commented out.

Every number is traceable and, importantly, **almost all of it is already published as Parquet on S3**. Component scores are the eight `*_ecoregion_rescaled` metrics. For a Program Area they come straight from the precomputed `zone_metric` table; the report's "Overall" is their plain mean, which I verified equals the published equal-weight composite on all 20 Program Areas. For a drawn polygon they are aggregated live from `cell_metric` — **and the two paths use different formulas**: `zone_metric` applies a pct-area down-weight (equivalently, a zero-filled coverage-weighted mean over every zone cell) while the drawn path inner-joins and omits it. Measured, that is up to 49 points on a single component and ~4 points on the composite. Two further inconsistencies exist: `cells_in_pra()` discards the real `pct_covered`, and `/species.csv` re-derives Program-Area cells from a GeoPackage rather than from `zone_cell`.

The species tables are computed live via a `VALUES` list of every cell id — 67,835 for the largest Program Area — joined against `cell_model`. That table is the one genuine blocker: it is deliberately server-local and never pushed to S3. For Program Areas the identical result is already published as `zone_taxon.parquet` (6 MB), so only arbitrary drawn polygons need it.

A browser replacement is well-precedented: `workflows/_output/query.html` already runs DuckDB-WASM against these exact Parquet files, and the bucket CORS policy allowing Range requests is committed and applied. The remaining work is publishing `cell_model` (and a slim `cell` table), replacing LaTeX/Pandoc output with print-CSS HTML, and deciding what happens to the restricted-release gate, which a static app cannot enforce.

---

# The 10 most important facts for a planner

1. **`zone_taxon.parquet` (6 MB) already contains the entire species table for every Program Area** — same SQL, same cells, computed with the *correct* `pct_covered`. A Program-Area report needs no per-model data at all. It is mandatory in the release (`release_marine-atlas.qmd:138-141`).

2. **`cell_model` — the only surface that can answer "which species are in this arbitrary polygon" — is deliberately NOT on S3.** It is ~1.4 GB across 422 spatial-tile partitions, kept local on msens1 because many-partition reads over HTTPS fail, and anonymous S3 LIST is denied on the bucket. Publishing it (predictable `tile=` paths, no LIST needed) is the single unlock for drawn-polygon species lists in the browser.

3. **The Program-Area and drawn-polygon score formulas disagree today.** `zone_metric` = coverage-weighted mean × pct-area coverage ratio (algebraically: `Σ(coalesce(val,0)·pct)/Σ(pct)` over *all* zone cells); `scores_for_cells()` = `Σ(val·pct)/Σ(pct)` over cells that *have* the metric. Measured gap: up to 49.1 points on turtle, 20.3 on primary producer, ~3.9 on the composite — and systematically high on all 20 Program Areas. Pick one convention; the zone one is correct.

4. **The published Parquet stores `val`, not `value`.** `value` exists only as a view alias in `serve.duckdb`/`attach_atlas()` (`release_marine-atlas.qmd:290-294`). `msens::cells_in_pra()` and `scores_for_pra()` reference `value` and therefore only work through that view layer. Use `val` (or `msens::sdm_val_col()`).

5. **Join metrics on `metric_key`, never `metric_seq`** — `score_cell_metrics.qmd:54` drops and recreates the `metric` table every run. Also: `_ecoregion_min`/`_max`/`_prepctareaweighting` have **zero** `cell_metric` rows, so a UI iterating `metric` shows 16 empty layers.

6. **Component = `Σ over species (ER × suitability)/100`, a SUM not a mean**, where ER is `taxon.er_score` normally, the **per-cell `model_cell_er.er`** for 19 NMFS DPS taxa (`er_mode='cell'`), and literal **100** for the 6 sea turtles (`er_mode='premultiplied'`, ER already inside `val`). Verified on cell 1080221: 30.54, where the naive scalar-ER formula gives 32.85. Rescaling is **min–max within each of the 12 BOEM Ecoregions**, coverage-blended for the 951 cells in two ecoregions.

7. **`manifest.json` is the contract to build on.** `msens::manifest_build()` publishes `ver, status, access, grid_id, id_field, capabilities{zone_taxon, cell_species_list, …}, tables{name → full HTTPS URL}, metrics[] (with COG URLs + rescale), zones[] (with PMTiles), grid{}`. Read it instead of hardcoding table names or grid geometry.

8. **DuckDB-WASM over this data is already proven and CORS is already configured.** `workflows/_output/query.html` uses `@duckdb/duckdb-wasm@1.29.0` + `httpfs` against `https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/{ver}/tables/{t}.parquet` (**path-style mandatory** — the dotted bucket breaks virtual-hosted TLS). `data/s3/cors_public.json` allows `GET`/`HEAD` with `AllowedHeaders:["*"]` so the Range preflight passes; a bare `AllowedMethods:[GET]` rule 403s.

9. **`cell.parquet` is 392 MB — 82 % of the whole 477 MB table set** — and is needed only for `area_km2`/`lon`/`lat`. A slim published `cell_slim.parquet` would make the browser app practical. Everything else is small: `cell_metric` 75 MB, `zone_taxon` 6 MB, `zone_cell` 152 KB, `zone_metric` 6.4 KB, `metric` 1.3 KB.

10. **Two things do not survive the move and need explicit decisions.** (a) The restricted-release gate: `/report` refuses a restricted version without a shared secret, because a titled citable PDF must not circulate pre-review — a static GitHub-Pages app cannot hold that secret. (b) PDF/DOCX: both need LaTeX/Pandoc. `latest.txt` is currently **v7**, so a browser app defaulting to "latest" shows v7 while v8/v9 are restricted and 302-redirected to the preview host.
