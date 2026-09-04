# v10 review page: "Sensitivity Across Regions"

The interactive review of Tim White's distribution-share brief (2026-09-03) that defines **v10**:
each species' risk-weighted share of its *global* range, summed per component and zone, expressed per
km² against the national average concentration (100), equal component weights, a 5 % coverage floor.
Published as a private artifact at <https://claude.ai/code/artifact/773b9b71-efa4-4d46-9a99-385415bc04e5>;
the memo with the findings and every addendum is
`.claude/plans/2026-09-04 distribution-share review + literature.md`, the brief itself
`.claude/plans/2026-09-03 Tim's MST_distribution_share_method_brief.md`.

Everything on the page is computed from the **v9** release on this machine by `build.sh`, which runs
the numbered scripts in order (intermediates in `tmp/`, gitignored):

| step | script | what it produces |
|---|---|---|
| 01 | `01_pull_v9.R` | `zone_metric` (long), `zone_taxon` (Program Areas, subregions, ecoregions), `taxon`, zone areas, Program Area × ecoregion areas from `{dir_big}/v9/sdm.duckdb`; asserts the cell-area formula and the `suit_er_area` definition |
| 02 | `02_global_totals.R` | per-taxon global suitability-area totals over `dist_merged_global` (the v10 denominator for range-constrained taxa; ~30 s) |
| 03 | `03_am_totals.R` | AquaMaps totals (all cells, US cells) for valid-US taxa with no merged global surface (~2 min) |
| 04 | `04_national_only.R` | the taxa whose only models are national or regional (FWS / NMFS ranges, AquaX) |
| 05 | `05_obis_fallback.py` | the R2 fallback fraction for those taxa: endemic / literature / OBIS occurrence fraction (area 266 "United States: all"); compares with the committed `data/us_share_fallback.csv` (`OBIS_WRITE_DATA=1` adopts a refreshed one; `REDO_OBIS=1` refetches) |
| 06 | `06_geo.R` | simplified, longitude-shifted Program Area + study-area GeoJSON for the maps |
| 07 | `07_build_v9_json.R` | `v9_data.json`: Program Area × category tables (v9 published, v9 cell-level variants, v10 with US and global denominators, densities, coverage), composites, Spearman, top species, turtles, category totals |
| 08 | `08_denom_check.R` | denominator diagnostics (merged-global vs AquaMaps source, model- vs range-based US fraction) folded into the JSON |
| 09 | `09_post_json.py` | embeds the geometry; fits the richness slope β per category |
| 10 | `10_build_page.py` | injects the JSON into `sensitivity-across-regions.tpl.html`, converts the document to pure ASCII (the artifact viewer rendered UTF-8 as mojibake), `node --check`s the script, writes `sensitivity-across-regions.html` (tracked) |

The page's JavaScript recomputes every v10 view from the JSON (`v10base/v10rows/v10all/v10composite`
at the top of the script), so the definition lives in one place; the v9 variants (percentile
endpoints, national min–max, US denominator, mean share) are demonstrations only.

Known approximation: steps 02 and 03 compute cell area from the latitude band on a sphere (R = 6371 km),
within 0.9 % of the ellipsoidal `cell.area_km2` the US numerators use; the pipeline implementation
(`.claude/plans_todo/v10-1 share metrics (share_sql).md`) uses `cell.area_km2` on both sides.

Requirements: R with DBI/duckdb/dplyr/tidyr/jsonlite/sf/glue (rmapshaper optional), python3, node;
the v9 release under `{dir_big}` as laid out in `libs/paths.R` (laptop: `~/_big/msens/derived/v9`).
Publish with the Artifact tool from the same file path to keep the URL (`label` = the revision).
