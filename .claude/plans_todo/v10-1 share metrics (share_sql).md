# v10-1 · The v10 score family: `msens::share_sql()` and the share metrics

**Phase:** P3 of `2026-09-04 v10 plan.md`. **Model:** Opus for `share_sql()`, its fixtures and the
scoring-notebook SQL (rule-level correctness over 784 M US rows and 3.1 B global rows; a dropped WHERE
here silently corrupts every zone); Sonnet for metric registration, the compare notebook columns and
docs text; Opus review at the end. **Touches:** `../msens/R/share.R` (new) + `tests/testthat/test-share.R`
(new), `merge_taxon.qmd` (global totals), `score_cell_metrics.qmd`, `score_zone_metrics.qmd`,
`compare_versions.qmd`, `data/us_share_fallback.csv` (already committed), `../docs/scoring.qmd`.

## The definition (settled; page revision 12, memo `.claude/plans/2026-09-04 distribution-share review + literature.md:421-427`)

    share_s,c  = v_s,c · A_c / Σ_c' v_s,c' · A_c'      denominator = the species' GLOBAL merged range
    share_c,g  = Σ_{s∈g} w_s,c · share_s,c / 100         w = er_sql of score_cell_metrics.qmd:96 (premultiplied → 100, cell → e.er, else er_score)
    share_z,g  = Σ_c p_zc · share_c,g                    p_zc = zone_cell.pct_covered / 100
    pct_z,g    = 100 · share_z,g / (Σ_{s∈g} w_s / 100)   percent of the WORLD's risk-weighted importance at stake
    v10_z,g    = 100 · (share_z,g / share_USA,g) / (A_z / A_USA)   location quotient; 100 = the national average per km²
    coverage_z,g = area of the zone's cells that carry a category-g value / A_z; reportable iff ≥ κ = 0.05
    composite_z = mean over reportable components of v10_z,g, plus primprod_v10_z = 100 · mean_z(primprod) / mean_USA(primprod)
    α = 0 (no richness damping; n_species and mean dependence are published beside the score)

Cell-level counterparts (the manifest lists cell metrics only, `msens/R/version.R:397-403`, so the app and
the docs' `tbl-metrics` never see a zone-only metric): `extrisk_{g}_share` = share_c,g;
`extrisk_{g}_v10` (cell) = 100 · (share_c,g / A_c) / (share_USA,g / A_USA), the same quotient per cell.

Facts (verified 2026-09-04):
- Per-species global value·area totals exist NOWHERE (`taxon.n_global` is a cell count; `taxon.range_km2`
  is the US surface, so `range_usa_km2/range_km2 == 1`). `zone_taxon.suit_er_area` (`msens/R/calc.R:491-503`)
  is the un-normalised zone numerator per species and equals Σ val/100 · area · pct_covered/100 · er/100
  (the `subregion_key = 'USA'` pseudo-zone row is the US total). The review page's
  `scripts/v10_review/02_global_totals.R` computes the global totals in ~30 s with the latitude-band area
  formula; use `cell.area_km2` in the pipeline instead (it is global: 25.9 M ocean cells).
- Taxa with no merged global surface: am-only taxa (15,011 scored) until P2 lands derived ranges; the
  27 national-only taxa (FWS/NMFS ranges, AquaX) always. Denominator rule, in order: merged global
  surface → (am-only) AquaMaps outside the US + merged inside → `data/us_share_fallback.csv`
  (`fraction` = US share of the world; the fallback takes precedence for national-only taxa because their
  "global" surface is just their US range). Record the source per taxon in a `taxon_share_denom` table.
- Zone aggregation in v9 weights by `pct_covered` only, NOT by cell area (`score_zone_metrics.qmd:76-91`;
  `docs/scoring.qmd:163` claims area weighting — fix the doc in P5). v10 uses `cell.area_km2` explicitly.
- Absent components are skipped by construction (`AVG` over present rows,
  `score_cell_metrics.qmd:157-163`, `score_zone_metrics.qmd:103-108`); the coverage floor makes a
  below-κ component ABSENT (no row), never zero.
- `pra_score_delta(metric_key = "score_v10_equalweights")` works unchanged (`msens/R/validate.R:178`);
  `zone_scored_flds()` defaults to the ecoregional composite — pass the v10 key.
- The v9 turtle premultiplied surfaces (`er_mode = 'premultiplied'`) are already ER × suit; their global
  surface is premultiplied too, so the share divides like with like. DPS taxa (`er_mode = 'cell'`) use
  `model_cell_er` per cell in the numerator; their global denominator is the suitability surface times
  the same per-cell ER where it exists (state this in the function doc; assert it in a fixture).

## Deliverables

1. **`msens::share_sql(...)`** in `R/share.R`, mirroring `merge_sql()`'s style: returns named SQL
   strings for (a) `taxon_global_total` (Σ v·A over the global surface per `ms_merge_key`, with the
   am-only and fallback branches selectable), (b) the per-cell category share `extrisk_{g}_share`,
   (c) the zone roll-up (`share_z,g`, `pct_world`, `coverage`, `v10` with the USA pseudo-zone as the
   national reference), (d) the cell-level `v10`, (e) the composite over reportable components with
   `primprod_v10`. Parameters: `cats`, `kappa = 0.05`, `denominator = c("global","usa")` (the `usa`
   variant exists only for the sensitivity report), `er_sql` (the same CASE expression the score
   notebook uses). `@concept score`. Bump `Version:` + `NEWS.md`.
2. **`tests/testthat/test-share.R`**, one fixture per rule, exact expected values (the brief's four plus
   the page's): endemic to one zone → share 1, v10 = 100 · (1/share_USA) / (A_z/A_USA); a species with
   0.5 % of its range in a zone → 0.005; every species' shares sum to 1 ± 1e-9 over its global cells;
   a floored turtle cell (val = 1 where suit rounds to 0) contributes its 1/Σ like any cell (v10 keeps
   the surface continuous — the brief's Step 1 is NOT applied; assert it is not); two zones of different
   area with the same intensity → equal v10, unequal `pct_world`; a zone exactly at the national
   average concentration → 100; a component covering 4.9 % of a zone → no row, 5.0 % → a row; the
   composite skips the absent component; national-only taxon with `fraction = 0.25` → global total =
   US total / 0.25; DPS taxon with per-cell ER → numerator and denominator both carry it.
3. **`merge_taxon.qmd`**: write `taxon_global_total` (`ms_merge_key`, `suit_area_global`, `source`)
   from `dist_merged_global` (+ the am-only branch from `dist/dataset=am` with the ax mask, + the
   fallback CSV), and `taxon.us_of_global` = US total / global total. Record `n_fallback` in the
   manifest; assert the 27 fallback taxa all resolve.
4. **`score_cell_metrics.qmd`**: after the `extrisk_{cat}` loop, register and write `extrisk_{cat}_share`
   and `extrisk_{cat}_v10` (cell) via `share_sql()`; **`score_zone_metrics.qmd`**: `extrisk_{cat}_share_zone`,
   `extrisk_{cat}_pct_world`, `extrisk_{cat}_coverage`, `extrisk_{cat}_v10` (NULL where not reportable),
   `extrisk_{cat}_n_species`, `extrisk_{cat}_mean_share` (the two richness factors), `primprod_v10`,
   `score_v10_equalweights`. Keep every v9 metric byte-identical (the brief's validation target): hash
   the ecoregional `cell_metric`/`zone_metric` rows before and after. `SHARE_METRICS=0` skips the family
   (control).
5. **Validation chunk** (in `score_zone_metrics.qmd`): the brief's targets — every species' shares sum to
   1 (or to `us_of_global` inside the US); Gulf of Alaska, Aleutian Arc, Bowers, Aleutian Basin turtle
   `pct_world` each < 1 %; the two Gulf areas hold most of the national turtle share; Rice's whale tops
   the Gulf mammal contribution; the Arctic keeps its mammal and bird columns (if they collapse, the
   denominator or fallback is wrong: STOP); GEO turtles not reportable (coverage 1.5 %); Spearman ρ
   between the v9 and v10 composite rankings printed; the review page's numbers reproduced within
   rounding (GAA turtles 390, ALA 19, composite GAA 242 on v9 inputs — run the family once on v9 with
   `ver` pinned as the acceptance test before running it on v10).
6. **`compare_versions.qmd`**: a v10 section (the re-issued Table 10-2 template of the memo's docs
   drop-ins: v10 score | % of world at stake | v9 rank | Δ rank, Spearman in the caption) for any two
   releases that carry the family; `zone_scored_flds(metric_key = "score_v10_equalweights")`.
7. **`zone_taxon`**: add `share_global` and `share_v10_contrib` (w × share) per species and zone
   (`msens::build_zone_taxon`, `calc.R:555-580`) so the treemap can explain the score.

## Steps

1. Read the memo §"Addendum: v10 as recommended" and the page's `v10base/v10rows` JS (the reference
   implementation, `scripts/v10_review/sensitivity-across-regions.tpl.html`, top of the script), then
   `score_cell_metrics.qmd`, `score_zone_metrics.qmd`, `merge.R`, `test-merge.R`, `calc.R:400-520`.
2. Write `share.R` + `test-share.R`; run tests; reinstall.
3. `merge_taxon.qmd` global totals on v9 (pin `ver`), then the score notebooks on v9 with
   `SHARE_METRICS=1`: reproduce the page numbers (acceptance). Only then run on v10.
4. Compare notebook; docs paragraph (P5 finishes the docs).
5. Commit with the acceptance numbers in the message.

## Review checklist (Opus)

- The global denominator is the whole merged global surface, valued exactly as the US numerator is
  (same `val` scale, same ER treatment per `er_mode`).
- No v9 metric moved (hash before/after); `SHARE_METRICS=0` skips cleanly.
- The USA pseudo-zone (`subregion_key = 'USA'`, `score_zones.qmd:199-210`) is the national reference,
  and `A_USA` is its coverage-weighted area, not `count(in_usa) × mean area`.
- A component below κ has NO row in `zone_metric` (and the composite divides by the reportable count).
- Fixtures cover every bullet in deliverable 2; `devtools::test()` green; `NEWS.md` written.
