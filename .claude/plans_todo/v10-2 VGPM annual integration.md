# v10-2 · Primary productivity: integrate the year, do not average the observed months

**Phase:** P1 of `2026-09-04 v10 plan.md`. **Model:** Sonnet for the implementation (contained raster +
SQL work, the rule is already decided), Opus for the review checklist at the end. **Touches:**
`ingest_productivity.qmd`, `../msens/R/` (+ tests), `score_cell_metrics.qmd` (only if the metric key
changes), `../docs/data/release_notes.yml`, `../docs/data-sources.qmd`, `../docs/scoring.qmd`.

## The defect (decided; do not re-litigate)

Michael K. Rasser's v1 review, comment C76 (`final-report_2025_review-responses_v5-MKR.md:483-528`): the
Arctic Planning Areas (Hope, Norton) carry implausibly high primary productivity. Cause:
`ingest_productivity.qmd:147-150` makes each year's raster as `mean(na.rm = TRUE)` over the 12 monthly
VGPM composites and `:163-176` averages the ten years the same way. Monthly VGPM has no retrieval
under polar night or ice, so north of ~65°N a "year" is the mean of the 4–6 observed peak-bloom months
while a Gulf year is the mean of all twelve. Annualising both by 365 inflates the high-latitude areas
roughly 2× (Hope Basin 875 t C km⁻² yr⁻¹ vs ~470 g C m⁻² yr⁻¹ in situ, Springer & McRoy 1993). The
review response (`:516-518`) committed to the fix: **integrate the year, treating polar-night and
ice-covered months as zero production, not missing.**

Facts the implementer needs (verified 2026-09-04):
- Source: Oregon State standard VGPM, R2022, VIIRS chl + sst, 2160×4320 (0.0833°), **monthly** HDF per
  year in a tarball (`ingest_productivity.qmd:85`, `-9999` = NA at `:123`), units mg C m⁻² d⁻¹, years
  2014–2023. Local copies under `~/_big/msens/raw/oregonstate.edu/` (per-year `vgpm.{yyyy}.tif` annual
  means and the two-band `…_2014-2023.avg.sd.tif` that scoring reads). The notebook still points at the
  old Google Drive root (`:18`); repoint it to `dir_raw` from `libs/paths.R`.
- The notebook is NOT in the targets DAG (no `msens:` front-matter); `score_cell_metrics.qmd:49,140-148`
  reads the finished TIF, resamples bilinearly to the cell grid with `msens::cells_from_raster(...,
  min_value = 0, zero_fill = FALSE)` for `in_usa` cells, registers metric `primprod`, then
  `rescale_ecoregion("primprod")`; the composite averages it with the seven `extrisk_*` components.
- Bio-Oracle `cell.ice_con_ann` (annual ice concentration) and `cell.prim_prod_mean` exist on the cell
  table and are unused; neither is monthly. A monthly CAFE stack sits in the raw dir unreferenced (CAFE
  was tried and abandoned; do not switch products in this phase).

## Deliverables

1. **`msens::vgpm_annual_integrate(monthly, year)`** (new, `R/productivity.R`): takes a 12-layer
   SpatRaster of monthly mean daily rates (NA = no retrieval), returns a two-layer SpatRaster
   `npp` = Σ_m rate_m · days_m / days_in_year (an annual-mean daily rate in the same units, so nothing
   downstream changes) and `n_valid` = number of months with a retrieval. Rules: NA months contribute
   0; a cell with `n_valid = 0` in a year stays NA (land/no data), so land is not painted as zero;
   leap years use 366. Roxygen, `@concept ingest`, exported.
2. **Tests** (`tests/testthat/test-productivity.R`): (a) a cell with 12 equal months → the same value
   as before; (b) a cell with 6 months of 100 and 6 NA → 50 · (days of those months)/365, NOT 100
   (the regression case); (c) all-NA cell → NA; (d) leap-year denominator; (e) `n_valid` counts.
3. **`ingest_productivity.qmd`**: replace both `mean(na.rm = TRUE)` steps: per year call
   `vgpm_annual_integrate`; across years take the mean of the ten integrated annual layers (NA only where
   all ten are NA) and keep the sd; write a new TIF beside the old one
   (`…_2014-2023.avg.sd_integrated.tif`) plus an `n_valid` climatology layer. Add a `msens:`
   front-matter block so the notebook joins the DAG (check `msens::build_targets_list()` for the
   `workflow_type` it accepts; `score_cell_metrics` must depend on it). Gate the download behind
   `REDO_VGPM_DOWNLOAD=1`; reading the cached tarballs/HDFs is the default. Keep the old TIF: the v9
   score must stay reproducible.
4. **`score_cell_metrics.qmd`**: read the integrated TIF for `ver >= v10` (a `VGPM_INTEGRATED=0` control
   flag keeps the v9 raster so the change is attributable in the version-equivalence gate). The metric
   key stays `primprod` (the description in `metric` changes to say "annual integral / 365, polar-night
   and ice months = 0").
5. **Attribution table** in the notebook output: per Program Area and Planning Area, `primprod` before
   and after, and the composite before and after; Hope, Norton, Chukchi, Beaufort should fall by roughly
   half, the Gulf of America areas by a few percent at most. Print the Arctic values against the
   literature figures cited in the review response (Springer & McRoy 1993; Arrigo & van Dijken 2015;
   Hill et al.; Saba et al. 2011) — a plausibility check, not a fit.
6. **Docs + notes**: `../docs/data-sources.qmd:304-310` and `../docs/scoring.qmd:153-155` describe the
   integration; `../docs/data/release_notes.yml` v10 entry bullet; `NEWS.md` + `Version:` bump in msens.

## Steps

1. Read the files above; run `Rscript -e 'devtools::test("../msens")'` to see the baseline is green.
2. Write the function + tests first; run the tests; reinstall msens (`devtools::install("../msens")`).
3. Edit the ingest; render it (`quarto render ingest_productivity.qmd`, laptop, the HDFs are local);
   confirm the new TIF's global mean over ice-free low-latitude cells is within 1 % of the old one and
   the Arctic cells are lower (print both).
4. Render `score_cell_metrics.qmd` and `score_zone_metrics.qmd` for v10 (after P0 bootstrap) once with
   `VGPM_INTEGRATED=0` and once with `=1`; save both `zone_metric` primprod columns; produce the
   attribution table.
5. Commit with the attribution table in the message body.

## Review checklist (Opus)

- The integration divides by days in the year, not by the number of valid months, and a zero month
  is a zero, not an NA.
- Land stays NA (no new cells gained `primprod` rows); count `cell_metric` rows for `primprod` before
  and after.
- `VGPM_INTEGRATED=0` reproduces v9's `primprod` zone values exactly (hash).
- No other metric moved in the control run.
- The old TIF and the v9 checkpoint hashes are untouched.
