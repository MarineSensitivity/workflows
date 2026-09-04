# Method: Distribution-Share Sensitivity Score for National Program Area Comparison

*Marine Sensitivity Toolkit — method brief for implementation. T. White, September 2026.*

## Purpose

The ecoregion-rescaled score (Formula 9.4) is relative within an ecoregion and cannot be compared across ecoregions. Table 10-2 of the 11th Program 1st Analysis invites exactly that comparison, and for turtles it ranks the Aleutian Arc (76) above every Gulf of America area. This method adds a **nationally comparable score per category** built from the fraction of each species' distribution a zone holds, and re-issues the Program Area table on it. The existing ecoregional score is not changed or removed.

## Scope note: versions

The published figures and Table 10-2 are **v1** (Planning Areas, reptile category, 0.2–1.0 risk weights, IUCN/AquaMaps turtle footprints). The follow-up report draft is **v6** (Program Areas, turtle category, SWOT+DPS range, multiplicative merge, 1–100 weights). v6 corrected the turtle cell values but left Formula 9.4 unchanged, so the cross-region illusion persists in the v6 table. Build this method on **v6** (or the v9 prerelease if that becomes the report release), not v1. Comparisons back to the published v1 table are by Planning-Area geometry; cell ids are not comparable across the v8 grid change.

## Concept in one paragraph

For each species, normalize its merged distribution so it sums to 1 across the species' whole range. A cell's value is then "the fraction of this species that is here." Sum those fractions across the species in a category, weighting by extinction risk as the pipeline already does, and roll up to zones. A zone's number is the share of the category's national (risk-weighted) importance it holds. A zone with a few vagrants scores near zero; a zone holding an entire endemic population scores at the top; no thresholds, masks, or region-specific rules are needed, and the same arithmetic applies to every category.

## Definitions

- `v_{s,c}` merged model value for species *s* in cell *c* (`model_cell.val`, 0–100). For turtles this already contains DPS extinction risk (Formula 8.2); for NMFS DPS-listed species the per-cell risk `er_{s,c}` sits beside it; for all others the scalar `er_score_s` applies at scoring (Formula 9.1).
- `A_c` cell area (`cell.area_km2`).
- `w_{s,c}` risk weight in the cell: 100 for turtles (pass-through), `er_{s,c}` for DPS-listed species, `er_score_s` otherwise.
- `p_{zc}` fraction of cell *c* inside zone *z* (`zone_cell.pct_covered / 100`).

## Steps

### Step 1 — Strip the presence floor for this metric (turtles and any premultiplied taxa)

The turtle merge is `max(1, round(v_er × v_suit / 100))`. The floor of 1 fills the entire SWOT range and, over the North Pacific, contributes a large area of 1s that would dilute the share. For the share calculation only:

- use the suitability surface `v_suit` (AquaMaps, or AquaX inside its mask) rather than the floored merged value, with `v_er` applied as the weight `w` in Step 3, **or**
- keep the merged value but set cells equal to the floor (value = 1 with `v_suit` rounding to 0) to 0.

Apply AquaX's TSS cutoff (recorded in v9, not applied) so sub-threshold AquaX cells are absences. The published merged model and ecoregional score are unchanged.

### Step 2 — Per-species share

For every valid scored species:

    share_{s,c} = v_{s,c} · A_c / Σ_{c'} v_{s,c'} · A_{c'}

The denominator runs over the species' **entire merged range**, global where the merge is global (v8+); inside the study area otherwise, and the metric description states which. Result: `Σ_c share_{s,c} = 1` for every species.

### Step 3 — Category cell score

    share_{c,g} = Σ_{s ∈ g} w_{s,c} · share_{s,c} / 100

Same structure as Formula 9.1 with `share` in place of `v`. Store as `extrisk_{g}_share` in `cell_metric`. Because `w` is inside the sum, an Endangered species' fraction counts more than a Least Concern species' fraction, exactly as today.

### Step 4 — Zone share

    share_{z,g} = Σ_{c} share_{c,g} · p_{zc}

Store in `zone_metric` for every spatial unit (Program Area, Ecoregion, Subregion; Planning Area where geometry is carried). Express as a percentage for reporting: `100 · share_{z,g}` is "the percent of national category-*g* importance in zone *z*."

### Step 5 — National 0–100 rescale for the table

    score_{z,g}^{nat} = 100 · share_{z,g} / max_{z'} share_{z',g}

Max taken over the reporting units of the release (20 Program Areas). Store as `extrisk_{g}_share_national_rescaled`. This is the number that goes in the re-issued table's component columns. Composite = equal-weighted mean of the category columns plus primary productivity, as now; primary productivity has no species share, so use its national (study-area) min–max rescale in place of the ecoregional one.

### Step 6 — Optional population weighting for turtles (Wallace et al. 2023 RMUs)

Assign each SWOT-range cell to its Regional Management Unit polygon. Multiply `share_{s,c}` by `N_{RMU} / N_{species}` (RMU nesting-population estimate over the species total), then renormalize so the species still sums to 1. Alaska needs no RMU of its own: its cells inherit the North Pacific loggerhead / West Pacific leatherback population weights, both small. Do this after Steps 1–5 are validated; it is a refinement, not a prerequisite.

### Step 7 — Re-issue the Program Area table

Columns per Program Area: Rank (new), Composite (new), one column per category from Step 5, plus a **share %** column for the category of interest (turtles at minimum), and a secondary table or appendix carrying the current ecoregional values unchanged. Include the change in rank against the published table.

## Implementation notes (msens conventions)

- Add `share_sql(ver, category)` to `msens` next to `merge_sql()`, `turtle_sql()`, `dps_sql()`. Scientific logic lives in the package, not in the notebook; the notebook calls it.
- Add synthetic fixtures: (a) one species endemic to a single ecoregion → that ecoregion's share = 1; (b) one species with 0.5% of its range in a region → that region's share ≈ 0.005; (c) a floored turtle cell contributes 0 after Step 1; (d) all per-species shares sum to 1 ± 1e-9.
- Register the new metrics in `metric` (`extrisk_{g}_share`, `extrisk_{g}_share_national_rescaled`, and the composite over them). Version bump and changelog entry per Chapter 15.
- Tables: `model_cell` (val), `cell` (area_km2), `taxon` (sp_cat, er_score, is_er_spatial), `zone_cell` (pct_covered), `zone`. v8+ keys models by `mdl_key`; earlier by `mdl_seq`.
- The existing `zone_taxon.suit_rl_area` is the un-normalized numerator of Step 2 summed to zones; the share metric is close to a per-species normalization of it, which makes the treemap a natural place to surface per-species shares.

## Validation targets

- Every species' shares sum to 1 (or to the fraction of its range inside the study area, if the denominator is study-area only).
- Existing ecoregion-rescaled `cell_metric` and `zone_metric` rows are byte-identical before and after.
- Turtle share: Gulf of Alaska, Aleutian Arc, Bowers Basin, Aleutian Basin each < 1%; Gulf of America Program Areas hold the majority of the national turtle share; Straits of Florida / Eastern Gulf at or near the national 100.
- Mammal share: Rice's whale at the top of the per-species contribution in its Program Area; Arctic endemics (bowhead, walrus, ice seals, polar bear) keep the Chukchi/Beaufort/Hope mammal columns high. If the Arctic collapses in the mammal or bird columns, the denominator or floor handling is wrong — stop and check before proceeding.
- Report the Spearman rank correlation between the published and re-issued composite rankings, per the v1 recommendation.

## Decisions needed from the team before Step 7

1. Does the re-issued table replace the ecoregional table as the headline, or run alongside it? (Recommended: replace for cross-region reporting, retain the ecoregional table for within-region use.)
2. Apply to all categories (recommended) or turtles only. A single national column beside six ecoregional ones is worse than either consistent choice and would mix scales in the composite.
3. Denominator scope where the global range is not merged: study-area share, stated as such, or exclude the species from the share metric.

## What the analyst delivers

1. `share_sql()` with fixtures, merged into `msens`.
2. New `cell_metric` / `zone_metric` rows for the current release, with the validation checks above passing.
3. The re-issued Program Area table beside the published one, with rank changes and the rank correlation.
4. A one-page methods note for the report: the concept paragraph above, Formulas in Steps 2–5, and the statement that ecoregional scores are relative within region and the share score is the cross-region comparison.
