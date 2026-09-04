# v10-3 · Ranges derived from AquaMaps for species without an expert range

**Phase:** P2 of `2026-09-04 v10 plan.md`. **Model:** Opus for the calibration design, the analysis
notebook and the merge fixture (scientific judgment + the most consequential rule change in v10);
Sonnet for the ingest boilerplate, registry rows, docs and release-notes text; Opus review at the end.
**Touches:** new `ingest_am-derived-ranges.qmd` (+ a calibration notebook), `../msens/R/` (+ tests),
`merge_models_prep.qmd` (dependency), `build_registry.qmd` (`ds_order`), `../docs/data/release_notes.yml`,
`../docs/data-sources.qmd`, `../docs/model-merging.qmd`, `references.bib`.

## Why (Ben's TODO, `.claude/msens_notes.md:7`)

> derive range maps from AquaMaps for species without IUCN range maps by finding taxa-based threshold
> to apply to AquaMaps that most closely matches species where they do match, probably per taxa
> class / component. See O'Hara paper evaluating this.

In v9 a species with an expert range (IUCN, BirdLife, FWS, NMFS, SWOT) is scored on suitability
**masked to that range**; a species with no range is scored on the raw AquaMaps/AquaX surface wherever it
predicts anything ≥ 1 (the am-only `UNION ALL` branch of `msens::merge_sql()`, `merge.R:94-103`). That is
**15,011 of the 17,781 scored taxa** (invertebrates 9,224, fishes 5,165, corals 331, primary producers
282, mammals 6; every bird and turtle has a range). The masked branch exists because AquaMaps
over-predicts (the ~750 `iucn_range_outside_us_eez` species; walrus to 9.75 °N); the am-only majority
gets no such protection, and the review page had to build their v10 global denominators from
"AquaMaps outside the US + merged inside" because they have no merged global surface at all.

Facts (verified 2026-09-04): range vs suitability is decided by exclusion — `ds_key NOT IN (suit_ds)`
is a range, so a new key is a range automatically; `has_range` is computed from the crosswalk in
`merge_models.qmd:237-242`; giving a taxon a range flips it out of the raw branch into the masked
one (and INTO the global surface, which today omits am-only taxa). Range datasets are valued by
`msens::compute_er_score()`; none of the am-only scored taxa carries an IUCN code, so their governing
`er_score` stays the default 1 (`merge_models_prep.qmd:299`). AquaMaps values are probability × 100,
bilinear, kept ≥ 1 (`ingest_aquamaps.qmd:60,141-152`); AquaX ships a per-model max-TSS `cutoff`
(`model_ax.csv`, median ≈ 55–59 on the 0–100 scale) that is recorded, not applied. The calibration
set (taxa with BOTH a suitability surface and an expert range) is 3,466 taxa, 2,770 scored: fish
1,125, bird 880, coral 452, invertebrate 200, mammal 69, primary producer 37, turtle 6.

Literature (checked 2026-09-04): O'Hara et al. 2017 (PLoS ONE 12:e0175739) compared IUCN and AquaMaps
for 2,330 species on a 0.5° grid with two metrics, distribution alignment (proportion of overlapping
presence cells) and area ratio (smaller/larger), and examined AquaMaps at P > 0 (approximating
IUCN's extent of occurrence) vs P > 0.5; mean alignment 63 %, 75 % for expert-reviewed AquaMaps; no
single threshold suited every taxon, "matched to the purpose"; IUCN over-predicts coral depth;
AquaMaps truncates at FAO-area boundaries; poorly aligned species are data-poor. The Ocean Health
Index moved its AquaMaps threshold from 0.4 to > 0 to approximate extent of occurrence (Halpern et
al. 2017). Jones & Cheung 2015 used a per-species empirical threshold (minimum suitability at known
occurrences). AquaMaps' own documentation states no cut-off. So: **calibrate, do not assume 0.5.**

## Deliverables

1. **Calibration notebook `calibrate_am_ranges.qmd`** (`msens:` block, `workflow_type: validate` or
   whatever `build_targets_list()` accepts; depends on `merge_models_prep`): for every calibration
   taxon, binarize its suitability surface (the v9 merge input: am, with ax superseding inside the
   AquaX mask) at thresholds t ∈ {1, 2, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80} plus, for ax taxa, the
   model's own max-TSS `cutoff`; compare with the expert-range footprint (`b_range` cells) using
   Jaccard, O'Hara's distribution alignment and area ratio, and sensitivity/commission. Report per
   `sp_cat` (and per WoRMS class where n ≥ 30) the threshold maximising median Jaccard, the curve, and
   the gain over t = 1 (today's footprint). Decision rule, stated in the notebook: adopt a class
   threshold only where it raises median Jaccard by ≥ 0.05 over t = 1 with n ≥ 30 calibration taxa;
   otherwise the class keeps t = 1 (the footprint), which is O'Hara's and OHI's extent-of-occurrence
   reading. Classes with no usable calibration set (primary producers 37, mammals 69 are borderline)
   inherit the nearest well-populated class or keep t = 1; say which.
2. **`msens::range_threshold_calibrate()`** (returns the metrics table for one taxon × thresholds) and
   **`msens::range_from_suitability(con, mdl_keys, threshold)`** (DuckDB filter over the Parquet:
   cells with val ≥ t, written as `(mdl_key, cell_id, val = er_score)`), both tested on synthetic
   surfaces: a surface that matches a range exactly at t = 50 must return Jaccard 1 at 50 and < 1
   elsewhere; a t above every value returns an empty range (and the ingest must then SKIP the taxon,
   never write an empty range, because an empty range would erase the species from the US score).
3. **`ingest_am-derived-ranges.qmd`** with a FULL `msens: dataset:` block (`ds_key: rng_am`,
   `response_type: range`, `name_short`, `name_display`, `description` naming the calibration and
   the per-class thresholds, `citation` O'Hara 2017, `is_mask: true`; template
   `ingest_nmfs-dps.qmd:1-19`); reads `data/am_range_thresholds.csv` written by the calibration
   notebook; writes `dist/dataset=rng_am/*.parquet` for the am-only taxa only (a taxon with an expert
   range never gets a derived one), `model_rng_am.csv` with `mdl_key`, `scientific_name`, `worms_id`
   (so `merge_models_prep` resolves by id), and the manifest. Add `rng_am` to `ds_order` in
   `build_registry.qmd:67` and to the `dependency:` of `merge_models_prep.qmd:6`. Gate with
   `AM_RANGE=0` (control run: no `rng_am` in the merge input, so the merge hash must equal the
   VGPM/share-free v10 baseline) — the same pattern as `AX_SUPERSEDE=0`.
4. **Merge fixture** in `test-merge.R`: `T_am_derived` (am cells {1,2,3,100}, derived range {1,2,100})
   → US `{1,2}` at the am values, cell 3 masked, global `{1,2,100}`; `T_am_single` unchanged. A
   derived range that is empty in the US must give zero US rows — assert it, and assert the ingest's
   skip rule prevents it in practice (count taxa lost).
5. **Consequences to report** (in the merge notebook and the release notes): taxa entering the
   global surface (17,131 → up to ~32,000), `dist_merged_global` size, merged-COG count (the am-only
   alias to `native/am` no longer holds for these taxa: `PUBLISH_MERGED_COG` repaints them), the US
   scoring surface row count, and the v10 share denominators (the review's "AquaMaps outside the US"
   fallback is retired for these taxa).
6. **Docs**: `../docs/model-merging.qmd` (new subsection: derived ranges, thresholds table, the
   O'Hara/OHI reasoning), `../docs/data-sources.qmd` entry, `references.bib` (O'Hara 2017, Halpern
   2017, Jones & Cheung 2015).

## Steps

1. Read `merge.R`, `test-merge.R`, `merge_models.qmd:150-260`, `ingest_nmfs-dps.qmd`,
   `.claude/skills/ingest-sdm/SKILL.md`, and the counts above; run the msens tests (green baseline).
2. Calibration notebook on v9 data (laptop; DuckDB over `dist/dataset={am,ax,rng_*}`); write
   `data/am_range_thresholds.csv` (`sp_cat`, `threshold`, `n_calib`, `jaccard_t`, `jaccard_1`,
   `adopted`). Show the curves; a reviewer must be able to see why each class got its number.
3. msens functions + tests; reinstall.
4. Ingest notebook; render; check the skip count and the per-class footprint areas before/after.
5. Fixture; run the merge with `AM_RANGE=0` (hash equals baseline) and `=1`; `pra_score_delta`
   against the baseline; attribution table per Program Area and category.
6. Docs; commit with the thresholds table and the attribution table in the message.

## Review checklist (Opus)

- The calibration compares like with like: the suitability surface used is the merge INPUT (with
  ax supersession), the range is the `b_range` footprint, both on the global05 grid, both global.
- Thresholds are adopted only by the stated rule; t = 1 classes are listed, not hidden.
- No taxon lost its US presence because of an empty derived range (count = 0, or each case named).
- `AM_RANGE=0` reproduces the baseline merge hash.
- The global surface, COG registry and share denominators account for the new taxa.
- `er_score` for derived-range taxa is still `compute_er_score()`'s value (1), never hard-coded.
