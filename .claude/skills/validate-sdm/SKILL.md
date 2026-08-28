---
name: validate-sdm
description: Validate a MarineSensitivity change — the rule-level testthat unit tests in ../msens AND the aggregate Program-Area score-equivalence gate. Use after changing merge/scoring logic, adding a distribution model, or bumping the version.
---

# Validate SDM scores (unit tests + version-equivalence gate)

Correctness has **two layers**, and you need both:

1. **Rule-level unit tests** (`../msens/tests/testthat/`) assert each merge/scoring rule's exact
   output on synthetic fixtures — the primary guard when you change logic or add a model.
2. **Aggregate score-equivalence gate** (`msens::pra_score_delta`) confirms Program-Area scores stay
   ~equivalent to the prior version on common inputs.

The aggregate gate can **hide** rule-level breakage: the v8 rewrite silently dropped the
`iucn_range_outside_us_eez` exclusion and re-introduced ~750 AquaMaps over-predictions (e.g. Sotalia
guianensis, a river dolphin), yet their near-zero suitability made the PRA delta look fine. Only a
rule-level assertion catches that. **Run the unit tests first**, then the aggregate gate.

## Unit tests (rule-level guard)

- Merge/scoring rules live in `msens` as the single source of truth (`merge_sql()`, `turtle_sql()`,
  `compute_er_score()`, …); the notebooks *call* them, so notebook and tests cannot drift.
- `test-merge.R` asserts one synthetic taxon per category: range-only, both-masked (am beyond range
  dropped from US), `iucn_range_outside_us_eez`-excluded (no US presence but present in the global
  viz surface), am-only single, am-only multi-model (RAW, duplicates preserved), turtle multiplicative.
- **When you change a rule or add a dataset, add/update its fixture in the same change**, run
  `Rscript -e 'devtools::test("../msens")'`, reinstall msens, and treat a red test as a hard stop.
  Every fixed bug becomes a permanent named assertion.

## The version-equivalence gate

On any pipeline change or version bump, the guardrail is: **Program-Area (PRA) scores stay
~equivalent to the prior version on the *common* inputs.** Real divergence must be explained
(more species, marine cull, taxonomy sp_cat, an intended exclusion like no_eez), not accidental.

## The gate

`msens::pra_score_delta` (run at the end of `score_zone_metrics.qmd`) compares the v8 PRA
composite to v7's and reports `n`, `mean_abs`, `max_abs`, `rmse`, `cor`. Expect **cor high**
(0.97+ once taxonomy sp_cat is in) and small deltas on common species.

## Apples-to-apples (isolate the algorithm from the species-set change)

- `SCORE_V7COMMON=1` restricts scoring to v7's scored set (`in_v7`) — isolates grid/algorithm
  from the v8 species expansion.
- `SCORE_ALLBIRDS=1` disables the marine-bird cull (diagnostic).
- The **strongest** control: score v8 cells with **v7's exact er_score for the shared species**
  (see the archived `apples.R` pattern) — then any residual delta is grid/merge, not ER/lists.

## Per-component breakdown

Compare `zone_metric` per PRA per `metric_key` (extrisk_{spcat}_ecoregion_rescaled + primprod +
the composite) between versions. v7 stores long-format zones (`z.value` key, `zone_metric.value`);
v8 is `z.val` key + `zone_metric.val`, `dataset=`… — parameterize the pull. Interpret:
- coral/invert/primprod cor ≥ 0.99 → untouched (sanity).
- **bird** moves most = the `is_marine` cull (intended); **mammal** = taxonomy MMPA floor.
- a component ballooning (cor low) usually means a **sp_cat mislabel** — check taxonomy assignment
  (v7-crosswalk default → 'other' was the classic bug; now taxonomy-based, no 'other').

## A flat merged surface is a symptom, not a style

If a species' merged map is one colour across its whole range, its governing `er_score` exceeds
its suitability everywhere (`max(er, suit)` at `NMFS:EN` = 100). Ask whether that ER is
species-wide in fact: DPS/ESU-scoped listings must go through `dps_nmfs` (spatial ER × suitability),
not the plain rule. `SELECT ms_merge_key FROM taxon WHERE er_score >= 50` ∩ taxa with a range and no
`dps_nmfs`/turtle model is the list to review.

## Sanity

Re-scoring the SAME inputs must reproduce (`cor = 1.000`). If not, a reserved-word (`value`/`val`,
`class`) or stale-table (CREATE IF NOT EXISTS keeping an old schema) bug is likely.


## A dataset that supersedes another (v9 AquaX) — the control run

When a new dataset REPLACES part of an old one, the aggregate gate diverges *by design* (v9: two
thirds of scored species change surface inside US waters), so "near-zero delta" is no longer the
test. Two things are:

1. **The control run** — `scripts/run_version.sh --control --from merge_models_prep --to merge_models`
   (`AX_SUPERSEDE=0`): AquaX is **excluded** from the merge, so the inputs are exactly `ver_prev`'s and
   `merge_models`' content hash must **equal ver_prev's checkpoint** (v8: `bd8b2931f17bb2b3`). Scoring
   is deterministic on that surface, so the hash is the whole proof — no need to re-score. (Leaving
   ax in but unsuperseded is not a control: it still `max()`es with AquaMaps.) That proves the version
   bootstrap, the generalized `merge_sql(suit_ds)` and every `ver_prev` fix moved nothing, so any
   drift in the real run is the supersession and nothing else. Evidence: `_output/*_control.html`.
2. **The explained gate** — the real run's delta is explained by the ingest's per-component /
   per-region Δ tables (`data/ax_vs_am_summary.csv`), and the components the new dataset cannot
   touch (bird, primary_producer, every non-modeled taxon) stay cor ≥ 0.999. Commit
   `validate_v8_v9.html` beside it.

Rule-level guards for the same change, all of which the v8 behaviour fails: `test-merge.R`
`T_ax_*` fixtures + the control fixture; `merge_models.qmd`'s check that no `am` row survives inside
`ax_mask` for a superseded taxon (on `mc_parts`, what both surfaces read); the ingest's
non-empty COG round trip and its asserted headline counts (10,703 / 10,517 / 182).

## Gates added with the multi-version work (2026-08)

- **`pra_score_delta(..., zone_set_key=)`** pins the comparison to ONE spatial unit rather than
  trusting that two releases meant the same polygons. Databases predating the column (v1–v7) fall
  back to the `fld` filter; naming a zone set a release lacks is an ERROR, not an empty join.
- **`build_zone_cells.qmd` gate**: the relocated `zone_cell` must reproduce the release's existing
  one row-for-row (zone keys, cell ids, coverage) before anything downstream uses it.
- **Score-COG equivalence** (done — the factory is retired as of 2026-08): a COG tile had to be
  byte-identical to the factory's for the same (metric, subregion) at every zoom. Measured 100/105
  byte-identical, the rest 3–6 px of 262,144 from float32 storage. Overviews break this — the
  renderer decimates from full resolution per request, so score COGs are published WITHOUT them.
  Re-run this check whenever the COG writer changes.
- **`mdl_id` stability**: `msens::assign_mdl_id()` must leave every published id untouched.
  `build_registry.qmd` asserts it against the published `model.parquet`. A renumbered id does not
  fail anything — it makes titiler serve the wrong species from the right-looking partition.
- **`dataset.is_scored`**: after a merge change, confirm the flag still matches which datasets have
  `taxon_model` edges. A dataset that is ingested but unmerged (gm, nc) must read FALSE, or every
  dataset count in the docs overstates the inputs.
- **After any scoring change, re-check Program-Area scores are unmoved.** The zone_cell relocation
  and the zone-set generalization were both verified at max |delta| == 0 across all 20 areas.

## Spatial-ER gates (v9)

- `score_zones` asserts every `er_mode = 'cell'` taxon has its per-cell ER (`model_cell_er`) at every
  scored cell — a missing row would weight the species NULL and silently drop it from `extrisk_*`.
- `test-merge.R` asserts `dps_sql()`'s value (suitability, ER as the fitting point where unmodeled,
  nothing beyond the ER footprint), its `er`, the scoring identity `er × val / 100`, AND the turtle-rule
  regression on the same fixture (the humpback collapse) so the two rules cannot be swapped unnoticed.
- The merge manifest's `content_hash` fingerprints `dist_merged` (val) alone; `er_hash` fingerprints
  `dist_merged_er`, and `score_zones` resumes on both — an ER-only change leaves the val surface intact.

