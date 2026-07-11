---
name: validate-sdm
description: Validate a MarineSensitivity version bump — confirm Program-Area scores stay ~equivalent to the prior version on common inputs (the score-equivalence gate), and diagnose per-component drift. Use after changing the merge/scoring pipeline or bumping the version.
---

# Validate SDM scores (version-equivalence gate)

On any pipeline change or version bump, the guardrail is: **Program-Area (PRA) scores stay
~equivalent to the prior version on the *common* inputs.** Real divergence must be explained
(more species, marine cull, taxonomy sp_cat), not accidental.

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

## Sanity

Re-scoring the SAME inputs must reproduce (`cor = 1.000`). If not, a reserved-word (`value`/`val`,
`class`) or stale-table (CREATE IF NOT EXISTS keeping an old schema) bug is likely.
