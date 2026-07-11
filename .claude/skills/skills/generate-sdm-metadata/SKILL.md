---
name: generate-sdm-metadata
description: Derive the taxon/model/dataset metadata that drives MarineSensitivity scoring — the dataset+model registry, taxonomy-based sp_cat, governing extinction-risk floors (MMPA/MBTA), and the marine-bird relevance cull. Use when adding datasets, fixing categorization, or debugging why a taxon is scored/valued wrongly.
---

# Generate SDM metadata (registry + taxonomy + scoring flags)

The v8 metadata is **consolidated in dedicated targets, not written per-ingest** — the per-ingest
writes are exactly what silently disappeared in the rewrite.

## Registry — `build_registry.qmd`

Rebuilds `dataset` + `model` tables (which the ingests only *imply*): `dataset` from every
`msens: dataset:` front-matter block (inherits v7 name/citation where keys match; v8 props
authoritative); `model` from unioning every `model_{ds}.csv` + the merged taxon. **Fails loudly**
if an ingested dataset lacks a metadata block; **warns** on declared-but-not-ingested (gm/nc).
Run it before release/STAC/apps. Do NOT re-add per-ingest `INSERT INTO dataset/model`.

## `sp_cat` by taxonomy — `merge_taxon.qmd`

Assigned from **WoRMS class/phylum/kingdom + BirdLife→bird**, NOT a v7 crosswalk (that dumped every
new taxon into a catch-all `other`). Rule: bird (botw|Aves), mammal (Mammalia), turtle (SWOT set),
fish (Teleostei/Elasmobranchii/Holocephali/Myxini/…), coral (Hexa/Octocorallia), primary_producer
(Plantae/Chromista/Bacteria), else invertebrate; **reptile/amphibian categorized but EXCLUDED** from
scoring (not in the `sp_cats` list). No `other`. If a component balloons, suspect a sp_cat mislabel.

**AlgaeBase gap**: the downloadable WoRMS DwC-A excludes AlgaeBase (macroalgae/seagrass), so
worms-authority AphiaIDs missing from the snapshot are fetched from the WoRMS API
(`worrms::wm_record`) and cached in `data/worms_taxonomy_supplement.csv` — self-maintaining (only
new ids fetched; offline → cache). The worms table itself is rebuilt by `ingest_worms.qmd` from the
monthly marinespecies.org download.

## Extinction-risk floors — `merge_models_prep.qmd`

Governing `er_score` = most-protective across a taxon's datasets (US-national overrides IUCN), via
`compute_er_score()` (errors on bad codes). Floors are **taxa-based laws → assign by taxonomy where
the law is**: **MMPA (+20) = all WoRMS Mammalia** (not the incomplete NMFS directory); **MBTA (+10)
= the FWS CFR 50 §10.13 list** (migratory birds native to US — NOT all Aves; a bird off the list gets
no floor). `merge_models` applies the governing er to range cells (the "fitting point"), so
`extrisk_*` sums `er_score × val`.

## Marine-bird cull — `is_marine` (`merge_taxon.qmd`)

v8 keeps whole (largely terrestrial) bird ranges, so cull terrestrial birds from scoring:
`is_marine` (bird) = in a marine/coastal **family** (`data/marine_bird_families.csv`) AND whole-range
`pct_marine ≥ 5%`, OR curated-include, minus curated-exclude (`data/marine_birds_curation.csv`).
The **family allow-list** is what kills the sub-pixel-island confound (kiwis/white-eyes score high
pct but aren't marine families). `pct_marine` MUST come from the **raw global bl ranges** (the merged
model_cell is US-scoped ~100%). Non-bird taxa → `is_marine = TRUE`.
