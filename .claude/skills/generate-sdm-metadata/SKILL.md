---
name: generate-sdm-metadata
description: Derive the taxon/model/dataset metadata that drives MarineSensitivity scoring — the dataset+model registry, taxonomy-based sp_cat, governing extinction-risk floors (MMPA/MBTA), and the marine-bird relevance cull. Use when adding datasets, fixing categorization, or debugging why a taxon is scored/valued wrongly.
---

# Generate SDM metadata (registry + taxonomy + scoring flags)

The v8 metadata is **consolidated in dedicated targets, not written per-ingest** — the per-ingest
writes are exactly what silently disappeared in the rewrite.

## Registry — `build_registry.qmd`

Rebuilds `dataset` + `model` + `taxon_model` + `listing`: `dataset` from every
`msens: dataset:` front-matter block (inherits v7 name/citation where keys match; v8 props
authoritative); `model` from unioning every `model_{ds}.csv` + the merged taxon; `taxon_model` and
`listing` carried across from `merge.duckdb`, which builds them but nothing forwarded — v8 was the
only release whose published tables could not answer *which models fed this taxon*.

Two derived facts are stamped here, both **introspected rather than declared**:

- **`dataset.is_scored`** (`msens::dataset_is_scored()`) — a dataset is scored iff it has
  `taxon_model` edges. `gm`/`nc` are ingested but excluded from the merge, so they read FALSE and no
  documented count includes them. Registered is not used.
- **`model.mdl_id`** (`msens::assign_mdl_id()`) — the serve partition key. It reuses the PUBLISHED
  registry's ids and appends new keys above the max, because a plain `dense_rank(mdl_key)` is a
  function of the model set: adding gm+nc moved 45,499 of 80,261 ids, which would have made titiler
  serve the wrong species with nothing failing.

**Fails loudly**
if an ingested dataset lacks a metadata block; **warns** on declared-but-not-ingested (gm/nc).
Run it before release/STAC/apps. Do NOT re-add per-ingest `INSERT INTO dataset/model`.

**A NEW dataset inherits nothing.** `dataset` rows get their rich metadata (name, description,
citation, links, `value_info`, `regions`, `is_mask`) from `ver_prev`'s table where the key matches;
a dataset the previous release never had (v9 `ax`) must declare them in its `msens: dataset:`
front-matter block — `fm_dataset()` reads the optional keys and they win over inherited ones.
Without them the app labels the input by the glue fallback and the docs print no citation.

**Partially superseded ≠ unused.** `am` stays `is_scored = TRUE` in v9 (it still feeds ~6,000 taxa
and every cell outside the AquaX mask); `ax` is TRUE via its `taxon_model` edges. The "registered but
unscored" case is only gm/nc. Which taxa lost AquaMaps *inside the mask* is `data/ax_supersedes_am.csv`.

**The docs are part of the metadata.** A new dataset needs, in `../docs`: a `data/release_notes.yml`
entry for the version (datasets/methods/scope/technology/zones), a hand-written interpretive note in
`data-sources.qmd` under "Notes on individual sources" (guard it with `if ("ax" %in% ds$ds_key)` in an
`output: asis` chunk so older books don't describe it), and the citation in `references.bib`. The
table/counts on that page come from the registry; the *why* does not.

## `sp_cat` by taxonomy — `merge_taxon.qmd`

Assigned from **WoRMS class/phylum/kingdom + BirdLife→bird**, NOT a v7 crosswalk (that dumped every
new taxon into a catch-all `other`). The rule is **`msens::sp_cat_from_taxonomy()`** (one test row
per branch); `merge_taxon.qmd` and `ingest_aquax.qmd` both call it — never copy the `case_when`. Rule: bird (botw|Aves), mammal (Mammalia), turtle (SWOT set),
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

## Metadata is per-RELEASE, and the docs read it that way

Every release publishes its own `dataset`/`taxon`/`metric` tables, and they disagree by design —
so a fact stated without a version is wrong for eight of nine releases:

| question | answered by | moves between releases |
|---|---|---|
| how many source datasets? | `dataset` where `is_scored` | v1 5 · v3 7 · v8 8 |
| which categories are scored? | `sp_cat` ∩ `extrisk_{cat}` in `metric` | v1 scores `reptile`; v3+ do not; v8 drops `other`, adds `primary_producer` |
| what is a valid species? | `is_ok` (v1–v7) / `is_valid_usa AND is_marine` (v8) | the DEFINITION changed at v3 and v7, so the count is not a trend |
| which models fed a taxon? | `taxon_model` | v1/v2 reconstructed from wide columns; v8 now publishes it |

`msens::sdm_cols(con)` resolves the column names per release (`is_ok`/`is_valid_usa`,
`mdl_seq`/`ms_merge_key`, `value`/`val`) by introspection — use it rather than branching on a version
string. The versioned documentation calls the same function for the same reason.
