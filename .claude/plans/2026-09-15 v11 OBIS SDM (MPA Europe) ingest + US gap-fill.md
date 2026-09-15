# v11 · OBIS species distribution models (`ob`) — the open replacement for AquaMaps 2.0

**Status:** PLAN (2026-09-15). v10 = national score framework (`.claude/plans_todo/2026-09-04 v10 plan.md`);
**v11 = OBIS SDMs.** Numbers below come from `explore_obis-sdm.qmd` (rendered `_output/explore_obis-sdm.html`;
gap list `data/obis_sdm_gap_species.csv`) unless marked *est.*

## Why (the constraint we are designing around)

AquaMaps 2.0 (the AquaX pipeline, ingested as `ax` in v9) will never be public. Gabriel Reygondeau,
2026-09-14: the raw grids belong to the FishBase / SeaLifeBase / Sea Around Us consortium; a *buffered
US-only* output is negotiable only against cost recovery; only a *new* product funded by a US grant would be
fully open. Ben's reply of the same day told him BOEM must prefer an equally credible, fully open alternative
if one exists. v9 keeps `ax` as delivered (display only, named "AquaMaps 2.0", request link to aquamaps.org).

OBIS's MPA Europe contribution is that alternative, and it is closer to drop-in than expected:

| property | OBIS / MPA Europe v2 (Aug 2025) | AquaMaps 2.0 (`ax`) | AquaMaps 1.0 (`am`) |
|---|---|---|---|
| licence | open, public S3 (`s3://obis-maps`, no sign-in) + STAC | closed; US-EEZ TIFs on loan, no download | CC-BY-NC, redistributed |
| grid | **exactly `global05`** (7200 × 3600, 0.05°, −180..180) | `global05`, masked to US | 0.5°, bilinear to `global05` |
| extent | whole globe | US study area | whole globe |
| fit data | global OBIS + GBIF, QC'd, spatially thinned | consortium occurrence DB | FishBase/SeaLifeBase envelopes |
| algorithms | Maxent + RF + XGBoost ensemble (median + sd); ESM for 10–29 pts | 10-algorithm ensemble | trapezoid envelope |
| per-model extras | CV metrics (CBI ≥ 0.3 gate), thresholds (p10, mtp, …), bootstrap CV, MESS/SHAPE, response curves, `log.json` | AUC, TSS, cutoff | none |
| range crop | **none** (7-band realm/hull/buffer mask only) | IUCN where available | none |
| species | 12,039 (European list), fitted globally | 10,536 US | 23,699 |

The realm mask is not a range: the walrus's `native_ecoregions` band admits 431k of the 634k US cells.
Our merge already masks suitability to the range dataset, so this is the same situation `am` was in
through v8 — and the v8 rule (`iucn_range_outside_us_eez` exclusion, am-only branch keyed on *global*
`has_range`) is what makes it safe.

## Numbers that size the work

**Walrus test** (AphiaID 137077; careful — `137209` in the CLAUDE.md examples is the *leatherback*):

| US cells | count |
|---|---|
| IUCN range | 100,312 |
| AquaMaps 1.0 (`am`) / of which outside IUCN | 137,464 / 54,322 |
| AquaMaps 2.0 (`ax`) / outside IUCN | 65,927 / 12,816 |
| OBIS ensemble > 0 (unmasked) | 502,164 |
| OBIS ≥ p10 (65.5) / of which inside IUCN | 51,952 / **48,079** |
| r on shared cells: OBIS~ax / OBIS~am / am~ax | **0.94** / 0.66 / 0.85 |
| Jaccard of OBIS≥p10 footprint vs am / vs ax | 0.37 / 0.44 |

OBIS ⊗ IUCN range ≈ AquaX; against AquaMaps 1.0 it is the same kind of improvement AquaX was.

**Coverage** (OBIS 2025-03-18 snapshot, species-rank records binned to `global05`, non-bird — birds are
BirdLife's and are keyed by BOTW id, so they were matched by name and excluded):

| set | species |
|---|---|
| any OBIS record in a US cell | 31,691 |
| ≥ 30 distinct US cells (standard-model floor) | 5,809 |
| 10–29 US cells (ESM floor) | 5,592 |
| ≥ 30 cells **and** already modelled by `ax` / am-only | 3,801 / 456 |
| ≥ 30 cells, **no suitability model at all** (the gap) | **1,552** (580 already have an MPA Europe SDM) |
| 10–29 cells, no suitability model | 2,938 |
| MPA Europe SDM species occurring in US waters / with ≥ 30 US cells | 5,884 / 2,070 |
| of those already covered by am or ax | 4,418 |
| `ax` taxa with ≥ 30 US cells (re-modellable openly) | 3,801 of 10,532 |

Gap composition (≥ 30 cells): polychaetes 314, malacostracans 297, gastropods 94, diatoms 90, octocorals 73,
teleosts 63, bivalves 55 … — i.e. the invertebrate/plankton tail AquaMaps never had; vertebrates are
already ~97 % covered (teleosts 1,779 with ≥ 30 cells: 1,683 `ax`, 33 am-only, 63 gap; mammals 59: 57 `ax`).
GBIF adds roughly half again to the record base (the speciesgrids OBIS+GBIF product, used only as a
counter, put 9,124 species at ≥ 30 hexes vs 5,809 here) — the framework uses both.

**Record floors** (`mpaeu_sdm/functions/model_species.R` `minptslim <- 30`; `model_species_esm.R` `10`;
`codes/model_fit.R` keeps a model when cross-validated continuous Boyce index ≥ 0.3). Their own list:
12,069 modelled; 10,789 "< 10 for ESM"; 1,683 "15–29, low for standard"; 1,542 "no good model"; 3,990 no data.
Published algorithm sets: 7,228 ens+maxent+rf+xgb · 1,373 ens+maxent+rf · 1,284 ESM · 788 maxent-only ·
645 ens+rf+xgb · 458 rf-only · 146 ens+maxent+xgb · 117 xgb-only.

**Scale.** An OBIS surface is ≥ 1 over most of the ocean (walrus 13.7 M of 17.1 M cells; leatherback
10.3 M): a ≥ 1 ingest like AquaMaps' is impossible and meaningless. At the model's own `mtp` (minimum
training presence) the walrus keeps 342k cells globally, the leatherback 7.9 M (a genuinely cosmopolitan
species); at `p10` 140k / 3.1 M. `am` today is 13.0 B rows / 50 GB for 23,699 models (548k rows/model);
`ax` 0.65 B / 3.9 GB (US-masked). S3 volume: ensemble/ESM current COGs 86 GB (10,676 files), masks 11 GB,
all current-scenario predictions 205 GB; the bucket is ~3.7 TB with 13 scenarios × bootstraps.

**Compute (for gap-filling with the same framework).** Walrus `log.json`: 150 min total, of which
~100 min is predicting 13 climate scenarios globally; fit ≈ 30 min for 1,257 points. Current-only,
US-relevant runs: *est.* 20–60 min/species → 1,552 standard + 2,938 ESM ≈ 1,500–3,000 CPU-h, i.e. days on a
32-core cloud box, not weeks. Inputs are all open: Bio-ORACLE v3 (partly in `raw/bio-oracle.org`), OBIS
(the server already mirrors `s3://obis-open-data/occurrence/`, ~99 GB, current to 2026-09), GBIF.

## Design decisions (to confirm with Tim; defaults in bold)

- **D1 dataset key `ob`** = MPA Europe `model=mpaeu` surfaces as published; `obus` = surfaces we (or OBIS)
  fit for US gap species with the same framework (P5). Same ingest code, two provenance rows in `dataset`.
  Species-app label "OBIS SDM (MPA Europe)" / "OBIS SDM (US fill)". Citation: the mpaeu_sdm repo + docs
  (Principe, Provoost, Appeltans; add to `references.bib`), and the data statement they ask for.
- **D2 value = ensemble median band (0–100) where an ensemble exists, else the single published
  algorithm** (the 1,363 single-model species), recorded in `model_ob.csv` (`method`, `cbi`, `p10`, `mtp`,
  `fit_n`, `bootcv` href). Store `sd` band href, do not ingest it.
- **D3 ingest threshold = `mtp`** (`OB_THRESHOLD=mtp|p10|1`), the model's own "no less suitable than the
  least suitable known occurrence". Cells below it are dropped from `dist/dataset=ob` the way AquaMaps' < 1
  are. `p10` is the reviewer alternative; measure both in P4.
- **D4 no download** — read each COG through `/vsicurl` with `msens::cells_from_aligned_raster()` (the
  pixel index IS `cell_id`; assert per file: 7200 × 3600, origin within 1e-4° of −180/90) and register the
  OBIS S3 href itself as the `native` asset (`on_grid: true`, "Delivered / As ingested"). `native/ob/` only
  holds the as-ingested (thresholded) COG that `publish_native` paints from the Parquet. The server's disk
  cannot take 86 GB of TIFs and does not need to.
- **D5 which species** — every `ob` taxon that either matches a v9 `worms` taxon, or has ≥ 1 OBIS record in
  a US cell (~6,900 incl. birds; skip `class == Aves`, BirdLife's). `OB_ALL=1` ingests all 12,039 (the
  extra ~5,000 European-only species cost storage, not correctness).
- **D6 precedence, not max.** Generalise `msens::supersede_sql()` from one (superseded, mask) pair to an
  ordered precedence per taxon: **`ob` > `ax` > `am`** over the *whole* extent for `ob` (its surface is
  global — the mask is the globe), `ax` > `am` inside `ax_mask` as in v9. Then
  `merge_sql(suit_ds = c("am","ax","ob"))`; a taxon still has at most one suitability per cell.
  `OB_SUPERSEDE=0` is the control run whose `merge_models` hash must equal v10's checkpoint.
  **`AX_INCLUDE=0`** produces the fully-open variant (ob + am + ranges) — that is the public product;
  the `ax`-in variant is the restricted preview for the agreement check. Both are v11 renders with the
  flag recorded in the manifest, not two versions.
- **D7 taxa with no range dataset** (most of the 1,552 gap species have no IUCN/FWS/NMFS range):
  an unclipped OBIS surface would paint the world. Derived range `rng_ob` = cells ≥ `p10` ∩
  `fit_region_max_depth` band (the fitted realms with a small buffer, limited to the species' observed
  depth) — written as a *range* dataset (value = `compute_er_score()`, never a magic number) so the
  existing has-range branch applies unchanged. Validate on the taxa that DO have an IUCN range: fraction
  of `rng_ob` cells outside IUCN (walrus: 7 %) per class; anything systematically leaky gets `buffer100m`
  intersected as well. Same family as v10-3's AquaMaps-derived ranges; reuse its notebook shape.
- **D8 v10 denominators** — `ob` is global, so the distribution-share denominator (species weight × global
  fraction) can use it directly for `ob` taxa; `taxon.n_global` from the global merged surface as today.

## Phases

**P0 · explore — DONE 2026-09-15.** `explore_obis-sdm.qmd` (OBIS records → `global05` counts, coverage vs
`am`/`ax`/MPA Europe, walrus test), `data/obis_sdm_gap_species.csv`, memory `project_v11_obis_sdm`. Repos
cloned to `~/Github/iobis/` (mpaeu_sdm, mpaeu_msdm = `obissdm`, mpaeu_docs, mpaeu_map_platform, …).
Still worth reading before P2: `mpaeu_docs/understanding.qmd` (post-processing they recommend),
`mpaeu_sdm/functions/components_model_species.R` `.cm_save_masks` (band semantics), the STAC item schema
(`stac/species-catalog/species-mpaeu/species-mpaeu-collection/taxonid=*/`).

**P1 · bootstrap v11** (`bootstrap-release` skill): `ver = v11`, `versions.csv` row
(`prerelease, restricted`), clone `dist/dataset=*` copy-on-write from v10, `BOOTSTRAP_VERIFY=1`.
`raw/obis-maps/` for the species list + `aws_files_list.zip` (already cached by P0).

**P2 · `ingest_obis-sdm.qmd`** (`ingest-sdm` skill; pattern `ingest_aquax.qmd`), `msens:` block with a full
`dataset:` entry (`ds_key: ob`, `on_grid: true`, `regions: global`). Steps: species table from
`species_files.parquet` + STAC (`fit_n`, methods, hrefs) ∩ D5 → `model_ob.csv` with native `worms_id`
(crosswalk short-circuits `match_taxa`); per model: pick band per D2, read thresholds parquet, `/vsicurl`
→ `cells_from_aligned_raster(scale = 1, threshold = mtp)` → Parquet; `furrr` workers `OB_WORKERS`;
resumable; `OB_TEST_N` smoke test writes nothing to `data/`. Persist `dist/ob_mask.parquet` = the globe
is trivial — instead persist per-model `n_cells_global`, `n_cells_usa`, `pct_usa` into `model_ob.csv`
(the v10 fallback fraction needs it). Manifest + content hash. Then `publish_native` registers the two
representations (D4).

**P3 · merge + tests (msens bump + NEWS).** `supersede_sql(precedence = c("ob","ax","am"), masks = …)`,
`merge_sql(suit_ds = c("am","ax","ob"))`, `data/ob_supersedes.csv` registry built from v10's
`taxon_model` by AphiaID (never by name); `rng_ob` derived-range writer (D7). **`test-merge.R` fixtures
before rendering:** (a) ob supersedes ax and am everywhere for a taxon that has all three; (b) ob-only taxon
with an IUCN range → masked; (c) ob-only taxon without a range → masked to `rng_ob`, and the union-with-
world case FAILS; (d) `AX_INCLUDE=0` drops ax and lets am carry on; (e) control run hash. Reinstall msens.

**P4 · what changed — measured, reviewable.** Extend `compare_aquax_examples.qmd` → `compare_obis_examples.qmd`
(walrus, humpback, cod, bluefin, red snapper, elkhorn, the two amphipods, plus 5 gap species): three
panels am | ax | ob-in-range, per-subregion means on shared cells. Systematic table in the ingest HTML like
`ax_vs_am_summary.csv`: per taxon × subregion r, Jaccard at `mtp`/`p10`, Δ mean, the 20 least/most
different vs `am` (improvement) and vs `ax` (agreement), each with a preview deep link. Gate:
`msens::pra_score_delta` v10 → v11 on common inputs, both variants (D6).

**P5 · US gap fill with the same framework (`obus`).** Two routes, not exclusive:
1. *Ask OBIS to run it.* Silas Principe / Pieter Provoost built `obissdm` to keep running after MPA
   Europe; a US species list (the 1,552 + 2,938 from P0, with GBIF the set grows) on their infrastructure
   is a modest batch and lands on the same bucket/STAC. This is the natural deliverable for the SDM
   workshop / open-data fund proposal (BOEM → Ocean Metrics window closes 30 Sep 2026) and a far better
   use of BOEM money than AquaMaps cost recovery — everything it buys is public.
2. *Run it ourselves.* `scripts/obus/` driver = `mpaeu_sdm`'s `p1–p4` with `outacro = "msus"`, the BOEM
   footprint as study area for the *species list only* (fits stay global as theirs do), current scenario
   only, on a rented 32–64-core box (not msens1: 16 GB RAM, disk 93 %). Output the same file layout, so P2
   ingests it unchanged with `ds_key: obus`. Budget *est.* 1,500–3,000 CPU-h + Bio-ORACLE v3 download.
   Keep their QC (`outqc_*`, distance-with-barriers) — the walrus's 47°S record shows what leaks without it.
Either way the ESM species (10–29 records) are reviewer-flagged: `model_obus.csv` carries `method = esm`
and the species app shows it.

**P6 · docs, apps, release.** `data-sources.qmd` note (guarded by `"ob" %in% ds$ds_key`), `release_notes.yml`
v11 entry, `references.bib`; species app: `on_grid` toggle already generic; the ER/inputs pill list picks
up `ob`/`obus`/`rng_ob` from the registry. Release restricted first (`PREVIEW_REVIEWERS_V11` incl. Tim
and, as courtesy, Silas/Pieter), `CHECK_PREVIEW`, then — for the `AX_INCLUDE=0` variant only — public.
**Before any public release audit the public bucket for `ax` bytes** (`native/ax*`, `dist/dataset=ax`,
`serve/model_cell` partitions of ax models): "display only" in the app is not the same as absent from
`s3://oceanmetrics.io-public`. That audit belongs in `release_marine-atlas.qmd` as an assertion, not a
checklist item.

## Open questions

1. Threshold for the stored surface: `mtp` (D3) vs `p10` vs continuous-inside-range-only. The walrus says
   `p10` is the better *range*, `mtp` the better *surface*; P4 decides on numbers.
2. Precedence `ob` > `ax` (D6): OBIS is open and reproducible; AquaX is a bigger ensemble on a curated
   DB. For the public product the question is moot (no `ax`); for the preview we should show both orders.
3. GBIF: MPA Europe used it; our counts here are OBIS-only. Using GBIF in `obus` fits is their default;
   whether to also count GBIF records in `taxon` metrics is a v10/v11 docs question.
4. Whether to ingest all 12,039 (`OB_ALL`) for the sake of a complete, citable mirror.
5. Contact OBIS (Silas Principe, s.principe@unesco.org; Pieter Provoost) before P2 finishes: version
   pinning (`model=mpaeu` v2 is Aug 2025 — will there be a v3?), their preferred citation, and P5 route 1.
   The Shiny app (`shiny.obis.org/distmaps`) was 502 on 2026-09-15; the S3 bucket and STAC were fine.

## Sources

- `s3://obis-maps/sdm/species/taxonid={AphiaID}/model=mpaeu/{predictions,metrics,models}/`,
  STAC `s3://obis-maps/sdm/stac/catalog.json`; species list
  `iobis/mpaeu_docs/files/sdm_list_iss_cons_20250903.csv`; file inventory `iobis/mpaeu_sdm/aws_files_list.zip`.
- Code: `~/Github/iobis/mpaeu_sdm` (pipeline, `functions/model_species*.R`, `sdm_conf.yml` variables per
  group), `mpaeu_msdm` (`obissdm` package), `mpaeu_map_platform` (`scripts/download_aws_model.R`, older
  bucket `mpaeu-dist`), `mpaeu_esdm` (presence-absence / abundance / integrated experiments).
- OBIS records: laptop `~/_big/obis.duckdb` (2025-03-18 export, 136 M rows); server
  `/share/data/obis/occurrence/` = `s3://obis-open-data/occurrence/` mirror (obisindicators' global store).
- Emails: Gabriel Reygondeau threads 2026-09-03 → 2026-09-14; Tim White 2026-09-10/11 brief.

## Appendix · AquaMaps 1.0 vs AquaX vs OBIS-SDM (2026-09-15)

Terms: **OBIS-SDM** = the modelled surfaces OBIS publishes (MPA Europe v2); **OBIS-obs / GBIF-obs** = the
occurrence records, which are both the model input and our coverage yardstick. AquaX = AquaMaps 2.0
(internal name). Birds stay BirdLife for now; eBird Status & Trends is the planned upgrade.

| criterion | AquaMaps 1.0 (`am`) | AquaX (`ax`) | OBIS-SDM (`ob`) |
|---|---|---|---|
| openness | CC-BY-NC download; redistributed | closed; US-EEZ TIFs on loan, display only | fully open: public S3 + STAC, code on GitHub |
| grid / extent | 0.5° global, bilinear to `global05` | `global05`, US study area only | `global05` exactly, whole globe |
| species (US-relevant) | 23,699 (12,868 taxa) | 10,536 | 12,039 (6,673 occur in US waters) |
| occurrence input | FishBase/SeaLifeBase + OBIS/GBIF via expert envelopes | AquaMaps + OBIS + GBIF, expert-verified, thinned 20–30 km | OBIS + GBIF, QC (`outqc_*`), spatially thinned |
| minimum records | envelope needs few (expert-adjusted) | not stated as a floor | 30 standard / 10 ESM |
| algorithm | trapezoid envelope on depth, temperature, salinity, PP, ice, land distance, O₂ | 10-algorithm ensemble (GLM, GAM, RF, ANN, FDA, CTA, GBM, Maxnet, XGBoost, MARS), TSS > 0.6 kept, mean + committee averaging + CV | Maxent + RF + XGBoost, CBI ≥ 0.3 kept, median ensemble + sd; ESM for 10–29 |
| predictors | HCAF cell means (Bio-ORACLE-derived SST/SBT, salinity, PP, ice, depth) | Bio-ORACLE v3 surface OR bottom layers | Bio-ORACLE v3; variable set by group (`sdm_conf.yml`): others = thetao_max, thetao_range, bathymetry, sws_max, so_min, o2_min (+rugosity/wavefetch); photosynthesizers add no3_min, par_min; mammals siconc_max; seabirds tas_mean, distcoast |
| vertical layer choice | per species `spp_prefs.layer` (`s` 21,696 / `b` 2,003) + `pelagic` flag — in `am.duckdb` | rule: pelagic → surface, demersal/benthic → bottom, from the D3OS habitat table (META file, **not in our delivery — request it**) | per species `hab_depth` in `log.json`: `depthsurf` (pelagic/default) or **`depthmean`** (benthic/demersal; water-column mean, NOT bottom `depthmax`); 300-log sample: 76 % depthmean; mode of life from WoRMS attributes, back-filled by genus/family majority |
| range constraint | none (v6+ MST applies IUCN at merge) | IUCN ERM + 1° buffer, ocean-basin mask; else occurrence-derived biogeographic provinces (PPOW/BPOW, 3.3 % rule) | none — realm/hull/buffer masks only; MST applies IUCN at merge, derived range for the rest |
| per-model extras | none | AUC, TSS, cutoff bands (CV layer offered) | cvmetrics, thresholds (p10, mtp, …), bootstrap sd, MESS/SHAPE, response curves, variable importance, fit log |
| scenarios | current | current (+ future in the framework) | current + 5 SSPs × 2 periods |
| walrus, US cells outside IUCN | 54k of 137k | 13k of 66k | 4k of 52k at p10 (500k unmasked) |
| discernible from data/APIs? | yes: `am.duckdb` `spp_prefs` | rule yes (paper); per-species only with META | yes: STAC item lacks it, `log.json` per species has it (7 KB; 12k × = 84 MB) |
