# Review of the distribution-share method brief (T. White, 2026-09-03) — with literature

*B. Best, 2026-09-04. Companion page (interactive): see the artifact link at the end. All numbers below are
computed from the v9 prerelease (`~/_big/msens/derived/v9/sdm.duckdb`, `marine-atlas/dist_merged_global`,
`dist/dataset=am`); scripts in the session scratchpad (`build_v9_json.R`, `global_totals.sql`, `am_totals.R`).*

## Verdict

The brief is right about the problem and right about the shape of the fix. Formula 9.4 rescales inside each
BOEM Ecoregion, so every ecoregion has a cell scoring 100 and a Program-Area number only says how close the area
sits to *its own* region's best. Table 10-2 compares those numbers across ecoregions, which the docs say not to do.
Per-species normalization ("what fraction of this species is here", summed with a risk weight and rescaled once,
nationally) is the standard move in spatial conservation planning and has a name: **rarity-weighted richness /
weighted endemism** (Williams et al. 1996; Crisp et al. 2001; Albuquerque & Beier 2015), and it is the per-feature
normalization Zonation applies to every layer (Moilanen et al. 2005). Build it. Six things need a change or a
decision first.

## What I checked

- Reproduced the published v9 Program-Area scores from `cell_metric` at cell level (ecoregional min–max,
  coverage-weighted): r = 0.9997, max |Δ| 2.5; the composite reproduces exactly (pipeline rule = plain mean of the
  components present in the zone).
- Computed the share metric for every Program Area × category from `zone_taxon` (per-species suitability-area in
  each PA and in the USA subregion) with two denominators: the **US study area** and the **global range** (merged
  global surfaces for range-constrained taxa; AquaMaps outside US waters + the merged US surface for the ~15,000
  AquaMaps-only taxa). Turtles and the 16 NMFS DPS taxa enter as their risk-weighted surfaces (w = 1); all others
  divide the scalar `er_score` back out.
- Computed the alternatives the brief mentions or implies: national min–max of the raw score (cell level), ecoregional
  rescale with 1st/99th-percentile endpoints (cell level), mean share per species present (OFWESA / corrected
  weighted endemism), and share **per km²**.

## Findings on the brief

1. **Step 1 (strip the floor) will not "do most of the work".** The floor of 1 is not what paints Alaska. In v9 the
   leatherback's mean merged value in the Aleutian Arc is 51/100 (SWOT-range Endangered weight × suitability), the
   loggerhead's 26. The Bering Sea's raw turtle ceiling (max cell 140) is a third of the Eastern Gulf's (454); Formula
   9.4 then makes both 100. The denominator removes the illusion; the floor strip only removes noise.
2. **Step 4 is not a percent yet.** Σ over cells of Σ_s w_s·share_s,c/100 sums to Σ_s w_s/100 per category, not to 1,
   and Program Areas do not tile the study area. Divide by Σ_s w_s/100 to report "% of the nation's category-g
   importance"; Step 5's max-rescale is unaffected.
3. **The denominator scope (decision 3) is the real decision, and the answer is global.** With a study-area
   denominator a species whose only US cells are a sliver has share 1.0 in that sliver: the vagrant problem returns at
   species level (the leatherback has 2 % of its modelled range in US waters, most of it Alaska; under the US
   denominator the Aleutian Arc holds 2.6 % of the national turtle share, under the global one 0.23 %). The global
   denominator is what the brief's own concept paragraph promises, and it matches how site-designation thresholds are
   written (Ramsar 1 %, KBA A1/B1/D1 fractions of the *global* population). It imports a "national responsibility"
   weighting (Schmeller et al. 2008, 2014): species for which the US is peripheral count less. Say so in the metric
   description; carry the US-share as a second column.
4. **A share is a total, so area counts.** Steps 2–5 sum fractions over a zone's cells: the score is *extensive*. The
   published score is an area-weighted *mean* (an intensity). On v9 the Aleutian Arc (873,000 km²) moves from composite
   rank 9 to 3 under the total share, Cook Inlet (21,700 km²) from 2 to 20, Hope Basin from 1 to 18. Both readings are
   defensible ("how much is at stake" vs "how concentrated"); the per-km² variant correlates better with the published
   ranking (Spearman 0.62 vs 0.25). Report which one the table uses; consider carrying both.
5. **Say what the metric rewards.** Share is endemism-weighted and blind to hotspots of wide-ranging species: humpback
   critical habitat contributes little because the humpback's range is enormous; a Cook Inlet beluga or Rice's whale
   dominates its area. That is irreplaceability (Kukkala & Moilanen 2013) and it is the right quantity for a national
   comparison, but it is a different map from the ecoregional one (Orme et al. 2005: richness, endemism and threat
   hotspots do not coincide). Keep both, labelled "relative within ecoregion" and "share of range" — the brief's
   run-alongside recommendation is right. If single endemics are still under-served, a core-area (max over species)
   variant is the principled alternative (Moilanen 2007).
6. **Equal category weights amplify small components.** Six turtles and one Gulf endemic (Kemp's ridley, 46 % of its
   global range in US waters) move the composite as much as 9,400 invertebrates. That is an OHI-style policy choice
   (Halpern et al. 2012), fine if stated; report the composite's sensitivity to a Σ-weight or species-count weighting.
7. **Adopt the two standing v1 recommendations now.** Percentile endpoints alone change several published numbers
   (Gulf of Alaska turtles 67.6 → 73.5; Aleutian Arc 50.0 → 52.4) while keeping the ranking (Spearman 0.90 on the
   composite). Put the Spearman between published and re-issued rankings in the re-issued table.
8. **The validation targets pass on v9 with the global denominator.** Alaska Program Areas together hold 0.7 % of the
   nation's turtle importance, the two Gulf areas 2.3 % (55 % of the share held by any Program Area); GAA is the
   national 100. Rice's whale is the top mammal contributor in GAA and GAB. The Arctic keeps its mammal columns through
   bowhead, walrus and the ice seals (Chukchi Sea mammal share rescaled 42 / 61 per km²). Under the US denominator
   Alaska holds 6.9 % and the Gulf 11.5 %, and the Gulf of Alaska turtle share (2.7 %) exceeds 1 %.
9. **Step 6 (RMU population weighting)** only after the denominator is settled: with a global denominator the North
   Pacific loggerhead and West Pacific leatherback are already small fractions in Alaska; a second down-weight on the
   same species needs an abundance argument, not a geographic one.

## Decisions (recommendation)

| Decision | Recommendation |
|---|---|
| Headline table | share-based for cross-region reporting; ecoregional retained for within-region use |
| Scope | all categories, one rule (a single national column beside six ecoregional ones mixes scales in the composite) |
| Denominator | global range (headline), study-area share as a second column, stated in the metric description |
| Quantity | state whether the table reports total share or share per km²; consider carrying both |
| Composite weights | equal, stated as policy, with the sensitivity reported |
| Formula 9.4 | 1st/99th-percentile endpoints, plus the rank correlation |

## Real v9 numbers (global denominator, total share)

Turtles — published (ecoregional) vs share:

| PA | published | share % of nation | share, nat. rescaled | per km², rescaled |
|---|---:|---:|---:|---:|
| GOA Program Area A | 78.5 | 2.05 | 100 | 100 |
| Gulf of Alaska | 67.6 | 0.24 | 11.6 | 11.5 |
| Central California | 61.5 | 0.22 | 10.8 | 28.4 |
| Kodiak | 61.3 | 0.12 | 5.9 | 8.2 |
| Shumagin | 60.1 | 0.11 | 5.5 | 7.4 |
| Southern California | 58.3 | 0.47 | 22.9 | 31.6 |
| Northern California | 55.7 | 0.23 | 11.5 | 25.5 |
| Aleutian Arc | 50.0 | 0.23 | 11.4 | 5.0 |
| GOA Program Area B | 45.6 | 0.26 | 12.9 | 60.3 |
| Cook Inlet | 34.1 | 0.01 | 0.4 | 7.2 |
| St. George Basin | 0.7 | 0.00 | 0.0 | 0.0 |

Nine Arctic/Bering Program Areas have no turtle cells (the pipeline composite averages seven components there).

Composite rank, published → total share (global) → share per km² (global): Hope Basin 1 → 18 → (see page);
Cook Inlet 2 → 20; GAA 3 → 1; Southern California 7 → 2; Aleutian Arc 9 → 3; Chukchi Sea 5 → 10.
Spearman with the published composite: total share/global 0.25, total share/US 0.28, share per km²/global 0.62,
national min–max 0.34, ecoregional with percentile endpoints 0.90, mean share per species 0.22.

Species scored per category (study area): invertebrates 9,411 · fish 6,283 · corals 783 · primary producers 319 ·
birds 228 · mammals 75 · turtles 6. US share of each turtle's modelled global range: Kemp's ridley 46 %, green 20 %,
loggerhead 19 %, hawksbill 9 %, olive ridley 9 %, leatherback 2 %.

## Literature (what the method is called elsewhere, and what it warns about)

**A. Range-fraction ("rarity-weighted") richness.** Williams et al. 1996 (*Cons. Biol.* 10:155) — range-size rarity
= Σ 1/range, the binary case of the share; rarity hotspots covered 98 % of British birds vs 89 % for richness hotspots.
Crisp et al. 2001 (*J. Biogeogr.* 28:183) — weighted endemism = "summing the proportion of species' ranges found in a
given area" (Steps 2–3 exactly); corrected weighted endemism (÷ richness) is the mean-share variant. Laffan & Crisp
2003 (*J. Biogeogr.* 30:511) — range-fraction metrics depend on the extent over which ranges are measured (the
denominator decision is a known sensitivity). Albuquerque & Beier 2015 (*PLoS ONE* 10:e0119905) — ranking by RWR was
on average 4 % more efficient than Zonation for representing all species; a defensible complementarity surrogate.
Veach et al. 2017 (*Div. Distrib.* 23:715) — richness-based priorities cluster and leave species uncovered; weighted
range-size rarity does not (the argument against "just rescale nationally"). Astudillo-Scalia & Albuquerque 2020
(*Biodiv. Conserv.* 29:1725) — same for the general case. Orme et al. 2005 (*Nature* 436:1016) — richness, endemism
and threat hotspots barely overlap: the two MST maps are different maps and should be labelled as such.

**B. Per-feature normalization in systematic conservation planning.** Moilanen et al. 2005 (*Proc. R. Soc. B*
272:1885) — Zonation expresses every cell as the proportion of each feature's total distribution; species counts
cancel by construction; weights multiply proportions. Moilanen 2007 (*Biol. Conserv.* 134:571) — additive benefit
function (a sum, like the brief) favours feature-rich cells; core-area Zonation (max over species) favours rarity.
Lehtomäki & Moilanen 2013 (*Env. Model. Softw.* 47:128) — ABF "may allow lowered representation for features occurring
in species-poor parts of the landscape" (the Arctic risk). Kukkala & Moilanen 2013 (*Biol. Rev.* 88:443) —
irreplaceability vs representation vs complementarity.

**C. Population-share thresholds in site designation.** Ramsar Criterion 6 — 1 % of the individuals in a population
of a waterbird. IUCN 2016 KBA Standard — A1 ≥ 0.5 % (CR/EN) / ≥ 1 % (VU) of the global population, B1 ≥ 10 %,
D1 ≥ 1 % in aggregation. Donald et al. 2019 (*Bird Cons. Int.* 29:177) and Lascelles et al. 2016 (*Div. Distrib.*) —
marine IBAs apply the 1 % criterion to at-sea distributions from tracking. IMMA criteria — vulnerability,
distribution/abundance, life cycle.

**D. National responsibility.** Schmeller et al. 2008 (*Cons. Biol.* 22:1273) — recommends a scalable index of
proportional distribution; Schmeller et al. 2014 (*J. Nat. Conserv.* 22:349) — responsibility depends on the reference
area (region vs globe), state it. Lesica & Allendorf 1995 (*Cons. Biol.* 9:753) — the counter-argument: peripheral
*populations* can be valuable; a vagrant is not a population; the DPS surfaces already carry that distinction.

**E. Offshore-energy sensitivity indices.** Garthe & Hüppop 2004 (*J. Appl. Ecol.* 41:724) and Furness et al. 2013
(*J. Env. Mgmt.* 119:56) — conservation importance = status + proportion of the biogeographic population in the
area. Bradbury et al. 2014 (*PLoS ONE* 9:e106366) — SeaMaST rescales across the whole English territorial sea.
Adams et al. 2016 (USGS OFR 2016-1154) / Kelsey et al. 2018 (*J. Env. Mgmt.* 227:229) — BOEM-funded Pacific OCS
index; population vulnerability includes the share of the population in the study area. Niedoroda et al. 2014
(RESA) — "compared study areas to each other". Morandi et al. 2018 (OCS Study BOEM 2018-031, OFWESA) — normalized
to hypothetical min/max "so that the results would be independent of the sensitivity of other regions", and
species-group sensitivity = mean over species evaluated: BOEM's own precedent for a fixed national scale and for
neutralizing species counts.

**F. Composite indicators.** OECD/JRC 2008 Handbook — min–max is outlier-sensitive (percentile endpoints); additive
equal weights are fully compensatory; run the sensitivity analysis. Halpern et al. 2012 (*Nature* 488:615, OHI) —
comparable across regions because the reference points are. Halpern et al. 2008 (*Science* 319:948) and 2009
(*Cons. Lett.* 2:138) — regional vs global rescaling of the same layers (R² 0.92, different absolute scale).
Stock & Micheli 2016 (*GEB* 25:1321) — transformation type and sum-vs-mean aggregation among the largest
uncertainties in cumulative-impact maps.

**G. Turtles in Alaska.** Hodge & Wing 2000 (*Herp. Rev.* 31:148) — four species in Alaska waters 1960–1998,
leatherback most often, a few animals per decade. Wallace et al. 2023 (*ESR* 52:209) — RMU 2.0 polygons for Step 6.

**H. Legal frame.** OCSLA 43 U.S.C. § 1344(a)(2)(G): "the relative environmental sensitivity and marine productivity
of different areas of the outer Continental Shelf"; (a)(3) "a proper balance". The statute asks for a cross-area
comparison.

## Implementation notes (msens conventions)

- `msens::share_sql(ver, category, denominator = c("global", "usa"), quantity = c("total", "per_km2"))` beside
  `merge_sql()`/`turtle_sql()`/`dps_sql()`; the notebook calls it, `test-share.R` asserts it.
- Fixtures: endemic → share 1; 0.5 % of range in a zone → 0.005; floored turtle cell → 0 after the floor strip;
  Σ_c share = 1 ± 1e-9 per species; a species with 1 % of its range in the study area → study-area share 1 but global
  share 0.01 (the sliver case); two zones of different area with the same intensity → equal per-km² share, unequal total.
- Denominators exist: `dist_merged_global` for range-constrained taxa; for AquaMaps-only taxa the merged global
  surface *is* the AquaMaps footprint (`dist/dataset=am`) — AquaX-superseded taxa need AquaMaps outside the ax mask
  plus the merged surface inside it (this page's approximation; make it exact in SQL).
- Metrics: `extrisk_{g}_share`, `_share_pct`, `_share_national_rescaled` (+ `_per_km2` variants) in `metric`; the
  share composite uses primary productivity rescaled nationally (min–max or percentile).
- Existing ecoregional rows stay byte-identical; version bump + `NEWS.md`; the re-issued table carries both numbers and
  the Spearman.
- Percentile endpoints: `quantile_cont(val, 0.01/0.99)` per ecoregion in `score_cell_metrics`, clamped.

## Interactive page

https://claude.ai/code/artifact/773b9b71-efa4-4d46-9a99-385415bc04e5 — *Sensitivity Across Regions*: the grading-curve chart, the two arithmetics, a 24-cell sandbox with presets, the v9 Program-Area explorer (category tabs, global/US denominator, total vs per-km², species contributions), the reading of the brief, and the annotated literature.
