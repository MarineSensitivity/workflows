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

| Decision | Recommendation (= the v10 definition, page revision 12) |
|---|---|
| Headline table | v10 for cross-region reporting; ecoregional v9 retained for within-region use |
| Scope | all categories, one rule (a single national column beside six ecoregional ones mixes scales in the composite) |
| Denominator | global range; 27 national-only taxa get the R2 fallback (endemic → literature → OBIS fraction); study-area denominator shown only as a demonstration |
| Quantity | the score is the intensity (per km²); the total share (% of the world's importance) is the companion, printed beside every score |
| Reference point | 100 = the national average concentration (location quotient: area's share of the nation's importance ÷ its share of the nation's surface); no best-performer stretch; "top area = 100" is a display comparison only |
| Reporting floor | a component is reportable in a zone only if its scored cells cover ≥ 5 % of the zone's area (GEO turtles 1.5 % → not reportable) |
| Richness | no damping (α = 0); publish n species and mean dependence beside the score |
| Composite weights | equal, stated as policy, with a sensitivity appendix (species-count and risk-weight bases, stakeholder sets, Spearman vs equal) |
| Formula 9.4 | moot under v10 (no min–max); percentile endpoints kept on the page as a v9 variant only; print the Spearman between the v9 and v10 rankings |

(Earlier revisions recommended the brief's per-component top-area = 100 with the absolute percent beside it, and
"total as the headline, per km² beside it"; superseded 2026-09-04 pm when Ben asked that every recommendation be the
default and the page define v10 outright.)

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

## Addendum 2026-09-04 (afternoon): page revision 2

The interactive page now carries everything in this memo plus: a redesigned sandbox (vertical
transect, south→north, colour-coded species, boxed zones, scenario notes), a "reading the controls"
explainer for section 4 (denominator, total vs per-km², what the green dot is), findings split from
recommendations with references, a literature synthesis, section 7 with drop-in text for the docs
(scoring.qmd section + callout, science.qmd paragraph, metric rows, glossary entries, references.bib
entries, a Table 10-2 template, an apps-guide paragraph) and section 8 on how to show two scores in the
apps without showing six numbers.

Additional finding (from Ben's note on TSS): the brief's Step 1 "apply AquaX's TSS cutoff" would
binarize suitability. TSS is a skill statistic (sensitivity + specificity − 1, −1..+1, prevalence-
independent; Allouche et al. 2006); the per-species `cutoff` AquaX ships is the suitability threshold
that maximizes TSS (max-SSS, Liu et al. 2013), recorded in `model_ax.csv` and applied only under
`AX_APPLY_CUTOFF=1` (about half of a typical model's pixels fall below it). Inside a share it changes
numerator and denominator, discards the continuous information the share uses (Guillera-Arroita et al.
2015) and pushes each species toward its cell-count fraction (Kemp's ridley 46 % by suitability vs 13 %
by footprint). Recommendation R3: keep the surface continuous; the cutoff is at most a range-footprint
option applied to numerator and denominator together, reported as a sensitivity.

Denominator availability in v9: 2,113 species use the merged global surface, ~15,000 AquaMaps outside US
waters + merged inside; 27 valid species have only a national dataset (12 invertebrates + 3 fishes
AquaX-only; 12 FWS-range-only birds/fishes/mammals) and no global total. Proposed fallback order: merged
global → AquaMaps global → expert-range footprint (IUCN/BirdLife/SWOT cell counts, whole-range ingests)
→ literature fraction of global population/range in US waters (IUCN Red List range/EOO, BirdLife and
Partners in Flight estimates, NOAA stock assessments, NatureServe national-responsibility ranks) →
endemic assumption, flagged. `taxon.range_usa_km2 / range_km2` is 1.0 for every taxon (both are the US
footprint), so the range-based fraction must come from the global range datasets, not the taxon table.

Why keep the ecoregional score: inside one ecoregion Formula 9.4 is a linear transform of the raw cell
score, so its within-region ranking equals the raw density ranking, which is the right answer for siting
inside a region, for the cell maps and for continuity; the share ranks by irreplaceability and under-serves
dense habitat of wide-ranging species. Two labelled numbers, each for its question; never mixed in a
composite.

Docs: `docs/.github/workflows/quarto-publish.yaml` now also writes a `404.html` that forwards unversioned
paths (`/docs/scoring.html#…`) to `/docs/{latest}/…` and explains restricted versions with a link to the
preview host; the prune regex keeps it. Uncommitted, needs a push to take effect.

Page-building lesson: the artifact viewer showed mojibake ("minâ€"max", "kmÂ²") for Ben although Chrome
rendered it; the page is now generated as pure ASCII (entities in markup, \u escapes in JS/JSON, \XXXX
in CSS) by `build_page.py` in the scratchpad, with a charset meta at the top.

## Naming (2026-09-04): v9 = the published rule (ecoregional rescale), v10 = the proposed rule (distribution share), applied to the same v9 merged models. The page, the docs drop-ins and the email use that naming.

## Draft email to Tim

Subject: v10 (distribution share): review, real numbers, and an interactive page

Hi Tim,

I worked through your distribution-share brief against the v9 tables and put the whole review on one page
(private link; sign in with your claude.ai account or ask me to share it):
https://claude.ai/code/artifact/773b9b71-efa4-4d46-9a99-385415bc04e5

Since it advances the method rather than re-doing v9, the page calls the resulting rule v10 and implements it
as the default view; the v9 variants (percentile endpoints, a single national min–max) stay on the page for
comparison only.

Short version: the brief is right about the problem and right about the fix. Per-species normalization is
the standard move in spatial conservation planning (rarity-weighted richness / weighted endemism, and the
first step of Zonation), and on v9 it does what you expected: the two Gulf Program Areas hold 2.3 % of the
world's risk-weighted turtle importance, the six Alaska areas 0.7 %, Rice's whale tops the Gulf mammal
column, and the Arctic keeps its mammal columns.

Where v10 as defined on the page departs from the brief's steps, with the numbers:

1. Step 1 (the floor and the TSS cutoff) is not where the work is. The leatherback's mean merged value in
   the Aleutian Arc is 51/100, not the floor of 1; the suitability model times the Endangered weight is the
   source, and the global denominator is what handles it (2 % of the leatherback's range is in US waters).
   Applying AquaX's TSS cutoff would binarize the surface at the max-TSS threshold and move every species
   toward its footprint fraction; the page keeps the surface continuous.
2. The denominator is the global range, not the study area. A study-area denominator brings the vagrant
   problem back at species level (Aleutian Arc: 2.6 % of the US turtle share, 0.2 % of the global). For the
   27 species with only a national model the page applies a fallback in order: endemic, literature fraction,
   OBIS occurrence fraction.
3. The score is a concentration, and 100 is the national average rather than the top area. A total share
   grows with the size of the Program Area (on total share the Aleutian Arc rises to composite rank 3 and
   Cook Inlet falls to 20), so v10 divides an area's share of the nation's importance by its share of the
   nation's surface: 100 = a fair share per km²; GOA Program Area A 390, Aleutian Arc 19. That reference is
   the same for every area and component and does not move between releases; the "top area = 100" stretch of
   Step 5 is on the page only for comparison. The total, as a percent of the world's importance, is printed
   beside every score as the amount at stake.
4. Three settled details: a coverage floor (a component whose scored cells cover < 5 % of an area is not
   reportable; St. George Basin turtles, 1.5 %); equal component weights with a sensitivity appendix
   (species-count weighting would hand 92 % of the composite to invertebrates and fishes); no richness
   damping, because the score is n species × mean dependence and no exponent in [0, 1] removes richness on
   v9's data, so the two factors are reported instead.

The v9 ecoregional score stays for within-region use, labelled as such, and the re-issued table carries the
Spearman between the two rankings. The page also has a sandbox for intuition, the literature, drop-in text
for the docs, the Table 10-2 template and a proposal for how the apps show both. Happy to walk through it.

Ben


## Addendum: richness (page section 3, added 2026-09-04)

Correction to an earlier line on the page ("richness-neutral by construction"): v10 is not richness-neutral.
Each species is counted once nationally, split across its range (so a wide-ranging species can no longer
inflate every region it touches), but a zone's share is still a sum over the species present:
share_z,g = n_z,g × mean_s(w_s · share_s,z / 100), richness × mean dependence. A species-rich zone outscores
a species-poor one only if its many species depend on it enough (1,000 tropical fishes at 1 % = 20 Arctic
fishes at 50 %). On v9 fishes: GAA 1,455 species at a mean 0.10 % of global range (1.5 % of the category);
Chukchi 158 at 0.03 % (0.056 %); Aleutian Arc 600 at 0.15 % (0.96 %). Options if richness should count less:
mean share per species (OFWESA/CWE; rewards single endemics), core-area max (Zonation CAZ), damped richness
total ÷ n^α. Recommendation: keep the total as the headline, publish n and the mean beside it; equal
component weights already bound richness across categories. Page also gained a symbol definition list and a
"richness gradient" sandbox scenario with a decomposition table; the v10 question now reads "what fraction
of each species' whole range lies here?".


## Addendum: absolute scale and R2 fallbacks applied (page revision 5, 2026-09-04)

Two operations were being read as one. The DENOMINATOR (what each species' fraction is a fraction of: global
range, R2) is separate from brief Step 5, a cosmetic stretch so the top Program Area reads 100. The page no
longer says "rescaled nationally"; section 5 now has a Scale control: "% of the world (absolute)" (default;
pct_z,g = zone total ÷ category total weight, nothing stretched, 100 = the whole world's importance, the way KBA
and Ramsar thresholds read) and "top area = 100" (Step 5). In absolute scale the v9 and green dots are hidden
(0–100 relative numbers would put two scales on one axis); ranks stay in the label.

R2 fallback chain applied to the 27 valid species with only national datasets (all have zone_taxon rows and
merged US surfaces; 12 FWS-range-only taxa even have a "merged global" surface, which IS their US range, so the
fallback takes precedence over it):
- endemic (fraction 1): Hawaiian stilt, Gulf sturgeon, tidewater goby, saltmarsh topminnow, four Hawaiian
  anchialine shrimps, southern sea otter;
- literature: northern sea otter 0.9 (USFWS stock assessments), laughing gull 0.5 (real fix: BOTW name
  crosswalk Leucophaeus atricilla), Eskimo curlew 0.25 (presumed extinct, flagged);
- OBIS occurrence fraction (records in OBIS area 266 "United States: all" ÷ worldwide, floor 0.02, effort-biased,
  confidence flagged) for the 15 AquaX-only fishes and invertebrates.
Draft curation file: `workflows/data/us_share_fallback.csv` (uncommitted; columns taxon_id, scientific_name,
common_name, sp_cat, ds, fraction, method, basis, obis_total, obis_us). OBIS query pattern:
https://api.obis.org/v3/occurrence?taxonid={aphia}&size=0 and &areaid=266.
The recomputed numbers barely move (the 27 species are small contributors): hero and Spearman unchanged.


## Addendum: sliver rule and the map (page revision 7, 2026-09-04)

Ben's intent: the sliver rule is GEOMETRIC, the fraction of a Program Area covered by scored pixels of the
component, not the component's share of value. St. George Basin (GEO), sea turtles, v9: scored cells cover
1.5 % of the area (a strip along the southern boundary), 4 species, v9 score 0.7, 0.011 % of the nation's
turtle importance. Rule (page section 4, recommendation in 7b, docs drop-in with an equation): a component is
reported for a zone only if its scored cells cover >= kappa = 5 % of the zone's area (area-weighted fraction
of the zone's cells carrying a value for the category); below that it is "not reportable": no data on maps,
footnoted, excluded from the composite like a category with no cells, never a zero. Defined on the footprint,
so identical for v9 and v10. On v9 it removes exactly one pair (GEO turtles); the next-lowest coverage is
18 % (primary producers, Northern California; nearshore category, 18-49 % across the Pacific areas), then High
Arctic corals at 40 %, so any kappa in 2-18 % gives the same result; 5 % keeps a wide margin below the
nearshore categories. A share floor tau (importance, not presence) is offered as an optional complement, off
by default, because at any useful setting it also removes low-share Arctic corals and primary producers.

Section 3 now has a choropleth (Program Areas, shifted longitude, study-area outline) with metric (composite or
component), layer (v9 | v10 at alpha, top area = 100 | delta) and a damped-richness slider alpha in [0,1]
(zone total / n^alpha before the top-area rescale; alpha = 1 is mean share per species). Geometry: v8 Program
Area gpkg simplified with rmapshaper (keep 3 %), embedded as GeoJSON; the 27-species fallback table moved to
full width; ecoregion names corrected (HAR = High Arctic, EBS = East Bering Sea, CBS = Chukchi and Beaufort Seas).


## Addendum: optimal alpha? (page revision 8, 2026-09-04)

Ben asked whether the damped-richness exponent alpha (v10 total / n^alpha, 0 = total, 1 = mean share per
species) has an optimum that balances importance across the study area. Answer on the page (section 3
panel "Is there an optimal alpha?"): no ecological optimum exists in the literature; alpha is an order
parameter like Hill's q (Hill 1973; Jost 2006; Chao et al. 2014: report the profile) or Zonation's
ABF-vs-CAZ choice (Moilanen 2007); the ends have arguments (alpha = 0: representation efficiency, Williams
1996 / Albuquerque & Beier 2015; alpha = 1: endemism independent of richness, Crisp 2001 / Laffan & Crisp
2003). The one statistical anchor, the exponent beta that decouples the score from species count (slope of
log total on log species present across the 20 PAs; the richness-corrected endemism idea, Jetz et al. 2004),
is 1.4-1.8 for every v9 category with r2 >= 0.3 (turtles 5.7 on 11 points, r2 0.17, unreadable): species-rich
Program Areas also hold LARGER mean fractions of their species, so richness and dependence reinforce and no
alpha in [0,1] removes the richness gradient; alpha = 1 is merely the least richness-driven. alpha profile of
the composite vs v9: rho 0.18 (alpha 0), 0.15, 0.19, 0.22, 0.28 (alpha 1); top three stable (GAA, SOC, then
ALA/GOA). Recommendation: publish alpha = 0, report beta per category as a diagnostic and print the profile
(alpha 0, 1/2, 1) in the report; alpha = 1/2 is a convention if a single compromise is demanded. Added refs:
Hill 1973, Jost 2006, Chao 2014, Jetz 2004, Kier 2009 (endemism richness).


## Addendum: reference points (page revision 10, 2026-09-04)

Ben asked whether v10 yields at least one Program Area at 100 per component or one max across all components.
Answer (new page section 3 "Reference points"): brief Step 5 takes the maximum PER COMPONENT over the reporting
units, so every component has exactly one Program Area at 100 (the one holding the largest share of the
nation's importance for that component: sea turtles GAA, etc.), and the composite (equal-weight mean of
component scores) reaches 100 only if one area tops every component; on v9 the highest v10 composite is GAA at
~75 (page value 74.8). Hero tiles now show the per-component rescaled v10 (GAA 100, ALA 11.4) with the absolute % beneath.
Four reference options tabulated on the turtle example (v9 best cell in ecoregion; v10 per-component max; one
max across all components (not recommended: drops equal component weighting); absolute % of the world; fixed
policy reference 100 = 1 % of the world's importance, uncapped). Recommendation, literature-backed: keep the
per-component max for the re-issued table (OHI "best performer" reference point, Halpern 2012 / Samhouri 2012;
preserves equal weights) with the absolute % beside every value; for the standing product from v10 onward use
the fixed 1 % reference ("distance to a reference" normalization preferred to min-max when an external benchmark
exists, OECD/JRC 2008; fixed target preferred to spatial comparison, Samhouri 2012; OFWESA's hypothetical maximum
"independent of the sensitivity of other regions", Morandi 2018; 1 % = Ramsar Criterion 6 / KBA D1, 0.5 % A1
for CR/EN). Added Samhouri et al. 2012 (Ecosphere 3:art41) to the literature. Sections renumbered 1-11.


## Addendum: area independence and component weights (page revision 11, 2026-09-04)

Area independence (section 3 addition): "absolute" on the page meant unstretched, not area-free. The total
share is extensive (grows with Program Area size); the area-independent form is the share per km2 (mean cell
share), the like-for-like successor of v9's area-weighted mean and the form of the offshore-wind sensitivity
maps. Refined recommendation: headline sensitivity score = intensity (per km2), rescaled per component for the
re-issued table and anchored for the standing product to the national average concentration, i.e. the location
quotient LQ = (area's share of the nation's category importance) / (area's share of the study-area surface),
1 = fair share (Isserman 1977). v9 turtles: GAA 2.97, GAB 2.38, SOC 2.01, ALA 0.39, GEO ~0 (per-km2 rescaled:
GAA 100, GAB 60, ALA 5). Total share % of the world printed beside it as the amount at stake. Hero still shows
the total (as the brief does).

Component weights (new section 4, interactive flower plot: PA picker, v9/v10 scores, weight base equal / species
count / total risk weight / custom sliders, dial gamma with W_g = (base_g / geometric mean)^gamma, primprod fixed
at 1; table of all 20 composites with ranks and Spearman vs equal weights). Facts: species count would hand
~92 % of the composite to invertebrates + fishes and 0.04 % to turtles; total risk weight favours fishes /
invertebrates / corals and nearly erases primary producers (sum w 3.3). Recommendation: equal weights published,
with a sensitivity appendix (gamma = 1 on both bases + regulatory and biodiversity stakeholder sets, Spearman vs
equal), as OHI reported extractive vs preservationist weightings (Halpern 2012). Literature: OECD/JRC 2008
(weights follow the framework), Burgass et al. 2017 (weighting = dominant structural uncertainty), McRae et al.
2017 (LPI diversity weighting = the species-count precedent, which here would double-count richness),
Lehtomaki & Moilanen 2013 (group-normalized weights), Morandi 2018 (OFWESA equal groups), Isserman 1977 (LQ).
Sections renumbered 1-12.

## Addendum: v10 as recommended is the default (page revision 12, 2026-09-04)

Ben: "This document IS the brief … Implement ALL your recommendations as the default view in this document for
v10. Do NOT show level-up versions of v9" (then: v9 variants may stay for demonstration / interactivity). v10 is now
DEFINED on the page (section 2, formulas v10·1–4 + companion + composite; symbol list):

    share_s,c  = v_s,c A_c / Σ_c' v_s,c' A_c'        (denominator = the species' global range; R2 fallback)
    share_c,g  = Σ_{s∈g} w_s,c share_s,c / 100
    share_z,g  = Σ_c p_zc share_c,g                   (pct_z,g = 100 share_z,g / Σ w_s/100 = % of the world at stake)
    v10_z,g    = 100 · (share_z,g / share_USA,g) / (A_z / A_USA)    location quotient; 100 = national average
    composite  = equal-weight mean of the reportable components (coverage ≥ κ = 5 %); primprod_v10 = zone mean
                 VGPM / national mean × 100; α = 0.

v9 turtles under v10: GAA 390 (3.9× the national average, 2.0 % of the world), GAB 235, SOC 124, NOC 100, CEC 111,
GOA 45, ALA 19 (0.19×, 0.23 %), GEO not reportable (coverage 1.5 %); highest composite GAA 242. The hero tiles now
read 390 / 19 (they read 100 / 11.4 under the top-area stretch of revision 10).

What defaults to v10 on the page: hero tiles; section 3 (alternatives table: v10 | total % of world | top area =
100 | v9; per-component "most concentrated" table); section 4 flower (dashed ring = 100 = national average);
section 5 map layer; section 7 sandbox (v10 column = LQ with equal-area zones, 100 = 6/N; columns reordered v9,
v9 variant, v10, v10 variant, mean share); section 8 explorer (defaults global / per km² / 100 = national
average; Reference seg also offers top area = 100 and absolute; bar mode with ranks; table columns v9 published |
v9 p1–p99 | v9 nat. min–max | v10 | % of world | rank Δ); section 9 findings/recommendations rewritten (reference
point, intensity, Spearman, next steps: `msens::share_sql(ver, category)` implementing v10·1–4 with the coverage
floor); section 11 docs drop-ins (eq-v10 added, eq-share-national removed, metric rows `extrisk_{g}_share`,
`_share_zone` + `_pct_world`, `_v10`, `_coverage`, `primprod_v10`, `score_v10_equalweights`; Table 10-2 template
with v10 | % at stake | v9 rank | Δ; apps guide text); section 12 apps card (v10 score 390, at stake 2.0 %, sentence
generated from the data); footer states the v10 computation. Page engineering: shared `V10` model object +
`v10base/v10rows/v10all/v10composite` at the top of the script feed every section, so one definition drives all views.
