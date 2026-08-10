---
title: "Response to Review Comments — BOEM Marine Sensitivity Toolkit, 2025 Final Report"
subtitle: "Reviewed draft: BOEM-MarineSensitivityToolkit_2025-final-report_v5_MKR.docx (25 comments, Michael K. Rasser, 22–27 July 2026)"
author: "Ben Best, EcoQuants LLC"
date: "2026-07-28"
---

# How to read this

Comments are in document order. Each entry gives the comment (abbreviated), our **response**, the
**edit** made or proposed, and a **status**:

- **Edited** — text is in the revised draft (`..._v6_BB.docx`) as a tracked change; accept or reject in Word.
- **Proposed** — text is drafted here and in the revised draft, but depends on a decision or on analysis not yet run.
- **Production** — a figure/formatting/app task, no body-text change.
- **Needs BOEM input** — we need an answer before editing.

Four comments (C42 rescaling, C76 Arctic productivity, C142 species selection, C48 presence values)
surface substantive issues rather than presentation ones. Two of them — **C42** and **C76** — change
numbers, not just wording, and we flag them as such. Rather than scatter the caveats, we propose a new
**Limitations and Known Issues** subsection at the end of Methods that collects them, which is also
where a reviewer will look for them.

---

# 1. Background and framing

## C19 — "Do we need to add MMA? Not sure how draft study reports are being handled." (Background, agency lineage paragraph)

**Response.** MMA = **Marine Minerals Administration**, the successor agency to BOEM — confirmed from
Tim White's signature block and the MMA logo circulated 16 July 2026. So this is a naming question, not
a scope question.

We have extended the agency-lineage sentence (BLM → MMS → BOEMRE → BOEM) to MMA and noted that the work
reported here was performed under BOEM:

> That lineage now continues with the Marine Minerals Administration (MMA), the successor agency to
> BOEM. Because the work reported here was performed under BOEM, the agency is referred to as BOEM
> throughout this report.

The larger question is the one the reviewer flags — how in-flight draft study reports are being handled.
Either the report is rebranded to MMA throughout (title page, headers, ~200 in-text references, the ESP
boilerplate and the DOI/BOEM mission statements at the back matter), or it stays as BOEM with MMA noted
as successor. That is BOEM/MMA's call; the global rename is mechanical if wanted.

**Status.** Edited (lineage sentence). Naming convention needs BOEM/MMA direction.

## C47 — "I really appreciate this cross check. There is a story about Walruses in the Gulf of Mexico I can tell you." (Spatial Validation)

**Response.** We would genuinely like to hear it, and not only for entertainment — that class of error
(an environmental-envelope model predicting suitable habitat in an ocean basin the species has never
occupied) is the single most common failure mode in a global SDM archive, and the anecdotes are how we
find the cases worth encoding as tests. The Pacific Walrus case is now handled structurally rather than
by hand: AquaMaps distributions are constrained to the IUCN Red List global range polygon where one
exists, and the constraint is implemented as a tested function in the project's R package with a
fixture asserting the expected output, so it cannot silently regress in a future release. If you can
send us the Gulf walrus case we will add it as a named regression test.

**Status.** No edit needed.

## C61 — "I like the Flower Plots, great way to represent the data." (Visualization)

**Response.** Thank you. Two things worth knowing: the flower plots are interactive in the app (click any
Planning Area to open its plot, hover a petal for the component score), and each is available as a
vector PDF, so they can be dropped into briefing slides or a PEIS at any size without pixelation. If
useful for the 11th National Program documents, we can produce them in a BOEM-branded theme.

**Status.** No edit needed.

---

# 2. Methods — conceptual framework

## C25 — "This methods section could use more detail." (Methods heading)

**Response.** Agreed, and most of this review is that comment in specific form. The revision adds: the
definitions of exposure/sensitivity/adaptive capacity (C26); where adaptive capacity actually sits in
the model, and the admission that it is folded into the weights rather than parameterized (C27); the
provenance of the extinction-risk weights (C28, C37); citations for every input dataset (C32, C34, C49);
the rationale and the cost of ecoregional rescaling (C42, C52); why the grid is geographic and how area
distortion is handled instead (C43); the provenance of the presence values (C48); and the annualization
caveat behind the Arctic productivity results (C76).

Beyond that, the complete computational methods — every parameter, every masking rule, every threshold
— exist as executed, rendered notebooks rather than prose, which is what makes the study reproducible.
Per your comment on the documentation site (C143) we propose to bring that content into the report as an
appendix rather than leaving it as an external link.

**Status.** Edited (see individual entries).

## C26 — "It might be good to provide brief definitions of these terms (E, S, V) since these are foundational to the follow on equation." (Methods, V = f(E,S,A))

**Edit.** Insert immediately after the equation:

> where:
>
> - **Exposure (E)** is the degree to which a place, and the species in it, overlaps in space and time
>   with an offshore energy activity and its stressors — a spill trajectory, a lease-block footprint, a
>   construction noise field. Exposure is specific to the activity being evaluated and is supplied by
>   the decision at hand; the MST provides the ecological surface that an exposure layer is intersected
>   with.
> - **Sensitivity (S)** is the degree to which the species present would be harmed by that exposure.
> - **Adaptive capacity (A)** is the ability of those species, and of the system they belong to, to
>   absorb, recover from, or compensate for the harm.
>
> This decomposition is the standard framing in ecological risk and climate-vulnerability assessment
> (IPCC 2014; Halpern et al. 2007; Hare et al. 2016).

**Status.** Edited.

## C27 — "This makes total sense. However, it is not clear to me how adaptive capacity is involved?" (Methods, v_c equation)

**Response.** It is not, as an independent term — and that is worth stating plainly in the report rather
than leaving the reader to notice the gap.

What v1 does is resolve S and A jointly through the extinction-risk weight and leave E to the specific
decision. The justification is that an IUCN or ESA category is not a sensitivity score alone: the
listing criteria assess population size, rate and direction of decline, range restriction, and
fragmentation — the same properties that determine whether a population can absorb an added impact and
rebound from it. A Critically Endangered species is, by construction, both more sensitive to an added
stressor and less able to recover from it than a Least Concern one. So A is present, but as an implicit
component of w_s rather than as its own multiplicand.

That is a real simplification and the largest structural limitation of v1. Two consequences follow.
First, adaptive capacity cannot be varied independently — we cannot ask what happens if a species is
sensitive but resilient. Second, the traits that most distinguish recovery potential *within* a risk
category (generation time, fecundity, dispersal, habitat specialization) are invisible to the model.
Separating the terms is the most valuable single upgrade available and we have added it to Next Steps.

**Edit.** Insert after the bulleted definitions of v_c, p_sc, w_s, S_g:

> **Where exposure and adaptive capacity sit in v1.** The implemented score resolves the sensitivity (S)
> and adaptive-capacity (A) terms jointly and leaves exposure (E) to be supplied by the decision being
> evaluated. Extinction risk serves as an integrated proxy for S and A because the IUCN and ESA listing
> criteria assess population size, rate of decline, range restriction, and fragmentation — the same
> properties that govern whether a population can absorb an added impact and recover from it. This is a
> deliberate simplification and the principal structural limitation of v1: adaptive capacity cannot be
> varied independently of sensitivity, and traits that distinguish recovery potential within a risk
> category (generation time, fecundity, dispersal, habitat specialization) are not represented. Phase 2
> would separate the terms by adding activity-specific exposure layers (spill-trajectory probability,
> lease-block footprints, modeled noise fields) and trait-based adaptive-capacity attributes drawn from
> established marine vulnerability protocols (Hare et al. 2016).

**Status.** Edited.

---

# 3. Methods — extinction-risk weights

## C28 — "I realize that this is a general weighting strategy, however, how were these weights determined? Are there similar studies or additive species models that can be referenced?" (w_s definition) — and C37, the same question on Table 1

**Response.** They are not ad hoc, and the report should have said so. The 0.2 / 0.4 / 0.6 / 0.8 / 1.0
scheme is the IUCN **"equal steps"** weighting — the weighting used to compute the Red List Index
(Butchart et al. 2004, 2007) and the IUCN Species Threat Abatement and Restoration (STAR) metric
(Mair et al. 2021) — rescaled from that scheme's 0–4 integer scale (LC = 0, NT = 1, VU = 2, EN = 3,
CR = 4) onto a 0–1 multiplier. STAR is the closest published analogue to what the MST does: an additive,
spatially explicit, extinction-risk-weighted sum of species presence, adopted by IUCN and used in
corporate and national biodiversity target-setting.

The one deliberate departure is that Least Concern is 0.2 rather than 0. Under the strict Red List Index
scaling every Least Concern species contributes nothing, and the sensitivity surface collapses onto the
small fraction of assessed species that are threatened — a defensible choice for a threat-abatement
metric, but not for a Section 18 sensitivity analysis, where overall biological richness is part of what
the statute asks BOEM to weigh. Setting LC = 0.2 keeps richness in the signal while still giving a
Critically Endangered species five times the weight of a Least Concern one.

Equal steps is the most widely adopted convention but not the only defensible one. Extinction-probability
weightings — Butchart et al.'s (2007) alternative scheme (LC = 0, NT = 0.0005, VU = 0.005, EN = 0.05,
CR = 0.5), or the IUCN50/IUCN100 transformations of Mooers et al. (2008) — are strongly convex and would
concentrate nearly all weight on CR and EN species, producing a map of endangered-species habitat rather
than a map of sensitivity. Mooers et al. show explicitly that the choice of transformation can reorder
resulting priority lists, so the honest treatment is to disclose the assumption and test it.

**Edit.** Append to the Extinction Risk paragraph preceding Table 1 (and cross-reference from a Table 1
footnote):

> These weights are a rescaling of the IUCN "equal steps" weighting used for the Red List Index
> (Butchart et al. 2004, 2007) and the Species Threat Abatement and Restoration (STAR) metric (Mair et
> al. 2021), which maps LC = 0, NT = 1, VU = 2, EN = 3, CR = 4; dividing by five and shifting Least
> Concern from 0 to 0.2 yields the multipliers in Table 1. STAR is the closest published analogue to the
> MST score: an additive, spatially explicit sum of species presence weighted by extinction risk. Least
> Concern is deliberately given non-zero weight so that overall biological richness remains part of the
> sensitivity signal, consistent with the statutory language, rather than the score being driven
> entirely by the small fraction of species that are threatened; a Critically Endangered species still
> carries five times the weight of a Least Concern one. Equal-steps weighting is the most widely used
> convention but not the only defensible one — extinction-probability weightings (Butchart et al. 2007;
> Mooers et al. 2008) are strongly convex and would concentrate nearly all weight on CR and EN species.
> Because the transformation can affect rank order (Mooers et al. 2008), Phase 2 will report a sensitivity
> analysis comparing Planning Area rankings under equal-steps, extinction-probability, and equal
> (richness-only) weightings.

**Status.** Edited (text). The sensitivity analysis itself is **Proposed** — it is a re-run, roughly a day
of compute and reporting, and we would do it for the revision if BOEM wants it in this report rather than
in Phase 2.

---

# 4. Methods — data source citations

## C32 — "I realize these are well recognized data sources, however these need to be cited with references and links to the appropriate website." (Species Distribution Models) — and C34 (Taxonomic Integration), C49 (Temporal Currency)

**Response.** Agreed without qualification; this was an oversight in converting the drafted report from
its citation-managed source. Every input dataset now carries an in-text citation with a version and an
access date, and the References list gains the corresponding entries. Versions matter here and are now
explicit: these are living databases and a reader reproducing the study needs to know which release was
used.

**Edit.** In-text citations added at each first mention, plus these References entries:

- Assis, J., et al. 2024. Bio-ORACLE v3.0: Pushing marine data layers to the CMIP6 Earth System Models of climate change research. *Global Ecology and Biogeography* 33:e13813. doi:10.1111/geb.13813
- BirdLife International and Handbook of the Birds of the World. 2024. Bird species distribution maps of the world, version 2024.2. http://datazone.birdlife.org/species/requestdis
- Butchart, S.H.M., et al. 2004. Measuring global trends in the status of biodiversity: Red List Indices for birds. *PLoS Biology* 2(12):e383.
- Butchart, S.H.M., et al. 2007. Improvements to the Red List Index. *PLoS ONE* 2(1):e140.
- GBIF.org. 2025. Global Biodiversity Information Facility. https://www.gbif.org
- Hare, J.A., et al. 2016. A vulnerability assessment of fish and invertebrates to climate change on the Northeast U.S. Continental Shelf. *PLoS ONE* 11(2):e0146756.
- Halpern, B.S., K.A. Selkoe, F. Micheli, and C.V. Kappel. 2007. Evaluating and ranking the vulnerability of global marine ecosystems to anthropogenic threats. *Conservation Biology* 21:1301–1315.
- Integrated Taxonomic Information System (ITIS). 2025. https://www.itis.gov. doi:10.5066/F7KH0KBK
- IUCN. 2025. The IUCN Red List of Threatened Species. https://www.iucnredlist.org
- Kaschner, K., K. Kesner-Reyes, C. Garilao, J. Segschneider, J. Rius-Barile, T. Rees, and R. Froese. 2023. AquaMaps: Predicted range maps for aquatic species. https://www.aquamaps.org
- Mair, L., et al. 2021. A metric for spatially explicit contributions to science-based species targets. *Nature Ecology & Evolution* 5:836–844.
- Mooers, A.Ø., D.P. Faith, and W.P. Maddison. 2008. Converting endangered species categories to probabilities of extinction for phylogenetic conservation prioritization. *PLoS ONE* 3(11):e3700.
- Ocean Biodiversity Information System (OBIS). 2025. Intergovernmental Oceanographic Commission of UNESCO. https://obis.org
- Ready, J., et al. 2010. Predicting the distributions of marine organisms at the global scale. *Ecological Modelling* 221:467–478.
- WoRMS Editorial Board. 2025. World Register of Marine Species. https://www.marinespecies.org. doi:10.14284/170

Plus the productivity references listed under C40.

**Status.** Edited. Access dates to be filled at final render.

---

# 5. Methods — primary productivity

## C40 — "More detail needs to be provided here… can you cite some recent examples that support the use of this specific satellite data for calculating NPP to support your method." (Primary Productivity)

**Response.** Agreed. VGPM is the standard operational product distributed by Oregon State University's
Ocean Productivity group and the most widely used chlorophyll-based NPP algorithm in the literature; the
VIIRS R2022 reprocessing was chosen over MODIS because it provides a continuous single-sensor record
spanning the whole 2014–2023 decade, avoiding a cross-sensor splice inside the averaging window.

The supporting literature also gives us the limitation that turns out to matter for the results (C76):
the round-robin intercomparison of Saba et al. (2011), against 1,156 in situ ¹⁴C measurements across ten
regions, found VGPM performs comparably to more complex spectrally resolved models at basin scale but
degrades in optically complex coastal water; the Arctic syntheses of Hill et al. (2013) and Arrigo and
van Dijken (2015) report the same for high-latitude and river-influenced shelves. Citing this properly
is what makes the Hope Basin / Norton Basin discussion defensible rather than surprising.

**Edit.** Append to the Primary Productivity paragraph:

> VGPM is the standard operational NPP product distributed by Oregon State University's Ocean
> Productivity group and the most widely applied chlorophyll-based NPP algorithm in the peer-reviewed
> literature, including for U.S. and Arctic shelf waters (Arrigo and van Dijken 2015; Hill et al. 2013).
> The VIIRS R2022 reprocessing was selected over MODIS to obtain a continuous single-sensor record across
> the full 2014–2023 decade, avoiding a cross-sensor discontinuity within the averaging window. In the
> multi-model round-robin evaluation of Saba et al. (2011), which compared 21 ocean-color productivity
> models against 1,156 in situ ¹⁴C measurements across ten marine regions, VGPM performed comparably to
> more complex spectrally resolved and absorption-based models at basin scale while tending to
> overestimate production in optically complex coastal waters — a limitation directly relevant to the
> Arctic and river-influenced Planning Areas discussed in Results.

New References entries:

- Arrigo, K.R., and G.L. van Dijken. 2015. Continued increases in Arctic Ocean primary production. *Progress in Oceanography* 136:60–70.
- Hill, V.J., P.A. Matrai, E. Olson, S. Suttles, M. Steele, L.A. Codispoti, and R.C. Zimmerman. 2013. Synthesis of integrated primary production in the Arctic Ocean: II. In situ and remotely sensed estimates. *Progress in Oceanography* 110:107–125.
- Saba, V.S., et al. 2011. An evaluation of ocean color model estimates of marine primary productivity in coastal and pelagic regions across the globe. *Biogeosciences* 8:489–503.
- Springer, A.M., and C.P. McRoy. 1993. The paradox of pelagic food webs in the northern Bering Sea — III. Patterns of primary production. *Continental Shelf Research* 13:575–599.

**Status.** Edited.

---

# 6. Methods — aggregation, rescaling, projection

## C42 (IMPORTANT) — "If I understand correctly, the current process would normalize differences among planning areas. However, I thought the goal was to compare the relative environmental sensitivity of these areas. By normalizing, we may inadvertently reduce some of the distinctions in sensitivity (such as species richness)… the rationale behind this method needs to be explained and justified more thoroughly."

**Response.** This is the most important comment in the review, your reading of the method is correct,
and we agree the report under-justifies it. Three parts: what rescaling does, why it is there, and what
we propose to change.

**What it does.** Min–max rescaling within an ecoregion is a monotonic transform, so it changes nothing
about the ordering of cells or Planning Areas *within* an ecoregion — those comparisons are untouched.
What it removes is precisely what you identify: the between-ecoregion difference in magnitude. After
rescaling, a Gulf of America score of 80 and an Arctic score of 80 mean "high relative to its own
region," not "equally sensitive in absolute terms." Ranking Planning Areas from different ecoregions
against one another therefore compares each to its own regional context rather than to a national
yardstick. The report asserts this as a feature without acknowledging that it is also a cost.

**Why it is there.** The raw score is a count-like quantity — it grows with the number of species modeled
in a cell — so unrescaled it is governed by the latitudinal species-richness gradient. Nationally
unrescaled, essentially every subtropical Planning Area outranks essentially every Arctic one, and the
map becomes a restatement of AquaMaps model density. That would tell BOEM the Arctic is not sensitive, a
conclusion neither the ESA/MMPA listing record nor the Section 18 record supports, and it would make the
toolkit useless for the question actually asked in practice: within the region where a sale is being
considered, which areas are most sensitive. Within-region normalization is also the approach the prior
RESA used (Niedoroda et al. 2014), so the MST is consistent with the method it replaces.

**What we propose to change.** We do not think the choice should stay implicit or exclusive:

1. State the trade-off explicitly in Methods and again in Results — what rescaled scores are and are not
   comparable across. (Text edit; done.)
2. **Report raw scores alongside rescaled ones** — an appendix table of raw and rescaled cell and
   Planning Area scores, and a raw-score layer selectable in the app. Species richness and absolute
   score then remain visible rather than being normalized out of sight.
3. **Add a nationally rescaled variant** (a single min–max across all U.S. cells) and report the rank
   correlation between the national and ecoregional Planning Area rankings. That quantifies exactly how
   much the normalization changes the answer instead of leaving it to intuition — and it is the direct,
   testable form of your concern.
4. **Replace the min–max endpoints with robust percentiles** (1st/99th). As implemented, a single outlier
   cell sets the 100 for an entire ecoregion, so one anomalous cell compresses every other score in the
   region. This is a genuine methodological weakness independent of the normalization question.

Items 2–4 change reported numbers and need a re-run plus your direction on which view is the headline
result. Our recommendation is that the **ecoregion-rescaled score remain the headline** (it answers the
management question), with **raw and nationally-rescaled scores reported alongside** so the distinctions
you are concerned about are never hidden — and that item 4 be adopted regardless.

**Edit.** New Methods subsection after Step 3, "What ecoregional rescaling does and does not preserve":

> Rescaling within ecoregions is a deliberate analytical choice with a cost that should be explicit.
> Because min–max rescaling is monotonic, it preserves the complete ordering of cells and Planning Areas
> *within* an ecoregion: no within-region comparison is affected. What it removes is the difference in
> absolute magnitude *between* ecoregions. A score of 80 in the Gulf of America and a score of 80 in the
> Arctic both denote "high relative to that region," not equal absolute sensitivity, and the difference
> in raw species richness between those regions is by design not visible in the rescaled value.
>
> The rationale is that the raw score is count-like — it increases with the number of species modeled in
> a cell — and is therefore governed by the latitudinal richness gradient. Without rescaling, nearly every
> subtropical Planning Area would outrank nearly every Arctic one and the result would approximate a map
> of model density rather than of conservation concern, which would not support the region-specific
> decisions the analysis exists to inform. Within-region normalization is also consistent with the prior
> RESA methodology (Niedoroda et al. 2014).
>
> Two consequences follow for interpretation. First, rescaled scores should not be read as an absolute
> national ranking; comparing Planning Areas across ecoregions compares each to its own regional context.
> Second, because the endpoints are the observed minimum and maximum, a single outlier cell can set the
> 100 for an entire ecoregion and compress every other score in that region; robust percentile endpoints
> (1st/99th) are recommended in place of the observed extrema. Because absolute magnitude is
> management-relevant information, reporting raw (unrescaled) cell and Planning Area scores alongside the
> rescaled scores, together with a nationally rescaled variant and the rank correlation between the two
> rankings, is recommended so that the effect of the normalization on Planning Area rank order can be
> assessed directly rather than assumed.

(The inserted text is worded as a recommendation rather than as a statement that the appendix exists,
since items 2–4 are not yet run. Once BOEM confirms, it becomes a statement of fact plus the appendix.)

**Status.** Text **Edited**; items 2–4 **Proposed**, pending BOEM direction (they change reported numbers).

## C52 — "What is this re-scaling generally based on and why is it needed? I think I kinda know but would be good to explicitly state here." (Geographic Scope)

**Edit.** Replace the rescaling clause in the Geographic Scope paragraph with:

> Rescaling is performed within each BOEM Ecoregion, using the minimum and maximum cell score observed
> inside that ecoregion for the component being scored, so each cell is expressed as its position between
> the least and most sensitive cell in its own region. It is needed because raw scores scale with the
> number of species modeled in a cell and are therefore dominated by the latitudinal species-richness
> gradient rather than by conservation concern; without it, regional comparisons would largely restate
> where more species have been modeled. See "What ecoregional rescaling does and does not preserve" above
> for what this preserves and what it removes.

**Status.** Edited.

## C43 — "Just curious why an equal area projection wasn't used? Incompatibility with related efforts? Distorted map?"

**Response.** Compatibility with the inputs, and area distortion is handled analytically rather than by
projection.

Every source dataset arrives on a regular geographic (WGS84) lat/long grid — AquaMaps half-degree cells,
the Bio-ORACLE reference layers, the VIIRS VGPM 2160 × 4320 grid, and the IUCN/BirdLife range polygons.
Analyzing on a geographic 0.05° grid means each input is resampled once, at ingest. Projecting to an
equal-area grid would resample everything a second time, blurring suitability surfaces and shifting range
boundaries, and no single equal-area projection serves both Florida and the Aleutians well.

The distortion an equal-area projection exists to fix is instead corrected in the arithmetic: cells are
never treated as equal. Every aggregation from cells to Planning Areas is weighted by each cell's true
spherical area and by the fraction of that cell falling inside the Planning Area, so the Planning Area
statistics are equal-area in effect even though the grid is not. This is the "area-weighted average"
referred to in Step 3, and it is why a 5.5 km Gulf cell and a 2.0 km Arctic cell contribute in proportion
to their real extent. Maps use region-appropriate equal-area projections for display only; no analysis is
performed in the display projection.

**Edit.** Append to Step 3:

> The analysis grid is geographic (WGS84) rather than projected, because every input dataset — AquaMaps
> half-degree cells, Bio-ORACLE reference layers, the VIIRS productivity grid, and IUCN/BirdLife range
> polygons — is distributed on a regular lat/long grid; analyzing in geographic coordinates resamples
> each input once rather than twice and avoids blurring suitability surfaces and displacing range
> boundaries, and no single equal-area projection serves both the Gulf of America and the Aleutians well.
> Area distortion is therefore corrected arithmetically rather than by projection: cell area is computed
> on the sphere and every cell-to-Planning-Area aggregation is weighted by that true area and by the
> cell's fractional overlap with the Planning Area, making the reported statistics equal-area in effect.
> Maps are drawn in region-appropriate equal-area projections for display only; no analysis is performed
> in a display projection.

**Status.** Edited.

---

# 7. Methods — data quality

## C48 — "Is this generated from this project or coming from some place else?" (Presence Value Calibration)

**Response.** Both, and the report should distinguish them — the sentence currently reads as one
coherent externally sanctioned scheme when it is not.

- **AquaMaps 0–100% suitability**: external, used exactly as published, unaltered.
- **Expert range maps at 50%**: an external convention. IUCN and BirdLife range polygons are
  extent-of-occurrence — they assert presence *somewhere* within the polygon, not everywhere in it — and
  0.5 is the conventional midpoint used when converting such a polygon to a presence probability.
- **Critical Habitat at 70–90%**: **ours.** There is no external standard. We assigned it on the reasoning
  that a legally designated critical-habitat polygon is a substantially stronger statement about occupancy
  than a range-map extent, but the specific values are a project assumption and should be labeled as one.

Two mitigating facts: in v1 the Critical Habitat and FWS range datasets contributed only 38 pilot models
(~0.2% of 17,333), so this assumption has negligible effect on the v1 results reported here; and the
assumption becomes testable once those datasets are scaled up, by comparing scores under alternative
values against independent occurrence density.

**Edit.** Replace the Presence Value Calibration paragraph with:

> **Presence Value Calibration**: Presence values are drawn from external convention where one exists and
> assigned by this study where one does not, and the two are distinguished here. Continuous suitability
> models (AquaMaps: 0–100%) are used as published, without modification. Expert range maps are assigned a
> 50% presence value following the convention for extent-of-occurrence polygons, which assert presence
> somewhere within the boundary rather than throughout it (IUCN 2025; BirdLife International 2024).
> Designated Critical Habitat is assigned 70–90%, reflecting the stronger occupancy evidence implied by a
> legal designation; these values are an assumption of this study rather than an external standard, and
> are identified as such. In v1 the Critical Habitat and FWS range datasets contributed 38 pilot models
> (~0.2% of 17,333), so this assumption has negligible influence on the results reported here; it will be
> tested against independent occurrence density before those datasets are scaled up.

**Status.** Edited.

## C142 — "I'm curious whether all the available data were used… when I looked up the White-winged Parakeet, I found a population in Miami, which doesn't seem relevant to the current analysis. Could you clarify how the data were selected and whether all records were included?" (Species distribution viewer)

**Response.** Good catch, and it is a real defect rather than a display quirk. Thank you for finding it.

**What happened.** v1's inclusion rule was spatial only: every AquaMaps model and every BirdLife range
polygon intersecting the study area was ingested and scored, with no test of whether the species is
actually marine. The White-winged Parakeet (*Brotogeris versicolurus*) is an introduced, non-marine,
Least Concern parrot whose established Miami population polygon clips coastal cells. It should never have
entered a marine sensitivity score.

**How much it matters — measured, not assumed.** The parakeet alone moves little. The *class* of error
does not: we quantified it in the current pipeline, and the marine-relevance filter removes **651 of 880
(74%) of the bird models** intersecting U.S. waters. Because the excluded species concentrate in a narrow
coastal band, they set the ecoregion maximum against which every other cell is rescaled, so removing them
**raises the rescaled bird component by ~18 points on the 0–100 scale on average across Planning Areas**
(range +0 to +57 for Cook Inlet; rank order largely preserved, Spearman ρ = 0.90). The v1 bird component
is therefore *understated*, in every Planning Area. And the credibility cost is your actual point: a user
who finds a parakeet in a marine sensitivity toolkit has no way to know which other inclusions to trust.

**What we did about it.** The current codebase applies a marine-relevance filter for birds: a species is
scored only if it belongs to a marine or coastal family *and* at least 5% of its whole global range falls
over water, with an explicit curated include/exclude list for edge cases, all committed to the repository
so every decision is reviewable rather than buried. Note the obvious approach does not work — the WoRMS
`isMarine` flag is unusable for birds, since almost no seabirds are registered in WoRMS — which is why
the family-plus-percent-marine test was built.

**What we recommend for this report.** Two options: (a) regenerate the v1 results with the marine-relevance
filter applied and report the corrected numbers, which we can do and which we recommend; or (b) leave the
numbers and document the issue in the new Limitations subsection. Option (a) is a re-run, and we would want
your go-ahead since it changes reported values.

**Edit.** Add to Data Quality Control and Validation:

> **Marine Relevance**: v1 applied a spatial inclusion rule — every model whose distribution intersected
> the study area was ingested — without an independent test of whether the species is marine. This admits
> a small number of non-marine species with coastal range polygons (for example the introduced
> White-winged Parakeet, *Brotogeris versicolurus*, in southern Florida). Because such species are almost
> always Least Concern and occupy few cells, the effect on scores is negligible, but their presence in the
> species viewer is not defensible. Subsequent versions apply an explicit marine-relevance filter: birds
> are scored only if they belong to a marine or coastal family and at least 5% of the whole global range
> falls over water, with a curated include/exclude list maintained in the project repository for edge
> cases. The standard WoRMS `isMarine` flag cannot be used for this purpose because seabirds are largely
> absent from that register.

**Status.** Edited (text). Re-running v1 with the filter is **Proposed**, pending your go-ahead.

---

# 8. Results

## C76 — "I find these results a little surprising, especially the very high primary production values in Hope and Norton Basin. Hope to see an explanation of this. Also, what is driving the great variability? Does this make sense to break this down by season?" (Figure 4)

**Response.** You are right to be surprised. We looked into it, and the answer is: partly real, partly an
artifact of how an annual rate is computed at high latitude — and the artifact is significant enough that
the Arctic values in Figure 4 should be read as biased high. This is the most consequential technical
finding in the review, so we set it out in full.

**Partly real.** Hope Basin and Norton Basin sit in the Bering Strait–Chukchi corridor, where nutrient-rich
Anadyr Water is advected onto a shallow (< 50 m), well-mixed shelf. During the open-water season this is
one of the most productive shelf systems on Earth; in situ estimates for the Anadyr production plume reach
~470 g C m⁻² yr⁻¹ (Springer and McRoy 1993), several times typical shelf values. A high ranking is not by
itself implausible.

**Partly an artifact.** The plotted figures (Hope Basin ~870, Norton Basin ~720 t C km⁻² yr⁻¹, numerically
equal to g C m⁻² yr⁻¹) are roughly twice that in situ benchmark, and the reason is the annualization.
Monthly VGPM composites require both sunlight and open water. North of about 65°N there are no valid
retrievals during polar night or under ice, so those months are *missing data*, not zero. Our processing
takes the mean of the available monthly values and annualizes by 365 days. A "year" at Hope Basin is
therefore the mean of roughly four to six observed, mid-summer, peak-bloom months, while a year in the Gulf
of America is the mean of all twelve, including low-productivity winter. Annualizing both by the same
factor systematically inflates the high-latitude areas relative to the low-latitude ones. The factor-of-two
gap against the in situ benchmark is consistent with exactly this. A secondary contributor is that VGPM's
chlorophyll-based formulation overestimates in optically complex coastal water (Saba et al. 2011; Hill et
al. 2013), and both areas receive large sediment- and CDOM-laden river discharge — the Yukon into Norton
Sound, the Kobuk and Noatak into Kotzebue Sound.

**The variability.** Same source. The error bars are the interannual spread of ten annual values, and in the
Arctic that spread combines genuine year-to-year variation in the timing and duration of ice retreat with
variation in how many months were observable in each year — a year with five observed months and a year
with three are not the same measurement. Planning Areas that are ice-free year-round (Straits of Florida,
Mid Atlantic) have visibly tighter bars, which is the signature of this mechanism rather than of real
ecological stability.

**Yes, break it down seasonally — and we recommend it.** The fix is to stop treating the annual value as a
mean of observed months and instead integrate: sum monthly daily-rate × days-in-month across all twelve
months, treating ice-covered and polar-night months as zero production rather than as missing. That yields
a true annual areal production that is comparable across latitudes and would materially change the Arctic
Planning Area values in Figure 4. We also recommend reporting monthly climatologies per Planning Area,
which is directly useful for the seasonal lease stipulations discussed under Foundation for Mitigation and
Adaptive Management (C130). Both are tractable from data already downloaded and processed — this is a
recomputation, not a new acquisition.

**Edit.** New paragraph after Figure 4 (condensed from the above; see the revised document for the exact
inserted text), plus an entry in the Limitations subsection.

**Status.** Text **Edited**. The corrected annual integration is **Proposed** — it changes Figure 4 and the
productivity component of the Alaska Planning Area scores. Our recommendation is to make this correction
before the report is finalized, since the current values are defensible only with the caveat attached.

## C93 — "This figure probably deserves its own page so it is easier to read." (Figure 9, flower plots)

**Response.** Agreed. Figure 9 will be set full-page (landscape) with the caption on the facing or
following page, and regenerated from the vector source so the petal labels and center scores stay legible
at print size.

**Status.** Production.

## C55 — "It could just be the browser version of MS Word that I am using but this figure appears as poor resolution." (Figure 1, methodology diagram)

**Response.** It is not just the browser — Figure 1 was placed as a screen-resolution PNG. It will be
regenerated at 300 dpi and embedded as vector (EMF/PDF) where the template allows, which also fixes print
output. We will audit the other figures for the same problem at the same time.

**Status.** Production.

---

# 9. Conclusions, Next Steps, Study Products

## C130 — "Not sure what this is referring to?" (Temporal Restrictions bullet)

**Response.** The bullet compressed too much. The point is that v1 cannot support seasonal advice at all,
because every species is treated as present in a cell year-round; season-specific stipulations require
season-resolved distributions, which is a Phase 2 addition. Rewritten to say that plainly.

**Edit.** Replace the bullet:

> **Temporal Restrictions**: v1 scores are annual and static — each species is treated as present in a cell
> for the entire year — so the toolkit cannot presently distinguish seasons. Ingesting month- or
> season-resolved distribution models (for example the seasonal cetacean density models developed for U.S.
> Atlantic and Gulf waters, and seasonal seabird distribution models) would let a Planning Area's score vary
> through the year and would directly support seasonal lease stipulations, such as avoiding construction
> during seabird breeding, marine mammal calving, or sea turtle nesting migrations. Seasonal primary
> productivity, discussed under Results, is available from data already processed and would be the first
> such layer.

**Status.** Edited.

## C136 — "What is this referring to? Need to cite." ("In Phase 2, planned enhancements include:")

**Response.** Fair — as written it implies a defined, funded Phase 2 exists. It does not; "Phase 2" is our
recommended follow-on scope, not a contracted one. Reworded to say so, and to separate enhancements that
are already prototyped in the codebase from those requiring new work, so the distinction is visible. If
BOEM has a follow-on vehicle or study number to reference, send it and we will cite it directly.

**Edit.** Replace the lead-in:

> The MST is designed as a living system, intended to be updated as new data become available and methods
> evolve. The enhancements below are recommended next steps rather than a contracted scope of work; where
> an enhancement is already prototyped in the project codebase this is noted, to distinguish work that is
> substantially complete from work that would require new effort.

**Status.** Edited.

## C140 — "Missing?" (Study Products: interactive web applications / code repositories)

**Response.** We read this two ways and addressed both; tell us if you meant a third.

(a) *Products missing from the list.* Several delivered items were not listed: the data release itself
(species × cell scores as partitioned Parquet with a STAC catalog and API access), the data dictionary and
ISO 19115 metadata accompanying the study footprint, and the rendered workflow notebooks. Added.

(b) *Links not live.* In this draft the URLs are plain text rather than hyperlinks, which may be what read
as missing. They are now live hyperlinks with full `https://` prefixes.

**Edit.** Study Products expanded to include the data release, metadata/data dictionary, and rendered
workflow documentation; all URLs hyperlinked.

**Status.** Edited. Confirm if a third reading was intended.

## C141 — "This website is well-designed and easy to navigate. However, could you provide a link to background information, such as the relevant report? Without access to these resources, the site feels somewhat like a 'black box.'" (Composite sensitivity scores app)

**Response.** Agreed, and this is the right criticism of the app as it stands. A user landing on a map with
no route back to the method has to take the numbers on faith, which is the opposite of what the project is
for. We will add to both apps an **About** panel containing: a link to this final report (PDF); a link to
the methods documentation; per-layer provenance — for any displayed layer, which datasets and which
notebook produced it; and a short plain-language "how to read this score" note covering what the 0–100
value means and, per C42, what it is and is not comparable across. A DOI link will be added once assigned.

**Status.** Production (app change). Study Products text updated to point at the documentation.

## C143 — "Some of the details that I think are missing in the report are found here… this documentation should be provided as an appendix to the report. There is a lot of valuable information that could be provided to the report without going to an external website." (Project documentation)

**Response.** Agreed, and this addresses the root of several other comments — the detail you found missing
in Methods (C25, C32, C34, C40, C49) largely exists, but in the documentation site rather than in the
report. An external link is also a poor archival strategy for a deliverable that needs to stand on its own
in ten years.

We propose two appendices:

- **Appendix A — Methods Documentation**: the rendered workflow documentation, print-formatted: data
  acquisition and versions per source, taxonomic matching rules, masking and threshold decisions, scoring
  implementation, and validation checks.
- **Appendix B — Workflow Notebook Index**: one line per notebook — what it consumes, what it produces,
  where it lives, and its permanent URL — so a reader can go from any number in the report to the code that
  produced it.

One question before we build it: Appendix A at full depth is substantial (on the order of 60–100 pages).
Would BOEM prefer (i) the full documentation inline, (ii) a condensed methods appendix of ~20 pages with the
full documentation as a separate deliverable file, or (iii) the full documentation as a separate archived
PDF referenced from the report? We would default to (ii) unless told otherwise.

**Status.** Proposed. Needs BOEM input on depth.

---

# 10. Cross-cutting: proposed new subsection

Several comments (C27, C42, C48, C76, C142) identify limitations that are currently either unstated or
scattered. We propose a short **Limitations and Known Issues** subsection at the end of Methods collecting:

1. Exposure and adaptive capacity are not independently parameterized; extinction risk proxies both (C27).
2. Ecoregional rescaling removes between-region magnitude differences by design; min–max endpoints are
   outlier-sensitive (C42).
3. Critical Habitat presence values (70–90%) are a project assumption, not an external standard (C48).
4. High-latitude annual NPP is biased high by annualizing a mean over observed months only; VGPM
   overestimates in turbid coastal water (C76).
5. v1's species inclusion rule was spatial only, admitting a small number of non-marine species (C142).

A reviewer looks for this section; its absence reads as either overconfidence or oversight, and having it
strengthens the report rather than weakening it.

---

# 11. Summary of items needing BOEM direction

| # | Item | Comment | Why it needs a decision |
|---|------|---------|-------------------------|
| 1 | What "MMA" refers to | C19 | Two different edits follow |
| 2 | Correct the Arctic NPP annualization and regenerate Figure 4 | C76 | Changes reported values and Alaska scores |
| 3 | Re-run v1 with the marine-relevance filter | C142 | Changes reported values |
| 4 | Report raw + nationally-rescaled scores alongside ecoregional | C42 | Changes what the headline result is |
| 5 | Adopt robust (1st/99th percentile) rescaling endpoints | C42 | Changes reported values; we recommend it regardless |
| 6 | Extinction-risk weighting sensitivity analysis in this report vs. Phase 2 | C28, C37 | Scope/schedule |
| 7 | Depth of Appendix A | C143 | ~60–100 pp vs. ~20 pp |
| 8 | Follow-on vehicle/study number to cite for "Phase 2" | C136 | Citation only |

Items 2 and 3 are corrections to known errors; our recommendation is to make both before the report is
finalized.
