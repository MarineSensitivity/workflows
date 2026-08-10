# Content for the v6 revision: replies to each comment + tracked-change edits.
# Keys of REPLIES are the w:comment/@w:id values in the reviewed docx.

AUTHOR = "Ben Best"
INITIALS = "BB"
DATE = "2026-07-28T12:00:00Z"

REPLIES = {
 "19": "MMA = Marine Minerals Administration, the successor agency to BOEM — confirmed from your own "
       "signature block and the MMA logo you sent on 16 Jul. So the question is naming, not scope. We have "
       "added one sentence extending the agency lineage in this paragraph (BLM to MMS to BOEMRE to BOEM) to "
       "MMA, and noting that the work reported here was performed under BOEM and is referred to as BOEM "
       "throughout. The larger question is the one you flag about how draft study reports are being handled: "
       "should this report be rebranded to MMA throughout (title page, headers, ~200 in-text references, the "
       "ESP boilerplate and the DOI/BOEM mission statements at the back), or stay as BOEM with MMA noted as "
       "successor? That is a decision for BOEM/MMA rather than for us, and we will do whichever you specify — "
       "the global rename is a small change if it is wanted.",

 "25": "Agreed — most of this review is that comment in specific form. This revision adds: definitions of "
       "E/S/A; where adaptive capacity actually sits in the model; provenance of the extinction-risk weights; "
       "citations for every input dataset; the rationale AND cost of ecoregional rescaling; why the grid is "
       "geographic and how area distortion is handled instead; provenance of presence values; and the "
       "annualization caveat behind the Arctic productivity results. Per your comment on the docs site, we "
       "also propose moving the full computational methods into the report as an appendix.",

 "26": "Agreed — definitions inserted after the equation (exposure, sensitivity, adaptive capacity), with "
       "citations to the standard ecological-risk / climate-vulnerability framing (IPCC 2014; Halpern et al. "
       "2007; Hare et al. 2016).",

 "27": "It isn't, as an independent term — and the report should say so rather than leave you to notice. v1 "
       "resolves S and A jointly through the extinction-risk weight and leaves E to the specific decision. The "
       "justification: an IUCN/ESA category assesses population size, rate of decline, range restriction and "
       "fragmentation — the same properties that govern whether a population can absorb an impact and recover. "
       "So A is present, but implicit in w_s. That's a real simplification and the largest structural "
       "limitation of v1; text inserted saying exactly that, plus what Phase 2 would do to separate the terms.",

 "28": "Not arbitrary — they're the IUCN 'equal steps' weighting used for the Red List Index (Butchart et al. "
       "2004, 2007) and the STAR metric (Mair et al. 2021), rescaled from that scheme's 0-4 integer scale "
       "(LC=0, NT=1, VU=2, EN=3, CR=4) onto a 0-1 multiplier. STAR is the closest published analogue to what "
       "the MST does: an additive, spatially explicit, extinction-risk-weighted sum of species presence. The "
       "one deliberate departure is LC=0.2 rather than 0, so that overall richness stays in the signal. Text "
       "and citations added; a weighting sensitivity analysis is proposed (see memo §C28).",

 "32": "Agreed without qualification — an oversight when the report was converted from its citation-managed "
       "source. In-text citations with versions are added at each first mention (AquaMaps: Kaschner et al. "
       "2023; Ready et al. 2010; BirdLife International 2024, BOTW v2024.2; Bio-ORACLE: Assis et al. 2024), "
       "and the References list gains the matching entries.",

 "34": "Same fix — WoRMS Editorial Board (2025), GBIF.org (2025), ITIS (2025), IUCN Red List (2025) and the "
       "BirdLife taxonomic checklist are now cited with versions and URLs, both in text and in References.",

 "37": "Same answer as the w_s comment above: these are the IUCN Red List Index 'equal steps' weights "
       "(Butchart et al. 2004, 2007; also used by the STAR metric, Mair et al. 2021) rescaled to 0-1, with LC "
       "shifted from 0 to 0.2 so Least Concern species still contribute to the biodiversity signal. Equal steps "
       "is the most widely used convention but not the only defensible one — extinction-probability weightings "
       "(Butchart et al. 2007; Mooers et al. 2008) are strongly convex and would concentrate nearly all weight "
       "on CR/EN. Because that can change rank order, we propose reporting a sensitivity analysis across "
       "weighting schemes.",

 "40": "Agreed. Added: VGPM is the standard operational product from OSU's Ocean Productivity group and the "
       "most widely applied chlorophyll-based NPP algorithm, including for U.S. and Arctic shelf waters (Arrigo "
       "& van Dijken 2015; Hill et al. 2013); VIIRS R2022 was chosen over MODIS to get a continuous "
       "single-sensor record across the whole 2014-2023 decade. Saba et al. (2011) — 21 models against 1,156 in "
       "situ 14C measurements — found VGPM performs comparably at basin scale but overestimates in optically "
       "complex coastal water. That last point turns out to matter for the Hope/Norton Basin results (your "
       "Figure 4 comment).",

 "42": "This is the most important comment in the review, your reading is correct, and we agree it's "
       "under-justified. Three parts. (1) What it does: min-max rescaling is monotonic, so it changes nothing "
       "about ordering WITHIN an ecoregion; what it removes is exactly what you identify — between-region "
       "magnitude. (2) Why: the raw score is count-like and dominated by the latitudinal richness gradient; "
       "unrescaled, nearly every subtropical PA outranks nearly every Arctic one and the map restates model "
       "density, implying the Arctic isn't sensitive. Within-region normalization is also what the prior RESA "
       "did (Niedoroda et al. 2014). (3) What we propose to change: state the trade-off explicitly (done); "
       "report RAW scores alongside rescaled ones in an appendix and as an app layer; add a nationally-rescaled "
       "variant and report the rank correlation, so the effect of normalization is quantified rather than "
       "assumed; and replace min-max endpoints with robust 1st/99th percentiles, since one outlier cell "
       "currently sets the 100 for an entire ecoregion. Items 2-4 change reported numbers and need your "
       "direction — see memo §C42.",

 "43": "Compatibility with the inputs, and the distortion is handled arithmetically instead. Every source "
       "(AquaMaps half-degree cells, Bio-ORACLE layers, the VIIRS 2160x4320 grid, IUCN/BirdLife polygons) "
       "arrives on a regular lat/long grid, so a geographic analysis grid resamples each input once rather than "
       "twice — projecting would blur suitability surfaces and shift range edges, and no single equal-area "
       "projection serves both Florida and the Aleutians. Cells are never treated as equal: every "
       "cell-to-Planning-Area aggregation is weighted by true spherical cell area and fractional overlap, so "
       "the statistics are equal-area in effect. Maps use equal-area projections for display only. Text added "
       "to Step 3.",

 "47": "We'd genuinely like to hear it. That failure mode — an environmental-envelope model predicting "
       "suitable habitat in a basin the species has never occupied — is the most common defect in a global SDM "
       "archive, and anecdotes like this are how we find the cases worth encoding as tests. The walrus case is "
       "now handled structurally rather than by hand (AquaMaps constrained to the IUCN global range polygon), "
       "and the rule is a tested function in the project R package with a fixture asserting expected output, so "
       "it can't silently regress. Send us the Gulf walrus case and we'll add it as a named regression test.",

 "48": "Both — and the sentence currently reads as one externally sanctioned scheme when it isn't, so we've "
       "split it. AquaMaps 0-100% suitability: external, used as published. Expert range maps at 50%: external "
       "convention, since IUCN/BirdLife polygons are extent-of-occurrence (presence somewhere within, not "
       "throughout). Critical Habitat at 70-90%: OURS — there is no external standard; we assigned it because a "
       "legal designation is stronger occupancy evidence than a range extent, and it's now labeled as a project "
       "assumption. Mitigating fact: in v1 the CH/FWS datasets contributed only 38 pilot models (~0.2% of "
       "17,333), so this has negligible effect on the results reported here.",

 "49": "Agreed — IUCN Red List version, BOTW v2024.2, the VIIRS R2022 reprocessing and the OBIS/GBIF snapshots "
       "are now cited with versions and access dates, in text and in References. Versions matter here: these "
       "are living databases and a reader reproducing the study needs to know which release was used.",

 "52": "Added an explicit statement here: rescaling is done within each BOEM Ecoregion using the min and max "
       "cell score observed inside that ecoregion for the component being scored, so each cell is expressed as "
       "its position between the least and most sensitive cell in its own region. It's needed because raw "
       "scores scale with the number of species modeled in a cell and are therefore dominated by the "
       "latitudinal richness gradient rather than by conservation concern. Cross-referenced to the new "
       "subsection on what rescaling does and does not preserve (see also your Step 2 comment).",

 "55": "Not just your browser — Figure 1 was placed as a screen-resolution PNG. It will be regenerated at 300 "
       "dpi and embedded as vector (EMF/PDF) where the template allows, which also fixes print output. We'll "
       "audit the other figures for the same problem at the same time.",

 "61": "Thank you. Two things worth knowing: the flower plots are interactive in the app (click a Planning Area "
       "to open its plot, hover a petal for the component score), and each is available as a vector PDF, so "
       "they can drop into briefing slides or a PEIS at any size without pixelation. If useful for the 11th "
       "National Program documents we can produce them in a BOEM-branded theme.",

 "76": "You're right to be surprised — we looked into it and the answer is partly real, partly an artifact, and "
       "the artifact is big enough that the Arctic values in Figure 4 should be read as biased high. REAL: Hope "
       "and Norton Basin sit in the Bering Strait-Chukchi corridor where nutrient-rich Anadyr Water floods a "
       "shallow, well-mixed shelf; in situ estimates for the Anadyr plume reach ~470 g C/m2/yr (Springer & "
       "McRoy 1993). ARTIFACT: monthly VGPM composites need sunlight and open water, so north of ~65N there are "
       "NO valid retrievals during polar night or under ice — those months are missing data, not zero. We take "
       "the mean of available months and annualize by 365, so a 'year' at Hope Basin is the mean of ~4-6 "
       "observed peak-bloom months while a year in the Gulf is all 12 including winter. That inflates "
       "high-latitude areas; ~870 vs the ~470 in situ benchmark is consistent with exactly this. VARIABILITY: "
       "the error bars combine genuine interannual variation in ice retreat with variation in how many months "
       "were observable each year — note that year-round ice-free PAs (Straits of Florida, Mid Atlantic) have "
       "visibly tighter bars. SEASONAL: yes, and we recommend it — integrate (sum monthly rate x days, treating "
       "ice/polar-night months as zero) rather than averaging observed months, and report monthly climatologies "
       "per PA, which also feeds the seasonal-stipulation use case. Both are recomputations from data already "
       "processed. This changes Figure 4 and the Alaska productivity scores; see memo §C76.",

 "93": "Agreed — Figure 9 will be set full-page landscape with the caption on the facing/following page, and "
       "regenerated from the vector source so petal labels and center scores stay legible at print size.",

 "130": "The bullet compressed too much. The point: v1 scores are annual and static — every species is treated "
        "as present in a cell year-round — so the toolkit cannot support seasonal advice at all today. "
        "Season-specific stipulations require season-resolved distributions (monthly cetacean density models, "
        "seasonal seabird models), which is a Phase 2 addition. Rewritten to say that plainly. Note seasonal "
        "primary productivity is available from data already processed and would be the first such layer (see "
        "the Figure 4 comment).",

 "136": "Fair — as written it implies a defined, funded Phase 2 exists. It doesn't; 'Phase 2' is our "
        "recommended follow-on scope, not a contracted one. Reworded to say so, and to flag which enhancements "
        "are already prototyped in the codebase versus which need new work. If BOEM has a follow-on vehicle or "
        "study number to reference, send it and we'll cite it directly.",

 "140": "We read this two ways and addressed both — tell us if you meant a third. (a) Products genuinely "
        "missing from the list: the data release itself (species x cell scores as partitioned Parquet with a "
        "STAC catalog and API access), the data dictionary and ISO 19115 metadata accompanying the study "
        "footprint, and the rendered workflow notebooks. Added. (b) The URLs are plain text rather than live "
        "hyperlinks in this draft, which may be what read as missing — being hyperlinked in the revision.",

 "141": "Agreed, and it's the right criticism as the app stands — a user landing on a map with no route back to "
        "the method has to take the numbers on faith, which is the opposite of the point. We'll add an About "
        "panel to both apps with: a link to this final report (PDF); a link to the methods documentation; "
        "per-layer provenance (for any displayed layer, which datasets and which notebook produced it); and a "
        "short 'how to read this score' note covering what the 0-100 value means and what it is/isn't "
        "comparable across. DOI link once assigned.",

 "142": "Good catch, and it's a real defect rather than a display quirk — thank you. v1's inclusion rule was "
        "SPATIAL ONLY: every AquaMaps model and BirdLife range polygon intersecting the study area was ingested "
        "and scored, with no test of whether the species is actually marine. The White-winged Parakeet "
        "(Brotogeris versicolurus) is an introduced, non-marine, Least Concern parrot whose Miami population "
        "polygon clips coastal cells; it should never have been in a marine sensitivity score. That one species moves little, but the class of error is NOT small: we measured it, and the filter removes ~74% of the bird models intersecting U.S. waters, raising the rescaled bird component by ~18 points on the 0-100 scale on average across Planning Areas (rank order largely preserved, rho = 0.90). So v1 understates birds. The current codebase applies a marine-relevance "
        "filter — a bird is scored only if it's in a marine/coastal family AND >=5% of its whole global range is "
        "over water, with a curated include/exclude list committed to the repo. (The obvious approach fails: the "
        "WoRMS isMarine flag is unusable for birds, since seabirds are largely absent from that register.) "
        "Text added documenting the limitation; we recommend re-running v1 with the filter applied, which "
        "changes reported values and needs your go-ahead.",

 "143": "Agreed, and this is the root of several other comments — the detail you found missing in Methods "
        "largely exists, but on the docs site rather than in the report, and an external link is a poor "
        "archival strategy for a deliverable that must stand alone in ten years. We propose Appendix A (Methods "
        "Documentation: acquisition and versions per source, taxonomic matching rules, masking/threshold "
        "decisions, scoring implementation, validation checks) and Appendix B (Workflow Notebook Index: one line "
        "per notebook — inputs, outputs, permanent URL — so any number in the report traces to the code that "
        "produced it). One question: Appendix A at full depth is ~60-100 pp. Would BOEM prefer (i) the full "
        "documentation inline, (ii) a condensed ~20 pp methods appendix with the full docs as a separate "
        "deliverable file, or (iii) full docs as a separate archived PDF referenced from the report? We'd "
        "default to (ii).",
}

# ---------------------------------------------------------------------------
# Tracked-change edits. Each entry: (kind, anchor_para_index, payload)
#   kind "append"  -> append tracked run(s) to the end of that paragraph
#   kind "after"   -> insert new tracked paragraphs after that paragraph
#   kind "replace" -> mark the paragraph's runs deleted, insert replacement text
# payload for "after" is a list of (style, [(text, bold), ...]) paragraph specs;
# style None means "copy the anchor paragraph's properties".
# ---------------------------------------------------------------------------

EDITS = [

 # --- C19: marine minerals / activity-agnostic framing (conditional, reject if not intended)
 ("append", 133, [(" That lineage now continues with the Marine Minerals Administration (MMA), the "
                   "successor agency to BOEM. Because the work reported here was performed under BOEM, the "
                   "agency is referred to as BOEM throughout this report.", False)]),

 # --- C26: define E, S, A after V = f(E,S,A)
 ("after", 146, [
   (None, [("where:", False)]),
   (None, [("Exposure (E)", True),
           (" is the degree to which a place, and the species in it, overlaps in space and time with an "
            "offshore energy activity and its stressors — a spill trajectory, a lease-block footprint, a "
            "construction noise field. Exposure is specific to the activity being evaluated and is supplied by "
            "the decision at hand; the MST provides the ecological surface that an exposure layer is "
            "intersected with.", False)]),
   (None, [("Sensitivity (S)", True),
           (" is the degree to which the species present would be harmed by that exposure.", False)]),
   (None, [("Adaptive capacity (A)", True),
           (" is the ability of those species, and of the system they belong to, to absorb, recover from, or "
            "compensate for the harm.", False)]),
   (None, [("This decomposition is the standard framing in ecological risk and climate-vulnerability "
            "assessment (IPCC 2014; Halpern et al. 2007; Hare et al. 2016).", False)]),
 ]),

 # --- C27: where E and A sit in v1
 ("after", 153, [
   (None, [("Where exposure and adaptive capacity sit in v1.", True),
           (" The implemented score resolves the sensitivity (S) and adaptive-capacity (A) terms jointly and "
            "leaves exposure (E) to be supplied by the decision being evaluated. Extinction risk serves as an "
            "integrated proxy for S and A because the IUCN and ESA listing criteria assess population size, "
            "rate of decline, range restriction, and fragmentation — the same properties that govern whether a "
            "population can absorb an added impact and recover from it. This is a deliberate simplification and "
            "the principal structural limitation of v1: adaptive capacity cannot be varied independently of "
            "sensitivity, and traits that distinguish recovery potential within a risk category (generation "
            "time, fecundity, dispersal, habitat specialization) are not represented. Phase 2 would separate the "
            "terms by adding activity-specific exposure layers (spill-trajectory probability, lease-block "
            "footprints, modeled noise fields) and trait-based adaptive-capacity attributes drawn from "
            "established marine vulnerability protocols (Hare et al. 2016).", False)]),
 ]),

 # --- C32: AquaMaps + Bio-ORACLE citations
 ("append", 159, [(" AquaMaps predicted range maps are produced and distributed by Kaschner et al. (2023), "
                   "following the environmental-envelope method of Kaschner et al. (2006) and Ready et al. "
                   "(2010); the 0.05° reference grid is that of Bio-ORACLE (Assis et al. 2024). Accessed "
                   "October 2025.", False)]),

 # --- C32: BirdLife citation
 ("append", 160, [(" Source: BirdLife International and Handbook of the Birds of the World (2024), Bird "
                   "species distribution maps of the world, version 2024.2.", False)]),

 # --- C34: taxonomic authority citations
 ("after", 169, [
   (None, [("Taxonomic authorities are cited as versioned data sources: WoRMS Editorial Board (2025); "
            "GBIF.org (2025); Integrated Taxonomic Information System (2025); IUCN (2025), Red List of "
            "Threatened Species; and the BirdLife International (2024) taxonomic checklist accompanying BOTW "
            "v2024.2. Because all five are continuously updated, the release used is recorded with each "
            "ingest and reported in the study metadata, so the taxonomy underlying these results can be "
            "reconstructed exactly.", False)]),
 ]),

 # --- C28 / C37: provenance of the extinction-risk weights
 ("after", 171, [
   (None, [("These weights are a rescaling of the IUCN “equal steps” weighting used for the Red List Index "
            "(Butchart et al. 2004, 2007) and the Species Threat Abatement and Restoration (STAR) metric "
            "(Mair et al. 2021), which maps LC = 0, NT = 1, VU = 2, EN = 3, CR = 4; dividing by five and "
            "shifting Least Concern from 0 to 0.2 yields the multipliers in Table 1. STAR is the closest "
            "published analogue to the MST score: an additive, spatially explicit sum of species presence "
            "weighted by extinction risk. Least Concern is deliberately given non-zero weight so that overall "
            "biological richness remains part of the sensitivity signal, consistent with the statutory "
            "language, rather than the score being driven entirely by the small fraction of species that are "
            "threatened; a Critically Endangered species still carries five times the weight of a Least "
            "Concern one. Equal-steps weighting is the most widely used convention but not the only defensible "
            "one — extinction-probability weightings (Butchart et al. 2007; Mooers et al. 2008) are strongly "
            "convex and would concentrate nearly all weight on CR and EN species. Because the transformation "
            "can affect rank order (Mooers et al. 2008), a sensitivity analysis comparing Planning Area "
            "rankings under equal-steps, extinction-probability, and equal (richness-only) weightings is "
            "recommended.", False)]),
 ]),

 # --- C40: NPP method support
 ("after", 199, [
   (None, [("VGPM is the standard operational NPP product distributed by Oregon State University’s Ocean "
            "Productivity group and the most widely applied chlorophyll-based NPP algorithm in the "
            "peer-reviewed literature, including for U.S. and Arctic shelf waters (Arrigo and van Dijken 2015; "
            "Hill et al. 2013). The VIIRS R2022 reprocessing was selected over MODIS to obtain a continuous "
            "single-sensor record across the full 2014–2023 decade, avoiding a cross-sensor discontinuity "
            "within the averaging window. In the multi-model round-robin evaluation of Saba et al. (2011), "
            "which compared 21 ocean-color productivity models against 1,156 in situ ¹⁴C measurements across "
            "ten marine regions, VGPM performed comparably to more complex spectrally resolved and "
            "absorption-based models at basin scale while tending to overestimate production in optically "
            "complex coastal waters — a limitation directly relevant to the Arctic and river-influenced "
            "Planning Areas discussed in Results.", False)]),
 ]),

 # --- C43: geographic grid + area weighting
 ("after", 205, [
   (None, [("Grid and projection.", True),
           (" The analysis grid is geographic (WGS84) rather than projected, because every input dataset — "
            "AquaMaps half-degree cells, Bio-ORACLE reference layers, the VIIRS productivity grid, and "
            "IUCN/BirdLife range polygons — is distributed on a regular latitude/longitude grid; analyzing in "
            "geographic coordinates resamples each input once rather than twice, avoiding blurred suitability "
            "surfaces and displaced range boundaries, and no single equal-area projection serves both the Gulf "
            "of America and the Aleutians well. Area distortion is therefore corrected arithmetically rather "
            "than by projection: cell area is computed on the sphere and every cell-to-Planning-Area "
            "aggregation is weighted by that true area and by the cell’s fractional overlap with the Planning "
            "Area, making the reported statistics equal-area in effect. Maps are drawn in region-appropriate "
            "equal-area projections for display only; no analysis is performed in a display projection.", False)]),
 ]),

 # --- C42: what rescaling does and does not preserve
 ("after", 228, [
   (None, [("What ecoregional rescaling does and does not preserve.", True),
           (" Rescaling within ecoregions is a deliberate analytical choice with a cost that should be "
            "explicit. Because min–max rescaling is monotonic, it preserves the complete ordering of cells and "
            "Planning Areas ", False),
           ("within", True),
           (" an ecoregion: no within-region comparison is affected. What it removes is the difference in "
            "absolute magnitude ", False),
           ("between", True),
           (" ecoregions. A score of 80 in the Gulf of America and a score of 80 in the Arctic both denote "
            "“high relative to that region,” not equal absolute sensitivity, and the difference in raw species "
            "richness between those regions is by design not visible in the rescaled value.", False)]),
   (None, [("The rationale is that the raw score is count-like — it increases with the number of species "
            "modeled in a cell — and is therefore governed by the latitudinal richness gradient. Without "
            "rescaling, nearly every subtropical Planning Area would outrank nearly every Arctic one and the "
            "result would approximate a map of model density rather than of conservation concern, which would "
            "not support the region-specific decisions the analysis exists to inform. Within-region "
            "normalization is also consistent with the prior RESA methodology (Niedoroda et al. 2014).", False)]),
   (None, [("Two consequences follow for interpretation. First, rescaled scores should not be read as an "
            "absolute national ranking; comparing Planning Areas across ecoregions compares each to its own "
            "regional context. Second, because the endpoints are the observed minimum and maximum, a single "
            "outlier cell can set the 100 for an entire ecoregion and compress every other score in that "
            "region; robust percentile endpoints (1st/99th) are recommended in place of the observed extrema. "
            "Because absolute magnitude is management-relevant information, reporting raw (unrescaled) cell and "
            "Planning Area scores alongside the rescaled scores, together with a nationally rescaled variant "
            "and the rank correlation between the two rankings, is recommended so that the effect of the "
            "normalization on Planning Area rank order can be assessed directly rather than assumed.", False)]),
 ]),

 # --- C48: presence value provenance (replace)
 ("replace", 234, [
   ("Presence Value Calibration", True),
   (": Presence values are drawn from external convention where one exists and assigned by this study where "
    "one does not, and the two are distinguished here. Continuous suitability models (AquaMaps: 0–100%) are "
    "used as published, without modification. Expert range maps are assigned a 50% presence value following the "
    "convention for extent-of-occurrence polygons, which assert presence somewhere within the boundary rather "
    "than throughout it (IUCN 2025; BirdLife International 2024). Designated Critical Habitat is assigned "
    "70–90%, reflecting the stronger occupancy evidence implied by a legal designation; these values are an "
    "assumption of this study rather than an external standard, and are identified as such. In v1 the Critical "
    "Habitat and FWS range datasets contributed 38 pilot models (~0.2% of 17,333), so this assumption has "
    "negligible influence on the results reported here; it will be tested against independent occurrence "
    "density before those datasets are scaled up.", False),
 ]),

 # --- C142: marine relevance
 ("after", 235, [
   (None, [("Marine Relevance", True),
           (": v1 applied a spatial inclusion rule — every model whose distribution intersected the study area "
            "was ingested — without an independent test of whether the species is marine. This admits a small "
            "number of non-marine species with coastal range polygons, for example the introduced White-winged "
            "Parakeet (", False),
           ("Brotogeris versicolurus", True),
           (") in southern Florida. Any single such species contributes little — a Least Concern species carries the "
            "lowest weight and occupies few cells — but the filter is not cosmetic in aggregate. It removes "
            "roughly three-quarters of the bird models intersecting U.S. waters, and because the excluded "
            "species concentrate in a narrow coastal band they set the ecoregion maximum against which every "
            "other cell is rescaled. Applying the filter in a later version of the pipeline raised the "
            "ecoregion-rescaled bird component by approximately 18 points on the 0–100 scale on average across "
            "Planning Areas, while largely preserving rank order (Spearman rho = 0.90). The bird component "
            "reported here is therefore understated, and the presence of non-marine species in the species "
            "viewer is not defensible. "
            "Subsequent versions apply an explicit marine-relevance filter: birds are scored only if they "
            "belong to a marine or coastal family and at least 5% of the whole global range falls over water, "
            "with a curated include/exclude list maintained in the project repository for edge cases. The "
            "standard WoRMS isMarine flag cannot be used for this purpose because seabirds are largely absent "
            "from that register.", False)]),
 ]),

 # --- C52: what rescaling is based on and why (Geographic Scope)
 ("after", 237, [
   (None, [("Rescaling is performed within each BOEM Ecoregion, using the minimum and maximum cell score "
            "observed inside that ecoregion for the component being scored, so each cell is expressed as its "
            "position between the least and most sensitive cell in its own region. It is needed because raw "
            "scores scale with the number of species modeled in a cell and are therefore dominated by the "
            "latitudinal species-richness gradient rather than by conservation concern; without it, regional "
            "comparisons would largely restate where more species have been modeled. See “What ecoregional "
            "rescaling does and does not preserve” above for what this preserves and what it removes.", False)]),
 ]),

 # --- Limitations subsection at the end of Methods (before Results)
 ("after", 245, [
   ("Heading2", [("Limitations and Known Issues", False)]),
   (None, [("Five limitations of v1 are material to interpreting the results that follow and are stated here "
            "rather than left implicit.", False)]),
   (None, [("1. Exposure and adaptive capacity are not independently parameterized.", True),
           (" Extinction risk proxies both sensitivity and adaptive capacity, and exposure is supplied by the "
            "decision being evaluated rather than by the toolkit. Recovery-relevant traits within a risk "
            "category are therefore not represented.", False)]),
   (None, [("2. Ecoregional rescaling removes between-region differences in magnitude by design.", True),
           (" Within-region ordering is fully preserved; absolute cross-region comparison is not supported by "
            "the rescaled score. Min–max endpoints are also outlier-sensitive, so a single extreme cell can set "
            "the 100 for an entire ecoregion.", False)]),
   (None, [("3. Critical Habitat presence values (70–90%) are an assumption of this study,", True),
           (" not an external standard. In v1 the affected datasets contribute 38 of 17,333 models, so the "
            "influence on reported results is negligible.", False)]),
   (None, [("4. High-latitude annual productivity is biased high.", True),
           (" Monthly satellite composites are unavailable during polar night and under sea ice, so annual "
            "values for Arctic Planning Areas average only the observed, productive months before annualizing; "
            "VGPM additionally overestimates in turbid, river-influenced coastal water. See the discussion "
            "accompanying Figure 4.", False)]),
   (None, [("5. Species inclusion in v1 was spatial only.", True),
           (" Any model intersecting the study area was scored, which admits a small number of non-marine "
            "species with coastal ranges. A marine-relevance filter is applied in subsequent versions.", False)]),
 ]),

 # --- C76: Arctic productivity explanation, after the Figure 4 caption
 ("after", 282, [
   (None, [("Interpreting the Arctic productivity values.", True),
           (" Two Planning Areas — Hope Basin and Norton Basin, at approximately 870 and 720 t C km⁻² yr⁻¹ — score far above the "
            "rest, and both the level and the spread require explanation. Part of the pattern is real: both lie "
            "in the Bering Strait–Chukchi corridor, where nutrient-rich Anadyr Water is advected onto a shallow "
            "(< 50 m), well-mixed shelf that supports one of the most productive shelf systems on Earth during "
            "the open-water season; in situ estimates for the Anadyr production plume reach approximately 470 g "
            "C m⁻² yr⁻¹ (Springer and McRoy 1993), several times typical shelf values.", False)]),
   (None, [("Part of it, however, is an artifact of how an annual rate is derived from monthly satellite "
            "composites at high latitude, and the values reported here for Arctic Planning Areas should be read "
            "as biased high. Monthly VGPM composites require both sunlight and open water; north of "
            "approximately 65°N there are no valid retrievals during polar night or under ice, so those months "
            "are missing data rather than zero production. The annual value is computed as the mean of the "
            "available monthly values and annualized across 365 days, so a year at Hope Basin represents the "
            "mean of roughly four to six observed, mid-summer months, whereas a year in the Gulf of America "
            "represents all twelve, including low-productivity winter months. Applying the same annualization "
            "to both systematically inflates high-latitude values relative to low-latitude ones; that the Hope "
            "Basin figure is approximately twice the in situ benchmark above is consistent with this bias. A "
            "secondary contributor is that VGPM’s chlorophyll-based formulation overestimates production in "
            "optically complex coastal water (Saba et al. 2011; Hill et al. 2013), and both areas receive large "
            "sediment- and CDOM-laden river discharge — the Yukon into Norton Sound, the Kobuk and Noatak into "
            "Kotzebue Sound.", False)]),
   (None, [("The large standard deviations have the same origin. The error bars show the interannual spread of "
            "ten annual values, which in the Arctic combines genuine year-to-year variation in the timing and "
            "duration of ice retreat with variation in how many months were observable in a given year. "
            "Planning Areas that are ice-free year-round (Straits of Florida, Mid Atlantic) show visibly "
            "tighter bars, which is the signature of this mechanism rather than of greater ecological "
            "stability.", False)]),
   (None, [("The correction is to integrate rather than average: sum monthly production (daily rate × days in "
            "month) across all twelve months, treating ice-covered and polar-night months as zero production "
            "rather than as missing data, which yields a true annual areal production comparable across "
            "latitudes. Reporting monthly climatologies per Planning Area is recommended alongside it, both "
            "because it makes the seasonal structure visible and because it directly supports the seasonal "
            "lease stipulations discussed under Foundation for Mitigation and Adaptive Management. Both are "
            "recomputations from data already acquired and processed.", False)]),
 ]),

 # --- C130: temporal restrictions (replace)
 ("replace", 356, [
   ("Temporal Restrictions", True),
   (": v1 scores are annual and static — each species is treated as present in a cell for the entire year — so "
    "the toolkit cannot presently distinguish seasons. Ingesting month- or season-resolved distribution models, "
    "for example the seasonal cetacean density models developed for U.S. Atlantic and Gulf waters and seasonal "
    "seabird distribution models, would allow a Planning Area’s score to vary through the year and would "
    "directly support seasonal lease stipulations such as avoiding construction during seabird breeding, marine "
    "mammal calving, or sea turtle nesting migrations. Seasonal primary productivity, discussed under Results, "
    "is available from data already processed and would be the first such layer.", False),
 ]),

 # --- C136: Phase 2 framing (replace)
 ("replace", 365, [
   ("The MST is designed as a living system, intended to be updated as new data become available and methods "
    "evolve. The enhancements below are recommended next steps rather than a contracted scope of work; where an "
    "enhancement is already prototyped in the project codebase this is noted, to distinguish work that is "
    "substantially complete from work that would require new effort.", False),
 ]),

 # --- C140: study products additions
 ("after", 384, [
   (None, [("Data release: species-by-cell sensitivity scores and component metrics published as partitioned "
            "Parquet with an accompanying SpatioTemporal Asset Catalog (STAC), queryable through the project "
            "APIs and readable directly by desktop GIS and R/Python clients.", False)]),
   (None, [("Metadata and data dictionary: ISO 19115 metadata (XML) and a data dictionary (Excel) accompanying "
            "the ESP Study Footprint geodatabase.", False)]),
   (None, [("Rendered workflow documentation: every processing notebook executed and rendered to HTML, "
            "archived alongside its source, and reproduced in this report as Appendix A (Methods Documentation) "
            "and Appendix B (Workflow Notebook Index).", False)]),
 ]),
]

# --- New References entries: (insert_after_para_index, text) ------------------
REFERENCES = [
 (387, "Arrigo, K.R., and G.L. van Dijken. 2015. Continued increases in Arctic Ocean primary production. "
       "Progress in Oceanography 136:60–70."),
 (387, "Assis, J., E. Fernández Bejarano, V.W. Salazar, et al. 2024. Bio-ORACLE v3.0: Pushing marine data "
       "layers to the CMIP6 Earth System Models of climate change research. Global Ecology and Biogeography "
       "33:e13813."),
 (389, "BirdLife International and Handbook of the Birds of the World. 2024. Bird species distribution maps of "
       "the world, version 2024.2. http://datazone.birdlife.org/species/requestdis"),
 (393, "Butchart, S.H.M., A.J. Stattersfield, J. Baillie, L.A. Bennun, S.N. Stuart, H.R. Akçakaya, C. "
       "Hilton-Taylor, and G.M. Mace. 2004. Measuring global trends in the status of biodiversity: Red List "
       "Indices for birds. PLoS Biology 2(12):e383."),
 (393, "Butchart, S.H.M., H.R. Akçakaya, J. Chanson, J.E.M. Baillie, B. Collen, S. Quader, W.R. Turner, R. "
       "Amin, S.N. Stuart, and C. Hilton-Taylor. 2007. Improvements to the Red List Index. PLoS ONE 2(1):e140."),
 (396, "GBIF.org. 2025. Global Biodiversity Information Facility. https://www.gbif.org"),
 (396, "Halpern, B.S., K.A. Selkoe, F. Micheli, and C.V. Kappel. 2007. Evaluating and ranking the vulnerability "
       "of global marine ecosystems to anthropogenic threats. Conservation Biology 21:1301–1315."),
 (396, "Hare, J.A., W.E. Morrison, M.W. Nelson, M.M. Stachura, E.J. Teeters, R.B. Griffis, et al. 2016. A "
       "vulnerability assessment of fish and invertebrates to climate change on the Northeast U.S. Continental "
       "Shelf. PLoS ONE 11(2):e0146756."),
 (396, "Hill, V.J., P.A. Matrai, E. Olson, S. Suttles, M. Steele, L.A. Codispoti, and R.C. Zimmerman. 2013. "
       "Synthesis of integrated primary production in the Arctic Ocean: II. In situ and remotely sensed "
       "estimates. Progress in Oceanography 110:107–125."),
 (396, "Integrated Taxonomic Information System (ITIS). 2025. https://www.itis.gov"),
 (396, "Intergovernmental Panel on Climate Change (IPCC). 2014. Climate Change 2014: Impacts, Adaptation, and "
       "Vulnerability. Contribution of Working Group II to the Fifth Assessment Report. Cambridge University "
       "Press, Cambridge, UK."),
 (396, "International Union for Conservation of Nature (IUCN). 2025. The IUCN Red List of Threatened Species. "
       "https://www.iucnredlist.org"),
 (396, "Kaschner, K., R. Watson, A.W. Trites, and D. Pauly. 2006. Mapping world-wide distributions of marine "
       "mammal species using a relative environmental suitability (RES) model. Marine Ecology Progress Series "
       "316:285–310."),
 (396, "Kaschner, K., K. Kesner-Reyes, C. Garilao, J. Segschneider, J. Rius-Barile, T. Rees, and R. Froese. "
       "2023. AquaMaps: Predicted range maps for aquatic species. https://www.aquamaps.org"),
 (396, "Mair, L., L.A. Bennun, T.M. Brooks, S.H.M. Butchart, F.C. Bolam, N.D. Burgess, et al. 2021. A metric "
       "for spatially explicit contributions to science-based species targets. Nature Ecology & Evolution "
       "5:836–844."),
 (397, "Mooers, A.Ø., D.P. Faith, and W.P. Maddison. 2008. Converting endangered species categories to "
       "probabilities of extinction for phylogenetic conservation prioritization. PLoS ONE 3(11):e3700."),
 (398, "Ocean Biodiversity Information System (OBIS). 2025. Intergovernmental Oceanographic Commission of "
       "UNESCO. https://obis.org"),
 (398, "Ocean Productivity. 2022. Standard VGPM product derived from VIIRS, R2022 reprocessing. Oregon State "
       "University, Corvallis, OR. https://orca.science.oregonstate.edu/"),
 (398, "Ready, J., K. Kaschner, A.B. South, P.D. Eastwood, T. Rees, J. Rius, E. Agbayani, S. Kullander, and R. "
       "Froese. 2010. Predicting the distributions of marine organisms at the global scale. Ecological "
       "Modelling 221:467–478."),
 (398, "Saba, V.S., M.A.M. Friedrichs, D. Antoine, R.A. Armstrong, I. Asanuma, M.J. Behrenfeld, et al. 2011. "
       "An evaluation of ocean color model estimates of marine primary productivity in coastal and pelagic "
       "regions across the globe. Biogeosciences 8:489–503."),
 (400, "Springer, A.M., and C.P. McRoy. 1993. The paradox of pelagic food webs in the northern Bering Sea — "
       "III. Patterns of primary production. Continental Shelf Research 13:575–599."),
 (400, "WoRMS Editorial Board. 2025. World Register of Marine Species. Flanders Marine Institute, Ostend, "
       "Belgium. https://www.marinespecies.org"),
]
