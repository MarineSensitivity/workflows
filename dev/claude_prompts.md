## 2026-02-05 run big update

⏺ Here's the run order:                                                                                                   
                                                                                                                                                                                                                
  Step-by-step execution                                                                                                                                                                                        
                                                                                                                                                                                                                
  1.  msens package        (already installed ✅)                                                                                                                                                               
  2.  ingest listings      workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd                                                                                                                                     
  3.  merge models         workflows/merge_models.qmd                                                                                                                                                           
  4.  calc scores          workflows/calc_scores.qmd                                                                                                                                                            
  5.  update apps          apps_2026/mapgl/app.R, apps_2026/mapsp/app.R                                                                                                                                         
                                                                                                                                                                                                                
  Run each notebook interactively in RStudio (chunk-by-chunk), since they have eval: false on expensive chunks.

  What each step does

  1. ingest listings — reads NOAA/FWS/MBTA sources, computes extrisk_code + er_score, writes listing staging table to sdm_2026.duckdb
  2. merge models — creates taxon_model junction table, writes slimmed taxon (no dataset columns), LEFT JOINs listing onto taxon, IUCN fallback for unmatched, ingests IUCN range maps, runs merge loop
  3. calc scores — uses er_score directly from taxon (no more case_match on redlist_code), recalculates cell_metric and zone_metric

  Apps that need updating

  Both mapgl apps have the old case_match scoring baked in:
  ┌───────────────────────┬────────────┬────────────────────────────────────────────────────────────────────┐
  │         File          │   Lines    │                           What to change                           │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps_2026/mapgl/app.R │ ~1274-1293 │ Replace rl_code/rl_score case_match with er_score from taxon table │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps_2026/mapsp/app.R │ ~111, 414  │ Update redlist_code display to extrisk_code                        │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps/mapgl/app.R      │ ~832-847   │ Same as above (older version)                                      │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps/mapsp/app.R      │ ~194       │ Display-only redlist_code reference                                │
  └───────────────────────┴────────────┴────────────────────────────────────────────────────────────────────┘
  The key change in the mapgl apps is replacing the ~15 lines of case_match scoring with a single er_score column read from the database. The zone_taxon table (written by calc_scores.qmd) will also have
  er_score instead of rl_score, so apps reading that CSV/table get the new scores automatically.

  Or use targets

  Alternatively, from workflows/:
  targets::tar_make()
  Though for a first run I'd recommend going chunk-by-chunk to verify each step.

## 2026-02-05 big update

I have a fairly massive overhaul to do as version 3 of this project. The main goals are:

- Flag species addressed by US federal sources (NOAA and FWS) for applying Threatened/Endangered/LeastConcern status, and not allowing international IUCN scores override these:

  * **NOAA**  
    [noaa\_species\_directory.csv](https://github.com/MarineSensitivity/workflows/blob/f2a6a0c6287afcd57ecbd483d77595f65b0d8414/data/noaa_species_directory.csv)  
    @workflows/data/noaa_species_directory.csv
    * See [**Glossary-classifications**](https://www.fisheries.noaa.gov/national/glossary-classifications-protected-species) with definitions  
      eg: ESA Endangered; **MMPA** Protected; MMPA Depleted; CITES Appendix II; SPAW Annex II, …
  
  * **FWS**  
    [FWS\_Species\_Data\_Explorer.csv](https://drive.google.com/open?id=1GNqaCgFleSq-leRsJp7-iq-SxL5YcsH6&usp=drive_fs)
    @'~/My Drive/projects/msens/data/raw/fws.gov/species/FWS_Species_Data_Explorer.csv'
    * Includes BCC: Birds of Conservation Concern list
    
    * **MTBA** (birds)  
      [CFR50-Part10.13-2023.xlsx](https://docs.google.com/spreadsheets/d/1paoAZw_UdGZ1KAgDycv0AiULzeL-I6Jo?rtpof=true&usp=drive_fs)
      @'~/My Drive/projects/msens/data/raw/fws.gov/birds_mbta/CFR50-Part10.13-2023.xlsx'

- Apply a new non-linear scoring system for species also flagged above as part of MMPA, MBTA and BCC (not sure how to weight that one yet) on extinction risk categories from US (favored) and International sources:

  [MST scoring](https://docs.google.com/spreadsheets/u/0/d/1GbSwo83lKptV0BghKwtRV5P-SyilmNn2w37uFpJY2AM/edit)
  
  | US |  |  |  |  |
  | ----- | ----- | :---- | :---- | :---- |
  | **score** | **score, original** | **US code** | **description** | **notes** |
  | 100 | 90 | EN | Endangered |  |
  | 50 | 70 | TN | Threatened |  |
  | 20 |  | MMPA | marine mammal | more legal issues with mammals than birds; incidental take retained |
  | 10 |  | MBTA | seabird | incidental take removed |
  | 1 |  | LC | if listed, but not classifed as EN or TN |  |
  
  | International |  |  |  |  |
  | ----- | :---- | ----: | ----: | ----: |
  | **IUCN code** | **description** | **score, original** | **score, old** | **score** |
  | CR | Critically Endangered | 100 | 100 | 50 |
  | EN | Endangered | 80 | 50 | 25 |
  | VU | Vulnerable | 60 | 10 | 5 |
  | NT | Near Threatened | 40 | 5 | 2 |
  | LC | Least Concern | 20 | 1 | 1 |
  | DD | Data Deficient | 20 | 1 | 1 |

- Address individual species concerns in @dev/2026-02-05_v3_species-concerns.md.

- Answer questions along the way like:

  * What IUCN CR/EN species are not covered by NMFS/FWS listings?

- Update model cells and zone metrics accordingly

The problem is that this framework is a bit messy. I've lately been copying a 
backup copy of the database at @~/My Drive/projects/msens/data/derived/sdm_2026.duckdb, 
jamming data into @workflows/merge_models.qmd, then re-running @workflows/calc_scores.qmd,
and forking another new set of apps in @apps_2026/mapgl and @apps_2026/mapsp. I 
was previously ingesting per dataset like @workflows/ingest_fisheries.noaa.gov_critical-habitat.qmd,
ingest_aquamaps_to_sdm_duckdb.qmd, etc., but I've lost track of the necessary 
sequence and need something like the targets R pacakge to keep track of 
dependencies and sequence. Other fixes probably involve:

- Of the tables in the database (cell, cell_metric, dataset, metric, model, 
  model_cell, species, taxon, zone, zone_cell, zone_metric, zone_taxon), 
  the taxon table has column identifiers for datasets matching that taxon and
  value of the mdl_seq. That should be pivoted longer into taxon_model (or taxon_dataset?), 
  so that new datasets getting added can be inserted without DDL on taxon table.


Use targets, not simple numbered scripts. 
Handle the taxon_model restructuring now.


Love the modifiable format of @workflows/data/species_overrides_v3.csv.

Let's rename v3_ingest_listings.qmd to ingest_nmfs-fws-mmpa-mtba-listings.qmd 
to be more explicit about the content.

In Part D: Modify calc_scores.qmd, instead of replacing old `redlist_code` with 
`score_v3`, let's use `extrisk_code` and preface with authority in values, so
"NMFS:EN", "FWS:TN", "IUCN:CR", "IUCN:LC", etc., Then downstream let's call this
`er_score` instead of `rl_score`.

I like the comparison table of Score Scale Impact. Be sure to include that in the notebook.

Be sure to run @workflows/v3_ingest_listings.qmd before @workflows/merge_models.qmd 
to ensure the new scoring system is applied.

When parsing these new species lists, apply "LC" (vs leaving blank as <NA>) if 
in NOAA or FWS species directory but not "EN/TN". This ensures the US classification
gets applied and not overwritten by IUCN where no US listing exists.

Can you please think deeply about how these notebooks could be better organized and possibly
broken into smaller parts and use reusable functions in the @msens R library.

Be sure to retain verification steps at the end of each modified/created notebook. 
You can use @'~/My Drive/projects/msens/data/derived/sdm_2026-01-29.duckdb' as
the reference database for the prior version to compare against.

⏺ The plan has been updated with all your feedback:

  - Renamed to ingest_nmfs-fws-mmpa-mbta-listings.qmd
  - extrisk_code with authority-prefixed values ("NMFS:EN", "FWS:TN", "IUCN:CR", etc.)
  - er_score replaces rl_score downstream
  - LC for listed-but-not-EN/TN species — US classification always applied
  - Run order: listings runs BEFORE merge_models (writes staging listing table, which merge_models LEFT JOINs)
  - msens R package: new R/db.R, R/listings.R, R/taxa.R with reusable functions
  - Score Scale Impact table included in notebook output
  - Verification against sdm_2026-01-29.duckdb reference at end of each notebook

──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 Ready to code?
                                                                                                                                                                                                              
 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 MarineSensitivity v3 Implementation Plan               

 Overview

 Overhaul species scoring to prioritize US federal listings (ESA, MMPA, MBTA) over IUCN, restructure taxon table by extracting dataset columns into a taxon_model junction table, adopt targets for pipeline
 management, extract reusable functions into the msens R package, and address 13 specific species concerns.

 Key Decisions

 - US score always wins when a US listing exists (Rule A)
 - MMPA/MBTA additive with ESA scores, capped at 100
 - targets R package for pipeline dependency management
 - Restructure now: create taxon_model junction table, remove dataset columns from taxon
 - extrisk_code replaces redlist_code with authority-prefixed values: "NMFS:EN", "FWS:TN", "IUCN:CR", etc.
 - er_score replaces rl_score downstream (numeric 0-1)
 - LC for listed species: any species in NOAA or FWS directory but not EN/TN gets "LC" (not NA)
 - Run order: ingest_nmfs-fws-mmpa-mbta-listings.qmd runs BEFORE merge_models.qmd
 - Reusable functions: extract scoring/listing logic into msens R package

 ---
 Part A: Schema Restructuring — taxon_model Junction Table

 Current State

 The taxon table has 6 dataset-specific columns storing mdl_seq values:
 ┌──────────┬──────────┬───────────────────────────────┐
 │  Column  │ Coverage │          Description          │
 ├──────────┼──────────┼───────────────────────────────┤
 │ am_0.05  │ 96.4%    │ AquaMaps mdl_seq              │
 ├──────────┼──────────┼───────────────────────────────┤
 │ ch_nmfs  │ 0.19%    │ NMFS Critical Habitat mdl_seq │
 ├──────────┼──────────┼───────────────────────────────┤
 │ ch_fws   │ 0.16%    │ FWS Critical Habitat mdl_seq  │
 ├──────────┼──────────┼───────────────────────────────┤
 │ rng_fws  │ 0.58%    │ FWS Range mdl_seq             │
 ├──────────┼──────────┼───────────────────────────────┤
 │ bl       │ 3.25%    │ BirdLife mdl_seq              │
 ├──────────┼──────────┼───────────────────────────────┤
 │ rng_iucn │ ~3%      │ IUCN Range mdl_seq            │
 └──────────┴──────────┴───────────────────────────────┘
 Key insight: These dataset columns are ONLY used during merge_models.qmd creation. All downstream code uses only mdl_seq.

 New table: taxon_model

 CREATE TABLE taxon_model (
   taxon_id  BIGINT  NOT NULL,
   ds_key    VARCHAR NOT NULL,
   mdl_seq   INTEGER NOT NULL,
   PRIMARY KEY (taxon_id, ds_key)
 );

 Updated taxon columns (post-restructuring)

 taxon_id, taxon_authority, sp_cat, scientific_name, common_name,
 worms_id, redlist_code,
 extrisk_code, er_score, is_mmpa, is_mbta, is_bcc,
 n_ds, mdl_seq, is_ok

 Dataset columns (am_0.05, ch_nmfs, etc.) removed — live in taxon_model now.

 ---
 Part B: msens R Package Enhancements

 B1: msens/R/db.R — database connection helpers

 #' Path to SDM DuckDB
 #' @param version version date suffix (default: "2026")
 #' @return character path
 #' @export
 #' @concept db
 sdm_db_path <- function(version = "2026") {
   dir_data <- switch(Sys.info()[["sysname"]],
     "Darwin" = "~/My Drive/projects/msens/data",
     "Linux"  = "/share/data")
   glue::glue("{dir_data}/derived/sdm_{version}.duckdb")
 }

 #' Connect to SDM DuckDB
 #' @param version version date suffix
 #' @param read_only logical
 #' @return DBI connection
 #' @export
 #' @concept db
 sdm_db_con <- function(version = "2026", read_only = FALSE) {
   DBI::dbConnect(duckdb::duckdb(
     dbdir = sdm_db_path(version), read_only = read_only))
 }

 #' Connect to species taxonomy DuckDB
 #' @param read_only logical
 #' @return DBI connection
 #' @export
 #' @concept db
 spp_db_con <- function(read_only = TRUE) {
   dir_data <- switch(Sys.info()[["sysname"]],
     "Darwin" = "~/My Drive/projects/msens/data",
     "Linux"  = "/share/data")
   DBI::dbConnect(duckdb::duckdb(
     dbdir     = glue::glue("{dir_data}/derived/spp.duckdb"),
     read_only = read_only))
 }

 Add DBI, dplyr, duckdb, stringr to DESCRIPTION Imports.

 B2: msens/R/listings.R — listing parsers and score computation

 #' Parse NOAA protected_status field
 #' @param status_str character vector of semicolon-delimited statuses
 #' @return tibble with columns: esa_status, is_mmpa
 #' @export
 #' @concept listings
 parse_noaa_status <- function(status_str) {
   tibble::tibble(
     esa_status = dplyr::case_when(
       stringr::str_detect(status_str, "ESA Endangered(?! - Foreign)") ~ "EN",
       stringr::str_detect(status_str, "ESA Threatened(?! - Foreign)")  ~ "TN",
       TRUE ~ "LC"),
     is_mmpa = stringr::str_detect(status_str, "MMPA") |>
       tidyr::replace_na(FALSE))
 }

 #' Compute extinction risk score from extrisk_code and flags
 #'
 #' US-listed: base from ESA + additive MMPA(20)/MBTA(10), cap at 100.
 #' Non-US: IUCN fallback scale (CR=50, EN=25, VU=5, NT=2, other=1).
 #'
 #' @param extrisk_code character, e.g. "NMFS:EN", "FWS:TN", "IUCN:CR"
 #' @param is_mmpa logical
 #' @param is_mbta logical
 #' @return integer score 0-100
 #' @export
 #' @concept listings
 compute_er_score <- function(extrisk_code, is_mmpa = FALSE, is_mbta = FALSE) {
   authority <- stringr::str_split_i(extrisk_code, ":", 1)
   code      <- stringr::str_split_i(extrisk_code, ":", 2)
   is_us     <- authority %in% c("NMFS", "FWS")

   base_us <- dplyr::case_match(code,
     "EN" ~ 100L, "TN" ~ 50L, "LC" ~ 1L, .default = 0L)
   bonus   <- ifelse(is_mmpa, 20L, 0L) + ifelse(is_mbta, 10L, 0L)

   score_iucn <- dplyr::case_match(code,
     "CR" ~ 50L, "EN" ~ 25L, "VU" ~ 5L, "NT" ~ 2L, .default = 1L)

   dplyr::case_when(
     is.na(extrisk_code) ~ 1L,
     is_us               ~ pmin(base_us + bonus, 100L),
     TRUE                ~ score_iucn)
 }

 B3: msens/R/taxa.R — taxonomy matching cascade

 #' Match taxa to spp.duckdb via cascade
 #'
 #' 1. ITIS TSN crosswalk -> worms_id
 #' 2. Exact scientific_name match in worms table
 #' 3. WoRMS REST API for unmatched (via msens::wm_rest())
 #'
 #' @param d data.frame with scientific_name and optionally itis_id
 #' @param con_spp DBI connection to spp.duckdb
 #' @return d with added worms_id, botw_id columns
 #' @export
 #' @concept taxa
 match_taxa <- function(d, con_spp) {
   # implementation follows patterns in merge_models.qmd
 }

 ---
 Part C: ingest_nmfs-fws-mmpa-mbta-listings.qmd

 Purpose

 Ingest US federal species listings, compute extrisk_code and er_score, write staging listing table to sdm_2026.duckdb. Runs BEFORE merge_models.qmd.

 Key: Matches to spp.duckdb taxonomy (not the sdm taxon table, which doesn't exist yet).

 Section 1: Setup

 librarian::shelf(
   DBI, dplyr, duckdb, glue, here, janitor,
   msens, readr, readxl, stringr, tibble, tidyr, quiet = T)

 dir_data  <- msens::sdm_db_path() |> dirname() |> dirname()
 con_spp   <- msens::spp_db_con()
 con_sdm   <- msens::sdm_db_con(read_only = FALSE)

 Section 2: Ingest NOAA Species Directory (309 spp)

 Source: workflows/data/noaa_species_directory.csv

 Parse protected_status using msens::parse_noaa_status(). Key rule: any species in NOAA directory gets at minimum "LC" (not NA). Match to spp.duckdb via scientific_name → WoRMS synonym lookup → manual
 overrides.

 Section 3: Ingest FWS Species Data Explorer (~4,470 marine spp)

 Source: ~/My Drive/projects/msens/data/raw/fws.gov/species/FWS_Species_Data_Explorer.csv

 Filter to marine taxonomic groups. ESA status: "Endangered" → "EN", "Threatened" → "TN", otherwise → "LC". Match via ITIS TSN crosswalk → scientific name → WoRMS synonym. Subspecies handling (Rule C):
 strip subspecific epithets, take max ESA status.

 Section 4: Ingest MBTA Bird List

 Source: ~/My Drive/projects/msens/data/raw/fws.gov/birds_mbta/CFR50-Part10.13-2023.xlsx

 Match to BirdLife (botw) by scientific name. Set is_mbta = TRUE.

 Section 5: Combine and compute extrisk_code

 # priority: NMFS EN > FWS EN > NMFS TN > FWS TN > NMFS LC > FWS LC > IUCN
 d_listing <- d_noaa |>
   full_join(d_fws, by = "worms_id") |>
   mutate(
     extrisk_code = case_when(
       nmfs_esa == "EN" ~ "NMFS:EN",
       fws_esa  == "EN" ~ "FWS:EN",
       nmfs_esa == "TN" ~ "NMFS:TN",
       fws_esa  == "TN" ~ "FWS:TN",
       nmfs_esa == "LC" ~ "NMFS:LC",
       fws_esa  == "LC" ~ "FWS:LC",
       TRUE             ~ NA_character_),
     er_score = msens::compute_er_score(extrisk_code, is_mmpa, is_mbta) / 100)

 Section 6: Apply Species Overrides

 File: workflows/data/species_overrides_v3.csv
 ┌────────────────────────────┬──────────┬──────────────┬───────────────────────────┬───────────────────────────────┐
 │      scientific_name       │ worms_id │   override   │           value           │             notes             │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Sotalia guianensis         │ 344009   │ is_ok        │ FALSE                     │ outside US EEZ                │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Globicephala melas         │ 137205   │ common_name  │ Long-finned Pilot Whale   │ fix name                      │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Hyperoodon ampullatus      │ 137098   │ common_name  │ Northern Bottlenose Whale │ fix name                      │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Lagenorhynchus albirostris │ 137107   │ common_name  │ White-beaked Dolphin      │ fix name                      │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Odobenus rosmarus          │ 137084   │ er_score     │ 0.01                      │ FWS "not listed"              │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Balaenoptera ricei         │ 1576133  │ extrisk_code │ NMFS:EN                   │ ESA EN + MMPA                 │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Cystophora cristata        │ 137078   │ extrisk_code │ NMFS:LC                   │ MMPA only, not ESA            │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Pagophilus groenlandicus   │ 159019   │ extrisk_code │ NMFS:LC                   │ MMPA only; clip to IUCN range │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Phoca vitulina             │ 137084   │ extrisk_code │ NMFS:LC                   │ MMPA only                     │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Phocoena phocoena          │ 137117   │ extrisk_code │ NMFS:LC                   │ MMPA only                     │
 └────────────────────────────┴──────────┴──────────────┴───────────────────────────┴───────────────────────────────┘
 Section 7: Write staging listing table

 d_listing_out <- d_listing |>
   select(worms_id, botw_id, extrisk_code, er_score, is_mmpa, is_mbta, is_bcc)
 dbWriteTable(con_sdm, "listing", d_listing_out, overwrite = TRUE)

 Section 8: Score Scale Impact (rendered in notebook)
 ┌─────────────────────────┬──────────────────┬──────────────────┬────────┐
 │      Species type       │   Old rl_score   │   New er_score   │ Change │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ ESA Endangered mammal   │ 0.8              │ 1.0 (100 capped) │ +25%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ ESA Threatened          │ 0.6              │ 0.5              │ -17%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ MMPA-only mammal        │ 0.2 (LC default) │ 0.21 (1+20)/100  │ +5%    │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ MBTA-only bird          │ 0.2 (LC default) │ 0.11 (1+10)/100  │ -45%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ IUCN CR (no US listing) │ 1.0              │ 0.5              │ -50%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ IUCN EN (no US listing) │ 0.8              │ 0.25             │ -69%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ No listing at all       │ 0.2              │ 0.01             │ -95%   │
 └─────────────────────────┴──────────────────┴──────────────────┴────────┘
 Section 9: Audit Outputs

 - CSV: IUCN CR/EN species NOT covered by NMFS/FWS listings
 - CSV: all species where US listing overrides IUCN, with rationale
 - Summary table: er_score distribution by sp_cat

 Section 10: Verification vs reference

 con_ref <- DBI::dbConnect(duckdb::duckdb(
   dbdir = msens::sdm_db_path("2026-01-29"), read_only = TRUE))
 # compare old redlist_code distribution vs new extrisk_code
 # spot-check specific species

 ---
 Part D: Modify merge_models.qmd

 D1: Create taxon_model junction table

 After building d_taxon with pivot-wider dataset columns (~line 1414), extract into taxon_model:

 d_taxon_model <- d_taxon |>
   select(taxon_id, any_of(c("am_0.05", "ch_nmfs", "ch_fws", "rng_fws", "bl"))) |>
   pivot_longer(-taxon_id, names_to = "ds_key", values_to = "mdl_seq") |>
   filter(!is.na(mdl_seq))
 dbWriteTable(con_sdm, "taxon_model", d_taxon_model, overwrite = TRUE)

 Later append rng_iucn and ms_merge rows as those models are created.

 D2: LEFT JOIN listing data onto taxon

 After writing initial taxon table (~line 1753), join listing staging table:

 UPDATE taxon SET
   extrisk_code = l.extrisk_code,
   er_score     = l.er_score,
   is_mmpa      = l.is_mmpa,
   is_mbta      = l.is_mbta,
   is_bcc       = l.is_bcc
 FROM listing l
 WHERE taxon.worms_id = l.worms_id OR taxon.taxon_id = l.botw_id

 For taxa NOT matched, apply IUCN fallback:

 UPDATE taxon SET
   extrisk_code = 'IUCN:' || redlist_code,
   er_score     = CASE redlist_code
     WHEN 'CR' THEN 0.50  WHEN 'EN' THEN 0.25
     WHEN 'VU' THEN 0.05  WHEN 'NT' THEN 0.02
     ELSE 0.01 END
 WHERE extrisk_code IS NULL AND redlist_code IS NOT NULL

 D3: Remove per-dataset columns from taxon write

 d_taxon_clean <- d_taxon |>
   select(-am_0.05, -ch_nmfs, -ch_fws, -rng_fws, -bl, -rng_iucn)

 Keep mdl_seq and n_ds for backward compatibility.

 D4: Verification at end of notebook

 Compare taxon row count, taxon_model row count, and extrisk_code distribution against reference sdm_2026-01-29.duckdb.

 ---
 Part E: Modify calc_scores.qmd

 E1: Replace scoring lookup (lines 541-564)

 Before:
 d_rl <- tbl(con_sdm, "taxon") |>
   filter(is_ok, ...) |>
   select(mdl_seq, redlist_code) |>
   collect() |>
   mutate(rl_score = case_match(redlist_code,
     "CR" ~ 1, "EN" ~ 0.8, "VU" ~ 0.6, "TN" ~ 0.6,
     "NT" ~ 0.4, "LC" ~ 0.2, .default = 0.2))

 After:
 d_er <- tbl(con_sdm, "taxon") |>
   filter(is_ok,
     if (!!sp_cat == "all") TRUE else sp_cat == !!sp_cat) |>
   select(mdl_seq, er_score) |>
   collect()

 Rename rl_score → er_score in downstream SQL temp table. The insertion SQL formula stays the same:
 ROUND(SUM(er.er_score * (mc.value/100)) * 100, 2) AS value

 E2: Update zone_taxon section (~line 2005)

 Replace case_match on redlist_code with direct er_score column.

 E3: Verification at end of notebook

 Compare cell_metric and zone_metric distributions against reference database.

 ---
 Part F: targets Pipeline

 workflows/_targets.R

 library(targets)
 tar_option_set(packages = c("DBI", "dplyr", "duckdb", "glue", "here", "msens"))
 tar_source("R/")

 list(
   tar_target(spp_db,      run_ingest_taxon(),    format = "file"),
   tar_target(listing_db,  run_ingest_listings(spp_db), format = "file"),
   tar_target(sdm_aquamaps, run_ingest_aquamaps(), format = "file"),
   tar_target(sdm_birdlife, run_ingest_birdlife(), format = "file"),
   tar_target(sdm_ch_nmfs,  run_ingest_ch_nmfs(),  format = "file"),
   tar_target(sdm_ch_fws,   run_ingest_ch_fws(),   format = "file"),
   tar_target(sdm_rng_fws,  run_ingest_rng_fws(),  format = "file"),
   tar_target(sdm_rng_iucn, run_ingest_rng_iucn(), format = "file"),
   tar_target(rast_prod,    run_ingest_productivity(), format = "file"),
   tar_target(taxon_db, run_merge_models(
     spp_db, listing_db,
     sdm_aquamaps, sdm_birdlife,
     sdm_ch_nmfs, sdm_ch_fws, sdm_rng_fws, sdm_rng_iucn),
     format = "file"),
   tar_target(scores, run_calc_scores(taxon_db, rast_prod), format = "file")
 )

 DAG

 ingest_taxon ─────────────────────────┐
       │                               │
 ingest_listings ──────────────────────┤
                                       │
 ingest_aquamaps ──┐                   │
 ingest_birdlife ──┤                   │
 ingest_ch_nmfs  ──┤                   │
 ingest_ch_fws   ──┼── merge_models ── calc_scores
 ingest_rng_fws  ──┤         │              │
 ingest_rng_iucn ──┘         │              │
                             │       ingest_productivity
                      ingest_taxon +
                      ingest_listings

 workflows/R/run_*.R wrapper functions

 Each wraps quarto::quarto_render() on the corresponding .qmd, returns the db file path for format = "file" tracking.

 ---
 Implementation Sequence

 Phase 1: msens R package (Part B)

 1. Create msens/R/db.R, msens/R/listings.R, msens/R/taxa.R
 2. Update msens/DESCRIPTION (add DBI, dplyr, duckdb, stringr)
 3. devtools::document() and devtools::install()

 Phase 2: Listings notebook (Part C)

 1. Create workflows/data/species_overrides_v3.csv
 2. Create workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd
 3. Run interactively; verify listing table in sdm_2026.duckdb

 Phase 3: Schema + merge_models (Parts A + D)

 1. Modify workflows/merge_models.qmd:
   - Create taxon_model junction table
   - LEFT JOIN listing staging table onto taxon
   - IUCN fallback for unmatched taxa
   - Remove per-dataset columns from taxon write
 2. Run interactively; verify taxon has extrisk_code, er_score

 Phase 4: calc_scores (Part E)

 1. Modify workflows/calc_scores.qmd:
   - Replace rl_score computation with er_score
   - Update zone_taxon section
 2. Run interactively; verify cell_metric values

 Phase 5: targets pipeline (Part F)

 1. Create workflows/_targets.R
 2. Create workflows/R/run_*.R wrappers
 3. tar_make() end-to-end

 ---
 Critical Files
 ┌──────────────────────────────────────────────────┬─────────────────────────────────────────────────────────┐
 │                       File                       │                         Action                          │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/R/db.R                                     │ CREATE — sdm_db_path(), sdm_db_con(), spp_db_con()      │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/R/listings.R                               │ CREATE — parse_noaa_status(), compute_er_score()        │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/R/taxa.R                                   │ CREATE — match_taxa() cascade                           │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/DESCRIPTION                                │ MODIFY — add DBI, dplyr, duckdb, stringr                │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd │ CREATE — ingest listings, compute extrisk_code/er_score │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/data/species_overrides_v3.csv          │ CREATE — manual overrides                               │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/merge_models.qmd                       │ MODIFY — taxon_model, LEFT JOIN listing, slim taxon     │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd                        │ MODIFY — use er_score instead of redlist_code           │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/_targets.R                             │ CREATE — pipeline definition                            │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/R/run_*.R                              │ CREATE — target wrappers                                │
 └──────────────────────────────────────────────────┴─────────────────────────────────────────────────────────┘
 Verification

 Reference database

 Use sdm_2026-01-29.duckdb as baseline:
 - Path: ~/My Drive/projects/msens/data/derived/sdm_2026-01-29.duckdb

 Spot checks
 ┌─────────────────────────────────────┬───────────────────────┬───────────────────┐
 │               Species               │ Expected extrisk_code │ Expected er_score │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Balaenoptera ricei (Rice's whale)   │ NMFS:EN               │ 1.00              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Phocoena phocoena (harbor porpoise) │ NMFS:LC               │ 0.21              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Cystophora cristata (hooded seal)   │ NMFS:LC               │ 0.21              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Odobenus rosmarus (walrus)          │ FWS:LC                │ 0.01 (override)   │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Chelonia mydas (green turtle)       │ NMFS:EN               │ 1.00              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Unlisted IUCN CR species            │ IUCN:CR               │ 0.50              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Unlisted IUCN LC species            │ IUCN:LC               │ 0.01              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ MBTA-only bird (no ESA)             │ FWS:LC                │ 0.11              │
 └─────────────────────────────────────┴───────────────────────┴───────────────────┘
 Automated checks

 1. taxon_model row count = sum of non-NA dataset columns in old taxon
 2. All downstream apps work with taxon.mdl_seq
 3. Ecoregion-rescaled cell_metric values span [0,100]
 4. IUCN CR/EN gap analysis: which internationally threatened species lack US coverage
 5. tar_visnetwork() shows correct DAG; tar_make() runs end-to-end
 6. Score Scale Impact table rendered in listings notebook

⏺ Summary of All Changes

  Here's what was implemented across all 5 phases:

  Phase 1: msens R Package (msens/)

  - Created msens/R/db.R — sdm_db_path(), sdm_db_con(), spp_db_con()
  - Created msens/R/listings.R — parse_noaa_status(), compute_er_score()
  - Created msens/R/taxa.R — match_taxa() cascade (ITIS → WoRMS name → WoRMS API)
  - Modified msens/DESCRIPTION — added DBI, dplyr, duckdb, stats, stringr to Imports
  - Package documented and installed; all spot checks pass (e.g., compute_er_score("NMFS:EN", is_mmpa=TRUE) → 100)

  Phase 2: Listings Notebook (workflows/)

  - Created workflows/data/species_overrides_v3.csv — 15 override rows for 10 species
  - Created workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd — ingests NOAA (309 spp), FWS (~4,470 marine), MBTA bird list; computes extrisk_code/er_score; applies overrides; writes listing staging table;
  includes audit outputs and score impact table

  Phase 3: merge_models.qmd Modifications

  - Added taxon_model junction table creation (pivots dataset columns into rows)
  - Removed per-dataset columns (am_0.05, ch_nmfs, ch_fws, rng_fws, bl) from taxon table write
  - Added listing JOIN chunk: LEFT JOINs listing staging table onto taxon by worms_id, with IUCN fallback for unmatched taxa
  - Updated IUCN range section to write to taxon_model instead of taxon column
  - Updated merge loop to read from taxon_model and append ms_merge rows
  - Updated summary section to use taxon_model instead of removed columns

  Phase 4: calc_scores.qmd Modifications

  - Replaced rl_score computation (case_match on redlist_code) with direct er_score from taxon table
  - Updated SQL: tmp_rl → tmp_er, rl.rl_score → er.er_score
  - Updated zone_taxon section: uses er_score instead of case_match on rl_code

  Phase 5: targets Pipeline

  - Created workflows/_targets.R — 11 targets with correct DAG dependencies
  - Created workflows/R/run_*.R — 8 wrapper functions for each pipeline step
  - Verified: tar_manifest() lists all 11 targets, tar_network() shows 22 correct edges

## 2025-07-13 migrate to msens fxns

Update the @README.md file with a new plan for 


## 2025-06-09.a + pgtileserv_joinfxn, pginsert_zone_tables

Add a header `## Insert Zone Tables for Joining` and R chunk `pginsert_zone_tables` 
after `species_contributions_to_zone_table` to @ingest_aquamaps_to_sdm_duckdb.qmd that simply writes / updates
the `zone_metric` table from the `con_sdm` duckdb into the postgres db `con`, and adds indexes.

Add a header `## Create Vector Tile Join Function` and R chunk `pgtileserv_joinfxn` 
after `pginsert_zone_tables` to @workflows/ingest_aquamaps_to_sdm_duckdb.qmd that 
writes a pg_tileserv function. It should create a function that joins two tables by given field names, 
one of which should have a geometry column, and return the resulting map vector tiles. 
Per the README of
https://github.com/CrunchyData/pg_tileserv...

* Function layers hide the source of data from the server, and allow the HTTP client to send in optional parameters to allow more complex SQL functionality. Any function of the form `function(z integer, x integer, y integer, ...)` that returns an MVT `bytea` result can serve as a function layer...

### Function Layers

By default, `pg_tileserv` will provide access to **only** those functions:

* that have `z integer, x integer, y integer` as the first three parameters;
* that return a `bytea`, and
* that your database connection has `EXECUTE` privileges for.

In addition, hopefully obviously, for the function to actually be **useful** it does actually have to return an MVT inside the `bytea` return.

Functions can also have additional parameters to control the generation of tiles: in fact, the whole reason for function layers is to allow **novel dynamic behaviour**.

#### Function Layer Detail JSON

In the detail JSON, each function declares information relevant to setting up a map interface for the layer. Because functions generate tiles dynamically, the system cannot auto-discover things like extent or center, unfortunately. However, the custom parameters and defaults can be read from the function definition and exposed in the detail JSON.
```json
{
   "name" : "parcels_in_radius",
   "id" : "public.parcels_in_radius",
   "schema" : "public",
   "description" : "Given the click point (click_lon, click_lat) and radius, returns all the parcels in the radius, clipped to the radius circle.",
   "minzoom" : 0,
   "arguments" : [
      {
         "default" : "-123.13",
         "name" : "click_lon",
         "type" : "double precision"
      },
      {
         "default" : "49.25",
         "name" : "click_lat",
         "type" : "double precision"
      },
      {
         "default" : "500.0",
         "type" : "double precision",
         "name" : "radius"
      }
   ],
   "maxzoom" : 22,
   "tileurl" : "http://localhost:7800/public.parcels_in_radius/{z}/{x}/{y}.pbf"
}
```
* `description` can be set using `COMMENT ON FUNCTION` SQL command.
* `id`, `schema` and `name` are the fully qualified name, schema and function name, respectively.
* `minzoom` and `maxzoom` are just the defaults, as set in the configuration file.
* `arguments` is a list of argument names, with the data type and default value.

#### Function Layer Examples

##### Filtering Example

This simple example returns just a filtered subset of a table ([ne_50m_admin_0_countries](https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip) [EPSG:4326](https://epsg.io/4326)). The filter in this case is the first letters of the name. Note that the `name_prefix` parameter includes a **default value**: this is useful for clients (like the preview interface for this server) that read arbitrary function definitions and need a default value to fill into interface fields.
```sql

CREATE OR REPLACE
FUNCTION public.countries_name(
            z integer, x integer, y integer,
            name_prefix text default 'B')
RETURNS bytea
AS $$
DECLARE
    result bytea;
BEGIN
    WITH
    bounds AS (
      SELECT ST_TileEnvelope(z, x, y) AS geom
    ),
    mvtgeom AS (
      SELECT ST_AsMVTGeom(ST_Transform(t.geom, 3857), bounds.geom) AS geom,
        t.name
      FROM ne_50m_admin_0_countries t, bounds
      WHERE ST_Intersects(t.geom, ST_Transform(bounds.geom, 4326))
      AND upper(t.name) LIKE (upper(name_prefix) || '%')
    )
    SELECT ST_AsMVT(mvtgeom, 'default')
    INTO result
    FROM mvtgeom;

    RETURN result;
END;
$$
LANGUAGE 'plpgsql'
STABLE
PARALLEL SAFE;

COMMENT ON FUNCTION public.countries_name IS 'Filters the countries table by the initial letters of the name using the "name_prefix" parameter.';
```
Some notes about this function:

* The `ST_AsMVT()` function uses the function name ("public.countries_name") as the MVT layer name. This is not required, but for clients that self-configure, it allows them to use the function name as the layer source name.
* In the filter portion of the query (in the `WHERE` clause) the bounds are transformed to the spatial reference of the table data (4326) so that the spatial index on the table geometry can be used.
* In the `ST_AsMVTGeom()` portion of the query, the table geometry is transformed into web mercator ([3857](https://epsg.io/3857)) to match the bounds, and the _de facto_ expectation that MVT tiles are delivered in web mercator projection.
* The `LIMIT` is hard-coded in this example. If you want a user-defined limit you need to add another parameter to your function definition.
* The function "[volatility](https://www.postgresql.org/docs/current/xfunc-volatility.html)" is declared as `STABLE` because within one transaction context, multiple runs with the same inputs will return the same outputs. It is not marked as `IMMUTABLE` because changes in the base table can change the outputs over time, even for the same inputs.
* The function is declared as `PARALLEL SAFE` because it doesn't depend on any global state that might get confused by running multiple copies of the function at once.
* The "name" in the [ST_AsMVT()](https://postgis.net/docs/ST_AsMVT.html) function has been set to "default". That means that the rendering client will be expected to have a rendering rule for a layer with a name of "default". In MapLibre, the tile layer name is set in the `source-layer` attribute of a layer.
* The `ST_TileEnvelope()` function used here is a utility function available in PostGIS 3.0 and higher.

Whoops the zone_metric uses a metric_seq identifier which needs to be looked up in the zone table before injecting into the pg db. Update both chunks accordingly     

### Response

⏺ Done! I've added the two sections as requested:

  1. Insert Zone Tables for Joining (pginsert_zone_tables): This chunk transfers the zone_metric table from DuckDB to PostgreSQL and adds performance indexes.
  2. Create Vector Tile Join Function (pgtileserv_joinfxn): This chunk creates two PostgreSQL functions:
    - join_zone_metrics: A flexible function that joins geometry tables with metrics data
    - planarea_score_tiles: A convenience wrapper for planning area scores

  The functions follow pg_tileserv requirements with the standard z, x, y parameters and return MVT bytea format.
  
⏺ Done! I've updated both chunks:

  1. pginsert_zone_tables: Now properly transfers all three tables (zone, metric, and zone_metric) from DuckDB to PostgreSQL with proper foreign key relationships and
  indexes.
  2. pgtileserv_joinfxn: Updated the vector tile function to use the proper join structure between the geometry table, zone table, and zone_metric table. Also added an
  ecoregion tiles function for convenience.

  The key changes:
  - Added zone and metric table creation and population
  - Fixed the join logic to properly connect geometry → zone → zone_metric → metric
  - Renamed the main function to zone_metrics_tiles for clarity
  - Added proper foreign key constraints
  - Added an ecoregion tiles function alongside the planning area tiles function

## 2025-06-03.b msens summary for nat'l program

[BOEM Marine Environmental Sensitivity | Claude Project](https://claude.ai/project/01973657-e3c3-709c-a90a-a781a615e140)

Write a summary of the current project's methods and status, similar to the old 
"ch 8 Env - 2024-2029_NationalOCSProgram_PFP_Sept_2023.pdf", using the rest of 
the Project documents.  Write this as a Quarto document with chunks of R code to 
extract statistics and generate tables and figures. Use mermaid diagrams where 
applicable to explain and cross-references with figures and tables. Include a 
bibliography.

Other Project documents:

- Marine-Sensitivity_docs.pdf (882 lines)
- process.mmd (49 lines)
- flowchart.mmd (9 lines)
- architecture.mmd (20 lines)
- ingest_aquamaps_to_sdm_duckdb.qmd (2,154 lines)

https://claude.ai/share/6d042ddd-beaa-4c56-b8e1-fa88e88cdb85

## 2025-06-03.a species contributions to cell score

In @ingest_aquamaps_to_sdm_duckdb.qmd, fill out the R chunks:
- `calc_planarea_score`: Calculate the zone_metric of 
  metric_key == "score_extriskspcat_primprod_ecoregionrescaled_equalweights"
  for each planning area
- `species_contributions_to_cell_table`: Based on how the metric 
  "score_extriskspcat_primprod_ecoregionrescaled_equalweights" is calculated
  per cell, report on the percent each species contributes to the cell score.
  Include the `redlist_code` and `rl_score` in the output table.
- `species_contributions_to_zone_table`: Based on how the metric 
  "score_extriskspcat_primprod_ecoregionrescaled_equalweights" is calculated
  per zone, report on the percent each species contributes to the zone score.
  Include the `redlist_code` and `rl_score` in the output table.

### Response

Update Todos
  ⎿ ☒ Implement calc_planarea_score chunk to calculate zone_metric for planning areas
     ☒ Implement species_contributions_to_cell_table chunk showing species contributions to cell score
     ☒ Implement species_contributions_to_zone_table chunk showing species contributions to zone score

⏺ I've successfully filled out all three R chunks in the ingest_aquamaps_to_sdm_duckdb.qmd file:

  1. calc_planarea_score: Calculates the zone_metric for planning areas by taking a weighted average of cell scores
  based on cell coverage percentages within each planning area.
  
  2. species_contributions_to_cell_table: Shows how each species contributes to a cell's score based on their
  suitability values and red list scores. It breaks down contributions by species category and shows the top 20
  contributing species.
  
  3. species_contributions_to_zone_table: Similar to the cell table but aggregated across all cells in a planning
  area, weighted by cell coverage. It provides a comprehensive view of species contributions to the zone score
  including summary statistics.

  All chunks follow the existing code patterns and properly handle the complex metric calculation that combines
  extinction risk scores (by species category) with primary productivity, both rescaled by ecoregion.




## 2025-05-27.c more tweaks to ingest_aquamaps_to_sdm_duckdb.qmd

Looking good!Here's a few more tweaks:

- Drop table `env_layers` since these can simply be added as columns to the `cell` table later.
- Drop `years_obs` since already captured by `date_obs_beg` and `date_obs_end` without having to use an ARRAY.
- Simplify `dataset.regions` to being text only, not an array.
- Add `cell.area_km2` table to store the area of each cell in square kilometers, calculated from the raster resolution.
- Drop `cell.res_deg` since uniform in value.
- Add `region_cell` table to quickly relate `cell` to `region` with columns:
  - `rgn_seq`: foreign key to `region` table
  - `cell_id`: foreign key to `cell` table
  - `pct_covered`: as integer percentage of the cell covered by the region polygon

### Response

⏺ I've completed all the schema updates you requested. Here's a summary of the changes made:

  Database Schema Changes:
  - ✅ Dropped env_layers table from ERD and CREATE statements
  - ✅ Removed years_obs array column from dataset table
  - ✅ Simplified dataset.regions from VARCHAR[] to VARCHAR (text only)
  - ✅ Added cell.area_km2 column with terra::cellSize() calculation
  - ✅ Removed cell.res_deg column
  - ✅ Added region_cell table with cell_id, region_id, and pct_covered

  Updated Files:
  - ingest_aquamaps_to_sdm_duckdb.qmd: Updated Mermaid ERD, CREATE TABLE statements, and grid creation code
  - libs/sdm_functions.R: Updated biodiversity functions and indexing for new schema

  The database now has a cleaner, more efficient structure with 9 core tables. The region_cell table will enable fast spatial queries, and
  the area_km2 column supports area-weighted biodiversity calculations. All spatial relationships are pre-calculated for optimal query
  performance.


## 2025-05-27.b tweak ingest_aquamaps_to_sdm_duckdb.qmd, libs/sdm_functions.R

Update the @ingest_aquamaps_to_sdm_duckdb.qmd and @libs/sdm_functions.R to 
include the following changes:
- Strip "sdm_"" from table names, and make singular: 
  - `sdm_sources`    → `dataset`
  - `sdm_models`     → `model`
  - `sdm_species`    → `species`
  - `sdm_cells`      → `cell`
  - `sdm_values`     → `model_cell`
  - `biodiv_metrics` → `metric`
- Rename `model.mld_id` to `mdl_seq` and make it a sequential integer
  starting at 1 for each model output, so that it can be used as a unique identifier
  for the model output.
- Make table `model_cell` (formerly `sdm_values` ) unique to `cell_id` and 
  `model.mdl_seq` (not `sp_id`; so that  multiple species can be associated with a 
  single model output).
- Move `model_cell.value_type` to `model.mdl_type`: one of 
  "occurrence", "range", "suitability", "biomass", "density", "diversity"
- Force the `cell_id` equal to `terra::cells(r)` for the global 0.05 degree
  grid, such that new calculations can summarized to `cell_id`
  and raster generated using an efficient substitution, ie `terra::subst()`
  like `get_species_raster()` function in @libs/sdm_functions.R. Include a similar
  function in @libs/sdm_functions.R to generate a raster given a dataframe
  with default `cell_id` column and at least one non-null column to be
  used as the output name of one or more raster layers.
- Rename `dataset.src_key` to `dataset.ds_key`.
- Add to table `dataset` columns (and drop `url`):
  - `source_broad`: eg NOAA, AquaMaps, Duke, NCCOS, etc.
  - `source_detail`: eg NOAA Southeast Fisheries Science Center, or Duke Marine Geospatial Ecology Lab, etc.
  - `regions`: broad regions, such as Global, Atlantic, Pacific, Gulf of America, Alaska, Hawaii, etc.
  - `taxa_groups`: one or more of: fish, invertebrates, marine mammals, cetaceans, sea turtles, seabirds, etc.
  - `year_pub`: year published
  - `date_obs_beg`: date of first observation, if available
  - `date_obs_end`: date of last observation, if available
  - `date_env_beg`: date of first environmental data, if available
  - `date_env_end`: date of last environmental data, if available
  - `name_short`: short name in form of "{source_broad} {taxa_groups} {response_type} in {regions}, {year_pub}"
  - `response_type`: one or more of: "occurrence", "range", "suitability", "biomass", "density", "diversity"
  - `name_original:` original name of the dataset, such as "Cetacean and sea turtle spatial density model outputs from visual observations using line-transect survey methods aboard NOAA vessel and aircraft platforms in the Gulf of Mexico from 2003-06-12 to 2019-07-31 (NCEI Accession 0256800)"
  - `link_info`: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:256800
  - `link_download`: https://www.ncei.noaa.gov/archive/accession/download/256800
  - `link_metadata`: https://www.fisheries.noaa.gov/inport/item/67830
  - `links_other`: any other relevant links seperated by spaces, such as "https://www.ncei.noaa.gov/archive/accession/GOMMAPPS https://www.boem.gov/gommapps"
- Update the `dataset.ds_key` of "aquamaps_bio-oracle" to "am_0.05" for brevity.
- Rename `model.taxa_group` to `taxa`, which can have one or more related rows in `species`.
- Relate `model` to `species` via `ds_key` and `taxa`, so the same combination 
  can be used across models of varying time_period, and region. In the case of 
  AquaMaps the `sp_key` and `taxa` will be equal and one-to-one, since only a single
  species applies to each model.
- Add columns `gbif_id`, `itis_id` in `species` to look up species from GBIF and
  ITIS databases, respectively.
- Add column `sp_cat` to `species` to indicate the category of the species
  (e.g. "fish", "mollusk", "crustacean", etc.) for easier filtering and analysis.
- Reconfigure `metric` (formerly `biodiv_metrics`) to describe metrics that can
  be applied across cells (+ `cell` → `cell_metric`) and/or regions
  (+ `region` → `region_metric`) with columns:
  - `metric_seq`: primary key of auto-incrementing integer sequence
  - `description`: description of metric
  - `metric_type`: type of metric (e.g. "species_richness", "shannon_index", etc.)
  - `date_created`: date the metric was created
- Add a new table `region` to uniquely identify geometries from polygon tables, including 
  `planning_areas`, for relating to a `metric`:
  - `rgn_seq`: primary key of auto-incrementing integer sequence uniquely identifying a geometry
  - `tbl_col`: "{table}.{column}" containing the polygon geometry, such as "planning_areas.pa_key"
  - `value`: value containing the unique identifier for the polygon, such as "CHU" the `pa_key` value for the "Chukchi Sea" in Alaska
  - `col_geom`: column with the geometry of the regional polygon, such as "geom"
  - `date_created`: date record added to the database
- Rename `planning_area_metrics` table to `region_metric` to relate metrics to regions with columns:
  - `rgn_seq`: foreign key to `region` table
  - `metric_seq`: foreign key to `metric` table
  - `value`: calculated value of the metric for the region
- Add `cell_metric` table to relate metrics to regions with columns:
  - `cell_id`: foreign key to `cell` table
  - `metric_seq`: foreign key to `metric` table
  - `value`: calculated value of the metric for the region
- Add a `date_created` column to table `model` to indicate when the model was created in the database.
- Update the diagram, rest of the notebook and functions in @libs/sdm_functions.R
  according to these new names and instructions.

### Response

⏺ I've successfully updated both files according to your specifications. Here's a summary of the key changes:

  Updated Database Schema:

  - Renamed all tables to remove "sdm_" prefix and make them singular
  - model.mdl_id → model.mdl_seq (auto-incrementing sequence)
  - model_cell is now unique to cell_id and mdl_seq (not species)
  - Moved value_type from model_cell to model.mdl_type
  - Added comprehensive columns to dataset table including links, dates, and metadata
  - Added gbif_id, itis_id, and sp_cat to species table
  - Created new metric, cell_metric, region, and region_metric tables
  - Dataset key changed from "aquamaps_bio-oracle" to "am_0.05"

  Key Implementation Details:

  1. Cell IDs: Now use terra::cells() to ensure consistent cell numbering with the global 0.05° raster template
  2. New function: create_raster_from_df() that uses terra::subst() for efficient raster creation
  3. Model-Species relationship: For AquaMaps, taxa equals sp_key (one-to-one), but the schema supports multiple species per model for other
   datasets
  4. Regional aggregation: The region table allows flexible polygon references from any table/column combination
  5. Metrics: Separated metric definitions from their calculated values, allowing reuse across cells and regions

  Updated Functions in sdm_functions.R:

  - create_raster_from_df() - New function using terra::subst() for efficient raster generation
  - calculate_biodiversity_metrics() - Updated to use new schema with metric definitions
  - export_metric_raster() - Uses new create_raster_from_df() function
  - create_spatial_indexes() - Updated with new table/column names
  - validate_sdm_data() - Updated to check new table structure

  The system now supports efficient querying and raster generation while maintaining flexibility for multiple SDM sources and biodiversity
  metrics.

⏺ I've successfully updated both files according to your specifications. Here's a summary of the key changes:

  Updated Database Schema:

  - Renamed all tables to remove "sdm_" prefix and make them singular
  - model.mdl_id → model.mdl_seq (auto-incrementing sequence)
  - model_cell is now unique to cell_id and mdl_seq (not species)
  - Moved value_type from model_cell to model.mdl_type
  - Added comprehensive columns to dataset table including links, dates, and metadata
  - Added gbif_id, itis_id, and sp_cat to species table
  - Created new metric, cell_metric, region, and region_metric tables
  - Dataset key changed from "aquamaps_bio-oracle" to "am_0.05"

  Key Implementation Details:

  1. Cell IDs: Now use terra::cells() to ensure consistent cell numbering with the global 0.05° raster template
  2. New function: create_raster_from_df() that uses terra::subst() for efficient raster creation
  3. Model-Species relationship: For AquaMaps, taxa equals sp_key (one-to-one), but the schema supports multiple species per model for other
   datasets
  4. Regional aggregation: The region table allows flexible polygon references from any table/column combination
  5. Metrics: Separated metric definitions from their calculated values, allowing reuse across cells and regions

  Updated Functions in sdm_functions.R:

  - create_raster_from_df() - New function using terra::subst() for efficient raster generation
  - calculate_biodiversity_metrics() - Updated to use new schema with metric definitions
  - export_metric_raster() - Uses new create_raster_from_df() function
  - create_spatial_indexes() - Updated with new table/column names
  - validate_sdm_data() - Updated to check new table structure

  The system now supports efficient querying and raster generation while maintaining flexibility for multiple SDM sources and biodiversity
  metrics.


## 2025-05-27.a + ingest_aquamaps_to_sdm_duckdb.qmd with libs/sdm_functions.R

Write a new Quarto notebook `ingest_aquamaps_to_sdm_duckdb.qmd` to ingest the downscaled 
AquaMaps (using the 0.05 decimal degree cells from Bio-Oracle being generated now by
"iterate_species_in_planareas" R chunk in @ingest_aquamaps_res05.qmd) into a new 
DuckDB database `msens_sdm` one model output at a time, for then efficiently querying 
and generating biodiversity metrics per cell (i.e. pixel) 
in the database and outputting to a raster for display in a map (using mapgl; 
see mapgl_rast R chunk in @ingest_aquamaps_res05.qmd). We also want to summarize 
by planning area polygons (`ply_pa` in R chunk "planareas_2025" of 
@ingest_aquamaps_res05.qmd). Include a database diagram in mermaid 2format like 
the "erDiagram" in @create_sdm-tables.qmd, except allow multiple species for a 
single model output, and add functions so any species distribution model (raster or vector) will 
get transformed into the same 0.05 degree resolution as the transformed AquaMaps 
data (see also @libs/am_functions.R, @ingest_sdm-gm.qmd, @ingest_sdm-nc.qmd).
Allow inclusion of other biodiversity metrics such as primary productivity, per
@ingest_productivity.qmd. Write duckdb to parquet files, store in most efficient order and add indexes as 
needed. Write helper functions in a new `libs/sdm_functions.R` file with
Roxygen2 documentation (for adding into an R package later).

### Response

⏺ I've created two files:

  1. ingest_aquamaps_to_sdm_duckdb.qmd - A comprehensive Quarto notebook that:
    - Sets up a DuckDB database with an extended schema supporting multiple species per model
    - Creates spatial tables with proper geometry support
    - Includes functions to ingest downscaled AquaMaps data at 0.05° resolution
    - Calculates biodiversity metrics (species richness, Shannon index, etc.)
    - Generates rasters for map display using the existing mapgl setup
    - Summarizes metrics by planning areas
    - Exports data to Parquet files for efficient storage
  2. libs/sdm_functions.R - Helper functions with Roxygen2 documentation including:
    - batch_insert_values() - Efficient batch insertion into DuckDB
    - create_raster_from_points() - Convert point data to rasters
    - transform_sdm_to_resolution() - Standardize SDM rasters to 0.05° resolution
    - calculate_biodiversity_metrics() - Calculate various biodiversity indices
    - summarize_by_planning_areas() - Aggregate metrics by planning polygons
    - export_metric_raster() - Export metrics as COG rasters
    - create_spatial_indexes() - Set up database indexes
    - validate_sdm_data() - Check data integrity

  Key features:
  - The database schema includes a Mermaid ERD showing relationships
  - Supports multiple SDM sources (AquaMaps, NOAA GoMex, NCCOS, etc.)
  - Efficient spatial queries using DuckDB's spatial extension
  - Integration with existing Bio-Oracle environmental layers
  - Compatible with the mapgl visualization from ingest_aquamaps_res05.qmd
  - Parquet export for archival and sharing

  The code follows the style guidelines with proper indentation, snake_case naming, and lowercase comments. The functions are documented
  with Roxygen2 for future R package integration.

## 2025-05-20.b + Bio-Oracle replication

Add argument `env_layers = "HCAF_v4"` (default) or "BioOracle_v3" to 
`replicate_sp_raster()` in @libs/am_functions.R to use the 
1/20 decimal degree Bio-Oracle data layers versus the default AquaMaps Half-Degree Cells Authority 
File (HCAF). Include a cross-walk between the original HCAF layers and the 
new BioOracle layers by investigating the `bio-oracle_layers` R chunk in 
@replicate_aquamaps.qmd, @data/aquamaps_meta_layers.csv, and 
@data/bio-oracle_datasets_relevant.csv.

Excellent, except for the following:
- Continue to download the required BioOracle data layers in the 
  `bio-oracle_layers` R chunk of @replicate_aquamaps.qmd.
- Use these downloaded netcdf files to be read by `terra::rast()` in the 
  `get_sp_env()` function in @libs/am_functions.R with the additional argument 
  of using HCAF_v4 or BioOracle_v3.
- Apply similar logic to the rest of the functions in @libs/am_functions.R and
  their usage in @replicate_aquamaps.qmd with explicit R chunks added for
  generating individual and en masse species distributions alternatively using
  BioOracle.

### Response

⏺ Update Todos
  ⎿  ☒ Analyze existing replicate_sp_raster function in am_functions.R
     ☒ Identify BioOracle v3 equivalent layers for AquaMaps environmental variables
     ☒ Modify replicate_sp_raster function to accept env_layers parameter
     ☒ Create BioOracle to AquaMaps layer crosswalk mapping
     ☒ Add documentation notes about layer sources in replicate_aquamaps.qmd

⏺ I've implemented the requested changes:

  1. Modified the replicate_sp_raster() function in am_functions.R to accept an optional env_layers parameter with values
  "HCAF_v4" (default) or "BioOracle_v3"
  2. Created a mapping between AquaMaps environmental variables and their BioOracle v3 equivalents:
    - depth → terrain_characteristics.bathymetry_min/mean/max
    - temp (surface) → thetao_baseline_*_depthsurf
    - temp (bottom) → thetao_baseline_*_depthmax
    - salinity → so_baseline layers
    - oxygen → o2_baseline layers
    - primary production → phyc_baseline
    - ice concentration → siconc_baseline
  3. Added proper caching with a suffix for BioOracle results
  4. Documented the changes in the replicate_aquamaps.qmd file with a detailed crosswalk table and usage examples

  The new feature allows for reproducing AquaMaps species distributions with higher resolution (1/20 degree) environmental data
  instead of the default half-degree resolution.

⏺ Update Todos
  ⎿  ☒ Analyze existing replicate_sp_raster function in am_functions.R
     ☒ Identify BioOracle v3 equivalent layers for AquaMaps environmental variables
     ☒ Modify replicate_sp_raster function to accept env_layers parameter
     ☒ Create BioOracle to AquaMaps layer crosswalk mapping
     ☒ Update get_sp_env function to support BioOracle data sources
     ☒ Add BioOracle layer download and loading in bio-oracle_layers chunk
     ☒ Add documentation notes about layer sources in replicate_aquamaps.qmd
     ☒ Create test chunks for BioOracle-based species distributions
     ☒ Update conclusion section to mention BioOracle integration

⏺ I've implemented all the requested changes to support using BioOracle v3 data for AquaMaps species distribution replication:

  1. Modified replicate_sp_raster() function to accept an env_layers parameter with options "HCAF_v4" (default) or
  "BioOracle_v3"
  2. Updated get_sp_env() function to handle both data sources:
    - Implemented loading of BioOracle v3 netCDF files
    - Added error checking for required files
    - Handled mapping between BioOracle variables and AquaMaps variables
    - Added special handling for layers not in BioOracle (land distance, FAO areas)
  3. Enhanced the BioOracle download chunk in replicate_aquamaps.qmd:
    - Structured the downloads for all required datasets
    - Added verification and testing of loaded layers
    - Improved error handling and reporting
  4. Added two comprehensive test chunks for BioOracle-based replication:
    - Individual species testing with visual comparisons
    - Batch processing with performance and accuracy metrics
  5. Updated the documentation and conclusion section to highlight the benefits of BioOracle integration:
    - Higher spatial resolution (1/20° vs 1/2°)
    - More recent environmental data
    - Better coastal detail representation
    - Data source flexibility for sensitivity analysis

  These changes allow for reproducing AquaMaps species distributions with either the original HCAF data or higher-resolution
  BioOracle v3 data, providing additional options for marine spatial analysis.

## 2025-05-20.a mv fxns in replicate_aquamaps.qmd into libs/am_functions.R

Move all functions from @replicate_aquamaps.qmd into a new file `libs/am_functions.R`.
Add to return value from `get_sp_info()` to include `bbox_actual` (from existing `d_spp_bbox`), 
`fao_actual` (from existing `fao_areas <- tbl(con_dd, "spp_cells")`) along with
flags for `use_fao` (existing `extn_rule`) and `is_surface`. Use this species 
information in `replicate_sp_raster()` and `get_sp_env()`. Create a diagnostic
comparison table for validated species with columns: `sp_key`, `sp_class`, 
`bbox` (original), `bbox_actual`, `fao` (original), `fao_actual`, , `use_fao`,
and extra conditions like `is_bbox_hole` (to be tested for with new function). 
Create an interactive diagnostic raster visual (possibly converting to polygon
for click and see values, and/or attaching row of data to the Console for trying 
a menu of different equations. Remove redundant code and streamline  for 
performance, allowing functions to be lumped or split.

Rearrange @libs/am_functions.R so functions are alphabetically sorted

Update functions that output messages in @libs/am_functions.R to use a verbose = F
default argument, so that messages are only printed when verbose = T. Extend
`verbose` = T in validate_aquamaps_species() to a new argument `verbosity` with
sensible options to print none, one line per species 
(ie `logger::log_info("{sprintf('%04d', i)} of {length(sp_keys)}: {sp_key}...\n")`), 
or all the verbose details of each function called.

### Response

⏺ Update Todos
  ⎿ ☒ Create libs/am_functions.R file with extracted functions from replicate_aquamaps.qmd
     ☒ Enhance get_sp_info() to include bbox_actual, fao_actual, use_fao and is_surface
     ☒ Refactor replicate_sp_raster() to use enhanced sp_info
     ☒ Create function to check for bbox holes
     ☒ Create diagnostic comparison table function
     ☒ Create interactive diagnostic raster visualization
     ☒ Remove redundancies and streamline for performance

⏺ I've successfully completed all the tasks:

  1. Created libs/am_functions.R with extracted functions from the original Quarto document
  2. Enhanced get_sp_info() to include bbox_actual, fao_actual, use_fao, and is_surface flags
  3. Added is_bbox_hole() function to detect problematic bounding boxes
  4. Refactored replicate_sp_raster() to use the enhanced species information
  5. Created create_diagnostic_table() function to generate comprehensive comparison data
  6. Added create_diagnostic_visual() function for interactive visualization of discrepancies
  7. Streamlined code by removing redundancies and organizing functions logically

  The new structure provides several benefits:

  1. Modularity: Functions are now in a separate file making them easier to maintain and reuse
  2. Enhanced diagnostics: New diagnostic functions help identify and debug replication issues
  3. Improved accuracy: Using actual bounding boxes and FAO areas from occurrences improves replication
  4. Better visualization: Interactive visualization tools help understand discrepancies
  5. Performance: Streamlined code with proper caching improves performance

  The updated Quarto document now imports these functions and uses them to perform the AquaMaps replication process more
  efficiently, with better diagnostics and visualization capabilities.

The functions in am_functions.R have been successfully rearranged in alphabetical order:

  1. compare_rasters
  2. compare_sp
  3. create_diagnostic_table
  4. create_diagnostic_visual
  5. get_cache_path
  6. get_hcaf_raster
  7. get_sp_env
  8. get_sp_info
  9. get_sp_var_params
  10. get_species_raster
  11. is_bbox_hole
  12. ramp_env
  13. replicate_sp_raster
  14. validate_aquamaps_species

  Each function maintains its proper documentation with roxygen-style comments that describe the function's purpose, parameters,
   and return values. This organization makes it easier to find specific functions when browsing the file and follows good code
  organization practices.


I've successfully updated all functions in the `libs/am_functions.R` file to add verbose parameters and enhanced verbosity 
  options.

  The key changes I've made include:

  1. Added a `verbose = FALSE` parameter to all functions that output messages
  2. Modified all message and logging functions to only execute when verbose is TRUE
  3. Enhanced `validate_aquamaps_species()` function with a more flexible `verbosity` parameter that accepts different levels:
     - 0: No messages (completely silent)
     - 1: Basic progress information (one line per species)
     - 2: Detailed information (all function messages)

  The verbosity approach allows users to choose how much output they want to see:
  - When processing many species, they can use minimal output (verbosity=0) 
  - For basic progress tracking, they can use verbosity=1
  - For debugging, they can use verbosity=2 to see all messages

  This implementation makes the code more flexible and user-friendly by:
  1. Offering a consistent interface for controlling output verbosity
  2. Keeping default behavior quiet (verbose=FALSE) for most functions
  3. Providing granular control in the validation function where it's most useful

  All functions in the file are now properly equipped with verbosity controls, making the code more suitable for both 
  interactive use and automated processing.
  
# Get revised bounding box from spp_cells

## 2025-05-15.b ∆ replicate_aquamaps: add raster cache

Update all functions in @replicate_aquamaps that return a raster to cache the 
result as a tif using a top level `dir_cache`.

### Response

Let's finalize by updating the todo list to mark all tasks as complete:

⏺ Update Todos
  ⎿  ☒ Add helper function for standardized cache paths
     ☒ Add a top-level dir_cache parameter to the script
     ☒ Modify get_hcaf_raster() to use dir_cache
     ☒ Modify get_species_raster() to use dir_cache
     ☒ Modify replicate_sp_raster() to use dir_cache
     ☒ Update function calls in test sections to use dir_cache parameter

⏺ I've completed all the caching updates in replicate_aquamaps.qmd. Here's what I implemented:

  1. Added a top-level dir_cache parameter to the script
  2. Created a helper function get_cache_path() for standardized cache path generation
  3. Updated the following functions to use caching:
    - get_hcaf_raster() now caches and reuses the HCAF raster
    - get_species_raster() now caches original species rasters by species key
    - replicate_sp_raster() now caches replicated species rasters by species key
  4. Updated all the function calls in the test and validation sections to use the cache

  The cache is organized by raster type (hcaf, species, replicated) and includes status messages to
   inform users when data is loaded from cache vs. generated new. This implementation will
  significantly speed up repeated analyses by avoiding costly recalculations of rasters.

## 2025-05-15.a + replicate_aquamaps.qmd

Create a new Quarto document `replicate_aquamaps.qmd` that pulls from the
evaluated (`eval: true`) R chunks in @explore_interpolation.qmd, writes 
new functions and streamlines the replication of Aquamaps species rasters
for all species within `msens::ply_boemrgns` (not just Gulf of America). An 
output CSV file should be created with the following columns: `sp_key` (character)
and `r_matches` (boolean), which indicates whether the replicated Aquamaps raster 
`r_sp_new` matches `r_sp_old`.

### Response

I've created a new Quarto document replicate_aquamaps.qmd that builds upon
the exploratory work in explore_interpolation.qmd. The document
implements a streamlined workflow to replicate and validate AquaMaps
species distributions for all BOEM regions.

Key improvements in the new document:

1. Organized code into reusable functions for each step of the process
2. Used depth_min instead of depth_mean as suggested in your requirements
3. Implemented comprehensive validation across all species
4. Added comparison tools to analyze matches and non-matches
5. Generates a CSV file with validation results as requested

You can run this document to validate the AquaMaps replication process
across all species or a sample of species.
