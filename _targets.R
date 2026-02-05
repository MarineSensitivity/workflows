# _targets.R — pipeline definition for MarineSensitivity workflows
# run with: targets::tar_make()
# visualize: targets::tar_visnetwork()

library(targets)

tar_option_set(
  packages = c("DBI", "dplyr", "duckdb", "glue", "here", "msens", "quarto"))

tar_source("R/")

list(
  # taxonomy reference database
  tar_target(
    spp_db,
    run_ingest_taxon(),
    format = "file"),

  # US federal listings (depends on taxonomy)
  tar_target(
    listing_db,
    run_ingest_listings(spp_db),
    format = "file"),

  # species distribution model ingests (independent of each other)
  tar_target(
    sdm_aquamaps,
    run_ingest_aquamaps(),
    format = "file"),
  tar_target(
    sdm_birdlife,
    run_ingest_birdlife(),
    format = "file"),
  tar_target(
    sdm_ch_nmfs,
    run_ingest_ch_nmfs(),
    format = "file"),
  tar_target(
    sdm_ch_fws,
    run_ingest_ch_fws(),
    format = "file"),
  tar_target(
    sdm_rng_fws,
    run_ingest_rng_fws(),
    format = "file"),
  tar_target(
    sdm_rng_iucn,
    run_ingest_rng_iucn(),
    format = "file"),

  # productivity raster
  tar_target(
    rast_prod,
    run_ingest_productivity(),
    format = "file"),

  # merge all models into taxon table (depends on all SDMs + listings)
  tar_target(
    taxon_db,
    run_merge_models(
      spp_db, listing_db,
      sdm_aquamaps, sdm_birdlife,
      sdm_ch_nmfs, sdm_ch_fws, sdm_rng_fws, sdm_rng_iucn),
    format = "file"),

  # calculate scores (depends on merged taxon + productivity raster)
  tar_target(
    scores,
    run_calc_scores(taxon_db, rast_prod),
    format = "file")
)
