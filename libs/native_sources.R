# native_sources.R: readers for the SOURCE vector ranges behind the per-model PMTiles ----
#
# ONE definition shared by publish_native.qmd (tiles them) and map_asset_store.qmd (hashes them
# for the store key, msens::native_vector_hash), so the key can never describe geometry the tiles
# were not built from. Each reader returns an sf with a `mdl_key` column (unfiltered: the caller
# joins to the served set).
#
#   native_pmt_specs(dir_raw, dir_big, iucn_gpkg, bl_sisids = NULL)  -> named list of reader functions
#   slug(), read_*()  are exposed too
suppressMessages({library(sf); library(dplyr); library(glue); library(purrr); library(fs)})

Sys.setenv(OGR_STROKE_CURVE = "TRUE")   # linearize BOTW MULTISURFACE curves
slug <- function(x) gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x))

native_pmt_specs <- function(dir_raw, dir_big, iucn_gpkg, bl_sisids = NULL) {
  read_iucn <- function() {
    # the v8 indexed ranges gpkg (one dissolved feature per served id_no), built by publish_native.qmd's
    # `iucn-src` chunk -- fast + off-Drive vs. crawling thousands of source shapefiles
    stopifnot("run publish_native.qmd's iucn-src chunk first" = file_exists(iucn_gpkg))
    st_read(iucn_gpkg, quiet = TRUE) |> transmute(mdl_key = as.character(mdl_key)) |> st_zm(drop = TRUE)
  }
  read_fws_rng <- function() {
    shps <- glue("{dir_raw}/fws.gov/usfws_complete_species_current_range/usfws_complete_species_current_range_{1:2}.shp")
    map_dfr(shps[file_exists(shps)], function(s) {
      x <- st_read(s, quiet = TRUE); names(x) <- tolower(names(x))
      x |> transmute(mdl_key = glue("rng_fws|{spcode}"))          # source is NAD83 -> publish_pmtiles reprojects
    })
  }
  read_ca_nmfs <- function() {
    s <- glue("{dir_raw}/fisheries.noaa.gov/core-areas/shapefile_Rices_whale_core_distribution_area_Jun19_SERO/shapefile_Rices_whale_core_distribution_area_Jun19_SERO.shp")
    st_read(s, quiet = TRUE) |> transmute(mdl_key = "ca_nmfs|Balaenoptera_ricei")
  }
  read_ch_fws <- function() {
    x <- st_read(glue("{dir_raw}/fws.gov/crithab_all_layers/crithab_poly.shp"), quiet = TRUE)
    names(x) <- tolower(names(x)); x |> transmute(mdl_key = glue("ch_fws|{spcode}"))
  }
  read_ch_nmfs <- function() {
    x <- st_read(glue("{dir_raw}/fisheries.noaa.gov/ply.gpkg"), layer = "ply", quiet = TRUE)
    x |> transmute(mdl_key = glue("ch_nmfs|{slug(SCIENAME)}"))
  }
  read_turtle <- function() {
    codes <- c("CC","CM","DC","EI","LK","LO")
    map_dfr(codes, function(cd) {
      s <- glue("{dir_raw}/swot_seamap.env.duke.edu/swot_distribution/Global_Distribution_{cd}.shp")
      if (!file_exists(s)) return(NULL)
      st_read(s, quiet = TRUE) |> transmute(mdl_key = glue("rng_turtle_swot_dps|{cd}")) |> st_zm(drop = TRUE)
    })
  }
  read_bl <- function(sisids = bl_sisids) {
    stopifnot("read_bl needs the sisids to read" = !is.null(sisids))
    gpkg <- c(glue("{dir_big}/../raw/BOTW_2024_2.gpkg"),
              glue("{dir_raw}/birdlife.org/BOTW_GPKG_2024_2/BOTW_2024_2.gpkg"))
    gpkg <- gpkg[file_exists(gpkg)][1]; stopifnot(!is.na(gpkg))
    ids  <- paste(unique(sub("^bl\\|", "", sisids)), collapse = ",")
    q    <- glue("SELECT sisid, geom FROM all_species WHERE sisid IN ({ids}) AND presence IN (1,2,3)")
    st_read(gpkg, query = q, quiet = TRUE) |> transmute(mdl_key = glue("bl|{sisid}"))
  }
  list(
    # fast/small first (seconds each), then bl (one local gpkg query), then rng_iucn
    # LAST (thousands of ranges from a 9 GB indexed gpkg -- minutes even warm).
    ca_nmfs = read_ca_nmfs, ch_fws = read_ch_fws, ch_nmfs = read_ch_nmfs,
    rng_turtle_swot_dps = read_turtle, rng_fws = read_fws_rng, bl = read_bl, rng_iucn = read_iucn)
}
