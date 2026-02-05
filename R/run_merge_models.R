#' run merge models notebook
#' @return path to sdm duckdb file
run_merge_models <- function(
    spp_db, listing_db,
    sdm_aquamaps, sdm_birdlife,
    sdm_ch_nmfs, sdm_ch_fws, sdm_rng_fws, sdm_rng_iucn) {
  quarto::quarto_render(
    here::here("merge_models.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}
