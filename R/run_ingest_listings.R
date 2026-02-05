#' run ingest listings notebook
#' @return path to sdm duckdb file
run_ingest_listings <- function(spp_db) {
  quarto::quarto_render(
    here::here("ingest_nmfs-fws-mmpa-mbta-listings.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}
