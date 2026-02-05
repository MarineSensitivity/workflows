#' run ingest aquamaps notebook
#' @return path to sdm duckdb file
run_ingest_aquamaps <- function() {
  quarto::quarto_render(
    here::here("ingest_aquamaps.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}

#' run ingest birdlife notebook
#' @return path to sdm duckdb file
run_ingest_birdlife <- function() {
  quarto::quarto_render(
    here::here("ingest_birdlife.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}

#' run ingest NMFS critical habitat
#' @return path to sdm duckdb file
run_ingest_ch_nmfs <- function() {
  quarto::quarto_render(
    here::here("ingest_ch-nmfs.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}

#' run ingest FWS critical habitat
#' @return path to sdm duckdb file
run_ingest_ch_fws <- function() {
  quarto::quarto_render(
    here::here("ingest_ch-fws.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}

#' run ingest FWS range maps
#' @return path to sdm duckdb file
run_ingest_rng_fws <- function() {
  quarto::quarto_render(
    here::here("ingest_rng-fws.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}

#' run ingest IUCN range maps
#' @return path to sdm duckdb file
run_ingest_rng_iucn <- function() {
  # rng_iucn ingest is part of merge_models.qmd currently
  msens::sdm_db_path()
}
