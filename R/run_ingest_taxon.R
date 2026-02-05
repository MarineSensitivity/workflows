#' run ingest taxon notebook
#' @return path to spp duckdb file
run_ingest_taxon <- function() {
  # ingest_taxon.qmd builds spp.duckdb
  quarto::quarto_render(
    here::here("ingest_taxon.qmd"),
    quiet = TRUE)
  dir_data <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "~/My Drive/projects/msens/data",
    "Linux"  = "/share/data")
  glue::glue("{dir_data}/derived/spp.duckdb")
}
