#' run ingest productivity data
#' @return path to productivity raster
run_ingest_productivity <- function() {
  dir_data <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "~/My Drive/projects/msens/data",
    "Linux"  = "/share/data")
  glue::glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
}
