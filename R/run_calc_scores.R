#' run calc scores notebook
#' @return path to sdm duckdb file
run_calc_scores <- function(taxon_db, rast_prod) {
  quarto::quarto_render(
    here::here("calc_scores.qmd"),
    quiet = TRUE)
  msens::sdm_db_path()
}
