# vars.R — manual re-run flags for expensive pipeline chunks ----
#
# Sourced by workflow notebooks AFTER libs/paths.R. Each flag controls whether an
# expensive, cached step rebuilds. Default FALSE = skip when the output already
# exists (resume); set TRUE to force a rebuild. This lets a chunk be re-run on its
# own without editing the notebook, and keeps `quarto render` / `tar_make` cheap
# once the heavy artifacts exist.
#
# Override per-run from the environment without editing this file, e.g.:
#   REDO_AM_INGEST=1 Rscript -e 'quarto::quarto_render("ingest_aquamaps.qmd")'

.flag <- function(env, default = FALSE) {
  v <- Sys.getenv(env, unset = NA)
  if (is.na(v) || v == "") default else tolower(v) %in% c("1", "true", "yes", "t")
}

# build_cell_grid.qmd
redo_cell_grid <- .flag("REDO_CELL_GRID")  # rebuild cell table + cellid COG

# ingest_aquamaps.qmd
redo_am_w05    <- .flag("REDO_AM_W05")     # rebuild the AquaMaps bilinear weight table (w05)
redo_am_ingest <- .flag("REDO_AM_INGEST")  # re-interpolate ALL species (else resume: keep existing Parquet)
