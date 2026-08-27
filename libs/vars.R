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

# generic for the other ingest_*.qmd (vector ranges, raster SDMs): re-do this
# dataset's models (delete + rebuild) instead of resuming from existing Parquet
redo_ingest    <- .flag("REDO_INGEST")

# ingest_aquax.qmd (v9 `ax`)
ax_workers          <- as.integer(Sys.getenv("AX_WORKERS", "6"))   # furrr workers for TIF->Parquet / COGs
ax_test_n           <- as.integer(Sys.getenv("AX_TEST_N", "0"))    # smoke test: only the first n models
ax_cog              <- .flag("AX_COG")                # build the two COG representations
ax_cog_s3           <- .flag("AX_COG_S3")             # ...and upload them to the marine-atlas
ax_apply_cutoff     <- .flag("AX_APPLY_CUTOFF")       # zero CUR_NR below the model's TSS cutoff (D2; default off)
ax_absent_supersedes<- .flag("AX_ABSENT_SUPERSEDES")  # AquaX "modeled, absent in US" also supersedes am (D4; default off)
ax_supersede        <- .flag("AX_SUPERSEDE", default = TRUE)  # AX_SUPERSEDE=0 = control run (ax registered, nothing superseded)

# bootstrap_version.qmd
bootstrap_verify    <- .flag("BOOTSTRAP_VERIFY")      # re-hash cloned datasets against their ingest manifests
bootstrap_publish   <- .flag("BOOTSTRAP_PUBLISH")     # server-side copy of ver_prev's published native assets (S3 + file host)
