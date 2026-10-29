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
ax_cog_s3           <- .flag("AX_COG_S3")             # RETIRED 2026-10: the ingest refuses it; publish_native.qmd uploads
ax_apply_cutoff     <- .flag("AX_APPLY_CUTOFF")       # zero CUR_NR below the model's TSS cutoff (D2; default off)
ax_absent_supersedes<- .flag("AX_ABSENT_SUPERSEDES")  # AquaX "modeled, absent in US" also supersedes am (D4; default off)
ax_supersede        <- .flag("AX_SUPERSEDE", default = TRUE)  # AX_SUPERSEDE=0 = control run (ax registered, nothing superseded)

# bootstrap_version.qmd
bootstrap_verify    <- .flag("BOOTSTRAP_VERIFY")      # re-hash cloned datasets against their ingest manifests
bootstrap_publish   <- .flag("BOOTSTRAP_PUBLISH")     # server-side copy of ver_prev's published native assets (S3 + file host)
bootstrap_clone_merge <- .flag("BOOTSTRAP_CLONE_MERGE")  # a SCORING-ONLY version (v10): also clone ver_prev's merge + scoring outputs

# ingest_nmfs-dps.qmd (v9.1 `dps_nmfs`)
dps_fetch <- .flag("DPS_FETCH")   # re-download the NMFS critical-habitat service layers into raw/

# build_taxon_global_total.qmd (v10 P2)
redo_denom <- .flag("REDO_DENOM")  # build_taxon_global_total.qmd: rebuild taxon_denom

# ingest_productivity.qmd (v11 Q4)
redo_vgpm_download <- .flag("REDO_VGPM_DOWNLOAD")  # ingest_productivity.qmd: re-download + re-untar the OSU monthly VGPM tarballs

# ingest_obis-occurrence.qmd (v11 Q1)
occ_buffer_km <- as.numeric(Sys.getenv("OCC_BUFFER_KM", "100"))  # rng_occ: cells within d km of a cell with >= 1 OBIS record

# v11 method flags (plan 2026-10-29 §4, §4.10 Q0): every flag OFF reproduces v10 — the control run
# `scripts/run_version.sh --control` is every flag off and its digests must equal v10's. Each flag is
# read HERE and consumed by the stage it enters (named in the comment); a stage that does not yet
# implement its flag asserts it is off, so a flag can never be silently ignored.
vgpm_integrated <- .flag("VGPM_INTEGRATED")                        # ingest_productivity: year integrated, polar night = 0 (Q4)
turtle_suit     <- match.arg(Sys.getenv("TURTLE_SUIT", "am"), c("am", "ogm"))      # merge_models_prep/merge_models: turtle suitability source (Q3)
turtle_rule     <- match.arg(Sys.getenv("TURTLE_RULE", "turtle"), c("turtle", "dps"))  # merge_models: turtles through dps_sql (Q3)
occ_rule        <- .flag("OCC_RULE")                               # merge_models: msens::occurrence_rule() zone exclusions (Q2)
region_weight   <- .flag("REGION_WEIGHT")                          # merge_models: msens::encounter_weight() per species x ecoregion (Q3)
fold_density    <- .flag("FOLD_DENSITY")                           # merge: density datasets replace suitability inside their domain (Q6)
worms_source    <- match.arg(Sys.getenv("WORMS_SOURCE", "download"), c("download", "obisindicators"))  # ingest_worms (Q1)
components_v11  <- .flag("COMPONENTS_V11")                         # data/components.csv labels: Flora / Productivity (Q5)
v11_flags <- list(VGPM_INTEGRATED = vgpm_integrated, TURTLE_SUIT = turtle_suit, TURTLE_RULE = turtle_rule,
                  OCC_RULE = occ_rule, REGION_WEIGHT = region_weight, FOLD_DENSITY = fold_density,
                  WORMS_SOURCE = worms_source, COMPONENTS_V11 = components_v11)
v11_flags_off <- identical(v11_flags, list(VGPM_INTEGRATED = FALSE, TURTLE_SUIT = "am", TURTLE_RULE = "turtle",
                                           OCC_RULE = FALSE, REGION_WEIGHT = FALSE, FOLD_DENSITY = FALSE,
                                           WORMS_SOURCE = "download", COMPONENTS_V11 = FALSE))
