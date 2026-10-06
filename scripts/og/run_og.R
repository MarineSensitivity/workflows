#!/usr/bin/env Rscript
# driver for the OBIS "MPA Europe" SDM pipeline (iobis/mpaeu_sdm, branch msens-patches) on the Mac mini.
# run FROM the mpaeu_sdm clone (the pipeline reads data/, sdm_conf.yml and functions/ relative to it):
#   cd ~/Github/iobis/mpaeu_sdm
#   OG_SPECIES=159023 OG_ACRO=ogc OG_CORES=8 Rscript ~/Github/MarineSensitivity/workflows/scripts/og/run_og.R
#   OG_STEP=bootstrap OG_SPECIES=159023 OG_ACRO=ogc Rscript .../run_og.R       # 20 bootstrap refits -> what=bootcv_cog.tif
# env:
#   OG_SPECIES  comma-separated AphiaIDs (required)            OG_ACRO   ogc = control (OBIS settings), og = ours
#   OG_OUT      output root (default ~/_big/sdm/og)             OG_CORES  workers when > 1 species, default 8
#   OG_STEP     fit (default) | bootstrap                      OG_ALGOS  default maxent,rf,xgboost (smoke tests only)
#   OG_HYP_FROM / OG_FITOCC_FROM  = ~/_big/sdm/obis/species: pin the layer set / the fit presences to the published run (controls)
#   OG_SCENARIOS=all restores OBIS's 11 scenarios (default current only)
# one species runs sequentially inside model_fit.R (run_parallel is FALSE for a single id): launch one run per species.
home <- path.expand("~")
Sys.setenv(PATH = paste(file.path(home, ".local/bin"), "/opt/homebrew/bin", "/usr/local/bin", Sys.getenv("PATH"), sep = ":"))
if (!nzchar(Sys.getenv("TERM"))) Sys.setenv(TERM = "xterm")      # model_fit.R calls system("clear")

sp <- Sys.getenv("OG_SPECIES")
stopifnot("set OG_SPECIES" = nzchar(sp))
sel_species <- as.numeric(strsplit(sp, ",")[[1]])
outacro     <- Sys.getenv("OG_ACRO", "og")
outfolder   <- Sys.getenv("OG_OUT", file.path(home, "_big/sdm/og"))
step        <- Sys.getenv("OG_STEP", "fit")
Sys.setenv(OG_CORES = Sys.getenv("OG_CORES", "8"))

stopifnot("run from the mpaeu_sdm clone" = file.exists("codes/model_fit.R"),
          "data/ link missing: run scripts/og/fetch_pipeline_data.sh" = file.exists("data/env/terrain/bathymetry_mean.tif"),
          "obissdm not from the patched clone: scripts/og/install_obissdm.sh" =
            exists(".cvpred_on", asNamespace("obissdm")),
          "rio (rio-cogeo) not on PATH" = nzchar(Sys.which("rio")))
dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
cat("og:", step, "species", sp, "acro", outacro, "out", outfolder, "cores", Sys.getenv("OG_CORES"), "\n")

if (step == "fit") {
  source("codes/model_fit.R")
} else if (step == "bootstrap") {
  acro <- outacro; results_folder <- outfolder
  source("codes/post_bootstrap.R")
} else stop("OG_STEP must be fit or bootstrap")
