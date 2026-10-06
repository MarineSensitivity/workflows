# Bio-ORACLE layers the og species need (current period only), via obissdm::get_env_data() = the call OBIS used
# (mpaeu_sdm/codes/get_env_data.R), narrowed to the variables the 'others' and 'mammals' groups read, for BOTH habitat
# depths so that surface-for-all and depth-averaged-turtle runs are covered. skip_exist = TRUE -> idempotent.
#   Rscript scripts/og/fetch_env_bio_oracle.R           (OG_DATA = ~/_big/sdm/mpaeu)
# files land as data/env/current/{var}_baseline_{depth}_{variant}.tif, data/env/terrain/{bathymetry_mean,rugosity}.tif
# A (dataset, depth) that Bio-ORACLE does not serve (e.g. sws/siconc depthmean) is skipped; get_envofgroup() then falls
# back to the surface layer, exactly as it did for OBIS.
suppressMessages({library(obissdm); library(terra)})
dir_data <- Sys.getenv("OG_DATA", file.path(path.expand("~"), "_big/sdm/mpaeu"))
outdir   <- file.path(dir_data, "data/env")
dir.create(file.path(dir_data, "data/env"), recursive = TRUE, showWarnings = FALSE)

time_steps <- list(current = c("2000-01-01T00:00:00Z", "2010-01-01T00:00:00Z"))

# dataset id (surface) -> variants needed. thetao_mean is read by the post-evaluation (thermal envelope)
need <- list(
  thetao_baseline_2000_2019_depthsurf  = c("max", "range", "mean"),
  so_baseline_2000_2019_depthsurf      = "min",
  o2_baseline_2000_2018_depthsurf      = "min",
  sws_baseline_2000_2019_depthsurf     = "max",
  siconc_baseline_2000_2020_depthsurf  = "max")

for (id in names(need)) {
  for (d in c(id, sub("depthsurf", "depthmean", id))) {
    message("== ", d, " ", paste(need[[id]], collapse = ","))
    try(get_env_data(datasets = d, future_scenarios = NULL, time_steps = time_steps,
                     variables = need[[id]], outdir = outdir, average_time = TRUE))
  }
}

# terrain (static)
try(get_env_data(datasets = NULL, terrain_vars = c("bathymetry_mean", "terrain_ruggedness_index"), outdir = outdir))
rugg <- file.path(outdir, "terrain/terrain_ruggedness_index.tif")
if (file.exists(rugg)) {
  r <- rast(rugg); names(r) <- "rugosity"
  writeRaster(r, file.path(outdir, "terrain/rugosity.tif"), overwrite = TRUE); file.remove(rugg)
}
unlink(file.path(outdir, "raw"), recursive = TRUE)

# report: what exists, and a grid check against the S3 terrain layers
lf <- list.files(outdir, recursive = TRUE, pattern = "tif$")
message(length(lf), " layers: ", paste(lf, collapse = " "))
ref <- rast(file.path(outdir, "terrain/wavefetch.tif"))
for (f in file.path(outdir, lf)) {
  r <- rast(f)
  if (!isTRUE(all.equal(as.vector(ext(r)), as.vector(ext(ref)), tolerance = 1e-6)) || any(res(r) != res(ref)))
    message("GRID MISMATCH vs wavefetch.tif: ", basename(f), " ", paste(round(as.vector(ext(r)), 3), collapse = ","), " res ", res(r)[1])
}
