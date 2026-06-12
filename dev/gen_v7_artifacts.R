# gen_v7_artifacts.R — generate the v7 derived artifacts the apps load at startup.
# Geometry files (gpkgs, subregion csv) are copied from v6 in shell (unchanged);
# this script regenerates the version-specific layers CSV and the r_metrics tif
# (zone-mask layers + per-metric rasters) from the v7 DB, mirroring calc_scores.qmd.

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(dbplyr)
  library(glue); library(stringr); library(readr); library(terra)
})

ver      <- "v7"
dir_data <- path.expand("~/My Drive/projects/msens/data")
dir_v    <- file.path(dir_data, "derived", ver)
dir.create(dir_v, showWarnings = FALSE, recursive = TRUE)
cell_tif <- file.path(dir_data, "derived", "r_bio-oracle_planarea.tif")
sdm_db   <- path.expand("~/_big/msens/derived/v7/sdm.duckdb")
stopifnot(file.exists(cell_tif), file.exists(sdm_db))

con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = TRUE)
on.exit(dbDisconnect(con_sdm, shutdown = TRUE), add = TRUE)

r_cell <- rast(cell_tif)
ext(r_cell) <- round(ext(r_cell), 3)

sp_cats <- tbl(con_sdm, "taxon") |> filter(is_ok) |> distinct(sp_cat) |>
  arrange(sp_cat) |> pull(sp_cat)

get_rast <- function(m_key) {
  d <- dbGetQuery(con_sdm, glue(
    "SELECT cm.cell_id, cm.value FROM cell_metric cm
     WHERE cm.metric_seq = (SELECT metric_seq FROM metric WHERE metric_key = '{m_key}')"))
  stopifnot(sum(duplicated(d$cell_id)) == 0)
  r <- init(r_cell[[1]], NA); r[d$cell_id] <- d$value; r
}

# --- layers CSV --------------------------------------------------------------
lyrs_csv <- file.path(dir_v, glue("layers_{ver}.csv"))
d_lyrs <- bind_rows(
  tibble(order = 1, category = "Overall", layer = "score",
         lyr = "score_extriskspcat_primprod_ecoregionrescaled_equalweights"),
  tibble(order = 2, category = "Species, rescaled by Ecoregion",
         layer = glue("{sp_cats}: ext. risk, ecorgn"),
         lyr = glue("extrisk_{sp_cats}_ecoregion_rescaled")),
  tibble(order = 3, category = "Primary Productivity, rescaled by Ecoregion",
         layer = glue("prim prod, ecorgn"), lyr = glue("primprod_ecoregion_rescaled")),
  tibble(order = 4, category = "Species, raw Extinction Risk",
         layer = glue("{sp_cats}: ext. risk"), lyr = glue("extrisk_{sp_cats}")),
  tibble(order = 5, category = "Primary Productivity, raw Phytoplankton",
         lyr = "primprod", layer = "prim prod, 2014-2023 avg (mg C/m^2/day)"))
write_csv(d_lyrs, lyrs_csv)
message(glue("wrote {lyrs_csv} ({nrow(d_lyrs)} layers)"))

# --- r_metrics tif (zone masks + metric rasters) -----------------------------
metrics_tif <- file.path(dir_v, glue("r_metrics_{ver}.tif"))

lst <- list()
for (zone_fld in c("ecoregion_key", "programarea_key")) {
  d <- tbl(con_sdm, "zone") |>
    filter(fld == !!zone_fld) |>
    left_join(tbl(con_sdm, "zone_cell"), by = "zone_seq") |>
    group_by(cell_id) |>
    window_order(pct_covered) |>
    summarize(value = last(value), .groups = "drop") |>
    collect() |>
    mutate("{zone_fld}" := as.factor(value)) |>
    select(all_of(c("cell_id", zone_fld)))
  lvls <- levels(d[[zone_fld]])
  r <- init(r_cell[[1]], NA)
  r[d$cell_id] <- d[[zone_fld]]
  varnames(r) <- zone_fld; names(r) <- zone_fld
  levels(r) <- tibble(id = seq_along(lvls), "{zone_fld}" := lvls)
  lst[[zone_fld]] <- r
}
r_zones <- do.call(c, lst |> unname())

lst <- list()
for (i in 1:nrow(d_lyrs)) {
  lyr <- d_lyrs$lyr[i]; layer <- d_lyrs$layer[i]
  r <- get_rast(lyr); names(r) <- lyr; varnames(r) <- layer
  lst[[i]] <- r
}
r_metrics <- do.call(c, lst)
r_metrics[["primprod_log"]] <- log(r_metrics[["primprod"]])
r_metrics <- c(r_zones, r_metrics)
print(sort(names(r_metrics)))
writeRaster(r_metrics, metrics_tif, overwrite = TRUE)
message(glue("wrote {metrics_tif} ({nlyr(r_metrics)} layers)"))
message("gen_v7_artifacts.R complete.")
