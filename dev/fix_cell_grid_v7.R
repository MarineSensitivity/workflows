# fix_cell_grid_v7.R — backfill the `cell` table with modeled-but-missing cells.
#
# ~703 cells have range-map model values (model_cell) but no Bio-Oracle environmental
# data, so they were excluded from `cell`. Because zone_taxon inner-joins `cell` for
# area_km2, the species whose only cells fall there were dropped (FULL 16,143 vs the
# 16,158 is_ok count; PRA 9,226 vs 9,230 — the 4 birds). This inserts those cells into
# `cell` with their geometric area (from the cell raster), refreshes the FULL zone's
# cells, and recomputes zone_taxon. Scores (zone_metric) are unaffected — they aggregate
# cell_metric, which already includes these cells. Portable + idempotent.

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(dbplyr)
  library(glue); library(stringr); library(terra); library(readr)
})

is_server <- Sys.info()[["sysname"]] == "Linux"
dir_big   <- if (is_server) "/share/data/big" else path.expand("~/_big/msens/derived")
dir_data  <- if (is_server) "/share/data"     else path.expand("~/My Drive/projects/msens/data")
ver <- "v7"
sdm_db   <- file.path(dir_big, ver, "sdm.duckdb")
cell_tif <- file.path(dir_data, "derived", "r_bio-oracle_planarea.tif")
qmd_path <- file.path(if (is_server) "/share/github/MarineSensitivity/workflows" else
                      "/Users/bbest/Github/MarineSensitivity/workflows", "calc_scores.qmd")
tbl_sr <- glue("ply_subregions_2026_{ver}")
stopifnot(file.exists(sdm_db), file.exists(cell_tif), file.exists(qmd_path))

con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = FALSE)
on.exit(dbDisconnect(con_sdm, shutdown = TRUE), add = TRUE)

# 1) backfill `cell` with modeled-but-missing cells (geometric area from the raster) --------
missing <- dbGetQuery(con_sdm,
  "SELECT DISTINCT cell_id FROM model_cell WHERE cell_id NOT IN (SELECT cell_id FROM cell)")
message(glue("cells in model_cell missing from cell: {nrow(missing)}"))
if (nrow(missing) > 0) {
  r <- rast(cell_tif)
  area_all <- terra::values(terra::cellSize(r[[1]], unit = "km", mask = FALSE))[, 1]
  d_add <- data.frame(cell_id = as.integer(missing$cell_id),
                      area_km2 = as.numeric(area_all[missing$cell_id]))
  stopifnot(!any(is.na(d_add$area_km2)))
  duckdb_register(con_sdm, "tmp_add_cell", d_add)
  dbExecute(con_sdm, "INSERT INTO cell (cell_id, area_km2) SELECT cell_id, area_km2 FROM tmp_add_cell")
  duckdb_unregister(con_sdm, "tmp_add_cell")
  message(glue("backfilled {nrow(d_add)} cells; cell now = {dbGetQuery(con_sdm,'SELECT count(*) n FROM cell')$n}"))
}

# 1b) refine is_ok: a valid species must have an actual mapped distribution (>=1 model_cell).
# A few taxa carry a stale mdl_seq with no model_cell rows (empty merged model); exclude them
# so the is_ok count matches the species actually mapped (zone_taxon / species table).
n_empty <- dbExecute(con_sdm,
  "UPDATE taxon SET is_ok = FALSE
   WHERE is_ok AND mdl_seq NOT IN (SELECT DISTINCT mdl_seq FROM model_cell WHERE mdl_seq IS NOT NULL)")
message(glue("is_ok set FALSE for {n_empty} taxa with mdl_seq but no model_cell"))

# 2) refresh the FULL study-area zone's cells to cover every cell --------------------------
full_seq <- dbGetQuery(con_sdm, glue(
  "SELECT zone_seq FROM zone WHERE tbl='{tbl_sr}' AND fld='subregion_key' AND value='FULL'"))$zone_seq
stopifnot(length(full_seq) == 1)
dbExecute(con_sdm, glue("DELETE FROM zone_cell WHERE zone_seq = {full_seq}"))
dbExecute(con_sdm, glue(
  "INSERT INTO zone_cell (zone_seq, cell_id, pct_covered) SELECT {full_seq}, cell_id, 100 FROM cell"))

# 3) recompute zone_taxon (canonical chunk; overwrite) — covers PRA + subregion (incl FULL) + ecoregion
tbl_pra <- glue("ply_programareas_2026_{ver}")
tbl_er  <- "ply_ecoregions_2025"
sp_cats <- tbl(con_sdm, "taxon") |> filter(is_ok) |> distinct(sp_cat) |> arrange(sp_cat) |> pull(sp_cat)
zone_taxon_csv <- file.path(dir_big, ver, glue("zone_taxon_{ver}.csv"))
qmd_lines <- readLines(qmd_path)
get_chunk <- function(label) {
  lab <- grep(paste0("^#\\|\\s*label:\\s*", label, "\\s*$"), qmd_lines); stopifnot(length(lab) == 1)
  open  <- max(grep("^```\\{", qmd_lines[seq_len(lab)]))
  close <- lab + grep("^```\\s*$", qmd_lines[(lab + 1):length(qmd_lines)])[1]
  code  <- qmd_lines[(open + 1):(close - 1)]
  code  <- paste(code[!grepl("^#\\|", code)], collapse = "\n")
  # ensure the FULL subregion is included even if the source isn't yet updated
  gsub("c(tbl_sr, tbl_pra)", "c(tbl_sr, tbl_pra, tbl_er)", code, fixed = TRUE)
}
message("=== recomputing zone_taxon ===")
eval(parse(text = get_chunk("zone_taxon")), envir = globalenv())

# 4) refresh FULL zone_metric (coverage-weighted over all cells; targeted, leaves others alone)
dbExecute(con_sdm, glue("DELETE FROM zone_metric WHERE zone_seq = {full_seq}"))
dbExecute(con_sdm, glue("
  INSERT INTO zone_metric (zone_seq, metric_seq, value)
  SELECT zc.zone_seq, cm.metric_seq, SUM(cm.value*zc.pct_covered)/SUM(zc.pct_covered)
  FROM zone_cell zc JOIN cell_metric cm ON zc.cell_id=cm.cell_id
  WHERE zc.zone_seq={full_seq} AND cm.value IS NOT NULL
  GROUP BY zc.zone_seq, cm.metric_seq"))

# 5) validate ------------------------------------------------------------------------------
message("\n=== VALIDATION ===")
print(dbGetQuery(con_sdm,
  "SELECT zone_fld, zone_value, count(DISTINCT mdl_seq) spp FROM zone_taxon
   WHERE zone_value IN ('FULL','USA') GROUP BY zone_fld, zone_value ORDER BY zone_value"))
print(dbGetQuery(con_sdm,
  "SELECT count(DISTINCT mdl_seq) AS pra_zone_taxon FROM zone_taxon WHERE zone_fld='programarea_key'"))
print(dbGetQuery(con_sdm, "SELECT count(*) FILTER (WHERE is_ok) AS is_ok FROM taxon"))
message("fix_cell_grid_v7.R complete.")
