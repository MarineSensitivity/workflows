# add_full_zone.R — add a "FULL" study-area zone (all cells) to a v7+ sdm DB.
#
# The apps' "Full study area" selection previously fell back to the "USA" subregion
# zone, which only covers Program-Area cells (9,226 species / Set B). This adds a
# real all-cells zone (subregion_key = 'FULL') with its own zone_taxon (species
# table -> 16,158) and zone_metric (flower plot / score), computed ONLY for this
# zone so the pct-area-weighted Program-Area metrics are left untouched.
# Portable + idempotent.

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(dbplyr); library(glue)
})

is_server <- Sys.info()[["sysname"]] == "Linux"
dir_big   <- if (is_server) "/share/data/big" else path.expand("~/_big/msens/derived")
ver <- "v7"
sdm_db  <- file.path(dir_big, ver, "sdm.duckdb")
tbl_sr  <- glue("ply_subregions_2026_{ver}")
stopifnot(file.exists(sdm_db))

con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = FALSE)
on.exit(dbDisconnect(con_sdm, shutdown = TRUE), add = TRUE)

# 1) ensure the FULL zone exists (zone + zone_cell = every cell) ------------------------------
ex <- dbGetQuery(con_sdm, glue(
  "SELECT zone_seq FROM zone WHERE tbl='{tbl_sr}' AND fld='subregion_key' AND value='FULL'"))
if (nrow(ex) == 0) {
  dbExecute(con_sdm, glue(
    "INSERT INTO zone (tbl, fld, value) VALUES ('{tbl_sr}','subregion_key','FULL')"))
  full_seq <- dbGetQuery(con_sdm, glue(
    "SELECT zone_seq FROM zone WHERE tbl='{tbl_sr}' AND fld='subregion_key' AND value='FULL'"))$zone_seq
  dbExecute(con_sdm, glue(
    "INSERT INTO zone_cell (zone_seq, cell_id, pct_covered) SELECT {full_seq}, cell_id, 100 FROM cell"))
  message(glue("created FULL zone_seq={full_seq} with {dbGetQuery(con_sdm, glue('SELECT count(*) n FROM zone_cell WHERE zone_seq={full_seq}'))$n} cells"))
} else {
  full_seq <- ex$zone_seq
  message(glue("FULL zone already exists (zone_seq={full_seq})"))
}

# 2) zone_metric for FULL: coverage-weighted average of every cell_metric over all cells ------
dbExecute(con_sdm, glue("DELETE FROM zone_metric WHERE zone_seq = {full_seq}"))
dbExecute(con_sdm, glue("
  INSERT INTO zone_metric (zone_seq, metric_seq, value)
  SELECT zc.zone_seq, cm.metric_seq,
         SUM(cm.value * zc.pct_covered) / SUM(zc.pct_covered) AS value
  FROM zone_cell zc JOIN cell_metric cm ON zc.cell_id = cm.cell_id
  WHERE zc.zone_seq = {full_seq} AND cm.value IS NOT NULL
  GROUP BY zc.zone_seq, cm.metric_seq"))
message(glue("zone_metric FULL: {dbGetQuery(con_sdm, glue('SELECT count(*) n FROM zone_metric WHERE zone_seq={full_seq}'))$n} metrics"))

# 3) zone_taxon for FULL: one row per valid species aggregated over all cells -----------------
dbExecute(con_sdm, glue(
  "DELETE FROM zone_taxon WHERE zone_tbl='{tbl_sr}' AND zone_fld='subregion_key' AND zone_value='FULL'"))

tbl_taxon <- tbl(con_sdm, "taxon") |>
  filter(is_ok) |>
  select(sp_cat, sp_common = common_name, sp_scientific = scientific_name,
         taxon_id, taxon_authority, rl_code = extrisk_code, er_score, is_er_spatial,
         is_mmpa, is_mbta, is_bcc, esa_code, esa_source, mdl_seq)

d <- tbl(con_sdm, "zone") |>
  filter(zone_seq == !!full_seq) |>
  select(zone_seq, zone_tbl = tbl, zone_fld = fld, zone_value = value) |>
  inner_join(tbl(con_sdm, "zone_cell") |> select(zone_seq, cell_id), by = "zone_seq") |>
  inner_join(
    tbl(con_sdm, "model_cell") |> select(mdl_seq, cell_id, value) |>
      inner_join(tbl_taxon, by = "mdl_seq"),
    by = "cell_id") |>
  inner_join(tbl(con_sdm, "cell") |> select(cell_id, area_km2), by = "cell_id") |>
  group_by(zone_tbl, zone_fld, zone_value, mdl_seq, sp_cat, sp_common, sp_scientific,
           taxon_id, taxon_authority, rl_code, er_score, is_er_spatial,
           is_mmpa, is_mbta, is_bcc, esa_code, esa_source) |>
  summarize(area_km2 = sum(area_km2, na.rm = TRUE),
            avg_suit = mean(value, na.rm = TRUE) / 100, .groups = "drop") |>
  collect() |>
  mutate(suit_rl = ifelse(is_er_spatial, avg_suit, avg_suit * er_score / 100),
         suit_rl_area = suit_rl * area_km2) |>
  group_by(zone_fld, zone_value, sp_cat) |>
  mutate(cat_suit_rl_area = sum(suit_rl_area, na.rm = TRUE)) |>
  ungroup() |>
  mutate(pct_cat = suit_rl_area / cat_suit_rl_area)

dbWriteTable(con_sdm, "zone_taxon", d, append = TRUE)

# 4) validate --------------------------------------------------------------------------------
print(dbGetQuery(con_sdm,
  "SELECT zone_fld, zone_value, count(DISTINCT mdl_seq) spp FROM zone_taxon
   WHERE zone_value IN ('FULL','USA') GROUP BY zone_fld, zone_value ORDER BY zone_value"))
message("add_full_zone.R complete.")
