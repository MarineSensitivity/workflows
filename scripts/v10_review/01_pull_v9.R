# 01 - pull the v9 tables the review reads: zone_metric (long), zone_taxon (program areas,
# subregions, ecoregions), taxon, zone areas, Program Area x ecoregion composition.
suppressMessages({library(DBI); library(duckdb); library(dplyr); library(glue)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); VER <- Sys.getenv("VER", "v9")
source(file.path(repo, "libs/paths.R"))                     # dir_big, dir_derived (laptop / server aware)
sdm_v <- path.expand(glue("{dir_big}/{VER}/sdm.duckdb"))
stopifnot(file.exists(sdm_v))
con <- dbConnect(duckdb(), sdm_v, read_only = TRUE)

zm <- dbGetQuery(con, "SELECT z.fld, z.val AS zone, m.metric_key, zm.val
                       FROM zone_metric zm JOIN zone z USING (zone_seq) JOIN metric m USING (metric_seq)")
write.csv(zm, file.path(S, "v9_zone_metric_long.csv"), row.names = FALSE)
dups <- zm |> filter(fld == "programarea_key") |> count(zone, metric_key) |> filter(n > 1)
stopifnot(nrow(dups) == 0)

zt <- dbGetQuery(con, "SELECT zone_fld, zone_value, sp_cat, sp_common, sp_scientific, taxon_id, er_score, area_km2,
                              avg_suit, suit_er, suit_er_area
                       FROM zone_taxon WHERE zone_fld IN ('programarea_key','subregion_key','ecoregion_key')")
write.csv(zt, file.path(S, "v9_zone_taxon.csv"), row.names = FALSE)

tx <- dbGetQuery(con, "SELECT taxon_id, ms_merge_key, scientific_name, common_name, er_score AS er_taxon, extrisk_code,
                              er_mode, is_valid_usa, is_valid_pra, sp_cat AS sp_cat_taxon, n_usa, n_global, range_km2, range_usa_km2
                       FROM taxon")
write.csv(tx, file.path(S, "v9_taxon.csv"), row.names = FALSE)

# zone_taxon.suit_er_area = sum(val/100 * area * pct_covered/100) x er/100 (coverage-weighted over the zone's
# cells) for taxon-mode species: verify on the largest one against model_cell
usa <- zt |> filter(zone_fld == "subregion_key", zone_value == "USA") |> inner_join(tx, by = "taxon_id")
ex  <- usa |> filter(er_mode == "taxon", er_taxon >= 50) |> arrange(desc(area_km2)) |> slice(1)
v   <- dbGetQuery(con, sprintf("SELECT sum(mc.val/100.0 * c.area_km2 * zc.pct_covered/100.0) suit_area
                                FROM model_cell mc JOIN cell c USING (cell_id) JOIN zone_cell zc USING (cell_id) JOIN zone z USING (zone_seq)
                                WHERE z.fld = 'subregion_key' AND z.val = 'USA' AND mc.mdl_key = '%s'", ex$ms_merge_key))
stopifnot(abs(v$suit_area * ex$er_taxon / 100 - ex$suit_er_area) / ex$suit_er_area < 1e-6)

# the analytic cell-id -> latitude band -> area formula used by 02/03 (sphere, R = 6371 km) must track
# cell.area_km2 (terra::cellSize, ellipsoidal): it does to within 0.9 % at the pole and 0.45 % at the
# equator. The page's global denominators carry that approximation; the pipeline (v10-1) uses cell.area_km2.
cc <- dbGetQuery(con, "SELECT cell_id, area_km2 FROM cell WHERE cell_id IN (1, 7200, 7201, 12960000, 25920000, 3600*7200)")
cc$area_f <- 40589641.0 * (0.05 * pi / 180) *
  (sin((90 - floor((cc$cell_id - 1) / 7200) * 0.05) * pi / 180) - sin((90 - (floor((cc$cell_id - 1) / 7200) + 1) * 0.05) * pi / 180))
stopifnot(max(abs(cc$area_f - cc$area_km2) / cc$area_km2) < 1e-2)

eco <- dbGetQuery(con, "
  WITH pa AS (SELECT z.val AS pa, zc.cell_id, zc.pct_covered FROM zone_cell zc JOIN zone z USING (zone_seq) WHERE z.fld = 'programarea_key'),
       er AS (SELECT z.val AS eco, zc.cell_id, zc.pct_covered FROM zone_cell zc JOIN zone z USING (zone_seq) WHERE z.fld = 'ecoregion_key')
  SELECT pa.pa, er.eco, sum(pa.pct_covered/100.0 * c.area_km2) AS area_km2
  FROM pa JOIN er USING (cell_id) JOIN cell c USING (cell_id) GROUP BY 1,2 ORDER BY 1, 3 DESC")
za <- dbGetQuery(con, "SELECT z.fld, z.val AS zone, sum(zc.pct_covered/100.0 * c.area_km2) AS area_km2, count(*) n_cells
                       FROM zone_cell zc JOIN zone z USING (zone_seq) JOIN cell c USING (cell_id) GROUP BY 1,2 ORDER BY 1,2")
write.csv(za,  file.path(S, "v9_zone_area.csv"),    row.names = FALSE)
write.csv(eco, file.path(S, "v9_pa_ecoregion.csv"), row.names = FALSE)
cat("zone_metric rows:", nrow(zm), " zone_taxon rows:", nrow(zt), " taxa:", nrow(tx), "\n")
dbDisconnect(con, shutdown = TRUE)
