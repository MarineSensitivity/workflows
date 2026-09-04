# 07 - assemble v9_data.json: Program Area x category tables (v9 published, cell-level v9 variants, v10 share
# metrics with US and global denominators, densities, fallback application), composites, top species, turtles,
# category totals, study-area constants. Read by the page template through 09/10.
suppressMessages({library(DBI); library(duckdb); library(dplyr); library(tidyr); library(jsonlite); library(sf)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); VER <- Sys.getenv("VER", "v9")
source(file.path(repo, "libs/paths.R"))
sdm_v <- path.expand(glue::glue("{dir_big}/{VER}/sdm.duckdb"))
con <- dbConnect(duckdb(), sdm_v, read_only = TRUE)
CATS <- c("bird","coral","fish","invertebrate","mammal","primary_producer","turtle")
ALL  <- c(CATS, "primprod")

# ---- inputs ----
zt <- read.csv(file.path(S, "v9_zone_taxon.csv"), colClasses = c(taxon_id = "character"))
tx <- read.csv(file.path(S, "v9_taxon.csv"), colClasses = c(taxon_id = "character"))
gl <- read.csv(file.path(S, "v9_global_totals.csv"), stringsAsFactors = FALSE)
am <- read.csv(file.path(S, "v9_am_totals.csv"), stringsAsFactors = FALSE)
za <- read.csv(file.path(S, "v9_zone_area.csv"))
eco_pa <- read.csv(file.path(S, "v9_pa_ecoregion.csv"))
zm <- read.csv(file.path(S, "v9_zone_metric_long.csv"))
g  <- st_drop_geometry(st_read(path.expand(ply_pra_gpkg), quiet = TRUE)) |>
  select(pa = programarea_key, name = programarea_name, region = region_key, region_name, planarea = planarea_key)
eco_names <- c(CAC = "California Current", CBS = "Chukchi and Beaufort Seas", EBS = "East Bering Sea", EGOA = "Eastern Gulf of America",
  GOA = "Gulf of Alaska", HAR = "High Arctic", NECS = "Northeast Continental Shelf", PIS = "Pacific Islands", PUR = "Puerto Rico and USVI",
  SECS = "Southeast Continental Shelf", WAOR = "Washington and Oregon", WCGOA = "Western and Central Gulf of America")  # docs/*.qmd names
pa_eco <- eco_pa |> group_by(pa) |> mutate(pct = 100 * area_km2 / sum(area_km2)) |> filter(pct >= 1) |>
  summarise(ecoregions = paste0(eco, ifelse(n() > 1, sprintf(" (%.0f%%)", pct), ""), collapse = " + "), eco_main = eco[which.max(pct)], .groups = "drop")
pas <- g |> left_join(pa_eco, by = "pa") |> left_join(za |> filter(fld == "programarea_key") |> select(pa = zone, area_km2), by = "pa") |> arrange(region, name)
pas$eco_main_name <- unname(eco_names[pas$eco_main])

# ---- per-species US totals (USA subregion row) and global totals ----
usa <- zt |> filter(zone_fld == "subregion_key", zone_value == "USA") |>
  select(taxon_id, sp_cat, sp_common, sp_scientific, er = er_score, area_us = area_km2, suit_er_area_us = suit_er_area) |>
  inner_join(tx |> select(taxon_id, ms_merge_key, er_mode, is_valid_pra, extrisk_code, n_global, range_km2), by = "taxon_id") |>
  left_join(gl |> select(ms_merge_key = mdl_key, gl_suit_area = suit_area_global), by = "ms_merge_key")
# am-only taxa: global = AquaMaps everywhere outside the US + the merged US surface inside (ax may supersede am there)
tm_am <- dbGetQuery(con, "SELECT mdl_key, taxon_id FROM taxon_model WHERE ds_key = 'am'")
am_tx <- am |> inner_join(tm_am, by = "mdl_key") |> group_by(taxon_id) |> summarise(am_all = sum(all_suit_area), am_us = sum(us_suit_area, na.rm = TRUE), .groups = "drop")
dps <- usa |> filter(er_mode == "cell")
dps_us <- dbGetQuery(con, sprintf("SELECT mc.mdl_key AS ms_merge_key, sum(mc.val/100.0 * c.area_km2) AS suit_area_us_mc FROM model_cell mc JOIN cell c USING (cell_id) WHERE c.in_usa AND mc.mdl_key IN (%s) GROUP BY 1",
  paste(sprintf("'%s'", dps$ms_merge_key), collapse = ",")))
fb <- read.csv(file.path(repo, "data/us_share_fallback.csv"), colClasses = c(taxon_id = "character"), stringsAsFactors = FALSE)
usa <- usa |> left_join(dps_us, by = "ms_merge_key") |> left_join(am_tx, by = "taxon_id") |> left_join(fb |> select(taxon_id, fb_fraction = fraction, fb_method = method, fb_basis = basis, fb_ds = ds), by = "taxon_id") |>
  mutate(w = ifelse(er_mode == "taxon", er, 1),
         # plain suitability-area in the US: taxon mode divides the scalar er back out; premultiplied (turtles) keeps the
         # risk-weighted surface (its global surface is premultiplied too); cell mode (16 NMFS DPS taxa) is read directly
         suit_area_us = case_when(er_mode == "taxon" ~ suit_er_area_us / er, er_mode == "cell" ~ suit_area_us_mc, TRUE ~ suit_er_area_us),
         # a taxon whose only datasets are national (FWS range, AquaX) has no global surface even when a merged
         # 'global' surface exists for it: that surface IS its US range. The fallback fraction takes precedence.
         global_src   = case_when(!is.na(fb_fraction) ~ paste0("fallback: ", fb_method), !is.na(gl_suit_area) ~ "merged_global", !is.na(am_all) ~ "aquamaps", TRUE ~ NA),
         suit_area_global = case_when(!is.na(fb_fraction) ~ suit_area_us / fb_fraction, !is.na(gl_suit_area) ~ gl_suit_area, !is.na(am_all) ~ pmax(am_all - am_us, 0) + suit_area_us, TRUE ~ NA),
         us_of_global = pmin(1, suit_area_us / suit_area_global))
cat("species with US totals:", nrow(usa), "| global source:\n"); print(table(usa$global_src, usa$er_mode, useNA = "ifany"))
cat("us_of_global summary:\n"); print(summary(usa$us_of_global))
print(usa |> filter(sp_cat == "turtle") |> select(sp_common, area_us, suit_area_us, suit_area_global, us_of_global))
usa <- usa |> filter(!is.na(suit_area_global))

# ---- per PA x species shares ----
pz <- zt |> filter(zone_fld == "programarea_key") |> select(pa = zone_value, taxon_id, area_pa = area_km2, avg_suit, suit_er_area_pa = suit_er_area) |>
  inner_join(usa, by = "taxon_id") |>
  mutate(share_us = pmin(1, suit_er_area_pa / suit_er_area_us),  # share of the species' US distribution (er cancels; premultiplied + cell: of the risk-weighted surface)
         share_gl = share_us * us_of_global,                      # share of the species' GLOBAL distribution
         contrib_us = w * share_us, contrib_gl = w * share_gl)
catw <- usa |> filter(sp_cat %in% CATS) |> group_by(sp_cat) |> summarise(n_sp = n(), n_pra = sum(is_valid_pra), sum_w = sum(w), .groups = "drop")
zone_share <- pz |> filter(sp_cat %in% CATS) |> group_by(pa, sp_cat) |>
  summarise(S_us = sum(contrib_us), S_gl = sum(contrib_gl), n_present = n(), mean_share_us = sum(contrib_us) / n(), .groups = "drop") |>
  left_join(catw, by = "sp_cat") |> left_join(pas |> select(pa, area_km2), by = "pa") |>
  mutate(pct_us = 100 * S_us / sum_w, pct_gl = 100 * S_gl / sum_w, dens_us = S_us / area_km2, dens_gl = S_gl / area_km2, pdens_us = pct_us / area_km2 * 1e5, pdens_gl = pct_gl / area_km2 * 1e5) |>
  group_by(sp_cat) |> mutate(share_us_rescaled = 100 * S_us / max(S_us), share_gl_rescaled = 100 * S_gl / max(S_gl),
                             dens_us_rescaled = 100 * dens_us / max(dens_us), dens_gl_rescaled = 100 * dens_gl / max(dens_gl),
                             mean_share_rescaled = 100 * mean_share_us / max(mean_share_us)) |> ungroup()

# ---- current (published) scores from zone_metric ----
cur <- zm |> filter(fld == "programarea_key") |>
  mutate(cat = case_when(metric_key == "primprod_ecoregion_rescaled" ~ "primprod",
                         metric_key == "score_extriskspcat_primprod_ecoregionrescaled_equalweights" ~ "composite",
                         grepl("^extrisk_.*_ecoregion_rescaled$", metric_key) ~ sub("^extrisk_(.*)_ecoregion_rescaled$", "\\1", metric_key), TRUE ~ NA)) |>
  filter(!is.na(cat)) |> select(pa = zone, cat, current = val)

# ---- cell-level rescaling variants; zone means are coverage-weighted over ALL zone cells (a cell with no value counts 0, as the pipeline's pct_area down-weight does) ----
sql <- "
CREATE TEMP TABLE pc AS SELECT z.val AS pa, zc.cell_id, zc.pct_covered/100.0 AS p FROM zone_cell zc JOIN zone z USING (zone_seq) WHERE z.fld = 'programarea_key';
CREATE TEMP TABLE ce AS SELECT zc.cell_id, z.val AS eco, zc.pct_covered * 1.0 / SUM(zc.pct_covered) OVER (PARTITION BY zc.cell_id) AS norm FROM zone_cell zc JOIN zone z USING (zone_seq) WHERE z.fld = 'ecoregion_key';
CREATE TEMP TABLE raw AS SELECT cm.cell_id, m.metric_key, cm.val FROM cell_metric cm JOIN metric m USING (metric_seq)
  WHERE m.metric_key IN ('extrisk_bird','extrisk_coral','extrisk_fish','extrisk_invertebrate','extrisk_mammal','extrisk_primary_producer','extrisk_turtle','primprod');
CREATE TEMP TABLE nat AS SELECT metric_key, min(val) mn, max(val) mx, quantile_cont(val, 0.01) p01, quantile_cont(val, 0.99) p99, count(*) n FROM raw JOIN cell c USING (cell_id) WHERE c.in_usa GROUP BY 1;
CREATE TEMP TABLE eco AS SELECT r.metric_key, ce.eco, min(val) mn, max(val) mx, quantile_cont(val, 0.01) p01, quantile_cont(val, 0.99) p99, count(*) n FROM raw r JOIN ce USING (cell_id) GROUP BY 1,2;
CREATE TEMP TABLE zarea AS SELECT pc.pa, sum(pc.p * c.area_km2) AS area FROM pc JOIN cell c USING (cell_id) GROUP BY 1;
"
for (stmt in strsplit(sql, ";")[[1]]) if (nzchar(trimws(stmt))) dbExecute(con, stmt)
cellv <- dbGetQuery(con, "
WITH x AS (
  SELECT pc.pa, r.metric_key, pc.p * c.area_km2 * ce.norm AS wgt, r.val,
    (r.val - n.mn)/(n.mx - n.mn)                                   AS nat_mm,
    least(1, greatest(0, (r.val - n.p01)/(n.p99 - n.p01)))         AS nat_pc,
    (r.val - e.mn)/(e.mx - e.mn)                                   AS eco_mm,
    least(1, greatest(0, (r.val - e.p01)/(e.p99 - e.p01)))         AS eco_pc
  FROM pc JOIN raw r USING (cell_id) JOIN cell c USING (cell_id) JOIN nat n ON n.metric_key = r.metric_key
  JOIN ce ON ce.cell_id = pc.cell_id JOIN eco e ON e.metric_key = r.metric_key AND e.eco = ce.eco)
SELECT x.pa, x.metric_key,
  100 * sum(wgt * nat_mm) / za.area AS nat_minmax, 100 * sum(wgt * nat_pc) / za.area AS nat_pctl,
  100 * sum(wgt * eco_mm) / za.area AS eco_minmax, 100 * sum(wgt * eco_pc) / za.area AS eco_pctl,
  sum(wgt * val) / za.area AS raw_mean_cell, max(val) AS raw_max_cell, sum(wgt) / za.area AS coverage
FROM x JOIN zarea za USING (pa) GROUP BY 1, 2, za.area")
cellv <- cellv |> mutate(cat = ifelse(metric_key == "primprod", "primprod", sub("^extrisk_", "", metric_key))) |> select(-metric_key)
natstats <- dbGetQuery(con, "SELECT * FROM nat ORDER BY 1"); ecostats <- dbGetQuery(con, "SELECT * FROM eco ORDER BY 1,2")

# ---- assemble PA x category table on the complete grid ----
grid <- expand.grid(pa = pas$pa, cat = ALL, stringsAsFactors = FALSE)
tab <- grid |> left_join(cur |> filter(cat != "composite"), by = c("pa","cat")) |> left_join(cellv, by = c("pa","cat")) |>
  left_join(zone_share |> select(pa, cat = sp_cat, S_us, S_gl, pct_us, pct_gl, pdens_us, pdens_gl, share_us_rescaled, share_gl_rescaled, dens_us_rescaled, dens_gl_rescaled, mean_share_rescaled, n_present), by = c("pa","cat"))
chk <- tab |> filter(!is.na(current)) |> summarise(n = n(), max_abs_diff = max(abs(current - eco_minmax)), cor = cor(current, eco_minmax))
cat("\ncheck: cell-level ecoregion min-max reproduces zone_metric current -> "); print(chk)
print(tab |> filter(!is.na(current)) |> mutate(d = abs(current - eco_minmax)) |> arrange(desc(d)) |> select(pa, cat, current, eco_minmax, coverage, d) |> head(5))
# primprod has no species share: the share-based composites use its national min-max instead of the ecoregional one
tab <- tab |> mutate(share_us_rescaled = ifelse(cat == "primprod", nat_minmax, share_us_rescaled),
                     share_gl_rescaled = ifelse(cat == "primprod", nat_minmax, share_gl_rescaled),
                     dens_us_rescaled = ifelse(cat == "primprod", nat_minmax, dens_us_rescaled),
                     dens_gl_rescaled = ifelse(cat == "primprod", nat_minmax, dens_gl_rescaled),
                     mean_share_rescaled = ifelse(cat == "primprod", nat_minmax, mean_share_rescaled))
# a category with cells in the zone but no zone_taxon rows cannot happen; a category with NO cells in the zone stays NA (the pipeline skips it in the composite)
absent <- tab |> filter(is.na(current)) |> select(pa, cat); cat("\nPA x category with no cells (skipped by the pipeline composite):\n"); print(absent)
tab <- tab |> mutate(across(c(share_us_rescaled, share_gl_rescaled, dens_us_rescaled, dens_gl_rescaled, mean_share_rescaled, pct_us, pct_gl, pdens_us, pdens_gl, S_us, S_gl), ~ifelse(is.na(.x) & !is.na(current), 0, .x)))
# composites: the pipeline rule = plain mean over the components present in the zone
mean_na <- function(x) mean(x, na.rm = TRUE)
comp <- tab |> group_by(pa) |> summarise(n_comp = sum(!is.na(current)), current = mean_na(current), nat_minmax = mean_na(nat_minmax), nat_pctl = mean_na(nat_pctl), eco_pctl = mean_na(eco_pctl),
                                         share_us = mean_na(share_us_rescaled), share_gl = mean_na(share_gl_rescaled), dens_us = mean_na(dens_us_rescaled), dens_gl = mean_na(dens_gl_rescaled), mean_share = mean_na(mean_share_rescaled), .groups = "drop")
cat("\ncomposite check vs pipeline composite: "); print(comp |> inner_join(cur |> filter(cat == "composite") |> select(pa, pipeline = current), by = "pa") |> summarise(max_abs_diff = max(abs(current - pipeline))))
rk <- function(x) rank(-x, ties.method = "min")
comp <- comp |> mutate(rank_current = rk(current), rank_share_us = rk(share_us), rank_share_gl = rk(share_gl), rank_dens_gl = rk(dens_gl), rank_nat_minmax = rk(nat_minmax), rank_eco_pctl = rk(eco_pctl), rank_mean_share = rk(mean_share))
spear <- list(share_us = cor(comp$current, comp$share_us, method = "spearman"), share_gl = cor(comp$current, comp$share_gl, method = "spearman"),
              dens_us = cor(comp$current, comp$dens_us, method = "spearman"), dens_gl = cor(comp$current, comp$dens_gl, method = "spearman"),
              nat_minmax = cor(comp$current, comp$nat_minmax, method = "spearman"), nat_pctl = cor(comp$current, comp$nat_pctl, method = "spearman"),
              eco_pctl = cor(comp$current, comp$eco_pctl, method = "spearman"), mean_share = cor(comp$current, comp$mean_share, method = "spearman"))
cat("\nSpearman vs current composite:\n"); print(round(unlist(spear), 3))
cat("\ncomposite table:\n"); print(comp |> left_join(pas |> select(pa, name), by = "pa") |> mutate(across(where(is.numeric), ~round(.x, 1))) |> arrange(rank_current) |> as.data.frame())
cat("\nturtle table:\n"); print(tab |> filter(cat == "turtle") |> left_join(pas |> select(pa, name), by = "pa") |> select(pa, name, current, nat_minmax, pct_us, pct_gl, share_gl_rescaled, dens_gl_rescaled, n_present) |> mutate(across(where(is.numeric), ~round(.x, 2))) |> arrange(desc(current)) |> as.data.frame())
cat("\nmammal table:\n"); print(tab |> filter(cat == "mammal") |> left_join(pas |> select(pa, name), by = "pa") |> select(pa, name, current, nat_minmax, pct_gl, share_gl_rescaled, dens_gl_rescaled) |> mutate(across(where(is.numeric), ~round(.x, 1))) |> arrange(desc(current)) |> as.data.frame())

# ---- top species contributions per PA x category ----
top <- pz |> filter(sp_cat %in% CATS) |> group_by(pa, sp_cat) |> arrange(desc(contrib_us)) |> mutate(rank_us = row_number()) |>
  arrange(desc(contrib_gl)) |> mutate(rank_gl = row_number()) |> filter(rank_us <= 8 | rank_gl <= 8) |> ungroup() |>
  transmute(pa, cat = sp_cat, sci = sp_scientific, common = sp_common, code = extrisk_code, er = round(er, 2), mode = er_mode,
            share_us = signif(share_us, 4), share_gl = signif(share_gl, 4), contrib_us = signif(contrib_us, 4), contrib_gl = signif(contrib_gl, 4),
            us_of_global = signif(us_of_global, 3), avg_suit = round(avg_suit, 3), area_pa = round(area_pa), global_src)
turt <- pz |> filter(sp_cat == "turtle") |> transmute(pa, sci = sp_scientific, common = sp_common, avg_suit = round(avg_suit, 3), area_pa = round(area_pa),
            share_us = signif(share_us, 4), share_gl = signif(share_gl, 4))
turt_sp <- usa |> filter(sp_cat == "turtle") |> transmute(sci = sp_scientific, common = sp_common, code = extrisk_code, area_us = round(area_us), suit_area_us = round(suit_area_us), suit_area_global = round(suit_area_global), us_of_global = signif(us_of_global, 3)) |>
  left_join(tx |> transmute(sci = scientific_name, us_of_global_cells = signif(n_usa / n_global, 3)), by = "sci")
catw <- catw |> left_join(usa |> filter(sp_cat %in% CATS) |> group_by(sp_cat) |> summarise(S_gl_usa = sum(w * us_of_global, na.rm = TRUE), .groups = "drop"), by = "sp_cat")
pp_usa_mean <- dbGetQuery(con, "SELECT sum(cm.val * c.area_km2) / sum(c.area_km2) AS m FROM cell_metric cm JOIN metric m USING (metric_seq) JOIN cell c USING (cell_id) WHERE m.metric_key = 'primprod' AND c.in_usa")$m
cats_out <- catw |> mutate(cat = sp_cat) |>
  left_join(tx |> filter(is_valid_usa) |> count(cat = sp_cat_taxon, name = "n_valid_usa"), by = "cat") |>
  left_join(pz |> filter(sp_cat %in% CATS) |> distinct(sp_cat, taxon_id) |> count(cat = sp_cat, name = "n_in_pa"), by = "cat") |>
  left_join(usa |> filter(sp_cat %in% CATS) |> group_by(cat = sp_cat) |> summarise(median_us_of_global = median(us_of_global, na.rm = TRUE), n_endemic_us = sum(us_of_global >= 0.9, na.rm = TRUE), n_er_ge50 = sum(er >= 0.5), .groups = "drop"), by = "cat") |>
  select(cat, n_sp, n_valid_usa, n_in_pa, sum_w, S_gl_usa, median_us_of_global, n_endemic_us, n_er_ge50)
print(cats_out)
fallback_rows <- fb |> transmute(sci = scientific_name, common = common_name, ds, fraction, method, basis, sp_cat, scored = taxon_id %in% usa$taxon_id[!is.na(usa$fb_fraction)]) |> arrange(desc(scored), sp_cat, sci)
denom <- list(
  by_cat = usa |> filter(sp_cat %in% CATS) |> group_by(cat = sp_cat) |> summarise(n = n(), n_merged_global = sum(global_src == "merged_global"), n_aquamaps = sum(global_src == "aquamaps"), n_fallback = sum(grepl("fallback", global_src)), med_model = signif(median(us_of_global, na.rm = TRUE), 3), .groups = "drop"),
  national_only = fallback_rows |> count(sp_cat, ds),
  fallback = fallback_rows,
  examples = usa |> filter(sp_cat == "turtle" | sp_scientific %in% c("Balaenoptera ricei","Delphinapterus leucas","Megaptera novaeangliae","Balaena mysticetus","Odobenus rosmarus","Eumetopias jubatus","Enhydra lutris","Orcinus orca","Trichechus manatus")) |>
    transmute(common = sp_common, sci = sp_scientific, cat = sp_cat, src = global_src, model = signif(us_of_global, 3)) |> left_join(tx |> transmute(sci = scientific_name, cells = signif(n_usa / n_global, 3)), by = "sci"))
out <- list(
  version = "v9", generated = format(Sys.time(), "%Y-%m-%d"), denom = denom,
  # both rounded: DuckDB's parallel sums differ in the last digits between runs, and the page must rebuild byte-identically
  primprod_usa_mean = signif(pp_usa_mean, 8),
  study_area_km2 = signif(za |> filter(fld == "subregion_key", zone == "USA") |> pull(area_km2), 10),
  pas = pas |> transmute(pa, name, region, region_name, ecoregions, eco_main, eco_main_name, area_km2 = round(area_km2)),
  cats = cats_out |> mutate(across(where(is.numeric), ~signif(.x, 4))),
  tab = tab |> mutate(across(where(is.numeric), ~signif(.x, 5))),
  comp = comp |> mutate(across(where(is.numeric), ~signif(.x, 5))),
  spearman = lapply(spear, function(x) round(x, 3)),
  natstats = natstats |> mutate(across(where(is.numeric), ~signif(.x, 5))),
  ecostats = ecostats |> mutate(across(where(is.numeric), ~signif(.x, 5))),
  top = top, turtles = turt, turtle_species = turt_sp)
write_json(out, file.path(S, "v9_data.json"), auto_unbox = TRUE, na = "null", digits = NA)
cat("\nwrote v9_data.json:", file.size(file.path(S, "v9_data.json")), "bytes\n")
dbDisconnect(con, shutdown = TRUE)
