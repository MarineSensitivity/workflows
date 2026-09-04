# 08 - denominator diagnostics (printed): merged global surface vs AquaMaps source, model- vs range-based US
# fraction per category, worked examples; adds the range-based US fraction to turtle_species in v9_data.json.
suppressMessages({library(DBI); library(duckdb); library(dplyr); library(jsonlite)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); VER <- Sys.getenv("VER", "v9")
source(file.path(repo, "libs/paths.R"))
sdm_v <- path.expand(glue::glue("{dir_big}/{VER}/sdm.duckdb"))
con <- dbConnect(duckdb(), sdm_v, read_only = TRUE)
tx <- dbGetQuery(con, "SELECT taxon_id, sp_cat, scientific_name, common_name, er_mode, n_datasets, range_km2, range_usa_km2, is_valid_usa FROM taxon WHERE is_valid_usa")
tmd <- dbGetQuery(con, "SELECT taxon_id, string_agg(DISTINCT ds_key, '+' ORDER BY ds_key) AS ds FROM taxon_model GROUP BY 1")
zt <- read.csv(file.path(S, "v9_zone_taxon.csv"), colClasses = c(taxon_id = "character"))
gl <- read.csv(file.path(S, "v9_global_totals.csv")); am <- read.csv(file.path(S, "v9_am_totals.csv"))
tm_am <- dbGetQuery(con, "SELECT mdl_key, taxon_id FROM taxon_model WHERE ds_key = 'am'")
am_tx <- am |> inner_join(tm_am, by = "mdl_key") |> group_by(taxon_id) |> summarise(am_all = sum(all_suit_area), am_us = sum(us_suit_area, na.rm = TRUE), .groups = "drop")
txk <- dbGetQuery(con, "SELECT taxon_id, ms_merge_key, er_score FROM taxon")
usa <- zt |> filter(zone_fld == "subregion_key", zone_value == "USA") |> select(taxon_id, sp_cat, er = er_score, suit_er_area_us = suit_er_area) |>
  inner_join(txk, by = "taxon_id") |> inner_join(tx |> select(taxon_id, er_mode, range_km2, range_usa_km2), by = "taxon_id") |>
  left_join(gl |> select(ms_merge_key = mdl_key, gl_suit_area = suit_area_global), by = "ms_merge_key") |> left_join(am_tx, by = "taxon_id") |> left_join(tmd, by = "taxon_id") |>
  mutate(suit_area_us = ifelse(er_mode == "taxon", suit_er_area_us / er, suit_er_area_us),
         global = ifelse(!is.na(gl_suit_area), gl_suit_area, pmax(am_all - am_us, 0) + suit_area_us),
         src = ifelse(!is.na(gl_suit_area), "merged global surface", "AquaMaps outside US + merged inside"),
         us_of_global_model = pmin(1, suit_area_us / global),
         us_of_global_range = ifelse(!is.na(range_km2) & range_km2 > 0, pmin(1, range_usa_km2 / range_km2), NA))
cat("global-denominator source by category:\n"); print(table(usa$sp_cat, usa$src))
cat("\ndataset combinations (top):\n"); print(usa |> count(ds, sort = TRUE) |> head(12))
cat("\nrange-based fraction available:\n"); print(usa |> group_by(sp_cat) |> summarise(n = n(), n_range = sum(!is.na(us_of_global_range)), med_model = median(us_of_global_model, na.rm = TRUE), med_range = median(us_of_global_range, na.rm = TRUE), cor = cor(us_of_global_model, us_of_global_range, use = "complete")))
cat("\nturtles + selected mammals: model-based vs range-based US fraction\n")
sel <- usa |> inner_join(tx |> select(taxon_id, common_name, scientific_name), by = "taxon_id") |>
  filter(sp_cat == "turtle" | scientific_name %in% c("Balaenoptera ricei","Delphinapterus leucas","Megaptera novaeangliae","Balaena mysticetus","Odobenus rosmarus","Eumetopias jubatus","Enhydra lutris","Orcinus orca","Phocoena sinus","Trichechus manatus")) |>
  transmute(common = common_name, sci = scientific_name, cat = sp_cat, ds, src, model = signif(us_of_global_model, 3), range = signif(us_of_global_range, 3))
print(sel |> as.data.frame())
denom <- list(
  by_cat = usa |> group_by(cat = sp_cat) |> summarise(n = n(), n_merged_global = sum(src == "merged global surface"), n_aquamaps = sum(src != "merged global surface"), n_range = sum(!is.na(us_of_global_range)),
    med_model = signif(median(us_of_global_model, na.rm = TRUE), 3), med_range = signif(median(us_of_global_range, na.rm = TRUE), 3), cor = signif(cor(us_of_global_model, us_of_global_range, use = "complete"), 3), .groups = "drop"),
  examples = sel)
d <- fromJSON(file.path(S, "v9_data.json"), simplifyVector = TRUE)
# d$denom is written by 07 (per-taxon source incl. the fallback); the diagnostics above are printed only
# add range-based fraction to turtle_species
ts <- usa |> inner_join(tx |> select(taxon_id, scientific_name), by = "taxon_id") |> filter(sp_cat == "turtle") |> transmute(sci = scientific_name, us_of_global_range = signif(us_of_global_range, 3))
d$turtle_species <- d$turtle_species |> left_join(ts, by = "sci")
write_json(d, file.path(S, "v9_data.json"), auto_unbox = TRUE, na = "null", digits = NA)
cat("\nv9_data.json updated:", file.size(file.path(S, "v9_data.json")), "bytes\n")
dbDisconnect(con, shutdown = TRUE)
