# 04 - taxa scored in the US whose only models are national or regional (FWS / NMFS ranges, AquaX): they have
# no global surface, so the v10 denominator needs a fallback (05).
suppressMessages({library(DBI); library(duckdb); library(dplyr)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); VER <- Sys.getenv("VER", "v9")
source(file.path(repo, "libs/paths.R"))
sdm_v <- path.expand(glue::glue("{dir_big}/{VER}/sdm.duckdb"))
con <- dbConnect(duckdb(), sdm_v, read_only = TRUE)
x <- dbGetQuery(con, "
WITH ds AS (SELECT t.taxon_id, string_agg(DISTINCT tm.ds_key, '+' ORDER BY tm.ds_key) AS ds
            FROM taxon t JOIN taxon_model tm USING (taxon_id) WHERE t.is_valid_usa GROUP BY 1)
SELECT t.taxon_id, t.scientific_name, t.common_name, t.sp_cat, t.extrisk_code, t.er_score, ds.ds, t.n_usa, t.n_pra, t.is_valid_pra,
       round(t.range_usa_km2) range_usa_km2
FROM taxon t JOIN ds USING (taxon_id)
WHERE t.is_valid_usa AND NOT regexp_matches(ds.ds, 'am|bl|rng_iucn|rng_turtle') ORDER BY sp_cat, scientific_name")
write.csv(x, file.path(S, "v9_national_only.csv"), row.names = FALSE)
cat("national-only taxa:", nrow(x), "\n"); print(count(x, sp_cat, ds))
dbDisconnect(con, shutdown = TRUE)
