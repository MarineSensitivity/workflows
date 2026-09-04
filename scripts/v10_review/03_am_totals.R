# 03 - AquaMaps suitability-area totals (all cells / US cells) for valid-US taxa that have NO merged global
# surface (AquaMaps-only taxa are not range-constrained, so dist_merged_global does not hold them). ~2 min.
suppressMessages({library(DBI); library(duckdb); library(dplyr)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); VER <- Sys.getenv("VER", "v9")
source(file.path(repo, "libs/paths.R"))
sdm_v <- path.expand(glue::glue("{dir_big}/{VER}/sdm.duckdb"))
t0 <- Sys.time()
con <- dbConnect(duckdb(), sdm_v, read_only = TRUE)
dbExecute(con, "SET threads TO 6"); dbExecute(con, "SET memory_limit = '12GB'"); dbExecute(con, "SET preserve_insertion_order = false")
gl <- read.csv(file.path(S, "v9_global_totals.csv"), stringsAsFactors = FALSE)
need <- dbGetQuery(con, "SELECT DISTINCT tm.mdl_key, tm.taxon_id, t.ms_merge_key, t.er_mode FROM taxon_model tm JOIN taxon t USING (taxon_id) WHERE tm.ds_key = 'am' AND t.is_valid_usa")
need <- need |> filter(!(ms_merge_key %in% gl$mdl_key))
cat("am models needed for valid_usa taxa lacking a merged global surface:", nrow(need), "taxa:", n_distinct(need$taxon_id), "\n")
am_dir <- path.expand(glue::glue("{dir_big}/{VER}/marine-atlas/dist/dataset=am"))
files <- file.path(am_dir, paste0(sub("^am\\|", "", need$mdl_key), ".parquet"))
ok <- file.exists(files); cat("files found:", sum(ok), "missing:", sum(!ok), "\n"); if (any(!ok)) print(head(need$mdl_key[!ok]))
files <- files[ok]
dbExecute(con, "CREATE TEMP TABLE usc AS SELECT cell_id FROM cell WHERE in_usa")
q <- sprintf("SELECT mdl_key, sum(val/100.0*area) AS all_suit_area, sum(CASE WHEN u.cell_id IS NOT NULL THEN val/100.0*area END) AS us_suit_area, count(*) AS n_cells
  FROM (SELECT mdl_key, cell_id, val, 40589641.0 * radians(0.05) * (sin(radians(90 - floor((cell_id - 1) / 7200) * 0.05)) - sin(radians(90 - (floor((cell_id - 1) / 7200) + 1) * 0.05))) AS area
        FROM read_parquet([%s])) a LEFT JOIN usc u USING (cell_id) GROUP BY 1", paste(sprintf("'%s'", files), collapse = ","))
res <- dbGetQuery(con, q)
write.csv(res, file.path(S, "v9_am_totals.csv"), row.names = FALSE)
cat("rows:", nrow(res), " elapsed:", format(Sys.time() - t0), "\n")
dbDisconnect(con, shutdown = TRUE)
