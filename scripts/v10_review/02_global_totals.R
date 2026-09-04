# 02 - per-taxon GLOBAL suitability-area totals from the merged global surfaces (the v10 denominator
# for range-constrained taxa). ~3.1 B rows; ~30 s with 6 threads. Cell area from the latitude band.
suppressMessages({library(DBI); library(duckdb); library(glue)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); VER <- Sys.getenv("VER", "v9")
source(file.path(repo, "libs/paths.R"))
glob <- path.expand(glue("{dir_big}/{VER}/marine-atlas/dist_merged_global/dataset=ms_merge/*/*.parquet"))
out  <- file.path(S, "v9_global_totals.csv")
con <- dbConnect(duckdb())
dbExecute(con, "SET threads TO 6"); dbExecute(con, "SET memory_limit = '12GB'"); dbExecute(con, "SET preserve_insertion_order = false")
t0 <- Sys.time()
dbExecute(con, glue("
COPY (
  SELECT mdl_key, sum(val / 100.0 * area_km2) AS suit_area_global, sum(area_km2) AS area_global, count(*) AS n_cells_global
  FROM (
    SELECT mdl_key, val,
      40589641.0 * radians(0.05) *
        (sin(radians(90 - floor((cell_id - 1) / 7200) * 0.05)) - sin(radians(90 - (floor((cell_id - 1) / 7200) + 1) * 0.05))) AS area_km2
    FROM read_parquet('{glob}')
  ) GROUP BY 1
) TO '{out}' (HEADER)"))
n <- nrow(read.csv(out)); cat("global totals:", n, "taxa; elapsed", format(Sys.time() - t0), "\n"); stopifnot(n > 15000)
dbDisconnect(con, shutdown = TRUE)
