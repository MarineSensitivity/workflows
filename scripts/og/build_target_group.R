#!/usr/bin/env Rscript
# target-group background locations for the og runs: one row per global05 cell where ANY species of the group has an OBIS
# record (max_year >= 1950). Reads the OBIS-by-cell table built for MST (~/_big/msens/derived/obis_grid.duckdb, occ_cell).
#   Rscript scripts/og/build_target_group.R            -> ~/_big/sdm/og_inputs/tg_{turtle,cetacean,mega}.parquet + tg_map.csv
# columns: cell_id (1-based pixel index of the 7200x3600 -180..180 grid, row-major from the top-left), lon, lat (cell centre).
#   turtle   = the 7 sea-turtle AphiaIDs (their class is NULL in occ_cell, so they are listed by id)
#   cetacean = taxonIDs of all_splist_20240724.csv with class Mammalia and order Cetartiodactyla (the OBIS-SDM species list)
#   mega     = class Aves or Mammalia, plus the turtles
# tg_map.csv is the per-species map OG_BG_FROM accepts (taxonid,file): the six turtles -> tg_turtle, the whale -> tg_cetacean.
# OG_BG_FROM=~/_big/sdm/og_inputs/tg_map.csv (variant og)   or   OG_BG_FROM=~/_big/sdm/og_inputs/tg_mega.parquet (variant ogm)
suppressMessages({library(DBI); library(duckdb); library(arrow)})
home <- path.expand("~")
db   <- Sys.getenv("OG_OBIS_GRID", file.path(home, "_big/msens/derived/obis_grid.duckdb"))
out  <- Sys.getenv("OG_INPUTS",    file.path(home, "_big/sdm/og_inputs"))
splist <- file.path(home, "_big/sdm/obis/source/model=mpaeu/data/all_splist_20240724.csv")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

nc <- 7200L; nr <- 3600L; res <- 0.05
turtles <- c(137205, 137206, 137207, 137208, 137209, 220293, 344093)
sp  <- read.csv(splist)
cet <- sp$taxonID[which(sp$class == "Mammalia" & sp$order == "Cetartiodactyla")]

con <- dbConnect(duckdb(), db, read_only = TRUE); on.exit(dbDisconnect(con, shutdown = TRUE))
ids <- function(x) paste(format(as.numeric(x), scientific = FALSE, trim = TRUE), collapse = ",")
q <- function(where) dbGetQuery(con, sprintf(
  "SELECT DISTINCT cell_id FROM occ_cell WHERE max_year >= 1950 AND (%s) ORDER BY cell_id", where))$cell_id
groups <- list(
  turtle   = q(sprintf("aphia IN (%s)", ids(turtles))),
  cetacean = q(sprintf("aphia IN (%s)", ids(cet))),
  mega     = q(sprintf("class IN ('Aves','Mammalia') OR aphia IN (%s)", ids(turtles))))

for (g in names(groups)) {
  cid <- groups[[g]]
  bad <- cid < 1 | cid > nc * nr
  if (any(bad)) message(g, ": dropping ", sum(bad), " cell_id outside the 7200x3600 grid (max ", max(cid), ")")
  cid <- cid[!bad]
  row <- (cid - 1L) %/% nc; col <- (cid - 1L) %% nc
  d <- data.frame(cell_id = as.integer(cid), lon = -180 + (col + 0.5) * res, lat = 90 - (row + 0.5) * res)
  write_parquet(d, file.path(out, paste0("tg_", g, ".parquet")))
  message(sprintf("tg_%-8s %9s cells  (%.1f MB)", g, format(nrow(d), big.mark = ","),
                  file.size(file.path(out, paste0("tg_", g, ".parquet"))) / 1e6))
}
message("cetacean species ids in the list: ", length(cet), " | turtles: ", length(turtles))

map <- data.frame(taxonid = c(137205, 137206, 137207, 137208, 137209, 220293, 159023),
                  file = file.path(out, c(rep("tg_turtle.parquet", 6), "tg_cetacean.parquet")))
write.csv(map, file.path(out, "tg_map.csv"), row.names = FALSE)
message("wrote ", file.path(out, "tg_map.csv"))
