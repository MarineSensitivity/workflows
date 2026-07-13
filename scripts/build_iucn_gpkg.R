# consolidate the raw IUCN shapefiles (12.5GB on Drive) into ONE indexed geopackage on
# ~/_big, so the ingest reads it fast + locally (no Drive dependency, index hit per id_no).
# Applies the ingest's exact filter (presence 1:3, marine=true, category in ok set) and keeps
# the fields the ingest needs (id_no, sci_name, class, category, grp).
suppressMessages({library(sf); library(dplyr); library(fs); library(glue); library(purrr)})
sf_use_s2(FALSE)
src_dir <- path.expand("~/My Drive/projects/msens/data/raw/iucnredlist.org")
out_dir <- path.expand("~/_big/msens/derived/iucnredlist.org"); dir_create(out_dir)
out_gpkg <- file.path(out_dir, "rng_iucn_all.gpkg")
iucn_ok <- c("CR","EN","VU","NT","LC","DD")

groups <- dir_ls(src_dir, type = "directory")
cat(sprintf("[%s] building %s from %d groups\n", format(Sys.time(),"%H:%M:%S"), out_gpkg, length(groups)))
if (file_exists(out_gpkg)) file_delete(out_gpkg)

n_written <- 0L
for (grp in groups) {
  gname <- path_file(grp)
  shps  <- dir_ls(grp, glob = "*.shp", recurse = TRUE)
  if (!length(shps)) next
  g <- tryCatch(
    suppressWarnings(bind_rows(lapply(shps, st_read, quiet = TRUE))) |>
      rlang::set_names(tolower) |>
      filter(presence %in% 1:3, tolower(marine) == "true", category %in% iucn_ok) |>
      st_zm(drop = TRUE) |>
      transmute(id_no = as.integer(id_no), sci_name = as.character(sci_name),
                class = as.character(class), category = as.character(category), grp = gname),
    error = function(e) { cat("  ERR", gname, conditionMessage(e), "\n"); NULL })
  if (is.null(g) || !nrow(g)) { cat(sprintf("  [%s] %-28s 0\n", format(Sys.time(),"%H:%M:%S"), gname)); next }
  g <- g[!st_is_empty(g), ]
  st_write(g, out_gpkg, layer = "rng_iucn", append = file_exists(out_gpkg), quiet = TRUE)
  n_written <- n_written + nrow(g)
  cat(sprintf("  [%s] %-28s +%d (total %d)\n", format(Sys.time(),"%H:%M:%S"), gname, nrow(g), n_written))
}

# attribute index on id_no so the ingest's per-species read is an index hit
system2("ogrinfo", c(shQuote(out_gpkg), "-sql",
                     shQuote("CREATE INDEX idx_rng_iucn_idno ON rng_iucn(id_no)")))
cat(sprintf("[%s] DONE: %s (%d features)\n", format(Sys.time(),"%H:%M:%S"), out_gpkg, n_written))
