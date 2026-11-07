# resolve the files of an obisindicators release (v11 Q1) ----
# sourced after libs/paths.R (obis_h3_dir, obis_h3_release, obis_h3_url). A release folder is
#   obis_cell.parquet   aphiaid, cell_id (global05), date_year, records
#   worms_taxon.parquet taxonID, parentNameUsageID, acceptedNameUsageID, scientificName, taxonRank, taxonomicStatus
#   release.json        the `mst` block records obis_snapshot, worms_version, row/record counts
# read from the local folder when present, else over HTTPS (DuckDB httpfs / jsonlite)

obis_h3_src <- function(file, release = obis_h3_release, dir = obis_h3_dir, url = obis_h3_url) {
  loc <- path.expand(file.path(dir, release, file))
  if (file.exists(loc)) return(list(path = loc, remote = FALSE))
  list(path = paste0(url, "/", release, "/", file), remote = TRUE)
}

# the release's provenance, from release.json's `mst` block (written by the MST-facing export)
obis_h3_info <- function(release = obis_h3_release) {
  src <- obis_h3_src("release.json", release)
  j   <- tryCatch(jsonlite::fromJSON(src$path, simplifyVector = FALSE),
                  error = \(e) stop("cannot read release.json for ", release, " (", src$path, "): ", conditionMessage(e)))
  stopifnot("release.json has no `mst` block: not an MST-facing export" = !is.null(j$mst),
            "release.json names another release than the folder" = identical(j$release, release))
  list(release       = release,
       obis_snapshot = j$mst$obis_snapshot,
       worms_version = j$mst$worms_version,
       h3_res        = j$mst$h3_res,
       grid_id       = j$mst$grid_id,
       n_rows        = j$mst$n_rows,
       records       = j$mst$records,
       n_aphiaid     = j$mst$n_aphiaid,
       files         = j$mst$files,
       remote        = src$remote)
}

# a DuckDB connection able to read https:// parquet when the release is not on local disk
obis_h3_httpfs <- function(con) {
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  invisible(con)
}
