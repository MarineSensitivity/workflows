# asset_catalog.R: a release that lands catalogs what it published ----
#
# The model and score COGs of a v1-v7c release are uploaded straight to the content-addressed store
# (`cog/{grid_id}/{hash}.tif`). The store's catalog (`marine-atlas/assets.parquet`) used to learn about them
# from a one-off migration, so a NEW patch release (v7c) left 16 objects in the bucket that its pointers named
# and the catalog did not: `msens::store_publish_gate()` and `scripts/check_release_pointers.R` then fail.
#
#   catalog_sweep(ver, urls, payload_hash, enc, local_dirs, do_s3)
#
# takes every store object the release POINTS at, finds the ones the PUBLISHED catalog lacks
# (`msens::asset_catalog_missing()`), builds their rows from the uploaded files with
# `msens::asset_catalog_register()` (bytes / md5 from the file, `first_ver` = this release), pushes
# `assets.parquet`, re-reads it and asserts every pointer is catalogued (`msens::store_publish_gate()`).
# Idempotent: a second run finds 0 missing and pushes nothing. With `do_s3 = FALSE` (BACKFILL_NO_S3=1 /
# SCORE_COGS_NO_S3=1, or no authenticated `aws`) nothing is written: it logs "would catalog N".
#
# WHERE THE BYTES COME FROM: each key is looked up by file name in `local_dirs` (the render's own
# `backfill_cogs/` and `score_cogs/` under the release's derived/big dir, which is where the COGs were built
# and uploaded from); a key with no local copy is DOWNLOADED from the store to a temp dir. Either way the
# object must answer HEAD 200 on the store first: the catalog never lists an object that is not there.
catalog_sweep <- function(ver, urls, payload_hash = rep(NA_character_, length(urls)),
                          enc = rep(NA_character_, length(urls)), local_dirs = character(), do_s3 = TRUE,
                          base = msens::atlas_base_url(), bucket_root = "s3://oceanmetrics.io-public/marine-atlas") {
  stopifnot(grepl("^v[0-9]+[a-z]?$", ver), length(payload_hash) == length(urls), length(enc) == length(urls))
  keep <- !is.na(urls); urls <- urls[keep]; payload_hash <- payload_hash[keep]; enc <- enc[keep]
  catalog <- msens::asset_catalog_read(base)
  miss    <- msens::asset_catalog_missing(catalog, urls)
  n_ptr   <- length(unique(msens::asset_key_from_url(urls)))
  if (!length(miss)) {
    msens::store_publish_gate(urls, catalog, ver)
    logger::log_info("catalog sweep {ver}: all {n_ptr} pointer keys are catalogued; nothing to add")
    return(invisible(character()))
  }
  if (!do_s3) {
    logger::log_info("catalog sweep {ver}: would catalog {length(miss)} of {n_ptr} pointer keys (not written: no S3 push this run); e.g. {miss[1]}")
    return(invisible(miss))
  }
  head_ok <- vapply(miss, function(k) {
    h <- suppressWarnings(system2("curl", c("-sI", "-m", "30", shQuote(paste0(base, "/", k))), stdout = TRUE, stderr = FALSE))
    grepl(" 200", h[1])
  }, NA)
  if (!all(head_ok))
    stop(sprintf("catalog sweep %s: %d object(s) the release points at are not on the store (e.g. %s); upload before cataloguing",
                 ver, sum(!head_ok), miss[!head_ok][1]), call. = FALSE)
  tmp <- tempfile("catalog_sweep_"); dir.create(tmp)
  fetch <- function(keys) {
    local <- vapply(keys, function(k) {
      f <- file.path(local_dirs, basename(k)); f <- f[file.exists(f)]
      if (length(f)) f[1] else NA_character_ }, "")
    dl <- is.na(local)
    for (k in keys[dl]) {
      f <- file.path(tmp, basename(k))
      utils::download.file(paste0(base, "/", k), f, mode = "wb", quiet = TRUE)
      local[k] <- f
    }
    logger::log_info("catalog sweep {ver}: {sum(!dl)} file(s) from local dirs, {sum(dl)} downloaded from the store")
    unname(local)
  }
  catalog2 <- msens::asset_catalog_register(catalog, urls, payload_hash, enc, first_ver = ver, fetch = fetch)
  f_cat <- file.path(tmp, "assets.parquet")
  msens::asset_catalog_write(catalog2, f_cat)
  o <- system2("aws", c("s3", "cp", shQuote(f_cat), shQuote(paste0(bucket_root, "/assets.parquet")),
                        "--content-type", "application/octet-stream", "--only-show-errors"), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(o, "status")) && attr(o, "status") != 0) stop(paste(o, collapse = "\n"))
  back <- msens::asset_catalog_read(base)                                   # read back what is PUBLISHED, not what was built
  stopifnot("catalog sweep: objects still missing from the published catalog" = !length(msens::asset_catalog_missing(back, urls)))
  msens::store_publish_gate(urls, back, ver)
  logger::log_info("catalog sweep {ver}: cataloged {length(miss)} object(s) (first_ver {ver}); catalog now {nrow(back)} objects; every one of {n_ptr} pointer keys is catalogued")
  invisible(miss)
}
