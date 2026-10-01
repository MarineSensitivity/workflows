#!/usr/bin/env Rscript
# check_release_pointers.R: after a release is re-pointed at the asset store, prove its PUBLISHED pointers work -- READ ONLY.
#
#   Rscript scripts/check_release_pointers.R <ver> [--n 200] [--shards 20] [--base https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas]
#                                            [--catalog <assets.parquet path or URL>]   (default {base}/assets.parquet)
#
# Reads {base}/{ver}/tables/native_asset.parquet and {base}/assets.parquet anonymously and checks:
#   1. every pointer URL is a store key (cog/{grid}/{hash}.tif | native/{ds}/{hash}.{tif|pmtiles}), none a versioned path
#   2. every pointer's key is a row of the catalog, and a pointer's content_hash is its key's hash
#   3. a random sample of --n distinct pointer URLs answers HTTP 200 (HEAD) with the right content type
#   4. a random sample of --shards taxon shards from {ver}/app/taxon/ validates against the schema, and every asset URL in it is
#      a catalog key
# Prints a summary, exits 0 only if every check passes. Never writes anything.
suppressMessages({library(arrow); library(dplyr); library(jsonlite); library(msens)})
a <- commandArgs(TRUE); ver <- a[1]; stopifnot("usage: check_release_pointers.R <ver> [--n N] [--shards K]" = grepl("^v[0-9]+[a-z]?$", ver))
opt <- function(flag, default) { i <- which(a == flag); if (length(i)) a[i + 1] else default }
n <- as.integer(opt("--n", "200")); k <- as.integer(opt("--shards", "20"))
base <- opt("--base", "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas")
fetch <- function(url) { f <- tempfile(); utils::download.file(url, f, mode = "wb", quiet = TRUE); f }
na  <- as.data.frame(read_parquet(fetch(sprintf("%s/%s/tables/native_asset.parquet", base, ver))))
cat_src <- opt("--catalog", sprintf("%s/assets.parquet", base))
cat_ <- as.data.frame(read_parquet(if (grepl("^https?://", cat_src)) fetch(cat_src) else cat_src))
key <- asset_key_from_url(na$asset_url)
off  <- sum(is.na(key)); miss <- sum(!is.na(key) & !key %in% cat_$key)
hash_ok <- if ("content_hash" %in% names(na)) all(is.na(na$content_hash) | is.na(key) | na$content_hash == sub("^.*/([0-9a-f]{16})\\.[a-z]+$", "\\1", key)) else NA
cat(sprintf("%s: %d pointers | not a store key: %d | store key missing from the catalog: %d | content_hash == key hash: %s\n", ver, nrow(na), off, miss, hash_ok))

set.seed(20261002)
urls <- unique(na$asset_url); smp <- sample(urls, min(n, length(urls)))
head1 <- function(u) {
  h <- system2("curl", c("-sI", "-m", "20", shQuote(u)), stdout = TRUE, stderr = FALSE)
  c(code = sub("^HTTP/[0-9.]+ ([0-9]+).*$", "\\1", h[1]), type = tolower(sub("^[Cc]ontent-[Tt]ype: *", "", trimws(grep("^[Cc]ontent-[Tt]ype", h, value = TRUE)[1]))))
}
hd <- t(vapply(smp, head1, c(code = "", type = "")))
want <- ifelse(grepl("\\.tif$", sub("\\?.*$", "", smp)), "image/tiff", "binary/octet-stream")
good <- !is.na(hd[, "code"]) & hd[, "code"] == "200" & !is.na(hd[, "type"]) & hd[, "type"] == want   # a missing header is a failure, never a pass
bad <- smp[!good]
cat(sprintf("HEAD sample: %d distinct pointer URLs | not 200 or wrong content type: %d\n", length(smp), length(bad)))
if (length(bad)) cat("  e.g.", paste(utils::head(bad, 3), collapse = "\n       "), "\n")

ids <- sprintf("%02x", 0:255); pick <- sample(ids, min(k, 256))
shard_bad <- 0L; asset_off <- 0L; both <- 0L; inputs <- 0L
for (s in pick) {
  f <- fetch(sprintf("%s/%s/app/taxon/%s.json", base, ver, s)); txt <- paste(readLines(gzfile(f), warn = FALSE), collapse = "\n"); x <- fromJSON(txt, simplifyVector = FALSE)
  ok <- isTRUE(tryCatch({ app_validate(x, "taxon"); TRUE }, error = function(e) FALSE)); if (!ok) shard_bad <- shard_bad + 1L
  for (card in x$taxa) {
    if (!is.null(card$merged$url) && is.na(asset_key_from_url(card$merged$url))) asset_off <- asset_off + 1L
    for (i in card$inputs) { inputs <- inputs + 1L; both <- both + (length(i$assets) > 1)
      for (as in i$assets) if (is.na(asset_key_from_url(as$url)) || !asset_key_from_url(as$url) %in% cat_$key) asset_off <- asset_off + 1L }
  }
}
cat(sprintf("shard sample: %d taxon shards | fail the schema: %d | asset/merged URLs off the store or not in the catalog: %d | inputs %d, with both representations %d\n",
            length(pick), shard_bad, asset_off, inputs, both))

ok <- off == 0 && miss == 0 && isTRUE(hash_ok %in% c(TRUE, NA)) && !length(bad) && shard_bad == 0 && asset_off == 0
cat(if (ok) sprintf("PASS: %s points only at catalogued store objects that answer\n", ver) else sprintf("FAIL: %s\n", ver))
quit(status = if (ok) 0L else 1L)
