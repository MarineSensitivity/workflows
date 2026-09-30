# native_store_map.R: map existing v8/v9 original-surface objects to the content-addressed native store.
#
# usage: Rscript scripts/native_store_map.R <out.parquet> <heads.tsv> [<heads.tsv> ...] [--pmt-md5 <md5sum.txt>]
#   heads.tsv   url <TAB> http status <TAB> content-length <TAB> etag <TAB> content-type, from anonymous
#               `curl -sI` of each S3 object (no listing needed: the bucket denies anonymous ListObjects).
#               An S3 ETag is the MD5 of the bytes for a single-part upload; a multipart ETag has a "-N"
#               suffix and cannot be used (those objects are reported and left to be hashed by bytes).
#   --pmt-md5   `md5sum` output taken ON the file host for the PMTiles (whose Caddy ETag is not an MD5).
# read-only: no network write, no bucket write. Also writes <out>.commands.sh (NOT run): the S3 server-side
# copies and the PMTiles copy-by-hash commands the release session runs.
suppressMessages({library(dplyr); library(arrow); library(readr)})
a <- commandArgs(TRUE)
pm <- match("--pmt-md5", a)
md5f <- if (!is.na(pm)) a[pm + 1] else NULL
a <- if (!is.na(pm)) a[-c(pm, pm + 1)] else a
out <- a[1]; heads <- a[-1]
S3 <- "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas"

h <- bind_rows(lapply(heads, read_tsv, col_names = c("asset_url", "status", "bytes", "etag", "ctype"),
                      col_types = "cidcc", show_col_types = FALSE)) |> distinct(asset_url, .keep_all = TRUE)
stopifnot("an object is not anonymously readable" = all(h$status == 200L))
h$etag <- gsub('"', "", h$etag)
h$multipart <- grepl("-", h$etag, fixed = TRUE)
h$hash <- ifelse(h$multipart | nchar(h$etag) != 32, NA_character_, h$etag)
h$type <- "cog"
# ds_key from the versioned key: native/{am_native|ax_native|...}/...; pmtiles/{ver}/{ds}/...
h$ds_key <- sub("_native$", "", sub("^.*/native/([^/]+)/.*$", "\\1", h$asset_url))

if (!is.null(md5f)) {
  m <- read_table(md5f, col_names = c("hash", "path"), col_types = "cc", show_col_types = FALSE)
  m$tail <- sub("^.*/pmtiles/", "", m$path)                       # {ver}/{ds}/{file}.pmtiles
  p <- tibble(asset_url = grep("file.marinesensitivity.org", readLines(file.path(dirname(out), "pmt_urls.txt")), value = TRUE))
  p$tail <- sub("\\?.*$", "", sub("^.*/pmtiles/", "", p$asset_url))
  p <- p |> left_join(m[, c("tail", "hash")], by = "tail")
  p$type <- "pmtiles"; p$ds_key <- sub("^[^/]+/([^/]+)/.*$", "\\1", p$tail); p$multipart <- FALSE
  p$bytes <- NA_real_
  h <- bind_rows(h, p[, names(h)[names(h) %in% names(p)]])
}
h$store_key <- ifelse(is.na(h$hash), NA_character_, mapply(msens::native_key, h$ds_key, h$hash, h$type))
h$store_url <- ifelse(is.na(h$hash), NA_character_, mapply(msens::native_url, h$ds_key, h$hash, h$type))
arrow::write_parquet(h, out, compression = "zstd")

cp <- h |> filter(!is.na(store_key), type == "cog") |> distinct(store_key, .keep_all = TRUE) |>
  transmute(cmd = sprintf("aws s3 cp s3://oceanmetrics.io-public/marine-atlas/%s s3://oceanmetrics.io-public/marine-atlas/%s --only-show-errors",
                          sub(paste0("^", S3, "/"), "", asset_url), store_key))
pmc <- h |> filter(!is.na(store_key), type == "pmtiles") |> mutate(
  tail = sub("\\?.*$", "", sub("^.*/pmtiles/", "", asset_url)), ver = sub("/.*$", "", tail), file = basename(tail)) |>
  distinct(store_key, .keep_all = TRUE)
pm_cmd <- c(
  # file host: copy (not move: v8/v9 keep working until their tables are re-pointed) by hash, no overwrite
  unlist(lapply(split(pmc, pmc$ds_key), function(d) c(
    sprintf("ssh msens 'mkdir -p /share/data/derived/pmtiles/native/%s'", d$ds_key[1]),
    sprintf("ssh msens 'cp -n /share/data/derived/pmtiles/%s /share/data/derived/pmtiles/native/%s/%s.pmtiles'",
            d$tail, d$ds_key, d$hash)))),
  # S3 mirror (only if the release-scoped mirror exists: verify with `aws s3 ls` first)
  sprintf("aws s3 cp s3://oceanmetrics.io-public/marine-atlas/%s/native/pmtiles/%s/%s s3://oceanmetrics.io-public/marine-atlas/%s --only-show-errors",
          pmc$ver, pmc$ds_key, pmc$file, pmc$store_key))
writeLines(c("#!/bin/sh", "# NOT RUN: server-side S3 copies (no download) + PMTiles copy-by-hash; release session only",
             cp$cmd, pm_cmd), paste0(out, ".commands.sh"))
cat(sprintf("%d objects | %d cog (%d multipart, unhashed) | %d pmtiles (%d hashed) | %d distinct store keys | %.1f MB cog\n",
            nrow(h), sum(h$type == "cog"), sum(h$multipart), sum(h$type == "pmtiles"),
            sum(h$type == "pmtiles" & !is.na(h$hash)), n_distinct(h$store_key, na.rm = TRUE),
            sum(h$bytes[h$type == "cog"], na.rm = TRUE) / 1e6))
