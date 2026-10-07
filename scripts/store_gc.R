#!/usr/bin/env Rscript
# store_gc.R: garbage-collect the asset store -- delete catalogued objects that NO published release points
# at. "Nothing is deleted except through a check that joins the catalog against every release's pointer
# tables" (docs, db.qmd): that check is msens::store_unreferenced() over libs/store_pointers.R. DESTRUCTIVE
# with --go; a dry run by default.
#
#   Rscript scripts/store_gc.R [--go] [--max <n>]
#
# Every run: fetch the catalog (assets.parquet) and every release's pointers, list the unreferenced rows
# (count, bytes, by store prefix / first_ver / created date) and write them to a CSV in tempdir() (keys only; the --go record keeps them).
# With --go: delete each object from the bucket (aws s3 rm, one call per key -- there are tens, not thousands),
# drop its row from the catalog and push the rewritten assets.parquet (asset_catalog_write(), the same writer
# publish_native.qmd uses), remove the local store-mirror copy if there is one, re-read the published catalog
# and assert the keys are gone, and record data/manifests/store_gc.json. --max refuses to delete more than n
# objects (default 500): garbage comes in tens; thousands means a pointer table failed to download, not garbage.
# Bucket versioning is on with a 30-day noncurrent expiry, so a deleted object is recoverable for 30 days.
suppressMessages({library(msens); library(arrow); library(jsonlite); library(glue)})
source(here::here("libs/store_pointers.R"))

a   <- commandArgs(TRUE)
go  <- "--go" %in% a
mx  <- { i <- which(a == "--max"); if (length(i)) as.integer(a[i + 1]) else 500L }
base   <- atlas_base_url(); bucket <- "oceanmetrics.io-public"; root <- "marine-atlas"
dir_as <- path.expand("~/_big/msens/derived/asset_store")          # the local catalog + store mirror publish_native.qmd keeps
say <- function(...) cat(sprintf(...), "\n")

f_cat <- tempfile("assets_", fileext = ".parquet")
stopifnot("could not fetch the catalog" = identical(as.integer(suppressWarnings(
  download.file(glue("{base}/assets.parquet"), f_cat, quiet = TRUE, mode = "wb"))), 0L))
cat0 <- as.data.frame(read_parquet(f_cat))
ptr  <- store_pointers(base)
say("catalog %d objects; pointers from %d releases (%d URLs); registered: %s",
    nrow(cat0), length(ptr), length(unique(unlist(ptr))), paste(attr(ptr, "versions"), collapse = " "))
g <- store_unreferenced(cat0, ptr)
say("unreferenced: %d objects, %.1f MB", nrow(g), sum(g$bytes, na.rm = TRUE) / 1e6)
if (nrow(g)) {
  print(table(prefix = dirname(g$key), first_ver = ifelse(is.na(g$first_ver), "NA", g$first_ver)))
  print(table(created = substr(as.character(g$created), 1, 10)))
}
f_c <- file.path(tempdir(), "store_gc_candidates.csv")               # not under _output/ (tracked + published)
write.csv(g, f_c, row.names = FALSE)
say("candidates written: %s", f_c)
if (!nrow(g)) { say("nothing to collect"); quit(status = 0) }
if (!go) { say("dry run: nothing deleted (add --go)"); quit(status = 0) }
stopifnot("more candidates than --max allows -- a pointer source probably failed to load; look before deleting" = nrow(g) <= mx)

# --go ----------------------------------------------------------------------------------------------------
t0 <- Sys.time(); gone <- character()
for (k in g$key) {
  rc <- system2("aws", c("s3", "rm", shQuote(glue("s3://{bucket}/{root}/{k}")), "--only-show-errors"))
  if (!identical(as.integer(rc), 0L)) stop(glue("aws s3 rm failed for {k} ({length(gone)} deleted so far; the catalog is untouched)"))
  gone <- c(gone, k)
  if (file.exists(file.path(dir_as, "store", k))) unlink(file.path(dir_as, "store", k))
}
say("deleted %d objects from s3://%s/%s/", length(gone), bucket, root)
cat1 <- cat0[!cat0$key %in% gone, , drop = FALSE]
f_new <- file.path(dir_as, "assets.parquet")                        # the local copy publish_native.qmd pushes from
asset_catalog_write(cat1, f_new)
rc <- system2("aws", c("s3", "cp", shQuote(f_new), shQuote(glue("s3://{bucket}/{root}/assets.parquet")), "--only-show-errors"))
if (!identical(as.integer(rc), 0L)) stop("pushing the rewritten catalog failed -- the objects are gone but the catalog still lists them: rerun")
say("catalog: %d -> %d rows, pushed assets.parquet", nrow(cat0), nrow(cat1))
# read back, as a client would
cat2 <- asset_catalog_read(base)
stopifnot("the published catalog still lists a deleted key" = !any(gone %in% cat2$key),
          "the published catalog lost rows it should keep" = nrow(cat2) == nrow(cat1))
left <- vapply(utils::head(gone, 5), function(k) { tryCatch(httr2::resp_status(httr2::req_perform(httr2::req_error(
  httr2::req_method(httr2::request(glue("{base}/{k}")), "HEAD"), is_error = \(r) FALSE))), error = function(e) NA_integer_) }, integer(1))
say("published catalog re-read: %d rows; HEAD of 5 deleted keys: %s", nrow(cat2), paste(left, collapse = " "))
rec <- list(collected_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), n_deleted = length(gone), bytes = sum(g$bytes, na.rm = TRUE),
            catalog_rows = c(before = nrow(cat0), after = nrow(cat2)), by_prefix = as.list(table(dirname(gone))),
            by_first_ver = as.list(table(ifelse(is.na(g$first_ver), "NA", g$first_ver))), keys = gone,
            minutes = round(as.numeric(Sys.time() - t0, units = "mins"), 1))
dir.create("data/manifests", showWarnings = FALSE)
write_json(rec, "data/manifests/store_gc.json", auto_unbox = TRUE, pretty = TRUE)
say("record written: data/manifests/store_gc.json")
