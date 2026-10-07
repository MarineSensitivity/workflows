#!/usr/bin/env Rscript
# prune_legacy_native.R: M6 of the asset-store plan -- delete the PRE-STORE copies of distribution files that
# v8 and v9 carried before every release was re-pointed at the content-addressed store (2026-10-02):
# `marine-atlas/{ver}/native/` on S3 and the file host's `/share/data/pmtiles_native/{ver}/`. DESTRUCTIVE.
#
#   Rscript scripts/prune_legacy_native.R [--ver v8 --ver v9] [--keep gm] [--go]
#
# Every run, read-only, before anything else (the plan's gates; the run STOPS on any failure):
#   1. a full pointer-URL sweep: no `native_asset` / `model_asset` row or manifest of ANY registered release names
#      a `{ver}/native/` or `pmtiles/{ver}/` URL of a version being pruned
#   2. `scripts/check_release_pointers.R <ver>` passes for every version being pruned (its pointers are store keys,
#      catalogued, and answer)
#   3. `msens::store_unreferenced()` over the catalog and every release's pointers -- REPORTED, not a blocker: the
#      store is never touched here, this is the garbage figure the plan asks to see
# then the inventory of what would go (object counts, bytes, file counts). Without --go that is the whole run.
# With --go: `aws s3 rm --recursive` per version (prefixes in --keep are excluded: `gm/` holds the gm density
# COGs of `scripts/publish_gm_cogs.R`, not pre-store copies), `rm -rf` of the file-host directory over ssh, then
# the inventory and check 2 again, and a record in data/manifests/prune_legacy_native.json.
# Bucket versioning is on with a 30-day noncurrent expiry (M2), so the S3 half is recoverable for 30 days; the
# file-host half is not, but every file there is a store object's twin (`native/{ds}/{hash}.pmtiles`).
# Run 2026-10-07 with Ben's explicit go, nine days before the plan's soak date, which he accepted.
suppressMessages({library(msens); library(arrow); library(jsonlite); library(glue)})

a <- commandArgs(TRUE)
opt_all <- function(flag) { i <- which(a == flag); if (length(i)) a[i + 1] else character() }
vers <- opt_all("--ver"); if (!length(vers)) vers <- c("v8", "v9")
keep <- opt_all("--keep"); if (!length(keep) && !"--keep-none" %in% a) keep <- "gm"
go   <- "--go" %in% a
stopifnot(all(grepl("^v[0-9]+[a-z]?$", vers)))
base   <- atlas_base_url()                                   # https://s3.../oceanmetrics.io-public/marine-atlas
bucket <- "oceanmetrics.io-public"; root <- "marine-atlas"
host_dir <- function(v) file.path("", "share", "data", "pmtiles_native", v)
tmp <- tempfile("prune_"); dir.create(tmp)
fetch <- function(u) { f <- file.path(tmp, gsub("[^A-Za-z0-9._-]", "_", sub(base, "", u))); if (!file.exists(f)) {
  r <- tryCatch(suppressWarnings(download.file(u, f, quiet = TRUE, mode = "wb")), error = function(e) 1L)
  if (!identical(as.integer(r), 0L)) { unlink(f); return(NA_character_) } }; f }
say <- function(...) cat(sprintf(...), "\n")

# 1. pointer-URL sweep over every registered release ------------------------------------------------------
reg  <- fromJSON(glue("{base}/versions.json"), simplifyVector = FALSE)
reg  <- if (is.list(reg) && !is.null(reg$versions)) reg$versions else reg
all_v <- vapply(reg, function(x) if (!is.null(x$ver)) x$ver else x$id, character(1))
alt <- paste(vers, collapse = "|")
legacy_re <- paste0("[/](", alt, ")/native/|pmtiles/(", alt, ")/")
pointers <- list(); hits <- 0L; swept <- 0L
for (v in all_v) {
  na <- fetch(glue("{base}/{v}/tables/native_asset.parquet"))
  ma <- fetch(glue("{base}/{v}/tables/model_asset.parquet"))
  mf <- fetch(glue("{base}/{v}/manifest.json"))
  urls <- c(
    if (!is.na(na)) as.data.frame(read_parquet(na))$asset_url,
    if (!is.na(ma)) { m <- as.data.frame(read_parquet(ma)); if ("cog_url" %in% names(m)) m$cog_url },
    # a manifest's `app.probed` entries are capability PROBES (app_bundle_probe(): HEAD of e.g.
    # `{ver}/native/pmtiles/index.json`, the pre-store PMTiles index), not pointers: after the prune that probe
    # answers 404 and the capability flips FALSE at the next manifest build, which is the right answer, so they
    # are not swept. Everything else in the manifest (score COG hrefs, table URLs) is.
    if (!is.na(mf)) { m <- fromJSON(mf, simplifyVector = FALSE); m$app$probed <- NULL
      t <- as.character(toJSON(m, auto_unbox = TRUE)); regmatches(t, gregexpr("https?://[^\"\\\\ ]+", t))[[1]] })
  urls <- unique(stats::na.omit(urls)); if (!length(urls)) next
  pointers[[v]] <- urls; swept <- swept + length(urls)
  h <- sum(grepl(legacy_re, urls)); hits <- hits + h
  say("sweep %-4s %6d urls, %d legacy", v, length(urls), h)
}
stopifnot("the pointer-URL sweep found legacy references -- nothing is pruned" = hits == 0L,
          "no pointers swept" = swept > 0L)

# 2. the pruned versions' published pointers pass check_release_pointers.R --------------------------------
chk <- function(v) { rc <- system2("Rscript", c("scripts/check_release_pointers.R", v, "--n", "100", "--shards", "5"),
                                   stdout = FALSE, stderr = FALSE); identical(as.integer(rc), 0L) }
for (v in vers) if (!chk(v)) stop(sprintf("check_release_pointers.R %s failed before the prune", v))
say("check_release_pointers.R passes for %s", paste(vers, collapse = ", "))

# 3. store garbage figure (reported) ----------------------------------------------------------------------
cat0 <- as.data.frame(read_parquet(fetch(glue("{base}/assets.parquet"))))
unref <- store_unreferenced(cat0, pointers)
say("store_unreferenced: %d of %d catalogued objects are named by no release (the store is not touched here)",
    nrow(unref), nrow(cat0))

# inventory ----------------------------------------------------------------------------------------------
s3_count <- function(prefix) {
  out <- system2("aws", c("s3", "ls", shQuote(glue("s3://{bucket}/{root}/{prefix}")), "--recursive", "--summarize"),
                 stdout = TRUE, stderr = TRUE)
  n <- as.integer(sub(".*Total Objects: *", "", grep("Total Objects", out, value = TRUE)))
  b <- as.numeric(sub(".*Total Size: *", "", grep("Total Size", out, value = TRUE)))
  c(n = if (length(n)) n else 0L, bytes = if (length(b)) b else 0) }
host_count <- function(v) as.integer(system2("ssh", c("msens", shQuote(glue(
  "[ -d {host_dir(v)} ] && find {host_dir(v)} -type f | wc -l || echo 0"))), stdout = TRUE))
inv <- function() do.call(rbind, lapply(vers, function(v) {
  s <- s3_count(glue("{v}/native/")); k <- sum(vapply(keep, function(p) s3_count(glue("{v}/native/{p}/"))[["n"]], numeric(1)))
  data.frame(ver = v, s3_objects = s[["n"]], s3_gb = round(s[["bytes"]] / 1e9, 1), s3_kept = k, host_files = host_count(v)) }))
before <- inv(); print(before, row.names = FALSE)

if (!go) { say("dry run: nothing deleted (add --go)"); quit(status = 0) }

# --go: delete -------------------------------------------------------------------------------------------
t0 <- Sys.time()
for (v in vers) {
  ex <- unlist(lapply(keep, function(p) c("--exclude", shQuote(paste0(p, "/*")))))
  rc <- system2("aws", c("s3", "rm", shQuote(glue("s3://{bucket}/{root}/{v}/native/")), "--recursive", "--only-show-errors", ex))
  if (!identical(as.integer(rc), 0L)) stop(sprintf("aws s3 rm failed for %s", v))
  say("deleted s3://%s/%s/%s/native/ (kept %s)", bucket, root, v, paste(keep, collapse = ", "))
  rc <- system2("ssh", c("msens", shQuote(glue("rm -rf {host_dir(v)}"))))
  if (!identical(as.integer(rc), 0L)) stop(sprintf("rm -rf of %s failed", host_dir(v)))
  say("removed msens:%s", host_dir(v))
}
after <- inv(); print(after, row.names = FALSE)
stopifnot("objects remain beyond the kept prefixes" = all(after$s3_objects == after$s3_kept),
          "file-host directories remain" = all(after$host_files == 0L))
for (v in vers) if (!chk(v)) stop(sprintf("check_release_pointers.R %s failed AFTER the prune", v))
say("pointers still answer for %s", paste(vers, collapse = ", "))

rec <- list(pruned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), versions = vers, kept_prefixes = keep,
            before = before, after = after, store_unreferenced = nrow(unref),
            minutes = round(as.numeric(Sys.time() - t0, units = "mins"), 1))
dir.create("data/manifests", showWarnings = FALSE)
write_json(rec, "data/manifests/prune_legacy_native.json", auto_unbox = TRUE, pretty = TRUE)
say("record written: data/manifests/prune_legacy_native.json")
