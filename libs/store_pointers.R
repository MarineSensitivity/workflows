# store_pointers.R: every URL a published release names -- the pointer set the asset store's garbage and
# prune checks are joined against (msens::store_unreferenced(), scripts/prune_legacy_native.R,
# scripts/store_gc.R). One definition, so the two destructive scripts cannot disagree about what "referenced"
# means. Read-only: anonymous HTTPS of versions.json and, per registered release, tables/native_asset.parquet
# (asset_url), tables/model_asset.parquet (cog_url, the legacy releases) and manifest.json (score COG hrefs,
# table URLs). A manifest's `app.probed` entries are capability PROBES (app_capabilities(): HEAD of a sample
# object), not pointers, so they are left out.
#
#   p <- store_pointers()            # named list, one character vector of URLs per release that publishes any
#   attr(p, "versions")              # every version versions.json registers (including those with no tables)
store_pointers <- function(base = msens::atlas_base_url(), tmp = tempfile("ptr_")) {
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  fetch <- function(u) {
    f <- file.path(tmp, gsub("[^A-Za-z0-9._-]", "_", sub(base, "", u)))
    if (!file.exists(f)) {
      r <- tryCatch(suppressWarnings(utils::download.file(u, f, quiet = TRUE, mode = "wb")), error = function(e) 1L)
      if (!identical(as.integer(r), 0L)) { unlink(f); return(NA_character_) } }
    f }
  reg <- jsonlite::fromJSON(sprintf("%s/versions.json", base), simplifyVector = FALSE)
  reg <- if (is.list(reg) && !is.null(reg$versions)) reg$versions else reg
  vers <- vapply(reg, function(x) if (!is.null(x$ver)) x$ver else x$id, character(1))
  out <- list()
  for (v in vers) {
    na <- fetch(sprintf("%s/%s/tables/native_asset.parquet", base, v))
    ma <- fetch(sprintf("%s/%s/tables/model_asset.parquet", base, v))
    mf <- fetch(sprintf("%s/%s/manifest.json", base, v))
    urls <- c(
      if (!is.na(na)) as.data.frame(arrow::read_parquet(na))$asset_url,
      if (!is.na(ma)) { m <- as.data.frame(arrow::read_parquet(ma)); if ("cog_url" %in% names(m)) m$cog_url },
      if (!is.na(mf)) { m <- jsonlite::fromJSON(mf, simplifyVector = FALSE); m$app$probed <- NULL
        t <- as.character(jsonlite::toJSON(m, auto_unbox = TRUE))
        regmatches(t, gregexpr("https?://[^\"\\\\ ]+", t))[[1]] })
    urls <- unique(stats::na.omit(urls))
    if (length(urls)) out[[v]] <- urls
  }
  stopifnot("no release publishes any pointer -- refusing to call everything garbage" = length(out) > 0)
  attr(out, "versions") <- vers
  out
}
