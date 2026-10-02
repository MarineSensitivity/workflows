# publish gate: no bulk bytes from the VM ----
#
# Rule: bulk files (.tif .pmtiles .parquet .gpkg ...) are fetched from the OBJECT STORE, never from
# a VM host (file / storage / app / preview ...). A pointer table, manifest, bundle shard or STAC
# tree that names a bulk file on a VM host sends every viewer's bytes through the server and on
# to its egress bill (the 2026-10 egress incident: ~150 GB/day of per-model PMTiles from the file
# host). msens::url_audit() classifies a URL (store | computed | vm_bulk | page | external);
# msens::url_audit_assert() stops on any vm_bulk URL. This wraps both for the notebooks:
#
#   source(here::here("libs/url_gate.R"))
#   url_gate(na$asset_url, "staged v9 native_asset.asset_url")   # at the point the URLs are final,
#                                                                # BEFORE any push chunk
#
# Escape hatch: URL_AUDIT_ALLOW='^https://file\\.marinesensitivity\\.org/pmtiles/(v8|v9)/' (comma-
# separated regexes; default none) lets matching vm_bulk URLs through. It is for a deliberate,
# temporary exemption -- every use is logged at WARN with the regex and how many URLs it passed.

# regexes from the env var, comma-separated, blanks dropped ----
url_gate_allow <- function(x = Sys.getenv("URL_AUDIT_ALLOW")) {
  a <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  a[nzchar(a)]
}

url_gate <- function(urls, what = "urls", allow = url_gate_allow()) {
  n_na <- sum(is.na(urls))
  urls <- urls[!is.na(urls)]
  au   <- msens::url_audit(urls)

  # the class table ----
  tb <- table(factor(au$class, levels = c("store", "computed", "vm_bulk", "page", "external")))
  logger::log_info("url gate [{what}]: {length(urls)} urls ({n_na} NA) -- {paste(names(tb), tb, sep = ' = ', collapse = ', ')}")

  # an escape hatch that lets something through is never silent ----
  bulk <- au$url[au$class == "vm_bulk"]
  for (rx in allow) {
    n_pass <- sum(grepl(rx, bulk))
    if (n_pass > 0)
      logger::log_warn("url gate [{what}]: URL_AUDIT_ALLOW regex '{rx}' lets {n_pass} vm_bulk url(s) through")
  }

  msens::url_audit_assert(urls, what = what, allow = allow)
  invisible(au)
}
