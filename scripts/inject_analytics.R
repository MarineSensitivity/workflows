# inject_analytics.R — put the GA4 tag into ALREADY-RENDERED notebook HTML.
#
# WHY THIS EXISTS: the workflows site is published from `_output/`, which is half
# Jekyll and half standalone Quarto output, and each half gets the tag a
# different way:
#
#   * the 3 Jekyll pages (index/query/compare) — via `{% include analytics.html %}`
#     in `_output/_layouts/default.html`
#   * notebooks rendered FROM NOW ON — via `include-in-header` in `_quarto.yml`
#
# That leaves the ~60 notebook HTMLs already committed: Jekyll copies them
# verbatim, and they only pick up the `_quarto.yml` header the next time their
# notebook is re-rendered — which for a finished ingest may be never. This script
# backfills those, so every published page is tagged today.
#
# IDEMPOTENT: a file that already contains the measurement ID is skipped, so it
# is safe to re-run (and safe to run after a partial pipeline re-render).
#
# Usage:
#   Rscript scripts/inject_analytics.R          # inject
#   Rscript scripts/inject_analytics.R --check  # report only, change nothing

args     <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args

dir_out  <- here::here("_output")
snippet_f <- file.path(dir_out, "_includes", "analytics.html")
stopifnot("missing _output/_includes/analytics.html" = file.exists(snippet_f))

snippet <- paste(readLines(snippet_f, warn = FALSE), collapse = "\n")
ga_id   <- regmatches(snippet, regexpr("G-[A-Z0-9]{8,}", snippet))
stopifnot("no G- measurement id found in the snippet" = length(ga_id) == 1)

# standalone Quarto output only: the Jekyll pages carry front matter (they get the
# tag from the layout, and injecting there would double-count page views).
htmls <- list.files(dir_out, pattern = "\\.html$", full.names = TRUE)
is_jekyll_page <- function(f) {
  l1 <- tryCatch(readLines(f, n = 1, warn = FALSE), error = function(e) "")
  length(l1) > 0 && trimws(l1)[1] == "---"
}

n_done <- 0L; n_skip <- 0L; n_jek <- 0L; n_nohead <- 0L
for (f in htmls) {
  if (is_jekyll_page(f)) { n_jek <- n_jek + 1L; next }
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  if (grepl(ga_id, txt, fixed = TRUE)) { n_skip <- n_skip + 1L; next }
  if (!grepl("</head>", txt, fixed = TRUE)) { n_nohead <- n_nohead + 1L; next }
  if (!check_only) {
    # inject before the FIRST </head> only
    txt <- sub("</head>", paste0(snippet, "\n</head>"), txt, fixed = TRUE)
    writeLines(txt, f)
  }
  n_done <- n_done + 1L
}

message(sprintf(
  "%s: %d notebook HTML %s, %d already tagged, %d Jekyll pages (tagged via layout), %d without <head>",
  if (check_only) "CHECK" else "INJECTED",
  n_done, if (check_only) "would be tagged" else "tagged", n_skip, n_jek, n_nohead))
