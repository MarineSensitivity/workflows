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
# IDEMPOTENT *AND* UPDATABLE: the snippet is delimited by
# `<!-- msens:analytics:begin -->` / `:end`, so a re-run REPLACES an older
# injected block rather than skipping it or stacking a second copy. Files whose
# block already matches the current snippet are left untouched. Safe to re-run,
# and safe after a partial pipeline re-render.
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
stopifnot(
  "no G- measurement id found in the snippet" = length(ga_id) == 1,
  "snippet must carry the msens:analytics begin/end markers" =
    grepl("msens:analytics:begin", snippet) && grepl("msens:analytics:end", snippet),
  # Liquid parses HTML comments too: a literal include tag naming this file makes
  # _layouts/default.html include it recursively ("stack level too deep").
  "snippet must not contain a Liquid tag" = !grepl("\\{%", snippet))

# matches a previously injected block, markers included
block_rx <- "(?s)<!-- msens:analytics:begin -->.*?<!-- msens:analytics:end -->\\s*"

# standalone Quarto output only: the Jekyll pages carry front matter (they get the
# tag from the layout, and injecting there would double-count page views).
htmls <- list.files(dir_out, pattern = "\\.html$", full.names = TRUE)
is_jekyll_page <- function(f) {
  l1 <- tryCatch(readLines(f, n = 1, warn = FALSE), error = function(e) "")
  length(l1) > 0 && trimws(l1)[1] == "---"
}

n_new <- 0L; n_upd <- 0L; n_skip <- 0L; n_jek <- 0L; n_nohead <- 0L
for (f in htmls) {
  if (is_jekyll_page(f)) { n_jek <- n_jek + 1L; next }
  txt  <- paste(readLines(f, warn = FALSE), collapse = "\n")
  had  <- grepl(block_rx, txt, perl = TRUE)
  # strip any previous block so a re-run replaces rather than stacks
  base <- if (had) sub(block_rx, "", txt, perl = TRUE) else txt
  if (had && identical(txt, sub(block_rx, paste0(snippet, "\n"), txt, perl = TRUE))) {
    n_skip <- n_skip + 1L; next                       # already current
  }
  if (!grepl("</head>", base, fixed = TRUE)) { n_nohead <- n_nohead + 1L; next }
  if (!check_only) {
    # inject before the FIRST </head> only
    writeLines(sub("</head>", paste0(snippet, "\n</head>"), base, fixed = TRUE), f)
  }
  if (had) n_upd <- n_upd + 1L else n_new <- n_new + 1L
}

message(sprintf(
  "%s: %d newly tagged, %d block%s updated, %d already current, %d Jekyll pages (tagged via layout), %d without <head>",
  if (check_only) "CHECK" else "INJECTED",
  n_new, n_upd, if (n_upd == 1L) "" else "s", n_skip, n_jek, n_nohead))
