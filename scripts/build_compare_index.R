# build_compare_index.R — the archive index of every version-comparison report.
#
# Writes _output/compare_versions.html: one row per registered pair (data/compare_pairs.csv),
# grouped by lineage (the main release sequence v7 -> v8 -> v10 -> v11 first), linking each
# _output/compare_versions_{a}_vs_{b}.html that exists, plus any rendered report not registered.
# Called at the end of every scripts/render_compare.R render; run alone with
#   Rscript scripts/build_compare_index.R
suppressMessages({ library(glue); library(htmltools) })

build_compare_index <- function(out_dir = here::here("_output"),
                                pairs_csv = here::here("data/compare_pairs.csv"),
                                versions_csv = here::here("data/versions.csv")) {
  pairs <- utils::read.csv(pairs_csv, stringsAsFactors = FALSE)
  vers  <- utils::read.csv(versions_csv, stringsAsFactors = FALSE)
  stopifnot(all(c("version_a", "version_b", "lineage", "ord", "note") %in% names(pairs)),
            !anyDuplicated(paste(pairs$version_a, pairs$version_b)))
  pairs <- pairs[order(pairs$ord), ]

  # rendered reports on disk, including any pair not (yet) in the registry
  files <- list.files(out_dir, "^compare_versions_v[0-9]+[a-z]?_vs_v[0-9]+[a-z]?\\.html$")
  ab    <- regmatches(files, regexec("^compare_versions_(v[0-9]+[a-z]?)_vs_(v[0-9]+[a-z]?)\\.html$", files))
  extra <- do.call(rbind, lapply(ab, \(x) data.frame(version_a = x[2], version_b = x[3])))
  if (!is.null(extra)) {
    extra <- extra[!paste(extra$version_a, extra$version_b) %in% paste(pairs$version_a, pairs$version_b), ]
    if (nrow(extra)) pairs <- rbind(pairs, data.frame(extra, lineage = "unregistered", ord = 999, note = ""))
  }

  title_of <- function(v) { t <- vers$title[match(v, vers$ver)]; ifelse(is.na(t), "", t) }
  status_of <- function(v) { s <- paste(vers$status[match(v, vers$ver)], vers$access[match(v, vers$ver)]); ifelse(grepl("NA", s), "", s) }
  LINEAGE <- c(main = "Main release sequence", cross = "Across the sequence", patch = "Patches of v7",
               branch = "Side branches", history = "Earlier releases", unregistered = "Other rendered reports")

  row_of <- function(i) {
    p <- pairs[i, ]
    f <- glue("compare_versions_{p$version_a}_vs_{p$version_b}.html")
    fp <- file.path(out_dir, f)
    link <- if (file.exists(fp)) tags$a(href = f, glue("{p$version_a} vs {p$version_b}")) else
      tags$span(class = "missing", glue("{p$version_a} vs {p$version_b} (not yet rendered)"))
    when <- if (file.exists(fp)) format(file.mtime(fp), "%Y-%m-%d") else ""
    tags$tr(tags$td(link),
            tags$td(tags$b(p$version_a), " ", title_of(p$version_a), tags$br(), tags$small(status_of(p$version_a))),
            tags$td(tags$b(p$version_b), " ", title_of(p$version_b), tags$br(), tags$small(status_of(p$version_b))),
            tags$td(p$note), tags$td(when))
  }
  sections <- lapply(intersect(names(LINEAGE), unique(pairs$lineage)), \(lg) {
    idx <- which(pairs$lineage == lg)
    tagList(tags$h2(LINEAGE[[lg]]),
            tags$table(tags$thead(tags$tr(tags$th("report"), tags$th("version A"), tags$th("version B"),
                                          tags$th("what changes"), tags$th("rendered"))),
                       tags$tbody(lapply(idx, row_of))))
  })
  main <- pairs[pairs$lineage == "main", ]
  chain <- paste(c(main$version_a[1], main$version_b), collapse = " → ")

  head <- paste0('<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
                 '<title>Version comparisons</title><style>', "
                body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;max-width:1100px;margin:24px auto;padding:0 16px;line-height:1.45;color:#1a1a1a;background:#fff}
                table{border-collapse:collapse;width:100%;margin-bottom:8px}th,td{border-bottom:1px solid #ddd;padding:6px 8px;text-align:left;vertical-align:top;font-size:14px}
                th{background:#f4f4f4}.missing{color:#999}small{color:#666}code{background:#f4f4f4;padding:1px 4px}", '</style></head>')
  body <- tags$body(
      tags$h1("Version comparisons"),
      tags$p("Program-Area scores of two releases side by side: the composite and every component, within each ecoregion and, from v10, across the nation. ",
             "Each report is a fixed file named for its two versions, so none is overwritten by a later version. ",
             "Main sequence: ", tags$b(chain), "."),
      tags$p(tags$small("Rendered by ", tags$code("scripts/render_compare_sequence.R"), " from ", tags$code("compare_versions.qmd"),
                        "; the pairs are ", tags$code("data/compare_pairs.csv"), ". Index built ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), ".")),
      sections)
  out <- file.path(out_dir, "compare_versions.html")
  writeLines(c("<!doctype html>", '<html lang="en">', head, as.character(body), "</html>"), out, useBytes = TRUE)
  message(glue("wrote {out} ({nrow(pairs)} pairs, {sum(file.exists(file.path(out_dir, glue('compare_versions_{pairs$version_a}_vs_{pairs$version_b}.html'))))} rendered)"))
  invisible(out)
}

if (sys.nframe() == 0L && !interactive()) build_compare_index()
