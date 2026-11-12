# render_compare_sequence.R — render the registered version comparisons (data/compare_pairs.csv),
# each to its own versioned file, then rebuild the archive index _output/compare_versions.html.
#
#   Rscript scripts/render_compare_sequence.R                 # every registered pair not yet rendered
#   Rscript scripts/render_compare_sequence.R --redo          # every registered pair whose databases exist
#   Rscript scripts/render_compare_sequence.R v10:v11 v8:v10  # these pairs (rendered even if present)
#   Rscript scripts/render_compare_sequence.R --lineage main  # the main sequence (v7 -> v8 -> v10 -> v11)
#
# A pair is skipped (with a message) when either release's sdm.duckdb is not on this machine.
suppressMessages(library(glue))
source(here::here("libs/paths.R"))
source(here::here("scripts/render_compare.R"))
source(here::here("scripts/build_compare_index.R"))

args    <- commandArgs(trailingOnly = TRUE)
redo    <- "--redo" %in% args
lin     <- if ("--lineage" %in% args) args[which(args == "--lineage") + 1] else NA_character_
named   <- grep("^v[0-9]+[a-z]?:v[0-9]+[a-z]?$", args, value = TRUE)

pairs <- utils::read.csv(here::here("data/compare_pairs.csv"), stringsAsFactors = FALSE)
pairs <- pairs[order(pairs$ord), ]
if (!is.na(lin)) pairs <- pairs[pairs$lineage == lin, ]
if (length(named)) {
  ab <- do.call(rbind, strsplit(named, ":"))
  pairs <- data.frame(version_a = ab[, 1], version_b = ab[, 2])
}

sdm_for <- function(v) sub(glue("/{ver}/"), glue("/{v}/"), path.expand(sdm_db))
out_of  <- function(a, b) here::here("_output", glue("compare_versions_{a}_vs_{b}.html"))

res <- character()
for (i in seq_len(nrow(pairs))) {
  a <- pairs$version_a[i]; b <- pairs$version_b[i]
  if (!all(file.exists(sdm_for(a), sdm_for(b)))) { message(glue("skip {a} vs {b}: sdm.duckdb missing on this machine")); next }
  if (file.exists(out_of(a, b)) && !redo && !length(named)) { message(glue("have {a} vs {b}")); next }
  message(glue("render {a} vs {b} ..."))
  ok <- tryCatch({ render_compare(a, b, quiet = TRUE, index = FALSE); TRUE },
                 error = \(e) { message(glue("FAILED {a} vs {b}: {conditionMessage(e)}")); FALSE })
  res <- c(res, glue("{a} vs {b}: {if (ok) 'ok' else 'FAILED'}"))
}
build_compare_index()
if (length(res)) message(paste(res, collapse = "\n"))
if (any(grepl("FAILED", res))) quit(status = 1)
