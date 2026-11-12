# render_compare.R — render the parameterized version-comparison report with a
# version-suffixed output filename. The comparison *core* is msens::pra_score_delta /
# zone_scores / zone_crosswalk / zone_score_delta / score_delta (unit-tested); this only drives the render.
#
#   source("scripts/render_compare.R"); render_compare("v6", "v9")
#   -> _output/compare_versions_v6_vs_v9.html   (then the archive index _output/compare_versions.html)
#
# compare_versions.qmd refuses to render unless MSENS_COMPARE_OUT names the versioned file, so a bare
# `quarto render compare_versions.qmd` cannot overwrite the archive index. Every pair of the release
# lineage is registered in data/compare_pairs.csv; scripts/render_compare_sequence.R renders them.
suppressMessages(library(glue))

render_compare <- function(version_a = "v8", version_b = "v9",
                           metric_key = NULL, quiet = FALSE, index = TRUE) {
  pat <- "^v[0-9]+[a-z]?$"
  stopifnot(grepl(pat, version_a), grepl(pat, version_b), version_a != version_b)
  out    <- glue("compare_versions_{version_a}_vs_{version_b}.html")
  params <- list(version_a = version_a, version_b = version_b)
  if (!is.null(metric_key)) params$metric_key <- metric_key
  old <- Sys.getenv("MSENS_COMPARE_OUT", NA)
  Sys.setenv(MSENS_COMPARE_OUT = out)          # the qmd's guard: the output names its two versions
  on.exit(if (is.na(old)) Sys.unsetenv("MSENS_COMPARE_OUT") else Sys.setenv(MSENS_COMPARE_OUT = old), add = TRUE)
  quarto::quarto_render(
    here::here("compare_versions.qmd"),
    output_file    = out,          # lands in the project output-dir (_output/)
    execute_params = params,
    metadata       = list(title = glue("Version comparison: {version_a} vs {version_b}")),
    quiet          = quiet)
  message(glue("rendered _output/{out}"))
  if (index) {
    source(here::here("scripts/build_compare_index.R"), local = TRUE)
    build_compare_index()
  }
  invisible(out)
}

# command-line use: Rscript scripts/render_compare.R v6 v9
if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 2) render_compare(a[1], a[2])
}
