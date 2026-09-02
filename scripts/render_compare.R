# render_compare.R — render the parameterized version-comparison report with a
# version-suffixed output filename. The comparison *core* is msens::pra_score_delta /
# zone_scores / zone_score_delta / score_delta (unit-tested); this only drives the render.
#
#   source("scripts/render_compare.R"); render_compare("v6", "v9")
#   -> _output/compare_versions_v6_vs_v9.html
suppressMessages(library(glue))

render_compare <- function(version_a = "v8", version_b = "v9",
                           metric_key = NULL, zone_set_key = NULL, quiet = FALSE) {
  out    <- glue("compare_versions_{version_a}_vs_{version_b}.html")
  params <- list(version_a = version_a, version_b = version_b)
  if (!is.null(metric_key))   params$metric_key   <- metric_key
  if (!is.null(zone_set_key)) params$zone_set_key <- zone_set_key
  quarto::quarto_render(
    here::here("compare_versions.qmd"),
    output_file    = out,          # lands in the project output-dir (_output/)
    execute_params = params,
    quiet          = quiet)
  message(glue("rendered _output/{out}"))
  invisible(out)
}

# command-line use: Rscript scripts/render_compare.R v6 v9
if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 2) render_compare(a[1], a[2])
}
