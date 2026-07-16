# render_validate.R — render the parameterized version-comparison report with a
# version-suffixed output filename. The comparison *core* is msens::pra_score_delta /
# score_delta / score_delta_summary (unit-tested); this only drives the render.
#
#   source("scripts/render_validate.R"); render_versions("v7", "v8")
#   -> _output/validate_v7_v8.html
suppressMessages(library(glue))

render_versions <- function(version_a = "v7", version_b = "v8",
                            metric_key = NULL, quiet = FALSE) {
  out    <- glue("validate_{version_a}_{version_b}.html")
  params <- list(version_a = version_a, version_b = version_b)
  if (!is.null(metric_key)) params$metric_key <- metric_key
  quarto::quarto_render(
    here::here("validate_versions.qmd"),
    output_file    = out,          # lands in the project output-dir (_output/)
    execute_params = params,
    quiet          = quiet)
  message(glue("rendered _output/{out}"))
  invisible(out)
}
