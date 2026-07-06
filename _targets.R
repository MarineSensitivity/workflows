# _targets.R — MarineSensitivity v8 pipeline, generated from `msens:` frontmatter
#
# The DAG is not hand-written here: every pipeline notebook declares its
# target_name / workflow_type / dependency / output in a `msens:` YAML block,
# and msens::build_targets_list() parses those blocks into the target list.
# To add or rewire a step, edit that notebook's `msens:` block — not this file.
#
#   targets::tar_make()                      # run the pipeline
#   targets::tar_make("calc_scores")         # run one target (+ its upstream)
#   targets::tar_make("validate_v7_v8")      # is the v8 migration still faithful?
#   targets::tar_visnetwork()                # dependency graph
#   targets::tar_manifest()                  # inspect targets as a data frame
#   targets::tar_outdated()                  # what would re-run

library(targets)

tar_option_set(
  packages = c("DBI", "dplyr", "duckdb", "glue", "here", "msens", "quarto"))

# use the sibling msens checkout during development so the latest generator +
# helpers are picked up without a reinstall; fall back to the installed package
if (requireNamespace("pkgload", quietly = TRUE) &&
    file.exists("../msens/DESCRIPTION")) {
  pkgload::load_all("../msens", quiet = TRUE)
} else {
  library(msens)
}

# build the target list from the `msens:` blocks in ./*.qmd
msens::build_targets_list(
  workflows_dir = getwd(),
  exclude       = NULL)
