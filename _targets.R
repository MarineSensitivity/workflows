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

# build the target list from the `msens:` blocks in ./*.qmd.
# gm/nc are not yet ingested (Phase D): their notebooks would fail and — via the
# `[auto]` dependency — block build_registry/release/publish_native. Exclude them
# until ready. Override with TARGETS_EXCLUDE="a,b" (or TARGETS_EXCLUDE="" for all).
# default excludes:
#   ingest_sdm_gm/ingest_sdm_nc — not yet ingested (Phase D)
#   ingest_taxon — WIP notebook hardcoding its own Drive paths (connects to an invalid
#     Drive sdm.duckdb); produces only DuckDB taxonomy (no Parquet) and its outputs already
#     exist in spp.duckdb, so it's not needed for a fresh-Parquet render. Needs a separate
#     paths.R cleanup, out of scope for the parquet/workflow revamp.
msens::build_targets_list(
  workflows_dir = getwd(),
  exclude = { e <- Sys.getenv("TARGETS_EXCLUDE", unset = "ingest_sdm_gm,ingest_sdm_nc,ingest_taxon")
              if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else NULL })
