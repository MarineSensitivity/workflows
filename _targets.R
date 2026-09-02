# _targets.R — MarineSensitivity v8 pipeline, generated from `msens:` frontmatter
#
# The DAG is not hand-written here: every pipeline notebook declares its
# target_name / workflow_type / dependency / output in a `msens:` YAML block,
# and msens::build_targets_list() parses those blocks into the target list.
# To add or rewire a step, edit that notebook's `msens:` block — not this file.
#
#   targets::tar_make()                      # run the pipeline
#   targets::tar_make("calc_scores")         # run one target (+ its upstream)
#   Rscript scripts/render_compare.R v8 v9   # version comparison report (not a target)
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
# Density ingests (Phase D) stay excluded until their density is folded into the merge — that is a
# composite-scoring change to validate with `pra_score_delta` (merge_models_prep drops gm/nc unless
# MERGE_FOLD_DENSITY=1). Override with TARGETS_EXCLUDE="a,b" (or TARGETS_EXCLUDE="" for all).
# default excludes:
#   ingest_sdm_nc — DONE (in-repo NCCOS COGs -> dist/dataset=nc, 313 mdl_keys); excluded pending
#     native-density publishing + the merge fold-in decision, not because it fails.
#   ingest_sdm_gm — rewritten to the v8 dist-Parquet pattern but UNRUN: its 19 shapefiles +
#     spp_gmx.xlsx live on Drive, so it needs GM_SHP_DIR (or ~/_big hydration) + a run on a host
#     with those inputs (e.g. msens1). Would fail without them.
#   ingest_taxon — WIP notebook hardcoding its own Drive paths (connects to an invalid
#     Drive sdm.duckdb); produces only DuckDB taxonomy (no Parquet) and its outputs already
#     exist in spp.duckdb, so it's not needed for a fresh-Parquet render. Needs a separate
#     paths.R cleanup, out of scope for the parquet/workflow revamp.
msens::build_targets_list(
  workflows_dir = getwd(),
  exclude = { e <- Sys.getenv("TARGETS_EXCLUDE", unset = "ingest_sdm_gm,ingest_sdm_nc,ingest_taxon")
              if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else NULL })
