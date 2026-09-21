# libs/app_bundle.R — helpers for build_app_bundle.qmd ----
#
# Two things live here rather than in msens: a PURE (no I/O) guard on what the
# push chunk may write, and the one piece of the `{ver}/app/` contract msens has
# no builder for (`taxonomy.parquet` — a curated CSV restricted to the release,
# not a database table). Both are plain functions so they can be sourced and
# exercised standalone, outside quarto, the way the notebook's self-test chunk
# does.

# ---- the push-prefix guard ---------------------------------------------------

#' Refuse to publish any key outside this release's `app/` prefix
#'
#' The one thing standing between a bug in the push chunk and an S3 key it had no
#' business touching. Pure: no S3 call, no side effect, no default-on escape hatch
#' — a vector of candidate keys in, an error (naming every offending key) or the
#' unchanged vector out. Call it on every key immediately before handing it to
#' `aws s3 cp`, not just once on a sample, so a loop that computed one bad key
#' among a thousand good ones still stops the run.
#'
#' The ONE named exception is `v7/serve/cell_model/` — v7's per-cell species-list
#' Parquet is missing from S3 today (atlas-1 plan, "Facts"), and this notebook is
#' also the vehicle for uploading it (Deliverable 1's `../serve/cell_model/`
#' row), but that must never ride along with an ordinary `{ver}/app/` push. The
#' exception is hardcoded to the literal string `"v7/serve/cell_model/"` — NOT
#' `"{ver}/serve/cell_model/"` — so running this notebook for `ver = "v9"` with
#' `allow_v7_cell_model = TRUE` (a copy-paste flag error) still refuses a
#' `v9/serve/cell_model/...` key.
#'
#' @param keys character vector of candidate object keys, relative to the atlas
#'   root (e.g. `"v9/app/boot.json"`; never a full `s3://` URI or a leading `/`)
#' @param ver the release this run is publishing (e.g. `"v9"`)
#' @param allow_v7_cell_model allow the one named exception (default `FALSE`)
#' @return `keys`, invisibly, when every key is allowed
app_bundle_assert_prefix <- function(keys, ver, allow_v7_cell_model = FALSE) {
  stopifnot(is.character(keys), is.character(ver), length(ver) == 1, nzchar(ver))
  ok_app <- startsWith(keys, sprintf("%s/app/", ver))
  ok_cm  <- if (isTRUE(allow_v7_cell_model))
    startsWith(keys, "v7/serve/cell_model/") else rep(FALSE, length(keys))
  bad <- keys[!(ok_app | ok_cm)]
  if (length(bad))
    stop(sprintf(
      "refusing to publish %d key(s) outside {%s/app/}%s:\n  %s%s",
      length(bad), ver, if (isTRUE(allow_v7_cell_model)) " or v7/serve/cell_model/" else "",
      paste(utils::head(bad, 10), collapse = "\n  "),
      if (length(bad) > 10) sprintf("\n  ... and %d more", length(bad) - 10) else ""),
      call. = FALSE)
  invisible(keys)
}

#' Self-test for [app_bundle_assert_prefix()] — run once, at notebook render time
#'
#' Written testthat-style (`expect_*`) and actually executed by the notebook's
#' "guard" chunk, not merely defined: the render itself is the proof the guard
#' works, on THIS run's msens/R versions, not just at authoring time.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_assert_prefix_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  testthat::test_that("app_bundle_assert_prefix allows every key under {ver}/app/", {
    testthat::expect_silent(app_bundle_assert_prefix(
      c("v9/app/boot.json", "v9/app/taxon/00.json", "v9/app/cell/tile=12/data_0.parquet"), "v9"))
  })
  testthat::test_that("app_bundle_assert_prefix refuses a key outside {ver}/app/", {
    testthat::expect_error(
      app_bundle_assert_prefix(c("v9/app/boot.json", "v9/tables/cell.parquet"), "v9"),
      "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix("v9/serve/model_cell/mdl_id=1/data_0.parquet", "v9"),
      "refusing to publish")
  })
  testthat::test_that("the v7 cell_model exception is OFF by default", {
    testthat::expect_error(
      app_bundle_assert_prefix("v7/serve/cell_model/tile=0/data_0.parquet", "v7"),
      "refusing to publish")
  })
  testthat::test_that("the v7 cell_model exception works only when named AND enabled", {
    testthat::expect_silent(app_bundle_assert_prefix(
      "v7/serve/cell_model/tile=0/data_0.parquet", "v7", allow_v7_cell_model = TRUE))
  })
  testthat::test_that("the v7 cell_model exception does not generalize to the running version", {
    # a copy-paste flag error (running v9 with the exception flag on) must still
    # refuse a v9/serve/cell_model key -- the exception names v7 literally
    testthat::expect_error(
      app_bundle_assert_prefix("v9/serve/cell_model/tile=0/data_0.parquet", "v9",
                               allow_v7_cell_model = TRUE),
      "refusing to publish")
  })
  testthat::test_that("the error names every offending key, not just the first", {
    err <- tryCatch(
      app_bundle_assert_prefix(c("v9/app/boot.json", "v9/tables/a.parquet", "v9/tables/b.parquet"),
                               "v9"),
      error = function(e) conditionMessage(e))
    testthat::expect_match(err, "2 key\\(s\\)")
    testthat::expect_match(err, "v9/tables/a.parquet")
    testthat::expect_match(err, "v9/tables/b.parquet")
  })
  invisible(TRUE)
}

# ---- app/taxonomy.parquet: the release's slice of the curated WoRMS hierarchy --

#' The release's slice of the curated WoRMS taxonomic hierarchy
#'
#' `app/taxonomy.parquet` — restricted to this release's taxa, so the Composition
#' treemap never ships a hierarchy row the picker cannot even select. msens has
#' no builder for this: the source is a curated CSV
#' (`data/taxonomic_hierarchy_worms_2025-10-30.csv`), not a database table, and
#' the join it feeds is a v1-v9-SHARED rule (not a per-version quirk), so it
#' belongs here rather than in `R/app_bundle.R`.
#'
#' Joins exactly the way the species app already does it
#' (`apps/scores/app.R:2857-2866`, `spp_comp`): `taxon_id` cast to character
#' against the CSV's `species_id`, restricted to `taxon_authority == "worms"`
#' (case-insensitive — the app's own comment records that the case and the
#' column TYPE both drift by generation).
#'
#' **A legacy-generation quirk, normalized here.** `msens::app_taxon_table()`'s
#' `taxon_id` is `CAST(t.taxon_id AS VARCHAR)` — fine when the underlying column
#' is INTEGER, but v1-v7's `taxon.taxon_id` is DOUBLE, and DuckDB's
#' `CAST(DOUBLE AS VARCHAR)` renders a whole number as `"125371.0"`, not
#' `"125371"` (confirmed 2026-09-21 against the real v7 release: 0 of 15,677
#' worms taxa matched the curated CSV before this fix, because the CSV's
#' `species_id` has no decimal). `.strip_trailing_dot0()` normalizes BOTH sides
#' before the join, so a v8/v9 integer id and a v1-v7 double-cast one compare
#' equal. This is fixed HERE, not in msens, because `app_taxon_table()`'s
#' `taxon_id` is used as-is by every OTHER consumer in the bundle (it is not
#' this function's place to change what msens publishes) — only this join
#' needs the normalized form.
#'
#' @param taxon_tbl the release's normalized taxon table, from
#'   [msens::app_taxon_table()] (`key, sci, common, sp_cat, taxon_id,
#'   taxon_authority, ...`)
#' @param csv_path path to the curated hierarchy CSV (default: this repo's copy)
#' @return a data frame: `taxon_id` (character, renamed from the CSV's
#'   `species_id`) + the CSV's hierarchy columns, one row per release taxon x
#'   worms authority, never duplicated
app_taxonomy_table <- function(taxon_tbl,
                               csv_path = here::here(
                                 "data/taxonomic_hierarchy_worms_2025-10-30.csv")) {
  stopifnot(
    "taxon_tbl needs taxon_id and taxon_authority (see msens::app_taxon_table())" =
      all(c("taxon_id", "taxon_authority") %in% names(taxon_tbl)),
    "no curated taxonomy CSV at csv_path" = file.exists(csv_path))

  .strip_trailing_dot0 <- function(x) sub("^([0-9]+)\\.0$", "\\1", x)

  is_worms  <- !is.na(taxon_tbl$taxon_authority) &
    tolower(taxon_tbl$taxon_authority) == "worms"
  worms_ids <- unique(.strip_trailing_dot0(
    taxon_tbl$taxon_id[is_worms & !is.na(taxon_tbl$taxon_id)]))

  hier <- readr::read_csv(csv_path, show_col_types = FALSE, guess_max = Inf)
  stopifnot("csv_path is missing species_id" = "species_id" %in% names(hier))
  hier$species_id <- .strip_trailing_dot0(as.character(hier$species_id))

  out <- hier[hier$species_id %in% worms_ids, , drop = FALSE]
  out <- out[!duplicated(out$species_id), , drop = FALSE]     # one row per taxon
  names(out)[names(out) == "species_id"] <- "taxon_id"
  rownames(out) <- NULL
  out
}
