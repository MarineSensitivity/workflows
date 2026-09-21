# libs/app_bundle.R — helpers for build_app_bundle.qmd ----
#
# Four things live here rather than in msens: a WHITELIST guard on what the
# push chunk may write, a strict parser for the publish on/off flags, the
# geometry-key lookup app_units() needs to exclude whole-study-area rollups,
# and the one piece of the `{ver}/app/` contract msens has no builder for
# (`taxonomy.parquet` — a curated CSV restricted to the release, not a
# database table). All four are plain functions so they can be sourced and
# exercised standalone, outside quarto, the way the notebook's self-test
# chunks do.

# ---- the push-prefix guard (WHITELIST, not startsWith()) ---------------------
#
# review round 1 (2026-09-21): the prior startsWith()-based guard ALLOWED
# "v9/app/../tables/x.parquet", "v9/app/./../../v7/app/x", backslash and
# percent-encoded ".." forms, "v9/app/" (empty basename), "v9/app/dir/",
# "v9/app/sub//x.json", control characters, embedded/trailing newlines,
# "?x=1", "#frag", and length(keys) == 0 silently passed (an empty vector
# has no bad element to complain about). None of those are legitimate S3
# keys; a whitelist that only ever ACCEPTS the shapes this bundle actually
# writes closes all of them at once, rather than enumerating attacks.

# is `s` a single valid PATH SEGMENT: non-empty, only [A-Za-z0-9._=-], and
# never literally "." or ".." (those two are the only strings the segment
# charset would otherwise accept that mean something other than themselves)
.app_bundle_valid_segment <- function(s)
  nzchar(s) && grepl("^[A-Za-z0-9._=-]+$", s, perl = TRUE) && !(s %in% c(".", ".."))

# is `key` exactly `{prefix}/{seg}(/{seg})*` for one or more valid segments —
# checked structurally (prefix match, then split, then per-segment), not by
# a single do-everything regex, so each rule stays independently testable
.app_bundle_key_under_prefix <- function(key, prefix) {
  if (!is.character(key) || length(key) != 1L || is.na(key)) return(FALSE)
  # control characters (incl. embedded/trailing newline), '%', backslash, and
  # URL query/fragment delimiters are refused OUTRIGHT, anywhere in the key —
  # never reached the segment charset check below anyway, but explicit here
  # so each is its own named, testable rule rather than an emergent side
  # effect of the character class
  # R string literals cannot embed \x00 at all ("nul character not allowed"),
  # and a real NUL cannot occur in an R character value from readLines()/
  # fs::path()/etc anyway (it is a C string terminator) -- \x01-\x1f + \x7f
  # covers every actual control character a key could carry
  if (grepl("[\x01-\x1f\x7f%\\\\]", key, perl = TRUE)) return(FALSE)
  if (grepl("[?#]", key, perl = TRUE)) return(FALSE)
  want <- paste0(prefix, "/")
  if (!startsWith(key, want)) return(FALSE)
  if (endsWith(key, "/")) return(FALSE)                    # empty basename / trailing slash
  rest <- substring(key, nchar(want) + 1L)
  if (!nzchar(rest)) return(FALSE)
  segs <- strsplit(rest, "/", fixed = TRUE)[[1]]
  # strsplit() drops a TRAILING empty field (already excluded above by
  # endsWith) but keeps an EMPTY MIDDLE one ("a//b" -> c("a","","b")), which
  # .app_bundle_valid_segment() rejects via nzchar()
  length(segs) > 0L && all(vapply(segs, .app_bundle_valid_segment, logical(1)))
}

#' Refuse to publish any key outside this release's `app/` prefix
#'
#' The one thing standing between a bug in the push chunk and an S3 key it had no
#' business touching. A WHITELIST, not a blocklist: a key is allowed only if it
#' matches `{ver}/app/(seg/)*seg` (or the one named exception below) where every
#' `seg` is non-empty, drawn from `[A-Za-z0-9._=-]`, and never literally `.` or
#' `..` — so `../`, backslash and percent-encoded traversal, an empty segment
#' (a trailing or doubled `/`), a control character, an embedded/trailing
#' newline, and a `?`/`#` are all refused by construction, not by enumeration.
#' Pure: no S3 call, no side effect, no default-on escape hatch. Call it on
#' every key immediately before handing it to `aws s3 cp`, not just once on a
#' sample, so a loop that computed one bad key among a thousand good ones still
#' stops the WHOLE run — including the good keys, which are never partially
#' published.
#'
#' `length(keys) == 0` is itself an error (not a silent no-op): an empty vector
#' has no bad element for a naive checker to find, which is exactly the shape
#' of bug this guard exists to catch.
#'
#' The ONE named exception is `v7/serve/cell_model/` — v7's per-cell species-list
#' Parquet is missing from S3 today (atlas-1 plan, "Facts"), and this notebook is
#' also the vehicle for uploading it (Deliverable 1's `../serve/cell_model/`
#' row), but that must never ride along with an ordinary `{ver}/app/` push. The
#' exception is hardcoded to the literal string `"v7/serve/cell_model/"` — NOT
#' `"{ver}/serve/cell_model/"` — so running this notebook for `ver = "v9"` with
#' `allow_v7_cell_model = TRUE` (a copy-paste flag error) still refuses a
#' `v9/serve/cell_model/...` key, and it is checked with the SAME whitelist as
#' the `app/` prefix, not a looser one.
#'
#' @param keys character vector of candidate object keys, relative to the atlas
#'   root (e.g. `"v9/app/boot.json"`; never a full `s3://` URI or a leading `/`).
#'   Build these from a directory walk of the directory you control (e.g.
#'   `list.files(dir_out, recursive = TRUE)`), and always pass the RESULT
#'   through this guard before upload — never trust a path-join helper's
#'   output unchecked, whatever computed it.
#' @param ver the release this run is publishing (e.g. `"v9"`)
#' @param allow_v7_cell_model allow the one named exception (default `FALSE`)
#' @return `keys`, invisibly, when every key is allowed
app_bundle_assert_prefix <- function(keys, ver, allow_v7_cell_model = FALSE) {
  stopifnot(is.character(ver), length(ver) == 1L, nzchar(ver))
  if (!is.character(keys))
    stop("app_bundle_assert_prefix(): `keys` must be a character vector", call. = FALSE)
  if (length(keys) == 0L)
    stop("app_bundle_assert_prefix(): `keys` must not be empty (length(keys) == 0) -- ",
         "an empty vector has no bad key for this guard to catch, which is not the same ",
         "as having verified there isn't one", call. = FALSE)

  ok_app <- vapply(keys, .app_bundle_key_under_prefix, logical(1),
                   prefix = sprintf("%s/app", ver))
  ok_cm  <- if (isTRUE(allow_v7_cell_model))
    vapply(keys, .app_bundle_key_under_prefix, logical(1), prefix = "v7/serve/cell_model")
  else rep(FALSE, length(keys))

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
#' works, on THIS run's msens/R versions, not just at authoring time. One case
#' per rule/branch, both directions: every seeded fault from the 2026-09-21
#' review round 1 must be REFUSED, and every legitimate shape must be ALLOWED.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_assert_prefix_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))

  testthat::test_that("ALLOWED: ordinary keys under {ver}/app/", {
    testthat::expect_silent(app_bundle_assert_prefix("v9/app/boot.json", "v9"))
    testthat::expect_silent(app_bundle_assert_prefix("v9/app/taxon/00.json", "v9"))
    testthat::expect_silent(app_bundle_assert_prefix(
      "v9/app/cell/tile=12/data_0.parquet", "v9"))
    testthat::expect_silent(app_bundle_assert_prefix(
      c("v9/app/boot.json", "v9/app/taxon/00.json"), "v9"))
  })
  testthat::test_that("ALLOWED: the v7 cell_model exception, named and enabled", {
    testthat::expect_silent(app_bundle_assert_prefix(
      "v7/serve/cell_model/tile=0/data_0.parquet", "v7", allow_v7_cell_model = TRUE))
  })

  testthat::test_that("REFUSED: parent-directory traversal ('..' segment)", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/../tables/x.parquet", "v9"), "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/./../../v7/app/x", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: backslash-based traversal", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/..\\x", "v9"), "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix("v9\\app\\boot.json", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: percent-encoded traversal", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/%2e%2e/tables/x.parquet", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: empty basename (trailing slash)", {
    testthat::expect_error(app_bundle_assert_prefix("v9/app/", "v9"), "refusing to publish")
    testthat::expect_error(app_bundle_assert_prefix("v9/app/dir/", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: doubled slash (empty middle segment)", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/sub//x.json", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: control characters", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/bo\x01ot.json", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: embedded / trailing newline", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/boot.json\nEvil: header", "v9"), "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/boot.json\n", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: query string / fragment smuggling", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/boot.json?x=1", "v9"), "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix("v9/app/boot.json#frag", "v9"), "refusing to publish")
  })
  testthat::test_that("REFUSED: length(keys) == 0 is itself an error", {
    testthat::expect_error(app_bundle_assert_prefix(character(0), "v9"), "must not be empty")
  })
  testthat::test_that("REFUSED: a vector mixing one good key and one bad key", {
    err <- tryCatch(
      app_bundle_assert_prefix(c("v9/app/boot.json", "v9/app/../tables/x.parquet"), "v9"),
      error = function(e) conditionMessage(e))
    testthat::expect_match(err, "refusing to publish")
    testthat::expect_match(err, "1 key\\(s\\)")
  })
  testthat::test_that("REFUSED: a key outside {ver}/app/ entirely", {
    testthat::expect_error(
      app_bundle_assert_prefix(c("v9/app/boot.json", "v9/tables/cell.parquet"), "v9"),
      "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix("v9/serve/model_cell/mdl_id=1/data_0.parquet", "v9"),
      "refusing to publish")
  })
  testthat::test_that("REFUSED: the v7 cell_model exception is OFF by default", {
    testthat::expect_error(
      app_bundle_assert_prefix("v7/serve/cell_model/tile=0/data_0.parquet", "v7"),
      "refusing to publish")
  })
  testthat::test_that("REFUSED: the v7 exception does not generalize to the running version", {
    # a copy-paste flag error (running v9 with the exception flag on) must still
    # refuse a v9/serve/cell_model key -- the exception names v7 literally
    testthat::expect_error(
      app_bundle_assert_prefix("v9/serve/cell_model/tile=0/data_0.parquet", "v9",
                               allow_v7_cell_model = TRUE),
      "refusing to publish")
  })
  testthat::test_that("REFUSED: the v7 exception is itself whitelisted, not a bare prefix", {
    testthat::expect_error(
      app_bundle_assert_prefix("v7/serve/cell_model/../../app/x", "v7",
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

# ---- strict publish-flag parsing ---------------------------------------------
#
# review round 1: Sys.getenv("APP_BUNDLE_S3") != "" means "0", "false", "no",
# and "=true" all PUSH -- a publish flag is either exactly on or the notebook
# must refuse to guess.

#' Strictly parse a boolean publish-gate environment variable
#'
#' ON only when the value is EXACTLY `"1"`. Unset or `""` is OFF. Anything
#' else — `"0"`, `"true"`, `"false"`, `"no"`, `"TRUE"`, trailing whitespace —
#' is a hard STOP naming the variable and the value it actually got, so a
#' typo (`APP_BUNDLE_S3=true`) can never be silently read as "did not push"
#' (or, the other direction, as "did").
#'
#' @param name environment variable name (e.g. `"APP_BUNDLE_S3"`)
#' @return `TRUE` or `FALSE`; never returns for an unrecognized value
app_bundle_flag <- function(name) {
  v <- Sys.getenv(name, unset = "")
  if (identical(v, "")) return(FALSE)
  if (identical(v, "1")) return(TRUE)
  stop(sprintf(
    "%s=\"%s\" is not a recognized value -- set %s=1 to enable it, or leave it unset (or \"\") ",
    name, v, name),
    "to disable it. Refusing to guess at \"0\"/\"true\"/\"false\"/\"no\"/etc: a publish flag ",
    "is exactly on or off, never inferred.", call. = FALSE)
}

#' Self-test for [app_bundle_flag()]
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_flag_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  once <- function(value, expr) {
    nm <- "APP_BUNDLE_FLAG_SELFTEST_VAR"
    old <- Sys.getenv(nm, unset = NA)
    on.exit({
      if (is.na(old)) Sys.unsetenv(nm) else do.call(Sys.setenv, stats::setNames(list(old), nm))
    }, add = TRUE)
    if (is.null(value)) Sys.unsetenv(nm) else do.call(Sys.setenv, stats::setNames(list(value), nm))
    force(expr)
  }
  testthat::test_that("unset is OFF", {
    testthat::expect_false(once(NULL, app_bundle_flag("APP_BUNDLE_FLAG_SELFTEST_VAR")))
  })
  testthat::test_that("empty string is OFF", {
    testthat::expect_false(once("", app_bundle_flag("APP_BUNDLE_FLAG_SELFTEST_VAR")))
  })
  testthat::test_that("exactly \"1\" is ON", {
    testthat::expect_true(once("1", app_bundle_flag("APP_BUNDLE_FLAG_SELFTEST_VAR")))
  })
  for (bad in c("0", "true", "TRUE", "false", "no", "yes", " 1", "1 ", "2"))
    testthat::test_that(sprintf("REFUSED (errors): %s = %s", "APP_BUNDLE_FLAG_SELFTEST_VAR", bad), {
      testthat::expect_error(
        once(bad, app_bundle_flag("APP_BUNDLE_FLAG_SELFTEST_VAR")),
        "not a recognized value")
    })
  invisible(TRUE)
}

# ---- byte-safe gzip (never readLines()/writeLines()) ------------------------
#
# review round 1: the push chunk gzipped through writeLines(readLines(file)),
# a TEXT round-trip that can silently drop a final line with no trailing
# newline, translate line endings, or misread encoding -- so the bytes
# uploaded were not provably the bytes app_validate() had just checked.

#' Gzip a file's bytes, byte-for-byte
#'
#' @param src_path the file to compress (read as RAW, never as text lines)
#' @param dest_path where to write the `.gz` (created/overwritten)
#' @param compression 1-9 (default 9, matching the prior behavior)
#' @return `dest_path`, invisibly
app_bundle_gzip_file <- function(src_path, dest_path, compression = 9L) {
  raw <- readBin(src_path, "raw", n = file.size(src_path))
  con <- gzfile(dest_path, "wb", compression = compression)
  on.exit(close(con), add = TRUE)
  writeBin(raw, con)
  invisible(dest_path)
}

#' Assert a gzip file decompresses to its source, byte-for-byte
#'
#' The proof that what would be uploaded IS what `app_validate()` checked —
#' run after every [app_bundle_gzip_file()] in the push chunk, before `aws s3
#' cp` ever sees the `.gz`.
#'
#' @param gz_path the gzip file (as it would be uploaded)
#' @param src_path the original, already-validated file
#' @return `TRUE`, invisibly; errors on any mismatch
app_bundle_assert_gzip_roundtrip <- function(gz_path, src_path) {
  want <- readBin(src_path, "raw", n = file.size(src_path))
  con  <- gzfile(gz_path, "rb")
  on.exit(close(con), add = TRUE)
  got  <- readBin(con, "raw", n = length(want) + 1L)
  if (!identical(got, want))
    stop(sprintf(
      "gzip round-trip mismatch: %s does not decompress to %s byte-for-byte (%d vs %d bytes)",
      gz_path, src_path, length(got), length(want)), call. = FALSE)
  invisible(TRUE)
}

#' Self-test for [app_bundle_gzip_file()] / [app_bundle_assert_gzip_roundtrip()]
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_gzip_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  testthat::test_that("a file with no trailing newline round-trips exactly", {
    src <- tempfile(); gz <- tempfile(fileext = ".gz")
    writeBin(charToRaw("no trailing newline"), src)   # deliberately no \n at EOF
    app_bundle_gzip_file(src, gz)
    testthat::expect_true(app_bundle_assert_gzip_roundtrip(gz, src))
    unlink(c(src, gz))
  })
  testthat::test_that("a corrupted .gz is caught, not silently accepted", {
    src <- tempfile(); gz <- tempfile(fileext = ".gz")
    writeBin(charToRaw("boot.json bytes"), src)
    app_bundle_gzip_file(src, gz)
    writeBin(charToRaw("boot.json BYTES"), src)   # mutate the source AFTER gzipping
    testthat::expect_error(app_bundle_assert_gzip_roundtrip(gz, src), "gzip round-trip mismatch")
    unlink(c(src, gz))
  })
  invisible(TRUE)
}

# ---- geometry keys for app_units() -------------------------------------------

#' Geometry keys actually present in each release's PUBLISHED zone geometry
#'
#' `msens::app_units()`'s `geom_keys` argument excludes whole-study-area
#' rollups (`USA` on v8, `FULL` on v7) from the drawable-unit list, WITHOUT
#' hardcoding either name — it just requires that at least two of a unit's
#' keys genuinely exist in the geometry the app will draw. This resolves that
#' geometry from the zone-set registry's own `source` GeoPackage
#' (`data/zone_sets.csv`), never a per-version guessed filename, and reads
#' the key column under the SAME name as `fld` (verified 2026-09-21 against
#' the real v7 ecoregion and Program Area GeoPackages: `ecoregion_key`,
#' `programarea_key` are literal attribute columns in each, not something
#' this function invents).
#'
#' A zone type whose source cannot be read (missing file, no such column) is
#' OMITTED from the result, never given an empty vector — `app_units()`
#' documents that a type absent from `geom_keys` is treated as "geometry not
#' checked", not "geometry has no keys", so this failure mode degrades to the
#' pre-review-fix behavior for that one type rather than hiding every unit.
#'
#' @param zones the release's zones table (`manifest_build()`'s `$zones`:
#'   `fld`, `zone_set_key`, ...)
#' @param zone_sets the zone-set registry (`data/zone_sets.csv`)
#' @param dir_derived base directory each registry `source` path is relative to
#' @return named list `zone_type -> character keys`
app_bundle_geom_keys <- function(zones, zone_sets, dir_derived) {
  out <- list()
  if (is.null(zones) || !nrow(zones) || !"zone_set_key" %in% names(zones)) return(out)
  for (i in seq_len(nrow(zones))) {
    fld <- zones$fld[i]
    zsk <- zones$zone_set_key[i]
    if (is.na(zsk) || !nzchar(zsk)) next
    src_row <- zone_sets[zone_sets$zone_set_key == zsk, , drop = FALSE]
    if (!nrow(src_row)) next
    type <- sub("_key$", "", fld)
    keys <- tryCatch({
      if (!requireNamespace("sf", quietly = TRUE)) stop("package 'sf' not available")
      path <- path.expand(file.path(dir_derived, src_row$source[1]))
      if (!file.exists(path)) stop(sprintf("no GeoPackage at %s", path))
      d <- sf::st_drop_geometry(sf::st_read(path, quiet = TRUE))
      if (!fld %in% names(d)) stop(sprintf("no column '%s' in %s", fld, path))
      sort(unique(as.character(d[[fld]])))
    }, error = function(e) {
      message(sprintf("app_bundle_geom_keys(): %s (%s) -- %s", zsk, type, conditionMessage(e)))
      NULL
    })
    if (!is.null(keys)) out[[type]] <- keys
  }
  out
}

# ---- the one place the atlas-1 subplan's per-object budgets live ------------

#' The subplan's per-object size budgets — ONE table, ONE place
#'
#' Corrected 2026-09-21 (review round 1): the notebook previously carried a
#' private, undocumented 9 MB threshold for `zone_taxon.parquet` and 1.5 MB
#' for `taxon.parquet`, which meant the table it printed could never flag
#' either object as over budget. These are the subplan's own numbers,
#' `zone_taxon.parquet` corrected to 8 MB against the two real measurements
#' (7,074,125 B on v9, 6,140,626 B on v7 — both under 8 MB, over the
#' subplan's earlier ~6 MB estimate).
#'
#' @return data frame: `object`, `kind` (`"gzip"` or `"raw"` — which bytes the
#'   budget applies to), `budget_bytes`
app_bundle_budgets <- function() {
  data.frame(
    object = c("boot.json", "taxa.json", "taxon/*.json", "alias/*.json",
              "cell/tile=*/data_0.parquet", "taxon.parquet",
              "taxonomy.parquet", "zone_taxon.parquet"),
    kind = c("gzip", "gzip", "gzip", "gzip", "raw", "raw", "raw", "raw"),
    budget_bytes = as.integer(c(60, 1024, 25, 15, 250, 1024, 1024, 8192) * 1024),
    stringsAsFactors = FALSE)
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
#' **A legacy-generation quirk, normalized here — TEMPORARY, remove once
#' msens's `app_taxon_table()` fix lands (tracked in the atlas-1 review; do
#' NOT delete `.strip_trailing_dot0()` until the orchestrator confirms that
#' fix is in and this notebook has been reinstalled against it).**
#' `msens::app_taxon_table()`'s `taxon_id` is `CAST(t.taxon_id AS VARCHAR)` —
#' fine when the underlying column is INTEGER, but v1-v7's `taxon.taxon_id`
#' is DOUBLE, and DuckDB's `CAST(DOUBLE AS VARCHAR)` renders a whole number as
#' `"125371.0"`, not `"125371"` (confirmed 2026-09-21 against the real v7
#' release: 0 of 15,677 worms taxa matched the curated CSV before this fix,
#' because the CSV's `species_id` has no decimal). `.strip_trailing_dot0()`
#' normalizes BOTH sides before the join, so a v8/v9 integer id and a v1-v7
#' double-cast one compare equal.
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

  # TEMPORARY -- see the roxygen note above. Remove this helper and its two
  # call sites below once msens's app_taxon_table() stops emitting "N.0".
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
