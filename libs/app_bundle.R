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

# review round 2 (2026-09-21), gap 2: `!(s %in% c(".",".."))` named only the
# TWO shortest all-dot strings, so a THIRD (or longer) one -- "..." -- passed
# the charset check and was allowed. The real rule is general: a segment
# meaning "traverse" is one with NO character other than ".", of any length.

# is `s` a single valid PATH SEGMENT: non-empty, only [A-Za-z0-9._=-], and
# containing at least one character that is NOT "." (a segment of only dots,
# any length, is a directory-traversal token, not a filename)
.app_bundle_valid_segment <- function(s)
  nzchar(s) && grepl("^[A-Za-z0-9._=-]+$", s, perl = TRUE) && grepl("[^.]", s, perl = TRUE)

# review round 2, gap 1: is `ver` shaped like a real atlas version label
# (msens::atlas_resolve_ver()'s own shape: "v" + digits + an optional single
# lowercase letter -- v7, v9, v7b, v10, ...)? `ver` is interpolated into the
# key prefix UNVALIDATED would let `ver = "v9/../v7"` build the prefix
# "v9/../v7/app", which every real key would then legitimately start with.
# Checking the shape FIRST is the escaping: this pattern cannot contain "/",
# ".", or any other character the whitelist below treats specially, so a
# `ver` that passes it cannot smuggle anything.
app_bundle_valid_ver <- function(ver)
  is.character(ver) && length(ver) == 1L && !is.na(ver) && grepl("^v[0-9]+[a-z]?$", ver, perl = TRUE)

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
#' @param ver the release this run is publishing (e.g. `"v9"`) — validated
#'   against `^v[0-9]+[a-z]?$` BEFORE it is interpolated into anything; a
#'   `ver` that is not this shape is refused outright, never regex-escaped
#'   and used anyway (review round 2, gap 1: `ver = "v9/../v7"` used to build
#'   a prefix a real key could legitimately start with)
#' @param allow_cell_model allow keys under `{ver}/serve/cell_model/` too
#'   (default `FALSE`). Round 3 (item 3): generalized from a literal, hardcoded
#'   `"v7/serve/cell_model"` exception to `{ver}/serve/cell_model` — the SAME
#'   `ver` this call is asserting, already validated above, never a second,
#'   independently-trusted version string. A key for any OTHER version's
#'   `serve/cell_model/` is still refused, because it fails the prefix match,
#'   not because the version was hardcoded.
#' @return `keys`, invisibly, when every key is allowed
app_bundle_assert_prefix <- function(keys, ver, allow_cell_model = FALSE) {
  if (!app_bundle_valid_ver(ver))
    stop(sprintf(
      "app_bundle_assert_prefix(): `ver` does not look like a version label (^v[0-9]+[a-z]?$) -- got %s",
      if (is.character(ver) && length(ver) == 1L && !is.na(ver)) shQuote(ver) else "a non-scalar-character value"),
      call. = FALSE)
  if (!is.character(keys))
    stop("app_bundle_assert_prefix(): `keys` must be a character vector", call. = FALSE)
  if (length(keys) == 0L)
    stop("app_bundle_assert_prefix(): `keys` must not be empty (length(keys) == 0) -- ",
         "an empty vector has no bad key for this guard to catch, which is not the same ",
         "as having verified there isn't one", call. = FALSE)

  ok_app <- vapply(keys, .app_bundle_key_under_prefix, logical(1),
                   prefix = sprintf("%s/app", ver))
  ok_cm  <- if (isTRUE(allow_cell_model))
    vapply(keys, .app_bundle_key_under_prefix, logical(1), prefix = sprintf("%s/serve/cell_model", ver))
  else rep(FALSE, length(keys))

  bad <- keys[!(ok_app | ok_cm)]
  if (length(bad))
    stop(sprintf(
      "refusing to publish %d key(s) outside {%s/app/}%s:\n  %s%s",
      length(bad), ver, if (isTRUE(allow_cell_model)) sprintf(" or %s/serve/cell_model/", ver) else "",
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
  testthat::test_that("ALLOWED: the cell_model exception, per-version, when enabled", {
    testthat::expect_silent(app_bundle_assert_prefix(
      "v7/serve/cell_model/tile=0/data_0.parquet", "v7", allow_cell_model = TRUE))
    # round 3 (item 3): generalized -- any registered version can use its OWN
    # cell_model prefix when it asks for the exception, not just v7
    testthat::expect_silent(app_bundle_assert_prefix(
      "v7b/serve/cell_model/tile=0/data_0.parquet", "v7b", allow_cell_model = TRUE))
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
  testthat::test_that("REFUSED (round 2, gap 1): `ver` itself smuggling traversal", {
    testthat::expect_error(
      app_bundle_assert_prefix("v9/../v7/app/x.json", ver = "v9/../v7"),
      "does not look like a version label")
    testthat::expect_error(
      app_bundle_assert_prefix("v9.evil.com/app/x.json", ver = "v9.evil.com"),
      "does not look like a version label")
    testthat::expect_error(app_bundle_assert_prefix("v9/app/x.json", ver = ""),
                           "does not look like a version label")
    testthat::expect_error(app_bundle_assert_prefix("v9/app/x.json", ver = c("v9", "v7")),
                           "does not look like a version label")
    # legitimate version shapes must still work, including the letter suffix
    testthat::expect_silent(app_bundle_assert_prefix("v7b/app/boot.json", "v7b"))
  })
  testthat::test_that("REFUSED (round 2, gap 2): a segment of only dots, any length", {
    testthat::expect_error(app_bundle_assert_prefix("v9/app/.../x.json", "v9"), "refusing to publish")
    testthat::expect_error(app_bundle_assert_prefix("v9/app/..../x.json", "v9"), "refusing to publish")
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
  testthat::test_that("REFUSED: the cell_model exception is OFF by default", {
    testthat::expect_error(
      app_bundle_assert_prefix("v7/serve/cell_model/tile=0/data_0.parquet", "v7"),
      "refusing to publish")
  })
  testthat::test_that("REFUSED: the exception is scoped to THIS call's own `ver`, never another version's", {
    # v7's exception must not admit a v9 key, and vice versa -- the prefix is
    # built from `ver`, which is already validated, but a key for a DIFFERENT
    # version smuggled into the same batch must still be refused
    testthat::expect_error(
      app_bundle_assert_prefix("v9/serve/cell_model/tile=0/data_0.parquet", "v7",
                               allow_cell_model = TRUE),
      "refusing to publish")
    testthat::expect_error(
      app_bundle_assert_prefix(c("v7/serve/cell_model/tile=0/data_0.parquet",
                                 "v9/serve/cell_model/tile=0/data_0.parquet"),
                               "v7", allow_cell_model = TRUE),
      "1 key\\(s\\)")
  })
  testthat::test_that("REFUSED: the cell_model exception is itself whitelisted, not a bare prefix", {
    testthat::expect_error(
      app_bundle_assert_prefix("v7/serve/cell_model/../../app/x", "v7",
                               allow_cell_model = TRUE),
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

# ---- anonymous COG spot-check (species assets, read the way the app does) ---

#' A reproducible sample of merged-COG URLs to spot-check anonymously
#'
#' ALL `sp_cat == "turtle"` taxa (a handful, always checked) plus up to
#' `n_random` other taxa with a merged COG, sampled from the WRITTEN
#' `taxon/*.json` shards — the exact bytes an app would fetch, not a query
#' against the database.
#'
#' @param dir_out the built bundle's directory (has a `taxon/` subdir)
#' @param n_random how many non-turtle taxa to sample (default 300)
#' @param seed RNG seed, for a reproducible sample across runs
#' @return data frame `key, url` — zero rows if there is no `taxon/` directory
#'   or no taxon has a merged COG (a release this notebook should SKIP the
#'   check for, not fail)
app_bundle_cog_sample <- function(dir_out, n_random = 300, seed = 2026) {
  taxon_dir <- file.path(dir_out, "taxon")
  none <- data.frame(key = character(), url = character(), stringsAsFactors = FALSE)
  if (!dir.exists(taxon_dir)) return(none)
  turtles <- list(); others <- list()
  for (f in list.files(taxon_dir, full.names = TRUE)) {
    d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    for (k in names(d$taxa)) {
      taxon <- d$taxa[[k]]
      m <- taxon$merged
      url <- if (is.null(m$url)) "" else m$url
      if (is.null(m) || !identical(m$type, "cog") || !nzchar(url)) next
      if (identical(taxon$sp_cat, "turtle")) turtles[[k]] <- m$url else others[[k]] <- m$url
    }
  }
  if (!length(turtles) && !length(others)) return(none)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) .Random.seed else NULL
  set.seed(seed)
  keys_o <- names(others)
  samp   <- if (length(keys_o) > n_random) sample(keys_o, n_random) else keys_o
  if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  data.frame(key = c(names(turtles), samp),
            url = c(unlist(turtles, use.names = FALSE), unlist(others[samp], use.names = FALSE)),
            stringsAsFactors = FALSE)
}

#' Anonymous HEAD each URL, in parallel, order preserved by construction
#'
#' Shells out to `curl`/`xargs` for speed (a few hundred URLs sequentially
#' through `httr2` is slow); each output line carries BOTH its URL and its
#' status code so `xargs -P`'s unordered completion can never misalign a
#' status with the wrong URL.
#'
#' @param urls character vector
#' @param timeout_s per-request timeout (seconds)
#' @param parallel concurrent requests
#' @return data frame `url, status` (integer; `NA` on a request that errored
#'   or timed out), same length and order as `urls`
app_bundle_head_check <- function(urls, timeout_s = 10, parallel = 12) {
  if (!length(urls)) return(data.frame(url = character(), status = integer()))
  # a helper SCRIPT FILE, not an inline `sh -c` string with {} substituted
  # twice -- xargs's own {}-in-a-quoted-sh-c-string composition was measured to
  # fail outright on macOS ("xargs: command line cannot be assembled, too
  # long"), so each URL is passed as $1 to a tiny standalone script instead;
  # {} is substituted exactly once, as xargs's own argument, never re-parsed
  # by a nested shell.
  helper <- tempfile(fileext = ".sh")
  urls_f <- tempfile()
  on.exit(unlink(c(helper, urls_f)), add = TRUE)
  writeLines(c(
    "#!/bin/sh",
    "url=\"$1\"",
    # curl's --max-time takes a real (fractional seconds ARE honoured, e.g.
    # 0.001) -- truncating to as.integer() here used to silently turn any
    # sub-second seeded-fault timeout into `--max-time 0`, which curl treats
    # as "no limit at all", defeating the exact test it was meant to run.
    sprintf("st=$(curl -s -o /dev/null -w '%%{http_code}' --max-time %s -I \"$url\")",
            format(as.numeric(timeout_s), scientific = FALSE)),
    "printf '%s\\t%s\\n' \"$url\" \"$st\""), helper)
  Sys.chmod(helper, "0755")
  writeLines(urls, urls_f)
  out <- system2("xargs", c("-P", as.integer(parallel), "-I{}", helper, "{}"),
                stdin = urls_f, stdout = TRUE, stderr = FALSE)
  parts <- strsplit(out, "\t", fixed = TRUE)
  got_url <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, "")
  got_st  <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, "")
  m <- match(urls, got_url)
  data.frame(url = urls, status = suppressWarnings(as.integer(got_st[m])), stringsAsFactors = FALSE)
}

#' Anonymous HEAD each URL, RETRYING only the "no answer" ones
#'
#' `curl -w %{http_code}` reports `000` when nothing came back at all (DNS
#' failure, connection refused, or `--max-time` expired) — that is a
#' statement about THIS MACHINE'S network right now, not about the object.
#' Found for real 2026-09-21: this gate reported "an object DOES NOT exist"
#' while four concurrent Playwright suites (load ~40) were saturating the
#' laptop's network stack, and 15/15 of the SAME URLs answered 200 seconds
#' later. A `000` (or any missing/unparseable status) is retried up to
#' `max_retries` times, at a LOWER parallelism than the first pass (contention
#' is exactly what caused it) and with backoff; only a REAL, non-200 HTTP
#' status that survives every retry is treated as "the object is missing" and
#' fails the render for that reason. A URL that still has no answer after
#' every retry is NOT the same as "verified missing" -- see
#' [app_bundle_head_classify()], which keeps the two apart.
#'
#' @param urls character vector
#' @param timeout_s per-request timeout (seconds)
#' @param parallel concurrent requests on the FIRST pass
#' @param max_retries retries for a `000`/unparseable result (default 3)
#' @param retry_parallel concurrent requests on a retry pass (default 4, lower
#'   than `parallel` on purpose)
#' @param backoff_s sleep before each retry pass, in order (default
#'   `c(2, 4, 8)`, recycled if `max_retries` exceeds its length)
#' @return data frame `url, status` (integer; `0L` if every attempt, including
#'   retries, came back with no answer), same length and order as `urls`
app_bundle_head_check_retry <- function(urls, timeout_s = 10, parallel = 12,
                                        max_retries = 3, retry_parallel = 4,
                                        backoff_s = c(2, 4, 8)) {
  if (!length(urls)) return(data.frame(url = character(), status = integer()))
  cur <- app_bundle_head_check(urls, timeout_s = timeout_s, parallel = parallel)
  attempt <- 0L
  while (attempt < max_retries && any(is.na(cur$status) | cur$status == 0L)) {
    attempt <- attempt + 1L
    Sys.sleep(backoff_s[((attempt - 1L) %% length(backoff_s)) + 1L])
    no_answer <- is.na(cur$status) | cur$status == 0L
    retry_urls <- unique(cur$url[no_answer])
    retried <- app_bundle_head_check(retry_urls, timeout_s = timeout_s, parallel = retry_parallel)
    m <- match(cur$url, retried$url)
    fixed <- no_answer & !is.na(m)
    cur$status[fixed] <- retried$status[m[fixed]]
  }
  cur
}

#' Classify a batch of [app_bundle_head_check_retry()] results
#'
#' Splits `status` into the categories that decide the gate's verdict: a real
#' HTTP code that is 200, a real HTTP code that is NOT 200 (the actual
#' defect -- the object answered and it is wrong), and "no answer" (every
#' attempt, including retries, failed -- NOTHING was verified for that URL,
#' which must never be silently read as "it's fine").
#'
#' @param chk result of [app_bundle_head_check_retry()] (or
#'   [app_bundle_head_check()])
#' @return list with `n_200`, `n_other` (real non-200), `n_no_answer`,
#'   `ok` (`TRUE` only if `n_other == 0 && n_no_answer == 0`), and
#'   `worst_status`/`worst_url` (a real non-200 first, else `NA`)
app_bundle_head_classify <- function(chk) {
  is_no_answer <- is.na(chk$status) | chk$status == 0L
  is_other     <- !is_no_answer & chk$status != 200L
  bad <- chk[is_other, , drop = FALSE]
  list(n_200 = sum(chk$status == 200L, na.rm = TRUE),
       n_other = sum(is_other), n_no_answer = sum(is_no_answer),
       ok = sum(is_other) == 0L && sum(is_no_answer) == 0L,
       worst_status = if (nrow(bad)) bad$status[1] else NA_integer_,
       worst_url = if (nrow(bad)) bad$url[1] else NA_character_)
}

#' Self-test for [app_bundle_head_check_retry()]/[app_bundle_head_classify()]
#' — seeded fault: EVERY request times out
#'
#' `timeout_s = 0.001` makes every real HTTPS request time out regardless of
#' network conditions (a real network round-trip cannot complete in 1 ms) --
#' the retries then also all fail the same way, by construction, without
#' depending on this machine's actual load at test time. Confirms the classify
#' step reports "no answer", never "missing", and that the overall verdict is
#' still NOT ok (nothing was verified, so it cannot pass) -- both the wrong
#' claim this replaces and the right one it makes are shown.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_head_check_retry_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  good <- paste0(msens::atlas_base_url(), "/latest.txt")

  testthat::test_that("RED: an all-timeout batch is classified 'no answer', never 'missing', and overall NOT ok", {
    chk <- app_bundle_head_check_retry(c(good, good), timeout_s = 0.001,
                                       max_retries = 1, backoff_s = 0)
    testthat::expect_true(all(chk$status == 0L))
    cl <- app_bundle_head_classify(chk)
    testthat::expect_equal(cl$n_other, 0L)          # NOT reported as "object missing"
    testthat::expect_equal(cl$n_no_answer, 2L)      # correctly reported as "no answer"
    testthat::expect_false(cl$ok)                   # verified nothing -> cannot pass
  })

  testthat::test_that("GREEN: a normal-timeout check of a real 200 URL is ok", {
    chk <- app_bundle_head_check_retry(good, timeout_s = 10, max_retries = 1)
    cl <- app_bundle_head_classify(chk)
    testthat::expect_true(cl$ok)
    testthat::expect_equal(cl$n_200, 1L)
  })

  testthat::test_that("a 000 on the first pass that a retry resolves to 200 is NOT a defect", {
    # simulate: first pass returns 0 (as if it timed out), the "retry" (still
    # timeout_s=10 here) actually reaches the real object -- since we cannot
    # force ONE specific pass to fail without controlling the network, this
    # exercises the retry PATH structurally: max_retries=2 with a real URL
    # must converge to 200 even if an early pass is slow/unlucky.
    chk <- app_bundle_head_check_retry(good, timeout_s = 10, max_retries = 2, backoff_s = c(1, 1))
    testthat::expect_equal(chk$status, 200L)
  })
  invisible(TRUE)
}

#' Self-test for [app_bundle_head_check()] — real network, tiny sample
#'
#' No mocking: one known-anonymous-readable URL (this bucket's `latest.txt`,
#' small and stable) and one URL that cannot resolve, so the round-trip is
#' proven against the real thing rather than a fake server.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_head_check_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  good <- paste0(msens::atlas_base_url(), "/latest.txt")
  bad  <- "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/does-not-exist-atlas1.txt"
  testthat::test_that("order is preserved and status is correct for a real 200 and a real non-200", {
    r <- app_bundle_head_check(c(bad, good, good, bad))
    testthat::expect_identical(r$url, c(bad, good, good, bad))
    testthat::expect_identical(r$status[c(2, 3)], c(200L, 200L))
    testthat::expect_true(all(r$status[c(1, 4)] != 200L))
  })
  testthat::test_that("empty input returns zero rows, not an error", {
    testthat::expect_equal(nrow(app_bundle_head_check(character(0))), 0L)
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
#' **Round 3 (2026-09-21): a missing/unreadable GeoPackage is now a HARD
#' FAILURE, not a message-and-continue.** The graceful-degradation design in
#' rounds 1-2 was right when the files genuinely did not exist anywhere on
#' this laptop; now that all five of `data/zone_sets.csv`'s registered
#' sources ARE present (copied from the server), a release publishing with
#' `geom_keys` silently missing a zone type is no longer "the best this
#' machine can do" — it is the exact bug D5 exists to prevent (v9's boot
#' listed 5 subregion keys where only 4 -- AK/AT/GA/PA -- are drawable,
#' because "no geometry to check against" silently meant "don't check").
#' Every zone type the release's manifest names now MUST resolve, or this
#' function stops, naming the missing/unreadable path.
#'
#' @param zones the release's zones table (`manifest_build()`'s `$zones`:
#'   `fld`, `zone_set_key`, ...)
#' @param zone_sets the zone-set registry (`data/zone_sets.csv`)
#' @param dir_derived base directory each registry `source` path is relative to
#' @return named list `zone_type -> character keys`, one entry per row of
#'   `zones` that carries a `zone_set_key` (`length(result) ==
#'   sum(!is.na(zones$zone_set_key))` is asserted before returning); errors
#'   otherwise, naming the exact path that could not be read
#' The release's PUBLISHED `{ver}/manifest.json`, fetched anonymously and cached
#'
#' Round 10 (2026-09-22): `app_bundle_build()` must be handed the manifest
#' the APP will actually read, not a freshly re-run `manifest_build()`.
#' `manifest_build()`'s one-row-per-`zone_set_key` collapse breaks a genuine
#' tie (v2's `subregion_key`: two zone tables, `n = 4` each) by DuckDB's own
#' row order, which was found to differ between an installed-package build
#' and a source-tree run of the identical commit -- non-deterministic in a
#' way a rebuild cannot fix. The PUBLISHED manifest is a fixed artifact
#' (already resolved, one row per `fld`); reading it is deterministic where
#' rebuilding it was not.
#'
#' @param ver version label
#' @param base atlas base URL ([msens::atlas_base_url()])
#' @param cache_dir directory to cache the download under (created if needed);
#'   a file already there is reused, never re-downloaded
#' @return the manifest as a list (`$zones` is a data frame, one row per
#'   `fld`, via `jsonlite::fromJSON(simplifyVector = TRUE)`), or `NULL` if it
#'   could not be fetched or parsed
app_bundle_fetch_published_manifest <- function(ver, base, cache_dir) {
  fs::dir_create(cache_dir)
  dest <- file.path(cache_dir, "manifest.json")
  if (!file.exists(dest)) {
    ok <- isTRUE(tryCatch(
      utils::download.file(sprintf("%s/%s/manifest.json", base, ver), dest,
                           mode = "wb", quiet = TRUE) == 0L,
      error = function(e) FALSE, warning = function(w) FALSE))
    if (!ok) { unlink(dest); return(NULL) }
  }
  tryCatch(jsonlite::fromJSON(dest, simplifyVector = TRUE), error = function(e) NULL)
}

app_bundle_geom_keys <- function(zones, zone_sets, dir_derived) {
  out <- list()
  if (is.null(zones) || !nrow(zones)) return(out)
  stopifnot("zones needs a zone_set_key column (pass zone_sets to manifest_build())" =
              "zone_set_key" %in% names(zones))
  want <- zones[!is.na(zones$zone_set_key) & nzchar(zones$zone_set_key), , drop = FALSE]
  for (i in seq_len(nrow(want))) {
    fld <- want$fld[i]
    zsk <- want$zone_set_key[i]
    type <- sub("_key$", "", fld)
    src_row <- zone_sets[zone_sets$zone_set_key == zsk, , drop = FALSE]
    if (!nrow(src_row))
      stop(sprintf(
        "app_bundle_geom_keys(): zone_set_key '%s' (%s) has no row in the zone-set registry (data/zone_sets.csv)",
        zsk, type), call. = FALSE)
    path <- path.expand(file.path(dir_derived, src_row$source[1]))
    if (!requireNamespace("sf", quietly = TRUE))
      stop("app_bundle_geom_keys(): package 'sf' is required and is not available", call. = FALSE)
    if (!file.exists(path))
      stop(sprintf(
        "app_bundle_geom_keys(): missing GeoPackage for zone_set_key '%s' (%s): %s does not exist -- ",
        zsk, type, path),
        "a release cannot publish units for a spatial unit whose drawable keys were never checked",
        call. = FALSE)
    d <- tryCatch(sf::st_drop_geometry(sf::st_read(path, quiet = TRUE)),
                 error = function(e) stop(sprintf(
                   "app_bundle_geom_keys(): could not read %s (zone_set_key '%s', %s): %s",
                   path, zsk, type, conditionMessage(e)), call. = FALSE))
    if (!fld %in% names(d))
      stop(sprintf("app_bundle_geom_keys(): %s has no column '%s' (zone_set_key '%s', %s)",
                   path, fld, zsk, type), call. = FALSE)
    out[[type]] <- sort(unique(as.character(d[[fld]])))
  }
  stopifnot(
    "app_bundle_geom_keys(): resolved fewer zone types than the release's manifest names" =
      length(out) == nrow(want))
  out
}
#' Self-test for [app_bundle_geom_keys()]
#'
#' Uses SCRATCH `zone_sets`/`zones` frames only — never touches the real
#' registry or the real GeoPackages, so this is safe to run unattended. The
#' "rename a GeoPackage away" seeded fault is exercised the same way: point a
#' scratch registry row at a path that does not exist, which is exactly what
#' a renamed-away file looks like to this function.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_geom_keys_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  scratch_gpkg <- function(keys, fld) {
    stopifnot(requireNamespace("sf", quietly = TRUE))
    p <- tempfile(fileext = ".gpkg")
    d <- sf::st_sf(x = keys, geometry = sf::st_sfc(lapply(keys, function(k) sf::st_point(c(0, 0))), crs = 4326))
    names(d)[1] <- fld
    sf::st_write(d, p, quiet = TRUE)
    p
  }
  testthat::test_that("resolves real keys from a real (scratch) GeoPackage", {
    p <- scratch_gpkg(c("AA", "BB", "CC"), "ecoregion_key")
    zone_sets <- data.frame(zone_set_key = "eco_test", source = basename(p), stringsAsFactors = FALSE)
    zones <- data.frame(fld = "ecoregion_key", zone_set_key = "eco_test", stringsAsFactors = FALSE)
    gk <- app_bundle_geom_keys(zones, zone_sets, dirname(p))
    testthat::expect_identical(gk, list(ecoregion = c("AA", "BB", "CC")))
    unlink(p)
  })
  testthat::test_that("REFUSED (round 3, seeded fault): GeoPackage path does not exist", {
    zone_sets <- data.frame(zone_set_key = "eco_test", source = "does_not_exist.gpkg", stringsAsFactors = FALSE)
    zones <- data.frame(fld = "ecoregion_key", zone_set_key = "eco_test", stringsAsFactors = FALSE)
    testthat::expect_error(
      app_bundle_geom_keys(zones, zone_sets, tempdir()),
      "missing GeoPackage")
  })
  testthat::test_that("REFUSED: zone_set_key absent from the registry", {
    zone_sets <- data.frame(zone_set_key = "other", source = "x.gpkg", stringsAsFactors = FALSE)
    zones <- data.frame(fld = "ecoregion_key", zone_set_key = "eco_test", stringsAsFactors = FALSE)
    testthat::expect_error(
      app_bundle_geom_keys(zones, zone_sets, tempdir()),
      "no row in the zone-set registry")
  })
  testthat::test_that("REFUSED: GeoPackage exists but lacks the fld column", {
    p <- scratch_gpkg(c("AA", "BB"), "wrong_key")
    zone_sets <- data.frame(zone_set_key = "eco_test", source = basename(p), stringsAsFactors = FALSE)
    zones <- data.frame(fld = "ecoregion_key", zone_set_key = "eco_test", stringsAsFactors = FALSE)
    testthat::expect_error(
      app_bundle_geom_keys(zones, zone_sets, dirname(p)),
      "no column")
    unlink(p)
  })
  testthat::test_that("zones with no zone_set_key at all resolve to an empty (not erroring) result", {
    zones <- data.frame(fld = "ecoregion_key", zone_set_key = NA_character_, stringsAsFactors = FALSE)
    gk <- app_bundle_geom_keys(zones, data.frame(zone_set_key = character(), source = character()), tempdir())
    testthat::expect_length(gk, 0)
  })
  invisible(TRUE)
}

# ---- per-unit geometry-consistency assertions (run AFTER app_bundle_build()) -

#' Assert every drawable unit's keys are a subset of >= 2 of its real geometry
#'
#' The point of [app_bundle_geom_keys()] existing at all: this is the
#' assertion that would have caught the original bug (v9's subregion unit
#' listing 5 keys — a whole-study-area rollup included — when only 4 keys
#' (`AK`, `AT`, `GA`, `PA`) are drawable, per its own GeoPackage).
#'
#' @param units `boot$units` (from a built bundle)
#' @param geom_keys the SAME `geom_keys` passed to `app_bundle_build()`
#' @return a data frame, one row per unit: `zone_type`, `n_keys`,
#'   `n_keys_in_geometry`, `keys_ge_2`, `all_keys_in_geometry`
app_bundle_assert_units_match_geometry <- function(units, geom_keys) {
  rows <- lapply(units, function(u) {
    gk <- geom_keys[[u$zone_type]]
    ks <- as.character(u$keys)
    in_geom <- if (is.null(gk)) rep(NA, length(ks)) else ks %in% gk
    data.frame(zone_type = u$zone_type, n_keys = length(ks),
              n_keys_in_geometry = sum(in_geom, na.rm = TRUE),
              keys_ge_2 = length(ks) >= 2,
              all_keys_in_geometry = if (is.null(gk)) NA else all(in_geom))
  })
  out <- do.call(rbind, rows)
  stopifnot(
    "a drawable unit has fewer than 2 keys" = nrow(out) == 0 || all(out$keys_ge_2),
    "a drawable unit advertises a key absent from its own geometry" =
      nrow(out) == 0 || all(out$all_keys_in_geometry, na.rm = TRUE))
  out
}

# ---- gate coverage: which gates ran per version, and why one didn't (round 3 item 2) --
#
# Round 3 item 2: a release that cannot supply a capability (no cell table, no
# zone_metric table, no merged COG, ...) must not FAIL its gates -- it must
# SKIP them, with a reason, and the notebook's summary must say so explicitly
# rather than the reader having to notice an empty table. Gates call
# [app_bundle_gate_note()] themselves (right beside the same condition that
# decides whether they run), so the coverage table can never drift from what
# the gate actually did.

.app_bundle_gate_coverage_env <- new.env(parent = emptyenv())
.app_bundle_gate_coverage_env$rows <- list()

#' Record whether a capability-conditioned gate ran for one version
#'
#' Call once per gate per version, right beside the `if` that decides it.
#'
#' @param gate short gate name, e.g. `"tile_width"`
#' @param ver version label
#' @param ran did the gate actually run its checks (not just get called)?
#' @param reason why not, when `ran` is `FALSE` (ignored, and recorded as `""`, when `ran` is `TRUE`)
#' @return `NULL`, invisibly
app_bundle_gate_note <- function(gate, ver, ran, reason = "") {
  .app_bundle_gate_coverage_env$rows[[length(.app_bundle_gate_coverage_env$rows) + 1L]] <-
    data.frame(gate = gate, ver = ver, ran = isTRUE(ran),
              reason = if (isTRUE(ran)) "" else reason, stringsAsFactors = FALSE)
  invisible(NULL)
}

#' The gate-coverage table recorded so far this render
#'
#' @param reset clear the recorded rows after reading them (default `TRUE`, so
#'   a second render in the same R session starts clean)
#' @return data frame `gate, ver, ran, reason`
app_bundle_gate_coverage <- function(reset = TRUE) {
  out <- if (length(.app_bundle_gate_coverage_env$rows))
    do.call(rbind, .app_bundle_gate_coverage_env$rows) else
    data.frame(gate = character(), ver = character(), ran = logical(),
              reason = character(), stringsAsFactors = FALSE)
  if (reset) .app_bundle_gate_coverage_env$rows <- list()
  out
}

# ---- serve/cell_model/ upload: local source, exact keys, guards, dry-run (round 3 item 3) --
#
# Generalized from a v7-only flag (`APP_BUNDLE_V7_CELLMODEL`) to an explicit
# ALLOW-LIST parameter of version labels (default empty -- nothing is planned
# unless asked for). Today's only two candidates are v7 and v7b: both
# advertise a per-cell species list but have no tiles on S3 yet. v8/v9 are
# never at risk even if mis-listed: they have no local `{ver}/cell_model/`
# directory at all (their asset registry lives in the database, not a
# filesystem export), so [app_bundle_cell_model_dir()] returns `NA` for them
# and the plan below refuses with a "no local source directory" reason before
# any key is even constructed.

#' The local `cell_model/` source directory for a version, or `NA`
#'
#' @param ver version label
#' @param dir_derived the machine's derived-data root (`~/_big/msens/derived`
#'   on the laptop, `/share/data/derived` on the server)
#' @return the directory path if it exists, else `NA_character_`
app_bundle_cell_model_dir <- function(ver, dir_derived) {
  d <- file.path(path.expand(dir_derived), ver, "cell_model")
  if (dir.exists(d)) d else NA_character_
}

#' The exact keys + byte total `serve/cell_model/` would upload for one version
#'
#' A pure directory walk -- never writes, never touches S3.
#'
#' @return `list(ver, dir, keys, bytes, n_tiles)`; `keys` is `character(0)`
#'   when there is no local source directory
app_bundle_cell_model_plan <- function(ver, dir_derived) {
  d <- app_bundle_cell_model_dir(ver, dir_derived)
  if (is.na(d))
    return(list(ver = ver, dir = NA_character_, keys = character(0), bytes = 0, n_tiles = 0L))
  files <- list.files(d, recursive = TRUE, full.names = TRUE)
  rels  <- fs::path_rel(files, d)
  keys  <- as.character(glue::glue("{ver}/serve/cell_model/{rels}"))
  list(ver = ver, dir = d, keys = keys, bytes = sum(file.size(files)),
       n_tiles = length(list.dirs(d, full.names = TRUE, recursive = FALSE)))
}

#' Assert every planned `serve/cell_model/` key matches the EXACT tile shape
#'
#' `^{ver}/serve/cell_model/tile=[0-9]+/data_0\.parquet$` -- exactly, nothing
#' else (no second file per tile, no other filename, no nesting, no other
#' version's prefix smuggled into `ver`). Stops naming the count and the first
#' offending key.
#'
#' @param keys character vector of proposed S3 keys
#' @param ver version label (checked for shape FIRST, before any key is
#'   matched against it -- same discipline as [app_bundle_assert_prefix()])
#' @return `TRUE`, invisibly, if every key matches
app_bundle_assert_cell_model_keys <- function(keys, ver) {
  stopifnot("ver must match ^v[0-9]+[a-z]?$" = app_bundle_valid_ver(ver))
  pat <- sprintf("^%s/serve/cell_model/tile=[0-9]+/data_0\\.parquet$", ver)
  bad <- keys[!grepl(pat, keys)]
  if (length(bad))
    stop(sprintf(
      "app_bundle_assert_cell_model_keys(): %d of %d key(s) do not match %s -- first: %s",
      length(bad), length(keys), pat, bad[1]), call. = FALSE)
  invisible(TRUE)
}

#' Refuse a version whose `serve/cell_model/` already has objects on S3
#'
#' The upload is all-or-nothing per version, never a partial refresh: if the
#' FIRST tile (`tile=0/data_0.parquet`) already answers 200 anonymously, the
#' whole version is refused rather than risk a silent partial overwrite.
#'
#' @param ver version label
#' @param base atlas base URL ([msens::atlas_base_url()])
#' @return `TRUE` if already present (should be refused), `FALSE` if clear
app_bundle_cell_model_already_on_s3 <- function(ver, base) {
  url <- as.character(glue::glue("{base}/{ver}/serve/cell_model/tile=0/data_0.parquet"))
  st  <- app_bundle_head_check(url)$status
  isTRUE(length(st) == 1 && !is.na(st) && st == 200L)
}

#' Self-test for the `serve/cell_model/` key-shape guard (offline, no network)
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_cell_model_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  testthat::test_that("exactly-shaped keys are accepted", {
    good <- c("v7/serve/cell_model/tile=0/data_0.parquet",
             "v7/serve/cell_model/tile=427/data_0.parquet")
    testthat::expect_true(app_bundle_assert_cell_model_keys(good, "v7"))
  })
  testthat::test_that("a second file in a tile directory is refused", {
    bad <- c("v7/serve/cell_model/tile=0/data_0.parquet",
            "v7/serve/cell_model/tile=0/data_1.parquet")
    testthat::expect_error(app_bundle_assert_cell_model_keys(bad, "v7"), "do not match")
  })
  testthat::test_that("a key stamped with a DIFFERENT version than `ver` is refused", {
    testthat::expect_error(
      app_bundle_assert_cell_model_keys("v9/serve/cell_model/tile=0/data_0.parquet", "v7"),
      "do not match")
  })
  testthat::test_that("nesting or an extra path segment is refused", {
    testthat::expect_error(
      app_bundle_assert_cell_model_keys("v7/serve/cell_model/tile=0/sub/data_0.parquet", "v7"),
      "do not match")
  })
  testthat::test_that("a malformed `ver` is refused before any key is even matched", {
    testthat::expect_error(app_bundle_assert_cell_model_keys(character(0), "v9/../v7"),
                           "must match")
  })
  testthat::test_that("a version with no local cell_model/ directory plans zero keys", {
    d <- tempfile("app_bundle_cm_")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    plan <- app_bundle_cell_model_plan("v9", d)
    testthat::expect_length(plan$keys, 0)
    testthat::expect_true(is.na(plan$dir))
  })
  invisible(TRUE)
}

# ---- D17: exactly one drawable unit per release, with the exact key count --
#
# Master plan D17 (Ben's decision, 2026-09-22) supersedes D16 in full:
# drawable units are Program Areas ONLY (v2-v9), Planning Areas on v1 -- no
# subregion or ecoregion unit on ANY release. There is no more "derive which
# zone types are drawable from score_% metrics" (D16's own fix, itself a
# correction of an earlier wrong per-release geometry cut): the answer is
# always exactly ONE unit, of a KNOWN type, with a KNOWN exact key count
# (v1 planarea = 36; v2-v9 programarea = 20) -- checked, not derived.

#' Does a release's rendered `boot$units` match the D17 expectation?
#'
#' @param zone_type_expected `"programarea"` or `"planarea"`
#' @param n_keys_expected the exact key count expected (36 for v1's
#'   `planarea`, 20 for `programarea`)
#' @param units the release's actual `boot$units`
#' @param n_units_expected normally `1` -- exposed as a parameter ONLY so
#'   [app_bundle_d17_check_selftest()] can demonstrate what "expect two"
#'   looks like; the notebook itself always calls this with the default.
#' @return list: `matches` (logical), `n_units_actual`, `n_keys_actual`,
#'   `reason` (one-line, never blank)
app_bundle_d17_check <- function(zone_type_expected, n_keys_expected, units, n_units_expected = 1L) {
  u <- Filter(function(x) identical(x$zone_type, zone_type_expected), units)
  n_units_actual <- length(units)
  n_keys_actual  <- if (length(u) == 1L) length(unique(unlist(u[[1]]$keys))) else NA_integer_
  matches <- n_units_actual == n_units_expected && length(u) == 1L &&
    isTRUE(n_keys_actual == n_keys_expected)
  reason <- if (matches)
    sprintf("exactly %d unit (%s), %d keys, as expected", n_units_expected, zone_type_expected, n_keys_actual)
  else
    sprintf("expected %d unit(s) of type '%s' with %d keys; got %d unit(s) total, %s keys",
           n_units_expected, zone_type_expected, n_keys_expected, n_units_actual,
           if (is.na(n_keys_actual)) "NA" else as.character(n_keys_actual))
  list(matches = matches, n_units_actual = n_units_actual, n_keys_actual = n_keys_actual, reason = reason)
}

#' Self-test for [app_bundle_d17_check()]
#'
#' Offline: fabricates a `boot$units`-shaped list directly. The seeded fault
#' IS the point of `n_units_expected` existing as a parameter at all: real
#' data always has exactly one unit, so "expect two" must always be red.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_d17_check_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  units_pa <- list(list(zone_type = "programarea", keys = as.list(sprintf("PA%02d", 1:20))))
  units_pl <- list(list(zone_type = "planarea",    keys = as.list(sprintf("PL%02d", 1:36))))

  testthat::test_that("GREEN: exactly one programarea unit, 20 keys, matches", {
    chk <- app_bundle_d17_check("programarea", 20L, units_pa)
    testthat::expect_true(chk$matches)
    testthat::expect_equal(chk$n_units_actual, 1L)
    testthat::expect_equal(chk$n_keys_actual, 20L)
  })
  testthat::test_that("GREEN: exactly one planarea unit, 36 keys, matches (v1)", {
    chk <- app_bundle_d17_check("planarea", 36L, units_pl)
    testthat::expect_true(chk$matches)
  })
  testthat::test_that("RED: wrong key count does not match", {
    chk <- app_bundle_d17_check("programarea", 19L, units_pa)
    testthat::expect_false(chk$matches)
  })
  testthat::test_that("RED: an extra unit (subregion/ecoregion reappearing) does not match", {
    chk <- app_bundle_d17_check("programarea", 20L, c(units_pa,
      list(list(zone_type = "subregion", keys = list("AK", "AT")))))
    testthat::expect_false(chk$matches)
    testthat::expect_equal(chk$n_units_actual, 2L)
  })
  testthat::test_that("SEEDED FAULT: expecting TWO units when there is only one is red", {
    chk <- app_bundle_d17_check("programarea", 20L, units_pa, n_units_expected = 2L)
    testthat::expect_false(chk$matches)
    testthat::expect_match(chk$reason, "expected 2 unit")
  })
  invisible(TRUE)
}

# ---- the one place the atlas-1 subplan's per-object budgets live ------------

#' The subplan's per-object size budgets — ONE table, ONE place
#'
#' Corrected 2026-09-21 (review round 1): the notebook previously carried a
#' private, undocumented 9 MB threshold for `zone_taxon.parquet` and 1.5 MB
#' for `taxon.parquet`, which meant the table it printed could never flag
#' either object as over budget.
#'
#' **`zone_taxon.parquet` no longer has a flat byte budget at all — see
#' [app_bundle_zone_taxon_check()].** Round 4 (2026-09-21) raised the flat cap
#' twice in a row chasing real releases (8 MB after v7/v9, then 12 MB after
#' v1) and the SAME motion broke again on v2 (12,244,691 B) before the render
#' that would have proven it wrong even finished. A flat byte cap on this
#' object was never the right shape of gate: size tracks ROW COUNT, not
#' vintage. It is excluded from this table; the calling chunk applies the
#' bytes-per-row rule to it separately.
#'
#' @return data frame: `object`, `kind` (`"gzip"` or `"raw"` — which bytes the
#'   budget applies to), `budget_bytes`
app_bundle_budgets <- function() {
  data.frame(
    object = c("boot.json", "taxa.json", "taxon/*.json", "alias/*.json",
              "cell/tile=*/data_0.parquet", "taxon.parquet",
              "taxonomy.parquet"),
    kind = c("gzip", "gzip", "gzip", "gzip", "raw", "raw", "raw"),
    budget_bytes = as.integer(c(60, 1024, 25, 15, 250, 1024, 1024) * 1024),
    stringsAsFactors = FALSE)
}

# ---- zone_taxon.parquet: bytes-PER-ROW, not a flat cap -----------------------

#' `zone_taxon.parquet`'s size gate: bytes per row, not a flat byte cap
#'
#' Measured across the FULL 11-release registry (2026-09-21), bytes / rows /
#' bytes-per-row:
#'
#' | ver | bytes      |    rows | B/row |
#' |-----|-----------:|--------:|------:|
#' | v1  |  9,699,882 | 220,534 |  44.0 |
#' | v2  | 12,244,691 | 282,808 |  43.3 |
#' | v3  |  2,911,834 |  61,219 |  47.6 |
#' | v4  |  2,903,429 |       — |     — |
#' | v4b |  2,904,087 |       — |     — |
#' | v5  |  2,904,087 |       — |     — |
#' | v6  |  2,826,737 |  59,440 |  47.6 |
#' | v7  |  6,151,245 | 126,835 |  48.5 |
#' | v7b |  6,145,430 |       — |     — |
#' | v8  |  6,041,791 | 115,700 |  52.2 |
#' | v9  |  7,074,136 | 140,717 |  50.3 |
#'
#' Size tracks ROWS (43–52 B/row across every release measured — a legacy
#' release scores more zone x taxon combinations, not bigger ones), not the
#' release's vintage or age: a flat byte cap chases whichever release is
#' currently biggest and breaks on the next one (round 4's own 8 MB, then
#' 12 MB). The gate that actually catches bloat — a duplicated column, an
#' uncompressed write, a join that fans out — is bytes-per-row, with an
#' absolute floor so the app's own pre-download refusal (> 25 MB) keeps
#' margin.
#'
#' @return `list(max_bytes_per_row = 60, max_bytes = 16 * 1024 * 1024)`
app_bundle_zone_taxon_budget <- function()
  list(max_bytes_per_row = 60, max_bytes = 16L * 1024L * 1024L)

#' Assert a written `zone_taxon.parquet` meets the bytes-per-row + absolute rule
#'
#' @param path path to the written `zone_taxon.parquet`
#' @return one-row data frame: `bytes`, `rows`, `bytes_per_row`,
#'   `budget_bytes_per_row`, `budget_bytes`, `ok`
app_bundle_zone_taxon_check <- function(path) {
  b <- app_bundle_zone_taxon_budget()
  if (!file.exists(path))
    return(data.frame(bytes = NA_real_, rows = NA_integer_, bytes_per_row = NA_real_,
                      budget_bytes_per_row = b$max_bytes_per_row, budget_bytes = b$max_bytes,
                      ok = NA))
  bytes <- as.numeric(file.size(path))
  rows  <- as.integer(nrow(arrow::read_parquet(path, col_select = 1)))
  bpr   <- bytes / max(rows, 1)
  data.frame(bytes = bytes, rows = rows, bytes_per_row = round(bpr, 1),
             budget_bytes_per_row = b$max_bytes_per_row, budget_bytes = b$max_bytes,
             ok = bpr <= b$max_bytes_per_row && bytes <= b$max_bytes)
}

#' Self-test for [app_bundle_zone_taxon_check()] — a real seeded fault, shown red then restored
#'
#' Writes a genuine `zone_taxon.parquet` (a handful of rows), confirms it
#' passes, then writes a BLOATED version with a duplicated wide text column
#' (each row several KB of repeated text) that stays comfortably under the
#' 16 MB absolute cap but fails bytes-per-row -- exactly the shape of fault
#' (a fan-out join, an accidental duplicate column) the absolute cap alone
#' would miss on a small release. Confirms RED, then restores the real file
#' and confirms GREEN again, so this proves the fault and the recovery, not
#' just the fault.
#'
#' @return `TRUE`, invisibly; stops on the first failed expectation
app_bundle_zone_taxon_check_selftest <- function() {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  d <- tempfile("zt_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  good_path <- file.path(d, "zone_taxon.parquet")
  bad_path  <- file.path(d, "zone_taxon_bloated.parquet")

  good <- data.frame(zone_fld = rep("subregion_key", 100),
                     zone_value = rep(c("AK", "AT", "GA", "PA"), 25),
                     key = sprintf("sp%03d", 1:100),
                     area_km2 = runif(100, 1, 1000))
  msens::write_atlas_parquet(good, good_path)

  testthat::test_that("a real zone_taxon.parquet passes bytes-per-row + absolute", {
    chk <- app_bundle_zone_taxon_check(good_path)
    testthat::expect_true(chk$ok)
    testthat::expect_lte(chk$bytes_per_row, chk$budget_bytes_per_row)
  })

  # seeded fault: bloat every row with a duplicated wide text column -- LOW-
  # ENTROPY padding (a repeated short string) compresses away under
  # write_atlas_parquet()'s zstd almost for free and does not reproduce the
  # fault; RANDOM text does not compress, which is the point: it stands in
  # for a real fan-out join or an accidental duplicate column, not for
  # "any large string"
  set.seed(2026)
  rand_str <- function(n) paste0(sample(c(letters, LETTERS, 0:9), n, replace = TRUE), collapse = "")
  bad <- good
  bad$bloat <- vapply(seq_len(nrow(bad)), function(i) rand_str(120), "")   # ~120 B/row, incompressible
  msens::write_atlas_parquet(bad, bad_path)

  testthat::test_that("RED: a bloated zone_taxon.parquet fails bytes-per-row while under the absolute cap", {
    chk <- app_bundle_zone_taxon_check(bad_path)
    testthat::expect_false(chk$ok)
    testthat::expect_gt(chk$bytes_per_row, chk$budget_bytes_per_row)
    testthat::expect_lte(chk$bytes, chk$budget_bytes)   # the absolute cap ALONE would have missed this
  })

  testthat::test_that("GREEN: restoring the real file passes again", {
    testthat::expect_true(app_bundle_zone_taxon_check(good_path)$ok)
  })
  invisible(TRUE)
}

# ---- app/taxonomy.parquet -----------------------------------------------------
#
# round 4 (2026-09-21): msens 0.43.0 @ 71aa231 moved this INTO
# app_bundle_build() itself (app_taxonomy(), fed by taxonomy_csv=) -- the
# notebook's own writer (formerly app_taxonomy_table(), here) is deleted; a
# stale caller should get a "could not find function" error, not a silent
# stale copy running beside the real one.
