# density_ref.R — the AquaMaps reference for the `qmap` density transform (v11 Q6) ----
#
# Sourced by the three density ingests (ingest_sdm-nc.qmd, ingest_sdm-gm.qmd, ingest_swfsc_cce.qmd) AFTER libs/paths.R:
#   source(here("libs/density_ref.R"))
#
# `*_TRANSFORM=qmap` quantile-maps a survey model's densities onto the SPECIES' OWN AquaMaps suitability over the same
# cells (msens::density_to_suit(method = "qmap", ref = ...)): the survey keeps its spatial ordering and takes AquaMaps'
# marginal, instead of the `ud` percentile that puts the densest survey cell at 100 whatever AquaMaps says.
#
# Reference of one model = the species' AquaMaps `val` over the cells of THIS model's domain (the regridded cells with a
# positive density; for a union of AquaMaps models of one taxon the per-cell MAX, which is what the merge's suitability
# surface is). AquaMaps rows exist only where val >= 1, so a domain cell AquaMaps does not cover contributes nothing.
#
# Which AquaMaps model(s)? The pipeline's own crosswalk: ver_prev's `taxon_model` (mdl_key <-> taxon_authority, taxon_id;
# `merge_models_prep` keeps it stable across versions), restricted to `am|` keys. A density model's taxon is
#   - gm / cce : WORMS:{worms_id}  (cce: else the accepted AphiaID -- the delivered name of Delphinus capensis is a synonym
#                that AquaMaps resolves to Delphinus delphis)
#   - nc       : BOTW:{sisid} (the bird authority), else WORMS:{worms_id}
# `model_am.csv` carries only (mdl_key, sp_key, taxa, iucn_id) -- no WoRMS id -- so it cannot be that map; it is used here
# to assert that every crosswalked `am|` key has a parquet in the dataset directory.

librarian::shelf(dplyr, tibble, quiet = TRUE)

density_ref_min_cells <- 50L                                    # fewer reference cells than this: fall back to `ud`
density_ref_fallback  <- "ud(fallback: no am ref)"              # what model CSVs record for a fallen-back model

#' ver_prev's crosswalk of the AquaMaps models: tibble(mdl_key, authority, taxon_id)
#'
#' Read as `merge_models_prep.qmd` reads it: the released `tables/taxon_model.parquet` when present, else `merge.duckdb`
#' (read-only). Only `am|` keys are kept.
density_am_index <- function(dir_big, ver_prev) {
  pq <- path.expand(file.path(dir_big, ver_prev, "marine-atlas/tables/taxon_model.parquet"))
  db <- path.expand(file.path(dir_big, ver_prev, "marine-atlas/merge.duckdb"))
  x <- if (file.exists(pq)) {
    arrow::read_parquet(pq) |> as_tibble()
  } else if (file.exists(db)) {
    con <- DBI::dbConnect(duckdb::duckdb(db, read_only = TRUE))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbGetQuery(con, "SELECT mdl_key, taxon_authority, taxon_id FROM taxon_model") |> as_tibble()
  } else stop("density_am_index: no ", ver_prev, " taxon_model at ", pq, " nor ", db)
  x |> filter(startsWith(mdl_key, "am|")) |>
    transmute(mdl_key, authority = tolower(taxon_authority), taxon_id = as.character(taxon_id))
}

#' The AquaMaps model keys of a taxon: the `am|` keys whose taxon is any of the given ids
#'
#' @param idx output of [density_am_index()].
#' @param worms character; candidate WoRMS AphiaIDs, in order of preference (NA / "" ignored).
#' @param botw character; candidate BirdLife sisids.
#' @return character vector of `am|...` keys (possibly empty). The FIRST candidate id (botw before worms) that has any
#'   AquaMaps model wins; ids are never pooled across candidates, so a synonym never adds a second species' surface.
density_am_keys <- function(idx, worms = character(), botw = character()) {
  ok <- \(v) { v <- unique(as.character(v)); v[!is.na(v) & nzchar(v) & v != "NA"] }
  for (id in ok(botw)) { k <- idx$mdl_key[idx$authority == "botw" & idx$taxon_id == id]; if (length(k)) return(k) }
  for (id in ok(worms)) { k <- idx$mdl_key[idx$authority == "worms" & idx$taxon_id == id]; if (length(k)) return(k) }
  character()
}

#' The species' AquaMaps surface over a domain: (cell_id, val), per-cell max over the taxon's `am|` models
#'
#' A pelagic species' AquaMaps surface is global (a humpback: 15.7 M rows), so the parquets are scanned by DuckDB with the
#' domain's cells as a semi-join: only the domain's rows ever reach R.
#'
#' @param keys `am|...` keys (from [density_am_keys()]).
#' @param am_dir the AquaMaps dataset directory (`dist/dataset=am`), opened read-only.
#' @param cell_ids integer; the cells of the species' domain (every model of the species). `NULL` = all rows.
#' @return a tibble(cell_id, val), or NULL for `keys` empty.
density_am_cells <- function(keys, am_dir, cell_ids = NULL) {
  if (!length(keys)) return(NULL)
  f <- file.path(am_dir, paste0(sub("^am\\|", "", keys), ".parquet"))
  stopifnot("an AquaMaps model in the crosswalk has no parquet in the dataset directory" = all(file.exists(f)))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "PRAGMA memory_limit='2GB'"); DBI::dbExecute(con, "PRAGMA threads=4")
  DBI::dbExecute(con, paste0("PRAGMA temp_directory='", tempdir(), "'"))
  src <- paste0("read_parquet([", paste0("'", f, "'", collapse = ", "), "])")
  if (!is.null(cell_ids)) {
    DBI::dbWriteTable(con, "dom", data.frame(cell_id = as.integer(unique(cell_ids))))
    where <- "AND cell_id IN (SELECT cell_id FROM dom)"
  } else where <- ""
  as_tibble(DBI::dbGetQuery(con, paste0("SELECT cell_id, max(val) AS val FROM ", src, " WHERE val IS NOT NULL ", where, " GROUP BY cell_id"))) |>
    arrange(cell_id)
}

#' Quantile-map one model's densities onto the AquaMaps reference over its own domain
#'
#' `val = density_to_suit(dens, "qmap", ref = <AquaMaps val over the cells with dens > 0>, digits = 2)`, or the `ud`
#' percentile (area-weighted) when the species has no AquaMaps model or fewer than `min_ref` reference cells in the domain.
#'
#' @param dens,cell_id the model's regridded cells (positive densities; a non-positive one is 0).
#' @param ref_cells tibble(cell_id, val) from [density_am_cells()], or NULL (no AquaMaps model).
#' @param area numeric; the cells' areas (km^2), for the `ud` fallback.
#' @param min_ref minimum reference cells for `qmap`.
#' @return list(val, transform = "qmap" | [density_ref_fallback], n_ref_cells, ref_p50, ref_p95, ref).
density_qmap_one <- function(dens, cell_id, ref_cells, area, min_ref = density_ref_min_cells, digits = 2) {
  ref <- if (is.null(ref_cells)) numeric() else ref_cells$val[ref_cells$cell_id %in% cell_id[!is.na(dens) & dens > 0]]
  q   <- \(p) if (length(ref)) as.numeric(stats::quantile(ref, p, names = FALSE)) else NA_real_
  if (length(ref) < min_ref)
    return(list(val = msens::density_to_suit(dens, "ud", area = area, digits = digits), transform = density_ref_fallback,
                n_ref_cells = length(ref), ref_p50 = q(.5), ref_p95 = q(.95), ref = ref))
  list(val = msens::density_to_suit(dens, "qmap", ref = ref, digits = digits), transform = "qmap",
       n_ref_cells = length(ref), ref_p50 = q(.5), ref_p95 = q(.95), ref = ref)
}

#' [density_qmap_one()] per group of rows (a model = region x interval, interval, ...)
#'
#' @param x data frame with `cell_id`, `dens` and the grouping columns `by`.
#' @param by character; grouping columns (model = one group), or `character()` for one group.
#' @param area_fun function(cell_id) -> cell areas (km^2).
#' @return list(val aligned to the rows of `x`, stats = one row per group: `by`, transform, n_ref_cells, ref_p50, ref_p95,
#'   n_domain, q10/q50/q90 of the kept qmap val), refs = list of the groups' reference vectors, parallel to `stats`).
density_qmap_groups <- function(x, by, ref_cells, area_fun, min_ref = density_ref_min_cells, digits = 2) {
  g   <- if (length(by)) do.call(paste, c(unname(as.list(x[by])), sep = "\r")) else rep("all", nrow(x))
  idx <- split(seq_len(nrow(x)), factor(g, levels = unique(g)))
  val <- numeric(nrow(x)); st <- list(); refs <- list()
  for (i in seq_along(idx)) {
    r <- idx[[i]]
    o <- density_qmap_one(x$dens[r], x$cell_id[r], ref_cells, area_fun(x$cell_id[r]), min_ref, digits)
    val[r] <- o$val
    kept   <- o$val[o$val >= 1]
    st[[i]] <- bind_cols(as_tibble(x[r[1], by, drop = FALSE]),
                         tibble(transform = o$transform, n_ref_cells = o$n_ref_cells, ref_p50 = o$ref_p50, ref_p95 = o$ref_p95,
                                n_domain = length(r), val_q10 = if (length(kept)) stats::quantile(kept, .1, names = FALSE) else NA_real_,
                                val_q50 = if (length(kept)) stats::quantile(kept, .5, names = FALSE) else NA_real_,
                                val_q90 = if (length(kept)) stats::quantile(kept, .9, names = FALSE) else NA_real_))
    refs[[i]] <- o$ref
  }
  list(val = val, stats = bind_rows(st), refs = refs)
}

#' Pool the per-group stats of several groups into ONE model row (regions of a union model): transform, n_ref_cells, ref_p50, ref_p95
#'
#' @param stats the `stats` of [density_qmap_groups()] restricted to the pooled groups; @param refs the matching `refs`.
density_qmap_pool <- function(stats, refs) {
  r <- unlist(refs)
  tibble(transform   = paste(unique(stats$transform), collapse = "|"),
         n_ref_cells = sum(stats$n_ref_cells),
         ref_p50     = if (length(r)) as.numeric(stats::quantile(r, .5,  names = FALSE)) else NA_real_,
         ref_p95     = if (length(r)) as.numeric(stats::quantile(r, .95, names = FALSE)) else NA_real_)
}
