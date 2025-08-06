# SDM helper functions for DuckDB integration

#' Get AquaMaps species probability raster
#'
#' @param sp_key Species key
#' @param con_am DuckDB connection
#' @param r_hcaf HCAF raster
#' @param dir_cache Directory for cached files
#' @param verbose Whether to print progress messages
#' @return Species probability raster
am_sp_rast <- function(sp_key, con_am, r_hcaf = NULL, verbose = FALSE) {

  d <- dplyr::tbl(con_am, "spp_cells") |>
    dplyr::filter(sp_key == !!sp_key) |>
    dplyr::select(cell_id, probability) |>
    dplyr::left_join(
      dplyr::tbl(con_am, "cells") |>
        dplyr::select(cell_id, cell_idx),
      by = "cell_id") |>
    dplyr::collect()

  if (is.null(r_hcaf))
    r_hcaf <- get_hcaf_raster()

  r <- init(r_hcaf[[1]], NA)
  r[d$cell_idx] <- d$probability

  names(r) <- "suitability"

  # convert to integer percentage for smaller file size
  r <- as.int(round(r * 100)) %>%
    ifel(. == 0, NA, .) |>
    trim()
  # plet(r)
  rng <- global(r, fun = "range", na.rm=T) |> as.numeric()
  stopifnot(all(rng >= 1 & rng <= 100))

  r
}

#' Downsample a species raster
#'
#' @param r_sp raster of species suitability [0,100]
#' @param r_mask raster template layer with topology to resample and NA to apply as mask
#' @param verbose Whether to print progress messages
#' @return Species probability raster
downsample_sp_rast <- function(r_sp, r_mask, method = "bilinear", verbose = FALSE) {

  # r_sp = r_am; r_mask = r_cell$depth_mean; method = "bilinear"

  # rng <- global(r_am, fun = "range", na.rm=T) |> as.numeric()
  # stopifnot(all(rng >= 1 & rng <= 100))
  # plot(r_am, main = "Downsampled Species Raster")

  r_ds <- resample(
    ifel(
      is.na(r_sp),
      0,
      r_sp),
    r_mask,
    method = method) |>
    mask(r_mask)
  # plot(r_ds, main = "Downsampled Species Raster")
  r_ds <- as.int(round(r_ds)) %>%
    ifel(. == 0, NA, .) # |>
    # trim()

  rng <- global(r_ds, fun = "range", na.rm=T) |> as.numeric()
  if (all(is.nan(rng))){
    message("Downsampled raster has no valid values")
  } else {
    stopifnot(all(rng >= 1 & rng <= 100))
  }

  r_ds
}


#' Batch insert values into a DuckDB table
#'
#' @param con DuckDB connection
#' @param df Data frame to insert
#' @param table_name Name of the target table
#' @param batch_size Number of rows per batch (default: 10000)
#' @param verbose Whether to print progress messages
#' @return Number of rows inserted
#' @export
batch_insert_values <- function(con, df, table_name, batch_size = 10000, verbose = TRUE) {
  n_rows <- nrow(df)
  n_batches <- ceiling(n_rows / batch_size)

  if (verbose) {
    message(glue::glue("Inserting {n_rows} rows into {table_name} in {n_batches} batches..."))
  }

  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_rows)

    batch <- df[start_idx:end_idx, ]

    DBI::dbWriteTable(con, table_name, batch, append = TRUE)

    if (verbose && i %% 10 == 0) {
      message(glue::glue("  Completed batch {i}/{n_batches}"))
    }
  }

  return(n_rows)
}

#' Create raster from dataframe using terra::subst
#'
#' @param df Data frame with cell_id column and value columns
#' @param r_template Terra raster template (default: global 0.05 degree)
#' @param cell_col Name of cell ID column (default: "cell_id")
#' @param value_col Name of value column to rasterize
#' @return Terra SpatRaster object with one layer per value column
#' @export
create_raster_from_df <- function(
    df, r_template, cell_col = "cell_id",
    value_col = NULL) {

  if (is.null(value_col))
    value_col <- setdiff(names(df), cell_col)[1]

  r <- init(r_template[[1]], NA)
  r[df[[cell_col]]] <- df[[value_col]]

  r
}


#' Create raster from point data
#'
#' @param df Data frame with lon, lat, and value columns
#' @param value_col Name of the value column
#' @param res Resolution in decimal degrees
#' @param crs Coordinate reference system
#' @param extent Optional extent vector c(xmin, xmax, ymin, ymax)
#' @return Terra SpatRaster object
#' @export
create_raster_from_points <- function(df, value_col = "value", res = 0.05,
                                      crs = "EPSG:4326", extent = NULL) {

  if (is.null(extent)) {
    extent <- c(
      xmin = floor(min(df$lon) / res) * res,
      xmax = ceiling(max(df$lon) / res) * res,
      ymin = floor(min(df$lat) / res) * res,
      ymax = ceiling(max(df$lat) / res) * res
    )
  }

  # create template raster
  r_template <- terra::rast(
    xmin = extent["xmin"],
    xmax = extent["xmax"],
    ymin = extent["ymin"],
    ymax = extent["ymax"],
    resolution = res,
    crs = crs
  )

  # convert points to spatial
  pts <- terra::vect(
    df,
    geom = c("lon", "lat"),
    crs = crs
  )

  # rasterize points
  r <- terra::rasterize(pts, r_template, field = value_col, fun = "mean")
  names(r) <- value_col

  return(r)
}

load_duckdb_extensions <- function(con, spatial=T, icu=T) {

  # check_spatial <- function(con) {
  #   tryCatch({
  #     res <- dbGetQuery(con, "SELECT ST_Point(0, 0)")
  #     return(TRUE)
  #   }, error = function(e) {
  #     return(FALSE)
  #   } ) }
  #
  # check_icu <- function(con) {
  #   tryCatch({
  #     res <- dbGetQuery(con, "SELECT TIMESTAMPTZ '1992-09-20 11:30:00.123456789';")
  #     return(TRUE)
  #   }, error = function(e) {
  #     return(FALSE)
  #   } ) }

  # if (spatial && !check_spatial(con))
  if (spatial)
    res <- dbExecute(con, "INSTALL spatial; LOAD spatial;")

  # if (icu && !check_icu(con))
  if (icu)
    res <- dbExecute(con, "INSTALL icu; LOAD icu;")

  con
}

#' Transform SDM raster to common resolution
#'
#' @param r_in Input raster
#' @param res_deg Target resolution in decimal degrees (default: 0.05)
#' @param method Resampling method (default: "bilinear")
#' @param extent_out Optional output extent
#' @return Transformed raster
#' @export
transform_sdm_to_resolution <- function(r_in, res_deg = 0.05,
                                        method = "bilinear", extent_out = NULL) {

  # define target extent
  if (is.null(extent_out)) {
    ext_in <- terra::ext(r_in)
    extent_out <- terra::ext(
      floor(ext_in$xmin / res_deg) * res_deg,
      ceiling(ext_in$xmax / res_deg) * res_deg,
      floor(ext_in$ymin / res_deg) * res_deg,
      ceiling(ext_in$ymax / res_deg) * res_deg
    )
  }

  # create target raster template
  r_template <- terra::rast(
    extent = extent_out,
    resolution = res_deg,
    crs = terra::crs(r_in)
  )

  # resample to target resolution
  r_out <- terra::resample(r_in, r_template, method = method)

  return(r_out)
}

#' Calculate biodiversity metrics for cells
#'
#' @param con DuckDB connection
#' @param metrics Vector of metric names to calculate
#' @param threshold Presence threshold for binary metrics (default: 0.5)
#' @param verbose Whether to print progress messages
#' @return Number of metrics calculated
#' @export
calculate_biodiversity_metrics <- function(con,
                                           metrics = c("species_richness", "shannon_index"),
                                           threshold = 0.5,
                                           verbose = TRUE) {

  n_metrics <- 0

  # first ensure metrics exist in metric table
  for (metric_type in metrics) {
    metric_exists <- DBI::dbGetQuery(con, glue::glue("
      SELECT COUNT(*) as n FROM metric WHERE metric_type = '{metric_type}'
    "))$n > 0

    if (!metric_exists) {
      description <- switch(metric_type,
        species_richness = "Number of species with value > threshold",
        shannon_index = "Shannon diversity index",
        simpson_index = "Simpson diversity index",
        weighted_endemism = "Weighted endemism index",
        "User-defined metric"
      )

      DBI::dbExecute(con, glue::glue("
        INSERT INTO metric (metric_type, description)
        VALUES ('{metric_type}', '{description}')
      "))
    }
  }

  # get metric IDs
  metric_ids <- DBI::dbGetQuery(con, "SELECT metric_seq, metric_type FROM metric")

  # species richness
  if ("species_richness" %in% metrics) {
    if (verbose) message("Calculating species richness...")

    metric_seq <- metric_ids$metric_seq[metric_ids$metric_type == "species_richness"]

    DBI::dbExecute(con, glue::glue("
      DELETE FROM cell_metric WHERE metric_seq = {metric_seq};

      INSERT INTO cell_metric (cell_id, metric_seq, value)
      SELECT
        mc.cell_id,
        {metric_seq} as metric_seq,
        COUNT(DISTINCT m.taxa) as value
      FROM model_cell mc
      JOIN model m ON mc.mdl_seq = m.mdl_seq
      WHERE mc.value > {threshold}
      GROUP BY mc.cell_id
    "))

    n_metrics <- n_metrics + 1
  }

  # shannon diversity index
  if ("shannon_index" %in% metrics) {
    if (verbose) message("Calculating Shannon diversity index...")

    metric_seq <- metric_ids$metric_seq[metric_ids$metric_type == "shannon_index"]

    DBI::dbExecute(con, glue::glue("
      DELETE FROM cell_metric WHERE metric_seq = {metric_seq};

      INSERT INTO cell_metric (cell_id, metric_seq, value)
      SELECT
        cell_id,
        {metric_seq} as metric_seq,
        -SUM(p * LN(p)) as value
      FROM (
        SELECT
          mc.cell_id,
          mc.value / SUM(mc.value) OVER (PARTITION BY mc.cell_id) as p
        FROM model_cell mc
        WHERE mc.value > 0
      ) t
      WHERE p > 0
      GROUP BY cell_id
    "))

    n_metrics <- n_metrics + 1
  }

  # simpson diversity index
  if ("simpson_index" %in% metrics) {
    if (verbose) message("Calculating Simpson diversity index...")

    metric_seq <- metric_ids$metric_seq[metric_ids$metric_type == "simpson_index"]

    DBI::dbExecute(con, glue::glue("
      DELETE FROM cell_metric WHERE metric_seq = {metric_seq};

      INSERT INTO cell_metric (cell_id, metric_seq, value)
      SELECT
        cell_id,
        {metric_seq} as metric_seq,
        1 - SUM(p * p) as value
      FROM (
        SELECT
          mc.cell_id,
          mc.value / SUM(mc.value) OVER (PARTITION BY mc.cell_id) as p
        FROM model_cell mc
        WHERE mc.value > 0
      ) t
      GROUP BY cell_id
    "))

    n_metrics <- n_metrics + 1
  }

  # weighted endemism
  if ("weighted_endemism" %in% metrics) {
    if (verbose) message("Calculating weighted endemism...")

    metric_seq <- metric_ids$metric_seq[metric_ids$metric_type == "weighted_endemism"]

    # first calculate taxa ranges
    DBI::dbExecute(con, glue::glue("
      CREATE TEMP TABLE taxa_ranges AS
      SELECT
        m.taxa,
        COUNT(DISTINCT mc.cell_id) as range_size
      FROM model_cell mc
      JOIN model m ON mc.mdl_seq = m.mdl_seq
      WHERE mc.value > {threshold}
      GROUP BY m.taxa
    "))

    # then calculate weighted endemism
    DBI::dbExecute(con, glue::glue("
      DELETE FROM cell_metric WHERE metric_seq = {metric_seq};

      INSERT INTO cell_metric (cell_id, metric_seq, value)
      SELECT
        mc.cell_id,
        {metric_seq} as metric_seq,
        SUM(1.0 / tr.range_size) as value
      FROM model_cell mc
      JOIN model m ON mc.mdl_seq = m.mdl_seq
      JOIN taxa_ranges tr ON m.taxa = tr.taxa
      WHERE mc.value > {threshold}
      GROUP BY mc.cell_id
    "))

    DBI::dbExecute(con, "DROP TABLE taxa_ranges")

    n_metrics <- n_metrics + 1
  }

  return(n_metrics)
}

#' Summarize SDM values by planning areas
#'
#' @param con DuckDB connection
#' @param metric_types Vector of biodiversity metric types to summarize
#' @param summary_funs Named list of summary functions (default: mean)
#' @param verbose Whether to print progress messages
#' @return Data frame of planning area summaries
#' @export
summarize_by_planning_areas <- function(con,
                                        metric_types = c("species_richness"),
                                        summary_funs = list(avg = "AVG", max = "MAX"),
                                        verbose = TRUE) {

  results <- list()

  for (metric_type in metric_types) {
    for (fun_name in names(summary_funs)) {
      fun_sql <- summary_funs[[fun_name]]

      if (verbose) {
        message(glue::glue("Calculating {fun_name} {metric_type} by planning area..."))
      }

      # delete existing metrics
      DBI::dbExecute(con, glue::glue("
        DELETE FROM planning_area_metrics
        WHERE metric_type = '{fun_name}_{metric_type}'
      "))

      # calculate new metrics
      DBI::dbExecute(con, glue::glue("
        INSERT INTO planning_area_metrics (pa_key, metric_type, value)
        SELECT
          p.pa_key,
          '{fun_name}_{metric_type}' as metric_type,
          {fun_sql}(m.value) as value
        FROM planning_areas p
        JOIN sdm_cells c ON ST_Within(c.geom, p.geom)
        JOIN biodiv_metrics m ON c.cell_id = m.cell_id
        WHERE m.metric_type = '{metric_type}'
        GROUP BY p.pa_key
      "))

      # query results
      result <- DBI::dbGetQuery(con, glue::glue("
        SELECT
          p.pa_key,
          p.pa_name,
          m.metric_type,
          m.value
        FROM planning_areas p
        JOIN planning_area_metrics m ON p.pa_key = m.pa_key
        WHERE m.metric_type = '{fun_name}_{metric_type}'
        ORDER BY m.value DESC
      "))

      results[[glue::glue("{fun_name}_{metric_type}")]] <- result
    }
  }

  # combine results
  combined <- results[[1]]
  if (length(results) > 1) {
    for (i in 2:length(results)) {
      combined <- dplyr::full_join(
        combined,
        results[[i]] |> dplyr::select(-pa_name),
        by = "pa_key"
      )
    }
  }

  return(combined)
}

#' Export biodiversity metric to raster
#'
#' @param con DuckDB connection
#' @param metric_type Type of biodiversity metric
#' @param extent Optional extent vector c(xmin, xmax, ymin, ymax)
#' @param res Resolution in decimal degrees (default: 0.05)
#' @param output_file Optional output file path
#' @param cog Whether to save as Cloud Optimized GeoTIFF
#' @return Terra SpatRaster object
#' @export
export_metric_raster <- function(con, metric_type, extent = NULL,
                                 res = 0.05, output_file = NULL, cog = TRUE) {

  # get metric ID
  metric_seq <- DBI::dbGetQuery(con, glue::glue("
    SELECT metric_seq FROM metric WHERE metric_type = '{metric_type}'
  "))$metric_seq

  if (length(metric_seq) == 0) {
    stop(glue::glue("Metric type '{metric_type}' not found in database"))
  }

  # build query
  query <- glue::glue("
    SELECT
      cm.cell_id,
      cm.value
    FROM cell_metric cm
    WHERE cm.metric_seq = {metric_seq}
  ")

  # add extent filter if provided
  if (!is.null(extent)) {
    query <- glue::glue("{query}
      AND cm.cell_id IN (
        SELECT cell_id FROM cell
        WHERE lon BETWEEN {extent[1]} AND {extent[2]}
          AND lat BETWEEN {extent[3]} AND {extent[4]}
      )
    ")
  }

  # get data
  df <- DBI::dbGetQuery(con, query)

  # create raster using the new function
  r <- create_raster_from_df(
    df,
    cell_col = "cell_id",
    value_cols = "value"
  )

  # set name
  names(r) <- metric_type

  # save if requested
  if (!is.null(output_file)) {
    if (cog) {
      terra::writeRaster(r, output_file, filetype = "COG", overwrite = TRUE)
    } else {
      terra::writeRaster(r, output_file, overwrite = TRUE)
    }
  }

  return(r)
}

#' Create spatial index for efficient queries
#'
#' @param con DuckDB connection
#' @param verbose Whether to print progress messages
#' @export
create_spatial_indexes <- function(con, verbose = TRUE) {

  indexes <- list(
    "idx_cell_geom" = "CREATE SPATIAL INDEX idx_cell_geom ON cell(geom)",
    "idx_planning_areas_geom" = "CREATE SPATIAL INDEX idx_planning_areas_geom ON planning_areas(geom)",
    "idx_model_cell_cell_mdl" = "CREATE INDEX idx_model_cell_cell_mdl ON model_cell(cell_id, mdl_seq)",
    "idx_cell_metric_cell_metric" = "CREATE INDEX idx_cell_metric_cell_metric ON cell_metric(cell_id, metric_seq)",
    "idx_region_metric_rgn_metric" = "CREATE INDEX idx_region_metric_rgn_metric ON region_metric(rgn_seq, metric_seq)",
    "idx_region_cell_rgn" = "CREATE INDEX idx_region_cell_rgn ON region_cell(rgn_seq)",
    "idx_region_cell_cell" = "CREATE INDEX idx_region_cell_cell ON region_cell(cell_id)",
    "idx_species_taxa" = "CREATE INDEX idx_species_taxa ON species(ds_key, taxa)",
    "idx_model_taxa" = "CREATE INDEX idx_model_taxa ON model(ds_key, taxa)"
  )

  for (idx_name in names(indexes)) {
    if (verbose) message(glue::glue("Creating index: {idx_name}"))

    tryCatch({
      DBI::dbExecute(con, indexes[[idx_name]])
    }, error = function(e) {
      if (!grepl("already exists", e$message)) {
        warning(glue::glue("Failed to create index {idx_name}: {e$message}"))
      }
    })
  }
}

#' Validate SDM data integrity
#'
#' @param con DuckDB connection
#' @param verbose Whether to print validation results
#' @return List of validation results
#' @export
validate_sdm_data <- function(con, verbose = TRUE) {

  results <- list()

  # check for orphaned records
  results$orphaned_model_cells <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as n_orphaned
    FROM model_cell mc
    LEFT JOIN cell c ON mc.cell_id = c.cell_id
    WHERE c.cell_id IS NULL
  ")$n_orphaned

  # check value ranges
  results$value_range <- DBI::dbGetQuery(con, "
    SELECT
      MIN(value) as min_value,
      MAX(value) as max_value
    FROM model_cell
  ")

  # check for duplicate entries
  results$duplicates <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as n_duplicates
    FROM (
      SELECT cell_id, mdl_seq, COUNT(*) as n
      FROM model_cell
      GROUP BY cell_id, mdl_seq
      HAVING COUNT(*) > 1
    ) t
  ")$n_duplicates

  # count records
  results$record_counts <- DBI::dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*) FROM dataset) as n_datasets,
      (SELECT COUNT(*) FROM model) as n_models,
      (SELECT COUNT(*) FROM species) as n_species,
      (SELECT COUNT(*) FROM cell) as n_cells,
      (SELECT COUNT(*) FROM model_cell) as n_model_cells,
      (SELECT COUNT(*) FROM metric) as n_metrics,
      (SELECT COUNT(*) FROM cell_metric) as n_cell_metrics,
      (SELECT COUNT(*) FROM region) as n_regions,
      (SELECT COUNT(*) FROM region_metric) as n_region_metrics
  ")

  if (verbose) {
    message("=== SDM Data Validation Results ===")
    message(glue::glue("Orphaned model_cell records: {results$orphaned_model_cells}"))
    message(glue::glue("Value range: [{results$value_range$min_value}, {results$value_range$max_value}]"))
    message(glue::glue("Duplicate entries: {results$duplicates}"))
    message("\nRecord counts:")
    for (name in names(results$record_counts)) {
      message(glue::glue("  {name}: {results$record_counts[[name]]}"))
    }
  }

  return(results)
}


set_geom_ctr_area <- function(x){

  x |>
    st_set_geometry("geom") |>
    mutate(
      ctr      = st_centroid(geom),
      ctr_lon  = ctr |> st_coordinates() %>% .[,"X"],
      ctr_lat  = ctr |> st_coordinates() %>% .[,"Y"],
      area_km2 = st_area(geom) |>
        units::set_units(km^2) |>
        as.numeric() ) |>
    select(-ctr)
}

drop_ctr_area <- function(x){
  x |>
    select(-ctr_lon, -ctr_lat, -area_km2)
}

register_zones <- function(ply, tbl, fld) {
  # register zones in the database
  dbExecute(con_sdm, glue("DELETE FROM zone WHERE tbl = '{tbl}' AND fld = '{fld}'"))

  # check that fld is unique in tbl
  stopifnot(sum(duplicated(ply[[fld]])) == 0)

  for (i in 1:nrow(ply)) { # i = 1
    dbExecute(con_sdm, glue("
      INSERT INTO zone (tbl, fld, value)
      VALUES ('{tbl}', '{fld}', '{ply[[fld]][i]}') " ) )
  }
}

insert_zone_cell_data <- function(df, tbl, fld) {
  # df = d_pa; tbl = "ply_planareas_2025"; fld = "planarea_key"
  # df = d_er; tbl = "ply_ecoregions_2025"; fld = "ecoregion_key"

  # get zone and metric IDs
  d_zone <- dbGetQuery(con_sdm, glue("
    SELECT zone_seq, value FROM zone WHERE tbl = '{tbl}' AND fld = '{fld}'" ))

  d_zone_cell <- df |>
    left_join(
      d_zone,
      by = c("layer" = "value")) |>
    mutate(
      zone_seq    = as.integer(zone_seq),
      pct_covered = as.integer(round(values * 100)) ) |>
    filter(pct_covered > 0) |>
    select(
      zone_seq,
      cell_id = cell,
      pct_covered)

  dbExecute(con_sdm, glue("DELETE FROM zone_cell WHERE zone_seq IN ({paste(d_zone$zone_seq, collapse = ',')})"))
  dbWriteTable(con_sdm, "zone_cell", d_zone_cell, append = T)
}

