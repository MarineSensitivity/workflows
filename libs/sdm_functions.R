# SDM helper functions for DuckDB integration

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
  
  # species richness
  if ("species_richness" %in% metrics) {
    if (verbose) message("Calculating species richness...")
    
    DBI::dbExecute(con, glue::glue("
      DELETE FROM biodiv_metrics WHERE metric_type = 'species_richness';
      
      INSERT INTO biodiv_metrics (cell_id, metric_type, value)
      SELECT 
        cell_id,
        'species_richness' as metric_type,
        COUNT(DISTINCT sp_id) as value
      FROM sdm_values
      WHERE value > {threshold}
      GROUP BY cell_id
    "))
    
    n_metrics <- n_metrics + 1
  }
  
  # shannon diversity index
  if ("shannon_index" %in% metrics) {
    if (verbose) message("Calculating Shannon diversity index...")
    
    DBI::dbExecute(con, "
      DELETE FROM biodiv_metrics WHERE metric_type = 'shannon_index';
      
      INSERT INTO biodiv_metrics (cell_id, metric_type, value)
      SELECT 
        cell_id,
        'shannon_index' as metric_type,
        -SUM(p * LN(p)) as value
      FROM (
        SELECT 
          cell_id,
          value / SUM(value) OVER (PARTITION BY cell_id) as p
        FROM sdm_values
        WHERE value > 0
      ) t
      WHERE p > 0
      GROUP BY cell_id
    ")
    
    n_metrics <- n_metrics + 1
  }
  
  # simpson diversity index
  if ("simpson_index" %in% metrics) {
    if (verbose) message("Calculating Simpson diversity index...")
    
    DBI::dbExecute(con, "
      DELETE FROM biodiv_metrics WHERE metric_type = 'simpson_index';
      
      INSERT INTO biodiv_metrics (cell_id, metric_type, value)
      SELECT 
        cell_id,
        'simpson_index' as metric_type,
        1 - SUM(p * p) as value
      FROM (
        SELECT 
          cell_id,
          value / SUM(value) OVER (PARTITION BY cell_id) as p
        FROM sdm_values
        WHERE value > 0
      ) t
      GROUP BY cell_id
    ")
    
    n_metrics <- n_metrics + 1
  }
  
  # weighted endemism
  if ("weighted_endemism" %in% metrics) {
    if (verbose) message("Calculating weighted endemism...")
    
    # first calculate species ranges
    DBI::dbExecute(con, "
      CREATE TEMP TABLE species_ranges AS
      SELECT 
        sp_id,
        COUNT(DISTINCT cell_id) as range_size
      FROM sdm_values
      WHERE value > {threshold}
      GROUP BY sp_id
    ")
    
    # then calculate weighted endemism
    DBI::dbExecute(con, glue::glue("
      DELETE FROM biodiv_metrics WHERE metric_type = 'weighted_endemism';
      
      INSERT INTO biodiv_metrics (cell_id, metric_type, value)
      SELECT 
        v.cell_id,
        'weighted_endemism' as metric_type,
        SUM(1.0 / r.range_size) as value
      FROM sdm_values v
      JOIN species_ranges r ON v.sp_id = r.sp_id
      WHERE v.value > {threshold}
      GROUP BY v.cell_id
    "))
    
    DBI::dbExecute(con, "DROP TABLE species_ranges")
    
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
  
  # build query
  query <- glue::glue("
    SELECT 
      c.lon, 
      c.lat, 
      COALESCE(m.value, 0) as value
    FROM sdm_cells c
    LEFT JOIN biodiv_metrics m 
      ON c.cell_id = m.cell_id 
      AND m.metric_type = '{metric_type}'
  ")
  
  # add extent filter if provided
  if (!is.null(extent)) {
    query <- glue::glue("{query}
      WHERE c.lon BETWEEN {extent[1]} AND {extent[2]}
        AND c.lat BETWEEN {extent[3]} AND {extent[4]}
    ")
  }
  
  # get data
  df <- DBI::dbGetQuery(con, query)
  
  # create raster
  r <- create_raster_from_points(
    df, 
    value_col = "value",
    res = res,
    crs = "EPSG:4326",
    extent = extent
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
    "idx_sdm_cells_geom" = "CREATE SPATIAL INDEX idx_sdm_cells_geom ON sdm_cells(geom)",
    "idx_planning_areas_geom" = "CREATE SPATIAL INDEX idx_planning_areas_geom ON planning_areas(geom)",
    "idx_sdm_values_cell_sp" = "CREATE INDEX idx_sdm_values_cell_sp ON sdm_values(cell_id, sp_id)",
    "idx_biodiv_metrics_cell_type" = "CREATE INDEX idx_biodiv_metrics_cell_type ON biodiv_metrics(cell_id, metric_type)"
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
  results$orphaned_values <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as n_orphaned
    FROM sdm_values v
    LEFT JOIN sdm_cells c ON v.cell_id = c.cell_id
    WHERE c.cell_id IS NULL
  ")$n_orphaned
  
  # check value ranges
  results$value_range <- DBI::dbGetQuery(con, "
    SELECT 
      MIN(value) as min_value,
      MAX(value) as max_value
    FROM sdm_values
  ")
  
  # check for duplicate entries
  results$duplicates <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as n_duplicates
    FROM (
      SELECT cell_id, sp_id, COUNT(*) as n
      FROM sdm_values
      GROUP BY cell_id, sp_id
      HAVING COUNT(*) > 1
    ) t
  ")$n_duplicates
  
  # count records
  results$record_counts <- DBI::dbGetQuery(con, "
    SELECT 
      (SELECT COUNT(*) FROM sdm_sources) as n_sources,
      (SELECT COUNT(*) FROM sdm_models) as n_models,
      (SELECT COUNT(*) FROM sdm_species) as n_species,
      (SELECT COUNT(*) FROM sdm_cells) as n_cells,
      (SELECT COUNT(*) FROM sdm_values) as n_values,
      (SELECT COUNT(*) FROM biodiv_metrics) as n_metrics
  ")
  
  if (verbose) {
    message("=== SDM Data Validation Results ===")
    message(glue::glue("Orphaned values: {results$orphaned_values}"))
    message(glue::glue("Value range: [{results$value_range$min_value}, {results$value_range$max_value}]"))
    message(glue::glue("Duplicate entries: {results$duplicates}"))
    message("\nRecord counts:")
    for (name in names(results$record_counts)) {
      message(glue::glue("  {name}: {results$record_counts[[name]]}"))
    }
  }
  
  return(results)
}