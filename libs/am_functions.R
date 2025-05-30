# AquaMaps replication functions
# Extracted from replicate_aquamaps.qmd

#' Compare original and replicated rasters
#'
#' @param r_old Original raster
#' @param r_new Replicated raster
#' @param return Return type: "boolean", "rast", or "tibble"
#' @param tolerance Tolerance for comparing raster values
#' @param sp_info Optional species information
#' @param con_dd Optional DuckDB connection
#' @param sp_key Optional species key (required if sp_info is NULL and tibble is requested)
#' @param verbose Whether to print progress messages
#' @return Comparison result based on return type
compare_rasters <- function(r_old, r_new, return = "boolean", tolerance = 0.02,
                           sp_info = NULL, con_dd = NULL, sp_key = NULL, verbose = FALSE) {
  # Check if rasters match
  # Return TRUE if they are identical within tolerance
  # Return FALSE otherwise
  stopifnot(return %in% c("boolean", "rast", "tibble"))

  # Prepare rasters for comparison by aligning them
  r_union   <- terra::ext(c(r_old, r_new))
  r_old_ext <- terra::extend(r_old, r_union)
  r_new_ext <- terra::extend(r_new, r_union)

  # Calculate difference
  r_diff <- round(r_new_ext - r_old_ext, 2)
  names(r_diff) <- "dif"

  # where NAs mismatch add 1 or -1
  r_diff <- terra::ifel(
    is.na(r_old_ext) & !is.na(r_new_ext),
    r_new_ext,
    r_diff)
  r_diff <- terra::ifel(
    is.na(r_new_ext) & !is.na(r_old_ext),
    r_old_ext,
    r_diff)

  # Convert 0 to NA
  r_diff <- terra::mask(r_diff, r_diff, maskvalue = 0)

  # Check if all values are within tolerance
  diff_values <- terra::values(r_diff, na.rm = TRUE)

  if (length(diff_values) == 0) {
    matches <- TRUE
  } else {
    matches <- all(abs(diff_values) <= tolerance)
  }

  if (return == "boolean")
    return(matches)

  if (return == "rast")
    return(r_diff)

  # For tibble return, we need species information
  if (is.null(sp_info)) {
    stopifnot(!is.null(sp_key) && !is.null(con_dd))
    sp_info <- get_sp_info(sp_key, verbose = verbose)
  }

  # Get environmental raster for the species
  r_env <- get_sp_env(sp_key, sp_info, verbose = verbose)

  names(r_diff) <- "dif"
  names(r_old)  <- "old"
  names(r_new)  <- "new"

  # Define environment layer names for extraction
  lyrs_env <- c(
    c_id      = "cell_id",        c_x        = "center_long",     c_y       = "center_lat",
    depth     = "depth_mean",     depth_min  = "depth_min",       depth_max = "depth_max",
    temp      = "sst_an_mean",    temp_b     = "sbt_an_mean",
    salinity  = "salinity_mean",  salinity_b = "salinity_b_mean",
    oxy       = "oxy_mean",       oxy_b      = "oxy_b_mean",
    prim_prod = "prim_prod_mean",
    ice_con   = "ice_con_ann",
    land_dist = "land_dist",
    fao_area  = "fao_area_m")

  r_env_s <- r_env |> terra::subset(lyrs_env)
  names(r_env_s) <- names(lyrs_env)

  # Combine all rasters for comparison
  r_cmp <- list(
    r_diff,
    r_old,
    r_new,
    r_env_s) |>
    terra::rast() |>
    terra::mask(r_diff)

  # Get species parameters for env variables
  vars_yes <- names(sp_info$env)

  # Get parameters for each environmental variable
  p_depth <- if ("depth" %in% vars_yes) get_sp_var_params(sp_key, "depth", sp_info) else NULL
  p_ice_con <- if ("ice_con" %in% vars_yes) get_sp_var_params(sp_key, "ice_con", sp_info) else NULL
  p_prim_prod <- if ("prim_prod" %in% vars_yes) get_sp_var_params(sp_key, "prim_prod", sp_info) else NULL
  p_salinity <- if ("salinity" %in% vars_yes) get_sp_var_params(sp_key, "salinity", sp_info) else NULL
  p_temp <- if ("temp" %in% vars_yes) get_sp_var_params(sp_key, "temp", sp_info) else NULL
  p_oxy <- if ("oxy" %in% vars_yes) get_sp_var_params(sp_key, "oxy", sp_info) else NULL
  p_land_dist <- if ("land_dist" %in% vars_yes) get_sp_var_params(sp_key, "land_dist", sp_info) else NULL

  # Extract values from rasters and create comparison dataframe
  d_cmp <- terra::values(r_cmp, na.rm = FALSE, dataframe = TRUE) |>
    tibble::tibble() |>
    dplyr::filter(!is.na(dif))

  # Add calculated fields
  d_cmp <- d_cmp |> dplyr::mutate(
    old     = round(old, 2),
    new     = round(new, 2),
    dif     = round(dif, 2),
    dif_abs = abs(dif))

  # Add depth transformations if depth is used
  if (!is.null(p_depth)) {
    d_cmp <- d_cmp |> dplyr::mutate(
      depth_tx     = ramp_env(depth, p_depth),
      depth_min_tx = ramp_env(depth_min, p_depth),
      depth_max_tx = ramp_env(depth_max, p_depth),
      depth_tx_max = pmax(depth_min_tx, depth_max_tx, na.rm = TRUE))
  }

  # Add ice_con transformation if used
  if (!is.null(p_ice_con)) {
    d_cmp <- d_cmp |> dplyr::mutate(
      ice_con_tx = ramp_env(ice_con, p_ice_con))
  }

  # Add prim_prod transformation if used
  if (!is.null(p_prim_prod)) {
    d_cmp <- d_cmp |> dplyr::mutate(
      prim_prod_tx = ramp_env(prim_prod, p_prim_prod))
  }

  # Add salinity transformation if used
  if (!is.null(p_salinity)) {
    d_cmp <- d_cmp |> dplyr::mutate(
      salinity_tx   = ramp_env(salinity, p_salinity),
      salinity_b_tx = ramp_env(salinity_b, p_salinity))
  }

  # Add temperature transformation if used
  if (!is.null(p_temp)) {
    d_cmp <- d_cmp |> dplyr::mutate(
      temp_tx   = ramp_env(temp, p_temp),
      temp_b_tx = ramp_env(temp_b, p_temp))
  }

  # Calculate product combinations to identify which matches original
  tx_vars <- c()
  if (!is.null(p_ice_con)) tx_vars <- c(tx_vars, "ice_con_tx")
  if (!is.null(p_prim_prod)) tx_vars <- c(tx_vars, "prim_prod_tx")
  if (!is.null(p_salinity)) tx_vars <- c(tx_vars, "salinity_tx") # or salinity_b_tx
  if (!is.null(p_temp)) tx_vars <- c(tx_vars, "temp_tx") # or temp_b_tx

  # Only add these calculations if we have the required transforms
  if (length(tx_vars) > 0) {
    # Base product without depth
    d_cmp <- d_cmp |> dplyr::mutate(
      prod_nodepth = round(Reduce(`*`, purrr::map(tx_vars, ~d_cmp[[.x]])), 2))

    # Add depth variants if depth is used
    if (!is.null(p_depth)) {
      d_cmp <- d_cmp |> dplyr::mutate(
        prod_depthmax  = round(depth_tx_max * prod_nodepth, 2),
        prod_depthmean = round(depth_tx * prod_nodepth, 2))

      # Check which calculation matches the original
      d_cmp <- d_cmp |> dplyr::mutate(
        prod_nodepth_eq_old   = prod_nodepth == old,
        prod_depthmax_eq_old  = prod_depthmax == old,
        prod_depthmean_eq_old = prod_depthmean == old)

      # Reorder columns for readability
      d_cmp <- d_cmp |> dplyr::relocate(
        dif_abs, dif, prod_nodepth_eq_old, prod_depthmax_eq_old, prod_depthmean_eq_old)
    } else {
      # Just check if no-depth matches original
      d_cmp <- d_cmp |> dplyr::mutate(
        prod_nodepth_eq_old = prod_nodepth == old)

      # Reorder columns for readability
      d_cmp <- d_cmp |> dplyr::relocate(dif_abs, dif, prod_nodepth_eq_old)
    }
  }

  # Order by absolute difference
  d_cmp <- d_cmp |> dplyr::arrange(dplyr::desc(dif_abs))

  return(d_cmp)
}

#' Compare species rasters side by side
#'
#' @param r_left Left raster
#' @param r_right Right raster
#' @param sp_key Species key
#' @param lbl_left Label for left panel
#' @param lbl_right Label for right panel
#' @param legend_title Title for legend
#' @param pal_col Color palette name
#' @param pal_rmp Color ramp function
#' @param pal_at Palette breakpoints
#' @param pal_alpha Opacity
#' @return Leaflet map with side-by-side comparison
compare_sp <- function(
  r_left,
  r_right,
  sp_key,
  lbl_left     = "native →",
  lbl_right    = "← replicated",
  legend_title = glue::glue("{sp_key}<br>AquaMaps<br>suitability"),
  pal_col      = "YlOrRd",
  pal_rmp      = colorRampPalette(RColorBrewer::brewer.pal(n=5, name=pal_col)),
  pal_at       = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  pal_alpha    = 0.9) {

  cols = RColorBrewer::brewer.pal(5, "YlOrRd")
  pal <- leaflet::colorBin(
    cols, c(0, 1),
    bins = length(cols), pretty = TRUE, na.color = "transparent")

  leaflet::leaflet(
    options = leaflet::leafletOptions(
      attributionControl = FALSE,
      zoomControl = FALSE)) |>
    leaflet::addMapPane("left",  zIndex = 1) |>
    leaflet::addMapPane("right", zIndex = 1) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.OceanBasemap,
      group = "base", layerId = "base_left",
      options = leaflet::pathOptions(pane = "left")) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.OceanBasemap,
      group = "base", layerId = "base_right",
      options = leaflet::pathOptions(pane = "right")) |>
    leaflet::addRasterImage(
      r_left, colors = pal, opacity = 0.8,
      options = leaflet::leafletOptions(pane = "left"),
      group = lbl_left) |>
    leaflet::addRasterImage(
      r_right, colors = pal, opacity = 0.8,
      options = leaflet::leafletOptions(pane = "right"),
      group = lbl_right) |>
    leaflet::addLegend(
      values    = seq(0, 1, length.out = 10),
      pal       = pal,
      title     = legend_title,
      position  = "bottomright",
      labFormat = function(x, type){
        b <- x[1:length(x)-1]*100
        e <- x[2:length(x)]*100
        glue::glue("{stringr::str_pad(b,2)} - {e}%")}) |>
    leaflet::addControl(lbl_left, position = "topleft") |>
    leaflet::addControl(lbl_right, position = "topright") |>
    leaflet.extras2::addSidebyside(
      layerId = "sidecontrols",
      rightId = "base_right",
      leftId  = "base_left")
}

#' Create diagnostic comparison table
#'
#' @param sp_keys Vector of species keys
#' @param con_dd DuckDB connection
#' @param verbose Whether to print progress messages
#' @return Diagnostic comparison table
create_diagnostic_table <- function(sp_keys, con_dd, verbose = FALSE) {
  results <- tibble::tibble(
    sp_key = character(),
    sp_class = character(),
    bbox_prefs_w = numeric(),
    bbox_prefs_e = numeric(),
    bbox_prefs_s = numeric(),
    bbox_prefs_n = numeric(),
    bbox_cells_w = numeric(),
    bbox_cells_e = numeric(),
    bbox_cells_s = numeric(),
    bbox_cells_n = numeric(),
    fao_prefs = character(),
    fao_cells = character(),
    use_fao = logical(),
    is_surface = logical(),
    is_bbox_hole = logical(),
    r_matches = logical()
  )

  # Get HCAF raster once for efficiency
  r_hcaf <- get_hcaf_raster(con_dd, verbose = verbose)

  for (sp_key in sp_keys) { # sp_key = "Fis-22747"
    if (verbose) {
      message(glue::glue("Processing {sp_key}"))
    }

    tryCatch({
      # Get species info
      sp_info <- get_sp_info(sp_key, verbose = verbose) # jsonedit(sp_info)

      # Get original and replicated rasters
      r_sp_old <- get_species_raster(sp_key, con_dd, r_hcaf, verbose = verbose)
      r_sp_new <- replicate_sp_raster(sp_key, con_dd, r_hcaf, verbose = verbose)

      # Compare rasters
      matches <- compare_rasters(r_sp_old, r_sp_new, verbose = verbose)

      # Add to results
      results <- dplyr::bind_rows(
        results,
        tibble::tibble(
          sp_key       = sp_key,
          sp_class     = sp_info$taxa$class,
          bbox_prefs_w = sp_info$bbox_prefs[["w_most_long"]],
          bbox_prefs_e = sp_info$bbox_prefs[["e_most_long"]],
          bbox_prefs_s = sp_info$bbox_prefs[["s_most_lat"]],
          bbox_prefs_n = sp_info$bbox_prefs[["n_most_lat"]],
          bbox_cells_w = sp_info$bbox_cells[["w_most_long"]],
          bbox_cells_e = sp_info$bbox_cells[["e_most_long"]],
          bbox_cells_s = sp_info$bbox_cells[["s_most_lat"]],
          bbox_cells_n = sp_info$bbox_cells[["n_most_lat"]],
          fao_prefs    = paste(sp_info$fao_prefs, collapse = ","),
          fao_cells    = paste(sp_info$fao_cells, collapse = ","),
          use_fao      = sp_info$use_fao,
          is_surface   = sp_info$is_surface,
          is_bbox_hole = sp_info$is_bbox_hole,
          r_matches    = matches
        )
      )
    }, error = function(e) {
      message(glue::glue("Error processing {sp_key}: {e$message}"))
      results <- dplyr::bind_rows(
        results,
        tibble::tibble(
          sp_key = sp_key,
          sp_class = NA_character_,
          bbox_prefs_w = NA_real_,
          bbox_prefs_e = NA_real_,
          bbox_prefs_s = NA_real_,
          bbox_prefs_n = NA_real_,
          bbox_cells_w = NA_real_,
          bbox_cells_e = NA_real_,
          bbox_cells_s = NA_real_,
          bbox_cells_n = NA_real_,
          fao_prefs = NA_character_,
          fao_cells = NA_character_,
          use_fao = NA,
          is_surface = NA,
          is_bbox_hole = NA,
          r_matches = NA
        )
      )
    })
  }

  return(results)
}

#' Create interactive diagnostic raster visualization
#'
#' @param sp_key Species key
#' @param con_dd DuckDB connection
#' @param r_hcaf Optional HCAF raster
#' @param show_diff Show difference raster in the middle pane
#' @param verbose Whether to print progress messages
#' @return Interactive leaflet map
create_diagnostic_visual <- function(sp_key, con_dd, r_hcaf = NULL, show_diff = TRUE, verbose = FALSE) {
  # Get species info
  sp_info <- get_sp_info(sp_key, verbose = verbose)

  # Get HCAF raster if needed
  if (is.null(r_hcaf))
    r_hcaf <- get_hcaf_raster(verbose = verbose)

  # Get original and replicated rasters
  r_sp_old <- get_species_raster(sp_key, con_dd, r_hcaf, verbose = verbose)
  r_sp_new <- replicate_sp_raster(sp_key, con_dd, r_hcaf, verbose = verbose)

  # Get difference raster if requested
  if (show_diff) {
    r_diff <- compare_rasters(r_sp_old, r_sp_new, return = "rast", verbose = verbose)
  }

  # Create color palette for probability
  pal_prob <- leaflet::colorNumeric(
    palette = RColorBrewer::brewer.pal(5, "YlOrRd"),
    domain = c(0, 1),
    na.color = "transparent")

  # Create color palette for difference
  pal_diff <- leaflet::colorNumeric(
    palette = RColorBrewer::brewer.pal(11, "RdBu"),
    domain = c(-1, 1),
    na.color = "transparent")

  # Initialize map
  m <- leaflet::leaflet(
    options = leaflet::leafletOptions(
      attributionControl = FALSE,
      zoomControl = TRUE))

  # Add base map
  m <- m |> leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap)

  # Add original probability raster
  m <- m |> leaflet::addRasterImage(
    r_sp_old,
    colors = pal_prob,
    opacity = 0.8,
    group = "Original")

  # Add replicated probability raster
  m <- m |> leaflet::addRasterImage(
    r_sp_new,
    colors = pal_prob,
    opacity = 0.8,
    group = "Replicated")

  # Add difference raster if requested
  if (show_diff) {
    m <- m |> leaflet::addRasterImage(
      r_diff,
      colors = pal_diff,
      opacity = 0.8,
      group = "Difference")
  }

  # Add layer controls
  m <- m |> leaflet::addLayersControl(
    baseGroups = if (show_diff) c("Original", "Difference", "Replicated") else c("Original", "Replicated"),
    options = leaflet::layersControlOptions(collapsed = FALSE))

  # Add legends
  m <- m |> leaflet::addLegend(
    position = "bottomright",
    pal = pal_prob,
    values = c(0, 1),
    title = glue::glue("{sp_info$sp_scientific}<br>({sp_key})<br>Probability"),
    opacity = 0.8,
    group = "prob_legend")

  if (show_diff) {
    m <- m |> leaflet::addLegend(
      position = "bottomleft",
      pal = pal_diff,
      values = c(-1, 1),
      title = "Difference<br>(Replicated - Original)",
      opacity = 0.8,
      group = "diff_legend")
  }

  # Add diagnostic info
  m <- m |> leaflet::addControl(
    html = glue::glue("
      <div style='background-color: white; padding: 8px; border-radius: 4px;'>
        <b>Species:</b> {sp_info$sp_scientific} ({sp_key})<br>
        <b>Class:</b> {sp_info$taxa$class}<br>
        <b>Pelagic:</b> {ifelse(sp_info$is_pelagic, 'Yes', 'No')}<br>
        <b>Surface:</b> {ifelse(sp_info$is_surface, 'Yes', 'No')}<br>
        <b>Use FAO:</b> {ifelse(sp_info$use_fao, 'Yes', 'No')}<br>
        <b>Bbox hole:</b> {ifelse(sp_info$is_bbox_hole, 'Yes', 'No')}<br>
      </div>
    "),
    position = "topright")

  return(m)
}

get_bo_raster <- function(
    bo_tif = NULL, verbose = F){

  # TODO: translate replicate_aquamaps.qmd to generation function like in get_hcaf_raster()

  if (is.null(bo_tif))
    bo_tif <- ifelse(
      Sys.info()[["sysname"]] == "Linux",
      "/share/data/raw/bio-oracle.org/bio-oracle_v3.tif",
      "~/My Drive/projects/msens/data/raw/bio-oracle.org/bio-oracle_v3.tif")

  stopifnot(file.exists(bo_tif))
  rast(bo_tif)
}

#' Generate cache paths for various file types
#'
#' @param type Type of cache file (e.g., "hcaf", "species", "replicated")
#' @param id Optional ID (e.g., species key)
#' @param dir_cache Directory for cached files
#' @return Path to the cache file
get_cache_path <- function(type, id = NULL, dir_cache = here::here("data/replicate_aquamaps")) {
  # Create cache directory if it doesn't exist
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)

  # Generate path based on type and optional id
  if (is.null(id)) {
    return(file.path(dir_cache, glue::glue("{type}.tif")))
  } else {
    # Create subdirectory for specific types if needed
    if (type %in% c("species", "replicated", "replicated_bio-oracle")) {
      subdir <- file.path(dir_cache, type)
      dir.create(subdir, showWarnings = FALSE, recursive = TRUE)
      return(file.path(subdir, glue::glue("{id}.tif")))
    } else {
      return(file.path(dir_cache, glue::glue("{type}_{id}.tif")))
    }
  }
}

#' Get the HCAF (Half-degree Cell Authority File) raster
#'
#' @param con_dd DuckDB connection
#' @param dir_cache Directory for cached files
#' @param verbose Whether to print progress messages
#' @return HCAF raster
get_hcaf_raster <- function(
    hcaf_tif = here::here("data/replicate_aquamaps/hcaf.tif"),
    verbose = FALSE) {

  if (!file.exists(hcaf_tif)) {
    if (verbose)
      message(glue::glue("Generating HCAF raster to {hcaf_tif}"))

    if (!exists("con_dd"))
      con_dd    <- dbConnect(
        duckdb(
          dbdir     = path_dd,
          read_only = T))

    # Create the global template raster
    xmin <- -180; xmax <- 180
    ymin <- -90; ymax <- 90
    res  <- 0.5  # Half-degree resolution

    r_g <- terra::rast(
      nrows = (ymax - ymin) / res,
      ncols = (xmax - xmin) / res,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      crs = "EPSG:4326")

    # data frame to points
    p <- dplyr::tbl(con_dd, "cells") |>
      dplyr::collect() |>
      sf::st_as_sf(
        coords = c("center_long", "center_lat"),
        remove = FALSE,
        crs = 4326) |>
      dplyr::arrange(center_long, center_lat)

    # points to raster
    get_r <- function(v) {
      r <- terra::rasterize(p, r_g, field = v)
      names(r) <- v
      r
    }

    r_hcaf <- NULL
    for (v in setdiff(names(p), "geometry")) {
      if (verbose) {
        message(glue::glue("v: {v}"))
      }

      if (is.null(r_hcaf)) {
        r_hcaf <- get_r(v)
      } else {
        r_hcaf <- terra::rast(list(r_hcaf, get_r(v)))
      }
    }

    # Save to cache
    terra::writeRaster(r_hcaf, hcaf_tif, overwrite=TRUE)
  } else {
    if (verbose) {
      message(glue::glue("Loading HCAF raster from cache: {dir_cache}/{basename(hcaf_tif)}"))
    }
  }

  return(terra::rast(hcaf_tif))
}

#' Get environmental layers for a species
#'
#' @param sp_key Species key
#' @param sp_info Species information from get_sp_info
#' @param env_layers Which environmental layers to use: "HCAF_v4" (default) or "BioOracle_v3"
#' @param verbose Whether to print progress messages
#' @return Environmental raster layers for the species
get_sp_env <- function(
    sp_key, sp_info = NULL,
    env_layers = "HCAF_v4",
    verbose = FALSE) {

  # env_layers = "BioOracle_v3"; sp_key = "Fis-22747"
  stopifnot(env_layers %in% c("HCAF_v4", "BioOracle_v3"))

  # r_bo <- get_bo_raster(verbose = verbose)
  if (verbose)
    message(glue("Getting environmental layers: {env_layers}"))
  r <- switch(
    env_layers,
    HCAF_v4      = get_hcaf_raster(verbose = verbose),
    BioOracle_v3 = get_bo_raster(verbose = verbose))

  if (is.null(sp_info)){
    if (verbose)
      message(glue("Getting species information: {sp_key}"))
    sp_info <- get_sp_info(sp_key, verbose = verbose)
  }

  # Apply bounding box
  bbox_ext <- sp_info$bbox_cells |>
    unlist() |>
    as.vector() |>
    terra::ext()
  if (verbose)
    message(glue("Cropping to bounding box: {bbox_ext}"))
  r <- terra::crop(r, bbox_ext)

  # Apply FAO areas if needed
  if (sp_info$use_fao && length(sp_info$fao_cells) > 0) {
    if (verbose)
      message(glue("Masking to FAO areas: {paste(sp_info$fao_cells, collapse = ',')}"))
    r_fao <- r[["fao_area_m"]] %in% sp_info$fao_cells
    r <- terra::mask(
      r,
      r_fao,
      maskvalue = FALSE)
  }

  cat_lyrs <- list(
    "depth"     = c(
      "depth_mean",
      "depth_min",
      "depth_max"),
    "temp"      = c(
      "sst_an_mean",
      "sbt_an_mean"),
    "salinity"  = c(
      "salinity_mean",
      "salinity_b_mean"),
    "oxy"       = c(
      "oxy_mean",
      "oxy_b_mean"), # TODO: oxygen at surface?
    "prim_prod" = "prim_prod_mean",
    "ice_con"   = "ice_con_ann")
    # "land_dist" = "land_dist")
  lyrs <- cat_lyrs |> unlist() |> unname()
  if (env_layers == "HCAF_v4"){
    lyrs_env <- c(
      "cell_id", "center_long", "center_lat", # TODO: add these layers to bo_tif (BioOracle_v3)
      lyrs,
      "fao_area_m", "land_dist")
  } else {
    lyrs_env <- c(lyrs, "fao_area_m")
  }
  r <- r |> subset(lyrs_env)

  # Process each set of env parameters for all applicable layers
  vars_yes <- setdiff(names(sp_info$env), "extn_rule")
  for (var in vars_yes) { # var = "depth"
    p <- get_sp_var_params(sp_key, var, sp_info, verbose = verbose)
    lyrs_var <- cat_lyrs[[var]]
    for (lyr in lyrs_var){
      lyr_tx <- glue("{lyr}_tx")
      r[[lyr_tx]] <- terra::app(
        x   = r[[lyr]],
        fun = ramp_env,
        p   = p)
    }
  }
  # names(r)

  # plet(r[["depth_minmax_tx"]], main = "depth_minmax_tx")
  r[["depth_minmax_tx"]] <- max(
    r[["depth_min_tx"]],
    r[["depth_max_tx"]], na.rm = TRUE)

  # pelagic true/false
  r[["depth_plmax_tf"]]  <- r[["depth_max"]] > p[3]
  r[["depth_plmean_tf"]] <- r[["depth_mean"]] > p[3]

  # non-Mammalia pelagic
  r[["depth_minmax_plmax_tx"]] <- ifel(
    r[["depth_plmax_tf"]],
    1,
    r[["depth_minmax_tx"]])
  # lyr="depth_minmax_plmax_tx"; plet(r[[lyr]], main=lyr)

  r[["depth_minmax_plmean_tx"]] <- ifel(
    r[["depth_plmean_tf"]],
    1,
    r[["depth_minmax_tx"]])

  # Mammalia pelagic
  r[["depth_mean_plmean_tx"]] <- ifel(
    r[["depth_plmean_tf"]],
    1,
    r[["depth_mean_tx"]])

  r[["depth_mean_plmax_tx"]] <- ifel(
    r[["depth_plmax_tf"]],
    1,
    r[["depth_mean_tx"]])

  return(r)
}

#' Get species information
#'
#' @param sp_key Species key
#' @param verbose Whether to print progress messages
#' @return List of species information
get_sp_info <- function(sp_key, verbose = FALSE) {
  if (verbose)
    message(glue::glue("Getting species info for {sp_key}"))

  if (!exists("con_dd"))
    con_dd    <- dbConnect(
      duckdb(
        dbdir     = path_dd,
        read_only = T))

  # Get species preferences and key attributes
  d_prefs <- dplyr::tbl(con_dd, "spp_prefs") |>
    dplyr::filter(sp_key == !!sp_key) |>
    dplyr::collect()

  vars_yes <- d_prefs |>
    dplyr::select(dplyr::ends_with("_yn")) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::filter(value == 1) |>
    dplyr::pull(name) |>
    stringr::str_replace("_yn$","")
  use_fao <- "extn_rule" %in% vars_yes
  vars_yes <- setdiff(vars_yes, "extn_rule")

  d_probs <- tibble::tribble(
    ~prob_name, ~prob_value,
    "min"     , 0,
    "pref_min", 1,
    "pref_max", 1,
    "max"     , 0)

  d_env <- d_prefs |>
    dplyr::select(dplyr::starts_with(vars_yes)) |>
    dplyr::select(!dplyr::ends_with("_yn")) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      values_to = "var_value") |>
    tidyr::separate_wider_regex(
      name,
      c(var       = paste(vars_yes, collapse = "|"),
        "_",
        prob_name = paste(d_probs$prob_name, collapse = "|"))) |>
    dplyr::left_join(
      d_probs,
      by = "prob_name")

  l_env <- d_env |>
    dplyr::group_by(var) |>
    dplyr::summarise(
      vec = list(var_value)) |>
    tibble::deframe()

  sp_info <- list()

  d_sp <- dplyr::tbl(con_dd, "spp") |>
    dplyr::filter(sp_key == !!sp_key) |>
    dplyr::collect() |>
    dplyr::mutate(
      sp_sci = glue::glue("{genus} {species}"))

  # bbox, original: from species preferences ----
  v_bbox_prefs <- d_prefs |>
    # 45	-28	-95	-33
    select(w_most_long, e_most_long, s_most_lat, n_most_lat) |>
    # mutate(
    #   w_most_long = w_most_long * -1,
    #   e_most_long = e_most_long * -1) |>
    pivot_longer(everything()) |>
    deframe() # |>
    # as.vector() |>
    # ext()

  # bbox, actual: from species preferences ----
  v_bbox_cells <- dplyr::tbl(con_dd, "spp_cells") |>
    dplyr::filter(sp_key == !!sp_key) |>
    dplyr::left_join(
      dplyr::tbl(con_dd, "cells"),
      by = "cell_id") |>
    dplyr::summarize(
      w_most_long = min(w_limit, na.rm = TRUE),
      e_most_long = max(e_limit, na.rm = TRUE),
      s_most_lat  = min(s_limit, na.rm = TRUE),
      n_most_lat  = max(n_limit, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::collect() |>
    pivot_longer(everything()) |>
    deframe()

  # FAO areas, original: from species preferences ----
  v_fao_prefs <- d_prefs$fao_areas |>
    stringr::str_split(",\\s*") %>%
    unlist() |>
    stringr::str_trim() |>
    as.numeric() |>
    sort()

  # FAO areas, actual: from species cells ----
  v_fao_cells <- dplyr::tbl(con_dd, "spp_cells") |>
    dplyr::filter(sp_key == !!sp_key) |>
    dplyr::left_join(
      dplyr::tbl(con_dd, "cells"),
      by = "cell_id") |>
    dplyr::group_by(fao_area_m) |>
    dplyr::summarize(.groups = "drop") |>
    dplyr::arrange(fao_area_m) |>
    dplyr::pull(fao_area_m)

  # Basic information
  sp_info[["sp_scientific"]] <- d_sp$sp_sci
  sp_info[["sp_key"]]        <- d_sp$sp_key
  sp_info[["sp_int"]]        <- d_sp$sp_int
  sp_info[["is_pelagic"]]    <- d_prefs$pelagic == 1

  # Layer information (surface vs bottom)
  sp_info[["is_surface"]]    <- d_prefs$layer == "s" # vs "b" bottom

  # Bounding box information
  sp_info[["bbox_prefs"]] <- v_bbox_prefs

  sp_info[["bbox_cells"]] <- v_bbox_cells

  sp_info[["is_bbox_hole"]] <- is_bbox_hole(sp_info$bbox_prefs)

  # FAO area information
  sp_info[["fao_prefs"]] <- v_fao_prefs
  sp_info[["fao_cells"]] <- v_fao_cells
  sp_info[["use_fao"]]   <- use_fao

  # Environmental preferences
  sp_info[["env"]] <- l_env

  # Taxonomic information
  sp_info[["taxa"]] <- d_sp |>
    dplyr::select(kingdom, phylum, class, order, family) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    tibble::deframe() |>
    as.list()

  # Species attributes
  sp_info[["attr"]] <- d_sp |>
    dplyr::select(
      deepwater, angling, diving,
      dangerous, m_invertebrates,
      highseas, invasive, resilience) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    tibble::deframe() |>
    as.list()

  # IUCN information
  sp_info$iucn <- d_sp |>
    dplyr::select(
      iucn_id, iucn_code, iucn_version, provider) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    tibble::deframe() |>
    as.list()

  return(sp_info)
}

#' Get species variable parameters
#'
#' @param sp_key Species key
#' @param var Environmental variable
#' @param sp_info Species information
#' @param con_dd DuckDB connection
#' @param verbose Whether to print progress messages
#' @return Vector of parameter values
get_sp_var_params <- function(sp_key, var, sp_info = NULL, con_dd = NULL, verbose = FALSE) {
  if (is.null(sp_info) && !is.null(con_dd)) {
    sp_info <- get_sp_info(sp_key, verbose = verbose)
  }

  stopifnot(!is.null(sp_info) || !is.null(con_dd))
  stopifnot(var %in% names(sp_info$env))

  # Get parameters
  p <- sp_info$env[[var]]

  # Check parameters
  stopifnot(
    length(p) == 4,
    all(is.finite(p)),
    all(diff(p) >= 0))

  return(p)
}

#' Get the original species probability raster
#'
#' @param sp_key Species key
#' @param con_dd DuckDB connection
#' @param r_hcaf HCAF raster
#' @param dir_cache Directory for cached files
#' @param verbose Whether to print progress messages
#' @return Species probability raster
get_species_raster <- function(sp_key, con_dd, r_hcaf = NULL, dir_cache = here::here("data/replicate_aquamaps"), verbose = FALSE) {
  # Get the original species probability raster
  cache_path <- get_cache_path("species", sp_key, dir_cache = dir_cache)

  if (file.exists(cache_path)) {
    if (verbose) {
      message(glue::glue("Loading species raster from cache: {dir_cache}/{basename(cache_path)}"))
    }
    return(terra::rast(cache_path))
  }

  if (verbose) {
    message(glue::glue("Generating species raster for {sp_key} and caching to {dir_cache}/{basename(cache_path)}"))
  }

  d_sp_cell <- dplyr::tbl(con_dd, "spp_cells") |>
    dplyr::filter(sp_key == !!sp_key) |>
    dplyr::collect()

  if (is.null(r_hcaf)) {
    r_hcaf <- get_hcaf_raster(con_dd, dir_cache = dir_cache, verbose = verbose)
  }

  r_sp <- with(
    d_sp_cell,
    terra::subst(  # substitute cell_id with probability
      r_hcaf[["cell_id"]],
      from   = cell_id,
      to     = probability,
      others = NA)) |>
    terra::trim()

  names(r_sp) <- "probability"

  # convert to integer percentage for smaller file size
  r_sp <- as.int(round(r_sp * 100)) %>%
    ifel(. == 0, NA, .) |>
    trim()

  writeRaster(
    r_sp, cache_path, datatype = "INT1U", filetype = "COG")

  return(r_sp)
}

#' Check if a bounding box has a hole (e.g., longitude wrapping issue)
#'
#' @param bbox A bounding box with w_most_long, e_most_long, s_most_lat, n_most_lat
#' @return TRUE if the bbox has a hole (e.g., E > W), FALSE otherwise
is_bbox_hole <- function(bbox) {
  if (is.list(bbox)) {
    return(bbox$e_most_long < bbox$w_most_long)
  } else if (is.data.frame(bbox)) {
    return(bbox$e_most_long < bbox$w_most_long)
  } else {
    return(FALSE)
  }
}

#' Ramp environmental variable based on species preference
#'
#' @param v Environmental variable value
#' @param p Preference vector (min, min_pref, max_pref, max)
#' @return Ramped probability value
ramp_env <- function(v, p) {
  stats::approx(
    x      = p,  # c(min, min_pref, max_pref, max)
    y      = c(0, 1, 1, 0),
    xout   = v,
    rule   = 2,  # return 0 if outside range
    method = "linear",
    ties   = max)$y
}

#' Replicate the species probability raster
#'
#' @param sp_key Species key
#' @param con_dd DuckDB connection
#' @param r_hcaf HCAF raster
#' @param dir_cache Directory for cached files
#' @param redo Force regeneration of raster
#' @param verbose Whether to print progress messages
#' @param env_layers Which environmental layers to use: "HCAF_v4" (default) or "BioOracle_v3"
#' @return Replicated species probability raster
replicate_sp_raster <- function(
    sp_key, con_dd,
    env_layers = "HCAF_v4",
    dir_cache = here::here("data/replicate_aquamaps"),
    redo = FALSE, verbose = FALSE) {

  # env_layers = "HCAF_v4" # env_layers = "BioOracle_v3" # verbose = T
  # sp_key = "Fis-22747"

  stopifnot(env_layers %in% c("HCAF_v4","BioOracle_v3"))

  # Replicate the species probability raster based on environmental preferences
  sp_tif <- get_cache_path(
    ifelse(env_layers == "BioOracle_v3", "replicated_bio-oracle", "replicated"),
    sp_key, dir_cache = dir_cache)

  if (!redo && file.exists(sp_tif)) {
    if (verbose)
      message(glue::glue("Loading species raster from cache: {basename(dirname(sp_tif))}/{basename(sp_tif)}"))
    return(terra::rast(sp_tif))
  }

  if (verbose)
    message(glue::glue("Replicating species raster for {sp_key} using {env_layers} and caching to {basename(dirname(sp_tif))}/{basename(sp_tif)}"))

  # Get species info
  sp_info <- get_sp_info(sp_key, verbose = verbose)

  # Get environment layers
  r_env <- get_sp_env(sp_key, sp_info, env_layers = env_layers, verbose = verbose)
  # names(r_env)

  # beg TODO ----
  sp_vars    <- names(sp_info$env)
  is_mammal  <- sp_info$taxa$class == "Mammalia"
  is_pelagic <- sp_info$is_pelagic
  is_surface <- sp_info$is_surface
  if (verbose)
    message(glue::glue("is_mammal: {is_mammal}, is_pelagic: {is_pelagic}, is_surface: {is_surface}"))

  eqn_vars <- list(
    depth       = case_when(
      is_mammal  ~ "depth_mean_tx",
      is_pelagic ~ "depth_minmax_plmax_tx",
      .default   = "depth_minmax_tx"),
    temp        = ifelse(
      is_surface,
      "sst_an_mean_tx",
      "sbt_an_mean_tx"),
    salinity  = ifelse(
      is_surface,
      "salinity_mean_tx",
      "salinity_b_mean_tx"),
    oxy       = ifelse(
      is_surface,
      "oxy_mean_tx",
      "oxy_b_mean_tx"),
    prim_prod = "prim_prod_mean_tx",
    ice_con   = "ice_con_ann_tx",
    land_dist = "land_dist_tx")
  eqn_vars <- eqn_vars[sp_vars] |> unlist() # |> unname()

  if (verbose)
    message(glue("equation: {paste(eqn_vars, collapse = ' * ')}"))

  # Multiply all layers to get final probability
  r_sp <- r_env |>
    subset(eqn_vars) |>
    app(fun = prod) |>
      round(2)

  # Mask zero values
  r_sp <- r_sp |>
    mask(r_sp, maskvalues = 0)

  # Cache the result
  writeRaster(r_sp, sp_tif, overwrite = TRUE)
  r_sp <- rast(r_sp)

  return(r_sp)
}

#' Validate AquaMaps species replication
#'
#' @param sp_keys Vector of species keys
#' @param con_dd DuckDB connection
#' @param dir_cache Directory for cached files
#' @param verbosity Level of verbosity: 0 (none), 1 (progress only), 2 (detailed)
#' @return Validation results tibble
validate_aquamaps_species <- function(
    sp_keys, con_dd,
    dir_cache = here::here("data/replicate_aquamaps"),
    redo = FALSE,
    verbosity = 0) {

  # Validate multiple species
  results <- tibble::tibble(
    sp_key    = character(),
    r_matches = logical() )

  # Get HCAF raster once
  verbose_hcaf <- verbosity >= 2
  r_hcaf <- get_hcaf_raster(con_dd, dir_cache = dir_cache, verbose = verbose_hcaf)

  for (i in 1:length(sp_keys)) {
    sp_key <- sp_keys[i]

    # Handle different verbosity levels
    if (verbosity >= 1) {
      logger::log_info("{sprintf('%04d', i)} of {length(sp_keys)}: {sp_key}...\n")
    }

    # Detailed verbosity for function calls
    verbose_funcs <- verbosity >= 2

    tryCatch({
      # Get original raster
      r_sp_old <- get_species_raster(sp_key, con_dd, r_hcaf, dir_cache = dir_cache, verbose = verbose_funcs)

      # Replicate raster
      r_sp_new <- replicate_sp_raster(sp_key, con_dd, r_hcaf, dir_cache = dir_cache, verbose = verbose_funcs, redo = redo)

      # Compare rasters
      matches <- compare_rasters(r_sp_old, r_sp_new, verbose = verbose_funcs)

      # Show match status for moderately verbose output
      if (verbosity >= 1) {
        message("  matches: ", matches)
      }

      # Add to results
      results <- dplyr::bind_rows(
        results,
        tibble::tibble(
          sp_key = sp_key,
          r_matches = matches
        )
      )
    }, error = function(e) {
      # Always show errors
      cat(glue::glue("Error processing {sp_key}: {e$message}\n"))

      results <- dplyr::bind_rows(
        results,
        tibble::tibble(
          sp_key = sp_key,
          r_matches = NA
        )
      )
    })
  }

  return(results)
}
