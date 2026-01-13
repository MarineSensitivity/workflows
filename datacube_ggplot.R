# parsimonious functions for creating 3d datacube visualizations with ggplot2
# requires: ggplot2, dplyr, tidyr, purrr

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# create 3d transformation matrix for perspective effect
transform_3d <- function(x, y, z, angle_x = -0.5, angle_z = -0.3, scale = 1) {
  # simple 3d to 2d projection with rotation
  x_new <- x * cos(angle_z) - y * sin(angle_z)
  y_new <- (x * sin(angle_z) + y * cos(angle_z)) * cos(angle_x) + z * sin(angle_x)
  z_new <- z * cos(angle_x) - (x * sin(angle_z) + y * cos(angle_z)) * sin(angle_x)
  
  tibble(
    x_proj = x_new * scale,
    y_proj = y_new * scale - z_new * 0.5,  # add depth effect
    z_order = z_new  # for proper layering
  )
}

# create a single raster layer as polygon data
create_raster_layer <- function(matrix_data, z_level = 0, band_name = "band", 
                               band_color = "blue", time_point = 1) {
  nr <- nrow(matrix_data)
  nc <- ncol(matrix_data)
  
  # create grid of cells
  expand_grid(row = 1:nr, col = 1:nc) |>
    mutate(
      value = as.vector(matrix_data),
      # define corners of each cell
      x1 = col - 1, x2 = col,
      y1 = row - 1, y2 = row
    ) |>
    rowwise() |>
    mutate(
      # create polygon coordinates for each cell
      coords = list(tibble(
        x = c(x1, x2, x2, x1, x1),
        y = c(y1, y1, y2, y2, y1),
        z = z_level
      ))
    ) |>
    unnest(coords) |>
    mutate(
      band = band_name,
      color = band_color,
      time = time_point,
      cell_id = paste(row, col, z_level, sep = "_")
    ) |>
    select(-x1, -x2, -y1, -y2)
}

# create a datacube stack
create_datacube <- function(data_list, band_names, band_colors, 
                           z_spacing = 0.8, time_point = 1,
                           angle_x = -0.5, angle_z = -0.3) {
  
  # combine all layers
  map2_dfr(seq_along(data_list), seq_along(band_names), 
           ~create_raster_layer(
             data_list[[.x]], 
             z_level = .x * z_spacing,
             band_name = band_names[[.x]],
             band_color = band_colors[[.x]],
             time_point = time_point
           )) |>
    # apply 3d transformation
    group_by(cell_id) |>
    mutate(
      coords_3d = list(transform_3d(x, y, z, angle_x, angle_z))
    ) |>
    unnest(coords_3d) |>
    ungroup() |>
    arrange(desc(z_order))
}

# plot a single datacube
plot_datacube <- function(datacube_data, show_values = FALSE, 
                         alpha = 0.8, border_color = "black") {
  
  p <- ggplot(datacube_data) +
    geom_polygon(aes(x = x, y = y, group = cell_id, fill = color),
                alpha = alpha, color = border_color, size = 0.2) +
    scale_fill_identity() +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")
  
  if (show_values) {
    cell_centers <- datacube_data |>
      group_by(cell_id, value) |>
      summarise(
        x = mean(x, na.rm = TRUE),
        y = mean(y, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- p + 
      geom_text(data = cell_centers, 
                aes(x = x, y = y, label = round(value)),
                size = 2)
  }
  
  p
}

# create time series of datacubes
create_timeseries_datacubes <- function(data_lists, band_names, band_colors,
                                       time_labels, z_spacing = 0.8,
                                       x_offset = 15, angle_x = -0.5, 
                                       angle_z = -0.3) {
  
  map2_dfr(seq_along(data_lists), seq_along(time_labels),
           ~create_datacube(
             data_lists[[.x]], 
             band_names, 
             band_colors,
             z_spacing = z_spacing,
             time_point = .x,
             angle_x = angle_x,
             angle_z = angle_z
           ) |>
           mutate(
             x = x + (.x - 1) * x_offset,
             time_label = time_labels[[.x]]
           ))
}

# plot time series with labels
plot_timeseries <- function(timeseries_data, time_labels = NULL,
                           band_labels = NULL, show_axes = TRUE) {
  
  p <- ggplot(timeseries_data) +
    geom_polygon(aes(x = x, y = y, group = interaction(cell_id, time),
                    fill = color),
                alpha = 0.8, color = "black", size = 0.2) +
    scale_fill_identity() +
    coord_equal() +
    theme_void()
  
  # add time labels
  if (!is.null(time_labels)) {
    label_data <- timeseries_data |>
      group_by(time, time_label) |>
      summarise(
        x = mean(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE) + 1,
        .groups = "drop"
      )
    
    p <- p + 
      geom_text(data = label_data, 
                aes(x = x, y = y, label = time_label),
                size = 3.5)
  }
  
  # add band labels
  if (!is.null(band_labels)) {
    band_label_data <- timeseries_data |>
      filter(time == 1) |>
      group_by(band, color) |>
      summarise(
        x = min(x, na.rm = TRUE) - 2,
        y = mean(y, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- p + 
      geom_text(data = band_label_data,
                aes(x = x, y = y, label = band),
                size = 3, hjust = 1)
  }
  
  # add axes if requested
  if (show_axes) {
    p <- p +
      geom_segment(aes(x = min(x) - 3, xend = max(x) + 3,
                      y = min(y) - 1, yend = min(y) - 1),
                  arrow = arrow(length = unit(0.2, "cm"))) +
      annotate("text", x = max(timeseries_data$x) + 2, 
               y = min(timeseries_data$y) - 2,
               label = "time", size = 3.5)
  }
  
  p
}

# example usage function
example_datacube_viz <- function() {
  # create sample data (6x7 matrices like in your code)
  set.seed(1331)
  
  # create 3 time points with 4 bands each
  create_sample_matrix <- function(offset = 0) {
    matrix(runif(42, 40 + offset, 50 + offset), ncol = 7)
  }
  
  # time point 1
  b1 <- create_sample_matrix(0)
  g1 <- create_sample_matrix(10)
  r1 <- create_sample_matrix(20)
  n1 <- create_sample_matrix(30)
  
  # time point 2 (with some variation)
  b2 <- create_sample_matrix(-10)
  g2 <- create_sample_matrix(5)
  r2 <- create_sample_matrix(15)
  n2 <- create_sample_matrix(25)
  
  # time point 3
  b3 <- create_sample_matrix(-20)
  g3 <- create_sample_matrix(0)
  r3 <- create_sample_matrix(10)
  n3 <- create_sample_matrix(20)
  
  # define colors (matching your original)
  band_colors <- c("#4A90E2", "#7FBF7F", "#E85449", "#9B7AA1")  # blue, green, red, purple
  band_names <- c("blue", "green", "red", "nir")
  time_labels <- c("2020-10-01", "2020-10-13", "2020-10-25")
  
  # create data lists for each time point
  data_lists <- list(
    list(b1, g1, r1, n1),
    list(b2, g2, r2, n2),
    list(b3, g3, r3, n3)
  )
  
  # create and plot time series
  ts_data <- create_timeseries_datacubes(
    data_lists, 
    band_names, 
    band_colors,
    time_labels,
    z_spacing = 0.8,
    x_offset = 12,
    angle_x = -0.4,
    angle_z = -0.3
  )
  
  plot_timeseries(ts_data, time_labels, band_names, show_axes = TRUE)
}

# additional utility functions for specific effects

# create pyramid wireframe overlay
add_pyramid_lines <- function(p, datacube_data) {
  # extract corners for pyramid lines
  corners <- datacube_data |>
    group_by(band, time) |>
    summarise(
      x_min = min(x), x_max = max(x),
      y_min = min(y), y_max = max(y),
      .groups = "drop"
    ) |>
    filter(band == max(band))  # top layer only
  
  # calculate apex
  apex <- corners |>
    group_by(time) |>
    summarise(
      x_apex = mean(c(x_min, x_max)),
      y_apex = y_max + 3,
      .groups = "drop"
    )
  
  # add pyramid lines
  pyramid_lines <- corners |>
    left_join(apex, by = "time") |>
    pivot_longer(c(x_min, x_max), names_to = "corner", values_to = "x_corner") |>
    pivot_longer(c(y_min, y_max), names_to = "y_type", values_to = "y_corner")
  
  p + 
    geom_segment(data = pyramid_lines,
                aes(x = x_corner, y = y_corner, 
                    xend = x_apex, yend = y_apex),
                color = "black", size = 0.5)
}

# create aggregation mask
apply_spatial_mask <- function(datacube_data, mask_matrix) {
  # mask should be same dimensions as original matrices
  mask_df <- create_raster_layer(mask_matrix, z_level = 0) |>
    filter(!is.na(value)) |>
    select(row, col)
  
  datacube_data |>
    inner_join(mask_df, by = c("row", "col"))
}

# run the example
# example_datacube_viz()