librarian::shelf(
  r-arcgis/arcgis, glue, mapview, sf,
  quiet = T)

# [11th National Draft Proposed Program Areas - Overview](https://boem.maps.arcgis.com/home/item.html?id=ed65f4351e494ff3a3c5589bb1960bae&sublayer=16)
pa_url  <- "https://services7.arcgis.com/G5Ma95RzqJRPKsWL/arcgis/rest/services/11th_National_Draft_Proposed_Program_Areas/FeatureServer/16"
dir_raw <- "~/My Drive/projects/msens/data/raw"
pa_gpkg <- glue("{dir_raw}/boem.gov/boem_program_areas_2025-11.gpkg")

# Open Feature Layer
pa_lyr <- arc_open(pa_url)

# Get all features into an R data frame (sf object)
sf_pa <- arc_select(pa_lyr)

# Write to GeoPackage
write_sf(sf_pa, pa_gpkg)

# Preview in mapview
mapView(sf_pa)
