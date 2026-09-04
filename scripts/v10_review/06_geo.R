# 06 - simplified, longitude-shifted (0-360) Program Area and study-area polygons for the page's maps
# (rmapshaper if installed, else st_simplify in Web Mercator). Shifted so the Aleutians do not wrap.
suppressMessages({library(sf); library(dplyr)})
S <- Sys.getenv("S"); repo <- Sys.getenv("MS_REPO", "."); sf_use_s2(FALSE)
source(file.path(repo, "libs/paths.R"))                     # ply_pra_gpkg
has_rms <- requireNamespace("rmapshaper", quietly = TRUE)
simp <- function(x, keep = 0.03, tol_m = 6000) {
  if (has_rms) rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE)
  else st_transform(x, 3857) |> st_simplify(dTolerance = tol_m, preserveTopology = TRUE) |> st_transform(4326)
}
nv <- function(x) sum(sapply(st_geometry(x), function(p) nrow(st_coordinates(p))))
g  <- st_read(path.expand(ply_pra_gpkg), quiet = TRUE) |> select(pa = programarea_key, name = programarea_name, region = region_key) |>
  st_transform(4326) |> st_make_valid()
gs <- simp(g) |> st_make_valid() |> st_shift_longitude()
st_write(gs, file.path(S, "pa_shift.geojson"), driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE, layer_options = "COORDINATE_PRECISION=2")
u  <- st_read(file.path(repo, "data/boem-mst_usa-study-area.geojson"), quiet = TRUE) |> st_transform(4326) |> st_make_valid()
us <- simp(u, keep = 0.02, tol_m = 10000) |> st_make_valid() |> st_shift_longitude()
st_write(us |> select(any_of(c("region", "name"))), file.path(S, "usa_shift.geojson"), driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
         layer_options = "COORDINATE_PRECISION=2")
cat("rmapshaper:", has_rms, "| PA vertices", nv(g), "->", nv(gs), "| study area", nv(u), "->", nv(us), "\n")
