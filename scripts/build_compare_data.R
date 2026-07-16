# build_compare_data.R — inject the AquaMaps + GoMex tile-URL data into _output/compare.html.
# Matches gm annual COGs (gm_cog_registry.csv) to AquaMaps native COGs by scientific name, builds
# titiler /cog tile templates for both, and replaces the __SPECIES_JSON__ marker.
suppressMessages({library(dplyr); library(readr); library(stringr); library(glue); library(jsonlite)
                  library(fs); library(readxl); library(msens)})
source(here::here("libs/paths.R"))

gm_reg <- read_csv(glue("{dir_big_v}/marine-atlas/native/gm_cog_registry.csv"), show_col_types = FALSE)

# common names from spp_gmx (strip Oceanic/Shelf qualifiers)
xls   <- Sys.getenv("GM_SHP_DIR"); xls <- glue("{xls}/../spp_gmx.xlsx")
common <- read_excel(xls) |>
  transmute(scientific_name = taxa_sci,
            common = str_squish(str_remove(taxa_common, "^(Oceanic|Shelf) "))) |>
  distinct(scientific_name, .keep_all = TRUE)

# AquaMaps native (0.5°) COGs for the same species
con <- attach_atlas(anon = TRUE)
am <- tbl(con, "native_asset") |>
  filter(ds_key == "am", representation == "native", asset_type == "cog") |>
  inner_join(tbl(con, "model") |> filter(ds_key == "am") |> select(mdl_key, sci_name), by = "mdl_key") |>
  select(scientific_name = sci_name, am_url = asset_url) |> collect()
DBI::dbDisconnect(con, shutdown = TRUE)

tile <- function(u) vapply(u, \(x) cog_tile_url(x, colormap = "spectral_r", rescale = c(1, 100)),
                           character(1), USE.NAMES = FALSE)   # cog_tile_url is scalar-only
d <- gm_reg |>
  inner_join(am, by = "scientific_name") |>
  left_join(common, by = "scientific_name") |>
  mutate(common = coalesce(common, scientific_name)) |>
  transmute(sci = scientific_name, common = common, am = tile(am_url), gm = tile(asset_url)) |>
  arrange(common)

message(glue("compare species (both am + gm): {nrow(d)}"))
json <- toJSON(d, auto_unbox = TRUE)

page <- here::here("_output/compare.html")
html <- readLines(page, warn = FALSE)
html <- gsub("__SPECIES_JSON__", json, html, fixed = TRUE)
writeLines(html, page)
cat(glue("injected {nrow(d)} species into {page}\n"))
