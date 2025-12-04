#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup

librarian::shelf(
  aws.s3,
  DBI,
  DT,
  dplyr,
  duckdb,
  glue,
  here,
  jsonlite,
  mapgl,
  mapview,
  purrr,
  readr,
  rgbif,
  sf,
  stringr,
  terra,
  tidyr,
  worrms,
  quiet = T
)

verbose <- T
is_server <- Sys.info()[["sysname"]] == "Linux"
dir_private <- ifelse(
  is_server,
  "/share/private",
  "~/My Drive/private"
)
dir_data <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data"
)
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
maptile_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_db <- glue("{dir_data}/derived/sdm.duckdb")
spp15_csv <- here("data/sdm_occ_spp15.csv")
er_gpkg <- glue("{dir_data}/derived/downloads/ply_ecoregions_2025.gpkg")

# db ----
con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)

# data prep ----
r_cell <- rast(cell_tif)

get_mdl_rast <- function(mdl_seq) {
  # mdl_seq = 3

  d <- tbl(con_sdm, "model_cell") |>
    filter(mdl_seq == !!mdl_seq) |>
    select(cell_id, value) |>
    collect()

  r <- init(r_cell[[1]], NA)
  r[d$cell_id] <- d$value
  names(r) <- "value"

  r
})

get_mdl_ecoregions <- function(mdl_seq) {
  # mdl_seq = 3
  tbl(con_sdm, "model_cell") |>
    filter(
      mdl_seq == !!mdl_seq,
      !is.na(value),
      value > 0) |>
    select(cell_id) |>
    left_join(
      tbl(con_sdm, "zone_cell") |> 
        select(cell_id, zone_seq),
      by = join_by(cell_id)) |> 
    left_join(
      tbl(con_sdm, "zone") |> 
        filter(tbl == "ply_ecoregions_2025") |> 
        select(zone_seq, er_key = value),
      by = join_by(zone_seq)) |> 
    filter(!is.na(er_key)) |>
    group_by(er_key) |>
    summarise(n_cell = n()) |>
    collect()
}
#
#
#
#| label: d_spp

if (!file.exists(spp15_csv)) {
  d_spp15 <- tribble(
    ~sp_sci                     , ~sp_cmn                      ,
    "Odobenus rosmarus"         , "Walrus"                     ,
    "Eubalaena glacialis"       , "North Atlantic Right Whale" ,
    "Megaptera novaeangliae"    , "Humpback Whale"             ,
    "Balaenoptera physalus"     , "Fin Whale"                  ,
    "Acipenser oxyrinchus"      , "Atlantic Sturgeon"          ,
    "Halichoerus grypus"        , "Grey Seal"                  ,
    "Gadus morhua"              , "Atlantic Cod"               ,
    "Delphinus delphis"         , "Common Dolphin"             ,
    "Homarus americanus"        , "Atlantic Lobster"           ,
    "Balaena mysticetus"        , "Bowhead Whale"              ,
    "Pterodroma hasitata"       , "Black-capped Petrel"        ,
    "Sterna hirundo"            , "Common Tern"                ,
    "Calanus finmarchicus"      , "Calanus finmarchicus"       ,
    "Meganyctiphanes norvegica" , "Meganyctiphanes norvegica"  ,
    "Balaenoptera ricei"        , "Rice's Whale"
  ) |>
    relocate(sp_cmn) |>
    arrange(sp_cmn) |>
    mutate(
      worms_id = map_int(sp_sci, \(x) {
        worrms::wm_name2id(x) |>
          as.integer()
      }),
      gbif_id = map_int(sp_sci, \(x) {
        rgbif::name_backbone(x) |>
          pull(usageKey) |>
          as.integer()
      }),
      mdl_seq = map_int(sp_sci, \(x) {
        tbl(con_sdm, "taxon") |>
          filter(is_ok) |>
          filter(scientific_name == x) |>
          pull(mdl_seq)
      })
    )
  write_csv(d_spp15, spp15_csv)
} else {
  d_spp15 <- read_csv(spp15_csv, show_col_types = F)
}

datatable(d_spp15)
#
#
#
#
#
#| label: ecoregions

ply_er <- read_sf(er_gpkg) |> 
  rename(er_key = ecoregion_key)

d_spp_er <- d_spp15 |>
  mutate(
    ecoregions = map(mdl_seq, get_mdl_ecoregions)) |>
  select(sp_cmn, ecoregions) |>
  unnest(ecoregions) |> 
  arrange(sp_cmn, er_key)
#
#
#
#
#
#| label: map

maplibre(
  style = maptiler_style("bright", variant = "dark"),
  zoom = 3.5,
  center = c(-106, 40.1)
) |>
  add_vector_source(
    id = "er_src",
    url = "https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025"
  ) |>
  add_vector_source(
    id = "pa_src",
    url = "https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025"
  ) |>
  add_line_layer(
    id = "pa_ln",
    source = "pa_src",
    source_layer = "public.ply_planareas_2025",
    line_color = "white",
    line_opacity = 1,
    line_width = 1
  ) |>
  add_line_layer(
    id = "er_ln",
    source = "er_src",
    source_layer = "public.ply_ecoregions_2025",
    line_color = "black",
    line_opacity = 1,
    line_width = 3,
    before_id = "pa_ln"
  ) |>
  add_fullscreen_control() |>
  add_navigation_control() |>
  add_scale_control() |>
  add_geocoder_control()
#
#
#
#| label: other
#| eval: false

# Read WKT from https://wktmap.com/?e6b28728
wkt <- fromJSON(
  "https://xpjpbiqaa3.execute-api.us-east-1.amazonaws.com/prod/wkt/e6b28728"
)$wkt %>%
  str_replace("<.*?>\\s", "")

# Set up duckdb connection and extensions
con <- dbConnect(duckdb())
dbSendQuery(con, "INSTALL httpfs; LOAD httpfs;")
dbSendQuery(con, "INSTALL spatial; LOAD spatial;")
dbSendQuery(con, "INSTALL h3 FROM community; LOAD h3;")


# Query
species <- dbGetQuery(
  con,
  glue(
    "
  select kingdom, phylum, class, family, genus, species, AphiaID
  from read_parquet('s3://obis-products/speciesgrids/h3_7/*')
  where ST_Intersects(geometry, ST_GeomFromText('{wkt}')) 
  group by kingdom, phylum, class, family, genus, species, AphiaID
"
  )
)

d_op <- get_bucket_df(
  's3://obis-products',
  max = Inf
)


d_sg <- get_bucket_df(
  's3://obis-products',
  prefix = 'speciesgrids/',
  max = Inf
)
which(d_sg$Key == "speedy/distribution_137077.geojson")

pq <- ""

column_info <- dbGetQuery(
  con,
  glue(
    "DESCRIBE SELECT * FROM read_parquet('{pq}');"
  )
)
print(column_info)

# get all cells for species with AphiaID 156263
aphia_id <- 137077 # _Odobenus rosmarus_ (walrus)
# Fix the query to use h3_cell_to_boundary_wkt instead of h3_cell_to_boundary_wkb
hex_sp_occ <- dbGetQuery(
  con,
  glue(
    "select *, h3_cell_to_boundary_wkt(cell) AS geometry_wkt 
    from read_parquet('s3://obis-products/speciesgrids/h3_7/*') 
    where AphiaID = {aphia_id}")) |> 
  mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) |>
  st_as_sf()

d_sp_er <- dbGetQuery(
  con,
  glue(
    "select *, h3_cell_to_boundary_wkt(cell) AS geometry_wkt 
    from read_parquet('{pq}') 
    where AphiaID = {aphia_id}"
  )
)

# speedy/distribution_137077.geojson

# Convert WKT to sf geometry and create the map
d_sp |>
  filter(
    source_obis == T,
    !is.na(min_year)
  ) |>
  mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) |>
  st_as_sf() |>
  mapview(zcol = "AphiaID")


# show interactive map of wkt
mapview(st_as_sfc(wkt, crs = 4326))

dbListTables(con)

#
#
#
#
#
#
#
#
#
