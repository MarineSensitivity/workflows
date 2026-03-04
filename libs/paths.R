# shared version and path configuration ----
# sourced by all workflow notebooks and apps

ver <- "v3" # version string; change on branches (e.g., "v3b")

is_server <- Sys.info()[["sysname"]] == "Linux"
dir_data <- ifelse(is_server, "/share/data", "~/My Drive/projects/msens/data")
dir_derived <- glue::glue("{dir_data}/derived")
dir_raw <- glue::glue("{dir_data}/raw")
dir_private <- ifelse(is_server, "/share/private", "~/My Drive/private")
dir_big <- ifelse(is_server, "/share/data/big", "~/_big/msens/derived")

# versioned directories
dir_v <- glue::glue("{dir_derived}/{ver}")
dir_big_v <- ifelse(is_server, dir_v, glue::glue("{dir_big}/{ver}"))

# databases
spp_db <- glue::glue("{dir_big}/spp.duckdb")
sdm_db <- glue::glue("{dir_big_v}/sdm.duckdb")

# zone table names
tbl_er <- glue::glue("ply_ecoregions_2025")
tbl_pra <- glue::glue("ply_programareas_2026_{ver}")
tbl_sr <- glue::glue("ply_subregions_2026_{ver}")

# version suffix for backward compatibility (e.g., "_v3")
v_sfx <- glue::glue("_{ver}")

# server type detection (MSENS_ENV=prod set in prod docker-compose.yml)
is_prod <- Sys.getenv("MSENS_ENV") == "prod"

# pmtiles
dir_pmtiles <- glue::glue("{dir_big_v}/pmtiles")
pmtiles_base_url <- ifelse(
  is_prod,
  "/pmtiles", # prod: relative path on same host
  "https://file.marinesensitivity.org/pmtiles"
) # laptop + dev: file subdomain
