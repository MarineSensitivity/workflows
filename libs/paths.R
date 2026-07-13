# shared version and path configuration ----
# sourced by all workflow notebooks and apps

ver <- "v8" # version string; change on branches (e.g., "v3b")
ver_prev <- "v7" # previous version (for copying inputs to new version)

# v8 spatial sampling unit: GLOBAL 0.05 degree raster cell in [-180,180]
# (rolled back from H3 hex; grid aligned to Bio-Oracle v3 + AquaX topology)

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

# v8 release target: partitioned Parquet "marine atlas" on S3 (replaces the
# single-duckdb release). DuckDB stays the working engine and attaches these
# via httpfs (see msens::attach_atlas()).
s3_atlas <- "s3://oceanmetrics.io-public/marine-atlas"
dir_atlas_v <- glue::glue("{s3_atlas}/{ver}")

# Bio-Oracle v3 global environmental raster (0.05 deg, 12 layers) — the source
# of cell env covariates and the ocean mask for the global cell grid
bio_oracle_tif <- glue::glue("{dir_raw}/bio-oracle.org/bio-oracle_v3.tif")

# global 0.05 deg cell-id COG in [-180,180] (built by build_cell_grid.qmd);
# any SDM raster on this topology (Bio-Oracle, AquaX, resampled AquaMaps) maps to
# cell_id by position. Shared across versions.
cellid_tif <- glue::glue("{dir_derived}/r_cellid_global.tif")

# AquaMaps source db + precomputed bilinear weight table (HCAF 0.5 deg -> 0.05 deg
# cells; built once by ingest_aquamaps.qmd, shared across versions on this grid).
# The 3.7GB am.duckdb + weights live under dir_big (~/_big, fast local disk) on the
# laptop; prefer that copy when present, else the shared dir_derived location.
.am_dir <- { p <- glue::glue("{dir_big}/aquamaps")
             if (dir.exists(path.expand(p))) p else glue::glue("{dir_derived}/aquamaps") }
am_db  <- glue::glue("{.am_dir}/am.duckdb")
w05_db <- glue::glue("{.am_dir}/am_w05.duckdb")

# study-area (to-shore US EEZ) + Program-Area polygons for in_usa / in_pra membership
ply_usa_gpkg <- glue::glue("{dir_derived}/v1/ply_boem-usa.gpkg")            # shared
ply_pra_gpkg <- glue::glue("{dir_derived}/{ver_prev}/ply_programareas_2026_{ver_prev}.gpkg")

# zone table names
tbl_er <- glue::glue("ply_ecoregions_2025")
tbl_pra <- glue::glue("ply_programareas_2026_{ver}")
tbl_sr <- glue::glue("ply_subregions_2026_{ver}")

# pmtiles table names (unversioned — scores joined at render time)
tbl_er_pm <- "ply_ecoregions_2025"
tbl_pra_pm <- "ply_programareas_2026"

# version suffix for backward compatibility (e.g., "_v3")
v_sfx <- glue::glue("_{ver}")

# server type detection (MSENS_ENV=prod set in prod docker-compose.yml)
is_prod <- Sys.getenv("MSENS_ENV") == "prod"

# pmtiles (shared across versions — geometry only, scores joined at render time)
dir_pmtiles <- glue::glue("{dir_derived}/pmtiles")
pmtiles_base_url <- ifelse(
  is_prod,
  "/pmtiles", # prod: relative path on same host
  "https://file.marinesensitivity.org/pmtiles"
) # laptop + dev: file subdomain
