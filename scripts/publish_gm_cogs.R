# publish_gm_cogs.R — paint each ANNUAL gm|{sp_id} surface to a COG on the global 0.05° grid and
# upload to public S3 for titiler /cog. Supports the AquaMaps-vs-GoMex swipe compare page. A focused
# demo publish (the reproducible home is publish_native.qmd once gm is folded into the pipeline).
suppressMessages({library(dplyr); library(readr); library(fs); library(glue); library(arrow)
                  library(terra); library(logger); library(purrr); library(msens)})
source(here::here("libs/paths.R"))

ds_key   <- "gm"
dir_dist <- glue("{dir_big_v}/marine-atlas/dist/dataset={ds_key}")
mdl_csv  <- glue("{dir_big_v}/marine-atlas/dist/model_{ds_key}.csv")
dir_cog  <- glue("{dir_big_v}/marine-atlas/native/{ds_key}"); dir_create(dir_cog)
s3_dst   <- glue("{s3_atlas}/{ver}/native/{ds_key}")            # s3://…/marine-atlas/v8/native/gm
stopifnot(dir_exists(dir_dist), file_exists(mdl_csv), file_exists(cellid_tif))

grid    <- grid_spec(rast(cellid_tif))
annual  <- read_csv(mdl_csv, show_col_types = FALSE) |> filter(interval == "annual")
log_info("gm annual surfaces to publish: {nrow(annual)}")

reg <- purrr::pmap_dfr(annual, function(mdl_key, sp_id, scientific_name, ...) {
  safe <- gsub("[^A-Za-z0-9._-]", "_", sp_id)
  pq   <- glue("{dir_dist}/{ds_key}_{safe}.parquet")
  if (!file_exists(pq)) { log_warn("missing {pq}"); return(NULL) }
  d <- read_parquet(pq)
  f <- glue("{dir_cog}/{ds_key}_{safe}.tif")
  msens::publish_cog(d$cell_id, d$val, f, grid, datatype = "INT1U", nodata = 0)
  tibble(mdl_key = mdl_key, sp_id = sp_id, scientific_name = scientific_name,
         cog = path_file(f), asset_url = glue("https://s3.us-east-1.amazonaws.com/{sub('^s3://','',s3_dst)}/{path_file(f)}"))
})
write_csv(reg, glue("{dir_cog}/../gm_cog_registry.csv"))
log_info("wrote {nrow(reg)} gm COGs -> {dir_cog}")

# upload to public S3 (native/gm)
if (nzchar(Sys.getenv("GM_COG_S3", unset = "1"))) {
  system2("aws", c("s3", "sync", dir_cog, s3_dst, "--only-show-errors", "--no-progress"))
  log_info("synced {dir_cog} -> {s3_dst}")
}
cat(glue("DONE: {nrow(reg)} gm annual COGs published\n"))
