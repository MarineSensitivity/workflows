# originally run from within apps/mapgl/app.R after get_rast()
# for '~/My Drive/projects/msens/figures/overview-v2/mst-methods edit2.svg'
# outputs, incl QGIS, moved to here("data/goa-maps_for_methods-figure")

dir_gpkg <- "~/My Drive/projects/msens/data/derived/downloads"
er <- read_sf(glue("{dir_gpkg}/ply_ecoregions_2025.gpkg")) |>
  filter(region_key == "GA")
pa <- read_sf(glue("{dir_gpkg}/ply_planareas_2025.gpkg")) |>
  filter(region_key == "GA")
r_r <- rotate(r)
names(r_r) <- "extrisk_mammal"

r_rc <- crop(r_r, er) |> mask(er)
er_e <- er |>
  filter(ecoregion_key == "EGOA")
er_w <- er |>
  filter(ecoregion_key == "WCGOA")
r_rce <- crop(r_r, er_e) |> mask(er_e)
plot(r_rce)
writeRaster(r_rce, here(glue("mapgl/tmp_mammals_EGOA.tif")), overwrite = T)
(rng_rce <- range(values(r_rce), na.rm = T)) # 102.4 966.0
r_rcer <- r_rce
r_rcer[!r_rce %in% rng_rce] <- NA
plot(r_rcer)
writeRaster(r_rcw, here(glue("mapgl/tmp_mammals_WCGOA.tif")), overwrite = T)

r_rcw <- crop(r_r, er_w) |> mask(er_w)
plot(r_rcw)
plot(r_rcw, col = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(256))
writeRaster(r_rcw, here(glue("mapgl/tmp_mammals_WCGOA.tif")), overwrite = T)
(rng_rcw <- range(values(r_rcw), na.rm = T)) # 65.4 974.6
r_rcwr <- r_rcw
r_rcwr[!r_rcw %in% rng_rcw] <- NA
plot(r_rcwr)

plot(er[1])

writeRaster(rotate(r), here(glue("mapgl/tmp_{m_key}_{subregion_key}.tif")), overwrite = T)
