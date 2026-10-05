# threshold sweep on the ensemble median: cells >= t in Alaska / Gulf / US-Atlantic boxes, + where Caretta's Alaska p10 cells sit
suppressMessages({library(terra); library(dplyr)}); options(width=250)
setwd("/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/1ea7da58-8567-445f-a474-5a832fb51319/scratchpad/obis_out")
sp <- c("137205"="Cc","137206"="Cm","137209"="Dc","137207"="Ei","137208"="Lk","220293"="Lo","159023"="Eg")
res <- list()
for (id in names(sp)) {
  e <- rast(sprintf("data/%s/taxonid=%s_model=mpaeu_method=ensemble_scen=current_cog.tif", id, id))
  v <- values(e[[1]], mat = FALSE); xy <- xyFromCell(e, 1:ncell(e))
  ak <- xy[,2] >= 50 & xy[,2] <= 75 & (xy[,1] <= -130 | xy[,1] >= 170); gu <- xy[,2] >= 18 & xy[,2] <= 31 & xy[,1] >= -98 & xy[,1] <= -81
  at <- xy[,2] >= 24 & xy[,2] <= 45 & xy[,1] >= -82 & xy[,1] <= -65
  for (t in c(50, 60, 70, 75, 80, 85, 90)) res[[paste(id, t)]] <- data.frame(sp = sp[id], t = t, ALASKA = sum(ak & v >= t, na.rm = TRUE), GULF = sum(gu & v >= t, na.rm = TRUE), ATL = sum(at & v >= t, na.rm = TRUE))
  if (id == "137205") { s <- ak & !is.na(v) & v >= 53; cat("Caretta Alaska p10 cells: lat q", quantile(xy[s,2], c(.05,.5,.95)), " lon (0-360) q", quantile((xy[s,1] + 360) %% 360, c(.05,.5,.95)), "\n") }
}
print(bind_rows(res))
