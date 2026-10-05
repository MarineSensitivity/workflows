# obis_region_test.R -- MPA Europe SDM thresholds / masks / uncertainty vs regions
# inputs: ./data/{aphia}/*  (downloaded from s3://obis-maps/sdm/species/taxonid={id}/model=mpaeu/)
# outputs: ./out_*.csv and printed tables
suppressMessages({library(terra); library(arrow); library(jsonlite); library(dplyr); library(tidyr)})
options(width = 250, dplyr.width = Inf)
setwd("/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/1ea7da58-8567-445f-a474-5a832fb51319/scratchpad/obis_out")

sp <- c("137205"="Caretta caretta","137206"="Chelonia mydas","137209"="Dermochelys coriacea",
        "137207"="Eretmochelys imbricata","137208"="Lepidochelys kempii","220293"="Lepidochelys olivacea",
        "159023"="Eubalaena glacialis")
fp <- function(id, suffix) sprintf("data/%s/taxonid=%s_model=mpaeu_%s", id, id, suffix)

# regions as functions of lon/lat ----
in_region <- function(lon, lat) list(
  ALASKA = lat >= 50 & lat <= 75 & ((lon >= -180 & lon <= -130) | (lon >= 170 & lon <= 180)),
  GULF   = lat >= 18 & lat <= 31 & lon >= -98 & lon <= -81,
  ATL    = lat >= 24 & lat <= 45 & lon >= -82 & lon <= -65,
  GLOBAL = rep(TRUE, length(lon)))
reg_names <- c("ALASKA","GULF","ATL","GLOBAL")

# A. log.json ----
A <- lapply(names(sp), function(id) {
  j <- fromJSON(fp(id, "what=log.json"), simplifyVector = FALSE)
  g <- function(x) { x <- unlist(x); if (is.null(x)) NA else paste(x, collapse = ",") }
  tm <- unlist(lapply(j$timing, function(z) z$time_mins))  # cumulative minutes
  data.frame(id = id, species = sp[id], group = g(j$group), hab_depth = g(j$hab_depth),
    range_depth = g(j$range_depth), model_date = g(j$model_date), obissdm = g(j$obissdm_version),
    n_init = g(j$n_init_points), n_fit = g(j$model_fit_points), n_eval = g(j$model_eval_points),
    results = paste(names(j$model_result), sapply(j$model_result, g), sep = "=", collapse = ";"),
    vars = paste(unlist(j$model_bestparams$variables %||% j$variables), collapse = ","),
    runtime_min = if (length(tm)) max(tm) else NA)
}) |> bind_rows()
cat("\n== A. log.json\n"); print(A |> select(-vars)); cat("\nlog top-level names:\n"); print(names(fromJSON(fp("159023","what=log.json"), simplifyVector=FALSE)))
j <- fromJSON(fp("159023","what=log.json"), simplifyVector = FALSE)
cat("variables (right whale):", paste(unlist(j$variables), collapse=","), "\n")
for (id in names(sp)) { jj <- fromJSON(fp(id,"what=log.json"), simplifyVector=FALSE); cat(id, "vars:", paste(unlist(jj$variables), collapse=","), "| fit_n:", unlist(jj$model_fit_points), "\n") }
write.csv(A, "out_A_log.csv", row.names = FALSE)

# B. thresholds + cvmetrics ----
th <- lapply(names(sp), function(id) cbind(id = id, species = sp[id], as.data.frame(read_parquet(fp(id, "what=thresholds.parquet"))))) |> bind_rows()
cat("\n== B. thresholds (0-1 scale as stored)\n"); print(th |> mutate(across(where(is.numeric), ~round(.x, 3))))
write.csv(th, "out_B_thresholds.csv", row.names = FALSE)

cvm <- lapply(names(sp), function(id) lapply(c(ensemble="ensemble", maxent="maxent", rf="rf_classification_ds", xgboost="xgboost"), function(m) {
  x <- as.data.frame(read_parquet(fp(id, sprintf("method=%s_what=cvmetrics.parquet", m)))); cbind(id = id, species = sp[id], method = names(which(c(ensemble="ensemble", maxent="maxent", rf="rf_classification_ds", xgboost="xgboost") == m)), x) }) |> bind_rows()) |> bind_rows()
cat("\ncvmetrics columns:\n"); print(setdiff(names(cvm), c("id","species","method")))
cat("rows per file:\n"); print(table(cvm$what, cvm$origin)[, ])
mets <- c("auc","cbi","tss_maxsss","sens_maxsss","spec_maxsss","kap_maxsss","tss_p10","sens_p10","spec_p10","kap_p10","tss_mtp","spec_mtp")
cv_mean <- cvm |> filter(what == "mean", origin == "avg_fit") |> group_by(id, species, method) |> summarise(across(all_of(mets), ~mean(.x, na.rm = TRUE)), .groups = "drop")
cv_eval <- cvm |> filter(what == "mean", origin == "avg_eval") |> select(id, species, method, all_of(mets))
cat("\ncvmetrics mean over avg_fit rows (CV-train/fit), per method:\n"); print(cv_mean |> mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\ncvmetrics avg_eval (held-out):\n"); print(cv_eval |> mutate(across(where(is.numeric), ~round(.x, 2))))
write.csv(cv_mean, "out_B_cv_mean_avgfit.csv", row.names = FALSE); write.csv(cv_eval, "out_B_cv_avgeval.csv", row.names = FALSE)

# C. region test ----
rows_c <- list(); rows_mask <- list(); rows_unc <- list(); rows_cost <- list(); rows_D <- list(); rows_chk <- list(); rows_q <- list()
mask_bands <- c("native_ecoregions","fit_ecoregions","fit_region","fit_region_max_depth","convex_hull","minbounding_circle","buffer100m")
for (id in names(sp)) {
  e  <- rast(fp(id, "method=ensemble_scen=current_cog.tif"))
  bc <- rast(fp(id, "method=ensemble_scen=current_what=bootcv_cog.tif"))
  mk <- rast(fp(id, "what=mask_cog.tif"))
  stopifnot(all(dim(e)[1:2] == dim(mk)[1:2]), all(dim(e)[1:2] == dim(bc)[1:2]))
  v   <- values(e[[1]], mat = FALSE); sd1 <- values(e[[2]], mat = FALSE); bsd <- values(bc[[1]], mat = FALSE)
  M <- sapply(1:7, function(k) values(mk[[k]], mat = FALSE)); colnames(M) <- mask_bands
  xy <- xyFromCell(e, 1:ncell(e)); R <- in_region(xy[,1], xy[,2])
  t <- th |> filter(id == !!id, model == "ensemble")
  thr <- c(mtp = t$mtp, p10 = t$p10, maxsss = t$max_spec_sens) * 100
  # threshold sanity: ensemble value at fit points
  fo <- as.data.frame(read_parquet(fp(id, "what=fitocc.parquet")))
  fv <- terra::extract(e[[1]], as.matrix(fo[, c("decimalLongitude","decimalLatitude")]))[, 1]
  rows_chk[[id]] <- data.frame(id, species = sp[id], n_fit = nrow(fo), n_fit_valid = sum(!is.na(fv)),
    q10_value_at_fit = quantile(fv, .1, na.rm = TRUE), p10_x100 = thr["p10"], q05 = quantile(fv, .05, na.rm = TRUE), median = median(fv, na.rm = TRUE),
    frac_fit_ge_mtp = mean(fv >= thr["mtp"], na.rm = TRUE), frac_fit_ge_p10 = mean(fv >= thr["p10"], na.rm = TRUE), frac_fit_ge_maxsss = mean(fv >= thr["maxsss"], na.rm = TRUE))
  # D. fit occurrences per region
  fr <- in_region(fo$decimalLongitude, fo$decimalLatitude)
  rows_D[[id]] <- data.frame(id, species = sp[id], total = nrow(fo), ALASKA = sum(fr$ALASKA), GULF = sum(fr$GULF), ATL = sum(fr$ATL),
                             elsewhere = sum(!(fr$ALASKA | fr$GULF | fr$ATL)))
  valid <- !is.na(v)
  for (rg in reg_names) {
    ir <- R[[rg]] & valid
    rows_c[[paste(id, rg)]] <- data.frame(id, species = sp[id], region = rg, n_valid = sum(ir), n_gt0 = sum(v[ir] > 0), n_ge_mtp = sum(v[ir] >= thr["mtp"]),
      n_ge_p10 = sum(v[ir] >= thr["p10"]), n_ge_maxsss = sum(v[ir] >= thr["maxsss"]), max_val = if (any(ir)) max(v[ir]) else NA)
    # uncertainty inside vs outside >= p10 (within region valid cells)
    inn <- ir & v >= thr["p10"]; out <- ir & v < thr["p10"]
    rows_unc[[paste(id, rg)]] <- data.frame(id, species = sp[id], region = rg, n_in = sum(inn), n_out = sum(out),
      sd_ens_in = mean(sd1[inn], na.rm = TRUE), sd_ens_out = mean(sd1[out], na.rm = TRUE), bootsd_in = mean(bsd[inn], na.rm = TRUE), bootsd_out = mean(bsd[out], na.rm = TRUE),
      sd_ens_in_q90 = if (sum(inn)) quantile(sd1[inn], .9, na.rm = TRUE) else NA, mean_val_in = if (sum(inn)) mean(v[inn]) else NA)
    # mask shares: of all valid cells, and of >=p10 cells
    for (b in mask_bands) {
      rows_mask[[paste(id, rg, b)]] <- data.frame(id, species = sp[id], region = rg, band = b,
        share_valid = if (sum(ir)) mean(M[ir, b] == 1, na.rm = TRUE) else NA,
        n_p10_admitted = sum(inn & M[, b] == 1, na.rm = TRUE), n_p10 = sum(inn))
    }
  }
  # cost table: cells >= threshold, optionally x mask band, per region
  for (rg in c("ALASKA","GULF","ATL")) for (tn in names(thr)) {
    ir <- R[[rg]] & valid & v >= thr[tn]
    rows_cost[[paste(id, rg, tn)]] <- data.frame(id, species = sp[id], region = rg, thr = tn, none = sum(ir),
      t(sapply(mask_bands, function(b) sum(ir & M[, b] == 1, na.rm = TRUE))),
      sd_le_10 = sum(ir & sd1 <= 10, na.rm = TRUE), sd_le_5 = sum(ir & sd1 <= 5, na.rm = TRUE))
  }
  # distribution of ensemble sd among >=p10 cells per region (quantiles) and fraction of fit points admitted by each mask
  fm <- terra::extract(mk, as.matrix(fo[, c("decimalLongitude","decimalLatitude")]))[, -1]
  rows_q[[id]] <- data.frame(id, species = sp[id], t(colMeans(fm == 1, na.rm = TRUE)))
  cat("done", id, "\n")
}
C <- bind_rows(rows_c); U <- bind_rows(rows_unc); MK <- bind_rows(rows_mask); CO <- bind_rows(rows_cost); D <- bind_rows(rows_D); CK <- bind_rows(rows_chk); FQ <- bind_rows(rows_q)
for (nm in c("C","U","MK","CO","D","CK","FQ")) write.csv(get(nm), sprintf("out_%s.csv", nm), row.names = FALSE)
cat("\n== threshold sanity (ensemble value at fit points vs p10*100)\n"); print(CK |> mutate(across(where(is.numeric), ~round(.x, 2))), row.names = FALSE)
cat("\n== C. region counts\n"); print(C |> select(-id), row.names = FALSE)
cat("\n== C. uncertainty inside vs outside >= p10\n"); print(U |> select(-id) |> mutate(across(where(is.numeric), ~round(.x, 2))), row.names = FALSE)
cat("\n== C. mask share of ALL valid region cells (per band)\n"); print(MK |> select(species, region, band, share_valid) |> pivot_wider(names_from = band, values_from = share_valid) |> mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n== C. mask: n >=p10 cells admitted / n >=p10 cells\n"); print(MK |> filter(region != "GLOBAL") |> mutate(s = sprintf("%d/%d", n_p10_admitted, n_p10)) |> select(species, region, band, s) |> pivot_wider(names_from = band, values_from = s))
cat("\n== D. fit occurrences\n"); print(D |> select(-id), row.names = FALSE)
cat("\n== fit-point share admitted by each mask band\n"); print(FQ |> select(-id) |> mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n== cost table: cells >= threshold (x mask band admitted)\n"); print(CO |> select(-id))
