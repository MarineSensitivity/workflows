#!/usr/bin/env Rscript
# compare a control run (model=ogc) with OBIS's published run (model=mpaeu) for one species:
#   Rscript scripts/og/compare_control.R <AphiaID>          # env OG_OUT (default ~/_big/sdm/og), OG_OBIS (~/_big/sdm/obis/species)
# per algorithm + ensemble: mean CV CBI (target |diff| <= 0.05), thresholds p10/mtp/max_kappa on the 0-100 scale (target:
# a few units), cell-wise Pearson r of the scen=current COG band 1 (target >= 0.95 for the ensemble). cvpred check: the
# ogc cvpred parquet reproduces the ogc cvmetrics CBI. Writes <OG_OUT>/taxonid=<id>/model=ogc/control_compare.csv.
suppressMessages({library(arrow); library(terra); library(dplyr)})
id   <- commandArgs(TRUE)[1]; stopifnot(!is.na(id))
home <- path.expand("~")
out  <- Sys.getenv("OG_OUT",  file.path(home, "_big/sdm/og"))
obis <- Sys.getenv("OG_OBIS", file.path(home, "_big/sdm/obis/species"))
acro_c <- Sys.getenv("OG_ACRO", "ogc")
dc <- file.path(out,  paste0("taxonid=", id), paste0("model=", acro_c))
dp <- file.path(obis, paste0("taxonid=", id), "model=mpaeu")
stopifnot("control run not found" = dir.exists(dc), "published run not found" = dir.exists(dp))

f_of <- function(d, acro, sub, tail) list.files(file.path(d, sub), pattern = paste0("^taxonid=", id, "_model=", acro, tail, "$"), full.names = TRUE)
algos <- c("maxent", "rf_classification_ds", "xgboost", "ensemble")
short <- c(maxent = "maxent", rf_classification_ds = "rf", xgboost = "xgboost", ensemble = "ensemble")

cvmean <- function(d, acro, m, col = "cbi") {
  f <- f_of(d, acro, "metrics", paste0("_method=", m, "_what=cvmetrics\\.parquet"))
  if (!length(f)) return(NA_real_)
  mean(read_parquet(f[1])[[col]], na.rm = TRUE)
}
thr <- function(d, acro) {
  f <- f_of(d, acro, "metrics", "_what=thresholds\\.parquet")
  if (!length(f)) return(NULL)
  read_parquet(f[1]) |> as.data.frame()
}
tc <- thr(dc, acro_c); tp <- thr(dp, "mpaeu")
cog1 <- function(d, acro, m) {
  f <- f_of(d, acro, "predictions", paste0("_method=", m, "_scen=current_cog\\.tif"))
  if (!length(f)) NULL else rast(f[1])[[1]]
}
pearson <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA_real_)
  if (!compareGeom(a, b, stopOnError = FALSE)) b <- resample(b, a)
  v <- na.omit(cbind(values(a)[, 1], values(b)[, 1]))     # cells valid in both (the models carry their own masks)
  if (nrow(v) < 100) NA_real_ else cor(v[, 1], v[, 2])
}
res <- do.call(rbind, lapply(algos, function(m) {
  sm <- short[[m]]
  t_c <- if (!is.null(tc)) tc[tc$model == sm, ] else NULL
  t_p <- if (!is.null(tp)) tp[tp$model == sm, ] else NULL
  g <- function(t, col) if (is.null(t) || !nrow(t)) NA_real_ else 100 * t[[col]][1]
  data.frame(algorithm = sm,
    cbi_ogc = cvmean(dc, acro_c, m), cbi_obis = cvmean(dp, "mpaeu", m),
    p10_ogc = g(t_c, "p10"), p10_obis = g(t_p, "p10"), mtp_ogc = g(t_c, "mtp"), mtp_obis = g(t_p, "mtp"),
    kappa_ogc = g(t_c, "max_kappa"), kappa_obis = g(t_p, "max_kappa"),
    cell_r = pearson(cog1(dc, acro_c, m), cog1(dp, "mpaeu", m)))
})) |> mutate(cbi_diff = cbi_ogc - cbi_obis, p10_diff = p10_ogc - p10_obis, mtp_diff = mtp_ogc - mtp_obis,
              pass_cbi = abs(cbi_diff) <= 0.05, pass_r = cell_r >= ifelse(algorithm == "ensemble", 0.95, 0.9))

# the saved held-out predictions reproduce the saved CV CBI (checks the cvpred files themselves)
cvp <- do.call(rbind, lapply(algos[1:3], function(m) {
  f <- f_of(dc, acro_c, "metrics", paste0("_method=", m, "_what=cvpred\\.parquet"))
  if (!length(f)) return(NULL)
  d <- read_parquet(f[1]); mt <- read_parquet(f_of(dc, acro_c, "metrics", paste0("_method=", m, "_what=cvmetrics\\.parquet"))[1])
  cb <- sapply(sort(unique(d$fold)), function(k) { x <- d[d$fold == k, ]; unname(obissdm::eval_metrics(x$presence, x$pred)["cbi"]) })
  data.frame(algorithm = short[[m]], cvpred_rows = nrow(d), cbi_from_cvpred = mean(cb), cbi_cvmetrics = mean(mt$cbi, na.rm = TRUE))
}))
if (!is.null(cvp)) res <- left_join(res, cvp, by = "algorithm")

write.csv(res, file.path(dc, "control_compare.csv"), row.names = FALSE)
p <- res |> transmute(algo = algorithm, cbi_c = round(cbi_ogc, 3), cbi_o = round(cbi_obis, 3), d_cbi = round(cbi_diff, 3),
                      p10_c = round(p10_ogc), p10_o = round(p10_obis), mtp_c = round(mtp_ogc), mtp_o = round(mtp_obis),
                      r = round(cell_r, 3), ok = ifelse(pass_cbi & pass_r, "PASS", "check"))
cat("species", id, ": control (", acro_c, ") vs OBIS published (mpaeu); thresholds on 0-100\n", sep = "")
print(p, row.names = FALSE)
if (!is.null(cvp)) cat("cvpred -> CBI reproduces cvmetrics: ", paste(sprintf("%s %.3f/%.3f", cvp$algorithm, cvp$cbi_from_cvpred, cvp$cbi_cvmetrics), collapse = "; "), "\n")
