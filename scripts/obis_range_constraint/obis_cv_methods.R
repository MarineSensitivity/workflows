# per-method mean over the 5 CV folds (component methods: 5 unlabeled rows; ensemble: the 5 'mean/avg_fit' rows)
suppressMessages({library(arrow); library(dplyr)}); options(width=250)
setwd("/private/tmp/claude-501/-Users-bbest-Github-MarineSensitivity-workflows/1ea7da58-8567-445f-a474-5a832fb51319/scratchpad/obis_out")
sp <- c("137205"="Cc","137206"="Cm","137209"="Dc","137207"="Ei","137208"="Lk","220293"="Lo","159023"="Eg")
mets <- c("auc","cbi","tss_maxsss","sens_maxsss","spec_maxsss","tss_p10","sens_p10","spec_p10")
ms <- c(maxent="maxent", rf="rf_classification_ds", xgboost="xgboost", ensemble="ensemble")
out <- bind_rows(lapply(names(sp), function(id) bind_rows(lapply(names(ms), function(k) {
  x <- as.data.frame(read_parquet(sprintf("data/%s/taxonid=%s_model=mpaeu_method=%s_what=cvmetrics.parquet", id, id, ms[[k]])))
  if (k == "ensemble") x <- x[x$what == "mean" & x$origin == "avg_fit", ]
  data.frame(sp = sp[id], method = k, n_fold_rows = nrow(x), t(colMeans(x[, mets], na.rm = TRUE)))
}))))
print(out |> mutate(across(where(is.numeric), ~round(.x, 2))))
write.csv(out, "out_B_cv_methods.csv", row.names = FALSE)
f <- as.data.frame(read_parquet("data/159023/taxonid=159023_model=mpaeu_method=maxent_what=fullmetrics.parquet")); cat("fullmetrics cols:", names(f), "\n")
