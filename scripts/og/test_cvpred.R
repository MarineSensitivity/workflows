# synthetic check of the patched obissdm modules: maxent / rf / xgboost return cvpred + cvpredtrain for the selected
# tune, indices align across methods, and the same seed reproduces the same predictions.
#   Rscript scripts/og/test_cvpred.R
suppressMessages(library(obissdm))
make <- function() {
  n_p <- 120; n_a <- 1200; n <- n_p + n_a
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  presence <- c(rep(1, n_p), rep(0, n_a))
  x1[presence == 1] <- x1[presence == 1] + 1.2
  d <- data.frame(presence = presence, x1 = x1, x2 = x2, x3 = x3)
  d <- d[sample(n), ]; rownames(d) <- NULL
  sd <- list(training = d, blocks = list(folds = list(spatial_grid = sample(1:5, n, TRUE))),
             eval_data = NULL, coord_training = data.frame(decimalLongitude = runif(n), decimalLatitude = runif(n)))
  class(sd) <- c("sdm_dat", "list"); sd
}
opts <- sdm_options()
opts$maxent$features <- c("lq", "h"); opts$maxent$remult <- 1:2
opts$xgboost$gamma <- c(0, 4); opts$xgboost$shrinkage <- c(0.1, 0.3); opts$xgboost$scale_pos_weight <- c("balanced", "equal")
opts$xgboost$rounds <- c(10, 50); opts$rf$n_trees <- 100
run <- function(seed) {
  set.seed(seed); sd <- make(); set.seed(seed + 1)
  list(sd = sd,
       maxent  = sdm_module_maxent(sd,  opts$maxent,  verbose = FALSE),
       rf      = sdm_module_rf(sd,      opts$rf,      verbose = FALSE),
       xgboost = sdm_module_xgboost(sd, opts$xgboost, verbose = FALSE))
}
a <- run(1); b <- run(1)
n <- nrow(a$sd$training); folds <- a$sd$blocks$folds$spatial_grid
for (m in c("maxent", "rf", "xgboost")) {
  fit <- a[[m]]
  stopifnot(!is.null(fit$cvpred), !is.null(fit$cvpredtrain),
            identical(names(fit$cvpred), c("fold", "idx", "presence", "pred")),
            nrow(fit$cvpred) == n,                                         # every row held out exactly once
            !anyDuplicated(fit$cvpred$idx),
            all(fit$cvpred$presence == a$sd$training$presence[fit$cvpred$idx]),
            all(fit$cvpred$fold == folds[fit$cvpred$idx]),
            nrow(fit$cvpredtrain) == n * 4,                                # each fold-model: all rows outside its fold
            all(fit$cvpredtrain$fold != folds[fit$cvpredtrain$idx]),
            all(fit$cvpred$pred >= 0 & fit$cvpred$pred <= 1),
            !any(c("cvpred", "cvpredtrain") %in% names(attributes(fit$cv_metrics))))   # not leaked into cvmetrics
  # held-out CBI recomputed from the stored predictions equals the stored cv metrics (selected tune), per fold
  f1 <- fit$cvpred[fit$cvpred$fold == 1, ]
  cbi <- unname(obissdm::eval_metrics(f1$presence, f1$pred)["cbi"])
  stopifnot(isTRUE(all.equal(cbi, fit$cv_metrics$cbi[1])))
  cat(sprintf("%-8s ok  test rows %d  train rows %d  fold-1 cbi %.3f\n", m, nrow(fit$cvpred), nrow(fit$cvpredtrain), cbi))
  stopifnot(isTRUE(all.equal(fit$cvpred$pred, b[[m]]$cvpred$pred)))      # same seed -> same held-out predictions
}
cat("same-seed reproducibility: identical cvpred for maxent, rf, xgboost\n")
options(obissdm.cvpred = FALSE); sd <- make()
stopifnot(is.null(sdm_module_xgboost(sd, opts$xgboost, verbose = FALSE)$cvpred)); cat("switch-off ok\n")
