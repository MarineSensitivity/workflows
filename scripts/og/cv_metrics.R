#!/usr/bin/env Rscript
# model quality of every og run (and of OBIS's published models), one row per species x acronym x algorithm:
# the pipeline's spatial-block CV metrics averaged over folds (origin "cv", what the acceptance gate reads: CBI
# >= 0.3) and the metrics on the independent evaluation records (origin "eval"); plus which algorithms the gate
# accepted and the background used. The comparison page (obis_range_compare.qmd) reads the csv, so it renders
# on any machine; this script needs the run folders.
#   Rscript scripts/og/cv_metrics.R          # prints the ensemble table, writes data/og/cv_metrics.csv
librarian::shelf(arrow, dplyr, jsonlite, purrr, readr, tidyr, quiet = TRUE)
home <- path.expand("~")
common <- c(`137205` = "loggerhead", `137206` = "green", `137207` = "hawksbill", `137208` = "kemps_ridley",
            `137209` = "leatherback", `220293` = "olive_ridley", `159023` = "right_whale")
f_log <- c(
  list.files(file.path(home, "_big/sdm/og"),           "_what=log\\.json$", recursive = TRUE, full.names = TRUE),
  list.files(file.path(home, "_big/sdm/obis/species"), "_what=log\\.json$", recursive = TRUE, full.names = TRUE))

one_run <- function(f) {
  j   <- read_json(f, simplifyVector = TRUE)
  dir <- dirname(f)
  id  <- as.character(unlist(j$taxonID)[1]); acro <- unlist(j$model_acro)[1]
  good <- unlist(j$model_good)
  man  <- file.path(dir, sprintf("taxonid=%s_model=%s_what=manifest.json", id, acro))
  bg   <- if (file.exists(man)) read_json(man, simplifyVector = TRUE)$background$type else "obis published"
  bg   <- sub(";.*", "", bg)
  f_cv <- list.files(file.path(dir, "metrics"), "_what=cvmetrics\\.parquet$", full.names = TRUE)
  # an algorithm whose fit failed writes no metrics file: one NA row so the table shows it
  failed <- names(j$model_result)[unlist(j$model_result) == "failed"]
  rows_failed <- if (length(failed)) tibble(taxon = id, common = unname(common[id]), acro, method = failed, origin = "cv",
    auc = NA_real_, cbi = NA_real_, tss_p10 = NA_real_, sens_p10 = NA_real_, spec_p10 = NA_real_, accepted = FALSE,
    n_fit = unlist(j$model_fit_points)[1], n_eval = unlist(j$model_eval_points)[1], background = bg) else NULL
  bind_rows(rows_failed, map_dfr(f_cv, \(x) {
    d <- as.data.frame(read_parquet(x))
    method <- sub("^.*_method=([a-z_]+)_what.*$", "\\1", basename(x))
    if (method == "ensemble") {
      # the ensemble file holds the per-algorithm averages: "mean"/"avg_fit" (CV) and "mean"/"avg_eval"
      d <- d |> filter(what == "mean", origin %in% c("avg_fit", "avg_eval")) |>
        mutate(origin = if_else(origin == "avg_fit", "cv", "eval")) |>
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .by = origin)
    } else {
      # an algorithm's file is its per-fold CV metrics; its eval metrics sit in log.json only through the ensemble
      d <- d |> summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) |> mutate(origin = "cv")
    }
    d |> transmute(taxon = id, common = unname(common[id]), acro, method = sub("_classification_ds$", "", method),
                   origin, auc, cbi, tss_p10, sens_p10, spec_p10,
                   accepted = method == "ensemble" | sub("_classification_ds$", "", method) %in% good,
                   n_fit = unlist(j$model_fit_points)[1], n_eval = unlist(j$model_eval_points)[1], background = bg)
  }))
}
d <- map_dfr(f_log, one_run) |> mutate(across(c(auc, cbi, tss_p10, sens_p10, spec_p10), ~ round(.x, 3)))
dir.create("data/og", showWarnings = FALSE)
write_csv(d, "data/og/cv_metrics.csv")
cat("ensemble, independent evaluation records (auc / cbi / tss at P10):\n")
print(as.data.frame(d |> filter(method == "ensemble", origin == "eval") |>
  mutate(v = sprintf("%.2f / %.2f / %.2f", auc, cbi, tss_p10)) |>
  select(common, acro, v) |> pivot_wider(names_from = acro, values_from = v) |> arrange(common)), row.names = FALSE)
