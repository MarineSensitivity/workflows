#!/usr/bin/env Rscript
# the pipeline's storr is a resume ledger: a species it calls finished is SKIPPED, and the run still exits 0.
# run_one.sh calls this around a fit so that the exit-code file of a run means "every species has a model":
#   ledger.R pre    a terminal status without its manifest.json is stale (the outputs were removed): clear it
#   ledger.R post   print status + manifest per species; stop unless every species succeeded and has a manifest
# env as run_og.R: OG_SPECIES, OG_ACRO, OG_OUT.
when <- commandArgs(trailingOnly = TRUE)[1]
stopifnot("usage: ledger.R pre|post" = when %in% c("pre", "post"))

ids   <- strsplit(Sys.getenv("OG_SPECIES"), ",")[[1]]
acro  <- Sys.getenv("OG_ACRO", "og")
out   <- Sys.getenv("OG_OUT", file.path(path.expand("~"), "_big/sdm/og"))
stopifnot("set OG_SPECIES" = length(ids) > 0)

f_man  <- file.path(out, paste0("taxonid=", ids), paste0("model=", acro), paste0("taxonid=", ids, "_model=", acro, "_what=manifest.json"))
d_st   <- file.path(out, paste0(acro, "_storr"))
st     <- if (dir.exists(d_st)) storr::storr_rds(d_st)
status <- vapply(ids, \(k) if (!is.null(st) && st$exists(k)) as.character(st$get(k)[[1]]) else NA_character_, "")
d      <- data.frame(species = ids, status = unname(status), manifest = file.exists(f_man))

if (when == "pre") {
  for (k in d$species[!is.na(d$status) & !d$manifest]) {
    cat("ledger:", k, "is", status[[k]], "in the storr but has no manifest; status cleared, it will be fitted\n")
    st$del(k)
  }
} else {
  print(d, row.names = FALSE)
  ok <- d$status %in% c("succeeded", "done") & d$manifest
  if (!all(ok)) stop("ledger: ", sum(!ok), " of ", nrow(d), " species without a model: ", paste(d$species[!ok], collapse = ", "))
}
