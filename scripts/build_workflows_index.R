#!/usr/bin/env Rscript
# build_workflows_index.R
#
# Generates _output/_data/workflows.yml, the data file that drives the
# MarineSensitivity workflows landing page (_output/index.html, rendered by
# Jekyll). One card per pipeline notebook carrying a `msens:` YAML front-matter
# block, grouped into pipeline-ordered categories by `workflow_type`
# (grid -> ingest -> merge -> score -> schema -> release), plus the full
# `targets` DAG rendered as Mermaid.
#
# Re-run whenever notebooks are added/removed/retitled or after a pipeline run
# (to refresh the per-card "last ran" times from data/manifests/*.json), then
# commit the result:
#   Rscript scripts/build_workflows_index.R
#
# Modeled on CalCOFI/workflows/scripts/build_workflows_index.R, adapted to the
# msens: metadata vocabulary + manifest timestamps + a targets DAG.

librarian::shelf(rmarkdown, yaml, jsonlite, quiet = TRUE)

wd <- getwd()
if (!dir.exists(file.path(wd, "_output")))
  stop("run from the workflows/ repo root (no ./_output found)")
out_dir  <- file.path(wd, "_output")
data_dir <- file.path(out_dir, "_data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a
oneline <- function(x) trimws(gsub("[[:space:]]+", " ", paste(x, collapse = " ")))

# category definitions, in pipeline (display) order ----
categories <- list(
  grid    = list(title = "Grid",
                 blurb = "Build the global 0.05° cell grid — the v8 sampling unit that every model is rasterized onto.",
                 layout = "cards", grouped = FALSE, color = "#845ef7"),
  ingest  = list(title = "Ingest",
                 blurb = "Acquire a source dataset and rasterize it onto the global cell grid as (mdl_key, cell_id, val). Each card's first chip is the data provider.",
                 layout = "cards", grouped = FALSE, color = "#4dabf7"),
  merge   = list(title = "Merge",
                 blurb = "Crosswalk each model to a taxon, combine per-taxon models per cell, and derive validity / range / rarity flags.",
                 layout = "cards", grouped = FALSE, color = "#20c997"),
  score   = list(title = "Score",
                 blurb = "Compute marine-sensitivity metrics per cell and per zone (program areas + ecoregions) over the US study area.",
                 layout = "cards", grouped = FALSE, color = "#fab005"),
  schema  = list(title = "Registry & schema",
                 blurb = "Consolidate the dataset + model registry and support tables the pipeline and apps depend on.",
                 layout = "cards", grouped = FALSE, color = "#e8590c"),
  release = list(title = "Release & publish",
                 blurb = "Freeze a versioned marine-atlas release to S3, build the view-DB + STAC, and publish native serving surfaces.",
                 layout = "cards", grouped = FALSE, color = "#f06595"))
cat_order <- names(categories)

# provider display labels + ordering within the ingest section ----
provider_label <- c(
  "AquaMaps" = "AquaMaps", "BirdLife" = "BirdLife International",
  "IUCN" = "IUCN Red List", "USFWS" = "USFWS", "NOAA NMFS" = "NOAA NMFS",
  "NOAA SEFSC" = "NOAA SEFSC", "NOAA NCCOS" = "NOAA NCCOS",
  "SWOT" = "SWOT + NMFS DPS", "WoRMS" = "WoRMS", "US federal" = "US federal")

# find the *.qmd source + its rendered html (if any) ----
qmds <- sort(list.files(wd, pattern = "[.]qmd$", full.names = FALSE))

recs <- lapply(qmds, function(q) {
  base <- sub("[.]qmd$", "", q)
  fm   <- tryCatch(rmarkdown::yaml_front_matter(file.path(wd, q)), error = function(e) list())
  m    <- fm$msens
  wt   <- m$workflow_type %||% ""
  if (is.null(m) || !nzchar(wt) || !wt %in% cat_order) return(NULL)   # only pipeline notebooks
  ds   <- m$dataset                                                   # ingests only
  # last-ran: the manifest `built` timestamp (falls back to qmd mtime)
  manf <- if (!is.null(m$output)) file.path(wd, m$output) else NA_character_
  built <- if (!is.na(manf) && file.exists(manf))
    tryCatch(jsonlite::fromJSON(manf)$built, error = function(e) NULL) else NULL
  html <- if (file.exists(file.path(out_dir, paste0(base, ".html")))) paste0(base, ".html") else ""

  list(
    base        = base,
    category    = wt,
    url         = html,
    qmd         = q,
    title       = oneline(fm$title %||% base),
    description = oneline(fm$subtitle %||% ""),
    target      = m$target_name %||% base,
    dependency  = if (is.null(m$dependency)) "" else oneline(paste(unlist(m$dependency), collapse = ", ")),
    ds_key          = ds$ds_key %||% "",
    source_authority= ds$source_authority %||% "",
    response_type   = ds$response_type %||% "",
    native_format   = ds$native_format %||% "",
    temporal        = ds$temporal_interval %||% "",
    last_ran        = oneline(built %||% ""))
})
recs <- Filter(Negate(is.null), recs)

emit_item <- function(r) {
  it <- list(url = r$url, qmd = r$qmd, title = r$title, target = r$target)
  for (k in c("description","dependency","ds_key","source_authority",
              "response_type","native_format","temporal","last_ran"))
    if (nzchar(r[[k]])) it[[k]] <- r[[k]]
  it
}

# assemble grouped, ordered structure ----
cats_out <- list()
for (cid in cat_order) {
  cdef  <- categories[[cid]]
  incat <- Filter(function(r) r$category == cid, recs)
  if (length(incat) == 0) next

  if (isTRUE(cdef$grouped)) {
    provs <- vapply(incat, function(r) if (nzchar(r$source_authority)) r$source_authority else "other", "")
    # providers with a label first (in map order), then any others alphabetically, "other" last
    ord   <- c(intersect(names(provider_label), provs),
               setdiff(sort(unique(provs)), c(names(provider_label), "other")),
               if ("other" %in% provs) "other")
    label_of <- function(p) {
      lab <- unname(provider_label[p])
      if (length(lab) == 0 || is.na(lab)) lab <- if (identical(p, "other")) "Other (reference)" else p
      lab
    }
    groups <- lapply(ord, function(p) {
      pin <- incat[provs == p]
      pin <- pin[order(vapply(pin, function(r) tolower(r$title), ""))]
      list(label = label_of(p), items = lapply(pin, emit_item))
    })
  } else {
    incat  <- incat[order(vapply(incat, function(r) tolower(r$title), ""))]
    groups <- list(list(label = "", items = lapply(incat, emit_item)))
  }

  cats_out[[length(cats_out) + 1]] <- list(
    id = cid, title = cdef$title, blurb = cdef$blurb, layout = cdef$layout,
    color = cdef$color, count = length(incat), groups = groups)
}

out_yaml <- file.path(data_dir, "workflows.yml")

# targets DAG as Mermaid (needs the targets/msens env). When that env is absent
# (e.g. CI installs only rmarkdown/yaml/jsonlite), preserve the DAG already
# committed in workflows.yml so a card-only refresh never drops it.
dag <- tryCatch({
  librarian::shelf(targets, quiet = TRUE)
  paste(targets::tar_mermaid(targets_only = TRUE, legend = FALSE), collapse = "\n")
}, error = function(e) {
  message("tar_mermaid unavailable (", conditionMessage(e), ") — preserving committed DAG")
  tryCatch(yaml::read_yaml(out_yaml)$dag_mermaid %||% "", error = function(e2) "")
})

doc <- list(
  generated  = format(Sys.time(), "%Y-%m-%d %H:%M"),
  n_total    = length(recs),
  categories = cats_out,
  dag_mermaid = dag)
writeLines(yaml::as.yaml(doc, indent = 2), out_yaml)
cat("wrote", out_yaml, "\n ", length(recs), "pipeline notebooks;",
    length(cats_out), "categories:",
    paste(vapply(cats_out, function(c) sprintf("%s(%d)", c$id, c$count), ""), collapse = " "),
    "\n  DAG:", if (nzchar(dag)) "ok" else "MISSING", "\n")
