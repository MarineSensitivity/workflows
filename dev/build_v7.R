# build_v7.R — derive v7 from v6 by recomputing scores over the full valid (Set A) species set.
#
# Prereqs (done in shell before running this):
#   - v7 sdm.duckdb = copy of v6
#   - taxon.is_ok already recomputed in v7 (program-area gate dropped) -> 16,158
#
# This script recomputes the is_ok-dependent metrics by running the *canonical* scoring
# chunks from calc_scores.qmd verbatim (extracted by chunk label) against the v7 DB, then
# extends zone_taxon to ecoregions. primprod metrics are environment-only and unchanged, so
# they are re-aggregated (idempotent) but not recomputed from source.

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(dbplyr)
  library(glue); library(stringr); library(tidyr); library(readr)
  library(msens)
})

ver      <- "v7"
ver_prev <- "v6"
qmd_path <- "/Users/bbest/Github/MarineSensitivity/workflows/calc_scores.qmd"
dir_v    <- path.expand("~/_big/msens/derived/v7")
sdm_db   <- file.path(dir_v, "sdm.duckdb")
stopifnot(file.exists(sdm_db), file.exists(qmd_path))

# file outputs referenced by chunks
component_pct_pra_csv <- file.path(dir_v, glue("tbl_component_pct_programarea_{ver}.csv"))
zone_taxon_csv        <- file.path(dir_v, glue("zone_taxon_{ver}.csv"))

con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = FALSE)
on.exit({ dbDisconnect(con_sdm, shutdown = TRUE) }, add = TRUE)

# --- rename versioned zone tables _v6 -> _v7 (mirror calc_scores.qmd setup) ------------------
zone_tbls_db <- tbl(con_sdm, "zone") |> distinct(tbl) |> pull(tbl)
for (old_tbl in zone_tbls_db[str_detect(zone_tbls_db, glue("_{ver_prev}$"))]) {
  new_tbl <- str_replace(old_tbl, glue("_{ver_prev}$"), glue("_{ver}"))
  dbExecute(con_sdm, glue("UPDATE zone       SET tbl      = '{new_tbl}' WHERE tbl      = '{old_tbl}'"))
  dbExecute(con_sdm, glue("UPDATE zone_taxon SET zone_tbl = '{new_tbl}' WHERE zone_tbl = '{old_tbl}'"))
  message(glue("renamed zone tbl: {old_tbl} -> {new_tbl}"))
}

tbl_pra <- glue("ply_programareas_2026_{ver}")
tbl_sr  <- glue("ply_subregions_2026_{ver}")
tbl_er  <- "ply_ecoregions_2025"

# species categories from the (already widened) is_ok set
sp_cats <- tbl(con_sdm, "taxon") |> filter(is_ok) |> distinct(sp_cat) |>
  arrange(sp_cat) |> pull(sp_cat)
message(glue("sp_cats: {paste(sp_cats, collapse=', ')}  | is_ok = ",
             "{tbl(con_sdm,'taxon') |> filter(is_ok) |> tally() |> pull(n)}"))

# --- chunk extractor: pull verbatim code for a given chunk label from the qmd ----------------
qmd_lines <- readLines(qmd_path)
get_chunk <- function(label) {
  lab <- grep(paste0("^#\\|\\s*label:\\s*", label, "\\s*$"), qmd_lines)
  stopifnot(length(lab) == 1)
  open  <- max(grep("^```\\{", qmd_lines[seq_len(lab)]))
  close <- lab + grep("^```\\s*$", qmd_lines[(lab + 1):length(qmd_lines)])[1]
  code  <- qmd_lines[(open + 1):(close - 1)]
  paste(code[!grepl("^#\\|", code)], collapse = "\n")
}
run_chunk <- function(label, transform = identity) {
  message(glue("\n=== chunk: {label} ==="))
  code <- transform(get_chunk(label))
  eval(parse(text = code), envir = globalenv())
  invisible(TRUE)
}

# --- recompute the is_ok-dependent metric pipeline (canonical order) -------------------------
run_chunk("calc_cell_metric_redlist")                    # raw extrisk_{sp_cat}
run_chunk("calc_ecoregion_minmax")                       # extrisk_{sp_cat}_ecoregion_min/max (zone_metric)
run_chunk("extrisk_spcat_ecoregion_rescaled_cell")       # extrisk_{sp_cat}_ecoregion_rescaled (cell)
run_chunk("cell_metrics_to_zone_metrics")                # PRA + subregion zone_metric
run_chunk("apply_pctarea_to_programarea_components")     # PRA component pct-area weighting
run_chunk("cell_metrics_to_ecoregion_metrics")           # ecoregion zone_metric
run_chunk("cell_metric_score")                           # composite score (cell)
run_chunk("calc_programarea_score_from_programarea_metrics") # PRA composite (zone_metric)
run_chunk("metric_labels")                               # display labels

# zone_taxon: extend the zone set to include ecoregions (Set A per-ecoregion lists)
run_chunk("zone_taxon", transform = function(code)
  gsub("c(tbl_sr, tbl_pra)", "c(tbl_sr, tbl_pra, tbl_er)", code, fixed = TRUE))

# --- validation -----------------------------------------------------------------------------
message("\n=== VALIDATION ===")

# (1) is_ok / Set B
print(dbGetQuery(con_sdm, "SELECT count(*) FILTER (WHERE is_ok) AS is_ok FROM taxon"))

# (2) zone_taxon now covers ecoregions too
print(dbGetQuery(con_sdm, "SELECT zone_fld, count(DISTINCT zone_value) AS zones, count(DISTINCT mdl_seq) AS spp FROM zone_taxon GROUP BY zone_fld"))

# (3) PRA composite score: v7 vs v6 baseline (the drift report)
base_v6 <- read_csv(file.path(dir_v, "baseline_v6_pra_score.csv"), show_col_types = FALSE)
v7_pra <- dbGetQuery(con_sdm, glue("
  SELECT z.value AS programarea_key, zm.value AS score_v7
  FROM zone z JOIN zone_metric zm USING(zone_seq) JOIN metric m USING(metric_seq)
  WHERE z.fld='programarea_key'
    AND m.metric_key='score_extriskspcat_primprod_ecoregionrescaled_equalweights'"))
delta <- base_v6 |> inner_join(v7_pra, by = "programarea_key") |>
  mutate(delta = round(score_v7 - score_v6, 3), score_v6 = round(score_v6,3), score_v7 = round(score_v7,3)) |>
  arrange(desc(abs(delta)))
message("PRA composite score: v7 vs v6 (sorted by |delta|)")
print(delta, n = Inf)
message(glue("PRA score delta: mean(abs)={round(mean(abs(delta$delta)),3)}, ",
             "max(abs)={round(max(abs(delta$delta)),3)}"))
write_csv(delta, file.path(dir_v, "v7_vs_v6_pra_score_delta.csv"))

message("\nbuild_v7.R complete.")
