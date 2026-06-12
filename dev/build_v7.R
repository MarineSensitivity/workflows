# build_v7.R — derive v7 from v6: drop the Program-Area gate on is_ok and recompute
# scores over the full valid (Set A) species set. Portable across laptop + server.
#
# Prereq: a v7 sdm.duckdb that is a copy of v6 (this script does NOT copy it).
#   laptop: ~/_big/msens/derived/v7/sdm.duckdb
#   server: /share/data/big/v7/sdm.duckdb
#
# What it does (idempotent):
#   1. recompute taxon.is_ok = taxonomic/data validity + has mdl_seq (no PRA gate)
#   2. run calc_scores.qmd's scoring chunks verbatim to rescale over the widened set
#   3. extend zone_taxon to ecoregions
#   4. validate: is_ok count, Set B, and PRA composite-score drift vs v6

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(dbplyr)
  library(glue); library(stringr); library(tidyr); library(readr); library(msens)
})

is_server <- Sys.info()[["sysname"]] == "Linux"
repo_wf  <- if (is_server) "/share/github/MarineSensitivity/workflows" else "/Users/bbest/Github/MarineSensitivity/workflows"
dir_big  <- if (is_server) "/share/data/big" else path.expand("~/_big/msens/derived")
ver <- "v7"; ver_prev <- "v6"
dir_v    <- file.path(dir_big, ver)
sdm_db   <- file.path(dir_v, "sdm.duckdb")
v6_db    <- file.path(dir_big, ver_prev, "sdm.duckdb")
qmd_path <- file.path(repo_wf, "calc_scores.qmd")
stopifnot(file.exists(sdm_db), file.exists(v6_db), file.exists(qmd_path))

component_pct_pra_csv <- file.path(dir_v, glue("tbl_component_pct_programarea_{ver}.csv"))
zone_taxon_csv        <- file.path(dir_v, glue("zone_taxon_{ver}.csv"))

con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = FALSE)
on.exit(dbDisconnect(con_sdm, shutdown = TRUE), add = TRUE)

# 1) recompute is_ok (validity only — drop the Program-Area narrowing) -----------------------
dbExecute(con_sdm, "
  UPDATE taxon SET is_ok = CASE
    WHEN taxon_id IS NULL THEN FALSE
    WHEN mdl_seq IS NULL THEN FALSE
    WHEN redlist_code IS NOT NULL AND redlist_code = 'EX' THEN FALSE
    WHEN worms_id IS NOT NULL AND worms_is_marine = FALSE THEN FALSE
    WHEN worms_id IS NOT NULL AND worms_is_extinct = TRUE THEN FALSE
    WHEN taxon_authority = 'worms' AND worms_taxonomic_status IS NOT NULL
         AND worms_taxonomic_status NOT IN ('accepted','alternative representation') THEN FALSE
    WHEN taxon_authority = 'worms' AND sp_cat = 'reptile' THEN FALSE
    ELSE TRUE END")
n_ok <- dbGetQuery(con_sdm, "SELECT count(*) n FROM taxon WHERE is_ok")$n
message(glue("is_ok recomputed: {n_ok}"))

# rename versioned zone tables _v6 -> _v7 (mirror calc_scores.qmd setup)
for (old_tbl in dbGetQuery(con_sdm, glue(
  "SELECT DISTINCT tbl FROM zone WHERE tbl LIKE '%\\_{ver_prev}' ESCAPE '\\'"))$tbl) {
  new_tbl <- str_replace(old_tbl, glue("_{ver_prev}$"), glue("_{ver}"))
  dbExecute(con_sdm, glue("UPDATE zone       SET tbl='{new_tbl}'      WHERE tbl='{old_tbl}'"))
  dbExecute(con_sdm, glue("UPDATE zone_taxon SET zone_tbl='{new_tbl}' WHERE zone_tbl='{old_tbl}'"))
  message(glue("renamed zone tbl: {old_tbl} -> {new_tbl}"))
}

tbl_pra <- glue("ply_programareas_2026_{ver}")
tbl_sr  <- glue("ply_subregions_2026_{ver}")
tbl_er  <- "ply_ecoregions_2025"
sp_cats <- tbl(con_sdm, "taxon") |> filter(is_ok) |> distinct(sp_cat) |> arrange(sp_cat) |> pull(sp_cat)

# 2) run scoring chunks verbatim from calc_scores.qmd ----------------------------------------
qmd_lines <- readLines(qmd_path)
get_chunk <- function(label) {
  lab <- grep(paste0("^#\\|\\s*label:\\s*", label, "\\s*$"), qmd_lines); stopifnot(length(lab) == 1)
  open  <- max(grep("^```\\{", qmd_lines[seq_len(lab)]))
  close <- lab + grep("^```\\s*$", qmd_lines[(lab + 1):length(qmd_lines)])[1]
  code  <- qmd_lines[(open + 1):(close - 1)]
  paste(code[!grepl("^#\\|", code)], collapse = "\n")
}
run_chunk <- function(label, transform = identity) {
  message(glue("=== chunk: {label} ==="))
  eval(parse(text = transform(get_chunk(label))), envir = globalenv())
}
for (lbl in c(
  "calc_cell_metric_redlist", "calc_ecoregion_minmax", "extrisk_spcat_ecoregion_rescaled_cell",
  "cell_metrics_to_zone_metrics", "apply_pctarea_to_programarea_components",
  "cell_metrics_to_ecoregion_metrics", "cell_metric_score",
  "calc_programarea_score_from_programarea_metrics", "metric_labels"))
  run_chunk(lbl)
run_chunk("zone_taxon", transform = function(code)
  gsub("c(tbl_sr, tbl_pra)", "c(tbl_sr, tbl_pra, tbl_er)", code, fixed = TRUE))

# 3) validation ------------------------------------------------------------------------------
message("\n=== VALIDATION ===")
print(dbGetQuery(con_sdm, "SELECT count(*) FILTER (WHERE is_ok) AS is_ok FROM taxon"))
print(dbGetQuery(con_sdm, "SELECT zone_fld, count(DISTINCT zone_value) zones, count(DISTINCT mdl_seq) spp FROM zone_taxon GROUP BY zone_fld"))

con6 <- dbConnect(duckdb(), dbdir = v6_db, read_only = TRUE)
q_pra <- "SELECT z.value programarea_key, zm.value score FROM zone z
  JOIN zone_metric zm USING(zone_seq) JOIN metric m USING(metric_seq)
  WHERE z.fld='programarea_key' AND m.metric_key='score_extriskspcat_primprod_ecoregionrescaled_equalweights'"
d6 <- dbGetQuery(con6, q_pra); dbDisconnect(con6, shutdown = TRUE)
d7 <- dbGetQuery(con_sdm, q_pra)
delta <- d6 |> rename(score_v6 = score) |> inner_join(d7 |> rename(score_v7 = score), by = "programarea_key") |>
  mutate(delta = round(score_v7 - score_v6, 3)) |> arrange(desc(abs(delta))) |> tibble::as_tibble()
print(as.data.frame(delta))
message(glue("PRA score delta: mean|d|={round(mean(abs(delta$delta)),3)}, max|d|={round(max(abs(delta$delta)),3)}"))
write_csv(delta, file.path(dir_v, "v7_vs_v6_pra_score_delta.csv"))
message("build_v7.R complete.")
