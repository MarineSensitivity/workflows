# migrate_derived_to_versions.R
# one-time script to reorganize derived/ into version subfolders
#
# run interactively — review each section before proceeding
# usage: source this file or run chunks in RStudio

librarian::shelf(fs, glue, quiet = T)

is_server <- Sys.info()[["sysname"]] == "Linux"
dir_data <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data"
)
dir_derived <- glue("{dir_data}/derived")
dir_big <- ifelse(
  is_server,
  dir_derived,
  "~/_big/msens/derived"
)

# create version directories ----
dirs <- c(
  glue("{dir_derived}/v1"),
  glue("{dir_derived}/v3"),
  glue("{dir_big}/v1"),
  glue("{dir_big}/v3")
)
for (d in dirs) {
  if (!dir_exists(d)) {
    dir_create(d)
    message(glue("Created: {d}"))
  } else {
    message(glue("Exists:  {d}"))
  }
}

# helper: move file if source exists ----
safe_move <- function(src, dst, label = "") {
  if (file_exists(src)) {
    file_move(src, dst)
    message(glue("  moved: {path_file(src)} -> {dst}"))
  } else {
    message(glue("  SKIP (not found): {src}  [{label}]"))
  }
}

# v1 file moves ----
message("\n--- Moving v1 files to derived/v1/ ---")

v1_moves <- list(
  # gpkg files (keep names)
  c("ply_planareas_2025.gpkg", "v1/ply_planareas_2025.gpkg"),
  c("ply_ecoregions_2025.gpkg", "v1/ply_ecoregions_2025.gpkg"),
  c("ply_subregions_2025.gpkg", "v1/ply_subregions_2025.gpkg"),
  # csv files
  c("layers_1.csv", "v1/layers.csv"),
  c("zone_taxon.csv", "v1/zone_taxon.csv"),
  c("zone_taxon_akl48.csv", "v1/zone_taxon_akl48.csv"),
  c("subregion_planareas.csv", "v1/subregion_planareas.csv"),
  # raster files
  c("r_metrics_akl48.tif", "v1/r_metrics_akl48.tif"),
  c("r_metrics_akl48.tif.aux.xml", "v1/r_metrics_akl48.tif.aux.xml"),
  c("r_metrics.tif.aux.xml", "v1/r_metrics.tif.aux.xml")
)

for (m in v1_moves) {
  safe_move(
    glue("{dir_derived}/{m[1]}"),
    glue("{dir_derived}/{m[2]}"),
    label = "v1"
  )
}

# v1 big file
message("\n--- Moving v1 big files ---")
safe_move(
  glue("{dir_derived}/sdm.duckdb"),
  glue("{dir_big}/v1/sdm.duckdb"),
  label = "v1 sdm"
)

# v3 file moves ----
message("\n--- Moving v3 files to derived/v3/ ---")

v3_moves <- list(
  # spatial outputs (keep year + version suffix)
  c("ply_programareas_2026_v3.gpkg", "v3/ply_programareas_2026_v3.gpkg"),
  c("ply_ecoregions_2025_v3.gpkg", "v3/ply_ecoregions_2025_v3.gpkg"),
  # ecoregion x program area intersection

  c(
    "ply_ecoregion_programarea_v3.gpkg",
    "v3/ply_ecoregion_programarea_v3.gpkg"
  ),
  c("ply_ecoregion_programarea_v3.csv", "v3/ply_ecoregion_programarea_v3.csv"),
  # label placement
  c("ply_label_placement_pra.csv", "v3/ply_label_placement_pra.csv"),
  # score tables
  c("tbl_er_pra_scores_v3.csv", "v3/tbl_er_pra_scores_v3.csv"),
  c("tbl_pra_scores_v3.csv", "v3/tbl_pra_scores_v3.csv"),
  # subregion crosswalk
  c("subregion_programareas.csv", "v3/subregion_programareas.csv"),
  # computed outputs — rename to drop _2026 (computed, not tied to input year)
  c("layers_2026_v3.csv", "v3/layers_v3.csv"),
  c("zone_taxon_2026_v3.csv", "v3/zone_taxon_v3.csv"),
  # raster — rename: drop _2026, use r_ prefix
  c("r_metrics_2026_v3.tif", "v3/r_metrics_v3.tif"),
  c("r_metrics_2026_v3.tif.aux.xml", "v3/r_metrics_v3.tif.aux.xml"),
  # raster — rename: akl48 -> programareas
  c("r_metrics_akl48_v3.tif", "v3/r_metrics_programareas_v3.tif"),
  c(
    "r_metrics_akl48_v3.tif.aux.xml",
    "v3/r_metrics_programareas_v3.tif.aux.xml"
  )
)

for (m in v3_moves) {
  safe_move(
    glue("{dir_derived}/{m[1]}"),
    glue("{dir_derived}/{m[2]}"),
    label = "v3"
  )
}

# also move the newly generated rast_metrics files (from calc_scores.qmd)
message("\n--- Moving rast_metrics (calc_scores output) to v3/ ---")
rast_v3_moves <- list(
  c("rast_metrics_v3.tif", "v3/r_metrics_v3.tif"),
  c("rast_metrics_v3.tif.aux.xml", "v3/r_metrics_v3.tif.aux.xml"),
  c("rast_metrics_programareas_v3.tif", "v3/r_metrics_programareas_v3.tif"),
  c(
    "rast_metrics_programareas_v3.tif.aux.xml",
    "v3/r_metrics_programareas_v3.tif.aux.xml"
  )
)

for (m in rast_v3_moves) {
  src <- glue("{dir_derived}/{m[1]}")
  dst <- glue("{dir_derived}/{m[2]}")
  # only move if dest doesn't already exist (r_metrics files already moved above)
  if (file_exists(src) && !file_exists(dst)) {
    safe_move(src, dst, label = "rast->r")
  } else if (file_exists(src) && file_exists(dst)) {
    message(glue(
      "  SKIP (dest exists): {m[1]} — delete source manually if stale"
    ))
  }
}

# also move layers_v3.csv and ply_programareas_v3.gpkg if they exist in derived/
# (these are duplicates with wrong names produced by calc_scores before the fix)
message("\n--- Moving duplicate calc_scores outputs ---")
dup_moves <- list(
  c("layers_v3.csv", "v3/layers_v3.csv"),
  c("ply_programareas_v3.gpkg", "v3/ply_programareas_v3_OLD.gpkg")
)

for (m in dup_moves) {
  src <- glue("{dir_derived}/{m[1]}")
  dst <- glue("{dir_derived}/{m[2]}")
  if (file_exists(src) && !file_exists(dst)) {
    safe_move(src, dst, label = "dup")
  } else if (file_exists(src)) {
    message(glue("  SKIP (dest exists): {m[1]}"))
  }
}

# v3 big file
message("\n--- Moving v3 big files ---")
safe_move(
  glue("{dir_derived}/sdm_v3.duckdb"),
  glue("{dir_big}/v3/sdm.duckdb"),
  label = "v3 sdm"
)

# move sdm_parquet to big-files ----
message("\n--- Moving sdm_parquet to big-files ---")
pq_src <- glue("{dir_derived}/sdm_parquet")
pq_dst <- glue("{dir_big}/v3/sdm_parquet")
if (dir_exists(pq_src) && !dir_exists(pq_dst)) {
  file_move(pq_src, pq_dst)
  message(glue("  moved: sdm_parquet -> {pq_dst}"))
} else if (dir_exists(pq_src)) {
  message("  SKIP (dest exists): sdm_parquet")
} else {
  message("  SKIP (not found): sdm_parquet")
}

# fix macOS alias — replace with unix symlink ----
message("\n--- Setting up _big symlink ---")
alias_old <- glue("{dir_derived}/_derived_big_files")
alias_new <- glue("{dir_derived}/_big")

# remove old alias/symlink if present
if (file_exists(alias_old) || is_link(alias_old)) {
  file_delete(alias_old)
  message("  removed old _derived_big_files")
}

# create (or replace) _big symlink
if (is_link(alias_new)) {
  file_delete(alias_new)
}
link_create(dir_big, alias_new)
message(glue("  created symlink _big -> {dir_big}"))

# list stale files for manual review ----
message(
  "\n--- Stale/intermediate files remaining in derived/ (review manually) ---"
)
stale_patterns <- c(
  "ply_programareas_v3\\.gpkg", # wrong name, superseded
  "ply_programareas_2026\\.gpkg", # v2 era
  "ply_programareas_2026_no-metrics", # intermediate
  "layers_v3\\.csv", # regenerated in v3/
  "layers_2026\\.csv", # v2 era
  "rast_metrics_v3\\.tif", # wrong prefix
  "rast_metrics_programareas_v3", # wrong prefix
  "r_metrics_2026\\.tif", # v2 era
  "r_metrics_2026\\.tif\\.aux\\.xml", # v2 era
  "_2025-06-18", # dated backup
  "_2025-06-27", # dated backup
  "spp\\.duckdb", # 4.4G — move to big-files if needed
  "msens_sdm\\.duckdb", # tiny, likely stale
  "tbl_er_pa_scores\\.csv", # v1 era, unversioned
  "tbl_er_pa_scores\\.xlsx", # v1 era
  "tbl_pa_scores\\.csv", # v1 era
  "tbl_pa_scores\\.xlsx", # v1 era
  "ply_ecoregion_planarea", # v1 era (plan areas, not program areas)
  "ply_label_placement\\.csv", # v1 era (not pra version)
  "spp_global_cache\\.csv", # possibly stale
  "sdm_parquet_" # old dated parquet backups
)

all_files <- dir_ls(dir_derived, recurse = F) |> path_file()
stale <- character(0)
for (pat in stale_patterns) {
  matches <- all_files[grepl(pat, all_files)]
  stale <- c(stale, matches)
}
stale <- unique(stale)

if (length(stale) > 0) {
  for (f in sort(stale)) {
    info <- file_info(glue("{dir_derived}/{f}"))
    sz <- ifelse(
      is.na(info$size),
      "???",
      format(info$size, big.mark = ",")
    )
    message(glue("  {f}  ({sz})"))
  }
  message(glue(
    "\n  {length(stale)} files flagged — review and delete manually"
  ))
} else {
  message("  no stale files found")
}

# summary ----
message("\n=== Migration complete ===")
message(glue(
  "  derived/v1/ files: {length(dir_ls(glue('{dir_derived}/v1'), recurse = F))}"
))
message(glue(
  "  derived/v3/ files: {length(dir_ls(glue('{dir_derived}/v3'), recurse = F))}"
))
if (!is_server) {
  message(glue(
    "  big-files/v1/ files: {length(dir_ls(glue('{dir_big}/v1'), recurse = F))}"
  ))
  message(glue(
    "  big-files/v3/ files: {length(dir_ls(glue('{dir_big}/v3'), recurse = F))}"
  ))
}
