## 2026-02-? TODO:

- Migrate from pg_tileserv to PMTiles, see:
  - [Creating PMTiles | Protomaps Docs](https://docs.protomaps.com/pmtiles/create)
  - [CalCOFI README_PLAN](https://calcofi.io/workflows/README_PLAN.html)

- Automate server updates:
  - Upload sdm.duckdb to v3/ on server (scp or rsync)
  - Update any Shiny Server config if app paths changed

- Generate GD:data/derived/v3/`README.md` from NEW generate_outputs.qmd
  - merges output generation from @`calc_scores.qmd` and @`msens-summary_programareas.qmd`
  - to be run after calc_scores.qmd
  - Differentitate in `README.md`:
    - Spatial data (, *.tif, *.gpkg)
    - Tables (*.csv, *.xlsx, *.docx)
    - Figures (*.png, *.pdf)
    - Big files (): too big for Google Drive, internally used

- Migrate to hexagons

## 2026-02-13 apply er_score spatially

Originally species distributions were brought in with cell values representing suitable habitat, starting with AquaMaps (`dataset.ds_key`: 'am_0.05'. Then range maps from BirdLife International (`dataset.ds_key`: 'bl') were ingested (since AquaMaps generally lacks seabirds) with cell values of 50% (halfway between 100% present and 0% absent). Then an extinction risk from IUCN RedList (`rl_score` 0 to 100%) for the species was multipled by the suitability to create the "vulnerability" of a species cell. (This species cell was then summed to taxonomic groups and rescaled by ecoregion max before component weighting to acheive a score.) Here's the relevant excerpt from @workflows/final-report_2025:

> For spatial implementation, the vulnerability of a cell ($v_c$) is calculated as the sum across all species in the given taxonomic group ($S_g$) of the products for the species presence in the cell ($p_{sc}$) and a species weight ($w_s$), which is the risk of that species going extinct:
> 
> $$
> v_c=∑_1^{S_g} p_{sc} * w_s
> $$
> 
> In other words, for each cell in the ocean, we add up the sensitivity of all the species found there.
> 
> -   $v_c$ is the vulnerability of a cell.
> 
> -   $p_{sc}$ is how likely species $s$ is to be present in that cell (from 0 to 1).
> 
> -   $w_s$ is how at-risk that species is of going extinct (also from 0 to 1; ranging from Least Concern 0.2 to Critically Endangered as 1).
> 
> -   $S_g$ is the total number of species in that taxonomic group.
> 
> If a cell has many species that are both likely to be present and at high risk of extinction, it gets a higher sensitivity score. This helps us find > places where rare or threatened species are concentrated. Ecoregional rescaling makes it easy to compare areas within the same region and planning area aggregation gives us an overall sensitivity score for each planning area, taking into account both the sensitivity of each part and how big each part is.

Then we ingested critical habitat and range maps from NMFS and FWS (`dataset.ds_key`: 'ca_nmfs', 'ch_fws', 'ch_nmfs', 'rng_fws') which got cell values assigned according to the US national ESA listing status Endangered (100%), Threatened (50%), or Least Concern (1%) of the given spatial footprint (aligned with a Distinct Population Segment, if applicable, so not necessarily the same value for the entire species distribution). This then muddied the definition of a cell value -- does it represent suitability and therefore a likelihood of presence, or is it a measure of extinction risk?

Could we have both? So, instead of multiplying a single scalar value of the maximum extinction risk available (ie from NMFS, FWS or IUCN) to the whole distribution raster, we can multiply the suitability raster by a raster representing extinction risk. In cases where we don't have any extinction-risk-specific species distribution models (ie  `dataset.ds_key`: 'rng_iucn', 'ca_nmfs', 'ch_fws', 'ch_nmfs', 'rng_fws'), we would assign the maximum (`max(er_score)`) (or 1 for assumed LC) to the whole distribution. In cases where more than one er_score layer is available for the same cell, the dataset.`sort_order` should determine the preferred value (ie prefer: US > International; NMFS > FWS > IUCN | BirdLife):

```r
d_datasets <- tbl(con_sdm, "dataset") |> collect()
d_datasets |>
  arrange(sort_order) |>
  select(sort_order, ds_key, name_display, value_info, is_mask, sort_order)
```
```
  sort_order ds_key   name_display          value_info                           is_mask
       <int> <chr>    <chr>                 <chr>                                <lgl>
1          0 ms_merge Merged Model          NA                                   FALSE
2          1 am_0.05  AquaMaps SDM          NA                                   FALSE
3          2 ca_nmfs  NMFS Core Area        Core: 100                            TRUE
4          3 ch_nmfs  NMFS Critical Habitat EN:100, TN:50                        TRUE
5          4 ch_fws   FWS Critical Habitat  EN:100, TN:50                        TRUE
6          5 rng_fws  FWS Range             EN:100, TN:50, LC:1                  TRUE
7          6 bl       BirdLife Range        NA                                   TRUE
8          7 rng_iucn IUCN Range            CR:50, EN:25, VU:5, NT:2, LC:1, DD:1 TRUE
```

Modifications will be needed to apply this new scheme:

- Add a new column `has_er` to `dataset` table, which for now has the same values as `is_mask`.
- I believe BirdLife  


What else should be considered or modified?




We still want to apply a mask to species with an IUCN range map (`ds_key`: 'rng_iucn') to the results.

```r
d_datasets <- tbl(con_sdm, "dataset") |> collect()
View(d_datasets)
d_datasets |>
  arrange(sort_order) |>
  select(sort_order, ds_key, name_display, value_info, is_mask, sort_order)
```
```
  sort_order ds_key   name_display          value_info                           is_mask
       <int> <chr>    <chr>                 <chr>                                <lgl>
1          0 ms_merge Merged Model          NA                                   FALSE
2          1 am_0.05  AquaMaps SDM          NA                                   FALSE
3          2 ca_nmfs  NMFS Core Area        Core: 100                            TRUE
4          3 ch_nmfs  NMFS Critical Habitat EN:100, TN:50                        TRUE
5          4 ch_fws   FWS Critical Habitat  EN:100, TN:50                        TRUE
6          5 rng_fws  FWS Range             EN:100, TN:50, LC:1                  TRUE
7          6 bl       BirdLife Range        NA                                   TRUE
8          7 rng_iucn IUCN Range            CR:50, EN:25, VU:5, NT:2, LC:1, DD:1 TRUE
```

 (which could be the same value for the entire distribution, so effectively the same as before)

Code the datasets table to account for which layers `has_er_info`.

ms_merge, 

- For all bird models (dataset.ds_key = "bl")

datasets: am_0.05, bl, ca_nmfs, ch_fws, ch_nmfs, ms_merge, rng_fws, rng_iucn

`msens::compute_er_score()`

## 2026-02-13.b + schema.qmd

Generate a workflow `schema.qmd` that shows me the schema and table info for the following two databases:

- **SDM** 
  _Species Distribution Model_
  path: `/Users/bbest/_big/msens/derived/v3/sdm.duckdb`

- **SPP**
  _species_: taxonomic authority tables
  path: `/Users/bbest/_big/msens/derived/spp.duckdb`

For each database (under its own level 2 headers), generate:

- **Schema**: an ERD mermaid diagram with inferred primary and foreign keys

- **Tables**: tabsets of each table with columns for:

  - `column`: column name
  - `type`: storage type
  - `description`: infer description
  - `range`: range of values (as '[min, max]') if numeric or date
  - `samples`: show some sample values with most frequent listed first


Generate a workflow `schema.qmd` that shows me the schema and table info for the following two databases:

- **SDM** 
  _Species Distribution Model_
  path: `/Users/bbest/_big/msens/derived/v3/sdm.duckdb`

- **SPP**
  _species_: taxonomic authority tables
  path: `/Users/bbest/_big/msens/derived/spp.duckdb`

For each database (under its own level 2 headers), generate:

- **Schema**: an ERD mermaid diagram with inferred primary and foreign keys

- **Tables**: tabsets of each table with columns for:

  - `column`: column name
  - `type`: storage type
  - `description`: infer description
  - `range`: range of values (as '[min, max]') if numeric or date
  - `samples`: show some sample values with most frequent listed first

* `schema.qmd` has been created and renders successfully to `_output/schema.html` (189K).

The file includes:
- **Setup**: read-only connections to both DuckDB databases (`sdm.duckdb` v3, `spp.duckdb`) with `is_server` path switching
- **Schema change detection**: compares actual `information_schema.columns` against expected lists; emits a `{.callout-warning}` if tables/columns are added or removed
- **SDM section** (14 tables): mermaid ERD showing all relationships (`dataset`, `model`, `species`, `cell`, `model_cell`, `cell_metric`, `metric`, `zone`, `zone_cell`, `zone_metric`, `taxon`, `taxon_model`, `zone_taxon`, `listing`) + tabset with interactive DT tables per table
- **SPP section** (10 tables): mermaid ERD with 5 authority/vernacular pairs (`worms`, `gbif`, `itis`, `botw`, `iucn_redlist`) + tabset with DT tables
- **`tbl_info()` helper**: for each table shows column name, type, inferred description, `[min, max]` range for numeric/date columns, and top 5 most frequent values (with sampling for tables >5M rows)


The ERD diagram has tiny text and is hard to read. Is there an option to zoom or expand it to full screen and move it around, like Github has for mermaid diagrams? If handling the SVG like this is not easily acheived, perhaps it could be conveted to a figure and        
  the lightbox Quarto extension applied?         

## 2026-02-13 add motherduck MCP to claude (but for calcofi for now)

```bash
claude mcp add duckdb --transport stdio -- uvx mcp-server-motherduck --db-path /Users/bbest/_big/msens/derived/v3/sdm.duckdb

'/Users/bbest/Library/CloudStorage/GoogleDrive-ben@ecoquants.com/My Drive/private/2026-02-13_motherduck.com_access-token_ben-read-write-01.txt'
```

## 2026-02-12.b apps versioning with git branches, deploy latest v3 to server

Now that I've confirmed all file restructuring help me with apps versioning with branches, releases or other idea?

    I would also like to maintain backword compatibility with `v1` (sdm.duckdb), `v2` (sdm_2026.duckdb) and `v3` (sdm_v3.duckdb) of the @../apps/{mapgl|mapsp} (now on main branch for v1 on laptop and server). What advice do you have on maintaining backwards compatability with the apps at different iterations? Seems like I should:

    - `v1`: rename @../apps to @../apps_v1 with a fork from "main" to branch "v1", 
    - `v2`: copy @../apps_2026 to @../apps_v2 and rollback to commits predating "v3" and fork to branch "v2"
    - `v3`: rename @../apps_2026 to @../apps and merge current branch "programareas_2026" to branch "main"

Here also is the current state of the Shiny app git repositories with branch, commit and date stamp:



```bash
# cd to symlinks of Shiny server folders
cd /srv/shiny-server
for d in *; do if [[ -L "$d" ]] && [[ "$d" == *mapgl* || "$d" == *mapsp* ]]; then (cd "$d" && printf "%-30s %-20s %s %s\n" "$d" "$(git branch --show-current 2>/dev/null)" "$(git log -1 --format='%h %ci' 2>/dev/null)"); fi; done
```
```
mapgl                          programareas_2026    76afd58 2026-02-11 13:56:00 -0600 
mapgl_2025                     main                 6cb5ae0 2026-01-13 13:12:14 -0800 
mapgl_v2                       programareas_2026    86448d1 2026-01-30 09:07:52 -0600 
mapsp                          programareas_2026    76afd58 2026-02-11 13:56:00 -0600 
mapsp_2025                     main                 6cb5ae0 2026-01-13 13:12:14 -0800 
mapsp_v2                       programareas_2026    86448d1 2026-01-30 09:07:52 -0600 
```

And here's the state of the data/derived folder on the server which needs to get cleaned up:

```bash
cd /share/data/derived
ls -latr
```
```
total 13349712
drwxr-xr-x  2 admin admin         44 May 22  2025 aquamaps
-rw-r--r--  1 admin admin   31672006 May 22  2025 pa_oa_2025.geojson
-rw-r--r--  1 admin admin      34753 May 26  2025 pa-2025_am-cid.tif
drwxr-xr-x  3 admin admin         66 May 26  2025 aquamaps.org
-rw-r--r--  1 admin admin   42102914 May 30  2025 r_bio-oracle_planarea.tif
drwxrwxr-x 12 admin admin       4096 Jun 18  2025 sdm_parquet_2025-06-18
-rw-r--r--  1 admin admin   14913536 Jun 18  2025 ply_planareas_2025.gpkg
-rw-r--r--  1 admin admin   18165760 Jun 18  2025 ply_ecoregions_2025.gpkg
drwxrwxr-x 14 admin admin       4096 Aug  6  2025 sdm_parquet
-rw-r--r--  1 admin admin 2005938176 Aug  6  2025 sdm.duckdb
-rw-r--r--  1 admin admin       1436 Aug  6  2025 layers.csv
-rw-rw-r--  1 admin admin   14905344 Aug  6  2025 ply_planareas_2025-06-18.gpkg
-rw-rw-r--  1 admin admin   18169856 Aug  6  2025 ply_ecoregions_2025-06-18.gpkg
-rw-r--r--  1 admin admin   39829222 Aug  6  2025 r_metrics.tif
-rw-rw-r--  1 admin admin        785 Aug  7  2025 subregion_planareas.csv
-rw-rw-r--  1 admin admin   52740096 Aug  7  2025 ply_subregions_2025.gpkg
drwxr-xr-x  7 admin admin         87 Sep 29 20:08 ..
-rw-rw-r--  1 admin admin        330 Jan 13 14:02 subregion_programareas.csv
-rw-rw-r--  1 admin admin       1432 Jan 13 14:02 layers_2026.csv
-rw-rw-r--  1 admin admin    7368704 Jan 13 14:02 ply_programareas_2026.gpkg
-rw-rw-r--  1 admin admin   39625975 Jan 13 14:02 r_metrics_2026.tif
-rw-rw-r--  1 admin admin   62022622 Jan 13 14:02 zone_taxon_2026.csv
-rw-r--r--  1 admin admin 2067804160 Jan 23 19:44 sdm_2026-01-13.duckdb
drwxrwxr-x  2 admin admin          6 Jan 23 19:48 new
-rw-rw-r--  1 admin admin 2205954048 Jan 29 15:37 sdm_2026-01-23.duckdb
-rw-rw-r--  1 admin admin 2240819200 Jan 29 16:05 sdm_2026.duckdb
-rw-rw-r--  1 admin admin        796 Feb 11 13:31 ply_label_placement_pra.csv
-rw-rw-r--  1 admin admin       1863 Feb 11 13:31 ply_ecoregion_programarea_v3.csv
-rw-rw-r--  1 admin admin      16696 Feb 11 13:31 r_metrics_2026_v3.tif.aux.xml
-rw-rw-r--  1 admin admin      16696 Feb 11 13:31 r_metrics_akl48_v3.tif.aux.xml
-rw-rw-r--  1 admin admin    7806976 Feb 11 13:32 ply_ecoregion_programarea_v3.gpkg
-rw-rw-r--  1 admin admin       1599 Feb 11 13:32 layers_2026_v3.csv
-rw-rw-r--  1 admin admin   14909440 Feb 11 13:32 ply_planareas_2025_v3.gpkg
-rw-rw-r--  1 admin admin    7360512 Feb 11 13:32 ply_programareas_2026_v3.gpkg
-rw-rw-r--  1 admin admin   18165760 Feb 11 13:32 ply_ecoregions_2025_v3.gpkg
-rw-rw-r--  1 admin admin   22390205 Feb 11 13:32 r_metrics_akl48_v3.tif
-rw-rw-r--  1 admin admin   41939639 Feb 11 13:32 r_metrics_2026_v3.tif
-rw-rw-r--  1 admin admin   60834144 Feb 11 13:32 zone_taxon_2026_v3.csv
-rw-rw-r--  1 admin admin 2281189376 Feb 11 17:30 sdm_v3.duckdb
-rw-rw-r--  1 admin admin 2280665088 Feb 12 11:08 sdm_v3_2026-02-12.duckdb
-rw-rw-r--  1 admin admin      16580 Feb 12 14:49 rast_metrics_programareas_v3.tif.aux.xml
-rw-rw-r--  1 admin admin       1091 Feb 12 14:49 tbl_pra_scores_v3.csv
-rw-rw-r--  1 admin admin       4700 Feb 12 14:49 tbl_er_pra_scores_v3.csv
drwxr-xr-x  7 admin admin       4096 Feb 12 14:49 .
-rw-rw-r--  1 admin admin      16580 Feb 12 14:49 rast_metrics_v3.tif.aux.xml
-rw-rw-r--  1 admin admin       1432 Feb 12 14:49 layers_v3.csv
-rw-rw-r--  1 admin admin    7368704 Feb 12 14:49 ply_programareas_v3.gpkg
-rw-rw-r--  1 admin admin   21846330 Feb 12 14:50 rast_metrics_programareas_v3.tif
-rw-rw-r--  1 admin admin   43395433 Feb 12 14:51 rast_metrics_v3.tif
```

Can you also advise on how best to automate the server updates, like with rclone, ssh commands or similar, for pushing files from laptop to server and pulling Github commits. We only need to do this for the latest version app (now v3).

⏺ Updated plan                                                                                                                                                    
  ⎿  /plan to preview                                                                                                                                                                  
 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Plan: Apps versioning with git branches                                                                                                                        

 Context

 The MarineSensitivity/apps repo serves three simultaneous app versions on the
 Shiny server. Currently main has v1 code and programareas_2026 has v2→v3
 code. Two local clones exist: apps/ (on main, uncommitted v1 path changes)
 and apps_2026/ (on programareas_2026, committed v3 path changes). The server
 has six symlinks pointing to clones at various commits. Goal: formalize with
 proper v1, v2 branches, merge v3 into main, restructure server data into
 v1/, v2/, v3/ subdirectories.

 Phase 1: Create v1 branch (in apps/ clone)

 cd ~/Github/MarineSensitivity/apps  # on main, with uncommitted v1 path changes

 git add mapgl/app.R mapsp/app.R
 git commit -m "update paths to derived/v1/ for versioned directory structure"
 git branch v1           # freeze v1 at this commit
 git push origin v1

 Phase 2: Create v2 branch (in apps_2026/ clone)

 cd ~/Github/MarineSensitivity/apps_2026

 git branch v2 86448d1   # v2 cutoff commit (Jan 30)
 git checkout v2

 Update mapgl/app.R paths (~line 48-72) to use derived/v2/:

 v_dir <- "v2"
 dir_v   <- glue("{dir_data}/derived/{v_dir}")
 dir_big <- ifelse(is_server, dir_v,
   glue("~/_big/msens/derived/{v_dir}"))

 sdm_db      <- glue("{dir_big}/sdm.duckdb")
 cell_tif    <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
 er_gpkg     <- glue("{dir_v}/ply_ecoregions_2025.gpkg")
 lyrs_csv    <- glue("{dir_v}/layers.csv")
 metrics_tif <- glue("{dir_v}/r_metrics.tif")
 pra_gpkg    <- glue("{dir_v}/ply_programareas_2026.gpkg")
 sr_pra_csv  <- glue("{dir_v}/subregion_programareas.csv")

 Update mapsp/app.R paths (~line 30-45):

 v_dir <- "v2"
 dir_v   <- glue("{dir_data}/derived/{v_dir}")
 dir_big <- ifelse(is_server, dir_v,
   glue("~/_big/msens/derived/{v_dir}"))

 sdm_db   <- glue("{dir_big}/sdm.duckdb")
 cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")

 git add mapgl/app.R mapsp/app.R
 git commit -m "update paths to derived/v2/ for versioned directory structure"
 git push origin v2

 Phase 3: Merge programareas_2026 into main

 ```bash
 cd ~/Github/MarineSensitivity/apps_2026

 git checkout main
 git pull origin main           # get the v1 commit from Phase 1
 git merge programareas_2026 -m "merge programareas_2026 (v3) into main"
 # resolve any conflicts (mapgl/app.R, mapsp/app.R — keep programareas_2026 version)
 git push origin main
 ```

 Phase 4: Rename local clones
 
 ```bash
 cd ~/Github/MarineSensitivity

 mv apps apps_v1                     # frozen v1 clone
 cd apps_v1 && git checkout v1 && cd ..

 mv apps_2026 apps                   # active v3 clone, now on main
 cd apps && git checkout main && git pull && cd ..

 # v2 worktree (lightweight, no separate clone)
 git -C apps worktree add ../apps_v2 v2

 Result:
 apps/     → main   (v3, active development)
 apps_v1/  → v1     (frozen)
 apps_v2/  → v2     (frozen)
 ```

 Phase 5: Server data — create v1/, v2/, v3/

 ```bash
 ssh msens
 cd /share/data/derived
 mkdir -p v1 v2 v3

 # --- v1 files ---
 mv ply_planareas_2025.gpkg      v1/
 mv ply_ecoregions_2025.gpkg     v1/
 mv ply_subregions_2025.gpkg     v1/
 mv layers.csv                   v1/
 mv r_metrics.tif                v1/
 mv subregion_planareas.csv      v1/
 mv sdm.duckdb                   v1/

 # --- v2 files (drop _2026 suffix where it's a computed output) ---
 mv sdm_2026.duckdb              v2/sdm.duckdb
 mv layers_2026.csv              v2/layers.csv
 mv ply_programareas_2026.gpkg   v2/
 mv r_metrics_2026.tif           v2/r_metrics.tif
 mv zone_taxon_2026.csv          v2/zone_taxon.csv
 cp subregion_programareas.csv   v2/
 cp v1/ply_ecoregions_2025.gpkg  v2/   # v2 also uses this

 # --- v3 files (standardized names) ---
 mv sdm_v3.duckdb                    v3/sdm.duckdb
 mv layers_2026_v3.csv               v3/layers_v3.csv
 mv layers_v3.csv                    v3/   # dup from calc_scores — overwrite OK
 mv ply_programareas_2026_v3.gpkg    v3/
 mv ply_ecoregions_2025_v3.gpkg      v3/
 mv ply_planareas_2025_v3.gpkg       v3/
 mv r_metrics_2026_v3.tif            v3/r_metrics_v3.tif
 mv r_metrics_2026_v3.tif.aux.xml    v3/r_metrics_v3.tif.aux.xml
 mv r_metrics_akl48_v3.tif           v3/r_metrics_programareas_v3.tif
 mv r_metrics_akl48_v3.tif.aux.xml   v3/r_metrics_programareas_v3.tif.aux.xml
 mv zone_taxon_2026_v3.csv           v3/zone_taxon_v3.csv
 mv subregion_programareas.csv       v3/
 mv ply_ecoregion_programarea_v3.gpkg v3/
 mv ply_ecoregion_programarea_v3.csv  v3/
 mv ply_label_placement_pra.csv       v3/
 mv tbl_er_pra_scores_v3.csv          v3/
 mv tbl_pra_scores_v3.csv             v3/
 mv rast_metrics_v3.tif               v3/   # stale dup — delete after confirming
 mv rast_metrics_programareas_v3.tif  v3/   # stale dup — delete after confirming
 mv ply_programareas_v3.gpkg          v3/   # stale dup — delete after confirming

 Stale files to archive/delete after apps confirmed working:
 sdm_2026-01-13.duckdb, sdm_2026-01-23.duckdb   (dated backups)
 sdm_v3_2026-02-12.duckdb                       (dated backup)
 sdm_parquet/, sdm_parquet_2025-06-18/           (old parquet exports)
 ply_planareas_2025-06-18.gpkg                   (dated)
 ply_ecoregions_2025-06-18.gpkg                  (dated)
 pa_oa_2025.geojson, pa-2025_am-cid.tif         (intermediate)
 rast_metrics_*.tif.aux.xml                      (wrong prefix, aux only)
 new/                                            (empty?)
 aquamaps/, aquamaps.org/                        (source data, not derived)
 ```

 Phase 6: Update server app clones + symlinks

 ```bash
 ssh msens
 cd /srv/shiny-server

 # v3: switch from programareas_2026 to main
 for d in mapgl mapsp; do
   cd $d && git fetch origin && git checkout main && git pull && cd ..
 done

 # v1: switch from main to v1 branch
 for d in mapgl_2025 mapsp_2025; do
   cd $d && git fetch origin && git checkout v1 && git pull origin v1 && cd ..
 done

 # v2: switch to v2 branch
 for d in mapgl_v2 mapsp_v2; do
   cd $d && git fetch origin && git checkout v2 && git pull origin v2 && cd ..
 done

 Optional: rename _2025 symlinks to _v1 for consistency:
 cd /srv/shiny-server
 ln -sfn $(readlink mapgl_2025) mapgl_v1 && rm mapgl_2025
 ln -sfn $(readlink mapsp_2025) mapsp_v1 && rm mapsp_2025
 ```

 Phase 7: Server data READMEs

 Copy README.md files to server v1/, v2/, v3/ directories
 (create v2 README matching the pattern of the v1/v3 READMEs already written
 to Google Drive).

 Files to modify
 ┌──────────────┬────────┬─────────────────────────────────────────────────┐
 │     File     │ Branch │                     Changes                     │
 ├──────────────┼────────┼─────────────────────────────────────────────────┤
 │ mapgl/app.R  │ v1     │ commit existing uncommitted derived/v1/ paths   │
 ├──────────────┼────────┼─────────────────────────────────────────────────┤
 │ mapsp/app.R  │ v1     │ commit existing uncommitted derived/v1/ paths   │
 ├──────────────┼────────┼─────────────────────────────────────────────────┤
 │ mapgl/app.R  │ v2     │ update flat derived/ paths → derived/v2/        │
 ├──────────────┼────────┼─────────────────────────────────────────────────┤
 │ mapsp/app.R  │ v2     │ update flat derived/ paths → derived/v2/        │
 ├──────────────┼────────┼─────────────────────────────────────────────────┤
 │ (no changes) │ main   │ v3 paths already committed on programareas_2026 │
 └──────────────┴────────┴─────────────────────────────────────────────────┘
 Phase 8: Deployment script — deploy_v3.sh

 Create workflows/deploy_v3.sh to automate syncing v3 data and pulling app
 code on the server. Uses rsync over SSH with the existing key.

 ```bash
 #!/usr/bin/env bash
 # deploy_v3.sh — sync v3 derived data + app code to msens server
 #
 # usage:
 #   ./deploy_v3.sh            # sync data + pull apps
 #   ./deploy_v3.sh data       # sync data only
 #   ./deploy_v3.sh apps       # pull apps only
 set -euo pipefail

 # ssh config ----
 SSH_KEY="$HOME/My Drive/private/msens_key_pair.pem"
 SSH_HOST="ubuntu@msens1.marinesensitivity.org"
 SSH_CMD="ssh -i \"$SSH_KEY\""

 # local paths ----
 DIR_V3="$HOME/My Drive/projects/msens/data/derived/v3/"
 DIR_BIG="$HOME/_big/msens/derived/v3/"
 DIR_SHARED="$HOME/My Drive/projects/msens/data/derived/r_bio-oracle_planarea.tif"

 # remote paths ----
 REMOTE_DERIVED="/share/data/derived"

 MODE="${1:-all}"

 # sync data ----
 if [[ "$MODE" == "all" || "$MODE" == "data" ]]; then
   echo "=== syncing derived/v3/ (small files) ==="
   rsync -avz --progress \
     -e "ssh -i \"$SSH_KEY\"" \
     --exclude='*.aux.xml' \
     "$DIR_V3" \
     "$SSH_HOST:$REMOTE_DERIVED/v3/"

   echo "=== syncing _big/v3/ (sdm.duckdb, parquet) ==="
   rsync -avz --progress \
     -e "ssh -i \"$SSH_KEY\"" \
     "$DIR_BIG" \
     "$SSH_HOST:$REMOTE_DERIVED/v3/"

   echo "=== syncing shared input raster ==="
   rsync -avz --progress \
     -e "ssh -i \"$SSH_KEY\"" \
     "$DIR_SHARED" \
     "$SSH_HOST:$REMOTE_DERIVED/"
 fi

 # pull app code ----
 if [[ "$MODE" == "all" || "$MODE" == "apps" ]]; then
   echo "=== pulling v3 apps on server ==="
   ssh -i "$SSH_KEY" "$SSH_HOST" bash -s <<'REMOTE'
     cd /srv/shiny-server
     for d in mapgl mapsp; do
       echo "--- $d ---"
       cd "$d"
       git fetch origin
       git checkout main
       git pull origin main
       cd ..
     done
     echo "=== done ==="
 REMOTE
 fi
 ```

 Notes:
 - rsync -avz = archive mode, verbose, compressed transfer
 - --progress shows per-file transfer progress (useful for big sdm.duckdb)
 - On the server, both small and big files land in /share/data/derived/v3/
 (no separate _big directory since server has everything colocated)
 - First run will transfer ~2.5GB (sdm.duckdb); subsequent runs only transfer
 changed files (rsync is incremental)
 - Add --dry-run flag to preview what would transfer without doing it
 - The script only deploys v3 (active version); v1/v2 are frozen

 Verification

 1. Locally: git checkout v1 in apps_v1/ — confirm file.exists() checks pass
 2. Locally: git checkout v2 in apps_v2/ — confirm paths resolve
 3. Locally: apps/ on main — confirm v3 app starts
 4. Server: restart each app pair, confirm correct v{N}/ data loads
 5. URLs: shiny.marinesensitivity.org/mapgl/ (v3), /mapgl_v1/ or /mapgl_2025/, /mapgl_v2/
 6. No app reads from flat derived/ except r_bio-oracle_planarea.tif

I was expecting to see just 3 apps repo clones on both server (first image) and laptop (2nd image): apps (main branch for v3 or latest working version),
  apps_v1, apps_v2. But that's not quite what I am seeing. Do I need to manually clean up or am I missing something?    

⏺ Clean on both sides now:        
  ┌───────────┬───────────────┬───────────────┬────────────────────┐
  │ Directory │ Laptop branch │ Server branch │      Symlinks      │                                                                                              
  ├───────────┼───────────────┼───────────────┼────────────────────┤
  │ apps/     │ main (v3)     │ main (v3)     │ mapgl, mapsp       │                                                                                              
  ├───────────┼───────────────┼───────────────┼────────────────────┤       
  │ apps_v1/  │ v1            │ v1            │ mapgl_v1, mapsp_v1 │
  ├───────────┼───────────────┼───────────────┼────────────────────┤
  │ apps_v2/  │ v2 (worktree) │ v2            │ mapgl_v2, mapsp_v2 │
  └───────────┴───────────────┴───────────────┴────────────────────┘

Since I want to have the main files on Google Drive for comparison, I copied the server's /share/data/derived/v2 to '~/My Drive/projects/msens/data/derived/v2'. I noticed that the sdm.duckdb is located in each of the /share/data/derived/v{#} folders, but not in the comprable '~/My Drive/projects/msens/data/derived/v{#}'. Will that persist with versions? And perhaps to prevent accidental copying for quick manual fixes later, we should create a seperate folder on the server /share/data/big to mimic laptop ~/_big/msens/data?

⏺ Done. Here's the final layout:     
 
  Server data:                       
  ┌───────────────────────────────────────────────┬───────────────────────────────────────────────────────────────┐
  │                     Path                      │                           Contents                            │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ /share/data/derived/v{1,2,3}/                 │ small files (gpkg, csv, tif) — safe to mirror on Google Drive │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ /share/data/big/v{1,2,3}/                     │ large files (sdm.duckdb ~2GB each) — mirrors laptop ~/_big/   │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ /share/data/derived/r_bio-oracle_planarea.tif │ shared input raster (all versions)                            │
  └───────────────────────────────────────────────┴───────────────────────────────────────────────────────────────┘
  App code (dir_big on all 3 branches):
  dir_big <- ifelse(
    is_server,
    glue("/share/data/big/{v_dir}"),
    glue("~/_big/msens/derived/{v_dir}"))

  This way if you copy /share/data/derived/v2/ to Google Drive, you won't accidentally grab the 2GB duckdb — it lives separately in /share/data/big/v2/.


## 2026-02-12 sensibly restructure output files in derived/, _big files outside Google Drive and v1/ v3/ versions within

In the process of trying to make files distinguishable from past versions and creating extra copies in subdirectories, I have confused myself as to which files to use. For example, here are some of the seeming same files for ply_programareas_*.gpkg:

@~/My Drive/projects/msens/data/derived:
- ply_programareas_2026_no-metrics.gpkg # before binding metric columns
- ply_programareas_2026_v3.gpkg
- ply_programareas_2026.gpkg
- ply_programareas_v3.gpkg

@~/My Drive/projects/msens/data/derived/downloads:
- ply_programareas_2026_v3.gpkg

The logic behind keeping `_2026` is that represents the input data, and `_v3` the overall output version of the scoring model, so the preferred winner is `ply_programareas_2026_v3.gpkg`.

There are also many more files in @~/My Drive/projects/msens/data/derived/downloads/* copied from the github versioned @workflows/figs/msens-summary_programareas_v3/*, which is generated by `@msens_summary_programareas.qmd`.

The culprit workflows are:

- `@calc_scores.qmd`
- `@render_derived_products.qmd`
- `@msens_summary_programareas.qmd`

All files under @~/My Drive/ are synchronized with Google Drive (but not versioned) for making available to end users.

And then files in @~/My Drive/projects/msens/data/derived also get used by @../apps_2026/mapgl/ and @../apps_2026/mapsp/, so need to get uploaded into the server directory at `/share/data/derived`.

Finally, I am having issues with Google Drive attempting and failing to synchronize large *.duckdb and *.parquet files under @~/My Drive, which do not need to be synchronized there in the first place. So I created a Mac alias at @~/My Drive/projects/msens/data/derived/_derived_big_files which goes to @~/big-files_gdrive-notsync/msens/derived, outside Google Drive's sync folder of @~/My Drive/.

I would also like to maintain backword compatibility with `v1` (sdm.duckdb), `v2` (sdm_2026.duckdb) and `v3` (sdm_v3.duckdb) of the @../apps/{mapgl|mapsp} (now on main branch for v1 on laptop and server). What advice do you have on maintaining backwards compatability with the apps at different iterations? Seems like I should:

- `v1`: rename @../apps to @../apps_v1 with a fork from "main" to branch "v1", 
- `v2`: copy @../apps_2026 to @../apps_v2 and rollback to commits predating "v3" and fork to branch "v2"
- `v3`: rename @../apps_2026 to @../apps and merge current branch "programareas_2026" to branch "main"

Can you help me make better sense of this, given the various purposes, draft a plan forward with fewer copies, implement edits in offending files and produce a quick fix script to get going for now.

It seems most sensible to seperate outputs as version number subfolders (eg v1, v2, v3) under @~/My Drive/projects/msens/data/derived/.

For files without obvious year (and possibly later year_month) inputs, just use overall model version:

- rast_metrics_v3.tif 
- rast_metrics_programareas_v3.tif
- layers_v3.csv
- zone_taxon_v3.csv

Do you agree this is a good?

⏺ The plan looks consistent and complete. Let me verify one last thing — that the mapgl app's layers_2026_v3.csv reference will be fixed correctly.               
                                                                               
  The mapgl v3 app currently has lyrs_csv <- glue("{dir_data}/derived/layers_2026{v_sfx}.csv") which resolves to layers_2026_v3.csv. In the plan, this changes to 
  lyrs_csv <- glue("{dir_v}/layers{v_sfx}.csv") → v3/layers_v3.csv. The migration renames layers_2026_v3.csv → v3/layers_v3.csv. Consistent.
                                                                                                                                                                  
  The mapsp v3 app's mask_tif currently has r_metrics{v_sfx}.tif → r_metrics_v3.tif. In the plan this becomes {dir_v}/r_metrics{v_sfx}.tif → v3/r_metrics_v3.tif. 
  The migration renames r_metrics_2026_v3.tif → v3/r_metrics_v3.tif. Consistent.

 Plan: Restructure derived data into version subfolders                        

 Context

 Output files from calc_scores.qmd, msens-summary_programareas.qmd, and render_derived_products.qmd have accumulated in a flat derived/ directory with
 inconsistent naming conventions. Key problems:

 1. Naming mismatches: calc_scores.qmd produces ply_programareas_v3.gpkg but consumers expect ply_programareas_2026_v3.gpkg. Similarly rast_metrics_v3.tif vs
 r_metrics_2026_v3.tif.
 2. Duplicate files: Multiple copies at different name variants (e.g., 4 gpkg files for program areas alone).
 3. Large files in Google Drive: sdm_v3.duckdb (2.1G), spp.duckdb (4.4G) cause sync failures.
 4. No version separation: v1, v2, v3 files mixed together in one directory.
 5. MacOS alias at _derived_big_files is not a Unix symlink — R cannot follow it.

 Design Decisions (from user)

 - Version subfolders: derived/v1/, derived/v3/ etc.
 - File naming: Spatial inputs keep year + version suffix (e.g., ply_programareas_2026_v3.gpkg) since the year reflects input data vintage. Computed outputs
 without an obvious year just use the model version (e.g., r_metrics_v3.tif, layers_v3.csv, zone_taxon_v3.csv). All files keep the version suffix so they remain
 identifiable when shared independently of their folder.
 - Big files: Use version subfolders at ~/big-files_gdrive-notsync/msens/derived/v3/ with NO version suffix (internal-only files).
 - Apps: All three versions (v1, v2, v3) must run simultaneously on the server.
 - Scope: Full restructure now; past workflow commits need not be reproducible, but v3+ must work.

 Target Directory Structure

 Google Drive (small, sharable files)

 ~/My Drive/projects/msens/data/derived/
 ├── r_bio-oracle_planarea.tif          # shared input (all versions)
 ├── gebco_depth*.tif                   # shared input
 ├── v1/                                # legacy outputs
 │   ├── ply_planareas_2025.gpkg
 │   ├── ply_ecoregions_2025.gpkg
 │   ├── ply_subregions_2025.gpkg
 │   ├── layers.csv
 │   ├── r_metrics.tif
 │   ├── r_metrics_akl48.tif
 │   ├── zone_taxon.csv
 │   ├── zone_taxon_akl48.csv
 │   ├── subregion_planareas.csv
 │   └── downloads/                     # end-user downloads
 ├── v3/                                # current outputs
 │   ├── ply_programareas_2026_v3.gpkg
 │   ├── ply_ecoregions_2025_v3.gpkg
 │   ├── ply_subregions_2026_v3.gpkg
 │   ├── ply_ecoregion_programarea_v3.gpkg
 │   ├── ply_ecoregion_programarea_v3.csv
 │   ├── layers_v3.csv
 │   ├── r_metrics_v3.tif
 │   ├── r_metrics_programareas_v3.tif
 │   ├── zone_taxon_v3.csv
 │   ├── subregion_programareas.csv
 │   ├── ply_label_placement_pra.csv
 │   ├── tbl_er_pra_scores_v3.csv
 │   ├── tbl_pra_scores_v3.csv
 │   └── downloads/
 │       ├── (gpkg, csv copies)
 │       └── figures/
 └── (stale files to clean up — see Phase 3)

 Big files (outside Google Drive)

 ~/big-files_gdrive-notsync/msens/derived/
 ├── v1/
 │   └── sdm.duckdb                     # moved from derived/sdm.duckdb
 ├── v3/
 │   └── sdm.duckdb                     # moved from derived/sdm_v3.duckdb
 │   └── sdm_parquet/                   # optional parquet exports
 └── sdm_v3_2026-02-12.duckdb          # dated backup (already there)

 Server (/share/data/derived/)

 Same structure but everything (big + small) in one tree:
 /share/data/derived/
 ├── r_bio-oracle_planarea.tif
 ├── v1/
 │   ├── sdm.duckdb
 │   ├── ply_planareas_2025.gpkg
 │   └── ...
 ├── v3/
 │   ├── sdm.duckdb
 │   ├── ply_programareas_2026_v3.gpkg
 │   ├── r_metrics_v3.tif
 │   └── ...

 Standardized File Names

 Naming rule: Spatial inputs with a specific data vintage keep the year (e.g., _2026, _2025). Computed outputs use version only.
 ┌────────────────────────────────────┬───────────────────────────────┬───────────────────────────────────┐
 │ calc_scores.qmd currently produces │  Standardized name (in v3/)   │              Reason               │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ ply_programareas_v3.gpkg           │ ply_programareas_2026_v3.gpkg │ BOEM 2026 input                   │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ ply_ecoregions_v3.gpkg             │ ply_ecoregions_2025_v3.gpkg   │ MEOW 2025 input                   │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ ply_subregions_v3.gpkg             │ ply_subregions_2026_v3.gpkg   │ 2026 boundaries                   │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ rast_metrics_v3.tif                │ r_metrics_v3.tif              │ computed output                   │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ rast_metrics_programareas_v3.tif   │ r_metrics_programareas_v3.tif │ computed output                   │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ layers_v3.csv                      │ layers_v3.csv                 │ computed output (already correct) │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ zone_taxon_v3.csv                  │ zone_taxon_v3.csv             │ computed output (already correct) │
 ├────────────────────────────────────┼───────────────────────────────┼───────────────────────────────────┤
 │ sdm_v3.duckdb (in derived/)        │ sdm.duckdb (in big-files/v3/) │ internal big file                 │
 └────────────────────────────────────┴───────────────────────────────┴───────────────────────────────────┘
 Also: mapsp v3 currently references r_metrics{v_sfx}.tif → maps correctly to r_metrics_v3.tif (already OK with new naming).

 Implementation

 Phase 1: Update path variables in calc_scores.qmd (~line 85-125)

 Add v_dir and dir_big to the setup chunk. Change all output paths to use dir_v with standardized names.

 `# version ----
 v_int <- 3
 v_sfx <- paste0("_v", v_int)
 v_dir <- paste0("v", v_int)

 `# versioned output directories
 dir_v   <- glue("{dir_derived}/{v_dir}")
 dir_big <- ifelse(
   is_server,
   dir_v,                                                       # server: all together
   glue("~/big-files_gdrive-notsync/msens/derived/{v_dir}"))    # local: big files outside GDrive

 `# inputs (shared, unversioned)
 cell_tif     <- glue("{dir_derived}/r_bio-oracle_planarea.tif")
 pra_raw_gpkg <- glue("{dir_raw}/boem.gov/boem_program_areas_2025-11.gpkg")

 `# big files (duckdb — no version suffix in folder)
 sdm_db <- glue("{dir_big}/sdm.duckdb")

 `# spatial outputs (year + version suffix — reflects input data vintage)
 pra_gpkg        <- glue("{dir_v}/ply_programareas_2026{v_sfx}.gpkg")
 er_gpkg         <- glue("{dir_v}/ply_ecoregions_2025{v_sfx}.gpkg")
 sr_gpkg         <- glue("{dir_v}/ply_subregions_2026{v_sfx}.gpkg")

 `# computed outputs (version suffix only — no obvious input year)
 sr_pra_csv      <- glue("{dir_v}/subregion_programareas.csv")
 metrics_tif     <- glue("{dir_v}/r_metrics{v_sfx}.tif")
 metrics_pra_tif <- glue("{dir_v}/r_metrics_programareas{v_sfx}.tif")
 zone_taxon_csv  <- glue("{dir_v}/zone_taxon{v_sfx}.csv")
 lyrs_csv        <- glue("{dir_v}/layers{v_sfx}.csv")

 `# parquet (big files)
 dir_pq <- glue("{dir_big}/sdm_parquet")

 Also add dir_create(dir_v) and dir_create(dir_big) near the top so directories exist.

 The pa_gpkg input (plan areas used to derive program areas) can stay as-is since it's an input from a prior step. If it gets generated into v3/, update its path
  too:
 pa_gpkg <- glue("{dir_v}/ply_planareas_2025{v_sfx}.gpkg")

 Phase 2: Update path variables in msens-summary_programareas.qmd (~lines 65-90)

 Same pattern — add v_dir, dir_v, dir_big. Update all file paths:

 v_dir       <- paste0("v", v_int)
 dir_v       <- glue("{dir_derived}/{v_dir}")
 dir_big     <- ifelse(is_server, dir_v,
   glue("~/big-files_gdrive-notsync/msens/derived/{v_dir}"))

 sdm_dd      <- glue("{dir_big}/sdm.duckdb")
 pra_gpkg    <- glue("{dir_v}/ply_programareas_2026{v_sfx}.gpkg")
 er_gpkg     <- glue("{dir_v}/ply_ecoregions_2025{v_sfx}.gpkg")
 metrics_tif <- glue("{dir_v}/r_metrics{v_sfx}.tif")
 sr_pra_csv  <- glue("{dir_v}/subregion_programareas.csv")
 er_pra_gpkg <- glue("{dir_v}/ply_ecoregion_programarea{v_sfx}.gpkg")
 er_pra_csv  <- glue("{dir_v}/ply_ecoregion_programarea{v_sfx}.csv")

 Also update dir_dl and dir_dl_figs:
 dir_dl      <- glue("{dir_v}/downloads")
 dir_dl_figs <- glue("{dir_dl}/figures")

 And the new output CSVs generated by this notebook:
 er_pra_n_csv <- glue("{dir_v}/tbl_er_pra_scores{v_sfx}.csv")
 pra_n_csv    <- glue("{dir_v}/tbl_pra_scores{v_sfx}.csv")
 lbls_csv     <- glue("{dir_v}/ply_label_placement_pra.csv")

 Phase 3: Update path variables in render_derived_products.qmd

 Update directory references to use v3/ subfolder:
 dir_v   <- glue("{dir_derived}/v3")
 dir_dl  <- glue("{dir_v}/downloads")

 Update all file references in the copy operations to use dir_v as the source.

 Phase 4: Update apps_2026/{mapgl,mapsp}/app.R (v3 apps)

 Add v_dir, dir_v, dir_big variables. Update all file paths.

 apps_2026/mapgl/app.R (~lines 48-72):
 v_int <- 3
 v_sfx <- paste0("_v", v_int)
 v_dir <- paste0("v", v_int)
 dir_v   <- glue("{dir_data}/derived/{v_dir}")
 dir_big <- ifelse(is_server, dir_v,
   glue("~/big-files_gdrive-notsync/msens/derived/{v_dir}"))

 sdm_db      <- glue("{dir_big}/sdm.duckdb")
 cell_tif    <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")  # shared
 er_gpkg     <- glue("{dir_v}/ply_ecoregions_2025{v_sfx}.gpkg")
 pra_gpkg    <- glue("{dir_v}/ply_programareas_2026{v_sfx}.gpkg")
 lyrs_csv    <- glue("{dir_v}/layers{v_sfx}.csv")
 metrics_tif <- glue("{dir_v}/r_metrics{v_sfx}.tif")
 sr_pra_csv  <- glue("{dir_v}/subregion_programareas.csv")

 apps_2026/mapsp/app.R (~lines 33-50):
 v_int <- 3
 v_sfx <- paste0("_v", v_int)
 v_dir <- paste0("v", v_int)
 dir_v   <- glue("{dir_data}/derived/{v_dir}")
 dir_big <- ifelse(is_server, dir_v,
   glue("~/big-files_gdrive-notsync/msens/derived/{v_dir}"))

 sdm_db   <- glue("{dir_big}/sdm.duckdb")
 cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
 mask_tif <- glue("{dir_v}/r_metrics{v_sfx}.tif")

 Phase 5: Update apps/{mapgl,mapsp}/app.R (v1 apps)

 Add version folder support so they read from derived/v1/:

 apps/mapgl/app.R (~lines 45-66):
 v_dir      <- "v1"
 dir_v      <- glue("{dir_data}/derived/{v_dir}")
 dir_big    <- ifelse(is_server, dir_v,
   glue("~/big-files_gdrive-notsync/msens/derived/{v_dir}"))

 sdm_db     <- glue("{dir_big}/sdm.duckdb")
 cell_tif   <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
 pa_gpkg    <- glue("{dir_v}/ply_planareas_2025.gpkg")
 er_gpkg    <- glue("{dir_v}/ply_ecoregions_2025.gpkg")
 lyrs_csv   <- glue("{dir_v}/layers.csv")
 metrics_tif <- glue("{dir_v}/r_metrics.tif")
 sr_gpkg    <- glue("{dir_v}/ply_subregions_2025.gpkg")
 sr_pa_csv  <- glue("{dir_v}/subregion_planareas.csv")

 apps/mapsp/app.R (~lines 30-45):
 v_dir    <- "v1"
 dir_v    <- glue("{dir_data}/derived/{v_dir}")
 dir_big  <- ifelse(is_server, dir_v,
   glue("~/big-files_gdrive-notsync/msens/derived/{v_dir}"))

 sdm_db   <- glue("{dir_big}/sdm.duckdb")
 cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")

 Phase 6: Migration script (R)

 Create workflows/migrate_derived_to_versions.R — a one-time script to:

 1. Create derived/v1/, derived/v3/, big-files/v1/, big-files/v3/ directories
 2. Move v1 files from derived/ to derived/v1/
 3. Move v3 files from derived/ to derived/v3/ (renaming where needed)
 4. Move big files from derived/ to big-files/v{N}/
 5. Move derived/downloads/ contents into derived/v1/downloads/ (old) and derived/v3/downloads/ (new)
 6. Replace the MacOS alias _derived_big_files with a proper Unix symlink
 7. List stale/intermediate files for manual review before deletion

 v1 file moves (from derived/ → derived/v1/):
 - ply_planareas_2025.gpkg, ply_ecoregions_2025.gpkg, ply_subregions_2025.gpkg
 - layers_1.csv → v1/layers.csv
 - r_metrics_akl48.tif (+.aux.xml), r_metrics.tif.aux.xml
 - zone_taxon.csv, zone_taxon_akl48.csv
 - subregion_planareas.csv
 - sdm.duckdb → big-files/v1/sdm.duckdb

 v3 file moves (from derived/ → derived/v3/):
 - ply_programareas_2026_v3.gpkg, ply_ecoregions_2025_v3.gpkg
 - ply_ecoregion_programarea_v3.{gpkg,csv}
 - r_metrics_2026_v3.tif → v3/r_metrics_v3.tif (drop _2026, computed output)
 - r_metrics_akl48_v3.tif → v3/r_metrics_programareas_v3.tif (rename for consistency)
 - layers_2026_v3.csv → v3/layers_v3.csv (drop _2026)
 - zone_taxon_2026_v3.csv → v3/zone_taxon_v3.csv (drop _2026)
 - subregion_programareas.csv → v3/subregion_programareas.csv
 - ply_label_placement_pra.csv, tbl_er_pra_scores_v3.csv, tbl_pra_scores_v3.csv
 - sdm_v3.duckdb → big-files/v3/sdm.duckdb

 Stale files to clean up (review manually, then delete):
 - ply_programareas_v3.gpkg (wrong name, superseded)
 - ply_programareas_2026.gpkg (v2 era)
 - ply_programareas_2026_no-metrics.gpkg (intermediate)
 - layers_v3.csv (will be regenerated in v3/ folder)
 - layers_2026.csv (v2 era)
 - rast_metrics_v3.tif, rast_metrics_programareas_v3.tif (wrong prefix, superseded)
 - r_metrics_2026.tif (v2 era)
 - Dated backup files: *_2025-06-18.*, *_2025-06-27.*
 - spp.duckdb (4.4G — move to big-files if still needed)
 - msens_sdm.duckdb (tiny, likely stale)

 Phase 7: Server migration (manual, documented)

 Document steps for replicating the structure on the server at /share/data/derived/:
 1. Create v1/ and v3/ directories
 2. Move/copy files matching the local structure
 3. Upload sdm.duckdb to v3/ on server (scp or rsync)
 4. Update any Shiny Server config if app paths changed

 Files to Modify
 ┌──────────────────────────────────────────────┬─────────────────────────────────────────────────────────────┐
 │                     File                     │                           Changes                           │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd                    │ Update ~15 path variables in setup chunk (~lines 85-125)    │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ workflows/msens-summary_programareas.qmd     │ Update ~12 path variables in setup chunk (~lines 65-90)     │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ workflows/render_derived_products.qmd        │ Update directory references and copy paths                  │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ apps_2026/mapgl/app.R                        │ Update ~8 path variables (~lines 48-72)                     │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ apps_2026/mapsp/app.R                        │ Update ~4 path variables (~lines 33-50); fix r_metrics name │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ apps/mapgl/app.R                             │ Update ~8 path variables to use v1/ (~lines 45-66)          │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ apps/mapsp/app.R                             │ Update ~3 path variables to use v1/ (~lines 30-45)          │
 ├──────────────────────────────────────────────┼─────────────────────────────────────────────────────────────┤
 │ NEW: workflows/migrate_derived_to_versions.R │ One-time migration script                                   │
 └──────────────────────────────────────────────┴─────────────────────────────────────────────────────────────┘
 Git Strategy (follow-up, not part of this implementation)

 For simultaneous v1/v3 deployment on server:
 - Option A: Two separate clones — one on v1 branch, one on main
 - Option B: Keep apps/ (v1) and apps_v3/ (v3) directories in same branch
 - Recommend discussing after file restructure is complete and tested

 Verification

 1. Run migrate_derived_to_versions.R locally — confirm files move correctly
 2. Verify derived/v3/ contains all expected files with correct names
 3. Verify big-files/v3/sdm.duckdb exists and is readable
 4. Open calc_scores.qmd and run the setup chunk — confirm all file.exists() checks pass
 5. Open msens-summary_programareas.qmd and run the setup — confirm inputs resolve
 6. Run apps_2026/mapgl/app.R locally — confirm app starts and loads data
 7. Run apps_2026/mapsp/app.R locally — confirm app starts
 8. Confirm sdm_v3.duckdb no longer in Google Drive sync path (check ~/My Drive/projects/msens/data/derived/ for absence of large duckdb files)

⏺ Everything looks consistent. Let me provide a summary of all changes made.                                                                                      
                                                                                                                                                                  
  Summary of all changes                                                                                                                                          
                                                                                                                                                                  
  Phase 1: workflows/calc_scores.qmd                                                                                                                              
                                                                                                                                                                  
  - Added v_dir, dir_v, dir_big, dir_pq variables in setup chunk
  - Added dir_create(dir_v) and dir_create(dir_big) to ensure directories exist
  - sdm_db now points to {dir_big}/sdm.duckdb (no version suffix)
  - pa_gpkg now under dir_v
  - All spatial outputs (pra_gpkg, er_gpkg, sr_gpkg) use {dir_v}/...2026{v_sfx}.gpkg naming
  - Computed outputs (metrics_tif, lyrs_csv, etc.) use {dir_v}/...{v_sfx}.* (no year)
  - rast_metrics renamed to r_metrics
  - Parquet export/import chunks now reference dir_pq from setup instead of redefining

  Phase 2: workflows/msens-summary_programareas.qmd

  - Added v_dir, dir_v, dir_big, dir_dl_figs in setup
  - dir_dl now under dir_v ({dir_v}/downloads)
  - sdm_dd → {dir_big}/sdm.duckdb
  - metrics_tif → {dir_v}/r_metrics{v_sfx}.tif (dropped _2026)
  - All outputs under dir_v
  - lbls_csv updated to use dir_v
  - Removed duplicate dir_dl_figs definition from copy_to_downloads chunk

  Phase 3: workflows/render_derived_products.qmd

  - All chunks now use dir_v = derived/v3 as base
  - Copy sources reference dir_v not dir_derived
  - File names updated: zone_taxon_v3.csv, layers_v3.csv, r_metrics_programareas_v3.tif
  - Verify chunk checks dir_v instead of flat dir_derived
  - Key outputs list updated to show v3/ prefix

  Phase 4: apps_2026/mapgl/app.R and apps_2026/mapsp/app.R

  - Added v_dir, dir_v, dir_big
  - sdm_db → {dir_big}/sdm.duckdb
  - All data files now read from dir_v with standardized names
  - lyrs_csv dropped _2026, metrics_tif dropped _2026

  Phase 5: apps/mapgl/app.R and apps/mapsp/app.R (v1)

  - Added v_dir = "v1", dir_v, dir_big
  - sdm_db → {dir_big}/sdm.duckdb
  - All data files now read from derived/v1/ with unversioned names

  Phase 6: workflows/migrate_derived_to_versions.R (new)

  - Creates v1/, v3/ directories in both derived/ and big-files/
  - Moves v1 files (gpkg, csv, tif) to derived/v1/
  - Moves v3 files to derived/v3/ with renames (drop _2026, rast_ → r_, akl48 → programareas)
  - Moves sdm.duckdb → big-files/v1/, sdm_v3.duckdb → big-files/v3/sdm.duckdb
  - Moves sdm_parquet/ to big-files/v3/
  - Replaces macOS alias with Unix symlink
  - Lists stale/intermediate files for manual cleanup

In @render_derived_products.qmd, use version variables like in @calc_scores.qmd:
```r
v_int <- 3
v_sfx <- paste0("_v", v_int)
v_dir <- paste0("v", v_int)
```
With @msens-summary_programareas.qmd and other *.qmd and @migrate_derived_to_versions.R, let's not populate seperate `downloads` (`dir_dl`) and `figures` (`dir_figs`) subfolders. Let's simply dump all output files into `dir_v`.
Change symlink from `_derived_big_files` to simply `_big`.

⏺ All three changes done. Here's a summary:                                                                                                                       
                                                                                
  Changes made                                                                                                                                                    
                                                                                
  1. render_derived_products.qmd — Rewrote to use version variables (v_int, v_sfx, v_dir) instead of hardcoded "v3"/"_v3". Removed Step 1 (archive old downloads) 
  and Step 3 (copy to downloads) since all outputs now go directly into dir_v. Simplified from 5 steps to 3.
  2. msens-summary_programareas.qmd — Removed dir_dl, dir_dl_figs from setup. Replaced the "Copy to Downloads" section with "Copy figures to dir_v" — data files
  are already written to dir_v directly, so only figures (generated in the local figs/ repo dir) need copying.
  3. migrate_derived_to_versions.R — Removed downloads and downloads/figures from directory creation. Changed symlink from _derived_big_files to _big. Removed
  unused safe_copy helper.

⏺ All phases complete. Here's a summary of what was done:
                                                        
  Summary                                                                                                                                                         
                                                       
  Phase 1 — v1 branch: Committed uncommitted v1 path changes in apps/, created v1 branch, pushed to origin.                                                       
                                                                                                                                                                  
  Phase 2 — v2 branch: Created v2 branch from commit 86448d1, updated mapgl/app.R and mapsp/app.R paths to use derived/v2/ with dir_v/dir_big pattern, committed  
  and pushed.                                                                                                                                                     
                                                                                                                                                                  
  Phase 3 — Merge: Merged programareas_2026 into main in apps_2026/, resolved conflicts keeping the programareas_2026 (v3) version, pushed.                       
                                                                                                                                                                  
  Phase 4 — Local clones:                                                                                                                                         
  apps/     → main   (v3, active development)
  apps_v1/  → v1     (frozen)                                                                                                                                     
  apps_v2/  → v2     (worktree, frozen)                                                                                                                           
                                                                                                                                                                  
  Phase 5 — Server data: Created v1/, v2/, v3/ subdirectories under /share/data/derived/ and moved all files into their versioned directories. Stale/dated files  
  remain in the root for later cleanup.                                                                                                                           
                                                                                                                                                                  
  Phase 6 — Server apps + symlinks:                                                                                                                               
  - apps_v3 → main (was programareas_2026)
  - apps → v1 (was main)
  - apps_2026 → v2 (was programareas_2026)
  - Renamed mapgl_2025/mapsp_2025 symlinks to mapgl_v1/mapsp_v1

  Phase 8 — Deploy script: Created workflows/deploy_v3.sh with rsync for data + git pull for apps, adapted to actual server paths
  (/share/github/MarineSensitivity/apps_v3/).

  Note: Phase 7 (server data READMEs) was not explicitly executed — you may want to scp those from Google Drive separately. Also, you'll want to verify the apps
  are working at their URLs before cleaning up the stale files.


## 2026-02-12 inspect after `calc_scores.qmd` cleanup

I just did a cleanup of @calc_scores.qmd. I'm concerned there may be typos or inconsistent logic. Please do an extensive review. 
Suggest any improvements for clarity.

⏺ All changes look correct. Here's a summary of all 15 fixes applied:
  Critical Bugs Fixed:                                                                                                                                            
  1. delete_extrisk_metrics: Changed mutate() to filter() — was pulling ALL metric_seqs instead of just extrisk ones
  2. calc_ecoregion_minmax: Changed {m_seq} to {existing_seq} in 3 delete statements — was using undefined/stale variable                                         
  3. primprod_ecoregion_avg_stddev: Changed m_seq to existing_seq in 3 delete statements — same bug pattern
  4. create_pg_ply_programareas_2026: Changed {tbl} to {tbl_pra} in 3 glue calls — undefined variable
  5. create_pg_v3_tables: Fixed double _v3_v3 suffix by deriving base name with str_remove(pg_tbl, v_sfx)
  6. subregion_programareas_lookup: Fixed get_sr_bbox() <- to get_sr_bbox <- and sr to d_sr_pra

  Logic Issues Fixed:
  7. Hardcoded 'ply_subregions_2026' → glue("...'{tbl_sr}'")
  8. Verification join: "ply_planareas_2025" → !!tbl_pra
  9. zone_taxon: "ply_subregions_2025"/"AKL48" → !!tbl_sr/"USA"
  10. r[["primprod_log"]] → r_metrics[["primprod_log"]]

  Typos & Cosmetic:
  11. Regex: removed extra ] in (min|max])
  12. Fixed "marinesensivity" typo (in fix #4)
  13. Fixed 8 misleading comments (planning areas→ecoregions, ecoregion→planning area, planning area→program area, etc.)
  14. Checked off stale "fix reptile" TODO
  15. Removed stale # file_delete(sdm_dd), unused yr <- 2023, stale "below" comment, updated outdated ply_planareas_2025 comment



## 2026-02-11.f _v3 for outputs, not inputs

I edited  @workflows/calc_scores.qmd so essential chunks should evaluate (default `eval: true`). But before running I realize that the version suffix (`{v_sfx} = "_v3"`) shouldn't have been applied to inputs, like `zone.tbl` values:

```r
tbl(con_sdm, "zone") |>
  pull(tbl) |> 
  table()
# ply_ecoregions_2025_v3    ply_planareas_2025_v3 ply_programareas_2026_v3   ply_subregions_2025_v3   ply_subregions_2026_v3 
#                     12                       36                       20                        4                        4 
```

The version suffix should however continue to be applied to the outputs, like the 




The zones: zone.tbl = ply_programareas_2026_v3 -> ply_programareas_2026; 

## 2026-02-11.e reptiles -> turtles

I am confused why there are still reptiles in the database? I thought I fixed this with the `update_turtle_to_reptile` R chunk in @workflows/merge_models.qmd.

```r
tbl(con_sdm, "species") |>
  filter(sp_cat %in% c("reptile", "turtle")) |>
  count(sp_cat)
#   sp_cat      n
#   <chr>   <dbl>
# 1 turtle      2
# 2 reptile    58

tbl(con_sdm, "taxon") |>
  filter(sp_cat %in% c("reptile", "turtle")) |>
  count(sp_cat)
#   sp_cat      n
#   <chr>   <dbl>
# 1 reptile    30
# 2 turtle      2

tbl(con_sdm, "taxon") |>
  filter(
    is_ok,
    sp_cat %in% c("reptile", "turtle")
  ) |>
  count(sp_cat)
#   sp_cat      n
#   <chr>   <dbl>
# 1 turtle      2
# 2 reptile     9
```

I need to fix this now and forever (so where in reproducible workflow should this appear?), plus determine if I need to re-run @workflows/calc_scores.qmd (assume yes).


## 2026-02-11.d mapsp: mask to program areas

## 2026-02-11.c mapgl,mapsp: rl_code|score -> er_code|score, + is_mmpa, is_mbta

Since we've now explicitly transitioned to a more generalized extinction risk code and score (`er_code`, `er_score`) that includes and favors US national ESA listings from NMFS and FWS over the international IUCN RedList code and score (`rl_code`, `rl_score`), update the @apps_2026/mapgl/app.R accordingly. Rename the table outputs and update the "Species table information".

Looks good, except the er_score is not additive. I reworded er_score to align with msens::compute_er_score() from @msens/ `er_score` extinction risk score (1-100%): derived from the max of extinction risk      
  codes (`NMFS|FWS:EN=100`, `NMFS|FWS:TN=50`, `IUCN:CR=50`, `IUCN:EN=25`, `IUCN:VU=5`, ``IUCN:NT=2`, `IUCN:LC|DD=1`) and if protected under MMPA (20) or MBTA (10). Now add `is_mmpa` and `is_mbta` to the      
  table

## 2026-02-11.b why not properly masked Alaska?

Why are the map_cell_[score|npp]_ak_v3.png outputs generatedy by @workflows/msens-summary_programareas.qmd not properly masked to the Alaska study area (whereas it is properly masked in the app @apps_2026/mapsp/app.R - area to be masked circled in pink)? Can this be easily fixed? Would the excess cell values have affected the scores (@workflows/calc_scores.qmd)?

~/Downloads/mapgl_alaska.png
~/My Drive/projects/msens/data/derived/downloads/figures/map_cell_score_ak_v3.png
~/My Drive/projects/msens/data/derived/downloads/figures/map_cell_npp_ak_v3.png

Ok, the fix for msens-summary_programareas.qmd makes good sense. I don't quite understand if calc_scores.qmd has the same issue -- if so, it should be fixed.
  If it's just about the slight bleeding of touching cells, then no worries.   

Doesn't this line to continue the problem: `mask(vect(er |> filter(region_key == "AK")))`? The Ecoregions (`er`) are much bigger and more inclusive than the
  Program Areas (pra) which have the proper areas masked, which is why this second masking in the file is correct: `mask(vect(pra |> filter(region_key !=         
  "AK")))`

╭─────────────────────────────────────────────────────────────────────────────╮
│ Plan to implement                                                           │
│                                                                             │
│ Fix raster masking in msens-summary_programareas.qmd                        │
│                                                                             │
│ Context                                                                     │
│                                                                             │
│ The map_imgs_cell_l48_ak chunk produces 4 cell-level map PNGs where raster  │
│ data visibly bleeds beyond polygon outlines. The mask uses rasterized zone  │
│ layers from r_metrics (created with rasterize(..., touches = T)), which     │
│ include cells that merely touch polygon edges. The scoring pipeline         │
│ (calc_scores.qmd) is unaffected because zonal stats are weighted by         │
│ pct_covered.                                                                │
│                                                                             │
│ File to modify                                                              │
│                                                                             │
│ workflows/msens-summary_programareas.qmd — map_imgs_cell_l48_ak chunk only  │
│ (lines 493–723)                                                             │
│                                                                             │
│ Changes                                                                     │
│                                                                             │
│ 1. Global mask (lines 513–522)                                              │
│                                                                             │
│ Replace raster-based mask with polygon-based mask:                          │
│                                                                             │
│ # before                                                                    │
│ r_mask <- (r_metrics$programarea_key %in% pra_l48) |                        │
│   (r_metrics$ecoregion_key %in% er_ak)                                      │
│ r_score <- r_metrics[[lyr_score]] |>                                        │
│   mask(r_mask, maskvalues = F)                                              │
│ r_npp <- r_vgpm |>                                                          │
│   log() |>                                                                  │
│   mask(r_mask, maskvalues = F)                                              │
│                                                                             │
│ # after                                                                     │
│ ply_mask <- vect(bind_rows(                                                 │
│   pra |> filter(region_key != "AK"),                                        │
│   er  |> filter(region_key == "AK")                                         │
│ ))                                                                          │
│ r_score <- r_metrics[[lyr_score]] |> mask(ply_mask)                         │
│ r_npp   <- r_vgpm |> log() |> mask(ply_mask)                                │
│                                                                             │
│ 2. Alaska branch (lines 552–554)                                            │
│                                                                             │
│ # before                                                                    │
│ r_area <- r |>                                                              │
│   mask(r_metrics$ecoregion_key %in% er_ak, maskvalues = F) |>               │
│   trim()                                                                    │
│                                                                             │
│ # after                                                                     │
│ r_area <- r |>                                                              │
│   mask(vect(er |> filter(region_key == "AK"))) |>                           │
│   trim()                                                                    │
│                                                                             │
│ 3. Lower-48 branch (lines 580–582)                                          │
│                                                                             │
│ # before                                                                    │
│ r_area <- r |>                                                              │
│   mask(r_metrics$programarea_key %in% pra_l48, maskvalues = F) |>           │
│   trim()                                                                    │
│                                                                             │
│ # after                                                                     │
│ r_area <- r |>                                                              │
│   mask(vect(pra |> filter(region_key != "AK"))) |>                          │
│   trim()                                                                    │
│                                                                             │
│ Affected outputs (all 4 cell-level maps)                                    │
│                                                                             │
│ - map_cell_score_ak_v3.png                                                  │
│ - map_cell_npp_ak_v3.png                                                    │
│ - map_cell_score_l48_v3.png                                                 │
│ - map_cell_npp_l48_v3.png                                                   │
│                                                                             │
│ Not affected                                                                │
│                                                                             │
│ - Polygon-based maps (map_pra_ak_v3.png, map_pra_l48_v3.png) — use vector   │
│ tiles                                                                       │
│ - Scores in calc_scores.qmd — weighted by pct_covered, boundary cells       │
│ contribute proportionally                                                   │
│                                                                             │
│ Verification                                                                │
│                                                                             │
│ Re-render the map_imgs_cell_l48_ak chunk and confirm raster data is clipped │
│  to polygon boundaries in all 4 PNGs.                                       │
╰─────────────────────────────────────────────────────────────────────────────

## 2026-02-11 duplicates in taxon_model

When running @apps_2026/mapsp/app.R I discovered that there are duplicates in the taxon_model table of the SDM db. There should be only one single model (`mdl_seq`) per `taxon_id` and `ds_key`.

```r
# duplicates
d_tm_dups <- tbl(con_sdm, "taxon_model") |>
  group_by(taxon_id, ds_key) |>
  summarize(
    n = n(),
    mdl_seqs = stringr::str_flatten(mdl_seq, collapse = ","),
    .groups = "drop"
  ) |>
  filter(n > 1) |>
  arrange(desc(n), taxon_id, ds_key)
d_tm_dups
```
```
# A tibble: 3,089 × 4
   taxon_id ds_key       n mdl_seqs         
      <dbl> <chr>    <dbl> <chr>            
 1   105787 rng_iucn     3 19605,19605,19605
 2   105788 rng_iucn     3 19608,19608,19608
 3   105789 rng_iucn     3 19609,19609,19609
 4   105790 rng_iucn     3 19610,19610,19610
 5   105791 rng_iucn     3 19611,19611,19611
 6   105792 rng_iucn     3 19612,19612,19612
 7   105793 rng_iucn     3 19613,19613,19613
 8   105794 rng_iucn     3 19614,19614,19614
 9   105795 rng_iucn     3 19615,19615,19615
10   105796 rng_iucn     3 19616,19616,19616
```

```r
# duplicates with distinct rows
d_tm_dups <- tbl(con_sdm, "taxon_model") |>
  distinct() |>
  group_by(taxon_id, ds_key) |>
  summarize(
    n = n(),
    mdl_seqs = stringr::str_flatten(mdl_seq, collapse = ","),
    .groups = "drop"
  ) |>
  filter(n > 1) |>
  arrange(desc(n), taxon_id, ds_key)
d_tm_dups
```
```
# A tibble: 2,361 × 4
   taxon_id ds_key       n mdl_seqs   
      <dbl> <chr>    <dbl> <chr>      
 1   105787 ms_merge     2 25418,23845
 2   105788 ms_merge     2 23846,25419
 3   105789 ms_merge     2 25420,23847
 4   105790 ms_merge     2 25421,23848
 5   105791 ms_merge     2 23849,25422
 6   105792 ms_merge     2 23850,25423
 7   105793 ms_merge     2 25424,23851
 8   105794 ms_merge     2 25425,23852
 9   105795 ms_merge     2 23853,25426
10   105796 ms_merge     2 23854,25427
```

I am hoping you can fix this in the database and ensure it doesn't happen again. Originally when I injected models into the database, I would search for the related `mdl_seq` and then delete in all related tables before inserting the new model (with an auto-incremented `mdl_seq`). But the `taxon_model` table only got subsequently created (from pivoting longer on the `taxon` table) and so not removed there.

I would guess that the smaller number (ie older) `mdl_seq` values are orphans, ie do not exist in the `model` or `model_cell` tables. But this should be confirmed.

I am concerned that these extra rows may have affected:

- Calculation of scores:
  - @workflows/update_scores.qmd
  - @workflows/merge_models.qmd
  - @workflows/calc_scores.qmd

- Creation of derived products:
  - @workflows/render_derived_products.qmd
  - @workflows/msens-summary_programareas.qmd

Note: In the apps (@apps_2026/mapsp/app.R; @apps_2026/mapgl/app.R) I changed the name of the SDM database from `sdm_2026.duckdb` to `sdm{v_sfx}` (where `v_int <- 3; v_sfx <- paste0("_v", v_int)`) and want this to be used throughout the recently used and relevant workflow scripts.

Please fix @workflows/ingest_nmfs_core-areas.qmd so "ca_nmfs" models do get added to the `taxon_model` table (and pre-existing `mdl_seq` for same ds_key and taxa would get deleted). Evaluate how this might effect downstream score calculation, especially for Rice's whale.

Since the @workflows/fix_taxon_model_dups.qmd will be a quick fix, that's fine to include "4. Adds a UNIQUE index on (taxon_id, ds_key) to prevent future duplicates" to get the database patched for now, but also add it to a sensible core workflow, after taxon_model creation and before model injection, for reproducibility into future (eg if we rebuild the whole database, which will happen later when reconstructing for global context).

⏺ Updated plan                             
  ⎿  /plan to preview
 Here is Claude's plan:                                           
 Fix taxon_model Duplicates & Update DB References                     
                                                            
 Problem Summary                                        

 The taxon_model table in sdm_v3.duckdb has 3,089 duplicate (taxon_id, ds_key) groups:
 - rng_iucn: Exact duplicates (same mdl_seq repeated 3x per group)
 - ms_merge: Distinct duplicates (different mdl_seq values — older ones are orphans not in model table)

 Root causes:
 1. ms_merge delete block (merge_models.qmd ~line 3419-3446) deletes from model, species, model_cell but NOT taxon_model
 2. rng_iucn append (merge_models.qmd ~line 3088) has no prior DELETE, so re-runs accumulate rows

 Impact Assessment

 Scores are NOT affected. All score calculations (calc_scores.qmd) join through taxon.mdl_seq directly, not through taxon_model. The recompute_merged_models in update_scores.qmd does join through taxon_model
  but only filters for ch_nmfs/ch_fws/rng_fws which have zero duplicates. No score recalculation needed.

 Fix 1: One-time database cleanup (workflows/fix_taxon_model_dups.qmd)

 Create a Quarto notebook that:
 1. Reports current duplicate counts (before state)
 2. Removes exact duplicates (rng_iucn) using DISTINCT
 3. Removes orphaned mdl_seq entries (ms_merge old rows not in model table)
 4. Adds a UNIQUE index on (taxon_id, ds_key) to prevent future duplicates
 5. Reports final state (after counts)

 -- step 1: recreate taxon_model without duplicates
 CREATE TABLE taxon_model_clean AS
 SELECT DISTINCT taxon_id, ds_key, mdl_seq
 FROM taxon_model
 WHERE mdl_seq IN (SELECT mdl_seq FROM model);

 DROP TABLE taxon_model;
 ALTER TABLE taxon_model_clean RENAME TO taxon_model;

 -- step 2: add unique constraint
 CREATE UNIQUE INDEX idx_taxon_model_tid_dskey ON taxon_model(taxon_id, ds_key);

 Fix 2: Add UNIQUE index on taxon_model after creation (merge_models.qmd)

 After dbWriteTable(con_sdm, "taxon_model", ...) at line 1838, add the UNIQUE index for reproducibility in future full rebuilds:

 dbWriteTable(con_sdm, "taxon_model", d_taxon_model, overwrite = TRUE)
 dbExecute(con_sdm,
   "CREATE UNIQUE INDEX idx_taxon_model_tid_dskey ON taxon_model(taxon_id, ds_key)")
 message(glue("taxon_model: {nrow(d_taxon_model)} rows"))

 This ensures that even on a full database rebuild, the constraint is in place before any rng_iucn/ms_merge/ca_nmfs injection steps run.

 Fix 3: Add DELETE from taxon_model in ms_merge loop (merge_models.qmd)

 In the ms_merge deletion block (~lines 3419-3446), add a DELETE from taxon_model alongside the existing deletes from model, species, model_cell:

 ```r
 # add before existing deletes in the ms_merge cleanup block
 dbExecute(con_sdm, glue(
   "DELETE FROM taxon_model
    WHERE ds_key = 'ms_merge' AND taxon_id = {tid}"))
 ```

 Fix 4: Add DELETE before rng_iucn append + fix latent bug (merge_models.qmd)

 Before the dbWriteTable(con_sdm, "taxon_model", d_iucn_tm, append = TRUE) call (~line 3088), add:

 ```r
 # delete existing rng_iucn rows before re-inserting
 dbExecute(con_sdm, "DELETE FROM taxon_model WHERE ds_key = 'rng_iucn'")
 ```

 Also fix the operator precedence bug at line 2747:
 - Before: if (length(mdl_seqs > 0))
 - After: if (length(mdl_seqs) > 0)

 Fix 5: Update sdm_2026.duckdb references to versioned pattern

 Add v_int <- 3; v_sfx <- paste0("_v", v_int) and change sdm_2026.duckdb to sdm{v_sfx}.duckdb in:
 ┌──────────────────────────────────────────┬────────────────────────┬───────────────────────────┬─────────────────────────────────────────────┐
 │                   File                   │        Line(s)         │          Current          │                     New                     │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ workflows/merge_models.qmd               │ ~50                    │ sdm_2026.duckdb           │ sdm{v_sfx}.duckdb (add v_int/v_sfx vars)    │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ workflows/update_scores.qmd              │ ~50                    │ sdm_2026.duckdb           │ sdm{v_sfx}.duckdb (add v_int/v_sfx vars)    │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd                │ ~110, ~3397            │ sdm_2026.duckdb           │ sdm{v_sfx}.duckdb (already has v_int/v_sfx) │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ workflows/msens-summary_programareas.qmd │ ~76                    │ sdm_2026.duckdb           │ sdm{v_sfx}.duckdb (already has v_int/v_sfx) │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ workflows/ingest_nmfs_core-areas.qmd     │ ~25                    │ sdm_2026.duckdb           │ sdm{v_sfx}.duckdb (add v_int/v_sfx vars)    │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ workflows/render_derived_products.qmd    │ ~18                    │ sdm_2026.duckdb (in docs) │ sdm{v_sfx}.duckdb                           │
 ├──────────────────────────────────────────┼────────────────────────┼───────────────────────────┼─────────────────────────────────────────────┤
 │ msens/R/db.R                             │ sdm_db_path/sdm_db_con │ default "2026"            │ default "v3"                                │
 └──────────────────────────────────────────┴────────────────────────┴───────────────────────────┴─────────────────────────────────────────────┘
 Fix 6: Add ca_nmfs entries to taxon_model (ingest_nmfs_core-areas.qmd)

 Problem: ingest_nmfs_core-areas.qmd creates model, species, and model_cell rows for ca_nmfs but never writes to taxon_model. This means the junction table is missing the (taxon_id, ds_key='ca_nmfs',
 mdl_seq) mapping.

 Fix: Add a new code chunk after the model_cell insertion that:
 1. Looks up taxon_id for Rice's whale from the taxon table (by worms_id = 1576133)
 2. Deletes any existing ca_nmfs rows from taxon_model for that taxon (idempotent re-runs)
 3. Inserts a new (taxon_id, ds_key='ca_nmfs', mdl_seq) row

 `### Add `taxon_model` entry

 ```r
 # look up taxon_id for rice's whale
 tid <- tbl(con_sdm, "taxon") |>
   filter(worms_id == !!worms_id) |>
   pull(taxon_id)
 stopifnot(length(tid) == 1)

 # delete existing ca_nmfs entries for this taxon (idempotent)
 dbExecute(con_sdm, glue(
   "DELETE FROM taxon_model WHERE ds_key = '{ds_key}' AND taxon_id = {tid}"))

 # insert new mapping
 d_tm <- tibble(taxon_id = tid, ds_key = ds_key, mdl_seq = mdl_seq)
 dbWriteTable(con_sdm, "taxon_model", d_tm, append = TRUE)
 message(glue("Added taxon_model: taxon_id={tid}, ds_key={ds_key}, mdl_seq={mdl_seq}"))
 ```

 Downstream impact assessment for Rice's whale

 Rice's whale (Balaenoptera ricei, worms_id 1576133):
 - extrisk_code: NMFS:EN → er_score: 100 (maximum)
 - Datasets: ca_nmfs (core area, value=100), rng_iucn, ms_merge

 Scores are NOT affected by adding ca_nmfs to taxon_model:
 - calc_scores.qmd computes cell_metric and zone_metric using taxon.mdl_seq (the ms_merge model) and taxon.er_score — neither comes from taxon_model
 - The ms_merge model for Rice's whale already incorporates ca_nmfs as a mask constituent during the merge loop in merge_models.qmd (ca_nmfs has is_mask = TRUE)
 - update_scores.qmd recompute_merged_models only touches ch_nmfs/ch_fws/rng_fws — ca_nmfs is static (always 100) so correctly excluded from recomputation
 - zone_taxon scores also use taxon.mdl_seq directly

 What DOES change: The mapsp app (apps_2026/mapsp/app.R) queries taxon_model to build dataset columns (lines 155-172). After this fix, Rice's whale will correctly show a ca_nmfs column with the associated
 mdl_seq, allowing users to view the core area layer separately.

 Execution Sequence

 1. Fix 1 — Create & run cleanup notebook workflows/fix_taxon_model_dups.qmd (one-time, adds UNIQUE index)
 2. Fix 2 — Edit merge_models.qmd: add UNIQUE index after taxon_model creation (line ~1838) for future rebuilds
 3. Fix 3 — Edit merge_models.qmd: add DELETE from taxon_model in ms_merge delete block
 4. Fix 4 — Edit merge_models.qmd: add DELETE before rng_iucn append + fix operator precedence bug
 5. Fix 5 — Update sdm_2026.duckdb references to versioned pattern across all files
 6. Fix 6 — Edit ingest_nmfs_core-areas.qmd to add taxon_model entry after model creation
 7. No score recalculation needed — all scores use taxon.mdl_seq (ms_merge) directly

 Verification

 After all fixes:
 - Run fix_taxon_model_dups.qmd to clean up existing duplicates and add UNIQUE index
 - Verify no duplicates remain: SELECT taxon_id, ds_key, COUNT(*) FROM taxon_model GROUP BY 1,2 HAVING COUNT(*) > 1
 - Verify ca_nmfs entry exists: SELECT * FROM taxon_model WHERE ds_key = 'ca_nmfs'
 - Verify UNIQUE index prevents re-insertion: attempting a duplicate INSERT should error
 - Spot-check Rice's whale in mapsp app to confirm ca_nmfs column appears

⏺ All edits verified. Let me also make sure there are no remaining sdm_2026 references in the source QMD files (excluding _output/ HTML, dev prompts, and data CSVs).                                           
                                                                      
⏺ Searched for 1 pattern (ctrl+o to expand)    

⏺ Those are just prose/message strings in ingest_nmfs-fws-listings.qmd, not code that constructs the path. That file uses msens::sdm_db_con() (which now defaults to "v3"). The references are cosmetic — let me check if the plan mentioned this file.                 
  The plan didn't include ingest_nmfs-fws-listings.qmd in the update list, so these prose references are expected to remain as-is.                                                                              

  Here's a summary of all changes made:                                                                                                                                                                                                         
  Changes Summary

  Fix 1: New file workflows/fix_taxon_model_dups.qmd

  - One-time cleanup notebook that reports duplicates, removes them via DISTINCT + orphan filtering, adds UNIQUE INDEX, and verifies

  Fix 2: workflows/merge_models.qmd ~line 1842

  - Added CREATE UNIQUE INDEX idx_taxon_model_tid_dskey ON taxon_model(taxon_id, ds_key) after initial dbWriteTable for future full rebuilds

  Fix 3: workflows/merge_models.qmd ~line 3434

  - Added DELETE FROM taxon_model WHERE ds_key = '{ds_key}' AND taxon_id = {d_sp$taxon_id} in the ms_merge delete block, before the existing model/species/model_cell deletes

  Fix 4: workflows/merge_models.qmd ~line 3094 + line 2752

  - Added DELETE FROM taxon_model WHERE ds_key = 'rng_iucn' before the rng_iucn append
  - Fixed operator precedence bug: length(mdl_seqs > 0) → length(mdl_seqs) > 0

  Fix 5: Versioned DB references across 7 files
  ┌──────────────────────────────────────────┬───────────────────────────────────────────────────────────────┐
  │                   File                   │                            Change                             │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ workflows/merge_models.qmd               │ Added v_int/v_sfx, changed to sdm{v_sfx}.duckdb               │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ workflows/update_scores.qmd              │ Added v_int/v_sfx, changed to sdm{v_sfx}.duckdb               │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ workflows/calc_scores.qmd                │ Changed 2 refs to sdm{v_sfx}.duckdb (already had v_int/v_sfx) │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ workflows/msens-summary_programareas.qmd │ Changed to sdm{v_sfx}.duckdb (already had v_int/v_sfx)        │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ workflows/ingest_nmfs_core-areas.qmd     │ Added v_int/v_sfx, changed to sdm{v_sfx}.duckdb               │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ workflows/render_derived_products.qmd    │ Changed prose reference to sdm_v3.duckdb                      │
  ├──────────────────────────────────────────┼───────────────────────────────────────────────────────────────┤
  │ msens/R/db.R                             │ Default parameter changed from "2026" to "v3"                 │
  └──────────────────────────────────────────┴───────────────────────────────────────────────────────────────┘
  Fix 6: workflows/ingest_nmfs_core-areas.qmd

  - Added new "Add taxon_model entry" section that looks up Rice's whale taxon_id, deletes existing ca_nmfs rows (idempotent), and inserts new (taxon_id, ds_key, mdl_seq) mapping
  - Added taxon_model verification to the existing Verify block

  Next steps

  1. Rename the actual DB file: mv sdm_2026.duckdb sdm_v3.duckdb (or copy)
  2. Run fix_taxon_model_dups.qmd to clean existing duplicates and add the UNIQUE index
  3. Run devtools::document() in msens/ to regenerate .Rd man pages with the new defaults
  4. Re-run ingest_nmfs_core-areas.qmd to add the ca_nmfs taxon_model entry

Go ahead and update @workflows/ingest_nmfs-fws-listings.qmd with any necessary changes, ie to SDM duckdb and mdl_seq bookkeeping, especially with new taxon_model table.

## 2026-02-10.d update derived products

The database has been updated with the NMFS/FWS full species listing (@workflows/ingest_nmfs-fws-listings.qmd), and new "ca_nmfs" model for Rice's whale (@workflows/ingest_nmfs_core-areas.qmd). Assume that the merging of models (@workflows/merge_models.qmd) and calculation of scores (@workflows/calc_scores.qmd) have also been run. I need to quickly render derived products which are located @'~/My Drive/projects/msens/data/derived' and subdirectory @'~/My Drive/projects/msens/data/derived/downloads' (which may include copies from derived parent). Sort these by most recently updated to find matching code in one of the @workflows/*.qmd that generated it. Evaluate the R chunk and its dependents to come up with a plan for regenerating these from the latest model outputs.

⏺ Updated plan
  ⎿  /plan to preview

─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 Ready to code?

 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Plan: Regenerate Derived Products from Latest Model Outputs

 Context

 After ingesting updated NMFS/FWS full species listings and the new Rice's whale "ca_nmfs" model, merge_models.qmd and calc_scores.qmd have
 been run — so sdm_2026.duckdb has updated cell_metric, zone_metric, and taxon tables. Now the derived export files need to be regenerated.

 Files to regenerate
 ┌───────────────────────────────────────────────────┬────────────────────┬────────────────────────────────────────────────────────────┐
 │                       File                        │      Location      │                           Source                           │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ zone_taxon_2026.csv                               │ derived/           │ calc_scores.qmd zone_taxon chunk (line 1967)               │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ ply_planareas_2025.gpkg                           │ derived/           │ calc_scores.qmd update_cached_downloads (line 2381)        │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ ply_programareas_2026.gpkg                        │ derived/           │ calc_scores.qmd update_cached_downloads (line 2381)        │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ ply_ecoregions_2025.gpkg                          │ derived/           │ calc_scores.qmd update_cached_downloads (line 2381)        │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ layers_2026.csv                                   │ derived/           │ calc_scores.qmd update_cached_downloads (line 2381)        │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ r_metrics_2026.tif                                │ derived/           │ calc_scores.qmd update_cached_downloads (line 2381)        │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ r_metrics_akl48.tif                               │ derived/           │ calc_scores.qmd within update_cached_downloads (line 2511) │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ OCS_Area_Polygons_env_sensitivity_metrics.geojson │ derived/           │ calc_scores.qmd pa_metrics_geo (line 2555)                 │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ sdm_parquet/                                      │ derived/           │ calc_scores.qmd export_parquet (line 2691)                 │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ ply_planareas_2025.gpkg                           │ derived/downloads/ │ manual copy from derived/                                  │
 ├───────────────────────────────────────────────────┼────────────────────┼────────────────────────────────────────────────────────────┤
 │ ply_ecoregions_2025.gpkg                          │ derived/downloads/ │ manual copy from derived/                                  │
 └───────────────────────────────────────────────────┴────────────────────┴────────────────────────────────────────────────────────────┘
 Bug fixes needed before running (3 code changes in calc_scores.qmd)

 Fix 1: redo_dl deletion loop is incomplete (line 2386)

 Currently only deletes old-version files (lyrs_csv, metrics_tif), not the 2026 versions. The 2026 files won't regenerate if they already
 exist.

 File: workflows/calc_scores.qmd line 2386
 Change:
 `# before:
 for (f in c(pa_gpkg, er_gpkg, lyrs_csv, metrics_tif)){

 `# after:
 for (f in c(pa_gpkg, pra_gpkg, er_gpkg, lyrs_csv, lyrs_2026_csv,
             metrics_tif, metrics_2026_tif, metrics_akl48_tif)){

 Fix 2: metrics_akl48_tif uses old raster and planarea_key (lines 2521-2527)

 The 2026 raster has programarea_key (not planarea_key), and the code reads the old metrics_tif instead of metrics_2026_tif.

 File: workflows/calc_scores.qmd lines 2517-2527
 Change:
 `# before:
   pa_sr <- read_csv(sr_pa_csv) |>
     filter(subregion_key == !!sr) |>
     select(subregion_key, planarea_key)

   r_metrics <- rast(metrics_tif)
   r_pa <- subset(r_metrics, "planarea_key")
   levels(r_pa) <- levels(r_pa)[[1]] |>
     left_join(
       pa_sr,
       by = join_by(planarea_key)) |>
     select(value, subregion_key)

 `# after:
   pra_sr <- read_csv(sr_pra_csv) |>
     filter(subregion_key == !!sr) |>
     select(subregion_key, programarea_key)

   r_metrics <- rast(metrics_2026_tif)
   r_pa <- subset(r_metrics, "programarea_key")
   levels(r_pa) <- levels(r_pa)[[1]] |>
     left_join(
       pra_sr,
       by = join_by(programarea_key)) |>
     select(value, subregion_key)

 Fix 3: export_parquet hard-codes old DB path (line 2695)

 File: workflows/calc_scores.qmd line 2695
 Change:
 `# before:
 sdm_db <- glue("{dir_data}/derived/sdm.duckdb")

 `# after:
 sdm_db <- glue("{dir_data}/derived/sdm_2026.duckdb")

 Execution order in calc_scores.qmd (interactive RStudio)

 Most chunks have eval: false — run them sequentially in RStudio:

 1. setup (line 22) — establishes con_sdm, paths, r_cell, get_rast()
 2. zone_taxon (line 1967) — rebuilds species-per-zone from updated data; writes zone_taxon_2026.csv and DuckDB zone_taxon table
 3. setup_update_pgtable_with_clear (line 2131) — defines update_table_with_clear(), opens PostGIS con
 4. pg_update_planarea_ecoregion_metrics_2025 (line 2184) — updates PostGIS planareas + ecoregions with latest metrics. NOTE: comment says
 "DO NOT run to keep mapgl unchanged" — run it now since we want updated scores
 5. pg_update_programareas_metrics_2026 (line 2256) — updates PostGIS programareas with latest metrics
 6. pg_rm_old_metrics (line 2291) — removes obsolete metric columns
 7. add_index_for_vector_tiles (line 2353) — ensures primary keys for pg_tileserv
 8. re-add_con_sdm.cell (line 2364) — ensures cell table in DuckDB (no-op if exists)
 9. Apply Fix 1, Fix 2 above before running the next chunk
 10. update_cached_downloads (line 2381) — regenerates all gpkg, csv, tif files
 11. r_metrics_akl48_primprod_log (line 2541) — adds log-primprod layer to akl48 raster
 12. pa_metrics_geo (line 2555) — regenerates GeoJSON
 13. Apply Fix 3 above, then run export_parquet (line 2691) — exports to sdm_parquet/

 Post-export: copy to downloads/

 dir_dl <- glue("{dir_derived}/downloads")
 file.copy(pa_gpkg,  glue("{dir_dl}/ply_planareas_2025.gpkg"),  overwrite = TRUE)
 file.copy(er_gpkg,  glue("{dir_dl}/ply_ecoregions_2025.gpkg"), overwrite = TRUE)

 Optional: update msens-summary_env-national-ocs.qmd

 This workflow still references old paths (sdm.duckdb, r_metrics.tif). If you want to regenerate the summary maps, flower plots, and tables:

 File: workflows/msens-summary_env-national-ocs.qmd
 - Line 38: sdm.duckdb → sdm_2026.duckdb
 - Line 43: r_metrics.tif → r_metrics_2026.tif
 - Verify raster layer names (the 2026 raster uses programarea_key not planarea_key)

 Then render to regenerate all figures, tables, and CSVs in figs/msens-summary_env-national-ocs/ and derived/.

 Verification

 After running all steps:
 - Check file dates: ls -lt ~/My\ Drive/projects/msens/data/derived/ — all key files should show today's date
 - Spot-check zone_taxon_2026.csv for Rice's whale entries
 - Verify r_metrics_2026.tif has correct layers: terra::rast(metrics_2026_tif) |> names()
 - Compare gpkg metric values for a known planning area against the DuckDB zone_metric table

Consider writing a lookup table in @workflows/data/derived_products.csv with workflows qmd filename and R chunk responsible for writing to the output and perhaps a list of qmd.chunk dependencies.
And a @workflows/render_derived_products.qmd with detailed instructions.

We also need planarea_flower-plot_scores.png|pdf, table_planarea_scores.docx|xlsx, subregion_planareas.csv, table_ecoregion_planarea_scores.docx, table_planarea_scores.xlsx, tbl_er_pa_scores.csv, map_cell_score_l48.png, map_cell_score_ak.png, map_cell_npp_l48.png, map_cell_npp_ak.png, map_pa_l48.png, map_pa_ak.png -- but for Program Areas, not Planning Areas. Also try matching up products in the subfolder @'~/My Drive/projects/msens/data/derived/downloads/figures' -- we definitely need the latest greatest flower plots for Program Areas.

⏺ Updated plan                                          
  ⎿  /plan to preview                                   
                                    
 Here is Claude's 
 
 Plan: Regenerate Derived Products for Program Areas
                                                        
 Context

 After ingesting updated NMFS/FWS listings and the Rice's whale "ca_nmfs" model, merge_models.qmd and calc_scores.qmd have been run —
  sdm_2026.duckdb now has updated scores. The derived export files and summary products need regeneration, focused on Program Areas
 (not Planning Areas). We also need to archive existing products and create documentation for the regeneration pipeline.

 Deliverables

 1. workflows/data/derived_products.csv — lookup table

 CSV with columns: output_file, output_dir, source_qmd, source_chunk, chunk_line, dependencies, description

 Maps every derived file to its generating workflow/chunk, including:
 - All calc_scores.qmd exports (gpkg, csv, tif, geojson, parquet)
 - All msens-summary_programareas.qmd outputs (maps, flower plots, tables)
 - Products copied to downloads/

 2. workflows/render_derived_products.qmd — regeneration guide

 A Quarto notebook with:
 - Step-by-step instructions for regenerating all derived products
 - Ordered chunk execution list for calc_scores.qmd
 - Archiving old files before overwriting
 - Copying files to downloads/
 - Reference to derived_products.csv for the full inventory

 3. Bug fixes in workflows/calc_scores.qmd (3 edits)

 Fix 1 (line 2386): redo_dl deletion loop is incomplete — add pra_gpkg, lyrs_2026_csv, metrics_2026_tif, metrics_akl48_tif

 `# before:
 for (f in c(pa_gpkg, er_gpkg, lyrs_csv, metrics_tif)){
 `# after:
 for (f in c(pa_gpkg, pra_gpkg, er_gpkg, lyrs_csv, lyrs_2026_csv,
             metrics_tif, metrics_2026_tif, metrics_akl48_tif)){

 Fix 2 (lines 2517-2527): metrics_akl48_tif reads old raster and uses planarea_key — switch to metrics_2026_tif and programarea_key

 `# before:
   pa_sr <- read_csv(sr_pa_csv) |>
     filter(subregion_key == !!sr) |>
     select(subregion_key, planarea_key)
   r_metrics <- rast(metrics_tif)
   r_pa <- subset(r_metrics, "planarea_key")
   levels(r_pa) <- levels(r_pa)[[1]] |>
     left_join(pa_sr, by = join_by(planarea_key)) |>
     select(value, subregion_key)
 `# after:
   pra_sr <- read_csv(sr_pra_csv) |>
     filter(subregion_key == !!sr) |>
     select(subregion_key, programarea_key)
   r_metrics <- rast(metrics_2026_tif)
   r_pa <- subset(r_metrics, "programarea_key")
   levels(r_pa) <- levels(r_pa)[[1]] |>
     left_join(pra_sr, by = join_by(programarea_key)) |>
     select(value, subregion_key)

 Fix 3 (line 2695): export_parquet hard-codes old DB

 `# before:
 sdm_db <- glue("{dir_data}/derived/sdm.duckdb")
 `# after:
 sdm_db <- glue("{dir_data}/derived/sdm_2026.duckdb")

 4. New workflows/msens-summary_programareas.qmd — Program Area summaries

 A new Quarto notebook adapted from msens-summary_env-national-ocs.qmd, producing Program Area versions of all summary products:

 Setup changes (vs original):
 - sdm_dd → sdm_2026.duckdb
 - metrics_tif → r_metrics_2026.tif
 - pra_gpkg → ply_programareas_2026.gpkg
 - dir_figs → figs/msens-summary_programareas
 - Zone table: ply_programareas_2026 instead of ply_planareas_2025
 - Zone field: programarea_key instead of planarea_key

 Outputs (all using programarea naming):
 ┌─────────────────────────────────────────┬──────────┬──────────────────────────┬───────────────────────────────────────────────┐
 │               Output File               │   Dir    │       Source Chunk       │                     Notes                     │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ map_pra_l48.png                         │ figs/    │ map_imgs_pra_ply_l48_ak  │ mapgl vector tiles from ply_programareas_2026 │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ map_pra_ak.png                          │ figs/    │ map_imgs_pra_ply_l48_ak  │ mapgl vector tiles from ply_programareas_2026 │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ map_cell_score_l48.png                  │ figs/    │ map_imgs_cell_l48_ak     │ cell-level from r_metrics_2026.tif            │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ map_cell_score_ak.png                   │ figs/    │ map_imgs_cell_l48_ak     │ cell-level from r_metrics_2026.tif            │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ map_cell_npp_l48.png                    │ figs/    │ map_imgs_cell_l48_ak     │ cell-level NPP                                │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ map_cell_npp_ak.png                     │ figs/    │ map_imgs_cell_l48_ak     │ cell-level NPP                                │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ programarea_flower-plot_scores.png|pdf  │ figs/    │ plot_programarea_flowers │ all 20 PRAs, 5 themes                         │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ ecoregion_flower-plot_scores.png|pdf    │ figs/    │ plot_ecoregion_flowers   │ ecoregion flowers (updated data)              │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ programarea_primprod.png|pdf            │ figs/    │ plot_primprod            │ NPP bar chart for PRAs                        │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ table_ecoregion_programarea_scores.docx │ figs/    │ flextable_er_pra         │ hierarchical table                            │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ table_programarea_scores.docx|xlsx      │ figs/    │ flextable_pra            │ PRA-only table                                │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ tbl_er_pra_scores.csv                   │ derived/ │ pra_score_table          │ ecoregion + PRA scores                        │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ tbl_pra_scores.csv                      │ derived/ │ flextable_pra            │ PRA scores                                    │
 ├─────────────────────────────────────────┼──────────┼──────────────────────────┼───────────────────────────────────────────────┤
 │ subregion_programareas.csv              │ derived/ │ already exists           │ from calc_scores.qmd                          │
 └─────────────────────────────────────────┴──────────┴──────────────────────────┴───────────────────────────────────────────────┘
 Key code adaptations from the PA version:

 - pa_score_table → pra_score_table: Query zone table where tbl == "ply_programareas_2026" and fld == "programarea_key". Join with
 ply_programareas_2026 PostGIS table for names/regions. Create ecoregion-programarea intersection (ply_ecoregion_programarea.gpkg)
 using st_intersection().
 - plot_programarea_flowers: Query zone_metric for programarea_key values (20 PRAs). Use same plot_flower() function, same
 components, same color palette. Facet by programarea_lbl (20 facets vs 27).
 - map_imgs_pra_ply_l48_ak: Use public.ply_programareas_2026 as vector tile source. Filter and color by
 score_extriskspcat_primprod_ecoregionrescaled_equalweights column.
 - flextable_er_pra / flextable_pra: Same flextable structure with programarea_key/programarea_name columns instead of
 planarea_key/planarea_name.

 Reusable code from original (copy these functions/patterns):
 - plot_flower() function (lines 57-151) — import via source()
 - update_table_with_clear() from libs/db.R
 - Component color palette generation
 - mapgl + webshot rendering pattern
 - flextable styling

 Files to archive before regeneration

 Move existing files from derived/downloads/ to derived/downloads/archive_2026-02-10/:
 - All .png, .pdf map and flower plot files
 - planarea_flower-plot_scores.*
 - table_planarea_scores.docx, table_planarea_scores.xlsx
 - table_ecoregion_planarea_scores.docx
 - tbl_er_pa_scores.csv, planarea_primprod.csv
 - map_pa_l48.png, map_pa_ak.png
 - map_cell_score_*.png, map_cell_npp_*.png
 - ply_planareas_2025.gpkg, ply_ecoregions_2025.gpkg
 - figures/ subdirectory files (flower plot themes, etc.)

 Execution order

 1. Archive existing downloads files → archive_2026-02-10/
 2. Apply 3 bug fixes to calc_scores.qmd
 3. Run calc_scores.qmd chunks interactively (in order):
   - setup (line 22)
   - zone_taxon (line 1967) — rebuilds zone_taxon_2026.csv
   - setup_update_pgtable_with_clear (line 2131)
   - pg_update_planarea_ecoregion_metrics_2025 (line 2184)
   - pg_update_programareas_metrics_2026 (line 2256)
   - pg_rm_old_metrics (line 2291)
   - add_index_for_vector_tiles (line 2353)
   - re-add_con_sdm.cell (line 2364)
   - update_cached_downloads (line 2381)
   - r_metrics_akl48_primprod_log (line 2541)
   - pa_metrics_geo (line 2555)
   - export_parquet (line 2691)
 4. Copy gpkg files to downloads/
 5. Render msens-summary_programareas.qmd to generate all PRA products
 6. Copy relevant outputs to downloads/ and downloads/figures/
 7. Create workflows/data/derived_products.csv
 8. Create workflows/render_derived_products.qmd

 Verification

 - Check all derived/ files have today's date: ls -lt ~/My\ Drive/projects/msens/data/derived/
 - Verify Rice's whale in zone_taxon_2026.csv: grep -i "rice" zone_taxon_2026.csv
 - Verify r_metrics_2026.tif layer names include programarea_key
 - Verify flower plots show 20 Program Areas (not 27 Planning Areas)
 - Verify PRA maps render correctly for both AK and L48 regions
 - Compare score values in PRA tables against DuckDB zone_metric table

Update plan to capture changes to `@workflows/calc_scores.qmd` that incorporate version of tables and file outputs. Apply this approach to the new `@workflows/msens-summary_programareas.qmd`, adding version specific suffixes (eg `_v3`) to output files and an easily updated variable at the top of the notebook (ie in `setup` chunk) to iterate in future.

Also track down where `metrics_tif` gets defined and ensure that gets updated (with version).


Update plan to capture changes to `@workflows/calc_scores.qmd` that incorporate version of tables and file outputs. Apply this approach to the new `@workflows/msens-summary_programareas.qmd`, adding version specific suffixes (eg `_v3`) to output files and an easily updated variable at the top of the notebook (ie in `setup` chunk) to iterate in future.Also track down where `metrics_tif` gets defined and ensure that gets updated (with version).

⏺ Updated plan                                          
                                           
 Here is Claude's Plan: Regenerate Versioned Derived Products for Program Areas                      
                                                        
 Context

 After ingesting updated NMFS/FWS listings and the Rice's whale "ca_nmfs" model, merge_models.qmd and calc_scores.qmd have been run — sdm_2026.duckdb
 now has updated scores. The file already uses a version variable (v_int <- 3, v_sfx <- "_v3") for most output paths. However, several paths and code
 blocks still reference old unversioned files. We need to:

 1. Fix remaining unversioned references in calc_scores.qmd
 2. Create a new msens-summary_programareas.qmd with the same versioning approach for Program Area products
 3. Document everything in a lookup CSV and a rendering guide QMD

 Deliverables

 A. Fix calc_scores.qmd versioning gaps (4 code changes)

 File: workflows/calc_scores.qmd

 Current versioning (already working): v_int <- 3; v_sfx <- "_v3" at line 78-79. Applied to:
 - pa_gpkg → ply_planareas_2025_v3.gpkg (line 94)
 - er_gpkg → ply_ecoregions_2025_v3.gpkg (line 95)
 - pra_gpkg → ply_programareas_2026_v3.gpkg (line 98)
 - metrics_2026_tif → r_metrics_2026_v3.tif (line 102)
 - metrics_akl48_tif → r_metrics_akl48_v3.tif (line 103)
 - zone_taxon_csv → zone_taxon_2026_v3.csv (line 105)
 - lyrs_2026_csv → layers_2026_v3.csv (line 108)
 - PostGIS tables: ply_planareas_2025_v3, ply_programareas_2026_v3, etc.

 Fix 1 — metrics_tif still unversioned (line 101)

 metrics_tif is defined as r_metrics.tif (no version). It's referenced in:
 - redo_dl loop (line 3035) — deletes old files
 - metrics_akl48_tif generation (line 3187) — reads as source raster

 Change: Either remove metrics_tif or leave it as the legacy reference. The key fix is in the code that USES it (Fixes 2 & 3 below).

 Fix 2 — redo_dl deletion loop incomplete (line 3035)

 The loop only deletes OLD unversioned files. It doesn't delete the versioned _v3 files, so they won't regenerate.

 `# line 3035, before:
   for (f in c(pa_gpkg, er_gpkg, lyrs_csv, metrics_tif)) {

 `# after:
   for (f in c(pa_gpkg, pra_gpkg, er_gpkg,
               lyrs_csv, lyrs_2026_csv,
               metrics_tif, metrics_2026_tif, metrics_akl48_tif)) {

 Fix 3 — metrics_akl48_tif uses old raster and planarea_key (lines 3183-3194)

 The 2026 raster has programarea_key (not planarea_key), and the code reads metrics_tif (old unversioned) instead of metrics_2026_tif.

 `# lines 3183-3194, before:
   pa_sr <- read_csv(sr_pa_csv) |>
     filter(subregion_key == !!sr) |>
     select(subregion_key, planarea_key)

   r_metrics <- rast(metrics_tif)
   r_pa <- subset(r_metrics, "planarea_key")
   levels(r_pa) <- levels(r_pa)[[1]] |>
     left_join(
       pa_sr,
       by = join_by(planarea_key)
     ) |>
     select(value, subregion_key)

 `# after:
   pra_sr <- read_csv(sr_pra_csv) |>
     filter(subregion_key == !!sr) |>
     select(subregion_key, programarea_key)

   r_metrics <- rast(metrics_2026_tif)
   r_pa <- subset(r_metrics, "programarea_key")
   levels(r_pa) <- levels(r_pa)[[1]] |>
     left_join(
       pra_sr,
       by = join_by(programarea_key)
     ) |>
     select(value, subregion_key)

 Fix 4 — export_parquet hard-codes old DB (line 3382)

 `# line 3382, before:
 sdm_db <- glue("{dir_data}/derived/sdm.duckdb")

 `# after:
 `# sdm_db already defined in setup as sdm_2026.duckdb; just remove this override
 `# or change to:
 sdm_db <- glue("{dir_data}/derived/sdm_2026.duckdb")

 B. New workflows/msens-summary_programareas.qmd

 A new Quarto notebook adapted from msens-summary_env-national-ocs.qmd, producing Program Area versions of all summary products.

 Setup chunk — key changes from original:

 `# version variable (same as calc_scores.qmd)
 v_int <- 3
 v_sfx <- paste0("_v", v_int)

 `# paths with version suffix
 sdm_dd         <- glue("{dir_data}/derived/sdm_2026.duckdb")
 pra_gpkg       <- glue("{dir_data}/derived/ply_programareas_2026{v_sfx}.gpkg")
 er_gpkg        <- glue("{dir_data}/derived/ply_ecoregions_2025{v_sfx}.gpkg")
 metrics_tif    <- glue("{dir_data}/derived/r_metrics_2026{v_sfx}.tif")
 sr_pra_csv     <- glue("{dir_data}/derived/subregion_programareas.csv")
 dir_figs       <- here("figs/msens-summary_programareas")
 dir_dl         <- glue("{dir_data}/derived/downloads")

 `# output files with version suffix
 er_pra_gpkg    <- glue("{dir_data}/derived/ply_ecoregion_programarea{v_sfx}.gpkg")
 er_pra_csv     <- glue("{dir_data}/derived/ply_ecoregion_programarea{v_sfx}.csv")
 er_pra_n_csv   <- glue("{dir_data}/derived/tbl_er_pra_scores{v_sfx}.csv")
 pra_n_csv      <- glue("{dir_data}/derived/tbl_pra_scores{v_sfx}.csv")

 Chunks to create (adapted from PA version):
 Chunk: setup
 Output Files: —
 Adaptation from PA version: Version vars, PRA paths, DuckDB+PostGIS connections, plot_flower() function (source from shared lib)
 ────────────────────────────────────────
 Chunk: read_plys
 Output Files: ply_ecoregion_programarea{v_sfx}.gpkg, .csv
 Adaptation from PA version: Intersect ecoregions × program areas (instead of planning areas)
 ────────────────────────────────────────
 Chunk: ply_lbl_pts
 Output Files: label placement data
 Adaptation from PA version: PRA centroids for map labels
 ────────────────────────────────────────
 Chunk: map_imgs_pra_ply_l48_ak
 Output Files: map_pra_l48.png, map_pra_ak.png
 Adaptation from PA version: Use ply_programareas_2026{v_sfx} PostGIS layer, programarea_key
 ────────────────────────────────────────
 Chunk: map_imgs_cell_l48_ak
 Output Files: map_cell_score_l48.png, map_cell_score_ak.png, map_cell_npp_l48.png, map_cell_npp_ak.png
 Adaptation from PA version: Use r_metrics_2026{v_sfx}.tif, mask by PRA ecoregions
 ────────────────────────────────────────
 Chunk: pra_score_table
 Output Files: tbl_er_pra_scores{v_sfx}.csv
 Adaptation from PA version: Query zone where tbl == "ply_programareas_2026{v_sfx}" and fld == "programarea_key"
 ────────────────────────────────────────
 Chunk: flextable_er_pra
 Output Files: table_ecoregion_programarea_scores{v_sfx}.docx
 Adaptation from PA version: Hierarchical ecoregion → program area table
 ────────────────────────────────────────
 Chunk: flextable_pra
 Output Files: table_programarea_scores{v_sfx}.docx, tbl_pra_scores{v_sfx}.csv, table_programarea_scores{v_sfx}.xlsx
 Adaptation from PA version: PRA-only table
 ────────────────────────────────────────
 Chunk: plot_ecoregion_flowers
 Output Files: ecoregion_flower-plot_scores{v_sfx}.png|pdf
 Adaptation from PA version: Same data source (ecoregion metrics), updated version suffix
 ────────────────────────────────────────
 Chunk: plot_programarea_flowers
 Output Files: programarea_flower-plot_scores{v_sfx}.png|pdf (5 themes × 2 formats)
 Adaptation from PA version: Query zone_metric for programarea_key (20 PRAs), facet by programarea_lbl
 ────────────────────────────────────────
 Chunk: plot_primprod
 Output Files: programarea_primprod{v_sfx}.png|pdf, programarea_primprod{v_sfx}.csv
 Adaptation from PA version: Query primprod_avg/stddev for PRA zones
 ────────────────────────────────────────
 Chunk: copy_to_downloads
 Output Files: —
 Adaptation from PA version: Copy versioned outputs to derived/downloads/ and downloads/figures/
 Key code patterns (from original, adapted for PRA):

 1. Zone queries use tbl == "ply_programareas_2026{v_sfx}" and fld == "programarea_key"
 2. PostGIS table: ply_programareas_2026{v_sfx} (already has metric columns from calc_scores)
 3. 20 Program Areas instead of 27 Planning Areas (includes GAA, GAB for GOA)
 4. PRA list: from tbl(con, "ply_programareas_2026{v_sfx}") filtering region_key == "AK" or specific L48 keys
 5. plot_flower() function sourced from msens-summary_env-national-ocs.qmd or extracted to a shared lib file

 C. workflows/data/derived_products.csv

 Columns: output_file, output_dir, source_qmd, source_chunk, chunk_line, dependencies, description

 Will catalog every derived product from both calc_scores.qmd and msens-summary_programareas.qmd, including:
 - All versioned file outputs (_v3 suffix)
 - DuckDB table writes (e.g., zone_taxon_v3)
 - PostGIS table updates
 - Files copied to downloads/

 D. workflows/render_derived_products.qmd

 Step-by-step regeneration guide:

 1. Archive — Move existing downloads/ files to archive_2026-02-10/
 2. calc_scores.qmd execution order (interactive chunks):
   - setup (line 53)
   - zone_taxon (line ~2600)
   - setup_update_pgtable_with_clear (line ~2730)
   - pg_update_planarea_ecoregion_metrics_2025 (line ~2785)
   - pg_update_programareas_metrics_2026 (line ~2870)
   - pg_rm_old_metrics (line ~2918)
   - add_index_for_vector_tiles (line ~2985)
   - re-add_con_sdm.cell (line ~3010)
   - update_cached_downloads (line 3029)
   - r_metrics_akl48_primprod_log (line 3208)
   - pa_metrics_geo (line 3223)
   - export_parquet (line 3377)
 3. Copy to downloads/ — gpkg files
 4. Render msens-summary_programareas.qmd — generates all PRA products
 5. Copy PRA products to downloads/ and downloads/figures/
 6. Verify — check dates, spot-check values

 Files to archive

 Move from derived/downloads/ to derived/downloads/archive_2026-02-10/:
 - ply_planareas_2025.gpkg, ply_ecoregions_2025.gpkg
 - planarea_flower-plot_scores.* (all variants)
 - table_planarea_scores.docx, table_planarea_scores.xlsx
 - table_ecoregion_planarea_scores.docx
 - tbl_er_pa_scores.csv, planarea_primprod.*
 - map_pa_l48.png, map_pa_ak.png
 - map_cell_score_*.png, map_cell_npp_*.png
 - r_metrics_akl48.tif
 - zone_taxon.csv, zone_taxon_akl48.csv
 - subregion_planareas.csv
 - figures/* (all existing flower plot files)

 Files modified

 - workflows/calc_scores.qmd — 4 targeted edits (lines 3035, 3183-3194, 3382)
 - workflows/msens-summary_programareas.qmd — new file
 - workflows/data/derived_products.csv — new file
 - workflows/render_derived_products.qmd — new file

 Verification

 - All _v3 files in derived/ should have today's date
 - zone_taxon_2026_v3.csv: grep for Rice's whale
 - r_metrics_2026_v3.tif: terra::rast() → layers include programarea_key
 - PRA flower plots: 20 facets (not 27)
 - PRA maps: show program area boundaries with correct score coloring
 - Compare table scores against zone_metric in DuckDB

⏺ All 4 deliverables are complete. Here's a summary of what was done:          
                                          
  A. Fixed calc_scores.qmd — 4 targeted edits                                                                                                           
                                          
  1. Fix 2 (line 3038): Expanded redo_dl deletion loop to include all versioned files: pra_gpkg, lyrs_2026_csv, metrics_tif, metrics_akl48_tif          
  2. Fix 3 (lines 3186-3199): Changed metrics_akl48_tif generation to use sr_pra_csv/programarea_key instead of sr_pa_csv/planarea_key, and reads
  metrics_2026_tif (already correct)                                                                                                                    
  3. Fix 4 (line 3385): Changed export_parquet DB path from sdm.duckdb to sdm_2026.duckdb                                                               
                                                                                                                                                        
  B. Created workflows/msens-summary_programareas.qmd                                                                                                   

  New ~700-line Quarto notebook adapted from the PA version with:
  - Versioned paths (v_int <- 3, v_sfx <- "_v3")
  - All chunks adapted: read_plys (ecoregion × program area intersection), ply_lbl_pts, map_imgs_pra_ply_l48_ak, map_imgs_cell_l48_ak, pra_score_table,
  flextable_er_pra, flextable_pra (docx + xlsx), plot_ecoregion_flowers, plot_programarea_flowers (5 themes), plot_primprod, copy_to_downloads
  - Queries use tbl == "ply_programareas_2026_v3" and fld == "programarea_key"
  - PostGIS tile URLs use versioned table names

  C. Created workflows/data/derived_products.csv

  Catalogs 35 output files with columns: output_file, output_dir, source_qmd, source_chunk, dependencies, description

  D. Created workflows/render_derived_products.qmd

  Step-by-step guide with: archive old files, calc_scores.qmd chunk execution order (12 steps), copy to downloads, render PRA summary, and verification
  checks (file dates, Rice's whale, raster layers, facet count).


What is the sequence I should run? I think I already ran everything updated in @workflows/calc_scores.qmd (but you could easily inspect filesystem to confirm)

⏺ Here's what I see from the filesystem:                                                                                                                
                                                                                                                                                        
  Already completed today (Feb 10)                                                                                                                      
  ┌───────────────────────────────┬───────────┬─────────────────────────┐                                                                               
  │             File              │ Timestamp │      Source chunk       │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ zone_taxon_2026_v3.csv        │ 12:46     │ zone_taxon              │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ ply_programareas_2026_v3.gpkg │ 14:43     │ update_cached_downloads │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ layers_2026_v3.csv            │ 14:43     │ update_cached_downloads │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ ply_planareas_2025_v3.gpkg    │ 14:45     │ update_cached_downloads │
  ├───────────────────────────────┼───────────┼─────────────────────────┤
  │ ply_ecoregions_2025_v3.gpkg   │ 14:45     │ update_cached_downloads │
  ├───────────────────────────────┼───────────┼─────────────────────────┤
  │ r_metrics_2026_v3.tif         │ 14:45     │ update_cached_downloads │
  ├───────────────────────────────┼───────────┼─────────────────────────┤
  │ sdm_parquet/                  │ 15:08     │ export_parquet          │
  └───────────────────────────────┴───────────┴─────────────────────────┘
  Still missing (not yet regenerated with v3)
  ┌────────────────────────┬────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────┐
  │          File          │                         Status                         │                    Source chunk                     │
  ├────────────────────────┼────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────┤
  │ r_metrics_akl48_v3.tif │ missing — only old r_metrics_akl48.tif (Aug 18) exists │ update_cached_downloads → metrics_akl48_tif section │
  ├────────────────────────┼────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────┤
  │ OCS_...Metrics.geojson │ stale (Aug 7)                                          │ pa_metrics_geo                                      │
  └────────────────────────┴────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────┘
  Remaining sequence to run

  1. update_cached_downloads (line 3032) — re-run just the metrics_akl48_tif section (line 3180). This will now use the fixed code (programarea_key +
  sr_pra_csv + metrics_2026_tif).
  2. r_metrics_akl48_primprod_log (line 3211) — adds primprod_log layer to the new AKL48 raster
  3. pa_metrics_geo (line 3226) — regenerate the GeoJSON (optional, planning areas only)
  4. Archive old downloads — run the archive chunk in render_derived_products.qmd
  5. Copy calc_scores outputs to downloads — run the copy chunk in render_derived_products.qmd
  6. Run msens-summary_programareas.qmd — all chunks, generates PRA maps/tables/flowers/primprod
  7. Verify — spot-check Rice's whale, raster layers, facet count

  The critical next step is #1 — generating r_metrics_akl48_v3.tif — since that's the only v3 derived file missing from calc_scores.qmd. Everything else
   from that notebook is done.



## 2026-02-10.c run sequence after ran ingest_nmfs_core-areas.qmd, update_scores.qmd

Now that I've added the NMFS Core Areas dataset for Rice's whale @workflows/ingest_nmfs_core-areas.qmd. I ran @workflows/update_scores.qmd, including the new pivoting of `taxon` to `taxon` and `taxon_model`, and it says for Next steps:

```
Re-run `calc_scores.qmd` interactively in Positron starting from chunk
`delete_extrisk_metrics` (line 472):

1. `delete_extrisk_metrics` -- clear old metrics
2. `calc_cell_metric_redlist` -- recompute cell-level extinction risk
3. `calc_ecoregion_minmax` through `cell_metric_score` -- rescale and aggregate
4. `zone_taxon` -- per-zone species sensitivity with updated er_score
```

But I am pretty sure I need to run some or all of @workflows/merge_models.qmd next before @workflows/calc_scores.qmd.

I need to also fix at least the Shiny app @apps_2026/mapsp/app.R and probably @apps_2026/mapgl/app.R to account for the new pivoted `taxon` to `taxon` and `taxon_model` structure. The model for rice's whale seems to have incorporated the ca_nmfs dataset correctly (since the core area has a score of 100 for NMFS:EN and not 50 for IUCN:CR), but the sidebar does not show the "ca_nmfs" model (mdl_seq = 23004; see screenshot attached). 

I see the following new fields in table `taxon`: esa_code <chr>, esa_source <chr>, extrisk_code <chr>, er_score <int>, is_mmpa <lgl>, is_mbta <lgl>, is_bcc <lgl>. But I do not see `ca_nmfs` in the `taxon` table, which could partially explain why the app doesn't show that model.

What needs fixing and what is the run sequence to get everything updated and working?

Plan: Integrate ca_nmfs into pipeline and fix apps for taxon/taxon_model schema

 Context

 After running ingest_nmfs_core-areas.qmd (adds Rice's whale core distribution area as ca_nmfs dataset) and update_scores.qmd (pivoted per-dataset columns out of taxon into taxon_model junction table), there
  are three problems:

 1. ca_nmfs is not in taxon_model — update_scores.qmd created taxon_model from tmp_taxon.csv which predates the ca_nmfs ingest. It also lacks rng_iucn rows.
 2. Merged models need re-running — The ms_merge models (which take max across datasets, masked by IUCN range) were not re-created to include ca_nmfs.
 3. Apps reference per-dataset columns in taxon — The mapsp app (apps_2026/mapsp/app.R line 110) selects am_0.05, ch_nmfs, ch_fws, rng_fws, bl, rng_iucn directly from the taxon table. After the schema
 normalization, these need to come from taxon_model via a pivot join. Also ca_nmfs is missing from the app UI entirely.

 Run Sequence

 Step 1: Re-run merge_models.qmd (from chunk notbirds_wormsid_ds_duplicates, line 795)

 The taxonomy resolution chunks (worms/itis/gbif ID updates, marine/extinct flags) are already done and stable. Re-run starting from the pivot step, which will:

 - Pick up ca_nmfs from the species table (the ingest already added it)
 - Build d_nb_ds / d_b_ds wide tables including ca_nmfs column
 - Write taxon table (core columns only, per-dataset columns removed per line 1770)
 - Write taxon_model junction table (with am_0.05, ca_nmfs, ch_nmfs, ch_fws, rng_fws, bl)
 - Add redlist_code, extrisk_code, er_score, is_mmpa, is_mbta, is_bcc (listing join)
 - Add worms_id, worms_is_marine, worms_is_extinct
 - Add rng_iucn rows to taxon_model + update n_ds
 - Add esa_code, esa_source
 - Run merge loop (ms_merge models) — ca_nmfs is already in ds_keys_mask at line 3091
 - Set is_ok flags
 - Export data/taxon.csv

 Chunks to run (in order):
 notbirds_wormsid_ds_duplicates → notbirds_wormsid_ds_pivoted → update_turtle_to_reptile → bird_bl_to_botwid → birds_notbl_to_botwid → birds_notbl_to_botwid_common_ds → birds_notbl_to_botwid_common_acc →
 birds_notbl_to_botwid_rmsubsp → birds_botwid_ds_duplicates → birds_botwid_ds_pivoted → bind_birds_notbirds_ds → notbirds_worms_sci → birds_botw_sci → birds_cmn → notbirds_cmn → taxon_to_db →
 taxon_redlist_code → taxon_listing_join → taxon_worms_id_inclbirds → (worms_is_marine/extinct) → (ESA codes) → add_rng_iucn_to_taxon → insert_dataset_merged → iterate_merge_ds_mdl → taxon_is_ok →
 taxon_summary

 Step 2: Skip update_scores.qmd

 Its functions are fully covered by merge_models.qmd:
 - taxon_model creation ✓ (merge_models taxon_to_db chunk)
 - model_cell value remapping ✓ (already done, values are now 1/50/100)
 - listing join ✓ (merge_models taxon_listing_join chunk)
 - taxon CSV export ✓ (merge_models taxon_summary chunk)

 Step 3: Run calc_scores.qmd from chunk delete_extrisk_metrics (line 472)

 As already noted in the update_scores.qmd next steps:
 1. delete_extrisk_metrics — clear old metrics
 2. calc_cell_metric_redlist — recompute cell-level extinction risk
 3. calc_ecoregion_minmax through cell_metric_score — rescale and aggregate
 4. zone_taxon — per-zone species sensitivity with updated er_score

 Step 4: Fix apps_2026/mapsp/app.R

 Update the app to use the normalized taxon + taxon_model structure and add ca_nmfs support.

 4a. Data prep: join taxon_model to reconstruct per-dataset columns

 File: apps_2026/mapsp/app.R, lines 106-136

 Replace direct select of per-dataset columns from taxon with a pivot-join from taxon_model:

 '# query taxon base data (no per-dataset columns)
 d_spp <- tbl(con_sdm, "taxon") |>
   filter(is_ok, !is.na(mdl_seq)) |>
   select(
     taxon_id, scientific_name, common_name, sp_cat, n_ds, mdl_seq,
     worms_id, redlist_code, esa_code, esa_source) |>
   collect()

 '# pivot taxon_model to get per-dataset mdl_seq columns
 d_tm <- tbl(con_sdm, "taxon_model") |>
   filter(taxon_id %in% !!d_spp$taxon_id) |>
   collect() |>
   pivot_wider(names_from = ds_key, values_from = mdl_seq)

 '# join
 d_spp <- d_spp |>
   left_join(d_tm, by = "taxon_id") |>
   mutate(...)  # existing label/URL mutations

 4b. Add ca_nmfs to ds_cols and UI definitions

 Line 155: Add ca_nmfs to ds_cols:
 ds_cols <- c("am_0.05", "ca_nmfs", "ch_nmfs", "ch_fws", "rng_fws", "bl", "rng_iucn")

 Line 211-219: Add ca_nmfs to radioButtons choices:
 "NMFS Core Area"  = "ca_nmfs",

 Line 240-247: Add to layer_names:
 "ca_nmfs"  = "NMFS Core Distribution Area",

 Lines 329-340: Add to mdl_names and mdl_info:
 '# in mdl_names:
 "ca_nmfs"  = "NMFS Core Area",

 '# in mdl_info:
 "ca_nmfs"  = "Core: 100",

 '# update ch_nmfs values (were remapped by update_scores.qmd):
 "ch_nmfs"  = "EN:100, TN:50",   # was "EN:90, TN:70"

 Lines 367, 371: Add ca_nmfs to ds_keys_values and ds_keys_mask:
 ds_keys_values <- c("am_0.05", "ca_nmfs", "ch_nmfs", "ch_fws", "rng_fws", "bl", "rng_iucn")
 ds_keys_mask <- c("rng_iucn", "ca_nmfs", "ch_nmfs", "ch_fws", "rng_fws")

 Lines 592-599: Add ca_nmfs to available layers check:
 if (!is.na(sp_row$ca_nmfs))    available <- c(available, "NMFS Core Area"  = "ca_nmfs")

 Step 5: Check apps_2026/mapgl/app.R

 The mapgl app does NOT query per-dataset columns from taxon — it uses zone_taxon for species tables and metric/cell_metric for raster layers. No changes needed unless the zone_taxon table columns changed
 (they shouldn't have).

 Files Modified
 ┌───────────────────────┬──────────────────────────────────────────────────────────┐
 │         File          │                          Change                          │
 ├───────────────────────┼──────────────────────────────────────────────────────────┤
 │ apps_2026/mapsp/app.R │ Join taxon_model for per-dataset cols; add ca_nmfs to UI │
 └───────────────────────┴──────────────────────────────────────────────────────────┘
 Files Run (not modified, executed interactively in Positron)
 ┌────────────────────────────┬──────────────────────────────────────────────────────────────────┐
 │            File            │                              Scope                               │
 ├────────────────────────────┼──────────────────────────────────────────────────────────────────┤
 │ workflows/merge_models.qmd │ From chunk notbirds_wormsid_ds_duplicates (line 795) through end │
 ├────────────────────────────┼──────────────────────────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd  │ From chunk delete_extrisk_metrics (line 472) through end         │
 └────────────────────────────┴──────────────────────────────────────────────────────────────────┘
 Verification

 1. After merge_models.qmd: check taxon_model has ca_nmfs rows for Rice's whale (worms_id: 1576133)
 2. After merge_models.qmd: check taxon.mdl_seq for Rice's whale is a merged model incorporating ca_nmfs
 3. After calc_scores.qmd: verify zone_taxon and cell_metric tables are updated
 4. After app fix: run mapsp app locally, select Rice's whale, verify sidebar shows "NMFS Core Area" link and ca_nmfs model is selectable

Please fix Next Steps in @update_scores.qmd, which is what I ran last, with these precise next steps and rationale from the plan.
When should dataset columns in taxon be removed from the table, so we're only pulling from taxon_model for this info?
Could `ds_cols` be updated via query of taxon_model for all datasets and renamed to something more sensible that doesn't referencing columns (`_cols`)?
Likewise could the dataset table be updated with a column or columns if not yet there that gets used for radioButton choices, layer_names, mdl_names, mdl_info, ds_keys_values, ds_keys_mask via query?

⏺ Updated plan                                          
  ⎿  /plan to preview
                                                                                                                                                                                                                
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 Ready to code?                                                                                                                                                                                                 
                                                                                                                                                                                                                
 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Plan: Integrate ca_nmfs, normalize taxon/taxon_model, data-driven app metadata

 Context

 After running ingest_nmfs_core-areas.qmd and update_scores.qmd, three problems exist:

 1. taxon_model is incomplete — Missing ca_nmfs and rng_iucn rows (created from old tmp_taxon.csv)
 2. Merged models stale — ms_merge models don't incorporate ca_nmfs
 3. Apps hardcode dataset metadata — Per-dataset columns queried from taxon (should use taxon_model), and all display names/info/mask flags are hardcoded in mapsp/app.R

 The fix normalizes the schema fully and makes app metadata data-driven from the dataset table.

 ---
 Part A: Schema & Pipeline Changes

 A1. Add metadata columns to dataset table

 Add these columns to the dataset table (via ALTER TABLE in merge_models.qmd, before the merge loop):
 ┌──────────────┬─────────┬──────────────────────────────────────────┬────────────────────────────────────────────────┐
 │    Column    │  Type   │                 Purpose                  │                 Example values                 │
 ├──────────────┼─────────┼──────────────────────────────────────────┼────────────────────────────────────────────────┤
 │ name_display │ VARCHAR │ Short UI label for radioButtons/legends  │ "AquaMaps SDM", "NMFS Core Area", "IUCN Range" │
 ├──────────────┼─────────┼──────────────────────────────────────────┼────────────────────────────────────────────────┤
 │ value_info   │ VARCHAR │ Value explanation tooltip (NULL if none) │ "EN:100, TN:50", "Core: 100", NULL             │
 ├──────────────┼─────────┼──────────────────────────────────────────┼────────────────────────────────────────────────┤
 │ is_mask      │ BOOLEAN │ Participates in IUCN range masking       │ TRUE for all except am_0.05 and ms_merge       │
 ├──────────────┼─────────┼──────────────────────────────────────────┼────────────────────────────────────────────────┤
 │ sort_order   │ INTEGER │ Display ordering in UI                   │ 1=AquaMaps, 2=NMFS Core, 3=NMFS CH, ...        │
 └──────────────┴─────────┴──────────────────────────────────────────┴────────────────────────────────────────────────┘
 Values to populate:
 ┌──────────┬───────────────────────┬─────────────────────────────────────────────┬─────────┬────────────┐
 │  ds_key  │     name_display      │                 value_info                  │ is_mask │ sort_order │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ am_0.05  │ AquaMaps SDM          │ NULL                                        │ FALSE   │ 1          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ ca_nmfs  │ NMFS Core Area        │ "Core: 100"                                 │ TRUE    │ 2          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ ch_nmfs  │ NMFS Critical Habitat │ "EN:100, TN:50"                             │ TRUE    │ 3          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ ch_fws   │ FWS Critical Habitat  │ "EN:100, TN:50"                             │ TRUE    │ 4          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ rng_fws  │ FWS Range             │ "EN:100, TN:50, LC:1"                       │ TRUE    │ 5          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ bl       │ BirdLife Range        │ NULL                                        │ TRUE    │ 6          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ rng_iucn │ IUCN Range            │ "CR:100, EN:80, VU:60, NT:40, LC:20, DD:20" │ TRUE    │ 7          │
 ├──────────┼───────────────────────┼─────────────────────────────────────────────┼─────────┼────────────┤
 │ ms_merge │ Merged Model          │ NULL                                        │ FALSE   │ 0          │
 └──────────┴───────────────────────┴─────────────────────────────────────────────┴─────────┴────────────┘
 File: workflows/merge_models.qmd — add a new chunk after insert_dataset_merged (line 3048) that runs ALTER TABLE + UPDATE statements.

 A2. Derive ds_keys dynamically (replaces hardcoded ds_cols)

 In merge_models.qmd, replace:
 ds_cols <- c("am_0.05", "ca_nmfs", "ch_nmfs", "ch_fws", "rng_fws", "bl")
 With:
 ds_keys <- tbl(con_sdm, "dataset") |>
   filter(!ds_key %in% c("ms_merge")) |>
   pull(ds_key)

 This automatically picks up any new datasets. Used for:
 - Pivoting taxon_model from wide format (merge_models.qmd:1754)
 - Removing per-dataset columns from taxon (merge_models.qmd:1770)
 - Building ds_keys_mask: tbl(con_sdm, "dataset") |> filter(is_mask) |> pull(ds_key)

 A3. Per-dataset columns removed from taxon table

 When: In merge_models.qmd, chunk taxon_to_db (line 1770) — this already does select(-any_of(ds_cols)). After re-running merge_models.qmd, the taxon table will have only core columns. The per-dataset mdl_seq
  values live exclusively in taxon_model.

 taxon table final columns:
 taxon_id, taxon_authority, sp_cat, scientific_name, common_name, n_ds, mdl_seq, redlist_code, extrisk_code, er_score, is_mmpa, is_mbta, is_bcc, worms_id, worms_is_marine, worms_is_extinct, esa_code,
 esa_source, is_ok

 taxon_model junction table columns:
 taxon_id, ds_key, mdl_seq

 ---
 Part B: Run Sequence

 B1. Re-run merge_models.qmd from chunk notbirds_wormsid_ds_duplicates (line 795) through end

 This rebuilds everything:
 - taxon + taxon_model (with ca_nmfs, later rng_iucn + ms_merge rows appended)
 - Merge loop (ca_nmfs already in ds_keys_mask at line 3091)
 - Listing join, ESA codes, is_ok flags, taxon CSV export

 B2. Skip update_scores.qmd

 All its steps are covered by merge_models.qmd. Only edit it to update the Next Steps section (see Part D).

 B3. Run calc_scores.qmd from chunk delete_extrisk_metrics (line 472)

 1. delete_extrisk_metrics → 2. calc_cell_metric_redlist → 3. calc_ecoregion_minmax through cell_metric_score → 4. zone_taxon

 ---
 Part C: Fix apps_2026/mapsp/app.R

 C1. Data prep: pivot taxon_model for per-dataset columns (lines 106-136)

 Replace hardcoded select of per-dataset columns from taxon:

 '# query taxon base data (no per-dataset columns)
 d_spp <- tbl(con_sdm, "taxon") |>
   filter(is_ok, !is.na(mdl_seq)) |>
   select(
     taxon_id, scientific_name, common_name, sp_cat, n_ds, mdl_seq,
     worms_id, redlist_code, esa_code, esa_source) |>
   collect()

 '# pivot taxon_model to wide format, join to taxon
 d_tm <- tbl(con_sdm, "taxon_model") |>
   filter(taxon_id %in% !!d_spp$taxon_id) |>
   collect() |>
   pivot_wider(names_from = ds_key, values_from = mdl_seq)

 d_spp <- d_spp |>
   left_join(d_tm, by = "taxon_id") |>
   mutate(...)  # existing label/URL mutations

 C2. Query dataset table for all metadata (replaces hardcoded maps)

 Replace all hardcoded named vectors with queries from dataset:

 '# query dataset metadata once at startup
 d_datasets <- tbl(con_sdm, "dataset") |>
   select(ds_key, name_display, value_info, is_mask, sort_order) |>
   collect() |>
   arrange(sort_order)

 '# derive what was previously hardcoded
 ds_keys     <- d_datasets |> filter(!ds_key %in% c("ms_merge")) |> pull(ds_key)
 layer_names <- c("mdl_seq" = "Merged Model", deframe(d_datasets |> filter(ds_key != "ms_merge") |> select(ds_key, name_display)))
 mdl_names   <- deframe(d_datasets |> filter(ds_key != "ms_merge") |> select(ds_key, name_display))
 mdl_info    <- deframe(d_datasets |> filter(!is.na(value_info)) |> select(ds_key, value_info))
 ds_keys_mask <- d_datasets |> filter(is_mask) |> pull(ds_key)

 C3. Update radioButtons and available layers (lines 211-219, 592-599)

 Build radioButtons choices dynamically from d_datasets:
 radio_choices <- c("Merged Model" = "mdl_seq")
 for (i in seq_len(nrow(d_datasets))) {
   dk <- d_datasets$ds_key[i]
   if (dk != "ms_merge") radio_choices[d_datasets$name_display[i]] <- dk
 }

 Build available layers dynamically:
 available <- c()
 if (!is.na(sp_row$mdl_seq)) available <- c(available, "Merged Model" = "mdl_seq")
 for (dk in ds_keys) {
   if (dk %in% names(sp_row) && !is.na(sp_row[[dk]])) {
     available <- c(available, setNames(dk, d_datasets$name_display[d_datasets$ds_key == dk]))
   }
 }

 C4. No changes needed for apps_2026/mapgl/app.R

 Uses zone_taxon and metric/cell_metric, not per-dataset taxon columns.

 ---
 Part D: Update update_scores.qmd Next Steps section

 File: workflows/update_scores.qmd, lines 411-419

 Replace with:

 '## Next steps

 Re-run `merge_models.qmd` interactively in Positron starting from chunk
 `notbirds_wormsid_ds_duplicates` (line 795). This rebuilds `taxon` (without
 per-dataset columns) and `taxon_model` (junction table with all dataset
 mdl_seq values including `ca_nmfs` and `rng_iucn`), re-runs the merge loop
 to create `ms_merge` models, populates listing/ESA/is_ok fields, and exports
 `data/taxon.csv`.

 Then re-run `calc_scores.qmd` starting from chunk `delete_extrisk_metrics`
 (line 472):

 1. `delete_extrisk_metrics` -- clear old metrics
 2. `calc_cell_metric_redlist` -- recompute cell-level extinction risk
 3. `calc_ecoregion_minmax` through `cell_metric_score` -- rescale and aggregate
 4. `zone_taxon` -- per-zone species sensitivity with updated er_score

 ---
 Files Modified
 ┌─────────────────────────────┬────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │            File             │                                                                          Changes                                                                           │
 ├─────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
 │ workflows/update_scores.qmd │ Update Next Steps section (lines 411-419)                                                                                                                  │
 ├─────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
 │ workflows/merge_models.qmd  │ Add dataset metadata columns chunk; replace hardcoded ds_cols with dynamic ds_keys query (line 1754); derive ds_keys_mask from dataset.is_mask (line 3091) │
 ├─────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
 │ apps_2026/mapsp/app.R       │ Pivot-join from taxon_model; query dataset table for all metadata; remove all hardcoded ds_key lists                                                       │
 └─────────────────────────────┴────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
 Files Run Interactively (not modified by Claude)
 ┌────────────────────────────┬────────────────────────────────────────────────────────────┐
 │            File            │                           Scope                            │
 ├────────────────────────────┼────────────────────────────────────────────────────────────┤
 │ workflows/merge_models.qmd │ From notbirds_wormsid_ds_duplicates (line 795) through end │
 ├────────────────────────────┼────────────────────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd  │ From delete_extrisk_metrics (line 472) through end         │
 └────────────────────────────┴────────────────────────────────────────────────────────────┘
 Verification

 1. After merge_models: SELECT ds_key, name_display, is_mask FROM dataset shows all 8 datasets with metadata
 2. After merge_models: SELECT * FROM taxon_model WHERE taxon_id = 1576133 shows ca_nmfs row for Rice's whale
 3. After merge_models: taxon table has NO per-dataset columns (only core + metadata columns)
 4. After calc_scores: zone_taxon and cell_metric tables updated
 5. After app fix: run mapsp locally, select Rice's whale → sidebar shows "NMFS Core Area" link, all layer switches work
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

⏺ All changes are implemented. Here's a summary of what was done:                                                                                                                                               
                                                                                   
  Files Modified

  workflows/update_scores.qmd — Updated Next Steps section to reference the full pipeline: re-run merge_models.qmd from notbirds_wormsid_ds_duplicates first, then calc_scores.qmd.                             
    
  workflows/merge_models.qmd — Three changes:                                                                                                                                                                   
  1. Line 1754: Replaced hardcoded ds_cols vector with dynamic ds_keys query from dataset table (also updated references at lines 1758, 1773)                                                                   
  2. After line 3048: Added new dataset_metadata chunk that ALTERs the dataset table to add name_display, value_info, is_mask, sort_order columns and populates them for all 8 datasets                         
  3. Line 3091: Replaced hardcoded ds_keys_mask with dynamic query: tbl(con_sdm, "dataset") |> filter(is_mask) |> pull(ds_key)                                                                                  

  apps_2026/mapsp/app.R — Made fully data-driven:
  1. Data prep (lines 102-185): Queries d_datasets from the dataset table at startup; derives ds_keys, layer_names, mdl_names, mdl_info, ds_keys_mask from it; queries taxon without per-dataset columns; pivots
   taxon_model to wide format and joins
  2. RadioButtons (line 228): Built dynamically from d_datasets
  3. Server: Removed hardcoded layer_names (now top-level); removed hardcoded mdl_names, mdl_info, ds_keys_values, ds_keys_mask from species_info; builds available layers dynamically in
  observeEvent(input$sel_sp)

I am manually executing R chunks from chunk `notbirds_wormsid_ds_duplicates` (line 795) in @workflows/merge_models.qmd, and having trouble with R chunk "taxon_listing_join" presumably with lines 1981 - 1992. Not having `worms_id` column in table `taxon` is deeply concerning -- is this now defined differently and what are the consequences for the rest of the code, per Next Steps in @workfolows/update_scores.qmd?

⏺ Here's the explanation:             

  Why worms_id is missing: The d_taxon data frame (and thus d_taxon_clean) never had a worms_id column — it uses taxon_id as the      
  primary key, where taxon_id = worms_id for non-birds and taxon_id = botw_id for birds. Previously, DELETE FROM taxon preserved the
  table schema (including worms_id from prior ALTER TABLE calls), and data went into the existing columns. With overwrite = T, the    
  table is recreated fresh with only d_taxon_clean columns. 

  The fix I just applied (in taxon_to_db chunk, right after dbWriteTable):
  dbExecute(con_sdm, "ALTER TABLE taxon ADD COLUMN IF NOT EXISTS worms_id INTEGER")
  dbExecute(con_sdm, "UPDATE taxon SET worms_id = taxon_id WHERE taxon_authority = 'worms'")

  Downstream impact — 3 SQL statements reference taxon.worms_id:
  1. Line ~1991 (taxon_listing_join): WHERE taxon.worms_id = l.worms_id — now works
  2. Line ~2994: WHERE taxon.worms_id = s.worms_id for rng_iucn redlist update — now works
  3. Line ~3043 (add_esa-code_in_taxon): WHERE taxon.worms_id = d_esa.worms_id — now works

  No impact on the rest of the pipeline:
  - worms_is_marine / worms_is_extinct are already added via ALTER TABLE at line ~2207
  - calc_scores.qmd doesn't use worms_id (it works with taxon_id, er_score, etc.)
  - The app's d_spp query selects worms_id — now it will be there
  - The is_ok computation (line ~3601) uses worms_id — now works

  You can re-run the taxon_to_db chunk and continue from taxon_listing_join.


Re-ran the taxon_to_db chunk and continued from taxon_listing_join and got error...


⏺ The good news: all the UPDATE statements succeeded (listing join: 349 + 137 taxa, IUCN fallback: 6701, default er_score: 10374). The
   error is only in the diagnostic summary at the end.
                                                                                                                                      
  The issue is CASE_WHEN — that's not a valid R/dplyr function. It should be case_when (lowercase). Let me fix it.
                                                                                                                                      
⏺ Searched for 1 pattern, read 1 file (ctrl+o to expand)     

⏺ Update(workflows/merge_models.qmd)
  ⎿  Added 1 line, removed 1 line
      2059  # summary                                                                                                                 
      2060  tbl(con_sdm, "taxon") |>
      2061    mutate(                                                                                                                 
      2062 -    authority = CASE_WHEN(                                                                                         
      2062 +    authority = case_when(
      2063        str_starts(extrisk_code, "NMFS") ~ "NMFS",
      2064        str_starts(extrisk_code, "FWS") ~ "FWS",
      2065        str_starts(extrisk_code, "IUCN") ~ "IUCN",

⏺ Fixed. CASE_WHEN is SQL syntax, not an R function — dplyr uses case_when (lowercase).                                               
  
  Your data is fine though — all the UPDATEs completed before the summary errored. You can re-run just the summary part, or simply    
  continue to the next chunk. 


The chunk "update_rng_iucn_to_redlist" seems to be applying the old scoring system, not the new one:

```r
rl_score =                
  case_match(              
    redlist_code,                                                                           # skipped: EX (extinct), NE (not evaluated), DD (data deficient), <NA> (not available)
    # https://oceanhealthindex.org/images/htmls/Supplement.html#62_Biodiversity
    "CR" ~ 1, #  - CR: Critically Endangered
    "EN" ~ 0.8, #  - EN: Endangered (+ESA)
    "VU" ~ 0.6, #  - VU: Vulnerable               
    "TN" ~ 0.6, #  - TN: Threatened (ESA only)
    "NT" ~ 0.4, #  - NT: Near Threatened    
    "LC" ~ 0.2, #  - LC: Least Concern   
    "DD" ~ 0.2 #  - DD: Data Deficient, so at least use distributional info and apply minimal rank
  )  
```

⏺ This chunk is actually correct for what it does, despite the confusing variable name. Let me explain the distinction:               
                                    
  Two different scoring systems, two different purposes:
  ┌─────────┬───────────────────────────────────────────┬─────────────────────────────────────┐
  │         │       model_cell.value (this chunk)       │ taxon.er_score (taxon_listing_join) │                                      
  ├─────────┼───────────────────────────────────────────┼─────────────────────────────────────┤
  │ Purpose │ Cell values in rng_iucn spatial raster    │ Per-taxon extinction risk weight    │
  ├─────────┼───────────────────────────────────────────┼─────────────────────────────────────┤
  │ Used by │ Merge loop (ms_merge)                     │ calc_scores.qmd sensitivity calc    │
  ├─────────┼───────────────────────────────────────────┼─────────────────────────────────────┤
  │ Values  │ CR:100, EN:80, VU:60, NT:40, LC:20, DD:20 │ CR:50, EN:25, VU:5, NT:2, else:1    │
  └─────────┴───────────────────────────────────────────┴─────────────────────────────────────┘
  The rl_score * 100 math produces the correct cell values:
  - CR: 1.0 × 100 = 100, EN: 0.8 × 100 = 80, VU: 0.6 × 100 = 60, NT: 0.4 × 100 = 40, LC: 0.2 × 100 = 20, DD: 0.2 × 100 = 20

  These match exactly what the value_info metadata says for rng_iucn: "CR:100, EN:80, VU:60, NT:40, LC:20, DD:20".

  The variable name rl_score is legacy naming but the output values are correct. The TN:60 case is a no-op since TN is ESA-only and
  never appears in IUCN data.

  This chunk is safe to run as-is. Do you want me to rename the variable from rl_score to something clearer (like cell_value), or is it fine to leave and move on?

Actually we want to apply the same new IUCN scoring (CR:50, EN:25, VU:5, NT:2, else:1) for both cell values and per-taxon weight. What else do we need to change and re-run to accomplish this? I know it's redundant, and something to flag for fixing later, but the way we need to proceed for now. It will only exagerate the non-linear nature of the scores, but that's OK.

⏺ Also update in apps_2026/mapsp/app.R — the app reads value_info from the DB, so it will pick up the new values automatically after re-running dataset_metadata. No app code changes needed.
                                                                            
  Summary of changes:

  1. update_rng_iucn_to_redlist-value: scoring changed from CR:100/EN:80/VU:60/NT:40/LC:20/DD:20  to CR:50/EN:25/VU:5/NT:2/LC:1/DD:1 (aligned with er_score)
  2. dataset_metadata: rng_iucn value_info updated to match

  What to re-run:
  - From update_rng_iucn_to_redlist-value through end of merge_models.qmd (this updates rng_iucn cell values, then the merge loop will
   incorporate the new values into ms_merge models, then dataset_metadata populates the DB)
  - Then calc_scores.qmd from delete_extrisk_metrics as before

I'm running chunk `zone_taxon` and noticing use of old redlist score. Does this chunk look in keeping with the latest extinction risk scoring?

```r
      suit_rl = avg_suit * er_score / 100,
      suit_rl_area = avg_suit * er_score / 100 * area_km2
```                                                                                               

⏺ The math is actually correct for the v3 system. Here's why:                                                                         
                              
  - avg_suit = mean(value) / 100 — model cell values (1-100) normalized to 0-1                                                        
  - er_score — integer 1-100 from taxon table (v3 system: CR:50, EN:25, VU:5, NT:2, default:1)
  - er_score / 100 — converts to 0-1 multiplier                                                                                       
  - suit_rl = avg_suit * er_score / 100 — suitability weighted by extinction risk (0-1 × 0-1)

  The formula correctly uses er_score (not the old rl_score), and the /100 normalizes the integer er_score to a 0-1 weight. The column
   names suit_rl are legacy naming but the computation is v3-correct.

  The only cosmetic issue is the column names still say rl (redlist). If you want to rename for clarity:

  - suit_rl → suit_er
  - suit_rl_area → suit_er_area
  - cat_suit_rl_area → cat_suit_er_area

  But that would cascade into downstream code (mapgl app, PostGIS updates, etc.) that references these column names. I'd recommend
  leaving the names as-is for now and flagging for a future cleanup pass. The math is sound — proceed with running the chunk.

I'm at the R chunk `setup_update_pgtable_with_clear` and realizing that we want to keep the PostGIS tables from earlier versions for the mapping apps `@apps_2026/mapgl/` and `@apps_2026/mapsp/`. Plus we want Program Areas for v3 Planning Areas.  Can you think about implications of appending versions to tables (eg `ply_programareas_2026_v3`) and come up with plan for execution?

⏺ For v3, should the DuckDB zone table references also be versioned? Currently zone.tbl stores 'ply_programareas_2026' — the calc_scores
 queries filter by this string. If we version the PostGIS names but not the DuckDB zone.tbl values, the DuckDB side stays clean but   
the names diverge. Or should zone.tbl also become 'ply_programareas_2026_v3'? 

Apply to both PostGIS + DuckDB. Update plan with changes to `@workflows/calc_scores.qmd` to include version specific suffixes (eg `_v3`) to output tables in databases as well as all output files. Use an easily updated variable, (eg `v_int = 3`) at the top of the notebook (ie in `setup` chunk) to easily iterate in future.

⏺ This uses the variables pa_gpkg, er_gpkg, lyrs_csv, metrics_tif. The first two are versioned, lyrs_csv is the unversioned layers.csv (an input) and metrics_tif is the unversioned r_metrics.tif (also an input). Those are fine — the outputs lyrs_2026_csv and              
  metrics_2026_tif are versioned via the setup variables.   
                                                                                                                                                                                                                                                                                
  All changes are complete. Here's a summary:               
                                                                                                                                                                                                                                                                                
  Summary of changes                                                                                                                                                                                                                                                            

  workflows/calc_scores.qmd:
  - Added v_int <- 3 and v_sfx <- paste0("_v", v_int) in setup chunk
  - Versioned all output file paths: gpkg, csv, tif files get _v3 suffix
  - Versioned dbWriteTable for zone_taxon → zone_taxon_v3
  - Added version_zone_tbl chunk to rename DuckDB zone.tbl values with _v3
  - Added create_pg_v3_tables chunk to create PostGIS tables from existing v2 tables
  - Updated all PostGIS write calls (update_table_with_clear) to target _v3 tables
  - Updated DuckDB zone filter queries (for pa, er, pra metrics) to use versioned tbl values
  - Updated pg_rm_old_metrics to reference versioned table names
  - Updated add_index_for_vector_tiles to reference versioned tables
  - Updated update_cached_downloads st_read calls to read from versioned PostGIS tables

  apps_2026/mapgl/app.R:
  - Added v_int/v_sfx variables
  - Versioned gpkg, lyrs_csv, metrics_tif paths
  - Versioned tilejson URLs and source_layer references (er, pra)
  - Versioned dbListFields call
  - Versioned DuckDB zone.tbl filters (ply_subregions_2026 → with v_sfx)
  - Versioned zone_taxon table references (subregion and programarea queries)

  apps_2026/mapsp/app.R:
  - Added v_int/v_sfx variables
  - Versioned tilejson URLs and all source_layer references (er, pra, including add_er_fill_layer helper)

⏺ I was about to submit the versioning plan for your approval. Let me do that now.

 Here is Claude's plan:
 Plan: Version PostGIS tables and outputs for v3 scoring

 Context

 We're at setup_update_pgtable_with_clear in calc_scores.qmd, about to write v3 metric scores to PostGIS. The existing PostGIS tables
  contain v2 metrics used by live apps. Writing v3 directly would overwrite v2 data. We need versioned output tables, files, and
 DuckDB references so v2 and v3 coexist.

 The approach: add v_int = 3 to the setup chunk and derive a _v3 suffix used for all output table names (PostGIS + DuckDB) and output
  file names.

 ---
 Part 1: calc_scores.qmd — setup chunk changes

 File: workflows/calc_scores.qmd, lines 78-107 (setup chunk)

 1a. Add version variable after line 81 (librarian::shelf(...))

 `# version ----
 v_int <- 3
 v_sfx <- paste0("_v", v_int)

 1b. Version the output file paths (lines 90-107)

 Replace hardcoded file paths with versioned ones. Keep the base (unversioned) paths as comments for reference:

 pa_gpkg          <- glue("{dir_derived}/ply_planareas_2025{v_sfx}.gpkg")
 er_gpkg          <- glue("{dir_derived}/ply_ecoregions_2025{v_sfx}.gpkg")
 sr_gpkg          <- glue("{dir_derived}/ply_subregions_2025{v_sfx}.gpkg")
 pra_gpkg         <- glue("{dir_derived}/ply_programareas_2026{v_sfx}.gpkg")
 zone_taxon_csv   <- glue("{dir_derived}/zone_taxon_2026{v_sfx}.csv")
 lyrs_2026_csv    <- glue("{dir_derived}/layers_2026{v_sfx}.csv")
 metrics_2026_tif <- glue("{dir_derived}/r_metrics_2026{v_sfx}.tif")
 metrics_akl48_tif <- glue("{dir_derived}/r_metrics_akl48{v_sfx}.tif")

 Keep existing unversioned paths (e.g., pra_raw_gpkg, sr_pa_csv, sr_pra_csv, sdm_db, cell_tif, pa_boem_geo) unchanged — these are
 inputs, not outputs.

 ---
 Part 2: calc_scores.qmd — DuckDB zone.tbl versioning

 The DuckDB zone table stores tbl values like "ply_programareas_2026". The mapgl app filters by these values. To version DuckDB
 consistently, update zone.tbl values to include v_sfx.

 2a. Add a chunk after source(here("libs/db.R")) (line 2732), before PostGIS writes

 New chunk version_zone_tbl:

 `# update zone.tbl to include version suffix ----
 zone_tbls <- tbl(con_sdm, "zone") |> distinct(tbl) |> pull(tbl)
 for (zt in zone_tbls) {
   zt_v <- if (!grepl("_v\\d+$", zt)) paste0(zt, v_sfx) else zt
   if (zt != zt_v) {
     dbExecute(con_sdm, glue(
       "UPDATE zone SET tbl = '{zt_v}' WHERE tbl = '{zt}'"))
     message(glue("zone.tbl: {zt} -> {zt_v}"))
   }
 }

 This is idempotent: if already versioned, no-op.

 2b. Version zone_taxon DuckDB table (line 2663)

 `# before:
 dbWriteTable(con_sdm, "zone_taxon", d, overwrite = T)

 `# after:
 dbWriteTable(con_sdm, glue("zone_taxon{v_sfx}"), d, overwrite = T)

 Also version the CSV export (line 2660) — already handled by 1b since zone_taxon_csv is versioned.

 ---
 Part 3: calc_scores.qmd — PostGIS table versioning

 3a. New chunk create_pg_v3_tables (after setup_update_pgtable_with_clear)

 Create versioned PostGIS tables as full copies of existing tables (geometry + existing columns), then clear metric data:

 pg_zone_tbls <- c("ply_planareas_2025", "ply_ecoregions_2025", "ply_programareas_2026")
 for (pg_tbl in pg_zone_tbls) {
   pg_tbl_v <- glue("{pg_tbl}{v_sfx}")
   if (!dbExistsTable(con, pg_tbl_v)) {
     dbExecute(con, glue("CREATE TABLE {pg_tbl_v} AS SELECT * FROM {pg_tbl}"))
     message(glue("Created {pg_tbl_v} from {pg_tbl}"))
   } else {
     message(glue("{pg_tbl_v} already exists"))
   }
 }

 3b. Update pg_update_planarea_ecoregion_metrics_2025 chunk (lines 2814-2820)

 update_table_with_clear(con, d_pa_metrics, glue("ply_planareas_2025{v_sfx}"),    "planarea_key")
 update_table_with_clear(con, d_er_metrics, glue("ply_ecoregions_2025{v_sfx}"),   "ecoregion_key")

 3c. Update pg_update_programareas_metrics_2026 chunk (line 2866)

 update_table_with_clear(con, d_pra_metrics, glue("ply_programareas_2026{v_sfx}"), "programarea_key")

 3d. Update pg_rm_old_metrics chunk (lines 2873-2930)

 Reference versioned tables:

 flds_pa_db <- dbListFields(con, glue("ply_planareas_2025{v_sfx}"))
 flds_er_db <- dbListFields(con, glue("ply_ecoregions_2025{v_sfx}"))
 `# ... and in the ALTER TABLE DROP loop:
 tbl <- d_rm$tbl[i]  # these values already include v_sfx if d_rm is built from the versioned table names

 3e. Update add_index_for_vector_tiles chunk (lines 2941-2954)

 dbExecute(con, glue(
   "ALTER TABLE public.ply_planareas_2025{v_sfx} ADD COLUMN IF NOT EXISTS planarea_id SERIAL PRIMARY KEY;"))
 dbExecute(con, glue(
   "ALTER TABLE public.ply_programareas_2026{v_sfx} ALTER COLUMN programarea_id TYPE INTEGER;"))
 dbExecute(con, glue(
   "ALTER TABLE public.ply_ecoregions_2025{v_sfx} ADD COLUMN IF NOT EXISTS ecoregion_id SERIAL PRIMARY KEY;"))

 3f. Update update_cached_downloads chunk (lines 2992-3010)

 The gpkg paths are already versioned from Part 1b. Update the st_read calls:

 st_read(con, glue("ply_planareas_2025{v_sfx}")) |> st_write(pa_gpkg, ...)
 st_read(con, glue("ply_programareas_2026{v_sfx}")) |> st_write(pra_gpkg, ...)
 st_read(con, glue("ply_ecoregions_2025{v_sfx}")) |> st_write(er_gpkg, ...)

 ---
 Part 4: apps_2026/mapgl/app.R

 4a. Tilejson URLs (lines 696, 704)

 url = "https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025_v3"
 url = "https://api.marinesensitivity.org/tilejson?table=public.ply_programareas_2026_v3"

 4b. Source layer references (lines 722, 730, 974)

 All source_layer = "public.ply_programareas_2026" → "public.ply_programareas_2026_v3"
 All source_layer = "public.ply_ecoregions_2025" → "public.ply_ecoregions_2025_v3"

 4c. gpkg paths (lines 62, 67)

 er_gpkg  <- glue("{dir_data}/derived/ply_ecoregions_2025_v3.gpkg")
 pra_gpkg <- glue("{dir_data}/derived/ply_programareas_2026_v3.gpkg")

 4d. dbListFields (line 332)

 lyrs_pra <- dbListFields(con, "ply_programareas_2026_v3")

 4e. DuckDB zone.tbl filter (line 416)

 tbl == "ply_subregions_2026_v3"

 4f. DuckDB zone_taxon reference (lines 1244, 1375)

 tbl(con_sdm, "zone_taxon_v3")

 ---
 Part 5: apps_2026/mapsp/app.R

 5a. Tilejson URLs and source_layer references

 - Line 466: ply_ecoregions_2025 → ply_ecoregions_2025_v3 (tilejson URL)
 - Line 474: ply_programareas_2026 → ply_programareas_2026_v3 (tilejson URL)
 - Line 487: ply_programareas_2026 → ply_programareas_2026_v3 (source_layer)
 - Line 495: ply_ecoregions_2025 → ply_ecoregions_2025_v3 (source_layer)
 - Line 563: ply_ecoregions_2025 → ply_ecoregions_2025_v3 (source_layer)

 ---

 Leave untouched
 
 ┌─────────────────────────────────────────┬───────────────────────────────────────────────────────────────────┐
 │                  Item                   │                              Reason                               │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ apps_2026/storymap/                     │ Uses v2 planareas/ecoregions — keep as-is                         │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ api/plumber.R                           │ Generic tilejson — table name comes from query param              │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ server/docker-compose.yml               │ pg_tileserv auto-discovers all tables                             │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ DuckDB cell_metric, zone_metric, metric │ Internal computation tables, overwritten each run — not versioned │
 └─────────────────────────────────────────┴───────────────────────────────────────────────────────────────────┘
 ---
 Files Modified
 File: workflows/calc_scores.qmd
 Changes: Add v_int/v_sfx in setup; version output file paths; add version_zone_tbl chunk; add create_pg_v3_tables chunk; update all
   PostGIS write calls, index calls, gpkg exports, and zone_taxon DuckDB write to use v_sfx
 ────────────────────────────────────────
 File: apps_2026/mapgl/app.R
 Changes: Update tilejson URLs, source_layer refs, gpkg paths, DuckDB zone.tbl filter, zone_taxon table name — all to _v3
 ────────────────────────────────────────
 File: apps_2026/mapsp/app.R
 Changes: Update tilejson URLs and source_layer refs to _v3
 Run Sequence

 1. Edit calc_scores.qmd as described
 2. Re-run from setup_update_pgtable_with_clear through update_cached_downloads
 3. Edit + test mapgl and mapsp apps locally

 Verification

 1. PostGIS: \dt public.ply_* shows both v2 (unversioned) and v3 tables side by side
 2. v2 tables unchanged: SELECT count(*) FROM ply_programareas_2026 WHERE score_extriskspcat_primprod_ecoregionrescaled_equalweights
 IS NOT NULL → same as before
 3. v3 tables populated: same query on ply_programareas_2026_v3 → returns 20
 4. pg_tileserv auto-discovers v3: https://tile.marinesensitivity.org/public.ply_programareas_2026_v3.json returns valid TileJSON
 5. DuckDB: tbl(con_sdm, "zone") |> distinct(tbl) |> collect() shows _v3 suffixed names
 6. DuckDB: tbl(con_sdm, "zone_taxon_v3") returns data
 7. mapgl app: run locally, confirm program area fills render with v3 metrics
 8. mapsp app: ecoregion and program area outlines render
 9. storymap: still works with v2 tables (untouched)

In @apps_2026/mapgl/app.R and @apps_2026/mapsp/app.R, similarly use v_int and v_sfx to update reading of versioned data.

⏺ Updated the plan. Both apps now use v_int = 3 / v_sfx = paste0("_v", v_int) variables at the top, with all versioned references using glue() interpolation — same pattern as calc_scores.qmd. This makes future version bumps a single-variable change in each file.

 Here is Claude's plan:                                 
 Plan: Version PostGIS tables and outputs for v3 scoring

 Context

 We're at setup_update_pgtable_with_clear in calc_scores.qmd, about to write v3 metric scores to PostGIS. The existing PostGIS tables contain v2 metrics used by live apps. Writing v3 directly would overwrite v2 data. We need versioned output tables, files, and DuckDB
 references so v2 and v3 coexist.

 The approach: add v_int = 3 to the setup chunk and derive a _v3 suffix used for all output table names (PostGIS + DuckDB) and output file names.

 ---
 Part 1: calc_scores.qmd — setup chunk changes

 File: workflows/calc_scores.qmd, lines 78-107 (setup chunk)

 1a. Add version variable after line 81 (librarian::shelf(...))

 `# version ----
 v_int <- 3
 v_sfx <- paste0("_v", v_int)

 1b. Version the output file paths (lines 90-107)

 Replace hardcoded file paths with versioned ones. Keep the base (unversioned) paths as comments for reference:

 pa_gpkg          <- glue("{dir_derived}/ply_planareas_2025{v_sfx}.gpkg")
 er_gpkg          <- glue("{dir_derived}/ply_ecoregions_2025{v_sfx}.gpkg")
 sr_gpkg          <- glue("{dir_derived}/ply_subregions_2025{v_sfx}.gpkg")
 pra_gpkg         <- glue("{dir_derived}/ply_programareas_2026{v_sfx}.gpkg")
 zone_taxon_csv   <- glue("{dir_derived}/zone_taxon_2026{v_sfx}.csv")
 lyrs_2026_csv    <- glue("{dir_derived}/layers_2026{v_sfx}.csv")
 metrics_2026_tif <- glue("{dir_derived}/r_metrics_2026{v_sfx}.tif")
 metrics_akl48_tif <- glue("{dir_derived}/r_metrics_akl48{v_sfx}.tif")

 Keep existing unversioned paths (e.g., pra_raw_gpkg, sr_pa_csv, sr_pra_csv, sdm_db, cell_tif, pa_boem_geo) unchanged — these are inputs, not outputs.

 ---
 Part 2: calc_scores.qmd — DuckDB zone.tbl versioning

 The DuckDB zone table stores tbl values like "ply_programareas_2026". The mapgl app filters by these values. To version DuckDB consistently, update zone.tbl values to include v_sfx.

 2a. Add a chunk after source(here("libs/db.R")) (line 2732), before PostGIS writes

 New chunk version_zone_tbl:

 `# update zone.tbl to include version suffix ----
 zone_tbls <- tbl(con_sdm, "zone") |> distinct(tbl) |> pull(tbl)
 for (zt in zone_tbls) {
   zt_v <- if (!grepl("_v\\d+$", zt)) paste0(zt, v_sfx) else zt
   if (zt != zt_v) {
     dbExecute(con_sdm, glue(
       "UPDATE zone SET tbl = '{zt_v}' WHERE tbl = '{zt}'"))
     message(glue("zone.tbl: {zt} -> {zt_v}"))
   }
 }

 This is idempotent: if already versioned, no-op.

 2b. Version zone_taxon DuckDB table (line 2663)

 `# before:
 dbWriteTable(con_sdm, "zone_taxon", d, overwrite = T)

 `# after:
 dbWriteTable(con_sdm, glue("zone_taxon{v_sfx}"), d, overwrite = T)

 Also version the CSV export (line 2660) — already handled by 1b since zone_taxon_csv is versioned.

 ---
 Part 3: calc_scores.qmd — PostGIS table versioning

 3a. New chunk create_pg_v3_tables (after setup_update_pgtable_with_clear)

 Create versioned PostGIS tables as full copies of existing tables (geometry + existing columns), then clear metric data:

 pg_zone_tbls <- c("ply_planareas_2025", "ply_ecoregions_2025", "ply_programareas_2026")
 for (pg_tbl in pg_zone_tbls) {
   pg_tbl_v <- glue("{pg_tbl}{v_sfx}")
   if (!dbExistsTable(con, pg_tbl_v)) {
     dbExecute(con, glue("CREATE TABLE {pg_tbl_v} AS SELECT * FROM {pg_tbl}"))
     message(glue("Created {pg_tbl_v} from {pg_tbl}"))
   } else {
     message(glue("{pg_tbl_v} already exists"))
   }
 }

 3b. Update pg_update_planarea_ecoregion_metrics_2025 chunk (lines 2814-2820)

 update_table_with_clear(con, d_pa_metrics, glue("ply_planareas_2025{v_sfx}"),    "planarea_key")
 update_table_with_clear(con, d_er_metrics, glue("ply_ecoregions_2025{v_sfx}"),   "ecoregion_key")

 3c. Update pg_update_programareas_metrics_2026 chunk (line 2866)

 update_table_with_clear(con, d_pra_metrics, glue("ply_programareas_2026{v_sfx}"), "programarea_key")

 3d. Update pg_rm_old_metrics chunk (lines 2873-2930)

 Reference versioned tables:

 flds_pa_db <- dbListFields(con, glue("ply_planareas_2025{v_sfx}"))
 flds_er_db <- dbListFields(con, glue("ply_ecoregions_2025{v_sfx}"))
 `# ... and in the ALTER TABLE DROP loop:
 tbl <- d_rm$tbl[i]  # these values already include v_sfx if d_rm is built from the versioned table names

 3e. Update add_index_for_vector_tiles chunk (lines 2941-2954)

 dbExecute(con, glue(
   "ALTER TABLE public.ply_planareas_2025{v_sfx} ADD COLUMN IF NOT EXISTS planarea_id SERIAL PRIMARY KEY;"))
 dbExecute(con, glue(
   "ALTER TABLE public.ply_programareas_2026{v_sfx} ALTER COLUMN programarea_id TYPE INTEGER;"))
 dbExecute(con, glue(
   "ALTER TABLE public.ply_ecoregions_2025{v_sfx} ADD COLUMN IF NOT EXISTS ecoregion_id SERIAL PRIMARY KEY;"))

 3f. Update update_cached_downloads chunk (lines 2992-3010)

 The gpkg paths are already versioned from Part 1b. Update the st_read calls:

 st_read(con, glue("ply_planareas_2025{v_sfx}")) |> st_write(pa_gpkg, ...)
 st_read(con, glue("ply_programareas_2026{v_sfx}")) |> st_write(pra_gpkg, ...)
 st_read(con, glue("ply_ecoregions_2025{v_sfx}")) |> st_write(er_gpkg, ...)

 ---
 Part 4: apps_2026/mapgl/app.R

 4a. Add version variables (after line 46, in # variables ---- section)

 `# version ----
 v_int <- 3
 v_sfx <- paste0("_v", v_int)

 4b. Version output file paths (lines 62-68)

 Use v_sfx in gpkg, lyrs_csv, and metrics_tif paths:

 er_gpkg     <- glue("{dir_data}/derived/ply_ecoregions_2025{v_sfx}.gpkg")
 lyrs_csv    <- glue("{dir_data}/derived/layers_2026{v_sfx}.csv")
 metrics_tif <- glue("{dir_data}/derived/r_metrics_2026{v_sfx}.tif")
 pra_gpkg    <- glue("{dir_data}/derived/ply_programareas_2026{v_sfx}.gpkg")

 4c. Tilejson URLs (lines 466, 474 in mapsp-style line refs; mapgl lines 696, 704)

 Use glue() with v_sfx:

 url = glue("https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025{v_sfx}")
 url = glue("https://api.marinesensitivity.org/tilejson?table=public.ply_programareas_2026{v_sfx}")

 4d. Source layer references (lines 722, 730, 974)

 Use glue() with v_sfx:

 source_layer = glue("public.ply_programareas_2026{v_sfx}")
 source_layer = glue("public.ply_ecoregions_2025{v_sfx}")

 Also update the add_er_fill_layer helper (line 563):

 source_layer = glue("public.ply_ecoregions_2025{v_sfx}")

 4e. dbListFields (line 332)

 lyrs_pra <- dbListFields(con, glue("ply_programareas_2026{v_sfx}"))

 4f. DuckDB zone.tbl filter (lines 123, 416)

 tbl == glue("ply_subregions_2026{v_sfx}")

 4g. DuckDB zone_taxon reference (lines 1244, 1375)

 tbl(con_sdm, glue("zone_taxon{v_sfx}"))

 Also update zone_tbl filter inside zone_taxon queries (line 1246):

 zone_tbl == glue("ply_subregions_2026{v_sfx}")

 ---
 Part 5: apps_2026/mapsp/app.R

 5a. Add version variables (after line 31, in # variables ---- section)

 `# version ----
 v_int <- 3
 v_sfx <- paste0("_v", v_int)

 5b. Tilejson URLs — use glue() with v_sfx

 - Line 466: url = glue("https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025{v_sfx}")
 - Line 474: url = glue("https://api.marinesensitivity.org/tilejson?table=public.ply_programareas_2026{v_sfx}")

 5c. Source layer references — use glue() with v_sfx

 - Line 487: source_layer = glue("public.ply_programareas_2026{v_sfx}")
 - Line 495: source_layer = glue("public.ply_ecoregions_2025{v_sfx}")
 - Line 563: source_layer = glue("public.ply_ecoregions_2025{v_sfx}")

 ---
 Leave untouched
 
 ┌─────────────────────────────────────────┬───────────────────────────────────────────────────────────────────┐
 │                  Item                   │                              Reason                               │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ apps_2026/storymap/                     │ Uses v2 planareas/ecoregions — keep as-is                         │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ api/plumber.R                           │ Generic tilejson — table name comes from query param              │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ server/docker-compose.yml               │ pg_tileserv auto-discovers all tables                             │
 ├─────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
 │ DuckDB cell_metric, zone_metric, metric │ Internal computation tables, overwritten each run — not versioned │
 └─────────────────────────────────────────┴───────────────────────────────────────────────────────────────────┘
 ---
 Files Modified
 ┌───────────────────────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │           File            │                                                                                                       Changes                                                                                                       │
 ├───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd │ Add v_int/v_sfx in setup; version output file paths; add version_zone_tbl chunk; add create_pg_v3_tables chunk; update all PostGIS write calls, index calls, gpkg exports, and zone_taxon DuckDB write to use v_sfx │
 ├───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
 │ apps_2026/mapgl/app.R     │ Add v_int/v_sfx variables; use glue() with v_sfx in gpkg paths, lyrs_csv, metrics_tif, tilejson URLs, source_layer refs, dbListFields, zone.tbl filter, zone_taxon table name                                       │
 ├───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
 │ apps_2026/mapsp/app.R     │ Add v_int/v_sfx variables; use glue() with v_sfx in tilejson URLs and source_layer refs                                                                                                                             │
 └───────────────────────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
 Run Sequence

 1. Edit calc_scores.qmd as described
 2. Re-run from setup_update_pgtable_with_clear through update_cached_downloads
 3. Edit + test mapgl and mapsp apps locally

 Verification

 1. PostGIS: \dt public.ply_* shows both v2 (unversioned) and v3 tables side by side
 2. v2 tables unchanged: SELECT count(*) FROM ply_programareas_2026 WHERE score_extriskspcat_primprod_ecoregionrescaled_equalweights IS NOT NULL → same as before
 3. v3 tables populated: same query on ply_programareas_2026_v3 → returns 20
 4. pg_tileserv auto-discovers v3: https://tile.marinesensitivity.org/public.ply_programareas_2026_v3.json returns valid TileJSON
 5. DuckDB: tbl(con_sdm, "zone") |> distinct(tbl) |> collect() shows _v3 suffixed names
 6. DuckDB: tbl(con_sdm, "zone_taxon_v3") returns data
 7. mapgl app: run locally, confirm program area fills render with v3 metrics
 8. mapsp app: ecoregion and program area outlines render
 9. storymap: still works with v2 tables (untouched)

In @apps_2026/mapgl/app.R and @apps_2026/mapsp/app.R, similarly use v_int and v_sfx to update reading of versioned data.

⏺ This uses the variables pa_gpkg, er_gpkg, lyrs_csv, metrics_tif. The first two are versioned, lyrs_csv is the unversioned layers.csv (an input) and metrics_tif is the unversioned r_metrics.tif (also an input). Those are fine — the outputs lyrs_2026_csv and              
  metrics_2026_tif are versioned via the setup variables.   
                                                                                                                                                                                                                                                                                
  All changes are complete. Here's a summary:               
                                                                                                                                                                                                                                                                                
  Summary of changes                                                                                                                                                                                                                                                            

  workflows/calc_scores.qmd:
  - Added v_int <- 3 and v_sfx <- paste0("_v", v_int) in setup chunk
  - Versioned all output file paths: gpkg, csv, tif files get _v3 suffix
  - Versioned dbWriteTable for zone_taxon → zone_taxon_v3
  - Added version_zone_tbl chunk to rename DuckDB zone.tbl values with _v3
  - Added create_pg_v3_tables chunk to create PostGIS tables from existing v2 tables
  - Updated all PostGIS write calls (update_table_with_clear) to target _v3 tables
  - Updated DuckDB zone filter queries (for pa, er, pra metrics) to use versioned tbl values
  - Updated pg_rm_old_metrics to reference versioned table names
  - Updated add_index_for_vector_tiles to reference versioned tables
  - Updated update_cached_downloads st_read calls to read from versioned PostGIS tables

  apps_2026/mapgl/app.R:
  - Added v_int/v_sfx variables
  - Versioned gpkg, lyrs_csv, metrics_tif paths
  - Versioned tilejson URLs and source_layer references (er, pra)
  - Versioned dbListFields call
  - Versioned DuckDB zone.tbl filters (ply_subregions_2026 → with v_sfx)
  - Versioned zone_taxon table references (subregion and programarea queries)

  apps_2026/mapsp/app.R:
  - Added v_int/v_sfx variables
  - Versioned tilejson URLs and all source_layer references (er, pra, including add_er_fill_layer helper)

What do I need to run next to update all outputs to the latest versions? 

⏺ Based on the changes, here's the run sequence:

  1. calc_scores.qmd — run these chunks in order

  From RStudio or Quarto, run the following labeled chunks sequentially:

  1. setup — loads packages, sets v_int=3/v_sfx="_v3", connects to DuckDB
  2. setup_update_pgtable_with_clear — defines update_table_with_clear() helper, sources libs/db.R (connects to PostGIS), and runs the version_zone_tbl
  loop to rename DuckDB zone.tbl values
  3. create_pg_v3_tables — creates ply_planareas_2025_v3, ply_ecoregions_2025_v3, ply_programareas_2026_v3 in PostGIS as copies of v2
  4. pg_update_planarea_ecoregion_metrics_2025 — writes pa/er metrics to _v3 PostGIS tables
  5. pg_update_programareas_metrics_2026 — writes pra metrics to _v3 PostGIS table
  6. pg_rm_old_metrics — drops stale metric columns from _v3 tables
  7. add_index_for_vector_tiles — adds primary key indexes for pg_tileserv on _v3 tables
  8. update_cached_downloads — exports _v3 PostGIS tables to versioned gpkg/csv/tif files

  2. Verify

  `# PostGIS: v3 tables exist and have data
  dbGetQuery(con, "SELECT count(*) FROM ply_programareas_2026_v3 WHERE score_extriskspcat_primprod_ecoregionrescaled_equalweights IS NOT NULL")

  `# v2 untouched
  dbGetQuery(con, "SELECT count(*) FROM ply_programareas_2026 WHERE score_extriskspcat_primprod_ecoregionrescaled_equalweights IS NOT NULL")

  `# DuckDB: zone.tbl renamed
  tbl(con_sdm, "zone") |> distinct(tbl) |> collect()

  `# DuckDB: zone_taxon_v3 exists
  tbl(con_sdm, "zone_taxon_v3")

  3. Test apps locally

  `# mapgl
  shiny::runApp(here::here("apps_2026/mapgl"))

  `# mapsp
  shiny::runApp(here::here("apps_2026/mapsp"))

  Confirm program area fills and ecoregion outlines render. The storymap app should still work with v2 tables (untouched).

What is the sequence I should run? I think I already ran everything updated in @workflows/calc_scores.qmd (but you could easily inspect filesystem to confirm) 

⏺ Here's what I see from the filesystem:                                                                                                                
                                                                                                                                                        
  Already completed today (Feb 10)                                                                                                                      
  ┌───────────────────────────────┬───────────┬─────────────────────────┐                                                                               
  │             File              │ Timestamp │      Source chunk       │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ zone_taxon_2026_v3.csv        │ 12:46     │ zone_taxon              │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ ply_programareas_2026_v3.gpkg │ 14:43     │ update_cached_downloads │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ layers_2026_v3.csv            │ 14:43     │ update_cached_downloads │                                                                               
  ├───────────────────────────────┼───────────┼─────────────────────────┤                                                                               
  │ ply_planareas_2025_v3.gpkg    │ 14:45     │ update_cached_downloads │
  ├───────────────────────────────┼───────────┼─────────────────────────┤
  │ ply_ecoregions_2025_v3.gpkg   │ 14:45     │ update_cached_downloads │
  ├───────────────────────────────┼───────────┼─────────────────────────┤
  │ r_metrics_2026_v3.tif         │ 14:45     │ update_cached_downloads │
  ├───────────────────────────────┼───────────┼─────────────────────────┤
  │ sdm_parquet/                  │ 15:08     │ export_parquet          │
  └───────────────────────────────┴───────────┴─────────────────────────┘
  Still missing (not yet regenerated with v3)
  ┌────────────────────────┬────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────┐
  │          File          │                         Status                         │                    Source chunk                     │
  ├────────────────────────┼────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────┤
  │ r_metrics_akl48_v3.tif │ missing — only old r_metrics_akl48.tif (Aug 18) exists │ update_cached_downloads → metrics_akl48_tif section │
  ├────────────────────────┼────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────┤
  │ OCS_...Metrics.geojson │ stale (Aug 7)                                          │ pa_metrics_geo                                      │
  └────────────────────────┴────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────┘
  Remaining sequence to run

  1. update_cached_downloads (line 3032) — re-run just the metrics_akl48_tif section (line 3180). This will now use the fixed code (programarea_key +
  sr_pra_csv + metrics_2026_tif).
  2. r_metrics_akl48_primprod_log (line 3211) — adds primprod_log layer to the new AKL48 raster
  3. pa_metrics_geo (line 3226) — regenerate the GeoJSON (optional, planning areas only)
  4. Archive old downloads — run the archive chunk in render_derived_products.qmd
  5. Copy calc_scores outputs to downloads — run the copy chunk in render_derived_products.qmd
  6. Run msens-summary_programareas.qmd — all chunks, generates PRA maps/tables/flowers/primprod
  7. Verify — spot-check Rice's whale, raster layers, facet count

  The critical next step is #1 — generating r_metrics_akl48_v3.tif — since that's the only v3 derived file missing from calc_scores.qmd. Everything else
   from that notebook is done.



## 2026-02-10.b + ingest_nmfs_core-areas.qmd for ricei

Create a new notebook ingest_nmfs_core-areas.qmd to add a dataset “NMFS Core Areas” (ca_nmfs) in sdm_2026.duckdb. Burn in a new model and model_cell values for Rice’s whale (_Balaenoptera ricei; worms_id:1576133) into the database. Apply the highest weighting of “NMFS:EN” (100). The spatial footprint is found here:

'/Users/bbest/Library/CloudStorage/GoogleDrive-ben@ecoquants.com/My Drive/projects/msens/data/raw/fisheries.noaa.gov/core-areas/shapefile_Rices_whale_core_distribution_area_Jun19_SERO/shapefile_Rices_whale_core_distribution_area_Jun19_SERO.shp'

But this is also the same shape as the IUCN range map (mdl_seq:19014), so you can more easily copy those rows and apply the 100 value.This new dataset needs to be added to the model_merge and calculate scores listing.

⏺ Updated plan                                                                                                     
  ⎿  /plan to preview
                                                                                                                   
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 Ready to code?                                                                                                    
                                                                                                                   
 Here is Claude's plan:                                                                                            
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Plan: Ingest NMFS Core Areas (ca_nmfs) for Rice's Whale                  

 Context

 Rice's whale (Balaenoptera ricei; worms_id: 1576133) has a designated NMFS Core Distribution Area that matches
 its IUCN range map footprint. This core area needs to be ingested as a new dataset (ca_nmfs) in sdm_2026.duckdb
 with maximum weighting (value=100), and integrated into the model merge and scoring pipeline so it contributes to
  sensitivity calculations.

 The IUCN range map already exists at mdl_seq:19014, so we can copy those cell_ids rather than rasterizing the
 shapefile from scratch.

 ---
 Files to Create

 1. ingest_nmfs_core-areas.qmd (new)

 Follow the pattern from ingest_fisheries.noaa.gov_critical-habitat.qmd. Sections:

 Setup chunk:
 librarian::shelf(
   DBI, dplyr, duckdb, glue, here, sf, terra,
   quiet = T)

 ds_key   <- "ca_nmfs"
 is_server <- Sys.info()[["sysname"]] == "Linux"
 dir_data  <- ifelse(is_server, "/share/data", "~/My Drive/projects/msens/data")
 sdm_db    <- glue("{dir_data}/derived/sdm_2026.duckdb")
 cell_tif  <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")

 con_sdm <- dbConnect(duckdb(dbdir = sdm_db, read_only = F))

 Insert dataset row (ds_key = "ca_nmfs"):
 - name_short: "NMFS Core Areas, 2019"
 - source_broad: "NMFS"
 - source_detail: "https://www.fisheries.noaa.gov"
 - response_type: "binary"
 - taxa_groups: "cetaceans"
 - spatial_res_deg: 0.05
 - temporal_res: "static"
 - Reference the shapefile source in link_download

 Delete existing ca_nmfs data (idempotent re-runs):
 - Delete from model_cell, model, species WHERE ds_key = 'ca_nmfs'

 Copy model_cell from IUCN range map (mdl_seq: 19014):
 - Query: SELECT cell_id FROM model_cell WHERE mdl_seq = 19014
 - Insert model row for Rice's whale: taxa = "ca_nmfs:Balaenoptera ricei", mdl_type = "binary"
 - Insert species row: worms_id = 1576133, redlist_code = "EN", sp_cat = "mammal"
 - Insert model_cell rows: same cell_ids, value = 100 (highest weighting)

 Optional visualization: Show the core area on a leaflet map (read the shapefile for visual confirmation, but use
 the IUCN cells for the data).

 ---
 Files to Modify

 2. merge_models.qmd

 Two changes needed:

 Line 1754 — Add "ca_nmfs" to ds_cols so it's included in the taxon_model junction table:
 `# before:
 ds_cols <- c("am_0.05", "ch_nmfs", "ch_fws", "rng_fws", "bl")
 `# after:
 ds_cols <- c("am_0.05", "ca_nmfs", "ch_nmfs", "ch_fws", "rng_fws", "bl")

 Line 3072 — Add "ca_nmfs" to ds_keys_mask so core areas contribute to the IUCN range mask:
 `# before:
 ds_keys_mask <- c("rng_iucn", "ch_fws", "ch_nmfs", "rng_fws", "bl")
 `# after:
 ds_keys_mask <- c("rng_iucn", "ca_nmfs", "ch_fws", "ch_nmfs", "rng_fws", "bl")

 No changes to calc_scores.qmd are needed — it already uses er_score from the taxon table (which is 100 for
 NMFS:EN via ingest_nmfs-fws-listings.qmd) multiplied by the merged model_cell.value. Adding ca_nmfs to the merge
 ensures it flows through to scoring automatically.

 ---
 How It Flows Through the Pipeline

 1. ingest_nmfs_core-areas.qmd → writes dataset, model, species, model_cell rows for ca_nmfs with value=100
 2. merge_models.qmd → picks up ca_nmfs in pivot_wider (creates ca_nmfs column in taxon), adds to taxon_model
 junction, includes in ds_keys_mask for range masking, and takes MAX value (100) when merging with other models
 for Rice's whale
 3. calc_scores.qmd → uses merged model value (boosted by 100 from core area) × er_score (100 for NMFS:EN) =
 maximum sensitivity contribution

 ---
 Verification

 After running the notebook:
 `# verify dataset row
 tbl(con_sdm, "dataset") |> filter(ds_key == "ca_nmfs") |> collect()

 `# verify model row
 tbl(con_sdm, "model") |> filter(ds_key == "ca_nmfs") |> collect()

 `# verify species row
 tbl(con_sdm, "species") |> filter(ds_key == "ca_nmfs") |> collect()

 `# verify model_cell count matches IUCN range
 n_ca   <- tbl(con_sdm, "model_cell") |> filter(mdl_seq == NEW_MDL_SEQ) |> count() |> pull(n)
 n_iucn <- tbl(con_sdm, "model_cell") |> filter(mdl_seq == 19014)       |> count() |> pull(n)
 stopifnot(n_ca == n_iucn)

 `# verify all values are 100
 tbl(con_sdm, "model_cell") |> filter(mdl_seq == NEW_MDL_SEQ) |>
   summarize(v_min = min(value), v_max = max(value)) |> collect()
 `# should show v_min = 100, v_max = 100

⏺ All changes are implemented and verified. Here's a summary:                                                      
                                                                           
  Created                                                                                                          
                                                                           
  - ingest_nmfs_core-areas.qmd — New Quarto notebook that:                                                         
    - Inserts a dataset row for ca_nmfs (NMFS Core Areas, 2019)            
    - Deletes any existing ca_nmfs data for idempotent re-runs
    - Copies cell_ids from IUCN range map mdl_seq = 19014 (Rice's whale) with value = 100
    - Inserts model (binary, Gulf of Mexico), species (worms_id 1576133, EN, mammal), and model_cell rows
    - Includes verification checks: cell count match, all values = 100

  Modified

  - merge_models.qmd line 1754 — Added "ca_nmfs" to ds_cols so it's included in the taxon_model junction table
  - merge_models.qmd line 3091 — Added "ca_nmfs" to ds_keys_mask so core areas contribute to the IUCN range mask

  Both edits place "ca_nmfs" right after "am_0.05" / "rng_iucn" respectively, keeping it grouped with the other
  NMFS datasets.

Use:
- source_detail = "https://www.fisheries.noaa.gov/resource/map/rices-whale-core-distribution-area-map-gis-data",
- link_download = "https://www.fisheries.noaa.gov/s3/2021-09/shapefile_Rices_whale_core_distribution_area_Jun19_SERO.zip" 

⏺ Updated both URLs in ingest_nmfs_core-areas.qmd:                                                                 
  - source_detail now points to the GIS data map page
  - link_download now points to the actual shapefile zip  

## 2026-02-10 merge_models, calc_scores deep think

Think deeply in evaluating next steps to update species with merge_models.qmd and calc_scores.qmd. Do I want 
   to run all chunks in these files or should we split these out a bit to better organize updating steps. We   
  also want to update scores for distributions ingested for NMFS (ie fisheries.noaa.gov) and FWS.gov           
  critical-habitat and range-maps. We applied an old scoring to those distributions of 70L and "TN" ~ 70L,     
  "EN" ~ 90L. Rather than re-running these workflows, it would be better to have a quick fix converting old to 
   new with a query updating the model_cell values in the sdm_2026.duckdb. For now we're conflating extinction 
   risk with distribution cell values in addition to an overall species weight. In future hopefully we'll      
  disentangle suitability..                                                            

⏺ Updated plan
  ⎿  /plan to preview

───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 Ready to code?

 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Update scores: model_cell values, listing join, recalculate metrics

 Context

 After fixing botw_id propagation and converting er_score to integer [0,100], we need to:
 1. Remap old model_cell values (70/90 for ch, 50/70/90 for rng) to the new er_score scale
 2. Run the listing join to populate extrisk_code/er_score on the taxon table (not yet run)
 3. Recalculate downstream scoring metrics

 Rather than re-running merge_models.qmd (49 chunks, 3620 lines) or calc_scores.qmd end-to-end, create a focused
 update_scores.qmd for the SQL updates + targeted recalculation.

 Note: For now, extinction risk stays conflated in both model_cell values AND the per-species er_score weight — a
 deliberate choice to disentangle suitability from risk in a future iteration.

 ---
 Current state (sdm_2026.duckdb)

 - taxon: 17,561 rows (16,947 worms + 614 botw). Has worms_id column. Does NOT yet have extrisk_code or er_score.
 - listing: 718 rows with spp_id, worms_id, botw_id, extrisk_code, er_score (integer [0,100])
 - model_cell values (old → new mapping):
   - ch_nmfs: 70→50 (TN), 90→100 (EN)
   - ch_fws: 70→50 (TN), 90→100 (EN)
   - rng_fws: 50→1 (LC), 70→50 (TN), 90→100 (EN)
 - Merged models (ms_merge): max(value) across datasets per cell — must be recomputed for affected taxa

 ---
 Step 1: Remap ch/rng model_cell values to er_score scale

 Remap old ad-hoc values to match the compute_er_score() scale: EN=100, TN=50, LC=1.

 UPDATE model_cell SET value = CASE value
   WHEN 90 THEN 100   -- EN
   WHEN 70 THEN 50    -- TN
   WHEN 50 THEN 1     -- LC (rng_fws only)
 END
 WHERE mdl_seq IN (
   SELECT mdl_seq FROM model
   WHERE ds_key IN ('ch_nmfs', 'ch_fws', 'rng_fws')
 )

 Step 2: Recompute merged model cells for affected taxa

 For taxa with merged models (n_ds > 1) that include ch/rng data, recompute cell values as max(all source values)
 since the source ch/rng values changed. This avoids re-running the expensive raster merge loop (~169 affected
 species).

 UPDATE model_cell mc SET value = src.max_val
 FROM (
   SELECT t.mdl_seq AS merged_seq, all_src.cell_id,
          MAX(all_src.value) AS max_val
   FROM taxon t
   JOIN taxon_model tm ON t.taxon_id = tm.taxon_id
   JOIN model_cell all_src ON tm.mdl_seq = all_src.mdl_seq
   WHERE t.mdl_seq IS NOT NULL
   AND t.n_ds > 1
   AND t.taxon_id IN (
     SELECT DISTINCT taxon_id FROM taxon_model
     WHERE ds_key IN ('ch_nmfs', 'ch_fws', 'rng_fws')
   )
   GROUP BY t.mdl_seq, all_src.cell_id
 ) AS src
 WHERE mc.mdl_seq = src.merged_seq
 AND mc.cell_id = src.cell_id

 Step 3: Listing join (with botw_id support)

 Adapted from merge_models.qmd chunk taxon_listing_join (line 1894). Key addition: a second UPDATE for BOTW-only
 birds (209 birds in taxon have no worms_id, only botw_id via taxon_id).

 -- add columns if not exist
 ALTER TABLE taxon ADD COLUMN IF NOT EXISTS extrisk_code VARCHAR;
 ALTER TABLE taxon ADD COLUMN IF NOT EXISTS er_score INTEGER;
 ALTER TABLE taxon ADD COLUMN IF NOT EXISTS is_mmpa BOOLEAN DEFAULT FALSE;
 ALTER TABLE taxon ADD COLUMN IF NOT EXISTS is_mbta BOOLEAN DEFAULT FALSE;
 ALTER TABLE taxon ADD COLUMN IF NOT EXISTS is_bcc  BOOLEAN DEFAULT FALSE;

 -- primary: join by worms_id (covers non-birds + birds with worms mapping)
 UPDATE taxon SET
   extrisk_code = l.extrisk_code, er_score = l.er_score,
   is_mmpa = l.is_mmpa, is_mbta = l.is_mbta, is_bcc = l.is_bcc
 FROM listing l
 WHERE taxon.worms_id = l.worms_id;

 -- secondary: join by botw_id for BOTW-only birds not matched above
 UPDATE taxon SET
   extrisk_code = l.extrisk_code, er_score = l.er_score,
   is_mmpa = l.is_mmpa, is_mbta = l.is_mbta, is_bcc = l.is_bcc
 FROM listing l
 WHERE taxon.taxon_authority = 'botw'
   AND taxon.taxon_id = l.botw_id
   AND taxon.extrisk_code IS NULL;

 -- IUCN fallback for unmatched taxa
 UPDATE taxon SET
   extrisk_code = 'IUCN:' || redlist_code,
   er_score     = CASE redlist_code
     WHEN 'CR' THEN 50  WHEN 'EN' THEN 25
     WHEN 'VU' THEN 5   WHEN 'NT' THEN 2  ELSE 1 END
 WHERE extrisk_code IS NULL AND redlist_code IS NOT NULL;

 -- default for taxa with no listing and no redlist_code
 UPDATE taxon SET er_score = 1 WHERE er_score IS NULL;

 Step 4: Re-export taxon summary CSV

 Reproduce merge_models.qmd chunk taxon_summary (line 3501) to write updated data/taxon.csv with new extrisk_code,
  er_score, is_mmpa, is_mbta, is_bcc columns.

 Step 5: Recalculate scoring metrics

 Re-run calc_scores.qmd interactively in Positron starting from chunk delete_extrisk_metrics (line 472). This
 avoids duplicating complex scoring logic (ecoregion normalization, rescaling, zone aggregation). The key chunks
 to run sequentially:

 1. delete_extrisk_metrics — clear old metrics
 2. calc_cell_metric_redlist — recompute cell-level extinction risk
 3. calc_ecoregion_minmax through cell_metric_score — rescale and aggregate
 4. zone_taxon — per-zone species sensitivity with updated er_score

 Also update: merge_models.qmd listing join

 Edit chunk taxon_listing_join (~line 1907) to add the botw_id secondary join, so future full re-runs also handle
 BOTW-only birds correctly.

 ---
 Files to create/modify
 ┌───────────────────┬─────────────────────────────────────────────────────────────────────────────────────┐
 │       File        │                                       Action                                        │
 ├───────────────────┼─────────────────────────────────────────────────────────────────────────────────────┤
 │ update_scores.qmd │ CREATE — focused update notebook (Steps 1-4)                                        │
 ├───────────────────┼─────────────────────────────────────────────────────────────────────────────────────┤
 │ merge_models.qmd  │ EDIT chunk taxon_listing_join (~line 1907) — add botw_id secondary join             │
 ├───────────────────┼─────────────────────────────────────────────────────────────────────────────────────┤
 │ calc_scores.qmd   │ No code changes needed (already updated for integer er_score); re-run interactively │
 └───────────────────┴─────────────────────────────────────────────────────────────────────────────────────┘
 Verification

 1. After Step 1: SELECT ds_key, value, count(*) FROM model_cell mc JOIN model m ON mc.mdl_seq = m.mdl_seq WHERE
 m.ds_key IN ('ch_nmfs','ch_fws','rng_fws') GROUP BY ds_key, value → values are 1, 50, or 100
 2. After Step 3: SELECT taxon_authority, count(*) FROM taxon WHERE extrisk_code IS NOT NULL GROUP BY
 taxon_authority → both worms and botw represented
 3. After Step 3: SELECT count(*) FROM taxon WHERE er_score IS NULL → 0
 4. After Step 5: scoring metrics reflect new values, no errors in calc_scores render

⏺ Everything looks good. Here's a summary of what was done:                                                        
                                                                                                                   
  Changes made                                                                                                     
                                                                                                                   
  1. Created update_scores.qmd (new file)                                                            
                                                                                                                   
  Focused update notebook with 4 steps:                                                                            
  - Step 1 (remap_model_cell_values): Remaps ch/rng model_cell values from old scale (50/70/90) to er_score scale
  (1/50/100), with before/after verification
  - Step 2 (recompute_merged_models): Recomputes merged model cells for ~169 affected taxa with n_ds > 1 that
  include ch/rng data
  - Step 3 (listing_join): Populates extrisk_code/er_score/is_mmpa/is_mbta/is_bcc on the taxon table via:
    - Primary join by worms_id
    - Secondary join by botw_id for BOTW-only birds
    - IUCN fallback for unmatched taxa
    - Default er_score=1 for remaining
  - Step 4 (taxon_summary): Re-exports data/taxon.csv with updated columns

  2. Edited merge_models.qmd (lines ~1919-1940)

  Added the botw_id secondary join to the taxon_listing_join chunk so future full re-runs also handle BOTW-only
  birds correctly. The new code:
  - Joins by botw_id for taxa where taxon_authority = 'botw' and extrisk_code IS NULL
  - Reports counts for both worms_id and botw_id matches

  Next steps

  After running update_scores.qmd, re-run calc_scores.qmd interactively from chunk delete_extrisk_metrics (line
  472) through zone_taxon to recalculate downstream scoring metrics.



## 2026-02-05 run big update


Rename @workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd to 
@workflows/ingest_nmfs-fws-listings.qmd since they both include MMPA and MBTA 
listings. Update any other dependencies, like the targets.

Let's make US-listed species er_score based on max (not additive) score from ESA, MMPA (20), MBTA (10)

Let's use NMFS vs NOAA since NMFS is more specific.

∆ wording:

Species not listed explicitly by NMFS or FWS fall back to IUCN scale in `merge_models.qmd`


⏺ Here's the run order:                                                                                                   
                                                                                                                                                                                                                
  Step-by-step execution                                                                                                                                                                                        
                                                                                                                                                                                                                
  1.  msens package        (already installed ✅)                                                                                                                                                               
  2.  ingest listings      workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd                                                                                                                                     
  3.  merge models         workflows/merge_models.qmd                                                                                                                                                           
  4.  calc scores          workflows/calc_scores.qmd                                                                                                                                                            
  5.  update apps          apps_2026/mapgl/app.R, apps_2026/mapsp/app.R                                                                                                                                         
                                                                                                                                                                                                                
  Run each notebook interactively in RStudio (chunk-by-chunk), since they have eval: false on expensive chunks.

  What each step does

  1. ingest listings — reads NOAA/FWS/MBTA sources, computes extrisk_code + er_score, writes listing staging table to sdm_2026.duckdb
  2. merge models — creates taxon_model junction table, writes slimmed taxon (no dataset columns), LEFT JOINs listing onto taxon, IUCN fallback for unmatched, ingests IUCN range maps, runs merge loop
  3. calc scores — uses er_score directly from taxon (no more case_match on redlist_code), recalculates cell_metric and zone_metric

  Apps that need updating

  Both mapgl apps have the old case_match scoring baked in:
  ┌───────────────────────┬────────────┬────────────────────────────────────────────────────────────────────┐
  │         File          │   Lines    │                           What to change                           │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps_2026/mapgl/app.R │ ~1274-1293 │ Replace rl_code/rl_score case_match with er_score from taxon table │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps_2026/mapsp/app.R │ ~111, 414  │ Update redlist_code display to extrisk_code                        │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps/mapgl/app.R      │ ~832-847   │ Same as above (older version)                                      │
  ├───────────────────────┼────────────┼────────────────────────────────────────────────────────────────────┤
  │ apps/mapsp/app.R      │ ~194       │ Display-only redlist_code reference                                │
  └───────────────────────┴────────────┴────────────────────────────────────────────────────────────────────┘
  The key change in the mapgl apps is replacing the ~15 lines of case_match scoring with a single er_score column read from the database. The zone_taxon table (written by calc_scores.qmd) will also have
  er_score instead of rl_score, so apps reading that CSV/table get the new scores automatically.

  Or use targets

  Alternatively, from workflows/:
  targets::tar_make()
  Though for a first run I'd recommend going chunk-by-chunk to verify each step.

## 2026-02-05 big update

I have a fairly massive overhaul to do as version 3 of this project. The main goals are:

- Flag species addressed by US federal sources (NOAA and FWS) for applying Threatened/Endangered/LeastConcern status, and not allowing international IUCN scores override these:

  * **NOAA**  
    [noaa\_species\_directory.csv](https://github.com/MarineSensitivity/workflows/blob/f2a6a0c6287afcd57ecbd483d77595f65b0d8414/data/noaa_species_directory.csv)  
    @workflows/data/noaa_species_directory.csv
    * See [**Glossary-classifications**](https://www.fisheries.noaa.gov/national/glossary-classifications-protected-species) with definitions  
      eg: ESA Endangered; **MMPA** Protected; MMPA Depleted; CITES Appendix II; SPAW Annex II, …
  
  * **FWS**  
    [FWS\_Species\_Data\_Explorer.csv](https://drive.google.com/open?id=1GNqaCgFleSq-leRsJp7-iq-SxL5YcsH6&usp=drive_fs)
    @'~/My Drive/projects/msens/data/raw/fws.gov/species/FWS_Species_Data_Explorer.csv'
    * Includes BCC: Birds of Conservation Concern list
    
    * **MTBA** (birds)  
      [CFR50-Part10.13-2023.xlsx](https://docs.google.com/spreadsheets/d/1paoAZw_UdGZ1KAgDycv0AiULzeL-I6Jo?rtpof=true&usp=drive_fs)
      @'~/My Drive/projects/msens/data/raw/fws.gov/birds_mbta/CFR50-Part10.13-2023.xlsx'

- Apply a new non-linear scoring system for species also flagged above as part of MMPA, MBTA and BCC (not sure how to weight that one yet) on extinction risk categories from US (favored) and International sources:

  [MST scoring](https://docs.google.com/spreadsheets/u/0/d/1GbSwo83lKptV0BghKwtRV5P-SyilmNn2w37uFpJY2AM/edit)
  
  | US |  |  |  |  |
  | ----- | ----- | :---- | :---- | :---- |
  | **score** | **score, original** | **US code** | **description** | **notes** |
  | 100 | 90 | EN | Endangered |  |
  | 50 | 70 | TN | Threatened |  |
  | 20 |  | MMPA | marine mammal | more legal issues with mammals than birds; incidental take retained |
  | 10 |  | MBTA | seabird | incidental take removed |
  | 1 |  | LC | if listed, but not classifed as EN or TN |  |
  
  | International |  |  |  |  |
  | ----- | :---- | ----: | ----: | ----: |
  | **IUCN code** | **description** | **score, original** | **score, old** | **score** |
  | CR | Critically Endangered | 100 | 100 | 50 |
  | EN | Endangered | 80 | 50 | 25 |
  | VU | Vulnerable | 60 | 10 | 5 |
  | NT | Near Threatened | 40 | 5 | 2 |
  | LC | Least Concern | 20 | 1 | 1 |
  | DD | Data Deficient | 20 | 1 | 1 |

- Address individual species concerns in @dev/2026-02-05_v3_species-concerns.md.

- Answer questions along the way like:

  * What IUCN CR/EN species are not covered by NMFS/FWS listings?

- Update model cells and zone metrics accordingly

The problem is that this framework is a bit messy. I've lately been copying a 
backup copy of the database at @~/My Drive/projects/msens/data/derived/sdm_2026.duckdb, 
jamming data into @workflows/merge_models.qmd, then re-running @workflows/calc_scores.qmd,
and forking another new set of apps in @apps_2026/mapgl and @apps_2026/mapsp. I 
was previously ingesting per dataset like @workflows/ingest_fisheries.noaa.gov_critical-habitat.qmd,
ingest_aquamaps_to_sdm_duckdb.qmd, etc., but I've lost track of the necessary 
sequence and need something like the targets R pacakge to keep track of 
dependencies and sequence. Other fixes probably involve:

- Of the tables in the database (cell, cell_metric, dataset, metric, model, 
  model_cell, species, taxon, zone, zone_cell, zone_metric, zone_taxon), 
  the taxon table has column identifiers for datasets matching that taxon and
  value of the mdl_seq. That should be pivoted longer into taxon_model (or taxon_dataset?), 
  so that new datasets getting added can be inserted without DDL on taxon table.


Use targets, not simple numbered scripts. 
Handle the taxon_model restructuring now.


Love the modifiable format of @workflows/data/species_overrides_v3.csv.

Let's rename v3_ingest_listings.qmd to ingest_nmfs-fws-mmpa-mtba-listings.qmd 
to be more explicit about the content.

In Part D: Modify calc_scores.qmd, instead of replacing old `redlist_code` with 
`score_v3`, let's use `extrisk_code` and preface with authority in values, so
"NMFS:EN", "FWS:TN", "IUCN:CR", "IUCN:LC", etc., Then downstream let's call this
`er_score` instead of `rl_score`.

I like the comparison table of Score Scale Impact. Be sure to include that in the notebook.

Be sure to run @workflows/v3_ingest_listings.qmd before @workflows/merge_models.qmd 
to ensure the new scoring system is applied.

When parsing these new species lists, apply "LC" (vs leaving blank as <NA>) if 
in NOAA or FWS species directory but not "EN/TN". This ensures the US classification
gets applied and not overwritten by IUCN where no US listing exists.

Can you please think deeply about how these notebooks could be better organized and possibly
broken into smaller parts and use reusable functions in the @msens R library.

Be sure to retain verification steps at the end of each modified/created notebook. 
You can use @'~/My Drive/projects/msens/data/derived/sdm_2026-01-29.duckdb' as
the reference database for the prior version to compare against.

⏺ The plan has been updated with all your feedback:

  - Renamed to ingest_nmfs-fws-mmpa-mbta-listings.qmd
  - extrisk_code with authority-prefixed values ("NMFS:EN", "FWS:TN", "IUCN:CR", etc.)
  - er_score replaces rl_score downstream
  - LC for listed-but-not-EN/TN species — US classification always applied
  - Run order: listings runs BEFORE merge_models (writes staging listing table, which merge_models LEFT JOINs)
  - msens R package: new R/db.R, R/listings.R, R/taxa.R with reusable functions
  - Score Scale Impact table included in notebook output
  - Verification against sdm_2026-01-29.duckdb reference at end of each notebook

──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 Ready to code?
                                                                                                                                                                                                              
 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 MarineSensitivity v3 Implementation Plan               

 Overview

 Overhaul species scoring to prioritize US federal listings (ESA, MMPA, MBTA) over IUCN, restructure taxon table by extracting dataset columns into a taxon_model junction table, adopt targets for pipeline
 management, extract reusable functions into the msens R package, and address 13 specific species concerns.

 Key Decisions

 - US score always wins when a US listing exists (Rule A)
 - MMPA/MBTA additive with ESA scores, capped at 100
 - targets R package for pipeline dependency management
 - Restructure now: create taxon_model junction table, remove dataset columns from taxon
 - extrisk_code replaces redlist_code with authority-prefixed values: "NMFS:EN", "FWS:TN", "IUCN:CR", etc.
 - er_score replaces rl_score downstream (numeric 0-1)
 - LC for listed species: any species in NOAA or FWS directory but not EN/TN gets "LC" (not NA)
 - Run order: ingest_nmfs-fws-mmpa-mbta-listings.qmd runs BEFORE merge_models.qmd
 - Reusable functions: extract scoring/listing logic into msens R package

 ---
 Part A: Schema Restructuring — taxon_model Junction Table

 Current State

 The taxon table has 6 dataset-specific columns storing mdl_seq values:
 ┌──────────┬──────────┬───────────────────────────────┐
 │  Column  │ Coverage │          Description          │
 ├──────────┼──────────┼───────────────────────────────┤
 │ am_0.05  │ 96.4%    │ AquaMaps mdl_seq              │
 ├──────────┼──────────┼───────────────────────────────┤
 │ ch_nmfs  │ 0.19%    │ NMFS Critical Habitat mdl_seq │
 ├──────────┼──────────┼───────────────────────────────┤
 │ ch_fws   │ 0.16%    │ FWS Critical Habitat mdl_seq  │
 ├──────────┼──────────┼───────────────────────────────┤
 │ rng_fws  │ 0.58%    │ FWS Range mdl_seq             │
 ├──────────┼──────────┼───────────────────────────────┤
 │ bl       │ 3.25%    │ BirdLife mdl_seq              │
 ├──────────┼──────────┼───────────────────────────────┤
 │ rng_iucn │ ~3%      │ IUCN Range mdl_seq            │
 └──────────┴──────────┴───────────────────────────────┘
 Key insight: These dataset columns are ONLY used during merge_models.qmd creation. All downstream code uses only mdl_seq.

 New table: taxon_model

 CREATE TABLE taxon_model (
   taxon_id  BIGINT  NOT NULL,
   ds_key    VARCHAR NOT NULL,
   mdl_seq   INTEGER NOT NULL,
   PRIMARY KEY (taxon_id, ds_key)
 );

 Updated taxon columns (post-restructuring)

 taxon_id, taxon_authority, sp_cat, scientific_name, common_name,
 worms_id, redlist_code,
 extrisk_code, er_score, is_mmpa, is_mbta, is_bcc,
 n_ds, mdl_seq, is_ok

 Dataset columns (am_0.05, ch_nmfs, etc.) removed — live in taxon_model now.

 ---
 Part B: msens R Package Enhancements

 B1: msens/R/db.R — database connection helpers

 #' Path to SDM DuckDB
 #' @param version version date suffix (default: "2026")
 #' @return character path
 #' @export
 #' @concept db
 sdm_db_path <- function(version = "2026") {
   dir_data <- switch(Sys.info()[["sysname"]],
     "Darwin" = "~/My Drive/projects/msens/data",
     "Linux"  = "/share/data")
   glue::glue("{dir_data}/derived/sdm_{version}.duckdb")
 }

 #' Connect to SDM DuckDB
 #' @param version version date suffix
 #' @param read_only logical
 #' @return DBI connection
 #' @export
 #' @concept db
 sdm_db_con <- function(version = "2026", read_only = FALSE) {
   DBI::dbConnect(duckdb::duckdb(
     dbdir = sdm_db_path(version), read_only = read_only))
 }

 #' Connect to species taxonomy DuckDB
 #' @param read_only logical
 #' @return DBI connection
 #' @export
 #' @concept db
 spp_db_con <- function(read_only = TRUE) {
   dir_data <- switch(Sys.info()[["sysname"]],
     "Darwin" = "~/My Drive/projects/msens/data",
     "Linux"  = "/share/data")
   DBI::dbConnect(duckdb::duckdb(
     dbdir     = glue::glue("{dir_data}/derived/spp.duckdb"),
     read_only = read_only))
 }

 Add DBI, dplyr, duckdb, stringr to DESCRIPTION Imports.

 B2: msens/R/listings.R — listing parsers and score computation

 ```r
 #' Parse NOAA protected_status field
 #' @param status_str character vector of semicolon-delimited statuses
 #' @return tibble with columns: esa_status, is_mmpa
 #' @export
 #' @concept listings
 parse_noaa_status <- function(status_str) {
   tibble::tibble(
     esa_status = dplyr::case_when(
       stringr::str_detect(status_str, "ESA Endangered(?! - Foreign)") ~ "EN",
       stringr::str_detect(status_str, "ESA Threatened(?! - Foreign)")  ~ "TN",
       TRUE ~ "LC"),
     is_mmpa = stringr::str_detect(status_str, "MMPA") |>
       tidyr::replace_na(FALSE))
 }

 #' Compute extinction risk score from extrisk_code and flags
 #'
 #' US-listed: base from ESA + additive MMPA(20)/MBTA(10), cap at 100.
 #' Non-US: IUCN fallback scale (CR=50, EN=25, VU=5, NT=2, other=1).
 #'
 #' @param extrisk_code character, e.g. "NMFS:EN", "FWS:TN", "IUCN:CR"
 #' @param is_mmpa logical
 #' @param is_mbta logical
 #' @return integer score 0-100
 #' @export
 #' @concept listings
 compute_er_score <- function(extrisk_code, is_mmpa = FALSE, is_mbta = FALSE) {
   authority <- stringr::str_split_i(extrisk_code, ":", 1)
   code      <- stringr::str_split_i(extrisk_code, ":", 2)
   is_us     <- authority %in% c("NMFS", "FWS")

   base_us <- dplyr::case_match(code,
     "EN" ~ 100L, "TN" ~ 50L, "LC" ~ 1L, .default = 0L)
   bonus   <- ifelse(is_mmpa, 20L, 0L) + ifelse(is_mbta, 10L, 0L)

   score_iucn <- dplyr::case_match(code,
     "CR" ~ 50L, "EN" ~ 25L, "VU" ~ 5L, "NT" ~ 2L, .default = 1L)

   dplyr::case_when(
     is.na(extrisk_code) ~ 1L,
     is_us               ~ pmin(base_us + bonus, 100L),
     TRUE                ~ score_iucn)
 }
 ```

 B3: msens/R/taxa.R — taxonomy matching cascade

 ```r
 #' Match taxa to spp.duckdb via cascade
 #'
 #' 1. ITIS TSN crosswalk -> worms_id
 #' 2. Exact scientific_name match in worms table
 #' 3. WoRMS REST API for unmatched (via msens::wm_rest())
 #'
 #' @param d data.frame with scientific_name and optionally itis_id
 #' @param con_spp DBI connection to spp.duckdb
 #' @return d with added worms_id, botw_id columns
 #' @export
 #' @concept taxa
 match_taxa <- function(d, con_spp) {
   # implementation follows patterns in merge_models.qmd
 }
 ```

 ---
 Part C: ingest_nmfs-fws-mmpa-mbta-listings.qmd

 Purpose

 Ingest US federal species listings, compute extrisk_code and er_score, write staging listing table to sdm_2026.duckdb. Runs BEFORE merge_models.qmd.

 Key: Matches to spp.duckdb taxonomy (not the sdm taxon table, which doesn't exist yet).

 Section 1: Setup

 librarian::shelf(
   DBI, dplyr, duckdb, glue, here, janitor,
   msens, readr, readxl, stringr, tibble, tidyr, quiet = T)

 dir_data  <- msens::sdm_db_path() |> dirname() |> dirname()
 con_spp   <- msens::spp_db_con()
 con_sdm   <- msens::sdm_db_con(read_only = FALSE)

 Section 2: Ingest NOAA Species Directory (309 spp)

 Source: workflows/data/noaa_species_directory.csv

 Parse protected_status using msens::parse_noaa_status(). Key rule: any species in NOAA directory gets at minimum "LC" (not NA). Match to spp.duckdb via scientific_name → WoRMS synonym lookup → manual
 overrides.

 Section 3: Ingest FWS Species Data Explorer (~4,470 marine spp)

 Source: ~/My Drive/projects/msens/data/raw/fws.gov/species/FWS_Species_Data_Explorer.csv

 Filter to marine taxonomic groups. ESA status: "Endangered" → "EN", "Threatened" → "TN", otherwise → "LC". Match via ITIS TSN crosswalk → scientific name → WoRMS synonym. Subspecies handling (Rule C):
 strip subspecific epithets, take max ESA status.

 Section 4: Ingest MBTA Bird List

 Source: ~/My Drive/projects/msens/data/raw/fws.gov/birds_mbta/CFR50-Part10.13-2023.xlsx

 Match to BirdLife (botw) by scientific name. Set is_mbta = TRUE.

 Section 5: Combine and compute extrisk_code

 # priority: NMFS EN > FWS EN > NMFS TN > FWS TN > NMFS LC > FWS LC > IUCN
 d_listing <- d_noaa |>
   full_join(d_fws, by = "worms_id") |>
   mutate(
     extrisk_code = case_when(
       nmfs_esa == "EN" ~ "NMFS:EN",
       fws_esa  == "EN" ~ "FWS:EN",
       nmfs_esa == "TN" ~ "NMFS:TN",
       fws_esa  == "TN" ~ "FWS:TN",
       nmfs_esa == "LC" ~ "NMFS:LC",
       fws_esa  == "LC" ~ "FWS:LC",
       TRUE             ~ NA_character_),
     er_score = msens::compute_er_score(extrisk_code, is_mmpa, is_mbta) / 100)

 Section 6: Apply Species Overrides

 File: workflows/data/species_overrides_v3.csv
 ┌────────────────────────────┬──────────┬──────────────┬───────────────────────────┬───────────────────────────────┐
 │      scientific_name       │ worms_id │   override   │           value           │             notes             │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Sotalia guianensis         │ 344009   │ is_ok        │ FALSE                     │ outside US EEZ                │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Globicephala melas         │ 137205   │ common_name  │ Long-finned Pilot Whale   │ fix name                      │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Hyperoodon ampullatus      │ 137098   │ common_name  │ Northern Bottlenose Whale │ fix name                      │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Lagenorhynchus albirostris │ 137107   │ common_name  │ White-beaked Dolphin      │ fix name                      │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Odobenus rosmarus          │ 137084   │ er_score     │ 0.01                      │ FWS "not listed"              │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Balaenoptera ricei         │ 1576133  │ extrisk_code │ NMFS:EN                   │ ESA EN + MMPA                 │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Cystophora cristata        │ 137078   │ extrisk_code │ NMFS:LC                   │ MMPA only, not ESA            │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Pagophilus groenlandicus   │ 159019   │ extrisk_code │ NMFS:LC                   │ MMPA only; clip to IUCN range │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Phoca vitulina             │ 137084   │ extrisk_code │ NMFS:LC                   │ MMPA only                     │
 ├────────────────────────────┼──────────┼──────────────┼───────────────────────────┼───────────────────────────────┤
 │ Phocoena phocoena          │ 137117   │ extrisk_code │ NMFS:LC                   │ MMPA only                     │
 └────────────────────────────┴──────────┴──────────────┴───────────────────────────┴───────────────────────────────┘
 Section 7: Write staging listing table

 d_listing_out <- d_listing |>
   select(worms_id, botw_id, extrisk_code, er_score, is_mmpa, is_mbta, is_bcc)
 dbWriteTable(con_sdm, "listing", d_listing_out, overwrite = TRUE)

 Section 8: Score Scale Impact (rendered in notebook)
 ┌─────────────────────────┬──────────────────┬──────────────────┬────────┐
 │      Species type       │   Old rl_score   │   New er_score   │ Change │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ ESA Endangered mammal   │ 0.8              │ 1.0 (100 capped) │ +25%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ ESA Threatened          │ 0.6              │ 0.5              │ -17%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ MMPA-only mammal        │ 0.2 (LC default) │ 0.21 (1+20)/100  │ +5%    │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ MBTA-only bird          │ 0.2 (LC default) │ 0.11 (1+10)/100  │ -45%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ IUCN CR (no US listing) │ 1.0              │ 0.5              │ -50%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ IUCN EN (no US listing) │ 0.8              │ 0.25             │ -69%   │
 ├─────────────────────────┼──────────────────┼──────────────────┼────────┤
 │ No listing at all       │ 0.2              │ 0.01             │ -95%   │
 └─────────────────────────┴──────────────────┴──────────────────┴────────┘
 Section 9: Audit Outputs

 - CSV: IUCN CR/EN species NOT covered by NMFS/FWS listings
 - CSV: all species where US listing overrides IUCN, with rationale
 - Summary table: er_score distribution by sp_cat

 Section 10: Verification vs reference

 con_ref <- DBI::dbConnect(duckdb::duckdb(
   dbdir = msens::sdm_db_path("2026-01-29"), read_only = TRUE))
 # compare old redlist_code distribution vs new extrisk_code
 # spot-check specific species

 ---
 Part D: Modify merge_models.qmd

 D1: Create taxon_model junction table

 After building d_taxon with pivot-wider dataset columns (~line 1414), extract into taxon_model:

 d_taxon_model <- d_taxon |>
   select(taxon_id, any_of(c("am_0.05", "ch_nmfs", "ch_fws", "rng_fws", "bl"))) |>
   pivot_longer(-taxon_id, names_to = "ds_key", values_to = "mdl_seq") |>
   filter(!is.na(mdl_seq))
 dbWriteTable(con_sdm, "taxon_model", d_taxon_model, overwrite = TRUE)

 Later append rng_iucn and ms_merge rows as those models are created.

 D2: LEFT JOIN listing data onto taxon

 After writing initial taxon table (~line 1753), join listing staging table:

 UPDATE taxon SET
   extrisk_code = l.extrisk_code,
   er_score     = l.er_score,
   is_mmpa      = l.is_mmpa,
   is_mbta      = l.is_mbta,
   is_bcc       = l.is_bcc
 FROM listing l
 WHERE taxon.worms_id = l.worms_id OR taxon.taxon_id = l.botw_id

 For taxa NOT matched, apply IUCN fallback:

 UPDATE taxon SET
   extrisk_code = 'IUCN:' || redlist_code,
   er_score     = CASE redlist_code
     WHEN 'CR' THEN 0.50  WHEN 'EN' THEN 0.25
     WHEN 'VU' THEN 0.05  WHEN 'NT' THEN 0.02
     ELSE 0.01 END
 WHERE extrisk_code IS NULL AND redlist_code IS NOT NULL

 D3: Remove per-dataset columns from taxon write

 d_taxon_clean <- d_taxon |>
   select(-am_0.05, -ch_nmfs, -ch_fws, -rng_fws, -bl, -rng_iucn)

 Keep mdl_seq and n_ds for backward compatibility.

 D4: Verification at end of notebook

 Compare taxon row count, taxon_model row count, and extrisk_code distribution against reference sdm_2026-01-29.duckdb.

 ---
 Part E: Modify calc_scores.qmd

 E1: Replace scoring lookup (lines 541-564)

 Before:
 d_rl <- tbl(con_sdm, "taxon") |>
   filter(is_ok, ...) |>
   select(mdl_seq, redlist_code) |>
   collect() |>
   mutate(rl_score = case_match(redlist_code,
     "CR" ~ 1, "EN" ~ 0.8, "VU" ~ 0.6, "TN" ~ 0.6,
     "NT" ~ 0.4, "LC" ~ 0.2, .default = 0.2))

 After:
 d_er <- tbl(con_sdm, "taxon") |>
   filter(is_ok,
     if (!!sp_cat == "all") TRUE else sp_cat == !!sp_cat) |>
   select(mdl_seq, er_score) |>
   collect()

 Rename rl_score → er_score in downstream SQL temp table. The insertion SQL formula stays the same:
 ROUND(SUM(er.er_score * (mc.value/100)) * 100, 2) AS value

 E2: Update zone_taxon section (~line 2005)

 Replace case_match on redlist_code with direct er_score column.

 E3: Verification at end of notebook

 Compare cell_metric and zone_metric distributions against reference database.

 ---
 Part F: targets Pipeline

 workflows/_targets.R

 library(targets)
 tar_option_set(packages = c("DBI", "dplyr", "duckdb", "glue", "here", "msens"))
 tar_source("R/")

 list(
   tar_target(spp_db,      run_ingest_taxon(),    format = "file"),
   tar_target(listing_db,  run_ingest_listings(spp_db), format = "file"),
   tar_target(sdm_aquamaps, run_ingest_aquamaps(), format = "file"),
   tar_target(sdm_birdlife, run_ingest_birdlife(), format = "file"),
   tar_target(sdm_ch_nmfs,  run_ingest_ch_nmfs(),  format = "file"),
   tar_target(sdm_ch_fws,   run_ingest_ch_fws(),   format = "file"),
   tar_target(sdm_rng_fws,  run_ingest_rng_fws(),  format = "file"),
   tar_target(sdm_rng_iucn, run_ingest_rng_iucn(), format = "file"),
   tar_target(rast_prod,    run_ingest_productivity(), format = "file"),
   tar_target(taxon_db, run_merge_models(
     spp_db, listing_db,
     sdm_aquamaps, sdm_birdlife,
     sdm_ch_nmfs, sdm_ch_fws, sdm_rng_fws, sdm_rng_iucn),
     format = "file"),
   tar_target(scores, run_calc_scores(taxon_db, rast_prod), format = "file")
 )

 DAG

 ingest_taxon ─────────────────────────┐
       │                               │
 ingest_listings ──────────────────────┤
                                       │
 ingest_aquamaps ──┐                   │
 ingest_birdlife ──┤                   │
 ingest_ch_nmfs  ──┤                   │
 ingest_ch_fws   ──┼── merge_models ── calc_scores
 ingest_rng_fws  ──┤         │              │
 ingest_rng_iucn ──┘         │              │
                             │       ingest_productivity
                      ingest_taxon +
                      ingest_listings

 workflows/R/run_*.R wrapper functions

 Each wraps quarto::quarto_render() on the corresponding .qmd, returns the db file path for format = "file" tracking.

 ---
 Implementation Sequence

 Phase 1: msens R package (Part B)

 1. Create msens/R/db.R, msens/R/listings.R, msens/R/taxa.R
 2. Update msens/DESCRIPTION (add DBI, dplyr, duckdb, stringr)
 3. devtools::document() and devtools::install()

 Phase 2: Listings notebook (Part C)

 1. Create workflows/data/species_overrides_v3.csv
 2. Create workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd
 3. Run interactively; verify listing table in sdm_2026.duckdb

 Phase 3: Schema + merge_models (Parts A + D)

 1. Modify workflows/merge_models.qmd:
   - Create taxon_model junction table
   - LEFT JOIN listing staging table onto taxon
   - IUCN fallback for unmatched taxa
   - Remove per-dataset columns from taxon write
 2. Run interactively; verify taxon has extrisk_code, er_score

 Phase 4: calc_scores (Part E)

 1. Modify workflows/calc_scores.qmd:
   - Replace rl_score computation with er_score
   - Update zone_taxon section
 2. Run interactively; verify cell_metric values

 Phase 5: targets pipeline (Part F)

 1. Create workflows/_targets.R
 2. Create workflows/R/run_*.R wrappers
 3. tar_make() end-to-end

 ---
 Critical Files
 ┌──────────────────────────────────────────────────┬─────────────────────────────────────────────────────────┐
 │                       File                       │                         Action                          │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/R/db.R                                     │ CREATE — sdm_db_path(), sdm_db_con(), spp_db_con()      │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/R/listings.R                               │ CREATE — parse_noaa_status(), compute_er_score()        │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/R/taxa.R                                   │ CREATE — match_taxa() cascade                           │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ msens/DESCRIPTION                                │ MODIFY — add DBI, dplyr, duckdb, stringr                │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd │ CREATE — ingest listings, compute extrisk_code/er_score │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/data/species_overrides_v3.csv          │ CREATE — manual overrides                               │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/merge_models.qmd                       │ MODIFY — taxon_model, LEFT JOIN listing, slim taxon     │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/calc_scores.qmd                        │ MODIFY — use er_score instead of redlist_code           │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/_targets.R                             │ CREATE — pipeline definition                            │
 ├──────────────────────────────────────────────────┼─────────────────────────────────────────────────────────┤
 │ workflows/R/run_*.R                              │ CREATE — target wrappers                                │
 └──────────────────────────────────────────────────┴─────────────────────────────────────────────────────────┘
 Verification

 Reference database

 Use sdm_2026-01-29.duckdb as baseline:
 - Path: ~/My Drive/projects/msens/data/derived/sdm_2026-01-29.duckdb

 Spot checks
 ┌─────────────────────────────────────┬───────────────────────┬───────────────────┐
 │               Species               │ Expected extrisk_code │ Expected er_score │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Balaenoptera ricei (Rice's whale)   │ NMFS:EN               │ 1.00              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Phocoena phocoena (harbor porpoise) │ NMFS:LC               │ 0.21              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Cystophora cristata (hooded seal)   │ NMFS:LC               │ 0.21              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Odobenus rosmarus (walrus)          │ FWS:LC                │ 0.01 (override)   │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Chelonia mydas (green turtle)       │ NMFS:EN               │ 1.00              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Unlisted IUCN CR species            │ IUCN:CR               │ 0.50              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ Unlisted IUCN LC species            │ IUCN:LC               │ 0.01              │
 ├─────────────────────────────────────┼───────────────────────┼───────────────────┤
 │ MBTA-only bird (no ESA)             │ FWS:LC                │ 0.11              │
 └─────────────────────────────────────┴───────────────────────┴───────────────────┘
 Automated checks

 1. taxon_model row count = sum of non-NA dataset columns in old taxon
 2. All downstream apps work with taxon.mdl_seq
 3. Ecoregion-rescaled cell_metric values span [0,100]
 4. IUCN CR/EN gap analysis: which internationally threatened species lack US coverage
 5. tar_visnetwork() shows correct DAG; tar_make() runs end-to-end
 6. Score Scale Impact table rendered in listings notebook

⏺ Summary of All Changes

  Here's what was implemented across all 5 phases:

  Phase 1: msens R Package (msens/)

  - Created msens/R/db.R — sdm_db_path(), sdm_db_con(), spp_db_con()
  - Created msens/R/listings.R — parse_noaa_status(), compute_er_score()
  - Created msens/R/taxa.R — match_taxa() cascade (ITIS → WoRMS name → WoRMS API)
  - Modified msens/DESCRIPTION — added DBI, dplyr, duckdb, stats, stringr to Imports
  - Package documented and installed; all spot checks pass (e.g., compute_er_score("NMFS:EN", is_mmpa=TRUE) → 100)

  Phase 2: Listings Notebook (workflows/)

  - Created workflows/data/species_overrides_v3.csv — 15 override rows for 10 species
  - Created workflows/ingest_nmfs-fws-mmpa-mbta-listings.qmd — ingests NOAA (309 spp), FWS (~4,470 marine), MBTA bird list; computes extrisk_code/er_score; applies overrides; writes listing staging table;
  includes audit outputs and score impact table

  Phase 3: merge_models.qmd Modifications

  - Added taxon_model junction table creation (pivots dataset columns into rows)
  - Removed per-dataset columns (am_0.05, ch_nmfs, ch_fws, rng_fws, bl) from taxon table write
  - Added listing JOIN chunk: LEFT JOINs listing staging table onto taxon by worms_id, with IUCN fallback for unmatched taxa
  - Updated IUCN range section to write to taxon_model instead of taxon column
  - Updated merge loop to read from taxon_model and append ms_merge rows
  - Updated summary section to use taxon_model instead of removed columns

  Phase 4: calc_scores.qmd Modifications

  - Replaced rl_score computation (case_match on redlist_code) with direct er_score from taxon table
  - Updated SQL: tmp_rl → tmp_er, rl.rl_score → er.er_score
  - Updated zone_taxon section: uses er_score instead of case_match on rl_code

  Phase 5: targets Pipeline

  - Created workflows/_targets.R — 11 targets with correct DAG dependencies
  - Created workflows/R/run_*.R — 8 wrapper functions for each pipeline step
  - Verified: tar_manifest() lists all 11 targets, tar_network() shows 22 correct edges

## 2025-07-13 migrate to msens fxns

Update the @README.md file with a new plan for 


## 2025-06-09.a + pgtileserv_joinfxn, pginsert_zone_tables

Add a header `## Insert Zone Tables for Joining` and R chunk `pginsert_zone_tables` 
after `species_contributions_to_zone_table` to @ingest_aquamaps_to_sdm_duckdb.qmd that simply writes / updates
the `zone_metric` table from the `con_sdm` duckdb into the postgres db `con`, and adds indexes.

Add a header `## Create Vector Tile Join Function` and R chunk `pgtileserv_joinfxn` 
after `pginsert_zone_tables` to @workflows/ingest_aquamaps_to_sdm_duckdb.qmd that 
writes a pg_tileserv function. It should create a function that joins two tables by given field names, 
one of which should have a geometry column, and return the resulting map vector tiles. 
Per the README of
https://github.com/CrunchyData/pg_tileserv...

* Function layers hide the source of data from the server, and allow the HTTP client to send in optional parameters to allow more complex SQL functionality. Any function of the form `function(z integer, x integer, y integer, ...)` that returns an MVT `bytea` result can serve as a function layer...

### Function Layers

By default, `pg_tileserv` will provide access to **only** those functions:

* that have `z integer, x integer, y integer` as the first three parameters;
* that return a `bytea`, and
* that your database connection has `EXECUTE` privileges for.

In addition, hopefully obviously, for the function to actually be **useful** it does actually have to return an MVT inside the `bytea` return.

Functions can also have additional parameters to control the generation of tiles: in fact, the whole reason for function layers is to allow **novel dynamic behaviour**.

#### Function Layer Detail JSON

In the detail JSON, each function declares information relevant to setting up a map interface for the layer. Because functions generate tiles dynamically, the system cannot auto-discover things like extent or center, unfortunately. However, the custom parameters and defaults can be read from the function definition and exposed in the detail JSON.
```json
{
   "name" : "parcels_in_radius",
   "id" : "public.parcels_in_radius",
   "schema" : "public",
   "description" : "Given the click point (click_lon, click_lat) and radius, returns all the parcels in the radius, clipped to the radius circle.",
   "minzoom" : 0,
   "arguments" : [
      {
         "default" : "-123.13",
         "name" : "click_lon",
         "type" : "double precision"
      },
      {
         "default" : "49.25",
         "name" : "click_lat",
         "type" : "double precision"
      },
      {
         "default" : "500.0",
         "type" : "double precision",
         "name" : "radius"
      }
   ],
   "maxzoom" : 22,
   "tileurl" : "http://localhost:7800/public.parcels_in_radius/{z}/{x}/{y}.pbf"
}
```
* `description` can be set using `COMMENT ON FUNCTION` SQL command.
* `id`, `schema` and `name` are the fully qualified name, schema and function name, respectively.
* `minzoom` and `maxzoom` are just the defaults, as set in the configuration file.
* `arguments` is a list of argument names, with the data type and default value.

#### Function Layer Examples

##### Filtering Example

This simple example returns just a filtered subset of a table ([ne_50m_admin_0_countries](https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip) [EPSG:4326](https://epsg.io/4326)). The filter in this case is the first letters of the name. Note that the `name_prefix` parameter includes a **default value**: this is useful for clients (like the preview interface for this server) that read arbitrary function definitions and need a default value to fill into interface fields.
```sql

CREATE OR REPLACE
FUNCTION public.countries_name(
            z integer, x integer, y integer,
            name_prefix text default 'B')
RETURNS bytea
AS $$
DECLARE
    result bytea;
BEGIN
    WITH
    bounds AS (
      SELECT ST_TileEnvelope(z, x, y) AS geom
    ),
    mvtgeom AS (
      SELECT ST_AsMVTGeom(ST_Transform(t.geom, 3857), bounds.geom) AS geom,
        t.name
      FROM ne_50m_admin_0_countries t, bounds
      WHERE ST_Intersects(t.geom, ST_Transform(bounds.geom, 4326))
      AND upper(t.name) LIKE (upper(name_prefix) || '%')
    )
    SELECT ST_AsMVT(mvtgeom, 'default')
    INTO result
    FROM mvtgeom;

    RETURN result;
END;
$$
LANGUAGE 'plpgsql'
STABLE
PARALLEL SAFE;

COMMENT ON FUNCTION public.countries_name IS 'Filters the countries table by the initial letters of the name using the "name_prefix" parameter.';
```
Some notes about this function:

* The `ST_AsMVT()` function uses the function name ("public.countries_name") as the MVT layer name. This is not required, but for clients that self-configure, it allows them to use the function name as the layer source name.
* In the filter portion of the query (in the `WHERE` clause) the bounds are transformed to the spatial reference of the table data (4326) so that the spatial index on the table geometry can be used.
* In the `ST_AsMVTGeom()` portion of the query, the table geometry is transformed into web mercator ([3857](https://epsg.io/3857)) to match the bounds, and the _de facto_ expectation that MVT tiles are delivered in web mercator projection.
* The `LIMIT` is hard-coded in this example. If you want a user-defined limit you need to add another parameter to your function definition.
* The function "[volatility](https://www.postgresql.org/docs/current/xfunc-volatility.html)" is declared as `STABLE` because within one transaction context, multiple runs with the same inputs will return the same outputs. It is not marked as `IMMUTABLE` because changes in the base table can change the outputs over time, even for the same inputs.
* The function is declared as `PARALLEL SAFE` because it doesn't depend on any global state that might get confused by running multiple copies of the function at once.
* The "name" in the [ST_AsMVT()](https://postgis.net/docs/ST_AsMVT.html) function has been set to "default". That means that the rendering client will be expected to have a rendering rule for a layer with a name of "default". In MapLibre, the tile layer name is set in the `source-layer` attribute of a layer.
* The `ST_TileEnvelope()` function used here is a utility function available in PostGIS 3.0 and higher.

Whoops the zone_metric uses a metric_seq identifier which needs to be looked up in the zone table before injecting into the pg db. Update both chunks accordingly     

### Response

⏺ Done! I've added the two sections as requested:

  1. Insert Zone Tables for Joining (pginsert_zone_tables): This chunk transfers the zone_metric table from DuckDB to PostgreSQL and adds performance indexes.
  2. Create Vector Tile Join Function (pgtileserv_joinfxn): This chunk creates two PostgreSQL functions:
    - join_zone_metrics: A flexible function that joins geometry tables with metrics data
    - planarea_score_tiles: A convenience wrapper for planning area scores

  The functions follow pg_tileserv requirements with the standard z, x, y parameters and return MVT bytea format.
  
⏺ Done! I've updated both chunks:

  1. pginsert_zone_tables: Now properly transfers all three tables (zone, metric, and zone_metric) from DuckDB to PostgreSQL with proper foreign key relationships and
  indexes.
  2. pgtileserv_joinfxn: Updated the vector tile function to use the proper join structure between the geometry table, zone table, and zone_metric table. Also added an
  ecoregion tiles function for convenience.

  The key changes:
  - Added zone and metric table creation and population
  - Fixed the join logic to properly connect geometry → zone → zone_metric → metric
  - Renamed the main function to zone_metrics_tiles for clarity
  - Added proper foreign key constraints
  - Added an ecoregion tiles function alongside the planning area tiles function

## 2025-06-03.b msens summary for nat'l program

[BOEM Marine Environmental Sensitivity | Claude Project](https://claude.ai/project/01973657-e3c3-709c-a90a-a781a615e140)

Write a summary of the current project's methods and status, similar to the old 
"ch 8 Env - 2024-2029_NationalOCSProgram_PFP_Sept_2023.pdf", using the rest of 
the Project documents.  Write this as a Quarto document with chunks of R code to 
extract statistics and generate tables and figures. Use mermaid diagrams where 
applicable to explain and cross-references with figures and tables. Include a 
bibliography.

Other Project documents:

- Marine-Sensitivity_docs.pdf (882 lines)
- process.mmd (49 lines)
- flowchart.mmd (9 lines)
- architecture.mmd (20 lines)
- ingest_aquamaps_to_sdm_duckdb.qmd (2,154 lines)

https://claude.ai/share/6d042ddd-beaa-4c56-b8e1-fa88e88cdb85

## 2025-06-03.a species contributions to cell score

In @ingest_aquamaps_to_sdm_duckdb.qmd, fill out the R chunks:
- `calc_planarea_score`: Calculate the zone_metric of 
  metric_key == "score_extriskspcat_primprod_ecoregionrescaled_equalweights"
  for each planning area
- `species_contributions_to_cell_table`: Based on how the metric 
  "score_extriskspcat_primprod_ecoregionrescaled_equalweights" is calculated
  per cell, report on the percent each species contributes to the cell score.
  Include the `redlist_code` and `rl_score` in the output table.
- `species_contributions_to_zone_table`: Based on how the metric 
  "score_extriskspcat_primprod_ecoregionrescaled_equalweights" is calculated
  per zone, report on the percent each species contributes to the zone score.
  Include the `redlist_code` and `rl_score` in the output table.

### Response

Update Todos
  ⎿ ☒ Implement calc_planarea_score chunk to calculate zone_metric for planning areas
     ☒ Implement species_contributions_to_cell_table chunk showing species contributions to cell score
     ☒ Implement species_contributions_to_zone_table chunk showing species contributions to zone score

⏺ I've successfully filled out all three R chunks in the ingest_aquamaps_to_sdm_duckdb.qmd file:

  1. calc_planarea_score: Calculates the zone_metric for planning areas by taking a weighted average of cell scores
  based on cell coverage percentages within each planning area.
  
  2. species_contributions_to_cell_table: Shows how each species contributes to a cell's score based on their
  suitability values and red list scores. It breaks down contributions by species category and shows the top 20
  contributing species.
  
  3. species_contributions_to_zone_table: Similar to the cell table but aggregated across all cells in a planning
  area, weighted by cell coverage. It provides a comprehensive view of species contributions to the zone score
  including summary statistics.

  All chunks follow the existing code patterns and properly handle the complex metric calculation that combines
  extinction risk scores (by species category) with primary productivity, both rescaled by ecoregion.




## 2025-05-27.c more tweaks to ingest_aquamaps_to_sdm_duckdb.qmd

Looking good!Here's a few more tweaks:

- Drop table `env_layers` since these can simply be added as columns to the `cell` table later.
- Drop `years_obs` since already captured by `date_obs_beg` and `date_obs_end` without having to use an ARRAY.
- Simplify `dataset.regions` to being text only, not an array.
- Add `cell.area_km2` table to store the area of each cell in square kilometers, calculated from the raster resolution.
- Drop `cell.res_deg` since uniform in value.
- Add `region_cell` table to quickly relate `cell` to `region` with columns:
  - `rgn_seq`: foreign key to `region` table
  - `cell_id`: foreign key to `cell` table
  - `pct_covered`: as integer percentage of the cell covered by the region polygon

### Response

⏺ I've completed all the schema updates you requested. Here's a summary of the changes made:

  Database Schema Changes:
  - ✅ Dropped env_layers table from ERD and CREATE statements
  - ✅ Removed years_obs array column from dataset table
  - ✅ Simplified dataset.regions from VARCHAR[] to VARCHAR (text only)
  - ✅ Added cell.area_km2 column with terra::cellSize() calculation
  - ✅ Removed cell.res_deg column
  - ✅ Added region_cell table with cell_id, region_id, and pct_covered

  Updated Files:
  - ingest_aquamaps_to_sdm_duckdb.qmd: Updated Mermaid ERD, CREATE TABLE statements, and grid creation code
  - libs/sdm_functions.R: Updated biodiversity functions and indexing for new schema

  The database now has a cleaner, more efficient structure with 9 core tables. The region_cell table will enable fast spatial queries, and
  the area_km2 column supports area-weighted biodiversity calculations. All spatial relationships are pre-calculated for optimal query
  performance.


## 2025-05-27.b tweak ingest_aquamaps_to_sdm_duckdb.qmd, libs/sdm_functions.R

Update the @ingest_aquamaps_to_sdm_duckdb.qmd and @libs/sdm_functions.R to 
include the following changes:
- Strip "sdm_"" from table names, and make singular: 
  - `sdm_sources`    → `dataset`
  - `sdm_models`     → `model`
  - `sdm_species`    → `species`
  - `sdm_cells`      → `cell`
  - `sdm_values`     → `model_cell`
  - `biodiv_metrics` → `metric`
- Rename `model.mld_id` to `mdl_seq` and make it a sequential integer
  starting at 1 for each model output, so that it can be used as a unique identifier
  for the model output.
- Make table `model_cell` (formerly `sdm_values` ) unique to `cell_id` and 
  `model.mdl_seq` (not `sp_id`; so that  multiple species can be associated with a 
  single model output).
- Move `model_cell.value_type` to `model.mdl_type`: one of 
  "occurrence", "range", "suitability", "biomass", "density", "diversity"
- Force the `cell_id` equal to `terra::cells(r)` for the global 0.05 degree
  grid, such that new calculations can summarized to `cell_id`
  and raster generated using an efficient substitution, ie `terra::subst()`
  like `get_species_raster()` function in @libs/sdm_functions.R. Include a similar
  function in @libs/sdm_functions.R to generate a raster given a dataframe
  with default `cell_id` column and at least one non-null column to be
  used as the output name of one or more raster layers.
- Rename `dataset.src_key` to `dataset.ds_key`.
- Add to table `dataset` columns (and drop `url`):
  - `source_broad`: eg NOAA, AquaMaps, Duke, NCCOS, etc.
  - `source_detail`: eg NOAA Southeast Fisheries Science Center, or Duke Marine Geospatial Ecology Lab, etc.
  - `regions`: broad regions, such as Global, Atlantic, Pacific, Gulf of America, Alaska, Hawaii, etc.
  - `taxa_groups`: one or more of: fish, invertebrates, marine mammals, cetaceans, sea turtles, seabirds, etc.
  - `year_pub`: year published
  - `date_obs_beg`: date of first observation, if available
  - `date_obs_end`: date of last observation, if available
  - `date_env_beg`: date of first environmental data, if available
  - `date_env_end`: date of last environmental data, if available
  - `name_short`: short name in form of "{source_broad} {taxa_groups} {response_type} in {regions}, {year_pub}"
  - `response_type`: one or more of: "occurrence", "range", "suitability", "biomass", "density", "diversity"
  - `name_original:` original name of the dataset, such as "Cetacean and sea turtle spatial density model outputs from visual observations using line-transect survey methods aboard NOAA vessel and aircraft platforms in the Gulf of Mexico from 2003-06-12 to 2019-07-31 (NCEI Accession 0256800)"
  - `link_info`: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:256800
  - `link_download`: https://www.ncei.noaa.gov/archive/accession/download/256800
  - `link_metadata`: https://www.fisheries.noaa.gov/inport/item/67830
  - `links_other`: any other relevant links seperated by spaces, such as "https://www.ncei.noaa.gov/archive/accession/GOMMAPPS https://www.boem.gov/gommapps"
- Update the `dataset.ds_key` of "aquamaps_bio-oracle" to "am_0.05" for brevity.
- Rename `model.taxa_group` to `taxa`, which can have one or more related rows in `species`.
- Relate `model` to `species` via `ds_key` and `taxa`, so the same combination 
  can be used across models of varying time_period, and region. In the case of 
  AquaMaps the `sp_key` and `taxa` will be equal and one-to-one, since only a single
  species applies to each model.
- Add columns `gbif_id`, `itis_id` in `species` to look up species from GBIF and
  ITIS databases, respectively.
- Add column `sp_cat` to `species` to indicate the category of the species
  (e.g. "fish", "mollusk", "crustacean", etc.) for easier filtering and analysis.
- Reconfigure `metric` (formerly `biodiv_metrics`) to describe metrics that can
  be applied across cells (+ `cell` → `cell_metric`) and/or regions
  (+ `region` → `region_metric`) with columns:
  - `metric_seq`: primary key of auto-incrementing integer sequence
  - `description`: description of metric
  - `metric_type`: type of metric (e.g. "species_richness", "shannon_index", etc.)
  - `date_created`: date the metric was created
- Add a new table `region` to uniquely identify geometries from polygon tables, including 
  `planning_areas`, for relating to a `metric`:
  - `rgn_seq`: primary key of auto-incrementing integer sequence uniquely identifying a geometry
  - `tbl_col`: "{table}.{column}" containing the polygon geometry, such as "planning_areas.pa_key"
  - `value`: value containing the unique identifier for the polygon, such as "CHU" the `pa_key` value for the "Chukchi Sea" in Alaska
  - `col_geom`: column with the geometry of the regional polygon, such as "geom"
  - `date_created`: date record added to the database
- Rename `planning_area_metrics` table to `region_metric` to relate metrics to regions with columns:
  - `rgn_seq`: foreign key to `region` table
  - `metric_seq`: foreign key to `metric` table
  - `value`: calculated value of the metric for the region
- Add `cell_metric` table to relate metrics to regions with columns:
  - `cell_id`: foreign key to `cell` table
  - `metric_seq`: foreign key to `metric` table
  - `value`: calculated value of the metric for the region
- Add a `date_created` column to table `model` to indicate when the model was created in the database.
- Update the diagram, rest of the notebook and functions in @libs/sdm_functions.R
  according to these new names and instructions.

### Response

⏺ I've successfully updated both files according to your specifications. Here's a summary of the key changes:

  Updated Database Schema:

  - Renamed all tables to remove "sdm_" prefix and make them singular
  - model.mdl_id → model.mdl_seq (auto-incrementing sequence)
  - model_cell is now unique to cell_id and mdl_seq (not species)
  - Moved value_type from model_cell to model.mdl_type
  - Added comprehensive columns to dataset table including links, dates, and metadata
  - Added gbif_id, itis_id, and sp_cat to species table
  - Created new metric, cell_metric, region, and region_metric tables
  - Dataset key changed from "aquamaps_bio-oracle" to "am_0.05"

  Key Implementation Details:

  1. Cell IDs: Now use terra::cells() to ensure consistent cell numbering with the global 0.05° raster template
  2. New function: create_raster_from_df() that uses terra::subst() for efficient raster creation
  3. Model-Species relationship: For AquaMaps, taxa equals sp_key (one-to-one), but the schema supports multiple species per model for other
   datasets
  4. Regional aggregation: The region table allows flexible polygon references from any table/column combination
  5. Metrics: Separated metric definitions from their calculated values, allowing reuse across cells and regions

  Updated Functions in sdm_functions.R:

  - create_raster_from_df() - New function using terra::subst() for efficient raster generation
  - calculate_biodiversity_metrics() - Updated to use new schema with metric definitions
  - export_metric_raster() - Uses new create_raster_from_df() function
  - create_spatial_indexes() - Updated with new table/column names
  - validate_sdm_data() - Updated to check new table structure

  The system now supports efficient querying and raster generation while maintaining flexibility for multiple SDM sources and biodiversity
  metrics.

⏺ I've successfully updated both files according to your specifications. Here's a summary of the key changes:

  Updated Database Schema:

  - Renamed all tables to remove "sdm_" prefix and make them singular
  - model.mdl_id → model.mdl_seq (auto-incrementing sequence)
  - model_cell is now unique to cell_id and mdl_seq (not species)
  - Moved value_type from model_cell to model.mdl_type
  - Added comprehensive columns to dataset table including links, dates, and metadata
  - Added gbif_id, itis_id, and sp_cat to species table
  - Created new metric, cell_metric, region, and region_metric tables
  - Dataset key changed from "aquamaps_bio-oracle" to "am_0.05"

  Key Implementation Details:

  1. Cell IDs: Now use terra::cells() to ensure consistent cell numbering with the global 0.05° raster template
  2. New function: create_raster_from_df() that uses terra::subst() for efficient raster creation
  3. Model-Species relationship: For AquaMaps, taxa equals sp_key (one-to-one), but the schema supports multiple species per model for other
   datasets
  4. Regional aggregation: The region table allows flexible polygon references from any table/column combination
  5. Metrics: Separated metric definitions from their calculated values, allowing reuse across cells and regions

  Updated Functions in sdm_functions.R:

  - create_raster_from_df() - New function using terra::subst() for efficient raster generation
  - calculate_biodiversity_metrics() - Updated to use new schema with metric definitions
  - export_metric_raster() - Uses new create_raster_from_df() function
  - create_spatial_indexes() - Updated with new table/column names
  - validate_sdm_data() - Updated to check new table structure

  The system now supports efficient querying and raster generation while maintaining flexibility for multiple SDM sources and biodiversity
  metrics.


## 2025-05-27.a + ingest_aquamaps_to_sdm_duckdb.qmd with libs/sdm_functions.R

Write a new Quarto notebook `ingest_aquamaps_to_sdm_duckdb.qmd` to ingest the downscaled 
AquaMaps (using the 0.05 decimal degree cells from Bio-Oracle being generated now by
"iterate_species_in_planareas" R chunk in @ingest_aquamaps_res05.qmd) into a new 
DuckDB database `msens_sdm` one model output at a time, for then efficiently querying 
and generating biodiversity metrics per cell (i.e. pixel) 
in the database and outputting to a raster for display in a map (using mapgl; 
see mapgl_rast R chunk in @ingest_aquamaps_res05.qmd). We also want to summarize 
by planning area polygons (`ply_pa` in R chunk "planareas_2025" of 
@ingest_aquamaps_res05.qmd). Include a database diagram in mermaid 2format like 
the "erDiagram" in @create_sdm-tables.qmd, except allow multiple species for a 
single model output, and add functions so any species distribution model (raster or vector) will 
get transformed into the same 0.05 degree resolution as the transformed AquaMaps 
data (see also @libs/am_functions.R, @ingest_sdm-gm.qmd, @ingest_sdm-nc.qmd).
Allow inclusion of other biodiversity metrics such as primary productivity, per
@ingest_productivity.qmd. Write duckdb to parquet files, store in most efficient order and add indexes as 
needed. Write helper functions in a new `libs/sdm_functions.R` file with
Roxygen2 documentation (for adding into an R package later).

### Response

⏺ I've created two files:

  1. ingest_aquamaps_to_sdm_duckdb.qmd - A comprehensive Quarto notebook that:
    - Sets up a DuckDB database with an extended schema supporting multiple species per model
    - Creates spatial tables with proper geometry support
    - Includes functions to ingest downscaled AquaMaps data at 0.05° resolution
    - Calculates biodiversity metrics (species richness, Shannon index, etc.)
    - Generates rasters for map display using the existing mapgl setup
    - Summarizes metrics by planning areas
    - Exports data to Parquet files for efficient storage
  2. libs/sdm_functions.R - Helper functions with Roxygen2 documentation including:
    - batch_insert_values() - Efficient batch insertion into DuckDB
    - create_raster_from_points() - Convert point data to rasters
    - transform_sdm_to_resolution() - Standardize SDM rasters to 0.05° resolution
    - calculate_biodiversity_metrics() - Calculate various biodiversity indices
    - summarize_by_planning_areas() - Aggregate metrics by planning polygons
    - export_metric_raster() - Export metrics as COG rasters
    - create_spatial_indexes() - Set up database indexes
    - validate_sdm_data() - Check data integrity

  Key features:
  - The database schema includes a Mermaid ERD showing relationships
  - Supports multiple SDM sources (AquaMaps, NOAA GoMex, NCCOS, etc.)
  - Efficient spatial queries using DuckDB's spatial extension
  - Integration with existing Bio-Oracle environmental layers
  - Compatible with the mapgl visualization from ingest_aquamaps_res05.qmd
  - Parquet export for archival and sharing

  The code follows the style guidelines with proper indentation, snake_case naming, and lowercase comments. The functions are documented
  with Roxygen2 for future R package integration.

## 2025-05-20.b + Bio-Oracle replication

Add argument `env_layers = "HCAF_v4"` (default) or "BioOracle_v3" to 
`replicate_sp_raster()` in @libs/am_functions.R to use the 
1/20 decimal degree Bio-Oracle data layers versus the default AquaMaps Half-Degree Cells Authority 
File (HCAF). Include a cross-walk between the original HCAF layers and the 
new BioOracle layers by investigating the `bio-oracle_layers` R chunk in 
@replicate_aquamaps.qmd, @data/aquamaps_meta_layers.csv, and 
@data/bio-oracle_datasets_relevant.csv.

Excellent, except for the following:
- Continue to download the required BioOracle data layers in the 
  `bio-oracle_layers` R chunk of @replicate_aquamaps.qmd.
- Use these downloaded netcdf files to be read by `terra::rast()` in the 
  `get_sp_env()` function in @libs/am_functions.R with the additional argument 
  of using HCAF_v4 or BioOracle_v3.
- Apply similar logic to the rest of the functions in @libs/am_functions.R and
  their usage in @replicate_aquamaps.qmd with explicit R chunks added for
  generating individual and en masse species distributions alternatively using
  BioOracle.

### Response

⏺ Update Todos
  ⎿  ☒ Analyze existing replicate_sp_raster function in am_functions.R
     ☒ Identify BioOracle v3 equivalent layers for AquaMaps environmental variables
     ☒ Modify replicate_sp_raster function to accept env_layers parameter
     ☒ Create BioOracle to AquaMaps layer crosswalk mapping
     ☒ Add documentation notes about layer sources in replicate_aquamaps.qmd

⏺ I've implemented the requested changes:

  1. Modified the replicate_sp_raster() function in am_functions.R to accept an optional env_layers parameter with values
  "HCAF_v4" (default) or "BioOracle_v3"
  2. Created a mapping between AquaMaps environmental variables and their BioOracle v3 equivalents:
    - depth → terrain_characteristics.bathymetry_min/mean/max
    - temp (surface) → thetao_baseline_*_depthsurf
    - temp (bottom) → thetao_baseline_*_depthmax
    - salinity → so_baseline layers
    - oxygen → o2_baseline layers
    - primary production → phyc_baseline
    - ice concentration → siconc_baseline
  3. Added proper caching with a suffix for BioOracle results
  4. Documented the changes in the replicate_aquamaps.qmd file with a detailed crosswalk table and usage examples

  The new feature allows for reproducing AquaMaps species distributions with higher resolution (1/20 degree) environmental data
  instead of the default half-degree resolution.

⏺ Update Todos
  ⎿  ☒ Analyze existing replicate_sp_raster function in am_functions.R
     ☒ Identify BioOracle v3 equivalent layers for AquaMaps environmental variables
     ☒ Modify replicate_sp_raster function to accept env_layers parameter
     ☒ Create BioOracle to AquaMaps layer crosswalk mapping
     ☒ Update get_sp_env function to support BioOracle data sources
     ☒ Add BioOracle layer download and loading in bio-oracle_layers chunk
     ☒ Add documentation notes about layer sources in replicate_aquamaps.qmd
     ☒ Create test chunks for BioOracle-based species distributions
     ☒ Update conclusion section to mention BioOracle integration

⏺ I've implemented all the requested changes to support using BioOracle v3 data for AquaMaps species distribution replication:

  1. Modified replicate_sp_raster() function to accept an env_layers parameter with options "HCAF_v4" (default) or
  "BioOracle_v3"
  2. Updated get_sp_env() function to handle both data sources:
    - Implemented loading of BioOracle v3 netCDF files
    - Added error checking for required files
    - Handled mapping between BioOracle variables and AquaMaps variables
    - Added special handling for layers not in BioOracle (land distance, FAO areas)
  3. Enhanced the BioOracle download chunk in replicate_aquamaps.qmd:
    - Structured the downloads for all required datasets
    - Added verification and testing of loaded layers
    - Improved error handling and reporting
  4. Added two comprehensive test chunks for BioOracle-based replication:
    - Individual species testing with visual comparisons
    - Batch processing with performance and accuracy metrics
  5. Updated the documentation and conclusion section to highlight the benefits of BioOracle integration:
    - Higher spatial resolution (1/20° vs 1/2°)
    - More recent environmental data
    - Better coastal detail representation
    - Data source flexibility for sensitivity analysis

  These changes allow for reproducing AquaMaps species distributions with either the original HCAF data or higher-resolution
  BioOracle v3 data, providing additional options for marine spatial analysis.

## 2025-05-20.a mv fxns in replicate_aquamaps.qmd into libs/am_functions.R

Move all functions from @replicate_aquamaps.qmd into a new file `libs/am_functions.R`.
Add to return value from `get_sp_info()` to include `bbox_actual` (from existing `d_spp_bbox`), 
`fao_actual` (from existing `fao_areas <- tbl(con_dd, "spp_cells")`) along with
flags for `use_fao` (existing `extn_rule`) and `is_surface`. Use this species 
information in `replicate_sp_raster()` and `get_sp_env()`. Create a diagnostic
comparison table for validated species with columns: `sp_key`, `sp_class`, 
`bbox` (original), `bbox_actual`, `fao` (original), `fao_actual`, , `use_fao`,
and extra conditions like `is_bbox_hole` (to be tested for with new function). 
Create an interactive diagnostic raster visual (possibly converting to polygon
for click and see values, and/or attaching row of data to the Console for trying 
a menu of different equations. Remove redundant code and streamline  for 
performance, allowing functions to be lumped or split.

Rearrange @libs/am_functions.R so functions are alphabetically sorted

Update functions that output messages in @libs/am_functions.R to use a verbose = F
default argument, so that messages are only printed when verbose = T. Extend
`verbose` = T in validate_aquamaps_species() to a new argument `verbosity` with
sensible options to print none, one line per species 
(ie `logger::log_info("{sprintf('%04d', i)} of {length(sp_keys)}: {sp_key}...\n")`), 
or all the verbose details of each function called.

### Response

⏺ Update Todos
  ⎿ ☒ Create libs/am_functions.R file with extracted functions from replicate_aquamaps.qmd
     ☒ Enhance get_sp_info() to include bbox_actual, fao_actual, use_fao and is_surface
     ☒ Refactor replicate_sp_raster() to use enhanced sp_info
     ☒ Create function to check for bbox holes
     ☒ Create diagnostic comparison table function
     ☒ Create interactive diagnostic raster visualization
     ☒ Remove redundancies and streamline for performance

⏺ I've successfully completed all the tasks:

  1. Created libs/am_functions.R with extracted functions from the original Quarto document
  2. Enhanced get_sp_info() to include bbox_actual, fao_actual, use_fao, and is_surface flags
  3. Added is_bbox_hole() function to detect problematic bounding boxes
  4. Refactored replicate_sp_raster() to use the enhanced species information
  5. Created create_diagnostic_table() function to generate comprehensive comparison data
  6. Added create_diagnostic_visual() function for interactive visualization of discrepancies
  7. Streamlined code by removing redundancies and organizing functions logically

  The new structure provides several benefits:

  1. Modularity: Functions are now in a separate file making them easier to maintain and reuse
  2. Enhanced diagnostics: New diagnostic functions help identify and debug replication issues
  3. Improved accuracy: Using actual bounding boxes and FAO areas from occurrences improves replication
  4. Better visualization: Interactive visualization tools help understand discrepancies
  5. Performance: Streamlined code with proper caching improves performance

  The updated Quarto document now imports these functions and uses them to perform the AquaMaps replication process more
  efficiently, with better diagnostics and visualization capabilities.

The functions in am_functions.R have been successfully rearranged in alphabetical order:

  1. compare_rasters
  2. compare_sp
  3. create_diagnostic_table
  4. create_diagnostic_visual
  5. get_cache_path
  6. get_hcaf_raster
  7. get_sp_env
  8. get_sp_info
  9. get_sp_var_params
  10. get_species_raster
  11. is_bbox_hole
  12. ramp_env
  13. replicate_sp_raster
  14. validate_aquamaps_species

  Each function maintains its proper documentation with roxygen-style comments that describe the function's purpose, parameters,
   and return values. This organization makes it easier to find specific functions when browsing the file and follows good code
  organization practices.


I've successfully updated all functions in the `libs/am_functions.R` file to add verbose parameters and enhanced verbosity 
  options.

  The key changes I've made include:

  1. Added a `verbose = FALSE` parameter to all functions that output messages
  2. Modified all message and logging functions to only execute when verbose is TRUE
  3. Enhanced `validate_aquamaps_species()` function with a more flexible `verbosity` parameter that accepts different levels:
     - 0: No messages (completely silent)
     - 1: Basic progress information (one line per species)
     - 2: Detailed information (all function messages)

  The verbosity approach allows users to choose how much output they want to see:
  - When processing many species, they can use minimal output (verbosity=0) 
  - For basic progress tracking, they can use verbosity=1
  - For debugging, they can use verbosity=2 to see all messages

  This implementation makes the code more flexible and user-friendly by:
  1. Offering a consistent interface for controlling output verbosity
  2. Keeping default behavior quiet (verbose=FALSE) for most functions
  3. Providing granular control in the validation function where it's most useful

  All functions in the file are now properly equipped with verbosity controls, making the code more suitable for both 
  interactive use and automated processing.
  
# Get revised bounding box from spp_cells

## 2025-05-15.b ∆ replicate_aquamaps: add raster cache

Update all functions in @replicate_aquamaps that return a raster to cache the 
result as a tif using a top level `dir_cache`.

### Response

Let's finalize by updating the todo list to mark all tasks as complete:

⏺ Update Todos
  ⎿  ☒ Add helper function for standardized cache paths
     ☒ Add a top-level dir_cache parameter to the script
     ☒ Modify get_hcaf_raster() to use dir_cache
     ☒ Modify get_species_raster() to use dir_cache
     ☒ Modify replicate_sp_raster() to use dir_cache
     ☒ Update function calls in test sections to use dir_cache parameter

⏺ I've completed all the caching updates in replicate_aquamaps.qmd. Here's what I implemented:

  1. Added a top-level dir_cache parameter to the script
  2. Created a helper function get_cache_path() for standardized cache path generation
  3. Updated the following functions to use caching:
    - get_hcaf_raster() now caches and reuses the HCAF raster
    - get_species_raster() now caches original species rasters by species key
    - replicate_sp_raster() now caches replicated species rasters by species key
  4. Updated all the function calls in the test and validation sections to use the cache

  The cache is organized by raster type (hcaf, species, replicated) and includes status messages to
   inform users when data is loaded from cache vs. generated new. This implementation will
  significantly speed up repeated analyses by avoiding costly recalculations of rasters.

## 2025-05-15.a + replicate_aquamaps.qmd

Create a new Quarto document `replicate_aquamaps.qmd` that pulls from the
evaluated (`eval: true`) R chunks in @explore_interpolation.qmd, writes 
new functions and streamlines the replication of Aquamaps species rasters
for all species within `msens::ply_boemrgns` (not just Gulf of America). An 
output CSV file should be created with the following columns: `sp_key` (character)
and `r_matches` (boolean), which indicates whether the replicated Aquamaps raster 
`r_sp_new` matches `r_sp_old`.

### Response

I've created a new Quarto document replicate_aquamaps.qmd that builds upon
the exploratory work in explore_interpolation.qmd. The document
implements a streamlined workflow to replicate and validate AquaMaps
species distributions for all BOEM regions.

Key improvements in the new document:

1. Organized code into reusable functions for each step of the process
2. Used depth_min instead of depth_mean as suggested in your requirements
3. Implemented comprehensive validation across all species
4. Added comparison tools to analyze matches and non-matches
5. Generates a CSV file with validation results as requested

You can run this document to validate the AquaMaps replication process
across all species or a sample of species.
