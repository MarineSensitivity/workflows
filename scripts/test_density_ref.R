# test_density_ref.R -- fixture assertions for libs/density_ref.R (the qmap reference helper of the three density ingests)
#   Rscript scripts/test_density_ref.R        (from workflows/; writes only to tempdir())
librarian::shelf(arrow, dplyr, fs, here, tibble, quiet = TRUE)
library(msens)
source(here("libs/density_ref.R"))

# fixture: two AquaMaps models of ONE taxon (WORMS:1) with overlapping cells, one of another (BOTW:9), one of nobody
am_dir <- path(tempdir(), "dataset=am"); dir_create(am_dir)
wr <- \(k, cells, val) write_parquet(tibble(mdl_key = paste0("am|", k), cell_id = as.integer(cells), val = val, dataset = "am"),
                                      path(am_dir, paste0(k, ".parquet")))
wr("A-1", 1:100,   rep(40, 100))
wr("A-2", 51:200,  rep(60, 150))
wr("B-1", 1:300,   seq(1, 100, length.out = 300))
idx <- tibble(mdl_key = c("am|A-1", "am|A-2", "am|B-1"), authority = c("worms", "worms", "botw"), taxon_id = c("1", "1", "9"))

# keys: BOTW first, then WoRMS in the order given; the first candidate with any model wins; no pooling across candidates
stopifnot(setequal(density_am_keys(idx, worms = "1"), c("am|A-1", "am|A-2")),
          identical(density_am_keys(idx, botw = "9"), "am|B-1"),
          identical(density_am_keys(idx, worms = "1", botw = "9"), "am|B-1"),
          setequal(density_am_keys(idx, worms = c("777", "1")), c("am|A-1", "am|A-2")),     # a synonym with no model falls through to the accepted id
          length(density_am_keys(idx, worms = c("777", NA, ""))) == 0,
          length(density_am_keys(idx)) == 0)

# cells: per-cell max over the taxon's models, restricted to the domain
x <- density_am_cells(c("am|A-1", "am|A-2"), am_dir, cell_ids = c(10, 60, 150, 250))
stopifnot(identical(x$cell_id, c(10L, 60L, 150L)), identical(x$val, c(40, 60, 60)),   # 60 is in both (max 60); 250 is in neither
          is.null(density_am_cells(character(), am_dir)))

# qmap_one: 60 reference cells >= 50 -> qmap, whose quantiles equal the reference's; < 50 or none -> the ud percentile
set.seed(1); cid <- 1:300; dens <- c(runif(299), 0)                     # the last cell is absent (density 0): outside the domain
ref <- density_am_cells("am|B-1", am_dir, cell_ids = cid)
o <- density_qmap_one(dens, cid, ref, area = rep(1, 300))
stopifnot(o$transform == "qmap", o$n_ref_cells == 299L,
          o$val[300] == 0, all(o$val[1:299] >= 1 & o$val[1:299] <= 100),
          abs(median(o$val[1:299]) - o$ref_p50) < 0.5, identical(order(o$val[1:299]), order(dens[1:299])))   # the spatial ordering is kept
few <- density_qmap_one(dens, cid, ref[1:10, ], area = rep(1, 300))
none <- density_qmap_one(dens, cid, NULL, area = rep(1, 300))
ud  <- msens::density_to_suit(dens, "ud", area = rep(1, 300), digits = 2)
stopifnot(few$transform == density_ref_fallback, few$n_ref_cells == 10, identical(few$val, ud),
          none$transform == density_ref_fallback, none$n_ref_cells == 0, is.na(none$ref_p50), identical(none$val, ud))

# groups: values aligned to the input rows whatever their order; one stats row per group; pooled stats
d2 <- tibble(region = c("a", "b", "a", "b", "a", "b"), interval = "annual", cell_id = c(1L, 2L, 3L, 4L, 5L, 6L), dens = c(.5, 1, 2, 3, 4, 5))
g  <- density_qmap_groups(d2, c("region", "interval"), NULL, \(id) rep(1, length(id)), min_ref = 50L)
stopifnot(length(g$val) == 6, nrow(g$stats) == 2, all(g$stats$transform == density_ref_fallback),
          identical(g$val[d2$region == "a"], msens::density_to_suit(d2$dens[d2$region == "a"], "ud", area = rep(1, 3), digits = 2)))
p <- density_qmap_pool(g$stats, g$refs)
stopifnot(p$transform == density_ref_fallback, p$n_ref_cells == 0)
cat("density_ref.R: all assertions passed\n")
