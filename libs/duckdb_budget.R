# size DuckDB to the MACHINE, not to the laptop a notebook was written on ----
#
# A hardcoded `memory_limit='12GB'` is a promise the server cannot keep: msens1 has 16 GB and
# 4 cores shared with ERDDAP, two titilers, the API and the Shiny workers. With a limit above
# what exists DuckDB never spills -- it grows until the box is out of memory. That happened
# twice on 2026-09-21 (v7.1 P4): `build_v7b.qmd` was OOM-killed in calc_cell_metric_redlist,
# and `build_v7_cell_model.qmd`'s 572 M-row partitioned write took R to 7.4 GB, left 175 MB
# available with no swap, and starved the host for ~25 min -- load 100, sshd unreachable,
# /scores, /species and STAC timing out -- until the render was killed by hand.
#
# So: 40 % of what is AVAILABLE, capped at 12 GB, threads <= cores, spilling to
# temp_directory beyond it. 40 and not more because the limit bounds DuckDB's buffer pool,
# not the process: measured on build_v7b, a 2.3 GB limit peaks at 3.2 GB resident (R + data
# frames + allocations outside the pool). Output is identical (build_v7b's reference digests
# reproduce at 12 GB, 2.3 GB and 1.5 GB). BUILD_MEMORY_GB / BUILD_THREADS override.
# scripts/srv_render.sh carries the second line of defence: a MemAvailable watchdog.

duckdb_budget <- function() {
  gb_env <- suppressWarnings(as.numeric(Sys.getenv("BUILD_MEMORY_GB")))
  th_env <- suppressWarnings(as.integer(Sys.getenv("BUILD_THREADS")))
  avail  <- NA_real_
  if (file.exists("/proc/meminfo")) {
    l     <- grep("^MemAvailable:", readLines("/proc/meminfo"), value = TRUE)
    avail <- as.numeric(gsub("\\D", "", l)) / 1024^2                  # kB -> GB
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    # macOS has no MemAvailable: free + inactive + speculative pages (vm_stat) is the reclaimable estimate.
    # Without this the budget fell back to 12 GB on a 48 GB machine, and a spill-heavy pass (the share
    # family's floor-strip variant, 2026-10-30) ran I/O-bound for hours on the external volume.
    vs <- tryCatch(system("vm_stat", intern = TRUE), error = function(e) character())
    pg <- suppressWarnings(as.numeric(sub(".*page size of (\\d+) bytes.*", "\\1", vs[1])))
    n  <- function(k) suppressWarnings(as.numeric(gsub("\\D", "", grep(k, vs, value = TRUE)[1])))
    if (is.finite(pg)) avail <- sum(n("^Pages free"), n("^Pages inactive"), n("^Pages speculative"), na.rm = TRUE) * pg / 1024^3
  }
  # the cap: 12 GB protects the 16 GB server (shared with production); a dedicated render machine
  # (the 48 GB Mac mini) may take up to 24 GB -- the output is identical at any budget
  cap <- if (Sys.info()[["sysname"]] == "Darwin") 24 else 12
  list(
    gb      = if (!is.na(gb_env)) gb_env else if (!is.na(avail)) max(1.5, min(cap, round(avail * 0.4, 1))) else 12,
    threads = if (!is.na(th_env)) th_env else min(6L, parallel::detectCores()),
    avail   = avail)
}

duckdb_tune <- function(con, tmp_dir) {
  b <- duckdb_budget()
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(con, glue::glue("PRAGMA memory_limit='{b$gb}GB'"))
  DBI::dbExecute(con, glue::glue("PRAGMA threads={b$threads}"))
  DBI::dbExecute(con, glue::glue("PRAGMA temp_directory='{tmp_dir}'"))
  # insertion order is not part of any result these notebooks keep, and preserving it
  # costs memory (a partitioned COPY buffers far less without it)
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  logger::log_info("duckdb budget: {b$gb} GB, {b$threads} threads (available: {round(b$avail, 1)} GB)")
  invisible(b)
}
