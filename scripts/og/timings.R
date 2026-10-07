#!/usr/bin/env Rscript
# step timings of the og runs, from the `timings` block the pipeline writes into every log.json
# (the block is CUMULATIVE minutes since the start at the end of each step; differenced here to minutes per step,
# per species x acronym), plus the published OBIS runs for comparison. "Model fit" spans the thinning, the data
# object + spatial blocks and the three algorithms' CV tuning; the pipeline does not time those apart.
#   Rscript scripts/og/timings.R            # prints the table, writes data/og/timings.csv
librarian::shelf(dplyr, jsonlite, purrr, readr, tidyr, quiet = TRUE)
home <- path.expand("~")
f_log <- c(
  list.files(file.path(home, "_big/sdm/og"),           "_what=log\\.json$", recursive = TRUE, full.names = TRUE),
  list.files(file.path(home, "_big/sdm/obis/species"), "_what=log\\.json$", recursive = TRUE, full.names = TRUE))
common <- c(`137205` = "loggerhead", `137206` = "green", `137207` = "hawksbill", `137208` = "kemps_ridley",
            `137209` = "leatherback", `220293` = "olive_ridley", `159023` = "right_whale")
d <- map_dfr(f_log, \(f) {
  j <- read_json(f, simplifyVector = TRUE)
  if (is.null(j$timings) || !length(j$timings)) return(NULL)
  tibble(taxon = as.character(unlist(j$taxonID)[1]), acro = unlist(j$model_acro)[1],
         n_init = unlist(j$n_init_points)[1], n_fit = unlist(j$model_fit_points)[1],
         step = as.character(j$timings$what), mins = diff(c(0, as.numeric(j$timings$time_mins))))
}) |>
  mutate(common = common[taxon], step = sub("^(.{40}).*", "\\1", step)) |>
  group_by(taxon, common, acro, n_init, n_fit, step) |> summarise(mins = sum(mins), .groups = "drop") |>
  group_by(taxon, acro) |> mutate(total_mins = sum(mins), pct = round(100 * mins / total_mins)) |> ungroup()
dir.create("data/og", showWarnings = FALSE)
write_csv(d, "data/og/timings.csv")
# species x acronym totals, then the share of the slowest step
tot <- d |> distinct(common, acro, n_init, n_fit, total_mins) |>
  pivot_wider(id_cols = c(common, n_init, n_fit), names_from = acro, values_from = total_mins) |>
  arrange(desc(n_init))
print(as.data.frame(tot |> mutate(across(where(is.numeric) & !c(n_init, n_fit), round))), row.names = FALSE)
cat("\nsteps taking >= 5% of a run, in order of total minutes:\n")
d |> group_by(step) |> summarise(mins = round(sum(mins)), max_pct = max(pct), n_runs = n()) |>
  filter(max_pct >= 5) |> arrange(desc(mins)) |> as.data.frame() |> print(row.names = FALSE)
