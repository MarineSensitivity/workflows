# packages ----
# if (!require("librarian")){
#   install.packages("librarian")
#   library(librarian)
# }
librarian::shelf(
  DBI, dbplyr, dplyr, glue, here, RPostgres,
  quiet = T)

is_server <- Sys.info()[["sysname"]] == "Linux"
host <- ifelse(
  is_server,
  "postgis",   # from rstudio to postgis docker container on server
  "localhost") # from laptop to locally tunneled connection to db container on server
# for localhost db, see: https://github.com/marinesensitivity/server#connect

# database connect ----
dir_private <- switch(
  Sys.info()[["sysname"]],
  "Darwin" = "/Users/bbest/My Drive/private",
  "Linux"  = "/share/private")

db_pass_txt <- glue("{dir_private}/msens-db_admin-pass.txt")

stopifnot(file.exists(db_pass_txt))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "msens",
  host     = host,
  port     = 5432,
  user     = "admin",
  password = readLines(db_pass_txt),
  options  ="-c search_path=public,raw,aquamaps")

# test connection:
# dbListTables(con)

# helper functions ----
create_index <- function(con, tbl, flds, schema = "public", geom=F, unique=F, overwrite=F, show=F, exec=T){
  # tbl = "taxa"; flds = c("tbl_orig", "aphia_id"); unique = T; geom=F
  stopifnot(!(geom == T & length(flds) != 1))
  sfx <- ifelse(
    geom,
    glue::glue(" USING GIST ({flds})"),
    glue::glue("({paste(flds, collapse=', ')})"))
  idx <- ifelse(
    unique,
    glue("{tbl}_unique_idx"),
    glue("{tbl}_{paste(flds, collapse='_')}_idx"))

  if (overwrite)
    dbExecute(con, glue("DROP INDEX IF EXISTS {schema}.{idx}"))

  sql <- glue::glue(
    "CREATE {ifelse(unique, 'UNIQUE','')} INDEX IF NOT EXISTS {idx} ON {schema}.{tbl}{sfx}")
  if (show)
    message(sql)
  if (exec)
    dbExecute(con, sql)
}
