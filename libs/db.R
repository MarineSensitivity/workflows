# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  DBI, dbplyr, dplyr, here, RPostgres,
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
  options  ="-c search_path=public,raw")

# test connection:
# dbListTables(con)
