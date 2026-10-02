#!/usr/bin/env Rscript
# egress_report.R -- who is pulling bytes off the VM and off the bucket ----
#
# A committed SCRIPT, not a notebook: it must not become a pipeline target (it has no output the
# pipeline depends on, and it reads logs that live outside the repo).
#
# usage (from workflows/):
#   Rscript scripts/egress_report.R <caddy_log_dir> [<s3_access_logs>] [--ips-to-file]
#
#   <caddy_log_dir>   directory of Caddy JSON access logs (*.log, *.log.gz; one JSON object per line)
#   <s3_access_logs>  optional: a directory, or an s3:// prefix, of S3 SERVER ACCESS logs
#                     (an s3:// prefix needs AWS credentials the CLI/credential chain can find)
#   --ips-to-file     also write the client-IP table into the markdown file (default: stdout only)
#
# output: stdout and _output/egress_report_{date}.md -- bytes + requests per day x host, per host by
# path class (extension or dir/) and by user-agent class (browser, named bot, tool, scanner,
# other), the status-code mix, and the top 20 client IPs by bytes with their user-agent.
#
# fetch the Caddy logs (document only -- this script never touches the server):
#   mkdir -p ~/_big/msens/logs/caddy
#   ssh msens 'sudo tar -C /share/logs/caddy -cz .' | tar -xz -C ~/_big/msens/logs/caddy
#
# THE LOGS CONTAIN IP ADDRESSES (personal data): keep them outside the repo and never commit them.
# for the same reason the client-IP table goes to STDOUT only unless --ips-to-file is given, and
# `_output/egress_report_*.md` is git-ignored (see .gitignore: `_output/` is tracked and published
# to the workflows website, so a report there would otherwise be committed and served).

librarian::shelf(DBI, duckdb, glue, here, knitr, quiet = TRUE)

# args ----
args       <- commandArgs(trailingOnly = TRUE)
ips_file   <- "--ips-to-file" %in% args
pos        <- args[!startsWith(args, "--")]
stopifnot("usage: egress_report.R <caddy_log_dir> [<s3_access_logs>] [--ips-to-file]" = length(pos) %in% 1:2)
dir_caddy  <- pos[1]
dir_s3     <- if (length(pos) == 2) pos[2] else NA_character_
stopifnot("caddy log directory not found" = dir.exists(dir_caddy))
out_md     <- here::here("_output", sprintf("egress_report_%s.md", format(Sys.Date())))

# classification rules (single place; regexes are RE2, matched on lower-case) ----
RX_SCANNER <- paste(c(
  "\\.(php|asp|aspx|jsp|cgi|sql|bak|ini|htaccess|htpasswd)(/|$)", "(^|/)\\.(env|git|svn|aws|ssh|ds_store)",
  "wp-(admin|login|content|includes|json)", "xmlrpc", "phpmyadmin", "cgi-bin", "actuator", "etc/passwd",
  "\\.\\./", "/vendor/", "(^|/)(config|credentials|secrets)\\.(json|yml|yaml)$"), collapse = "|")
RX_BOT <- c(                                        # named bots -> the label shown
  GPTBot = "gptbot", `ChatGPT-User` = "chatgpt-user", `OAI-SearchBot` = "oai-searchbot",
  ClaudeBot = "claudebot", `Claude-User` = "claude-user", `Claude-SearchBot` = "claude-searchbot",
  Amazonbot = "amazonbot", Googlebot = "googlebot", GoogleOther = "googleother",
  `Google-Extended` = "google-extended", bingbot = "bingbot", Bytespider = "bytespider",
  PetalBot = "petalbot", AhrefsBot = "ahrefsbot", SemrushBot = "semrushbot", MJ12bot = "mj12bot",
  DotBot = "dotbot", Applebot = "applebot", YandexBot = "yandex", CCBot = "ccbot",
  PerplexityBot = "perplexity", `meta-externalagent` = "meta-externalagent",
  facebookexternalhit = "facebookexternalhit", DuckDuckBot = "duckduckbot", bitsexplorer = "bitsexplorer",
  `LetsEncrypt-validation` = "let.s encrypt validation", UptimeRobot = "uptimerobot")
RX_TOOL <- c(                                       # command-line / library clients
  curl = "^curl/", python = "python|aiohttp|httpx", node = "^node($|[ /-])|undici|axios",
  duckdb = "duckdb", rclone = "rclone", wget = "^wget", go = "go-http-client", `aws-cli` = "aws-cli|botocore|boto3",
  java = "^java/|apache-httpclient|okhttp", libwww = "libwww")
RX_BOT_GENERIC <- "bot|crawl|spider|scan|checker|validat|monitor"

sql_case <- function(rx, prefix) paste(sprintf("WHEN regexp_matches(lua, '%s') THEN '%s: %s'", gsub("'", "''", rx), prefix, names(rx)), collapse = "\n      ")

# connect + load both log kinds into ONE shape: req(src, ts, day, host, path, status, size, ip, ua) ----
con <- dbConnect(duckdb(), ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

parts <- character()
# make_timestamp(micros) is a naive UTC timestamp: days are UTC whatever the session time zone
invisible(dbExecute(con, glue("
  CREATE VIEW caddy AS
  SELECT 'caddy' AS src, ts,
         CAST(make_timestamp(CAST(ts * 1000000 AS BIGINT)) AS DATE)                                  AS day,
         regexp_replace(lower(json_extract_string(request, '$.host')), ':[0-9]+$', '')               AS host,
         json_extract_string(request, '$.uri')                                                       AS uri,
         status, coalesce(size, 0)                                                                   AS size,
         coalesce(json_extract_string(request, '$.client_ip'), json_extract_string(request, '$.remote_ip')) AS ip,
         coalesce(json_extract_string(request, '$.headers.\"User-Agent\"[0]'), '')                   AS ua
  FROM read_json('{dir_caddy}/**/*.log*', format = 'newline_delimited', ignore_errors = true,
                 columns = {{ts: 'DOUBLE', status: 'INTEGER', size: 'BIGINT', request: 'JSON'}})
  WHERE request IS NOT NULL AND ts IS NOT NULL")))
parts <- c(parts, "SELECT * FROM caddy")

if (!is.na(dir_s3)) {
  if (startsWith(dir_s3, "s3://")) {
    invisible(dbExecute(con, "INSTALL httpfs; LOAD httpfs;"))
    invisible(dbExecute(con, "CREATE SECRET (TYPE s3, PROVIDER credential_chain)"))
  }
  glob_s3 <- paste0(sub("/+$", "", dir_s3), "/**")
  # S3 server access log record (AWS docs): owner bucket [time] ip requester reqid operation key
  # "request-uri" status error bytes-sent object-size total-ms turnaround-ms "referrer" "user-agent" ...
  rx_s3 <- paste0('^(\\S+) (\\S+) \\[([^\\]]+)\\] (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) "([^"]*)" (\\S+) (\\S+) (\\S+) ',
                  '(\\S+) (\\S+) (\\S+) "([^"]*)" "([^"]*)"')
  nm_s3 <- c("owner", "bucket", "time", "ip", "requester", "reqid", "op", "key", "request_uri", "status", "err",
             "bytes", "objsize", "total_ms", "turn_ms", "referrer", "ua")
  invisible(dbExecute(con, glue("
    CREATE VIEW s3 AS
    WITH l AS (SELECT unnest(string_split_regex(content, '\\r?\\n')) AS line FROM read_text('{glob_s3}')),
    p AS (SELECT regexp_extract(line, '{gsub(\"'\", \"''\", rx_s3)}', [{paste0(\"'\", nm_s3, \"'\", collapse = ', ')}]) AS r FROM l WHERE line <> '')
    SELECT 's3' AS src,
           epoch(strptime(substr(r.time, 1, 20), '%d/%b/%Y:%H:%M:%S'))        AS ts,   -- S3 logs are always +0000
           CAST(strptime(substr(r.time, 1, 20), '%d/%b/%Y:%H:%M:%S') AS DATE) AS day,
           r.bucket AS host, '/' || r.key AS uri,
           try_cast(r.status AS INTEGER) AS status, coalesce(try_cast(r.bytes AS BIGINT), 0) AS size,
           r.ip AS ip, r.ua AS ua
    FROM p WHERE r.bucket <> ''")))
  parts <- c(parts, "SELECT * FROM s3")
}

# derived: path, path class, user-agent class ----
invisible(dbExecute(con, glue("
  CREATE VIEW req AS
  WITH u AS (SELECT *, split_part(uri, '?', 1) AS path, lower(ua) AS lua FROM ({paste(parts, collapse = ' UNION ALL ')}))
  SELECT src, ts, day, host, path, status, size, ip, ua,
    CASE WHEN path LIKE '%/' THEN 'dir/'
         WHEN regexp_extract(lower(path), '\\.([a-z0-9]{{1,8}})$', 1) <> '' THEN '.' || regexp_extract(lower(path), '\\.([a-z0-9]{{1,8}})$', 1)
         ELSE '(no extension)' END AS path_class,
    CASE WHEN regexp_matches(lower(path), '{RX_SCANNER}') AND NOT starts_with(path, '/.well-known/') THEN 'scanner'
         WHEN lua = '' THEN 'other: empty UA'
         {sql_case(RX_BOT, 'bot')}
         {sql_case(RX_TOOL, 'tool')}
         WHEN regexp_matches(lua, '{RX_BOT_GENERIC}') THEN 'bot: other'
         WHEN starts_with(lua, 'mozilla/') THEN 'browser'
         ELSE 'other' END AS ua_class
  FROM u")))

q <- function(sql) dbGetQuery(con, sql)
n_req <- q("SELECT count(*) n FROM req")$n
stopifnot("no requests found: check the log directory and file names (*.log)" = n_req > 0)

# formatting ----
fmt_b   <- function(x) ifelse(is.na(x), "", ifelse(x >= 1e9, sprintf("%.2f GB", x / 1e9), ifelse(x >= 1e6, sprintf("%.1f MB", x / 1e6), ifelse(x >= 1e3, sprintf("%.1f kB", x / 1e3), sprintf("%d B", as.integer(x))))))
fmt_n   <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
trunc_s <- function(x, n = 70) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "..."), x)
md_tbl  <- function(d, ...) paste(knitr::kable(d, format = "pipe", row.names = FALSE, ...), collapse = "\n")
tbl_bytes <- function(d, bcol = "bytes") { d[[bcol]] <- fmt_b(d[[bcol]]); for (n in names(d)[vapply(d, is.numeric, NA)]) d[[n]] <- fmt_n(d[[n]]); d }

out <- character()
add <- function(...) out <<- c(out, ...)

# summary ----
tot  <- q("SELECT count(*) AS requests, sum(size) AS bytes, count(DISTINCT ip) AS ips, min(day) AS d0, max(day) AS d1 FROM req")
day1 <- q("SELECT day, sum(size) AS bytes, count(*) AS requests FROM req GROUP BY 1 ORDER BY 2 DESC LIMIT 1")
ua1  <- q("SELECT ua, sum(size) AS bytes, count(*) AS requests FROM req GROUP BY 1 ORDER BY 2 DESC LIMIT 1")
st4  <- q("SELECT count(*) FILTER (WHERE status = 404) AS n404, count(*) FILTER (WHERE status >= 400) AS n4xx5xx FROM req")
add(sprintf("# Egress report, %s", format(Sys.Date())), "",
    sprintf("Logs: `%s`%s", dir_caddy, if (is.na(dir_s3)) "" else sprintf(" + S3 access logs `%s`", dir_s3)),
    "(days are UTC; `size` is the response size the server logged, so it is bytes sent, not bytes of the object)", "",
    "## Summary", "",
    sprintf("- window: %s to %s (%d day(s) with traffic)", tot$d0, tot$d1, q("SELECT count(DISTINCT day) n FROM req")$n),
    sprintf("- requests: %s; bytes: %s (%s); distinct client IPs: %s", fmt_n(tot$requests), fmt_b(tot$bytes), fmt_n(tot$bytes), fmt_n(tot$ips)),
    sprintf("- largest day: %s with %s in %s requests (%.0f%% of all bytes)", day1$day, fmt_b(day1$bytes), fmt_n(day1$requests), 100 * day1$bytes / tot$bytes),
    sprintf("- largest user agent by bytes: `%s` with %s in %s requests (%.0f%% of all bytes)", trunc_s(ua1$ua), fmt_b(ua1$bytes), fmt_n(ua1$requests), 100 * ua1$bytes / tot$bytes),
    sprintf("- responses with status 404: %s; any 4xx/5xx: %s (of %s)", fmt_n(st4$n404), fmt_n(st4$n4xx5xx), fmt_n(tot$requests)), "",
    md_tbl(tbl_bytes(q("SELECT src AS source, count(*) AS requests, sum(size) AS bytes FROM req GROUP BY 1 ORDER BY 3 DESC")), caption = "By log source"), "")

# bytes + requests per day x host ----
add("## Bytes and requests per day x host", "",
    md_tbl(tbl_bytes(q("SELECT day, host, count(*) AS requests, sum(size) AS bytes FROM req GROUP BY 1, 2 ORDER BY 1 DESC, 4 DESC LIMIT 400")),
           caption = "Newest day first, at most 400 rows"), "")

# per host by path class ----
add("## Per host by path class (extension or `dir/`)", "",
    md_tbl(tbl_bytes(q("
      SELECT host, path_class, count(*) AS requests, sum(size) AS bytes FROM req
      GROUP BY 1, 2 QUALIFY row_number() OVER (PARTITION BY host ORDER BY sum(size) DESC) <= 12
      ORDER BY 1, 4 DESC")), caption = "Top 12 path classes per host, by bytes"), "")

# per host by user-agent class ----
add("## Per host by user-agent class", "",
    md_tbl(tbl_bytes(q("
      SELECT host, ua_class, count(*) AS requests, sum(size) AS bytes FROM req
      GROUP BY 1, 2 QUALIFY row_number() OVER (PARTITION BY host ORDER BY sum(size) DESC) <= 15
      ORDER BY 1, 4 DESC")), caption = "Top 15 user-agent classes per host, by bytes"), "",
    md_tbl(tbl_bytes(q("SELECT ua_class, count(*) AS requests, sum(size) AS bytes, count(DISTINCT ip) AS ips FROM req GROUP BY 1 ORDER BY 3 DESC")),
           caption = "All hosts"), "")

# status-code mix ----
add("## Status-code mix", "",
    md_tbl(tbl_bytes(q("
      SELECT status, count(*) AS requests, round(100.0 * count(*) / (SELECT count(*) FROM req), 1) AS pct_requests, sum(size) AS bytes
      FROM req GROUP BY 1 ORDER BY 2 DESC")), caption = "By status code"), "",
    md_tbl(tbl_bytes(q("
      SELECT host, CAST(status / 100 AS INTEGER) || 'xx' AS status_class, count(*) AS requests, sum(size) AS bytes
      FROM req GROUP BY 1, 2 ORDER BY 1, 2")), caption = "Per host by status class"), "")

# top client IPs (personal data: stdout only unless --ips-to-file) ----
ips <- q("
  WITH g AS (SELECT ip, ua, sum(size) AS b, count(*) AS n FROM req GROUP BY 1, 2)
  SELECT ip, sum(b) AS bytes, sum(n) AS requests, arg_max(ua, b) AS user_agent
  FROM g GROUP BY 1 ORDER BY 2 DESC LIMIT 20")
ips$user_agent <- trunc_s(ips$user_agent, 60)
ip_md <- c("## Top 20 client IPs by bytes", "", md_tbl(tbl_bytes(ips), caption = "Client IPs are personal data: do not commit or publish"), "")
if (ips_file) add(ip_md) else
  add("## Top 20 client IPs by bytes", "",
      "_Omitted from this file (IP addresses are personal data); printed to stdout only. Re-run with `--ips-to-file` to include it._", "")

# emit ----
dir.create(dirname(out_md), showWarnings = FALSE, recursive = TRUE)
writeLines(out, out_md)
cat(out, sep = "\n")
if (!ips_file) cat(ip_md, sep = "\n")
cat(sprintf("\n[wrote %s]\n", normalizePath(out_md)))
