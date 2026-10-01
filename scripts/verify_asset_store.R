#!/usr/bin/env Rscript
# verify_asset_store.R: compare the store prefixes of the bucket with the catalog (assets.parquet) -- READ ONLY.
#
#   Rscript scripts/verify_asset_store.R <assets.parquet> [--prefix cog/usa05 --prefix cog/global05 --prefix native]
#                                        [--bucket oceanmetrics.io-public] [--root marine-atlas] [--sample 200]
#
# Lists each store prefix with `aws s3api list-objects-v2` (one read per prefix) and checks, BOTH ways:
#   * every catalog row exists under its key, with the catalogued size and (for a single-part object) ETag == md5
#   * nothing exists under a store prefix that the catalog does not list (an unexpected object)
# then HEADs a random sample of the catalog's keys anonymously over HTTPS (public readability + content type).
# Prints a summary and exits 0 only when there is no difference; exit 1 otherwise. Never writes anything.
suppressMessages({library(arrow); library(dplyr)})
a <- commandArgs(TRUE)
opt <- function(flag, default = NULL) { i <- which(a == flag); if (length(i)) a[i + 1] else default }
many <- function(flag) { i <- which(a == flag); if (length(i)) a[i + 1] else character() }
cat_path <- a[1]; stopifnot("usage: verify_asset_store.R <assets.parquet> [--prefix P ...]" = !is.na(cat_path) && file.exists(cat_path))
bucket <- opt("--bucket", "oceanmetrics.io-public"); root <- opt("--root", "marine-atlas")
prefixes <- many("--prefix"); if (!length(prefixes)) prefixes <- c("cog/usa05", "cog/global05", "native")
n_sample <- as.integer(opt("--sample", "200"))

cat_ <- as.data.frame(read_parquet(cat_path))
stopifnot("catalog needs key, bytes, md5" = all(c("key", "bytes", "md5") %in% names(cat_)), !anyDuplicated(cat_$key))
cat_ <- cat_[Reduce(`|`, lapply(prefixes, function(p) startsWith(cat_$key, paste0(p, "/")))), ]

list_prefix <- function(p) {
  out <- system2("aws", c("s3api", "list-objects-v2", "--bucket", bucket, "--prefix", shQuote(sprintf("%s/%s/", root, p)),
                          "--query", shQuote("Contents[].[Key,Size,ETag]"), "--output", "text"), stdout = TRUE, stderr = FALSE)
  if (!length(out) || identical(trimws(out[1]), "None")) return(data.frame(key = character(), size = numeric(), etag = character()))
  x <- read.delim(text = out, header = FALSE, sep = "\t", quote = "", col.names = c("key", "size", "etag"), stringsAsFactors = FALSE)
  x$key <- sub(sprintf("^%s/", root), "", x$key); x$etag <- gsub('"', "", x$etag); x
}
live <- do.call(rbind, lapply(prefixes, list_prefix))
cat(sprintf("catalog rows under %s: %d | objects listed: %d\n", paste(prefixes, collapse = ", "), nrow(cat_), nrow(live)))

missing    <- setdiff(cat_$key, live$key)
unexpected <- setdiff(live$key, cat_$key)
j <- merge(cat_[, c("key", "bytes", "md5")], live, by = "key")
size_bad <- j$key[j$bytes != j$size]
md5_bad  <- j$key[!grepl("-", j$etag, fixed = TRUE) & !is.na(j$md5) & j$md5 != j$etag]
cat(sprintf("missing from the bucket: %d | unexpected in the bucket: %d | size differs: %d | single-part ETag != md5: %d\n",
            length(missing), length(unexpected), length(size_bad), length(md5_bad)))
show <- function(label, x) if (length(x)) cat(sprintf("  %s (first 5): %s\n", label, paste(utils::head(x, 5), collapse = ", ")))
show("missing", missing); show("unexpected", unexpected); show("size differs", size_bad); show("md5 differs", md5_bad)

# anonymous HEAD of a random sample: the object is publicly readable and carries the content type the clients expect
set.seed(20261001)
smp <- cat_$key[seq_len(nrow(cat_)) %in% sample.int(nrow(cat_), min(n_sample, nrow(cat_)))]
head1 <- function(k) {
  h <- system2("curl", c("-sI", "-m", "20", shQuote(sprintf("https://s3.us-east-1.amazonaws.com/%s/%s/%s", bucket, root, k))), stdout = TRUE, stderr = FALSE)
  c(code = sub("^HTTP/[0-9.]+ ([0-9]+).*$", "\\1", h[1]), type = tolower(sub("^[Cc]ontent-[Tt]ype: *", "", trimws(grep("^[Cc]ontent-[Tt]ype", h, value = TRUE)[1]))))
}
hd <- t(vapply(smp, head1, c(code = "", type = "")))
want_type <- ifelse(grepl("\\.tif$", smp), "image/tiff", "binary/octet-stream")
bad_head <- smp[hd[, "code"] != "200" | hd[, "type"] != want_type]
cat(sprintf("anonymous HEAD sample: %d keys | not 200 / wrong content type: %d\n", length(smp), length(bad_head)))
show("bad HEAD", bad_head)

ok <- !length(missing) && !length(unexpected) && !length(size_bad) && !length(md5_bad) && !length(bad_head)
cat(if (ok) "VERIFIED: the store prefixes match the catalog\n" else "DIFFERENCES FOUND: the store prefixes do not match the catalog\n")
quit(status = if (ok) 0L else 1L)
