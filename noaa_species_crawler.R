# ==============================================================================
# NOAA Fisheries Species Directory Web Crawler
# ==============================================================================
# Crawls all species from: https://www.fisheries.noaa.gov/species-directory
#
# Two approaches provided:
#   1. Standard read_html() - Works for this site since content is server-rendered
#   2. read_html_live() - For JavaScript-heavy sites (requires chromote + Chrome)
#
# Requirements:
#   install.packages(c("rvest", "dplyr", "purrr", "stringr", "tibble"))
#
# For read_html_live() additionally:
#   install.packages("chromote")
#   # Plus Chrome/Chromium browser installed on system
#
# Usage:
#   source("noaa_species_crawler.R")
#   species <- crawl_noaa_species()           # Standard approach (recommended)
#   species <- crawl_noaa_species_live()      # Live browser approach
# ==============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
BASE_URL <- "https://www.fisheries.noaa.gov"
DIRECTORY_URL <- "https://www.fisheries.noaa.gov/species-directory"
REQUEST_DELAY <- 1.5  # seconds between requests

# ------------------------------------------------------------------------------
# Helper: Parse species data from a table row
# ------------------------------------------------------------------------------
parse_species_row <- function(row) {

 # Column 2: Species name, scientific name, and aliases
  name_element <- row |> html_element("td:nth-child(2) a")
  common_name <- name_element |> html_text(trim = TRUE)
  species_url <- name_element |> html_attr("href")

  # Parse the full name cell text
  name_cell_text <- row |>
    html_element("td:nth-child(2)") |>
    html_text(trim = TRUE)

  lines <- str_split(name_cell_text, "\n")[[1]] |>
    str_trim() |>
    (\(x) x[x != ""])()

  # Scientific name (usually second line, in Genus species format)
  scientific_name <- NA_character_
  if (length(lines) >= 2) {
    candidate <- lines[2]
    if (str_detect(candidate, "^[A-Z][a-z]+ [a-z]+")) {
      scientific_name <- candidate
    }
  }

  # Also known as (alternative names)
  aka_idx <- which(str_detect(lines, "Also Known As"))
  also_known_as <- if (length(aka_idx) > 0 && aka_idx < length(lines)) {
    paste(lines[(aka_idx + 1):length(lines)], collapse = ", ")
  } else {
    NA_character_
  }

  # Column 3: Species category
  category_text <- row |>
    html_element("td:nth-child(3)") |>
    html_text(trim = TRUE)

  category_lines <- str_split(category_text, "\n")[[1]] |>
    str_trim() |>
    (\(x) x[x != "" & x != "Species Category"])()
  category <- paste(category_lines, collapse = "; ")

  # Column 4: Protected status (ESA, CITES, etc.)
  status_text <- row |>
    html_element("td:nth-child(4)") |>
    html_text(trim = TRUE)

  status_lines <- str_split(status_text, "\n")[[1]] |>
    str_trim() |>
    (\(x) x[x != "" & x != "---"])()
  protected_status <- if (length(status_lines) > 0) {
    paste(status_lines, collapse = "; ")
  } else {
    NA_character_
  }

  # Column 5: Region
  region_text <- row |>
    html_element("td:nth-child(5)") |>
    html_text(trim = TRUE)

  region_lines <- str_split(region_text, "\n")[[1]] |>
    str_trim() |>
    (\(x) x[x != ""])() |>
    str_replace("^Region\\s*", "")
  region <- paste(region_lines, collapse = "; ")

  tibble(
    common_name = common_name,
    scientific_name = scientific_name,
    also_known_as = also_known_as,
    category = category,
    protected_status = protected_status,
    region = region,
    species_url = if (!is.na(species_url)) paste0(BASE_URL, species_url) else NA_character_
  )
}

# ------------------------------------------------------------------------------
# Helper: Extract all species from HTML document
# ------------------------------------------------------------------------------
extract_species <- function(html) {
  rows <- html |> html_elements("table tbody tr")

  if (length(rows) == 0) {
    message("  No species rows found")
    return(tibble())
  }

  message(sprintf("  Found %d species", length(rows)))
  map_dfr(rows, parse_species_row)
}

# ------------------------------------------------------------------------------
# Helper: Get total pages from pagination
# ------------------------------------------------------------------------------
get_total_pages <- function(html) {
  # Try to find "Last" link
  last_link <- html |>
    html_element(".pager__item--last a") |>
    html_attr("href")

  if (!is.na(last_link)) {
    page_num <- str_extract(last_link, "page=\\d+") |>
      str_extract("\\d+") |>
      as.numeric()
    return(page_num + 1)  # 0-indexed
  }

  # Fallback: find max page number
  page_numbers <- html |>
    html_elements(".pager__item a") |>
    html_attr("href") |>
    str_extract("page=\\d+") |>
    str_extract("\\d+") |>
    as.numeric() |>
    na.omit()

  if (length(page_numbers) == 0) return(1)
  max(page_numbers) + 1
}

# ------------------------------------------------------------------------------
# Helper: Safe HTTP request with retry
# ------------------------------------------------------------------------------
safe_read_html <- function(url, retries = 3) {
  for (i in 1:retries) {
    result <- tryCatch(
      read_html(url),
      error = function(e) {
        if (i < retries) {
          message(sprintf("  Retry %d/%d after error: %s", i, retries, e$message))
          Sys.sleep(REQUEST_DELAY * 2)
        }
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }
  stop(sprintf("Failed to fetch %s after %d attempts", url, retries))
}

# ==============================================================================
# APPROACH 1: Standard read_html() with pagination
# ==============================================================================
#' Crawl NOAA Species Directory using standard HTTP requests
#'
#' @param verbose Print progress messages
#' @return tibble with species data
crawl_noaa_species <- function(verbose = TRUE) {

  if (verbose) {
    message(strrep("=", 60))
    message("NOAA Fisheries Species Directory Crawler")
    message("Method: Standard HTTP (read_html)")
    message(strrep("=", 60))
  }

  # Get first page and determine total pages
  if (verbose) message("\nFetching page 1...")
  html <- safe_read_html(DIRECTORY_URL)
  total_pages <- get_total_pages(html)

  if (verbose) message(sprintf("Total pages: %d\n", total_pages))

  # Extract from first page
  all_species <- extract_species(html)
  if (verbose) message(sprintf("  Cumulative: %d species\n", nrow(all_species)))

  # Process remaining pages
  for (page in 2:total_pages) {
    Sys.sleep(REQUEST_DELAY)

    page_url <- sprintf("%s?page=%d", DIRECTORY_URL, page - 1)
    if (verbose) message(sprintf("Fetching page %d...", page))

    html <- safe_read_html(page_url)
    page_species <- extract_species(html)
    all_species <- bind_rows(all_species, page_species)

    if (verbose) message(sprintf("  Cumulative: %d species\n", nrow(all_species)))
  }

  # Clean and deduplicate
  all_species <- all_species |>
    distinct() |>
    arrange(common_name)

  if (verbose) {
    message(strrep("=", 60))
    message(sprintf("Complete! Total unique species: %d", nrow(all_species)))
    message(strrep("=", 60))
  }

  all_species
}

# ==============================================================================
# APPROACH 2: read_html_live() for JavaScript-rendered content
# ==============================================================================
#' Crawl NOAA Species Directory using live browser session
#'
#' Requires chromote package and Chrome/Chromium installed
#'
#' @param verbose Print progress messages
#' @return tibble with species data
crawl_noaa_species_live <- function(verbose = TRUE) {

  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("Package 'chromote' required. Install with: install.packages('chromote')")
  }

  if (verbose) {
    message(strrep("=", 60))
    message("NOAA Fisheries Species Directory Crawler")
    message("Method: Live Browser (read_html_live)")
    message(strrep("=", 60))
  }

  # Initialize live browser session
  if (verbose) message("\nStarting browser session...")
  session <- read_html_live(DIRECTORY_URL)
  Sys.sleep(3)  # Wait for initial render

  # Get total pages
  total_pages <- get_total_pages(session)
  if (verbose) message(sprintf("Total pages: %d\n", total_pages))

  all_species <- tibble()

  for (page in 1:total_pages) {
    if (verbose) message(sprintf("Processing page %d of %d", page, total_pages))

    if (page > 1) {
      page_url <- sprintf("%s?page=%d", DIRECTORY_URL, page - 1)
      session$session$Page$navigate(page_url)
      Sys.sleep(REQUEST_DELAY + 1)  # Extra time for JS rendering
    }

    # Extract using the live session's html_elements method
    rows <- session$html_elements("table tbody tr")

    if (length(rows) > 0) {
      if (verbose) message(sprintf("  Found %d species", length(rows)))

      page_species <- map_dfr(rows, function(row) {
        parse_species_row(row)
      })

      all_species <- bind_rows(all_species, page_species)
      if (verbose) message(sprintf("  Cumulative: %d species\n", nrow(all_species)))
    }

    if (page < total_pages) Sys.sleep(REQUEST_DELAY)
  }

  # Clean up
  all_species <- all_species |>
    distinct() |>
    arrange(common_name)

  if (verbose) {
    message(strrep("=", 60))
    message(sprintf("Complete! Total unique species: %d", nrow(all_species)))
    message(strrep("=", 60))
  }

  all_species
}

# ==============================================================================
# APPROACH 3: Single page load (all 350 species at once)
# ==============================================================================
#' Crawl all species in a single request
#'
#' Uses items_per_page=350 to load all species at once.
#' May timeout on slow connections.
#'
#' @param use_live Use read_html_live() instead of read_html()
#' @param verbose Print progress messages
#' @return tibble with species data
crawl_noaa_species_single <- function(use_live = FALSE, verbose = TRUE) {

  url_all <- paste0(
    DIRECTORY_URL,
    "?oq=&field_species_categories_vocab=All&field_region_vocab=All&items_per_page=350"
  )

  if (verbose) {
    message(strrep("=", 60))
    message("NOAA Species Directory - Single Page Load")
    message(strrep("=", 60))
    message("\nLoading all species (this may take a while)...")
  }

  if (use_live) {
    if (!requireNamespace("chromote", quietly = TRUE)) {
      stop("Package 'chromote' required for live mode")
    }
    html <- read_html_live(url_all)
    Sys.sleep(8)  # Extra wait for full render
  } else {
    html <- safe_read_html(url_all)
  }

  all_species <- extract_species(html) |>
    distinct() |>
    arrange(common_name)

  if (verbose) {
    message(sprintf("\nComplete! Total species: %d", nrow(all_species)))
  }

  all_species
}

# ==============================================================================
# Main execution (when run as script)
# ==============================================================================
if (sys.nframe() == 0) {

  cat("\n")
  cat("NOAA Fisheries Species Directory Crawler\n")
  cat("=========================================\n\n")
  cat("Choose method:\n")
  cat("  1. Standard HTTP with pagination (recommended)\n")
  cat("  2. Live browser with pagination (requires chromote + Chrome)\n")
  cat("  3. Single page load (faster but may timeout)\n\n")

  # Default: Use standard approach
  species_data <- crawl_noaa_species(verbose = TRUE)

  # Preview results
  cat("\n--- Data Preview ---\n")
  print(species_data, n = 10)

  # Save to CSV
  output_file <- here::here("data/noaa_species_directory.csv")
  write.csv(species_data, output_file, row.names = FALSE)
  cat(sprintf("\nSaved to: %s\n", output_file))

  # Summary
  cat("\n--- Summary ---\n")
  cat(sprintf("Total species: %d\n", nrow(species_data)))

  cat("\nBy primary category:\n")
  species_data |>
    mutate(primary_cat = str_extract(category, "^[^;]+")) |>
    count(primary_cat, sort = TRUE) |>
    head(10) |>
    print()

  cat("\nWith ESA protection:\n")
  species_data |>
    filter(str_detect(protected_status, "ESA", negate = FALSE)) |>
    nrow() |>
    (\(n) cat(sprintf("  %d species\n", n)))()
}
