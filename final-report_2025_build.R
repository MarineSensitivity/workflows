# final-report_2025_build.R
# build script for BOEM 2025 Final Report (NT-23-03)
# produces: ESP-compliant DOCX, PDF, and Study Footprint package

librarian::shelf(
  officer, xml2, sf, openxlsx, glue, fs, dplyr, uuid,
  quiet = TRUE)

# paths ----
DIR_WF         <- "/Users/bbest/Github/MarineSensitivity/workflows"
DIR_SPECS      <- "~/My Drive/projects/msens/docs/2025 final report/specs"
DIR_OUT        <- file.path(DIR_WF, "_output")
TPL_FULL       <- file.path(DIR_WF, "libs/ESP Report Template 2025_1.docx")
TPL_TECH       <- file.path(DIR_WF, "libs/ESP Report Template 2025_1 tech.docx")
GPKG_PATH      <- "~/My Drive/projects/msens/data/derived/v1/ply_planareas_2025.gpkg"
COVER_IMG      <- file.path(DIR_WF, "../docs/figures/overview-methods.png")
XML_TPL        <- file.path(DIR_SPECS, "footprint/studyTitle-(1).xml")
DD_VALUES_PATH <- file.path(DIR_WF, "libs/MSTv1-2025-09_DataDictionary_values.xlsx")
QMD_FILE       <- file.path(DIR_WF, "final-report_2025.qmd")

# build-time flags ----
skip_zip           <- TRUE   # do NOT build NT-23-03_StudyFootprint.zip
                             #   (assembled in Google Drive after inspection)
redo_footprint_gdb <- FALSE  # if FALSE, reuse existing GDB at gdb_path;
                             #   set TRUE to force regeneration

dir_create(DIR_OUT)

# contract metadata ----
META <- list(
  study_no       = "NT-23-03",
  study_title    = "Marine Sensitivity Toolkit, v1",
  contract_no    = "140M0123P0018",
  pub_no         = "BOEM 2025-XXX",
  pub_date       = "April 2025",
  period_start   = "2023-09-25",
  period_end     = "2025-09-24",
  award          = "$265,000",
  # PI

  pi_name        = "Ben Best",
  pi_initials    = "Best BB",
  pi_title       = "Environmental Data Scientist",
  pi_org         = "EcoQuants LLC",
  pi_street      = "211 W Gutierrez St",
  pi_city        = "Santa Barbara",
  pi_state       = "CA",
  pi_zip         = "93101-3487",
  pi_country     = "USA",
  pi_email       = "ben@ecoquants.com",
  # BOEM COR
  cor_name       = "Stephanie Brock",
  cor_title      = "Contracting Officer's Representative",
  cor_org        = "Environmental Studies Program, Bureau of Ocean Energy Management",
  cor_street     = "45600 Woodland Road, VAE-AMD",
  cor_city       = "Sterling",
  cor_state      = "VA",
  cor_zip        = "20166-9216",
  cor_country    = "USA",
  cor_phone      = "703-787-1151",
  cor_email      = "stephanie.brock@boem.gov",
  # spatial extent (from GeoPackage bbox)
  bbox_w = "-180.0",
  bbox_e = "180.0",
  bbox_s = "-17.555",
  bbox_n = "82.477"
)

# abstract for technical summary & metadata
ABSTRACT <- paste(
  "The Bureau of Ocean Energy Management (BOEM) developed the Marine Sensitivity",
  "Toolkit (MST), a cloud-native system for assessing the relative environmental",
  "sensitivity of marine ecosystems to offshore energy development across U.S.",
  "waters. The MST integrates over 17,000 spatially explicit species distribution",
  "models, comprehensive extinction risk data, and satellite-based primary",
  "productivity to deliver a transparent, reproducible, and scalable assessment",
  "framework at 0.05-degree resolution across all 27 OCS Planning Areas.")

# keywords
KEYWORDS <- paste(
  "marine sensitivity, species distribution models, environmental sensitivity,",
  "OCS, BOEM, extinction risk, primary productivity, planning areas, ecoregions")

# =============================================================================
# PHASE 1: render body-only DOCX from QMD
# =============================================================================
cat("=== Phase 1: Rendering QMD to body-only DOCX ===\n")

setwd(DIR_WF)
system(paste(
  "quarto render", basename(QMD_FILE), "--to docx",
  "--metadata toc:false --metadata lof:false --metadata lot:false"))

body_docx <- file.path(DIR_OUT, "final-report_2025.docx")
stopifnot("Quarto render failed" = file_exists(body_docx))
body_out <- file.path(DIR_OUT, "final-report_2025_body.docx")
file_copy(body_docx, body_out, overwrite = TRUE)
cat("  Body DOCX:", body_out, "\n")

# 1b. fix caption styles in the body DOCX ----
# Quarto puts figure/table captions in table cells without proper Word styles.
# Word LOF/LOT needs "Figure Caption" / "Table Title" styles.
# We modify the raw document.xml using explicit namespace, then repackage.
cat("  Fixing caption styles in body DOCX...\n")

tmp_dir <- file.path(tempdir(), "body_fix")
if (dir_exists(tmp_dir)) dir_delete(tmp_dir)
dir_create(tmp_dir)
unzip(body_out, exdir = tmp_dir)

doc_xml_path <- file.path(tmp_dir, "word/document.xml")
raw_xml <- read_xml(doc_xml_path)
ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

# find caption paragraphs by their style (ImageCaption or TableCaption)
# and also by text pattern as fallback
all_p  <- xml_find_all(raw_xml, "//w:p", ns = ns)

# collect indices of caption paragraphs
cap_indices <- c()
for (i in seq_along(all_p)) {
  p <- all_p[[i]]
  # check style
  style_node <- xml_find_first(p, ".//w:pStyle", ns = ns)
  style_val  <- if (!is.na(style_node)) xml_attr(style_node, "val") else ""
  is_caption <- style_val %in% c(
    "ImageCaption", "FigureCaption", "Caption",
    "TableCaption", "TableTitle")

  # also check text content
  t_nodes <- xml_find_all(p, ".//w:t", ns = ns)
  txt <- paste(sapply(t_nodes, xml_text), collapse = "")
  is_fig_text <- grepl("^Figure [0-9]+[.:]", txt)
  is_tbl_text <- grepl("^Table [0-9]+[.:]", txt)

  if (is_caption || is_fig_text || is_tbl_text) {
    cap_indices <- c(cap_indices, i)
  }
}

fig_n <- 0; tbl_n <- 0
ns_uri <- ns[["w"]]

for (idx in cap_indices) {
  p_node <- all_p[[idx]]

  # get full text to determine figure vs table and extract caption body
  t_nodes <- xml_find_all(p_node, ".//w:t", ns = ns)
  txt <- paste(sapply(t_nodes, xml_text), collapse = "")
  is_fig <- grepl("Figure", txt)

  label    <- if (is_fig) "Figure" else "Table"
  style_id <- if (is_fig) "FigureCaption" else "TableTitle"
  if (is_fig) fig_n <- fig_n + 1 else tbl_n <- tbl_n + 1
  counter  <- if (is_fig) fig_n else tbl_n

  # extract caption text after "Figure N: " or "Table N. "
  # note: Quarto uses non-breaking space (U+00A0) between label and number
  caption_body <- sub(
    paste0("^", label, "[\\s\u00a0]+[0-9]+[.:]+\\s*"), "", txt, perl = TRUE)

  # escape XML special characters
  caption_body <- gsub("&", "&amp;", caption_body)
  caption_body <- gsub("<", "&lt;",  caption_body)
  caption_body <- gsub(">", "&gt;",  caption_body)

  # step 1: fix paragraph properties (style + remove duplicate pPr)
  all_ppr <- xml_find_all(p_node, "w:pPr", ns = ns)
  if (length(all_ppr) == 0) {
    ppr_new <- read_xml(glue(
      '<w:pPr xmlns:w="{ns_uri}"><w:pStyle w:val="{style_id}"/></w:pPr>'))
    xml_add_child(p_node, ppr_new, .where = 0)
  } else {
    if (length(all_ppr) > 1) {
      for (j in 2:length(all_ppr)) xml_remove(all_ppr[[j]])
    }
    ppr <- all_ppr[[1]]
    style_node <- xml_find_first(ppr, "w:pStyle", ns = ns)
    if (is.na(style_node)) {
      style_new <- read_xml(glue(
        '<w:pStyle xmlns:w="{ns_uri}" w:val="{style_id}"/>'))
      xml_add_child(ppr, style_new, .where = 0)
    } else {
      xml_set_attr(style_node, "val", style_id)
    }
  }

  # step 2: replace all runs/hyperlinks with SEQ field-based caption
  # Word needs: "Figure " + SEQ field + ". caption text"
  # First, collect ALL text including from hyperlinks
  all_txt_nodes <- xml_find_all(p_node, ".//w:t", ns = ns)
  full_txt <- paste(sapply(all_txt_nodes, xml_text), collapse = "")

  # strip hyperlink text + existing "Figure N:" / "Table N:" prefix
  # pattern handles optional leading text (e.g. "mapgl") before the label
  # and non-breaking space (U+00A0) that Quarto uses between label and number
  caption_body <- sub(
    paste0("^.*?", label, "[\\s\u00a0]+[0-9]+[.:]+\\s*"), "", full_txt, perl = TRUE)
  caption_body <- gsub("&", "&amp;", caption_body)
  caption_body <- gsub("<", "&lt;",  caption_body)
  caption_body <- gsub(">", "&gt;",  caption_body)

  # remove all runs, hyperlinks, and fldSimple from paragraph
  for (el in xml_find_all(p_node, "w:r", ns = ns))          xml_remove(el)
  for (el in xml_find_all(p_node, "w:hyperlink", ns = ns))  xml_remove(el)
  for (el in xml_find_all(p_node, "w:fldSimple", ns = ns))  xml_remove(el)

  # build replacement: label + SEQ field + ". " + caption text
  new_runs_xml <- glue(
    '<w:r xmlns:w="{ns_uri}">',
      '<w:t xml:space="preserve">{label} </w:t>',
    '</w:r>',
    '<w:fldSimple xmlns:w="{ns_uri}" w:instr=" SEQ {label} \\* ARABIC ">',
      '<w:r xmlns:w="{ns_uri}"><w:t>{counter}</w:t></w:r>',
    '</w:fldSimple>',
    '<w:r xmlns:w="{ns_uri}">',
      '<w:t xml:space="preserve">. {caption_body}</w:t>',
    '</w:r>')

  wrapper <- read_xml(paste0(
    '<w:temp xmlns:w="', ns_uri, '">', new_runs_xml, '</w:temp>'))
  for (child in xml_children(wrapper)) {
    xml_add_child(p_node, child)
  }
}

cat(glue("    Fixed {fig_n} figure captions, {tbl_n} table titles\n"))

# write modified XML back and repackage DOCX
write_xml(raw_xml, doc_xml_path)

# repackage using system zip (preserves DOCX format better than R zip)
old_wd <- getwd()
setwd(tmp_dir)
file_delete(body_out)
system(glue('zip -r -q "{body_out}" .'))
setwd(old_wd)
dir_delete(tmp_dir)
cat("  Saved fixed body DOCX\n")

# =============================================================================
# PHASE 2: merge body into full ESP template
# =============================================================================
cat("\n=== Phase 2: Merging body into ESP template ===\n")

doc <- read_docx(TPL_FULL)

# 2a. remove sample body content (elements 48-74) ----
# these are the template guidance sections that need to be replaced
# strategy: find and remove elements between "Heading 1 [Style Definitions"
# and "Appendix A: Title" (inclusive)
sample_keywords <- c(
  "Heading 1 \\[Style Definitions",
  "Do not modify the style",
  "This is the Normal style",
  "Heading 2 \\[Heading Styles",
  "Heading styles in this template",
  "Use Headings 1",
  "Heading 3 \\[Table Sample",
  "See a sample table in Table 1",
  "Data/text in all other cells",
  "Table 1\\. Sample table",
  "Note: Use the Table Notes",
  "Heading 4 \\[Figure Sample",
  "Figure 1 provides a sample",
  "Figure 1\\. Figure caption",
  "Use the Figure Notes style",
  "Heading 5 \\[Bulleted List",
  "List Bullet style is default",
  "Use the Numbered List style",
  "References",
  "\\[Insert end reference list",
  "Appendix A: Title",
  "\\[Use Heading 1 for appendix"
)

for (kw in sample_keywords) {
  tryCatch({
    doc <- cursor_reach(doc, keyword = kw)
    doc <- body_remove(doc)
  }, error = function(e) {
    # skip if keyword not found (may already be removed)
  })
}

# also remove sample table rows if they persist
# (the sample 5x5 table at elements 119-138 is part of the title page area, keep it)

cat("  Removed sample body content\n")

# 2b. insert body DOCX after abbreviations list ----
tryCatch({
  doc <- cursor_reach(doc, keyword = "Bureau of Ocean Energy Management")
  # move forward past the BOEM abbreviation entry
}, error = function(e) {
  # fallback: position at end
  doc <- cursor_end(doc)
})

doc <- body_add_docx(doc, src = body_out)
cat("  Inserted body content from Quarto\n")

# 2c. fill title page / front matter ----
# use body_replace_all_text with fixed = TRUE to handle bracket characters
rpl <- function(old, new) {
  body_replace_all_text(doc, old, new, fixed = TRUE, warn = FALSE)
}

# front cover
doc <- rpl("BOEM 20xx-xxx", META$pub_no)
doc <- rpl("Report Specifications and Template", META$study_title)

# title page table
doc <- rpl("[month year]", META$pub_date)
doc <- rpl("[organization(s) and address(es)]",
  paste(META$pi_org, META$pi_street,
    paste0(META$pi_city, ", ", META$pi_state, " ", META$pi_zip), sep = "\n"))
doc <- rpl("City, ST [of the BOEM Office or Region publishing]",
  paste0(META$cor_city, ", ", META$cor_state))

# citation
doc <- rpl(
  "Lastname FM, Lastname FM, Lastname FM [include up to 10 authors, then use et al.]",
  META$pi_initials)
doc <- rpl("[Format is CSE 8th Edition. ", "")
doc <- rpl("After title, use the City (ST) of the BOEM region or office.", "")
doc <- rpl("Number of pages is the total number of pages in the file.]", "")

# disclaimer: fill [INSERT NUMBER] and [INSERT ORGANIZATION] placeholders
doc <- rpl("INSERT NUMBER",       META$contract_no)
doc <- rpl("INSERT ORGANIZATION", META$pi_org)
# clear instruction + unused variants
doc <- rpl(
  "[Select and fill out the appropriate disclaimer statement. Remove unused text, but do not change the remaining text.]",
  "")
doc <- rpl("[Contracts] ", "")
doc <- rpl("[Cooperative Agreements] ", "")

# publisher info
doc <- rpl(
  "[additional publisher organization(s), location(s), or partner logo(s)]", "")
doc <- rpl("[contractor logos not permitted]", "")

# cover photo credit
doc <- rpl(
  "[optional photo information or credit and/or relevant research permit number]",
  "Environmental Sensitivity Score Methodology overview.")

# contributors
doc <- rpl(
  "[list in this table any other significant contributors who are not authors]",
  "No additional contributors beyond the author.")

# acknowledgments
doc <- rpl(
  "[Include any recognition of assistance, advice, support, and/or services.]",
  paste("This work was supported by the Bureau of Ocean Energy Management",
    "Environmental Studies Program under Contract", META$contract_no, ".",
    "The author thanks", META$cor_name, "(BOEM COR) and Timothy White",
    "(BOEM) for guidance and technical review throughout the project."))

# technical summary placeholders
doc <- rpl(
  "[Insert text using plain language appropriate for a non-scientific audience]",
  ABSTRACT)
doc <- rpl("[text]",
  "See main body of the report for details.")
doc <- rpl(
  "[list of published or in press at the time of report submission]",
  "No peer-reviewed publications at the time of report submission.")
doc <- rpl(
  "[include here if appropriate and if not already shown in the report]",
  "Study area maps are presented in the main body of the report.")

# insert cover image placeholder text
doc <- rpl("[insert optional image]", "")

cat("  Filled title page and technical summary\n")

# 2e. add abbreviations ----
abbreviations <- c(
  "API"   = "Application Programming Interface",
  "BOEM"  = "Bureau of Ocean Energy Management",
  "COG"   = "Cloud-Optimized GeoTIFF",
  "COR"   = "Contracting Officer's Representative",
  "DAG"   = "Directed Acyclic Graph",
  "ESA"   = "Endangered Species Act",
  "ESP"   = "Environmental Studies Program",
  "ESPIS" = "Environmental Studies Program Information System",
  "FAIR"  = "Findable, Accessible, Interoperable, Reusable",
  "FWS"   = "U.S. Fish and Wildlife Service",
  "GBIF"  = "Global Biodiversity Information Facility",
  "IUCN"  = "International Union for Conservation of Nature",
  "MST"   = "Marine Sensitivity Toolkit",
  "NMFS"  = "National Marine Fisheries Service",
  "NPP"   = "Net Primary Productivity",
  "OBIS"  = "Ocean Biodiversity Information System",
  "OCS"   = "Outer Continental Shelf",
  "OCSLA" = "Outer Continental Shelf Lands Act",
  "RESA"  = "Relative Environmental Sensitivity Analysis",
  "STAC"  = "SpatioTemporal Asset Catalog",
  "VGPM"  = "Vertically Generalized Production Model",
  "WoRMS" = "World Register of Marine Species"
)

# add after existing BOEM abbreviation
tryCatch({
  doc <- cursor_reach(doc, keyword = "Bureau of Ocean Energy Management")
  for (i in seq_along(abbreviations)) {
    abbr <- names(abbreviations)[i]
    defn <- abbreviations[[i]]
    if (abbr == "BOEM") next  # already in template
    doc <- body_add_par(doc, glue("{abbr} \t{defn}"),
      style = "Acronym List", pos = "after")
  }
}, error = function(e) {
  cat("  Warning: could not add abbreviations\n")
})
cat("  Added abbreviations list\n")

# 2f. insert cover image ----
if (file_exists(COVER_IMG)) {
  tryCatch({
    doc <- cursor_reach(doc, keyword = "\\[insert optional image\\]")
    doc <- body_remove(doc)
    doc <- body_add_img(doc, src = COVER_IMG,
      width = 6.5, height = 3.5,
      style = "Normal", pos = "before")
  }, error = function(e) {
    cat("  Warning: could not insert cover image\n")
  })
}
cat("  Inserted cover image\n")

# 2g. save final DOCX ----
out_docx <- file.path(DIR_OUT, "BOEM_2025-XXX_FinRpt.docx")
print(doc, target = out_docx)
cat("  Saved:", out_docx, "\n")

# =============================================================================
# PHASE 3: PDF conversion
# =============================================================================
cat("\n=== Phase 3: PDF Conversion ===\n")

out_pdf <- file.path(DIR_OUT, "BOEM_2025-XXX_FinRpt.pdf")

# try Microsoft Word via AppleScript
applescript <- glue('
  tell application "Microsoft Word"
    activate
    open POSIX file "{normalizePath(out_docx)}"
    delay 3
    set theDoc to active document
    save as theDoc file name POSIX file "{normalizePath(out_pdf, mustWork = FALSE)}" file format format PDF
    close theDoc saving no
  end tell
')

result <- tryCatch(
  system(paste0('osascript -e \'', applescript, '\''), intern = TRUE),
  error = function(e) {
    cat("  Word AppleScript failed:", e$message, "\n")
    cat("  Trying LibreOffice fallback...\n")
    system(glue(
      'soffice --headless --convert-to pdf --outdir "{DIR_OUT}" "{out_docx}"'))
  })

if (file_exists(out_pdf)) {
  cat("  Saved:", out_pdf, "\n")
} else {
  cat("  WARNING: PDF not created. Open DOCX in Word and save as PDF manually.\n")
}

# =============================================================================
# PHASE 4: ESP Study Footprint package
# =============================================================================
cat("\n=== Phase 4: ESP Study Footprint ===\n")

# 4a. convert GeoPackage to FileGDB ----
gdb_path <- file.path(DIR_OUT, glue("{META$study_no}_StudyFootprint.gdb"))

if (redo_footprint_gdb || !dir_exists(gdb_path)) {
  pa <- st_read(GPKG_PATH, quiet = TRUE)
  cat("  Read GeoPackage:", nrow(pa), "features\n")

  # dissolve to single study area polygon
  study_area <- pa |>
    st_union() |>
    st_sf(geometry = _) |>
    mutate(
      FootPrintID   = 1L,
      Region        = "National",
      StudiesID     = META$study_no,
      DateCompleted = META$period_end,
      Method        = "GIS-ready boundary") |>
    select(FootPrintID, Region, StudiesID, DateCompleted, Method, geometry)

  if (dir_exists(gdb_path)) dir_delete(gdb_path)
  st_write(study_area, dsn = gdb_path, layer = "StudyFootprint",
    driver = "OpenFileGDB", quiet = TRUE)
  cat("  Created FileGDB:", gdb_path, "\n")
} else {
  cat("  Reusing existing FileGDB:", gdb_path,
      "\n    (set redo_footprint_gdb <- TRUE to regenerate)\n")
}

# 4b. read authoritative data dictionary values ----
# the user-curated workbook at DD_VALUES_PATH is the source of truth for all
# field values used in both the output data dictionary and the metadata XML.
stopifnot(
  "DD values file not found" = file_exists(DD_VALUES_PATH))

dd_sheet <- read.xlsx(
  DD_VALUES_PATH, sheet = "Data Dictionary Template",
  colNames = FALSE, skipEmptyRows = FALSE)

# strip trailing "--See ... tab--" annotations so keys are plain field names
clean_key <- function(x) sub("\\s*--.*$", "", trimws(x))

dd_values <- list()
for (i in seq_len(nrow(dd_sheet))) {
  key <- clean_key(dd_sheet[i, 1])
  val <- dd_sheet[i, 2]
  if (is.na(key) || nchar(key) == 0) next
  if (is.na(val) || nchar(as.character(val)) == 0) next
  # skip section header rows (col A is a sentence, not a camelCase fieldname)
  if (grepl("\\s", key)) next
  dd_values[[key]] <- as.character(val)
}
cat("  Loaded", length(dd_values), "values from",
    basename(DD_VALUES_PATH), "\n")

# auto-generate the output data dictionary by copying the curated source
dd_out <- file.path(DIR_OUT, glue("{META$study_no}_DataDictionary.xlsx"))
file_copy(DD_VALUES_PATH, dd_out, overwrite = TRUE)
cat("  Saved data dictionary:", dd_out, "\n")

# 4c. fill metadata XML ----
# the XML template has placeholder text equal to camelCase field names; we
# replace them with values from dd_values. xml_ns_strip() does NOT actually
# rewrite element names from <gco:CharacterString> to <CharacterString>, so
# we use local-name() predicates to match across the gco/gmd namespaces.
xml <- read_xml(XML_TPL)

# DD field name -> XML placeholder name (only for the few mismatches)
dd_to_xml <- c(
  metadataPOCContactName         = "metadataPOCName",
  studyFootprintMethodDefinition = "studyFootprintMethodDescription",
  DataContentType                = "dataContentType",
  spatiaExtentNorth              = "spatialExtentNorth")

# build (xml_placeholder -> value) lookup, applying any DD->XML aliases
xml_replacements <- list()
for (dd_key in names(dd_values)) {
  xml_key <- if (dd_key %in% names(dd_to_xml)) dd_to_xml[[dd_key]] else dd_key
  xml_replacements[[xml_key]] <- dd_values[[dd_key]]
}
# the XML PIName placeholder takes the PIPrimaryContactName value
if (!"PIName" %in% names(xml_replacements) &&
    !is.null(dd_values$PIPrimaryContactName)) {
  xml_replacements[["PIName"]] <- dd_values$PIPrimaryContactName
}

# replace placeholders using local-name() (works across gco:/gmd: namespaces)
n_replaced <- 0
for (placeholder in names(xml_replacements)) {
  value <- xml_replacements[[placeholder]]
  xpath <- glue(
    ".//*[(local-name()='CharacterString' or local-name()='Date' or ",
    "local-name()='DateTime' or local-name()='Decimal' or ",
    "local-name()='description' or local-name()='beginPosition' or ",
    "local-name()='endPosition') and text()='{placeholder}']")
  nodes <- xml_find_all(xml, xpath)
  for (node in nodes) {
    xml_set_text(node, value)
    n_replaced <- n_replaced + 1
  }
}
cat("  Replaced", n_replaced, "XML placeholders\n")

# also replace the obligation/contract number pattern
credit_nodes <- xml_find_all(xml,
  ".//*[local-name()='CharacterString' and contains(text(), 'MXXYYXXXXX')]")
for (node in credit_nodes) {
  txt <- xml_text(node)
  xml_set_text(node, gsub("MXXYYXXXXX", META$contract_no, txt))
}

xml_out <- file.path(DIR_OUT, glue("{META$study_no}_metadata.xml"))
write_xml(xml, xml_out)
cat("  Saved metadata XML:", xml_out, "\n")

# 4d. package as ZIP ----
zip_file <- file.path(DIR_OUT, glue("{META$study_no}_StudyFootprint.zip"))

if (skip_zip) {
  cat("  Skipping StudyFootprint zip",
      "(set skip_zip <- FALSE to build it).\n")
  if (file_exists(zip_file)) file_delete(zip_file)
} else {
  if (file_exists(zip_file)) file_delete(zip_file)

  # get relative paths for zip
  old_wd <- getwd()
  setwd(DIR_OUT)

  # for GDB (directory), need to include all files within
  gdb_files  <- dir_ls(basename(gdb_path), recurse = TRUE)
  zip_inputs <- c(gdb_files, basename(dd_out), basename(xml_out))

  zip(basename(zip_file), zip_inputs)
  setwd(old_wd)

  cat("  Saved footprint package:", zip_file, "\n")
}

# =============================================================================
# PHASE 5: QA checklist summary
# =============================================================================
cat("\n=== Phase 5: QA Checklist ===\n")
cat("
=== ESP Report Checklist QA ===
Open the DOCX in Word and verify:
  [ ] 1.  General: Uses 2025 ESP template styles
  [ ] 2.  File Format: DOCX + PDF, no password, 8.5x11, 1-inch margins
  [ ] 3.  Section 508: Run Word accessibility checker (Review > Check Accessibility)
  [ ] 4.  Page Numbers: Per template
  [ ] 5.  Copyrighted Material: All figures are project-generated
  [ ] 6.  Front Cover: No additions, date = publication month/year
  [ ] 7.  Back Cover: No alterations
  [ ] 8.  Title Page: All fields completed
  [ ] 9.  Technical Summary: 1.5-3 pages, all fields filled
  [ ] 10. Frontmatter: Update fields (Ctrl+A, F9) for TOC/LOF/LOT
  [ ] 11. References: Author-Year, CSE 8th ed
  [ ] 12. Appendices: N/A or numbered
  [ ] 13. Volumes: Single volume
  [ ] 14. Numbers: Commas, leading zeros
  [ ] 15. Equations: Word equation editor or image with alt text
  [ ] 16. Abbreviations: Spelled out first use + list
  [ ] 17. Tables/Figures: Referenced in text, close to reference
  [ ] 18. Table Titles: Above table, same page, Table Title style
  [ ] 19. Table Notes: Table Notes style
  [ ] 20. Figure Captions: Below figure, same page, Figure Caption style
  [ ] 21. Figure Notes: Figure Notes style
  [ ] 22. Tables: Real tables, no merge/split/empty, repeat headers
  [ ] 23. Figures: Single image, alt text, border, inline wrap
  [ ] 24. Hyperlinks: Active, descriptive text

IMPORTANT: After opening in Word, press Ctrl+A then F9 to update all fields
(TOC, LOF, LOT, cross-references, page numbers).
")

cat("\n=== Build complete ===\n")
cat("Output files:\n")
cat("  Report DOCX:    ", out_docx, "\n")
cat("  Report PDF:     ", out_pdf, "\n")
cat("  Data Dictionary:", dd_out, "\n")
cat("  Metadata XML:   ", xml_out, "\n")
cat("  Footprint GDB:  ", gdb_path, "\n")
if (skip_zip) {
  cat("  Footprint ZIP:   skipped (skip_zip <- TRUE)\n")
} else {
  cat("  Footprint ZIP:  ", zip_file, "\n")
}
