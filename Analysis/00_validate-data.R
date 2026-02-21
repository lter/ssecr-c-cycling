# 00_validate-data.R
# Data validation script: checks that all ready data files match the column key
# Run this before harmonization to catch issues early.

library(dplyr)

source("R/utils_labels.R")
source("R/utils_dates.R")

cat("=== DATA VALIDATION REPORT ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

# Read column key and metadata
key <- read.csv("data/column_key.csv", stringsAsFactors = FALSE)
metadata <- get_site_metadata()

sources <- unique(key$source)
cat("Sources in column key:", length(sources), "\n")

# Track issues
issues <- list()
n_issues <- 0

for (src in sources) {
  filepath <- file.path("data", "ready", src)
  src_key <- key[key$source == src, ]
  site <- src_key$tidy_sit..name[1]

  # Check 1: File exists
  if (!file.exists(filepath)) {
    n_issues <- n_issues + 1
    issues[[length(issues) + 1]] <- paste0("[MISSING FILE] ", src)
    cat("  FAIL: File not found -", src, "\n")
    next
  }

  # Read file
  df <- tryCatch(
    read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      n_issues <<- n_issues + 1
      issues[[length(issues) + 1]] <<- paste0("[READ ERROR] ", src, ": ", e$message)
      NULL
    }
  )
  if (is.null(df)) next

  # Check 2: Required columns exist
  missing_cols <- setdiff(src_key$raw_name, names(df))
  if (length(missing_cols) > 0) {
    n_issues <- n_issues + 1
    issues[[length(issues) + 1]] <- paste0("[MISSING COLS] ", src, ": ", paste(missing_cols, collapse = ", "))
    cat("  FAIL: Missing columns in", src, "-", paste(missing_cols, collapse = ", "), "\n")
  }

  # Check 3: Date column parses
  date_col <- src_key$raw_name[src_key$tidy_name == "Date"]
  if (length(date_col) > 0 && date_col %in% names(df)) {
    dates_raw <- as.character(df[[date_col]])
    dates_parsed <- parse_dates(dates_raw)
    n_na <- sum(is.na(dates_parsed) & !is.na(dates_raw) & dates_raw != "NA")
    if (n_na > 0) {
      n_issues <- n_issues + 1
      bad_dates <- unique(dates_raw[is.na(dates_parsed) & !is.na(dates_raw) & dates_raw != "NA"])
      issues[[length(issues) + 1]] <- paste0("[DATE PARSE] ", src, ": ", n_na,
                                              " unparseable dates (e.g., ",
                                              paste(head(bad_dates, 3), collapse = ", "), ")")
    }
    date_range <- range(dates_parsed, na.rm = TRUE)
    cat(sprintf("  %-50s  %s  rows=%-6d  dates=%s to %s\n",
                src, site, nrow(df),
                format(date_range[1], "%Y"), format(date_range[2], "%Y")))
  } else {
    cat(sprintf("  %-50s  %s  rows=%-6d  [no date column found]\n", src, site, nrow(df)))
  }

  # Check 4: Response variable has data
  resp_col <- src_key$raw_name[src_key$tidy_name == "Response.Variable"]
  if (length(resp_col) > 0 && resp_col %in% names(df)) {
    n_na_resp <- sum(is.na(df[[resp_col]]))
    pct_na <- round(n_na_resp / nrow(df) * 100, 1)
    if (pct_na > 50) {
      n_issues <- n_issues + 1
      issues[[length(issues) + 1]] <- paste0("[HIGH NA] ", src, ": ", pct_na, "% NA in response variable")
    }
  }
}

# Check 5: Metadata coverage
cat("\n--- Metadata Coverage ---\n")
sources_without_metadata <- setdiff(sources, metadata$source)
if (length(sources_without_metadata) > 0) {
  cat("  Sources in key but not in metadata:", paste(sources_without_metadata, collapse = "\n    "), "\n")
  n_issues <- n_issues + length(sources_without_metadata)
}

# Check 6: Registry and manifest integrity
cat("\n--- Registry & Manifest Integrity ---\n")
registry_path <- "data/dataset_registry.csv"
manifest_dir <- "data/manifests"

if (file.exists(registry_path)) {
  registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  registry <- registry[registry$include == TRUE, ]
  cat("  Registry: ", nrow(registry), " included datasets\n")

  # Check that every ready/ file is traceable to a registry entry
  ready_files <- list.files("data/ready", pattern = "\\.(csv|CSV)$")
  registry_outputs <- unique(registry$ready_filename)
  orphaned <- setdiff(ready_files, registry_outputs)
  if (length(orphaned) > 0) {
    cat("  WARNING: Ready files not in registry:", paste(head(orphaned, 5), collapse = ", "), "\n")
  }

  # Check manifests
  if (dir.exists(manifest_dir)) {
    manifests <- list.files(manifest_dir, pattern = "\\.json$")
    cat("  Manifests found:", length(manifests), "\n")

    # Check every included dataset has a manifest
    for (i in seq_len(nrow(registry))) {
      did <- registry$dataset_id[i]
      mf <- file.path(manifest_dir, paste0(did, ".json"))
      if (!file.exists(mf)) {
        cat("    MISSING manifest:", did, "\n")
        n_issues <- n_issues + 1
        issues[[length(issues) + 1]] <- paste0("[NO MANIFEST] ", did)
      }
    }

    # Verify checksums for existing manifests
    source("R/pipeline/manifest.R")
    integrity <- check_all_manifests(manifest_dir, "data/raw")
    if (nrow(integrity) > 0) {
      bad <- integrity[!integrity$checksum_ok, ]
      if (nrow(bad) > 0) {
        cat("    CHECKSUM FAILURES:", paste(bad$dataset_id, collapse = ", "), "\n")
        n_issues <- n_issues + nrow(bad)
      } else {
        cat("    All manifest checksums OK\n")
      }
    }
  } else {
    cat("  No manifests directory found (run 00_download-and-preprocess.R first)\n")
  }
} else {
  cat("  No registry file found at", registry_path, "\n")
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Total sources checked:", length(sources), "\n")
cat("Total issues found:", n_issues, "\n")
if (length(issues) > 0) {
  cat("\nIssues:\n")
  for (issue in issues) cat("  -", issue, "\n")
}
cat("\nValidation complete.\n")
