# download.R
# Functions for downloading datasets from EDI and other sources
# Uses EDIutils for EDI repositories, httr/download.file for direct URLs

library(EDIutils)
library(httr)

source("R/pipeline/manifest.R")

# =============================================================================
# EDI DOWNLOAD
# =============================================================================

#' Parse an EDI package ID into its components
#' @param package_id e.g., "knb-lter-cdr.302.13"
#' @return Named list: scope, identifier, revision
parse_package_id <- function(package_id) {
  parts <- strsplit(package_id, "\\.")[[1]]
  if (length(parts) != 3) stop("Invalid package_id format: ", package_id)
  list(
    scope = parts[1],
    identifier = as.integer(parts[2]),
    revision = as.integer(parts[3])
  )
}

#' Discover available data entity IDs in an EDI package
#' @param package_id e.g., "knb-lter-cdr.302.13"
#' @return Character vector of entity IDs
discover_entity_ids <- function(package_id) {
  p <- parse_package_id(package_id)
  entity_names <- read_data_entity_names(
    packageId = package_id
  )
  entity_names
}

#' Download a dataset from EDI
#' @param package_id EDI package ID (e.g., "knb-lter-cdr.302.13")
#' @param entity_id Specific data entity ID (if known); NULL to auto-discover
#' @param dataset_id Unique dataset identifier for this project
#' @param dest_dir Base directory for raw downloads (e.g., "data/raw")
#' @param manifest_dir Directory for manifests (e.g., "data/manifests")
#' @param force Re-download even if cached
#' @return Path to the downloaded file
download_from_edi <- function(package_id, entity_id = NULL, dataset_id,
                              dest_dir = "data/raw", manifest_dir = "data/manifests",
                              force = FALSE) {

  # Create dataset-specific subdirectory
  dataset_dir <- file.path(dest_dir, dataset_id)
  if (!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE)

  # Check if already downloaded (manifest exists and checksum matches)
  manifest_path <- file.path(manifest_dir, paste0(dataset_id, ".json"))
  if (!force && file.exists(manifest_path)) {
    manifest <- read_manifest(manifest_path)
    raw_file <- file.path(dataset_dir, manifest$raw_filename)
    if (file.exists(raw_file) && check_manifest(manifest_path, dest_dir)) {
      cat("  Already downloaded (checksum OK), skipping\n")
      return(raw_file)
    }
  }

  # Discover entity ID if not provided
  if (is.null(entity_id) || is.na(entity_id) || entity_id == "") {
    cat("  Discovering entity IDs for", package_id, "...\n")
    entities <- discover_entity_ids(package_id)
    if (length(entities) == 0) stop("No data entities found in ", package_id)
    # If multiple entities, use the first one and warn
    if (nrow(entities) > 1) {
      cat("  Found", nrow(entities), "entities:\n")
      for (i in seq_len(nrow(entities))) {
        cat("    ", i, ":", entities$entityName[i], "\n")
      }
      cat("  Using first entity. Set entity_id in registry for a specific one.\n")
    }
    entity_id <- entities$entityId[1]
  }

  # Build PASTA download URL
  p <- parse_package_id(package_id)
  download_url <- sprintf(
    "https://pasta.lternet.edu/package/data/eml/%s/%s/%s/%s",
    p$scope, p$identifier, p$revision, entity_id
  )

  # Download
  dest_file <- file.path(dataset_dir, "data.csv")
  cat("  Downloading from EDI:", package_id, "...\n")

  response <- GET(download_url, write_disk(dest_file, overwrite = TRUE))
  if (http_error(response)) {
    stop("Download failed for ", package_id, ": HTTP ", status_code(response))
  }

  cat("  Downloaded", round(file.info(dest_file)$size / 1024, 1), "KB\n")

  # Write manifest
  write_manifest(
    manifest_dir = manifest_dir,
    dataset_id = dataset_id,
    edi_package_id = package_id,
    download_url = download_url,
    raw_file_path = dest_file
  )

  dest_file
}

# =============================================================================
# DIRECT URL DOWNLOAD (for non-EDI sources)
# =============================================================================

#' Download a dataset from a direct URL
#' @param url Download URL
#' @param dataset_id Unique dataset identifier
#' @param filename Output filename (default: "data.csv")
#' @param dest_dir Base directory for raw downloads
#' @param manifest_dir Directory for manifests
#' @param force Re-download even if cached
#' @return Path to the downloaded file
download_from_url <- function(url, dataset_id, filename = "data.csv",
                              dest_dir = "data/raw", manifest_dir = "data/manifests",
                              force = FALSE) {

  dataset_dir <- file.path(dest_dir, dataset_id)
  if (!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE)

  # Check cache
  manifest_path <- file.path(manifest_dir, paste0(dataset_id, ".json"))
  if (!force && file.exists(manifest_path)) {
    manifest <- read_manifest(manifest_path)
    raw_file <- file.path(dataset_dir, manifest$raw_filename)
    if (file.exists(raw_file) && check_manifest(manifest_path, dest_dir)) {
      cat("  Already downloaded (checksum OK), skipping\n")
      return(raw_file)
    }
  }

  dest_file <- file.path(dataset_dir, filename)
  cat("  Downloading from URL:", url, "...\n")

  download.file(url, dest_file, mode = "wb", quiet = TRUE)

  if (!file.exists(dest_file) || file.info(dest_file)$size == 0) {
    stop("Download failed or empty file for ", dataset_id)
  }

  cat("  Downloaded", round(file.info(dest_file)$size / 1024, 1), "KB\n")

  write_manifest(
    manifest_dir = manifest_dir,
    dataset_id = dataset_id,
    download_url = url,
    raw_file_path = dest_file
  )

  dest_file
}

# =============================================================================
# DOWNLOAD ALL
# =============================================================================

#' Download all datasets listed in the registry
#' @param registry_path Path to dataset_registry.csv
#' @param raw_dir Base directory for raw downloads
#' @param manifest_dir Directory for manifests
#' @param datasets Optional character vector of dataset_ids to download (NULL = all)
#' @param force Re-download even if cached
download_all <- function(registry_path = "data/dataset_registry.csv",
                         raw_dir = "data/raw",
                         manifest_dir = "data/manifests",
                         datasets = NULL,
                         force = FALSE) {

  registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  registry <- registry[registry$include == TRUE, ]

  if (!is.null(datasets)) {
    registry <- registry[registry$dataset_id %in% datasets, ]
  }

  cat("=== DOWNLOADING", nrow(registry), "DATASETS ===\n\n")

  results <- data.frame(
    dataset_id = character(), status = character(), stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, ]
    cat(sprintf("[%d/%d] %s (%s)\n", i, nrow(registry), row$dataset_id, row$site_abbr))

    status <- tryCatch({
      if (row$source_type == "edi") {
        download_from_edi(
          package_id = row$edi_package_id,
          entity_id = if (row$edi_entity_id == "") NULL else row$edi_entity_id,
          dataset_id = row$dataset_id,
          dest_dir = raw_dir,
          manifest_dir = manifest_dir,
          force = force
        )
      } else {
        download_from_url(
          url = row$download_url,
          dataset_id = row$dataset_id,
          dest_dir = raw_dir,
          manifest_dir = manifest_dir,
          force = force
        )
      }
      "OK"
    }, error = function(e) {
      cat("  FAILED:", conditionMessage(e), "\n")
      paste("FAILED:", conditionMessage(e))
    })

    results <- rbind(results, data.frame(
      dataset_id = row$dataset_id, status = status, stringsAsFactors = FALSE
    ))
    cat("\n")
  }

  # Summary
  n_ok <- sum(results$status == "OK")
  n_fail <- nrow(results) - n_ok
  cat("=== DOWNLOAD COMPLETE ===\n")
  cat("  Success:", n_ok, "/", nrow(results), "\n")
  if (n_fail > 0) {
    cat("  Failed:\n")
    failed <- results[results$status != "OK", ]
    for (j in seq_len(nrow(failed))) {
      cat("    ", failed$dataset_id[j], ":", failed$status[j], "\n")
    }
  }

  invisible(results)
}
