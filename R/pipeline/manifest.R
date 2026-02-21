# manifest.R
# Functions for reading, writing, and checking download manifest files
# Manifests track provenance: what was downloaded, when, from where, with what checksum

library(jsonlite)
library(digest)

#' Write a download manifest for a dataset
#' @param manifest_dir Directory to write manifests (e.g., "data/manifests")
#' @param dataset_id Unique dataset identifier from registry
#' @param edi_package_id EDI package ID (e.g., "knb-lter-cdr.302.13"), or NA
#' @param download_url The URL data was downloaded from
#' @param raw_file_path Path to the downloaded raw file
#' @return Path to the written manifest file (invisibly)
write_manifest <- function(manifest_dir, dataset_id, edi_package_id = NA,
                           download_url = NA, raw_file_path) {
  if (!dir.exists(manifest_dir)) dir.create(manifest_dir, recursive = TRUE)

  manifest <- list(
    dataset_id = dataset_id,
    edi_package_id = ifelse(is.na(edi_package_id), NULL, edi_package_id),
    download_url = ifelse(is.na(download_url), NULL, download_url),
    download_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    checksum_md5 = digest(file = raw_file_path, algo = "md5"),
    file_size_bytes = file.info(raw_file_path)$size,
    raw_filename = basename(raw_file_path)
  )

  manifest_path <- file.path(manifest_dir, paste0(dataset_id, ".json"))
  write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  cat("  Manifest written:", basename(manifest_path), "\n")
  invisible(manifest_path)
}

#' Read a manifest file
#' @param manifest_path Path to the JSON manifest
#' @return Named list with manifest fields
read_manifest <- function(manifest_path) {
  if (!file.exists(manifest_path)) {
    warning("Manifest not found: ", manifest_path)
    return(NULL)
  }
  fromJSON(manifest_path)
}

#' Check manifest integrity: does the raw file still match the recorded checksum?
#' @param manifest_path Path to the JSON manifest
#' @param raw_dir Directory containing raw data files
#' @return TRUE if checksum matches, FALSE otherwise
check_manifest <- function(manifest_path, raw_dir) {
  manifest <- read_manifest(manifest_path)
  if (is.null(manifest)) return(FALSE)

  raw_file <- file.path(raw_dir, manifest$dataset_id, manifest$raw_filename)
  if (!file.exists(raw_file)) {
    cat("  Raw file missing:", raw_file, "\n")
    return(FALSE)
  }

  current_checksum <- digest(file = raw_file, algo = "md5")
  matches <- identical(current_checksum, manifest$checksum_md5)
  if (!matches) {
    cat("  Checksum mismatch for", manifest$dataset_id, "\n")
    cat("    Expected:", manifest$checksum_md5, "\n")
    cat("    Got:     ", current_checksum, "\n")
  }
  matches
}

#' Check all manifests in a directory
#' @param manifest_dir Directory containing manifest JSON files
#' @param raw_dir Directory containing raw data
#' @return data.frame with dataset_id, has_manifest, has_raw, checksum_ok
check_all_manifests <- function(manifest_dir = "data/manifests", raw_dir = "data/raw") {
  manifest_files <- list.files(manifest_dir, pattern = "\\.json$", full.names = TRUE)

  if (length(manifest_files) == 0) {
    cat("No manifests found in", manifest_dir, "\n")
    return(data.frame())
  }

  results <- lapply(manifest_files, function(mf) {
    m <- read_manifest(mf)
    raw_file <- file.path(raw_dir, m$dataset_id, m$raw_filename)
    data.frame(
      dataset_id = m$dataset_id,
      has_manifest = TRUE,
      has_raw = file.exists(raw_file),
      checksum_ok = check_manifest(mf, raw_dir),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
