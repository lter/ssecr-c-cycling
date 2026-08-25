# registry.R
# Functions for reading and querying the dataset registry

#' Read the dataset registry
#' @param path Path to dataset_registry.csv
#' @param include_only If TRUE (default), only return included datasets
#' @return data.frame
read_registry <- function(path = "data/dataset_registry.csv", include_only = TRUE) {
  reg <- read.csv(path, stringsAsFactors = FALSE)
  if (include_only) reg <- reg[reg$include == TRUE, ]
  reg
}

#' Get registry entry for a specific dataset
#' @param dataset_id The dataset_id to look up
#' @param registry The registry data.frame (or NULL to read from file)
#' @return Single-row data.frame
get_registry_entry <- function(dataset_id, registry = NULL) {
  if (is.null(registry)) registry <- read_registry()
  entry <- registry[registry$dataset_id == dataset_id, ]
  if (nrow(entry) == 0) stop("Dataset not found in registry: ", dataset_id)
  entry
}

#' Get all registry entries for a site
#' @param site_abbr Site abbreviation (e.g., "CDR")
#' @param registry The registry data.frame
#' @return data.frame of matching entries
get_site_entries <- function(site_abbr, registry = NULL) {
  if (is.null(registry)) registry <- read_registry()
  registry[registry$site_abbr == site_abbr, ]
}

#' Get the raw file path for a dataset
#' @param dataset_id The dataset_id
#' @param raw_dir Base raw directory
#' @return Path to the raw data file
get_raw_path <- function(dataset_id, raw_dir = "data/raw") {
  manifest_path <- file.path("data/manifests", paste0(dataset_id, ".json"))
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::fromJSON(manifest_path)
    return(file.path(raw_dir, dataset_id, manifest$raw_filename))
  }
  # Fallback: look for any file in the dataset directory
  dataset_dir <- file.path(raw_dir, dataset_id)
  if (dir.exists(dataset_dir)) {
    files <- list.files(dataset_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
    if (length(files) > 0) return(files[1])
  }
  stop("No raw file found for dataset: ", dataset_id)
}

#' List datasets that share a ready_filename (none currently; kept for generality)
#' @param ready_filename The output filename
#' @param registry The registry data.frame
#' @return data.frame of entries that produce this output
get_inputs_for_output <- function(ready_filename, registry = NULL) {
  if (is.null(registry)) registry <- read_registry()
  registry[registry$ready_filename == ready_filename, ]
}
