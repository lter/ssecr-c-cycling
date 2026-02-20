# utils_labels.R
# Site metadata lookup and label formatting utilities

CASE_STUDY_SITES <- c("KNZ", "HBR", "KBS", "SBC", "GCE", "HFR", "BNZ", "CAP")

#' Read the site metadata lookup table
#' @param data_dir Path to the data directory containing site_metadata.csv
#' @return data.frame with site metadata
get_site_metadata <- function(data_dir = "data") {
  read.csv(file.path(data_dir, "site_metadata.csv"), stringsAsFactors = FALSE)
}

#' Map source filenames to site abbreviations
#' @param source_names Character vector of source filenames
#' @param metadata Optional pre-loaded metadata data.frame
#' @return Character vector of site abbreviations
source_to_abbr <- function(source_names, metadata = NULL) {
  if (is.null(metadata)) metadata <- get_site_metadata()
  lookup <- setNames(metadata$site_abbr, metadata$source)
  result <- lookup[source_names]
  # Fallback: extract prefix before first underscore
  missing <- is.na(result)
  if (any(missing)) {
    result[missing] <- sub("_.*", "", source_names[missing])
  }
  unname(result)
}

#' Map source filenames to descriptive experiment labels
#' Combines site abbreviation with experiment type for readable labels
#' @param source_names Character vector of source filenames
#' @param metadata Optional pre-loaded metadata data.frame
#' @return Character vector of labels like "KNZ (Fertilization)"
source_to_label <- function(source_names, metadata = NULL) {
  if (is.null(metadata)) metadata <- get_site_metadata()
  lookup_abbr <- setNames(metadata$site_abbr, metadata$source)
  lookup_exp <- setNames(metadata$experiment_type, metadata$source)
  abbrs <- lookup_abbr[source_names]
  exps <- lookup_exp[source_names]
  # Handle CDR duplicates by adding response type
  lookup_flux <- setNames(metadata$stock_or_flux, metadata$source)
  fluxes <- lookup_flux[source_names]
  dupes <- duplicated(paste(abbrs, exps)) | duplicated(paste(abbrs, exps), fromLast = TRUE)
  labels <- ifelse(dupes, paste0(abbrs, " (", exps, ", ", fluxes, ")"),
                   paste0(abbrs, " (", exps, ")"))
  labels[is.na(abbrs)] <- source_names[is.na(abbrs)]
  unname(labels)
}

#' Check that all case-study sites are present in the data
#' @param data data.frame with a 'source' column
#' @param metadata Optional pre-loaded metadata data.frame
check_case_studies <- function(data, metadata = NULL) {
  if (is.null(metadata)) metadata <- get_site_metadata()
  data_sites <- unique(source_to_abbr(data$source, metadata))
  missing <- setdiff(CASE_STUDY_SITES, data_sites)
  if (length(missing) > 0) {
    warning(paste("Missing case-study sites:", paste(missing, collapse = ", ")))
  } else {
    message("All 8 case-study sites present.")
  }
  invisible(missing)
}
