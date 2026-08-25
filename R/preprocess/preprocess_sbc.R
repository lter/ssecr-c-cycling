# preprocess_sbc.R
# Source: knb-lter-sbc.119
# SBC LTER: Reef: Long-term experiment: biomass of kelp forest species
# Treatment: CONTROL, ANNUAL (annual kelp removal), CONTINUAL (continual kelp removal)
# NOTE: experiment_type should be "Vegetation Removal" not "Fertilization"

library(dplyr)

#' Preprocess SBC algal biomass data from EDI
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with summary statistics per Year/Site/Treatment
preprocess_sbc_algal_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: YEAR, MONTH, SITE, TRANSECT, TREATMENT, SP_CODE,
  # PERCENT_COVER, DENSITY, WM_GM2, DRY_GM2, SFDM, AFDM, VIS, etc.
  # Need to aggregate across species and transects

  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R

  # Select relevant columns and aggregate
  numeric_cols <- c("VIS", "PERCENT_COVER", "DENSITY", "WM_GM2", "DRY_GM2", "SFDM", "AFDM")
  available_cols <- numeric_cols[numeric_cols %in% names(data)]

  result <- data %>%
    select(YEAR, MONTH, SITE, TRANSECT, TREATMENT, all_of(available_cols)) %>%
    summarize_by_columns(c("YEAR", "MONTH", "SITE", "TRANSECT", "TREATMENT"))

  as.data.frame(result)
}
