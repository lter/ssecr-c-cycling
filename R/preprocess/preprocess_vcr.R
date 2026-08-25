# preprocess_vcr.R
# Sources:
#   knb-lter-vcr.169.24 (2nd UPC Inundation Experiment 1998-2010)
#   knb-lter-vcr.168.24 (1st UPC Inundation Experiment 1994-2014)
# Treatment: locationName identifies treatment
#   2nd exp: (B)Border Control, (C)Control Plot, (P)Pumped Plot
#   1st exp: Inside Juncus, Outside Juncus

# NOTE: skip = 22 in both readers matches the EDI CSV's metadata header block
# for revision 24. If the packages are updated, verify the header offset —
# a different header depth would silently misparse the files.

library(dplyr)

# Aggregation uses summarize_by_columns() from R/preprocess/preprocess_utils.R

#' Preprocess VCR 2nd inundation experiment (1998-2010)
#' @param raw_path Path to raw CSV from EDI (knb-lter-vcr.169.24)
#' @return data.frame with summary statistics per Year/Location/Replicate
preprocess_vcr_inundation_2nd <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE, skip = 22)

  # Raw EDI (after skipping 22 header rows) has:
  # EOYBYear, locationName, replicate, marshRegion, isExperiment,
  # transect, locationID, ITISID, liveMass, deadMass, unknownMass, totalMass, etc.

  result <- summarize_by_columns(data, c("EOYBYear", "locationName", "replicate"))

  as.data.frame(result)
}

#' Preprocess VCR 1st inundation experiment (1994-2014)
#' @param raw_path Path to raw CSV from EDI (knb-lter-vcr.168.24)
#' @return data.frame with summary statistics per Year/Location/Replicate
preprocess_vcr_inundation_1st <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE, skip = 22)

  # Same structure as 2nd experiment but with capital R in Replicate
  result <- summarize_by_columns(data, c("EOYBYear", "locationName", "Replicate"))

  as.data.frame(result)
}
