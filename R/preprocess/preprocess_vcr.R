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
  # Upper Phillips Creek inundation x wrack experiment (Tolley & Christian 1999):
  # nine 4 x 3 m plots in three blocks; within every plot, Spartina alterniflora
  # wrack was laid over half of the area in April 1994. The package does not
  # say which plots were flooded, but it does record the wrack contrast:
  # Transect "W" = wrack half, "V" = vegetated (no-wrack) half, sampled 1994-1998.
  # ("Inside/Outside Juncus", used by the 2025 hand-processed file as the
  # treatment, is the vegetation zone sampled, not a manipulation.)
  #
  # Response: live aboveground biomass, g m-2. Species rows of a 0.0625 m2
  # quadrat are summed (x16), then quadrats are averaged per plot x half x year.
  data <- read.csv(raw_path, stringsAsFactors = FALSE, comment.char = "#")
  data %>%
    filter(Transect %in% c("V", "W"),
           !speciesName %in% c("Wrack", "Combined Dead")) %>%
    mutate(Treatment = ifelse(Transect == "W", "Wrack", "No wrack")) %>%
    group_by(EOYBYear, Plot = marshRegion, Treatment, locationName, Replicate) %>%
    summarise(live_gm2 = sum(liveMass, na.rm = TRUE) * 16, .groups = "drop") %>%
    group_by(EOYBYear, Plot, Treatment) %>%
    summarise(live_gm2 = mean(live_gm2), n_quadrats = n(), .groups = "drop") %>%
    as.data.frame()
}
