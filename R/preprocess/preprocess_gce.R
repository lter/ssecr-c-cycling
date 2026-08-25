# preprocess_gce.R
# Source: PLT-GCED-2207 (also published on EDI as knb-lter-gce.786.13)
# Critical slowing down: vegetation height, cover and composition
# Treatment: Control vs Disturbed (March 2010)
#
# NOTE: skip = 22 matches the GCE-portal export's metadata header block.
# If re-downloading from EDI, verify the entity's header offset first —
# a different header depth would silently misparse the file.

library(dplyr)

#' Preprocess GCE vegetation data from direct URL download
#' @param raw_path Path to raw CSV
#' @return data.frame with summary statistics per Date/Site/Treatment
preprocess_gce_vegetation <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE, skip = 22)

  # After skipping 22 header rows, raw data has:
  # Date, Site, Latitude, Longitude, Treatment, Plot, Sub.plot,
  # Vegetation_Cover, Vegetation_Height, and individual species columns

  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R

  # Fix date format: ensure YYYY (not "YYYY-MM" or corrupted)
  if ("Date" %in% names(data)) {
    data$Date <- sub("^(\\d{4}).*", "\\1", as.character(data$Date))
  }

  result <- summarize_by_columns(
    data,
    c("Date", "Site", "Latitude", "Longitude", "Treatment")
  )

  as.data.frame(result)
}
