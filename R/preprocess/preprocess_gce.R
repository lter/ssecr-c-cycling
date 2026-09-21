# preprocess_gce.R
# Source: knb-lter-gce.786.13, entity PLT-GCED-2207_vegetation_2_0.CSV
# Critical slowing down: vegetation height, cover and composition
# Treatment: Control vs Disturbed (March 2010)
#
# The EDI entity has two title lines above the column names, then two
# descriptor rows (units, variable type) before the data.

library(dplyr)

#' Preprocess GCE vegetation data from direct URL download
#' @param raw_path Path to raw CSV
#' @return data.frame with summary statistics per Date/Site/Treatment
preprocess_gce_vegetation <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE, skip = 2)
  stopifnot(identical(names(data)[1:5], c("Date", "Site", "Latitude", "Longitude", "Treatment")))
  # Drop the units / variable-type descriptor rows, then restore numeric columns
  data <- data[grepl("^\\d{4}", data$Date), ]
  num_cols <- setdiff(names(data), c("Date", "Site", "Treatment"))
  data[num_cols] <- lapply(data[num_cols], function(x) suppressWarnings(as.numeric(x)))

  # Raw data has:
  # Date, Site, Latitude, Longitude, Treatment, Plot, Sub.plot,
  # Vegetation_Cover, Vegetation_Height, and individual species columns

  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R

  # Fix date format: ensure YYYY (not "YYYY-MM" or corrupted)
  if ("Date" %in% names(data)) {
    data$Date <- sub("^(\\d{4}).*", "\\1", as.character(data$Date))
  }

  # Monitoring of a marsh stopped once its disturbed plots had recovered, so
  # after 2014 only the slowest sites remain. The balanced panel (all twelve
  # sites, 2010-2014) is analyzed.
  data <- data %>% filter(as.integer(Date) <= 2014)

  result <- summarize_by_columns(
    data,
    c("Date", "Site", "Latitude", "Longitude", "Treatment")
  )

  as.data.frame(result)
}
