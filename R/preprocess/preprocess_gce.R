# preprocess_gce.R
# Source: PLT-GCED-2207 from gce-lter.marsci.uga.edu (non-EDI)
# Critical slowing down: vegetation height, cover and composition
# Treatment: Control vs Disturbed (March 2010)

library(dplyr)

#' Preprocess GCE vegetation data from direct URL download
#' @param raw_path Path to raw CSV
#' @return data.frame with summary statistics per Date/Site/Treatment
preprocess_gce_vegetation <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE, skip = 22)

  # After skipping 22 header rows, raw data has:
  # Date, Site, Latitude, Longitude, Treatment, Plot, Sub.plot,
  # Vegetation_Cover, Vegetation_Height, and individual species columns

  # Helper function
  summarize_by_columns <- function(data, group_cols) {
    data %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(across(where(is.numeric), list(
        sd = ~sd(.x, na.rm = TRUE),
        mean = ~mean(.x, na.rm = TRUE),
        sum = ~sum(.x, na.rm = TRUE)
      ), .names = "{.col}_{.fn}"),
      .groups = "drop")
  }

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
