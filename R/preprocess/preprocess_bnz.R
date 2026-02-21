# preprocess_bnz.R
# Source: knb-lter-bnz.481.23
# CiPEHR: Half-hourly CO2 flux data 2009-2021
# Treatment: factorial winter warming (W/C) × summer warming (S/C)
#   C C = Control, W C = Winter only, C S = Summer only, W S = Both
# Replicate: fence number

library(dplyr)

#' Preprocess BNZ NEE data from EDI
#' Aggregates half-hourly flux to annual totals per fence × treatment
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: year, fence, treatment, NEE
preprocess_bnz_nee <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has half-hourly data: year, doy, hour, fence, WW, SW, NEE_g, etc.
  # WW = winter warming treatment (W/C)
  # SW = summer warming treatment (S/C)
  # Create combined treatment, then aggregate to annual sums

  # Create treatment column
  ww_col <- grep("WW|winter|Winter", names(data), value = TRUE)[1]
  sw_col <- grep("SW|summer|Summer", names(data), value = TRUE)[1]
  nee_col <- grep("NEE_g|NEE|nee", names(data), value = TRUE)[1]
  fence_col <- grep("fence|Fence", names(data), value = TRUE)[1]
  year_col <- grep("year|Year", names(data), value = TRUE)[1]
  doy_col <- grep("doy|DOY|julian", names(data), value = TRUE)[1]

  data[[nee_col]] <- suppressWarnings(as.numeric(data[[nee_col]]))

  data <- data %>%
    filter(!is.na(.data[[nee_col]])) %>%
    mutate(treatment = paste(.data[[ww_col]], .data[[sw_col]]))

  # Sum to daily totals first, then annual
  result <- data %>%
    group_by(
      year = .data[[year_col]],
      fence = .data[[fence_col]],
      treatment,
      doy = .data[[doy_col]]
    ) %>%
    summarise(NEE_daily = sum(.data[[nee_col]], na.rm = TRUE), .groups = "drop") %>%
    group_by(year, fence, treatment) %>%
    summarise(NEE = sum(NEE_daily, na.rm = TRUE), .groups = "drop")

  as.data.frame(result)
}
