# preprocess_mcm.R
# Source: knb-lter-mcm.4014.5
# McMurdo Dry Valleys Stoichiometry Experiment - CO2 flux data
# Replaces the original knb-lter-mcm.4013.6 (invertebrate abundance, not carbon)
#
# The stoichiometry experiment applies aqueous nutrient additions (C, N, P, CN, CP)
# at the Redfield Ratio (106:16:1) in the Bonney and Fryxell basins.
# CO2 flux is measured with a LI-8100 soil gas flux system.
#
# CONTROL: "W" = Water only (the control). "U" = Unamended (excluded in harmonization).
# NOTE: "C" = Carbon (mannitol) addition — NOT a control label!
#
# Columns in raw EDI data:
#   DATASET_CODE, BASIN, BLOCK_ID, TREATMENT, DATE_TIME,
#   PRE_CO2 (µmol CO2/m²/sec), POST_CO2 (µmol CO2/m²/sec),
#   PRE_TEMP, POST_TEMP, PRE_MOISTURE, POST_MOISTURE, comments
#
# We use POST_CO2 as the response variable (flux after treatment reapplication),
# averaged to annual means per treatment × block.

library(dplyr)

#' Preprocess MCM CO2 flux data from EDI
#' @param raw_path Path to raw CSV from EDI (knb-lter-mcm.4014.5 entity SOILS_SE_CO2)
#' @return data.frame with columns: Year, BLOCK_ID, TREATMENT, CO2_flux
preprocess_mcm_co2flux <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Extract year from DATE_TIME column
  date_col <- grep("DATE_TIME|date_time|Date", names(data), value = TRUE)[1]
  if (is.null(date_col) || is.na(date_col)) {
    stop("Cannot find date column in MCM CO2 flux data")
  }

  # Parse date and extract year
  data$date_parsed <- as.Date(data[[date_col]], format = "%m/%d/%y")
  if (all(is.na(data$date_parsed))) {
    data$date_parsed <- as.Date(data[[date_col]], format = "%Y-%m-%d")
  }
  data$Year <- as.integer(format(data$date_parsed, "%Y"))

  # Identify columns
  block_col <- grep("BLOCK_ID|block", names(data), value = TRUE, ignore.case = TRUE)[1]
  treatment_col <- grep("^TREATMENT$|^treatment$", names(data), value = TRUE)[1]

  # Use POST_CO2 as the response (flux after treatment reapplication)
  # Fall back to PRE_CO2 if POST_CO2 doesn't exist
  co2_col <- grep("POST_CO2|post_co2", names(data), value = TRUE, ignore.case = TRUE)[1]
  if (is.null(co2_col) || is.na(co2_col)) {
    co2_col <- grep("PRE_CO2|pre_co2", names(data), value = TRUE, ignore.case = TRUE)[1]
  }
  if (is.null(co2_col) || is.na(co2_col)) {
    stop("Cannot find CO2 flux column in MCM data")
  }

  data[[co2_col]] <- suppressWarnings(as.numeric(data[[co2_col]]))

  # Average to annual means per treatment × block
  result <- data %>%
    filter(!is.na(.data[[co2_col]]), !is.na(Year)) %>%
    group_by(
      Year = Year,
      BLOCK_ID = .data[[block_col]],
      TREATMENT = .data[[treatment_col]]
    ) %>%
    summarise(
      CO2_flux = mean(.data[[co2_col]], na.rm = TRUE),
      .groups = "drop"
    )

  as.data.frame(result)
}
