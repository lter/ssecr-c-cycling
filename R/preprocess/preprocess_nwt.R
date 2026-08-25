# preprocess_nwt.R
# Source: knb-lter-nwt.13.7
# Increased temperature, N and snowpack experiment 2006-ongoing
# Treatment: 3-factor factorial (snow P/X × N N/X × temp W/X); X = ambient.
#
# The analysis uses only the N contrast (column_key maps N -> Treatment), so
# this script restricts the data to plots with ambient snow AND ambient temp.
# Without that filter, warmed/snow-fenced plots with ambient N (N = "X") would
# be pooled into the control group downstream.

library(dplyr)

#' Preprocess NWT aboveground biomass (ANPP) from EDI raw download
#' @param raw_path Path to raw CSV from EDI (ANPP entity)
#' @return data.frame with columns matching current output structure
preprocess_nwt_anpp <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI ANPP data has: year, block, snow, N, temp, plot, mass
  # Keep only ambient-snow, ambient-temp plots so N vs X is a clean contrast,
  # then aggregate to block × treatment combination level (mean across plots).
  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R.

  result <- data %>%
    select(year, block, snow, N, temp, plot, mass) %>%
    filter(!is.na(mass), snow == "X", temp == "X") %>%
    summarize_by_columns(c("year", "block", "snow", "N", "temp"))

  as.data.frame(result)
}
