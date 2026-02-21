# preprocess_nwt.R
# Source: knb-lter-nwt.13.7
# Increased temperature, N and snowpack experiment 2006-ongoing
# Treatment: 3-factor factorial (snow × N × temp), each P/X or N/X or W/X
# Control = XXX (ambient snow, ambient N, ambient temp)

library(dplyr)

#' Preprocess NWT aboveground biomass (ANPP) from EDI raw download
#' @param raw_path Path to raw CSV from EDI (ANPP entity)
#' @return data.frame with columns matching current output structure
preprocess_nwt_anpp <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI ANPP data has: year, block, snow, N, temp, plot, mass
  # Aggregate to block × treatment combination level (mean across plots)

  # Helper to calculate summary stats
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

  result <- data %>%
    select(year, block, snow, N, temp, plot, mass) %>%
    filter(!is.na(mass)) %>%
    summarize_by_columns(c("year", "block", "snow", "N", "temp"))

  as.data.frame(result)
}
