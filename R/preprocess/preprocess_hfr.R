# preprocess_hfr.R
# Source: knb-lter-hfr.5.37
# Prospect Hill Soil Warming Experiment since 1991
# Treatment: C (control), DC (disturbance control), H (heated)

library(dplyr)

#' Preprocess HFR soil respiration data from EDI raw download
#' @param raw_path Path to raw CSV from EDI (hf005-05 entity)
#' @return data.frame with columns: year, treatment, plot, soil_res
preprocess_hfr_soil_respiration <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI data (hf005-05) has: year, julian.day, treatment, plot, co2.flux
  # plus potentially other measurement columns
  # Aggregate to annual mean per treatment × plot

  data <- data %>%
    filter(!is.na(co2.flux)) %>%
    group_by(year, treatment, plot) %>%
    summarise(soil_res = mean(co2.flux, na.rm = TRUE), .groups = "drop")

  as.data.frame(data)
}
