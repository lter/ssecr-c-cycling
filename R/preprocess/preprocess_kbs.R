# preprocess_kbs.R
# Source: knb-lter-kbs.19.85
# Annual NPP on Main Cropping System Experiment 1990-2022
# Treatment: T1-T7+ (different agricultural management practices)

library(dplyr)

#' Preprocess KBS biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Treatment, Replicate, Biomass
preprocess_kbs_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Year, Treatment, Replicate, Station, Species, Fraction, Biomass_g_m2
  # Aggregate: sum across Fraction → mean across Station → sum across Species
  data <- data %>%
    filter(!is.na(Biomass_g_m2)) %>%
    group_by(Year, Treatment, Replicate, Station, Species) %>%
    summarise(Biomass = sum(Biomass_g_m2, na.rm = TRUE), .groups = "drop") %>%
    group_by(Year, Treatment, Replicate, Species) %>%
    summarise(Biomass = mean(Biomass, na.rm = TRUE), .groups = "drop") %>%
    group_by(Year, Treatment, Replicate) %>%
    summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop")

  as.data.frame(data)
}
