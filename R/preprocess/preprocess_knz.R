# preprocess_knz.R
# Source: knb-lter-knz.57.15
# Belowground Plot Experiment: Aboveground NPP of tallgrass prairie 1986-2021
# Treatment: burn × mow × nutrient factorial (e.g., "u u c", "b m n+p")

library(dplyr)

#' Preprocess KNZ biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: RECYEAR, RECMONTH, RECDAY, PLOT, TREATMENT, BIOMASS
preprocess_knz_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI columns include: RecYear, RecMonth, RecDay, Watershed, Plot,
  # Burn (b/u), Mow (m/u), Nutrient (c/n/p/n+p), Lvgrass, Forbs, Woody, etc.
  # Create combined treatment and sum biomass components

  data <- data %>%
    mutate(
      across(c(Lvgrass, Forbs, Woody), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(!is.na(Lvgrass) | !is.na(Forbs)) %>%
    mutate(
      TREATMENT = paste(tolower(Burn), tolower(Mow), tolower(Nutrient)),
      BIOMASS = rowSums(cbind(
        ifelse(is.na(Lvgrass), 0, Lvgrass),
        ifelse(is.na(Forbs), 0, Forbs),
        ifelse(is.na(Woody), 0, Woody)
      ))
    ) %>%
    group_by(
      RECYEAR = RecYear,
      RECMONTH = RecMonth,
      RECDAY = RecDay,
      PLOT = Plot,
      TREATMENT
    ) %>%
    summarise(BIOMASS = mean(BIOMASS, na.rm = TRUE), .groups = "drop")

  as.data.frame(data)
}
