# preprocess_sev.R
# Source: knb-lter-sev.186.208431
# NFert: Seasonal Biomass and Annual NPP at Sevilleta
# Treatment: C (control) vs F (nitrogen fertilization)

library(dplyr)

#' Preprocess SEV biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: year, season, plot, treatment, Biomass, Cover
preprocess_sev_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: year, season, plot, quad, treatment, kartez, cover, biomass.BM
  # Aggregate: sum across species (kartez) → mean across quads
  data <- data %>%
    filter(!is.na(biomass.BM) & !is.na(cover)) %>%
    group_by(year, season, plot, quad, treatment, kartez) %>%
    summarise(
      Biomass = sum(biomass.BM, na.rm = TRUE),
      Cover = sum(cover, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(year, season, plot, quad, treatment) %>%
    summarise(
      Biomass = mean(Biomass, na.rm = TRUE),
      Cover = mean(Cover, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(year, season, plot, treatment) %>%
    summarise(
      Biomass = mean(Biomass, na.rm = TRUE),
      Cover = mean(Cover, na.rm = TRUE),
      .groups = "drop"
    )

  as.data.frame(data)
}
