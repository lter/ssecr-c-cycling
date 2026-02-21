# preprocess_cap.R
# Source: knb-lter-cap.632.17
# Desert Fertilization Experiment: Sonoran desert response to N deposition
# Treatment: C1 (control), N, NP, P

library(dplyr)

#' Preprocess CAP biomass data from EDI
#' Hierarchical averaging: subplot → location → site × treatment
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: year, site_code, treatment_code, Biomass
preprocess_cap_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: site_code, treatment_code, location_within_plot, subplot,
  # year, mass, species, etc.
  # Hierarchical aggregation matching the original preprocessing

  mass_col <- grep("mass|Mass|biomass|Biomass", names(data), value = TRUE)[1]
  data[[mass_col]] <- suppressWarnings(as.numeric(data[[mass_col]]))

  result <- data %>%
    filter(!is.na(.data[[mass_col]])) %>%
    # Mean across subplots within each location
    group_by(year, site_code, treatment_code, location_within_plot, subplot) %>%
    summarise(Biomass = mean(.data[[mass_col]], na.rm = TRUE), .groups = "drop") %>%
    # Mean across subplots
    group_by(year, site_code, treatment_code, location_within_plot) %>%
    summarise(Biomass = mean(Biomass, na.rm = TRUE), .groups = "drop") %>%
    # Mean across locations within plot
    group_by(year, site_code, treatment_code) %>%
    summarise(Biomass = mean(Biomass, na.rm = TRUE), .groups = "drop")

  as.data.frame(result)
}
