# preprocess_pie.R
# Source: knb-lter-pie.202.5
# Marsh plant species shoot parameters for TIDE experiment at Rowley River
# Treatment: Control vs Enriched (NO3 fertilization)

library(dplyr)

#' Preprocess PIE plant shoot data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Creek, Treatment, Replicate, shoot_mass
preprocess_pie_plant_shoot <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Year, Species, Creek, Branch, Transect, Shoot.Mass (and other shoot metrics)
  # Creek determines treatment: NE, SW = Enriched; CL, WE = Control
  # Aggregate: mean across species → sum across species → mean across transects

  data <- data %>%
    filter(!is.na(Shoot.Mass)) %>%
    # Average within species per Year, Creek, Branch, Transect
    group_by(Year, Creek, Branch, Transect, Species) %>%
    summarise(shoot_mass = mean(Shoot.Mass, na.rm = TRUE), .groups = "drop") %>%
    # Sum across species
    group_by(Year, Creek, Branch, Transect) %>%
    summarise(shoot_mass = sum(shoot_mass, na.rm = TRUE), .groups = "drop") %>%
    # Mean across transects within each creek-branch combination
    group_by(Year, Creek, Branch) %>%
    summarise(shoot_mass = mean(shoot_mass, na.rm = TRUE), .groups = "drop")

  # Map creek to treatment: NE/SW = Enriched, CL/WE = Control; drop any
  # other creek codes rather than silently labeling them Control
  data <- data %>%
    mutate(
      Treatment = case_when(
        Creek %in% c("NE", "SW") ~ "Enriched",
        Creek %in% c("CL", "WE") ~ "Control",
        TRUE ~ NA_character_
      ),
      Replicate = Branch
    ) %>%
    filter(!is.na(Treatment)) %>%
    select(Year, Creek, Treatment, Replicate, shoot_mass)

  as.data.frame(data)
}
