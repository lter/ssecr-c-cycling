# preprocess_hbr.R
# Source: knb-lter-hbr.404.1
# MELNHE: Litterfall mass 2009-2022
# Treatment: C (control), N, NP, P (nutrient additions)

library(dplyr)

#' Preprocess HBR litterfall data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Season, Site, Stand, Treatment, Mass_gm2
preprocess_hbr_litterfall <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI data has: Year, Season, Site, Stand, Treatment, Total_Mass_g_m2
  # (and possibly individual component columns like Leaf_Mass, etc.)
  # Filter to Fall season, then aggregate to Stand level

  data <- data %>%
    filter(Season == "Fall") %>%
    filter(!is.na(Total_Mass_g_m2)) %>%
    group_by(Year, Season, Site, Stand, Treatment) %>%
    summarise(Mass_gm2 = mean(Total_Mass_g_m2, na.rm = TRUE), .groups = "drop")

  as.data.frame(data)
}
