# preprocess_knz.R
# Source: knb-lter-knz.57.15 (Konza datacode PBB01)
# Belowground Plot Experiment: Aboveground NPP of tallgrass prairie 1986-2021
# Treatment: burn x mow x nutrient factorial, written "BURN MOW NUTRIENT"
#   BURN     b = annually burned, u = unburned
#   MOW      m = mowed,           u = unmowed
#   NUTRIENT c = control, n = +N, p = +P, n+p = +N+P
#   -> 16 treatment labels, e.g. "u u c", "b m n+p"

library(dplyr)

#' Preprocess KNZ biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI (PBB01 entity)
#' @return data.frame with columns: RECYEAR, RECMONTH, RECDAY, PLOT, TREATMENT, BIOMASS
preprocess_knz_biomass <- function(raw_path) {
  # Raw EDI layout (one row per clipped quadrat), all column names upper case:
  #   DATACODE, RECTYPE, RECYEAR, RECMONTH, RECDAY, PLOT (1-64),
  #   REPLICATE (quadrat within plot: "a"/"b"; "." in 1986-1988 when only one
  #   quadrat was recorded), BURN, MOW, NUTRIENT, LVGRASS, FORBS, CUYRDD,
  #   PRYRDD, WOODY, COMMENTS
  # Missing values are coded "." (or left blank), which makes read.csv() import
  # the biomass columns as character. Declare the codes explicitly so they
  # become NA on read.
  data <- read.csv(raw_path, stringsAsFactors = FALSE,
                   na.strings = c("", "NA", ".", "-9999", "-99999", "9999"),
                   strip.white = TRUE)

  biomass_cols <- c("LVGRASS", "FORBS", "WOODY")

  data <- data %>%
    mutate(across(all_of(biomass_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    # Legacy rule: keep quadrats with both live grass and forbs recorded
    filter(!is.na(LVGRASS), !is.na(FORBS)) %>%
    mutate(
      TREATMENT = paste(tolower(BURN), tolower(MOW), tolower(NUTRIENT)),
      # Live aboveground biomass per quadrat (current-year dead CUYRDD and
      # previous-year dead PRYRDD are not included, as in the legacy file)
      BIOMASS = LVGRASS + FORBS + WOODY
    ) %>%
    # Mean across quadrats (REPLICATE a/b) within plot and harvest date
    group_by(RECYEAR, RECMONTH, RECDAY, PLOT, TREATMENT) %>%
    summarise(BIOMASS = mean(BIOMASS), .groups = "drop")

  as.data.frame(data)
}
