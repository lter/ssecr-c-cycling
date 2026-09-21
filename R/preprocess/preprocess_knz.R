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

  biomass_cols <- c("LVGRASS", "FORBS", "CUYRDD")

  # Herbaceous aboveground production per quadrat. Current-year dead (CUYRDD)
  # was sorted separately until 2002 and is part of LVGRASS afterwards, so it is
  # added back for a consistent series. Woody stems are multi-year biomass and
  # single shrub quadrats (400-660 g) in the control plots otherwise swing the
  # denominator of every treatment, so WOODY is not included.
  data <- data %>%
    mutate(across(all_of(biomass_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    filter(!is.na(LVGRASS), !is.na(FORBS)) %>%
    mutate(
      MOWED = tolower(MOW) == "m",
      FERTILIZED = tolower(NUTRIENT) != "c",
      TREATMENT = paste(tolower(BURN), tolower(MOW), tolower(NUTRIENT)),
      BIOMASS = LVGRASS + FORBS + coalesce(CUYRDD, 0)
    ) %>%
    # Treatments are analyzed only while they were applied (package metadata):
    # mowing was last implemented in 2003, fertilization ended in 2017
    filter(!(MOWED & RECYEAR > 2003), !(FERTILIZED & RECYEAR > 2016)) %>%
    # mean of the quadrats clipped on a date
    group_by(RECYEAR, RECMONTH, RECDAY, PLOT, TREATMENT, MOWED) %>%
    summarise(BIOMASS = mean(BIOMASS), .groups = "drop") %>%
    # One value per plot-year: mowed plots were clipped before mowing and again
    # at peak biomass, so their production is the SUM of the clips; unmowed
    # plots use the last (peak-season) clip of the year
    arrange(RECYEAR, PLOT, RECMONTH, RECDAY) %>%
    group_by(RECYEAR, PLOT, TREATMENT) %>%
    summarise(BIOMASS = if (first(MOWED)) sum(BIOMASS) else last(BIOMASS),
              N_CLIPS = n(), .groups = "drop")

  as.data.frame(data)
}
