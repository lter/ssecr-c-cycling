# preprocess_arc.R
# Source: knb-lter-arc.10004.8
# Above ground plant biomass in mesic acidic tussock tundra 1982-2015
# Toolik Lake fertilization/warming/shade experiment
# Treatment: Control, NP (= N+P fertilizer; a label, NOT a missing code),
#            GH (greenhouse), GHNP, SH (shade)
#
# RAW LAYOUT (wide; one row per Date x Site x Treatment x Species x Tissue):
#   Date ("31-Jul-1982"), Site, Treatment, Growth Form, Species, Tissue,
#   Biomass Categroy [sic] (new above / old above / below / non-vascular / Litter),
#   B1Q1..B4Q8        quadrat dry mass, g per quadrat (4 blocks x up to 8 quadrats)
#   Average, Std. Err., Count
#   B1Q1gm2..B4Q7gm2  the same quadrats scaled to g/m^2 (x25)
#   Average g/m^2, StdERR g/m^2, Count  g/m^2, Species Comments, Comments
#   - "." is the missing-value marker (quadrat not harvested); read as NA.
#   - Sites: "Toolik Tussock 1981 plots" (1982-2000) and "1981 Acidic Tussock"
#     (2015; same plots). The 2015 harvest has NO values in the *gm2 columns,
#     so its Biomass is NA (as in the committed ready file).
#   - Several harvests per year in 1982 (Jun, Jul, Sep) and 1983 (May, Jun,
#     3 Jul, 30 Jul, Aug); one (late July) in all other years.
#
# PROCESSING
#   1. Peak-season harvest only: the late-July harvest of each year (day >= 15;
#      1983 also has a 3 July harvest, which is excluded).
#   2. Aboveground biomass only: categories "new above", "old above" and
#      "non-vascular". Belowground tissues and litter are excluded, matching
#      the dataset title and the response variable reported in the paper.
#   3. Quadrat totals are computed from the raw per-quadrat masses (x25 to
#      g/m^2; quadrats are 20 x 20 cm), using every quadrat that was harvested
#      for that Date x Treatment (absent species are recorded as 0, unharvested
#      quadrats as "."). This recovers 2015, whose *gm2 columns are empty.
#   4. One value per Year x Treatment x block: the mean of the block's quadrat
#      totals.
#
# The 2025 hand-processed file differed: it summed the two July 1983 harvests
# for Control and NP, included belowground biomass and litter, dropped quadrats
# B1Q2 and B4Q5, and had no values for 2015.

library(dplyr)
library(tidyr)

#' Preprocess ARC biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Treatment, Biomass (g/m^2), block
preprocess_arc_biomass <- function(raw_path) {
  # "." = missing; "NP" is a treatment label, so "NA"-like strings are not
  # added to na.strings beyond the explicit marker and empty cells.
  data <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE,
                   na.strings = c(".", ""), strip.white = TRUE)

  harvest_date <- as.Date(data$Date, format = "%d-%b-%Y")
  if (any(is.na(harvest_date))) {
    stop("ARC: unparseable Date value(s): ",
         paste(unique(data$Date[is.na(harvest_date)]), collapse = ", "))
  }
  data$Year <- as.integer(format(harvest_date, "%Y"))
  data$Month <- as.integer(format(harvest_date, "%m"))

  data$Day <- as.integer(format(harvest_date, "%d"))

  # 1. Late-July (peak-season) harvest; 2. aboveground categories
  data <- data %>%
    filter(Month == 7, Day >= 15,
           # mosses and lichens ("non-vascular") were only sorted from 1989 on,
           # so including them makes 1983-84 incomparable with later harvests
           `Biomass Categroy` %in% c("new above", "old above"))

  # 3. Quadrat totals from the raw masses of every harvested quadrat
  quad_cols <- grep("^B[1-4]Q[1-8]$", names(data), value = TRUE)
  long <- data %>%
    select(Year, Treatment, all_of(quad_cols)) %>%
    mutate(across(all_of(quad_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    pivot_longer(all_of(quad_cols), names_to = "quadrat", values_to = "mass") %>%
    filter(!is.na(mass)) %>%
    mutate(block = paste("block", substr(quadrat, 2, 2))) %>%
    group_by(Year, Treatment, block, quadrat) %>%
    summarise(gm2 = sum(mass) * 25, .groups = "drop")

  # 4. Block means
  result <- long %>%
    group_by(Year, Treatment, block) %>%
    summarise(Biomass = mean(gm2), n_quadrats = n(), .groups = "drop") %>%
    select(Year, Treatment, Biomass, block, n_quadrats)

  as.data.frame(result)
}
