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
# PROCESSING (reproduces legacy "ARC Processing Script.Rmd"):
#   1. Keep July harvests only (peak season).
#   2. Per Year x Treatment x Species x Tissue and block: mean of each quadrat
#      column (na.rm), then the mean of those quadrat means, multiplied by the
#      number of raw rows in the group. The legacy script used mutate() and
#      then summed the repeated value, so a group holding two harvests (only
#      1983 Control/NP: 3 Jul + 30 Jul) contributes the SUM of the two
#      harvests, not their mean. Kept for fidelity with the ready file.
#   3. Legacy quadrat subset: block 1 = Q1,Q3,Q4,Q5 (Q2 omitted); blocks 2-3 =
#      Q1-Q5; block 4 = Q1-Q4 (Q5 omitted); Q6-Q7 (2000 only) never used.
#   4. Sum over species and tissues (all biomass categories, incl. below,
#      non-vascular and litter) -> one Biomass value per Year x Treatment x block.

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

  # 1. Peak-season (July) harvests only
  data <- data %>% filter(Month == 7)

  # 3. Legacy quadrat subset per block (g/m^2 columns)
  block_quads <- list(
    "block 1" = c("B1Q1gm2", "B1Q3gm2", "B1Q4gm2", "B1Q5gm2"),
    "block 2" = paste0("B2Q", 1:5, "gm2"),
    "block 3" = paste0("B3Q", 1:5, "gm2"),
    "block 4" = paste0("B4Q", 1:4, "gm2")
  )
  quad_cols <- unlist(block_quads, use.names = FALSE)
  missing_cols <- setdiff(quad_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("ARC: expected quadrat column(s) not found: ",
         paste(missing_cols, collapse = ", "))
  }
  for (col in quad_cols) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }

  # 2. + 4. Block value per species x tissue, then sum over species/tissues
  result <- lapply(names(block_quads), function(b) {
    cols <- block_quads[[b]]
    data %>%
      group_by(Year, Treatment, Species, Tissue) %>%
      summarise(
        value = n() * mean(colMeans(pick(all_of(cols)), na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      group_by(Year, Treatment) %>%
      summarise(Biomass = sum(value), .groups = "drop") %>%
      mutate(block = b)
  }) %>%
    bind_rows() %>%
    mutate(Biomass = ifelse(is.nan(Biomass), NA_real_, Biomass))

  as.data.frame(result)
}
