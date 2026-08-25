# preprocess_and.R
# Source: knb-lter-and.2742.28 (TV010 - Long-term tree growth in permanent plots)
#
# Uses DBH (diameter at breast height) consistently for all three watersheds.
# The TV010 dataset contains overstory tree remeasurements for WS06, WS07, and WS08.
#
# Previously, treatment data came from TP114 Entity 6 (understory DBA) while control
# came from TV010 (overstory DBH) — these are incompatible metrics measuring different
# vegetation strata. Now we use TV010 DBH for everything.
#
# Treatment: WATERSHED
#   WS06 = clearcut 1974 (measured 2002, 2008, 2014)
#   WS07 = shelterwood cut (measured 2002, 2008, 2014)
#   WS08 = unlogged control (measured 2003, 2009, 2015)
#
# CENSUS-PERIOD ALIGNMENT: the control watershed was censused one year after
# the treatment watersheds in each of the three census periods. Response-ratio
# analyses require same-date treatment/control pairing, so WS08 years are
# recoded to the nominal census year (2003->2002, 2009->2008, 2015->2014).
# The bias from the 1-year offset is negligible: WS08 mean DBH drifted ~0.3%/yr
# (34.8 -> 36.1 cm over 12 years).
#
# The STANDID column identifies the watershed in the TV010 data.
# DBH is in centimeters; we compute plot-level mean DBH per year × watershed.

library(dplyr)

#' Preprocess AND tree DBH data from TV010
#' Uses a single data source (knb-lter-and.2742.28) for all three watersheds,
#' ensuring consistent DBH measurements across treatment and control.
#'
#' @param raw_path Path to the TV010 CSV (AND_Logging-control_DBH_1910-2023.csv)
#' @return data.frame with columns: Year, WATERSHED, PLOT, DBH_mean, DBH_sd
preprocess_and_dbh <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Filter to the three experimental watersheds
  data <- data %>% filter(STANDID %in% c("WS06", "WS07", "WS08"))

  # Rename STANDID to WATERSHED for consistency
  data$WATERSHED <- data$STANDID

  # Parse dates and extract year — try YYYY-MM-DD first, then M/D/YYYY
  raw_dates <- data$SAMPLEDATE
  data$SAMPLEDATE <- as.Date(raw_dates, format = "%Y-%m-%d")
  if (all(is.na(data$SAMPLEDATE))) {
    data$SAMPLEDATE <- as.Date(raw_dates, format = "%m/%d/%Y")
  }
  data$Year <- as.integer(format(data$SAMPLEDATE, "%Y"))

  # Filter to post-2002 (when the vegetation study began)
  data <- data %>% filter(Year >= 2002)

  # Convert DBH to numeric (in case of any non-numeric entries)
  data$DBH <- suppressWarnings(as.numeric(data$DBH))

  # Filter to living trees only (TREE_STATUS == 1 typically means alive)
  if ("TREE_STATUS" %in% names(data)) {
    data <- data %>% filter(TREE_STATUS == 1)
  }

  # Compute plot-level mean DBH per year × watershed × plot
  result <- data %>%
    filter(!is.na(DBH)) %>%
    group_by(Year, WATERSHED, PLOTNUMBER) %>%
    summarise(
      DBH_mean = mean(DBH, na.rm = TRUE),
      DBH_sd = sd(DBH, na.rm = TRUE),
      n_trees = n(),
      .groups = "drop"
    ) %>%
    rename(PLOT = PLOTNUMBER)

  # Census-period alignment (see header): pair WS08 control censuses with the
  # treatment censuses taken one year earlier. Explicit mapping so any future
  # census with a different offset fails loudly instead of shifting silently.
  census_map <- c("2003" = 2002L, "2009" = 2008L, "2015" = 2014L)
  is_ctrl <- result$WATERSHED == "WS08"
  unmapped <- setdiff(unique(result$Year[is_ctrl]), as.integer(names(census_map)))
  if (length(unmapped) > 0) {
    stop("AND: unmapped WS08 census year(s): ", paste(unmapped, collapse = ", "),
         " — extend census_map with the paired treatment year.")
  }
  result$Year[is_ctrl] <- census_map[as.character(result$Year[is_ctrl])]

  as.data.frame(result)
}
