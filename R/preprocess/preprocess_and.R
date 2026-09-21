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
# RAW LAYOUT (TV010 entity 2, "individual tree remeasurements", ~611k rows):
#   DBCODE, ENTITY (= 2), TREEID, PSP_STUDYID, STANDID, PLOTNUMBER, QUARTER,
#   SPECIES, TAG, YEAR, TREE_STATUS, DBH (cm), DBH_CODE, CANOPY_CLASS,
#   TREE_VIGOR, CROWN_RATIO, MAIN_STEM, ROOTING, CROWN_PCT, TREE_PCT,
#   LEAN_ANGLE, SAMPLEDATE (M/D/YYYY or YYYY-MM-DD), CHECK_NOTES, DB_NOTES
#   - STANDID identifies the watershed (WS06 / WS07 / WS08 among ~170 stands).
#   - TREE_STATUS: 1 = live (previously tagged), 2 = ingrowth, 3 = other live
#     code, 6 = dead, 9 = missing/not found. Dead and missing trees carry an
#     empty DBH (DBH_CODE "M"). Only TREE_STATUS == 1 is kept, as in the
#     committed ready file (NB: this excludes live ingrowth, status 2).
#   - Missing values are empty cells; 9999/99999 occur only as "unknown" codes
#     in PLOTNUMBER and TAG (never in DBH) and not in WS06-08. They are set to
#     NA defensively so they can never become a plot key.
#
# NOTE: TV010 entity 1 (the one-row-per-tree attribute table: ... SPECIES,
# YEAR, MONTH, XCOORD, YCOORD, CROWN1..3, BOLE1..3, ROOT, DISTURB1..2, AGE)
# has NO DBH column and does not contain the WS06-08 stands. This function
# stops with an explicit message if it is handed that table.
#
# DBH is in centimeters; we compute plot-level mean DBH per year x watershed.

library(dplyr)

#' Preprocess AND tree DBH data from TV010
#' Uses a single data source (knb-lter-and.2742.28, entity 2) for all three
#' watersheds, ensuring consistent DBH measurements across treatment and control.
#'
#' @param raw_path Path to the TV010 entity-2 (tree remeasurement) CSV
#'   (legacy name: AND_Logging-control_DBH_1910-2023.csv)
#' @return data.frame with columns: Year, WATERSHED, PLOT, DBH_mean, DBH_sd, n_trees
preprocess_and_dbh <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Guard against the wrong TV010 entity (the tree-attribute table has no DBH)
  required <- c("STANDID", "PLOTNUMBER", "YEAR", "TREE_STATUS", "DBH")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("AND: raw file lacks column(s) ", paste(missing_cols, collapse = ", "),
         ". preprocess_and_dbh() needs TV010 entity 2 (tree remeasurements, ",
         "with DBH/TREE_STATUS/SAMPLEDATE); this looks like ",
         if ("ENTITY" %in% names(data)) paste0("entity ", data$ENTITY[1]) else "another table",
         ".")
  }

  # Filter to the three experimental watersheds
  data <- data %>% filter(STANDID %in% c("WS06", "WS07", "WS08"))

  # Rename STANDID to WATERSHED for consistency
  data$WATERSHED <- data$STANDID

  if (nrow(data) == 0) {
    stop("AND: no WS06/WS07/WS08 rows found in STANDID.")
  }

  # Unknown-plot codes -> NA (never observed in WS06-08; defensive)
  data$PLOTNUMBER[data$PLOTNUMBER %in% c(9999, 99999)] <- NA

  # Census year: from SAMPLEDATE (YYYY-MM-DD or M/D/YYYY) when present, falling
  # back to the YEAR column (identical for every WS06-08 record).
  data$Year <- as.integer(data$YEAR)
  if ("SAMPLEDATE" %in% names(data)) {
    raw_dates <- data$SAMPLEDATE
    sample_date <- as.Date(raw_dates, format = "%Y-%m-%d")
    if (all(is.na(sample_date))) {
      sample_date <- as.Date(raw_dates, format = "%m/%d/%Y")
    }
    date_year <- as.integer(format(sample_date, "%Y"))
    data$Year <- ifelse(is.na(date_year), data$Year, date_year)
  }

  # Filter to post-2002 (when the vegetation study began)
  data <- data %>% filter(Year >= 2002)

  # Convert DBH to numeric (in case of any non-numeric entries)
  data$DBH <- suppressWarnings(as.numeric(data$DBH))

  # Living, previously tagged trees only (TREE_STATUS == 1; see header)
  data <- data %>% filter(TREE_STATUS == 1)

  # Compute plot-level mean DBH per year × watershed × plot
  result <- data %>%
    filter(!is.na(DBH), !is.na(PLOTNUMBER)) %>%
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
