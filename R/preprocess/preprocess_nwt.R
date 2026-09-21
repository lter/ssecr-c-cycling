# preprocess_nwt.R
# Source: knb-lter-nwt.13.7
# Increased temperature, N and snowpack experiment 2006-ongoing
# Treatment: 3-factor factorial (snow P/X x N N/X x temp W/X); X = ambient.
#
# The analysis uses only the N contrast (column_key maps N -> Treatment), so
# this script restricts the data to plots with ambient snow AND ambient temp.
# Without that filter, warmed/snow-fenced plots with ambient N (N = "X") would
# be pooled into the control group downstream.
#
# Raw layout REQUIRED (aboveground biomass entity, g m-2, one row per
# clipped subsample; 2 rows per block x treatment in 2006, 4 in 2007-2019,
# no 2018):
#   year, block, snow, N, temp, plot, mass   (other columns are ignored)
#
# !! The entity currently recorded in data/dataset_registry.csv for this
# !! package (4eb00e2fd65329c4463732974907061f -> data/raw/NWT_anpp/data.csv)
# !! is NOT that table. It is the point-quadrat species-composition entity:
# !!   LTER_site, local_site, year, date, monitors, block, plot, code, snow,
# !!   N, temp, NWT_code, USDA_code, USDA_name, growth_habit, hits
# !! (2006-2023, ~100 hits per plot, "NaN" as missing code). It contains no
# !! biomass and biomass cannot be derived from it, so the function stops
# !! with an explicit error instead of returning something misleading.
# !! Fix: point the registry at the biomass ("ANPP") entity of the package.

library(dplyr)

#' Preprocess NWT aboveground biomass (ANPP) from EDI raw download
#' @param raw_path Path to raw CSV from EDI (aboveground biomass entity)
#' @return data.frame: year, block, snow, N, temp, plot_{sd,mean,sum},
#'   mass_{sd,mean,sum}; ambient-snow, ambient-temperature plots only
preprocess_nwt_anpp <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE,
                   na.strings = c("NA", "NaN", "", "."))

  needed <- c("year", "block", "snow", "N", "temp", "plot", "mass")
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    hint <- if ("hits" %in% names(data)) {
      paste0(" This file is the point-quadrat species-composition entity ",
             "(column 'hits'), not the aboveground biomass entity; update ",
             "edi_entity_id for NWT_anpp in data/dataset_registry.csv.")
    } else ""
    stop("preprocess_nwt_anpp: raw file lacks required column(s): ",
         paste(missing_cols, collapse = ", "), ".", hint, call. = FALSE)
  }

  # Keep only ambient-snow, ambient-temp plots so N vs X is a clean contrast,
  # then aggregate subsamples to block x treatment combination (sd/mean/sum).
  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R.
  result <- data %>%
    select(all_of(needed)) %>%
    mutate(
      plot = as.numeric(plot),
      mass = as.numeric(mass),
      # explicit missing-value codes -> NA before aggregating
      mass = ifelse(mass %in% c(-9999, -99999, 9999), NA_real_, mass)
    ) %>%
    filter(!is.na(mass), snow == "X", temp == "X") %>%
    summarize_by_columns(c("year", "block", "snow", "N", "temp")) %>%
    arrange(year, block, snow, N, temp)

  as.data.frame(result)
}
