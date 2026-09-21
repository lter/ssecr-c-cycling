# preprocess_pie.R
# Source: knb-lter-pie.202.5
# Marsh plant species shoot parameters for TIDE experiment at Rowley River
# Treatment: Control vs Enriched (NO3 fertilization)
#
# Raw EDI layout (one row per measured shoot, 2004-2020):
#   Year, Species, Creek, Branch, Transect, Rep, Shoot Height, Shoot Mass,
#   Shoot-specific Mass, Stem Diameter, Flower
# Quirks of the raw file:
#   - header starts with a UTF-8 BOM and several names carry trailing spaces
#     ("Species ", "Creek ", "Branch ", "Rep "); the body is NOT valid UTF-8
#     throughout (non-breaking spaces), so do not read it with
#     fileEncoding = "UTF-8-BOM" -- R silently truncates at the first bad byte.
#   - some code VALUES also carry trailing whitespace: Species "TSA " and
#     "TSA<nbsp>" (2016, 2018), Creek "WE " (2016), Branch "L " (2012).
#   - missing values are "NA"; no numeric sentinel codes occur.
# Codes: Species SP / DS / SSA / TSA; Creek WE, NE = reference, SW, CL = nutrient;
#   Branch L / R; Transect 1-4.
#
# Legacy transformation (legacy/preprocessing_scripts_manual/PIE Processing
# Script.Rmd, regrouped in legacy commit c6f12a9):
#   drop NA Shoot Mass -> mean shoot mass per Year x Creek x Branch x Transect
#   x Species -> sum across species -> one row per Year x Creek x Branch x
#   Transect, Replicate = "<Branch> <Transect>".
#
# The legacy file was built WITHOUT trimming the whitespace variants, so
# "TSA " / "TSA<nbsp>" were treated as extra species and "WE " as an extra
# creek. clean_codes = TRUE (default) reproduces the committed ready file
# exactly; clean_codes = TRUE gives the corrected values (differs in 14 legacy
# rows: 2016 WE x 12 collapse to 6, 2016 SW "L 1", 2018 SW "R 1").

library(dplyr)

#' Preprocess PIE plant shoot data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @param clean_codes If TRUE, trim whitespace / non-breaking spaces from the
#'   Species, Creek and Branch codes before aggregating (corrected output).
#'   If FALSE (default), keep the codes verbatim as the legacy ready file did.
#' @return data.frame with columns: Year, Creek, Treatment, Replicate, shoot_mass
preprocess_pie_plant_shoot <- function(raw_path, clean_codes = TRUE) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE,
                   na.strings = c("NA", "NaN", "", "."))

  # Strip BOM and trailing spaces from the header
  nm <- sub("^\357\273\277", "", names(data), useBytes = TRUE)
  names(data) <- make.names(trimws(nm))   # "Shoot Mass" -> "Shoot.Mass"

  trim_code <- function(x) trimws(gsub(" ", " ", enc2utf8(x), fixed = TRUE))
  if (clean_codes) {
    data <- data %>% mutate(across(c(Species, Creek, Branch), trim_code))
  }

  data <- data %>%
    # Defensive missing-value handling (none of these codes occur in 202.5)
    mutate(Shoot.Mass = ifelse(Shoot.Mass %in% c(-9999, -99999, 9999),
                               NA_real_, Shoot.Mass)) %>%
    filter(!is.na(Shoot.Mass)) %>%
    # Mean shoot mass within species per Year, Creek, Branch, Transect
    group_by(Year, Creek, Branch, Transect, Species) %>%
    summarise(shoot_mass = mean(Shoot.Mass), .groups = "drop") %>%
    # Sum across species
    group_by(Year, Creek, Branch, Transect) %>%
    summarise(shoot_mass = sum(shoot_mass), .groups = "drop")

  # Map creek-years to treatment from the package metadata: reference creeks
  # are West (WE) and Nelson (NE); Sweeney (SW) was fertilized 2004-2012 and
  # Clubhead (CL) in 2005 and 2009-2019. Creek-years in which a nutrient creek
  # was not being fertilized (CL 2004, 2006-2008, 2020; SW after 2012) are
  # neither treatment nor reference and are dropped. (The 2025 hand-processed
  # file had NE as enriched and CL as control, in every year.)
  data <- data %>%
    mutate(
      Creek = trim_code(Creek),
      Replicate = paste(Creek, Branch, Transect),
      Treatment = case_when(
        Creek %in% c("WE", "NE") ~ "Control",
        Creek == "SW" & Year >= 2004 & Year <= 2012 ~ "Enriched",
        Creek == "CL" & (Year == 2005 | (Year >= 2009 & Year <= 2019)) ~ "Enriched",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Treatment)) %>%
    arrange(Year, Creek, Branch, Transect) %>%
    select(Year, Creek, Treatment, Replicate, shoot_mass)

  as.data.frame(data)
}
