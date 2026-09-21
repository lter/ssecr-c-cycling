# preprocess_cdr.R
# 8 preprocessing functions for CDR (Cedar Creek) datasets
# Each dataset comes from a different EDI package and experiment

library(dplyr)

# =============================================================================
# CDR 1: BioCON Biomass (knb-lter-cdr.302.13)
# =============================================================================

# RAW LAYOUT (long; one row per Sampling x Plot x Species, ~80k rows):
#   Sampling (1-38), Date (M/D/YYYY; June and August harvests 1998-2011,
#   August only from 2012), Plot, Ring (1-6), CO2 Treatment (Camb / Cenrich /
#   "Cenriched "), Nitrogen Treatment (Namb / Nenrich), CountOfSpecies,
#   CountOfGroup, Experiment (M = main, S = species-level), monospecies,
#   Monogroup, Water Treatment ("" / " " / H2Oamb / H2Oneg), Temp Treatment
#   ("" / " " / HTamb / HTelv), Species, Aboveground Biomass (g/m^2)
#   - Species includes sorted species plus bulk fractions ("Miscellaneous
#     litter", "Unsorted Biomass", "Green Biomass", "16 Species Weeds", "Real
#     Weeds", "Bare ground", ...). The committed ready file sums ALL of them.
#   - 9999 in the biomass column is a missing-value code (108 "Bare ground"
#     rows, 2000-2004). It is set to NA before aggregating. NOTE: the legacy
#     ready file summed these 9999s as data, inflating 101 Ring x Treatment x
#     harvest means by 9999/n_plots (+385 to +800 g/m^2); this function
#     deliberately does not reproduce that.
#   - Treatment strings are pasted from the raw columns WITHOUT trimming, so
#     they keep the raw whitespace/spelling quirks ("Camb Namb M  " vs
#     "Camb Namb M    ", "Cenriched  Namb M ..."), exactly as in the committed
#     ready file; analysis/01_harmonize.R normalizes them downstream.
#
# PROCESSING: 9999 -> NA; litter and bare-ground records dropped; August
# (peak-season) harvest only; sum biomass over species within Date x Plot; mean
# of the plot totals per Date x Ring x Treatment; Date reduced to the year.
# The 2025 hand-processed file averaged the 9999 code into 101 plot means,
# counted litter (~26% of all mass) as aboveground biomass, and carried the June
# harvests of 1998-2011 as extra rows.

#' Preprocess CDR BioCON aboveground biomass
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Date (year), Ring, Treatment, Biomass
#'   (g/m^2; mean plot total per harvest x ring x treatment)
preprocess_cdr_biocon_biomass <- function(raw_path) {
  # strip.white = FALSE: raw whitespace is part of the Treatment keys (see above)
  data <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE,
                   strip.white = FALSE)

  required <- c("Date", "Plot", "Ring", "CO2 Treatment", "Nitrogen Treatment",
                "Experiment", "Water Treatment", "Temp Treatment",
                "Aboveground Biomass (g/m^2)")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("CDR BioCON: expected column(s) not found: ",
         paste(missing_cols, collapse = ", "))
  }

  # From 2013 four water x warming plots carry only one of their two factor
  # labels. Plots 97 and 226 (warming label present, water label blank) were
  # "H2Oamb" in 2012 and are restored; plots 83 and 194 (water label present,
  # warming label blank, and unlabelled in 2012) cannot be assigned and are
  # dropped for those years rather than forming a spurious one-plot treatment.
  yr <- as.integer(format(as.Date(data$Date, format = "%m/%d/%Y"), "%Y"))
  fix <- yr >= 2013 & data$Plot %in% c(97, 226) & trimws(data$`Water Treatment`) == ""
  data$`Water Treatment`[fix] <- "H2Oamb"
  data <- data[!(yr >= 2013 & data$Plot %in% c(83, 194)), ]

  data <- data %>%
    rename(Biomass_raw = `Aboveground Biomass (g/m^2)`) %>%
    mutate(
      # Missing-value code -> NA before any aggregation
      Biomass_raw = ifelse(Biomass_raw == 9999, NA_real_, as.numeric(Biomass_raw)),
      Treatment = paste(`CO2 Treatment`, `Nitrogen Treatment`, Experiment,
                        `Water Treatment`, `Temp Treatment`),
      SampleDate = as.Date(Date, format = "%m/%d/%Y")
    )
  if (any(is.na(data$SampleDate))) {
    stop("CDR BioCON: unparseable Date value(s): ",
         paste(head(unique(data$Date[is.na(data$SampleDate)])), collapse = ", "))
  }

  result <- data %>%
    # Aboveground *plant* biomass: litter and bare-ground records are not biomass
    filter(!grepl("litter|^Oak Leaves$|^Bare ground$", trimws(Species), ignore.case = TRUE)) %>%
    # Peak-season (August) harvest only. A June harvest also exists for
    # 1998-2011; mixing it in would make early and late years incomparable.
    filter(format(SampleDate, "%m") == "08") %>%
    # Plot total per harvest (all live species and unsorted live fractions)
    group_by(SampleDate, Ring, Treatment, Plot) %>%
    summarise(Biomass = sum(Biomass_raw, na.rm = TRUE), .groups = "drop") %>%
    # Mean plot total per harvest x ring x treatment
    group_by(SampleDate, Ring, Treatment) %>%
    summarise(Biomass = mean(Biomass), .groups = "drop") %>%
    arrange(SampleDate, Ring, Treatment) %>%
    mutate(Date = as.integer(format(SampleDate, "%Y"))) %>%
    select(Date, Ring, Treatment, Biomass)

  as.data.frame(result)
}

# =============================================================================
# CDR 2: Soil Biomass %C - E002 (knb-lter-cdr.449.9)
# =============================================================================

#' Preprocess CDR E002 soil biomass percent carbon
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Plot, NAdd.g.m2.yr., PercentC
preprocess_cdr_soil_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Year, Field, Plot, Depth, NTrt, NAdd, %Carbon, etc.
  # Aggregate: filter to relevant depth, group by Year/Plot/NAdd → mean %C

  # Identify key columns
  year_col <- grep("Year|year", names(data), value = TRUE)[1]
  plot_col <- grep("Plot|plot", names(data), value = TRUE)[1]
  nadd_col <- grep("NAdd|Nadd|NitrAdd", names(data), value = TRUE)[1]
  carbon_col <- grep("Carbon|carbon|%C|pctC|PercentC", names(data), value = TRUE)[1]

  result <- data
  if (!is.na(carbon_col)) {
    result[[carbon_col]] <- suppressWarnings(as.numeric(result[[carbon_col]]))
    result <- result %>%
      filter(!is.na(.data[[carbon_col]])) %>%
      group_by(
        Year = .data[[year_col]],
        Plot = .data[[plot_col]],
        NAdd.g.m2.yr. = .data[[nadd_col]]
      ) %>%
      summarise(PercentC = mean(.data[[carbon_col]], na.rm = TRUE), .groups = "drop")
  }

  as.data.frame(result)
}

# =============================================================================
# CDR 3: sIDE Aboveground Biomass (knb-lter-cdr.707.2)
# =============================================================================

#' Preprocess CDR sIDE aboveground biomass
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: date, plot, treatment, total_mass
preprocess_cdr_side_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: date, plot, treatment, taxa, mass
  # Sum mass across taxa per date/plot/treatment

  data <- data %>%
    mutate(across(where(is.character), trimws))

  # Normalize taxa names
  if ("taxa" %in% names(data)) {
    data <- data %>%
      mutate(taxa = tolower(taxa)) %>%
      mutate(taxa = case_when(
        taxa %in% c("forb", "forbs") ~ "Forbs",
        taxa %in% c("grass", "grasses") ~ "Grasses",
        taxa %in% c("legume", "legumes") ~ "Legumes",
        taxa %in% c("woody", "woodys") ~ "Woody",
        TRUE ~ taxa
      ))
  }

  result <- data %>%
    group_by(date, plot, treatment) %>%
    summarise(total_mass = sum(mass, na.rm = TRUE), .groups = "drop")

  # Format date to year only
  result <- result %>%
    mutate(date = sub("^(\\d{4}).*", "\\1", as.character(date)))

  as.data.frame(result)
}

# =============================================================================
# CDR 4: sIDE Percent Cover (knb-lter-cdr.708.2)
# =============================================================================

#' Preprocess CDR sIDE percent cover
#' NOTE: The previous version had duplicated biomass data here. This version
#' correctly extracts percent cover from the proper EDI entity.
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: date, plot, treatment, total_cover
preprocess_cdr_side_pctcover <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: date, plot, treatment, taxa, subplot, cover
  # Sum cover across taxa and subplots per date/plot/treatment

  data <- data %>%
    mutate(across(where(is.character), trimws))

  result <- data %>%
    group_by(date, plot, treatment) %>%
    summarise(total_cover = sum(cover, na.rm = TRUE), .groups = "drop")

  result <- result %>%
    mutate(date = sub("^(\\d{4}).*", "\\1", as.character(date)))

  as.data.frame(result)
}

# =============================================================================
# CDR 5: tIDE Percent Cover (knb-lter-cdr.710.2)
# =============================================================================

#' Preprocess CDR tIDE percent cover
#' NOTE: The previous version had duplicated sIDE biomass data here. This version
#' correctly extracts tIDE percent cover from the proper EDI entity.
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: date, plot, treatment, total_cover
preprocess_cdr_tide_pctcover <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI tIDE data has: date, plot, subplot, shelter, fertilized,
  # formerly_irrigated, taxa, cover
  # Create combined treatment and sum cover

  data <- data %>%
    mutate(across(where(is.character), trimws))

  # Build combined treatment column if individual factors exist
  if (all(c("shelter", "fertilized") %in% names(data))) {
    # Fix common typo: fomerly_irrigated
    irrig_col <- grep("irrigat", names(data), value = TRUE)[1]
    if (!is.na(irrig_col)) {
      data$formerly_irrigated <- data[[irrig_col]]
    } else {
      data$formerly_irrigated <- ""
    }

    data <- data %>%
      mutate(
        treatment = paste(shelter, fertilized, formerly_irrigated, sep = "_")
      )
  }

  result <- data %>%
    group_by(date, plot, treatment) %>%
    summarise(total_cover = sum(cover, na.rm = TRUE), .groups = "drop")

  result <- result %>%
    mutate(date = sub("^(\\d{4}).*", "\\1", as.character(date)))

  as.data.frame(result)
}

# =============================================================================
# CDR 6: Small Biodiversity Experiment Biomass (knb-lter-cdr.291.8)
# =============================================================================

#' Preprocess CDR Small Biodiversity Experiment aboveground biomass
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Date, Plot, Grazed, total_mass
preprocess_cdr_small_biodiv <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has individual species biomass records
  # Sum across taxa per Date/Plot/Grazed treatment

  # Identify columns - names may vary
  date_col <- grep("Date|Sampling.date|Year", names(data), value = TRUE)[1]
  plot_col <- grep("Plot.number|Plot", names(data), value = TRUE)[1]
  grazed_col <- grep("Grazed|grazed|Graze", names(data), value = TRUE)[1]
  biomass_col <- grep("Biomass|biomass|mass", names(data), value = TRUE)[1]

  # Convert date: YYMMDD format → year. as.Date() returns NA (not an error)
  # on mismatched formats, so fall back per-value rather than via tryCatch.
  raw_dates <- as.character(data[[date_col]])
  parsed <- as.Date(raw_dates, format = "%y%m%d")
  data$date_clean <- ifelse(!is.na(parsed),
                            format(parsed, "%Y"),
                            # If already a year or other format, extract the year
                            sub("^(\\d{4}).*", "\\1", raw_dates))

  result <- data %>%
    mutate(biomass_val = suppressWarnings(as.numeric(.data[[biomass_col]]))) %>%
    filter(!is.na(biomass_val)) %>%
    group_by(
      Date = date_clean,
      Plot = .data[[plot_col]],
      Grazed = .data[[grazed_col]]
    ) %>%
    summarise(total_mass = sum(biomass_val, na.rm = TRUE), .groups = "drop")

  as.data.frame(result)
}

# =============================================================================
# CDR 7: Percent Carbon E001 (knb-lter-cdr.472.8)
# =============================================================================

#' Preprocess CDR E001 percent carbon
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Field_Plot, NAdd, total_Carpercent
preprocess_cdr_pctcarbon <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Year, Field, Plot, NTrt, NAdd, %Carbon, Depth, etc.
  # Create Field_Plot identifier, sum %Carbon per grouping

  year_col <- grep("Year|year", names(data), value = TRUE)[1]
  field_col <- grep("Field|field", names(data), value = TRUE)[1]
  plot_col <- grep("Plot|plot", names(data), value = TRUE)[1]
  nadd_col <- grep("NAdd|Nadd|NitrAdd", names(data), value = TRUE)[1]
  carbon_col <- grep("Carbon|carbon|%C", names(data), value = TRUE)[1]

  data[[carbon_col]] <- suppressWarnings(as.numeric(data[[carbon_col]]))

  result <- data %>%
    filter(!is.na(.data[[carbon_col]])) %>%
    mutate(Field_Plot = paste(.data[[field_col]], .data[[plot_col]], sep = "_")) %>%
    group_by(
      Year = .data[[year_col]],
      Field_Plot,
      NAdd = .data[[nadd_col]]
    ) %>%
    summarise(total_Carpercent = sum(.data[[carbon_col]], na.rm = TRUE), .groups = "drop")

  as.data.frame(result)
}

# =============================================================================
# CDR 8: Soil Carbon Flux E004 (knb-lter-cdr.590.8)
# =============================================================================

#' Preprocess CDR E004 soil carbon flux
#' NOTE: Previous version summed SCF across points. The correct aggregation is
#' to AVERAGE across measurement points within a plot, not sum.
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Date, Plot, Cover_Fertilizer, total_SCF
preprocess_cdr_soil_carbon_flux <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Date, Plot, 1996 Canopy cover (in/out), Annual N fertilizer,
  # SCF (soil carbon flux), Point
  # Recode canopy cover, create combined treatment, average across points

  date_col <- grep("Date|Year|year", names(data), value = TRUE)[1]
  plot_col <- grep("Plot|plot", names(data), value = TRUE)[1]
  cover_col <- grep("Canopy|canopy|cover", names(data), value = TRUE)[1]
  fert_col <- grep("Nitrogen|fertilizer|Fertilizer|NAdd", names(data), value = TRUE)[1]
  scf_col <- grep("SCF|scf|flux|Flux", names(data), value = TRUE)[1]

  data[[scf_col]] <- suppressWarnings(as.numeric(data[[scf_col]]))

  # Recode canopy cover: in → shaded, out → unshaded
  if (!is.na(cover_col)) {
    data$cover_recode <- ifelse(
      grepl("in", data[[cover_col]], ignore.case = TRUE), "shaded", "unshaded"
    )
  } else {
    data$cover_recode <- ""
  }

  # Create combined treatment
  data$Cover_Fertilizer <- paste(data$cover_recode, data[[fert_col]], sep = "_")

  # Extract year from date
  data$date_year <- sub("^(\\d{4}).*", "\\1", as.character(data[[date_col]]))

  # Average (not sum) across measurement points within plot × treatment × date
  result <- data %>%
    filter(!is.na(.data[[scf_col]])) %>%
    group_by(
      Date = date_year,
      Plot = .data[[plot_col]],
      Cover_Fertilizer
    ) %>%
    summarise(total_SCF = mean(.data[[scf_col]], na.rm = TRUE), .groups = "drop")

  as.data.frame(result)
}
