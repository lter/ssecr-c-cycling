# preprocess_cdr.R
# 8 preprocessing functions for CDR (Cedar Creek) datasets
# Each dataset comes from a different EDI package and experiment

library(dplyr)

# =============================================================================
# CDR 1: BioCON Biomass (knb-lter-cdr.302.13)
# =============================================================================

#' Preprocess CDR BioCON aboveground biomass
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Date, Ring, Treatment, Biomass
preprocess_cdr_biocon_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Date/Year, Ring, CO2 Treatment, N Treatment, Species richness,
  # Aboveground biomass, etc.
  # Need to create combined treatment: e.g., "Cenrich Namb M"
  # Aggregate biomass across species to plot level

  # Identify the correct columns (names may vary by EDI version)
  # Expected: monoculture/species richness info, CO2 and N treatments
  # The preprocessing creates: Date (year), Ring, Treatment (combined), Biomass

  # Trim whitespace from character columns
  data <- data %>%
    mutate(across(where(is.character), trimws))

  # If the data already has Treatment and Biomass columns (pre-aggregated entity):
  if (all(c("Date", "Ring", "Treatment", "Biomass") %in% names(data))) {
    return(as.data.frame(data %>% select(Date, Ring, Treatment, Biomass)))
  }

  # Otherwise, build Treatment column from CO2/N treatment columns
  # Common column patterns in BioCON EDI data:
  co2_col <- grep("CO2|co2|Cenrich|Camb", names(data), value = TRUE)[1]
  n_col <- grep("^N$|NTrt|Nitrogen|Nenrich|Namb", names(data), value = TRUE)[1]
  sr_col <- grep("SR|Species.Rich|Monoculture|CountOfSpecies", names(data), value = TRUE)[1]
  biomass_col <- grep("Biomass|biomass|AbvBioAnnProd", names(data), value = TRUE)[1]
  date_col <- grep("Date|Year|year", names(data), value = TRUE)[1]
  ring_col <- grep("Ring|ring", names(data), value = TRUE)[1]

  result <- data %>%
    mutate(
      Treatment = paste(
        ifelse(!is.na(co2_col) & !is.null(data[[co2_col]]),
               data[[co2_col]], ""),
        ifelse(!is.na(n_col) & !is.null(data[[n_col]]),
               data[[n_col]], ""),
        ifelse(!is.na(sr_col) & !is.null(data[[sr_col]]),
               data[[sr_col]], "")
      )
    )

  # Aggregate if needed
  if (!is.null(biomass_col) && !is.null(date_col) && !is.null(ring_col)) {
    result <- result %>%
      group_by(
        Date = .data[[date_col]],
        Ring = .data[[ring_col]],
        Treatment
      ) %>%
      summarise(Biomass = sum(.data[[biomass_col]], na.rm = TRUE), .groups = "drop")
  }

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
  if (!is.null(carbon_col)) {
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
    if (!is.null(irrig_col)) {
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

  # Convert date: YYMMDD format → year
  data$date_clean <- tryCatch({
    # Try YYMMDD format
    dates <- as.Date(as.character(data[[date_col]]), format = "%y%m%d")
    format(dates, "%Y")
  }, error = function(e) {
    # If already year or other format, just extract year
    sub("^(\\d{4}).*", "\\1", as.character(data[[date_col]]))
  })

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
  if (!is.null(cover_col)) {
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
