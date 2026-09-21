# preprocess_kbs.R
# Source: knb-lter-kbs.19.85, KBS datatable 39
#   "Annual Crops and Alfalfa/Switchgrass Biomass"
#   (https://lter.kbs.msu.edu/datatables/39)
# Main Cropping System Experiment (MCSE) 1990-2022
# Treatment: T1 conventional, T2 no-till, T3 reduced input, T4 biologically
#   based, T6 alfalfa/switchgrass, T21 (a few records only)
#
# NOTE: the package knb-lter-kbs.19 holds several data tables. The entity must
# be the crop biomass table (datatable 39); the species-code lookup table
# (datatable 36: Code, Family, Genus, ...) is NOT usable and is rejected below.

library(dplyr)

#' Preprocess KBS biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI / KBS (datatable 39)
#' @return data.frame with columns: Year, Treatment, Replicate, Biomass
preprocess_kbs_biomass <- function(raw_path) {
  # Raw layout: a block of "#"-prefixed metadata lines, the header row, a
  # "#"-prefixed units row, then one row per sample:
  #   Year, Campaign, Date, Treatment (T1..), Replicate (R1-R6),
  #   Station (S1-S5, or "unspecified" in early years), Species (crop),
  #   Fraction (SEED, STOVER, WHOLE, STOV_VEG, STOV_REP, STOVER-NONLEAF, LITTER),
  #   Biomass (g/m2; blank = missing)
  data <- read.csv(raw_path, stringsAsFactors = FALSE, comment.char = "#",
                   na.strings = c("", "NA", ".", "-9999", "-99999", "9999"),
                   strip.white = TRUE)

  # Column names differ in case between the KBS website and EDI copies
  canonical <- c("Year", "Treatment", "Replicate", "Station", "Species",
                 "Fraction", "Biomass")
  lower <- tolower(names(data))
  lower[lower %in% c("biomass_g_m2", "biomass_g")] <- "biomass"
  idx <- match(tolower(canonical), lower)
  if (anyNA(idx)) {
    stop("preprocess_kbs_biomass: raw file lacks column(s) ",
         paste(canonical[is.na(idx)], collapse = ", "),
         "; found: ", paste(names(data), collapse = ", "),
         ". Expected KBS datatable 39 (crop biomass), not another entity of ",
         "knb-lter-kbs.19 -- check edi_entity_id in data/dataset_registry.csv.")
  }
  data <- data[, idx]
  names(data) <- canonical

  # Aggregate as in the legacy script:
  #   sum across Fraction (and repeated cuttings within a year, e.g. alfalfa)
  #   -> mean across Station -> sum across Species
  data <- data %>%
    mutate(Biomass = suppressWarnings(as.numeric(Biomass))) %>%
    filter(!is.na(Biomass)) %>%
    group_by(Year, Treatment, Replicate, Station, Species) %>%
    summarise(Biomass = sum(Biomass), .groups = "drop") %>%
    group_by(Year, Treatment, Replicate, Species) %>%
    summarise(Biomass = mean(Biomass), .groups = "drop") %>%
    group_by(Year, Treatment, Replicate) %>%
    summarise(Biomass = sum(Biomass), .groups = "drop")

  as.data.frame(data)
}
