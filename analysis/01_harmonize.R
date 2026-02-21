# 01_harmonize.R
# Harmonize all processed datasets into a single analysis-ready file
# Uses the ltertools package for harmonization, then adds site metadata

library(dplyr)
library(ltertools)

source("R/utils_labels.R")

cat("=== HARMONIZATION ===\n")

# Read column key
key <- read.csv("data/column_key.csv", stringsAsFactors = FALSE)
cat("Column key loaded:", nrow(key), "mappings across", length(unique(key$source)), "sources\n")

# Harmonize using ltertools
harmony <- ltertools::harmonize(
  key = key,
  raw_folder = file.path("data", "ready"),
  data_format = "csv",
  quiet = TRUE
)

cat("Harmonized dataset:", nrow(harmony), "rows\n")

# Read site metadata and join
metadata <- get_site_metadata()

# Add site abbreviation
harmony$site_abbr <- source_to_abbr(harmony$source, metadata)

# Add site type, experiment type, and stock/flux from metadata
meta_cols <- metadata %>%
  select(source, site_type, experiment_type, stock_or_flux)
harmony <- harmony %>%
  left_join(meta_cols, by = "source")

# Trim whitespace in Treatment names (e.g., HFR has "DC " and "H ")
harmony$Treatment <- trimws(harmony$Treatment)

# CDR BioCON fix: normalize treatment names
# - "Cenriched " → "Cenrich" (inconsistent spelling across years)
# - Collapse multiple internal spaces to single space
harmony$Treatment <- gsub("\\s+", " ", harmony$Treatment)
harmony$Treatment <- gsub("Cenriched", "Cenrich", harmony$Treatment)

# HFR fix: exclude original "C" (Control) plots — they only ran 1991-2002 and overlap
# with DC (Disturbance Control) plots that run the full 1991-2021 series.
# DC plots have cables buried but not heated; they are the appropriate control.
harmony <- harmony %>% filter(!(site_abbr == "HFR" & Treatment == "C"))
harmony$Treatment[harmony$site_abbr == "HFR" & harmony$Treatment == "DC"] <- "C"
cat("HFR: Excluded original C plots; remapped DC -> C for full 1991-2021 time series\n")

# MCM fix: exclude "U" (Unamended) treatment — "W" (Water only) is the correct control
harmony <- harmony %>% filter(!(site_abbr == "MCM" & Treatment == "U"))
cat("MCM: Excluded U (Unamended); W (Water only) is the control\n")

# NOTE: BNZ NEE can be negative (negative NEE = carbon sink). This means treatment/control
# ratios and log-response ratios produce NaN when signs differ. These are silently dropped
# in downstream analyses. The trend classification still works on the non-NaN ratios.
# A difference-based approach would be more appropriate for signed fluxes but is not
# implemented to keep the analysis framework consistent across datasets.

# Exclude datasets: one dataset per LTER site
# CDR: keep BioCON only; VCR: keep 1st inundation (longer record)
# Also exclude CDR sIDE/tIDE percent cover (not carbon)
excluded_sources <- c(
  "CDR_sIDEPercentCover_2016-2020_processed.csv",
  "CDR_tIDEPercentCover_2016-2020_processed.csv",
  "CDR_SoilBiomass_1982-2018_%C_processed.csv",
  "CDR_AbovegroundBiomass_2016-2020_gm2_processed.csv",
  "CDR_AbovegroundBiomass_1995-2005_gm2_processed.csv",
  "CDR_PercentCarbon_1982-2011_processed.csv",
  "CDR_SoilCarbonFlux_1999-2005_processed.csv",
  "VCR_flora manipulation and inundation_live_dead_biomass_1998-2010_gm2_processed.csv"
)
n_before <- nrow(harmony)
harmony <- harmony %>% filter(!source %in% excluded_sources)
cat("Excluded", n_before - nrow(harmony), "rows (one dataset per site rule + non-carbon):\n")
cat("  CDR: kept BioCON; excluded E001, E002, E004, sIDE, Small Biodiv, pctcover\n")
cat("  VCR: kept 1st inundation (1994-2014); excluded 2nd (1998-2010)\n")
cat("NOTE: GCE (vegetation cover) and NTL (chlorophyll) included as carbon proxies\n")

# Fix Experiment_Type inconsistency: "Fertilizer" -> "Fertilization"
harmony$experiment_type <- gsub("^Fertilizer$", "Fertilization", harmony$experiment_type)

# Verify
cat("\nSite breakdown:\n")
site_summary <- harmony %>%
  group_by(site_abbr, source) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(site_abbr)
print(as.data.frame(site_summary))

# Write output
output_path <- file.path("data", "harmonized", "harmonized_current.csv")
write.csv(harmony, output_path, row.names = FALSE)
cat("\nHarmonized data written to:", output_path, "\n")
cat("Total rows:", nrow(harmony), "\n")
cat("Total sources:", length(unique(harmony$source)), "\n")
