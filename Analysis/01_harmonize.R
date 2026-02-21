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
  select(source, site_type, experiment_type, stock_or_flux, is_case_study)
harmony <- harmony %>%
  left_join(meta_cols, by = "source")

# Trim whitespace in Treatment names (e.g., HFR has "DC " and "H ")
harmony$Treatment <- trimws(harmony$Treatment)

# HFR fix: exclude original "C" (Control) plots — they only ran 1991-2002 and overlap
# with DC (Disturbance Control) plots that run the full 1991-2021 series.
# DC plots have cables buried but not heated; they are the appropriate control.
harmony <- harmony %>% filter(!(site_abbr == "HFR" & Treatment == "C"))
harmony$Treatment[harmony$site_abbr == "HFR" & harmony$Treatment == "DC"] <- "C"
cat("HFR: Excluded original C plots; remapped DC -> C for full 1991-2021 time series\n")

# MCM fix: exclude "U" (Unamended) treatment — "W" (Water only) is the correct control
harmony <- harmony %>% filter(!(site_abbr == "MCM" & Treatment == "U"))
cat("MCM: Excluded U (Unamended); W (Water only) is the control\n")

# Exclude datasets that are not usable for treatment/control carbon analysis
# - GCE: vegetation percent cover, not carbon mass
# - CDR sIDE/tIDE: percent cover, not carbon
# - NTL: chlorophyll proxy, not direct carbon
# NOTE: MCM re-included using CO2 flux from knb-lter-mcm.4014.5
# NOTE: AND re-included using DBH from TV010 (knb-lter-and.2742.28) for all 3 watersheds
excluded_sources <- c(
  "GCE1.csv",
  "CDR_sIDEPercentCover_2016-2020_processed.csv",
  "CDR_tIDEPercentCover_2016-2020_processed.csv",
  "NTL_NutrientAddition1.csv"
)
n_before <- nrow(harmony)
harmony <- harmony %>% filter(!source %in% excluded_sources)
cat("Excluded", n_before - nrow(harmony), "rows from unusable datasets:",
    paste(excluded_sources, collapse = ", "), "\n")

# Fix Experiment_Type inconsistency: "Fertilizer" -> "Fertilization"
harmony$experiment_type <- gsub("^Fertilizer$", "Fertilization", harmony$experiment_type)

# Verify
cat("\nSite breakdown:\n")
site_summary <- harmony %>%
  group_by(site_abbr, source) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(site_abbr)
print(as.data.frame(site_summary))

# Check case-study sites
check_case_studies(harmony, metadata)

# Write output
output_path <- file.path("data", "harmonized", "harmonized_current.csv")
write.csv(harmony, output_path, row.names = FALSE)
cat("\nHarmonized data written to:", output_path, "\n")
cat("Total rows:", nrow(harmony), "\n")
cat("Total sources:", length(unique(harmony$source)), "\n")
