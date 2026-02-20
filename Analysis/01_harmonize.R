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
