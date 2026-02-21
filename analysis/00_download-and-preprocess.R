# 00_download-and-preprocess.R
# ===========================
# Entry point for the reproducible data pipeline.
# Downloads all datasets from EDI and other sources, then preprocesses
# each into standardized CSVs in data/ready/ for harmonization.
#
# Usage:
#   source("analysis/00_download-and-preprocess.R")
#
# Requirements:
#   - R packages: EDIutils, httr, jsonlite, digest, dplyr, tidyr, readr
#   - Internet connection (for EDI downloads)
#
# What this does:
#   1. Reads data/dataset_registry.csv (the authoritative source of truth)
#   2. Downloads each dataset from EDI (or direct URL for non-EDI sources)
#   3. Caches raw files in data/raw/ (gitignored) with JSON manifests
#   4. Runs site-specific preprocessing functions from R/preprocess/
#   5. Writes standardized CSVs to data/ready/ for ltertools::harmonize()
#
# To reprocess a specific dataset:
#   source("R/pipeline/preprocess_runner.R")
#   load_preprocess_functions()
#   registry <- read_registry()
#   preprocess_one("CDR_BioCON_biomass", registry)
#
# To force re-download everything:
#   run_pipeline(force_download = TRUE)
# ===========================

# Source the pipeline orchestrator (which sources all dependencies)
source("R/pipeline/preprocess_runner.R")

# Run the full pipeline
results <- run_pipeline()

# Print any failures for review
if (any(results$download_results$status != "OK")) {
  cat("\n!!! DOWNLOAD FAILURES - check above for details !!!\n")
}
if (any(results$preprocess_results$status != "OK" &
        results$preprocess_results$status != "SKIPPED")) {
  cat("\n!!! PREPROCESSING FAILURES - check above for details !!!\n")
}
