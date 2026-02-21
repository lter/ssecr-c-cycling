# preprocess_arc.R
# Source: knb-lter-arc.10004.8
# Above ground plant biomass in mesic acidic tussock tundra 1982-2015
# Toolik Lake fertilization/warming/shade experiment
# Treatment: Control, NP, GH, GHNP, SH

library(dplyr)
library(tidyr)

#' Preprocess ARC biomass data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, Treatment, Biomass, block
preprocess_arc_biomass <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI data has wide format: Year, Site, Treatment, Species, Tissue,
  # B1Q1gm2, B1Q2gm2, ... B4Q5gm2 (4 blocks × 5 quadrats)
  # Filter to Toolik Tussock site, then aggregate:
  #   1. Sum across species and tissue types
  #   2. Average within blocks (across quadrats)
  #   3. Present per block

  data <- data %>%
    filter(grepl("Toolik Tussock", Site, ignore.case = TRUE))

  # Convert block/quadrat columns to numeric
  block_cols <- grep("^B[1-4]Q[1-5]gm2$", names(data), value = TRUE)
  for (col in block_cols) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }

  # Calculate block-level means (average across quadrats within each block)
  for (b in 1:4) {
    q_cols <- paste0("B", b, "Q", 1:5, "gm2")
    q_cols <- q_cols[q_cols %in% names(data)]
    if (length(q_cols) > 0) {
      data[[paste0("block_", b)]] <- rowMeans(data[, q_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Sum across species and tissue types per Year × Treatment × block
  block_mean_cols <- grep("^block_[1-4]$", names(data), value = TRUE)

  result <- data %>%
    group_by(Year, Treatment) %>%
    summarise(across(all_of(block_mean_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  # Pivot to long format: one row per Year × Treatment × block
  result_long <- result %>%
    pivot_longer(
      cols = all_of(block_mean_cols),
      names_to = "block",
      names_prefix = "block_",
      values_to = "Biomass"
    ) %>%
    mutate(block = paste("block", block))

  as.data.frame(result_long)
}
