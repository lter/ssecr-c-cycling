# preprocess_luq.R
# Source: knb-lter-luq.164.678951 (GHG), knb-lter-luq.143.1058123 (seedling)
#
# BUG FIX: The previous version had the Treatment column mapped to
# "Reported_N2O_Flux_ng_N_cm.2_hr.1_mean" in the column key because
# the block→treatment mapping was lost. This version restores the correct
# treatment mapping from the CTE (Canopy Trimming Experiment).
#
# Treatment mapping (Block × Plot → Treatment):
#   Block A: Plot 1=Control, 2=Trim+clear, 3=Trim+debris, 4=No trim+debris
#   Block B: Plot 1=Control, 2=Trim+debris, 3=No trim+debris, 4=Trim+clear
#   Block C: Plot 1=No trim+debris, 2=Trim+debris, 3=Trim+clear, 4=Control

library(dplyr)
library(tidyr)

# Shared treatment mapping for CTE
luq_treatment_map <- function(block, plot) {
  dplyr::case_when(
    block == "A" & plot == 1 ~ "Control",
    block == "A" & plot == 2 ~ "Trim + clear",
    block == "A" & plot == 3 ~ "Trim + debris",
    block == "A" & plot == 4 ~ "No trim + debris",

    block == "B" & plot == 1 ~ "Control",
    block == "B" & plot == 2 ~ "Trim + debris",
    block == "B" & plot == 3 ~ "No trim + debris",
    block == "B" & plot == 4 ~ "Trim + clear",

    block == "C" & plot == 1 ~ "No trim + debris",
    block == "C" & plot == 2 ~ "Trim + debris",
    block == "C" & plot == 3 ~ "Trim + clear",
    block == "C" & plot == 4 ~ "Control",

    TRUE ~ NA_character_
  )
}

#' Helper: summarize all numeric columns by group
luq_summarize <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(where(is.numeric), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}

#' Preprocess LUQ GHG flux data from EDI
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with treatment and flux columns
preprocess_luq_ghg <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: DATE, Block-plot-rep (e.g., "A-1-1"), and flux columns
  # Parse the date, separate block-plot-rep, map to treatments

  # Fix date format
  date_col <- grep("DATE|Date|date", names(data), value = TRUE)[1]
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%m/%d/%Y")

  # Separate Block-plot-rep
  bpr_col <- grep("Block.plot.rep|Block-plot-rep", names(data), value = TRUE)[1]
  if (!is.null(bpr_col)) {
    data <- data %>%
      separate(!!bpr_col, into = c("Block", "Plot", "Rep"), sep = "-", remove = FALSE)
    data$Plot <- as.integer(data$Plot)
  }

  # Create Block_Plot and treatment
  data <- data %>%
    mutate(
      Block_Plot = paste(Block, Plot, sep = "-"),
      treatment = luq_treatment_map(Block, Plot)
    )

  # Add YEAR column
  data$YEAR <- as.integer(format(data[[date_col]], "%Y"))

  # Summarize by Block_Plot and DATE
  result <- luq_summarize(data, c("Block_Plot", date_col))

  as.data.frame(result)
}

#' Preprocess LUQ seedling data from EDI
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with treatment and seedling measurement columns
preprocess_luq_seedling <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: START_DATE, BLOCK, PLOT, SEEDLINGPLOT, HEIGHT, DIAMETER
  # Map to treatments using the CTE block-plot mapping

  data <- data %>%
    mutate(
      treatment = luq_treatment_map(BLOCK, PLOT)
    ) %>%
    select(START_DATE, BLOCK, PLOT, HEIGHT, DIAMETER, treatment) %>%
    filter(!is.na(treatment))

  result <- luq_summarize(data, c("BLOCK", "PLOT", "treatment", "START_DATE"))

  as.data.frame(result)
}
