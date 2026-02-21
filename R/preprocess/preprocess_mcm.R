# preprocess_mcm.R
# Source: knb-lter-mcm.4013.6
# Stoichiometry Experiment in Taylor Valley, Antarctica 2007-2016
#
# BUG FIX: The filename was "MCM_SoilOC" but the response variable is actually
# Total_abundance_per_kg (invertebrate abundance), NOT soil organic carbon.
# The file keeps the same output name for backward compatibility with column_key.
#
# CONTROL NOTE: "W" = water only (the control per the team's control names doc).
# "C" = Carbon addition (NOT control!). "U" = nothing added, not even water.

library(dplyr)

#' Preprocess MCM stoichiometry experiment data from EDI
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with columns: Year, BLOCK_ID, TREATMENT, Total_abundance_per_kg
preprocess_mcm_stoichiometry <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has: Year, BLOCK_ID, TREATMENT, various invertebrate counts
  # (TOTAL_ROTIFERS, TOTAL nematodes, tardigrades, etc.)
  # Need to sum invertebrate groups into Total_abundance_per_kg

  # Identify relevant columns
  year_col <- grep("Year|year", names(data), value = TRUE)[1]
  block_col <- grep("BLOCK|block", names(data), value = TRUE)[1]
  treatment_col <- grep("TREATMENT|treatment", names(data), value = TRUE)[1]
  total_col <- grep("^TOTAL$|Total_abund", names(data), value = TRUE)[1]
  rotifer_col <- grep("ROTIFER|rotifer", names(data), value = TRUE)[1]

  # If there's a pre-computed total column, use it
  if (!is.null(total_col)) {
    abundance_col <- total_col
  } else if (!is.null(rotifer_col)) {
    # Sum the available invertebrate group columns
    invert_cols <- grep("TOTAL|nematode|tardigrade|rotifer", names(data),
                        value = TRUE, ignore.case = TRUE)
    data$total_abund <- rowSums(data[, invert_cols, drop = FALSE], na.rm = TRUE)
    abundance_col <- "total_abund"
  } else {
    stop("Cannot find invertebrate abundance columns in MCM data")
  }

  data[[abundance_col]] <- suppressWarnings(as.numeric(data[[abundance_col]]))

  result <- data %>%
    filter(!is.na(.data[[abundance_col]])) %>%
    group_by(
      Year = .data[[year_col]],
      BLOCK_ID = .data[[block_col]],
      TREATMENT = .data[[treatment_col]]
    ) %>%
    summarise(
      Total_abundance_per_kg = sum(.data[[abundance_col]], na.rm = TRUE),
      .groups = "drop"
    )

  as.data.frame(result)
}
