# preprocess_ntl.R
# Source: knb-lter-ntl.413.2
# Cascade Project: Daily Bloom Data for Whole Lake Experiments 2011-2019
# Treatment: none vs added (N addition to whole lakes)

library(dplyr)

#' Preprocess NTL Cascade bloom data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with aggregated annual summary columns
preprocess_ntl_bloom <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE)

  # Raw EDI has daily bloom data: Lake, N_addition, Year, DOY, BGA_HYLB,
  # Manual_Chl, DO_Sat, pH, etc.
  # Aggregate to annual summaries using SD, mean, sum for numeric cols

  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R
  result <- data %>%
    summarize_by_columns(c("Lake", "N_addition", "Year"))

  as.data.frame(result)
}
