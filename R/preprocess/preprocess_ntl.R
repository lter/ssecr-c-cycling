# preprocess_ntl.R
# Source: knb-lter-ntl.413.2
# Cascade Project: Daily Bloom Data for Whole Lake Experiments 2011-2019
# Treatment: none vs added (whole-lake nutrient addition)
#
# Raw EDI layout (one row per lake-day):
#   Lake (L = Paul, R = Peter, T = Tuesday), Year, DOY, BGA_HYLB,
#   Manual_Chl, DO_Sat, pH
# Missing values are coded "NA" in the raw file; no numeric sentinel codes
# (-9999 etc.) occur, but they are converted defensively below.
#
# The raw file has NO treatment column. The legacy ready file was made by
# summarising every numeric column by Lake x Year (sd / mean / sum) and then
# adding N_addition BY HAND (legacy commit dbd619c). That hand-entered
# lake-year schedule is reproduced here as an explicit lookup table:
#   L (Paul)    : reference lake, never enriched
#   R (Peter)   : enriched 2013, 2014, 2015, 2019
#   T (Tuesday) : enriched 2013, 2014, 2015
# All other lake-years are "none".

library(dplyr)

#' Preprocess NTL Cascade bloom data from EDI raw download
#' @param raw_path Path to raw CSV from EDI
#' @return data.frame with one row per Lake x Year: Lake, N_addition, Year and
#'   {DOY,BGA_HYLB,Manual_Chl,DO_Sat,pH}_{sd,mean,sum}
preprocess_ntl_bloom <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE,
                   na.strings = c("NA", "NaN", "", "."))

  # Defensive missing-value handling (none of these codes occur in 413.2)
  measure_cols <- c("BGA_HYLB", "Manual_Chl", "DO_Sat", "pH")
  data <- data %>%
    mutate(across(all_of(measure_cols),
                  ~ ifelse(.x %in% c(-9999, -99999, 9999), NA_real_, as.numeric(.x))))

  # Nutrient-addition schedule (hand-entered in the legacy ready file)
  added <- data.frame(
    Lake = c("R", "R", "R", "R", "T", "T", "T"),
    Year = c(2013L, 2014L, 2015L, 2019L, 2013L, 2014L, 2015L),
    N_addition = "added",
    stringsAsFactors = FALSE
  )

  # Annual summaries (sd, mean, sum) of every numeric column per lake-year.
  # summarize_by_columns() comes from R/preprocess/preprocess_utils.R
  result <- data %>%
    summarize_by_columns(c("Lake", "Year")) %>%
    left_join(added, by = c("Lake", "Year")) %>%
    mutate(N_addition = ifelse(is.na(N_addition), "none", N_addition)) %>%
    # mean of an all-NA year is NaN; the ready file stores NA
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x))) %>%
    select(Lake, N_addition, Year, everything()) %>%
    arrange(Lake, Year)

  as.data.frame(result)
}
