# utils_dates.R
# Date parsing utilities for the harmonized dataset

#' Parse dates flexibly across multiple formats
#' Handles: YYYY, YYYY-MM, YYYY-MM-DD, MM/DD/YYYY, DD/MM/YYYY, DD-MM-YYYY
#' @param date_string A single date string
#' @return A Date object or NA
parse_date_flexible <- function(date_string) {
  if (is.na(date_string) || date_string == "" || date_string == "NA") return(NA)
  date_string <- trimws(as.character(date_string))

  date_parsed <- tryCatch({
    if (grepl("^\\d{4}$", date_string)) {
      # YYYY -> January 1st of that year
      as.Date(paste0(date_string, "-01-01"))
    } else if (grepl("^\\d{4}-\\d{2}$", date_string)) {
      # YYYY-MM -> 1st of that month
      as.Date(paste0(date_string, "-01"))
    } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_string)) {
      # YYYY-MM-DD (ISO)
      as.Date(date_string, format = "%Y-%m-%d")
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_string)) {
      # M/D/YYYY or D/M/YYYY -- try M/D/YYYY first
      parsed <- as.Date(date_string, format = "%m/%d/%Y")
      if (is.na(parsed)) parsed <- as.Date(date_string, format = "%d/%m/%Y")
      parsed
    } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_string)) {
      # DD-MM-YYYY
      as.Date(date_string, format = "%d-%m-%Y")
    } else {
      as.Date(date_string)
    }
  }, error = function(e) NA)

  return(date_parsed)
}

#' Vectorized version of parse_date_flexible
#' @param date_strings Character vector of date strings
#' @return Date vector
parse_dates <- function(date_strings) {
  parsed <- sapply(date_strings, parse_date_flexible, USE.NAMES = FALSE)
  as.Date(parsed, origin = "1970-01-01")
}

#' Extract year from flexibly-formatted date strings
#' @param date_strings Character vector of date strings
#' @return Integer vector of years
extract_year <- function(date_strings) {
  as.integer(format(parse_dates(date_strings), "%Y"))
}
