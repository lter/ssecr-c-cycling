# preprocess_and.R
# Sources:
#   AND TP114 from andlter.forestry.oregonstate.edu (experimental: WS06, WS07)
#   knb-lter-and.2742.28 (control: WS08)
#
# NOTE: The experimental data uses DBA (diameter at breast age?) while the
# control data uses DBH (diameter at breast height). These are different metrics
# and may not be directly comparable. This is documented in the registry.
#
# Treatment: WATERSHED (WS06=clearcut 1974, WS07=shelterwood cut, WS08=control)

library(dplyr)

#' Helper: summarize all numeric columns by group
and_summarize <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(where(is.numeric), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}

#' Preprocess AND plant biomass data
#' Merges experimental (WS06/07) and control (WS08) data
#' @param raw_path_experimental Path to experimental CSV (TP114 DBA/Height data)
#' @param raw_path_control Path to control CSV (knb-lter-and.2742.28 DBH data)
#' @return data.frame with merged summary statistics
preprocess_and_biomass <- function(raw_path_experimental, raw_path_control) {
  data_exp <- read.csv(raw_path_experimental, stringsAsFactors = FALSE)
  data_control <- read.csv(raw_path_control, stringsAsFactors = FALSE)

  # Filter control to match experimental time period (post 2002-06-18)
  date_col <- grep("SAMPLEDATE|SampleDate|Date", names(data_control), value = TRUE)[1]
  data_control[[date_col]] <- as.Date(data_control[[date_col]], format = "%m/%d/%Y")
  data_control <- data_control %>%
    filter(.data[[date_col]] > as.Date("2002-06-18"))

  # Add watershed designation to control
  data_control$WATERSHED <- "WS08"

  # Bind experimental and control
  # Use common columns (handle DBA vs DBH column difference)
  common_cols <- c("WATERSHED", "PLOT", "YEAR", "SAMPLEDATE")

  # Add DBA/HEIGHT/DBH as available
  if ("DBA" %in% names(data_exp)) data_exp_select <- data_exp %>%
    select(any_of(c(common_cols, "DBA", "HEIGHT", "DBH", "PLOTID")))
  if ("DBH" %in% names(data_control)) data_control_select <- data_control %>%
    select(any_of(c(common_cols, "DBA", "HEIGHT", "DBH", "PLOTID")))

  data_join <- bind_rows(data_exp_select, data_control_select)

  # Summarize by date × watershed × plot × year
  result <- and_summarize(
    data_join %>% select(SAMPLEDATE, WATERSHED, PLOT, YEAR,
                         any_of(c("DBA", "HEIGHT", "DBH"))),
    c("SAMPLEDATE", "WATERSHED", "PLOT", "YEAR")
  )

  as.data.frame(result)
}
