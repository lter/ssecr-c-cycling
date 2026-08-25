# preprocess_utils.R
# Shared helpers for site preprocessing scripts.
# All preprocess_*.R files are sourced together by
# R/pipeline/preprocess_runner.R::load_preprocess_functions(), so functions
# defined here are available to every site script.

library(dplyr)

#' Summarize all numeric columns by group (sd, mean, sum)
#' Previously copy-pasted into several site scripts; centralized here so the
#' aggregation cannot drift between sites.
#' @param data data.frame
#' @param group_cols Character vector of grouping column names
#' @return data.frame with one row per group and {col}_{sd,mean,sum} columns
summarize_by_columns <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(where(is.numeric), list(
      sd = ~sd(.x, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE),
      sum = ~sum(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}
