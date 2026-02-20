library(dplyr)

summarize_by_columns <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(where(is.numeric), list(
      sd   = ~sd(.x, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE),
      sum  = ~sum(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}
