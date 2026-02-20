# utils_analysis.R
# Core analysis functions for relative response, trend classification, and detection time

library(dplyr)

# Centralized list of control treatment names (was defined 4+ times across scripts)
CONTROL_NAMES <- c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4",
                   "u u c", "W", "C C", "pre", "XXX", "WS08", "none",
                   "CONTROL", "Outside Juncus", "C(Control Plot)")

#' Calculate treatment response relative to control
#' @param data data.frame with source, Date, Treatment, and response columns
#' @param response_var Name of the response variable column
#' @param control_names Character vector of control treatment identifiers
#' @return data.frame with relative_response and log_response_ratio columns added
calculate_relative_response <- function(data, response_var = "Response.Variable",
                                        control_names = CONTROL_NAMES) {
  source("R/utils_dates.R", local = TRUE)

  # Parse dates
  data$Date_parsed <- parse_dates(data$Date)

  # Separate control and treatment data
  control_data <- data %>%
    filter(Treatment %in% control_names) %>%
    group_by(source, Date, Date_parsed) %>%
    summarise(control_mean = mean(.data[[response_var]], na.rm = TRUE),
              control_n = n(),
              .groups = "drop")

  treatment_data <- data %>%
    filter(!Treatment %in% control_names)

  # Join and calculate relative response
  relative_data <- treatment_data %>%
    left_join(control_data, by = c("source", "Date", "Date_parsed")) %>%
    filter(!is.na(control_mean), control_mean > 0, !is.na(.data[[response_var]])) %>%
    mutate(relative_response = .data[[response_var]] / control_mean,
           log_response_ratio = log(.data[[response_var]] / control_mean))

  return(relative_data)
}

#' Classify temporal trend for a single treatment time series
#' @param data data.frame with Date_parsed and mean_response columns
#' @param p_threshold Significance threshold (default 0.05)
#' @param cv_threshold CV threshold for stable vs variable (default 0.2)
#' @return data.frame with trend_class, slope, p_value, r_squared, cv, mean_ratio, n_timepoints
classify_trend <- function(data, p_threshold = 0.05, cv_threshold = 0.2) {
  if (nrow(data) < 3) {
    return(data.frame(
      trend_class = "insufficient_data",
      slope = NA_real_, p_value = NA_real_, r_squared = NA_real_,
      cv = NA_real_, mean_ratio = NA_real_, n_timepoints = nrow(data)
    ))
  }

  data$time_numeric <- as.numeric(data$Date_parsed - min(data$Date_parsed))

  lm_fit <- lm(mean_response ~ time_numeric, data = data)
  lm_summary <- summary(lm_fit)

  slope <- coef(lm_fit)[2]
  p_value <- coef(lm_summary)[2, 4]
  r_squared <- lm_summary$r.squared
  cv <- sd(data$mean_response, na.rm = TRUE) / mean(data$mean_response, na.rm = TRUE)
  mean_ratio <- mean(data$mean_response, na.rm = TRUE)

  if (p_value < p_threshold) {
    trend_class <- ifelse(slope > 0, "increasing", "decreasing")
  } else {
    trend_class <- ifelse(cv < cv_threshold, "stable", "variable")
  }

  data.frame(
    trend_class = trend_class,
    slope = slope, p_value = p_value, r_squared = r_squared,
    cv = cv, mean_ratio = mean_ratio, n_timepoints = nrow(data)
  )
}

#' Sequential analysis: how many timepoints needed to detect a significant trend?
#' @param data data.frame with Date_parsed and mean_response columns
#' @param p_threshold Significance threshold (default 0.05)
#' @return data.frame with detection time results
detect_trend_timepoints <- function(data, p_threshold = 0.05) {
  if (nrow(data) < 3) {
    return(data.frame(
      min_n_for_detection = NA_integer_, final_n = nrow(data),
      final_slope = NA_real_, final_p = NA_real_,
      time_to_detect = NA_real_, detected = FALSE
    ))
  }

  data <- data %>% arrange(Date_parsed)
  data$time_numeric <- as.numeric(data$Date_parsed - min(data$Date_parsed))

  detection_n <- NA_integer_
  for (i in 3:nrow(data)) {
    subset_data <- data[1:i, ]
    lm_fit <- lm(mean_response ~ time_numeric, data = subset_data)
    p_value <- summary(lm_fit)$coefficients[2, 4]
    if (p_value < p_threshold) {
      detection_n <- i
      break
    }
  }

  final_lm <- lm(mean_response ~ time_numeric, data = data)
  final_summary <- summary(final_lm)

  data.frame(
    min_n_for_detection = ifelse(is.na(detection_n), nrow(data), detection_n),
    final_n = nrow(data),
    final_slope = coef(final_lm)[2],
    final_p = final_summary$coefficients[2, 4],
    time_to_detect = ifelse(is.na(detection_n), NA_real_,
                            as.numeric(data$Date_parsed[detection_n] - data$Date_parsed[1]) / 365.25),
    detected = !is.na(detection_n)
  )
}

#' Count sign flips (trend reversals) in a time series
#' @param data data.frame with Date_parsed and mean_response columns
#' @return data.frame with n_flips and flip_years
count_sign_flips <- function(data) {
  data <- data %>% arrange(Date_parsed)
  if (nrow(data) < 3) return(data.frame(n_flips = NA_integer_, flip_years = NA_character_))

  signs <- sign(data$mean_response - 1)
  signs <- signs[signs != 0]

  if (length(signs) < 2) return(data.frame(n_flips = 0L, flip_years = NA_character_))

  flips <- sum(diff(signs) != 0)
  flip_indices <- which(diff(signs) != 0)
  flip_years <- if (length(flip_indices) > 0) {
    paste(format(data$Date_parsed[flip_indices], "%Y"), collapse = "; ")
  } else {
    NA_character_
  }

  data.frame(n_flips = as.integer(flips), flip_years = flip_years)
}

#' Calculate log-response ratio with variance
#' @param data data.frame with source, Treatment, means, response_sd, sample_size, and a time column
#' @param time_col Name of the time column
#' @param control_names Character vector of control treatment identifiers
#' @return data.frame with LRR and confidence intervals
calculate_lrr <- function(data, time_col = "Date", control_names = CONTROL_NAMES) {
  required_cols <- c("source", "Treatment", "means", "response_sd", "sample_size", time_col)
  if (!all(required_cols %in% colnames(data))) {
    stop(paste("Missing columns:", paste(setdiff(required_cols, colnames(data)), collapse = ", ")))
  }

  control_data <- data %>%
    filter(Treatment %in% control_names) %>%
    group_by(source, .data[[time_col]]) %>%
    slice(1) %>%
    ungroup() %>%
    select(source, all_of(time_col),
           control_mean = means, control_sd = response_sd, control_n = sample_size)

  results <- data %>%
    filter(!Treatment %in% control_names) %>%
    left_join(control_data, by = c("source", time_col)) %>%
    filter(!is.na(control_mean), control_mean > 0, means > 0) %>%
    mutate(
      log_response_ratio = log(means / control_mean),
      log_rr_variance = (response_sd^2 / (means^2 * sample_size)) +
        (control_sd^2 / (control_mean^2 * control_n)),
      log_rr_se = sqrt(log_rr_variance),
      log_rr_ci_lower = log_response_ratio - 1.96 * log_rr_se,
      log_rr_ci_upper = log_response_ratio + 1.96 * log_rr_se,
      response_ratio = exp(log_response_ratio),
      response_ratio_ci_lower = exp(log_rr_ci_lower),
      response_ratio_ci_upper = exp(log_rr_ci_upper)
    )

  return(results)
}
