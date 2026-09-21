# utils_analysis.R
# Core analysis functions for relative response, trend classification, and detection time

library(dplyr)

# Centralized list of control treatment names (was defined 4+ times across scripts)
CONTROL_NAMES <- c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4",
                   "u u c", "W", "C C", "pre", "WS08", "none",
                   "CONTROL", "No wrack", "(C)Control Plot",
                   "shaded_0", "unshaded_0", "X")

# Per-source overrides: sources where the default control names would be wrong.
# MCM: "C" is a carbon (mannitol) addition treatment, NOT a control — only
# "W" (water only) is the control (see R/preprocess/preprocess_mcm.R).
CONTROL_OVERRIDES <- list(
  "MCM_CO2flux_2003-2010_processed.csv" = "W"
)

# CDR BioCON: the experiment is several nested designs that use different plot
# sets - the main CO2 x N experiment ("M"), the four-species functional-group plots ("S"), and the
# water (2007-) and water x warming (2012-) manipulations, which run in the
# 9-species plots. A treatment is only comparable with the ambient control of
# its own design, so controls are matched within a stratum.
CDR_SOURCE <- "CDR_Biomass_1998-2021_gm2_processed.csv"

cdr_stratum <- function(treatment) {
  vapply(strsplit(as.character(treatment), " +"), function(tok) {
    paste(c(tok[3],
            if (any(grepl("^H2O", tok))) "water",
            if (any(grepl("^HT", tok))) "warming"), collapse = "+")
  }, character(1))
}

cdr_is_control <- function(treatment) {
  vapply(strsplit(as.character(treatment), " +"), function(tok) {
    tok[1] == "Camb" && tok[2] == "Namb" &&
      all(tok[grepl("^H2O", tok)] == "H2Oamb") &&
      all(tok[grepl("^HT", tok)] == "HTamb")
  }, logical(1))
}

# Sources whose replicates sit in blocks with very different baselines (reefs,
# forest stands, lake basins) and whose treatments are not present in every
# block: a treatment replicate is compared with the control of its own block.
BLOCK_MATCHED_SOURCES <- c("SBC", "HBR", "MCM")   # matched on the source's site prefix

#' Stratum within which a treatment is compared with its control.
#' One stratum per source, except (a) CDR, which holds several nested designs,
#' and (b) block-matched sources, where the stratum is the replicate block
#' (SBC reef, HBR stand, MCM lake basin).
control_stratum <- function(source, treatment, replicate = NULL) {
  out <- rep("all", length(source))
  idx <- source == CDR_SOURCE
  if (any(idx)) out[idx] <- cdr_stratum(treatment[idx])
  if (!is.null(replicate)) {
    site <- substr(source, 1, 3)
    blk <- site %in% c("SBC", "HBR")
    out[blk] <- as.character(replicate[blk])
    mcm <- site == "MCM"
    out[mcm] <- sub(" .*$", "", as.character(replicate[mcm]))   # "Bonney 3" -> "Bonney"
  }
  out
}

#' Determine whether each row is a control, honoring per-source overrides
#' @param source Character vector of source filenames
#' @param treatment Character vector of treatment labels
#' @param control_names Default control labels
is_control <- function(source, treatment, control_names = CONTROL_NAMES) {
  ctrl <- treatment %in% control_names
  for (src in names(CONTROL_OVERRIDES)) {
    idx <- source == src
    if (any(idx)) ctrl[idx] <- treatment[idx] %in% CONTROL_OVERRIDES[[src]]
  }
  idx <- source == CDR_SOURCE
  if (any(idx)) ctrl[idx] <- cdr_is_control(treatment[idx])
  ctrl
}

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
  data$control_stratum <- control_stratum(data$source, data$Treatment, data$Replicate)

  control_data <- data %>%
    filter(is_control(source, Treatment, control_names)) %>%
    group_by(source, control_stratum, Date, Date_parsed) %>%
    summarise(control_mean = mean(.data[[response_var]], na.rm = TRUE),
              control_n = n(),
              .groups = "drop")

  treatment_data <- data %>%
    filter(!is_control(source, Treatment, control_names))

  # Join and calculate relative response
  relative_data <- treatment_data %>%
    left_join(control_data, by = c("source", "control_stratum", "Date", "Date_parsed")) %>%
    filter(!is.na(control_mean), control_mean > 0, !is.na(.data[[response_var]])) %>%
    mutate(relative_response = .data[[response_var]] / control_mean,
           log_response_ratio = log(.data[[response_var]] / control_mean))

  # Guard against silent data loss: a source disappears entirely when its
  # control and treatment dates never coincide (the join matches on exact Date)
  dropped_sources <- setdiff(unique(treatment_data$source), unique(relative_data$source))
  if (length(dropped_sources) > 0) {
    warning("Sources dropped entirely (no control measured on any treatment date): ",
            paste(dropped_sources, collapse = ", "))
  }

  return(relative_data)
}

#' Classify temporal trend for a single treatment time series
#' @param data data.frame with Date_parsed and mean_response columns
#' @param p_threshold Significance threshold (default 0.05)
#' @param cv_threshold CV threshold for stable vs variable (default 0.3)
#' @return data.frame with trend_class, slope, p_value, r_squared, cv, mean_ratio, n_timepoints, year_span
classify_trend <- function(data, p_threshold = 0.05, cv_threshold = 0.3) {
  if (nrow(data) < 3) {
    return(data.frame(
      trend_class = "insufficient_data",
      slope = NA_real_, p_value = NA_real_, r_squared = NA_real_,
      cv = NA_real_, mean_ratio = NA_real_, n_timepoints = nrow(data),
      year_span = NA_real_
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
  year_span <- as.numeric(max(data$Date_parsed) - min(data$Date_parsed)) / 365.25

  # A p-value can be NA/NaN (e.g., constant response or all measurements on one
  # date); treat that as non-significant rather than crashing the group_modify
  if (!is.na(p_value) && p_value < p_threshold) {
    trend_class <- ifelse(slope > 0, "increasing", "decreasing")
  } else {
    trend_class <- ifelse(cv < cv_threshold, "stable", "variable")
  }

  data.frame(
    trend_class = trend_class,
    slope = slope, p_value = p_value, r_squared = r_squared,
    cv = cv, mean_ratio = mean_ratio, n_timepoints = nrow(data),
    year_span = round(year_span, 1)
  )
}

#' Sequential analysis: how many timepoints needed to detect a significant trend?
#' Also computes measurement frequency/density and study duration metrics to
#' disentangle sampling effort from temporal extent.
#' @param data data.frame with Date_parsed and mean_response columns
#' @param p_threshold Significance threshold (default 0.05)
#' @return data.frame with detection time results plus duration/density columns
detect_trend_timepoints <- function(data, p_threshold = 0.05) {
  if (nrow(data) < 3) {
    return(data.frame(
      min_n_for_detection = NA_integer_, final_n = nrow(data),
      final_slope = NA_real_, final_p = NA_real_,
      time_to_detect = NA_real_, detected = FALSE,
      total_duration_yr = NA_real_,
      meas_density_per_yr = NA_real_,
      median_interval_days = NA_real_,
      detect_density_per_yr = NA_real_
    ))
  }

  data <- data %>% arrange(Date_parsed)
  data$time_numeric <- as.numeric(data$Date_parsed - min(data$Date_parsed))

  # Measurement density metrics for the full time series
  total_span_days <- as.numeric(max(data$Date_parsed) - min(data$Date_parsed))
  total_duration_yr <- total_span_days / 365.25
  meas_density_per_yr <- if (total_duration_yr > 0) nrow(data) / total_duration_yr else NA_real_
  intervals <- diff(sort(as.numeric(data$Date_parsed)))
  median_interval_days <- median(intervals, na.rm = TRUE)

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

  detect_dur_yr <- if (!is.na(detection_n)) {
    as.numeric(data$Date_parsed[detection_n] - data$Date_parsed[1]) / 365.25
  } else {
    NA_real_
  }
  detect_density <- if (!is.na(detection_n) && !is.na(detect_dur_yr) && detect_dur_yr > 0) {
    detection_n / detect_dur_yr
  } else {
    NA_real_
  }

  data.frame(
    min_n_for_detection = ifelse(is.na(detection_n), nrow(data), detection_n),
    final_n = nrow(data),
    final_slope = coef(final_lm)[2],
    final_p = final_summary$coefficients[2, 4],
    time_to_detect = detect_dur_yr,
    detected = !is.na(detection_n),
    total_duration_yr = total_duration_yr,
    meas_density_per_yr = meas_density_per_yr,
    median_interval_days = median_interval_days,
    detect_density_per_yr = detect_density
  )
}

#' Count sign flips (trend reversals) in a time series
#' @param data data.frame with Date_parsed and mean_response columns
#' @return data.frame with n_flips and flip_years
count_sign_flips <- function(data) {
  data <- data %>% arrange(Date_parsed)
  if (nrow(data) < 3) return(data.frame(n_flips = NA_integer_, flip_years = NA_character_))

  signs <- sign(data$mean_response - 1)
  # Drop exact-1 responses but keep dates aligned with the filtered signs
  nonzero <- signs != 0
  signs_nz <- signs[nonzero]
  dates_nz <- data$Date_parsed[nonzero]

  if (length(signs_nz) < 2) return(data.frame(n_flips = 0L, flip_years = NA_character_))

  flip_indices <- which(diff(signs_nz) != 0)
  flips <- length(flip_indices)
  flip_years <- if (flips > 0) {
    # Report the year at which the flipped sign is first observed
    paste(format(dates_nz[flip_indices + 1], "%Y"), collapse = "; ")
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

  # Pool all control rows per source x time (there can be several control
  # treatment labels or replicate groups); combine as a single weighted group
  # rather than arbitrarily taking the first row
  if (!"control_stratum" %in% names(data)) {
    data$control_stratum <- control_stratum(data$source, data$Treatment)
  }

  control_data <- data %>%
    filter(is_control(source, Treatment, control_names)) %>%
    group_by(source, control_stratum, .data[[time_col]]) %>%
    summarise(
      control_n = sum(sample_size),
      control_mean = sum(means * sample_size) / sum(sample_size),
      control_sd = if (n() == 1) first(response_sd) else {
        # Combined-groups SD: within-group + between-group variance
        sqrt((sum((sample_size - 1) * response_sd^2) +
                sum(sample_size * (means - sum(means * sample_size) / sum(sample_size))^2)) /
               (sum(sample_size) - 1))
      },
      .groups = "drop"
    ) %>%
    select(source, control_stratum, all_of(time_col), control_mean, control_sd, control_n)

  results <- data %>%
    filter(!is_control(source, Treatment, control_names)) %>%
    left_join(control_data, by = c("source", "control_stratum", time_col)) %>%
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
