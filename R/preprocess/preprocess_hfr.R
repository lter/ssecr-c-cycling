# preprocess_hfr.R
# Source: knb-lter-hfr.5.37
# Prospect Hill Soil Warming Experiment since 1991
# Treatment: C (control), DC (disturbance control), H (heated)
#
# Package hf005 holds more than one flux table and they are laid out
# differently; this function accepts both:
#   (a) coded table, 1991-2002 (the entity currently recorded in the registry):
#         year, rep, date, treatment (1/2/3), block (1-6), co2_flux, ch4_flux,
#         n2o_flux, temp_2cm, temp_4cm, grav_h2o_org, grav_h2o_min, vol_h2o
#       treatment codes: 1 = C, 2 = DC, 3 = H (verified against the legacy
#       ready file); plots are not given but follow from block x treatment.
#   (b) labelled table, 1991-2021 (the one the legacy ready file was made from):
#         year, ..., treatment ("C"/"DC"/"H"), plot (1-18), co2_flux, ...
# Only (b) reproduces the full 1991-2021 series.

library(dplyr)

# Plot number for each block (rows 1-6) x treatment (columns C, DC, H).
# Blocks are consecutive triplets of plots (block 1 = plots 1-3, ...).
hfr_plot_lookup <- matrix(
  c( 2,  3,  1,
     4,  5,  6,
     7,  9,  8,
    11, 10, 12,
    14, 13, 15,
    18, 17, 16),
  ncol = 3, byrow = TRUE,
  dimnames = list(block = 1:6, treatment = c("C", "DC", "H"))
)

#' Preprocess HFR soil respiration data from EDI raw download
#' @param raw_path Path to raw CSV from EDI (hf005 soil respiration entity)
#' @return data.frame with columns: year, treatment, plot, soil_res
preprocess_hfr_soil_respiration <- function(raw_path) {
  data <- read.csv(raw_path, stringsAsFactors = FALSE,
                   na.strings = c("", "NA", ".", "-9999", "-99999", "9999"),
                   strip.white = TRUE)
  names(data) <- gsub(".", "_", tolower(names(data)), fixed = TRUE)

  if (!all(c("year", "treatment", "co2_flux") %in% names(data))) {
    stop("preprocess_hfr_soil_respiration: expected columns year, treatment, ",
         "co2_flux; found: ", paste(names(data), collapse = ", "))
  }

  # Treatment: numeric codes -> labels; labels are trimmed (the 1991-2021 table
  # contains a few "DC " / "H " entries with trailing whitespace)
  trt_labels <- c("C", "DC", "H")
  if (is.numeric(data$treatment)) {
    data$treatment <- trt_labels[data$treatment]
  } else {
    data$treatment <- toupper(trimws(data$treatment))
  }
  if (any(!data$treatment %in% trt_labels)) {
    stop("preprocess_hfr_soil_respiration: unrecognised treatment code(s)")
  }

  # Plot: taken from the file if present, otherwise from block x treatment
  if (!"plot" %in% names(data)) {
    if (!"block" %in% names(data)) {
      stop("preprocess_hfr_soil_respiration: need a 'plot' or 'block' column")
    }
    data$plot <- as.integer(hfr_plot_lookup[cbind(data$block,
                                              match(data$treatment, trt_labels))])
  }

  # Annual mean CO2 flux per treatment x plot
  data <- data %>%
    mutate(co2_flux = suppressWarnings(as.numeric(co2_flux))) %>%
    filter(!is.na(co2_flux)) %>%
    # Heating began in July 1991; the two June 1991 sampling dates are
    # pre-treatment (heated/control ~ 1.0)
    filter(!(year == 1991 & "month" %in% names(.) & month < 7)) %>%
    group_by(year, treatment, plot) %>%
    summarise(soil_res = mean(co2_flux), .groups = "drop")

  as.data.frame(data)
}
