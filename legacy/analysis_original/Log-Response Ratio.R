# Log-Response Ratio Calculator
# This script calculates log-response ratios for experimental treatments vs control
# within each experiment

# Load required libraries
library(dplyr)
library(tidyverse)

#read in data 
data <- read.csv("Analysis/LR_master_file.csv")


# Function to calculate log-response ratio and its variance by a time variable
calculate_lrr_by_time <- function(data, time_col = "Date") {
  
  # Ensure we have the required columns
  required_cols <- c("source", "Treatment", "means", "response_sd", "sample_size", time_col)
  
  if (!all(required_cols %in% colnames(data))) {
    stop(paste("Data must contain columns: source, Treatment, means, response_sd, sample_size, and", time_col))
  }
  
  # A vector of possible names for the control group to make the code cleaner
  control_names <- c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4", "u u c", 
                     "W", "C C", "pre", "XXX", "WS08", "none", "CONTROL", 
                     "Outside Juncus", "C(Control Plot)")
  
  # 1. Separate control and treatment data
  control_data <- data %>% 
    filter(Treatment %in% control_names)
  
  treatment_data <- data %>% 
    filter(!Treatment %in% control_names)
  
  # 2. Prepare the control data for joining
  # We select only the necessary columns and rename them to avoid conflicts after joining.
  # This groups by BOTH source and the time column.
  control_summary <- control_data %>%
    group_by(source, .data[[time_col]]) %>%
    # If multiple controls exist for a given year, this takes the first one, mimicking the original code's warning.
    slice(1) %>% 
    ungroup() %>%
    select(
      source, 
      {{time_col}}, 
      control_mean = means, 
      control_sd = response_sd, 
      control_n = sample_size
    )
  
  # 3. Join treatment data with its corresponding control data for each source and time point
  results <- treatment_data %>%
    left_join(control_summary, by = c("source", time_col)) %>%
    # Remove rows where a matching control was not found for that year
    filter(!is.na(control_mean)) %>%
    # 4. Calculate LRR and other metrics
    mutate(
      # Log-response ratio
      log_response_ratio = log(means / control_mean),
      
      # Variance of log-response ratio
      log_rr_variance = (response_sd^2 / (means^2 * sample_size)) + 
        (control_sd^2 / (control_mean^2 * control_n)),
      
      # Standard error
      log_rr_se = sqrt(log_rr_variance),
      
      # 95% Confidence intervals
      log_rr_ci_lower = log_response_ratio - 1.96 * log_rr_se,
      log_rr_ci_upper = log_response_ratio + 1.96 * log_rr_se,
      
      # Back-transformed response ratio and CIs
      response_ratio = exp(log_response_ratio),
      response_ratio_ci_lower = exp(log_rr_ci_lower),
      response_ratio_ci_upper = exp(log_rr_ci_upper)
    )
  
  return(results)
}

# Calculate the log response ratio for each year within each experiment
lrr_results <- calculate_lrr_by_time(data, time_col = "Date")

# View the results
print(lrr_results)

#write csv
write.csv(lrr_results, "Analysis/LRR_results.csv")

########################################################################################################################################
########################################################################################################################################

# Optional: Create a simple plot
  library(ggplot2)
  
  # Forest plot of log-response ratios
  plot <- ggplot(lrr_results, aes(x = log_response_ratio, y = interaction(source, Treatment), fill = Date, color = Date)) +
    geom_point(size = 2) +
    #geom_errorbarh(aes(xmin = log_rr_ci_lower, xmax = log_rr_ci_upper), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Log-Response Ratios with 95% Confidence Intervals",
      x = "Log-Response Ratio",
      y = "Experiment : Treatment",
      caption = "Dashed line at 0 indicates no effect"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  
  print(plot)
  
  # Save plot
  plot_file <- gsub("\\.[^.]*$", "_log_response_plot.png", file_path)
  ggsave(plot_file, plot, width = 10, height = 6, dpi = 300)
  cat("Plot saved to:", plot_file, "\n")