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

# Function to read and process data
process_log_response_data <- function(file_path, control_codes = c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4", "u u c", "W", "C C", "pre", "XXX", "WS08", "none", "CONTROL", "Outside Juncus", "C(Control Plot)")) {
  
  # Read the data
  cat("Reading data from:", file_path, "\n")
  
  # Try different file formats
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    data <- read_csv(file_path)
  } else if (grepl("\\.(xlsx|xls)$", file_path, ignore.case = TRUE)) {
    library(readxl)
    data <- read_excel(file_path)
  } else {
    # Try as CSV by default
    data <- read_csv(file_path)
  }
  
  cat("Data dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  cat("Column names:", paste(colnames(data), collapse = ", "), "\n\n")
  
  # Calculate log-response ratios
  results <- calculate_log_response_ratio(data)
  
  return(results)
}

# Main execution
# MODIFY THIS PATH TO YOUR DATA FILE
file_path <- "harmonized_Oct10_v2.csv"  # Change this to your actual file path

# If you need to specify different control codes, modify this vector
control_codes <- c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4", "u u c", "W", "C C", "pre", "XXX", "WS08", "none", "CONTROL", "Outside Juncus", "C(Control Plot)")

# Process the data
if (file.exists(file_path)) {
  
  cat("=== LOG-RESPONSE RATIO ANALYSIS ===\n\n")
  
  # Calculate results
  results <- process_log_response_data(file_path, control_codes)
  
  # Display summary
  if (nrow(results) > 0) {
    cat("Results Summary:\n")
    cat("Number of experiments:", length(unique(results$source)), "\n")
    cat("Total treatment comparisons:", nrow(results), "\n\n")
    
    # Show first few results
    cat("First few results:\n")
    print(results %>% 
            select(source, Treatment, log_response_ratio, log_rr_se, 
                   response_ratio, response_ratio_ci_lower, response_ratio_ci_upper) %>%
            head(10))
    
    # Summary statistics
    cat("\n=== SUMMARY STATISTICS ===\n")
    cat("Log-Response Ratio Statistics:\n")
    cat("Mean:", round(mean(results$log_response_ratio, na.rm = TRUE), 4), "\n")
    cat("Median:", round(median(results$log_response_ratio, na.rm = TRUE), 4), "\n")
    cat("Range:", round(range(results$log_response_ratio, na.rm = TRUE), 4), "\n\n")
    
    cat("Response Ratio Statistics (back-transformed):\n")
    cat("Mean:", round(mean(results$response_ratio, na.rm = TRUE), 4), "\n")
    cat("Median:", round(median(results$response_ratio, na.rm = TRUE), 4), "\n")
    cat("Range:", round(range(results$response_ratio, na.rm = TRUE), 4), "\n\n")
    
    # Save results
    output_file <- gsub("\\.[^.]*$", "_log_response_ratios.csv", file_path)
    write_csv(results, output_file)
    cat("Results saved to:", output_file, "\n")
    
  } else {
    cat("No results calculated. Check your data format and control group coding.\n")
  }
  
} else {
  cat("ERROR: File not found:", file_path, "\n")
  cat("Please update the file_path variable with the correct path to your data file.\n")
  cat("\nExpected data format:\n")
  cat("Columns required: source, Treatment, means, response_sd, sample_size\n")
  cat("Example:\n")
  cat("source | Treatment | means | response_sd | sample_size\n")
  cat("exp1         | control        | 10.5          | 2.1         | 20\n")
  cat("exp1         | treatment1     | 12.3          | 2.5         | 18\n")
  cat("exp1         | treatment2     | 11.8          | 2.0         | 22\n")
  cat("exp2         | control        | 8.7           | 1.8         | 25\n")
  cat("exp2         | treatment1     | 9.4           | 2.2         | 23\n")
}

# Optional: Create a simple plot
if (exists("results") && nrow(results) > 0) {
  library(ggplot2)
  
  # Forest plot of log-response ratios
  plot <- ggplot(results, aes(x = log_response_ratio, y = interaction(source, Treatment))) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = log_rr_ci_lower, xmax = log_rr_ci_upper), height = 0.2) +
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
}