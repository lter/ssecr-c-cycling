# Log-Response Ratio Calculator
# This script calculates log-response ratios for experimental treatments vs control
# within each experiment

# Load required libraries
library(dplyr)
library(tidyverse)

# Function to calculate log-response ratio and its variance
calculate_log_response_ratio <- function(data) {
  # Ensure we have the required columns
  required_cols <- c("source", "Treatment", "Response.Variable", "response_sd", "sample_size")
  
  if (!all(required_cols %in% colnames(data))) {
    stop("Data must contain columns: source, Treatment, Response.Variable, response_sd, sample_size")
  }
  
  # Calculate log-response ratio for each experiment
  results <- data %>%
    group_by(source) %>%
    do({
      experiment_data <- .
      
      # Identify control group (assuming it's coded as "control", "Control", "C", or "0")
      # You may need to modify this based on your control coding
      control_data <- experiment_data %>%
        filter(Treatment %in% c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4", "u u c", "W", "C C", "pre", "XXX", "WS08", "none", "CONTROL", "Outside Juncus", "C(Control Plot)"))
      
      if (nrow(control_data) == 0) {
        warning(paste("No control group found for experiment", unique(experiment_data$source)))
        return(data.frame())
      }
      
      if (nrow(control_data) > 1) {
        warning(paste("Multiple control groups found for experiment", unique(experiment_data$source), "- using first one"))
        control_data <- control_data[1, ]
      }
      
      # Get treatment groups (all non-control groups)
      treatment_data <- experiment_data %>%
        filter(!Treatment %in% c("control", "Control", "C", "0", "ctrl"))
      
      if (nrow(treatment_data) == 0) {
        warning(paste("No treatment groups found for experiment", unique(experiment_data$source)))
        return(data.frame())
      }
      
      # Calculate log-response ratio for each treatment vs control
      treatment_results <- treatment_data %>%
        rowwise() %>%
        mutate(
          # Log-response ratio
          log_response_ratio = log(Response.Variable / control_data$Response.Variable),
          
          # Variance of log-response ratio (delta method approximation)
          # Var(ln(X_t/X_c)) ≈ (SD_t/Mean_t)²/n_t + (SD_c/Mean_c)²/n_c
          log_rr_variance = (response_sd^2 / (Response.Variable^2 * sample_size)) + 
            (control_data$response_sd^2 / (control_data$Response.Variable^2 * control_data$sample_size)),
          
          # Standard error
          log_rr_se = sqrt(log_rr_variance),
          
          # 95% Confidence intervals
          log_rr_ci_lower = log_response_ratio - 1.96 * log_rr_se,
          log_rr_ci_upper = log_response_ratio + 1.96 * log_rr_se,
          
          # Back-transformed response ratio and CIs
          response_ratio = exp(log_response_ratio),
          response_ratio_ci_lower = exp(log_rr_ci_lower),
          response_ratio_ci_upper = exp(log_rr_ci_upper),
          
          # Control group information for reference
          control_mean = control_data$Response.Variable,
          control_sd = control_data$response_sd,
          control_n = control_data$sample_size
        ) %>%
        ungroup()
      
      return(treatment_results)
    }) %>%
    ungroup()
  
  return(results)
}

# Function to read and process data
process_log_response_data <- function(file_path, control_codes = c("control", "Control", "C", "0", "ctrl")) {
  
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
file_path <- "harmonized_aug7.csv"  # Change this to your actual file path

# If you need to specify different control codes, modify this vector
control_codes <- c("control", "Control", "C", "0", "ctrl")

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
  cat("Columns required: source, Treatment, Response.Variable, response_sd, sample_size\n")
  cat("Example:\n")
  cat("source | Treatment | Response.Variable | response_sd | sample_size\n")
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