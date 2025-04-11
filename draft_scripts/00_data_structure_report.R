# Dataset Structure Explorer
# This script will explore all CSV files in a directory and output their structure

# Load required libraries
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(knitr)

# Function to discover all dataset files in a directory
discover_datasets <- function(directory = ".", pattern = "*.csv|*.CSV") {
  # List all files matching the pattern
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  return(files)
}

# Function to safely read a CSV file
safe_read_csv <- function(file_path) {
  result <- tryCatch({
    data <- read_csv(file_path, show_col_types = FALSE)
    list(success = TRUE, data = data, error = NULL)
  }, error = function(e) {
    list(success = FALSE, data = NULL, error = e$message)
  })
  return(result)
}

# Function to explore a dataset and return its structure information
explore_dataset <- function(file_path) {
  # Get file name
  file_name <- basename(file_path)
  cat("\n\n====================================================\n")
  cat("Exploring file:", file_name, "\n")
  cat("====================================================\n\n")
  
  # Try to read the file
  read_result <- safe_read_csv(file_path)
  
  if (!read_result$success) {
    cat("ERROR: Failed to read file:", read_result$error, "\n")
    return(NULL)
  }
  
  data <- read_result$data
  
  # Display structure
  cat("STRUCTURE:\n")
  str_output <- capture.output(str(data))
  cat(paste(str_output, collapse = "\n"), "\n\n")
  
  # Display head
  cat("HEAD:\n")
  head_output <- capture.output(head(data))
  cat(paste(head_output, collapse = "\n"), "\n\n")
  
  # Display summary
  cat("SUMMARY:\n")
  summary_output <- capture.output(summary(data))
  cat(paste(summary_output, collapse = "\n"), "\n\n")
  
  # Return a concise summary
  return(list(
    file_name = file_name,
    num_rows = nrow(data),
    num_cols = ncol(data),
    col_names = names(data)
  ))
}

# Main function to explore all datasets
explore_all_datasets <- function(directory, output_file = "dataset_structure_report.txt") {
  # Open a connection to the output file
  sink(output_file)
  
  # Discover all datasets
  files <- discover_datasets(directory)
  
  if (length(files) == 0) {
    cat("No CSV files found in directory:", directory, "\n")
    sink() # Close the connection
    return(NULL)
  }
  
  cat("Found", length(files), "CSV files in directory:", directory, "\n")
  
  # Explore each dataset
  summaries <- map(files, explore_dataset)
  
  # Create a summary table
  summary_df <- data.frame(
    file_name = map_chr(summaries, ~ .x$file_name),
    num_rows = map_dbl(summaries, ~ .x$num_rows),
    num_cols = map_dbl(summaries, ~ .x$num_cols),
    stringsAsFactors = FALSE
  )
  
  # Add columns string (limited to first 5 for readability)
  summary_df$columns <- map_chr(summaries, function(x) {
    cols <- x$col_names
    if (length(cols) > 5) {
      paste0(paste(cols[1:5], collapse = ", "), ", ...")
    } else {
      paste(cols, collapse = ", ")
    }
  })
  
  # Display summary table
  cat("\n\nSUMMARY OF ALL DATASETS:\n")
  print(summary_df)
  
  # Close the connection to the output file
  sink()
  
  cat("Report has been saved to:", output_file, "\n")
  return(summary_df)
}

# Set the directory path where your data files are located
# You can change this to the actual directory where your files are stored
data_dir <- "drive-download-20250404T194054Z-001"  # Current directory

# Define the output file path
output_file <- "dataset_structure_report.txt"

# Run the data exploration and save to file
explore_all_datasets(data_dir, output_file)