# Data Harmonization for Standardized Effect Size Analysis
# This script flexibly harmonizes ecological datasets from different experiments

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(metafor)  # For effect size calculations
library(ggplot2)  # For visualization
library(purrr)    # For functional programming

source("FUNCTIONS_data_loadings.R")  # Load data loading functions
source("FUNCTIONS_data_harmonizing.R")  # Load harmonizing functions
source("FUNCTIONS_effect_sizes.R")  # Load effect size calculation functions
source("FUNCTIONS_metaanaysis.R")  # Load meta-analysis functions


# Set working directory to where your files are located
# setwd("your_directory_path")

#=========================#
# RUN THE HARMONIZATION PROCESS
#=========================#

# Set the directory path where your data files are located
data_dir <- "drive-download-20250404T194054Z-001"

# Discover all datasets in the specified directory
all_datasets <- discover_datasets(data_dir)

# View a summary of discovered datasets
cat("Discovered", nrow(all_datasets), "datasets\n")
print(head(all_datasets[, c("filename", "site", "experiment_type", "measured_variable", "unit")]))

# Create an empty list to store the harmonized datasets
harmonized_list <- list()

# Loop through each dataset and process it
for(i in 1:nrow(all_datasets)) {
  # Extract metadata for the current file
  meta <- all_datasets[i, ]
  
  # Load and process the dataset
  dataset <- load_dataset(meta$filepath, meta)
  
  # Harmonize the dataset if it loaded successfully
  if(!is.null(dataset)) {
    harmonized <- harmonize_dataset(dataset, meta)
    
    # Add to the list if harmonization was successful
    if(!is.null(harmonized)) {
      harmonized_list[[length(harmonized_list) + 1]] <- harmonized
    }
  }
}

# Combine all harmonized datasets
all_harmonized_data <- bind_rows(harmonized_list)

# Save the harmonized data to a CSV file
write_csv(all_harmonized_data, file.path(data_dir, "all_harmonized_ecological_data.csv"))

# Print a summary of the harmonization process
cat("Data Harmonization Complete\n")
cat("Processed", nrow(all_datasets), "datasets\n")
cat("Harmonized", nrow(all_harmonized_data), "observations\n")

# Calculate effect sizes if there are sufficient harmonized observations
if(nrow(all_harmonized_data) > 0) {
  # Calculate effect sizes
  effect_sizes <- calculate_effect_sizes(all_harmonized_data)
  
  # Save effect sizes
  write_csv(effect_sizes, file.path(data_dir, "standardized_effect_sizes.csv"))
  
  # Print summary
  cat("Calculated effect sizes for", nrow(effect_sizes), "comparisons\n")
  
  # Perform meta-analysis if there are sufficient effect sizes
  if(nrow(effect_sizes) >= 3) {
    meta_results <- meta_analysis(effect_sizes)
    meta_summary <- summarize_meta_results(meta_results)
    
    # Save meta-analysis results
    write_csv(meta_summary, file.path(data_dir, "meta_analysis_summary.csv"))
    
    # Print summary
    cat("Performed meta-analysis for", length(meta_results), "experiment type x response variable combinations\n")
    
    # Create forest plots
    create_forest_plots(effect_sizes, meta_results)
    
    # Create summary visualization of effect sizes
    effect_size_plot <- ggplot(effect_sizes, aes(x = experiment_type, y = hedges_g, color = site)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~response_variable, scales = "free_y") +
      labs(
        title = "Standardized Effect Sizes by Experiment Type and Response Variable",
        subtitle = "Hedges' g (positive values indicate treatment > control)",
        x = "Experiment Type",
        y = "Hedges' g",
        color = "Site"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    # Save the effect size visualization
    ggsave(file.path(data_dir, "effect_size_summary.png"), 
           effect_size_plot, width = 12, height = 8)
    
    # Create lnRR visualization
    lnrr_plot <- ggplot(effect_sizes, aes(x = experiment_type, y = lnRR, color = site)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~response_variable, scales = "free_y") +
      labs(
        title = "Log Response Ratios by Experiment Type and Response Variable",
        subtitle = "ln(Treatment/Control) (positive values indicate treatment > control)",
        x = "Experiment Type",
        y = "ln(Response Ratio)",
        color = "Site"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    # Save the lnRR visualization
    ggsave(file.path(data_dir, "lnrr_summary.png"), 
           lnrr_plot, width = 12, height = 8)
  } else {
    cat("Not enough effect sizes for meta-analysis (minimum 3 required)\n")
  }
} else {
  cat("No harmonized data available for effect size calculation\n")
}

# Create a summary of all datasets
dataset_summary <- data.frame(
  site = all_datasets$site,
  experiment_type = all_datasets$experiment_type,
  measured_variable = all_datasets$measured_variable,
  time_period = all_datasets$time_period,
  unit = all_datasets$unit,
  status = NA,
  observations = NA,
  response_vars = NA,
  treatments = NA,
  stringsAsFactors = FALSE
)

# Fill in the summary with processing results
for(i in 1:nrow(all_datasets)) {
  idx <- which(harmonized_list %>% 
                 map(~unique(.x$source_file)) %>% 
                 map_lgl(~all_datasets$filename[i] %in% .x))
  
  if(length(idx) > 0) {
    harm_data <- harmonized_list[[idx[1]]]
    dataset_summary$status[i] <- "Processed"
    dataset_summary$observations[i] <- nrow(harm_data)
    dataset_summary$response_vars[i] <- paste(unique(harm_data$response_variable), collapse = ", ")
    dataset_summary$treatments[i] <- paste(unique(harm_data$treatment_type), collapse = ", ")
  } else {
    dataset_summary$status[i] <- "Failed"
  }
}

# Save dataset summary
write_csv(dataset_summary, file.path(data_dir, "dataset_processing_summary.csv"))

# Print some final statistics
cat("\nSummary of Harmonization Results:\n")
cat("Total datasets:", nrow(all_datasets), "\n")
cat("Successfully processed:", sum(dataset_summary$status == "Processed"), "\n")
cat("Failed to process:", sum(dataset_summary$status == "Failed"), "\n")
cat("Total observations in harmonized data:", nrow(all_harmonized_data), "\n")

# Create a summary visualization of the harmonized datasets
if(nrow(all_harmonized_data) > 0) {
  # Count observations by site and experiment type
  dataset_counts <- all_harmonized_data %>%
    group_by(site, experiment_type) %>%
    summarize(observations = n(), .groups = "drop")
  
  # Create visualization
  dataset_viz <- ggplot(dataset_counts, aes(x = site, y = observations, fill = experiment_type)) +
    geom_col(position = "stack") +
    labs(
      title = "Number of Observations by Site and Experiment Type",
      x = "Site",
      y = "Number of Observations",
      fill = "Experiment Type"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  # Save visualization
  ggsave(file.path(data_dir, "dataset_summary.png"), 
         dataset_viz, width = 10, height = 6)
}

cat("\nAll results have been saved to:", data_dir, "\n")