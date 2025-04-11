# Run the LTER Data Harmonization Pipeline
# This script executes the enhanced data harmonization pipeline on LTER datasets

# Load the main script with all functions
source("draft_scripts/Enhanced_Ecological_Data_Harmonization_Pipeline.R")
source("draft_scripts/harmonization_fix.R")  # Fix for the first issue
source("draft_scripts/combine_data_fix.R")   # Fix for the type mismatch issue

# Set directory paths
data_dir <- "drive-download-20250404T194054Z-001"  # Directory containing the dataset files
output_dir <- "lter_meta_results"                  # Directory where results will be saved

# Run the pipeline
results <- run_harmonization_pipeline(data_dir, output_dir)
# Set directory paths
data_dir <- "drive-download-20250404T194054Z-001"  # Directory containing the dataset files
output_dir <- "lter_meta_results"                  # Directory where results will be saved

# Run the pipeline
results <- run_harmonization_pipeline(data_dir, output_dir)

# Print summary
cat("\n====== HARMONIZATION COMPLETE ======\n")
cat("Processed", nrow(results$datasets), "datasets\n")
cat("Harmonized", nrow(results$harmonized_data), "observations\n")

if(!is.null(results$effect_sizes)) {
  cat("Calculated", nrow(results$effect_sizes), "effect sizes\n")
} else {
  cat("No effect sizes could be calculated\n")
}

if(!is.null(results$meta_summary) && nrow(results$meta_summary) > 0) {
  cat("Performed", nrow(results$meta_summary), "meta-analyses\n")
  
  # Print significant results
  sig_results <- results$meta_summary[results$meta_summary$p_value < 0.05,]
  if(nrow(sig_results) > 0) {
    cat("\nSignificant Meta-Analysis Results (p < 0.05):\n")
    print(sig_results[, c("experiment_type", "response_variable", "effect_measure", 
                          "effect_size", "ci_lower", "ci_upper", "p_value")])
  } else {
    cat("No significant meta-analysis results found\n")
  }
} else {
  cat("No meta-analyses could be performed\n")
}

# Show path to the HTML report
if(!is.null(results$report_file)) {
  cat("\nHTML report generated:", results$report_file, "\n")
  cat("Open this file in a web browser to view the complete results\n")
}

cat("\n====================================\n")