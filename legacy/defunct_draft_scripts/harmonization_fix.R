# Fix for the harmonization error in the pipeline
# This patch addresses the row mismatch issue in the harmonize_dataset function

# Locate the bug in the harmonize_dataset function:
# The error happens when creating the harmonized data frame with columns from different sources.
# The issue is likely with columns that don't exist (is_treatment, treatment_type, treatment_column)
# after the identify_treatments function doesn't find any treatment columns.

# Here's the revised harmonize_dataset function:

harmonize_dataset <- function(df, metadata) {
  # Skip if df is NULL or empty
  if(is.null(df) || nrow(df) == 0) {
    warning("Empty dataset: ", metadata$filename)
    return(NULL)
  }
  
  message("Harmonizing dataset: ", metadata$filename)
  
  # Step 1: Identify column types
  column_types <- identify_column_types(df)
  
  # Step 2: Standardize dates
  df <- standardize_dates(df, column_types$date_cols)
  
  # Step 3: Identify treatments
  treatment_result <- identify_treatments(df, column_types$treatment_cols)
  df <- treatment_result$df
  has_treatment <- treatment_result$has_treatment
  
  # Step 4: Identify response variables
  response_vars <- identify_response_variables(df, column_types$response_cols, metadata)
  
  # Step 5: Identify spatial hierarchy
  spatial_hierarchy <- identify_spatial_hierarchy(df, column_types$spatial_cols)
  
  # Step 6: Identify taxonomy
  taxonomy_col <- identify_taxonomy(df, column_types$taxonomy_cols)
  
  # If we didn't find any response variables, return NULL
  if(length(response_vars) == 0) {
    warning("No response variables identified for: ", metadata$filename)
    return(NULL)
  }
  
  # Create harmonized data frames for each response variable
  harmonized_list <- list()
  
  for(var_name in names(response_vars)) {
    var_info <- response_vars[[var_name]]
    
    # Skip if the column doesn't exist or is all NA
    if(!var_name %in% names(df) || all(is.na(df[[var_name]]))) next
    
    # Create base harmonized data - using a more robust approach
    # First start with core columns that must exist
    harmonized <- data.frame(
      source_file = metadata$filename,
      site = metadata$site_code,
      experiment_type = metadata$experiment_type,
      response_variable = var_info$label,
      response_value = df[[var_name]],
      unit = ifelse(!is.na(metadata$unit), metadata$unit, "unknown"),
      stringsAsFactors = FALSE
    )
    
    # Now add treatment-related columns if they exist in df
    # Otherwise create them with NA values
    if("is_treatment" %in% names(df)) {
      harmonized$is_treatment <- df$is_treatment
    } else {
      harmonized$is_treatment <- NA
    }
    
    if("treatment_type" %in% names(df)) {
      harmonized$treatment_type <- df$treatment_type
    } else {
      harmonized$treatment_type <- NA
    }
    
    if("treatment_column" %in% names(df)) {
      harmonized$treatment_column <- df$treatment_column
    } else {
      harmonized$treatment_column <- NA
    }
    
    # Add year if available
    if("year" %in% names(df)) {
      harmonized$year <- df$year
    }
    
    # Add spatial hierarchy columns
    for(level_name in names(spatial_hierarchy)) {
      col_name <- spatial_hierarchy[[level_name]]
      if(col_name %in% names(df)) {
        harmonized[[level_name]] <- df[[col_name]]
      }
    }
    
    # Add taxonomy if available
    if(!is.null(taxonomy_col) && taxonomy_col %in% names(df)) {
      harmonized$taxa_group <- df[[taxonomy_col]]
    }
    
    # Filter out rows with NA response values
    harmonized <- harmonized[!is.na(harmonized$response_value), ]
    
    # Add to the list
    harmonized_list[[length(harmonized_list) + 1]] <- harmonized
  }
  
  # Combine all harmonized data for this dataset
  if(length(harmonized_list) > 0) {
    return(bind_rows(harmonized_list))
  } else {
    warning("No valid response data for: ", metadata$filename)
    return(NULL)
  }
}

# To fix the issue:

# 1. Save this function in a file named "harmonization_fix.R"

# 2. In your main script, add these lines after loading the original script:
# source("harmonization_fix.R")  # This will replace the buggy function

# 3. Then run your pipeline as before:
# results <- run_harmonization_pipeline(data_dir, output_dir)