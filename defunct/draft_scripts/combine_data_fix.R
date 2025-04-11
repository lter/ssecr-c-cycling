# Fix for data combination error in the harmonization pipeline
# This addresses the type mismatch when combining harmonized datasets

# Function to fix the type inconsistency when combining harmonized data
fix_data_combination <- function() {
  # Save the original run_harmonization_pipeline function
  original_run_harmonization <- run_harmonization_pipeline
  
  # Create a new version with a more robust data combination approach
  run_harmonization_pipeline <- function(data_dir, output_dir = "results") {
    # Create output directory if it doesn't exist
    if(!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Step 1: Discover datasets
    all_datasets <- discover_datasets(data_dir)
    
    if(is.null(all_datasets) || nrow(all_datasets) == 0) {
      stop("No datasets found in directory: ", data_dir)
    }
    
    # Save dataset metadata
    write_csv(all_datasets, file.path(output_dir, "dataset_metadata.csv"))
    
    # Step 2: Process each dataset
    message("Processing ", nrow(all_datasets), " datasets...")
    harmonized_list <- list()
    
    for(i in 1:nrow(all_datasets)) {
      meta <- all_datasets[i, ]
      
      # Load dataset
      dataset <- load_dataset(meta$filepath)
      
      # Skip if loading failed
      if(is.null(dataset)) next
      
      # Harmonize dataset
      harmonized <- harmonize_dataset(dataset, meta)
      
      # Add to the list if harmonization was successful
      if(!is.null(harmonized) && nrow(harmonized) > 0) {
        harmonized_list[[length(harmonized_list) + 1]] <- harmonized
      }
    }
    
    # Step 3: Combine all harmonized data with type correction
    message("Combining harmonized data...")
    if(length(harmonized_list) == 0) {
      stop("No datasets were successfully harmonized")
    }
    
    # Find all unique column names across all datasets
    all_cols <- unique(unlist(lapply(harmonized_list, names)))
    
    # Determine the most appropriate type for each column
    col_types <- list()
    
    for(col in all_cols) {
      # Check which datasets have this column
      has_col <- sapply(harmonized_list, function(df) col %in% names(df))
      
      if(sum(has_col) > 0) {
        # Extract column types from each dataset
        types <- sapply(harmonized_list[has_col], function(df) {
          if(is.character(df[[col]])) return("character")
          if(is.numeric(df[[col]])) return("numeric")
          if(is.logical(df[[col]])) return("logical")
          if(inherits(df[[col]], "Date")) return("Date")
          return("unknown")
        })
        
        # Choose the most appropriate type (prioritize character > numeric > logical)
        if("character" %in% types) {
          col_types[[col]] <- "character"
        } else if("numeric" %in% types) {
          col_types[[col]] <- "numeric"
        } else if("logical" %in% types) {
          col_types[[col]] <- "logical"
        } else if("Date" %in% types) {
          col_types[[col]] <- "Date"
        } else {
          col_types[[col]] <- "character"  # Default to character
        }
      }
    }
    
    # Fix each dataset to ensure consistent column types
    for(i in seq_along(harmonized_list)) {
      df <- harmonized_list[[i]]
      
      # Add missing columns
      for(col in all_cols) {
        if(!col %in% names(df)) {
          df[[col]] <- NA
        }
      }
      
      # Convert columns to the consistent type
      for(col in names(df)) {
        if(col %in% names(col_types)) {
          target_type <- col_types[[col]]
          
          # Convert to target type
          if(target_type == "character" && !is.character(df[[col]])) {
            df[[col]] <- as.character(df[[col]])
          } else if(target_type == "numeric" && !is.numeric(df[[col]])) {
            # Try to convert to numeric, but if it fails, use NA
            df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
          } else if(target_type == "logical" && !is.logical(df[[col]])) {
            df[[col]] <- as.logical(df[[col]])
          } else if(target_type == "Date" && !inherits(df[[col]], "Date")) {
            # Try to convert to Date if possible
            if(is.character(df[[col]])) {
              df[[col]] <- suppressWarnings(as.Date(df[[col]]))
            } else {
              df[[col]] <- as.Date(NA)
            }
          }
        }
      }
      
      # Update the harmonized list
      harmonized_list[[i]] <- df
    }
    
    # Combine the fixed datasets
    all_harmonized_data <- bind_rows(harmonized_list)
    
    # Save harmonized data
    write_csv(all_harmonized_data, file.path(output_dir, "all_harmonized_data.csv"))
    
    # Continue with the rest of the original function (effect sizes, meta-analysis, etc.)
    message("Calculating effect sizes...")
    effect_sizes <- calculate_effect_sizes(all_harmonized_data)
    
    # Save effect sizes if available
    if(!is.null(effect_sizes) && nrow(effect_sizes) > 0) {
      write_csv(effect_sizes, file.path(output_dir, "effect_sizes.csv"))
      
      # Step 5: Perform meta-analysis
      message("Performing meta-analysis...")
      meta_results <- perform_meta_analysis(effect_sizes, "both")
      
      # Summarize meta-analysis results
      if(length(meta_results) > 0) {
        meta_summary <- summarize_meta_results(meta_results)
        
        # Save meta-analysis summary
        write_csv(meta_summary, file.path(output_dir, "meta_analysis_summary.csv"))
        
        # Create forest plots
        message("Creating forest plots...")
        create_forest_plots(effect_sizes, meta_results, output_dir)
      } else {
        meta_summary <- NULL
        message("No meta-analyses could be performed")
      }
    } else {
      effect_sizes <- NULL
      meta_summary <- NULL
      message("No effect sizes could be calculated")
    }
    
    # Step 6: Create summary visualizations
    message("Creating data visualizations...")
    create_data_summary_plots(all_harmonized_data, output_dir)
    
    # Step 7: Generate HTML report
    message("Generating final report...")
    report_file <- create_html_report(
      all_harmonized_data, 
      effect_sizes, 
      meta_summary, 
      output_dir, 
      "Ecological Data Harmonization and Meta-Analysis Report"
    )
    
    # Return a list with all results
    return(list(
      datasets = all_datasets,
      harmonized_data = all_harmonized_data,
      effect_sizes = effect_sizes,
      meta_summary = meta_summary,
      report_file = report_file
    ))
  }
  
  # Return the new function
  return(run_harmonization_pipeline)
}

# Replace the original function with our patched version
run_harmonization_pipeline <- fix_data_combination()