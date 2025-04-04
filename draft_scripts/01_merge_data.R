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

# Set working directory to where your files are located
# setwd("your_directory_path")

#=========================#
# DATA LOADING FUNCTIONS
#=========================#

# Function to parse file metadata from filenames
parse_filename_metadata <- function(filename) {
  # Remove file extension
  base_name <- tools::file_path_sans_ext(filename)
  
  # Split by underscores and extract components
  parts <- str_split(base_name, "_", simplify = TRUE)
  
  # Extract site code (usually first component)
  site_code <- parts[1]
  
  # Extract experiment type (usually second component or combination of components)
  # This is a simplification - may need adjustment based on actual naming patterns
  experiment_type <- if(ncol(parts) > 1) paste(parts[2:min(3, ncol(parts))], collapse = "_") else NA_character_
  
  # Extract measured variable
  measured_var_idx <- which(grepl("biomass|cover|height|chla|flux|stock|parameters|measurements", 
                                  parts, ignore.case = TRUE))
  measured_var <- if(length(measured_var_idx) > 0) parts[measured_var_idx[1]] else NA_character_
  
  # Extract time period (look for year patterns like 20162020, 19992014, etc.)
  time_period_idx <- which(grepl("^[0-9]{4,}", parts) | grepl("[0-9]{4}$", parts))
  time_period <- if(length(time_period_idx) > 0) parts[time_period_idx[1]] else NA_character_
  
  # Extract unit (last component or component with unit patterns)
  unit_idx <- which(grepl("gm2|mm|cm|percent", parts, ignore.case = TRUE))
  unit <- if(length(unit_idx) > 0) parts[unit_idx[1]] else NA_character_
  
  # Clean up unit string
  if(!is.na(unit)) {
    # Map common unit patterns to standardized units
    unit <- case_when(
      grepl("gm2", unit, ignore.case = TRUE) ~ "g/m2",
      grepl("mm", unit, ignore.case = TRUE) ~ "mm",
      grepl("cm", unit, ignore.case = TRUE) ~ "cm",
      grepl("percent", unit, ignore.case = TRUE) ~ "percent",
      TRUE ~ unit
    )
  }
  
  # Return a list of metadata
  return(list(
    site_code = site_code,
    experiment_type = experiment_type,
    measured_variable = measured_var,
    time_period = time_period,
    unit = unit,
    full_filename = filename
  ))
}

# Function to identify date-time columns in a dataframe
identify_datetime_columns <- function(df) {
  # Common date column names
  date_patterns <- c("date", "time", "year", "month", "day", "doy", "datetime")
  
  # Get column names
  cols <- names(df)
  
  # Find columns that might contain dates
  date_cols <- cols[grepl(paste(date_patterns, collapse = "|"), cols, ignore.case = TRUE)]
  
  return(date_cols)
}

# Function to standardize date formats across datasets with best-effort parsing
standardize_datetime <- function(df) {
  # Get potential date columns
  date_cols <- identify_datetime_columns(df)
  
  # If no date columns found, return dataframe as is
  if(length(date_cols) == 0) return(df)
  
  # Check for year, month, day columns that might need combining
  has_year <- any(grepl("year", date_cols, ignore.case = TRUE))
  has_month <- any(grepl("month", date_cols, ignore.case = TRUE))
  has_day <- any(grepl("day", date_cols, ignore.case = TRUE))
  
  # If we have separate year, month, day columns, try to combine them
  if(has_year && (has_month || has_day)) {
    year_col <- date_cols[grep("year", date_cols, ignore.case = TRUE)[1]]
    
    # Create a datetime column if possible
    if(has_month && has_day) {
      month_col <- date_cols[grep("month", date_cols, ignore.case = TRUE)[1]]
      day_col <- date_cols[grep("day", date_cols, ignore.case = TRUE)[1]]
      
      df$datetime <- make_datetime(
        year = df[[year_col]],
        month = df[[month_col]],
        day = df[[day_col]]
      )
    } else if(has_month) {
      month_col <- date_cols[grep("month", date_cols, ignore.case = TRUE)[1]]
      df$datetime <- make_datetime(
        year = df[[year_col]],
        month = df[[month_col]],
        day = 15  # Middle of the month as default
      )
    } else {
      # Only year is available
      df$datetime <- make_datetime(
        year = df[[year_col]],
        month = 7,  # Middle of the year as default
        day = 1
      )
    }
    
    # Extract year, month, day components
    df$year <- year(df$datetime)
    df$month <- month(df$datetime)
    df$day <- day(df$datetime)
    df$doy <- yday(df$datetime)
  } else {
    # Try to parse each date column
    for(col in date_cols) {
      if(is.character(df[[col]])) {
        # Try to parse using various formats
        parsed_dates <- tryCatch({
          # Check for common date patterns
          if(any(grepl("/", df[[col]], fixed = TRUE))) {
            mdy(df[[col]])
          } else if(any(grepl("-", df[[col]], fixed = TRUE))) {
            ymd(df[[col]])
          } else if(all(nchar(na.omit(df[[col]])) == 4) && all(grepl("^[0-9]+$", na.omit(df[[col]])))) {
            # Likely just a year
            as.Date(paste0(df[[col]], "-07-01"))  # July 1st as default date
          } else {
            # Last resort, try parse_date_time with multiple orders
            parse_date_time(df[[col]], orders = c("ymd", "mdy", "dmy", "ym", "my", "y"))
          }
        }, error = function(e) {
          return(NULL)
        })
        
        if(!is.null(parsed_dates)) {
          df[[paste0(col, "_parsed")]] <- parsed_dates
          
          # Extract components
          df$year <- year(parsed_dates)
          if(!col %in% c("year", "Year")) {  # Avoid overwriting existing year column
            df$month <- month(parsed_dates)
            df$day <- day(parsed_dates)
            df$doy <- yday(parsed_dates)
          }
        }
      } else if(col == "year" || col == "Year") {
        # If column is already called "year", make sure it's consistent
        df$year <- df[[col]]
      }
    }
  }
  
  # Make sure we at least have a year field
  if(!"year" %in% names(df) && !"Year" %in% names(df)) {
    # If we couldn't parse any dates but have a column that might contain years
    for(col in names(df)) {
      if(is.numeric(df[[col]])) {
        # Check if values are in a reasonable year range
        vals <- df[[col]][!is.na(df[[col]])]
        if(length(vals) > 0 && all(vals >= 1900 & vals <= 2100)) {
          df$year <- df[[col]]
          break
        }
      }
    }
  }
  
  # Standardize year column name
  if("Year" %in% names(df) && !"year" %in% names(df)) {
    df$year <- df$Year
  }
  
  return(df)
}

# Function to discover all dataset files in a directory with pattern matching
discover_datasets <- function(directory = ".", pattern = "*.csv|*.CSV") {
  # List all files matching the pattern
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  # Extract metadata from filenames
  metadata <- lapply(basename(files), parse_filename_metadata)
  
  # Combine filenames with metadata
  result <- data.frame(
    filepath = files,
    filename = basename(files),
    site = sapply(metadata, function(x) x$site_code),
    experiment_type = sapply(metadata, function(x) x$experiment_type),
    measured_variable = sapply(metadata, function(x) x$measured_variable),
    time_period = sapply(metadata, function(x) x$time_period),
    unit = sapply(metadata, function(x) x$unit),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Function to load and process a single dataset
load_dataset <- function(filepath, metadata) {
  message("Loading: ", basename(filepath))
  
  # Try to read the CSV file
  df <- tryCatch({
    read_csv(filepath, show_col_types = FALSE)
  }, error = function(e) {
    message("Error reading ", basename(filepath), ": ", e$message)
    return(NULL)
  })
  
  # Return NULL if loading failed
  if(is.null(df)) return(NULL)
  
  # Process dates in the dataset
  df <- standardize_datetime(df)
  
  # Add metadata columns
  df$source_file <- basename(filepath)
  df$site <- metadata$site
  df$experiment_type <- metadata$experiment_type
  df$variable_type <- metadata$measured_variable
  df$unit <- metadata$unit
  
  return(df)
}

#=========================#
# DATA HARMONIZATION FUNCTIONS
#=========================#

# Function to identify potential response variables in a dataset
identify_response_variables <- function(df, metadata) {
  # Common response variable column patterns
  response_patterns <- c(
    "mass", "biomass", "cover", "height", "diameter", "chlorophyll", "chla", 
    "productivity", "carbon", "flux", "stock", "production"
  )
  
  # Look for columns matching these patterns
  potential_responses <- names(df)[grepl(paste(response_patterns, collapse = "|"), 
                                         names(df), ignore.case = TRUE)]
  
  # If no potential response columns found, look for numeric columns
  if(length(potential_responses) == 0) {
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    # Exclude columns that are likely identifiers or dates
    exclude_patterns <- c("id", "plot", "year", "date", "time", "day", "doy", "month")
    potential_responses <- numeric_cols[!grepl(paste(exclude_patterns, collapse = "|"), 
                                               numeric_cols, ignore.case = TRUE)]
  }
  
  # If we have metadata about the measured variable, prioritize columns matching it
  if(!is.na(metadata$measured_variable)) {
    matching_cols <- names(df)[grepl(metadata$measured_variable, names(df), ignore.case = TRUE)]
    if(length(matching_cols) > 0) {
      potential_responses <- unique(c(matching_cols, potential_responses))
    }
  }
  
  return(potential_responses)
}

# Function to identify spatial hierarchy columns
identify_spatial_hierarchy <- function(df) {
  # Common spatial hierarchy column patterns
  spatial_patterns <- c(
    "plot", "block", "transect", "quadr", "quadrat", "subplot", "site", "location",
    "station", "point", "row", "col", "position"
  )
  
  # Look for columns matching these patterns
  spatial_cols <- names(df)[grepl(paste(spatial_patterns, collapse = "|"), 
                                  names(df), ignore.case = TRUE)]
  
  # Organize by likely hierarchy (based on common naming conventions)
  hierarchy <- list()
  level <- 1
  
  # Try to determine the hierarchy
  for(pattern in c("block", "plot", "subplot", "quadr", "transect", "row", "col", "position")) {
    matches <- spatial_cols[grepl(pattern, spatial_cols, ignore.case = TRUE)]
    if(length(matches) > 0) {
      for(col in matches) {
        hierarchy[[paste0("spatial_level_", level)]] <- col
        level <- level + 1
      }
    }
  }
  
  # Add any remaining spatial columns
  remaining <- setdiff(spatial_cols, unlist(hierarchy))
  for(col in remaining) {
    hierarchy[[paste0("spatial_level_", level)]] <- col
    level <- level + 1
  }
  
  return(hierarchy)
}

# Function to identify treatment hierarchy columns
identify_treatment_hierarchy <- function(df) {
  # Common treatment column patterns
  treatment_patterns <- c(
    "treat", "fertiliz", "irrigation", "manipul", "shelter", "control", "ambient",
    "mowed", "burned", "grazed", "warmed", "heated", "elevated", "ambient", "drought"
  )
  
  # Look for columns matching these patterns
  treatment_cols <- names(df)[grepl(paste(treatment_patterns, collapse = "|"), 
                                    names(df), ignore.case = TRUE)]
  
  # Organize treatments
  hierarchy <- list()
  level <- 1
  
  for(col in treatment_cols) {
    hierarchy[[paste0("treatment_level_", level)]] <- col
    level <- level + 1
  }
  
  return(hierarchy)
}

# Function to identify taxonomic columns
identify_taxonomic_columns <- function(df) {
  # Common taxonomic column patterns
  taxonomic_patterns <- c(
    "taxa", "species", "genus", "family", "spcode", "sp_", "spp"
  )
  
  # Look for columns matching these patterns
  taxonomic_cols <- names(df)[grepl(paste(taxonomic_patterns, collapse = "|"), 
                                    names(df), ignore.case = TRUE)]
  
  return(taxonomic_cols)
}

# Function to determine control vs treatment based on column values
determine_treatment_status <- function(df, treatment_cols) {
  if(length(treatment_cols) == 0) return(rep(NA, nrow(df)))
  
  # Common control keywords
  control_keywords <- c("control", "ambient", "reference", "0", "false", "no", "none")
  
  # For each treatment column, try to determine if rows are control or treatment
  treatment_status <- rep(NA, nrow(df))
  
  for(col in treatment_cols) {
    # Skip if column doesn't exist
    if(!col %in% names(df)) next
    
    # Get unique values
    values <- unique(df[[col]])
    
    # If binary (2 unique values) or has control keywords
    if(length(values) == 2 || any(tolower(values) %in% control_keywords)) {
      # Determine which values represent control
      control_values <- values[tolower(values) %in% control_keywords]
      
      # If we found control values
      if(length(control_values) > 0) {
        # Mark rows as control (0) or treatment (1)
        treatment_status <- ifelse(df[[col]] %in% control_values, 0, 1)
        break  # Found a good treatment indicator, stop looking
      }
    }
  }
  
  return(treatment_status)
}

# Function to harmonize a single dataset
harmonize_dataset <- function(df, metadata) {
  # Skip if dataset is NULL or empty
  if(is.null(df) || nrow(df) == 0) return(NULL)
  
  # Convert column names to lowercase for easier matching
  names(df) <- tolower(names(df))
  
  # Identify key columns
  response_vars <- identify_response_variables(df, metadata)
  spatial_hierarchy <- identify_spatial_hierarchy(df)
  treatment_hierarchy <- identify_treatment_hierarchy(df)
  taxonomic_cols <- identify_taxonomic_columns(df)
  
  # Skip if we can't identify any response variables
  if(length(response_vars) == 0) {
    message("No response variables identified in ", metadata$filename)
    return(NULL)
  }
  
  # Extract treatment columns
  treatment_cols <- unlist(treatment_hierarchy)
  
  # Determine treatment status
  treatment_status <- determine_treatment_status(df, treatment_cols)
  
  # Create treatment types column (combine all treatment columns)
  treatment_types <- apply(df[, treatment_cols, drop = FALSE], 1, function(row) {
    paste(names(row), row, sep = ":", collapse = "; ")
  })
  
  # Create a harmonized dataset for each response variable
  harmonized_list <- list()
  
  for(resp_var in response_vars) {
    # Skip if response variable is all NA or not numeric
    if(all(is.na(df[[resp_var]])) || !is.numeric(df[[resp_var]])) next
    
    # Create variable name from column name
    var_name <- gsub("\\.", "_", resp_var)
    
    # Create base harmonized data
    harmonized <- data.frame(
      source_file = metadata$filename,
      site = metadata$site,
      experiment_type = metadata$experiment_type,
      response_variable = var_name,
      response_value = df[[resp_var]],
      is_treatment = treatment_status,
      treatment_type = treatment_types,
      unit = ifelse(!is.na(metadata$unit), metadata$unit, "unknown")
    )
    
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
    if(length(taxonomic_cols) > 0) {
      # Take the first taxonomic column as the taxa group
      first_tax_col <- taxonomic_cols[1]
      harmonized$taxa_group <- df[[first_tax_col]]
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
    return(NULL)
  }
}

#=========================#
# EFFECT SIZE CALCULATION
#=========================#

# Function to calculate standardized effect sizes
calculate_effect_sizes <- function(data) {
  # Group by relevant variables
  grouped_data <- data %>%
    filter(!is.na(is_treatment) & !is.na(response_value)) %>%
    group_by(site, experiment_type, year, treatment_type, response_variable, unit) 
  
  # Calculate mean, SD, and N for control and treatment groups
  effect_sizes <- grouped_data %>%
    summarize(
      control_mean = mean(response_value[is_treatment == 0], na.rm = TRUE),
      treatment_mean = mean(response_value[is_treatment == 1], na.rm = TRUE),
      control_sd = sd(response_value[is_treatment == 0], na.rm = TRUE),
      treatment_sd = sd(response_value[is_treatment == 1], na.rm = TRUE),
      control_n = sum(!is.na(response_value[is_treatment == 0])),
      treatment_n = sum(!is.na(response_value[is_treatment == 1])),
      .groups = "drop"
    ) %>%
    # Filter for groups that have both control and treatment data
    filter(control_n > 0 & treatment_n > 0 & 
             !is.na(control_mean) & !is.na(treatment_mean) & 
             !is.na(control_sd) & !is.na(treatment_sd) &
             control_sd > 0 & treatment_sd > 0)  # Avoid division by zero
  
  # Calculate effect sizes
  effect_sizes <- effect_sizes %>%
    rowwise() %>%
    mutate(
      # Calculate pooled standard deviation
      pooled_sd = sqrt(((control_n - 1) * control_sd^2 + (treatment_n - 1) * treatment_sd^2) / 
                         (control_n + treatment_n - 2)),
      
      # Calculate standardized mean difference (SMD)
      smd = (treatment_mean - control_mean) / pooled_sd,
      
      # Apply small sample size correction factor (Hedges' g)
      j = 1 - (3 / (4 * (control_n + treatment_n - 2) - 1)),
      hedges_g = j * smd,
      
      # Calculate variance of Hedges' g
      var_g = (control_n + treatment_n) / (control_n * treatment_n) + 
        hedges_g^2 / (2 * (control_n + treatment_n)),
      
      # Calculate standard error of Hedges' g
      se_g = sqrt(var_g),
      
      # Calculate 95% confidence intervals
      ci_lower = hedges_g - 1.96 * se_g,
      ci_upper = hedges_g + 1.96 * se_g,
      
      # Calculate log response ratio (lnRR)
      lnRR = log(treatment_mean / control_mean),
      
      # Calculate variance of lnRR
      var_lnRR = (control_sd^2 / (control_n * control_mean^2)) + 
        (treatment_sd^2 / (treatment_n * treatment_mean^2)),
      
      # Calculate standard error of lnRR
      se_lnRR = sqrt(var_lnRR),
      
      # Calculate 95% confidence intervals for lnRR
      lnRR_ci_lower = lnRR - 1.96 * se_lnRR,
      lnRR_ci_upper = lnRR + 1.96 * se_lnRR
    ) %>%
    ungroup()
  
  return(effect_sizes)
}

#=========================#
# META-ANALYSIS FUNCTIONS
#=========================#

# Function to perform meta-analysis for each experiment type and response variable
meta_analysis <- function(effect_data) {
  # Create lists to store results
  meta_results <- list()
  
  # Group data by experiment type and response variable
  grouped_data <- effect_data %>%
    group_by(experiment_type, response_variable) %>%
    group_split()
  
  # Loop through each group and perform meta-analysis
  for(i in seq_along(grouped_data)) {
    group <- grouped_data[[i]]
    
    # Skip if group is too small
    if(nrow(group) < 3) next
    
    # Extract grouping variables
    exp_type <- unique(group$experiment_type)
    resp_var <- unique(group$response_variable)
    
    # Meta-analysis using Hedges' g
    meta_g <- try({
      rma(yi = hedges_g, vi = var_g, data = group,
          method = "REML", slab = paste(site, year, sep = "_"))
    }, silent = TRUE)
    
    # Meta-analysis using lnRR
    meta_lnRR <- try({
      rma(yi = lnRR, vi = var_lnRR, data = group,
          method = "REML", slab = paste(site, year, sep = "_"))
    }, silent = TRUE)
    
    # Store results if analyses were successful
    result <- list(
      experiment_type = exp_type,
      response_variable = resp_var,
      k = nrow(group),
      meta_g = if(!inherits(meta_g, "try-error")) meta_g else NULL,
      meta_lnRR = if(!inherits(meta_lnRR, "try-error")) meta_lnRR else NULL
    )
    
    meta_results[[length(meta_results) + 1]] <- result
  }
  
  return(meta_results)
}

# Function to extract and summarize meta-analysis results
summarize_meta_results <- function(meta_results) {
  result_df <- data.frame(
    experiment_type = character(),
    response_variable = character(),
    k = integer(),
    effect_measure = character(),
    overall_effect = numeric(),
    se = numeric(),
    pval = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    tau2 = numeric(),
    i2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(res in meta_results) {
    # Process Hedges' g results
    if(!is.null(res$meta_g)) {
      g_row <- data.frame(
        experiment_type = res$experiment_type,
        response_variable = res$response_variable,
        k = res$k,
        effect_measure = "Hedges_g",
        overall_effect = res$meta_g$b[1],
        se = res$meta_g$se,
        pval = res$meta_g$pval,
        ci_lower = res$meta_g$ci.lb,
        ci_upper = res$meta_g$ci.ub,
        tau2 = res$meta_g$tau2,
        i2 = res$meta_g$I2,
        stringsAsFactors = FALSE
      )
      result_df <- rbind(result_df, g_row)
    }
    
    # Process lnRR results
    if(!is.null(res$meta_lnRR)) {
      lnRR_row <- data.frame(
        experiment_type = res$experiment_type,
        response_variable = res$response_variable,
        k = res$k,
        effect_measure = "lnRR",
        overall_effect = res$meta_lnRR$b[1],
        se = res$meta_lnRR$se,
        pval = res$meta_lnRR$pval,
        ci_lower = res$meta_lnRR$ci.lb,
        ci_upper = res$meta_lnRR$ci.ub,
        tau2 = res$meta_lnRR$tau2,
        i2 = res$meta_lnRR$I2,
        stringsAsFactors = FALSE
      )
      result_df <- rbind(result_df, lnRR_row)
    }
  }
  
  return(result_df)
}

# Function to create forest plots for meta-analysis results
create_forest_plots <- function(effect_sizes, meta_results) {
  # Loop through meta-analysis results and create forest plots
  for(i in seq_along(meta_results)) {
    res <- meta_results[[i]]
    
    # Skip if no results or not significant
    if(is.null(res$meta_g) || res$meta_g$pval >= 0.05) next
    
    # Prepare data for forest plot
    exp_type <- res$experiment_type
    resp_var <- res$response_variable
    
    # Filter relevant effect sizes
    plot_data <- effect_sizes %>%
      filter(experiment_type == exp_type, response_variable == resp_var)
    
    # Create forest plot using ggplot2
    forest_g <- ggplot(plot_data, aes(x = hedges_g, y = paste(site, year))) +
      geom_point(aes(size = 1/sqrt(var_g))) +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = res$meta_g$b[1], color = "red") +
      geom_rect(aes(xmin = res$meta_g$ci.lb, xmax = res$meta_g$ci.ub, 
                    ymin = -Inf, ymax = Inf), 
                fill = "red", alpha = 0.1, inherit.aes = FALSE) +
      labs(
        title = paste("Forest Plot -", exp_type),
        subtitle = paste("Response Variable:", resp_var),
        x = "Hedges' g (95% CI)",
        y = "",
        caption = paste("Overall effect =", round(res$meta_g$b[1], 3), 
                        "(95% CI:", round(res$meta_g$ci.lb, 3), "to", 
                        round(res$meta_g$ci.ub, 3), "), p =", round(res$meta_g$pval, 4))
      ) +
      theme_bw() +
      theme(legend.position = "none")
    
    # Save forest plot
    filename <- paste0("forest_plot_", gsub(" ", "_", exp_type), "_", 
                       gsub(" ", "_", resp_var), ".png")
    
    # Save to the data directory
    ggsave(filename, forest_g, width = 10, height = 8)
  }
}

#=========================#
# RUN THE HARMONIZATION PROCESS
#=========================#

# Set the directory path where your data files are located
data_dir <- "/Users/jongewirtzman/Downloads/drive-download-20250404T194054Z-001"

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