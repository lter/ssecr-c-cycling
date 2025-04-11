# Enhanced Ecological Data Harmonization Pipeline
# This script provides a robust approach to harmonizing diverse ecological datasets
# for meta-analysis of experimental effects across LTER sites

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(metafor)    # For meta-analysis
library(ggplot2)    # For visualization
library(vroom)      # For robust CSV reading
library(janitor)    # For cleaning column names
library(broom)      # For tidying statistical output

#=========================#
# DATA INGESTION FUNCTIONS
#=========================#

# Enhanced file discovery function with better metadata extraction
discover_datasets <- function(directory = ".", pattern = "\\.csv$|\\.CSV$") {
  message("Searching for datasets in ", directory)
  
  # List all files matching the pattern
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    warning("No CSV files found in directory: ", directory)
    return(NULL)
  }
  
  message("Found ", length(files), " datasets")
  
  # Extract metadata from filenames
  metadata <- data.frame(
    filepath = files,
    filename = basename(files),
    stringsAsFactors = FALSE
  )
  
  # Extract site code (typically the first 3 letters)
  metadata$site_code <- str_extract(metadata$filename, "^[A-Z]{2,5}")
  
  # Extract experiment type
  metadata$experiment_type <- case_when(
    str_detect(metadata$filename, "Fertilizer|fertilizer") ~ "Fertilizer",
    str_detect(metadata$filename, "Nutrient|nutrient") ~ "Nutrient addition",
    str_detect(metadata$filename, "manipulation|Manipulation") ~ "Manipulation",
    str_detect(metadata$filename, "precipitation|Precipitation") ~ "Precipitation control",
    str_detect(metadata$filename, "HeatWave|heatwave") ~ "Heat wave",
    str_detect(metadata$filename, "Logging|logging") ~ "Logging",
    str_detect(metadata$filename, "Inundation|inundation") ~ "Inundation",
    str_detect(metadata$filename, "ClearCutting|clearcutting") ~ "Clear cutting",
    TRUE ~ "Other"
  )
  
  # Extract measured response variable
  metadata$response_variable <- case_when(
    str_detect(metadata$filename, "biomass|Biomass") ~ "Biomass",
    str_detect(metadata$filename, "height|Height") ~ "Height",
    str_detect(metadata$filename, "cover|Cover") ~ "Cover",
    str_detect(metadata$filename, "diameter|Diameter") ~ "Diameter",
    str_detect(metadata$filename, "Chla|chla|chlorophyll") ~ "Chlorophyll",
    str_detect(metadata$filename, "flux|Flux") ~ "Flux",
    str_detect(metadata$filename, "SOC") ~ "Soil organic carbon",
    TRUE ~ "Other"
  )
  
  # Extract time period
  metadata$time_period <- str_extract(metadata$filename, "[0-9]{4}[-_][0-9]{4}")
  
  # Extract common units
  metadata$unit <- case_when(
    str_detect(metadata$filename, "gm2") ~ "g/m2",
    str_detect(metadata$filename, "mm") ~ "mm",
    str_detect(metadata$filename, "cm") ~ "cm",
    str_detect(metadata$filename, "percent") ~ "percent",
    TRUE ~ NA_character_
  )
  
  return(metadata)
}

# Robust dataset loading function with flexible error handling
load_dataset <- function(file_path, max_problems = 100) {
  file_name <- basename(file_path)
  message("Loading: ", file_name)
  
  # Approach 1: Try standard read_csv first
  result <- tryCatch({
    df <- read_csv(file_path, show_col_types = FALSE)
    list(success = TRUE, data = df, method = "read_csv")
  }, error = function(e) {
    message("Standard read_csv failed, trying alternative methods...")
    return(list(success = FALSE, error = e$message))
  })
  
  # If standard approach failed, try with vroom and various options
  if(!result$success) {
    # Approach 2: Try vroom with problem rows skipped
    result <- tryCatch({
      df <- vroom(file_path, 
                  skip_empty_rows = TRUE,
                  comment = "#",
                  na = c("", "NA", ".", "NaN"),
                  trim_ws = TRUE,
                  col_types = cols(.default = col_character()),
                  n_max = 10000)
      list(success = TRUE, data = df, method = "vroom")
    }, error = function(e) {
      message("Vroom approach failed: ", e$message)
      
      # Approach 3: Try read.csv (base R)
      tryCatch({
        df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
        df <- as_tibble(df)
        list(success = TRUE, data = df, method = "base_read.csv")
      }, error = function(e) {
        message("All loading methods failed for ", file_name, ": ", e$message)
        return(list(success = FALSE, error = e$message))
      })
    })
  }
  
  # If loading was successful, clean up the data
  if(result$success) {
    # Clean column names using janitor
    df <- result$data %>% 
      clean_names() %>%
      mutate(across(where(is.character), ~na_if(., "")))
    
    # Try to convert appropriate character columns to numeric
    df <- df %>%
      mutate(across(where(is.character), ~{
        # Check if the column could be numeric
        if(all(is.na(.) | grepl("^[0-9]+\\.?[0-9]*$|^\\.[0-9]+$|^NA$|^\\.$", .))) {
          as.numeric(.)
        } else {
          .
        }
      }))
    
    message("Successfully loaded ", file_name, " using ", result$method, 
            " (", ncol(df), " columns, ", nrow(df), " rows)")
    
    # Add file source information
    df$source_file <- file_name
    
    return(df)
  } else {
    warning("Failed to load ", file_name)
    return(NULL)
  }
}

#=========================#
# DATA HARMONIZATION FUNCTIONS
#=========================#

# Function to identify the type of each column in a dataset
identify_column_types <- function(df) {
  col_types <- list(
    id_cols = character(),
    date_cols = character(),
    treatment_cols = character(),
    spatial_cols = character(),
    taxonomy_cols = character(),
    response_cols = character(),
    other_cols = character()
  )
  
  # Define patterns for different column types
  patterns <- list(
    id = "\\bid\\b|code|tag|sample|\\bID\\b|position|^id_|_id$",
    date = "date|year|month|day|time|doy|season|^dt$|^yr$|DATE|^date$",
    treatment = "treatment|fertiliz|treat|manipul|manip|control|ambient|shelter|irrigat|burned|grazed|warming|heated|exp|TREATMENT",
    spatial = "plot|block|transect|quadr|subplot|site|location|station|rep|row|col|position|PLOT|BLOCK",
    taxonomy = "species|taxa|genus|family|sp\\.|^sp$|spp|life_form|plant|SPECIES|^taxa$",
    response = "mass|biomass|cover|height|diameter|flux|stock|production|richness|diversity|abundance|density|yield|growth|weight|productivity|^dba$|chlorophyll|chla|temp|DO|pH"
  )
  
  # Check each column against patterns
  for(col in names(df)) {
    # Skip the source_file column we added
    if(col == "source_file") next
    
    col_lower <- tolower(col)
    
    # Check for each category
    matched <- FALSE
    for(type in names(patterns)) {
      if(grepl(patterns[[type]], col_lower)) {
        type_list_name <- paste0(type, "_cols")
        col_types[[type_list_name]] <- c(col_types[[type_list_name]], col)
        matched <- TRUE
        break
      }
    }
    
    # If no match found, add to other_cols
    if(!matched) {
      col_types$other_cols <- c(col_types$other_cols, col)
    }
  }
  
  return(col_types)
}

# Function to standardize date information
standardize_dates <- function(df, date_cols) {
  # Skip if no date columns or empty dataframe
  if(length(date_cols) == 0 || nrow(df) == 0) return(df)
  
  # First check for explicit year column
  year_col <- date_cols[grep("^year$|^yr$|^YEAR$", date_cols, ignore.case = TRUE)]
  
  if(length(year_col) > 0) {
    # Make sure year is numeric
    df$year <- as.numeric(df[[year_col[1]]])
  } else {
    # Try to extract year from date columns
    for(col in date_cols) {
      # Skip if column doesn't exist
      if(!col %in% names(df)) next
      
      # If column is already Date type
      if(inherits(df[[col]], "Date")) {
        df$year <- year(df[[col]])
        break
      }
      
      # If column is character, try to parse
      if(is.character(df[[col]])) {
        # Try common date formats
        parsed_dates <- tryCatch({
          # Try various formats based on patterns
          if(any(grepl("/", df[[col]]))) {
            # MM/DD/YYYY or DD/MM/YYYY format
            parse_date_time(df[[col]], orders = c("mdy", "dmy", "ymd"))
          } else if(any(grepl("-", df[[col]]))) {
            # YYYY-MM-DD or DD-MM-YYYY format
            parse_date_time(df[[col]], orders = c("ymd", "dmy", "mdy"))
          } else {
            # Try without separators or other formats
            parse_date_time(df[[col]], orders = c("ymd", "dmy", "mdy", "ym", "y"))
          }
        }, error = function(e) NULL)
        
        if(!is.null(parsed_dates) && !all(is.na(parsed_dates))) {
          df$year <- year(parsed_dates)
          # Also add the parsed date
          df$date_parsed <- parsed_dates
          break
        }
      }
      
      # Direct year extraction for YYYY format
      if(is.character(df[[col]]) && all(nchar(na.omit(df[[col]])) == 4) && 
         all(grepl("^[0-9]+$", na.omit(df[[col]])))) {
        df$year <- as.numeric(df[[col]])
        break
      }
    }
  }
  
  # If we still don't have a year column, look for a 4-digit number in any column
  if(!"year" %in% names(df)) {
    for(col in names(df)) {
      if(is.character(df[[col]])) {
        year_matches <- str_extract(df[[col]], "\\b[12][0-9]{3}\\b")
        if(!all(is.na(year_matches))) {
          df$year <- as.numeric(year_matches)
          break
        }
      }
    }
  }
  
  return(df)
}

# Function to identify treatment variables and code control vs treatment
identify_treatments <- function(df, treatment_cols) {
  # Skip if no treatment columns or empty dataframe
  if(length(treatment_cols) == 0 || nrow(df) == 0) {
    return(list(df = df, has_treatment = FALSE))
  }
  
  # Look for the most promising treatment column
  best_treatment_col <- NULL
  treatment_values <- NULL
  
  # Examine each potential treatment column
  for(col in treatment_cols) {
    # Skip if column doesn't exist
    if(!col %in% names(df)) next
    
    # Get unique values and their counts
    vals <- table(df[[col]])
    
    # Skip if too many unique values (likely not a treatment indicator)
    if(length(vals) > 10) next
    
    # Look for control keywords in the values
    has_control <- any(grepl("control|cont|ctrl|reference|ambient|none|untreated|^c$|^0$|^n$|^no$|^false$", 
                             names(vals), ignore.case = TRUE))
    
    # Look for treatment keywords in the values
    has_treatment <- any(grepl("treat|exp|manipul|fertiliz|heat|warm|irrigat|^t$|^1$|^y$|^yes$|^true$", 
                               names(vals), ignore.case = TRUE))
    
    # If column has both control and treatment indicators, it's likely our treatment column
    if(has_control && has_treatment) {
      best_treatment_col <- col
      treatment_values <- names(vals)
      break
    }
    
    # If column has either control or treatment indicators, save it as a candidate
    if(has_control || has_treatment) {
      best_treatment_col <- col
      treatment_values <- names(vals)
    }
  }
  
  # If no good treatment column found, try looking for columns with binary values
  if(is.null(best_treatment_col)) {
    for(col in treatment_cols) {
      if(!col %in% names(df)) next
      
      # Check if column has exactly 2 unique values (excluding NA)
      uniq_vals <- unique(na.omit(df[[col]]))
      if(length(uniq_vals) == 2) {
        best_treatment_col <- col
        treatment_values <- uniq_vals
        break
      }
    }
  }
  
  # If we found a treatment column, code control vs treatment
  if(!is.null(best_treatment_col)) {
    # Extract the column data
    treatment_data <- df[[best_treatment_col]]
    
    # Define control values based on common patterns
    control_patterns <- "control|cont|ctrl|reference|ambient|none|untreated|^c$|^0$|^n$|^no$|^false$"
    
    # Create is_treatment column
    df$is_treatment <- case_when(
      # Known control values
      tolower(treatment_data) %in% c("control", "cont", "ctrl", "reference", "ambient", 
                                     "none", "untreated", "c", "0", "n", "no", "false") ~ 0,
      grepl(control_patterns, treatment_data, ignore.case = TRUE) ~ 0,
      # Otherwise, treat as treatment
      TRUE ~ 1
    )
    
    # Save the treatment column name and description
    df$treatment_column <- best_treatment_col
    df$treatment_type <- treatment_data
    
    return(list(df = df, has_treatment = TRUE))
  }
  
  # If no treatment column was identified
  df$is_treatment <- NA
  df$treatment_column <- NA
  df$treatment_type <- NA
  
  return(list(df = df, has_treatment = FALSE))
}

# Function to identify response variables based on column names and types
identify_response_variables <- function(df, response_cols, metadata) {
  # Skip if no response columns or empty dataframe
  if(length(response_cols) == 0 || nrow(df) == 0) return(NULL)
  
  # Create a list to store potential response variables
  response_vars <- list()
  
  # Prioritize columns matching the expected response variable from metadata
  if(!is.na(metadata$response_variable)) {
    # Look for columns matching the metadata response variable
    matching_cols <- response_cols[grepl(tolower(metadata$response_variable), 
                                         tolower(response_cols))]
    
    if(length(matching_cols) > 0) {
      # Check each matching column
      for(col in matching_cols) {
        # Skip if column doesn't exist
        if(!col %in% names(df)) next
        
        # Check if numeric and not all NA
        if(is.numeric(df[[col]]) && !all(is.na(df[[col]]))) {
          response_vars[[col]] <- list(
            column = col,
            priority = 1,  # High priority since it matches metadata
            data_type = class(df[[col]])[1],
            na_count = sum(is.na(df[[col]])),
            unique_count = length(unique(na.omit(df[[col]]))),
            mean_value = mean(df[[col]], na.rm = TRUE),
            label = paste(metadata$response_variable, col, sep = "_")
          )
        }
      }
    }
  }
  
  # Then look for other response variables
  # Check remaining response columns
  remaining_cols <- setdiff(response_cols, names(response_vars))
  for(col in remaining_cols) {
    # Skip if column doesn't exist
    if(!col %in% names(df)) next
    
    # Check if numeric and not all NA
    if(is.numeric(df[[col]]) && !all(is.na(df[[col]]))) {
      response_vars[[col]] <- list(
        column = col,
        priority = 2,  # Lower priority
        data_type = class(df[[col]])[1],
        na_count = sum(is.na(df[[col]])),
        unique_count = length(unique(na.omit(df[[col]]))),
        mean_value = mean(df[[col]], na.rm = TRUE),
        label = col
      )
    }
  }
  
  # If we didn't find any response variables, try looking at other numeric columns
  if(length(response_vars) == 0) {
    # Get all numeric columns except identified special columns
    special_cols <- c(
      names(df)[grepl("year|date|id|plot|block|treatment|is_treatment|source_file", 
                      names(df), ignore.case = TRUE)],
      "treatment_column", "treatment_type"
    )
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, special_cols)
    
    for(col in numeric_cols) {
      if(!all(is.na(df[[col]]))) {
        response_vars[[col]] <- list(
          column = col,
          priority = 3,  # Lowest priority
          data_type = class(df[[col]])[1],
          na_count = sum(is.na(df[[col]])),
          unique_count = length(unique(na.omit(df[[col]]))),
          mean_value = mean(df[[col]], na.rm = TRUE),
          label = col
        )
      }
    }
  }
  
  return(response_vars)
}

# Function to identify spatial hierarchy columns
identify_spatial_hierarchy <- function(df, spatial_cols) {
  # Skip if no spatial columns or empty dataframe
  if(length(spatial_cols) == 0 || nrow(df) == 0) return(list())
  
  # Define hierarchy of spatial scales (from largest to smallest)
  spatial_hierarchy <- c(
    "site", "block", "plot", "subplot", "quadrat", "rep", 
    "transect", "row", "col", "position"
  )
  
  # Map columns to spatial hierarchy
  mapped_hierarchy <- list()
  
  for(level in spatial_hierarchy) {
    # Find columns matching this level
    matching_cols <- spatial_cols[grepl(level, spatial_cols, ignore.case = TRUE)]
    
    if(length(matching_cols) > 0) {
      # Use first matching column
      col <- matching_cols[1]
      if(col %in% names(df)) {
        mapped_hierarchy[[level]] <- col
      }
    }
  }
  
  return(mapped_hierarchy)
}

# Function to identify taxonomic information
identify_taxonomy <- function(df, taxonomy_cols) {
  # Skip if no taxonomy columns or empty dataframe
  if(length(taxonomy_cols) == 0 || nrow(df) == 0) return(NULL)
  
  # Define taxonomy hierarchy
  taxonomy_hierarchy <- c("species", "genus", "family", "taxa", "sp", "spp")
  
  # Find best taxonomy column
  best_taxon_col <- NULL
  
  for(level in taxonomy_hierarchy) {
    # Find columns matching this level
    matching_cols <- taxonomy_cols[grepl(level, taxonomy_cols, ignore.case = TRUE)]
    
    if(length(matching_cols) > 0) {
      # Use first matching column
      col <- matching_cols[1]
      if(col %in% names(df)) {
        best_taxon_col <- col
        break
      }
    }
  }
  
  # If no specific taxonomy column found, use first available
  if(is.null(best_taxon_col) && length(taxonomy_cols) > 0) {
    for(col in taxonomy_cols) {
      if(col %in% names(df)) {
        best_taxon_col <- col
        break
      }
    }
  }
  
  return(best_taxon_col)
}

# Main function to harmonize a dataset
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
    
    # Create base harmonized data
    harmonized <- data.frame(
      source_file = metadata$filename,
      site = metadata$site_code,
      experiment_type = metadata$experiment_type,
      response_variable = var_info$label,
      response_value = df[[var_name]],
      is_treatment = df$is_treatment,
      treatment_type = df$treatment_type,
      treatment_column = df$treatment_column,
      unit = ifelse(!is.na(metadata$unit), metadata$unit, "unknown"),
      stringsAsFactors = FALSE
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

#=========================#
# EFFECT SIZE CALCULATION
#=========================#

# Function to calculate standardized effect sizes
calculate_effect_sizes <- function(data) {
  # First check that we have treatment coding
  if(!all(c("is_treatment", "response_value") %in% names(data))) {
    warning("Missing required columns for effect size calculation")
    return(NULL)
  }
  
  # Check if we have enough data with treatment coding
  coded_data <- data %>% filter(!is.na(is_treatment) & !is.na(response_value))
  
  if(nrow(coded_data) < 10) {
    warning("Insufficient data with treatment coding for effect size calculation")
    return(NULL)
  }
  
  # Group by relevant variables for comparison
  grouped_data <- coded_data %>%
    group_by(site, experiment_type, year, response_variable, unit) 
  
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
    # Filter for valid effect size calculation
    filter(
      # Ensure we have both control and treatment data
      control_n > 2 & treatment_n > 2 &
        # Ensure means and SDs are valid 
        !is.na(control_mean) & !is.na(treatment_mean) & 
        !is.na(control_sd) & !is.na(treatment_sd) &
        # Avoid division by zero
        control_sd > 0 & treatment_sd > 0 &
        # Avoid extreme values
        is.finite(control_mean) & is.finite(treatment_mean)
    )
  
  # If no valid comparisons, return NULL
  if(nrow(effect_sizes) == 0) {
    warning("No valid control-treatment comparisons found")
    return(NULL)
  }
  
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
      
      # Calculate log response ratio (lnRR) only for positive means
      lnRR = if_else(control_mean > 0 & treatment_mean > 0,
                     log(treatment_mean / control_mean), NA_real_),
      
      # Calculate variance of lnRR
      var_lnRR = if_else(!is.na(lnRR),
                         (control_sd^2 / (control_n * control_mean^2)) + 
                           (treatment_sd^2 / (treatment_n * treatment_mean^2)),
                         NA_real_),
      
      # Calculate standard error of lnRR
      se_lnRR = if_else(!is.na(var_lnRR), sqrt(var_lnRR), NA_real_),
      
      # Calculate 95% confidence intervals for lnRR
      lnRR_ci_lower = if_else(!is.na(lnRR), lnRR - 1.96 * se_lnRR, NA_real_),
      lnRR_ci_upper = if_else(!is.na(lnRR), lnRR + 1.96 * se_lnRR, NA_real_)
    ) %>%
    ungroup()
  
  return(effect_sizes)
}

#=========================#
# META-ANALYSIS FUNCTIONS
#=========================#

# Function to perform meta-analysis 
perform_meta_analysis <- function(effect_sizes, effect_measure = "hedges_g") {
  # Validate input
  if(is.null(effect_sizes) || nrow(effect_sizes) == 0) {
    warning("No effect sizes provided for meta-analysis")
    return(NULL)
  }
  
  # Define list to store results
  meta_results <- list()
  
  # Group data by experiment type and response variable
  grouped_data <- effect_sizes %>%
    group_by(experiment_type, response_variable) %>%
    group_split()
  
  # Perform meta-analysis for each group
  for(i in seq_along(grouped_data)) {
    group_data <- grouped_data[[i]]
    
    # Skip if too few studies
    if(nrow(group_data) < 3) {
      message("Skipping meta-analysis for group with < 3 studies: ", 
              unique(group_data$experiment_type), "/", unique(group_data$response_variable))
      next
    }
    
    # Extract group identifiers
    exp_type <- unique(group_data$experiment_type)
    resp_var <- unique(group_data$response_variable)
    
    # Perform meta-analysis with Hedges' g
    if(effect_measure == "hedges_g" || effect_measure == "both") {
      meta_g <- tryCatch({
        rma(yi = hedges_g, vi = var_g, data = group_data,
            method = "REML", slab = paste(site, year, sep = "_"))
      }, error = function(e) {
        message("Error in meta-analysis (Hedges' g) for ", exp_type, "/", resp_var, ": ", e$message)
        return(NULL)
      })
      
      if(!is.null(meta_g)) {
        meta_results[[paste0(exp_type, "_", resp_var, "_g")]] <- list(
          experiment_type = exp_type,
          response_variable = resp_var,
          measure = "Hedges_g",
          k = meta_g$k,
          result = meta_g
        )
      }
    }
    
    # Perform meta-analysis with log response ratio
    if(effect_measure == "lnRR" || effect_measure == "both") {
      # Filter rows with valid lnRR data
      lnrr_data <- group_data %>% filter(!is.na(lnRR) & !is.na(var_lnRR))
      
      if(nrow(lnrr_data) >= 3) {
        meta_lnRR <- tryCatch({
          rma(yi = lnRR, vi = var_lnRR, data = lnrr_data,
              method = "REML", slab = paste(site, year, sep = "_"))
        }, error = function(e) {
          message("Error in meta-analysis (lnRR) for ", exp_type, "/", resp_var, ": ", e$message)
          return(NULL)
        })
        
        if(!is.null(meta_lnRR)) {
          meta_results[[paste0(exp_type, "_", resp_var, "_lnRR")]] <- list(
            experiment_type = exp_type,
            response_variable = resp_var,
            measure = "lnRR",
            k = meta_lnRR$k,
            result = meta_lnRR
          )
        }
      }
    }
  }
  
  return(meta_results)
}

# Function to summarize meta-analysis results
summarize_meta_results <- function(meta_results) {
  # Validate input
  if(length(meta_results) == 0) {
    warning("No meta-analysis results to summarize")
    return(NULL)
  }
  
  # Initialize results data frame
  summary_df <- data.frame(
    experiment_type = character(),
    response_variable = character(),
    effect_measure = character(),
    k = numeric(),
    effect_size = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    p_value = numeric(),
    tau2 = numeric(),
    i2 = numeric(),
    q_stat = numeric(),
    q_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract information from each meta-analysis
  for(i in seq_along(meta_results)) {
    result <- meta_results[[i]]
    
    # Skip if NULL result
    if(is.null(result$result)) next
    
    # Extract model results
    model <- result$result
    
    # Create summary row
    row <- data.frame(
      experiment_type = result$experiment_type,
      response_variable = result$response_variable,
      effect_measure = result$measure,
      k = model$k,
      effect_size = model$b[1,1],
      se = model$se,
      ci_lower = model$ci.lb,
      ci_upper = model$ci.ub,
      p_value = model$pval,
      tau2 = model$tau2,
      i2 = model$I2,
      q_stat = model$QE,
      q_p = model$QEp,
      stringsAsFactors = FALSE
    )
    
    # Add to summary data frame
    summary_df <- rbind(summary_df, row)
  }
  
  return(summary_df)
}

# Function to create forest plots for meta-analysis results
create_forest_plots <- function(effect_sizes, meta_results, output_dir = ".") {
  # Validate inputs
  if(is.null(effect_sizes) || nrow(effect_sizes) == 0) {
    warning("No effect sizes provided for forest plots")
    return(NULL)
  }
  
  if(length(meta_results) == 0) {
    warning("No meta-analysis results provided for forest plots")
    return(NULL)
  }
  
  # Make sure output directory exists
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # List to store plot filenames
  plot_files <- list()
  
  # Process each meta-analysis result
  for(i in seq_along(meta_results)) {
    result <- meta_results[[i]]
    
    # Skip if NULL result
    if(is.null(result$result)) next
    
    # Extract info
    exp_type <- result$experiment_type
    resp_var <- result$response_variable
    measure <- result$measure
    model <- result$result
    
    # Get data for this specific group
    group_data <- effect_sizes %>%
      filter(experiment_type == exp_type & response_variable == resp_var)
    
    # Skip if no data
    if(nrow(group_data) == 0) next
    
    # Set up effect size and variance based on measure
    if(measure == "Hedges_g") {
      group_data <- group_data %>%
        mutate(
          yi = hedges_g,
          vi = var_g,
          ci_lo = ci_lower,
          ci_hi = ci_upper
        )
      measure_label <- "Hedges' g"
      filename_suffix <- "g"
    } else {
      group_data <- group_data %>%
        filter(!is.na(lnRR) & !is.na(var_lnRR)) %>%
        mutate(
          yi = lnRR,
          vi = var_lnRR,
          ci_lo = lnRR_ci_lower,
          ci_hi = lnRR_ci_upper
        )
      measure_label <- "ln(Response Ratio)"
      filename_suffix <- "lnRR"
    }
    
    # Skip if no valid data
    if(nrow(group_data) == 0) next
    
    # Create plot labels
    group_data <- group_data %>%
      mutate(study_label = paste0(site, " (", year, ")"))
    
    # Sort by effect size
    group_data <- group_data %>%
      arrange(yi)
    
    # Create forest plot
    p <- ggplot(group_data, aes(x = yi, y = study_label)) +
      geom_point(aes(size = 1/sqrt(vi)), shape = 15) +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = model$b[1,1], color = "red", linetype = "solid") +
      geom_rect(aes(xmin = model$ci.lb, xmax = model$ci.ub, 
                    ymin = -Inf, ymax = Inf), 
                alpha = 0.1, fill = "red", inherit.aes = FALSE) +
      labs(
        title = paste("Forest Plot:", exp_type),
        subtitle = paste("Response Variable:", resp_var),
        x = paste0(measure_label, " (95% CI)"),
        y = "",
        caption = paste0(
          "Overall effect: ", round(model$b[1,1], 3),
          " (95% CI: ", round(model$ci.lb, 3), " to ", round(model$ci.ub, 3), ")",
          "\np = ", format.pval(model$pval, digits = 3),
          ", I² = ", round(model$I2, 1), "%",
          ", k = ", model$k, " studies"
        )
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        axis.text.y = element_text(hjust = 0),
        panel.grid.minor = element_blank()
      )
    
    # Generate filename
    filename <- paste0(
      "forest_plot_", 
      gsub("[^a-zA-Z0-9]", "_", exp_type), "_", 
      gsub("[^a-zA-Z0-9]", "_", resp_var), "_",
      filename_suffix, ".png"
    )
    
    filepath <- file.path(output_dir, filename)
    
    # Save plot
    ggsave(filepath, p, width = 10, height = max(6, 0.3 * nrow(group_data) + 2))
    
    # Store filename
    plot_files[[length(plot_files) + 1]] <- filepath
  }
  
  # Return list of generated files
  return(plot_files)
}

#=========================#
# REPORTING FUNCTIONS
#=========================#

# Function to create summary visualizations of the harmonized data
create_data_summary_plots <- function(harmonized_data, output_dir = ".") {
  # Validate input
  if(is.null(harmonized_data) || nrow(harmonized_data) == 0) {
    warning("No harmonized data provided for visualization")
    return(NULL)
  }
  
  # Make sure output directory exists
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # List to store plot filenames
  plot_files <- list()
  
  # Plot 1: Observations by site and experiment type
  p1 <- harmonized_data %>%
    group_by(site, experiment_type) %>%
    summarise(observations = n(), .groups = "drop") %>%
    ggplot(aes(x = site, y = observations, fill = experiment_type)) +
    geom_col() +
    labs(
      title = "Number of Observations by Site and Experiment Type",
      x = "Site",
      y = "Number of Observations",
      fill = "Experiment Type"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  # Save plot
  filename1 <- file.path(output_dir, "observations_by_site.png")
  ggsave(filename1, p1, width = 10, height = 6)
  plot_files[[length(plot_files) + 1]] <- filename1
  
  # Plot 2: Observations by response variable
  p2 <- harmonized_data %>%
    group_by(response_variable) %>%
    summarise(observations = n(), .groups = "drop") %>%
    arrange(desc(observations)) %>%
    head(15) %>%  # Top 15 response variables
    ggplot(aes(x = reorder(response_variable, observations), y = observations)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Number of Observations by Response Variable",
      subtitle = "Top 15 response variables",
      x = "",
      y = "Number of Observations"
    ) +
    theme_bw()
  
  # Save plot
  filename2 <- file.path(output_dir, "observations_by_response.png")
  ggsave(filename2, p2, width = 10, height = 6)
  plot_files[[length(plot_files) + 1]] <- filename2
  
  # Plot 3: Observations by year
  if("year" %in% names(harmonized_data)) {
    p3 <- harmonized_data %>%
      filter(!is.na(year)) %>%
      group_by(year) %>%
      summarise(observations = n(), .groups = "drop") %>%
      ggplot(aes(x = year, y = observations)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      labs(
        title = "Number of Observations by Year",
        x = "Year",
        y = "Number of Observations"
      ) +
      theme_bw()
    
    # Save plot
    filename3 <- file.path(output_dir, "observations_by_year.png")
    ggsave(filename3, p3, width = 10, height = 6)
    plot_files[[length(plot_files) + 1]] <- filename3
  }
  
  # Return list of generated files
  return(plot_files)
}

# Function to create an HTML report summarizing the meta-analysis
create_html_report <- function(harmonized_data, effect_sizes, meta_summary, 
                               output_dir = ".", title = "Ecological Data Harmonization Report") {
  # Validate inputs
  if(is.null(harmonized_data) || nrow(harmonized_data) == 0) {
    warning("No harmonized data provided for report")
    return(NULL)
  }
  
  # Make sure output directory exists
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create report content
  report <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    paste0("<title>", title, "</title>"),
    "<style>",
    "body { font-family: Arial, sans-serif; line-height: 1.6; margin: 40px; }",
    "h1, h2, h3 { color: #2c3e50; }",
    "table { border-collapse: collapse; width: 100%; margin-bottom: 20px; }",
    "th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }",
    "th { background-color: #f2f2f2; }",
    "tr:hover { background-color: #f5f5f5; }",
    ".container { max-width: 1200px; margin: 0 auto; }",
    ".summary { background-color: #f9f9f9; padding: 15px; border-radius: 5px; margin-bottom: 20px; }",
    ".highlight { color: #e74c3c; font-weight: bold; }",
    "img { max-width: 100%; height: auto; margin-bottom: 20px; }",
    "</style>",
    "</head>",
    "<body>",
    "<div class='container'>",
    
    # Header
    paste0("<h1>", title, "</h1>"),
    paste0("<p>Report generated on: ", format(Sys.time(), "%B %d, %Y at %H:%M:%S"), "</p>"),
    
    # Summary section
    "<div class='summary'>",
    "<h2>Summary Statistics</h2>",
    "<ul>",
    paste0("<li><strong>Total harmonized observations:</strong> ", nrow(harmonized_data), "</li>"),
    paste0("<li><strong>Number of sites:</strong> ", length(unique(harmonized_data$site)), "</li>"),
    paste0("<li><strong>Number of experiment types:</strong> ", length(unique(harmonized_data$experiment_type)), "</li>"),
    paste0("<li><strong>Number of response variables:</strong> ", length(unique(harmonized_data$response_variable)), "</li>")
  )
  
  # Add effect size info if available
  if(!is.null(effect_sizes) && nrow(effect_sizes) > 0) {
    report <- c(report,
                paste0("<li><strong>Number of effect size calculations:</strong> ", nrow(effect_sizes), "</li>")
    )
  }
  
  # Add meta-analysis info if available
  if(!is.null(meta_summary) && nrow(meta_summary) > 0) {
    report <- c(report,
                paste0("<li><strong>Number of meta-analyses:</strong> ", nrow(meta_summary), "</li>"),
                paste0("<li><strong>Significant effects (p < 0.05):</strong> ", sum(meta_summary$p_value < 0.05), "</li>")
    )
  }
  
  report <- c(report,
              "</ul>",
              "</div>",
              
              # Data overview section
              "<h2>Data Overview</h2>"
  )
  
  # Add site information
  site_counts <- harmonized_data %>%
    group_by(site) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  report <- c(report,
              "<h3>Observations by Site</h3>",
              "<table>",
              "<tr><th>Site</th><th>Number of Observations</th></tr>"
  )
  
  for(i in 1:nrow(site_counts)) {
    report <- c(report,
                paste0("<tr><td>", site_counts$site[i], "</td><td>", site_counts$count[i], "</td></tr>")
    )
  }
  
  report <- c(report, "</table>")
  
  # Add experiment type information
  exp_counts <- harmonized_data %>%
    group_by(experiment_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  report <- c(report,
              "<h3>Observations by Experiment Type</h3>",
              "<table>",
              "<tr><th>Experiment Type</th><th>Number of Observations</th></tr>"
  )
  
  for(i in 1:nrow(exp_counts)) {
    report <- c(report,
                paste0("<tr><td>", exp_counts$experiment_type[i], "</td><td>", exp_counts$count[i], "</td></tr>")
    )
  }
  
  report <- c(report, "</table>")
  
  # Add response variable information (top 10)
  resp_counts <- harmonized_data %>%
    group_by(response_variable) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    head(10)
  
  report <- c(report,
              "<h3>Top 10 Response Variables</h3>",
              "<table>",
              "<tr><th>Response Variable</th><th>Number of Observations</th></tr>"
  )
  
  for(i in 1:nrow(resp_counts)) {
    report <- c(report,
                paste0("<tr><td>", resp_counts$response_variable[i], "</td><td>", resp_counts$count[i], "</td></tr>")
    )
  }
  
  report <- c(report, "</table>")
  
  # Add meta-analysis results if available
  if(!is.null(meta_summary) && nrow(meta_summary) > 0) {
    report <- c(report,
                "<h2>Meta-Analysis Results</h2>",
                "<p>The following table shows the results of meta-analyses for different experiment types and response variables.</p>",
                "<table>",
                "<tr>",
                "<th>Experiment Type</th>",
                "<th>Response Variable</th>",
                "<th>Effect Measure</th>",
                "<th>Studies (k)</th>",
                "<th>Effect Size</th>",
                "<th>95% CI</th>",
                "<th>p-value</th>",
                "<th>I²</th>",
                "</tr>"
    )
    
    # Sort by significance
    meta_summary <- meta_summary %>%
      arrange(p_value)
    
    for(i in 1:nrow(meta_summary)) {
      row <- meta_summary[i, ]
      
      # Format effect size and CI
      effect_str <- sprintf("%.3f", row$effect_size)
      ci_str <- sprintf("%.3f to %.3f", row$ci_lower, row$ci_upper)
      
      # Format p-value
      p_str <- if(row$p_value < 0.001) {
        "<strong><0.001</strong>"
      } else {
        sprintf("%.3f", row$p_value)
      }
      
      # Add row class for significant results
      row_class <- if(row$p_value < 0.05) " class='highlight'" else ""
      
      report <- c(report,
                  paste0("<tr", row_class, ">"),
                  paste0("<td>", row$experiment_type, "</td>"),
                  paste0("<td>", row$response_variable, "</td>"),
                  paste0("<td>", row$effect_measure, "</td>"),
                  paste0("<td>", row$k, "</td>"),
                  paste0("<td>", effect_str, "</td>"),
                  paste0("<td>", ci_str, "</td>"),
                  paste0("<td>", p_str, "</td>"),
                  paste0("<td>", sprintf("%.1f%%", row$i2), "</td>"),
                  "</tr>"
      )
    }
    
    report <- c(report, "</table>")
  }
  
  # Include visualization images
  report <- c(report,
              "<h2>Visualizations</h2>",
              "<p>The following visualizations summarize the harmonized data and meta-analysis results.</p>"
  )
  
  # Include data summary plots
  vis_files <- list.files(output_dir, pattern = "observations_by_.+\\.png$", full.names = FALSE)
  if(length(vis_files) > 0) {
    for(file in vis_files) {
      report <- c(report,
                  paste0("<h3>", gsub("_", " ", tools::file_path_sans_ext(file)), "</h3>"),
                  paste0("<img src='", file, "' alt='", tools::file_path_sans_ext(file), "'>")
      )
    }
  }
  
  # Include forest plots if available
  forest_files <- list.files(output_dir, pattern = "forest_plot_.+\\.png$", full.names = FALSE)
  if(length(forest_files) > 0) {
    report <- c(report, "<h2>Forest Plots</h2>")
    
    for(file in forest_files) {
      # Extract info from filename
      parts <- strsplit(tools::file_path_sans_ext(file), "_")[[1]]
      exp_type <- gsub("_", " ", paste(parts[3:(length(parts)-1)], collapse = " "))
      measure <- parts[length(parts)]
      
      title <- paste0("Forest Plot: ", exp_type, " (", 
                      ifelse(measure == "g", "Hedges' g", "ln(Response Ratio)"), ")")
      
      report <- c(report,
                  paste0("<h3>", title, "</h3>"),
                  paste0("<img src='", file, "' alt='", title, "'>")
      )
    }
  }
  
  # Close HTML tags
  report <- c(report,
              "</div>",
              "</body>",
              "</html>"
  )
  
  # Write the report to a file
  report_file <- file.path(output_dir, "meta_analysis_report.html")
  writeLines(report, report_file)
  
  message("Report generated: ", report_file)
  return(report_file)
}

#=========================#
# MAIN EXECUTION SCRIPT
#=========================#

# Main function to run the full harmonization and meta-analysis pipeline
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
  
  # Step 3: Combine all harmonized data
  message("Combining harmonized data...")
  if(length(harmonized_list) == 0) {
    stop("No datasets were successfully harmonized")
  }
  
  all_harmonized_data <- bind_rows(harmonized_list)
  
  # Save harmonized data
  write_csv(all_harmonized_data, file.path(output_dir, "all_harmonized_data.csv"))
  
  # Step 4: Calculate effect sizes
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

# Example usage:
# results <- run_harmonization_pipeline("path/to/data", "path/to/output")