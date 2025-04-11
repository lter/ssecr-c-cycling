
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
