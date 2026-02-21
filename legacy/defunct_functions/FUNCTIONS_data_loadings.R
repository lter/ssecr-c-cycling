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