# Processed by: Carla López Lloreda

# load libraries
library(googledrive)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# function to summarize
summarize_by_columns <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(where(is.numeric), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}

# Authenticate with Google Drive (this will open a browser window)
# drive_auth()

# File ID from the Google Drive URL
# The ID is the part after /folders/ in your URL
folder_id <- "1GmISK_OKXJ_VBKsTkoS_4FTgsi7dgEz9"

# Find the specific file in the folder
file_info <- drive_ls(path = as_id(folder_id), pattern = "AND_Logging_DBAHeight_2002-2021_m.csv")

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
data_exp <- read_csv(temp_file)

# Download the control data
file_info <- drive_ls(path = as_id(folder_id), pattern = "AND_Logging-control_DBH_1910-2023.csv")

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
data_control <- read_csv(temp_file)

# Filtering control data for same time period as experimental

data_control$SAMPLEDATE <- as.Date(data_control$SAMPLEDATE, format = "%m/%d/%Y")
data_control <- filter(data_control, SAMPLEDATE > "2002-06-18")

data_control$WATERSHED <- "WS08"

data_join <- bind_rows(data_exp, data_control)

# summarize means and sd of the dataset by plot
sum_df <- data_join %>%
  select(WATERSHED,PLOT,YEAR,DBA,HEIGHT,DBH, PLOTID,SAMPLEDATE) %>%
  summarize_by_columns(c("SAMPLEDATE", "WATERSHED", "PLOT", "YEAR"))

# save file
write.csv(sum_df, "Ready_data/AND_plant_biomass_2002-2021_processed.csv")
