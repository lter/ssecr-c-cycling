# Carla López Lloreda

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
      sd   = ~sd(.x, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}

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
folder_id <- "191tCGsKz2ghXsbFPFbirjp9G3RrgrI3W"

# Find the specific file in the folder
file_info <- drive_ls(path = as_id(folder_id), pattern = "LUQ_gas_fluxes_2003-2010.csv")

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
data <- read_csv(temp_file)

# check out columns and summary
# summary(data)
# head(data)

# fix date
data$DATE <- as.POSIXct(data$DATE, format = "%m/%d/%Y")

# select columns and separate out info in location column
data <- data %>%
  select(-YEAR, -MONTH) %>%
  separate("Block-plot-rep", into = c("Block", "Plot", "Rep"), sep = "-", remove = FALSE) %>%
  mutate(Block_Plot = paste(Block, Plot, sep = "-"))

# summarize means and sd of the dataset
sum_df <- summarize_by_columns(data2, c("Block_Plot", "DATE"))

# save file
write.csv(sum_df, "Ready_data/LUQ_GHG_fluxes_2003-2010.csv")
