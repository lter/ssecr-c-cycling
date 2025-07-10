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
file_info <- drive_ls(path = as_id(folder_id), pattern = "AND_Logging_DBAHeight_2002-2021_m.csv")

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
data <- read_csv(temp_file)

# check out columns and summary
summary(data)
head(data)

ggplot(data, aes(x=SAMPLEDATE, y = DBA, color = WATERSHED)) +
  geom_point()

# need to finish adding plot summarizing

new_data <- data %>%
  select(-COMMENTS)

# summarize means and sd of the dataset
sum_df <- summarize_by_columns(data, c("SAMPLEDATE"))

# save file
write.csv(new_data, "Ready_data/AND_plant_biomass_2002-2021_processed.csv")
