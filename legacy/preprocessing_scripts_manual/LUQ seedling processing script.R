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
folder_id <- "191tCGsKz2ghXsbFPFbirjp9G3RrgrI3W"

# Find the specific file in the folder
file_info <- drive_ls(path = as_id(folder_id), pattern = "LUQ_seedling_growth_2003-2021.csv")

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
data <- read_csv(temp_file)

# Adding treatment information

data <- data %>%
  mutate(treatment = case_when(
    BLOCK == "A" & PLOT == 1 ~ "Control",
    BLOCK == "A" & PLOT == 2 ~ "Trim + clear",
    BLOCK == "A" & PLOT == 3 ~ "Trim + debris",
    BLOCK == "A" & PLOT == 4 ~ "No trim + debris",
    
    BLOCK == "B" & PLOT == 1 ~ "Control",
    BLOCK == "B" & PLOT == 2 ~ "Trim + debris",
    BLOCK == "B" & PLOT == 3 ~ "No trim + debris",
    BLOCK == "B" & PLOT == 4 ~ "Trim + clear",
    
    BLOCK == "C" & PLOT == 1 ~ "No trim + debris",
    BLOCK == "C" & PLOT == 2 ~ "Trim + debris",
    BLOCK == "C" & PLOT == 3 ~ "Trim + clear",
    BLOCK == "C" & PLOT == 4 ~ "Control",
    
    TRUE ~ NA_character_
  ))

# summarize means and sd of the dataset

sum_df <- data %>%
  group_by(BLOCK, PLOT, SEEDLINGPLOT) %>%
  summarise(Diameter = mean(DIAMETER))
    
sum_df <- data %>%
  select(START_DATE, BLOCK, PLOT, HEIGHT, DIAMETER, treatment) %>%
  summarize_by_columns(c("BLOCK", "PLOT", "treatment", "START_DATE"))

# save file
write.csv(sum_df, "Ready_data/LUQ_seedling_2003-2021_processed.csv")
