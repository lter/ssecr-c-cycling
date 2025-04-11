# Install and load necessary packages if not already installed
if (!requireNamespace("googledrive", quietly = TRUE)) {
  install.packages("googledrive")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}

library(googledrive)
library(readr)

# Authenticate with Google Drive (this will open a browser window)
drive_auth()

# File ID from the Google Drive URL
# The ID is the part after /folders/ in your URL
folder_id <- "191tCGsKz2ghXsbFPFbirjp9G3RrgrI3W"

# Find the specific file in the folder
file_info <- drive_ls(path = as_id(folder_id), pattern = "ARC_Fertilizer_Biomass_1982-2015_gm2.csv")

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
biomass_data <- read_csv(temp_file)

# Now you can work with the data
head(biomass_data)
str(biomass_data)
summary(biomass_data)

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Assuming biomass_data is already loaded into R
# If not, you'd load it first:
# biomass_data <- read_csv("ARC_Fertilizer_Biomass_1982-2015_gm2.csv")

# Convert Date column to proper date format
biomass_data <- biomass_data %>%
  mutate(Date = dmy(Date))

# Function to convert specific columns from character to numeric
convert_to_numeric <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df) && is.character(df[[col]])) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  return(df)
}

# Identify character columns that should be numeric in gm2 data
char_cols_to_convert <- names(biomass_data)[grepl("gm2$", names(biomass_data)) & 
                                              sapply(biomass_data, is.character)]

# Convert them to numeric
biomass_data <- convert_to_numeric(biomass_data, char_cols_to_convert)

# Calculate total biomass per plot per timepoint
biomass_summary <- biomass_data %>%
  # Group by the key identifiers
  group_by(Date, Site, Treatment) %>%
  # Summarize to get the total biomass per plot
  summarize(
    # Calculate total biomass using the Average g/m^2 column
    Total_Biomass_gm2 = sum(`Average g/m^2`, na.rm = TRUE),
    
    # Calculate biomass for individual plots if needed
    Block1_Biomass_gm2 = sum(B1Q1gm2 + B1Q2gm2 + B1Q3gm2 + B1Q4gm2 + B1Q5gm2, na.rm = TRUE),
    Block2_Biomass_gm2 = sum(B2Q1gm2 + B2Q2gm2 + B2Q3gm2 + B2Q4gm2 + B2Q5gm2, na.rm = TRUE),
    Block3_Biomass_gm2 = sum(B3Q1gm2 + B3Q2gm2 + B3Q3gm2 + B3Q4gm2 + B3Q5gm2, na.rm = TRUE),
    Block4_Biomass_gm2 = sum(B4Q1gm2 + B4Q2gm2 + B4Q3gm2 + B4Q4gm2 + B4Q5gm2, na.rm = TRUE),
    
    # Count the number of measurements contributing to each summary
    N_measurements = n()
  ) %>%
  ungroup()

# Split by above/below ground biomass
biomass_by_position <- biomass_data %>%
  # Group by the key identifiers plus the biomass category (above/below ground)
  group_by(Date, Site, Treatment, `Biomass Categroy`) %>%
  # Summarize to get the above/below ground total biomass
  summarize(
    Biomass_gm2 = sum(`Average g/m^2`, na.rm = TRUE),
    N_measurements = n()
  ) %>%
  # Spread the result to have separate columns for different biomass categories
  pivot_wider(
    names_from = `Biomass Categroy`,
    values_from = Biomass_gm2,
    values_fill = list(Biomass_gm2 = 0)
  ) %>%
  ungroup()

# Create a more detailed summary including biomass by tissue type
biomass_by_tissue <- biomass_data %>%
  # Group by the key identifiers plus tissue type
  group_by(Date, Site, Treatment, Tissue) %>%
  # Summarize
  summarize(
    Biomass_gm2 = sum(`Average g/m^2`, na.rm = TRUE),
    N_measurements = n()
  ) %>%
  ungroup()

# Output the results
print("Summary of total biomass per plot per timepoint:")
print(biomass_summary)

print("Summary of above/below ground biomass:")
print(biomass_by_position)

print("Summary of biomass by tissue type:")
print(biomass_by_tissue)

# Save the results to CSV files
write_csv(biomass_summary, "biomass_summary_by_plot.csv")
write_csv(biomass_by_position, "biomass_summary_by_position.csv")
write_csv(biomass_by_tissue, "biomass_summary_by_tissue.csv")

# Create visualizations
# Plot total biomass over time by treatment
ggplot(biomass_summary, aes(x = Date, y = Total_Biomass_gm2, color = Treatment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site, scales = "free_y") +
  labs(
    title = "Total Biomass Over Time by Treatment",
    x = "Date",
    y = "Total Biomass (g/m²)",
    color = "Treatment"
  ) +
  theme_minimal()

# Save the plot
ggsave("biomass_over_time.png", width = 10, height = 8)