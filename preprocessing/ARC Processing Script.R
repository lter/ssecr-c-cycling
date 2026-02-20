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

# Filter for the specific site
biomass_data <- biomass_data %>%
  filter(Site == "Toolik Tussock 1981 plots")

# Convert Date column to proper date format
# The format seems to be causing issues, so let's be more explicit
biomass_data <- biomass_data %>%
  mutate(Date = parse_date_time(Date, orders = c("dmy", "mdy")))

# Ensure all necessary columns are numeric
biomass_data <- biomass_data %>%
  mutate(across(ends_with("gm2"), ~as.numeric(as.character(.))))

# Create a function to categorize the biomass
# First, let's check what unique categories exist
unique_categories <- unique(biomass_data$`Biomass Categroy`)
print(paste("Unique biomass categories:", paste(unique_categories, collapse = ", ")))

# Create the categorization function based on existing categories
categorize_biomass <- function(category) {
  case_when(
    category %in% c("new above", "old above") ~ "above",
    category == "below" ~ "below",
    category == "non-vascular" ~ "non_vascular",
    category == "Litter" ~ "litter",
    TRUE ~ "other"
  )
}

# Add a simplified biomass category column
biomass_data <- biomass_data %>%
  mutate(biomass_position = categorize_biomass(`Biomass Categroy`))

# Calculate total biomass per plot per timepoint, separated by above/below
biomass_summary <- biomass_data %>%
  # Group by the key identifiers
  group_by(Date, Treatment, biomass_position) %>%
  # Summarize to get the total biomass by position for each timepoint/treatment
  summarize(
    # Use the Average g/m^2 column for the main calculation
    Biomass_gm2 = sum(`Average g/m^2`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot wider to have separate columns for above/below/etc.
  pivot_wider(
    names_from = biomass_position,
    values_from = Biomass_gm2,
    values_fill = list(Biomass_gm2 = 0)
  )

# Use a safer approach to calculate the total that doesn't require all columns to exist
biomass_summary <- biomass_summary %>%
  rowwise() %>%
  mutate(
    # Use c_across to sum all columns except Date and Treatment
    Total_Biomass_gm2 = sum(c_across(-c(Date, Treatment)), na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate biomass by block
block_biomass_summary <- biomass_data %>%
  # Group by relevant factors 
  group_by(Date, Treatment, biomass_position) %>%
  # Summarize by block
  summarize(
    Block1_Biomass_gm2 = sum(B1Q1gm2 + B1Q2gm2 + B1Q3gm2 + B1Q4gm2 + B1Q5gm2, na.rm = TRUE),
    Block2_Biomass_gm2 = sum(B2Q1gm2 + B2Q2gm2 + B2Q3gm2 + B2Q4gm2 + B2Q5gm2, na.rm = TRUE),
    Block3_Biomass_gm2 = sum(B3Q1gm2 + B3Q2gm2 + B3Q3gm2 + B3Q4gm2 + B3Q5gm2, na.rm = TRUE),
    Block4_Biomass_gm2 = sum(B4Q1gm2 + B4Q2gm2 + B4Q3gm2 + B4Q4gm2 + B4Q5gm2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot to get above/below categories
  pivot_wider(
    names_from = biomass_position,
    values_from = c(Block1_Biomass_gm2, Block2_Biomass_gm2, Block3_Biomass_gm2, Block4_Biomass_gm2),
    values_fill = 0
  )

# Display the results
print("Biomass summary by Date and Treatment with above/below separation:")
print(biomass_summary)

print("Block-specific biomass summary:")
print(block_biomass_summary)


# Convert block_biomass_summary from wide to long format
# First pivot the block columns to create a long format by block
block_biomass_long <- block_biomass_summary %>%
  pivot_longer(
    cols = starts_with("Block"),
    names_to = c("Block", ".value"),
    names_pattern = "(Block\\d)_Biomass_(\\w+)"
  )

# Now we need to pivot the biomass position columns
block_biomass_long <- block_biomass_long %>%
  pivot_longer(
    cols = -c(Date, Treatment, Block),
    names_to = "biomass_position",
    values_to = "Biomass_gm2"
  )

# Clean up the Block column to just keep the block number
block_biomass_long <- block_biomass_long %>%
  mutate(Block = str_replace(Block, "Block", ""))

# View the result
head(block_biomass_long)

# If you need to save this to a file
# write_csv(block_biomass_long, "block_biomass_long_format.csv")



# Starting from the block_biomass_long data we already created
# Filter for only "gm2_above" and "gm2_below" biomass positions (the actual column names)
biomass_for_plot <- block_biomass_long %>%
  filter(biomass_position %in% c("gm2_above", "gm2_below"))

# Fix the biomass position names for better display in the plot
biomass_for_plot <- biomass_for_plot %>%
  mutate(biomass_position = case_when(
    biomass_position == "gm2_above" ~ "above",
    biomass_position == "gm2_below" ~ "below",
    TRUE ~ biomass_position
  ))

# Calculate mean and SE by Treatment, Date, and biomass position
biomass_summary_stats <- biomass_for_plot %>%
  group_by(Date, Treatment, biomass_position) %>%
  summarise(
    mean_biomass = mean(Biomass_gm2, na.rm = TRUE),
    se_biomass = sd(Biomass_gm2, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Create a more readable date format for the plot
biomass_summary_stats <- biomass_summary_stats %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    Date_Label = paste(Month, Year)
  )

# Create the plot
biomass_plot <- ggplot(biomass_summary_stats, 
                       aes(x = Date, y = mean_biomass, color = Treatment, 
                           group = interaction(Treatment, biomass_position))) +
  geom_point(size = 3, position = position_dodge(width = 10)) +
  geom_line(position = position_dodge(width = 10)) +
  geom_errorbar(aes(ymin = mean_biomass - se_biomass, 
                    ymax = mean_biomass + se_biomass),
                width = 10, position = position_dodge(width = 10)) +
  facet_wrap(~ biomass_position, scales = "free_y", ncol = 1) +
  labs(
    title = "Biomass by Treatment Over Time",
    subtitle = "Mean ± SE for Above and Below Ground Biomass",
    x = "Date",
    y = "Biomass (g/m²)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  scale_color_brewer(palette = "Set1")

# Display the plot
print(biomass_plot)

# Alternative visualization with dates as categorical x-axis
biomass_plot_alt <- ggplot(biomass_summary_stats, 
                           aes(x = Date_Label, y = mean_biomass, color = Treatment, 
                               group = Treatment)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_biomass - se_biomass, 
                    ymax = mean_biomass + se_biomass),
                width = 0.3, position = position_dodge(width = 0.5)) +
  facet_wrap(~ biomass_position, scales = "free_y", ncol = 1) +
  labs(
    title = "Biomass by Treatment Over Time",
    subtitle = "Mean ± SE for Above and Below Ground Biomass",
    x = "",
    y = "Biomass (g/m²)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  scale_color_brewer(palette = "Set1")

# Display the alternative plot
print(biomass_plot_alt)

# Save the plots if needed
# ggsave("biomass_by_treatment_time.png", biomass_plot, width = 10, height = 8)
# ggsave("biomass_by_treatment_time_alt.png", biomass_plot_alt, width = 10, height = 8)