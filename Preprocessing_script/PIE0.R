# Load necessary package
library(dplyr)
library(readr)

# Your file ID: find the share link for your data file,
# The file ID is the part between /d/ and /view?, 
# don't forget to make the file open to anyone with the link
file_id <- "1WOk0ED2-XU2p0fTrx2Zx0c8G8NDGLJWg"

# Construct the download URL
url <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)

# Read the CSV file
df <- read.csv(url)



# General structure and summary, 
#don't have to run this but good to know the column names

str(df)              # Structure of the dataset
summary(df)          # Summary statistics for each column
head(df)             # First few rows

# This is a customized function 
# to summarize all the numbers with the same value in
# the mentioned columns, for example, here we are summarizing
# any data with the same Year, Species, Creek... etc
# It gives you mean, standard deviation and total for all the measured variables.
summarize_by_columns <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(where(is.numeric), list(
      sd   = ~sd(.x, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE),
      sum  = ~sum(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop")
}

sum_df <- summarize_by_columns(df, c("Year", "Species","Creek","Branch"))
head(sum_df) 
# saving the processed data to a new spreadsheet and put it into the ready folder!
# Sip your coffee and carry on to the next dataset!

write.csv(sum_df, "Ready_data/PIE1.csv", row.names = FALSE)


