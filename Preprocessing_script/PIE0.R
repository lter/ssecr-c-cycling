# Load necessary package
library(dplyr)
library(readr)

# Your file ID: find the share link for your data file,
# The file ID is the part between /d/ and /view?, 
# don't forget to make the file open to anyone with the link
file_id <-  "1Ill4MrO0rmowhkvNBu62zkO04O-jc1xP"
  # NWT "1RztkWdZEq9pGXdhnMZp-B9hpOEQjqQ4E"
  # SEV "1EJc1LbdqN2dyjG1x-GZB8U2n1DshUGh2" 
  # VCR Flora Manipulation "1ohOuZ_2YXsohG2cK7bDsvzALJIkOQJdV"
  # GCE Clear Cutting "1JFQQbocvR653UvkONsF_HPD6gt6E8xeK"
  # NTL Nutrient Addition "1aRxYd1DVEt2oq5zryWk8jqeW61DgPlnx"
  # PIE Nutrient Addition "1WOk0ED2-XU2p0fTrx2Zx0c8G8NDGLJWg"

# Construct the download URL
url <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)

# Read the CSV file
df <- read.csv(url,skip = 0)
#df <- df[c(-1,-2),]
#for (i in 6:31){
#df[,i] <- as.numeric(df[,i])
#}

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

# use command unique(df$block) to find variables that needs to be summarized
sum_df <- summarize_by_columns(df, c("YEAR","MONTH","SITE","TRANSECT","TREATMENT"))
head(sum_df) 
# saving the processed data to a new spreadsheet and put it into the ready folder!
# Sip your coffee and carry on to the next dataset!

write.csv(sum_df, "Ready_data/SBC_algal_biomass_2008-2024_processed.csv", row.names = FALSE)


