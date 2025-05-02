library(googledrive)
library(readr)

# Authenticate with Google Drive (this will open a browser window)
drive_auth()

# File ID from the Google Drive URL
# The ID is the part after /folders/ in your URL
folder_id <- "191tCGsKz2ghXsbFPFbirjp9G3RrgrI3W"

# Find the specific file in the folder
file_info <- drive_ls(path = as_id(folder_id), pattern = "ARC_Fertilizer_Biomass_1982-2015_gm2.csv")
file_info <- drive_ls(path = as_id(folder_id), pattern = "LUQ_gas_fluxes_2003-2010.csv")


# Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

# Read the CSV file
data <- read_csv(temp_file)

summary(data)
head(data)
