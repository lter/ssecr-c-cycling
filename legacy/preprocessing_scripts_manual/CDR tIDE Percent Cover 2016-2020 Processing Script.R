#Install Necessary Packages
install.packages("googledrive")
install.packages("readr")

#Load Necessary Packages
library(googledrive)
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)


# Authenticate with Google Drive, follow instructions in console 
# And then follow instructions in the browser window that opens
drive_auth()

#Grab the folder ID from designated folder in Google Drive
#The ID will appear after the "/folders/" 
#In the url e.g., "https://drive.google.com/drive/u/1/folders/Th!S!sth3P4rTUw4nt"
folder_id <- "191tCGsKz2ghXsbFPFbirjp9G3RrgrI3W"

#Paste file name from the designated folder ID which was set in the line above
file_info <- drive_ls(path = as_id(folder_id), pattern = "CDR_nutrient addition.soil manipulation.precipitation cover_tIDE_percent_cover_2016-2020.csv")

#Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)

#Read in the data
CDR_PercentCov <- read_csv(temp_file)

#Getting a feel for the data
head(CDR_PercentCov)
str(CDR_PercentCov)
summary(CDR_PercentCov)
unique(CDR_PercentCov$taxa)
unique(CDR_PercentCov$subplot)
unique(CDR_PercentCov$shelter)
unique(CDR_PercentCov$fertilized)
unique(CDR_PercentCov$fomerly_irrigated)


#Reformatting the date from yyyymmdd to ddmmyyyy 
CDR_PercentCov$date <- format(as.Date(CDR_PercentCov$date, "%Y/%m/%d"), "%d/%m/%Y")


#Correcting column name
CDR_PercentCov <- CDR_PercentCov %>%
  rename(`formerly irrigated` = `fomerly_irrigated`)

#Aggregating and overwriting the plot and subplot columns into 1 column
CDR_PercentCov <-CDR_PercentCov %>%
  mutate(plot_subplot = paste(plot, subplot, sep = "_")) %>%
  relocate(plot_subplot, .before = subplot) %>%  # put new column before subplot
  select(-plot, -subplot)  

#Collapsing 3 treatment related columns into 1 single column
CDR_PercentCov <- CDR_PercentCov %>%
  mutate(
   shelter_fertilized_irrigated = paste(
      shelter,
      fertilized,
      gsub(" ", "", `formerly irrigated`),  
      sep = "_"
    )
  ) %>%
  select(-shelter, -fertilized, -`formerly irrigated`) %>%
  relocate(shelter_fertilized_irrigated, .before = taxa) 

#Aggregating taxa column by plot, date, & shelter_fertilized_irrigated, since separating
#by taxa is not of interest for our study
CDR_PercentCov_totals <- CDR_PercentCov %>%
  group_by(date, plot_subplot, shelter_fertilized_irrigated) %>%              # keeps these columns intact
  summarise(total_cover = sum(cover, na.rm = TRUE),  
            .groups = "drop")


# unique Row combos kept; Should be TRUE
nrow(CDR_PercentCov_totals) == nrow(distinct(CDR_PercentCov, date, plot_subplot, shelter_fertilized_irrigated))


# Confirm totals add up to original (ignoring NA); Should be TRUE
sum(CDR_PercentCov_totals$total_cover, na.rm = TRUE) ==
  sum(CDR_PercentCov$cover, na.rm = TRUE)


#Now Overwrite the original data to reflect the summed Per Cov
CDR_PercentCov <- CDR_PercentCov %>%
  group_by(date, plot_subplot, shelter_fertilized_irrigated) %>%
  summarise(
    total_cover = sum(cover, na.rm = TRUE),   
    .groups = "drop"
  )


write.csv(CDRabg_biomass_data,
          "CDR_tIDEPercentCover_2016-2020_processed.csv",
          row.names = FALSE)
