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
file_info <- drive_ls(path = as_id(folder_id), pattern = "CDR_nutrient.addition_soil.carbon.flux_1999-2005.csv")

#Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)


#Read in the data
CDR_SoilFlx <- read_csv(temp_file)

#Getting a feel for the data
head(CDR_SoilFlx)
str(CDR_SoilFlx)
summary(CDR_SoilFlx)
unique(CDR_SoilFlx$`Annual Nitrogen fertilizer addition (g/m2/year)`)#Can be removed since there's only 1 unqiue value
unique(CDR_SoilFlx$`Point`)


#Renaming values in canopy cover column to be more informative
CDR_SoilFlx <- CDR_SoilFlx %>%
  mutate(`1996 Canopy cover` = recode(`1996 Canopy cover`,
                                      "in"  = "shaded",
                                      "out" = "unshaded")) %>%
  relocate(`1996 Canopy cover`, .before = `Annual Nitrogen fertilizer addition (g/m2/year)`)

CDR_SoilFlx <- CDR_SoilFlx %>%
  rename(Shaded_status = `1996 Canopy cover`)


#Making 1 column for treatments administered
CDR_SoilFlx <- CDR_SoilFlx %>%
  mutate(
    `Cover_Fertilizer(g/m2/year)` = paste0(Shaded_status, "_", `Annual Nitrogen fertilizer addition (g/m2/year)`)
  ) %>%
  select(-Shaded_status, -`Annual Nitrogen fertilizer addition (g/m2/year)`) %>%
  relocate(`Cover_Fertilizer(g/m2/year)`, .before = SCF)


#Removing subplot aspects to avoid pseudoreplication
CDR_SoilFlx_totals <- CDR_SoilFlx %>%
  group_by(Date, Plot, `Cover_Fertilizer(g/m2/year)`) %>%
  summarise(total_SCF = sum(SCF, na.rm = TRUE), .groups = "drop")


# unique Row combos kept; Should be TRUE
nrow(CDR_SoilFlx_totals) == nrow(distinct(CDR_SoilFlx, Date, Plot, `Cover_Fertilizer(g/m2/year)`))

# Confirm totals add up to original (ignoring NA); Should be TRUE
sum(CDR_SoilFlx_totals$total_SCF) == sum(CDR_SoilFlx$ `SCF`, na.rm = TRUE)

#Now Overwrite the original data to reflect the summed %Carbon
CDR_SoilFlx <- CDR_SoilFlx %>%
  group_by(Date, Plot, `Cover_Fertilizer(g/m2/year)`) %>%
  summarise(total_SCF = sum(SCF, na.rm = TRUE), .groups = "drop")


write.csv(CDR_SoilFlx,
          "CDR_SoilCarbonFlux_1999-2005_processed.csv",
          row.names = FALSE)
