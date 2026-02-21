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
file_info <- drive_ls(path = as_id(folder_id), pattern = "CDR__nutrient addition.soil manipulation.precipitation control_sIDE_aboveground_biomass_2016-2020_gm2.csv")

#Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)


#Read in the data
CDRabg_biomass_data <- read_csv(temp_file)

#Getting a feel for the data
head(CDRabg_biomass_data)
str(CDRabg_biomass_data)
summary(CDRabg_biomass_data)
unique(CDRabg_biomass_data$treatment)
unique(CDRabg_biomass_data$taxa)
unique(CDRabg_biomass_data$plot)


#Reformatting the date from mdy to dmy 
CDRabg_biomass_data$date <- format(as.Date(CDRabg_biomass_data$date, "%m/%d/%Y"), "%d/%m/%Y")


#Aggregating Redundant Taxa
normalize_taxa <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_to_lower(x)
  x <- str_squish(x)
  x <- str_replace_all(x, "[[:punct:]]+$", "")   # drop trailing punctuation if any

  # collapse plurals/synonyms to singular format
  x <- str_replace(x, "^forbs?$",   "forb")
  x <- str_replace(x, "^grasses?$", "grass")
  x <- str_replace(x, "^legumes?$", "legume")

  #Uppercase + plural for clean taxa names 
  recode(x,
    "forb"   = "Forbs",
    "grass"  = "Grasses",
    "legume" = "Legumes",
    "litter" = "Litter",
    "woody"  = "Woody",
    .default = str_to_title(x)
  )
}

# #Overwrite old taxa column with corrected taxa names
CDRabg_biomass_data <- CDRabg_biomass_data %>%
  mutate(taxa = normalize_taxa(taxa))

#Check names
unique(CDRabg_biomass_data$taxa)


#Summarizing aboveground biomass so there is 1 value per plot 
#instead of the original 5 values (1/taxa)
CDRabg_totals <- CDRabg_biomass_data %>%
  group_by(date, plot, treatment) %>%              # keeps these columns intact
  summarise(total_mass = sum(mass, na.rm = TRUE),  
            .groups = "drop")

# unique Row combos kept; Should be TRUE
nrow(CDRabg_totals) == nrow(distinct(CDRabg_biomass_data, date, plot, treatment))

# Confirm totals add up to original (ignoring NA); Should be TRUE
sum(CDRabg_totals$total_mass) == sum(CDRabg_biomass_data$mass, na.rm = TRUE)

#Now Overwrite the original data to reflect the summed ABG mass
CDRabg_biomass_data <- CDRabg_biomass_data %>%
  group_by(date, plot, treatment) %>%
  summarise(
    total_mass = sum(mass, na.rm = TRUE),   
    .groups = "drop"
  )


#Save [pre]processed csv
write.csv(CDRabg_biomass_data,
          "CDR_AbovegroundBiomass_2016-2020_gm2_processed.csv",
          row.names = FALSE)
