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
file_info <- drive_ls(path = as_id(folder_id), pattern = "CDR_flora manipulation_plant_aboveground_biomass_1995-2005_gm2.csv")

#Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)


#Read in the data
CDRabg_biomass_data <- read_csv(temp_file)

#Getting a feel for the data
head(CDRabg_biomass_data)
str(CDRabg_biomass_data)
summary(CDRabg_biomass_data)
unique(CDRabg_biomass_data$`Plot number`)
unique(CDRabg_biomass_data$`Experiment number`)
unique(CDRabg_biomass_data$`Year`)
unique(CDRabg_biomass_data$`Grazed`)

#Renaming Columns
CDRabg_biomass_data <- CDRabg_biomass_data %>%
  rename(Taxa = `...5`)

CDRabg_biomass_data <- CDRabg_biomass_data %>%
  rename(Plot = `Plot number`)

CDRabg_biomass_data <- CDRabg_biomass_data %>%
  rename(Date = `Sampling date (YYMMDD)`)

#Changing Date format from yymmdd to ddmmyyyy
CDRabg_biomass_data <- CDRabg_biomass_data %>%
  mutate(Date = format(ymd(Date), "%d-%m-%Y"))


#Eliminating taxa specific biomass measurements by combining across species
#So that there's one measurement per date, plot, and treatment/grazed
CDRabg_totals <- CDRabg_biomass_data %>%
  group_by(Date, Plot,`Grazed`) %>%              # keeps these columns intact
  summarise(total_mass = sum(`Species Biomass (g/m2)`, na.rm = TRUE),  
            .groups = "drop")


# unique Row combos kept; Should be TRUE
nrow(CDRabg_totals) == nrow(distinct(CDRabg_biomass_data, Date, Plot,`Grazed`))

# Confirm totals add up to original (ignoring NA); Should be TRUE
sum(CDRabg_totals$total_mass) == sum(CDRabg_biomass_data$`Species Biomass (g/m2)`, na.rm = TRUE)

#Now Overwrite the original data to reflect the summed ABG mass
CDRabg_biomass_data <- CDRabg_biomass_data %>%
  group_by(Date, Plot,`Grazed`) %>%
    summarise(total_mass = sum(`Species Biomass (g/m2)`, na.rm = TRUE),  
              .groups = "drop")


#Save [pre]processed csv
write.csv(CDRabg_biomass_data,
          "CDR_AbovegroundBiomass_1995-2005_gm2_processed.csv",
          row.names = FALSE)
