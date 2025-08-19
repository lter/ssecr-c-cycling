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
file_info <- drive_ls(path = as_id(folder_id), pattern = "CDR_nutrient.addition_percent.carbon_1982-2011.csv")

#Download the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
drive_download(file = file_info$id, path = temp_file, overwrite = TRUE)


#Read in the data
CDR_PerCar <- read_csv(temp_file)


#Getting a feel for the data
head(CDR_PerCar)
str(CDR_PerCar)
summary(CDR_PerCar)
unique(CDR_PerCar$Exp)#Can be removed since there's only 1 unqiue value
unique(CDR_PerCar$Field)#Should probably be combined with Plot
unique(CDR_PerCar$Sample)#Not really clear on what this is, but seems irrelevant
unique(CDR_PerCar$Depth)#Can be removed since all samples were collected from 0-20cm
unique(CDR_PerCar$NTrt)#Seems like nominal short form of Naddition (e.g., treatment 2 rec'd 25mL of fertilizer but only latter was used), can be removed
unique(CDR_PerCar$NAdd)#Keep, calculated nitrogen added
unique(CDR_PerCar$NitrAdd)#Ammonnium Nitrate added aka fertilizer. Multiply this and you get Nadd; remove
unique(CDR_PerCar$`NAtm+NAdd`)#Can be removed as well me thinks, as it's just +1 of Nadd

#Aggregating and overwriting the plot and field columns into 1 column
CDR_PerCar <-CDR_PerCar %>%
  mutate(Field_Plot = paste(Field, Plot, sep = "_")) %>%
  relocate(Field_Plot, .before = Sample) %>%  # put new column before Sample column
  select(-Plot, -Field)  


#Combining %Carbon across samples collected from each plot on a given date
CDR_PerCar_totals <- CDR_PerCar %>%
  group_by(Year, Field_Plot, NAdd) %>%              # keeps these columns intact
  summarise(total_Carpercent = sum(`% Carbon`, na.rm = TRUE),  
            .groups = "drop")


# unique Row combos kept; Should be TRUE
nrow(CDR_PerCar_totals) == nrow(distinct(CDR_PerCar, Year, Field_Plot, NAdd))

# Confirm totals add up to original (ignoring NA); Should be TRUE
sum(CDR_PerCar_totals$total_Carpercent) == sum(CDR_PerCar$ `% Carbon`, na.rm = TRUE)

#Now Overwrite the original data to reflect the summed %Carbon
CDR_PerCar <- CDR_PerCar %>%
  group_by(Year, Field_Plot, NAdd) %>%              # keeps these columns intact
  summarise(total_Carpercent = sum(`% Carbon`, na.rm = TRUE),  
            .groups = "drop")

#Save [pre]processed csv
write.csv(CDR_PerCar,
          "CDR_PercentCarbon_1982-2011_processed.csv",
          row.names = FALSE)
