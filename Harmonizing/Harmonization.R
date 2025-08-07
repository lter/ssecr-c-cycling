# Use the command lines below to install or update "ltertools"
# install.packages("devtools")
# devtools::install_github("lter/ltertools")
# more harmonization tips at: https://lter.github.io/ltertools/articles/ltertools.html

#load library ltertools
library(ltertools)

# Generate a column key with "guesses" at tidy column names
#test_key <- ltertools::begin_key(raw_folder = "Ready_data", data_format = "csv", 
#                                 guess_tidy = TRUE)

# Examine what that generated
#test_key

# Write the newly generated test key into a csv file
#write.csv(test_key, "Harmonizing/test_key.csv", row.names = FALSE)

# read in the column key generated from google sheet
Column_key <- read.csv("Harmonizing/Column_key.csv")

                
# Use the key to harmonize our example data
harmony <- ltertools::harmonize(key = Column_key, raw_folder = "Ready_data", 
                                data_format = "csv", quiet = TRUE)

# Check the structure of that
utils::str(harmony)

# Write the newly generated harmony data into a csv file
write.csv(harmony, "Harmonizing/harmonized_aug7.csv", row.names = FALSE)
