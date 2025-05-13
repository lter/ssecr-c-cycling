# Use the command lines below to install or update "ltertools"
# install.packages("devtools")
# devtools::install_github("lter/ltertools")
# more harmonization tips at: https://lter.github.io/ltertools/articles/ltertools.html



# Generate a column key with "guesses" at tidy column names
test_key <- ltertools::begin_key(raw_folder = "Ready_data", data_format = "csv", 
                                 guess_tidy = TRUE)

# Examine what that generated
test_key

# Write the newly generated test key into a csv file
write.csv(test_key, "Harmonizing/test_key.csv", row.names = FALSE)

# Use the key to harmonize our example data
harmony <- ltertools::harmonize(key = test_key, raw_folder = "Ready_data", 
                                data_format = "csv", quiet = TRUE)

# Check the structure of that
utils::str(harmony)
