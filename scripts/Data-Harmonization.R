install.packages("librarian")
librarian::shelf(ltertools, lterdatasampler, psych, supportR, tidyverse)

# Example code from Nick for how to extract information from file names directly for data harmonization
# Break useful information out of file name ("source" column)
combo_v7 <- combo_v6 %>% 
  # Separate by underscore
  tidyr::separate_wider_delim(cols = source, delim = "_",
                              names = c("organization", "site", 
                                        "experiment.name", "sampling.years", 
                                        "excluded.group", "measured.group"),
                              cols_remove = F) %>%
  # Remove file name from final bit of file
  dplyr::mutate(measured.group = gsub(pattern = "\\.csv", replacement = "",
                                      x = measured.group)) %>% 
  # Relocate source back to first position
  dplyr::relocate(source, .before = dplyr::everything())

# Check structure
dplyr::glimpse(combo_v7)


