install.packages("librarian")
librarian::shelf(ltertools, lterdatasampler, psych, supportR, tidyverse, usethis, here)

# Define URL as an object
dt_url <- "https://raw.githubusercontent.com/lter/ssecr-c-cycling/refs/heads/main/data/Konza%20Prairie%20Biomass.csv"

# Read it into R
Konza <- read_csv(file = dt_url)

Konza_clean <- Konza %>%
  rename(Year = RECYEAR, Month = RECMONTH) %>%
  mutate(Month = month.name[Month])
