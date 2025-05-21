# Just visualizing the data
df <- read.csv("Harmonizing/harmonized_Ian.csv")
file.name <- unique(df$source)
file_name <- file.name[7]
single.data <-filter(df,df$source==file_name)
treatment <- unique(single.data$Treatment)
Trmt.data <- filter(df,df$source==file_name
                      & df$Treatment==treatment[3])
Cntl.data <- filter(df,df$source==file_name
                    & df$Treatment==treatment[2])
Trmt.data <- summarize_by_columns(Trmt.data, c("Date"))
Cntl.data <- summarize_by_columns(Cntl.data, c("Date"))
# Cntl.data <- head(Cntl.data,-1)
Response <- Trmt.data$Biomass_mean - Cntl.data$Biomass_mean
plot(Response ~ Trmt.data$Date)
