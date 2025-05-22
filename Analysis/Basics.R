library(dplyr)
library(readr)
# Just visualizing the data
df <- read.csv("Harmonizing/harmonized_Ian.csv")
file.name <- unique(df$source)
file_name <- file.name[2]
single.data <- filter(df,df$source==file_name)
treatment <- unique(single.data$Treatment)
Trmt.data <- filter(df,df$source==file_name
                      & df$Treatment==treatment[5])
Cntl.data <- filter(df,df$source==file_name
                    & df$Treatment==treatment[1])
res.data <- data.frame(Response = Trmt.data$Biomass/Cntl.data$Biomass,
                 Date = Trmt.data$Date)
# Trmt.data <- summarize_by_columns(Trmt.data, c("Date"))
# Cntl.data <- summarize_by_columns(Cntl.data, c("Date"))
# Cntl.data <- head(Cntl.data,-1)
#boxplot(Response ~ Trmt.data$Date)

## ggplot
ggplot(res.data,aes(Date, Response)) +
       stat_summary(geom = "line", fun.y = mean) +
       stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)
