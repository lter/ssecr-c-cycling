library(dplyr)
library(readr)
library(ggplot2)
# 
df <- read.csv("Harmonizing/harmonized_Ian.csv")
file.name <- unique(df$source)
file_name <- file.name[6]
single.data <- filter(df,df$source==file_name)
## make ways to separate control and treatment
treatment <- unique(single.data$Treatment)
Trmt.data <- filter(single.data,
                        grepl("F",single.data$Treatment))
Cntl.data <- filter(single.data,
                        grepl("C",single.data$Treatment))
## pair the treatment and control together
#Trmt.data$Treatment <- substr(Trmt.data$Treatment,1,3)
#Cntl.data$Treatment <- substr(Cntl.data$Treatment,1,3)
#Cntl.data$Treatment <- sub("Namb","N",Cntl.data$Treatment)
#Trmt.data$Treatment <- sub("Nenrich","N",Trmt.data$Treatment)
pair <- merge(Trmt.data,Cntl.data,by = c("Date","source"),all=FALSE)
## calculate response ratio
res.data.6 <- data.frame(Response = log(pair$Biomass.x/pair$Biomass.y),
                 Date = pair$Date)
# Trmt.data <- summarize_by_columns(Trmt.data, c("Date"))
# Cntl.data <- summarize_by_columns(Cntl.data, c("Date"))
# Cntl.data <- head(Cntl.data,-1)
#boxplot(Response ~ Trmt.data$Date)

## ggplot
ggplot(res.data.6,aes(Date, Response)) +
       stat_summary(geom = "line", fun.y = mean) +
       stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.1)
## ggplot all together
ggplot() +
stat_summary(data = res.data.1, aes(x = Date, y = CDR, color = "CDR"), geom = "line", fun.y = mean) +
  stat_summary(data = res.data.1, aes(x = Date, y = CDR), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.1)+
stat_summary(data = res.data.3, aes(x = Date, y = KNZ, color = "KNZ"), geom = "line", fun.y = mean) +
  stat_summary(data = res.data.3, aes(x = Date, y = KNZ), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.1)+
stat_summary(data = res.data.4, aes(x = Date, y = PIE, color = "PIE"), geom = "line", fun.y = mean) +
  stat_summary(data = res.data.4, aes(x = Date, y = PIE), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.1)+
stat_summary(data = res.data.6, aes(x = Date, y = SEV, color = "SEV"), geom = "line", fun.y = mean) +
  stat_summary(data = res.data.6, aes(x = Date, y = SEV), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.1)+
  scale_color_manual(name = "Key:-", 
                     values = c("CDR" = "blue", "KNZ" = "red", "PIE" = "darkorchid",
                                "SEV" = "dodgerblue"))+
  ggtitle("4 Nutrient Addition Experiment Across LTER Sites")
