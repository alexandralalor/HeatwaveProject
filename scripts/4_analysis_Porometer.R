#Data analysis - porometer
#find asymptote
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-03
#Last updated: 2022-09-03

#load tidyverse
library(tidyverse)

#check out data
glimpse(Phase1_Data_Porometer)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Porometer$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_Porometer$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_Porometer$Time), Time))))
Phase1_Data_Porometer$DateTime <- as.POSIXct(Phase1_Data_Porometer$DateTime, format = "%m/%d/%Y %H:%M")
Phase1_Data_Porometer$Date <- as.Date(Phase1_Data_Porometer$Date, format = "%m/%d/%Y")

Phase1_Data_Porometer$Phase <- as.factor(Phase1_Data_Porometer$Phase)
Phase1_Data_Porometer$Chamber <- as.factor(Phase1_Data_Porometer$Chamber)
Phase1_Data_Porometer$ScientificName <- as.factor(Phase1_Data_Porometer$ScientificName)
Phase1_Data_Porometer$CommonName <- as.factor(Phase1_Data_Porometer$CommonName)
Phase1_Data_Porometer$Species <- as.factor(Phase1_Data_Porometer$Species)
Phase1_Data_Porometer$Treatment_temp <- as.factor(Phase1_Data_Porometer$Treatment_temp)
Phase1_Data_Porometer$Treatment_water <- as.factor(Phase1_Data_Porometer$Treatment_water)
Phase1_Data_Porometer$PorometerSubset <- as.factor(Phase1_Data_Porometer$PorometerSubset)
Phase1_Data_Porometer$Dead <- as.factor(Phase1_Data_Porometer$Dead)
Phase1_Data_Porometer$Dead_Count <- as.factor(Phase1_Data_Porometer$Dead_Count)
Phase1_Data_Porometer$Heatwave_graph <- as.factor(Phase1_Data_Porometer$Heatwave_graph)
Phase1_Data_Porometer$Heatwave <- as.factor(Phase1_Data_Porometer$Heatwave)

################################################################################
#Find asymptote
################################################################################

#filter for porometer data
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Porometer), Treatment_water == "Drought")

#finding sd??
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  group_by(Heatwave_graph) %>%
  mutate(SD = sd(Porometer))


# Phase1_Data_Porometer_PIPO <- Phase1_Data_Porometer %>% 
#   filter(Species == "PIPO")
# 
# Phase1_Data_Porometer_filter <- Phase1_Data_Porometer_PIPO %>% 
#   filter(SpeciesID == "PIPO04")


smooth <- smooth.spline(x = Phase1_Data_Porometer$Week,
                        y = Phase1_Data_Porometer$Porometer)
plot(smooth)
lines(smooth, col = 2)
?plot
predict(smooth)
?predict








