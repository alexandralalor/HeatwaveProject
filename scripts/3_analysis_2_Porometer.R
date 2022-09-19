#Data analysis - porometer
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-18
#Last updated: 2022-09-18


#load packages
library(tidyverse)

#read csv
Phase1_Data_Porometer <- read_csv("data_analysis/Phase1_Data_Porometer.csv")

################################################################################
# Average stress week by species and treatment
# Remember that watered trees don't have a stress week
################################################################################

Phase1_Data_Porometer_add <- Phase1_Data_Porometer %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Heatwave_graph) %>% 
  mutate(Stress_Week_Avg_Porometer = round(mean(Stress_Week, na.rm = TRUE), digits = 1))

Phase1_Data_Porometer <- merge(Phase1_Data_Porometer, Phase1_Data_Porometer_add, all = T)

################################################################################
# SD
################################################################################

Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD_Porometer = sd(Porometer_Est, na.rm = T))

#save as csv
write.csv(Phase1_Data_Porometer, "data_analysis/Phase1_Data_Porometer.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Average
################################################################################

#filter for NAs
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  filter(!is.na(Porometer_Est)) %>% 
  mutate(Stress_Week = ifelse(Treatment_water == "Watered", NA, Stress_Week))

#Avg Porometer
Phase1_Data_Porometer_Avg <- Phase1_Data_Porometer %>%
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  summarize(Dead_Count = sum(Dead_Count),
            Porometer = mean(Porometer, na.rm = T),
            Porometer_Est = mean(Porometer_Est, na.rm = T),
            Temperature_C = mean(Temperature_C, na.rm = T),
            LeafSensor_PercentRH = mean(LeafSensor_PercentRH, na.rm = T),
            FilterSensor_PercentRH = mean(FilterSensor_PercentRH, na.rm = T),
            SD_Porometer = mean(SD_Porometer, na.rm = T),
            Stress_Week_Avg_Porometer = mean(Stress_Week_Avg_Porometer, na.rm = TRUE))

#save as csv
write.csv(Phase1_Data_Porometer_Avg, "data_analysis/Phase1_Data_Porometer_Avg.csv", quote = FALSE, row.names = FALSE)

