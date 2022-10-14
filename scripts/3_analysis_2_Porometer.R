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
  group_by(Species, Treatment_temp, Treatment_water, Week) %>%
  mutate(SD_Porometer = sd(Porometer_Est, na.rm = T))


################################################################################
# Samples sizes per week
################################################################################

summary_1 <- Phase1_Data_Porometer %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Porometer = sum(!is.na(Porometer_Est)))

Phase1_Data_Porometer <- merge(Phase1_Data_Porometer, summary_1, all.x = T)

################################################################################
# Stress Week to Dead Week
################################################################################

summary_2 <- Phase1_Data_Porometer %>% 
  group_by(Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  filter(Dead == "dead") %>% 
  mutate(Dead_Week = min(Week)) %>% 
  summarize(Dead_Week = mean(Dead_Week),
            Stress_Week = mean(Stress_Week)) %>% 
  mutate(Stress_to_Dead_Porometer = Dead_Week - Stress_Week) %>% 
  select("Stress_to_Dead_Porometer")
  
Phase1_Data_Porometer <- merge(Phase1_Data_Porometer, summary_2, all.x = T)

Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  mutate(Stress_Week_Porometer = Stress_Week) %>% 
  select(-c("Stress_Week"))


#save as csv
write.csv(Phase1_Data_Porometer, "data_analysis/Phase1_Data_Porometer.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Average
################################################################################

#filter for NAs
Phase1_Data_Porometer_Avg <- Phase1_Data_Porometer %>% 
  filter(!is.na(Porometer_Est)) %>% 
  mutate(Stress_Week = ifelse(Treatment_water == "Watered", NA, Stress_Week))

#Avg Porometer
Phase1_Data_Porometer_Avg <- Phase1_Data_Porometer_Avg %>%
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  summarize(SampleSize_Weekly_Porometer = mean(SampleSize_Weekly_Porometer),
            Dead_Count = sum(Dead_Count),
            Porometer = mean(Porometer, na.rm = T),
            Porometer_Est = mean(Porometer_Est, na.rm = T),
            Temperature_C = mean(Temperature_C, na.rm = T),
            LeafSensor_PercentRH = mean(LeafSensor_PercentRH, na.rm = T),
            FilterSensor_PercentRH = mean(FilterSensor_PercentRH, na.rm = T),
            SD_Porometer = mean(SD_Porometer, na.rm = T),
            Stress_Week_Avg_Porometer = mean(Stress_Week_Avg_Porometer, na.rm = T),
            Stress_to_Dead_Porometer = mean(Stress_to_Dead_Porometer, na.rm = T))

#save as csv
write.csv(Phase1_Data_Porometer_Avg, "data_analysis/Phase1_Data_Porometer_Avg.csv", quote = FALSE, row.names = FALSE)

