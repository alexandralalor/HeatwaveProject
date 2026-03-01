#Data analysis - porometer
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-18
#Last updated: 2026-03-01


#load packages
library(tidyverse)

#read csv
Data_Porometer <- read_csv("data/data_analysis/Data_Porometer.csv")

################################################################################
# Average stress week by species and treatment
# Remember that watered trees don't have a stress week
################################################################################

Data_Porometer_add <- Data_Porometer %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Phase, Heatwave_graph) %>% 
  mutate(Stress_Week_Avg_Porometer = round(mean(Stress_Week, na.rm = TRUE), digits = 1))

Data_Porometer <- merge(Data_Porometer, Data_Porometer_add, all = T)

################################################################################
# SD
################################################################################

Data_Porometer <- Data_Porometer %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water, Week) %>%
  mutate(SD_Porometer = sd(Porometer_Est, na.rm = T))


################################################################################
# Samples sizes per week
################################################################################

summary_1 <- Data_Porometer %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Porometer = sum(!is.na(Porometer_Est)))

Data_Porometer <- merge(Data_Porometer, summary_1, all.x = T)

################################################################################
# Stress Week to Dead Week
################################################################################

summary_2 <- Data_Porometer %>% 
  group_by(Phase, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  filter(Dead == "dead") %>% 
  mutate(Dead_Week = min(Week)) %>% 
  summarize(Dead_Week = mean(Dead_Week),
            Stress_Week = mean(Stress_Week)) %>% 
  mutate(Stress_to_Dead_Porometer = Dead_Week - Stress_Week) %>% 
  select("Stress_to_Dead_Porometer")
  
Data_Porometer <- merge(Data_Porometer, summary_2, all.x = T)

Data_Porometer <- Data_Porometer %>% 
  mutate(Stress_Week_Porometer = Stress_Week) %>% 
  select(-c("Stress_Week"))


#save as csv
write.csv(Data_Porometer, "data/data_analysis/Data_Porometer.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Average
################################################################################

#filter for NAs
Data_Porometer_Avg <- Data_Porometer %>% 
  filter(!is.na(Porometer_Est)) %>% 
  mutate(Stress_Week = ifelse(Treatment_water == "Watered", NA, Stress_Week_Porometer))

#Avg Porometer
Data_Porometer_Avg <- Data_Porometer_Avg %>%
  group_by(Phase, Week, Species, Treatment_temp, Treatment_water) %>% 
  summarize(SampleSize_Weekly_Porometer = mean(SampleSize_Weekly_Porometer),
            Dead_Count = sum(Dead_Count),
            Porometer = mean(Porometer, na.rm = T),
            Porometer_Est = mean(Porometer_Est, na.rm = T),
            #Temperature_C = mean(Temperature_C, na.rm = T),
            #LeafSensor_PercentRH = mean(LeafSensor_PercentRH, na.rm = T),
            #FilterSensor_PercentRH = mean(FilterSensor_PercentRH, na.rm = T),
            SD_Porometer = mean(SD_Porometer, na.rm = T),
            Stress_Week_Avg_Porometer = mean(Stress_Week_Avg_Porometer, na.rm = T),
            Stress_to_Dead_Porometer = mean(Stress_to_Dead_Porometer, na.rm = T))

#save as csv
write.csv(Data_Porometer_Avg, "data/data_analysis/Data_Porometer_Avg.csv", quote = FALSE, row.names = FALSE)

