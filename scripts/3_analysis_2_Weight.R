#Data analysis - weights
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-01
#Last updated: 2022-09-03


#load packages
library(tidyverse)

#read csv
Phase1_Data_Weight <- read_csv("data_analysis/Phase1_Data_Weight.csv")

################################################################################
# Average stress week by species and treatment
# Remember that watered trees don't have a stress week
################################################################################

Phase1_Data_Weight <- Phase1_Data_Weight %>%
  group_by(Heatwave_graph) %>%
  mutate(Stress_Week_Avg = mean(Stress_Week, na.rm = T))

################################################################################
# SD
################################################################################

Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD_Weight_Total = sd(Weight_Est, na.rm = T),
         SD_Weight_Water = sd(WaterWeight_Calc, na.rm = T))


################################################################################
# Samples sizes per week
################################################################################

#samples sizes per week
summary_1 <- Phase1_Data_Weight %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Weight = sum(!is.na(Weight_Est)))

Phase1_Data_Weight <- merge(Phase1_Data_Weight, summary_1, all.x = T)


################################################################################
# Stress Week to Dead Week
################################################################################

summary_2 <- Phase1_Data_Weight %>% 
  group_by(Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  filter(Dead == "dead") %>% 
  mutate(Dead_Week = min(Week)) %>% 
  summarize(Dead_Week = mean(Dead_Week),
            Stress_Week = mean(Stress_Week)) %>% 
  mutate(Stress_to_Dead_Weight = Dead_Week - Stress_Week) %>% 
  select("Stress_to_Dead_Weight")

#select("Species", "SpeciesID", "Treatment_temp", "Treatment_water", "Dead_Week")

Phase1_Data_Weight <- merge(Phase1_Data_Weight, summary_2, all.x = T)

#save csv
write.csv(Phase1_Data_Weight, "data_analysis/Phase1_Data_Weight.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Average
################################################################################

#filter NAs
Phase1_Data_Weight_Avg <- Phase1_Data_Weight %>% 
  filter(!is.na(Weight_Est))

#average data 
Phase1_Data_Weight_Avg <- Phase1_Data_Weight_Avg %>%
  #filter(SpeciesID != "PIFL16") %>%
  group_by(Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(SampleSize_Weekly_Weight = mean(SampleSize_Weekly_Weight),
            Dead_Count = sum(Dead_Count),
            Weight_g = round(mean(Weight_g, na.rm = T), digits = 0),
            Weight_Est = round(mean(Weight_Est, na.rm = T), digits = 0),
            WeightMin = round(mean(WeightMin, na.rm = T), digits = 0),
            WeightMax = round(mean(WeightMax, na.rm = T), digits = 0),
            WaterWeight_Base = round(mean(WaterWeight_Base, na.rm = T), digits = 0),
            WaterWeight_Calc = round(mean(WaterWeight_Calc, na.rm = T), digits = 0),
            PercentWater = round(mean(PercentWater, na.rm = T), digits = 0),
            SD_Weight_Total = mean(SD_Weight_Total, na.rm = T),
            SD_Weight_Water = mean(SD_Weight_Water, na.rm = T),
            Stress_Week_Avg_Weight = mean(Stress_Week_Avg_Weight, na.rm = T),
            Stress_to_Dead_Weight = mean(Stress_to_Dead_Weight, na.rm = T))

#save csv
write.csv(Phase1_Data_Weight_Avg, "data_analysis/Phase1_Data_Weight_Avg.csv", quote = FALSE, row.names = FALSE)

