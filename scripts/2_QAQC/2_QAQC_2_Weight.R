#QAQC - weights
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2026-03-01

#load tidyverse
library(tidyverse)

#read CSVs
Data <- read_csv("data/data_QAQC/Data.csv")

#check structure, ensure consistent formats
glimpse(Data)

#convert variables
Data$Phase <- as.factor(Data$Phase)
Data$Chamber <- as.factor(Data$Chamber)
Data$ScientificName <- as.factor(Data$ScientificName)
Data$CommonName <- as.factor(Data$CommonName)
Data$Species <- as.factor(Data$Species)
Data$Treatment_temp <- as.factor(Data$Treatment_temp)
Data$Treatment_water <- as.factor(Data$Treatment_water)
Data$PorometerSubset <- as.factor(Data$PorometerSubset)
Data$Dead <- as.factor(Data$Dead)
Data$Heatwave_graph <- as.factor(Data$Heatwave_graph)
Data$Heatwave <- as.factor(Data$Heatwave)


################################################################################
#Check weights
################################################################################

#First, look for outliers in weight data
#For droughted plants, make sure that weights are always decreasing

#filter for weight data
Data_Weight <- Data %>% 
  select(c("Phase","Species","SpeciesID","Week","Treatment_temp","Treatment_water","Weight_g")) %>% 
  filter(Treatment_water == "Drought", !is.na(Weight_g)) %>% 
  arrange(Phase, SpeciesID, Week)

#Check that each drought plant is decreasing in weight
#add column for weight differences
Data_Weight <- Data_Weight %>% 
  mutate(Difference = Weight_g - lag(Weight_g))

#isolate incorrect values
#Seems like 2grams is a reasonable error, so look for differences above this
Data_Weight_test <- Data_Weight %>% 
  filter(Difference > 2, Week != 1)

#5 errors which are slightly high, but not outrageous. Keep in data 
#2 major errors (100+ grams) replaced with NA

#make changes to Data in data_QAQC folder
################################################################################

#Next, add info to weight data
#WeightMin, WeightMax, WaterWeight_Base, WaterWeight_Calc, PercentWater, Weight_Est

#Calculate WeightMin and WeightMax for each individual plant
Data_Weight_add_1 <- Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Phase,SpeciesID) %>% 
  mutate(WeightMin = min(Weight_g, na.rm = TRUE),
         WeightMax = max(Weight_g, na.rm = TRUE)) %>% 
  select(Phase, Species, SpeciesID, Week, WeightMin, WeightMax)

#Add data to Data_Weight
Data_Weight <- merge(Data, Data_Weight_add_1, by = c("Phase","Species","SpeciesID", "Week"), all = TRUE)

#Calculate Weight_Est
#Weight_Est = estimated weight of plants which have died and dropped out of study
#assuming WeightMin is completely dry, and as the lowest weight
Data_Weight_add_2 <- Data_Weight %>% 
  filter(!grepl(".5", Data_Weight$Week, fixed = TRUE)) %>% 
  filter(Treatment_water == "Drought", is.na(PercentBrown)) %>% 
  mutate(Weight_Est = ifelse(Dead == "dead", WeightMin, Weight_g)) %>% 
  select(c("Phase","Species","SpeciesID","Week","Weight_Est"))

#Add data to Data_Weight
Data_Weight <- merge(Data_Weight, Data_Weight_add_2, by = c("Phase","Species","SpeciesID", "Week"), all = TRUE)

#Combine all weight data to include Weight_Est
Data_Weight <- Data_Weight %>% 
  mutate(Weight_Est = ifelse(is.na(Weight_Est), Data_Weight$Weight_g, Data_Weight$Weight_Est))

# #Now convert to NA during missing christmas readings
# Data_Weight_testing_1 <- Data_Weight
# Data_Weight_testing_1$Weight_Est1 <- ifelse(Data_Weight_testing_1$Species == "PIFL" & Data_Weight_testing_1$Week == 16, NA, Data_Weight_testing_1$Weight_Est)
# Data_Weight <- Data_Weight_testing_1
# Data_Weight_testing_2 <- Data_Weight
# Data_Weight_testing_2$Weight_Est2 <- ifelse(Data_Weight_testing_2$Species == "PSME" & Data_Weight_testing_2$Week == 16, NA, Data_Weight_testing_2$Weight_Est1)
# Data_Weight <- Data_Weight_testing_2
# Data_Weight_testing_3 <- Data_Weight
# Data_Weight_testing_3$Weight_Est <- ifelse(Data_Weight_testing_3$Species == "PIED" & Data_Weight_testing_3$Week == 18, NA, Data_Weight_testing_3$Weight_Est2)
# Data_Weight <- Data_Weight_testing_3

# #last, remove extra columns
# Data_Weight <- Data_Weight %>% 
#   select(-c("Weight_Est1","Weight_Est2"))

#Calculate WaterWeight_Base, Waterweight_Calc, and PercentWater for each plant
#WaterWeight_Base = how many grams of water did the plant start out with?
#WaterWeight_Calc = how many grams of water does a plant have weekly?
#PercentWater = what percent water does a plant have weekly?
# -- all assuming WeightMin is completely dry and WeightMax is at field capacity
Data_Weight <- Data_Weight %>% 
  mutate(WaterWeight_Base = WeightMax - WeightMin,
         WaterWeight_Calc = Weight_Est - WeightMin) %>% 
  mutate(PercentWater = 100*(WaterWeight_Calc/WaterWeight_Base))

Data_Weight <- Data_Weight[ ,c(1,5,6,7,2,3,8,9,10,11,12,13,14,4,15,16,17,18,19,20,21,22,23,24,25,26,27)]


#clean up data, remove half weeks
#filter for weight data
Data_Weight <- Data_Weight %>% 
  filter(!grepl(".5", Data_Weight$Week, fixed = TRUE))


#save as csv
write.csv(Data_Weight, "data/data_QAQC/Data_Weight.csv", quote = FALSE, row.names = FALSE)
