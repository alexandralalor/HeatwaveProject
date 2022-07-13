#QAQC - percent brown, weights, and dead
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-07-08

#load tidyverse
library(tidyverse)
library(data.table) #for duplist and uniqlist

#read CSVs
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

#check structure, ensure consistent formats
glimpse(Phase1_Data)

#convert variables
Phase1_Data$Phase <- as.factor(Phase1_Data$Phase)
Phase1_Data$Chamber <- as.factor(Phase1_Data$Chamber)
Phase1_Data$ScientificName <- as.factor(Phase1_Data$ScientificName)
Phase1_Data$CommonName <- as.factor(Phase1_Data$CommonName)
Phase1_Data$Species <- as.factor(Phase1_Data$Species)
Phase1_Data$Treatment_temp <- as.factor(Phase1_Data$Treatment_temp)
Phase1_Data$Treatment_water <- as.factor(Phase1_Data$Treatment_water)
Phase1_Data$PorometerSubset <- as.factor(Phase1_Data$PorometerSubset)
Phase1_Data$Whorls <- as.factor(Phase1_Data$Whorls)
Phase1_Data$PercentBrown <- as.factor(Phase1_Data$PercentBrown)
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)


################################################################################
#Check PercentBrown - 10, 25, 50, 75, 90
################################################################################

#check for values outside of set range
unique(Phase1_Data$PercentBrown)

#isolate incorrect values
Phase1_Data_PercentBrown <- Phase1_Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","PercentBrown")) %>% 
  filter(!is.na(PercentBrown)) %>% 
  arrange(SpeciesID, Week)

Phase1_Data_PercentBrown_test <- Phase1_Data_PercentBrown %>%
  filter(PercentBrown != 10, 
         PercentBrown != 25, 
         PercentBrown != 50,
         PercentBrown != 75,
         PercentBrown != 90)

#make changes to Phase1_Data in data_QAQC folder
################################################################################

################################################################################
#Check weights
################################################################################

#First, look for outliers in weight data
#For droughted plants, make sure that weights are always decreasing

#filter for weight data
Phase1_Data_Weight <- Phase1_Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Weight_g")) %>% 
  filter(Treatment_water == "Drought", !is.na(Weight_g)) %>% 
  arrange(SpeciesID, Week)

#Check that each drought plant is decreasing in weight
#add column for weight differences
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  mutate(Difference = Weight_g - lag(Weight_g))

#isolate incorrect values
#Seems like 2grams is a reasonable error, so look for differences above this
Phase1_Data_Weight_test <- Phase1_Data_Weight %>% 
  filter(Difference > 2, Week != 1)

#5 errors which are slightly high, but not outrageous. Keep in data 
#2 major errors (100+ grams) replaced with NA

#make changes to Phase1_Data in data_QAQC folder
################################################################################

#Next, add info to weight data
#WeightMin, WeightMax, WaterWeight_Base, WaterWeight_Calc, PercentWater, Weight_Est

#Calculate WeightMin and WeightMax for each individual plant
Phase1_Data_Weight_add_1 <- Phase1_Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(SpeciesID) %>% 
  mutate(WeightMin = min(Weight_g, na.rm = TRUE),
         WeightMax = max(Weight_g, na.rm = TRUE)) %>% 
  select(Species, SpeciesID, Week, WeightMin, WeightMax)

#Add data to Phase1_Data_Weight
Phase1_Data_Weight <- merge(Phase1_Data, Phase1_Data_Weight_add_1, by = c("Species","SpeciesID", "Week"), all = TRUE)

#Calculate Weight_Est
#Weight_Est = estimated weight of plants which have died and dropped out of study
#assuming WeightMin is completely dry, and as the lowest weight
Phase1_Data_Weight_add_2 <- Phase1_Data_Weight %>% 
  filter(!grepl(".5", Phase1_Data_Weight$Week, fixed = TRUE)) %>% 
  filter(Treatment_water == "Drought", is.na(PercentBrown)) %>% 
  mutate(Weight_Est = ifelse(Dead == "dead", WeightMin, Weight_g)) %>% 
  select(c("Species","SpeciesID","Week","Weight_Est"))

#Add data to Phase1_Data_Weight
Phase1_Data_Weight <- merge(Phase1_Data_Weight, Phase1_Data_Weight_add_2, by = c("Species","SpeciesID", "Week"), all = TRUE)

#Combine all weight data to include Weight_Est
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  mutate(Weight_Est = ifelse(is.na(Weight_Est), Phase1_Data_Weight$Weight_g, Phase1_Data_Weight$Weight_Est))

#Now convert to NA during missing christmas readings
Phase1_Data_Weight_testing_1 <- Phase1_Data_Weight
Phase1_Data_Weight_testing_1$Weight_Est1 <- ifelse(Phase1_Data_Weight_testing_1$Species == "PIFL" & Phase1_Data_Weight_testing_1$Week == 16, NA, Phase1_Data_Weight_testing_1$Weight_Est)
Phase1_Data_Weight <- Phase1_Data_Weight_testing_1
Phase1_Data_Weight_testing_2 <- Phase1_Data_Weight
Phase1_Data_Weight_testing_2$Weight_Est2 <- ifelse(Phase1_Data_Weight_testing_2$Species == "PSME" & Phase1_Data_Weight_testing_2$Week == 16, NA, Phase1_Data_Weight_testing_2$Weight_Est1)
Phase1_Data_Weight <- Phase1_Data_Weight_testing_2
Phase1_Data_Weight_testing_3 <- Phase1_Data_Weight
Phase1_Data_Weight_testing_3$Weight_Est <- ifelse(Phase1_Data_Weight_testing_3$Species == "PIED" & Phase1_Data_Weight_testing_3$Week == 18, NA, Phase1_Data_Weight_testing_3$Weight_Est2)
Phase1_Data_Weight <- Phase1_Data_Weight_testing_3

#last, remove extra columns
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  select(-c("Weight_Est1","Weight_Est2"))

#Calculate WaterWeight_Base, Waterweight_Calc, and PercentWater for each plant
#WaterWeight_Base = how many grams of water did the plant start out with?
#WaterWeight_Calc = how many grams of water does a plant have weekly?
#PercentWater = what percent water does a plant have weekly?
# -- all assuming WeightMin is completely dry and WeightMax is at field capacity
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  mutate(WaterWeight_Base = WeightMax - WeightMin,
         WaterWeight_Calc = Weight_Est - WeightMin) %>% 
  mutate(PercentWater = 100*(WaterWeight_Calc/WaterWeight_Base))

Phase1_Data_Weight <- Phase1_Data_Weight[ ,c(4,5,6,7,1,2,8,9,10,11,12,13,14,3,15,16,17,18,19,20,21,22,23,24,25)]

#save as csv
write.csv(Phase1_Data_Weight, "data_QAQC/Phase1_Data_Weight.csv", quote = FALSE, row.names = FALSE)


################################################################################
#Check dead
################################################################################
#look for NA dead
test <- Phase1_Data %>% 
  filter(!grepl(".5", Phase1_Data_Weight$Week)) %>% 
  filter(is.na(Dead)) %>% 
  select(c("SpeciesID","Week","Dead","Dead_Count"))

#make sure Dead_Count does not equal 0
#make changes to Phase1_Data in data_QAQC folder
################################################################################
#look for plants which come back alive

#filter for weight data
Phase1_Data_Dead_1 <- Phase1_Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Dead")) %>% 
  filter(!is.na(Dead), Dead == "dead") %>% 
  arrange(SpeciesID, Week)
Phase1_Data_Dead_2 <- Phase1_Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Dead")) %>% 
  filter(!is.na(Dead)) %>% 
  arrange(SpeciesID, Week)

#make sure negative weeks are attributed to SpeciesID change
Phase1_Data_Dead_test_1 <- Phase1_Data_Dead_1 %>% 
  filter(Species == "PIPO") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Phase1_Data_Dead_test_1$SpeciesID)

Phase1_Data_Dead_test_2 <- Phase1_Data_Dead_1 %>% 
  filter(Species == "PIED") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Phase1_Data_Dead_test_2$SpeciesID)

Phase1_Data_Dead_test_3 <- Phase1_Data_Dead_1 %>% 
  filter(Species == "PIFL") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Phase1_Data_Dead_test_3$SpeciesID)

Phase1_Data_Dead_test_4 <- Phase1_Data_Dead_1 %>% 
  filter(Species == "PSME") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Phase1_Data_Dead_test_4$SpeciesID)

Phase1_Data_Dead_test_5 <- Phase1_Data_Dead_1 %>% 
  filter(Species == "PIEN") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Phase1_Data_Dead_test_5$SpeciesID)


#double check positive values
Phase1_Data_Dead_test_1 <- Phase1_Data_Dead_2 %>% 
  filter(Species == "PIPO") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Phase1_Data_Dead_test_2 <- Phase1_Data_Dead_2 %>% 
  filter(Species == "PIED") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Phase1_Data_Dead_test_3 <- Phase1_Data_Dead_2 %>% 
  filter(Species == "PIFL") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Phase1_Data_Dead_test_4 <- Phase1_Data_Dead_2 %>% 
  filter(Species == "PSME") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Phase1_Data_Dead_test_5 <- Phase1_Data_Dead_2 %>% 
  filter(Species == "PIEN") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

#make changes to Phase1_Data in data_QAQC folder
################################################################################

