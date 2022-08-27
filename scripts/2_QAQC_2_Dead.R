#QAQC - dead
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-07-13

#load tidyverse
library(tidyverse)

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
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)


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

