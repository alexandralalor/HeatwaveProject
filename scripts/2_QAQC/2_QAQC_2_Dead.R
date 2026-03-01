#QAQC - dead
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
#Check dead
################################################################################
#look for NA dead
test <- Data %>% 
  filter(!grepl(".5", Data$Week)) %>% 
  filter(is.na(Dead)) %>% 
  select(c("SpeciesID","Week","Dead","Dead_Count"))

#make sure Dead_Count does not equal 0
#make changes to Data in data_QAQC folder
################################################################################

#look for plants which come back alive

#filter for weight data
Data_Dead_1 <- Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Dead")) %>% 
  filter(!is.na(Dead), Dead == "dead") %>% 
  arrange(SpeciesID, Week)
Data_Dead_2 <- Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Dead")) %>% 
  filter(!is.na(Dead)) %>% 
  arrange(SpeciesID, Week)

#make sure negative weeks are attributed to SpeciesID change
Data_Dead_test_1 <- Data_Dead_1 %>% 
  filter(Species == "PIPO") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Data_Dead_test_1$SpeciesID)

Data_Dead_test_2 <- Data_Dead_1 %>% 
  filter(Species == "PIED") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Data_Dead_test_2$SpeciesID)

Data_Dead_test_3 <- Data_Dead_1 %>% 
  filter(Species == "PIFL") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Data_Dead_test_3$SpeciesID)

Data_Dead_test_4 <- Data_Dead_1 %>% 
  filter(Species == "PSME") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Data_Dead_test_4$SpeciesID)

Data_Dead_test_5 <- Data_Dead_1 %>% 
  filter(Species == "PIEN") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference < 0)
unique(Data_Dead_test_5$SpeciesID)


#double check positive values
Data_Dead_test_1 <- Data_Dead_2 %>% 
  filter(Species == "PIPO") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Data_Dead_test_2 <- Data_Dead_2 %>% 
  filter(Species == "PIED") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Data_Dead_test_3 <- Data_Dead_2 %>% 
  filter(Species == "PIFL") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Data_Dead_test_4 <- Data_Dead_2 %>% 
  filter(Species == "PSME") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

Data_Dead_test_5 <- Data_Dead_2 %>% 
  filter(Species == "PIEN") %>% 
  mutate(Difference = Week - lag(Week)) %>% 
  filter(Difference > 0.5)

#make changes to Data in data_QAQC folder
################################################################################

