#QAQC - check porometer data against plant data
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-06
#Last updated: 2022-07-06

#load tidyverse
library(tidyverse)

#read in clean csvs
Phase1_Porometer <- read_csv("data_clean/Phase1_Porometer.csv")
Phase1_Plants <- read_csv("data_clean/Phase1_Plants.csv")
Phase1_InitialData <- read_csv("data_clean/Phase1_InitialData.csv")

#check structure, ensure consistent formats
#DateTime as <dttm>
#Date as <date>
#Time as <chr>
#Species as <fct>
#Dead as <fct>
glimpse(Phase1_Porometer)
glimpse(Phase1_Plants)
glimpse(Phase1_InitialData)

#Convert variables
Phase1_Porometer$Species <- as.factor(Phase1_Porometer$Species)
Phase1_Plants$Species <- as.factor(Phase1_Plants$Species)
Phase1_Plants$Dead <- as.factor(Phase1_Plants$Dead)


#select initial variables
Phase1_InitialData_Add <- Phase1_InitialData %>% 
  select(c("Species","SpeciesID","PorometerSubset"))
#select porometer data
Phase1_Porometer_Add <- Phase1_Porometer %>% 
  select(c("Date","Week","Species","SpeciesID","Conductance"))


#merge Plant data
Phase1_Plants <- merge(Phase1_Plants, Phase1_InitialData_Add, all.x = TRUE)

#filter Plant data
Phase1_Plants_filter <- Phase1_Plants %>% 
  filter(PorometerSubset == "yes")

#merge Plant data again
Phase1_Plants_test <- merge(Phase1_Plants_filter, Phase1_Porometer_Add)
Phase1_Plants_test_filter <- Phase1_Plants_test %>% 
  select(c("Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  arrange(SpeciesID)

#compare values
Phase1_Plants_test_filter <- Phase1_Plants_test_filter %>% 
  mutate(Difference = ifelse(Phase1_Plants_test_filter$Porometer == Phase1_Plants_test_filter$Conductance, "no","yes"))


#QAQC check, should be no "yes"
unique(Phase1_Plants_test_filter$Difference)

Phase1_Plants_test_filter %>% 
  filter(Difference == "yes")

