#QAQC - percent brown
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
Phase1_Data$Dead_Count <- as.factor(Phase1_Data$Dead_Count)
Phase1_Data$Heatwave_graph <- as.factor(Phase1_Data$Heatwave_graph)
Phase1_Data$Heatwave <- as.factor(Phase1_Data$Heatwave)



################################################################################
#Check PercentBrown - 10, 25, 50, 75, 90
################################################################################

#first, check for values outside of set range
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

#Next, add info to percent brown data
#PercentBrownMin, PercentBrownMax, PercentBrown_Est

Phase1_Data_PercentBrown_add_1 <- Phase1_Data %>% 
  group_by(SpeciesID) %>% 
  mutate(PercentBrownMin = min(PercentBrown, na.rm = TRUE),
         PercentBrownMax = max(PercentBrown, na.rm = TRUE)) %>% 
  select(Species, SpeciesID, Week, PercentBrownMin, PercentBrownMax)

#Add data to Phase1_Data_PercentBrown
Phase1_Data_PercentBrown <- merge(Phase1_Data, Phase1_Data_PercentBrown_add_1, by = c("Species","SpeciesID", "Week"), all = TRUE)

#Calculate PercentBrown_Est
#PercentBrown_Est = estimated percent brown of plants which have died and dropped out of study
Phase1_Data_PercentBrown_add_2 <- Phase1_Data_PercentBrown %>% 
  filter(is.na(PercentBrown)) %>% 
  mutate(PercentBrown_Est = ifelse(Dead == "dead", PercentBrownMax, PercentBrown)) %>% 
  select(c("Species","SpeciesID","Week","PercentBrown_Est"))

#Add data to Phase1_Data_PercentBrown
Phase1_Data_PercentBrown <- merge(Phase1_Data_PercentBrown, Phase1_Data_PercentBrown_add_2, by = c("Species","SpeciesID", "Week"), all = TRUE)

# #TEST - Combine all percent brown data to include PercentBrown_Est
# Phase1_Data_PercentBrown_test <- Phase1_Data_PercentBrown %>% 
#   mutate(PercentBrown_Est = ifelse(is.na(PercentBrown_Est), Phase1_Data_PercentBrown$PercentBrown, Phase1_Data_PercentBrown$PercentBrown_Est)) %>% 
#   select(c("Species","SpeciesID","Week","Dead_Count","PercentBrown","PercentBrownMax","PercentBrown_Est"))

#Combine all percent brown data to include PercentBrown_Est
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown %>% 
  mutate(PercentBrown_Est = ifelse(is.na(PercentBrown_Est), Phase1_Data_PercentBrown$PercentBrown, Phase1_Data_PercentBrown$PercentBrown_Est))


#Now convert to NA during missing christmas readings, and missing data week 9.5
Phase1_Data_PercentBrown_testing_1 <- Phase1_Data_PercentBrown
Phase1_Data_PercentBrown_testing_1$PercentBrown_Est1 <- ifelse(Phase1_Data_PercentBrown_testing_1$Species == "PIFL" & Phase1_Data_PercentBrown_testing_1$Week == 16, NA, Phase1_Data_PercentBrown_testing_1$PercentBrown_Est)
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown_testing_1
Phase1_Data_PercentBrown_testing_2 <- Phase1_Data_PercentBrown
Phase1_Data_PercentBrown_testing_2$PercentBrown_Est2 <- ifelse(Phase1_Data_PercentBrown_testing_2$Species == "PSME" & Phase1_Data_PercentBrown_testing_2$Week == 16, NA, Phase1_Data_PercentBrown_testing_2$PercentBrown_Est1)
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown_testing_2
Phase1_Data_PercentBrown_testing_3 <- Phase1_Data_PercentBrown
Phase1_Data_PercentBrown_testing_3$PercentBrown_Est3 <- ifelse(Phase1_Data_PercentBrown_testing_3$Species == "PIED" & Phase1_Data_PercentBrown_testing_3$Week == 18, NA, Phase1_Data_PercentBrown_testing_3$PercentBrown_Est2)
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown_testing_3
Phase1_Data_PercentBrown_testing_4 <- Phase1_Data_PercentBrown
Phase1_Data_PercentBrown_testing_4$PercentBrown_Est <- ifelse(Phase1_Data_PercentBrown_testing_4$Species == "PIPO" & Phase1_Data_PercentBrown_testing_4$Week == 9.5, NA, Phase1_Data_PercentBrown_testing_4$PercentBrown_Est3)
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown_testing_4


test <- Phase1_Data_PercentBrown %>% 
  select(c("Species","SpeciesID","Week","Dead_Count","PercentBrown","PercentBrownMax","PercentBrown_Est1","PercentBrown_Est2","PercentBrown_Est")) %>% 
  arrange(Species, Week, SpeciesID)

#last, remove extra columns and reorder columns
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown %>% 
  select(-c("PercentBrown_Est1","PercentBrown_Est2","PercentBrown_Est3"))

Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown[ ,c(4,5,6,7,1,2,8,9,10,11,12,13,14,3,15,16,17,18,19,20,21,22,23,24)]

#save as csv
write.csv(Phase1_Data_PercentBrown, "data_QAQC/Phase1_Data_PercentBrown.csv", quote = FALSE, row.names = FALSE)
