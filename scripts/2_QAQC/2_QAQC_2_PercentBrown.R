#QAQC - percent brown
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-07-13

#load tidyverse
library(tidyverse)

#read CSVs
Data <- read_csv("data_QAQC/Data.csv")

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
#Check PercentBrown - 10, 25, 50, 75, 90
################################################################################

#first, check for values outside of set range
unique(Data$PercentBrown)

#isolate incorrect values
Data_PercentBrown <- Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","PercentBrown")) %>% 
  filter(!is.na(PercentBrown)) %>% 
  arrange(SpeciesID, Week)

Data_PercentBrown_test <- Data_PercentBrown %>%
  filter(PercentBrown != 10, 
         PercentBrown != 25, 
         PercentBrown != 50,
         PercentBrown != 75,
         PercentBrown != 90)

#make changes to Data in data_QAQC folder
################################################################################

#Next, add info to percent brown data
#PercentBrownMin, PercentBrownMax, PercentBrown_Est

Data_PercentBrown_add_1 <- Data %>% 
  group_by(SpeciesID) %>% 
  mutate(PercentBrownMin = min(PercentBrown, na.rm = TRUE),
         PercentBrownMax = max(PercentBrown, na.rm = TRUE)) %>% 
  select(Species, SpeciesID, Week, PercentBrownMin, PercentBrownMax)

#Add data to Data_PercentBrown
Data_PercentBrown <- merge(Data, Data_PercentBrown_add_1, by = c("Species","SpeciesID", "Week"), all = TRUE)

#Calculate PercentBrown_Est
#PercentBrown_Est = estimated percent brown of plants which have died and dropped out of study
Data_PercentBrown_add_2 <- Data_PercentBrown %>% 
  filter(is.na(PercentBrown)) %>% 
  mutate(PercentBrown_Est = ifelse(Dead == "dead", PercentBrownMax, PercentBrown)) %>% 
  select(c("Species","SpeciesID","Week","PercentBrown_Est"))

#Add data to Data_PercentBrown
Data_PercentBrown <- merge(Data_PercentBrown, Data_PercentBrown_add_2, by = c("Species","SpeciesID", "Week"), all = TRUE)

# #TEST - Combine all percent brown data to include PercentBrown_Est
# Data_PercentBrown_test <- Data_PercentBrown %>% 
#   mutate(PercentBrown_Est = ifelse(is.na(PercentBrown_Est), Data_PercentBrown$PercentBrown, Data_PercentBrown$PercentBrown_Est)) %>% 
#   select(c("Species","SpeciesID","Week","Dead_Count","PercentBrown","PercentBrownMax","PercentBrown_Est"))

#Combine all percent brown data to include PercentBrown_Est
Data_PercentBrown <- Data_PercentBrown %>% 
  mutate(PercentBrown_Est = ifelse(is.na(PercentBrown_Est), Data_PercentBrown$PercentBrown, Data_PercentBrown$PercentBrown_Est))


#Now convert to NA during missing christmas readings, and missing data week 9.5
Data_PercentBrown_testing_1 <- Data_PercentBrown
Data_PercentBrown_testing_1$PercentBrown_Est1 <- ifelse(Data_PercentBrown_testing_1$Species == "PIFL" & Data_PercentBrown_testing_1$Week == 16, NA, Data_PercentBrown_testing_1$PercentBrown_Est)
Data_PercentBrown <- Data_PercentBrown_testing_1
Data_PercentBrown_testing_2 <- Data_PercentBrown
Data_PercentBrown_testing_2$PercentBrown_Est2 <- ifelse(Data_PercentBrown_testing_2$Species == "PSME" & Data_PercentBrown_testing_2$Week == 16, NA, Data_PercentBrown_testing_2$PercentBrown_Est1)
Data_PercentBrown <- Data_PercentBrown_testing_2
Data_PercentBrown_testing_3 <- Data_PercentBrown
Data_PercentBrown_testing_3$PercentBrown_Est3 <- ifelse(Data_PercentBrown_testing_3$Species == "PIED" & Data_PercentBrown_testing_3$Week == 18, NA, Data_PercentBrown_testing_3$PercentBrown_Est2)
Data_PercentBrown <- Data_PercentBrown_testing_3
Data_PercentBrown_testing_4 <- Data_PercentBrown
Data_PercentBrown_testing_4$PercentBrown_Est <- ifelse(Data_PercentBrown_testing_4$Species == "PIPO" & Data_PercentBrown_testing_4$Week == 9.5, NA, Data_PercentBrown_testing_4$PercentBrown_Est3)
Data_PercentBrown <- Data_PercentBrown_testing_4


test <- Data_PercentBrown %>% 
  select(c("Species","SpeciesID","Week","Dead_Count","PercentBrown","PercentBrownMax","PercentBrown_Est1","PercentBrown_Est2","PercentBrown_Est")) %>% 
  arrange(Species, Week, SpeciesID)

#last, remove extra columns and reorder columns
Data_PercentBrown <- Data_PercentBrown %>% 
  select(-c("PercentBrown_Est1","PercentBrown_Est2","PercentBrown_Est3"))

Data_PercentBrown <- Data_PercentBrown[ ,c(4,5,6,7,1,2,8,9,10,11,12,13,14,3,15,16,17,18,19,20,21,22,23,24)]

#save as csv
write.csv(Data_PercentBrown, "data_QAQC/Data_PercentBrown.csv", quote = FALSE, row.names = FALSE)
