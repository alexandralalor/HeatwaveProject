#Data analysis - dead
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-08-27

#load packages
library(tidyverse)

#read CSVs
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

################################################################################
# add in samples size
################################################################################

#all sample sizes
summary_1 <- Phase1_Data %>% 
  group_by(Species, Treatment_temp, Treatment_water) %>% 
  summarize(SampleSize = length(unique(SpeciesID)))

Phase1_Data <- merge(Phase1_Data, summary_1, 
                     by = c("Species", "Treatment_temp", "Treatment_water"), all.x = T)

#porometer samples sizes
summary_2 <- Phase1_Data %>% 
  filter(PorometerSubset == "yes") %>% 
  group_by(Species, Treatment_temp, Treatment_water, PorometerSubset) %>% 
  summarize(SampleSize_Porometer = length(unique(SpeciesID)))

Phase1_Data <- merge(Phase1_Data, summary_2,
                     by = c("Species", "Treatment_temp", "Treatment_water", "PorometerSubset"),
                     all.x = T)

#weekly sample sizes
summary_3 <- Phase1_Data %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Dead = sum(!is.na(Dead)))

Phase1_Data <- merge(Phase1_Data, summary_3, all.x = T)



################################################################################
# figure out which week plants died
################################################################################
summary_4 <- Phase1_Data %>% 
  group_by(Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  filter(Dead == "dead") %>% 
  mutate(Dead_Week = min(Week)) %>% 
  summarize(Dead_Week = mean(Dead_Week)) %>% 
  select("Species", "SpeciesID", "Treatment_temp", "Treatment_water", "Dead_Week")

Phase1_Data <- merge(Phase1_Data, summary_4, all.x = T)



#reorder and rearrange columns
Phase1_Data <- Phase1_Data[, c(7,8,9,10,1,4,2,3,6,22,23,24,11,12,13,14,5,15,16,17,18,25,19,20,21)]
Phase1_Data <- Phase1_Data %>% 
  group_by(Species) %>% 
  arrange(SpeciesID, Week)

################################################################################

#save csv
write.csv(Phase1_Data, "data_analysis/Phase1_Data.csv", quote = FALSE, row.names = FALSE)

