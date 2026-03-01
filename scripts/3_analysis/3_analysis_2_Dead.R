#Data analysis - dead
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2026-03-01

#load packages
library(tidyverse)

#read CSVs
Data <- read_csv("data/data_QAQC/Data.csv")

################################################################################
# add in samples size
################################################################################

#all sample sizes
summary_1 <- Data %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water) %>% 
  summarize(SampleSize = length(unique(SpeciesID)))

Data <- merge(Data, summary_1, 
                     by = c("Phase", "Species", "Treatment_temp", "Treatment_water"), all.x = T)

#porometer samples sizes
summary_2 <- Data %>% 
  filter(PorometerSubset == "yes") %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water, PorometerSubset) %>% 
  summarize(SampleSize_Porometer = length(unique(SpeciesID)))

Data <- merge(Data, summary_2,
                     by = c("Phase", "Species", "Treatment_temp", "Treatment_water", "PorometerSubset"),
                     all.x = T)

#weekly sample sizes
summary_3 <- Data %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Dead = sum(!is.na(Dead)))

Data <- merge(Data, summary_3, all.x = T)



################################################################################
# figure out which week plants died
################################################################################
summary_4 <- Data %>% 
  group_by(Phase, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  filter(Dead == "dead") %>% 
  mutate(Dead_Week = min(Week)) %>% 
  summarize(Dead_Week = mean(Dead_Week)) %>% 
  select("Phase", "Species", "SpeciesID", "Treatment_temp", "Treatment_water", "Dead_Week")

Data <- merge(Data, summary_4, all.x = T)



#reorder and rearrange columns
Data <- Data[, c(1,8,9,10,2,5,3,4,7,22,23,24,11,12,13,14,6,15,16,17,18,25,19,20,21)]
Data <- Data %>% 
  group_by(Phase, Species) %>% 
  arrange(SpeciesID, Week)

################################################################################

#save csv
write.csv(Data, "data/data_analysis/Data.csv", quote = FALSE, row.names = FALSE)

