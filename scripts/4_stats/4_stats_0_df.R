#Data analysis - photos
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-25
#Last updated: 2026-03-01


#load packages
library(tidyverse)

################################################################################
#data frames for analysis

#read csv
Data_All <- read_csv("data/data_analysis/Data_All.csv")

#factor levels
# Data_All <- 
#   transform(Data_All, Species = factor(Species, levels = c("PIPO", "PIED", "PSME", "PIEN", "PIFL")))
Data_All <- 
  transform(Data_All, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))

#Dead Week data
Dead_Week <- Data_All %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Phase, Species, Treatment_temp, Dead_Week) %>% 
  summarize(SpeciesID = unique(SpeciesID))
#Dead Week data
Dead_Week_Weight <- Data_All %>% 
  filter(Treatment_water == "Drought", !is.na(Stress_to_Dead_Weight)) %>% 
  mutate(Stress_to_Dead_Weight = round(Stress_to_Dead_Weight, digits = 1)) %>% 
  group_by(Phase, Species, Treatment_temp, Stress_to_Dead_Weight) %>% 
  summarize(SpeciesID = unique(SpeciesID))
#Dead Week data
Dead_Week_Porometer <- Data_All %>% 
  filter(Treatment_water == "Drought", !is.na(Stress_to_Dead_Porometer)) %>% 
  group_by(Phase, Species, Treatment_temp, Stress_to_Dead_Porometer) %>% 
  summarize(SpeciesID = unique(SpeciesID))

# #save as csv
write.csv(Dead_Week, "data/data_analysis/Dead_Week.csv", quote = FALSE, row.names = FALSE)
write.csv(Dead_Week_Weight, "data/data_analysis/Dead_Week_Weight.csv", quote = FALSE, row.names = FALSE)
write.csv(Dead_Week_Porometer, "data/data_analysis/Dead_Week_Porometer.csv", quote = FALSE, row.names = FALSE)
