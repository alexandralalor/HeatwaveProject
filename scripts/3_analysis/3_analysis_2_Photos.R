#Heatwave Project Phase 1
#combine color data and porometer data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-10-25

#load packages
library(tidyverse)
library(ggtern) #for rbg2hex
library(colorfindr) #for 3d color plots

#read csv
Phase1_Data_Photos <- read_csv("data_analysis/Phase1_Data_Photos.csv")


################################################################################
# SD
################################################################################

Phase1_Data_Photos <- Phase1_Data_Photos %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD_PercentRed = sd(PercentRed, na.rm = T))


################################################################################
# Samples sizes per week
################################################################################

summary_1 <- Phase1_Data_Photos %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Photos = length(unique(SpeciesID)))

Phase1_Data_Photos <- merge(Phase1_Data_Photos, summary_1, all.x = T)

#rearrange columns
#Phase1_Data_Photos <- Phase1_Data_Photos[ ,c(1,2,5,3,4,6,29,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]


#save csv
write.csv(Phase1_Data_Photos, "data_analysis/Phase1_Data_Photos.csv", quote = FALSE, row.names = FALSE)





