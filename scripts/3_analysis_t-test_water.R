#Data analysis - anova
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-21
#Last updated: 2022-10-21

#load packages
library(tidyverse)

#read csvs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")
Phase1_Data_All_Avg <- read_csv("data_analysis/Phase1_Data_All_Avg.csv")

#dead count
summary <- Phase1_Data_All_Avg %>% 
  group_by(Species, Treatment_temp, Treatment_water) %>% 
  summarize(SampleSize = max(SampleSize),
            Dead_Count = max(Dead_Count, na.rm = T))
summary <- summary %>% 
  group_by(Species, Treatment_temp, Treatment_water) %>% 
  summarize(SampleSize = SampleSize,
            Dead_Count = Dead_Count,
            Alive_Count = SampleSize - Dead_Count,
            Percent_Allive = (Alive_Count / SampleSize)*100)
