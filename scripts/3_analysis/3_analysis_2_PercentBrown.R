#Data analysis - percent brown
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-18
#Last updated: 2026-03-01


#load packages
library(tidyverse)

#read csv
Data_PercentBrown <- read_csv("data/data_QAQC/Data_PercentBrown.csv")

################################################################################
# SD
################################################################################

Data_PercentBrown <- Data_PercentBrown %>% 
  group_by(Phase, Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD_PercentBrown = sd(PercentBrown_Est, na.rm = T))

################################################################################
# Samples sizes per week
################################################################################

#samples sizes per week
summary_1 <- Data_PercentBrown %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_PercentBrown = sum(!is.na(PercentBrown_Est)))

Data_PercentBrown <- merge(Data_PercentBrown, summary_1, all.x = T)

#reorder columns
Data_PercentBrown <- Data_PercentBrown[, c(1,6,7,8,2,9,3,4,10,11,12,13,14,5,26,15,16,17,18,19,20,21,22,23,24,25)]

#save as csv
write.csv(Data_PercentBrown, "data/data_analysis/Data_PercentBrown.csv", quote = FALSE, row.names = FALSE)


################################################################################
#read csv
Data_PercentBrown <- read_csv("data/data_analysis/Data_PercentBrown.csv")


################################################################################
# Average
################################################################################

#filter NAs
Data_PercentBrown <- Data_PercentBrown %>% 
  filter(!is.na(PercentBrown_Est)) %>% 
  mutate(PercentGreen_Est = 100 - PercentBrown_Est)

#average data 
Data_PercentBrown_Avg <- Data_PercentBrown %>%
  group_by(Phase, ScientificName, Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(SampleSize_Weekly_PercentBrown = mean(SampleSize_Weekly_PercentBrown),
            Dead_Count = sum(Dead_Count),
            PercentBrown = round(mean(PercentBrown, na.rm = T), digits = 0),
            PercentBrown_Est = round(mean(PercentBrown_Est, na.rm = T), digits = 0),
            PercentGreen_Est = round(mean(PercentGreen_Est, na.rm = T), digits = 0),
            SD_PercentBrown = mean(SD_PercentBrown, na.rm = T))

#save as csv
write.csv(Data_PercentBrown_Avg, "data/data_analysis/Data_PercentBrown_Avg.csv", quote = FALSE, row.names = FALSE)

