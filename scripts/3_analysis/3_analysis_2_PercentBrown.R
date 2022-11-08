#Data analysis - percent brown
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-18
#Last updated: 2022-09-18


#load packages
library(tidyverse)

#read csv
Phase1_Data_PercentBrown <- read_csv("data_QAQC/Phase1_Data_PercentBrown.csv")

################################################################################
# SD
################################################################################

Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD_PercentBrown = sd(PercentBrown_Est, na.rm = T))

################################################################################
# Samples sizes per week
################################################################################

#samples sizes per week
summary_1 <- Phase1_Data_PercentBrown %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_PercentBrown = sum(!is.na(PercentBrown_Est)))

Phase1_Data_PercentBrown <- merge(Phase1_Data_PercentBrown, summary_1, all.x = T)

#reorder columns
#Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown[, c(5,6,7,8,1,9,2,3,10,11,12,13,14,4,26,15,16,17,18,19,20,21,22,23,24,25)]

#save as csv
write.csv(Phase1_Data_PercentBrown, "data_analysis/Phase1_Data_PercentBrown.csv", quote = FALSE, row.names = FALSE)


################################################################################
#read csv
Phase1_Data_PercentBrown <- read_csv("data_analysis/Phase1_Data_PercentBrown.csv")


################################################################################
# Average
################################################################################

#filter NAs
Phase1_Data_PercentBrown <- Phase1_Data_PercentBrown %>% 
  filter(!is.na(PercentBrown_Est))


#average data 
Phase1_Data_PercentBrown_Avg <- Phase1_Data_PercentBrown %>%
  group_by(Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(SampleSize_Weekly_PercentBrown = mean(SampleSize_Weekly_PercentBrown),
            Dead_Count = sum(Dead_Count),
            PercentBrown = round(mean(PercentBrown, na.rm = T), digits = 0),
            PercentBrown_Est = round(mean(PercentBrown_Est, na.rm = T), digits = 0),
            SD_PercentBrown = mean(SD_PercentBrown, na.rm = T))

#save as csv
write.csv(Phase1_Data_PercentBrown_Avg, "data_analysis/Phase1_Data_PercentBrown_Avg.csv", quote = FALSE, row.names = FALSE)

