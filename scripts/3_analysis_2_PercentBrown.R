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
  mutate(SD_PercentBrown = sd(PercentBrown, na.rm = T))


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
  group_by(Week, Species, Treatment_temp, Treatment_water) %>%
  summarize(Dead_Count = sum(Dead_Count),
            PercentBrown = round(mean(PercentBrown, na.rm = T), digits = 0),
            PercentBrown_Est = round(mean(PercentBrown_Est, na.rm = T), digits = 0),
            SD_PercentBrown = mean(SD_PercentBrown, na.rm = T))

#save as csv
write.csv(Phase1_Data_PercentBrown_Avg, "data_analysis/Phase1_Data_PercentBrown_Avg.csv", quote = FALSE, row.names = FALSE)

