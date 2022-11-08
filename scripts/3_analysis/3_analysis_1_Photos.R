#Heatwave Project Phase 1
#find %green %brown
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-18
#Last updated: 2022-09-18

#load packages
library(tidyverse)
library(colorfindr) #for 3d color plots

#read csv
Phase1_Data_Photos <- read_csv("data_QAQC/Phase1_Data_Photos.csv")

################################################################################
# find % green and % brown
################################################################################

#separate green and brown
Phase1_Data_Photos <- Phase1_Data_Photos %>% 
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA))

# #test separation of green and brown
# Phase1_Data_Photos_test %>%
#   filter(Week == 1, red_only > 0) %>%
#   plot_colors_3d(sample_size = 10000, marker_size = 2.5, color_space = "RGB")


#find percent green
Phase1_Data_Photos_green <- Phase1_Data_Photos %>% 
  filter(green_only > 0) %>% 
  group_by(Week, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(PercentGreen = sum(col_share))

#find percent brown (red)
Phase1_Data_Photos_red <- Phase1_Data_Photos %>% 
  filter(red_only > 0) %>% 
  group_by(Week, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(PercentRed = sum(col_share))

#merge
Phase1_Data_Photos_test <- merge(Phase1_Data_Photos_green, Phase1_Data_Photos_red, all = T)


#fill in NA values
sum <- Phase1_Data_Photos_test %>% 
  group_by(Week, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  summarize(PercentGreen = mean(PercentGreen, na.rm = T),
            PercentRed = mean(PercentRed, na.rm = T))
sum <- sum %>% 
  mutate(PercentGreen = ifelse(is.nan(PercentGreen), 0, PercentGreen),
         PercentRed = ifelse(is.nan(PercentRed), 0, PercentRed))

#merge
Phase1_Data_Photos <- merge(Phase1_Data_Photos, sum, by = c("Week", "Species", "SpeciesID", "Treatment_temp", "Treatment_water"), all.x = T)


################################################################################

#save csv
write.csv(Phase1_Data_Photos, "data_analysis/Phase1_Data_Photos.csv", quote=FALSE, row.names = FALSE)

