#Heatwave Project Phase 1
#Data taken from tree_rgb_sum_filter
#Moving from filtering grey colors to finding thresholds for pixel %
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-08
#Last updated: 2022-09-12

#load libraries
library(tidyverse)
#library(colorfindr) #for get_colors

#read csv
tree_rgb_sum_filter <- read_csv("data_clean/Phase1_Photos.csv")

#here we have some room to play around with different thresholds to best
#display the data. I'm choosing to remove colors with less than 100 pixels
#(which is about 0.01% of pixels per photo). Filtering this way removes about
#1/2 of the data points, but only about ~1% of the total data. Seems to be the 
#best choice to maximize data while reducing file size!

#test impact of removing small pixels
tree_rgb_small_percent <- tree_rgb_filter_viz %>% 
  filter(col_freq <= 100)
tree_rgb_big_percent <- tree_rgb_filter_viz %>% 
  filter(col_freq > 100)

#check out a histogram to see pixel distribution before and after
hist(tree_rgb_sum_filter$col_freq, breaks = 200) #before
hist(Phase1_Photos$col_freq, breaks = 200) #after


#filter to exclude infrequent colors
Phase1_Photos <- tree_rgb_sum_filter %>%
  filter(col_freq >= 100) %>%
  arrange(desc(col_share))

#check that col_shar is 98-99%
Phase1_Photos %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  summarize(percent = sum(col_share))

Phase1_Photos <- Phase1_Photos %>% 
  mutate(Treatment_temp = ifelse(Treatment_temp == "Ambient+HW", "Ambient_HW", "Ambient"))

#save csv
write.csv(Phase1_Photos, "data_QAQC/Phase1_Photos.csv", quote=FALSE, row.names = FALSE)


################################################################################

#combine with meta data
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

glimpse(Phase1_Photos)
glimpse(Phase1_Data)

Phase1_Data_add <- Phase1_Data %>%
  select(-c("Species","Treatment_temp","Treatment_water"))

Phase1_Data_Photos <- merge(Phase1_Data_add, Phase1_Photos, by = c("Week", "SpeciesID"))
Phase1_Data_Photos <- Phase1_Data_Photos[ ,c(1,19,20,2,21,22,7,12,13,14,15,16,17,18,23,24,25,26,27,28,29,30,31,32)]
Phase1_Data_Photos <- Phase1_Data_Photos %>%
  select(-c("Date"))

################################################################################

#save csv
write.csv(Phase1_Data_Photos, "data_QAQC/Phase1_Data_Photos.csv", quote=FALSE, row.names = FALSE)
