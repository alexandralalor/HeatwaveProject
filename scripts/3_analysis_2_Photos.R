#Heatwave Project Phase 1
#combine color data and porometer data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-09-18

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


################################################################################
# Add dead week and stress weeks to photos data
################################################################################

Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

Phase1_Data_add <- Phase1_Data_All %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(SpeciesID) %>% 
  select(c("Dead_Week", "Stress_Week_Weight", "Stress_Week_Porometer")) %>% 
  summarize(Dead_Week = mean(Dead_Week),
            Stress_Week_Weight = round(mean(Stress_Week_Weight, na.rm = T), digits = 0),
            Stress_Week_Porometer = mean(Stress_Week_Porometer, na.rm = T))

Phase1_Data_Photos <- merge(Phase1_Data_Photos, Phase1_Data_add, by = "SpeciesID", all.x = T)

Phase1_Data_Photos <- Phase1_Data_Photos %>% 
  arrange(SpeciesID, Week)

#save csv
write.csv(Phase1_Data_Photos, "data_analysis/Phase1_Data_Photos.csv", quote = FALSE, row.names = FALSE)


################################################################################
# find average
################################################################################

# it seems like, over time, blue stays the same, red increases, and green decreases
# goal: find average rgb across all individuals per species for each week

Phase1_Data_Photos_Avg <- Phase1_Data_Photos %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
  summarize(SampleSize_Weekly_Photos = mean(SampleSize_Weekly_Photos),
            Dead_Count = sum(Dead_Count),
            red = round(mean(red, na.rm = T)),
            green = round(mean(green, na.rm = T)),
            blue = round(mean(blue, na.rm = T)),
            col_freq = sum(col_freq),
            PercentGreen = mean(PercentGreen, na.rm = T),
            PercentRed = mean(PercentRed, na.rm = T),
            SD_PercentRed = mean(SD_PercentRed, na.rm = T))

#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = Phase1_Data_Photos_Avg$red,
                             g = Phase1_Data_Photos_Avg$green,
                             b = Phase1_Data_Photos_Avg$blue))
colnames(hex) <- "col_hex"
Phase1_Data_Photos_Avg <- cbind(Phase1_Data_Photos_Avg, hex)

#calculate total # pixels and percent of each color, add to summary df
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

#reorder columns
#Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg[, c(1,2,3,4,8,9,5,6,7,10,11,12,17,13,18,19,14,15,16)]


#separate green and brown
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA))

#percent green / brown
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  mutate(PercentGreen = mean(PercentGreen, na.rm = T),
         PercentRed = mean(PercentRed, na.rm = T))

#save csv
write.csv(Phase1_Data_Photos_Avg, "data_analysis/Phase1_Data_Photos_Avg.csv", quote=FALSE, row.names = FALSE)



