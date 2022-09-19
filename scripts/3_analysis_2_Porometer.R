#Data analysis - porometer
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-18
#Last updated: 2022-09-18


#load packages
library(tidyverse)

#read csv
Phase1_Data_Porometer <- read_csv("data_analysis/Phase1_Data_Porometer.csv")

################################################################################
# Average stress week by species and treatment
# Remember that watered trees don't have a stress week
################################################################################

Phase1_Data_Porometer_add <- Phase1_Data_Porometer %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Heatwave_graph) %>% 
  mutate(Stress_Week_Avg = round(mean(Stress_Week, na.rm = TRUE), digits = 1))

Phase1_Data_Porometer <- merge(Phase1_Data_Porometer, Phase1_Data_Porometer_add, all = T)

################################################################################
# SD
################################################################################

Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD = sd(Porometer_Est, na.rm = T))

#save as csv
write.csv(Phase1_Data_Porometer, "data_analysis/Phase1_Data_Porometer.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Average
################################################################################

