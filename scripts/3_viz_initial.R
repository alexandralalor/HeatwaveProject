#Data viz - initial data
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-11
#Last updated: 2022-07-12

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

#check out data
glimpse(Phase1_Data)

#convert variables
Phase1_Data$Phase <- as.factor(Phase1_Data$Phase)
Phase1_Data$Chamber <- as.factor(Phase1_Data$Chamber)
Phase1_Data$ScientificName <- as.factor(Phase1_Data$ScientificName)
Phase1_Data$CommonName <- as.factor(Phase1_Data$CommonName)
Phase1_Data$Species <- as.factor(Phase1_Data$Species)
Phase1_Data$Treatment_temp <- as.factor(Phase1_Data$Treatment_temp)
Phase1_Data$Treatment_water <- as.factor(Phase1_Data$Treatment_water)
Phase1_Data$PorometerSubset <- as.factor(Phase1_Data$PorometerSubset)
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)


################################################################################
#Graph! Initial Data
################################################################################

#size scatterplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = BasalDia_mm,
             y = Height_mm,
             color = Species)) +
  geom_point(alpha = 0.7) +
  xlim(0,12) +
  ylim(0,600) +
  xlab("Basal Stem Diameter") +
  ylab("Height (mm)") +
  labs(title = "Species Size Distribution") +
  theme_minimal()
