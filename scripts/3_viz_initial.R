#Data viz - initial data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-11
#Last updated: 2022-07-12

#load tidyverse
library(tidyverse)
library(scales)

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
Phase1_Data$Heatwave_graph <- as.factor(Phase1_Data$Heatwave_graph)
Phase1_Data$Heatwave <- as.factor(Phase1_Data$Heatwave)


################################################################################
#Graph! Initial Data
################################################################################

#size scatterplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = BasalDia_mm,
             y = Height_mm,
             color = CommonName)) +
  geom_point(alpha = 0.03) +
  #geom_point(aes(size = Phase1_Data$Biomass_g)) +
  xlim(2,11) +
  ylim(0,500) +
  xlab("Basal Stem Diameter") +
  ylab("Height (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal()

#size boxplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Height_mm,
             color = CommonName)) +
  geom_boxplot() +
  #geom_point(aes(size = Phase1_Data$Biomass_g)) +
  #xlim(2,11) +
  ylim(0,600) +
  xlab("Basal Stem Diameter") +
  ylab("Height (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal() +
  theme(legend.position = "none")

