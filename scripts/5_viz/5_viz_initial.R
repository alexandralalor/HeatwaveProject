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
  geom_point() +
  ylim(0,600) +
  xlab("Basal Stem Diameter") +
  ylab("Height (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal() +
  theme(legend.position = "none")


#size boxplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = BasalDia_mm,
             color = CommonName)) +
  geom_boxplot() +
  #ylim(0,.03) +
  xlab("Species") +
  ylab("Basal Diameter (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal() +
  theme(legend.position = "none")

#size boxplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Biomass_g,
             color = CommonName)) +
  geom_boxplot() +
  #ylim(0,.03) +
  xlab("Species") +
  ylab("Biomass (g)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal() +
  theme(legend.position = "none")

#size boxplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Whorls,
             color = CommonName)) +
  geom_boxplot() +
  ylim(0,8) +
  xlab("Species") +
  ylab("Whorls") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal() +
  theme(legend.position = "none")


################################################################################
#density
# basal diameter to area
# pi*(basaldai/2)^2
# basal area * height = volume
Phase1_Data_test <- Phase1_Data %>% 
  mutate(Volume = Height_mm * (pi * (BasalDia_mm / 2)^2))

#density boxplot
Phase1_Data_test %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Biomass_g / Volume,
             color = CommonName)) +
  geom_boxplot() +
  #geom_point(aes(size = Phase1_Data$Biomass_g)) +
  #xlim(2,11) +
  #ylim(0,.03) +
  xlab("Species") +
  ylab("Volume (g/mm^3)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal() +
  theme(legend.position = "none")

#################################################################################
# calculations

Phase1_Data %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Height_mm),
            range_min = min(Height_mm),
            range_max = max(Height_mm))
Phase1_Data %>% 
  group_by(Species) %>% 
  summarize(mean = mean(BasalDia_mm),
            range_min = min(BasalDia_mm),
            range_max = max(BasalDia_mm))

Phase1_Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Biomass_g),
            range_min = min(Biomass_g),
            range_max = max(Biomass_g))

Phase1_Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Whorls),
            range_min = min(Whorls),
            range_max = max(Whorls))

Phase1_Data %>% 
  #filter(Treatment_water == "Drought") %>% 
  group_by(Species, Treatment_temp, Treatment_water) %>% 
  summarize(N = length(unique(SpeciesID)))

