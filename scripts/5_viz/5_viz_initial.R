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
Data <- read_csv("data/data_QAQC/Data.csv")

#check out data
glimpse(Data)

#convert variables
Data$Phase <- as.factor(Data$Phase)
Data$Chamber <- as.factor(Data$Chamber)
Data$ScientificName <- as.factor(Data$ScientificName)
Data$CommonName <- as.factor(Data$CommonName)
Data$Species <- as.factor(Data$Species)
Data$Treatment_temp <- as.factor(Data$Treatment_temp)
Data$Treatment_water <- as.factor(Data$Treatment_water)
Data$PorometerSubset <- as.factor(Data$PorometerSubset)
Data$Dead <- as.factor(Data$Dead)
Data$Heatwave_graph <- as.factor(Data$Heatwave_graph)
Data$Heatwave <- as.factor(Data$Heatwave)


################################################################################
#Graph! Initial Data
################################################################################

#size scatterplot
Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = BasalDia_mm,
             y = Height_mm,
             color = Phase)) +
  geom_point(alpha = 0.03) +
  facet_wrap(~CommonName) +
  #geom_point(aes(size = Data$Biomass_g)) +
  xlim(2,11) +
  ylim(0,500) +
  xlab("Basal Diameter (mm)") +
  ylab("Height (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal()

#size boxplot
Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Height_mm,
             color = Phase)) +
  geom_boxplot() +
  ylim(0,600) +
  xlab("Species") +
  ylab("Height (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal()


#size boxplot
Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = BasalDia_mm,
             color = Phase)) +
  geom_boxplot() +
  #ylim(0,.03) +
  xlab("Species") +
  ylab("Basal Diameter (mm)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal()

#size boxplot
Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Biomass_g,
             color = Phase)) +
  geom_boxplot() +
  #ylim(0,.03) +
  xlab("Species") +
  ylab("Biomass (g)") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal()

#size boxplot
Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Whorls,
             color = Phase)) +
  geom_boxplot() +
  ylim(0,8) +
  xlab("Species") +
  ylab("Whorls") +
  labs(title = "Species Size Distribution") +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_minimal()


################################################################################
#density
# basal diameter to area
# pi*(basaldai/2)^2
# basal area * height = volume
Data_test <- Data %>% 
  mutate(Volume = Height_mm * (pi * (BasalDia_mm / 2)^2))

#density boxplot
Data_test %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = Biomass_g / Volume,
             color = CommonName)) +
  geom_boxplot() +
  #geom_point(aes(size = Data$Biomass_g)) +
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

Data %>% 
  group_by(Phase, Species) %>% 
  summarize(mean = mean(Height_mm),
            range_min = min(Height_mm),
            range_max = max(Height_mm))
Data %>% 
  group_by(Phase, Species) %>% 
  summarize(mean = mean(BasalDia_mm),
            range_min = min(BasalDia_mm),
            range_max = max(BasalDia_mm))

Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Phase, Species) %>% 
  summarize(mean = mean(Biomass_g),
            range_min = min(Biomass_g),
            range_max = max(Biomass_g))

Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Phase, Species) %>% 
  summarize(mean = mean(Whorls),
            range_min = min(Whorls),
            range_max = max(Whorls))

Data %>% 
  #filter(Treatment_water == "Drought") %>% 
  group_by(Phase, Species, Treatment_temp, Treatment_water) %>% 
  summarize(N = length(unique(SpeciesID)))

