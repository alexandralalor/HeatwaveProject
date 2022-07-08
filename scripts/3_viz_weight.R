#Data viz - weights
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Plants_Meta <- read_csv("data_QAQC/Phase1_Plants_Meta.csv")

#filter for weight data
Phase1_Plants_Meta_weight <- Phase1_Plants_Meta %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Weight_g")) %>% 
  filter(Treatment_water == "Drought", !is.na(Weight_g))

#summarize weight data
Phase1_Plants_Meta_weight_graph <- Phase1_Plants_Meta_weight %>% 
  group_by(Species, Week) %>% 
  summarize(Weight = mean(Weight_g))

#Graphs!
#Weight
Phase1_Plants_Meta_weight_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = Weight,
             color = Species)) +
  geom_point() +
  geom_line() +
  ylim(200, 800) +
  xlim(0,36) +
  annotate("segment",
           x = 7, xend = 7,
           y = 200, yend = 800,
           color = "red",
           linetype = "dashed",
           size = 0.8) +
  geom_text(label = "Heatwave",
            x = 9, y = 800, color = "red", size = 3) +
  xlab("Week") +
  ylab("Weight (g)") +
  labs(title = "Weight of Droughted Trees") +
  theme_minimal()
