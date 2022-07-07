#QAQC - check porometer data against plant data
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read in csvs
Phase1_Plants_Porometer_graph <- read_csv("data_QAQC/Phase1_Plants_Porometer_graph.csv")

#Look at droughted trees
Phase1_Plants_Porometer_graph <- Phase1_Plants_Porometer_graph %>% 
  filter(Treatment_water == "Drought")


#Graphs!
#Porometer Conductance
Phase1_Plants_Porometer_graph %>% 
  filter(Species == "PSME") %>% 
  group_by(Treatment_temp) %>%
  ggplot(aes(x = Week,
             y = Conductance,
             color = Treatment_temp)) +
  geom_point() +
  geom_line() +
  ylim(0, 350) +
  xlim(0,36) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 350,
           color = "red",
           linetype = "dashed",
           size = 0.8) +
  geom_text(label = "Heatwave",
            x = 9, y = 300, color = "red", size = 3) +
  xlab("Week") +
  ylab("Stomatal Conductance") +
  labs(title = "Stomatal Conductance of Droughted Trees") +
  theme_minimal()

#Porometer Temperatures
Phase1_Plants_Porometer_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = Temperature,
             color = Species)) +
  geom_point() +
  ylim(0, 40) +
  xlab("Week") +
  ylab("Temperature (Celcius)") +
  labs(title = "Temperature Fluctuations") +
  theme_minimal()

#Porometer Leaf Sensor RH
Phase1_Plants_Porometer_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = LeafSensor,
             color = Species)) +
  geom_point() +
  ylim(0, 100) +
  xlab("Week") +
  ylab("Leaf Sensor (%RH)") +
  labs(title = "Leaf Sensor RH Fluctuations") +
  theme_minimal()

#Porometer Filter Sensor RH
Phase1_Plants_Porometer_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = FilterSensor,
             color = Species)) +
  geom_point() +
  ylim(0, 40) +
  xlab("Week") +
  ylab("Filter Sensor (%RH)") +
  labs(title = "Filter Sensor RH Fluctuations") +
  theme_minimal()

