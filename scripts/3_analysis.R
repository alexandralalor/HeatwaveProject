#Data analysis
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-08-27

#load packages
library(tidyverse)

#read csv
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

#Dead Week data
Dead_Week <- Phase1_Data_All %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Species, Treatment_temp, Dead_Week) %>% 
  summarize(SpeciesID = unique(SpeciesID))
#Dead Week data
Dead_Week_Weight <- Phase1_Data_All %>% 
  filter(Treatment_water == "Drought", !is.na(Stress_to_Dead_Weight)) %>% 
  mutate(Stress_to_Dead_Weight = round(Stress_to_Dead_Weight, digits = 1)) %>% 
  group_by(Species, Treatment_temp, Stress_to_Dead_Weight) %>% 
  summarize(SpeciesID = unique(SpeciesID))
#Dead Week data
Dead_Week_Porometer <- Phase1_Data_All %>% 
  filter(Treatment_water == "Drought", !is.na(Stress_to_Dead_Porometer)) %>% 
  group_by(Species, Treatment_temp, Stress_to_Dead_Porometer) %>% 
  summarize(SpeciesID = unique(SpeciesID))

# #save as csv
# write.csv(Dead_Week, "data_analysis/Dead_Week.csv", quote = FALSE, row.names = FALSE)
# write.csv(Dead_Week_Weight, "data_analysis/Dead_Week_Weight.csv", quote = FALSE, row.names = FALSE)
# write.csv(Dead_Week_Porometer, "data_analysis/Dead_Week_Porometer.csv", quote = FALSE, row.names = FALSE)


#for some reason need to make a function to round
rnd <- function(x) trunc(x+sign(x)*0.5)
Dead_Week <- Dead_Week %>% 
  mutate(Dead_Week_round = rnd(Dead_Week))


PIPO_Amb <- Dead_Week %>% 
  filter(Species == "PIPO", Treatment_temp == "Ambient")
PIPO_HW <- Dead_Week %>% 
  filter(Species == "PIPO", Treatment_temp == "Ambient_HW")
PIED_Amb <- Dead_Week %>% 
  filter(Species == "PIED", Treatment_temp == "Ambient")
PIED_HW <- Dead_Week %>% 
  filter(Species == "PIED", Treatment_temp == "Ambient_HW")
PIFL_Amb <- Dead_Week %>% 
  filter(Species == "PIFL", Treatment_temp == "Ambient")
PIFL_HW <- Dead_Week %>% 
  filter(Species == "PIFL", Treatment_temp == "Ambient_HW")
PSME_Amb <- Dead_Week %>% 
  filter(Species == "PSME", Treatment_temp == "Ambient")
PSME_HW <- Dead_Week %>% 
  filter(Species == "PSME", Treatment_temp == "Ambient_HW")
PIEN_Amb <- Dead_Week %>% 
  filter(Species == "PIEN", Treatment_temp == "Ambient")
PIEN_HW <- Dead_Week %>% 
  filter(Species == "PIEN", Treatment_temp == "Ambient_HW")





#histogram
Dead_Week %>% 
  group_by(Species, Treatment_temp) %>% 
  ggplot(aes(x = Dead_Week,
             fill = Treatment_temp)) +
  geom_histogram(binwidth = 1) +
  scale_fill_discrete(direction = -1) +
  #ylim(0, 12.5) +
  xlim(0, 40) +
  facet_grid(reorder(Species, Dead_Week, mean) ~ Treatment_temp) +
  theme_minimal()


#boxplot
Dead_Week %>% 
  group_by(Species, Treatment_temp) %>% 
  arrange(Dead_Week) %>% 
  #filter(Species == "PIFL") %>% 
  ggplot(aes(x = Dead_Week,
             y = reorder(Species, Dead_Week, mean),
             fill = Treatment_temp)) +
  geom_boxplot() +
  xlim(0, 40) +
  ylab("Species") +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 6,
           color = "red",
           linetype = "dashed",
           size = 0.4) +
  theme_minimal() +
  scale_fill_discrete(direction = -1)
  




