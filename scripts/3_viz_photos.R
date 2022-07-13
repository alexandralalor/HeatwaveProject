#Heatwave Project Phase 1
#Data from Phase1_Photos, filtered for grey colors AND pixel count
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-05-02
#Last updated: 2022-07-08

#load libraries
library(tidyverse)
library(colorfindr) #for make_palette

#read csv
Phase1_Photos <- read_csv("data_QAQC/Phase1_Photos.csv")

#check data
glimpse(Phase1_Photos)

#check these...
Phase1_Photos$Date <- as.Date(Phase1_Photos$Date, format = "%Y-%m-%d")

#convert variables
Phase1_Photos$Week <- as.double(Phase1_Photos$Week)
Phase1_Photos$Species <- as.factor(Phase1_Photos$Species)
Phase1_Photos$Treatment_temp <- as.factor(Phase1_Photos$Treatment_temp)
Phase1_Photos$Treatment_water <- as.factor(Phase1_Photos$Treatment_water)
Phase1_Photos$PorometerSubset <- as.factor(Phase1_Photos$PorometerSubset)
Phase1_Photos$Dead <- as.factor(Phase1_Photos$Dead)
Phase1_Photos$red_class <- as.factor(Phase1_Photos$red_class)
Phase1_Photos$green_class <- as.factor(Phase1_Photos$green_class)
Phase1_Photos$blue_class <- as.factor(Phase1_Photos$blue_class)


#select which trees to visualize
Phase1_Photos_graph <- Phase1_Photos %>% 
  filter(SpeciesID %in% c("PIEN08", "PIEN42")) %>% 
  arrange(SpeciesID, Date, desc(col_share))


#save hex colors for visualization
colors <- Phase1_Photos_graph$col_hex

#graph!
#try to find a way to arrange the y-axis by most frequent colors to least frequent
Phase1_Photos_graph %>% 
  mutate(Treatment = ifelse(Treatment_temp == "Ambient+HW", "Drought with Heatwave", "Drought")) %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  facet_wrap(~Treatment) +
  #facet_grid(rows = vars(SpeciesID)) +
  scale_x_continuous(breaks = 1:36) +
  ylab("Color Percent") +
  xlab("Weeks") +
  labs(title = "Color change of droughted Ponderosa Pine seedlings over time",
       subtitle = "With and without heatwave (week 7)",
       caption = "Data collected using photographs from August 26 2021 to November 19 2021, as part of Alexandra Lalor's MS project") +
  theme_minimal()


#colorfindr make_palette
Phase1_Photos_graph %>%
  filter(Week == 11, SpeciesID == "PIPO23") %>% 
  make_palette(n = 15)






