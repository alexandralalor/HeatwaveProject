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

#size scatterplot
Phase1_Data %>% 
  group_by(Species) %>% 
  ggplot(aes(x = BasalDia_mm,
             y = Height_mm,
             color = Species)) +
  geom_point() +
  xlim(0,12) +
  ylim(0,600) +
  xlab("Basal Stem Diameter") +
  ylab("Height (mm)") +
  theme_minimal()
