#Heatwave Project Phase 1
#play around with visualizations for photo data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-09-11

#load packages
library(tidyverse)
# library(ggtern) #for rbg2hex

#read_csv
Phase1_Data_Photos_Avg <- read_csv("data_analysis/Phase1_Data_Photos_Avg.csv")
#select which trees to visualize

# YES!
# must arrange according to order of graph display
# First, all ambient displayed (week 1 - end). Then all hw (week 1-end)
# So, arrange by treatment_temp, then week. 
Phase1_Data_Photos_graph <- Phase1_Data_Photos_Avg %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Species == "PIFL") %>% 
  arrange(Species, Treatment_temp, Week, green_only, desc(red_only))

#save hex colors for visualization
colors <- Phase1_Data_Photos_graph$col_hex

#graph!
#try to find a way to arrange the y-axis by most frequent colors to least frequent
Phase1_Data_Photos_graph %>% 
  #mutate(Treatment = ifelse(Treatment_temp == "Ambient+HW", "Drought with Heatwave", "Drought")) %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  facet_wrap(~Treatment_temp) +
  # facet_grid(rows = vars(Species),
  #            cols = vars(Treatment_temp)) +
  annotate("rect",
           xmin = 6.5, xmax = 7.5,
           ymin = 0, ymax = 100,
           color = "red",
           alpha = 0,
           size = 0.3) +
  geom_errorbar(aes(x = Phase1_Data_Photos_graph$Week,
                    ymin = (Phase1_Data_Photos_graph$PercentGreen - Phase1_Data_Photos_graph$SD_PercentRed),
                    ymax = (Phase1_Data_Photos_graph$PercentGreen + Phase1_Data_Photos_graph$SD_PercentRed)))
  scale_x_continuous(breaks = 1:22) +
  ylab("Color Percent") +
  xlab("Weeks") +
  labs(title = "Color - PIED") +
  theme_minimal()

  