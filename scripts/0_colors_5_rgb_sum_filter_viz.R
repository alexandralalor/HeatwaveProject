#Data Viz Final Project
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-05-02
#Last updated: 2022-05-02

#load libraries
library(tidyverse)
# library(colorfindr) #for get_colors
# library(sjmisc) #for rotate_df
# library(ggtern) #for rbg2hex
# library(countcolors) #for masking and reducing black colors to one point
# library(tools) #for file naming

tree_rgb_sum_filter <- read.csv("data_raw/final_project/tree_rgb_sum_filter/tree_rgb_sum_filter_all.csv")

#visualize

### 
tree_rgb_filter_viz <- tree_rgb_sum_filter %>% 
  filter(SpeciesID %in% c("PIPO23","PIPO42","PIPO45","PIPO49")) %>% 
  arrange(SpeciesID, Date, desc(col_share))

colors <- tree_rgb_filter_viz$col_hex



tree_rgb_filter_viz %>% 
  mutate(Treatment = ifelse(Treatment_temp == "Ambient+HW", "Drought with Heatwave", "Drought")) %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  facet_wrap(~SpeciesID + Treatment) +
  #facet_grid(rows = vars(SpeciesID)) +
  scale_x_continuous(breaks = 1:13) +
  ylab("Color Percent") +
  xlab("Weeks") +
  labs(title = "Color change of droughted Ponderosa Pine seedlings over time",
       subtitle = "With and without heatwave (week 7)",
       caption = "Data collected using photographs from August 26 2021 to November 19 2021, as part of Alexandra Lalor's MS project") +
  theme_minimal()
