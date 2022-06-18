#Heatwave Project Phase 1
#Filter rgb data
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-05-02
#Last updated: 2022-06-18

#load libraries
library(tidyverse)
# library(colorfindr) #for get_colors
# library(sjmisc) #for rotate_df
# library(ggtern) #for rbg2hex
# library(countcolors) #for masking and reducing black colors to one point
# library(tools) #for file naming


#read in tree_rgb_sum_all csv
tree_rgb_sum_all <- read_csv("data_raw/photos/tree_rgb_sum_all.csv")

#check that all dates are in new df
tree_rgb_sum_all %>% 
  summarize(date = unique(Date))


#Filter for col_freq >= 100
tree_rgb_sum_all <- tree_rgb_sum_all %>% 
  filter(col_freq >= 100) %>% 
  arrange(desc(col_share))



#filter to exclude more grey and to most frequent colors

#use this space to test out the parameters below
# tree_rgb_filter %>% 
#   group_by(Date, SpeciesID) %>% 
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

#method to ignore grey colors
#identify in color space where grey colors are, and isolate them
#add all these colors to tree_rgb_ignore dataframe
tree_rgb_ignore <- data.frame(matrix(ncol = 16, nrow = 0))
colnames(tree_rgb_ignore) <- c("Week", "Date", "Species", "SpeciesID", "Treatment_temp", "Treatment_water",
                        "red_class", "green_class", "blue_class",
                        "red", "green", "blue",
                        "col_hex", "col_freq", "col_total", "col_share")

tree_rgb_ignore_1 <- tree_rgb_sum_all %>% 
  group_by(Date, SpeciesID) %>% 
  filter(red < 160) %>% 
  filter((red_class == green_class & red_class == blue_class))

tree_rgb_ignore_2 <-tree_rgb_sum_all %>% 
  group_by(Date, SpeciesID) %>% 
  filter(red_class == "65-96" & green_class == "33-64" & blue_class == "33-64") 

tree_rgb_ignore_3 <- tree_rgb_sum_all %>%
  group_by(Date, SpeciesID) %>%
  filter(red_class == "97-128" & green_class == "65-96" & blue_class == "33-64")

tree_rgb_ignore <- rbind(tree_rgb_ignore_1, tree_rgb_ignore_2, tree_rgb_ignore_3)



#save as a vector
colors_ignore <- tree_rgb_ignore$col_hex

tree_rgb_sum_filter <- subset(tree_rgb_sum_all,!(col_hex %in% colors_ignore))

tree_rgb_sum_filter <- tree_rgb_sum_filter %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1)) %>% 
  arrange(SpeciesID, Date, desc(col_share))

#save csv
write.csv(tree_rgb_sum_filter, "data_raw/final_project/tree_rgb_sum_filter/tree_rgb_sum_filter_all.csv", quote=FALSE, row.names = FALSE)

#save hex colors for visualization
#make sure that tree_rgb_filter is arranged by SpeciesID, Date, col_share
colors <- tree_rgb_sum_filter$col_hex

# write.csv(tree_rgb_filter, "data_raw/final_project/tree_rgb_sum_filter/tree_rgb_sum_filter_all.csv", quote=FALSE, row.names = FALSE)
# tree_rgb_sum_filter <- read.csv("data_raw/final_project/tree_rgb_sum_filter/tree_rgb_sum_filter_all.csv")

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


# tree_rgb_sum_August_26_2021 %>% 
#   if (tree_rgb_sum_August_26_2021$red_class %in% c("129-160","97-128","65-96","33-64","1-32")) {
#     filter(!(red_class == green_class & red_class == blue_class))
#   }
# 
#      
#      
# #  ifelse(red_class %in% c("129-160","97-128","65-96","33-64","1-32"), filter(!(red_class == green_class & red_class == blue_class)),{}) %>% 
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
# 
# 
#   filter(red < 150) %>% 
#   filter(filter(!(red_class == green_class & red_class == blue_class))) %>% 
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
# 
# ?ifelse
#   
# tree_rgb_sum_filter %>% 
#   group_by(Date, SpeciesID) %>% 
#   filter(col_share > 10) %>% 
#   make_palette()
# 
# tree_rgb_sum_filter <- tree_rgb_sum %>% 
#   filter(col_hex != "#707150") %>% 
#   filter(col_hex != "#909170") %>% 
#   filter(col_hex != "#4f5131") %>% 
#   filter(col_hex != "#505130") %>% 
#   filter(col_hex != "#707150") %>% 
#   filter(col_hex != "#8f9270") %>% 
#   filter(col_hex != "#505130") %>% 
#   filter(col_hex != "#707250") %>%
#   filter(col_hex != "#707151") %>%
#   filter(col_hex != "#8e9370") %>%
#   filter(col_hex != "#505230") %>% 
#   filter(col_hex != "#8e9270") %>%
#   filter(col_hex != "#4f5230") %>% 
#   filter(col_hex != "#8f9071")
# 
# 
# #grey colors: r=g=b
# #there seems to be a good threshold of green under 150 t0 180 which filters out
# #most grey, I just have to figure out how to make this into a list
# 
# tree_rgb_sum_filter <- tree_rgb_sum %>% 
#   group_by(Date, SpeciesID) %>% 
#   filter(green < 180) %>% 
#   filter(!(red_class == green_class & red_class == blue_class))
# 
# tree_rgb_sum_filter <- tree_rgb_sum_filter %>% 
#   group_by(Date, SpeciesID, Treatment_temp, Treatment_water) %>% 
#   mutate(col_total = sum(col_freq)) %>% 
#   mutate(col_share = round(100*(col_freq/col_total), digits = 1))
# 
# 
# tree_rgb_sum_filter <- tree_rgb_sum %>% 
#   filter(col_hex != "#525649") %>% 
#   filter(col_hex != "#323629") %>% 
#   filter(col_hex != "#555749") %>% 
#   filter(col_hex != "#545649") %>% 
#   filter(col_hex != "#565749") %>% 
#   filter(col_hex != "#333429") %>% 
#   filter(col_hex != "#555549") %>% 
#   filter(col_hex != "#323429") %>% 
#   filter(col_hex != "#343429") %>% 
#   filter(col_hex != "#333529") %>% 
#   filter(col_hex != "#747669") %>% 
#   filter(col_hex != "#727669") %>% 
#   filter(col_hex != "#737669") %>% 
#   filter(col_hex != "#747769") %>% 
#   filter(col_hex != "#757669") %>% 
#   filter(col_hex != "#545749") %>% 
#   filter(col_hex != "#333629") %>% 
#   filter(col_hex != "#929789") %>%
#   filter(col_hex != "#929689") %>%
#   filter(col_hex != "#343529") %>%
#   filter(col_hex != "#939689") %>%
#   filter(col_hex != "#949689") %>%
#   filter(col_hex != "#939789") %>%
#   filter(col_hex != "#949789") %>% 
#   filter(col_hex != "#505130") %>%
#   filter(col_hex != "#949789") %>% 
#   filter(col_hex != "#707150")
# 
# #remove some columns if you want
# # tree_rgb_sum_2 <- tree_rgb_sum %>% 
# #   select(-c(red_class, green_class, blue_class))
# 
# #colorfindr make_palette
# tree_rgb_sum_filter %>% 
#   make_palette(n = 3)


#try to find a way to arrange the y-axis by most frequent colors to least frequent


