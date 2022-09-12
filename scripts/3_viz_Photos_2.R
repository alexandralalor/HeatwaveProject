#Heatwave Project Phase 1
#play around with visualizations for photo data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-09-11

#load packages
library(tidyverse)
library(colorfindr) #getcolors
library(ggtern) #for rbg2hex

#read_csv
Phase1_Photos <- read_csv("data_QAQC/Phase1_Photos.csv")

# it seems like, over time, blue stays the same, red increases, and green decreases

# goal: find average rgb across all individuals per species for each week
# script from 0_colors_rgb_sum, with modifications
# seems to work pretty well

Phase1_Photos_Avg <- Phase1_Photos %>% 
  group_by(Week, Date, Species, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
  summarize(red = round(mean(red)),
            green = round(mean(green)),
            blue = round(mean(blue)),
            col_freq = sum(col_freq))

#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = Phase1_Photos_Avg$red,
                             g = Phase1_Photos_Avg$green,
                             b = Phase1_Photos_Avg$blue))
colnames(hex) <- "col_hex"
Phase1_Photos_Avg <- cbind(Phase1_Photos_Avg, hex)
#reorder columns
Phase1_Photos_Avg <- Phase1_Photos_Avg[, c(1,2,3,4,5,6,7,8,9,10,11,13,12)]

#calculate total # pixels and percent of each color, add to summary df
Phase1_Photos_Avg <- Phase1_Photos_Avg %>% 
  group_by(Week, Date, Species, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

#Separates green and brown
Phase1_Photos_Avg <- Phase1_Photos_Avg %>% 
  group_by(Treatment_temp, Species, Week) %>% 
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA))

# #test separation of green and brown
# Phase1_Photos_Avg %>%
#   filter(Week == 1, red_only > 0) %>% 
#   plot_colors_3d(sample_size = 10000, marker_size = 2.5, color_space = "RGB")

#save csv
write.csv(Phase1_Photos_Avg, "data_QAQC/Phase1_Photos_Avg.csv", quote=FALSE, row.names = FALSE)

################################################################################

#read_csv
Phase1_Data_Photos_Avg <- read_csv("data_analysis/Phase1_Data_Photos_Avg.csv")
#select which trees to visualize

# YES!
# must arrange according to order of graph display
# First, all ambient displayed (week 1 - end). Then all hw (week 1-end)
# So, arrange by treatment_temp, then week. 
Phase1_Photos_graph <- Phase1_Photos_Avg %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Species == "PIED") %>% 
  arrange(Species, Treatment_temp, Week, green_only, desc(red_only))

#save hex colors for visualization
colors <- Phase1_Photos_graph$col_hex

#graph!
#try to find a way to arrange the y-axis by most frequent colors to least frequent
Phase1_Photos_graph %>% 
  #mutate(Treatment = ifelse(Treatment_temp == "Ambient+HW", "Drought with Heatwave", "Drought")) %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  facet_wrap(~Treatment_temp) +
  facet_grid(rows = vars(Species),
             cols = vars(Treatment_temp)) +
  annotate("rect",
           xmin = 6.5, xmax = 7.5,
           ymin = 0, ymax = 100,
           color = "red",
           alpha = 0,
           size = 0.3) +
  scale_x_continuous(breaks = 1:21) +
  ylab("Color Percent") +
  xlab("Weeks") +
  labs(title = "Color - PIED") +
  theme_minimal()

  