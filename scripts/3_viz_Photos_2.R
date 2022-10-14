#Heatwave Project Phase 1
#play around with visualizations for photo data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-09-11

#load packages
library(tidyverse)
library(ggtern) #for ternary diagram and rgb2hex

#read_csv
Phase1_Data_Photos_Avg <- read_csv("data_analysis/Phase1_Data_Photos_Avg.csv")
#select which trees to visualize

# YES!
# must arrange according to order of graph display
# First, all ambient displayed (week 1 - end). Then all hw (week 1-end)
# So, arrange by treatment_temp, then week. 
Phase1_Data_Photos_graph <- Phase1_Data_Photos_Avg %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Species == "PSME") %>% 
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
                    ymax = (Phase1_Data_Photos_graph$PercentGreen + Phase1_Data_Photos_graph$SD_PercentRed))) +
  scale_x_continuous(breaks = 1:22) +
  ylab("Color Percent") +
  xlab("Weeks") +
  labs(title = "Color - PIED") +
  theme_minimal()

  
################################################################################
# gridded color view, three time points (start, stress, dead)
################################################################################

#read_csv
Phase1_Data_Photos <- read_csv("data_analysis/Phase1_Data_Photos.csv")
Phase1_Data_Photos_Avg <- read_csv("data_analysis/Phase1_Data_Photos_Avg.csv")
Phase1_Data_All_Avg <- read_csv("data_analysis/Phase1_Data_All_Avg.csv")

#add infor to Phase1_Data_Photos
Phase1_Data_Photos_Avg_add <- Phase1_Data_Photos_Avg %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(PercentGreen_Avg = mean(PercentGreen, na.rm = T),
            PercentRed_Avg = mean(PercentRed, na.rm = T))

Phase1_Data_All_Avg_add <- Phase1_Data_All_Avg %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(Dead_Week_Avg = round(mean(Dead_Week_Avg), digits = 1),
            Stress_Week_Avg_Weight = round(mean(Stress_Week_Avg_Weight), digits = 1),
            Stress_Week_Avg_Porometer = round(mean(Stress_Week_Avg_Porometer), digits = 1))

Phase1_Data_Photos_add <- merge(Phase1_Data_Photos_Avg_add, Phase1_Data_All_Avg_add,
                                by = c("Species", "Treatment_temp","Treatment_water", "Week"), all.x = T)

Phase1_Data_Photos <- merge(Phase1_Data_Photos, Phase1_Data_Photos_add, 
                            by = c("Species", "Treatment_temp","Treatment_water", "Week"), all.x = T)

Phase1_Data_Photos <- Phase1_Data_Photos %>% 
  mutate(Dead_Week = round(Dead_Week))


#add info about stress  weeks
Phase1_Data_Photos_graph <- Phase1_Data_Photos %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Week == 1 | Week == Dead_Week | Week == Stress_Week_Weight | Week == Stress_Week_Porometer) %>% 
  arrange(Species, Treatment_temp, Week, green_only, desc(red_only))

Phase1_Data_Photos_graph <- Phase1_Data_Photos_graph %>% 
  mutate(Order = ifelse(Week == 1, "Start", 
                        ifelse(Week == Dead_Week, "Dead", 
                               ifelse(Week == Stress_Week_Weight, "Stress_Weight", "Stress_Porometer")))) %>% 
  mutate(across(Order, factor, levels = c("Start", "Stress_Weight", "Stress_Porometer", "Dead")))


#############
#summarize for bar graphs
Phase1_Data_Photos_graph_bar <- Phase1_Data_Photos_graph %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Species, Treatment_temp, Order, red_class, green_class, blue_class) %>% 
  summarize(red = round(mean(red)),
            green = round(mean(green)),
            blue = round(mean(blue)),
            col_freq = sum(col_freq),
            Dead_Week_Avg = round(mean(Dead_Week_Avg, na.rm = T), digits = 1),
            Stress_Week_Avg_Weight = round(mean(Stress_Week_Avg_Weight, na.rm = T), digits = 1),
            Stress_Week_Avg_Porometer = round(mean(Stress_Week_Avg_Porometer, na.rm = T), digits = 1))


#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = Phase1_Data_Photos_graph_bar$red, 
                             g = Phase1_Data_Photos_graph_bar$green, 
                             b = Phase1_Data_Photos_graph_bar$blue))
colnames(hex) <- "col_hex"
Phase1_Data_Photos_graph_bar <- cbind(Phase1_Data_Photos_graph_bar, hex)
#calculate total # pixels and percent of each color, add to summary df
Phase1_Data_Photos_graph_bar <- Phase1_Data_Photos_graph_bar %>% 
  group_by(Species, Treatment_temp, Order) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))
#separate green and brown
Phase1_Data_Photos_graph_bar <- Phase1_Data_Photos_graph_bar %>% 
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA)) %>%
  arrange(Species, Treatment_temp, Order, green_only, desc(red_only))  
##################################

# # Viz with stress_weight
# Phase1_Data_Photos_graph_w <- Phase1_Data_Photos_graph %>% 
#   filter(Order != "Stress_Porometer")
# # Viz with stress_porometer
# Phase1_Data_Photos_graph_p <- Phase1_Data_Photos_graph %>% 
#   filter(Order != "Stress_Weight")

#save hex colors for visualization
size <- Phase1_Data_Photos_graph$col_share
colors <- Phase1_Data_Photos_graph$col_hex
# colors_w <- Phase1_Data_Photos_graph_w$col_hex
# colors_p <- Phase1_Data_Photos_graph_p$col_hex

#viz with both stress weeks
Phase1_Data_Photos_graph %>% 
  ggplot(aes(x = red,
             y = green,
             color = colors)) +
  geom_point(color = colors,
             size = size*2) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              size = 0.3) +
  facet_grid(Species ~ Order) +
  theme_minimal()

#viz with stress_weight
Phase1_Data_Photos_graph_w %>% 
  ggplot(aes(x = red,
             y = green,
             color = colors_w)) +
  geom_point(color = colors_w,
             size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              size = 0.3) +
  facet_grid(Species ~ Order) +
  theme_minimal()

#viz with stress_porometer
Phase1_Data_Photos_graph_p %>% 
  ggplot(aes(x = red,
             y = green,
             color = colors_p)) +
  geom_point(color = colors_p,
             size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              size = 0.3) +
  facet_grid(Species ~ Order) +
  theme_minimal()


# #just PSME
# Phase1_Data_Photos_graph_PSME <- Phase1_Data_Photos_graph %>% 
#   filter(Species == "PSME")
# colors_PSME <- Phase1_Data_Photos_graph_PSME$col_hex
# size_PSME <- Phase1_Data_Photos_graph_PSME$col_share
# #viz with both stress weeks - PSME
# Phase1_Data_Photos_graph_PSME %>% 
#   ggplot(aes(x = red,
#              y = green,
#              color = colors_PSME)) +
#   geom_point(color = colors_PSME,
#              size = size_PSME*2) +
#   geom_abline(intercept = 0,
#               slope = 1,
#               linetype = "dashed",
#               size = 0.3) +
#   facet_grid(Species ~ Order) +
#   theme_minimal()


################################################################################
# bar graph view, three time points (start, stress, dead)
################################################################################

Phase1_Data_Photos_graph_bar <- Phase1_Data_Photos_graph_bar %>% 
  filter(Treatment_temp == "Ambient")

#set colors
colors_bar <- Phase1_Data_Photos_graph_bar$col_hex

#bar graph viz, all stress weeks
Phase1_Data_Photos_graph_bar %>% 
  ggplot(aes(x = Order,
             y = col_share,
             fill = colors_bar)) +
  geom_col(fill = colors_bar) +
  facet_grid(Species ~ .) +
  ylab("Color Percent") +
  xlab("Weeks") +
  labs(title = "Color") +
  theme_minimal()


################################################################################
# Ternary
################################################################################

Phase1_Data_Photos_graph %>% 
  ggtern(aes(x = red,
             y = green,
             z = blue)) +
  geom_point(color = colors,
             size = 1) +
  facet_grid(Species ~ Order) +
  theme_minimal()
