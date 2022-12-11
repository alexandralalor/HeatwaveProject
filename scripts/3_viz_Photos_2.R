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
library(ggplot2)

#read_csv
#Phase1_Data_Photos <- read_csv("data_analysis/Phase1_Data_Photos.csv")
Phase1_Data_Photos_Avg <- read_csv("data_analysis/Phase1_Data_Photos_Avg.csv")
Phase1_Data_PercentBrown_Avg  <- read_csv("data_analysis/Phase1_Data_PercentBrown_Avg.csv")

#condense data to graph
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  filter(Treatment_water == "Drought", Treatment_temp == "Ambient") %>% 
  mutate(PercentRed = round(PercentRed, digits = 1)) %>% 
  mutate(label = paste0(PercentRed, " %"))

#combine Photos_Avg and PercentBrown_Avg
Phase1_Data_Photos_PercentBrown <- merge(Phase1_Data_Photos_Avg, Phase1_Data_PercentBrown_Avg, all.x = T)

# must arrange according to order of graph display
# First, all ambient displayed (week 1 - end). Then all hw (week 1-end)
# So, arrange by treatment_temp, then week. 
#legend
Phase1_Data_Photos_graph <- Phase1_Data_Photos_PercentBrown %>%
  mutate(ScientificName = ifelse(Species == "PIPO", "Pinus ponderosa",
                                 ifelse(Species == "PIED", "Pinus edulis",
                                        ifelse(Species == "PIEN", "Picea engelmannii",
                                               ifelse(Species == "PSME", "Pseudotsuga menziesii", "Pinus flexilis")))))
Phase1_Data_Photos_graph <- Phase1_Data_Photos_graph %>%
  mutate(Legend = ScientificName)
Phase1_Data_Photos_graph$Legend <- as.factor(Phase1_Data_Photos_graph$Legend)
Phase1_Data_Photos_graph <-
  transform(Phase1_Data_Photos_graph, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis"))) %>% 
  arrange(Legend, Treatment_temp, Week, green_only, desc(red_only))
levels(Phase1_Data_Photos_graph$Legend)

# #PSME
# Phase1_Data_Photos_graph <- Phase1_Data_Photos_Avg %>% 
#   filter(Treatment_water == "Drought", Treatment_temp == "Ambient", Species == "PSME") %>% 
#   mutate(PercentRed = round(PercentRed, digits = 1)) %>% 
#   mutate(label = paste0(PercentRed, " %")) %>% 
#   arrange(Species, Treatment_temp, Week, green_only, desc(red_only))


################################################################################
# week 1 to dead, all colors
################################################################################

#save hex colors for visualization
#try to find a way to arrange the y-axis by most frequent colors to least frequent
colors <- Phase1_Data_Photos_graph$col_hex
#dev.off()

#graph!
Phase1_Data_Photos_graph %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  geom_point(data = Phase1_Data_Photos_graph, 
             aes(x = Phase1_Data_Photos_graph$Week, 
                 y = Phase1_Data_Photos_graph$PercentGreen_Est,
                 color = Legend)) +
  scale_y_continuous("Percent Green (Photos)", 
                     sec.axis = sec_axis(~ . , name = "Percent Green (Estimates)")) +
  facet_grid(Legend ~ .) +
  geom_errorbar(aes(x = Week,
                    ymin = (PercentGreen - SD_PercentRed),
                    ymax = (PercentGreen + SD_PercentRed))) +
  # geom_text(label = Phase1_Data_Photos_graph$label,
  #           y = 80,
  #           size = 2) +
  # scale_x_continuous(breaks = 1:36) +
  ylab("Percent Green") +
  xlab("Weeks") +
  labs(title = "Colors over Time") +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif"),
        strip.text.y = element_text(angle = 0),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


  # theme(panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       plot.background = element_rect(fill = "black"),
  #       panel.background = element_rect(fill = 'black'))

  
################################################################################
# gridded color view, three time points (start, stress, dead)
################################################################################


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
  geom_point(color = colors) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              size = 0.3) +
  #facet_grid(Species ~ Order) +
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

################################################################################
# bar graph view, three time points (start, stress, dead)
################################################################################


#add info about stress  weeks
Phase1_Data_Photos_graph_bar_1 <- Phase1_Data_Photos %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Week == 1 | Week == Dead_Week | Week == Stress_Week_Weight | Week == Stress_Week_Porometer) %>% 
  arrange(Species, Treatment_temp, Week, green_only, desc(red_only))

Phase1_Data_Photos_graph_bar_1 <- Phase1_Data_Photos_graph_bar_1 %>% 
  mutate(Order = ifelse(Week == 1, "Start", 
                        ifelse(Week == Dead_Week, "Dead", 
                               ifelse(Week == Stress_Week_Weight, "Stress_Weight", "Stress_Porometer")))) %>% 
  mutate(across(Order, factor, levels = c("Start", "Stress_Weight", "Stress_Porometer", "Dead")))


#####################
#summarize for bar graphs

#summary data
Phase1_Data_Photos_graph_bar <- Phase1_Data_Photos_graph_bar_1 %>% 
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



##############
# add info about greens and reds

# add graphing info
Phase1_Data_Photos_graph_bar <- Phase1_Data_Photos_graph_bar %>%
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA)) %>%
  mutate(graph_week = ifelse(Order == "Start", 1,
                             ifelse(Order == "Stress_Weight", Stress_Week_Avg_Weight,
                                    ifelse(Order == "Stress_Porometer", Stress_Week_Avg_Porometer, Dead_Week_Avg)))) %>%
  arrange(Species, Treatment_temp, Order, green_only, desc(red_only))

#find percent green
Phase1_Data_Photos_green <- Phase1_Data_Photos_graph_bar %>% 
  filter(green_only > 0) %>% 
  group_by(Species, Treatment_temp, Order) %>% 
  mutate(PercentGreen = sum(col_share))

#find percent brown (red)
Phase1_Data_Photos_red <- Phase1_Data_Photos_graph_bar %>% 
  filter(red_only > 0) %>% 
  group_by(Species, Treatment_temp, Order) %>% 
  mutate(PercentRed = sum(col_share))

#merge
Phase1_Data_Photos_graph_bar_test <- merge(Phase1_Data_Photos_green, Phase1_Data_Photos_red, all = T)

#fill in NA values
sum <- Phase1_Data_Photos_graph_bar_test %>% 
  group_by(Species, Treatment_temp, Order) %>%  
  summarize(PercentGreen = mean(PercentGreen, na.rm = T),
            PercentRed = mean(PercentRed, na.rm = T))

sd <- Phase1_Data_Photos_graph_bar_1 %>% 
  group_by(Species, Treatment_temp, Order) %>%
  summarise(SD_PercentRed = mean(SD_PercentRed))

sum_sd <- merge(sum, sd, by = c("Species","Treatment_temp", "Order"), all = T)

#merge
Phase1_Data_Photos_graph_bar <- merge(Phase1_Data_Photos_graph_bar, sum_sd, by = c("Species","Treatment_temp", "Order"), all.x = T)

#################################
#some statistics
sd_stats <- sd %>% 
  filter(Treatment_temp == "Ambient") %>% 
  group_by(Order) %>% 
  summarize(SD_PercentRed = mean(SD_PercentRed))
sum_stats <- sum %>% 
  filter(Treatment_temp == "Ambient") %>% 
  group_by(Order) %>% 
  summarize(mean_PercentRed = mean(PercentRed))
#########################################

# graphing
Phase1_Data_Photos_graph_b <- Phase1_Data_Photos_graph_bar %>% 
  filter(Treatment_temp == "Ambient") %>% 
  mutate(label = paste0(PercentRed, " %Brown"),
         graph_week =  paste0("Avg Week = ", graph_week))

Phase1_Data_Photos_graph_b <- transform(Phase1_Data_Photos_graph_b,
                                      Species = factor(Species, levels = c("PIPO", "PIED", "PSME", "PIEN", "PIFL"))) %>% 
  arrange(Species, Treatment_temp, Order, green_only, desc(red_only))


  
#set colors
colors_bar <- Phase1_Data_Photos_graph_b$col_hex

Phase1_Data_Photos_graph_b %>% 
  ggplot(aes(x = Order,
             y = col_share,
             fill = colors_bar)) +
  geom_col(fill = colors_bar) +
  facet_grid(Species ~ .) +
  geom_text(label = Phase1_Data_Photos_graph_b$label,
            y = 80,
            size = 3) +
  geom_text(label = Phase1_Data_Photos_graph_b$graph_week,
            y = 10,
            size = 2.5) +
  # geom_errorbar(aes(x = Order,
  #                   ymin = (PercentGreen - SD_PercentRed),
  #                   ymax = (PercentGreen + SD_PercentRed))) +
  scale_x_discrete(labels = c ('Start', 'Water\nStress', 'Stomatal\nClosure\nStress', 'Dead')) +
  scale_y_continuous(breaks = c (0, 25, 50, 75, 100)) +
  ylab("Percent Green") +
  xlab("Week") +
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
