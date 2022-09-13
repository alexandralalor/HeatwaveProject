#Heatwave Project Phase 1
#combine color data and poromter data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-09-12

#load packages
library(tidyverse)
library(ggtern) #for rbg2hex

#read_csv
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")
Phase1_Data_Photos <- read_csv("data_QAQC/Phase1_Data_Photos.csv")


#create df to add to photos, to include trees without pictures (retain sample size)
Phase1_Data_add <- Phase1_Data %>% 
  filter(!grepl(".5", Phase1_Data$Week, fixed = TRUE)) %>% 
  filter(Dead == "dead", is.na(Weight_g)) %>% 
  select(c("Week", "Species", "SpeciesID", "Treatment_temp", "Treatment_water", "PorometerSubset",
           "Weight_g", "Porometer", "PercentBrown", "Dead"))

Phase1_Data_Photos_add <- Phase1_Data_Photos %>% 
  group_by(SpeciesID) %>% 
  filter(Week == max(Week))
Phase1_Data_Photos_add <- Phase1_Data_Photos_add %>% 
  select(-c("Week", "PorometerSubset", "Weight_g", "Porometer", "PercentBrown", 
            "Dead"))


#add last week data to Phase1_Data_add
Phase1_Data_Photos_1 <- merge(Phase1_Data_Photos_add, Phase1_Data_add, 
                            by = c("Species", "SpeciesID", "Treatment_temp", "Treatment_water"), all.y = T)

#reorder columns
Phase1_Data_Photos_1 <- Phase1_Data_Photos_1[, c(26,1,2,3,4,27,28,29,30,31,13,14,15,16,17,18,19,20,21,22,23,24,25)]

#combine df
Phase1_Data_Photos <- merge(Phase1_Data_Photos, Phase1_Data_Photos_1, all = T)


#save csv
write.csv(Phase1_Data_Photos, "data_analysis/Phase1_Data_Photos.csv", quote=FALSE, row.names = FALSE)


################################################################################
# find average
################################################################################
Phase1_Data_Photos <- read_csv("data_analysis/Phase1_Data_Photos.csv")

# it seems like, over time, blue stays the same, red increases, and green decreases

# goal: find average rgb across all individuals per species for each week
# script from 0_colors_rgb_sum, with modifications
# seems to work pretty well

Phase1_Data_Photos_Avg <- Phase1_Data_Photos %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
  summarize(red = round(mean(red, na.rm = T)),
            green = round(mean(green, na.rm = T)),
            blue = round(mean(blue, na.rm = T)),
            col_freq = sum(col_freq))

#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = Phase1_Data_Photos_Avg$red,
                             g = Phase1_Data_Photos_Avg$green,
                             b = Phase1_Data_Photos_Avg$blue))
colnames(hex) <- "col_hex"
Phase1_Data_Photos_Avg <- cbind(Phase1_Data_Photos_Avg, hex)
#reorder columns
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg[, c(1,2,3,4,5,6,7,8,9,10,12,11)]

#calculate total # pixels and percent of each color, add to summary df
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

#Separates green and brown
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  group_by(Treatment_temp, Species, Week) %>% 
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA))

# #test separation of green and brown
# Phase1_Data_Photos_Avg %>%
#   filter(Week == 1, red_only > 0) %>% 
#   plot_colors_3d(sample_size = 10000, marker_size = 2.5, color_space = "RGB")

#save csv
write.csv(Phase1_Data_Photos_Avg, "data_analysis/Phase1_Data_Photos_Avg.csv", quote=FALSE, row.names = FALSE)







##########################3
#filter for NAs
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  filter(!is.na(Porometer_Est)) %>% 
  mutate(Stress_Week = ifelse(Treatment_water == "Watered", NA, Stress_Week))

#Avg Porometer
Phase1_Data_Porometer_Avg <- Phase1_Data_Porometer %>%
  group_by(ScientificName, CommonName, Species, Week, Treatment_temp, Treatment_water) %>% 
  summarize(Dead_Count = sum(Dead_Count),
            PercentDead = 100*(Dead_Count/20),
            Porometer_Est = mean(Porometer_Est, na.rm = T),
            Temperature_C = mean(Temperature_C, na.rm = T),
            LeafSensor_PercentRH = mean(LeafSensor_PercentRH, na.rm = T),
            FilterSensor_PercentRH = mean(FilterSensor_PercentRH, na.rm = T),
            SD_Avg = mean(SD, na.rm = T),
            Stress_Week_Avg = round(mean(Stress_Week, na.rm = TRUE), digits = 1)) %>% 
  arrange(Species, Week)

Phase1_Data_Porometer_Avg <- Phase1_Data_Porometer_Avg %>%
  filter(Treatment_water == "Drought")

#################################################################################

#combine df
test <- merge(Phase1_Photos_Avg, Phase1_Data_Porometer_Avg, 
              by = c("Species", "Week", "Treatment_temp"), all.x = T)



#################################################################################
Phase1_Data_Porometer_Avg %>% 
  ggplot(aes(x = Week,
             y = Porometer_Est,
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
           size = 0.4) +
  # annotate("segment",
  #          x = 0, xend = 36,
  #          y = 90, yend = 90,
  #          color = "black",
  #          linetype = "solid",
  #          size = 0.4) +
  geom_rect(data = subset(Porometer_Stress_Label, CommonName == "Ponderosa Pine"),
            aes(xmin = Week, xmax = Week,
                ymin = ymin, ymax = ymax1),
            color = "black",
            linetype = "dashed",
            size = 0.4) +
  geom_rect(data = subset(Porometer_Stress_Label, CommonName == "Pinyon Pine"),
            aes(xmin = Week, xmax = Week,
                ymin = ymin, ymax = ymax1),
            color = "black",
            linetype = "dashed",
            size = 0.4) +
  geom_rect(data = subset(Porometer_Stress_Label, CommonName == "Limber Pine"),
            aes(xmin = Week, xmax = Week,
                ymin = ymin, ymax = ymax1),
            color = "black",
            linetype = "dashed",
            size = 0.4) +
  geom_rect(data = subset(Porometer_Stress_Label, CommonName == "Douglas fir"),
            aes(xmin = Week, xmax = Week,
                ymin = ymin, ymax = ymax1),
            color = "black",
            linetype = "dashed",
            size = 0.4) +
  geom_rect(data = subset(Porometer_Stress_Label, CommonName == "Engelman Spruce"),
            aes(xmin = Week, xmax = Week,
                ymin = ymin, ymax = ymax1),
            color = "black",
            linetype = "dashed",
            size = 0.4) +
  geom_errorbar(aes(x = Phase1_Data_Porometer_Avg$Week,
                    ymin = (Phase1_Data_Porometer_Avg$Porometer_Est - Phase1_Data_Porometer_Avg$SD_Avg),
                    ymax = (Phase1_Data_Porometer_Avg$Porometer_Est + Phase1_Data_Porometer_Avg$SD_Avg))) +
  geom_text(label = "Heatwave",
            x = 12, y = 300, color = "red", size = 3) +
  facet_wrap(~CommonName) +
  xlab("Week") +
  ylab("Stomatal Conductance") +
  labs(title = "Stomatal Conductance of Droughted Trees") +
  theme_minimal() +
  scale_color_discrete(direction = -1) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 5),
        legend.title = element_blank())

################################################################################

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

