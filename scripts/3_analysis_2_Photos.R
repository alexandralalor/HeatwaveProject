#Heatwave Project Phase 1
#combine color data and porometer data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-11
#Last updated: 2022-09-18

#load packages
library(tidyverse)
library(ggtern) #for rbg2hex
library(colorfindr) #for 3d color plots

#read csv
Phase1_Data_Photos <- read_csv("data_analysis/Phase1_Data_Photos.csv")


################################################################################
# SD
################################################################################

Phase1_Data_Photos <- Phase1_Data_Photos %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD_PercentRed = sd(PercentRed, na.rm = T))


################################################################################
# Samples sizes per week
################################################################################

summary_1 <- Phase1_Data_Photos %>% 
  group_by(Species, Treatment_temp, Treatment_water, Week) %>% 
  summarize(SampleSize_Weekly_Photos = length(unique(SpeciesID)))

Phase1_Data_Photos <- merge(Phase1_Data_Photos, summary_1, all.x = T)

#rearrange columns
Phase1_Data_Photos <- Phase1_Data_Photos[ ,c(1,2,5,3,4,6,29,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]


#save csv
write.csv(Phase1_Data_Photos, "data_analysis/Phase1_Data_Photos.csv", quote = FALSE, row.names = FALSE)

################################################################################
# find average
################################################################################

# it seems like, over time, blue stays the same, red increases, and green decreases
# goal: find average rgb across all individuals per species for each week

Phase1_Data_Photos_Avg <- Phase1_Data_Photos %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
  summarize(SampleSize_Weekly_Photos = mean(SampleSize_Weekly_Photos),
            Dead_Count = sum(Dead_Count),
            red = round(mean(red, na.rm = T)),
            green = round(mean(green, na.rm = T)),
            blue = round(mean(blue, na.rm = T)),
            col_freq = sum(col_freq),
            PercentGreen = mean(PercentGreen, na.rm = T),
            PercentRed = mean(PercentRed, na.rm = T),
            SD_PercentRed = mean(SD_PercentRed, na.rm = T))

#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = Phase1_Data_Photos_Avg$red,
                             g = Phase1_Data_Photos_Avg$green,
                             b = Phase1_Data_Photos_Avg$blue))
colnames(hex) <- "col_hex"
Phase1_Data_Photos_Avg <- cbind(Phase1_Data_Photos_Avg, hex)

#calculate total # pixels and percent of each color, add to summary df
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

#reorder columns
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg[, c(1,2,3,4,8,9,5,6,7,10,11,12,17,13,18,19,14,15,16)]


#separate green and brown
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  mutate(green_only = ifelse(green >= red, green, NA),
         red_only = ifelse(green < red, red, NA))

#percent green / brown
Phase1_Data_Photos_Avg <- Phase1_Data_Photos_Avg %>% 
  group_by(Week, Species, Treatment_temp, Treatment_water) %>% 
  mutate(PercentGreen = mean(PercentGreen, na.rm = T),
         PercentRed = mean(PercentRed, na.rm = T))

#save csv
write.csv(Phase1_Data_Photos_Avg, "data_analysis/Phase1_Data_Photos_Avg.csv", quote=FALSE, row.names = FALSE)


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
Pha
# YES!
# must arrange according to order of graph display
# First, all ambient displayed (week 1 - end). Then all hw (week 1-end)
# So, arrange by treatment_temp, then week. 
Phase1_Photos_graph <- Phase1_Data_Photos_Avg %>% 
  filter(Treatment_water == "Watered") %>% 
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

