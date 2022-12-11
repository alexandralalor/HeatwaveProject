#Data viz - percent brown
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-12-10
#Last updated: 2022-12-10

#load packages
library(tidyverse)
library(RColorBrewer)

#read csvs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")
#Phase1_Data_PercentBrown  <- read_csv("data_analysis/Phase1_Data_PercentBrown.csv")
#Phase1_Data_PercentBrown_Avg  <- read_csv("data_analysis/Phase1_Data_PercentBrown_Avg.csv")

#filter NAs
Phase1_Data_All_pb <- Phase1_Data_All %>% 
  filter(!is.na(PercentBrown_Est))

#average data 
Phase1_Data_All_pb <- Phase1_Data_All_pb %>%
  group_by(ScientificName, Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(SampleSize_Weekly_PercentBrown = mean(SampleSize_Weekly_PercentBrown),
            Dead_Count = sum(Dead_Count),
            PercentBrown = round(mean(PercentBrown, na.rm = T), digits = 0),
            PercentBrown_Est = round(mean(PercentBrown_Est, na.rm = T), digits = 0),
            SD_PercentBrown = mean(SD_PercentBrown, na.rm = T))

Phase1_Data_All_pb <- Phase1_Data_All_pb %>%
  mutate(Legend = ScientificName)
Phase1_Data_All_pb$Legend <- as.factor(Phase1_Data_All_pb$Legend)
Phase1_Data_All_pb <-
  transform(Phase1_Data_All_pb, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))
levels(Phase1_Data_All_pb$Legend)

#filter data
Phase1_Data_All_pb <- Phase1_Data_All_pb %>% 
  filter(Treatment_temp == "Ambient", Treatment_water == "Drought",
         !is.na(PercentBrown_Est))

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")
names(myColorsPaired) <- levels(Phase1_Data_All_pb$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_pb$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_pb$Legend)

custom_colors <- scale_colour_manual(values = myColorsDark)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)


#Graph
Phase1_Data_All_pb %>% 
  ggplot(aes(x = Week,
             y = PercentBrown_Est,
             color = Legend)) +
  geom_point() +
  scale_y_reverse() +
  geom_line() +
  geom_errorbar(aes(x = Week,
                    ymin = (PercentBrown_Est - SD_PercentBrown),
                    ymax = (PercentBrown_Est + SD_PercentBrown))) +
  # geom_segment(aes(x = 0, xend = 36,
  #                  y = 90, yend = 90),
  #              color = "grey",
  #              linetype = "solid",
  #              size = 0.8) +
  xlim(0, 36) +
  facet_grid(Legend~.) +
  xlab("Weeks") +
  ylab("Percent Brown") +
  labs(caption = "FIGURE 5 | Ocular estimates of percent brown, averaged by each species per week") +
  custom_colors +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif"),
        strip.text.y = element_text(angle = 0),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

