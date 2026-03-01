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
Data_All <- read_csv("data/data_analysis/Data_All.csv")
#Data_PercentBrown  <- read_csv("data/data_analysis/Data_PercentBrown.csv")
#Data_PercentBrown_Avg  <- read_csv("data/data_analysis/Data_PercentBrown_Avg.csv")

#filter NAs
Data_All_pb <- Data_All %>% 
  filter(!is.na(PercentBrown_Est)) %>% 
  mutate(PercentGreen_Est = 100 - PercentBrown_Est)

#average data 
Data_All_pb <- Data_All_pb %>%
  group_by(Phase, ScientificName, Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(SampleSize_Weekly_PercentBrown = mean(SampleSize_Weekly_PercentBrown),
            Dead_Count = sum(Dead_Count),
            PercentBrown = round(mean(PercentBrown, na.rm = T), digits = 0),
            PercentBrown_Est = round(mean(PercentBrown_Est, na.rm = T), digits = 0),
            PercentGreen_Est = round(mean(PercentGreen_Est, na.rm = T), digits = 0),
            SD_PercentBrown = mean(SD_PercentBrown, na.rm = T))

#legend
Data_All_pb <- Data_All_pb %>%
  mutate(Legend = ScientificName)
Data_All_pb$Legend <- as.factor(Data_All_pb$Legend)
Data_All_pb <-
  transform(Data_All_pb, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))
levels(Data_All_pb$Legend)

#filter data
Data_All_pb <- Data_All_pb %>% 
  filter(Treatment_water == "Drought")

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")
names(myColorsPaired) <- levels(Data_All_pb$Legend)
names(myColorsDark) <- levels(Data_All_pb$Legend)
names(myColorsLight) <- levels(Data_All_pb$Legend)

custom_colors <- scale_colour_manual(values = myColorsDark)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)


#Graph
Data_All_pb %>% 
  ggplot(aes(x = Week,
             y = PercentGreen_Est,
             color = Legend)) +
  geom_point() +
  #scale_y_reverse() +
  geom_line() +
  geom_errorbar(aes(x = Week,
                    ymin = (PercentGreen_Est - SD_PercentBrown),
                    ymax = (PercentGreen_Est + SD_PercentBrown))) +
  # geom_segment(aes(x = 0, xend = 36,
  #                  y = 90, yend = 90),
  #              color = "grey",
  #              linetype = "solid",
  #              size = 0.8) +
  xlim(0, 36) +
  facet_grid(Legend~Phase) +
  xlab("Weeks") +
  ylab("Percent Green") +
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

