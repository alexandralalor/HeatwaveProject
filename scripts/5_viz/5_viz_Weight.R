#Data viz - weights
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
#Data_Weight  <- read_csv("data/data_analysis/Data_Weight.csv")
#Data_Weight_Avg  <- read_csv("data/data_analysis/Data_Weight_Avg.csv")

Data_All_w <- Data_All %>%
  mutate(Legend = ScientificName)
Data_All_w$Legend <- as.factor(Data_All_w$Legend)
Data_All_w <-
  transform(Data_All_w, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))
levels(Data_All_w$Legend)

#filter data
Data_All_w <- Data_All_w %>% 
  filter(Treatment_water == "Drought",
         !is.na(WaterWeight_Calc))

#make label for inflection point segment
Data_All_w %>% 
  group_by(Legend) %>% 
  summarize(Stress_Week_Avg = mean(Stress_Week_Avg_Weight))

label <- data.frame(x = c(4.39, 4.32, 7.67, 7.82, 11.48),
                   Legend = factor(c("Pinus ponderosa","Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis"), 
                                   levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")
names(myColorsPaired) <- levels(Data_All_w$Legend)
names(myColorsDark) <- levels(Data_All_w$Legend)
names(myColorsLight) <- levels(Data_All_w$Legend)

custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#Graph
Data_All_w %>% 
  filter(Phase == 1) %>% 
  ggplot(aes(x = Week,
             y = WaterWeight_Calc,
             color = SpeciesID)) +
  geom_point() +
  geom_line() +
  geom_segment(data = label,
               aes(x = x, xend = x,
                   y = 0, yend = 500),
               color = "black",
               linetype = "dashed",
               size = 0.8) +
  ylim(0, 500) +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  facet_wrap(~Legend) +
  xlab("Weeks") +
  ylab("Water Weight (g)") +
  #labs(caption = "\nFIGURE 3 | Water Weight of Juveniles under Droughted and Ambient Treatments.\nCurves were calculated using individual weight data (n = 20 per species). Black dotted lines show the\naverage inflection point among all curves to show where concavity changes from concave up to concave down,\nreflecting the time when water availability transitioned from a non-limiting to a limiting resource.") +
  #custom_colors +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = "italic"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

#Graph
Data_All_w %>% 
  filter(Phase == 2) %>% 
  ggplot(aes(x = Week,
             y = WaterWeight_Calc,
             color = SpeciesID)) +
  geom_point() +
  geom_line() +
  ylim(0, 500) +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  facet_wrap(~Legend) +
  xlab("Weeks") +
  ylab("Water Weight (g)") +
  #labs(caption = "\nFIGURE 3 | Water Weight of Juveniles under Droughted and Ambient Treatments.\nCurves were calculated using individual weight data (n = 20 per species). Black dotted lines show the\naverage inflection point among all curves to show where concavity changes from concave up to concave down,\nreflecting the time when water availability transitioned from a non-limiting to a limiting resource.") +
  #custom_colors +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = "italic"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

