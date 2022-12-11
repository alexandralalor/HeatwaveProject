#Data viz - porometer
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
#Phase1_Data_Weight  <- read_csv("data_analysis/Phase1_Data_Weight.csv")
#Phase1_Data_Weight_Avg  <- read_csv("data_analysis/Phase1_Data_Weight_Avg.csv")

Phase1_Data_All_w <- Phase1_Data_All %>%
  mutate(Legend = ScientificName)
Phase1_Data_All_w$Legend <- as.factor(Phase1_Data_All_w$Legend)
Phase1_Data_All_w <-
  transform(Phase1_Data_All_w, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))
levels(Phase1_Data_All_w$Legend)

#filter data
Phase1_Data_All_w <- Phase1_Data_All_w %>% 
  filter(Treatment_temp == "Ambient", Treatment_water == "Drought",
         !is.na(WaterWeight_Calc))

#make label for inflection point segment
Phase1_Data_All_w %>% 
  group_by(Legend) %>% 
  summarize(Stress_Week_Avg = mean(Stress_Week_Avg_Weight))

label <- data.frame(x = c(4.39, 4.32, 7.67, 7.82, 11.48),
                    Legend = factor(c("Pinus ponderosa","Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis"), 
                                    levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")
names(myColorsPaired) <- levels(Phase1_Data_All_w$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_w$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_w$Legend)

custom_colors <- scale_colour_manual(values = myColorsLight)


#Graph
Phase1_Data_All_w %>% 
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
  xlim(0, 36) +
  facet_wrap(~Legend) +
  xlab("Weeks") +
  ylab("Water Weight (g)") +
  labs(caption = "FIGURE 3 | Water loss curves of juveniles under droughted and ambient treatments, grouped by species. \n Curves were calculated using individual weight data (n = 20 per species). Black dotted lines show the average inflection point \n among all curves to show where concavity changes from concave up to concave down, reflecting the time when water availability \n transitioned from a non-limiting to a limiting resource.") +
  #custom_colors +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))
