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
#Phase1_Data_Porometer  <- read_csv("data_analysis/Phase1_Data_Porometer.csv")
#Phase1_Data_Porometer_Avg  <- read_csv("data_analysis/Phase1_Data_Porometer_Avg.csv")

Phase1_Data_All_p <- Phase1_Data_All %>%
  mutate(Legend = ScientificName)
Phase1_Data_All_p$Legend <- as.factor(Phase1_Data_All_p$Legend)
Phase1_Data_All_p <-
  transform(Phase1_Data_All_p, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))
levels(Phase1_Data_All_p$Legend)

#filter data
Phase1_Data_All_p <- Phase1_Data_All_p %>% 
  filter(Treatment_temp == "Ambient", Treatment_water == "Drought",
         !is.na(Porometer_Est))

#make label for permanent stomatal closure segment
Phase1_Data_All_p %>% 
  group_by(Legend) %>% 
  summarize(Stress_Week_Avg = mean(Stress_Week_Avg_Porometer))

label <- data.frame(x = c(6.0, 8.1, 12.5, 11.5, 16.4),
                    Legend = factor(c("Pinus ponderosa","Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis"), 
                                    levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")
names(myColorsPaired) <- levels(Phase1_Data_All_p$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_p$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_p$Legend)

custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#Graph
Phase1_Data_All_p %>% 
  ggplot(aes(x
             = Week,
             y = Porometer_Est,
             color = SpeciesID)) +
  geom_point() +
  geom_line() +
  geom_segment(data = label,
               aes(x = x, xend = x,
                   y = 0, yend = 500),
               color = "black",
               linetype = "dashed",
               size = 0.8) +
  geom_segment(aes(x = 0, xend = 36,
                   y = 90, yend = 90),
               color = "grey",
               linetype = "solid",
               size = 0.8) +
  ylim(0, 500) +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  facet_wrap(~Legend) +
  xlab("Weeks") +
  ylab(bquote("Stomatal Conductance  "~(mmol/m^2*s))) +
  #labs(caption = "\nFIGURE 4 | Stomatal Conductance of Juveniles under Droughted and Ambient Treatments.\nThe black dotted line shows the average time of permanent stomatal closure (n = 10 per species), when \nmeasurements were consistently at or below a minimum value of 90 mmol m-2s-1 (solid gray horizonal line).") +
  #custom_colors +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif", size = 10),
        #axis.title.y = element_markdown(),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 10, face = "italic"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    size = 10))



