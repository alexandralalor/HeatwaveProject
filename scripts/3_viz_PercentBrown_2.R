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

Phase1_Data_All_pb <- Phase1_Data_All %>%
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

custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)



