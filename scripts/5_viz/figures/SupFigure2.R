#supplementary Figure 2 - KM curve adjusted by water stress
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-08-27

#load packages
library(tidyverse)
library(ggplot2)
library(survival) # core survival analysis function
library(survminer) # recommended for visualizing survival curves
library(ggfortify)
library(gridExtra) # for 2 panel graphs
library(RColorBrewer)

#read CSVs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

#add stress adjustments
Phase1_Data_All <- Phase1_Data_All %>% 
  mutate(WeightAdj = Week - Stress_Week_Avg_Weight,
         PorometerAdj = Week - Stress_Week_Avg_Porometer)

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")


################################################################################
################################################################################
# Panel (a) - Ambient temperature survival curves - Weight

Phase1_Data_All_amb_w <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (b)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))))
Phase1_Data_All_amb_w$Legend <- as.factor(Phase1_Data_All_amb_w$Legend)
Phase1_Data_All_amb_w <-
  transform(Phase1_Data_All_amb_w, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (b)", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))
levels(Phase1_Data_All_amb_w$Legend)

#adjustments
Phase1_Data_All_amb_w <- Phase1_Data_All_amb_w %>% 
  filter(WeightAdj > 0)

#filter data
Phase1_Data_All_amb_w <- Phase1_Data_All_amb_w %>% 
  filter(Treatment_temp == "Ambient")

#define colors
#names(myColorsPaired) <- levels(Phase1_Data_All_amb_w$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_amb_w$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_amb_w$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_w <- survfit(Surv(WeightAdj, Dead_Count)~Legend, data = Phase1_Data_All_amb_w)


a <- autoplot(km_species_fit_stress_w) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0 , 36, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  # geom_text(label = "ponderosa pine",
  #           x = 8.5, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "limber pine",
  #           x = 22, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "Pinus ponderosa",
  #           x = 8.5, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 22, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(a)") +
  #labs(caption = "\nFIGURE S3a | Kaplan Meier Survival Curve. \nJuvenile Survival Probability under Drought, Adjusted by Water Stress. \nCurves shows the survival probability of each species under droughted and ambient temperature\ntreatments (n = 20 per species). Weeks show time since water stress for each individual. \nSurvival probability is estimated from observed data. Letters in the legend (a, b, c) show \nspecies which are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 7, face = "italic"),
        strip.text.x = element_text(size = 10),
        plot.tag = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


################################################################################
################################################################################
# Panel (b) - Heatwave temperature survival curves - Weight

Phase1_Data_All_hw_w <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa_HW (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis_HW (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii_HW (b)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))))
Phase1_Data_All_hw_w$Legend <- as.factor(Phase1_Data_All_hw_w$Legend)
Phase1_Data_All_hw_w <-
  transform(Phase1_Data_All_hw_w, Legend = factor(Legend, levels = c("Pinus ponderosa_HW (a)", "Pinus edulis_HW (b)", "Picea engelmannii_HW (b)", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))
levels(Phase1_Data_All_hw_w$Legend)

#adjustments
Phase1_Data_All_hw_w <- Phase1_Data_All_hw_w %>% 
  filter(WeightAdj > 0)

#filter data
Phase1_Data_All_hw_w <- Phase1_Data_All_hw_w %>% 
  filter(Treatment_temp == "Ambient_HW")

#define colors
#names(myColorsPaired) <- levels(Phase1_Data_All_hw_w$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_hw_w$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_hw_w$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_w <- survfit(Surv(WeightAdj, Dead_Count)~Legend, data = Phase1_Data_All_hw_w)


b <- autoplot(km_species_fit_stress_w) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0 , 36, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  # geom_text(label = "ponderosa pine",
  #           x = 8.5, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "limber pine",
  #           x = 24, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "Pinus ponderosa",
  #           x = 8.5, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 24, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(b)") +
  #labs(caption = "\nFIGURE S3b | Kaplan Meier Survival Curve. \nJuvenile Survival Probability under Drought and Heatwave, Adjusted by Water Stress. \nCurves shows the survival probability of each species under droughted and heatwave temperature \ntreatments (n = 20 per species). Weeks show time since water stress for each individual. \nSurvival probability is estimated from observed data. Letters in the legend (a, b, c) show \nspecies which are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 6, face = "italic"),
        strip.text.x = element_text(size = 10),
        plot.tag = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

################################################################################
################################################################################
# 2 panel grid (a and b) for Sup Figure 2

grid.arrange(a, b, nrow=2)
