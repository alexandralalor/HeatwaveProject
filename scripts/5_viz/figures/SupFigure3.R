#supplementary Figure 3 - KM curve adjusted by permanent stomatal closure stress
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
#library(ggfortify)
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
# Panel (a) - Ambient temperature survival curves - Porometer

Phase1_Data_All_amb_p <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (ab)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (ab)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))))
Phase1_Data_All_amb_p$Legend <- as.factor(Phase1_Data_All_amb_p$Legend)
Phase1_Data_All_amb_p <-
  transform(Phase1_Data_All_amb_p, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (ab)", "Picea engelmannii (ab)", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))
levels(Phase1_Data_All_amb_p$Legend)

#adjustments
Phase1_Data_All_amb_p <- Phase1_Data_All_amb_p %>% 
  filter(PorometerAdj > 0)

#filter data
Phase1_Data_All_amb_p <- Phase1_Data_All_amb_p %>% 
  filter(Treatment_temp == "Ambient")

#define colors
#names(myColorsPaired) <- levels(Phase1_Data_All_amb_p$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_amb_p$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_amb_p$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_p <- survfit(Surv(PorometerAdj, Dead_Count)~Legend, data = Phase1_Data_All_amb_p)


a <- autoplot(km_species_fit_stress_p) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0 , 36, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  # geom_text(label = "ponderosa pine",
  #           x = 6.5, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "limber pine",
  #           x = 17.5, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "Pinus ponderosa",
  #           x = 6.5, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 17.3, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(a)") +
  #labs(caption = "\nFIGURE S4a | Kaplan Meier Survival Curve. \nJuvenile Survival Probability under Drought, Adjusted by Permanent Stomatal Closure Stress. \nCurves shows the survival probability of each species under droughted and ambient temperature \ntreatments (n = 10 per species). Weeks show time since permanent stomatal closure stress for each \nmeasured individual. Survival probability is estimated from observed data. Letters in the legend \n(a, b, c) show species which are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
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
# Panel (b) - Heatwave temperature survival curves - Porometer

Phase1_Data_All_hw_p <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa_HW (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis_HW (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii_HW (ab)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))))
Phase1_Data_All_hw_p$Legend <- as.factor(Phase1_Data_All_hw_p$Legend)
Phase1_Data_All_hw_p <-
  transform(Phase1_Data_All_hw_p, Legend = factor(Legend, levels = c("Pinus ponderosa_HW (a)", "Pinus edulis_HW (b)", "Picea engelmannii_HW (ab)", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))
levels(Phase1_Data_All_hw_p$Legend)

#adjustments
Phase1_Data_All_hw_p <- Phase1_Data_All_hw_p %>% 
  filter(PorometerAdj > 0)

#filter data
Phase1_Data_All_hw_p <- Phase1_Data_All_hw_p %>% 
  filter(Treatment_temp == "Ambient_HW")

#define colors
#names(myColorsPaired) <- levels(Phase1_Data_All_hw_p$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_hw_p$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_hw_p$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_p <- survfit(Surv(PorometerAdj, Dead_Count)~Legend, data = Phase1_Data_All_hw_p)


b <- autoplot(km_species_fit_stress_p) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0 , 36, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  # geom_text(label = "ponderosa pine",
  #           x = 6.5, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "limber pine",
  #           x = 17.75, y = 0.05, color = "black", size = 3) +
  # geom_text(label = "Pinus ponderosa",
  #           x = 6.5, y = 0.04, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 17.6, y = 0.04, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(b)") +
  #labs(caption = "\nFIGURE S4b | Kaplan Meier Survival Curve. \nJuvenile Survival Probability under Drought and Heatwave, Adjusted by Permanent Stomatal Closure Stress \nCurves shows the survival probability of each species under droughted and heatwave temperature \ntreatments (n = 10 per species). Weeks show time since permanent stomatal closure stress for each \nmeasured individual. Survival probability is estimated from observed data. Letters in the legend \n(a, b, c) show species which are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
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

