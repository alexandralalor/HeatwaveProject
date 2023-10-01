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
# KM curve - Ambient and Heatwave

Phase1_Data_All_amb_hw <- Phase1_Data_All %>%
  mutate(Legend = Heatwave_graph) %>% 
  mutate(Legend = ifelse(Legend == "Ponderosa Pine", "Pinus ponderosa (a)",
                         ifelse(Legend == "Ponderosa Pine_heatwave", "Pinus ponderosa_HW (a)",
                                ifelse(Legend == "Pinyon Pine", "Pinus edulis (b)",
                                       ifelse(Legend == "Pinyon Pine_heatwave", "Pinus edulis_HW (b)",
                                              ifelse(Legend == "Engelman Spruce", "Picea engelmannii (c)",
                                                     ifelse(Legend == "Engelman Spruce_heatwave", "Picea engelmannii_HW (c)",
                                                            ifelse(Legend == "Douglas fir", "Pseudotsuga menziesii (c)",
                                                                   ifelse(Legend == "Douglas fir_heatwave", "Pseudotsuga menziesii_HW (c)",
                                                                          ifelse(Legend == "Limber Pine", "Pinus flexilis (d)", "Pinus flexilis_HW (d)"))))))))))
Phase1_Data_All_amb_hw$Legend <- as.factor(Phase1_Data_All_amb_hw$Legend)
Phase1_Data_All_amb_hw <-
  transform(Phase1_Data_All_amb_hw, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus ponderosa_HW (a)",
                                                                       "Pinus edulis (b)", "Pinus edulis_HW (b)",
                                                                       "Picea engelmannii (c)", "Picea engelmannii_HW (c)", 
                                                                       "Pseudotsuga menziesii (c)", "Pseudotsuga menziesii_HW (c)",
                                                                       "Pinus flexilis (d)", "Pinus flexilis_HW (d)")))
levels(Phase1_Data_All_amb_hw$Legend)

#define colors
names(myColorsPaired) <- levels(Phase1_Data_All_amb_hw$Legend)
#names(myColorsDark) <- levels(Phase1_Data_All_amb_hw$Legend)
#names(myColorsLight) <- levels(Phase1_Data_All_amb_hw$Legend)
custom_colors <- scale_colour_manual(values = myColorsPaired)
custom_colors_fill <- scale_fill_manual(values = myColorsPaired)

#Kaplan Meier Survival Curve - separated by treatment
km_treatment_fit <- survfit(Surv(Week, Dead_Count)~Legend, data=Phase1_Data_All_amb_hw)
#summary(km_treatment_fit)


autoplot(km_treatment_fit) +
  # scale_fill_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0 , 36, by = 2)) +
  annotate("rect",
           xmin = 7, xmax = 8,
           ymin = 0, ymax = 1,
           color = "red",
           fill = "red",
           linetype = "dashed",
           size = 0.8,
           alpha = 0.1) +
  geom_text(label = "heatwave",
            x = 5, y = 0.05, color = "red", size = 3.5, family = "serif") +
  xlab("Weeks") +
  ylab("Survival Probability") +
  # geom_text(label = "ponderosa pine",
  #           x = 13, y = 0.05, color = "black", size = 3, family = "serif") +
  # geom_text(label = "limber pine",
  #           x = 33, y = 0.05, color = "black", size = 3, family = "serif") +
  # geom_text(label = "Pinus ponderosa",
  #           x = 12.8, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 33, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(c)") +
  #labs(caption = "\nFIGURE S2b | Kaplan Meier Survival Curve of Juvenile Survival Probability under Drought. \nCurves shows the survival probability of each species under both ambient and heatwave temperature \ntreatments (n = 20 per species and treatment). Weeks show time since the start of the experiment, \nadjusted to account for staggered start times. Survival probability is estimated from observed data \nusing a Kaplan Meier survival function. Letters in the legend (a, b, c, d) show species \nwhich are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
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
