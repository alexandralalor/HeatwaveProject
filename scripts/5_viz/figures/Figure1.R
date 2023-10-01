#kaplan meier survival curve - Figure 1
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-30
#Last updated: 2022-10-01

#load packages
library(tidyverse)
library(ggplot2)
library(survival) # core survival analysis function
library(survminer) # recommended for visualizing survival curves, publication quality
library(gridExtra) # for 2 panel graphs
#library(RColorBrewer)


################################################################################

# Panel (a) - Ambient temperature survival curves

#read CSVs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")


# KM curve - Ambient

Phase1_Data_All_amb <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (c)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))))
Phase1_Data_All_amb$Legend <- as.factor(Phase1_Data_All_amb$Legend)
Phase1_Data_All_amb <-
  transform(Phase1_Data_All_amb, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (c)", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))
levels(Phase1_Data_All_amb$Legend)

#filter data
Phase1_Data_All_amb <- Phase1_Data_All_amb %>% 
  filter(Treatment_temp == "Ambient")

#define colors
#names(myColorsPaired) <- levels(Phase1_Data_All_amb$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_amb$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_amb$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#Kaplan Meier Survival Curve - combined
km_species_fit <- survfit(Surv(Week, Dead_Count)~Legend, data = Phase1_Data_All_amb)

KM_amb <- autoplot(km_species_fit) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0 , 36, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  # geom_text(label = "ponderosa pine",
  #           x = 12, y = 0.05, color = "black", size = 3, family = "serif") +
  # geom_text(label = "limber pine",
  #           x = 33, y = 0.05, color = "black", size = 3, family = "serif") +
  # geom_text(label = "Pinus ponderosa",
  #           x = 12, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 33, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(a)") +
  #labs(caption = "\nFIGURE 1 | Kaplan Meier Survival Curve of Juvenile Survival Probability under Drought.\nCurves shows the survival probability of each species under droughted and ambient temperature\ntreatments (n = 20 per species). Weeks show time since the start of the experiment, adjusted to\naccount for staggered start times. Survival probability is estimated from observed data\nusing a Kaplan Meier survival function. Letters in the legend (a, b, c, d) show species\nwhich are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
  theme_pubclean() +
  custom_colors + 
  custom_colors_fill +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 8, face = "italic"),
        strip.text.x = element_text(size = 10),
        plot.tag = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


################################################################################
################################################################################

# Panel (b) - Heatwave temperature survival curves

# KM curve - Heatwave

Phase1_Data_All_hw <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (c)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))))
Phase1_Data_All_hw$Legend <- as.factor(Phase1_Data_All_hw$Legend)
Phase1_Data_All_hw <-
  transform(Phase1_Data_All_hw, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (c)", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))
levels(Phase1_Data_All_hw$Legend)

#filter data
Phase1_Data_All_hw <- Phase1_Data_All_hw %>% 
  filter(Treatment_temp == "Ambient_HW")

#define colors
#names(myColorsPaired) <- levels(Phase1_Data_All_hw$Legend)
names(myColorsDark) <- levels(Phase1_Data_All_hw$Legend)
names(myColorsLight) <- levels(Phase1_Data_All_hw$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#Kaplan Meier Survival Curve - combined
km_species_fit <- survfit(Surv(Week, Dead_Count)~Legend, data = Phase1_Data_All_hw)

KM_HW <- autoplot(km_species_fit) +
  # scale_fill_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Paired") +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0 , 36, by = 2)) +
  #ylim(0,1) +
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
  #           x = 13, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  # geom_text(label = "Pinus flexilis",
  #           x = 33, y = 0.05, color = "black", size = 3, family = "serif", fontface = "italic") +
  labs(color = "", fill = "", tag = "(b)") +
  #labs(caption = "\nFIGURE S2a | Kaplan Meier Survival Curve of Juvenile Survival Probability under Drought and Heatwave. \nCurves shows the survival probability of each species under droughted and heatwave temperature \ntreatments (n = 20 per species). Weeks show time since the start of the experiment, adjusted to \naccount for staggered start times. Survival probability is estimated from observed data \nusing a Kaplan Meier survival function. Letters in the legend (a, b, c, d) show species \nwhich are significantly different (post-hoc Tukey HSD test, p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 8, face = "italic"),
        strip.text.x = element_text(size = 10),
        plot.tag = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

################################################################################
################################################################################
# 2 panel grid (a and b) for Figure 1

grid.arrange(KM_amb, KM_HW, nrow=2)
