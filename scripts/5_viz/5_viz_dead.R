#kaplan meier survival curve analysis
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2026-03-01

#load packages
library(tidyverse)
library(ggplot2)
#library(ggtext) #for italics
library(RColorBrewer)
library(survival) # core survival analysis function
library(survminer) # recommended for visualizing survival curves
library(ggfortify)


#read CSVs
Data_All <- read_csv("data/data_analysis/Data_All.csv")

#add stress adjustments
Data_All <- Data_All %>% 
  mutate(WeightAdj = Week - Stress_Week_Avg_Weight,
         PorometerAdj = Week - Stress_Week_Avg_Porometer)

#define custom color scale
myColorsPaired <- c("#6A3D9A", "#CAB2D6", "#FF7F00", "#FDBF6F",  "#33A02C", "#B2DF8A", "#E31A1C", "#FB9A99", "#1F78B4", "#A6CEE3")
myColorsDark <- c("#6A3D9A", "#FF7F00", "#33A02C", "#E31A1C", "#1F78B4")
myColorsLight <- c("#CAB2D6", "#FDBF6F", "#B2DF8A", "#FB9A99", "#A6CEE3")

# names(myColorsPaired) <- levels(Data_All_w$Legend)
# names(myColorsDark) <- levels(Data_All_w$Legend)
# names(myColorsLight) <- levels(Data_All_w$Legend)
# custom_colors <- scale_colour_manual(values = myColorsLight)
# custom_colors_fill <- scale_fill_manual(values = myColorsDark)


################################################################################
# KM curve - Ambient

Data_All_amb <- Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                       ifelse(Legend == "Picea engelmannii", "Picea engelmannii (c)",
                              ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))))
Data_All_amb$Legend <- as.factor(Data_All_amb$Legend)
Data_All_amb <-
  transform(Data_All_amb, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (c)", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))
levels(Data_All_amb$Legend)

#filter data
Data_All_amb <- Data_All_amb %>% 
  filter(Treatment_temp == "Ambient")

#define colors
#names(myColorsPaired) <- levels(Data_All_amb$Legend)
names(myColorsDark) <- levels(Data_All_amb$Legend)
names(myColorsLight) <- levels(Data_All_amb$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#Kaplan Meier Survival Curve - combined
km_species_fit <- survfit(Surv(Week, Dead_Count)~Legend, data = Data_All_amb)

autoplot(km_species_fit) +
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
        legend.text = element_text(size = 7, face = "italic"),
        strip.text.x = element_text(size = 10),
        plot.tag = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


################################################################################
# KM curve - Ambient Weight

Data_All_amb_w <- Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (b)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))))
Data_All_amb_w$Legend <- as.factor(Data_All_amb_w$Legend)
Data_All_amb_w <-
  transform(Data_All_amb_w, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (b)", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))
levels(Data_All_amb_w$Legend)

#adjustments
Data_All_amb_w <- Data_All_amb_w %>% 
  filter(WeightAdj > 0)

#filter data
Data_All_amb_w <- Data_All_amb_w %>% 
  filter(Treatment_temp == "Ambient")

#define colors
#names(myColorsPaired) <- levels(Data_All_amb_w$Legend)
names(myColorsDark) <- levels(Data_All_amb_w$Legend)
names(myColorsLight) <- levels(Data_All_amb_w$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_w <- survfit(Surv(WeightAdj, Dead_Count)~Legend, data = Data_All_amb_w)


autoplot(km_species_fit_stress_w) +
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
# KM curve - Ambient Porometer

Data_All_amb_p <- Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (ab)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (ab)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))))
Data_All_amb_p$Legend <- as.factor(Data_All_amb_p$Legend)
Data_All_amb_p <-
  transform(Data_All_amb_p, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (ab)", "Picea engelmannii (ab)", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))
levels(Data_All_amb_p$Legend)

#adjustments
Data_All_amb_p <- Data_All_amb_p %>% 
  filter(PorometerAdj > 0)

#filter data
Data_All_amb_p <- Data_All_amb_p %>% 
  filter(Treatment_temp == "Ambient")

#define colors
#names(myColorsPaired) <- levels(Data_All_amb_p$Legend)
names(myColorsDark) <- levels(Data_All_amb_p$Legend)
names(myColorsLight) <- levels(Data_All_amb_p$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_p <- survfit(Surv(PorometerAdj, Dead_Count)~Legend, data = Data_All_amb_p)


autoplot(km_species_fit_stress_p) +
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
# KM curve - Heatwave

Data_All_hw <- Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (c)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))))
Data_All_hw$Legend <- as.factor(Data_All_hw$Legend)
Data_All_hw <-
  transform(Data_All_hw, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (c)", "Pseudotsuga menziesii (c)", "Pinus flexilis (d)")))
levels(Data_All_hw$Legend)

#filter data
Data_All_hw <- Data_All_hw %>% 
  filter(Treatment_temp == "Ambient_HW")

#define colors
#names(myColorsPaired) <- levels(Data_All_hw$Legend)
names(myColorsDark) <- levels(Data_All_hw$Legend)
names(myColorsLight) <- levels(Data_All_hw$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#Kaplan Meier Survival Curve - combined
km_species_fit <- survfit(Surv(Week, Dead_Count)~Legend, data = Data_All_hw)

autoplot(km_species_fit) +
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
        legend.text = element_text(size = 7, face = "italic"),
        strip.text.x = element_text(size = 10),
        plot.tag = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

################################################################################
# KM curve - Heatwave Weight

Data_All_hw_w <- Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa_HW (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis_HW (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii_HW (b)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))))
Data_All_hw_w$Legend <- as.factor(Data_All_hw_w$Legend)
Data_All_hw_w <-
  transform(Data_All_hw_w, Legend = factor(Legend, levels = c("Pinus ponderosa_HW (a)", "Pinus edulis_HW (b)", "Picea engelmannii_HW (b)", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))
levels(Data_All_hw_w$Legend)

#adjustments
Data_All_hw_w <- Data_All_hw_w %>% 
  filter(WeightAdj > 0)

#filter data
Data_All_hw_w <- Data_All_hw_w %>% 
  filter(Treatment_temp == "Ambient_HW")

#define colors
#names(myColorsPaired) <- levels(Data_All_hw_w$Legend)
names(myColorsDark) <- levels(Data_All_hw_w$Legend)
names(myColorsLight) <- levels(Data_All_hw_w$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_w <- survfit(Surv(WeightAdj, Dead_Count)~Legend, data = Data_All_hw_w)


autoplot(km_species_fit_stress_w) +
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
# KM curve - Heatwave Porometer

Data_All_hw_p <- Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa_HW (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis_HW (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii_HW (ab)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))))
Data_All_hw_p$Legend <- as.factor(Data_All_hw_p$Legend)
Data_All_hw_p <-
  transform(Data_All_hw_p, Legend = factor(Legend, levels = c("Pinus ponderosa_HW (a)", "Pinus edulis_HW (b)", "Picea engelmannii_HW (ab)", "Pseudotsuga menziesii_HW (b)", "Pinus flexilis_HW (c)")))
levels(Data_All_hw_p$Legend)

#adjustments
Data_All_hw_p <- Data_All_hw_p %>% 
  filter(PorometerAdj > 0)

#filter data
Data_All_hw_p <- Data_All_hw_p %>% 
  filter(Treatment_temp == "Ambient_HW")

#define colors
#names(myColorsPaired) <- levels(Data_All_hw_p$Legend)
names(myColorsDark) <- levels(Data_All_hw_p$Legend)
names(myColorsLight) <- levels(Data_All_hw_p$Legend)
custom_colors <- scale_colour_manual(values = myColorsLight)
custom_colors_fill <- scale_fill_manual(values = myColorsDark)

#KM curve
km_species_fit_stress_p <- survfit(Surv(PorometerAdj, Dead_Count)~Legend, data = Data_All_hw_p)


autoplot(km_species_fit_stress_p) +
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
# KM curve - Ambient and Heatwave

Data_All_amb_hw <- Data_All %>%
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
Data_All_amb_hw$Legend <- as.factor(Data_All_amb_hw$Legend)
Data_All_amb_hw <-
  transform(Data_All_amb_hw, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus ponderosa_HW (a)",
                                                                       "Pinus edulis (b)", "Pinus edulis_HW (b)",
                                                                       "Picea engelmannii (c)", "Picea engelmannii_HW (c)", 
                                                                       "Pseudotsuga menziesii (c)", "Pseudotsuga menziesii_HW (c)",
                                                                       "Pinus flexilis (d)", "Pinus flexilis_HW (d)")))
levels(Data_All_amb_hw$Legend)

#define colors
names(myColorsPaired) <- levels(Data_All_amb_hw$Legend)
#names(myColorsDark) <- levels(Data_All_amb_hw$Legend)
#names(myColorsLight) <- levels(Data_All_amb_hw$Legend)
custom_colors <- scale_colour_manual(values = myColorsPaired)
custom_colors_fill <- scale_fill_manual(values = myColorsPaired)

#Kaplan Meier Survival Curve - separated by treatment
km_treatment_fit <- survfit(Surv(Week, Dead_Count)~Legend, data=Data_All_amb_hw)
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



#################################################################################
heatwave_summary <- read_csv("data/data_analysis/heatwave_summary.csv")
heatwave_summary_d <- read_csv("data/data_analysis/anova_Dead_Week.csv")
heatwave_summary_w <- read_csv("data/data_analysis/anova_Dead_Week_Weight.csv")
heatwave_summary_p <- read_csv("data/data_analysis/anova_Dead_Week_Porometer.csv")

heatwave_summary_CI <- heatwave_summary %>% 
  filter(Treatment_temp == "Ambient") %>% 
  select(c("Species", "Treatment_temp", "half_width"))

#merge and factor levels
heatwave_summary_d <- merge(heatwave_summary_d, heatwave_summary_CI, by = "Species")
heatwave_summary_d <- 
  transform(heatwave_summary_d, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))
heatwave_summary_d <- heatwave_summary_d %>% 
  group_by(Species) %>% 
  summarize(Mean = mean(Baseline, na.rm = T),
            CI_lower_mean = Mean - half_width,
            CI_upper_mean = Mean + half_width)

heatwave_summary_w <- merge(heatwave_summary_w, heatwave_summary_CI, by = "Species")
heatwave_summary_w <- 
  transform(heatwave_summary_w, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))
heatwave_summary_w <- heatwave_summary_w %>% 
  group_by(Species) %>% 
  summarize(Mean = mean(Ambient, na.rm = T),
            CI_lower_mean = Mean - half_width,
            CI_upper_mean = Mean + half_width)

heatwave_summary_p <- merge(heatwave_summary_p, heatwave_summary_CI, by = "Species")
heatwave_summary_p <- 
  transform(heatwave_summary_p, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))
heatwave_summary_p <- heatwave_summary_p %>% 
  group_by(Species) %>% 
  summarize(Mean = mean(Ambient, na.rm = T),
            CI_lower_mean = Mean - half_width,
            CI_upper_mean = Mean + half_width)



heatwave_summary_d %>% 
  ggplot(aes(x = Mean,
             y = Species,
             color = Species)) +
  geom_point() +
  xlim(0,36) +
  #scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  xlab("Mean Death Date, 95% C.I.") +
  ylab("Species") +
  labs(title = "Death Date") +
  geom_errorbar(aes(xmin = CI_lower_mean, 
                    xmax = CI_upper_mean)) +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################

Data_Avg <- Data_All %>% 
  #filter(SpeciesID != "PIFL16") %>%
  group_by(Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(Dead_Count = sum(Dead_Count))
Data_Avg <- Data_Avg %>% 
  mutate(Live_Count = 20 - Dead_Count,
         Survivorship = ((Live_Count/20)*100))

Data_Avg %>% 
  filter(Treatment_water == "Drought", !is.na(Dead_Count)) %>% 
  group_by(Species, Treatment_temp) %>% 
  ggplot(aes(x = Week,
             y = Survivorship,
             color = Treatment_temp)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Species) +
  theme_minimal() +
  scale_color_discrete(direction = -1)

Data_Avg %>% 
  filter(Treatment_water == "Drought", !is.na(Dead_Count)) %>% 
  group_by(Species, Treatment_temp) %>% 
  ggplot(aes(x = Week,
             y = Survivorship,
             color = Species,
             fill = Treatment_temp)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Species) +
  theme_minimal() +
  scale_color_discrete(direction = -1)

###################################################################################
#read csv
Data_All <- read_csv("data/data_analysis/Data_All.csv")

#factor levels
# Data_All <- 
#   transform(Data_All, Species = factor(Species, levels = c("PIPO", "PIED", "PSME", "PIEN", "PIFL")))
# Data_All <- 
#   transform(Data_All, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))

# Data_All <- Data_All %>%
#   mutate(Legend = Heatwave_graph) %>% 
#   mutate(Legend = ifelse(Legend == "Ponderosa Pine", "Pinus ponderosa",
#                          ifelse(Legend == "Ponderosa Pine_heatwave", "Pinus ponderosa_heatwave",
#                                 ifelse(Legend == "Pinyon Pine", "Pinus edulis",
#                                        ifelse(Legend == "Pinyon Pine_heatwave", "Pinus edulis_heatwave",
#                                               ifelse(Legend == "Engelman Spruce", "Picea engelmannii",
#                                                      ifelse(Legend == "Engelman Spruce_heatwave", "Picea engelmannii_heatwave",
#                                                             ifelse(Legend == "Douglas fir", "Pseudotsuga menziesii",
#                                                                    ifelse(Legend == "Douglas fir_heatwave", "Pseudotsuga menziesii_heatwave",
#                                                                           ifelse(Legend == "Limber Pine", "Pinus flexilis", "Pinus flexilis_heatwave"))))))))))
# Data_All$Legend <- as.factor(Data_All$Legend)
# Data_All <-
#   transform(Data_All, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus ponderosa_heatwave",
#                                                                        "Pinus edulis", "Pinus edulis_heatwave",
#                                                                        "Picea engelmannii", "Picea engelmannii_heatwave", 
#                                                                        "Pseudotsuga menziesii", "Pseudotsuga menziesii_heatwave",
#                                                                        "Pinus flexilis", "Pinus flexilis_heatwave")))
# levels(Data_All$Legend)
# 
# #define colors
# names(myColorsPaired) <- levels(Data_All$Legend)
# #names(myColorsDark) <- levels(Data_All$Legend)
# #names(myColorsLight) <- levels(Data_All$Legend)
# custom_colors <- scale_colour_manual(values = myColorsPaired)
# custom_colors_fill <- scale_fill_manual(values = myColorsPaired)


Data_All <- Data_All %>%
  mutate(Legend = ScientificName)
Data_All$Legend <- as.factor(Data_All$Legend)
Data_All <-
  transform(Data_All, Legend = factor(Legend, levels = c("Pinus flexilis",
                                                                "Pseudotsuga menziesii",
                                                                "Picea engelmannii",
                                                                "Pinus edulis",
                                                                "Pinus ponderosa")))
levels(Data_All$Legend)

#boxplot
Data_All %>% 
  group_by(Phase, Species, Treatment_temp) %>% 
  arrange(Dead_Week) %>% 
  ggplot(aes(x = Dead_Week,
             y = Legend,
             fill = Treatment_temp)) +
  geom_boxplot() +
  facet_wrap(~Phase)+
  scale_x_continuous(limits = c(0, 40), breaks = seq(0 , 40, by = 2)) +
  xlab("Weeks to Mortality") +
  annotate("rect",
           xmin = 7, xmax = 8,
           ymin = 0, ymax = 5.25,
           color = "red",
           linetype = "dashed",
           size = 0.4,
           fill = "red",
           alpha = 0.3) +
  geom_text(label = "heatwave",
            x = 7.5, y = 5.5, color = "red", size = 3.5, family = "serif") +
  annotate("segment",
           x = 36, xend = 36,
           y = 0.5, yend = 1.3,
           color = "gray50", linetype = "solid", size = 0.6) +
  geom_text(label = "0.018*",
            x = 38, y = 1, color = "gray50", size = 3.5, family = "serif") +
  annotate("segment",
           x = 36, xend = 36,
           y = 1.6, yend = 2.5,
           color = "gray50", linetype = "solid", size = 0.6) +
  geom_text(label = "0.005**",
            x = 38.3, y = 2, color = "gray50", size = 3.5, family = "serif") +
  labs(fill = "") +
  #labs(caption = "\nFIGURE 2 | Heatwave Differences of Droughted Juveniles.\nHalf the droughted individuals (n = 20 per species) were exposed to a week long heatwave\n(indicated by the vertical red box). Boxplots show median time to mortality (rather than the mean). \nGrey bars on the right hand side show p-values of species which are significantly different in their \nmean time to mortality (two-sample t-test, p < 0.05).") +
  scale_fill_discrete(direction = -1,
                      labels = c("Drought Treatment", "Drought + Heatwave Treatment")) +
  theme_pubclean() +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(face = "italic"),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


  


################################################################################3
#read csv
Dead_Week <- read_csv("data/data_analysis/Dead_Week.csv")
Dead_Week_Weight <- read_csv("data/data_analysis/Dead_Week_Weight.csv")
Dead_Week_Porometer <- read_csv("data/data_analysis/anova_Dead_Week_Porometer.csv")

#histogram
Dead_Week %>% 
  group_by(Phase,Species, Treatment_temp) %>% 
  ggplot(aes(x = Dead_Week,
             fill = Treatment_temp)) +
  geom_histogram(binwidth = 1) +
  scale_fill_discrete(direction = -1) +
  #ylim(0, 12.5) +
  xlim(0, 40) +
  facet_grid(reorder(Species, Dead_Week, mean) ~ Treatment_temp) +
  theme_minimal()


#boxplot
Dead_Week %>% 
  group_by(Phase,Species, Treatment_temp) %>% 
  arrange(Dead_Week) %>% 
  #filter(Species == "PIFL") %>% 
  ggplot(aes(x = Dead_Week,
             y = Species,
             fill = Treatment_temp)) +
  geom_boxplot() +
  xlim(0, 40) +
  ylab("Species") +
  xlab("Dead Week") +
  annotate("rect",
           xmin = 7, xmax = 8,
           ymin = 0, ymax = 6,
           color = "red",
           linetype = "dashed",
           size = 0.4,
           fill = "red",
           alpha = 0.3) +
  theme_minimal() +
  scale_fill_discrete(direction = -1)






