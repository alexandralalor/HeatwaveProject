#kaplan meier survival curve analysis
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-08-27

#load packages
library(tidyverse)
library(survival) # core survival analysis function
library(survminer) # recommended for visualizing survival curves
library(ggplot2)
library(ggfortify)

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

# names(myColorsPaired) <- levels(Phase1_Data_All_w$Legend)
# names(myColorsDark) <- levels(Phase1_Data_All_w$Legend)
# names(myColorsLight) <- levels(Phase1_Data_All_w$Legend)
# custom_colors <- scale_colour_manual(values = myColorsLight)
# custom_colors_fill <- scale_fill_manual(values = myColorsDark)

################################################################################
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

autoplot(km_species_fit) +
  scale_x_continuous(breaks = seq(0 , 36, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 13, y = 0.05, color = "black", size = 4, family = "serif") +
  geom_text(label = "limber pine",
            x = 33, y = 0.05, color = "black", size = 4, family = "serif") +
  labs(color = "", fill = "",
       caption = "FIGURE 1 | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought. \n Curves shows the survival probability of each species under droughted and ambient temperature treatments (n = 20 per species). \n Weeks show time since the start of the experiment, adjusted to account for staggered start times. Survival probability is estimated \n from observed data. Letters in the legend (a, b, c, d) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors + 
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

################################################################################
# KM curve - Ambient Weight

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


autoplot(km_species_fit_stress_w) +
  scale_x_continuous(breaks = seq(0 , 26, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 10, y = 0.05, color = "black", size = 3) +
  geom_text(label = "limber pine",
            x = 23, y = 0.05, color = "black", size = 3) +
  labs(color = "", fill = "",
       caption = "FIGURE S2a | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought, Adjusted by Water Stress \n Curves shows the survival probability of each species under droughted and ambient temperature treatments (n = 20 per species). \n Weeks show time since water stress for each individual. Survival probability is estimated from observed data. \n Letters in the legend (a, b, c) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    size = 10))

################################################################################
# KM curve - Ambient Porometer

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


autoplot(km_species_fit_stress_p) +
  scale_x_continuous(breaks = seq(0 , 20, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 8, y = 0.05, color = "black", size = 3) +
  geom_text(label = "limber pine",
            x = 18, y = 0.75, color = "black", size = 3) +
  labs(color = "", fill = "",
       caption = "FIGURE S3a | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought, Adjusted by Permanent Stomatal Closure Stress \n Curves shows the survival probability of each species under droughted and ambient temperature treatments (n = 10 per species). \n Weeks show time since permanent stomatal closure stress for each measured individual. Survival probability is estimated from observed data. \n Letters in the legend (a, b, c) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    size = 10))


################################################################################
################################################################################
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

autoplot(km_species_fit) +
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
            x = 5, y = 0.5, color = "red", size = 4, family = "serif") +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 13, y = 0.05, color = "black", size = 4, family = "serif") +
  geom_text(label = "limber pine",
            x = 33, y = 0.05, color = "black", size = 4, family = "serif") +
  labs(color = "", fill = "",
       caption = "FIGURE S1a | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought and Heatwave. \n Curves shows the survival probability of each species under droughted and heatwave temperature treatments (n = 20 per species). \n Weeks show time since the start of the experiment, adjusted to account for staggered start times. Survival probability is estimated \n from observed data. Letters in the legend (a, b, c, d) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    size = 10))

################################################################################
# KM curve - Heatwave Weight

Phase1_Data_All_hw_w <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (b)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))))
Phase1_Data_All_hw_w$Legend <- as.factor(Phase1_Data_All_hw_w$Legend)
Phase1_Data_All_hw_w <-
  transform(Phase1_Data_All_hw_w, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (b)", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))
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


autoplot(km_species_fit_stress_w) +
  scale_x_continuous(breaks = seq(0 , 26, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 10, y = 0.05, color = "black", size = 3) +
  geom_text(label = "limber pine",
            x = 24, y = 0.05, color = "black", size = 3) +
  labs(color = "", fill = "",
       caption = "FIGURE S2b | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought and Heatwave, Adjusted by Water Stress \n Curves shows the survival probability of each species under droughted and heatwave temperature treatments (n = 20 per species). \n Weeks show time since water stress for each individual. Survival probability is estimated from observed data. \n Letters in the legend (a, b, c) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    size = 10))

################################################################################
# KM curve - Heatwave Porometer

Phase1_Data_All_hw_p <- Phase1_Data_All %>%
  mutate(Legend = ScientificName) %>% 
  mutate(Legend = ifelse(Legend == "Pinus ponderosa", "Pinus ponderosa (a)",
                         ifelse(Legend == "Pinus edulis", "Pinus edulis (b)",
                                ifelse(Legend == "Picea engelmannii", "Picea engelmannii (ab)",
                                       ifelse(Legend == "Pseudotsuga menziesii", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))))
Phase1_Data_All_hw_p$Legend <- as.factor(Phase1_Data_All_hw_p$Legend)
Phase1_Data_All_hw_p <-
  transform(Phase1_Data_All_hw_p, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus edulis (b)", "Picea engelmannii (ab)", "Pseudotsuga menziesii (b)", "Pinus flexilis (c)")))
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


autoplot(km_species_fit_stress_p) +
  scale_x_continuous(breaks = seq(0 , 20, by = 2)) +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 8, y = 0.05, color = "black", size = 3) +
  geom_text(label = "limber pine",
            x = 17, y = 0.75, color = "black", size = 3) +
  labs(color = "", fill = "",
       caption = "FIGURE S3b | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought and Heatwave, Adjusted by Permanent Stomatal Closure Stress \n Curves shows the survival probability of each species under droughted and heatwave temperature treatments (n = 10 per species). \n Weeks show time since permanent stomatal closure stress for each measured individual. Survival probability is estimated from observed data. \n Letters in the legend (a, b, c) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    size = 10))

################################################################################
# KM curve - Ambient and Heatwave

Phase1_Data_All_amb_hw <- Phase1_Data_All %>%
  mutate(Legend = Heatwave_graph) %>% 
  mutate(Legend = ifelse(Legend == "Ponderosa Pine", "Pinus ponderosa (a)",
                         ifelse(Legend == "Ponderosa Pine_heatwave", "Pinus ponderosa_heatwave (a)",
                         ifelse(Legend == "Pinyon Pine", "Pinus edulis (b)",
                                ifelse(Legend == "Pinyon Pine_heatwave", "Pinus edulis_heatwave (b)",
                                ifelse(Legend == "Engelman Spruce", "Picea engelmannii (c)",
                                       ifelse(Legend == "Engelman Spruce_heatwave", "Picea engelmannii_heatwave (c)",
                                       ifelse(Legend == "Douglas fir", "Pseudotsuga menziesii (c)",
                                              ifelse(Legend == "Douglas fir_heatwave", "Pseudotsuga menziesii_heatwave (c)",
                                                     ifelse(Legend == "Limber Pine", "Pinus flexilis (d)", "Pinus flexilis_heatwave (d)"))))))))))
Phase1_Data_All_amb_hw$Legend <- as.factor(Phase1_Data_All_amb_hw$Legend)
Phase1_Data_All_amb_hw <-
  transform(Phase1_Data_All_amb_hw, Legend = factor(Legend, levels = c("Pinus ponderosa (a)", "Pinus ponderosa_heatwave (a)",
                                                                       "Pinus edulis (b)", "Pinus edulis_heatwave (b)",
                                                                       "Picea engelmannii (c)", "Picea engelmannii_heatwave (c)", 
                                                                       "Pseudotsuga menziesii (c)", "Pseudotsuga menziesii_heatwave (c)",
                                                                       "Pinus flexilis (d)", "Pinus flexilis_heatwave (d)")))
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
            x = 5, y = 0.5, color = "red", size = 4, family = "serif") +
  xlab("Weeks") +
  ylab("Survival Probability") +
  geom_text(label = "ponderosa pine",
            x = 13, y = 0.05, color = "black", size = 4, family = "serif") +
  geom_text(label = "limber pine",
            x = 33, y = 0.05, color = "black", size = 4, family = "serif") +
  labs(color = "", fill = "",
       caption = "FIGURE S1b | Kaplan Meier Survival Curve of Seedling Survival Probability under Drought. \n Curves shows the survival probability of each species under ambient and heatwave temperature treatments (n = 20 per species and treatment). \n Weeks show time since the start of the experiment, adjusted to account for staggered start times. Survival probability is estimated \n from observed data. Letters in the legend (a, b, c, d) show species which are significantly different (p < 0.05).") +
  theme_pubclean() +
  custom_colors +
  custom_colors_fill +
  theme(text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


#################################################################################
heatwave_summary <- read_csv("data_analysis/heatwave_summary.csv")
heatwave_summary_d <- read_csv("data_analysis/anova_Dead_Week.csv")
heatwave_summary_w <- read_csv("data_analysis/anova_Dead_Week_Weight.csv")
heatwave_summary_p <- read_csv("data_analysis/anova_Dead_Week_Porometer.csv")

heatwave_summary_CI <- heatwave_summary %>% 
  filter(Treatment_temp == "Ambient") %>% 
  select(c("Species", "Treatment_temp", "half_width"))

#merge and factor levels
heatwave_summary_d <- merge(heatwave_summary_d, heatwave_summary_CI, by = "Species")
heatwave_summary_d <- 
  transform(heatwave_summary_d, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))
heatwave_summary_d <- heatwave_summary_d %>% 
  group_by(Species) %>% 
  summarize(Mean = mean(Ambient, na.rm = T),
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

Phase1_Data_Avg <- Phase1_Data %>% 
  #filter(SpeciesID != "PIFL16") %>%
  group_by(Species, Treatment_temp, Treatment_water, Week) %>%
  summarize(Dead_Count = sum(Dead_Count))
Phase1_Data_Avg <- Phase1_Data_Avg %>% 
  mutate(Live_Count = 20 - Dead_Count,
         Survivorship = ((Live_Count/20)*100))

Phase1_Data_Avg %>% 
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

Phase1_Data_Avg %>% 
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
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

#factor levels
# Phase1_Data_All <- 
#   transform(Phase1_Data_All, Species = factor(Species, levels = c("PIPO", "PIED", "PSME", "PIEN", "PIFL")))
Phase1_Data_All <- 
  transform(Phase1_Data_All, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))

Dead_Week <- read_csv("data_analysis/Dead_Week.csv")
Dead_Week_Weight <- read_csv("data_analysis/Dead_Week_Weight.csv")
Dead_Week_Porometer <- read_csv("data_analysis/anova_Dead_Week_Porometer.csv")


#histogram
Dead_Week %>% 
  group_by(Species, Treatment_temp) %>% 
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
  group_by(Species, Treatment_temp) %>% 
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





