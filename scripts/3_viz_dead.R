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

# Phase1_Data_All <- 
#   transform(Phase1_Data_All, Species = factor(Species, levels = c("PIFL", "PIEN", "PSME", "PIED", "PIPO")))
Phase1_Data_All <- 
  transform(Phase1_Data_All, CommonName = factor(CommonName, levels = c("Ponderosa Pine", "Pinyon Pine", "Douglas fir", "Engelman Spruce", "Limber Pine")))

# #check out data
# glimpse(Phase1_Data)
# 
# #convert variables
# Phase1_Data$Phase <- as.factor(Phase1_Data$Phase)
# Phase1_Data$Chamber <- as.factor(Phase1_Data$Chamber)
# Phase1_Data$ScientificName <- as.factor(Phase1_Data$ScientificName)
# Phase1_Data$CommonName <- as.factor(Phase1_Data$CommonName)
# Phase1_Data$Species <- as.factor(Phase1_Data$Species)
# Phase1_Data$Treatment_temp <- as.factor(Phase1_Data$Treatment_temp)
# Phase1_Data$Treatment_water <- as.factor(Phase1_Data$Treatment_water)
# Phase1_Data$PorometerSubset <- as.factor(Phase1_Data$PorometerSubset)
# Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)
# Phase1_Data$Heatwave_graph <- as.factor(Phase1_Data$Heatwave_graph)
# Phase1_Data$Heatwave <- as.factor(Phase1_Data$Heatwave)

#filter data
Phase1_Data_All <- Phase1_Data_All %>% 
  filter(Treatment_temp == "Ambient")

#Kaplan Meier Survival Curve - combined
km <- with(Phase1_Data_All, Surv(Week, Dead_Count))
km_species_fit <- survfit(Surv(Week, Dead_Count)~CommonName, data = Phase1_Data_All)

autoplot(km_species_fit) +
  # scale_fill_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  # annotate("segment",
  #          x = 7, xend = 7,
  #          y = 0, yend = 1,
  #          color = "red",
  #          linetype = "dashed",
  #          size = 0.8) +
  xlab("Weeks") +
  #ylab("Survivorship") +
  geom_text(label = "Ponderosa Pine",
            x = 13, y = 0.05, color = "black", size = 4) +
  geom_text(label = "Limber Pine",
            x = 33, y = 0.05, color = "black", size = 4) +
  labs(title = "Seedling Survival Probability under Drought\nKaplan Meier Survival Curve",
       color = "Species:", fill = "Species:") +
  theme_pubclean()


#Kaplan Meier Survival Curve- STRESS WEEK

km_stress_w <- with(Phase1_Data_All, Surv(Stress_to_Dead_Weight, Dead_Count))
km_species_fit_stress_w <- survfit(Surv(Stress_to_Dead_Weight, Dead_Count)~CommonName, data = Phase1_Data_All)

km_stress_w <- with(Phase1_Data_All, Surv(Stress_Week_Avg_Weight, Dead_Count))
km_species_fit_stress_w <- survfit(Surv(Stress_Week_Avg_Weight, Dead_Count)~CommonName, data = Phase1_Data_All)

km_stress_p <- with(Phase1_Data_All, Surv(Stress_to_Dead_Porometer, Dead_Count))
km_species_fit_stress_p <- survfit(Surv(Stress_to_Dead_Porometer, Dead_Count)~CommonName, data = Phase1_Data_All)


autoplot(km_species_fit_stress_w) +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  xlab("Weeks") +
  #ylab("Survivorship") +
  geom_text(label = "Ponderosa Pine",
            x = 13, y = 0.05, color = "black", size = 4) +
  geom_text(label = "Limber Pine",
            x = 33, y = 0.05, color = "black", size = 4) +
  labs(title = "Seedling Survival Probability under Drought\nKaplan Meier Survival Curve",
       color = "Species:", fill = "Species:") +
  theme_pubclean()


################################################################################

#Kaplan Meier Survival Curve - separated by treatment
km_treatment_fit <- survfit(Surv(Week, Dead_Count)~Species, data=Phase1_Data_All)
summary(km_treatment_fit)

autoplot(km_treatment_fit) +
  # scale_fill_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  # annotate("segment",
  #          x = 7, xend = 7,
  #          y = 0, yend = 1,
  #          color = "red",
  #          linetype = "dashed",
  #          size = 0.8) +
  # geom_text(label = "Heatwave",
  #           x = 5, y = 0.78, color = "red", size = 3) +
  xlab("Weeks") +
  #ylab("Survivorship") +
  labs(title = "Kaplan Meier Survival Curve: Heatwave Effects by Species",
       color = "Species_Treatment", fill = "Species_Treatment") +
  theme_bw()



#Kaplan Meier Survival Curve - separated by treatment - STRESS WEEK

Phase1_Data_Weight <- read.csv("data_analysis/Phase1_Data_Weight.csv")

km_treatment_fit_stress <- survfit(Surv(Stress_Week_Start, Dead_Count)~Heatwave_graph, data=Phase1_Data_Weight)

km_treatment_fit_stress <- survfit(Surv(Stress_to_Dead_Weight, Dead_Count)~CommonName, data=Phase1_Data_Weight)


autoplot(km_species_fit_stress_w) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  xlab("Weeks after Stress Point") +
  #ylab("Survivorship") +
  labs(title = "Kaplan Meier Survival Curve: Heatwave Effects by Species",
       color = "Species_Treatment", fill = "Species_Treatment") +
  theme_pubclean()



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





