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
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

#check out data
glimpse(Phase1_Data)

#convert variables
Phase1_Data$Phase <- as.factor(Phase1_Data$Phase)
Phase1_Data$Chamber <- as.factor(Phase1_Data$Chamber)
Phase1_Data$ScientificName <- as.factor(Phase1_Data$ScientificName)
Phase1_Data$CommonName <- as.factor(Phase1_Data$CommonName)
Phase1_Data$Species <- as.factor(Phase1_Data$Species)
Phase1_Data$Treatment_temp <- as.factor(Phase1_Data$Treatment_temp)
Phase1_Data$Treatment_water <- as.factor(Phase1_Data$Treatment_water)
Phase1_Data$PorometerSubset <- as.factor(Phase1_Data$PorometerSubset)
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)


#create new heatwave variables for graphing
Phase1_Data <- Phase1_Data %>% 
  mutate(Heatwave_graph = Treatment_temp) %>% 
  separate(Heatwave_graph, sep = "_",
           into = c("Ambient", "Heatwave_graph")) %>% 
  mutate(Heatwave = Heatwave_graph)

Phase1_Data$Heatwave[is.na(Phase1_Data$Heatwave)] <- "no"
Phase1_Data <- Phase1_Data %>% 
  mutate(Heatwave = ifelse(Phase1_Data$Heatwave == "HW", "yes", "no"))
Phase1_Data <- Phase1_Data %>% 
  mutate(Heatwave_graph = ifelse(Phase1_Data$Heatwave_graph == "HW", "heatwave", Phase1_Data$Heatwave_graph))

Phase1_Data$Heatwave_graph <- str_c(Phase1_Data$CommonName, "_", Phase1_Data$Heatwave_graph)
Phase1_Data$Heatwave_graph[is.na(Phase1_Data$Heatwave_graph)] <- "X"
Phase1_Data <- Phase1_Data %>% 
  mutate(Heatwave_graph = ifelse(Phase1_Data$Heatwave_graph == "X" & Phase1_Data$CommonName == "Ponderosa Pine", "Ponderosa Pine", 
                                 ifelse(Phase1_Data$Heatwave_graph == "X" & Phase1_Data$CommonName == "Pinyon Pine", "Pinyon Pine",
                                        ifelse(Phase1_Data$Heatwave_graph == "X" & Phase1_Data$CommonName == "Limber Pine", "Limber Pine",
                                               ifelse(Phase1_Data$Heatwave_graph == "X" & Phase1_Data$CommonName == "Engelman Spruce", "Engelman Spruce",
                                                      ifelse(Phase1_Data$Heatwave_graph == "X" & Phase1_Data$CommonName == "Douglas fir", "Douglas fir", Phase1_Data$Heatwave_graph))))))


#Kaplan Meier Survival Curve - combined
km <- with(Phase1_Data, Surv(Week, Dead_Count))
km_species_fit <- survfit(Surv(Week, Dead_Count)~Species, data=Phase1_Data)
autoplot(km_species_fit)  

#summary stats
range(Phase1_Data$Week)


#Kaplan Meier Survival Curve - separated by treatment
km_treatment_fit <- survfit(Surv(Week, Dead_Count)~Heatwave_graph, data=Phase1_Data)
summary(km_treatment_fit)

autoplot(km_treatment_fit) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 1,
           color = "red",
           linetype = "dashed",
           size = 0.8) +
  geom_text(label = "Heatwave",
            x = 5, y = 0.78, color = "red", size = 3) +
  xlab("Weeks") +
  ylab("Survivorship") +
  labs(title = "Kaplan Meier Survival Curve: Heatwave Effects by Species",
       color = "Species_Treatment", fill = "Species_Treatment") +
  theme_bw()



#Kaplan Meier Survival Curve - separated by treatment - STRESS WEEK


Phase1_Data_Weight_Stress <- Phase1_Data %>% 
  mutate(Stress_Week = ifelse(Phase1_Data$Species == "PIPO", PIPO_add$Week, 
                              ifelse(Phase1_Data$Species == "PIED", PIED_add$Week,
                                     ifelse(Phase1_Data$Species == "PIFL", PIFL_add$Week,
                                            ifelse(Phase1_Data$Species == "PSME", PSME_add$Week, PIEN_add$Week)))))

Phase1_Data_Weight_Stress <- Phase1_Data_Weight_Stress %>% 
  mutate(Stress_Week_test = round(Phase1_Data_Weight_Stress$Week - Phase1_Data_Weight_Stress$Stress_Week, digits = 2))


km_treatment_fit_stress <- survfit(Surv(Stress_Week_test, Dead_Count)~Heatwave_graph, data=Phase1_Data_Weight_Stress)
summary(km_treatment_fit_stress)

autoplot(km_treatment_fit_stress) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  #scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  # annotate("segment",
  #          x = 7, xend = 7,
  #          y = 0, yend = 1,
  #          color = "red",
  #          linetype = "dashed",
  #          size = 0.8) +
  # geom_text(label = "Heatwave",
  #           x = 5, y = 0.78, color = "red", size = 3) +
  xlab("Weeks after Stress Point") +
  ylab("Survivorship") +
  labs(title = "Kaplan Meier Survival Curve: Heatwave Effects by Species",
       color = "Species_Treatment", fill = "Species_Treatment") +
  theme_bw()

