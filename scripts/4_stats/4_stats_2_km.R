#Data analysis - photos
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-25
#Last updated: 2026-03-01

#load packages
library(tidyverse)
library(survival)

#read csv
Data_All <- read_csv("data/data_analysis/Data_All.csv")

# #check out data
# glimpse(Data)
# 
# #convert variables
# Data$Phase <- as.factor(Data$Phase)
# Data$Chamber <- as.factor(Data$Chamber)
# Data$ScientificName <- as.factor(Data$ScientificName)
# Data$CommonName <- as.factor(Data$CommonName)
# Data$Species <- as.factor(Data$Species)
# Data$Treatment_temp <- as.factor(Data$Treatment_temp)
# Data$Treatment_water <- as.factor(Data$Treatment_water)
# Data$PorometerSubset <- as.factor(Data$PorometerSubset)
# Data$Dead <- as.factor(Data$Dead)
# Data$Dead_Count <- as.factor(Data$Dead_Count)
# Data$Heatwave_graph <- as.factor(Data$Heatwave_graph)
# Data$Heatwave <- as.factor(Data$Heatwave)

Data <- Data_All %>% 
  filter(Phase == 2)

#summary stats
range(Data$Week)

#Kaplan Meier Survival Curve - combined
km <- with(Data, Surv(Week, Dead_Count))
km_species_fit <- survfit(Surv(Week, Dead_Count)~Species, data=Data)

#Kaplan Meier Survival Curve - separated by treatment
km_treatment_fit <- survfit(Surv(Week, Dead_Count)~Heatwave_graph, data=Data)
summary(km_treatment_fit)




#############
# Cox Regression

# This shows us how all the variables, when considered together, 
# act to influence survival.
# Cox PH regression can assess the effect of both categorical and continuous 
# variables, and can model the effect of multiple variables at once.

# The exp(coef) column is the hazard ratio – 
# the multiplicative effect of that variable on the hazard rate.
# HR = 1: No effect
# HR > 1: Increase in hazard
# HR < 1: Decrease in hazard

#separate by species
Data_PIPO <- Data %>% 
  filter(Species == "PIPO")
Data_PIED <- Data %>% 
  filter(Species == "PIED")
Data_PIFL <- Data %>% 
  filter(Species == "PIFL")
Data_PSME <- Data %>% 
  filter(Species == "PSME")
Data_PIEN <- Data %>% 
  filter(Species == "PIEN")

#test effect of heatwave on each species
cox_fit_PIPO <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Data_PIPO)
cox_fit_PIED <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Data_PIED)
cox_fit_PIFL <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Data_PIFL)
cox_fit_PSME <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Data_PSME)
cox_fit_PIEN <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Data_PIEN)

cox_fit_PIPO
cox_fit_PIED
cox_fit_PIFL
cox_fit_PSME
cox_fit_PIEN

summary(cox_fit_PIPO)


#test effect of size factors on species
cox_fit_PIPO_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Data_PIPO)
cox_fit_PIED_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Data_PIED)
cox_fit_PIFL_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Data_PIFL)
cox_fit_PSME_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Data_PSME)
cox_fit_PIEN_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Data_PIEN)

cox_fit_PIPO_all
cox_fit_PIED_all
cox_fit_PIFL_all
cox_fit_PSME_all
cox_fit_PIEN_all

