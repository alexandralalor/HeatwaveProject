#Data analysis - dead
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-08-27

#load packages
library(tidyverse)
library(survival)

#read CSVs
Phase1_Data <- read_csv("data_analysis/Phase1_Data.csv")

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
Phase1_Data$Dead_Count <- as.factor(Phase1_Data$Dead_Count)
Phase1_Data$Heatwave_graph <- as.factor(Phase1_Data$Heatwave_graph)
Phase1_Data$Heatwave <- as.factor(Phase1_Data$Heatwave)



#summary stats
range(Phase1_Data$Week)

#Kaplan Meier Survival Curve - combined
km <- with(Phase1_Data, Surv(Week, Dead_Count))
km_species_fit <- survfit(Surv(Week, Dead_Count)~Species, data=Phase1_Data)

#Kaplan Meier Survival Curve - separated by treatment
km_treatment_fit <- survfit(Surv(Week, Dead_Count)~Heatwave_graph, data=Phase1_Data)
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
Phase1_Data_PIPO <- Phase1_Data %>% 
  filter(Species == "PIPO")
Phase1_Data_PIED <- Phase1_Data %>% 
  filter(Species == "PIED")
Phase1_Data_PIFL <- Phase1_Data %>% 
  filter(Species == "PIFL")
Phase1_Data_PSME <- Phase1_Data %>% 
  filter(Species == "PSME")
Phase1_Data_PIEN <- Phase1_Data %>% 
  filter(Species == "PIEN")

#test effect of heatwave on each species
cox_fit_PIPO <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Phase1_Data_PIPO)
cox_fit_PIED <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Phase1_Data_PIED)
cox_fit_PIFL <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Phase1_Data_PIFL)
cox_fit_PSME <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Phase1_Data_PSME)
cox_fit_PIEN <- coxph(Surv(Week, Dead_Count)~Heatwave, data=Phase1_Data_PIEN)

cox_fit_PIPO
cox_fit_PIED
cox_fit_PIFL
cox_fit_PSME
cox_fit_PIEN

summary(cox_fit_PIPO)


#test effect of size factors on species
cox_fit_PIPO_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Phase1_Data_PIPO)
cox_fit_PIED_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Phase1_Data_PIED)
cox_fit_PIFL_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Phase1_Data_PIFL)
cox_fit_PSME_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Phase1_Data_PSME)
cox_fit_PIEN_all <- coxph(Surv(Week, Dead_Count)~Heatwave+Biomass_g+BasalDia_mm+Height_mm, data=Phase1_Data_PIEN)

cox_fit_PIPO_all
cox_fit_PIED_all
cox_fit_PIFL_all
cox_fit_PSME_all
cox_fit_PIEN_all
