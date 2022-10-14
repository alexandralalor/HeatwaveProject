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
Phase1_Data_Weight <- read_csv("data_analysis/Phase1_Data_Weight.csv")
Phase1_Data_Porometer <- read_csv("data_analysis/Phase1_Data_Porometer.csv")
Phase1_Data_PercentBrown <- read_csv("data_analysis/Phase1_Data_PercentBrown.csv")
Phase1_Data_Photos <- read_csv("data_analysis/Phase1_Data_Photos.csv")

#condense data to include important columns only
Phase1_Data_Weight_add <- Phase1_Data_Weight %>% 
  group_by(SpeciesID, Week) %>% 
  select(c("SampleSize_Weekly_Weight", "Weight_Est", "WaterWeight_Calc", "PercentWater",
           "Stress_Week_Weight", "Stress_Week_Avg_Weight", "Stress_to_Dead_Weight",
           "SD_Weight_Total", "SD_Weight_Water")) %>% 
  arrange(SpeciesID, Week)

Phase1_Data_Porometer_add <- Phase1_Data_Porometer %>% 
  group_by(SpeciesID, Week) %>% 
  select(c("SampleSize_Weekly_Porometer", "Porometer_Est", 
           "Stress_Week_Porometer", "Stress_Week_Avg_Porometer", "Stress_to_Dead_Porometer",
           "SD_Porometer")) %>% 
  arrange(SpeciesID, Week)

Phase1_Data_PercentBrown_add <- Phase1_Data_PercentBrown %>% 
  group_by(SpeciesID, Week) %>% 
  select(c("SampleSize_Weekly_PercentBrown", "PercentBrown_Est",
           "SD_PercentBrown")) %>% 
  arrange(SpeciesID, Week)

Phase1_Data_Photos_add <- Phase1_Data_Photos %>% 
  group_by(SpeciesID, Week) %>% 
  select(c("SampleSize_Weekly_Photos", "PercentGreen", "PercentRed",
           "SD_PercentRed")) %>% 
  arrange(SpeciesID, Week)
Phase1_Data_Photos_add <- Phase1_Data_Photos_add %>% 
  group_by(SpeciesID, Week) %>% 
  summarize(SampleSize_Weekly_Photos = mean(SampleSize_Weekly_Photos, na.rm = T),
            PercentGreen = mean(PercentGreen, na.rm = T),
            PercentRed = mean(PercentRed, na.rm = T),
            SD_PercentRed = mean(SD_PercentRed, na.rm = T)) %>% 
  arrange(SpeciesID, Week)

Phase1_Data_add <- Phase1_Data %>% 
  group_by(SpeciesID, Week) %>% 
  select(c("SampleSize", "SampleSize_Porometer", "SampleSize_Weekly_Dead",
           "Dead_Week")) %>% 
  arrange(SpeciesID, Week)
Phase1_Data <- Phase1_Data %>% 
  group_by(SpeciesID, Week) %>% 
  select(-c("SampleSize", "SampleSize_Porometer", "SampleSize_Weekly_Dead",
           "Dead_Week")) %>% 
  arrange(SpeciesID, Week)

#merge data frames
Phase1_Data_Porometer_Weight <- merge(Phase1_Data_Porometer_add, Phase1_Data_Weight_add, 
                                      by = c("SpeciesID", "Week"), all.y = T)
Phase1_Data_PercentBrown_Photos <- merge(Phase1_Data_PercentBrown_add, Phase1_Data_Photos_add,
                                         by = c("SpeciesID", "Week"), all.x = T)
Phase1_Data_Porometer_Weight_PercentBrown_Photos <- merge(Phase1_Data_Porometer_Weight, Phase1_Data_PercentBrown_Photos,
                                                          by = c("SpeciesID", "Week"), all.y = T)
Phase1_Data_Porometer_Weight_PercentBrown_Photos_Dead <- merge(Phase1_Data_add, Phase1_Data_Porometer_Weight_PercentBrown_Photos,
                                                               by = c("SpeciesID", "Week"), all = T)

#reorder and rearrange columns
Phase1_Data_Porometer_Weight_PercentBrown_Photos_Dead_add <- 
  Phase1_Data_Porometer_Weight_PercentBrown_Photos_Dead[, c("SpeciesID", "Week",
                                                            "SampleSize","SampleSize_Porometer",
                                                            "SampleSize_Weekly_Dead", "SampleSize_Weekly_PercentBrown", "SampleSize_Weekly_Photos",
                                                            "SampleSize_Weekly_Weight", "SampleSize_Weekly_Porometer",
                                                            "PercentBrown_Est", "PercentGreen", "PercentRed",
                                                            "Weight_Est", "Porometer_Est",
                                                            "Dead_Week",
                                                            "Stress_Week_Weight", "Stress_Week_Avg_Weight", "Stress_to_Dead_Weight",
                                                            "Stress_Week_Porometer", "Stress_Week_Avg_Porometer","Stress_to_Dead_Porometer",
                                                            "SD_PercentBrown", "SD_PercentRed",
                                                            "SD_Weight_Total", "SD_Weight_Water", "SD_Porometer",
                                                            "WaterWeight_Calc", "PercentWater")]
Phase1_Data_Porometer_Weight_PercentBrown_Photos_Dead_add <- Phase1_Data_Porometer_Weight_PercentBrown_Photos_Dead_add %>% 
  arrange(SpeciesID, Week)

# add data
Phase1_Data_All <- merge(Phase1_Data, Phase1_Data_Porometer_Weight_PercentBrown_Photos_Dead_add,
                         by = c("SpeciesID", "Week"), all = T)

#Phase1_Data_all <- Phase1_Data_all[, c(3,4,5,6,7,1,8,9,10,11,12,13,14,2,15,32,16,33,17,29,30,31,18,19,20,21,22,23,24,25,26,27,28,34,35,36,37,38,39,40,41,42,43,44,45,46,47)]

#save csv
write.csv(Phase1_Data_All, "data_analysis/Phase1_Data_All.csv", quote = FALSE, row.names = FALSE)



################################################################################
# Average
################################################################################
#read CSVs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

#filter NAs
Phase1_Data_All_Avg <- Phase1_Data_All

#average data 
Phase1_Data_All_Avg <- Phase1_Data_All_Avg %>%
  #filter(SpeciesID != "PIFL16") %>%
  group_by(ScientificName, CommonName, Species, Treatment_temp, Treatment_water, Heatwave_graph, Heatwave, Week) %>%
  summarize(PercentBrown_Est = mean(PercentBrown_Est, na.rm = T),
            PercentGreen = mean(PercentGreen, na.rm = T),
            PercentRed = mean(PercentRed, na.rm = T),
            Weight_Est = mean(Weight_Est, na.rm = T),
            Porometer_Est = mean(Porometer_Est, na.rm = T),
            SampleSize = mean(SampleSize, na.rm = T),
            SampleSize_Porometer = mean(SampleSize_Porometer, na.rm = T),
            SampleSize_Weekly_Dead = mean(SampleSize_Weekly_Dead, na.rm = T),
            SampleSize_Weekly_PercentBrown = mean(SampleSize_Weekly_PercentBrown, na.rm = T),
            SampleSize_Weekly_Photos = mean(SampleSize_Weekly_Photos, na.rm = T),
            SampleSize_Weekly_Weight = mean(SampleSize_Weekly_Weight, na.rm = T),
            SampleSize_Weekly_Porometer = mean(SampleSize_Weekly_Porometer, na.rm = T),
            Dead_Count = sum(Dead_Count),
            Dead_Week_Avg = mean(Dead_Week, na.rm = T),
            Stress_Week_Avg_Weight = mean(Stress_Week_Avg_Weight, na.rm = T),
            Stress_to_Dead_Avg_Weight = mean(Stress_to_Dead_Weight, na.rm = T),
            Stress_Week_Avg_Porometer = mean(Stress_Week_Avg_Porometer, na.rm = T),
            Stress_to_Dead_Avg_Porometer = mean(Stress_to_Dead_Porometer, na.rm = T),
            SD_PercentBrown = mean(SD_PercentBrown, na.rm = T),
            SD_PercentRed = mean(SD_PercentRed, na.rm = T),
            SD_Weight_Total = mean(SD_Weight_Total, na.rm = T),
            SD_Weight_Water = mean(SD_Weight_Water, na.rm = T),
            SD_Porometer = mean(SD_Porometer, na.rm = T),
            WaterWeight_Calc = mean(WaterWeight_Calc, na.rm = T),
            PercentWater = mean(PercentWater, na.rm = T))


#save csv
write.csv(Phase1_Data_All_Avg, "data_analysis/Phase1_Data_All_Avg.csv", quote = FALSE, row.names = FALSE)

################################################################################



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
# Phase1_Data$Dead_Count <- as.factor(Phase1_Data$Dead_Count)
# Phase1_Data$Heatwave_graph <- as.factor(Phase1_Data$Heatwave_graph)
# Phase1_Data$Heatwave <- as.factor(Phase1_Data$Heatwave)



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
