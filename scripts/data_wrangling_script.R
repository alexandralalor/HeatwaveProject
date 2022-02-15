#Data wrangling script - Phase 1 PIPO/PIED
#ARL Feb 2022

#working directory
setwd("~/Desktop/UofA/HW project/analysis/HeatwaveProject")

#read in data
Phase1_Chamber1_Dates <- read.csv(file = "data_raw/Phase1_Chamber1_Dates.csv")
Phase1_InitialData <- read.csv(file = "data_raw/Phase1_InitialData.csv")
Phase1_PIED_Dead <- read.csv(file = "data_raw/Phase1_PIED_Dead.csv")
Phase1_PIED_PercentBrown <- read.csv(file = "data_raw/Phase1_PIED_PercentBrown.csv")
Phase1_PIED_Porometer <- read.csv(file = "data_raw/Phase1_PIED_Porometer.csv")
Phase1_PIED_Weight <- read.csv(file = "data_raw/Phase1_PIED_Weight.csv")
Phase1_PIPO_Dead <- read.csv(file = "data_raw/Phase1_PIPO_Dead.csv")
Phase1_PIPO_PercentBrown <- read.csv(file = "data_raw/Phase1_PIPO_PercentBrown.csv")
Phase1_PIPO_Porometer <- read.csv(file = "data_raw/Phase1_PIPO_Porometer.csv")
Phase1_PIPO_Weight <- read.csv(file = "data_raw/Phase1_PIPO_Weight.csv")

#check data
head(Phase1_Chamber1_Dates)
head(Phase1_InitialData)
head(Phase1_PIED_Dead)
head(Phase1_PIED_PercentBrown)
head(Phase1_PIED_Porometer)
head(Phase1_PIED_Weights)
head(Phase1_PIPO_Dead)
head(Phase1_PIPO_PercentBrown)
head(Phase1_PIPO_Porometer)
head(Phase1_PIPO_Weights)

#load tidyverse
library(tidyverse)

#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable

#rename weeks so that 1.0 --> week_1.0
#I ended up just doing this in excel... so bad I know
#Phase1_PIED_Dead %>% rename_at(vars(-SpeciesID), ~paste0(.,))


#Replace X with 0 in all columns
#Don't just try in excel! would be useful to have R convert so I can keep the original
replace(Phase1_PIED_Weight$weeK_14.0, Phase1_PIED_Weight$week_14.0=="X", 0)
?replace
#working!
Phase1_PIED_Weight[Phase1_PIED_Weight == "X"] <- 0
#make sure that columns which previously had X are not characters, but integers instead
#make changes across all columns using "across()" function
Phase1_PIED_Weight <- Phase1_PIED_Weight %>% 
  mutate(across(week_14.0:week_22.0, as.integer))

#view structure of data, should show all data as int
str(Phase1_PIED_Weight)

#pivot my data so that weeks become columns.
#tidyr::pivot_longer(Phase1_PIED_Weights,
                    cols = starts_with("week"),
                    names_to = "Week",
                    values_to = "Weight",
                    names_prefix = "week_",
                    names_transform = list(Weight = as.character),
                    values_drop_na = TRUE)
#ITS ALIVE. Make sure to change "values_to" for whatever value I'm importing
#I had to take out annotations of "dead" "not dead" in my import. 
#"X" also changed to "0" to indicate dropped species
Phase1_PIED_Weights_new <- tidyr::pivot_longer(Phase1_PIED_Weights,
                                               cols = starts_with("week"),
                                               names_to = "Week",
                                               values_to = "Weight_g",
                                               names_prefix = "week_")


#add column for Species
