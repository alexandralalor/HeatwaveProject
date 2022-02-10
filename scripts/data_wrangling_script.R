#Data wrangling script - Phase 1 PIPO/PIED
#ARL Feb 8 2022

#working directory
setwd("~/Desktop/UofA/HW project/analysis/HeatwaveProject")

#read in data
Phase1_Chamber1_Dates <- read.csv(file = "data/Phase1_Chamber1_Dates.csv")
Phase1_InitialData <- read.csv(file = "data/Phase1_InitialData.csv")
Phase1_PIED_Dead <- read.csv(file = "data/Phase1_PIED_Dead.csv")
Phase1_PIED_PercentBrown <- read.csv(file = "data/Phase1_PIED_PercentBrown.csv")
Phase1_PIED_Porometer <- read.csv(file = "data/Phase1_PIED_Porometer.csv")
Phase1_PIED_Weights <- read.csv(file = "data/Phase1_PIED_Weights.csv")
Phase1_PIPO_Dead <- read.csv(file = "data/Phase1_PIPO_Dead.csv")
Phase1_PIPO_PercentBrown <- read.csv(file = "data/Phase1_PIPO_PercentBrown.csv")
Phase1_PIPO_Porometer <- read.csv(file = "data/Phase1_PIPO_Porometer.csv")
Phase1_PIPO_Weights <- read.csv(file = "data/Phase1_PIPO_Weights.csv")

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

#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable

#rename weeks so that 1.0 --> week_1.0
#Phase1_PIED_Dead %>% rename_at(vars(-SpeciesID), ~paste0(.,))

#Replace X with 0
#replace(Phase1_PIED_Weights$week_14, Phase1_PIED_Weights$week_14=="X", 0)


#pivot my data so that weeks become columns. H
tidyr::pivot_longer(Phase1_PIED_Weights,
                    cols = starts_with("week"),
                    names_to = "Week",
                    values_to = "Weight",
                    names_prefix = "week_",
                    names_transform = list(Weight = as.character),
                    values_drop_na = TRUE)
#ITS ALIVE. 
#I had to take out annotations of "dead" "not dead" in my import. 
#"X" also changed to "0" to indicate dropped species
Phase1_PIED_Weights_new <- tidyr::pivot_longer(Phase1_PIED_Weights,
                                               cols = starts_with("week"),
                                               names_to = "Week",
                                               values_to = "Weight",
                                               names_prefix = "week_")

help(pivot_longer)

#add column for Species
