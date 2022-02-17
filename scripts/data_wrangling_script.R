#Data wrangling script - Phase 1 PIPO/PIED
#ARL Feb 2022

#working directory
setwd("~/Desktop/UofA/HW project/analysis/HeatwaveProject")

#load tidyverse
library(tidyverse)
library(dplyr)

#read in data
Phase1_Chamber1_Dates <- read.csv(file = "data_raw/Phase1_Chamber1_Dates.csv")
Phase1_InitialData <- read.csv(file = "data_raw/Phase1_InitialData.csv")
Phase1_PIED_Dead <- read.csv(file = "data_raw/Phase1_PIED_Dead.csv")
Phase1_PIED_PercentBrown <- read.csv(file = "data_raw/Phase1_PIED_PercentBrown.csv")
Phase1_PIED_Porometer <- read.csv(file = "data_raw/Phase1_PIED_Porometer.csv")
Phase1_PIED_Weight<- read.csv(file = "data_raw/Phase1_PIED_Weight.csv")
Phase1_PIPO_Dead <- read.csv(file = "data_raw/Phase1_PIPO_Dead.csv")
Phase1_PIPO_PercentBrown <- read.csv(file = "data_raw/Phase1_PIPO_PercentBrown.csv")
Phase1_PIPO_Porometer <- read.csv(file = "data_raw/Phase1_PIPO_Porometer.csv")
Phase1_PIPO_Weight <- read.csv(file = "data_raw/Phase1_PIPO_Weight.csv")


#view data
View(Phase1_PIED_Weight)



#my data has Xs embedded, and I need to change these to 0 or NA to make R happy
#right now I don't know which is better for analysis. So try both!
#start with weight data


###########PIED to 0
Phase1_PIED_Weight<- read.csv(file = "data_raw/Phase1_PIED_Weight.csv")
#Replace X with 0 in all columns
Phase1_PIED_Weight_0 <- Phase1_PIED_Weight
Phase1_PIED_Weight_0[Phase1_PIED_Weight_0 == "X"] <- 0

#make sure that columns which previously had X are not characters, but integers instead
#make changes across all columns using "across()" function
Phase1_PIED_Weight_0 <- Phase1_PIED_Weight_0 %>% 
  mutate(across(week_1.0:week_22.0, as.integer))

#view structure of data to make sure all data is int
str(Phase1_PIED_Weight_0)

#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Weight_0_pivot <- tidyr::pivot_longer(Phase1_PIED_Weight_0,
                                                  cols = starts_with("week"),
                                                  names_to = "Week",
                                                  values_to = "Weight_g",
                                                  names_prefix = "week_")



###########PIED to NA
Phase1_PIED_Weight<- read.csv(file = "data_raw/Phase1_PIED_Weight.csv")
#Replace X with NA in all columns
Phase1_PIED_Weight_NA <- Phase1_PIED_Weight
Phase1_PIED_Weight_NA[Phase1_PIED_Weight_NA == "X"] <- NA


#make sure that columns which previously had X are not characters, but integers instead
#make changes across all columns using "across()" function

#can't to the function below because Species and SpeciesID cannot be changed to NA
#Phase1_PIED_Weight_NA <- Phase1_PIED_Weight_NA %>%
#  mutate(across(where(is.character), as.integer))

Phase1_PIED_Weight_NA <- Phase1_PIED_Weight_NA %>%
  mutate(across(week_1.0:week_22.0, as.integer))



#view structure of data, should show all data as int
str(Phase1_PIED_Weight_NA)

#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Weight_NA_pivot <- tidyr::pivot_longer(Phase1_PIED_Weight_NA,
                                                   cols = starts_with("week"),
                                                   names_to = "Week",
                                                   values_to = "Weight_g",
                                                   names_prefix = "week_")


###YAY it worked!! Not for all the other protocols! 
#I'm going to keep working with NA data because I think it'll cause less problems in the future
#But 0 data is still above if I decide this makes more sense

###########PIED to NA
#let's do PIED Porometer
Phase1_PIED_Porometer <- read.csv(file = "data_raw/Phase1_PIED_Porometer.csv")

#Replace X with NA in all columns
Phase1_PIED_Porometer_NA <- Phase1_PIED_Porometer
Phase1_PIED_Porometer_NA[Phase1_PIED_Porometer_NA == "X"] <- NA

####IMPORTANT we dont want integers here! because Porometer readings have decimals
#make sure that columns which previously had X are not characters, but numeric instead
#make changes across all columns using "across()" function
Phase1_PIED_Porometer_NA <- Phase1_PIED_Porometer_NA %>% 
  mutate(across(week_1.0:week_21.0, as.numeric))

#view structure of data, should show all data as num
str(Phase1_PIED_Porometer_NA)

#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Porometer_NA_pivot <- tidyr::pivot_longer(Phase1_PIED_Porometer_NA,
                                                   cols = starts_with("week"),
                                                   names_to = "Week",
                                                   values_to = "Porometer",
                                                   names_prefix = "week_")


#now how can I stitch together these data frames?
