#QAQC
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-06
#Last updated: 2022-07-06

#load tidyverse
library(tidyverse)

#read in clean csvs
Phase1_Dates <- read_csv("data_clean/Phase1_Dates.csv")
Phase1_InitialData <- read_csv("data_clean/Phase1_InitialData.csv")
Phase1_Porometer <- read_csv("data_clean/Phase1_Porometer.csv")
Phase1_Plants <- read_csv("data_clean/Phase1_Plants.csv")

#check structure, ensure consistent formats
#Date as <date>
#Time as <chr>
#DateTime as <dttm>
#Phase as <fct>
#Chamber as <fct>
#Whorls as <fct>
#PorometerSubset as <fct>
#Treatment_temp as <fct>
#Treatment_water as <fct>
#Species as <fct>
#Dead as <fct>
glimpse(Phase1_Porometer)
glimpse(Phase1_InitialData)
glimpse(Phase1_Plants)
glimpse(Phase1_Dates)

#Convert variables
Phase1_InitialData$Phase <- as.factor(Phase1_InitialData$Phase)
Phase1_InitialData$Chamber <- as.factor(Phase1_InitialData$Chamber)
Phase1_InitialData$Whorls <- as.factor(Phase1_InitialData$Whorls)
Phase1_InitialData$PorometerSubset <- as.factor(Phase1_InitialData$PorometerSubset)
Phase1_InitialData$Treatment_temp <- as.factor(Phase1_InitialData$Treatment_temp)
Phase1_InitialData$Treatment_water <- as.factor(Phase1_InitialData$Treatment_water)
Phase1_InitialData$Species <- as.factor(Phase1_InitialData$Species)
Phase1_Plants$Species <- as.factor(Phase1_Plants$Species)
Phase1_Plants$Dead <- as.factor(Phase1_Plants$Dead)

#check values
unique(Phase1_InitialData$Phase)
unique(Phase1_InitialData$Chamber)
unique(Phase1_InitialData$ScientificName)
unique(Phase1_InitialData$CommonName)
unique(Phase1_InitialData$Species)
unique(Phase1_InitialData$Treatment_temp)
unique(Phase1_InitialData$Treatment_water)
unique(Phase1_InitialData$PorometerSubset)
unique(Phase1_InitialData$Whorls)
