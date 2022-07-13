#Data viz - percent brown
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-13
#Last updated: 2022-07-13

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

#check out data
glimpse(Phase1_Data)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Phase1_Data$Phase <- as.factor(Phase1_Data$Phase)
Phase1_Data$Chamber <- as.factor(Phase1_Data$Chamber)
Phase1_Data$ScientificName <- as.factor(Phase1_Data$ScientificName)
Phase1_Data$CommonName <- as.factor(Phase1_Data$CommonName)
Phase1_Data$Species <- as.factor(Phase1_Data$Species)
Phase1_Data$Treatment_temp <- as.factor(Phase1_Data$Treatment_temp)
Phase1_Data$Treatment_water <- as.factor(Phase1_Data$Treatment_water)
Phase1_Data$PorometerSubset <- as.factor(Phase1_Data$PorometerSubset)
Phase1_Data$Whorls <- as.factor(Phase1_Data$Whorls)
Phase1_Data$PercentBrown <- as.factor(Phase1_Data$PercentBrown)
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)

