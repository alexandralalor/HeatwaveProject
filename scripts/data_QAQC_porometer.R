#QAQC - check porometer data against plant data
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-06
#Last updated: 2022-07-06

#load tidyverse
library(tidyverse)

#read in clean csvs
Phase1_Porometer <- read_csv("data_clean/Phase1_Porometer.csv")
Phase1_Plants <- read_csv("data_clean/Phase1_Plants.csv")

#check structure, ensure consistent formats
#DateTime as <dttm>
#Date as <date>
#Time as <chr>
#Species as <fct>
#Dead as <fct>
glimpse(Phase1_Porometer)
glimpse(Phase1_Plants)

#Convert variables
Phase1_Porometer$Species <- as.factor(Phase1_Porometer$Species)
Phase1_Plants$Species <- as.factor(Phase1_Plants$Species)
Phase1_Plants$Dead <- as.factor(Phase1_Plants$Dead)



