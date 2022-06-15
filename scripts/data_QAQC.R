#QAQC
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-11

#load tidyverse
library(tidyverse)

#read in clean csvs
Phase1_Kestrel_Meta <- read_csv("data_clean/Phase1_Kestrel_Meta.csv")
Phase1_Dates <- read_csv("data_clean/Phase1_Dates.csv")
Phase1_InitialData <- read_csv("data_clean/Phase1_InitialData.csv")
Phase1_TempSettings <- read_csv("data_clean/Phase1_TempSettings.csv")
Phase1_Kestrel <- read_csv("data_clean/Phase1_Chamber1_Kestrel.csv")
Phase1_Porometer <- read_csv("data_clean/Phase1_Porometer.csv")
Phase1_Plants <- read_csv("data_clean/Phase1_Plants.csv")

#check structure, ensure consistent formats
#Date as <date>
#Time as <chr>
#DateTime as <dttm>
#Phase as <fctr>
#Chamber as <fctr>
#Kestrel as <fctr>
glimpse(Phase1_Kestrel_Meta)
glimpse(Phase1_Dates)
glimpse(Phase1_InitialData)
glimpse(Phase1_TempSettings)
glimpse(Phase1_Kestrel)
glimpse(Phase1_Porometer)
glimpse(Phase1_Plants)

#now add metadata/combine info

#Kestrel + Kestrel_Meta data
Phase1_Kestrel <- merge(Phase1_Kestrel, Phase1_Kestrel_Meta, by = c("Phase", "Chamber", "Kestrel"))

#Plants + InitialData



Phase1_InitialData_Treatments <- subset(Phase1_InitialData, ,c(2, 4:7))

Phase1_meta <- merge(Phase1_all,
                     Phase1_InitialData_Treatments,
                     by=c("SpeciesID"))
#temp
Phase1_meta_temp <- merge(Phase1_all_temp,
                     Phase1_InitialData_Treatments,
                     by=c("SpeciesID"))

#Error checking
#PercentBrown as 10, 25, 50, 75, 90
#I'm not gonna worry if 50% changes to 25% because this isn't a data collection error
#it's just a subjective human measurement that's likely to change

#look for outliers in weight data
#For droughted plants, make sure that weights are always decreasing


#make sure porometer measurements match what I have
#If measurements don't match, default to porometer device
#This means I gotta data wrangle that script too

#Get weather data together and match with the chamber
#add to meta data?

  


