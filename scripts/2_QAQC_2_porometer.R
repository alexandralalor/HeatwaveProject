#QAQC - porometer
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-06
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Porometer <- read_csv("data_QAQC/Phase1_Porometer.csv")
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

#check structure, ensure consistent formats
#DateTime as <dttm>
#Date as <date>
#Time as <chr>
glimpse(Phase1_Porometer)
glimpse(Phase1_Data)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Phase1_Porometer <- Phase1_Porometer %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Porometer$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_Porometer$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_Porometer$Time), Time))))
Phase1_Porometer$DateTime <- as.POSIXct(Phase1_Porometer$DateTime, format = "%m/%d/%Y %H:%M")
Phase1_Porometer$Date <- as.Date(Phase1_Porometer$Date, format = "%m/%d/%Y")

Phase1_Porometer$Species <- as.factor(Phase1_Porometer$Species)

Phase1_Data$Phase <- as.factor(Phase1_Data$Phase)
Phase1_Data$Chamber <- as.factor(Phase1_Data$Chamber)
Phase1_Data$ScientificName <- as.factor(Phase1_Data$ScientificName)
Phase1_Data$CommonName <- as.factor(Phase1_Data$CommonName)
Phase1_Data$Species <- as.factor(Phase1_Data$Species)
Phase1_Data$Treatment_temp <- as.factor(Phase1_Data$Treatment_temp)
Phase1_Data$Treatment_water <- as.factor(Phase1_Data$Treatment_water)
Phase1_Data$PorometerSubset <- as.factor(Phase1_Data$PorometerSubset)
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)

#merge data
Phase1_Data_Porometer <- merge(Phase1_Data, Phase1_Porometer, all = TRUE)
Phase1_Data_Porometer <- Phase1_Data_Porometer[ ,c(4,5,6,7,1,2,8,9,10,11,12,13,14,3,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]

#save as csv
write.csv(Phase1_Data_Porometer, "data_QAQC/Phase1_Data_Porometer.csv", quote = FALSE, row.names = FALSE)

################################################################################
#Test for extra conductance data
################################################################################

#test to see if plants not in porometer subset have porometer readings.

#filter for NA values
Phase1_Data_Porometer_1.1 <- Phase1_Data_Porometer %>% 
  filter(!is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#isolate errors. should be 0.
Phase1_Data_Porometer_1.1_test <- Phase1_Data_Porometer_1.1 %>% 
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(PorometerSubset == "no")


#test to see if plants in porometer subset have porometer readings.

#filter for NA values
Phase1_Data_Porometer_1.2 <- Phase1_Data_Porometer %>% 
  filter(!is.na(Porometer)) %>% 
  arrange(SpeciesID, Week)

#isolate errors, find NA values should be 0.
Phase1_Data_Porometer_1.2_test <- Phase1_Data_Porometer_1.2 %>% 
  select(c("PorometerSubset","Date", "Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(is.na(Conductance))

#one missing porometer reading 


#test to find duplicate porometer readings

#filter for Porometer Subset and NA values
Phase1_Data_Porometer_1.3 <- Phase1_Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#Check for duplicates
Phase1_Data_Porometer_1.3_test <- Phase1_Data_Porometer_1.3 %>%
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>%
  group_by(SpeciesID, Week) %>% 
  mutate(duplicate = n()>1) %>% 
  filter(duplicate == "TRUE")



#test to see if plants have porometer readings after measurements stop

#filter for Porometer Subset and NA values
Phase1_Data_Porometer_1.4 <- Phase1_Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#isolate errors, find NA values. Should be 0
Phase1_Data_Porometer_1.4_test <- Phase1_Data_Porometer_1.4 %>% 
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(is.na(Porometer))


#make changes to Phase1_Porometer in data_QAQC folder
###############################################################################

################################################################################
#Test Porometer vs Conductance
################################################################################

#filter for PorometerSubset plants
Phase1_Data_Porometer_2 <- Phase1_Data_Porometer %>% 
  filter(PorometerSubset == "yes") %>% 
  arrange(SpeciesID, Week)

Phase1_Data_Porometer_2 <- Phase1_Data_Porometer_2 %>% 
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  mutate(Difference = ifelse(Phase1_Data_Porometer_2$Porometer == Phase1_Data_Porometer_2$Conductance, 
                             "no","yes"))

#QAQC check, should all be "no"
unique(Phase1_Data_Porometer_2$Difference)

#isolate errors
Phase1_Data_Porometer_2_test <- Phase1_Data_Porometer_2 %>% 
  filter(Difference == "yes")

#make changes to Phase1_Data in data_QAQC folder
################################################################################
