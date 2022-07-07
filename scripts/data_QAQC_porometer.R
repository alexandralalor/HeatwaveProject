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
Phase1_Data$Whorls <- as.factor(Phase1_Data$Whorls)
Phase1_Data$PercentBrown <- as.factor(Phase1_Data$PercentBrown)
Phase1_Data$Dead <- as.factor(Phase1_Data$Dead)

#merge data
Phase1_Porometer_QAQC <- merge(Phase1_Data, Phase1_Porometer, all = TRUE)

#save as csv
#write.csv(Phase1_Porometer_QAQC, "data_QAQC/Phase1_Porometer_QAQC.csv", quote = FALSE, row.names = FALSE)

################################################################################
#Test for extra conductance data
################################################################################

#test to see if plants not in porometer subset have porometer readings.

#filter for NA values
Phase1_Porometer_QAQC_1 <- Phase1_Porometer_QAQC %>% 
  filter(!is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#isolate errors. should be 0.
Phase1_Porometer_QAQC_1_test <- Phase1_Porometer_QAQC_1 %>% 
  select(c("PorometerSubset","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(PorometerSubset == "no")


#test to see if plants have porometer readings after measurements stop

#filter for Porometer Subset and NA values
Phase1_Porometer_QAQC_1.5 <- Phase1_Porometer_QAQC %>% 
  filter(PorometerSubset == "yes", !is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#Do the number of observations match?

#make changes to Phase1_Porometer in data_QAQC folder
###############################################################################

################################################################################
#Test Porometer vs Conductance
################################################################################

#filter for PorometerSubset plants
Phase1_Porometer_QAQC_2 <- Phase1_Porometer_QAQC %>% 
  filter(PorometerSubset == "yes") %>% 
  arrange(SpeciesID, Week)

Phase1_Porometer_QAQC_2 <- Phase1_Porometer_QAQC_2 %>% 
  select(c("PorometerSubset","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  mutate(Difference = ifelse(Phase1_Porometer_QAQC_2$Porometer == Phase1_Porometer_QAQC_2$Conductance, 
                             "no","yes"))

#QAQC check, should all be "no"
unique(Phase1_Porometer_QAQC_2$Difference)

#isolate errors
Phase1_Porometer_QAQC_2_test <- Phase1_Porometer_QAQC_2 %>% 
  filter(Difference == "yes")

#make changes to Phase1_Data in data_QAQC folder
################################################################################

################################################################################
#Summarize and Save
################################################################################

#filter for Porometer Subset and NA values
Phase1_Porometer_QAQC_3 <- Phase1_Porometer_QAQC %>% 
  filter(PorometerSubset == "yes", !is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#summarize average data 
Phase1_Porometer_QAQC_sum <- Phase1_Porometer_QAQC_3 %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>% 
  summarize(Porometer = mean(Porometer),
            Temperature = mean(Temperature_C),
            LeafSensor = mean(LeafSensor_PercentRH),
            FilterSensor = mean(FilterSensor_PercentRH))

#save as csv
write.csv(Phase1_Plants_Porometer_graph, "data_QAQC/Phase1_Plants_Porometer_graph.csv", quote = FALSE, row.names = FALSE)
