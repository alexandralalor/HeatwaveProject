#QAQC - porometer
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-06
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Porometer <- read_csv("data_QAQC/Phase1_Porometer.csv")
Data <- read_csv("data_QAQC/Data.csv")

#check structure, ensure consistent formats
#DateTime as <dttm>
#Date as <date>
#Time as <chr>
glimpse(Porometer)
glimpse(Data)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Porometer <- Phase1_Porometer %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Porometer$Time), 
                       ifelse(Time == 30, paste0("00", Porometer$Time), 
                              ifelse(Time == 0, paste0("000", Porometer$Time), Time))))
Porometer$DateTime <- as.POSIXct(Porometer$DateTime, format = "%m/%d/%Y %H:%M")
Porometer$Date <- as.Date(Porometer$Date, format = "%m/%d/%Y")

Porometer$Species <- as.factor(Porometer$Species)

Data$Phase <- as.factor(Data$Phase)
Data$Chamber <- as.factor(Data$Chamber)
Data$ScientificName <- as.factor(Data$ScientificName)
Data$CommonName <- as.factor(Data$CommonName)
Data$Species <- as.factor(Data$Species)
Data$Treatment_temp <- as.factor(Data$Treatment_temp)
Data$Treatment_water <- as.factor(Data$Treatment_water)
Data$PorometerSubset <- as.factor(Data$PorometerSubset)
Data$Dead <- as.factor(Data$Dead)
Data$Heatwave_graph <- as.factor(Data$Heatwave_graph)
Data$Heatwave <- as.factor(Data$Heatwave)


#merge data
Data_Porometer <- merge(Data, Porometer, all = TRUE)
#Data_Porometer <- Data_Porometer[ ,c(4,5,6,7,1,2,8,9,10,11,12,13,14,3,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]


#clean up data, remove half weeks, filter for porometer data
Data_Porometer <- Data_Porometer %>% 
  filter(!grepl(".5", Data_Porometer$Week, fixed = TRUE), 
         PorometerSubset == "yes")

#save as csv
write.csv(Data_Porometer, "data_QAQC/Data_Porometer.csv", quote = FALSE, row.names = FALSE)

################################################################################
#Test for extra conductance data
################################################################################

#test to see if plants not in porometer subset have porometer readings.

#filter for NA values
Data_Porometer_1.1 <- Data_Porometer %>% 
  filter(!is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#isolate errors. should be 0.
Data_Porometer_1.1_test <- Data_Porometer_1.1 %>% 
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(PorometerSubset == "no")


#test to see if plants in porometer subset have porometer readings.

#filter for NA values
Data_Porometer_1.2 <- Data_Porometer %>% 
  filter(!is.na(Porometer)) %>% 
  arrange(SpeciesID, Week)

#isolate errors, find NA values should be 0.
Data_Porometer_1.2_test <- Data_Porometer_1.2 %>% 
  select(c("PorometerSubset","Date", "Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(is.na(Conductance))

#one missing porometer reading 


#test to find duplicate porometer readings

#filter for Porometer Subset and NA values
Data_Porometer_1.3 <- Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#Check for duplicates
Data_Porometer_1.3_test <- Data_Porometer_1.3 %>%
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>%
  group_by(SpeciesID, Week) %>% 
  mutate(duplicate = n()>1) %>% 
  filter(duplicate == "TRUE")



#test to see if plants have porometer readings after measurements stop

#filter for Porometer Subset and NA values
Data_Porometer_1.4 <- Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Conductance)) %>% 
  arrange(SpeciesID, Week)

#isolate errors, find NA values. Should be 0
Data_Porometer_1.4_test <- Data_Porometer_1.4 %>% 
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  filter(is.na(Porometer))


#make changes to Porometer in data_QAQC folder
###############################################################################

################################################################################
#Test Porometer vs Conductance
################################################################################

#filter for PorometerSubset plants
Data_Porometer_2 <- Data_Porometer %>% 
  filter(PorometerSubset == "yes") %>% 
  arrange(SpeciesID, Week)

Data_Porometer_2 <- Data_Porometer_2 %>% 
  select(c("PorometerSubset","Date","Week","Species","SpeciesID","Porometer","Conductance")) %>% 
  mutate(Difference = ifelse(Data_Porometer_2$Porometer == Data_Porometer_2$Conductance, 
                             "no","yes"))

#QAQC check, should all be "no"
unique(Data_Porometer_2$Difference)

#isolate errors
Data_Porometer_2_test <- Data_Porometer_2 %>% 
  filter(Difference == "yes")

#make changes to Data in data_QAQC folder
################################################################################


################################################################################
# Porometer_Est
################################################################################

#read csv
Data_Porometer <- read_csv("data_QAQC/Data.csv")

#Next, add info to porometer data
#Porometer_Est: 
#to keep plants in the study after they die, change porometer reading to 0

Data_Porometer_add_1 <- Data_Porometer %>% 
  filter(Dead == "dead", is.na(Porometer)) %>% 
  mutate(Porometer_Est = 0) %>% 
  select(c("Phase","Species","SpeciesID","Week","Porometer_Est"))

#Add data to Data_Porometer
Data_Porometer <- merge(Data_Porometer, Data_Porometer_add_1, by = c("Phase","Species","SpeciesID", "Week"), all = TRUE)

#Combine data to include Poromter_Est
Data_Porometer <- Data_Porometer %>% 
  mutate(Porometer_Est = ifelse(is.na(Porometer_Est), Data_Porometer$Porometer, Data_Porometer$Porometer_Est))

# #Now convert to NA during missing christmas readings
# Data_Porometer_testing_1 <- Data_Porometer
# Data_Porometer_testing_1$Porometer_Est1 <- ifelse(Data_Porometer_testing_1$Species == "PSME" & Data_Porometer_testing_1$Week == 17, NA, Data_Porometer_testing_1$Porometer_Est)
# Data_Porometer <- Data_Porometer_testing_1
# Data_Porometer_testing_2 <- Data_Porometer
# Data_Porometer_testing_2$Porometer_Est <- ifelse(Data_Porometer_testing_2$Species == "PSME" & Data_Porometer_testing_2$Week == 18, NA, Data_Porometer_testing_2$Porometer_Est1)
# Data_Porometer <- Data_Porometer_testing_2
# # Data_Porometer_testing_3 <- Data_Porometer
# # Data_Porometer_testing_3$Porometer_Est <- ifelse(Data_Porometer_testing_3$Species == "PIED" & Data_Porometer_testing_3$Week == 18, NA, Data_Porometer_testing_3$Porometer_Est2)
# # Data_Porometer <- Data_Porometer_testing_3

# #last, remove extra columns
# Data_Porometer <- Data_Porometer %>% 
#   select(-c("Porometer_Est1"))


#rearrange columns
#Data_Porometer <- Data_Porometer[ ,c(4,5,6,7,1,2,8,9,10,11,12,13,14,3,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]

#save as csv
write.csv(Data_Porometer, "data_QAQC/Data_Porometer.csv", quote = FALSE, row.names = FALSE)

################################################################################
