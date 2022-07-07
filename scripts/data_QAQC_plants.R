#QAQC - percent brown and weights
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

#check structure, ensure consistent formats
glimpse(Phase1_Data)

#convert variables
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

#check values
unique(Phase1_Data$Phase)
unique(Phase1_Data$Chamber)
unique(Phase1_Data$ScientificName)
unique(Phase1_Data$CommonName)
unique(Phase1_Data$Species)
unique(Phase1_Data$SpeciesID)
unique(Phase1_Data$Treatment_temp)
unique(Phase1_Data$Treatment_water)
unique(Phase1_Data$PorometerSubset)
unique(Phase1_Data$Week)
unique(Phase1_Data$Dead)


################################################################################
#Check PercentBrown - 10, 25, 50, 75, 90
################################################################################

#check for values outside of set range
unique(Phase1_Data$PercentBrown)

#isolate incorrect values
Phase1_Data %>% 
  filter(PercentBrown != 10, 
         PercentBrown != 25, 
         PercentBrown != 50,
         PercentBrown != 75,
         PercentBrown != 90)

#make changes to Phase1_Data in data_QAQC folder
################################################################################

################################################################################
#Check weights
################################################################################
#look for outliers in weight data
#For droughted plants, make sure that weights are always decreasing

#filter for weight data
Phase1_Data_Weight <- Phase1_Data %>% 
  select(c("Species","SpeciesID","Week","Treatment_temp","Treatment_water","Weight_g")) %>% 
  filter(Treatment_water == "Drought", !is.na(Weight_g)) %>% 
  arrange(SpeciesID, Week)

#Check that each drought plant is decreasing in weight
#add column for weight differences
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  mutate(Difference = Weight_g - lag(Weight_g))

#isolate incorrect values
#Seems like 2grams is a reasonable error, so look for differences above this
test <- Phase1_Data_Weight %>% 
  filter(Difference > 2, Week != 1)

#5 errors which are slightly high, but not outrageous. Keep in data 
#2 major errors (100+ grams) replaced with NA

#make changes to Phase1_Data in data_QAQC folder
################################################################################
