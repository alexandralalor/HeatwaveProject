#QAQC - merge initial data with plant data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Plants <- read_csv("data_clean/Phase1_Plants.csv")
Phase1_InitialData <- read_csv("data_clean/Phase1_InitialData.csv")

#check structure, ensure consistent formats
#Date as <date>
#Time as <chr>
#DateTime as <dttm>
#Phase as <fctr>
#Chamber as <fctr>
#Kestrel as <fctr>
glimpse(Phase1_InitialData)
glimpse(Phase1_Plants)

#convert variables
Phase1_InitialData$Phase <- as.factor(Phase1_InitialData$Phase)
Phase1_InitialData$Chamber <- as.factor(Phase1_InitialData$Chamber)
Phase1_InitialData$ScientificName <- as.factor(Phase1_InitialData$ScientificName)
Phase1_InitialData$CommonName <- as.factor(Phase1_InitialData$CommonName)
Phase1_InitialData$Species <- as.factor(Phase1_InitialData$Species)
Phase1_InitialData$Treatment_temp <- as.factor(Phase1_InitialData$Treatment_temp)
Phase1_InitialData$Treatment_water <- as.factor(Phase1_InitialData$Treatment_water)
Phase1_InitialData$PorometerSubset <- as.factor(Phase1_InitialData$PorometerSubset)

Phase1_Plants$Species <- as.factor(Phase1_Plants$Species)
Phase1_Plants$Dead <- as.factor(Phase1_Plants$Dead)

#check values
unique(Phase1_InitialData$Phase)
unique(Phase1_InitialData$Chamber)
unique(Phase1_InitialData$ScientificName)
unique(Phase1_InitialData$CommonName)
unique(Phase1_InitialData$Species)
unique(Phase1_InitialData$SpeciesID)
unique(Phase1_InitialData$Treatment_temp)
unique(Phase1_InitialData$Treatment_water)
unique(Phase1_InitialData$PorometerSubset)

unique(Phase1_Plants$Species)
unique(Phase1_Plants$SpeciesID)
unique(Phase1_Plants$Week)
unique(Phase1_Plants$Dead)


#Add metadata/combine info - Plants + InitialData
Phase1_Data <- merge(Phase1_InitialData, Phase1_Plants, all = TRUE)
Phase1_Data <- Phase1_Data %>% 
  select(-c("BiomassBag_g","Bag_g","Comments")) %>% 
  arrange(SpeciesID, Week)
Phase1_Data <- Phase1_Data[ ,c(3,4,5,6,1,2,7,8,9,10,11,12,13,14,15,16,17,18)]
Phase1_Data <- Phase1_Data %>% 
  mutate(Dead_Count = ifelse(Phase1_Data$Dead == "dead", 1, 0))


################################################################################
# the file Phase1_Data contains all important metadata and experimental values #
#         use this file to make changes found through the QAQC process         #
#              after QAQC, this file is ready to use for analysis              #
################################################################################

#save as csv
write.csv(Phase1_Data, "data_QAQC/Phase1_Data.csv", quote = FALSE, row.names = FALSE)

