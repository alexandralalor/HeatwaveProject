#QAQC - merge initial data with plant data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read CSVs
Plants <- read_csv("data_clean/Plants.csv")
InitialData <- read_csv("data_clean/InitialData.csv")

#check structure, ensure consistent formats
#Date as <date>
#Time as <chr>
#DateTime as <dttm>
#Phase as <fctr>
#Chamber as <fctr>
#Kestrel as <fctr>
glimpse(InitialData)
glimpse(Plants)

#convert variables
InitialData$Phase <- as.factor(InitialData$Phase)
InitialData$Chamber <- as.factor(InitialData$Chamber)
InitialData$ScientificName <- as.factor(InitialData$ScientificName)
InitialData$CommonName <- as.factor(InitialData$CommonName)
InitialData$Species <- as.factor(InitialData$Species)
InitialData$Treatment_temp <- as.factor(InitialData$Treatment_temp)
InitialData$Treatment_water <- as.factor(InitialData$Treatment_water)
InitialData$PorometerSubset <- as.factor(InitialData$PorometerSubset)

Plants$Species <- as.factor(Plants$Species)
Plants$Dead <- as.factor(Plants$Dead)

#check values
unique(InitialData$Phase)
unique(InitialData$Chamber)
unique(InitialData$ScientificName)
unique(InitialData$CommonName)
unique(InitialData$Species)
unique(InitialData$SpeciesID)
unique(InitialData$Treatment_temp)
unique(InitialData$Treatment_water)
unique(InitialData$PorometerSubset)

unique(Plants$Species)
unique(Plants$SpeciesID)
unique(Plants$Week)
unique(Plants$Dead)


#Add metadata/combine info - Plants + InitialData
Data <- merge(InitialData, Plants, all = TRUE)
Data <- Data %>% 
  select(-c("BiomassBag_g","Bag_g","Comments")) %>% 
  arrange(Phase, SpeciesID, Week)
Data <- Data[ ,c(1,4,5,6,2,3,7,8,9,10,11,12,13,14,15,16,17,18)]
Data <- Data %>% 
  mutate(Dead_Count = ifelse(Data$Dead == "dead", 1, 0))


#create new heatwave variables for graphing
Data <- Data %>% 
  mutate(Heatwave_graph = Treatment_temp) %>% 
  separate(Heatwave_graph, sep = "_",
           into = c("Background_Temp", "Heatwave_graph")) %>% 
  mutate(Heatwave = Heatwave_graph)

Data$Heatwave[is.na(Data$Heatwave)] <- "no"
Data <- Data %>% 
  mutate(Heatwave = ifelse(Data$Heatwave == "HW", "yes", "no"))
Data <- Data %>% 
  mutate(Heatwave_graph = ifelse(Data$Heatwave_graph == "HW", "heatwave", Data$Heatwave_graph))

Data$Heatwave_graph <- str_c(Data$CommonName, "_", Data$Heatwave_graph)
Data$Heatwave_graph[is.na(Data$Heatwave_graph)] <- "X"
Data <- Data %>% 
  mutate(Heatwave_graph = ifelse(Data$Heatwave_graph == "X" & Data$CommonName == "Ponderosa Pine", "Ponderosa Pine", 
                                 ifelse(Data$Heatwave_graph == "X" & Data$CommonName == "Pinyon Pine", "Pinyon Pine",
                                        ifelse(Data$Heatwave_graph == "X" & Data$CommonName == "Limber Pine", "Limber Pine",
                                               ifelse(Data$Heatwave_graph == "X" & Data$CommonName == "Engelman Spruce", "Engelman Spruce",
                                                      ifelse(Data$Heatwave_graph == "X" & Data$CommonName == "Douglas fir", "Douglas fir", Data$Heatwave_graph))))))

Data <- Data %>% 
  select(-("Background_Temp"))



################################################################################
# the file Data contains all important metadata and experimental values        #
#         use this file to make changes found through the QAQC process         #
#              after QAQC, this file is ready to use for analysis              #
################################################################################

#save as csv
write.csv(Data, "data_QAQC/Data.csv", quote = FALSE, row.names = FALSE)
