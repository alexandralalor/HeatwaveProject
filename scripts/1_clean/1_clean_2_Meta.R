#Data wrangling script - Phase 1
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2026-03-01

#working directory
setwd("~/Desktop/R Projects/HeatwaveProject/")

#load tidyverse
library(tidyverse)

#read in csv
### Dates data not needed
#Phase1_Dates <- read_csv(file = "data_raw/meta/Phase1_Dates.csv")
Phase1_InitialData <- read_csv(file = "data/data_raw/meta/Phase1_InitialData.csv")
Phase1_TempSettings <- read_csv("data/data_raw/meta/Phase1_TempSettings.csv")

Phase2_InitialData <- read_csv("data/data_raw/meta/Phase2_InitialData.csv")
Phase2_TempSettings <- read_csv("data/data_raw/meta/Phase2_TempSettings.csv")



################################################################################
#Convert Variables

###Dates
#Phase1_Dates$Chamber <- as.factor(Phase1_Dates$Chamber)

###Initial Data
Phase1_InitialData$Phase <- as.factor(Phase1_InitialData$Phase)
Phase1_InitialData$Chamber <- as.factor(Phase1_InitialData$Chamber)

Phase2_InitialData$Phase <- as.factor(Phase2_InitialData$Phase)
Phase2_InitialData$Chamber <- as.factor(Phase2_InitialData$Chamber)


###Temperature Settings
Phase1_TempSettings$Phase <- as.factor(Phase1_TempSettings$Phase)
Phase1_TempSettings$Chamber <- as.factor(Phase1_TempSettings$Chamber)
Phase1_TempSettings <- Phase1_TempSettings %>% 
  mutate(Kestrel = "calculated")

Phase2_TempSettings$Phase <- as.factor(Phase2_TempSettings$Phase)
Phase2_TempSettings$Chamber <- as.factor(Phase2_TempSettings$Chamber)
Phase2_TempSettings <- Phase2_TempSettings %>% 
  mutate(Kestrel = "calculated")

#DateTime - Temperature Settings
Phase1_TempSettings <- Phase1_TempSettings %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_TempSettings$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_TempSettings$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_TempSettings$Time), Time))))

Phase2_TempSettings <- Phase2_TempSettings %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase2_TempSettings$Time), 
                       ifelse(Time == 30, paste0("00", Phase2_TempSettings$Time), 
                              ifelse(Time == 0, paste0("000", Phase2_TempSettings$Time), Time))))

#reorder columns - Temperature Settings
Phase1_TempSettings <- Phase1_TempSettings[, c(2,3,6,4,1,5)]
Phase2_TempSettings <- Phase2_TempSettings[, c(2,3,6,4,1,5)]


################################################################################
#Combine data
#Dates <- rbind(Phase1_Dates)
InitialData <- rbind(Phase1_InitialData, Phase2_InitialData)
TempSettings <- rbind(Phase1_TempSettings, Phase2_TempSettings)


################################################################################
#save as csv
#write.csv(Dates, "data_clean/Dates.csv", quote = FALSE, row.names = FALSE)
write.csv(InitialData, "data/data_clean/InitialData.csv", quote = FALSE, row.names = FALSE)
write.csv(TempSettings, "data/data_clean/TempSettings.csv", quote=FALSE, row.names = FALSE)

