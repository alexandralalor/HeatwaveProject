#Data wrangling script - Phase 1
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-11

#load tidyverse
library(tidyverse)

#read in csv
Phase1_Dates <- read_csv(file = "data_raw/meta/Phase1_Dates.csv")
Phase1_InitialData <- read_csv(file = "data_raw/meta/Phase1_InitialData.csv")
Phase1_Kestrel_Meta <- read_csv("data_raw/meta/Phase1_Kestrel_Meta.csv")
Phase1_TempSettings <- read_csv("data_raw/meta/Phase1_TempSettings.csv")

#check out data and make sure it looks ok
glimpse(Phase1_Dates)
glimpse(Phase1_InitialData)
glimpse(Phase1_Kestrel_Meta)
glimpse(Phase1_TempSettings)

#Convert Variables
Phase1_Dates$Chamber <- as.factor(Phase1_Dates$Chamber)

Phase1_InitialData$Phase <- as.factor(Phase1_InitialData$Phase)
Phase1_InitialData$Chamber <- as.factor(Phase1_InitialData$Chamber)
Phase1_InitialData$Whorls <- as.factor(Phase1_InitialData$Whorls)

Phase1_Kestrel_Meta$Phase <- as.factor(Phase1_Kestrel_Meta$Phase)
Phase1_Kestrel_Meta$Chamber <- as.factor(Phase1_Kestrel_Meta$Chamber)
Phase1_Kestrel_Meta$Kestrel <- as.factor(Phase1_Kestrel_Meta$Kestrel)

Phase1_TempSettings$Phase <- as.factor(Phase1_TempSettings$Phase)
Phase1_TempSettings$Chamber <- as.factor(Phase1_TempSettings$Chamber)
Phase1_TempSettings <- Phase1_TempSettings %>% 
  mutate(Kestrel = "calculated")

#DateTime - Temperature Settings
Phase1_TempSettings <- Phase1_TempSettings %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_TempSettings$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_TempSettings$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_TempSettings$Time), Time))))

#reorder columns - Temperature Settings
Phase1_TempSettings <- Phase1_TempSettings[, c(2,3,6,4,1,5)]

#save as csv
write.csv(Phase1_Dates, "data_clean/Phase1_Dates.csv", quote = FALSE, row.names = FALSE)
write.csv(Phase1_InitialData, "data_clean/Phase1_InitialData.csv", quote = FALSE, row.names = FALSE)
write.csv(Phase1_Kestrel_Meta, "data_clean/Phase1_Kestrel_Meta.csv", quote = FALSE, row.names = FALSE)
write.csv(Phase1_TempSettings, "data_clean/Phase1_TempSettings.csv", quote=FALSE, row.names = FALSE)

